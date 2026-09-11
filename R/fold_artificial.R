# =========================================================================== #
# fold_artificial.R  —  make a folded scenario solver-ready.
#
# interp_mod(fold = TRUE) collapses a parameter's region / timeslice column to NA when
# the value is invariant across that dimension over its WHOLE domain (whole-column
# fold). NA is not a set member, so no solver accepts it. This pass replaces the
# NA wildcard with an artificial set member (ANYREGION / ANYTIMESLICE) and rewrites the
# model code so every folded-parameter lookup indexes that member:
#   pX[t,c,r,y,s]  ->  pX[t,c,'ANYREGION',y,s]      (region folded)
# The member is added to the SET only (never to a membership map). Every variable,
# equation and sum in the model is map-gated, so the member is inert to model
# STRUCTURE -- it exists solely to hold each folded parameter's single value.
#
# Substitution is position-based (the index aliases differ per equation, e.g.
# `r`, `region`), using each parameter's dimSets to locate the folded position.
# GLPK declarations use `{}` and only `[]` usages are rewritten; JuMP and Pyomo
# lookups have their own call shapes; GAMS spells a declaration and a use
# alike, so its rewrite skips declaration lines. The member is a property of the
# WRITTEN files: `revert_fold_artificial()` takes it back out of the scenario.
# =========================================================================== #

# Map a solver language to its `.modelCode` block name.
.fold_code_block <- function(lang) {
  lang <- tolower(as.character(lang))
  if (length(lang) == 0 || is.na(lang)) return("GLPK")
  if (grepl("gams", lang)) "GAMS"
  else if (grepl("jump", lang)) "JuMP"
  else if (grepl("pyomo", lang)) "PYOMOConcrete"
  else "GLPK"
}

# dim -> artificial set member written into the data / set, and whether it is a
# quoted string in the model code. `year` is INTEGER in energyRt (2025, 2030, ...),
# so its wildcard is the integer 0 (never a real milestone), written unquoted; the
# string dims use a quoted ANY* token.
.fold_any <- list(
  region = list(member = "ANYREGION", quote = TRUE),
  timeslice  = list(member = "ANYTIMESLICE",  quote = TRUE),
  year   = list(member = 0L,          quote = FALSE),
  comm   = list(member = "ANYCOMM",   quote = TRUE),
  tech   = list(member = "ANYTECH",   quote = TRUE),
  stg    = list(member = "ANYSTG",    quote = TRUE),
  trade  = list(member = "ANYTRADE",  quote = TRUE)
)

# Split a string on top-level commas, respecting () and [] nesting.
.split_top_commas <- function(s) {
  parts <- character(0); cur <- ""; depth <- 0L
  for (k in seq_len(nchar(s))) {
    ch <- substr(s, k, k)
    if (ch %in% c("[", "(")) depth <- depth + 1L
    else if (ch %in% c("]", ")")) depth <- depth - 1L
    if (ch == "," && depth == 0L) { parts <- c(parts, cur); cur <- "" }
    else cur <- paste0(cur, ch)
  }
  c(parts, cur)
}

# Replace the `pos`-th comma-separated index of every `<prefix><open> ... <close>`
# usage in `code` with `member`. `prefix` is a literal string ending right before
# the bracket that opens the index list; the char before a `prefix` match must be a
# non-identifier (so `pX` does not match inside `vpX`). Matching close found by
# bracket depth, so nested brackets and commas are safe.
#
# `skip` (optional) is a logical vector over `code` marking lines the rewrite
# must not touch: GAMS declaration blocks (`.gams_decl_lines`), where a use and
# a declaration are spelled alike.
.subst_indexed <- function(code, prefix, open, close, pos, member, skip = NULL) {
  hit <- which(vapply(code, function(l) grepl(prefix, l, fixed = TRUE), logical(1)))
  if (!is.null(skip)) hit <- hit[!skip[hit]]
  for (li in hit) {
    line <- code[li]; res <- ""; rest <- line
    repeat {
      m <- regexpr(prefix, rest, fixed = TRUE)
      if (m < 0) { res <- paste0(res, rest); break }
      pre  <- substr(rest, 1, m - 1)
      aft  <- substr(rest, m + nchar(prefix), nchar(rest))   # right after prefix
      # word-boundary: prefix must not continue an identifier on its left
      if (nchar(pre) > 0 && grepl("[A-Za-z0-9_.]$", substr(pre, nchar(pre), nchar(pre)))) {
        res <- paste0(res, pre, prefix); rest <- aft; next
      }
      if (substr(aft, 1, 1) != open) { res <- paste0(res, pre, prefix); rest <- aft; next }
      depth <- 0L; endi <- NA_integer_
      for (k in seq_len(nchar(aft))) {
        ch <- substr(aft, k, k)
        if (ch == open) depth <- depth + 1L
        else if (ch == close) { depth <- depth - 1L; if (depth == 0L) { endi <- k; break } }
      }
      if (is.na(endi)) { res <- paste0(res, pre, prefix); rest <- aft; next }
      inner <- substr(aft, 2, endi - 1)
      args  <- trimws(.split_top_commas(inner))
      if (length(args) >= pos) args[pos] <- member
      res  <- paste0(res, pre, prefix, open, paste(args, collapse = ","), close)
      rest <- substr(aft, endi + 1, nchar(aft))
    }
    code[li] <- res
  }
  code
}

# Per-backend index-usage patterns for a (possibly Up/Lo-suffixed) parameter name.
.subst_patterns <- function(backend, name) {
  if (backend == "GLPK")
    list(list(prefix = name, open = "[", close = "]"))
  else if (backend == "JuMP")
    list(list(prefix = paste0(name, "["),         open = "(", close = ")"),
         list(prefix = paste0("haskey(", name, ", "), open = "(", close = ")"))
  else if (grepl("PYOMO", backend))
    list(list(prefix = paste0(name, ".get("),     open = "(", close = ")"))
  else if (backend == "GAMS")
    # GAMS spells a declaration and a use identically (`p(tech, region, year)`
    # is both), so the pattern is GLPK's over `()` and the rewrite is confined
    # to non-declaration lines by `.gams_decl_lines()`.
    list(list(prefix = name, open = "(", close = ")"))
  else list()
}

# Lines belonging to a GAMS DECLARATION block: a block keyword at line start
# through the terminating `;`. Declarations must be left alone -- the artificial
# member is a real member of its set, so the declared domain
# `p(tech, region, year, timeslice)` already covers the wildcard key, while
# rewriting it to `p(tech, 'ANYREGION', year, timeslice)` is not a valid domain
# (a quoted label is an element, not a set) and would also break the `$loadm`
# GDX read that the declaration governs.
#
# A `*` comment inside a block may carry a `;`; treating that as the terminator
# drops the rest of the block and every declaration after it gets rewritten.
# `$ontext` / `$offtext` blocks likewise terminate nothing.
.gams_decl_lines <- function(code) {
  hdr <- paste0("^[[:space:]]*(sets?|parameters?|scalars?|table|equations?|",
                "((free|positive|negative|binary|integer)[[:space:]]+)?",
                "variables?)([[:space:]]|$)")
  out <- logical(length(code))
  inblk <- FALSE
  intext <- FALSE
  for (i in seq_along(code)) {
    ln <- code[i]
    if (grepl("^[[:space:]]*[$]ontext", ln, ignore.case = TRUE)) {
      intext <- TRUE; out[i] <- TRUE; next
    }
    if (intext) {
      out[i] <- TRUE
      if (grepl("^[[:space:]]*[$]offtext", ln, ignore.case = TRUE)) intext <- FALSE
      next
    }
    # `*` in column 1 is a full-line GAMS comment
    if (grepl("^[*]", ln)) { out[i] <- TRUE; next }
    if (!inblk && grepl(hdr, ln, ignore.case = TRUE)) inblk <- TRUE
    if (inblk) {
      out[i] <- TRUE
      # a block ends at the first `;`, which may sit on the header line itself
      if (grepl(";", ln, fixed = TRUE)) inblk <- FALSE
    }
  }
  out
}

# Member literal as written in each backend's model code, matching how that
# backend keys a REAL member of the dimension. Pyomo stringifies every set member
# (so the integer `year` wildcard is the string "0"); GLPK keeps `year` numeric
# (unquoted) and single-quotes string members. JuMP keys the `year` slot of its
# parameter Dicts numerically (the `as.character(year)` coercion in write_jump.R
# is disabled) while keying string dims (region/timeslice) as strings -- so the year
# wildcard must be the bare integer `0` for JuMP (a quoted "0" never matches the
# stored integer key, silently returning the default), but string wildcards stay
# double-quoted. `quote == FALSE` marks the numeric (`year`) wildcard.
.fold_member_literal <- function(backend, dim) {
  a <- .fold_any[[dim]]
  str_backend <- backend %in% c("JuMP", "PYOMOConcrete", "PYOMOAbstract")
  if (str_backend) {
    if (backend == "JuMP" && !isTRUE(a$quote)) return(as.character(a$member))
    return(paste0('"', a$member, '"'))
  }
  # GAMS: a label in an index position is always quoted, the numeric `year`
  # wildcard included (`'0'`; a bare 0 is a number, not a label).
  if (backend == "GAMS") return(paste0("'", a$member, "'"))
  if (!isTRUE(a$quote)) as.character(a$member) else paste0("'", a$member, "'")
}

# Identify which value parameters are whole-column folded on each foldable dim.
.folded_params <- function(scen, dims = names(.fold_any)) {
  out <- stats::setNames(vector("list", length(dims)), dims)
  for (nm in names(scen@modInp@parameters)) {
    p <- scen@modInp@parameters[[nm]]
    if (is.null(p) || p@type %in% c("set", "map")) next
    d <- as.data.frame(get_data_slot(p))
    if (is.null(d) || nrow(d) == 0) next
    for (dim in dims) {
      if (dim %in% names(d) && all(is.na(d[[dim]]))) out[[dim]] <- c(out[[dim]], nm)
    }
  }
  out
}

# -----------------------------------------------------------------------------#
# .partial_wildcard_params: parameters whose wildcard column is PARTIAL, i.e.
# NA in SOME rows and explicit in others.
#
# `.folded_params()` deliberately registers only WHOLE columns, and `fold.R`
# (`.fold_one_dim`) deliberately never creates a partial one -- because the code
# rewrite is per-PARAMETER: rewriting every `pX[...]` lookup to index
# 'ANYREGION' would strand the explicit rows.
#
# But a partial column can still arrive from the SOURCE data: a wildcard that
# `unfold_scenario_parameters()` could not materialise (no membership row for
# that entity) and that the fold pass then correctly declined to fold. It is
# nobody's output, so nothing converts it, nothing rewrites it, and
# `validate_scenario_parameters()` exempts the trimmable dims from its NA check.
# The raw NA reaches the solver, where it is not a set member, so every lookup
# that should hit it misses and silently takes the parameter's default.
#
# Measured consequence (IB_PTL50_CU50_P10, fold = TRUE): pTechEac carried NA in
# 106 of 208 region rows; only 3 of 141 mTechNew tuples found a value; capital
# cost effectively vanished and the model returned a NEGATIVE objective.
# -----------------------------------------------------------------------------#
.partial_wildcard_params <- function(scen, dims = names(.fold_any)) {
  out <- stats::setNames(vector("list", length(dims)), dims)
  for (nm in names(scen@modInp@parameters)) {
    p <- scen@modInp@parameters[[nm]]
    if (is.null(p) || p@type %in% c("set", "map")) next
    d <- as.data.frame(get_data_slot(p))
    if (is.null(d) || nrow(d) == 0) next
    for (dim in dims) {
      if (!dim %in% names(d)) next
      n <- sum(is.na(d[[dim]]))
      if (n > 0 && n < nrow(d)) out[[dim]] <- c(out[[dim]], nm)
    }
  }
  out
}

# -----------------------------------------------------------------------------#
# .materialise_partial_wildcards: expand partial wildcard rows to explicit
# members, so an UNREWRITTEN lookup finds them.
#
# This is the only correct treatment: the artificial member cannot represent a
# partial column (see above), so the rows have to become real. Values are
# unchanged -- one wildcard row becomes one row per allowed member.
#
# NOTE this is NOT undone by `revert_fold_artificial()`, and cannot be: the fold
# pass would decline to re-fold a partial column, so there is nothing to fold
# back to. For an on-disk parameter `.fold_write_back()` therefore leaves the
# stored data expanded. That is deliberate and safe -- the values are identical
# and these rows were never compressed in the first place (the fold pass refused
# them) -- but it does mean the row count of such a parameter grows once, on the
# first write after this fix.
# -----------------------------------------------------------------------------#
.materialise_partial_wildcards <- function(scen, dims = names(.fold_any),
                                           verbose = FALSE) {
  partial <- .partial_wildcard_params(scen, dims)
  todo <- unique(unlist(partial, use.names = FALSE))
  if (length(todo) == 0) return(scen)

  for (nm in todo) {
    p <- scen@modInp@parameters[[nm]]
    d <- as.data.frame(get_data_slot(p))
    ms <- .fold_member_sets(scen, d, dims = intersect(dims, names(d)))
    if (length(ms) == 0) next
    out <- tryCatch(unfold_parameter(p, ms), error = function(e) NULL)
    if (is.null(out) || nrow(out) == 0) next
    if (verbose) {
      message(sprintf("  materialise partial wildcard %-22s %d -> %d rows",
                      nm, nrow(d), nrow(out)))
    }
    scen@modInp@parameters[[nm]] <- .fold_write_back(p, as.data.frame(out))
  }
  scen
}

# -----------------------------------------------------------------------------#
# .assert_no_raw_wildcards: nothing may reach a writer with a raw NA in a
# foldable index column.
#
# NA is not a set member in any backend. GLPK/Pyomo/JuMP all resolve a missed
# key to the parameter's default, so the model stays feasible and solves to a
# confidently wrong answer -- there is no error to notice. This turns that into
# a build-time failure.
#
# It fires only on the broken case: measured across two unfolded production runs
# (613 and 603 written parameter files) the count of raw NAs is 0, while the
# folded run that produced the wrong objective had exactly 4.
# -----------------------------------------------------------------------------#
.assert_no_raw_wildcards <- function(scen, dims = names(.fold_any)) {
  bad <- character()
  for (nm in names(scen@modInp@parameters)) {
    p <- scen@modInp@parameters[[nm]]
    if (is.null(p) || p@type %in% c("set", "map")) next
    d <- as.data.frame(get_data_slot(p))
    if (is.null(d) || nrow(d) == 0) next
    for (dim in intersect(dims, names(d))) {
      n <- sum(is.na(d[[dim]]))
      if (n > 0) {
        bad <- c(bad, sprintf("  %s$%s: %d of %d rows", nm, dim, n, nrow(d)))
      }
    }
  }
  if (length(bad) == 0) return(invisible(TRUE))
  stop("fold: ", length(bad), " parameter column(s) still hold a raw NA ",
       "wildcard and cannot be written.
",
       paste(bad, collapse = "
"),
       "
  NA is not a set member: every lookup that should hit these rows ",
       "would miss and silently take the parameter's default, so the model ",
       "would solve to a wrong answer rather than fail.
",
       "  They could not be expanded to explicit members (no membership rows ",
       "for those entities). Fix the source data, or re-interpolate with ",
       "fold = FALSE.", call. = FALSE)
}

# Replace NA wildcards with the artificial set member, register the member in the
# set, and rewrite the model code of `backends` so folded lookups index it.
apply_fold_artificial <- function(scen, backends = "GLPK",
                                  dims = names(.fold_any), verbose = FALSE) {
  # A PARTIAL wildcard column cannot be represented by the artificial member --
  # the rewrite is per-parameter, so pointing every lookup at 'ANYREGION' would
  # strand the explicit rows. Expand those to real members first, leaving only
  # the whole-column case the rewrite below is built for.
  scen <- .materialise_partial_wildcards(scen, dims, verbose = verbose)

  folded <- .folded_params(scen, dims)
  if (all(lengths(folded) == 0)) {
    .assert_no_raw_wildcards(scen, dims)
    return(scen)
  }

  for (dim in names(folded)) {
    if (length(folded[[dim]]) == 0) next
    member <- .fold_any[[dim]]$member

    # 1. add the artificial member to the set parameter, EXCEPT the `year`
    #    wildcard when targeting JuMP. JuMP parameters are plain Julia `Dict`s, so
    #    the wildcard only has to be a Dict KEY (handled in step 2 + the lookup
    #    rewrite) -- it must NOT join the `year` set that constraint loops iterate.
    #    JuMP gate conditions hard-index year params, e.g.
    #    `ordYear[(yp)] for yp in year`, so a spurious `0` in the `year` set throws
    #    `KeyError: key 0 not found`. GLPK/Pyomo declare params over the set and
    #    default missing keys, so they need the member in the set and tolerate it
    #    in the loop. (Assumes single-backend calls, as from solve_scenario's `.blk`.)
    skip_set_member <- dim == "year" && all(backends == "JuMP")
    if (!skip_set_member) {
      setp <- scen@modInp@parameters[[dim]]
      sd <- as.data.frame(get_data_slot(setp))
      if (!member %in% sd[[dim]]) {
        sd <- rbind(sd, stats::setNames(data.frame(member, stringsAsFactors = FALSE), dim))
        scen@modInp@parameters[[dim]] <- .fold_write_back(setp, sd)
      }
    }

    # 2. replace NA -> member in every folded parameter's data
    for (nm in folded[[dim]]) {
      p <- scen@modInp@parameters[[nm]]
      d <- as.data.frame(get_data_slot(p))
      d[[dim]][is.na(d[[dim]])] <- member
      scen@modInp@parameters[[nm]] <- .fold_write_back(p, d)
    }
  }

  # 3. rewrite the model code: index each folded parameter at the member literal
  for (bk in backends) {
    code <- scen@settings@sourceCode[[bk]]
    if (is.null(code)) next
    # Computed once: substitution rewrites lines in place, so the line count --
    # and hence the mask -- stays valid across the loop below.
    skip <- if (bk == "GAMS") .gams_decl_lines(code) else NULL
    for (dim in names(folded)) {
      lit <- .fold_member_literal(bk, dim)
      for (nm in folded[[dim]]) {
        p <- scen@modInp@parameters[[nm]]
        pos <- match(dim, p@dimSets)
        if (is.na(pos)) next
        # bounds parameters are emitted in the model code with Up / Lo / Fx
        # suffixes (the `type` column is not part of `dimSets`, so the folded
        # position is unchanged); numpar parameters keep their bare name.
        targets <- if (as.character(p@type) == "bounds")
          paste0(nm, c("Up", "Lo", "Fx")) else nm
        for (tg in targets) {
          for (pat in .subst_patterns(bk, tg)) {
            code <- .subst_indexed(code, pat$prefix, pat$open, pat$close, pos,
                                   lit, skip = skip)
          }
        }
      }
    }
    scen@settings@sourceCode[[bk]] <- code
  }
  # what `revert_fold_artificial()` has to take back out
  scen@misc$fold_artificial <- names(folded)[lengths(folded) > 0]
  # Last line of defence: after the conversion above, a surviving raw NA is a
  # wildcard nothing can represent, and writing it produces a wrong answer with
  # no error. Fail here instead.
  .assert_no_raw_wildcards(scen, dims)
  scen
}

# Undo `apply_fold_artificial()` on the scenario object: the artificial member
# back to the NA wildcard in every folded value parameter, and out of each set.
# Left in, the region set reads `R1 R2 ANYREGION` and the year set `2020 0`; a
# read-time unfold then expands the region wildcard over ANYREGION too, and the
# year wildcard `0` (neither NA nor ANY*) is not expanded at all. Idempotent.
# The rewritten model source stays: it is re-copied at interpolation and the
# substitution rewrites an already substituted position to the same literal.
revert_fold_artificial <- function(scen, dims = scen@misc$fold_artificial) {
  dims <- intersect(dims, names(.fold_any))
  if (length(dims) == 0) return(scen)
  members <- lapply(.fold_any[dims], `[[`, "member")
  for (dim in dims) {
    setp <- scen@modInp@parameters[[dim]]
    if (is.null(setp)) next
    sd <- as.data.frame(get_data_slot(setp))
    if (nrow(sd) > 0 && any(sd[[dim]] %in% members[[dim]])) {
      scen@modInp@parameters[[dim]] <-
        .fold_write_back(setp, sd[!sd[[dim]] %in% members[[dim]], , drop = FALSE])
    }
  }
  # one pass over the value parameters, every substituted dim at once
  for (nm in names(scen@modInp@parameters)) {
    p <- scen@modInp@parameters[[nm]]
    if (is.null(p) || p@type %in% c("set", "map")) next
    d <- as.data.frame(get_data_slot(p))
    if (is.null(d) || nrow(d) == 0) next
    touched <- FALSE
    for (dim in intersect(dims, names(d))) {
      hit <- !is.na(d[[dim]]) & d[[dim]] %in% members[[dim]]
      if (!any(hit)) next
      d[[dim]][hit] <- NA
      touched <- TRUE
    }
    if (touched) scen@modInp@parameters[[nm]] <- .fold_write_back(p, d)
  }
  scen@misc$fold_artificial <- NULL
  scen
}
