# =========================================================================== #
# fold_artificial.R  —  make a folded scenario solver-ready.
#
# interp_mod(fold = TRUE) collapses a parameter's region / slice column to NA when
# the value is invariant across that dimension over its WHOLE domain (whole-column
# fold). NA is not a set member, so no solver accepts it. This pass replaces the
# NA wildcard with an artificial set member (ANYREGION / ANYSLICE) and rewrites the
# model code so every folded-parameter lookup indexes that member:
#   pX[t,c,r,y,s]  ->  pX[t,c,'ANYREGION',y,s]      (region folded)
# The member is added to the SET only (never to a membership map). Every variable,
# equation and sum in the model is map-gated, so the member is inert to model
# STRUCTURE -- it exists solely to hold each folded parameter's single value.
#
# Substitution is position-based (the index aliases differ per equation, e.g.
# `r`, `region`), using each parameter's dimSets to locate the folded position.
# Declarations use `{}` and are left untouched; only `[]` usages are rewritten.
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
  slice  = list(member = "ANYSLICE",  quote = TRUE),
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
.subst_indexed <- function(code, prefix, open, close, pos, member) {
  hit <- which(vapply(code, function(l) grepl(prefix, l, fixed = TRUE), logical(1)))
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
  else list()  # GAMS: declaration/usage share `()`, needs section-aware handling
}

# Member literal as written in each backend's model code, matching how that
# backend keys a REAL member of the dimension. Pyomo stringifies every set member
# (so the integer `year` wildcard is the string "0"); GLPK keeps `year` numeric
# (unquoted) and single-quotes string members. JuMP keys the `year` slot of its
# parameter Dicts numerically (the `as.character(year)` coercion in write_jump.R
# is disabled) while keying string dims (region/slice) as strings -- so the year
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

# Replace NA wildcards with the artificial set member, register the member in the
# set, and rewrite the model code of `backends` so folded lookups index it.
apply_fold_artificial <- function(scen, backends = "GLPK",
                                  dims = names(.fold_any)) {
  folded <- .folded_params(scen, dims)
  if (all(lengths(folded) == 0)) return(scen)

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
    #    in the loop. (Assumes single-backend calls, as from solve_scen's `.blk`.)
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
            code <- .subst_indexed(code, pat$prefix, pat$open, pat$close, pos, lit)
          }
        }
      }
    }
    scen@settings@sourceCode[[bk]] <- code
  }
  scen
}
