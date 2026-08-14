# =============================================================================
# techspec — YAML process specifications
# =============================================================================
# A *techspec* is a YAML document describing one process (a `technology` for
# now; storage and trade share the same anatomy and follow later) as data:
# ports (inputs/outputs/aux), parameter blocks (efficiency, costs, vintages,
# capacity, ...), and design-time context. It is the durable format behind
# the process designer (`process_designer()`, inst/designer/) and a validated
# replacement for spreadsheet-based technology definition.
#
# Validation policy (the anti-spreadsheet rules):
#   * unknown sections or columns are ERRORS — nothing is silently dropped;
#   * a row without a dimension key applies to ALL members of that dimension
#     (writable explicitly as "ALL"); this is a deliberate, visible choice;
#   * NOTHING is auto-invented: the reader never adds vintages or values;
#   * per-vintage values covering only a subset of the declared vintages are
#     REPORTED as an issue (warning), never silently filled.
#
# API: tech_from_spec(), tech_to_spec(), tech_spec_issues(),
#      tech_spec_code() (the equivalent newTechnology() call as text).
# =============================================================================

# Section registry: spec key -> newTechnology() argument + allowed columns.
# Dimension columns get the ALL/absent semantics; `vintage_dim` marks tables
# that participate in the vintage-coverage check.
.TECHSPEC_SECTIONS <- list(
  inputs = list(
    arg = "input", cols = c("comm", "unit", "group", "combustion"),
    dims = character()),
  outputs = list(
    arg = "output", cols = c("comm", "unit", "group"),
    dims = character()),
  aux = list(
    arg = "aux", cols = c("acomm", "unit"),
    dims = character()),
  groups = list(
    arg = "group", cols = c("group", "desc", "unit"),
    dims = character()),
  efficiency = list(
    arg = "ceff",
    cols = c("vintage", "cluster", "region", "year", "timeslice", "comm",
             "cinp2use", "use2cact", "cact2cout", "cinp2ginp",
             "share.lo", "share.up", "share.fx",
             "afc.lo", "afc.up", "afc.fx"),
    dims = c("vintage", "cluster", "region", "year", "timeslice")),
  group_efficiency = list(
    arg = "geff",
    cols = c("vintage", "cluster", "region", "year", "timeslice", "group",
             "ginp2use"),
    dims = c("vintage", "cluster", "region", "year", "timeslice")),
  aux_efficiency = list(
    arg = "aeff",
    cols = c("vintage", "cluster", "acomm", "comm", "region", "year",
             "timeslice", "cinp2ainp", "cinp2aout", "cout2ainp", "cout2aout",
             "act2ainp", "act2aout", "cap2ainp", "cap2aout",
             "ncap2ainp", "ncap2aout"),
    dims = c("vintage", "cluster", "region", "year", "timeslice")),
  availability = list(
    arg = "af",
    cols = c("vintage", "cluster", "region", "year", "timeslice",
             "af.lo", "af.up", "af.fx", "rampup", "rampdown"),
    dims = c("vintage", "cluster", "region", "year", "timeslice")),
  afs = list(
    arg = "afs",
    cols = c("vintage", "cluster", "region", "year", "timeslice",
             "afs.lo", "afs.up", "afs.fx"),
    dims = c("vintage", "cluster", "region", "year", "timeslice")),
  weather = list(
    arg = "weather",
    cols = c("vintage", "cluster", "weather", "comm",
             "wafc.lo", "wafc.up", "wafc.fx",
             "waf.lo", "waf.up", "waf.fx",
             "wafs.lo", "wafs.up", "wafs.fx"),
    dims = c("vintage", "cluster")),
  invcost = list(
    arg = "invcost",
    cols = c("vintage", "cluster", "region", "year",
             "invcost", "wacc", "payback", "eac", "retcost"),
    dims = c("vintage", "cluster", "region", "year")),
  fixom = list(
    arg = "fixom",
    cols = c("vintage", "cluster", "region", "year", "fixom"),
    dims = c("vintage", "cluster", "region", "year")),
  varom = list(
    arg = "varom",
    cols = c("vintage", "cluster", "region", "year", "timeslice",
             "comm", "acomm", "varom", "cvarom", "avarom"),
    dims = c("vintage", "cluster", "region", "year", "timeslice")),
  vintages = list(
    arg = "vintage",
    cols = c("vintage", "region", "cluster", "start", "end", "olife"),
    dims = c("region", "cluster")),
  clusters = list(
    arg = "cluster",
    cols = c("cluster", "desc", "region", "order"),
    dims = c("region")),
  capacity = list(
    arg = "capacity",
    cols = c("vintage", "cluster", "region", "year", "stock",
             "cap.lo", "cap.up", "cap.fx", "ncap.lo", "ncap.up", "ncap.fx",
             "ret.lo", "ret.up", "ret.fx"),
    dims = c("vintage", "cluster", "region", "year"))
)

# Scalar / non-tabular spec keys. `olife` is the simple lifetime for
# non-vintaged processes; with a `vintages` section, put olife there.
# `image` is a link (URL or path) to an illustration, used in reports.
# `levcost` holds levelized-cost assumptions (list: discount, costs =
# named commodity prices) used to pre-fill the designer's levcost/report
# forms; both live on @misc.
# `report` names the report template (a built-in template name or an .Rmd
# path); like `image` and `levcost` it lives on @misc.
.TECHSPEC_SCALARS <- c("techspec", "class", "name", "desc", "units",
                       "cap2act", "olife", "timeframe", "region", "fullYear",
                       "optimizeRetirement", "notes", "image", "levcost",
                       "report", "context")

#' Read and validate a techspec
#'
#' Parses a techspec file (or an already-parsed list) into the
#' normalised list form used by [tech_from_spec()] and the process
#' designer. YAML (`.yml`/`.yaml`) and JSON (`.json`) are alternative
#' containers for the same structure.
#'
#' @param spec Path to a `.yml`/`.yaml`/`.json` file, or a list as
#'   returned by [yaml::read_yaml()].
#' @return The normalised spec list (invisibly validated; see
#'   [tech_spec_issues()] for the report).
#' @export
read_techspec <- function(spec) {
  if (is.character(spec) && length(spec) == 1) {
    if (!file.exists(spec)) stop("techspec file not found: ", spec)
    spec <- if (grepl("[.]json$", spec, ignore.case = TRUE)) {
      .techspec_need_jsonlite("read a JSON techspec")
      # object arrays stay lists of named lists, scalar arrays become
      # vectors -- the same shape yaml::read_yaml() yields
      jsonlite::fromJSON(spec, simplifyVector = TRUE,
                         simplifyDataFrame = FALSE,
                         simplifyMatrix = FALSE)
    } else {
      yaml::read_yaml(spec)
    }
  }
  if (!is.list(spec)) stop("`spec` must be a file path or a list")
  spec
}

#' @noRd
.techspec_need_jsonlite <- function(what) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("the 'jsonlite' package is required to ", what,
         "; install it with install.packages(\"jsonlite\")",
         call. = FALSE)
  }
}

#' Validate a techspec and report issues
#'
#' Applies the techspec validation policy: unknown sections/columns are
#' errors; per-vintage values covering only a subset of the declared
#' vintages are reported (never silently filled); nothing is auto-invented.
#'
#' @param spec Path or list (see [read_techspec()]).
#' @return A `data.frame` with columns `severity` (`"error"`/`"warning"`),
#'   `section`, and `message`. Zero rows means a clean spec.
#' @examples
#' f <- system.file("techspec", "examples", "coal_power.yml",
#'                  package = "energyRt")
#' if (nzchar(f)) tech_spec_issues(f)
#' @export
tech_spec_issues <- function(spec) {
  spec <- read_techspec(spec)
  iss <- list()
  add <- function(severity, section, message) {
    iss[[length(iss) + 1L]] <<- data.frame(
      severity = severity, section = section, message = message,
      stringsAsFactors = FALSE)
  }

  # -- structure ---------------------------------------------------------------
  known <- c(.TECHSPEC_SCALARS, names(.TECHSPEC_SECTIONS))
  unknown <- setdiff(names(spec), known)
  if (length(unknown) > 0) {
    add("error", "spec", paste0("unknown section(s): ",
                                paste(unknown, collapse = ", ")))
  }
  if (is.null(spec$name) || !nzchar(as.character(spec$name)[1])) {
    add("error", "name", "`name` is required")
  }
  cls <- spec$class %||% "technology"
  if (!identical(cls, "technology")) {
    add("error", "class",
        paste0("class '", cls, "' is not supported yet (technology only; ",
               "storage and trade follow later)"))
  }

  # -- sections: unknown columns ----------------------------------------------
  for (sec in intersect(names(spec), names(.TECHSPEC_SECTIONS))) {
    rows <- spec[[sec]]
    if (is.null(rows)) next
    if (!is.list(rows)) {
      add("error", sec, "must be a list of rows (mappings)")
      next
    }
    cols <- .TECHSPEC_SECTIONS[[sec]]$cols
    for (i in seq_along(rows)) {
      bad <- setdiff(names(rows[[i]]), cols)
      if (length(bad) > 0) {
        add("error", sec,
            paste0("row ", i, ": unknown column(s): ",
                   paste(bad, collapse = ", "),
                   " (allowed: ", paste(cols, collapse = ", "), ")"))
      }
    }
  }

  # -- ports -------------------------------------------------------------------
  n_inp <- length(spec$inputs %||% list())
  n_out <- length(spec$outputs %||% list())
  if (n_inp == 0) add("error", "inputs", "at least one input is required")
  if (n_out == 0) add("error", "outputs", "at least one output is required")

  port_comms <- c(
    vapply(spec$inputs %||% list(), function(r) as.character(r$comm %||% NA),
           character(1)),
    vapply(spec$outputs %||% list(), function(r) as.character(r$comm %||% NA),
           character(1)))
  port_comms <- port_comms[!is.na(port_comms)]
  eff_comms <- vapply(spec$efficiency %||% list(),
                      function(r) as.character(r$comm %||% NA), character(1))
  bad_eff <- setdiff(eff_comms[!is.na(eff_comms)], port_comms)
  if (length(bad_eff) > 0) {
    add("error", "efficiency",
        paste0("commodity not among declared ports: ",
               paste(unique(bad_eff), collapse = ", ")))
  }
  if (n_inp > 0 && n_out > 0 && length(spec$efficiency %||% list()) == 0) {
    add("warning", "efficiency",
        "no efficiency rows: inputs are not linked to outputs")
  }

  # -- vintages ----------------------------------------------------------------
  vint_rows <- spec$vintages %||% list()
  declared <- unique(vapply(vint_rows,
                            function(r) as.character(r$vintage %||% NA),
                            character(1)))
  declared <- declared[!is.na(declared)]
  for (i in seq_along(vint_rows)) {
    r <- vint_rows[[i]]
    if (!is.null(r$start) && !is.null(r$end) &&
        is.finite(as.numeric(r$end)) && is.finite(as.numeric(r$start)) &&
        as.numeric(r$end) < as.numeric(r$start)) {
      add("error", "vintages",
          paste0("row ", i, " (", r$vintage, "): end (", r$end,
                 ") is before start (", r$start, ")"))
    }
  }

  # -- vintage coverage: subset-coverage must be reported ----------------------
  if (length(declared) > 0) {
    # `capacity` is exempt: stock and bounds legitimately belong to specific
    # vintages (the pre-existing fleet is one vintage, not all of them)
    for (sec in setdiff(names(.TECHSPEC_SECTIONS),
                        c("vintages", "capacity"))) {
      rows <- spec[[sec]]
      if (is.null(rows) || length(rows) == 0) next
      if (!"vintage" %in% .TECHSPEC_SECTIONS[[sec]]$dims) next
      vint_of <- vapply(rows, function(r) {
        v <- r$vintage
        if (is.null(v)) NA_character_ else as.character(v)
      }, character(1))
      has_all  <- any(is.na(vint_of) | vint_of == "ALL")
      specific <- unique(vint_of[!is.na(vint_of) & vint_of != "ALL"])
      if (length(specific) == 0) next   # everything applies to ALL: fine
      undeclared <- setdiff(specific, declared)
      if (length(undeclared) > 0) {
        add("error", sec,
            paste0("value(s) for undeclared vintage(s): ",
                   paste(undeclared, collapse = ", ")))
      }
      if (!has_all) {
        missing <- setdiff(declared, specific)
        if (length(missing) > 0) {
          add("warning", sec,
              paste0("defined for ", length(specific), " of ",
                     length(declared), " declared vintages - missing: ",
                     paste(missing, collapse = ", "),
                     " (values are NOT auto-filled)"))
        }
      }
    }
  }

  if (length(iss) == 0) {
    return(data.frame(severity = character(), section = character(),
                      message = character(), stringsAsFactors = FALSE))
  }
  do.call(rbind, iss)
}

#' Rows of one spec section as a data.frame
#'
#' `"ALL"` in a dimension column becomes `NA` (the energyRt wildcard); a
#' dimension key that is absent from a row is also `NA` — both mean "applies
#' to all members".
#' @noRd
.techspec_rows_df <- function(rows, cols) {
  if (is.null(rows) || length(rows) == 0) return(NULL)
  used <- unique(unlist(lapply(rows, names)))
  used <- cols[cols %in% used]
  chr_cols <- c("vintage", "cluster", "region", "timeslice", "comm",
                "acomm", "group", "unit", "desc", "combustion", "weather")
  out <- lapply(used, function(cc) {
    vals <- lapply(rows, function(r) {
      v <- r[[cc]]
      if (is.null(v) || identical(v, "ALL")) NA else v
    })
    v <- unlist(vals, use.names = FALSE)
    # YAML scalars arrive as integer/character; the slot prototypes want
    # character dimensions and double values
    if (cc %in% chr_cols) as.character(v) else as.numeric(v)
  })
  names(out) <- used
  as.data.frame(out, stringsAsFactors = FALSE, optional = TRUE)
}

#' Build a technology object from a techspec
#'
#' Reads, validates, and constructs the [newTechnology()] call described by
#' the spec. Errors in the issue report stop the build (warnings are shown).
#'
#' @param spec Path or list (see [read_techspec()]).
#' @param strict Stop on warnings too? Default `FALSE`.
#' @return A `technology` object.
#' @examples
#' f <- system.file("techspec", "examples", "coal_power.yml",
#'                  package = "energyRt")
#' if (nzchar(f)) tech_from_spec(f)
#' @export
tech_from_spec <- function(spec, strict = FALSE) {
  spec <- read_techspec(spec)
  iss <- tech_spec_issues(spec)
  errs <- iss[iss$severity == "error", , drop = FALSE]
  wrns <- iss[iss$severity == "warning", , drop = FALSE]
  if (nrow(errs) > 0 || (strict && nrow(wrns) > 0)) {
    stop("techspec has issues:\n",
         paste0("  [", iss$severity, "] ", iss$section, ": ", iss$message,
                collapse = "\n"))
  }
  for (i in seq_len(nrow(wrns))) {
    warning("techspec [", wrns$section[i], "]: ", wrns$message[i],
            call. = FALSE)
  }

  args <- list(name = as.character(spec$name))
  if (!is.null(spec$desc))      args$desc      <- as.character(spec$desc)
  if (!is.null(spec$units))     args$units     <- spec$units
  if (!is.null(spec$cap2act))   args$cap2act   <- as.numeric(spec$cap2act)
  if (!is.null(spec$olife) && is.null(spec$vintages)) {
    args$olife <- list(olife = as.numeric(spec$olife))
  }
  if (!is.null(spec$timeframe)) args$timeframe <- as.character(spec$timeframe)
  if (!is.null(spec$region))    args$region    <- spec$region
  if (!is.null(spec$fullYear))  args$fullYear  <- isTRUE(spec$fullYear)
  if (!is.null(spec$optimizeRetirement)) {
    args$optimizeRetirement <- isTRUE(spec$optimizeRetirement)
  }
  misc <- list()
  if (!is.null(spec$notes)) misc$notes <- spec$notes
  if (!is.null(spec$image)) misc$image <- as.character(spec$image)
  if (!is.null(spec$report)) misc$report <- as.character(spec$report)
  if (!is.null(spec$levcost)) misc$levcost <- spec$levcost
  if (length(misc) > 0) args$misc <- misc

  for (sec in intersect(names(spec), names(.TECHSPEC_SECTIONS))) {
    df <- .techspec_rows_df(spec[[sec]], .TECHSPEC_SECTIONS[[sec]]$cols)
    if (!is.null(df) && nrow(df) > 0) {
      args[[.TECHSPEC_SECTIONS[[sec]]$arg]] <- df
    }
  }
  do.call(newTechnology, args)
}

#' Export a technology object as a techspec list
#'
#' The reverse of [tech_from_spec()]: slot tables become row lists (all-`NA`
#' columns and empty tables are omitted; `NA` dimension values are omitted
#' from rows, i.e. read back as "applies to ALL").
#'
#' @param tech A `technology` object.
#' @param file Optional path; when given, the spec is written as YAML,
#'   or as JSON when the path ends in `.json` (same structure, an
#'   alternative container).
#' @return The spec list (invisibly when `file` is given).
#' @export
tech_to_spec <- function(tech, file = NULL) {
  stopifnot(is(tech, "technology"))
  spec <- list(techspec = 1L, class = "technology",
               name = tech@name)
  if (nzchar(tech@desc %||% "")) spec$desc <- tech@desc
  un <- tech@units
  if (is.data.frame(un) && nrow(un) > 0) {
    keep <- !vapply(un, function(x) all(is.na(x)), logical(1))
    if (any(keep)) spec$units <- as.list(un[1, keep, drop = FALSE])
  }
  if (length(tech@cap2act) == 1 && is.finite(tech@cap2act)) {
    spec$cap2act <- unname(tech@cap2act)
  }
  if (length(tech@timeframe) > 0 && nzchar(tech@timeframe[1])) {
    spec$timeframe <- as.character(tech@timeframe[1])
  }
  if (length(tech@region) > 0) spec$region <- as.character(tech@region)
  if (isFALSE(tech@fullYear)) spec$fullYear <- FALSE
  if (isTRUE(tech@optimizeRetirement)) spec$optimizeRetirement <- TRUE
  if (!is.null(tech@misc$image)) spec$image <- as.character(tech@misc$image)
  if (!is.null(tech@misc$report)) spec$report <- as.character(tech@misc$report)
  if (!is.null(tech@misc$levcost)) spec$levcost <- tech@misc$levcost
  if (!is.null(tech@misc$notes)) spec$notes <- tech@misc$notes

  for (sec in names(.TECHSPEC_SECTIONS)) {
    df <- slot(tech, .TECHSPEC_SECTIONS[[sec]]$arg)
    if (!is.data.frame(df) || nrow(df) == 0) next
    rows <- lapply(seq_len(nrow(df)), function(i) {
      r <- as.list(df[i, , drop = FALSE])
      r <- r[!vapply(r, function(v) length(v) == 0 || is.na(v), logical(1))]
      lapply(r, function(v) if (is.factor(v)) as.character(v) else unname(v))
    })
    rows <- rows[vapply(rows, length, integer(1)) > 0]
    if (length(rows) > 0) spec[[sec]] <- rows
  }

  if (!is.null(file)) {
    if (grepl("[.]json$", file, ignore.case = TRUE)) {
      .techspec_need_jsonlite("write a JSON techspec")
      # digits = NA is load-bearing: the jsonlite default (4) would
      # corrupt efficiencies like use2cact = 1798.6111111
      jsonlite::write_json(spec, file, auto_unbox = TRUE, pretty = TRUE,
                           digits = NA, na = "null")
    } else {
      yaml::write_yaml(spec, file)
    }
    return(invisible(spec))
  }
  spec
}

#' The newTechnology() call equivalent to a techspec, as text
#'
#' Used by the designer's export tab.
#'
#' @param spec Path or list (see [read_techspec()]).
#' @return A single character string of R code.
#' @export
tech_spec_code <- function(spec) {
  spec <- read_techspec(spec)
  fmt_df <- function(df) {
    cols <- vapply(names(df), function(cc) {
      v <- df[[cc]]
      vs <- if (is.character(v)) {
        paste0("c(", paste(ifelse(is.na(v), "NA",
                                  paste0('"', v, '"')), collapse = ", "), ")")
      } else {
        paste0("c(", paste(ifelse(is.na(v), "NA", format(v)),
                           collapse = ", "), ")")
      }
      paste0("    ", if (grepl("[.]", cc)) paste0("`", cc, "`") else cc,
             " = ", vs)
    }, character(1))
    paste0("data.frame(\n", paste(cols, collapse = ",\n"),
           ",\n    stringsAsFactors = FALSE)")
  }
  parts <- c(sprintf('  name = "%s"', spec$name %||% ""),
             if (!is.null(spec$desc)) sprintf('  desc = "%s"', spec$desc))
  if (!is.null(spec$units)) {
    parts <- c(parts, paste0("  units = list(",
                             paste(sprintf('%s = "%s"', names(spec$units),
                                           unlist(spec$units)),
                                   collapse = ", "), ")"))
  }
  if (!is.null(spec$cap2act)) {
    parts <- c(parts, sprintf("  cap2act = %s", format(spec$cap2act)))
  }
  if (!is.null(spec$olife) && is.null(spec$vintages)) {
    parts <- c(parts, sprintf("  olife = list(olife = %s)",
                              format(as.numeric(spec$olife))))
  }
  if (!is.null(spec$timeframe)) {
    parts <- c(parts, sprintf('  timeframe = "%s"', spec$timeframe))
  }
  if (!is.null(spec$region)) {
    parts <- c(parts, paste0("  region = ",
                             paste(deparse(as.character(spec$region)),
                                   collapse = "")))
  }
  if (!is.null(spec$fullYear)) {
    parts <- c(parts, paste0("  fullYear = ", isTRUE(spec$fullYear)))
  }
  if (!is.null(spec$optimizeRetirement)) {
    parts <- c(parts, paste0("  optimizeRetirement = ",
                             isTRUE(spec$optimizeRetirement)))
  }
  # misc-bound scalars (image, report template, notes, levcost assumptions)
  misc <- list()
  if (!is.null(spec$notes))   misc$notes   <- spec$notes
  if (!is.null(spec$image))   misc$image   <- as.character(spec$image)
  if (!is.null(spec$report))  misc$report  <- as.character(spec$report)
  if (!is.null(spec$levcost)) misc$levcost <- spec$levcost
  if (length(misc) > 0) {
    ms <- paste(deparse(misc, width.cutoff = 60),
                collapse = "\n    ")
    parts <- c(parts, paste0("  misc = ", ms))
  }
  for (sec in intersect(names(spec), names(.TECHSPEC_SECTIONS))) {
    df <- .techspec_rows_df(spec[[sec]], .TECHSPEC_SECTIONS[[sec]]$cols)
    if (is.null(df) || nrow(df) == 0) next
    parts <- c(parts, paste0("  ", .TECHSPEC_SECTIONS[[sec]]$arg, " = ",
                             fmt_df(df)))
  }
  paste0("newTechnology(\n", paste(parts, collapse = ",\n"), "\n)")
}
