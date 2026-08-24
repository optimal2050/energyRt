# =============================================================================
# procspec — YAML process specifications
# =============================================================================
# A *process spec* is a YAML document describing one process — a `technology`,
# a `storage` or a `trade` — as data: ports (inputs/outputs/aux), parameter
# blocks (efficiency, costs, vintages, capacity, ...), and design-time context.
# It is the durable format behind the process designer (`process_designer()`,
# inst/designer/) and a validated replacement for spreadsheet-based process
# definition.
#
# The spec's `class:` field selects the process class. Everything else is
# driven by two per-class registries, so adding a class is data rather than
# code: `.SPEC_SECTIONS` (spec key -> constructor argument + allowed columns)
# and `.SPEC_SCALAR_ARGS` (non-tabular keys -> constructor argument).
#
# Validation policy (the anti-spreadsheet rules):
#   * unknown sections or columns are ERRORS — nothing is silently dropped;
#   * a row without a dimension key applies to ALL members of that dimension
#     (writable explicitly as "ALL"); this is a deliberate, visible choice;
#   * NOTHING is auto-invented: the reader never adds vintages or values;
#   * per-vintage values covering only a subset of the declared vintages are
#     REPORTED as an issue (warning), never silently filled.
#
# API: process_from_spec(), process_to_spec(), process_spec_issues(),
#      process_spec_code() (the equivalent constructor call as text).
#      The former `tech_*` names remain as deprecated aliases.
# =============================================================================

# -- section registries -------------------------------------------------------
# Per class: spec key -> constructor argument + allowed columns. Dimension
# columns get the ALL/absent semantics; sections carrying a `vintage` dim
# participate in the vintage-coverage check.

.SPEC_SECTIONS <- list(

  technology = list(
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
    # `pho2*` / `ret2*` (phase-out and retirement driven auxiliary flows) and
    # the `stg2*` / `sinp2*` / `sout2*` family arrived with the storage
    # refactor; without them a technology using one wrote a spec that would
    # not read back.
    aux_efficiency = list(
      arg = "aeff",
      cols = c("vintage", "cluster", "acomm", "comm", "region", "year",
               "timeslice", "cinp2ainp", "cinp2aout", "cout2ainp", "cout2aout",
               "act2ainp", "act2aout", "cap2ainp", "cap2aout",
               "ncap2ainp", "ncap2aout", "pho2ainp", "pho2aout",
               "ret2ainp", "ret2aout", "stg2ainp", "stg2aout",
               "sinp2ainp", "sinp2aout", "sout2ainp", "sout2aout"),
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
  ),

  # A storage has three PARTS — charger, reservoir, discharger — and the
  # quantitative slots name the part with an `inp.` / `stg.` / `out.` prefix.
  # The three port sections carry the declaration only (comm, unit, cap2act);
  # putting a cost there is an error the constructor reports.
  storage = list(
    inputs = list(
      arg = "input", cols = c("comm", "unit", "cap2act"),
      dims = character()),
    outputs = list(
      arg = "output", cols = c("comm", "unit", "cap2act"),
      dims = character()),
    reservoir = list(
      arg = "storage", cols = c("comm", "unit"),
      dims = character()),
    aux = list(
      arg = "aux", cols = c("acomm", "unit"),
      dims = character()),
    efficiency = list(
      arg = "seff",
      cols = c("vintage", "cluster", "region", "year", "timeslice",
               "stgeff", "inpeff", "outeff"),
      dims = c("vintage", "cluster", "region", "year", "timeslice")),
    aux_efficiency = list(
      arg = "aeff",
      cols = c("vintage", "cluster", "acomm", "region", "year", "timeslice",
               "stg2ainp", "cinp2ainp", "cout2ainp",
               "stg2aout", "cinp2aout", "cout2aout",
               "cap2ainp", "cap2aout", "ncap2ainp", "ncap2aout",
               "pho2ainp", "pho2aout", "ret2ainp", "ret2aout", "ncap2stg"),
      dims = c("vintage", "cluster", "region", "year", "timeslice")),
    availability = list(
      arg = "af",
      cols = c("vintage", "cluster", "region", "year", "timeslice",
               "af.lo", "af.up", "af.fx",
               "cinp.lo", "cinp.up", "cinp.fx",
               "cout.lo", "cout.up", "cout.fx"),
      dims = c("vintage", "cluster", "region", "year", "timeslice")),
    start_level = list(
      arg = "startLevel",
      cols = c("vintage", "cluster", "region", "year", "startLevel"),
      dims = c("vintage", "cluster", "region", "year")),
    duration = list(
      arg = "duration",
      cols = c("vintage", "cluster", "region", "year",
               "duration", "duration.lo", "duration.up", "duration.fx"),
      dims = c("vintage", "cluster", "region", "year")),
    inp2out = list(
      arg = "inp2out",
      cols = c("vintage", "cluster", "region", "year",
               "inp2out", "inp2out.lo", "inp2out.up", "inp2out.fx"),
      dims = c("vintage", "cluster", "region", "year")),
    weather = list(
      arg = "weather",
      cols = c("vintage", "cluster", "weather",
               "waf.lo", "waf.up", "waf.fx",
               "wcinp.lo", "wcinp.up", "wcinp.fx",
               "wcout.lo", "wcout.up", "wcout.fx"),
      dims = c("vintage", "cluster")),
    invcost = list(
      arg = "invcost",
      cols = c("vintage", "cluster", "region", "year",
               "out.invcost", "out.wacc", "out.payback", "out.eac",
               "out.retcost",
               "inp.invcost", "inp.wacc", "inp.payback", "inp.eac",
               "inp.retcost",
               "stg.invcost", "stg.wacc", "stg.payback", "stg.eac",
               "stg.retcost"),
      dims = c("vintage", "cluster", "region", "year")),
    fixom = list(
      arg = "fixom",
      cols = c("vintage", "cluster", "region", "year",
               "out.fixom", "inp.fixom", "stg.fixom"),
      dims = c("vintage", "cluster", "region", "year")),
    varom = list(
      arg = "varom",
      cols = c("vintage", "cluster", "region", "year", "timeslice",
               "inpcost", "outcost", "stgcost"),
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
      cols = c("vintage", "cluster", "region", "year",
               "out.stock", "out.cap.lo", "out.cap.up", "out.cap.fx",
               "out.ncap.lo", "out.ncap.up", "out.ncap.fx",
               "out.ret.lo", "out.ret.up", "out.ret.fx",
               "inp.stock", "inp.cap.lo", "inp.cap.up", "inp.cap.fx",
               "inp.ncap.lo", "inp.ncap.up", "inp.ncap.fx",
               "inp.ret.lo", "inp.ret.up", "inp.ret.fx",
               "stg.stock", "stg.cap.lo", "stg.cap.up", "stg.cap.fx",
               "stg.ncap.lo", "stg.ncap.up", "stg.ncap.fx",
               "stg.ret.lo", "stg.ret.up", "stg.ret.fx"),
      dims = c("vintage", "cluster", "region", "year"))
  ),

  # A trade route is located by its `src`/`dst` pair rather than a region, so
  # its tables are keyed on those. `@capacity` has no region index at all: a
  # route IS a pair of regions.
  trade = list(
    routes = list(
      arg = "routes", cols = c("src", "dst"),
      dims = character()),
    trade = list(
      arg = "trade",
      cols = c("vintage", "cluster", "src", "dst", "year", "timeslice",
               "ava.lo", "ava.up", "ava.fx", "af.lo", "af.up", "af.fx",
               "teff", "reactance", "resistance"),
      dims = c("vintage", "cluster", "src", "dst", "year", "timeslice")),
    aux = list(
      arg = "aux", cols = c("acomm", "unit"),
      dims = character()),
    aux_efficiency = list(
      arg = "aeff",
      cols = c("vintage", "cluster", "acomm", "src", "dst", "year",
               "timeslice", "csrc2aout", "csrc2ainp", "cdst2aout",
               "cdst2ainp"),
      dims = c("vintage", "cluster", "src", "dst", "year", "timeslice")),
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
      cols = c("vintage", "cluster", "src", "dst", "year", "timeslice",
               "varom", "markup"),
      dims = c("vintage", "cluster", "src", "dst", "year", "timeslice")),
    vintages = list(
      arg = "vintage",
      cols = c("vintage", "region", "cluster", "start", "end", "olife"),
      dims = c("region", "cluster")),
    clusters = list(
      arg = "cluster",
      cols = c("cluster", "desc", "share", "order"),
      dims = character()),
    capacity = list(
      arg = "capacity",
      cols = c("vintage", "cluster", "year", "stock",
               "cap.lo", "cap.up", "cap.fx", "ncap.lo", "ncap.up", "ncap.fx",
               "ret.lo", "ret.up", "ret.fx"),
      dims = c("vintage", "cluster", "year"))
  )
)

#' The process classes a spec can describe
#' @noRd
.SPEC_CLASSES <- names(.SPEC_SECTIONS)

#' Constructor per class
#' @noRd
.SPEC_CTOR <- c(technology = "newTechnology",
                storage    = "newStorage",
                trade      = "newTrade")

# Back-compat: the designer and older code read the technology table directly.
#' @noRd
.TECHSPEC_SECTIONS <- .SPEC_SECTIONS$technology

# -- scalar (non-tabular) keys ------------------------------------------------
# Shared across every class. `olife` is the simple lifetime for non-vintaged
# processes; with a `vintages` section, put olife there. `image` is a link
# (URL or path) to an illustration, used in reports. `levcost` holds
# levelized-cost assumptions (list: discount, costs = named commodity prices)
# used to pre-fill the designer's levcost/report forms. `report` names the
# report template. `image`, `report`, `levcost` and `notes` live on @misc.
.SPEC_COMMON_SCALARS <- c("procspec", "techspec", "class", "name", "desc",
                          "olife", "optimizeRetirement",
                          "notes", "image", "levcost", "report", "context")

# Per-class scalar keys -> constructor argument, with the coercion to apply.
.SPEC_SCALAR_ARGS <- list(
  technology = list(units = "asis", cap2act = "num", timeframe = "chr",
                    region = "chr", fullYear = "lgl",
                    optimizeRetirement = "lgl"),
  storage    = list(commodity = "chr", region = "chr", fullYear = "lgl",
                    optimizeRetirement = "lgl"),
  trade      = list(commodity = "chr", cap2act = "num",
                    optimizeRetirement = "lgl")
)

# Back-compat alias for the technology scalar list.
#' @noRd
.TECHSPEC_SCALARS <- c(.SPEC_COMMON_SCALARS,
                       names(.SPEC_SCALAR_ARGS$technology))

#' The class a spec describes, defaulting to technology
#' @noRd
.spec_class <- function(spec) {
  cls <- spec$class %||% "technology"
  as.character(cls)[1]
}

#' Section registry for one class (NULL when the class is unknown)
#' @noRd
.spec_sections <- function(cls) .SPEC_SECTIONS[[cls]]

#' All recognised scalar keys for one class
#' @noRd
.spec_scalars <- function(cls) {
  c(.SPEC_COMMON_SCALARS, names(.SPEC_SCALAR_ARGS[[cls]] %||% list()))
}

#' Read and validate a process spec
#'
#' Parses a spec file (or an already-parsed list) into the normalised list
#' form used by [process_from_spec()] and the process designer. YAML
#' (`.yml`/`.yaml`) and JSON (`.json`) are alternative containers for the
#' same structure.
#'
#' @param spec Path to a `.yml`/`.yaml`/`.json` file, or a list as
#'   returned by [yaml::read_yaml()].
#' @return The normalised spec list (invisibly validated; see
#'   [process_spec_issues()] for the report).
#' @family process spec
#' @export
read_procspec <- function(spec) {
  if (is.character(spec) && length(spec) == 1) {
    if (!file.exists(spec)) stop("process spec file not found: ", spec)
    spec <- if (grepl("[.]json$", spec, ignore.case = TRUE)) {
      .techspec_need_jsonlite("read a JSON process spec")
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

#' @rdname read_procspec
#' @export
read_techspec <- function(spec) {
  .Deprecated("read_procspec")
  read_procspec(spec)
}

#' @noRd
.techspec_need_jsonlite <- function(what) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("the 'jsonlite' package is required to ", what,
         "; install it with install.packages(\"jsonlite\")",
         call. = FALSE)
  }
}

#' Validate a process spec and report issues
#'
#' Applies the spec validation policy: unknown sections/columns are errors;
#' per-vintage values covering only a subset of the declared vintages are
#' reported (never silently filled); nothing is auto-invented.
#'
#' @param spec Path or list (see [read_procspec()]).
#' @return A `data.frame` with columns `severity` (`"error"`/`"warning"`),
#'   `section`, and `message`. Zero rows means a clean spec.
#' @family process spec
#' @examples
#' f <- system.file("techspec", "examples", "coal_power.yml",
#'                  package = "energyRt")
#' if (nzchar(f)) process_spec_issues(f)
#' @export
process_spec_issues <- function(spec) {
  spec <- read_procspec(spec)
  iss <- list()
  add <- function(severity, section, message) {
    iss[[length(iss) + 1L]] <<- data.frame(
      severity = severity, section = section, message = message,
      stringsAsFactors = FALSE)
  }
  done <- function() {
    if (length(iss) == 0) {
      return(data.frame(severity = character(), section = character(),
                        message = character(), stringsAsFactors = FALSE))
    }
    do.call(rbind, iss)
  }

  # -- class -------------------------------------------------------------------
  cls <- .spec_class(spec)
  SECS <- .spec_sections(cls)
  if (is.null(SECS)) {
    add("error", "class",
        paste0("unknown class '", cls, "'; supported: ",
               paste(.SPEC_CLASSES, collapse = ", ")))
    return(done())   # nothing else can be checked without a section registry
  }

  # -- structure ---------------------------------------------------------------
  known <- c(.spec_scalars(cls), names(SECS))
  unknown <- setdiff(names(spec), known)
  if (length(unknown) > 0) {
    add("error", "spec", paste0("unknown section(s) for class '", cls, "': ",
                                paste(unknown, collapse = ", ")))
  }
  if (is.null(spec$name) || !nzchar(as.character(spec$name)[1])) {
    add("error", "name", "`name` is required")
  }

  # -- sections: unknown columns ----------------------------------------------
  for (sec in intersect(names(spec), names(SECS))) {
    rows <- spec[[sec]]
    if (is.null(rows)) next
    if (!is.list(rows)) {
      add("error", sec, "must be a list of rows (mappings)")
      next
    }
    cols <- SECS[[sec]]$cols
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

  # -- ports: what "connected" means differs by class ---------------------------
  if (identical(cls, "technology")) {
    n_inp <- length(spec$inputs %||% list())
    n_out <- length(spec$outputs %||% list())
    if (n_inp == 0) add("error", "inputs", "at least one input is required")
    if (n_out == 0) add("error", "outputs", "at least one output is required")

    port_comms <- c(
      vapply(spec$inputs %||% list(),
             function(r) as.character(r$comm %||% NA), character(1)),
      vapply(spec$outputs %||% list(),
             function(r) as.character(r$comm %||% NA), character(1)))
    port_comms <- port_comms[!is.na(port_comms)]
    eff_comms <- vapply(spec$efficiency %||% list(),
                        function(r) as.character(r$comm %||% NA), character(1))
    bad_eff <- setdiff(eff_comms[!is.na(eff_comms)], port_comms)
    if (length(bad_eff) > 0) {
      add("error", "efficiency",
          paste0("commodity not among declared ports: ",
                 paste(unique(bad_eff), collapse = ", ")))
    }
    if (n_inp > 0 && n_out > 0 &&
        length(spec$efficiency %||% list()) == 0) {
      add("warning", "efficiency",
          "no efficiency rows: inputs are not linked to outputs")
    }

  } else if (identical(cls, "storage")) {
    # `commodity` is the shorthand that fills all three roles at once, so a
    # store is fully declared by it OR by naming the parts individually.
    has_comm <- !is.null(spec$commodity) &&
      nzchar(as.character(spec$commodity)[1])
    parts <- c("inputs", "outputs", "reservoir")
    n_parts <- sum(vapply(parts, function(p) length(spec[[p]] %||% list()),
                          integer(1)))
    if (!has_comm && n_parts == 0) {
      add("error", "commodity",
          paste0("a storage needs `commodity` (the shorthand filling all ",
                 "three parts) or at least one of: ",
                 paste(parts, collapse = ", ")))
    }
    if (!is.null(spec$duration) && !is.null(spec$inp2out) &&
        length(spec$duration) > 0 && length(spec$inp2out) > 0) {
      add("warning", "duration",
          paste0("both `duration` and `inp2out` are given: the three parts ",
                 "are then fully determined by the discharger, which may ",
                 "over-constrain the model"))
    }

  } else if (identical(cls, "trade")) {
    if (is.null(spec$commodity) ||
        !nzchar(as.character(spec$commodity)[1])) {
      add("error", "commodity", "a trade needs `commodity`")
    }
    if (length(spec$routes %||% list()) == 0) {
      add("error", "routes", "at least one route (src, dst) is required")
    }
    for (i in seq_along(spec$routes %||% list())) {
      r <- spec$routes[[i]]
      if (is.null(r$src) || is.null(r$dst)) {
        add("error", "routes",
            paste0("row ", i, ": both `src` and `dst` are required"))
      } else if (identical(as.character(r$src), as.character(r$dst))) {
        add("error", "routes",
            paste0("row ", i, ": `src` and `dst` are the same region (",
                   r$src, ")"))
      }
    }
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
    for (sec in setdiff(names(SECS), c("vintages", "capacity"))) {
      rows <- spec[[sec]]
      if (is.null(rows) || length(rows) == 0) next
      if (!"vintage" %in% SECS[[sec]]$dims) next
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

  done()
}

#' @rdname process_spec_issues
#' @export
tech_spec_issues <- function(spec) {
  .Deprecated("process_spec_issues")
  process_spec_issues(spec)
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
  # Dimension/label columns the slot prototypes want as character. NB
  # `combustion` is NOT one of them: it is a numeric share (0-1) on @input, and
  # listing it here coerced it to "0", so any technology setting it -- the
  # INFR_* refuelling stations, for one -- wrote a valid spec that then failed
  # to read back with "expecting numeric". `src`/`dst` are trade's region
  # columns and belong here for the same reason `region` does.
  chr_cols <- c("vintage", "cluster", "region", "src", "dst", "timeslice",
                "comm", "acomm", "group", "unit", "desc", "weather")
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

#' Coerce one scalar spec value for its constructor argument
#' @noRd
.spec_scalar_value <- function(v, how) {
  switch(how,
         num  = as.numeric(v),
         chr  = as.character(v),
         lgl  = isTRUE(v),
         v)          # "asis"
}

#' Build a process object from a spec
#'
#' Reads, validates, and evaluates the constructor call described by the
#' spec — [newTechnology()], [newStorage()] or [newTrade()] according to the
#' spec's `class`. Errors in the issue report stop the build (warnings are
#' shown).
#'
#' @param spec Path or list (see [read_procspec()]).
#' @param strict Stop on warnings too? Default `FALSE`.
#' @return A `technology`, `storage` or `trade` object.
#' @family process spec
#' @examples
#' f <- system.file("techspec", "examples", "coal_power.yml",
#'                  package = "energyRt")
#' if (nzchar(f)) process_from_spec(f)
#' @export
process_from_spec <- function(spec, strict = FALSE) {
  spec <- read_procspec(spec)
  iss <- process_spec_issues(spec)
  errs <- iss[iss$severity == "error", , drop = FALSE]
  wrns <- iss[iss$severity == "warning", , drop = FALSE]
  if (nrow(errs) > 0 || (strict && nrow(wrns) > 0)) {
    stop("process spec has issues:\n",
         paste0("  [", iss$severity, "] ", iss$section, ": ", iss$message,
                collapse = "\n"))
  }
  for (i in seq_len(nrow(wrns))) {
    warning("process spec [", wrns$section[i], "]: ", wrns$message[i],
            call. = FALSE)
  }

  cls  <- .spec_class(spec)
  SECS <- .spec_sections(cls)

  args <- list(name = as.character(spec$name))
  if (!is.null(spec$desc)) args$desc <- as.character(spec$desc)
  for (k in names(.SPEC_SCALAR_ARGS[[cls]])) {
    if (!is.null(spec[[k]])) {
      args[[k]] <- .spec_scalar_value(spec[[k]], .SPEC_SCALAR_ARGS[[cls]][[k]])
    }
  }
  if (!is.null(spec$olife) && is.null(spec$vintages)) {
    args$olife <- list(olife = as.numeric(spec$olife))
  }
  misc <- list()
  if (!is.null(spec$notes)) misc$notes <- spec$notes
  if (!is.null(spec$image)) misc$image <- as.character(spec$image)
  if (!is.null(spec$report)) misc$report <- as.character(spec$report)
  if (!is.null(spec$levcost)) misc$levcost <- spec$levcost
  if (length(misc) > 0) args$misc <- misc

  for (sec in intersect(names(spec), names(SECS))) {
    df <- .techspec_rows_df(spec[[sec]], SECS[[sec]]$cols)
    if (!is.null(df) && nrow(df) > 0) args[[SECS[[sec]]$arg]] <- df
  }
  do.call(.SPEC_CTOR[[cls]], args)
}

#' @rdname process_from_spec
#' @export
tech_from_spec <- function(spec, strict = FALSE) {
  .Deprecated("process_from_spec")
  process_from_spec(spec, strict = strict)
}

#' Export a process object as a spec list
#'
#' The reverse of [process_from_spec()]: slot tables become row lists
#' (all-`NA` columns and empty tables are omitted; `NA` dimension values are
#' omitted from rows, i.e. read back as "applies to ALL").
#'
#' @param object A `technology`, `storage` or `trade` object.
#' @param file Optional path; when given, the spec is written as YAML,
#'   or as JSON when the path ends in `.json` (same structure, an
#'   alternative container).
#' @return The spec list (invisibly when `file` is given).
#' @family process spec
#' @export
process_to_spec <- function(object, file = NULL) {
  cls <- .SPEC_CLASSES[vapply(.SPEC_CLASSES, function(k) is(object, k),
                              logical(1))]
  if (length(cls) != 1) {
    stop("`object` must be one of: ", paste(.SPEC_CLASSES, collapse = ", "),
         call. = FALSE)
  }
  SECS <- .spec_sections(cls)

  spec <- list(techspec = 1L, class = cls, name = object@name)
  if (nzchar(object@desc %||% "")) spec$desc <- object@desc

  # scalar slots, in the same order the constructor takes them
  for (k in names(.SPEC_SCALAR_ARGS[[cls]])) {
    if (!k %in% slotNames(object)) next
    v <- slot(object, k)
    if (k == "units") {
      if (is.data.frame(v) && nrow(v) > 0) {
        keep <- !vapply(v, function(x) all(is.na(x)), logical(1))
        if (any(keep)) spec$units <- as.list(v[1, keep, drop = FALSE])
      }
      next
    }
    if (is.logical(v)) {
      # only record a flag that departs from its default
      if (k == "fullYear" && isFALSE(v)) spec[[k]] <- FALSE
      if (k == "optimizeRetirement" && isTRUE(v)) spec[[k]] <- TRUE
      next
    }
    if (is.numeric(v) && length(v) == 1 && is.finite(v)) spec[[k]] <- unname(v)
    if (is.character(v) && length(v) > 0 && any(nzchar(v))) {
      spec[[k]] <- as.character(v)
    }
  }

  if (!is.null(object@misc$image))   spec$image   <- as.character(object@misc$image)
  if (!is.null(object@misc$report))  spec$report  <- as.character(object@misc$report)
  if (!is.null(object@misc$levcost)) spec$levcost <- object@misc$levcost
  if (!is.null(object@misc$notes))   spec$notes   <- object@misc$notes

  for (sec in names(SECS)) {
    arg <- SECS[[sec]]$arg
    if (!arg %in% slotNames(object)) next
    df <- slot(object, arg)
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
      .techspec_need_jsonlite("write a JSON process spec")
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

#' @rdname process_to_spec
#' @export
tech_to_spec <- function(object, file = NULL) {
  .Deprecated("process_to_spec")
  process_to_spec(object, file)
}

#' The constructor call equivalent to a process spec, as text
#'
#' Used by the designer's export tab.
#'
#' @param spec Path or list (see [read_procspec()]).
#' @return A single character string of R code.
#' @family process spec
#' @export
process_spec_code <- function(spec) {
  spec <- read_procspec(spec)
  cls  <- .spec_class(spec)
  SECS <- .spec_sections(cls)
  if (is.null(SECS)) {
    stop("unknown class '", cls, "'; supported: ",
         paste(.SPEC_CLASSES, collapse = ", "), call. = FALSE)
  }
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
  for (k in names(.SPEC_SCALAR_ARGS[[cls]])) {
    v <- spec[[k]]
    if (is.null(v)) next
    how <- .SPEC_SCALAR_ARGS[[cls]][[k]]
    parts <- c(parts, switch(
      how,
      num  = sprintf("  %s = %s", k, format(as.numeric(v))),
      lgl  = sprintf("  %s = %s", k, isTRUE(v)),
      chr  = paste0("  ", k, " = ",
                    paste(deparse(as.character(v)), collapse = "")),
      asis = paste0("  ", k, " = list(",
                    paste(sprintf('%s = "%s"', names(v), unlist(v)),
                          collapse = ", "), ")")))
  }
  if (!is.null(spec$olife) && is.null(spec$vintages)) {
    parts <- c(parts, sprintf("  olife = list(olife = %s)",
                              format(as.numeric(spec$olife))))
  }
  # misc-bound scalars (image, report template, notes, levcost assumptions)
  misc <- list()
  if (!is.null(spec$notes))   misc$notes   <- spec$notes
  if (!is.null(spec$image))   misc$image   <- as.character(spec$image)
  if (!is.null(spec$report))  misc$report  <- as.character(spec$report)
  if (!is.null(spec$levcost)) misc$levcost <- spec$levcost
  if (length(misc) > 0) {
    ms <- paste(deparse(misc, width.cutoff = 60), collapse = "\n    ")
    parts <- c(parts, paste0("  misc = ", ms))
  }
  for (sec in intersect(names(spec), names(SECS))) {
    df <- .techspec_rows_df(spec[[sec]], SECS[[sec]]$cols)
    if (is.null(df) || nrow(df) == 0) next
    parts <- c(parts, paste0("  ", SECS[[sec]]$arg, " = ", fmt_df(df)))
  }
  paste0(.SPEC_CTOR[[cls]], "(\n", paste(parts, collapse = ",\n"), "\n)")
}

#' @rdname process_spec_code
#' @export
tech_spec_code <- function(spec) {
  .Deprecated("process_spec_code")
  process_spec_code(spec)
}
