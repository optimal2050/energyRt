# =============================================================================
# Building a multimod model from an energyRt scenario
# =============================================================================
#
# This file was moved here from multimod on 2026-09-24. It used to live in
# multimod as R/energyrt_pipeline.R, R/import_energyrt.R, R/energyrt_unfold.R,
# R/energyrt_index_aliases.R and the energyRt half of R/data_management.R.
#
# It belongs here, not there: multimod is a general engine for algebraic models
# and energyRt is one consumer of it. Having the consumer-specific code live in
# the engine meant multimod carried `Suggests: energyRt` and reached into
# energyRt's private surface (`.modelCode`, `.mapping_spec`, `.modInp`) - a
# general package depending on one particular model's internals.
#
# -----------------------------------------------------------------------------
# The two halves of a multimod model
# -----------------------------------------------------------------------------
#   structure  energyRt's own GAMS model code, shipped as `.modelCode$GAMS`.
#              The `gams/` directory is .Rbuildignore'd, so reading the shipped
#              character vector is the only route that works from an installed
#              package - and it pins the AST to this energyRt's version.
#   data       the interpolated scenario: sets, maps and parameters off
#              `@modInp`, plus any newConstraint()/newCosts() compiled to the
#              GAMS string IR on `@modInp@user_constraints`.
#
# multimod is a Suggests: every entry point checks for it.
# =============================================================================

#' Fail with an actionable message when multimod is absent
#' @keywords internal
#' @noRd
.need_multimod <- function(what) {
  if (!requireNamespace("multimod", quietly = TRUE)) {
    stop(what, " needs the 'multimod' package.
",
         '  pak::pak("optimal2050/multimod")', call. = FALSE)
  }
  invisible(TRUE)
}

# --- index aliases -----------------------------------------------------------

#' Short index names for the energyRt model
#'
#' Maps each energyRt set (and set alias) to a short iterator name used when
#' rendering equations, e.g. `tech` -> `h`, `timeslice` -> `t`. Consumed by
#' [add_index_aliases()] when building the bundled `example_models` fixture.
#'
#' Names must match the sets and aliases declared in energyRt's GAMS model.
#' As of energyRt 0.89.5 those are: comm, region, year, timeslice, sup, dem,
#' tech, stg, trade, expp, imp, group, weather; plus the alias families
#' tech/techp, region/regionp/src/dst/region2, year/yearp/yeare/yearn/year2,
#' timeslice/timeslicep/timeslicepp/timeslice2, group/groupp,
#' comm/commp/acomm/comme, sup/supp.
#'
#' @keywords internal
#' @noRd
index_aliases_energyRt <- c(
  # base sets
  comm        = "c",   # commodity
  region      = "r",   # region
  year        = "y",   # year
  timeslice   = "t",   # time slice
  sup         = "u",   # supply
  dem         = "d",   # demand
  tech        = "h",   # technology
  stg         = "s",   # storage
  trade       = "z",   # interregional trade
  expp        = "x",   # export to RoW
  imp         = "m",   # import from RoW
  group       = "g",   # group of related commodities or tags
  weather     = "w",   # weather
  # set aliases
  techp       = "hp",
  regionp     = "rp",
  region2     = "r2",
  src         = "rs",
  dst         = "rd",
  yearp       = "yp",
  yeare       = "ye",
  yearn       = "yn",
  year2       = "y2",
  timeslicep  = "tp",
  timeslicepp = "tpp",
  timeslice2  = "t2",
  groupp      = "gp",
  commp       = "cp",
  acomm       = "ca",
  comme       = "ce",
  supp        = "up"
)


# --- reading energyRt's GAMS model code --------------------------------------

#' Extract energyRt domain mappings from GAMS file comments
#'
#' Reads *@ domain mapping hints from the GAMS source file and applies them
#' to variables in a model_structure object. This enables sparse indexing
#' in Julia/JuMP export.
#'
#' @param model A model_structure object (from read_gams())
#' @param verbose Print progress messages
#' @return Modified model_structure with domain field populated for variables
#' @keywords internal
#' @noRd
#'
#' @examples
#' \dontrun{
#' model <- read_gams("energyRt.gms")
#' model <- extract_domains_from_comments(model)
#' }
extract_domains_from_comments <- function(model, verbose = FALSE) {
  if (!inherits(model, "model_structure")) {
    stop("model must be a model_structure object from multimod::read_gams()")
  }

  # `source` is whatever read_gams() was handed: a file path, or the GAMS
  # source itself as a character vector - which is how energyRt's shipped
  # `.modelCode$GAMS` arrives. `extract_gams_domain_hints()` below already
  # accepts either, so the only thing this guard has to do is not call
  # file.exists() on a 2,954-element vector.
  src <- model$source
  is_path <- length(src) == 1L && !is.na(src) && nzchar(src) && file.exists(src)
  is_text <- is.character(src) && length(src) > 1L
  if (!is_path && !is_text) {
    warning("Cannot extract domain hints: no readable GAMS source")
    return(model)
  }

  if (verbose) {
    cat("Extracting domain hints from:",
        if (is_path) src else sprintf("<%d lines of GAMS source>", length(src)),
        "\n")
  }

  # Extract hints from source file
  domain_hints <- extract_gams_domain_hints(model$source)

  if (verbose) {
    cat(sprintf("Found %d domain hints\n", length(domain_hints)))
  }

  # Apply hints to model
  model <- apply_domain_hints(model, domain_hints, verbose = verbose)

  return(model)
}


#' Extract domain mapping hints from GAMS file comments
#'
#' Reads a GAMS file and extracts *@ domain mapping comments that appear
#' before variable declarations. Does NOT modify the model structure.
#'
#' @param file_or_text Path to GAMS file or text content
#' @return Named list mapping variable names to domain mapping comments
#' @keywords internal
extract_gams_domain_hints <- function(file_or_text) {
  # Read the file
  if (length(file_or_text) == 1 && file.exists(file_or_text)) {
    lines <- readLines(file_or_text, warn = FALSE)
  } else {
    lines <- file_or_text
  }

  # Remove $ontext/$offtext blocks
  lines <- multimod::remove_ontext_offtext(lines)

  domain_hints <- list()
  pending_hint <- NULL
  in_variables <- FALSE

  for (i in seq_along(lines)) {
    line <- trimws(lines[i])

    # Skip blank lines
    if (line == "") next

    # Detect variable declaration block
    if (grepl("^(positive\\s+)?variable(s)?\\b", line, ignore.case = TRUE)) {
      in_variables <- TRUE
      next
    }

    # End of variable block
    if (in_variables && grepl("^;", line)) {
      in_variables <- FALSE
      pending_hint <- NULL
      next
    }

    # In variable block: capture *@ comments
    if (in_variables && grepl("^\\*@", line)) {
      pending_hint <- line
      next
    }

    # In variable block: parse variable declaration
    if (in_variables && grepl("^[a-zA-Z]", line)) {
      # Extract variable name
      var_match <- regexec("^([a-zA-Z0-9_]+)\\s*\\(", line)
      var_result <- regmatches(line, var_match)[[1]]

      if (length(var_result) >= 2) {
        var_name <- var_result[2]

        # Store the hint if we have one
        if (!is.null(pending_hint)) {
          domain_hints[[var_name]] <- pending_hint
        }
      }

      # Clear the pending hint after processing
      pending_hint <- NULL
    }
  }

  return(domain_hints)
}


#' Apply domain hints to a model_structure object
#'
#' Takes domain hints extracted from GAMS comments and applies them
#' to variables in a model_structure object.
#'
#' @param model A model_structure object from read_gams()
#' @param domain_hints Named list from extract_gams_domain_hints()
#' @param verbose Print progress messages
#' @return Modified model_structure with domain field populated
#' @keywords internal
apply_domain_hints <- function(model, domain_hints, verbose = FALSE) {
  if (!inherits(model, "model_structure")) {
    stop("model must be a model_structure object from multimod::read_gams()")
  }

  vars_updated <- 0

  for (var_name in names(model$variables)) {
    if (var_name %in% names(domain_hints)) {
      hint <- domain_hints[[var_name]]

      # Parse the hint using existing function
      parsed <- parse_domain_hint(hint, model$variables[[var_name]]$dims)

      if (!is.null(parsed)) {
        model$variables[[var_name]]$domain <- parsed
        model$variables[[var_name]]$comment <- hint
        vars_updated <- vars_updated + 1

        if (verbose) {
          cat(sprintf("  %s -> %s\n", var_name, parsed))
        }
      }
    }
  }

  if (verbose) {
    cat(sprintf("\nApplied domain hints to %d / %d variables\n",
                vars_updated, length(model$variables)))
  }

  return(model)
}


#' Parse a single *@ domain hint comment
#'
#' @param hint Comment line like "*@ mTechNew(tech,region,year)"
#' @param dims Variable dimensions from declaration
#' @return Domain mapping name or NULL
#' @keywords internal
parse_domain_hint <- function(hint, dims) {
  # Remove *@ prefix and whitespace
  hint <- sub("^\\*@\\s*", "", hint)
  hint <- trimws(hint)

  # Empty hint means unused variable
  if (hint == "") {
    return(character(0))
  }

  # Extract mapping name
  if (grepl("^([a-zA-Z0-9_]+)\\s*\\(", hint)) {
    mapping_match <- regexec("^([a-zA-Z0-9_]+)\\s*\\(([^)]*)\\)", hint)
    result <- regmatches(hint, mapping_match)[[1]]

    if (length(result) >= 3) {
      mapping_name <- result[2]
      hint_dims <- trimws(unlist(strsplit(result[3], ",")))

      # Validate dimensions match
      if (length(dims) != length(hint_dims)) {
        warning(sprintf("Dimension mismatch for mapping %s: expected %d, got %d",
                        mapping_name, length(dims), length(hint_dims)))
      }

      return(mapping_name)
    }
  }

  return(NULL)
}

#' Get default values from energyRt modInp
#'
#' @return Named list of parameter default values
#' @keywords internal
#' @noRd
get_defvals <- function() {
  # Access internal .modInp object from energyRt package
  modinp <- .modInp

  # Extract defVal for each parameter
  defvals <- list()
  for (param_name in names(modinp)) {
    param <- modinp[[param_name]]
    if (!is.null(param$defVal)) {
      defvals[[param_name]] <- param$defVal
    }
  }

  defvals
}

#' Update model parameters with default values from energyRt
#'
#' @param model multimod model object
#' @return Updated model with defVal populated
#' @keywords internal
#' @noRd
populate_defvals <- function(model) {
  defvals <- get_defvals()
  defvals <- defvals[grepl("^p", names(defvals))] # parameters only

  if (length(defvals) == 0) {
    warning("No default values found in modInp")
    return(model)
  }

  # Update parameters
  if (!is.null(model$parameters) && length(model$parameters) > 0) {
    updated_count <- 0

    for (param_name in names(defvals)) {
      dv <- defvals[[param_name]]

      # Handle parameters with two values (lower and upper bounds)
      if (length(dv) == 2) {
        # Assign first value to *Lo parameter
        lo_name <- paste0(param_name, "Lo")
        if (lo_name %in% names(model$parameters)) {
          model$parameters[[lo_name]]$defVal <- dv[1]
          updated_count <- updated_count + 1
        }

        # Assign second value to *Up parameter
        up_name <- paste0(param_name, "Up")
        if (up_name %in% names(model$parameters)) {
          model$parameters[[up_name]]$defVal <- dv[2]
          updated_count <- updated_count + 1
        }
      } else {
        # Single value - assign directly if parameter exists
        if (param_name %in% names(model$parameters)) {
          model$parameters[[param_name]]$defVal <- dv
          updated_count <- updated_count + 1
        }
      }
    }

    message("Populated default values for ", updated_count,
            " parameters from energyRt modInp")
  }

  model
}

#' Convert energyRt parameter to multimod format
#'
#' @param ert_param An energyRt parameter object
#' @param orig_param Original multimod parameter structure (optional)
#' @param inMemory Logical. Embed data in-memory?
#' @return Updated parameter with data reference
#' @keywords internal
convert_parameter <- function(ert_param, orig_param = NULL, inMemory = FALSE,
                                       scenario = NULL) {
  # Start with original parameter structure if available
  result <- if (!is.null(orig_param)) {
    orig_param
  } else {
    list(
      name = ert_param@name,
      dims = ert_param@dimSets
    )
  }

  # Add default value if available
  if (!is.null(ert_param@defVal)) {
    result$defVal <- ert_param@defVal
  }

  param_misc <- ert_param@misc
  source_path <- if (!is.null(param_misc$path)) param_misc$path else NULL
  source_on_disk <- if (!is.null(param_misc$onDisk)) param_misc$onDisk else NULL
  source_in_memory <- if (!is.null(param_misc$inMemory)) param_misc$inMemory else FALSE
  result$data <- data.frame()
  result$misc <- list(
    inMemory = source_in_memory,
    path = source_path,
    onDisk = source_on_disk
  )

  if (isTRUE(inMemory)) {
    loaded <- collect_scenario_parameter_data(ert_param, scenario = scenario)
    result$data <- loaded
    result$misc$inMemory <- TRUE
    result$misc$path <- NULL
  }

  # Preserve class if original had one
  if (!is.null(orig_param) && !is.null(class(orig_param))) {
    class(result) <- class(orig_param)
  }

  result
}



#' Dimensions recorded in an on-disk summary, or NULL
#'
#' energyRt keys the record by slot name (`onDisk$data$dim`); a flat record
#' (`onDisk$dim`) also occurs. Accept either.
#'
#' multimod carries its own copy in R/model_storage.R for the same record: it
#' is five lines describing a shape both packages read, and exporting a
#' dot-prefixed internal across the package boundary would be worse than the
#' duplication. If the on-disk record shape changes, BOTH must change.
#' @keywords internal
#' @noRd
.ondisk_dim <- function(rec) {
  if (is.null(rec)) return(NULL)
  if (!is.null(rec$dim)) return(rec$dim)
  rec[["data"]]$dim
}

# --- folded (wildcard) parameters --------------------------------------------

#' Index columns of a parameter's data table
#' @keywords internal
#' @noRd
.index_cols <- function(d) setdiff(names(d), "value")

#' Does this table carry a wildcard in an index column?
#'
#' `NA` is the wildcard energyRt writes when folding. (`ANY*` tokens are a
#' property of the *written* model files, not of `modInp`, so they are not
#' expected here.)
#' @keywords internal
#' @noRd
.has_wildcard <- function(d) {
  if (is.null(d) || !is.data.frame(d) || !nrow(d)) return(FALSE)
  cols <- .index_cols(d)
  if (!length(cols)) return(FALSE)
  for (k in cols) if (anyNA(d[[k]])) return(TRUE)
  FALSE
}

#' Is this scenario folded?
#'
#' True when any parameter records a fold. Note this is necessary but not
#' sufficient for detecting wildcards: on the R7 kit `fold = TRUE` leaves
#' wildcards in 22 parameters while only 11 carry `fold_info`, because
#' `fold = FALSE` also densifies wildcards that came from the source data and
#' `fold = TRUE` skips that step. Use it to decide *whether to look*, never as
#' the per-parameter test.
#' @keywords internal
#' @noRd
.scenario_is_folded <- function(scenario) {
  params <- scenario@modInp@parameters
  for (p in params) {
    if (!isS4(p) || !methods::is(p, "parameter")) next
    fi <- p@misc$fold_info
    if (is.null(fi) || !isTRUE(fi$folded)) next
    # Only a WILDCARD fold needs expanding: the dimension column is still
    # there with NA in the folded rows. A "drop" fold has removed the column
    # outright, so there is nothing to materialise. energyRt records the
    # encoding in fold_info$mode; a missing mode predates the field, and
    # every energyRt that wrote one wrote a wildcard, so treat it as such
    # rather than silently skipping an expansion that is needed.
    if (identical(fi$mode, "drop")) next
    return(TRUE)
  }
  FALSE
}

#' Materialise a folded parameter's wildcard rows
#'
#' Delegates to energyRt's read-time helper, which builds the per-entity
#' membership maps this parameter needs and expands each wildcard row to one
#' row per member. Returns `data` unchanged when there is nothing to expand or
#' when the helper is unavailable.
#'
#' @param data The parameter's data, as loaded.
#' @param ert_param The energyRt parameter object.
#' @param scenario The energyRt scenario (needed for the membership maps).
#' @keywords internal
#' @noRd
.unfold_param_data <- function(data, ert_param, scenario) {
  if (is.null(scenario) || !.has_wildcard(data)) return(data)
  unfold1 <- tryCatch(
    unfold_scenario_parameter,
    error = function(e) NULL
  )
  if (is.null(unfold1)) {
    stop("Parameter '", ert_param@name, "' carries wildcard (NA) index values, ",
         "which means the scenario was interpolated with fold = TRUE, but ",
         "energyRt::unfold_scenario_parameter() is not available to expand ",
         "them.\n  Joining on an NA key silently drops the row and substitutes ",
         "the default, so continuing would produce a wrong model rather than ",
         "an error.\n  Re-interpolate with fold = FALSE, or install an ",
         "energyRt that provides the helper.", call. = FALSE)
  }
  out <- tryCatch(unfold1(scenario, ert_param), error = function(e) {
    stop("Could not unfold folded parameter '", ert_param@name, "': ",
         conditionMessage(e), call. = FALSE)
  })
  out <- as.data.frame(out)
  if (.has_wildcard(out)) {
    left <- .index_cols(out)[vapply(.index_cols(out),
                                    function(k) anyNA(out[[k]]), logical(1))]
    warning("Parameter '", ert_param@name, "' still carries wildcard values in ",
            paste(left, collapse = ", "), " after unfolding.", call. = FALSE)
  }
  out
}


# --- scenario data -> multimod model -----------------------------------------

#' Rows an energyRt parameter holds, attached or detached
#'
#' A detached (on-disk) parameter keeps its row count in the `misc$onDisk`
#' summary; its `@data` slot is empty by design.
#' @keywords internal
.ert_n_rows <- function(ert_param) {
  if (!is.null(ert_param@data) && nrow(ert_param@data) > 0) {
    return(nrow(ert_param@data))
  }
  d <- .ondisk_dim(ert_param@misc$onDisk)
  if (!is.null(d)) return(as.integer(d[1]))
  0L
}

#' Collect scenario parameter data into memory
#'
#' @param ert_param energyRt parameter object
#' @return data.frame with parameter values (may be empty)
#'
#' @keywords internal
collect_scenario_parameter_data <- function(ert_param, scenario = NULL) {
  if (!is.null(ert_param@data) && nrow(ert_param@data) > 0) {
    return(.unfold_param_data(as.data.frame(ert_param@data), ert_param, scenario))
  }

  param_path <- ert_param@misc$path
  if (!is.null(param_path) && (dir.exists(param_path) || file.exists(param_path))) {
    data <- tryCatch(
      multimod::load_arrow_data(param_path, collect = TRUE),
      error = function(e) {
        warning("Failed to load data for ", ert_param@name, ": ", conditionMessage(e))
        NULL
      }
    )
    if (!is.null(data) && nrow(data) > 0) {
      return(.unfold_param_data(as.data.frame(data), ert_param, scenario))
    }
  }

  data.frame()
}

#' Create log entry for parameter/mapping link
#'
#' @keywords internal
create_link_log_entry <- function(multimod_name, scenario_name, type, status,
                                   ert_param, inMemory) {
  list(
    multimod_name = multimod_name,
    scenario_name = scenario_name,
    type = type,
    status = status,
    dims = paste(ert_param@dimSets, collapse = ", "),
    n_dims = length(ert_param@dimSets),
    defVal = if (!is.null(ert_param@defVal)) paste(ert_param@defVal, collapse = ", ") else NA,
    has_data = .ert_n_rows(ert_param) > 0,
    # Row count as stored, not as attached. An on-disk parameter's @data slot is
    # detached and always reports 0, which made the import summary announce
    # "0 rows, all empty" for a scenario whose data was entirely readable - and
    # so made the real version of that failure impossible to notice.
    n_rows = .ert_n_rows(ert_param),
    path = if (!is.null(ert_param@misc$path)) ert_param@misc$path else NA,
    # Whether the data can be reached at all - in memory now, or on disk via
    # the per-object reference that get_lazy_data() resolves. An on-disk
    # scenario legitimately reports n_rows = 0 here while being perfectly
    # readable, so row counts alone cannot tell a lazy link from a lost one.
    resolvable = (!is.null(ert_param@data) && nrow(ert_param@data) > 0) ||
      !is.null(ert_param@misc$path) || isTRUE(ert_param@misc$onDisk),
    inMemory = inMemory
  )
}

#' Import energyRt scenario data into a multimod model
#'
#' Combines set population, parameter linking, and special handling for
#' bounds-style parameters (stored as single objects in energyRt but split
#' into *Lo/*Up entries inside multimod models). Creates a detailed import
#' log stored in model$misc$data_import_log.
#'
#' @param model Multimod model
#' @param scenario energyRt scenario object
#' @param inMemory Logical. Load parameter data into memory?
#' @param log_file Optional path to export import log as CSV
#' @return Modified model with scenario data linked and import log
#' @export
multimod_import_data <- function(model, scenario, inMemory = scenario@inMemory, log_file = NULL) {
  stopifnot(inherits(model, "multimod") || inherits(model, "model"))
  if (!inherits(scenario, "scenario")) {
    stop("scenario must be an energyRt scenario object")
  }

  if (is.null(inMemory)) inMemory <- FALSE

  # A folded scenario must be unfolded as it is read, and unfolding needs the
  # data in hand. The lazy path loads straight from the parameter store later,
  # with no scenario to resolve membership against, so it would read the
  # wildcard rows back and silently drop them on the join.
  if (!isTRUE(inMemory) && .scenario_is_folded(scenario)) {
    stop("This scenario was interpolated with fold = TRUE, so its parameters ",
         "carry wildcard (NA) index values that must be expanded on read.\n",
         "  Pass inMemory = TRUE to multimod_import_data(); the lazy path ",
         "cannot expand them and would build a model that solves to a wrong ",
         "answer without erroring.", call. = FALSE)
  }

  cat("Importing energyRt scenario data...\n")
  
  # Initialize import log
  import_log <- list()
  
  # Step 1: Populate sets and track sources
  set_log <- populate_sets_with_log(model, scenario)
  import_log$sets <- set_log$log
  model <- set_log$model
  
  # Step 1b: Declare this scenario's user-constraint support symbols
  # (mCns*/pCns*/mCosts*/pCosts*). They are per-scenario, so energyRt.gms does
  # not declare them, and step 2 only links symbols the model already has.
  model <- declare_user_constraint_symbols(model, scenario)

  # Step 2: Link regular parameters and mappings
  link_result <- link_scenario_data_with_log(model, scenario, inMemory = inMemory)
  import_log$parameters <- link_result$param_log
  import_log$mappings <- link_result$mapping_log
  model <- link_result$model

  # Step 2b: Parse the compiled user constraints and user costs into equations.
  model <- add_user_constraints(model, scenario)

  # Step 3: Handle bounds parameters
  scenario_params <- scenario@modInp@parameters
  bounds_names <- names(Filter(function(p) {
    identical(as.character(p@type), "bounds")
  }, scenario_params))

  if (length(bounds_names) > 0) {
    cat("  Processing", length(bounds_names), "bounds parameters...\n")
    bounds_result <- process_bounds_with_log(model, scenario_params, bounds_names,
                                            scenario = scenario)
    import_log$bounds <- bounds_result$log
    model <- bounds_result$model
    
    # Report bounds processing
    cat("    Bound datasets attached:", bounds_result$attached_count, "\n")
    if (length(bounds_result$missing_targets) > 0) {
      cat("    Missing target parameters for bounds:\n")
      for (nm in names(bounds_result$missing_targets)) {
        cat("      -", nm, "->", paste(bounds_result$missing_targets[[nm]], collapse = ", "), "\n")
      }
    }
  }
  
  # Step 4: Identify unmatched elements
  import_log$unmatched <- find_unmatched_elements(model, scenario, import_log)
  
  # Store log in model
  if (is.null(model$misc)) model$misc <- list()
  model$misc$data_import_log <- import_log
  model$misc$data_import_timestamp <- Sys.time()
  
  # Export to CSV if requested
  if (!is.null(log_file)) {
    multimod::export_import_log(import_log, log_file)
    cat("  Import log exported to:", log_file, "\n")
  }
  
  # Print summary
  print_import_summary(import_log)

  model
}

#' Link scenario data with logging
#'
#' @param model Multimod model
#' @param scenario energyRt scenario object
#' @param inMemory Logical
#' @return List with model and logs
#'
#' @keywords internal
link_scenario_data_with_log <- function(model, scenario, inMemory = FALSE) {
  param_names <- names(scenario@modInp@parameters)
  
  param_log <- list()
  mapping_log <- list()
  
  for (pname in param_names) {
    ert_param <- scenario@modInp@parameters[[pname]]
    param_type <- as.character(ert_param@type)
    
    # Skip bounds - they'll be handled separately
    if (param_type == "bounds") {
      next
    }
    
    if (param_type == "map") {
      # Mapping
      if (pname %in% names(model$mappings)) {
        model$mappings[[pname]] <- convert_parameter(
          ert_param,
          model$mappings[[pname]],
          inMemory = inMemory,
          scenario = scenario
        )
        
        mapping_log[[pname]] <- create_link_log_entry(
          multimod_name = pname,
          scenario_name = pname,
          type = "mapping",
          status = "linked",
          ert_param = ert_param,
          inMemory = inMemory
        )
      } else {
        mapping_log[[pname]] <- create_link_log_entry(
          multimod_name = NA,
          scenario_name = pname,
          type = "mapping",
          status = "unmatched_in_multimod",
          ert_param = ert_param,
          inMemory = inMemory
        )
      }
    } else if (param_type == "numpar") {
      # Numeric parameter
      if (pname %in% names(model$parameters)) {
        model$parameters[[pname]] <- convert_parameter(
          ert_param,
          model$parameters[[pname]],
          inMemory = inMemory,
          scenario = scenario
        )
        
        param_log[[pname]] <- create_link_log_entry(
          multimod_name = pname,
          scenario_name = pname,
          type = "parameter",
          status = "linked",
          ert_param = ert_param,
          inMemory = inMemory
        )
      } else {
        param_log[[pname]] <- create_link_log_entry(
          multimod_name = NA,
          scenario_name = pname,
          type = "parameter",
          status = "unmatched_in_multimod",
          ert_param = ert_param,
          inMemory = inMemory
        )
      }
    }
  }
  
  cat("  Mappings linked: ", sum(sapply(mapping_log, function(x) x$status == "linked")), "\n")
  cat("  Parameters linked:", sum(sapply(param_log, function(x) x$status == "linked")), "\n")
  
  list(
    model = model,
    param_log = param_log,
    mapping_log = mapping_log
  )
}

# `populate_sets_from_scenario()` was removed here on 2026-09-24: it was
# superseded by populate_sets_with_log() (which multimod_import_data()
# actually calls) and had no caller left - the same fate as the
# link_scenario_data() / link_scenario_data_with_log() pair.


#' Populate sets and track data sources
#'
#' @param model Multimod model
#' @param scenario energyRt scenario object
#' @return List with model and log
#'
#' @keywords internal
populate_sets_with_log <- function(model, scenario) {
  cat("Populating sets from energyRt scenario...\n")
  
  # Initialize set data storage and log
  set_members <- list()
  set_sources <- list()  # Track which parameters contributed to each set
  
  for (set_name in names(model$sets)) {
    set_members[[set_name]] <- character()
    set_sources[[set_name]] <- character()
  }
  
  # Scan through scenario parameters
  param_names <- names(scenario@modInp@parameters)
  cat("  Scanning", length(param_names), "parameters...\n")
  
  for (pname in param_names) {
    ert_param <- scenario@modInp@parameters[[pname]]
    
    # Get dimension names
    dims <- ert_param@dimSets
    if (length(dims) == 0) next
    
    # Load data if available
    path <- ert_param@misc$path
    data <- NULL
    
    if (!is.null(path) && (dir.exists(path) || file.exists(path))) {
      data <- tryCatch(
        multimod::load_arrow_data(path, collect = TRUE),
        error = function(e) {
          cat("  Warning: could not load", pname, "-", conditionMessage(e), "\n")
          NULL
        }
      )
    } else if (!is.null(ert_param@data) && nrow(ert_param@data) > 0) {
      data <- ert_param@data
    }
    
    # Extract members for each dimension
    if (!is.null(data) && nrow(data) > 0) {
      for (i in seq_along(dims)) {
        dim_name <- dims[i]
        if (dim_name %in% names(set_members) && i <= ncol(data)) {
          members <- as.character(data[[i]])
          set_members[[dim_name]] <- c(set_members[[dim_name]], members)
          set_sources[[dim_name]] <- c(set_sources[[dim_name]], pname)
        }
      }
    }
  }
  
  # Update model sets with unique members and create log
  n_populated <- 0
  log_entries <- list()
  
  for (set_name in names(set_members)) {
    members <- unique(set_members[[set_name]])
    sources <- unique(set_sources[[set_name]])
    
    if (length(members) > 0) {
      model$sets[[set_name]]$data <- sort(members)
      n_populated <- n_populated + 1
      cat("  ", set_name, ":", length(members), "members\n")
    }
    
    log_entries[[set_name]] <- list(
      multimod_name = set_name,
      type = "set",
      status = if (length(members) > 0) "populated" else "empty",
      n_members = length(members),
      source_parameters = if (length(sources) > 0) sources else NA,
      n_sources = length(sources)
    )
  }
  
  cat("Populated", n_populated, "of", length(model$sets), "sets\n")
  
  list(
    model = model,
    log = log_entries
  )
}

#' Process bounds parameters with logging
#'
#' @keywords internal
process_bounds_with_log <- function(model, scenario_params, bounds_names,
                                    scenario = NULL) {
  log_entries <- list()
  attached_total <- 0
  missing_targets <- list()
  
  for (pname in bounds_names) {
    ert_param <- scenario_params[[pname]]
    result <- apply_bounds_to_model(model, ert_param, pname, scenario = scenario)
    model <- result$model
    
    # Create log entry for this bounds parameter
    lo_name <- paste0(pname, "Lo")
    up_name <- paste0(pname, "Up")
    
    log_entries[[pname]] <- list(
      scenario_name = pname,
      type = "bounds",
      multimod_lo = lo_name,
      multimod_up = up_name,
      status_lo = if (lo_name %in% result$attached) "split_linked" else "unmatched_in_multimod",
      status_up = if (up_name %in% result$attached) "split_linked" else "unmatched_in_multimod",
      dims = paste(ert_param@dimSets, collapse = ", "),
      n_dims = length(ert_param@dimSets),
      defVal_lo = if (length(ert_param@defVal) >= 1) ert_param@defVal[1] else NA,
      defVal_up = if (length(ert_param@defVal) >= 2) ert_param@defVal[2] else ert_param@defVal[1],
      path = if (!is.null(ert_param@misc$path)) ert_param@misc$path else NA,
      issues = if (length(result$issues) > 0) paste(result$issues, collapse = "; ") else NA
    )
    
    attached_total <- attached_total + length(result$attached)
    if (length(result$missing) > 0) {
      missing_targets[[pname]] <- result$missing
    }
  }
  
  list(
    model = model,
    log = log_entries,
    attached_count = attached_total,
    missing_targets = missing_targets
  )
}

#' Split bounds-style parameter into low/high tables
#'
#' @param ert_param energyRt parameter object of type "bounds"
#' @return List with lo/up data.frames (each containing dims + value)
#'
#' @keywords internal
split_bounds_parameter_data <- function(ert_param, scenario = NULL) {
  # Bounds take their own import path, so the unfold has to be threaded here
  # too - a folded pTechAf reaches the matrix as pTechAfLo / pTechAfUp.
  data <- collect_scenario_parameter_data(ert_param, scenario = scenario)
  if (nrow(data) == 0) {
    return(NULL)
  }

  if (!"type" %in% names(data)) {
    warning("Bounds parameter ", ert_param@name, " lacks a 'type' column")
    return(NULL)
  }

  dims <- as.character(ert_param@dimSets)
  available_dims <- intersect(dims, names(data))
  if (length(available_dims) == 0) {
    warning("Bounds parameter ", ert_param@name, " has no matching dimension columns")
    return(NULL)
  }

  value_col <- if ("value" %in% names(data)) "value" else tail(names(data), 1)
  dtype <- tolower(as.character(data$type))

  select_subset <- function(indices) {
    if (!any(indices)) {
      return(data.frame())
    }
    subset <- data[indices, c(available_dims, value_col), drop = FALSE]
    colnames(subset) <- c(available_dims, "value")
    subset$value <- suppressWarnings(as.numeric(subset$value))
    subset
  }

  list(
    lo = select_subset(dtype %in% c("lo", "lower", "min")),
    up = select_subset(dtype %in% c("up", "upper", "max")),
    dims = available_dims
  )
}

#' Attach bounds data to model parameters
#'
#' @param model Multimod model
#' @param scenario_param energyRt bounds parameter
#' @param base_name Base parameter name (without Lo/Up suffix)
#' @return List with model and diagnostic info
#'
#' @keywords internal
apply_bounds_to_model <- function(model, scenario_param, base_name, scenario = NULL) {
  lo_name <- paste0(base_name, "Lo")
  up_name <- paste0(base_name, "Up")
  if (is.null(model$parameters)) model$parameters <- list()

  # Validate bounds parameter has two default values
  defaults <- scenario_param@defVal
  if (length(defaults) < 2) {
    stop("Bounds parameter '", base_name, "' must have exactly 2 default values (lo, up), but has ", 
         length(defaults), ": [", paste(defaults, collapse = ", "), "]")
  }
  
  lo_default <- defaults[1]
  up_default <- defaults[2]

  attached <- character()
  missing <- character()
  issues <- character()
  
  # Try to split data if available
  splits <- split_bounds_parameter_data(scenario_param, scenario = scenario)
  has_data <- !is.null(splits)
  
  # If no data available, create empty data frames
  if (!has_data) {
    splits <- list(
      lo = data.frame(),
      up = data.frame(),
      dims = as.character(scenario_param@dimSets)
    )
    issues <- c(issues, paste0(base_name, " (no data to split)"))
  }

  assign_side <- function(target_name, data_block, default_value, side_label) {
    if (!target_name %in% names(model$parameters)) {
      missing <<- c(missing, target_name)
      return()
    }
    param_obj <- model$parameters[[target_name]]
    
    # Set data
    param_obj$data <- data_block
    
    # Get dimension names from both model and scenario
    scenario_dims <- as.character(scenario_param@dimSets)
    
    if (is.null(param_obj$dims) || length(param_obj$dims) == 0) {
      # No existing dims - use scenario dims
      param_obj$dims <- multimod::ast_dims(scenario_dims)
    } else {
      # Ensure dims are a dims object
      if (!inherits(param_obj$dims, "dims")) {
        param_obj$dims <- multimod::ast_dims(param_obj$dims)
      }
      
      # Check for dimension name differences (but allow aliases)
      model_dims <- sapply(param_obj$dims, function(d) {
        if (inherits(d, "symbol")) d$name else as.character(d)
      })
      
      if (!identical(model_dims, scenario_dims)) {
        # Check if this is just an alias situation (e.g., src/dst for region)
        # If model has duplicate dimension names, it's likely an alias case in scenario
        has_duplicates_in_model <- length(model_dims) != length(unique(model_dims))
        
        if (has_duplicates_in_model && length(model_dims) == length(scenario_dims)) {
          # Model has duplicates (e.g., region, region), scenario likely has aliases (e.g., src, dst)
          # This is the expected alias pattern - no warning needed
        } else if (length(model_dims) != length(scenario_dims)) {
          warning("Dimension count mismatch for ", target_name,
                  ": model has ", length(model_dims), " dims, scenario has ", length(scenario_dims))
        } else {
          # Genuine mismatch - warn
          warning("Dimension mismatch for ", target_name, 
                  ": model has [", paste(model_dims, collapse=","), 
                  "], scenario has [", paste(scenario_dims, collapse=","), "]")
        }
      }
    }
    
    # Set default value
    param_obj$defVal <- default_value
    
    # Set metadata
    if (is.null(param_obj$misc)) param_obj$misc <- list()
    param_obj$misc$inMemory <- TRUE
    param_obj$misc$source <- scenario_param@name
    param_obj$misc$boundType <- side_label
    
    # Store path reference for lazy loading if data is on disk
    if (!is.null(scenario_param@misc$path)) {
      param_obj$misc$source_path <- scenario_param@misc$path
    }
    
    # Ensure value column is named correctly
    if (!"value" %in% names(param_obj$data) && nrow(param_obj$data) > 0) {
      colnames(param_obj$data)[ncol(param_obj$data)] <- "value"
    }
    
    # Use super-assignment to modify parent scope
    model$parameters[[target_name]] <<- param_obj
    attached <<- c(attached, target_name)
  }

  assign_side(lo_name, splits$lo, lo_default, "lo")
  assign_side(up_name, splits$up, up_default, "up")

  if (nrow(splits$lo) == 0 && is.na(lo_default)) {
    issues <- c(issues, paste0(base_name, " (missing lo data)"))
  }
  if (nrow(splits$up) == 0 && is.na(up_default)) {
    issues <- c(issues, paste0(base_name, " (missing up data)"))
  }

  list(model = model, attached = attached, missing = missing, issues = issues)
}

#' Find unmatched elements between model and scenario
#'
#' @keywords internal
find_unmatched_elements <- function(model, scenario, import_log) {
  unmatched <- list()
  
  # Find multimod parameters not in scenario
  multimod_params <- names(model$parameters)
  linked_params <- names(import_log$parameters)
  
  # Also check bounds splits
  bounds_targets <- unlist(lapply(import_log$bounds, function(b) {
    c(b$multimod_lo, b$multimod_up)
  }))
  
  unmatched$multimod_params_without_data <- setdiff(
    multimod_params,
    c(linked_params, bounds_targets)
  )
  
  # Find multimod mappings not in scenario
  multimod_mappings <- names(model$mappings)
  linked_mappings <- names(import_log$mappings)
  
  unmatched$multimod_mappings_without_data <- setdiff(
    multimod_mappings,
    linked_mappings
  )
  
  # Find scenario parameters not matched
  unmatched$scenario_params_unmatched <- names(Filter(function(x) {
    !is.na(x$status) && x$status == "unmatched_in_multimod"
  }, import_log$parameters))
  
  unmatched$scenario_mappings_unmatched <- names(Filter(function(x) {
    !is.na(x$status) && x$status == "unmatched_in_multimod"
  }, import_log$mappings))
  
  unmatched$scenario_bounds_unmatched <- names(Filter(function(x) {
    (!is.na(x$status_lo) && x$status_lo == "unmatched_in_multimod") ||
    (!is.na(x$status_up) && x$status_up == "unmatched_in_multimod")
  }, import_log$bounds))
  
  unmatched
}

# "linked" counts symbols attached, not data reachable. A run in which every
# symbol links and every one carries zero rows builds a 1x1 model that solves
# cleanly and returns a plausible wrong answer -- the failure this reporting
# exists to make visible.
.report_rows <- function(entries, what) {
  linked <- Filter(function(x) identical(x$status, "linked"), entries)
  if (!length(linked)) return(invisible(NULL))
  rows <- vapply(linked, function(x) {
    n <- x$n_rows
    if (is.null(n) || is.na(n)) 0 else as.numeric(n)
  }, numeric(1))
  empty <- sum(rows == 0)
  reachable <- vapply(linked, function(x) isTRUE(x$resolvable), logical(1))
  cat(sprintf("            %s rows across %d linked %s, %d empty, %d reachable\n",
              format(sum(rows), big.mark = ","), length(linked), what, empty,
              sum(reachable)))
  # Not a warning. A model in which nothing is reachable still builds, still
  # solves, and still returns a plausible number - built entirely from default
  # values. That failure has to stop the import, not decorate its log.
  if (!any(reachable)) {
    stop(sprintf(paste0(
      "Import failed: not one of the %d linked %s can be reached - no rows in ",
      "memory and no on-disk reference. The model would be built from default ",
      "values alone and would solve to a plausible wrong answer.\n",
      "  If the scenario is stored on disk, load it first, or pass ",
      "inMemory = TRUE to multimod_import_data()."),
      length(linked), what), call. = FALSE)
  }
  invisible(sum(rows))
}

#' Print import summary
#'
#' @keywords internal
print_import_summary <- function(import_log) {
  cat("\n=== Data Import Summary ===\n")
  
  # Sets
  if (!is.null(import_log$sets)) {
    n_populated <- sum(sapply(import_log$sets, function(x) x$status == "populated"))
    cat("Sets:      ", n_populated, "/", length(import_log$sets), "populated\n")
  }
  
  # Parameters
  if (!is.null(import_log$parameters)) {
    n_linked <- sum(sapply(import_log$parameters, function(x) x$status == "linked"))
    cat("Parameters:", n_linked, "/", length(import_log$parameters), "linked\n")
    .report_rows(import_log$parameters, "parameters")
  }
  
  # Mappings
  if (!is.null(import_log$mappings)) {
    n_linked <- sum(sapply(import_log$mappings, function(x) x$status == "linked"))
    cat("Mappings:  ", n_linked, "/", length(import_log$mappings), "linked\n")
    .report_rows(import_log$mappings, "mappings")
  }
  
  # Bounds
  if (!is.null(import_log$bounds)) {
    n_lo_linked <- sum(sapply(import_log$bounds, function(x) x$status_lo == "split_linked"))
    n_up_linked <- sum(sapply(import_log$bounds, function(x) x$status_up == "split_linked"))
    cat("Bounds:    ", length(import_log$bounds), "parameters split into",
        n_lo_linked, "Lo +", n_up_linked, "Up\n")
  }
  
  # Unmatched
  if (!is.null(import_log$unmatched)) {
    um <- import_log$unmatched
    if (length(um$multimod_params_without_data) > 0) {
      cat("\nWarning:", length(um$multimod_params_without_data), 
          "multimod parameters have no data\n")
    }
    if (length(um$scenario_params_unmatched) > 0) {
      cat("Warning:", length(um$scenario_params_unmatched), 
          "scenario parameters not matched in multimod\n")
    }
  }
  
  cat("===========================\n\n")
}

#' Get data import log from model
#'
#' @param model Multimod model with import log
#' @return Import log list or NULL if not available
#' @export
multimod_import_log <- function(model) {
  if (is.null(model$misc$data_import_log)) {
    message("No import log found. Run multimod_import_data() to create one.")
    return(NULL)
  }
  model$misc$data_import_log
}

#' Export import log to CSV
#'
#' @param model Multimod model with import log
#' @param file Path to CSV file
#' @keywords internal
#' @noRd
export_import_log_from_model <- function(model, file) {
  log <- multimod_import_log(model)
  if (is.null(log)) {
    stop("No import log available")
  }
  multimod::export_import_log(log, file)
  cat("Import log exported to:", file, "\n")
  invisible(file)
}

#' Show bounds parameter mapping
#'
#' @param model Multimod model with import log
#' @return Data frame showing bounds parameter splits
#' @keywords internal
#' @noRd
show_bounds_mapping <- function(model) {
  log <- multimod_import_log(model)
  if (is.null(log) || is.null(log$bounds)) {
    message("No bounds mapping found")
    return(NULL)
  }
  
  rows <- lapply(names(log$bounds), function(name) {
    entry <- log$bounds[[name]]
    data.frame(
      scenario_param = entry$scenario_name,
      multimod_lo = entry$multimod_lo,
      multimod_up = entry$multimod_up,
      status_lo = entry$status_lo,
      status_up = entry$status_up,
      dims = entry$dims,
      defVal_lo = entry$defVal_lo,
      defVal_up = entry$defVal_up,
      stringsAsFactors = FALSE
    )
  })
  
  do.call(rbind, rows)
}

#' Show unmatched elements
#'
#' @param model Multimod model with import log
#' @export
multimod_unmatched <- function(model) {
  log <- multimod_import_log(model)
  if (is.null(log) || is.null(log$unmatched)) {
    message("No unmatched elements information found")
    return(NULL)
  }
  
  um <- log$unmatched
  
  cat("\n=== Unmatched Elements ===\n\n")
  
  if (length(um$multimod_params_without_data) > 0) {
    cat("Multimod parameters without scenario data:\n")
    cat("  ", paste(um$multimod_params_without_data, collapse = ", "), "\n\n")
  }
  
  if (length(um$multimod_mappings_without_data) > 0) {
    cat("Multimod mappings without scenario data:\n")
    cat("  ", paste(um$multimod_mappings_without_data, collapse = ", "), "\n\n")
  }
  
  if (length(um$scenario_params_unmatched) > 0) {
    cat("Scenario parameters not in multimod:\n")
    cat("  ", paste(um$scenario_params_unmatched, collapse = ", "), "\n\n")
  }
  
  if (length(um$scenario_mappings_unmatched) > 0) {
    cat("Scenario mappings not in multimod:\n")
    cat("  ", paste(um$scenario_mappings_unmatched, collapse = ", "), "\n\n")
  }
  
  if (length(um$scenario_bounds_unmatched) > 0) {
    cat("Scenario bounds with missing targets in multimod:\n")
    cat("  ", paste(um$scenario_bounds_unmatched, collapse = ", "), "\n\n")
  }
  
  cat("==========================\n")
}

# --- the entry point ---------------------------------------------------------

#' Fail with an actionable message when energyRt is absent
#' @keywords internal
#' @noRd
.need_energyRt <- function(what) {
  if (!requireNamespace("energyRt", quietly = TRUE)) {
    stop(what, " needs the 'energyRt' package.\n",
         '  pak::pak("optimal2050/energyRt")', call. = FALSE)
  }
  invisible(TRUE)
}

#' Variable -> gating-map table from energyRt's mapping spec
#'
#' `.mapping_spec` is internal energyRt data. It is read through
#' `getFromNamespace()` rather than `:::` so the dependency is explicit and
#' fails with a clear message if energyRt's internals move.
#'
#' @return A named list: variable name -> character vector of mapping names.
#' @keywords internal
#' @noRd
.energyrt_gates_var <- function() {
  .need_multimod("Resolving variable domains")
  spec <- tryCatch(.mapping_spec, error = function(e) NULL)
  if (is.null(spec)) {
    stop("energyRt no longer exposes the internal `.mapping_spec` object, ",
         "which carries the authoritative variable -> gating-map table.\n",
         "  Without it 24 of 93 variables have no domain and would silently ",
         "contribute no columns.", call. = FALSE)
  }
  inv <- list()
  for (nm in names(spec)) {
    g <- spec[[nm]]$gates_var
    if (length(g)) for (v in g[nzchar(g)]) inv[[v]] <- c(inv[[v]], nm)
  }
  inv
}

#' Name of a symbol's domain, whichever form it takes
#'
#' `variable$domain` is a character (mapping name); `equation$domain` is an
#' `ast_mapping` node. Both occur.
#' @keywords internal
#' @noRd
.dom_name <- function(d) {
  if (is.null(d)) return(NA_character_)
  if (is.character(d)) return(if (length(d)) d[1] else NA_character_)
  if (!is.null(d$name)) return(d$name)
  NA_character_
}

#' Fill in variable domains that the GAMS comments do not carry
#'
#' `extract_domains_from_comments()` reads `*@ mXxx(...)` hints out of the
#' GAMS template. As of energyRt 0.89.5 there are hints for 70 of 93 variables,
#' so the rest (phase-out / retirement / storage auxiliary capacity) would get
#' no domain. energyRt's own `.mapping_spec` carries the authoritative table and
#' covers 92 of 93 (`vObjective` is a genuine scalar).
#'
#' @param model A multimod model.
#' @param prefer Where the hint and the spec disagree: `"spec"` trusts
#'   `.mapping_spec`, `"hint"` keeps the GAMS comment. Three variables differ
#'   (`vTechStockCap`, `vStorageInp`, `vStorageOut`). `"spec"` is the default
#'   because it is the choice that reproduces GLPK's column count (2,318 on the
#'   R1 kit, against 2,295 for `"hint"`) - the three hints are stale.
#' @param scalars Variables that legitimately have no domain.
#' @param verbose Logical; report what was filled and what conflicted.
#'
#' @return The model, with every variable carrying a domain.
#' @keywords internal
#' @noRd
fill_variable_domains <- function(model, prefer = c("spec", "hint"),
                                  scalars = "vObjective", verbose = TRUE) {
  stopifnot(inherits(model, "multimod") || inherits(model, "model"))
  prefer <- match.arg(prefer)
  inv <- .energyrt_gates_var()

  filled <- conflicts <- unresolved <- character()
  for (v in names(model$variables)) {
    cur <- .dom_name(model$variables[[v]]$domain)
    spec <- inv[[v]]

    if (is.na(cur)) {
      if (v %in% scalars) next                       # genuinely scalar
      if (is.null(spec)) { unresolved <- c(unresolved, v); next }
      model$variables[[v]]$domain <- spec[1]
      filled <- c(filled, v)
    } else if (!is.null(spec) && !(cur %in% spec)) {
      conflicts <- c(conflicts, sprintf("%s (hint=%s, spec=%s)", v, cur, spec[1]))
      if (prefer == "spec") model$variables[[v]]$domain <- spec[1]
    }
  }

  if (verbose) {
    message("fill_variable_domains(prefer = '", prefer, "'):")
    message("  filled from .mapping_spec: ", length(filled))
    if (length(conflicts)) {
      message("  hint/spec conflicts (", prefer, " wins): ", length(conflicts))
      for (x in conflicts) message("    ", x)
    }
    if (length(unresolved)) {
      message("  UNRESOLVED: ", paste(unresolved, collapse = ", "))
    }
  }

  # A variable with no domain contributes no columns, so this must stop rather
  # than warn: the model would build, solve, and be missing a whole block.
  if (length(unresolved)) {
    stop("No domain map for variable(s): ", paste(unresolved, collapse = ", "),
         ".\n  Add a '*@ mXxx(...)' hint in energyRt/gams/energyRt.gms, or a ",
         "gates_var entry in energyRt's mapping spec.", call. = FALSE)
  }
  model
}

#' Path to energyRt's GAMS template
#'
#' @return The installed template path, or `NULL` when it cannot be found.
#' @keywords internal
#' @noRd
.energyrt_gms <- function() {
  .need_multimod("Reading the energyRt model")
  for (p in c(system.file("gams", "energyRt.gms", package = "energyRt"),
              system.file("energyRt.gms", package = "energyRt"))) {
    if (nzchar(p) && file.exists(p)) return(p)
  }
  NULL
}

#' energyRt's GAMS model code, as shipped
#'
#' energyRt `.Rbuildignore`s its `gams/` directory, so `system.file()` finds
#' nothing in an *installed* energyRt and [.energyrt_gms()] returns NULL - which
#' meant `gms = NULL` could never resolve and every caller had to point at a
#' source checkout.
#'
#' The code is shipped, though: `.modelCode$GAMS` in energyRt's `R/sysdata.rda`
#' carries it as a character vector of ~3,000 lines (alongside the JuMP, Pyomo
#' and GLPK templates). Reading that is strictly better than a file lookup - it
#' needs no checkout, and it ties the AST to the *installed* energyRt version
#' rather than to whichever of the nine `.gms` files in `gams/` happened to be
#' on disk. (The two differ: 0.90.0.9000 ships 2,954 lines where the working
#' tree's `gams/energyRt.gms` had 2,979.)
#'
#' Read through `getFromNamespace()` rather than `:::`, as `.mapping_spec` is,
#' so the dependency is explicit and fails with a clear message if it moves.
#'
#' @return Character vector of GAMS source lines, or `NULL`.
#' @keywords internal
#' @noRd
.energyrt_model_code <- function() {
  .need_multimod("Reading the energyRt model")
  mc <- tryCatch(.modelCode, error = function(e) NULL)
  if (is.null(mc) || is.null(mc$GAMS) || !length(mc$GAMS)) return(NULL)
  mc$GAMS
}

#' Build a multimod model from an interpolated energyRt scenario
#'
#' Structure is read from energyRt's GAMS template; data comes from the
#' scenario. This is the entry point for the direct-matrix / MPS route -
#' everything downstream ([model_to_lp()], [write_mps()], [solve_highs()])
#' takes the model this returns.
#'
#' @param scen An interpolated energyRt scenario.
#' @param gms The GAMS model source. `NULL` (the default) reads the code
#'   energyRt ships in `.modelCode$GAMS`, which needs no source checkout and
#'   matches the installed energyRt version. Pass a path to read a working
#'   tree instead, or a character vector of GAMS source directly.
#' @param prefer Passed to [fill_variable_domains()].
#' @param inMemory Load the scenario's data into memory. Must be `TRUE` for a
#'   folded scenario (the lazy path cannot expand wildcards) - see
#'   [multimod_import_data()].
#' @param verbose Logical; report progress.
#'
#' @return A multimod model with data attached, ready for [model_to_lp()].
#'
#' @examples
#' \dontrun{
#' scen <- energyRt::interpolate_model(mod, name = "BASE")
#' m <- multimod_from_energyRt(scen)
#' lp <- model_to_lp(m)
#' write_mps(m, "model.mps", lp = lp)
#' }
#' @export
multimod_from_energyRt <- function(scen, gms = NULL, prefer = c("spec", "hint"),
                                   inMemory = TRUE, verbose = TRUE) {
  .need_multimod("multimod_from_energyRt()")
  if (!inherits(scen, "scenario")) {
    stop("`scen` must be an energyRt scenario object.", call. = FALSE)
  }
  prefer <- match.arg(prefer)

  # Where the equations come from. The default is the code energyRt SHIPS
  # (.modelCode$GAMS); a path or a character vector is honoured if given,
  # and a source checkout is the last resort.
  if (is.null(gms)) {
    code <- .energyrt_model_code()
    src <- sprintf("energyRt::.modelCode$GAMS (energyRt %s)",
                   utils::packageVersion("energyRt"))
    if (is.null(code)) {
      code <- .energyrt_gms()
      src <- code
    }
    if (is.null(code)) {
      stop("energyRt ships its model code as `.modelCode$GAMS`, which this ",
           "energyRt does not expose, and no `gams/energyRt.gms` was found ",
           "either.\n  Pass `gms = ` explicitly (a path, or a character ",
           "vector of GAMS source).", call. = FALSE)
    }
  } else if (length(gms) == 1L && file.exists(gms)) {
    code <- gms                       # multimod::read_gams() takes a path
    src <- gms
  } else if (is.character(gms) && length(gms) > 1L) {
    code <- gms                       # ... or the source itself
    src <- "<character vector>"
  } else {
    stop("`gms` must be a path to a .gms file or a character vector of GAMS ",
         "source; got ", class(gms)[1], " of length ", length(gms), ".",
         call. = FALSE)
  }

  ms <- multimod::read_gams(code, include = FALSE)
  ms <- extract_domains_from_comments(ms)
  ms <- populate_defvals(ms)
  m <- multimod::as_multimod(ms)
  m <- fill_variable_domains(m, prefer = prefer, verbose = verbose)

  m <- multimod_import_data(m, scen, inMemory = inMemory)
  m <- suppressWarnings(
    multimod::add_index_aliases(m, index_aliases_energyRt, overwrite = TRUE))

  # Constraints and cost terms added with energyRt's newConstraint() /
  # newCosts() are NOT in the GAMS template: interpolation compiles them to a
  # GAMS string IR on `modInp@user_constraints`, per scenario. Omitting this
  # step produced a model of exactly the right shape with those rows missing -
  # the silent-wrong-answer failure this package keeps guarding against.
  m <- add_user_constraints(m, scen, verbose = verbose)

  attr(m, "energyRt_version") <- as.character(utils::packageVersion("energyRt"))
  attr(m, "energyRt_gms") <- src
  m
}

#' Build a multimod model from an interpolated scenario
#'
#' Converts an energyRt scenario into a \pkg{multimod} model: the equations come
#' from energyRt's own GAMS model code (shipped as `.modelCode$GAMS`), the data
#' from the scenario's `@modInp`, and any `newConstraint()` / `newCosts()`
#' objects from the GAMS string IR interpolation compiled onto
#' `@modInp@user_constraints`.
#'
#' The model can then be rendered to any backend multimod supports, or
#' assembled straight into a matrix and solved - see `multimod::model_to_lp()`
#' and `multimod::solve_highs()`.
#'
#' @param x An interpolated energyRt scenario.
#' @param ... Passed to [multimod_from_energyRt()]: `gms`, `prefer`,
#'   `inMemory`, `verbose`.
#' @return A multimod model.
#' @seealso [multimod_from_energyRt()]
#' @examples
#' \dontrun{
#' scen <- interpolate_model(mod, name = "BASE")
#' m    <- multimod::as_multimod(scen)
#' lp   <- multimod::model_to_lp(m)
#' }
#' @exportS3Method multimod::as_multimod
as_multimod.scenario <- function(x, ...) {
  multimod_from_energyRt(x, ...)
}
