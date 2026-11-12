#' Extract data from energyRt objects
#'
#' Generic accessor. Dispatches on the object class:
#' \itemize{
#'   \item \code{scenario} (or a list of scenarios): interpolated/solved data
#'     from \code{modInp}/\code{modOut} -- see the \code{scenario} method below.
#'   \item model objects (\code{technology}, \code{commodity}, \code{storage},
#'     \code{supply}, \code{demand}, \code{trade}, ...): the object's own
#'     \emph{raw input} slot data (pre-interpolation).
#'   \item \code{model} / \code{repository}: raw input slots stacked across all
#'     contained objects.
#' }
#'
#' @param object a \code{scenario} (or list of scenarios), a model object, a
#'   \code{model}, or a \code{repository}.
#' @param ... passed to the dispatched method (filters, \code{name}, \code{merge}, ...).
#' @seealso the per-class methods for the full argument list.
#' @export
getData <- function(object, ...) UseMethod("getData")

#' Performs search for available data in _scenario_ object.
#'
#' @param object object _scenario_ with model solution.
#' @param dataType type of data to lok for (currently only "parameters" and "variables").
#' @param dropEmpty logical, if TRUE drops parameters and variables with zero length.
#' @param valueColumn logical, if TRUE will return variables and parameters with 'value' column (to filter sets and mappings).
#' @param dfDim logical, if TRUE returns dimension _dim_.
#' @param dfNames logical, when TRUE returns names of the data frame column.
#' @param asMatrix return results as a matrix (not implemented).
#' @param setsNames_ regular expression pattern for names of sets which will be included in search.
#' @param add_weights accepted and currently ignored.
#' @param allSets logical, if TRUE _and_ operator should be used in search the sets, _or_ will be used if FALSE.
#' @param ignore.case grepl parameter for matching names.
#'
#' @return list with variables and parameters name, each includes _dim_ and _names_ character vectors.
#'
#' @export
#' @examples
#' \dontrun{
#' open_project(tempdir())
#'
#' topia_elc <- topia$modules$electricity
#' mod  <- newModel("TOPIA", data = topia_elc$repo,
#'                  calendar = topia$modules$calendars$s4_h24,
#'                  region = topia_elc$regions,
#'                  horizon = topia$modules$horizons$BY2025_50_by10,
#'                  discount = 0.05)
#' mod <- setGeoscale(mod, topia_elc$geoscale)
#' scen <- solve_model(mod, name = "BASE", solver = solver_options$glpk)
#'
#' # which parameters carry a `tech` set, and what their dimensions are
#' findData(scen, setsNames_ = "tech")
#'
#' # the solved variables only
#' findData(scen, dataType = "variables", setsNames_ = "tech")
#' }
findData <- function(object,
                     dataType = c("parameters", "variables"),
                     setsNames_ = NULL,
                     valueColumn = TRUE,
                     allSets = TRUE,
                     ignore.case = FALSE,
                     add_weights = "auto",
                     dropEmpty = TRUE,
                     dfDim = TRUE,
                     dfNames = TRUE,
                     asMatrix = FALSE) {
  ll <- lt <- list()
  # 1 Parameters
  ii <- dataType == "parameters"
  if (any(ii)) {
    dataType <- dataType[!ii]
    lt <- lapply(object@modInp@parameters, function(x) {
      qu <- get_lazy_dim_names(x, slot = "data")
      qu
    })
  }
  ll <- c(ll, lt)

  # 2. Variables
  ii <- dataType == "variables"
  if (any(ii)) {
    lt <- list()
    dataType <- dataType[!ii]
    for (v in names(object@modOut@variables)) {
      # The `variable` object itself, exactly as the parameter branch above
      # passes the `parameter`. Passing the modOut container instead returns
      # `colnames()` of an S4 object -- NULL -- and the value-column filter
      # below then drops every variable without a word.
      qu <- get_lazy_dim_names(object@modOut@variables[[v]], slot = "data")
      lt[[v]] <- list(
        dim = qu$dim,
        names = qu$names
      )
    }
    ll <- c(ll, lt)
  }
  if (valueColumn) {
    ii <- sapply(ll, function(x) {
      any(grepl("^value$", x$names,
        ignore.case = ignore.case
      ))
    })
    ll <- ll[ii]
  }

  if (length(setsNames_) > 0) {
    ii <- sapply(ll, function(x) {
      if (allSets) {
        all(
          sapply(setsNames_, function(y) {
            any(grepl(y, x$names,
              ignore.case = ignore.case
            ))
          })
        )
      } else {
        any(
          sapply(setsNames_, function(y) {
            any(grepl(y, x$names,
              ignore.case = ignore.case
            ))
          })
        )
      }
    })
    ll <- ll[ii]
  }

  if (length(dataType) > 0) warning("Data type '", dataType, "' is not found.")

  if (dropEmpty) {
    ii <- sapply(ll, is.null)
    ll <- ll[!ii]
  }
  return(ll)
}

# @param drop if TRUE, the sets with only one unique value will be dropped (not implemented)

#' Extracts information from scenario objects, based on filters.
#'
#' @rdname getData
#' @param object Object scenario or list of scenarios.
#' @param ... filters for various sets (setname = c(val1, val2) or setname_ = "matching pattern"), see details.
#' @param name character vector with names of parameters and/or variables.
#' @param merge if TRUE, the search results will be merged in one dataframe; the named list will be returned if FALSE. When TRUE, a data.frame (empty if nothing matched) is always returned, never NULL.
#' @param geoframe controls spatial aggregation of results that carry a `region` column, the spatial twin of `timeframe`. One of `"finest"` (default, native resolution as stored), `"coarsest"` (aggregate up to the top geoscale level), `"all"` (return every level stacked), or an explicit geoscale level name (e.g. `"zone"`, `"nation"`). Requires a geoscale on the model; without one this is inert. How a variable combines is declared per variable (`recast` in `data-raw/variables.yml`): a quantity is summed, while an inter-regional flow (e.g. `vTradeIr`) is summed only where it CROSSES the new border. Rows whose endpoints land in the same target region are no longer a transfer, so they leave the total and are recorded as in-region flow in `attr(x, "absorbed")` rather than double-counted.
#' @param timeframe controls sub-annual time aggregation of results that carry a `timeslice` column. One of `"highest"` (default, native/finest resolution, as stored), `"lowest"` (aggregate/sum flows up to the coarsest level, normally `ANNUAL`), `"all"` (return every timeframe level stacked), or an explicit calendar level name (e.g. `"SEASON"`, `"YDAY"`) to aggregate to that level. Non-timeslice data is returned unchanged. How a variable combines is declared per variable (`recast` in `data-raw/variables.yml`): a quantity is summed at the child/target weight ratio, while a state/level variable (e.g. `vStorageLevel`) takes its CLOSING value, with the swing inside each target slice recorded in `attr(x, "absorbed")`.
#'
#'   The default was `"lowest"` up to v0.80 and is now `"highest"`, matching the spatial twin `geoframe`, whose default `"finest"` has always meant "as stored". Returning data at native resolution unless asked otherwise is the safer default: the old one silently summed an 8760-slice hourly series into a single annual number, which reads as a plausible value rather than as an error, and is indistinguishable from a genuine annual result. Aggregation is easy to ask for and hard to notice when it is not wanted. Code that relied on annual totals must now pass `timeframe = "lowest"` explicitly.
#' @param process `TRUE` renames the process-class id dimension ("tech", "stg",
#'   "trade", "sup", "dem", "imp", "impp", "expp") to "process", so results from
#'   different classes stack into one frame. A character value instead SELECTS
#'   the named process(es) and renames -- `process = "COAL"`. Use `process_` for
#'   a regex, e.g. `process_ = "^E"`. A character value is always a selection,
#'   so "T"/"TRUE" are read as process names, not logicals.
#' @param parameters if TRUE, parameters will be included in the search and returned if found.
#' @param variables if TRUE, variables will be included in the search and returned if found.
#' @param maps if TRUE, map-type parameters (membership mappings, no `value` column) are also returned.
#' @param na.rm if TRUE, NA values will be dropped.
#' @param digits if integer, indicates the number of decimal places for rounding, if NULL - no actions.
#' @param run character, one or more runs to read (`"<solve>"` or
#'   `"<variant>/<solve>"`), or `"all"` for every run of each scenario. The
#'   returned rows gain a `run` column naming their source. `NULL` (default)
#'   reads each object's ACTIVE run and adds no column, so existing calls are
#'   unchanged. Each run is read on a COPY of the scenario, so the object you
#'   passed keeps its active run; a variant run brings its own problem with
#'   it, parameters included. Spanning runs of different problems warns —
#'   their results are not like-for-like. Not to be confused with `variants`,
#'   below, which is about technologies.
#' @param base_year logical, default `FALSE`. When `TRUE`, a scenario whose
#'   horizon marks a base year also returns that year, read from the separate
#'   run the scenario recorded (see [solve_base_year()]), and every row gains
#'   a `source` column — `"solved"` for the scenario's own LP, `"base"` for
#'   the base year. `vObjective` is not unioned: the base year and the
#'   projection are separate optimisations, so only the projection's is
#'   returned. A scenario that records no base year, or whose recorded source
#'   cannot be read, warns and returns its own rows alone. Note that
#'   post-processing applies the FIRST object's horizon and calendar labels to
#'   every row.
#' @param variants logical, default `TRUE`: attach the technology-variant
#'   provenance columns `base`, `vintage` and `cluster` (from
#'   `scenario@modInp@sets$variant`) to the returned data, so results of a
#'   vintaged / clustered technology can be grouped or rolled up by those
#'   dimensions without parsing the variant names. These are TECHNOLOGY
#'   variants — unrelated to the own-problem run variants under `runs/`, which
#'   are selected by switching the scenario with [read_solution()].
#'   Has no effect when the model
#'   has no variants, so the returned shape is unchanged for such models. Set
#'   `FALSE` to suppress the columns.
#' @param drop.zeros logical, should rows containing zero values be filtered out.
#' @param asTibble logical, if the data.frames should be converted into tibbles.
#' @param newNames renaming sets, named character vector or list with new names as values, and old names as names - the input parameter to renameSets function. The operation is performed before merging the data (merge parameter).
#' @param newValues revalue sets, named character vector or list with new values as values, and old values as names - the input parameter to revalueSets function. The operation is performed after merging the data (merge parameter).
#' @param ignore.case grepl parameter if regular expressions are used in '...' or 'name_'.
#' @param stringsAsFactors logical, should the sets values be converted to factors?
#' @param yearsAsFactors logical, should `year` be converted to factors? Set 'year' is integer by default.
#' @param scenNameInList logical, should the name of the scenarios be used if not provided in the list with several scenarios?
#' @param verbose logical, print progress and diagnostic messages.
#' @param sets logical, include set-type parameters in the result (by default
#'   only value-bearing parameters are returned).
#' @param object the object whose data is returned, for the per-class methods
#'   (`technology`, `commodity`, `storage`, ...).
#' @param unfold logical, expand a stored parameter back over its full domain
#'   before returning it.
#' @param add_weights,add_period_length,apply_weights,apply_period_length,as_data_table
#'   accepted and currently ignored.
#' @param drop_duplicated_scenarios logical, when several scenarios carry the
#'   same name, keep only the first.
#'
#' @aliases getData get_data
#'
#' @examples
#' \dontrun{
#' data("topia_scen_BAU.RData")
#' getData(scen, name = "pDemand", year = 2015, merge = TRUE)
#' getData(scen, name = "vTechOut", comm = "ELC", merge = TRUE, year = 2015)
#' elc2050 <- getData(scen, parameters = FALSE, comm = "ELC", year = 2050)
#' names(elc2050)
#' elc2050$vBalance
#' }
#' @method getData scenario
#' @export
getData.scenario <- function(
    object,
    name = NULL,
    ...,
    merge = FALSE,
    timeframe = c("highest", "lowest", "all"),
    geoframe = c("finest", "coarsest", "all"),
    process = FALSE,
    parameters = TRUE,
    variables = TRUE,
    sets = FALSE,
    maps = FALSE,
    ignore.case = TRUE,
    newNames = NULL,
    newValues = NULL,
    na.rm = FALSE,
    digits = NULL,
    drop.zeros = FALSE,
    add_weights = "auto",
    add_period_length = "auto",
    apply_weights = FALSE,
    apply_period_length = FALSE,
    asTibble = TRUE,
    as_data_table = FALSE,
    stringsAsFactors = FALSE,
    yearsAsFactors = FALSE,
    drop_duplicated_scenarios = TRUE,
    scenNameInList = as.logical(length(object) - 1),
    unfold = TRUE,
    variants = TRUE,
    run = NULL,
    base_year = FALSE,
    verbose = FALSE) {
  arg <- list(...)
  argnam <- names(arg)
  stopifnot(!any(duplicated(argnam)))
  # `process` takes either a logical -- rename the class id column to `process`
  # -- or process name(s), which select AND rename. A character value is always
  # a selection, so "T"/"TRUE"/"F"/"FALSE" are read as process names, not
  # logicals. The regex form is `process_ =`, which reaches `...` untouched.
  process_flt <- NULL
  if (is.character(process)) {
    process_flt <- process
    process <- TRUE
  }
  process_rx <- any(grepl("^process_$", names(arg)))
  if (process_rx) process <- TRUE
  if (isTRUE(process)) {
    stopifnot(length(newNames) == length(unique(newNames)))
    newNamesDefault <- stats::setNames(
      rep("process", length(.process_dims)), .process_dims
    )
    if (!is.null(newNames)) {
      ii <- names(newNamesDefault) %in% names(newNames)
      newNames <- c(newNames, newNamesDefault[!ii])
    } else {
      newNames <- newNamesDefault
    }
  }
  # Select scenarios, check and add names if not provided
  if (!is.list(object)) {
    object <- list(object)
    names(object) <- object[[1]]@name
  } else {
    ii <- sapply(object, class) == "scenario"
    if (sum(ii) == 0) {
      .say("Scenario object is not found")
      return(.getdata_empty(merge, asTibble))
    }
    object <- object[ii] # keep scenarios only
    nm <- names(object)
    if (is.null(nm)) nm <- rep("", length(object))
    ii <- nm == ""
    nm[ii] <- sapply(object[ii], function(x) x@name) # work on names
    names(object) <- nm
    ii <- duplicated(nm)
    if (any(ii)) {
      if (drop_duplicated_scenarios) {
        warning("Dropping duplicated scenarios: ", nm[ii])
        object <- object[!ii]
      } else {
        if (verbose) cat("Found scenarios with identical names: ", nm[ii], "\n")
      }
    }
  }

  # Runs. `run = NULL` means each object's ACTIVE run -- today's behaviour,
  # and no `run` column, because getData()'s output shape must not move for
  # every existing caller. A run id positions a COPY of the scenario on that
  # run exactly as read_solution() does, including the problem swap a variant
  # needs: without it a variant's PARAMETERS would come from the base problem
  # while its results came from the variant.
  run_of <- NULL
  if (!is.null(run) && length(run)) {
    exp_scen <- list(); exp_run <- character(0); exp_nm <- character(0)
    for (s in seq_along(object)) {
      sc0 <- object[[s]]
      nmi <- names(object)[s]
      rids <- if (identical(as.character(run)[1], "all")) {
        rr <- tryCatch(scenario_runs(sc0)$run, error = function(e) character(0))
        rr[nzchar(rr)]
      } else as.character(run)
      if (!length(rids)) {
        warning("Scenario '", nmi, "' has no runs to read.", call. = FALSE)
        next
      }
      for (rid in rids) {
        sc1 <- tryCatch(
          suppressMessages(read_solution(sc0, run = rid, verbose = FALSE,
                                         ondisk = FALSE)),
          error = function(e) {
            stop("getData(run = ): cannot read run '", rid, "' of scenario '",
                 nmi, "': ", conditionMessage(e), call. = FALSE)
          })
        exp_scen[[length(exp_scen) + 1L]] <- sc1
        exp_run <- c(exp_run, rid); exp_nm <- c(exp_nm, nmi)
      }
    }
    if (!length(exp_scen)) return(.getdata_empty(merge, asTibble))
    # A variant is a DIFFERENT problem -- its own calendar, geoscale or
    # horizon. Stacking such runs in one frame invites a comparison that is
    # not like-for-like, so say so; the `run` column makes it visible, but a
    # reader has to notice it.
    vs <- unique(vapply(strsplit(exp_run, "/", fixed = TRUE),
                        function(x) if (length(x) > 1L) x[1] else "",
                        character(1)))
    if (length(vs) > 1L) {
      warning("getData(run = ) spans runs of different problems (",
              paste(ifelse(nzchar(vs), vs, "<base>"), collapse = ", "),
              "). An own-problem variant has its own modInp, so these ",
              "results are not like-for-like.", call. = FALSE)
    }
    object <- exp_scen; names(object) <- exp_nm; run_of <- exp_run
  }

  # The base year, on request. A marked scenario's LP does not contain it, so
  # its rows come from the separate run the scenario recorded -- read through
  # the same positioning machinery as `run =`, and labelled by a `source`
  # column so the two halves are never mistaken for one solve.
  src_of <- NULL
  if (isTRUE(base_year)) {
    exp_scen <- list(); exp_nm <- character(0)
    exp_run <- character(0); exp_src <- character(0)
    for (s in seq_along(object)) {
      sc0 <- object[[s]]; nmi <- names(object)[s]
      exp_scen[[length(exp_scen) + 1L]] <- sc0
      exp_nm <- c(exp_nm, nmi)
      exp_run <- c(exp_run, if (is.null(run_of)) "" else run_of[s])
      exp_src <- c(exp_src, "solved")
      link <- .by_link_of(sc0)
      if (is.null(link)) {
        warning("getData(base_year = TRUE): scenario '", nmi,
                "' records no base year; its own rows only.", call. = FALSE)
        next
      }
      sb <- .by_link_open(link)
      if (is.null(sb)) {
        warning("getData(base_year = TRUE): scenario '", nmi, "' records a ",
                "base year (", link$year, ") whose solution cannot be read",
                if (nzchar(link$scenario)) paste0(" from '", link$scenario,
                  if (nzchar(link$run)) paste0("/", link$run) else "", "'")
                else " (no source was carried)", ".", call. = FALSE)
        next
      }
      exp_scen[[length(exp_scen) + 1L]] <- sb
      exp_nm <- c(exp_nm, nmi)
      exp_run <- c(exp_run, link$run)
      exp_src <- c(exp_src, "base")
    }
    object <- exp_scen; names(object) <- exp_nm
    src_of <- exp_src
    if (!is.null(run_of)) run_of <- exp_run
  }

  # Identify filters
  ii <- grepl("name_", argnam, ignore.case = ignore.case)
  if (any(ii)) {
    if (!is.null(name)) stop("Duplicated parameter 'name' ('name_')")
    name_ <- arg[ii][[1]]
    arg <- arg[!ii]
  } else {
    name_ <- NULL
  }
  # A `process = "<name>"` selection becomes an ordinary exact filter on the
  # `process` column, which the per-parameter loop below creates by renaming.
  if (!is.null(process_flt)) {
    if ("process" %in% names(arg)) stop("Duplicated parameters process")
    arg$process <- process_flt
  }
  ii <- grepl("_$", names(arg), ignore.case = ignore.case)
  flt_ <- arg[ii]
  flt <- arg[!ii]
  # check for duplicates
  nflt <- names(flt)
  nflt_ <- names(flt_)
  nflt0 <- sub("_$", "", nflt_)
  ii <- (nflt %in% nflt0)
  if (any(ii)) stop("Duplicated parameters ", nflt_[ii])
  nflt1 <- c(nflt, nflt0)

  # Fishing for the data in scenarios
  ll <- list()
  parvar <- c(parameters = parameters, variables = variables)
  for (s in 1:length(object)) { # loop over scenarios
    sc <- names(object)[s]
    # Data availability: an un-interpolated scenario has no modInp parameters.
    # Solvedness is read off `@stage` OR off actual DATA rows -- `modOut`
    # pre-populates one empty `variable` per declared variable (so COUNTING
    # variables proves nothing), but non-empty rows are real: a solver that
    # stopped short of proven optimality (a MIP at its gap/time limit, an
    # odd status code) still exports its incumbent, and withholding it
    # silently made getData() return an empty frame while the data sat in
    # `@modOut` in plain sight. Serve it, and SAY so.
    stage_s <- tryCatch(object[[s]]@modOut@stage, error = function(e) "")
    solved_s <- identical(stage_s, "solved")
    has_rows_s <- tryCatch(
      any(vapply(object[[s]]@modOut@variables,
                 function(v) {
                   nrow(v@data) > 0 || isTRUE(v@misc$onDisk$data$dim[1] > 0)
                 }, logical(1))),
      error = function(e) FALSE)
    avail <- c(
      parameters = tryCatch(length(object[[s]]@modInp@parameters) > 0,
        error = function(e) FALSE),
      variables = solved_s || has_rows_s
    )
    if (isTRUE(variables) && !solved_s && length(stage_s) == 1L &&
        nzchar(stage_s)) {
      # only speak up when the query can actually touch variables: a full
      # dump, or a name matching a declared variable
      vnames <- tryCatch(names(object[[s]]@modOut@variables),
                         error = function(e) character(0))
      touches <- is.null(name) ||
        any(tolower(vnames) %in% tolower(as.character(name)))
      if (touches && has_rows_s) {
        warning("Solution stage of scenario '", sc, "' is \"", stage_s,
                "\" — returning the stored solution anyway; treat the ",
                "values as not proven optimal.", call. = FALSE)
      } else if (touches && !has_rows_s) {
        .say("Scenario '", sc, "' carries no solution values (stage: \"",
                stage_s, "\").")
      }
    }
    if (verbose && !any(avail)) {
      message("Scenario '", sc, "' has no interpolated/solved data to extract.")
    }
    for (datype in names(parvar)[parvar]) { # loop over data sources
      if (!isTRUE(avail[[datype]])) {
        if (verbose) cat("No", datype, "available in scenario '", sc, "'.\n")
        next
      }
      if (verbose) cat("Extracting data from", datype, "\n")
      # `process` is not a dimension of any parameter or variable -- it is the
      # class id column, renamed per frame below -- so pre-selecting on it would
      # match nothing and silently empty the result.
      nflt_find <- setdiff(nflt1, "process")
      if (length(nflt_find) > 0) {
        sets_names <- paste0("^", nflt_find, "$")
      } else {
        sets_names <- NULL
      }
      lt <- findData(object[[s]],
        dataType = datype, setsNames_ = sets_names,
        valueColumn = !(sets || maps),
        ignore.case = ignore.case
      )
      # Restrict parameters by type: value-bearing always; sets/maps on request.
      # (Reproduces the old `valueColumn = !sets` gate while adding `maps`.)
      if (datype == "parameters" && (sets || maps)) {
        keep_types <- c("numpar", "bounds")
        if (sets) keep_types <- c(keep_types, "set", "map")
        if (maps) keep_types <- c(keep_types, "map")
        keep_types <- unique(keep_types)
        ptype <- vapply(names(lt), function(nm) {
          p <- object[[s]]@modInp@parameters[[nm]]
          if (is.null(p) || !methods::.hasSlot(p, "type")) {
            return(NA_character_)
          }
          t <- p@type
          if (length(t) != 1L) NA_character_ else as.character(t)[1]
        }, character(1))
        lt <- lt[is.na(ptype) | ptype %in% keep_types]
      }
      pvNames <- names(lt)
      # filter for variable/parameter names
      if (!is.null(name)) {
        ii <- pvNames %in% name
        lt <- lt[ii]
      } else if (!is.null(name_)) {
        ii <- sapply(pvNames, function(x) {
          any(sapply(name_, function(y) grepl(y, x, ignore.case = ignore.case)))
        })
        lt <- lt[ii]
      }
      clNames <- unique(purrr::flatten_chr(lapply(lt, function(x) x$names))) # All par/var df-columns names
      # Filter for columns/sets
      if (length(nflt_find) > 0 & length(clNames) > 0) {
        # Check if provided sets/filters exist
        ii <- nflt_find %in% clNames
        if (!all(ii)) {
          warning(
            "Sets '", paste(nflt_find[!ii], collapse = "', '"),
            "' have not been found in scenario '", sc, "',", datype, "'."
          )
        }
        # find all matching names of columns
        ii <- sapply(clNames, function(x) {
          any(grepl(x, nflt_find, ignore.case = ignore.case))
        })
        clNames <- clNames[ii]
        if (length(clNames) == 0) {
          warning("Inconsistent combination of filters.")
          return(.getdata_empty(merge, asTibble))
        }
        # find pars/vars which have any of the col-names for filtration
        ii <- sapply(lt, function(x) {
          any(sapply(x$names, function(y) {
            any(grepl(y, clNames, ignore.case = ignore.case))
          }))
        })
        lt <- lt[ii]
      }
      pvNames <- names(lt)
      if (length(pvNames) == 0) {
        if (verbose) {
          cat("No ", datype,
            " found for the selected set of filters, scenario '",
            sc, "'.\n",
            sep = ""
          )
        }
      } else {
        for (pv in pvNames) { # selected pars/vars
          if (datype == "parameters") {
            dat <- get_lazy_data(object[[s]]@modInp@parameters[[pv]],
              slot = "data"
            )
            if (!is.null(dat)) {
              dat <- collect(dat)
            }
            # Unfold wildcard (NA) rows of folded parameters back to explicit
            # members at read time, using the scenario's membership maps.
            if (isTRUE(unfold) && !is.null(dat) && nrow(dat) > 0) {
              fi <- object[[s]]@modInp@parameters[[pv]]@misc[["fold_info"]]
              if (!is.null(fi) && isTRUE(fi[["folded"]])) {
                dat <- unfold_scenario_parameter(
                  object[[s]], object[[s]]@modInp@parameters[[pv]]
                )
              }
            }
          } else {
            dat <- get_lazy_data(object[[s]]@modOut@variables[[pv]],
              slot = "data"
            )
            if (!is.null(dat)) {
              # Push the plain membership selectors onto the dataset before
              # collecting: filtering after `collect()` reads the whole
              # variable to return a slice of it. Only selectors whose name IS
              # a column are pushed, and the authoritative filter below runs
              # again on the collected rows, so this only reduces the read.
              # Solution data carries no wildcard rows, so nothing an `unfold`
              # would need is lost here (the parameter branch above, which
              # does, is left alone).
              for (k in intersect(names(dat), nflt)) {
                .vals <- flt[[k]]
                if (length(.vals) == 0) next
                dat <- tryCatch(dplyr::filter(dat, .data[[k]] %in% .vals),
                                error = function(e) dat)
              }
              dat <- collect(dat)
            }
          }
          dim1 <- dim(dat)[1]
          if (is.null(dim1)) dim1 <- 0
          kk <- rep(TRUE, dim1)
          # A `process` filter -- and `process = TRUE` -- needs the class id
          # column to BE `process` while filtering; the generic rename at the end
          # of the scenario loop runs too late. At most one process-class column
          # can occur in one frame, which is what makes the union unambiguous.
          if (isTRUE(process) && !is.null(dat)) {
            pc <- intersect(names(dat), .process_dims)
            if (length(pc) == 1L) {
              names(dat)[names(dat) == pc] <- "process"
            }
            if ((!is.null(process_flt) || process_rx) &&
                !("process" %in% names(dat))) {
              # a selection cannot be honoured on a frame with no process
              # dimension: drop it rather than return it unfiltered
              kk <- rep(FALSE, dim1)
            }
          }
          if (length(nflt1) > 0) { # the data should be filtered
            if (dim1 > 0) { # data exists
              prcl <- names(dat)
              prcl <- prcl[prcl %in% nflt1]
              for (st in prcl) { # selected sets (columns)
                cl_ <- nflt0[grepl(st, nflt_, ignore.case = ignore.case)] # regex match of sets names (find all comm* etc.) for regex match selection
                for (k in cl_) { # loop over sets for regex filtration
                  kk <- kk & grepl(flt_[[paste0(k, "_")]], dat[[k]],
                    ignore.case = ignore.case
                  )
                }
                cl <- nflt[grepl(st, nflt, ignore.case = ignore.case)] # regex match of sets names (find all comm* etc.) for exact match selection
                for (k in cl) { # loop over sets/columns for exact filtration
                  kk2 <- rep(FALSE, length(kk))
                  for (h in flt[[k]]) { # loop over filtration vector
                    kk2 <- kk2 | (dat[[k]] == h)
                  }
                  kk <- kk & kk2
                }
              }
            } else {
              if (verbose) cat("   ", pv, " has no data.\n")
            }
          }
          if (!is.null(dat)) {
            if (anyDuplicatedSets(dat)) dat <- rename_duplicated_sets(dat)
            dkk <- dat |>
              collect() |>
              filter(kk)
            if (!is.null(dkk) && nrow(dkk) > 0) {
              # The base year and the projection are separate optimisations,
              # so their objectives do not union into anything -- the same
              # refusal getData.recursive() makes. Every other variable does.
              if (!is.null(src_of) && identical(src_of[s], "base") &&
                  identical(pv, "vObjective")) {
                next
              }
              nkk <- sum(kk)
              tagcols <- data.frame(
                scenario = rep(sc, nkk),
                name = rep(pv, nkk)
              )
              # `run`, not `variant`: `variant` already means a technology's
              # vintage/cluster provenance here (modInp@sets$variant, the
              # `variants =` argument, levcost), while `run` is free -- it is
              # not a model dimension.
              if (!is.null(run_of)) {
                tagcols$run <- rep(run_of[s], nkk)
              }
              # which solve the row came from: "solved" or "base"
              if (!is.null(src_of)) {
                tagcols$source <- rep(src_of[s], nkk)
              }
              dat <- dplyr::bind_cols(tagcols, dkk)
              le <- length(ll) + 1
              nm_ll <- names(ll)
              nm_le <- if (!is.null(src_of)) {
                # a scenario and its base year share the scenario's name
                paste(sc, src_of[s], pv, sep = ".")
              } else if (!is.null(run_of)) {
                # several runs of one scenario share its name, so the run has
                # to be in the element name or merge = FALSE returns a list
                # whose names do not identify its elements
                paste(sc, run_of[s], pv, sep = ".")
              } else if (scenNameInList) {
                paste(sc, pv, sep = ".")
              } else {
                pv
              }
              ll[[le]] <- dat
              names(ll) <- c(nm_ll, nm_le)
            }
          }
        }
      }
    }
  }


  force_format <- function(x) {
    # converts sets-columns to strings, year to integer
    cnames <- colnames(x)
    for (j in 1:length(cnames)) {
      if (cnames[j] == "value") {
        x[[j]] <- as.numeric(x[[j]])
      } else if (cnames[j] == "year") {
        x[[j]] <- as.integer(x[[j]])
      } else {
        x[[j]] <- as.character(x[[j]])
      }
    }
    x
  }
  ll <- lapply(ll, force_format) # Workaround for merging of inconsistent formats

  # Round
  if (!is.null(digits)) {
    stopifnot(is.numeric(digits))
    ll <- lapply(ll, function(x) {
      mutate(x, value = round(value, digits = digits))
    })
  }

  # Drop zeros
  if (drop.zeros) {
    ll <- lapply(ll, function(x) {
      x <- filter(x, value != 0)
      if (nrow(x) == 0) {
        return(NULL)
      }
      return(x)
    })
  }

  ii <- sapply(ll, is.null)
  if (all(ii)) {
    ll <- list()
  } else {
    ll <- ll[!ii]
  }

  # Renaming sets
  if (!is.null(newNames)) {
    ll <- lapply(ll, function(x) renameSets(x, newNames))
  }

  # Technology variant provenance: attach `base` / `vintage` / `cluster` from
  # `modInp@sets$tech_variant` so results can be analysed (or rolled up) by
  # vintage and cluster without ever parsing the mangled variant names.
  if (isTRUE(variants) && length(ll) > 0) {
    tv <- tryCatch(object[[1]]@modInp@sets$variant, error = function(e) NULL)
    if (is.null(tv) || NROW(tv) == 0) {
      if (verbose) message("No technology variants in this scenario.")
    } else {
      ll <- lapply(ll, function(x) .attach_variants(x, as.data.frame(tv)))
    }
  }

  # Sub-annual time aggregation (timeslice roll-up), see `timeframe` argument.
  if (length(ll) > 0) {
    cal <- tryCatch(object[[1]]@settings@calendar, error = function(e) NULL)
    ycal <- tryCatch(getYearCalendars(object[[1]]), error = function(e) NULL)
    if (!is.null(cal)) ll <- .apply_timeframe(ll, cal, timeframe, ycal)
  }

  # Spatial roll-up, see `geoframe`. Inert without a geoscale, and
  # `geoframe = "finest"` (the default) leaves results at native resolution --
  # so this changes nothing unless asked for.
  if (length(ll) > 0 && !identical(tolower(as.character(geoframe)[1]), "finest")) {
    hier <- tryCatch(.scen_geo_hierarchy(object[[1]]), error = function(e) NULL)
    # corridor-keyed capacity variables carry no region column; their endpoints
    # come from `mTradeRoutes`. NULL where the model has no trade.
    rts <- tryCatch(.trade_routes_tbl(object[[1]]), error = function(e) NULL)
    if (!is.null(hier)) ll <- .apply_geoframe(ll, hier, geoframe, routes = rts)
  }

  if (merge) {
    if (length(ll) == 1) {
      dat <- ll[[1]]
    } else if (length(ll) > 1) {
      dat <- ll[[1]]
      for (i in 2:length(ll)) {
        suppressMessages(
          suppressWarnings({
            dat <- dplyr::full_join(dat, ll[[i]])
          })
        )
      }
    } else {
      dat <- .getdata_empty(merge = TRUE, asTibble = asTibble)
    }
    if (!is.null(dat) && ncol(dat) > 0) {
      if (na.rm) {
        ii <- rowSums(apply(dat, 2, is.na))
        dat <- dat[!ii, ]
      }
      if (stringsAsFactors) {
        for (i in 1:length(names(dat))) {
          if (is.character(dat[[i]])) {
            dat[[i]] <- .crs2fct(dat[[i]])
          }
        }
      } else {
        for (i in 1:length(names(dat))) {
          if (is.factor(dat[[i]])) {
            dat[[i]] <- as.character(dat[[i]])
          }
        }
      }
      if (!is.null(dat$year)) {
        if (yearsAsFactors) {
          dat$year <- .crs2fct(dat$year, labels = .year_label_safe(object))
        } else {
          dat$year <- .crs2int(dat$year)
        }
      }
      if (asTibble) {
        dat <- tibble::as_tibble(dat)
      }
    }
    if (!is.null(newValues)) {
      dat <- revalueSets(dat, newValues)
    }
    return(dat)
  } else {
    if (length(ll) > 0) {
      for (i in 1:length(ll)) {
        if (!is.null(ll[[i]]$year)) {
          if (yearsAsFactors) {
            if (!is(ll[[i]]$year, "factor")) {
              ll[[i]]$year <- .crs2fct(ll[[i]]$year,
                                       labels = .year_label_safe(object))
            }
          } else {
            ll[[i]]$year <- .crs2int(ll[[i]]$year)
          }
        }
        if (stringsAsFactors) {
          colnam <- names(ll[[i]])[sapply(ll[[i]], is.character)]
          for (j in colnam) {
            ll[[i]][[j]] <- .crs2fct(ll[[i]][[j]])
          }
        } else {
          colnam <- names(ll[[i]])[sapply(ll[[i]], is.factor)]
          for (j in colnam) {
            ll[[i]][[j]] <- as.character(ll[[i]][[j]])
          }
        }
        if (asTibble) ll[[i]] <- tibble::as_tibble(ll[[i]])
        if (!is.null(newValues)) {
          ll[[i]] <- revalueSets(ll[[i]], newValues)
        }
      }
    }
    return(ll)
  }
}

#' A list of scenarios dispatches through the same scenario extractor (its body
#' already handles a list of scenarios).
#' @rdname getData
#' @method getData list
#' @export
getData.list <- getData.scenario

#' Scenario NAMES resolve through the registry-backed [getScenario()] cache:
#' `getData(c("base", "policy"), name = "vTechOut", merge = TRUE)` needs no
#' explicit loading — first use loads the thin scenario objects into `.scen`, repeat
#' calls are free.
#' @rdname getData
#' @method getData character
#' @export
getData.character <- function(object, ...) {
  scens <- lapply(object, getScenario)
  names(scens) <- object
  getData.scenario(scens, ...)
}

#' An environment of scenarios (e.g. the session bank `.scen`) is read as a
#' list: `getData(.scen, name = "vTechOut", merge = TRUE)`.
#' @rdname getData
#' @method getData environment
#' @export
getData.environment <- function(object, ...) {
  getData.scenario(as.list(object), ...)
}

#' @rdname getData
#' @method getData default
#' @export
getData.default <- function(object, ...) {
  stop(
    "getData() has no method for class '", paste(class(object), collapse = "/"),
    "'. Supported: scenario (or list of scenarios), model, repository, and ",
    "model objects (technology, commodity, storage, supply, demand, trade).",
    call. = FALSE
  )
}

# ---- getData helpers: empty result + timeframe (timeslice) aggregation ----------

# Uniform "nothing found" return: an (empty) data.frame when merge = TRUE,
# an empty list otherwise. Keeps `merge = TRUE` from ever returning NULL.
.getdata_empty <- function(merge, asTibble = TRUE) {
  if (!isTRUE(merge)) {
    return(list())
  }
  if (isTRUE(asTibble)) tibble::tibble() else data.frame()
}

# Canonical timeslice -> integer rank (1 = ANNUAL / coarsest; larger = finer),
# derived from the number of ancestors each timeslice has in the calendar.
.timeslice_rank_map <- function(calendar) {
  timeslices <- calendar@timeslice_share$timeslice
  anc <- calendar@timeslice_ancestry # columns: parent (ancestor), child
  if (is.null(anc) || nrow(anc) == 0 || length(timeslices) == 0) {
    return(stats::setNames(rep(1L, length(timeslices)), timeslices))
  }
  nanc <- table(factor(anc$child, levels = timeslices))
  stats::setNames(as.integer(nanc) + 1L, timeslices)
}

# Map every canonical timeslice to its representative at `target_rank`:
#   finer timeslices -> their ancestor at target_rank; at-or-coarser -> themselves.
.timeslice_target_map <- function(calendar, target_rank) {
  rk <- .timeslice_rank_map(calendar)
  anc <- calendar@timeslice_ancestry
  out <- character(0)
  if (!is.null(anc) && nrow(anc) > 0) {
    keep <- rk[anc$parent] == target_rank # ancestors sitting exactly at target
    out <- stats::setNames(anc$parent[keep], anc$child[keep])
  }
  self <- names(rk)[rk <= target_rank] # at or coarser than target: keep as-is
  c(out, stats::setNames(self, self))
}

# State/level variables whose timeslice dimension is a snapshot, not a flow: summing
# them over timeslices is meaningless, so timeframe roll-up leaves them at native
# resolution. Read off the variable specification (`role: stock` in
# data-raw/variables.yml) rather than being listed here, so a new state variable
# is covered by declaring what it is.
.is_state_var <- function(nm) {
  spec <- tryCatch(.variables, error = function(e) NULL)
  if (is.null(spec)) return(rep(FALSE, length(nm)))
  vapply(nm, function(v) {
    identical(tryCatch(spec[[v]]$role, error = function(e) NULL), "stock")
  }, logical(1), USE.NAMES = FALSE)
}

# The declared recast rule for the one variable a result frame holds. Each
# element of `getData()`'s `ll` is one variable, tagged by a `name` column.
# No `name` column means the caller assembled the frame itself: keep the
# pre-0.92 behaviour and roll it as a quantity.
#' @noRd
.df_variable_rule <- function(df, axis, default = "sum") {
  if (!("name" %in% names(df))) return(default)
  nm <- unique(as.character(df$name))
  if (length(nm) != 1L) return(NA_character_)   # mixed: leave it alone
  r <- .get_variable_recast_rule(nm, axis)
  unname(r[1])
}

# `last`: the closing value of each target slice, with the swing inside it
# recorded. A level is a snapshot, so it is never summed over slices -- but the
# peak and trough the coarse slice hides are what a reader loses, so they ride
# on the result as `attr(, "absorbed")`.
#' @noRd
.rollup_last_df <- function(df, calendar, tgt, same) {
  # the timetable is chronological, so its row order IS the order within a
  # target slice
  ord <- match(as.character(df$timeslice),
               as.character(calendar@timetable$timeslice))
  ord[is.na(ord)] <- 0L
  grp <- setdiff(names(df), c("value", "timeslice"))
  d <- df
  d$.tgt <- tgt
  d$.ord <- ord
  keys <- do.call(paste, c(d[, c(grp, ".tgt"), drop = FALSE], sep = "\r"))
  d$.key <- keys
  # the swing that the roll-up hides, before it is hidden
  absorbed <- stats::aggregate(list(value = d$value), by = list(.key = d$.key),
                               FUN = function(x) c(min(x), max(x), length(x)))
  ab <- data.frame(.key = absorbed$.key,
                   level_min = absorbed$value[, 1],
                   level_max = absorbed$value[, 2],
                   slices = absorbed$value[, 3],
                   stringsAsFactors = FALSE)
  pick <- d[order(d$.key, d$.ord), , drop = FALSE]
  pick <- pick[!duplicated(pick$.key, fromLast = TRUE), , drop = FALSE]
  ab <- merge(ab, pick[, c(".key", grp, ".tgt"), drop = FALSE], by = ".key")
  ab$.key <- NULL
  names(ab)[names(ab) == ".tgt"] <- "timeslice"
  out <- pick
  out$timeslice <- out$.tgt
  out <- out[, names(df), drop = FALSE]
  rownames(out) <- NULL
  attr(out, "absorbed") <- list(rule = "last", in_slice_swing = ab)
  out
}

# `sum_external`: a row whose two endpoints land in the same target region is
# no longer a transfer. Those rows leave the total and are recorded as
# in-region flow; the rest are summed per new endpoint pair.
#
# The endpoints are the frame's own two region columns -- `src`/`dst` for
# `vTradeIr`, whose dims repeat `region` so the writer renames them.
#' @noRd
# Corridor -> (src, dst) from `mTradeRoutes`, for the capacity variables that
# carry no region column of their own. Returns NULL when the scenario has no
# trade, which is the common case.
#' @noRd
.trade_routes_tbl <- function(scen) {
  p <- tryCatch(scen@modInp@parameters[["mTradeRoutes"]],
                error = function(e) NULL)
  if (is.null(p)) return(NULL)
  d <- tryCatch(as.data.frame(get_data_slot(p, optional = TRUE)),
                error = function(e) NULL)
  if (!is.data.frame(d) || !nrow(d) ||
        !all(c("trade", "src", "dst") %in% names(d))) {
    return(NULL)
  }
  d[, c("trade", "src", "dst"), drop = FALSE]
}

# `sum_external` for a frame keyed by CORRIDOR rather than by region. A
# corridor is external when at least one of its routes still crosses the new
# border (ruled: it then keeps its full capacity, since the capacity is one
# number for the corridor and not per route). One whose every route became
# internal leaves the total and is recorded.
#' @noRd
.rollup_sum_external_trade <- function(df, map, routes) {
  if (is.null(routes) || !("trade" %in% names(df))) return(df)
  s <- unname(map[as.character(routes$src)])
  d <- unname(map[as.character(routes$dst)])
  s[is.na(s)] <- as.character(routes$src)[is.na(s)]
  d[is.na(d)] <- as.character(routes$dst)[is.na(d)]
  crosses <- s != d
  # any route crossing makes the corridor external
  ext <- unique(as.character(routes$trade)[crosses])
  seen <- unique(as.character(routes$trade))
  keep_rows <- as.character(df$trade) %in% ext
  # a corridor with no route at all in the table is left alone rather than
  # assumed internal
  unknown <- !(as.character(df$trade) %in% seen)
  keep_rows <- keep_rows | unknown
  if (all(keep_rows)) return(df)
  keep <- df[keep_rows, , drop = FALSE]
  gone <- df[!keep_rows, , drop = FALSE]
  rownames(keep) <- NULL
  rownames(gone) <- NULL
  attr(keep, "absorbed") <- list(rule = "sum_external",
                                 in_region_capacity = gone)
  keep
}

.rollup_sum_external_df <- function(df, map) {
  pair <- intersect(c("src", "dst"), names(df))
  if (length(pair) != 2L) return(df)   # not pair-keyed; see the trade variant
  s <- unname(map[as.character(df$src)])
  d <- unname(map[as.character(df$dst)])
  s[is.na(s)] <- as.character(df$src)[is.na(s)]
  d[is.na(d)] <- as.character(df$dst)[is.na(d)]
  if (all(s == as.character(df$src)) && all(d == as.character(df$dst))) {
    return(df)
  }
  out <- df
  out$src <- s
  out$dst <- d
  internal <- out$src == out$dst
  grp <- setdiff(names(out), "value")
  agg <- function(x) {
    if (!nrow(x)) return(x[0, , drop = FALSE])
    r <- x |>
      dplyr::group_by(dplyr::across(dplyr::all_of(grp))) |>
      dplyr::summarise(value = sum(value), .groups = "drop")
    as.data.frame(r)
  }
  keep <- agg(out[!internal, , drop = FALSE])
  gone <- agg(out[internal, , drop = FALSE])
  attr(keep, "absorbed") <- list(rule = "sum_external", in_region_flow = gone)
  keep
}

# Aggregate one data.frame to `target_rank` by summing `value` over child timeslices.
# Left-join the variant provenance onto a result frame. The process id column is
# `tech` / `stg` / `trade` depending on the class, or `process` when
# `getData(process = TRUE)` has renamed it -- so the join key is whichever of
# those the frame carries, not `tech` alone.
.attach_variants <- function(x, tv) {
  # Native id column first, `process` last: getData(process = TRUE) renames
  # them all to `process`, so the specific names must win when present.
  col <- intersect(c(.process_dims, "process"), names(x))
  if (length(col) == 0L) return(x)
  col <- col[1]
  if (any(c("base", "vintage", "cluster") %in% names(x))) return(x)
  tv <- tv |>
    select(all_of(c("name", "class", "base", "vintage", "cluster"))) |>
    rename("{col}" := "name")
  left_join(x, tv, by = col)
}

.aggregate_timeframe_df <- function(df, calendar, target_rank,
                                     year_calendars = NULL) {
  # A milestone solved on a SAMPLE of the model calendar rolls up with its own
  # weights. The ratio below is child-weight over target-weight, and a sampled
  # season carries a larger weight than a full-calendar one (it stands for
  # more of the year), so reading the model calendar's weights for every year
  # would understate a sampled year's annual total by exactly its
  # `year_fraction`. NULL -- no milestone naming a calendar -- skips this
  # entirely, so nothing changes for a model that declares none.
  if (!is.null(year_calendars) && nrow(df) &&
        "year" %in% names(df) && length(unique(df$year)) > 1L) {
    ys <- as.character(df$year)
    if (any(ys %in% names(year_calendars))) {
      pieces <- lapply(split(seq_len(nrow(df)), ys), function(i) {
        cl <- year_calendars[[ys[i[1]]]] %||% calendar
        .aggregate_timeframe_df(df[i, , drop = FALSE], cl, target_rank)
      })
      return(dplyr::bind_rows(pieces))
    }
  }
  if (is.na(target_rank)) {
    return(df)
  }
  if (!("timeslice" %in% names(df)) || !("value" %in% names(df))) {
    return(df)
  }
  # The declared rule decides. `sum` is the weight-ratio sum below; `last`
  # is a level's closing value; NA means the variable is not indexed on this
  # axis, and `weighted_mean` / `copy` are in the vocabulary but declared by no
  # variable, so they are left alone rather than guessed at.
  rule <- .df_variable_rule(df, "time")
  if (is.na(rule) || !(rule %in% c("sum", "last"))) {
    return(df)
  }
  map <- .timeslice_target_map(calendar, target_rank)
  tgt <- unname(map[as.character(df$timeslice)])
  na <- is.na(tgt)
  tgt[na] <- as.character(df$timeslice)[na] # unknown timeslices: leave untouched
  # A folded parameter stores an unset timeslice as NA ("all timeslices"). Both sides of
  # the comparison are then NA, so `all()` returned NA and `if` errored -- guard
  # it: an NA timeslice has nothing to roll up.
  same <- tgt == as.character(df$timeslice)
  same[is.na(same)] <- TRUE
  if (all(same)) {
    return(df) # nothing to roll up
  }
  if (identical(rule, "last")) {
    return(.rollup_last_df(df, calendar, tgt, same))
  }
  # Timeframe contract: rolled-up values carry the child/target weight ratio
  # (1 within sub-annual levels and on full calendars; 1/year_fraction when
  # the target is the top slice of a sampled calendar), matching the LP's own
  # coarse-level totals (pTimesliceAgg in eqOutTot/eqInpTot). Rows ALREADY at
  # the target level are the LP's own aggregated cells; they take precedence
  # and their children are not rolled on top (which would double-count).
  w <- stats::setNames(as.numeric(calendar@timeslice_share$weight),
                       as.character(calendar@timeslice_share$timeslice))
  ratio <- w[as.character(df$timeslice)] / w[tgt]
  ratio[!is.finite(ratio)] <- 1
  grp <- setdiff(names(df), "value")
  at_tgt <- df[same, , drop = FALSE]
  ch <- df[!same, , drop = FALSE]
  ch$value <- ch$value * ratio[!same]
  ch$timeslice <- tgt[!same]
  if (nrow(at_tgt) > 0 && nrow(ch) > 0) {
    key <- function(x) do.call(paste, c(x[, grp, drop = FALSE], sep = "\r"))
    ch <- ch[!(key(ch) %in% key(at_tgt)), , drop = FALSE]
  }
  out <- rbind(at_tgt, ch) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(grp))) |>
    dplyr::summarise(value = sum(value), .groups = "drop")
  as.data.frame(out)
}

# ---- geoframe (region roll-up): the spatial twin of the block above --------
#
# Same shape as `.timeslice_rank_map()` / `.aggregate_timeframe_df()`, with two
# deliberate differences:
#
#  * rank comes from the geoscale (1 = coarsest), not from ancestry counting;
#  * the variable exempted from roll-up is different. Summing a STOCK over
#    timeslices is meaningless (it double-counts time), so timeframe roll-up skips
#    state variables. Summing a stock over REGIONS is perfectly meaningful
#    (total storage in a zone), but summing an INTER-REGIONAL FLOW is not --
#    trade between two regions of the same zone cancels rather than adds. So
#    the spatial exemption is flow variables, read off the same catalogue.

# region -> its ancestor at `target_rank`, or itself when already at or coarser.
#' @noRd
.region_target_map <- function(hier, target_rank) {
  lvl <- hier$levels
  rank <- stats::setNames(seq_along(lvl), lvl)
  out <- character(0)
  for (lv in names(hier$members)) {
    if (is.na(rank[lv]) || rank[[lv]] < target_rank) {
      # already at or coarser than the target: keep as-is
      m <- hier$members[[lv]]
      out <- c(out, stats::setNames(m, m))
    }
  }
  target_members <- hier$members[[lvl[target_rank]]]
  if (is.null(target_members)) return(out)
  # walk each finer region up to its ancestor at the target level
  fam <- hier$family # region = parent, regionp = child
  up <- stats::setNames(as.character(fam$region), as.character(fam$regionp))
  for (r in unlist(hier$members[rank[names(hier$members)] >= target_rank],
                   use.names = FALSE)) {
    cur <- r
    for (i in seq_along(lvl)) {
      if (cur %in% target_members) break
      nxt <- up[[cur]]
      if (is.null(nxt) || is.na(nxt)) { cur <- NA_character_; break }
      cur <- nxt
    }
    if (!is.na(cur) && cur %in% target_members) out[r] <- cur
  }
  out
}

#' @noRd
.aggregate_geoframe_df <- function(df, hier, target_rank, routes = NULL) {
  if (is.na(target_rank)) return(df)
  if (!("value" %in% names(df))) return(df)
  rule <- .df_variable_rule(df, "space")
  # A corridor-keyed frame has neither `region` nor `src`/`dst`: its endpoints
  # live in `mTradeRoutes`. Handled before the `region` guard, which would
  # otherwise return first.
  if (identical(rule, "sum_external") && !("region" %in% names(df)) &&
        !all(c("src", "dst") %in% names(df))) {
    if (is.null(routes)) return(df)
    return(.rollup_sum_external_trade(df, .region_target_map(hier, target_rank),
                                      routes))
  }
  if (!("region" %in% names(df))) {
    # `src`/`dst`-keyed frames are handled below; anything else is untouched
    if (!all(c("src", "dst") %in% names(df))) return(df)
  }
  if (is.na(rule) || !(rule %in% c("sum", "sum_external"))) return(df)
  map <- .region_target_map(hier, target_rank)
  if (identical(rule, "sum_external")) {
    # an exchange, not a quantity: mapping both ends to a common parent would
    # total flows that are internal to it. Those are recorded, not summed.
    return(.rollup_sum_external_df(df, map))
  }
  tgt <- unname(map[as.character(df$region)])
  na <- is.na(tgt)
  tgt[na] <- as.character(df$region)[na] # unknown regions: leave untouched
  same <- tgt == as.character(df$region)
  same[is.na(same)] <- TRUE
  if (all(same)) return(df)
  df$region <- tgt
  grp <- setdiff(names(df), "value")
  out <- df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(grp))) |>
    dplyr::summarise(value = sum(value), .groups = "drop")
  as.data.frame(out)
}

# Apply the requested `geoframe` to a list of result data.frames.
#   "finest" -> native (unchanged); "coarsest" -> the top level;
#   "all"    -> native + every coarser aggregate, stacked;
#   <level>  -> aggregate to that named geoscale level.
#' @noRd
.apply_geoframe <- function(ll, hier, geoframe, routes = NULL) {
  if (length(ll) == 0 || is.null(hier)) return(ll)
  lvl <- hier$levels
  if (length(lvl) <= 1) return(ll)
  gl <- as.character(geoframe)[1]
  if (identical(tolower(gl), "finest")) return(ll)

  if (identical(tolower(gl), "all")) {
    out <- lapply(ll, function(df) {
      if (!("value" %in% names(df))) return(df)
      pieces <- lapply(seq_along(lvl), function(r) {
        .aggregate_geoframe_df(df, hier, r, routes = routes)
      })
      dplyr::distinct(dplyr::bind_rows(pieces))
    })
    names(out) <- names(ll)
    return(out)
  }

  target_rank <- if (gl %in% lvl) {
    which(lvl == gl)[1]
  } else if (identical(tolower(gl), "coarsest")) {
    1L
  } else {
    stop("Unknown 'geoframe' = '", gl, "'. Use 'coarsest', 'finest', 'all', ",
         "or a geoscale level name: ", paste(lvl, collapse = ", "))
  }
  lapply(ll, function(df)
    .aggregate_geoframe_df(df, hier, target_rank, routes = routes))
}

# Apply the requested `timeframe` to a list of result data.frames.
#   "highest" -> native (unchanged); "lowest" -> coarsest (ANNUAL);
#   "all"     -> native + every coarser aggregate, stacked;
#   <level>   -> aggregate to that named calendar level.
.apply_timeframe <- function(ll, calendar, timeframe,
                             year_calendars = NULL) {
  if (length(ll) == 0) {
    return(ll)
  }
  rank <- calendar@timeframe_rank
  if (length(rank) <= 1) {
    return(ll)
  } # single-level calendar: nothing to aggregate
  tf <- as.character(timeframe)[1]
  if (identical(tolower(tf), "highest")) {
    return(ll)
  }

  if (identical(tolower(tf), "all")) {
    ranks <- sort(unique(as.integer(rank)), decreasing = TRUE)
    out <- lapply(ll, function(df) {
      if (!("timeslice" %in% names(df)) || !("value" %in% names(df))) {
        return(df)
      }
      pieces <- lapply(ranks, function(r)
        .aggregate_timeframe_df(df, calendar, r, year_calendars))
      dplyr::distinct(dplyr::bind_rows(pieces))
    })
    names(out) <- names(ll)
    return(out)
  }

  # keyword ("lowest") or an explicit calendar level name
  target_rank <- if (tf %in% names(rank)) {
    as.integer(rank[[tf]])
  } else if (identical(tolower(tf), "lowest")) {
    as.integer(min(rank))
  } else {
    stop(
      "Unknown 'timeframe' = '", tf, "'. Use 'lowest', 'highest', 'all', ",
      "or a calendar level name: ", paste(names(rank), collapse = ", ")
    )
  }
  lapply(ll, function(df)
    .aggregate_timeframe_df(df, calendar, target_rank, year_calendars))
}


# The id column each of the seven process classes contributes. `process` is the
# union of exactly these classes (`sets$process`, R/interpolation.R), so at most
# one of these columns occurs in any one frame -- which is what lets them all
# rename to `process`. `impp` is the alias spelling of `imp`.
#
# This is MEMBERSHIP. Do not fold purpose-built subsets into it: e.g.
# `.cl_groupable` (R/process-groups.R) omits `demand` because demand is not
# clusterable, which is correct for that job and wrong here.
.process_dims <- c("tech", "stg", "trade", "sup", "dem", "imp", "impp", "expp")

.crs2int <- function(x) {
  # coerce to integer from factor or character
  if (is(x, "factor")) x <- as.character(x)
  if (is(x, "character")) x <- as.integer(x)
  x
}

.crs2fct <- function(x, levels = NULL, ordered = TRUE, labels = NULL) {
  if (!is.null(labels) && length(labels)) {
    key <- as.character(x)
    out <- unname(labels[key])
    miss <- is.na(out)
    if (any(miss)) out[miss] <- key[miss]
    lv <- unique(c(unname(labels), out[miss]))
    x <- factor(out, levels = lv)
    return(if (ordered) as.ordered(x) else x)
  }
  if (is(x, "factor")) return(x)
  if (!is.character(x)) x <- as.character(x)
  if (!is.null(levels)) {
    x <- factor(x, levels = levels)
  } else {
    x <- as.factor(x)
  }
  if (ordered) x <- as.ordered(x)
  x
}

# Replacement for `plyr::revalue()`. `plyr` is retired and was kept in Imports
# by these two call sites alone, while dplyr -- already imported -- covers
# everything else. Same contract: character or factor in, same type out; the
# NAMES of `replace` are the old values; anything absent is left untouched
# (plyr's `warn_missing = FALSE`). Verified identical to plyr::revalue over
# character/factor, no-match, repeated-value, level-merging, NA and empty cases.
#' @noRd
.revalue <- function(x, replace) {
  if (is.null(replace) || length(replace) == 0L) return(x)
  repl <- as.character(replace)
  if (is.factor(x)) {
    lv <- levels(x)
    i <- match(names(replace), lv)
    ok <- !is.na(i)
    lv[i[ok]] <- repl[ok]
    levels(x) <- lv
    x
  } else {
    i <- match(x, names(replace))
    ok <- !is.na(i)
    x[ok] <- repl[i[ok]]
    x
  }
}

#' Rename data.frame columns of list of data.frames.
#'
#' @param x a data.frame or a list with data frames.
#' @param newNames named character vector or list with new names as values, and old names as names.
#'
#' @return depending on input, the renamed data.frame or the list with renamed data.frames.
#' @export renameSets
#' @examples
#' \dontrun{
#' x <- data.frame(a = letters, n = 1:length(letters))
#' x
#' renameSets(x[1:3, ], c(a = "A", n = "N"))
#' renameSets(x[1:3, ], list(a = "B", n = "M"))
#' }
renameSets <- function(x, newNames = NULL) {
  if (any(class(x) == "list")) {
    returnList <- TRUE
  } else {
    returnList <- FALSE
    x <- list(x)
  }
  x <- lapply(x, function(y) {
    nms <- names(y)
    if (is.null(nms)) {
      y
    } else {
      nms <- .revalue(nms, newNames)
      names(y) <- nms
      y
    }
  })
  if (returnList) {
    x
  } else {
    x[[1]]
  }
}


#' Replace specified values with new values in factor or character columns of a data.frame.
#'
#' @param x vector
#' @param newValues a names list with named vectors. The names of the list should be equal to the names of the data.frame columns in wich values will be replaced. The named vector should have new names as values and old values as names.
#'
#' @return the x data.frame with revalued variables.
#' @export revalueSets
#' @examples
#' \dontrun{
#' x <- data.frame(a = letters, n = 1:length(letters))
#' nw1 <- LETTERS[1:10]
#' names(nw1) <- letters[1:10]
#' nw2 <- formatC(1:9, width = 3, flag = "0")
#' names(nw2) <- 1:9
#' newValues <- list(a = nw1, n = nw2)
#' newValues
#' revalueSets(x, newValues)
#' }
revalueSets <- function(x, newValues = NULL) {
  stopifnot(any(class(newValues) == "list"))
  stopifnot(any(class(x) == "data.frame"))
  nnms <- names(newValues)
  xnms <- names(x)
  jj <- xnms %in% nnms
  for (j in xnms[jj]) {
    x[[j]] <- .revalue(x[[j]], newValues[[j]])
  }
  x
}


.getNames <- function(
    obj, cls, regex = NULL, ignore.case = FALSE,
    fixed = FALSE, useBytes = FALSE, invert = FALSE, ...) {
  if (is.null(regex)) {
    grep2 <- function(x, y, FL) {
      if (FL) {
        grep(x, as.character(y),
          ignore.case = ignore.case, fixed = fixed,
          useBytes = useBytes, invert = invert
        )
      } else {
        y %in% x
      }
    }
  } else if (regex) {
    grep2 <- function(x, y, FL) {
      grep(x, as.character(y),
        ignore.case = ignore.case, fixed = fixed,
        useBytes = useBytes, invert = invert
      )
    }
  } else {
    grep2 <- function(x, y, FL) y %in% x
  }
  arg <- list(...)
  if (any(class(obj) == "scenario")) obj <- obj@model
  if (any(class(obj) == "repository")) {
    obj <- add(new("model"), obj)
  }
  if (is.null(cls)) {
    lst <- list()
    cls <- unique(c(
      lapply(
        obj@data,
        function(xx) unique(sapply(xx@data, class))
      ),
      recursive = TRUE
    ))
    for (cl in cls) {
      ll <- .getNames(obj, cl,
        regex = regex, ignore.case = ignore.case, fixed = fixed,
        useBytes = useBytes, invert = invert, ...
      )
      for (i in seq(along = ll)) {
        lst[[names(ll)[i]]] <- ll[[i]]
      }
    }
    lst
  } else {
    rst <- data.frame(rp = numeric(), ob = numeric(), use = logical())
    for (i in seq(along = obj@data)) {
      jj <- seq(along = obj@data[[i]]@data)[sapply(obj@data[[i]]@data, class) == cls]
      if (length(jj) != 0) {
        nn <- nrow(rst) + 1:length(jj)
        rst[nn, ] <- NA
        rst[nn, "rp"] <- i
        rst[nn, "ob"] <- jj
        rst[nn, "use"] <- TRUE
      }
    }
    s1 <- getSlots(cls)
    s2 <- new(cls)
    FL <- rep(FALSE, length(arg))
    FL[grep("[_]$", names(arg))] <- TRUE
    names(arg) <- gsub("[_]$", "", names(arg))
    names(FL) <- names(arg)
    for (a in seq(along = arg)) {
      if (all(names(s1) != names(arg)[a])) {
        rst <- rst[0, , drop = FALSE]
      } else {
        if (nrow(rst) > 0) {
          error_msg <- paste('.getNames: undefined condition argument "',
            names(arg)[a], '" for class "', cls, '"',
            sep = ""
          )
          nm <- names(arg)[a]
          cnd <- arg[[a]]
          if (s1[nm] == "list") stop(error_msg)
          if (s1[nm] %in% c("character", "factor")) {
            # Character
            if (!(class(cnd) %in% c("character", "factor"))) stop(error_msg)
            for (i in seq(length.out = nrow(rst))) {
              rst[i, "use"] <- any(
                grep2(
                  cnd,
                  slot(obj@data[[rst[i, 1]]]@data[[rst[i, 2]]], nm),
                  FL[nm]
                )
              )
            }
            rst <- rst[rst$use, , drop = FALSE]
          } else if (s1[nm] == "logical") {
            # Logical
            if (!is(cnd, "logical")) stop(error_msg)
            for (i in seq(length.out = nrow(rst))) {
              rst[i, "use"] <- any(
                cnd == slot(obj@data[[rst[i, 1]]]@data[[rst[i, 2]]], nm),
                na.rm = TRUE
              )
            }
            rst <- rst[rst$use, , drop = FALSE]
          } else if (s1[nm] == "numeric") {
            # Numeric
            if (!(class(cnd) %in% c("integer", "numeric"))) stop(error_msg)
            if (is.null(names(cnd)) && length(cnd) > 2) stop(error_msg)
            if (is.null(names(cnd)) && length(cnd) == 2) {
              names(cnd) <- c("ge", "le")
            }
            if (is.null(names(cnd)) && length(cnd) == 1) names(cnd) <- "e"
            if (any(!(names(cnd) %in% c("l", "le", "e", "ge", "g", "ne")))) {
              stop(error_msg)
            }
            if (any(names(cnd) == "le")) {
              for (i in seq(length.out = nrow(rst))) {
                rst[i, "use"] <- any(
                  cnd["le"] >= slot(
                    obj@data[[rst[i, 1]]]@data[[rst[i, 2]]],
                    nm
                  ),
                  na.rm = TRUE
                )
              }
              rst <- rst[rst$use, , drop = FALSE]
            }
            if (any(names(cnd) == "l")) {
              for (i in seq(length.out = nrow(rst))) {
                rst[i, "use"] <- any(
                  cnd["l"] > slot(obj@data[[rst[i, 1]]]@data[[rst[i, 2]]], nm),
                  na.rm = TRUE
                )
              }
              rst <- rst[rst$use, , drop = FALSE]
            }
            if (any(names(cnd) == "e")) {
              for (i in seq(length.out = nrow(rst))) {
                rst[i, "use"] <- any(
                  cnd["e"] == slot(obj@data[[rst[i, 1]]]@data[[rst[i, 2]]], nm),
                  na.rm = TRUE
                )
              }
              rst <- rst[rst$use, , drop = FALSE]
            }
            if (any(names(cnd) == "ge")) {
              for (i in seq(length.out = nrow(rst))) {
                rst[i, "use"] <- any(
                  cnd["ge"] <= slot(
                    obj@data[[rst[i, 1]]]@data[[rst[i, 2]]],
                    nm
                  ),
                  na.rm = TRUE
                )
              }
              rst <- rst[rst$use, , drop = FALSE]
            }
            if (any(names(cnd) == "g")) {
              for (i in seq(length.out = nrow(rst))) {
                rst[i, "use"] <- any(
                  cnd["g"] < slot(obj@data[[rst[i, 1]]]@data[[rst[i, 2]]], nm),
                  na.rm = TRUE
                )
              }
              rst <- rst[rst$use, , drop = FALSE]
            }
            if (any(names(cnd) == "ne")) {
              for (i in seq(length.out = nrow(rst))) {
                rst[i, "use"] <- any(
                  cnd["ne"] != slot(
                    obj@data[[rst[i, 1]]]@data[[rst[i, 2]]],
                    nm
                  ),
                  na.rm = TRUE
                )
              }
              rst <- rst[rst$use, , drop = FALSE]
            }
          } else if (s1[nm] == "data.frame") {
            # data.frame
            FL2 <- rep(FALSE, length(cnd))
            FL2[grep("[_]$", names(cnd))] <- TRUE
            names(cnd) <- gsub("[_]$", "", names(cnd))
            names(FL2) <- names(cnd)
            for (nm2 in names(cnd)) {
              cnd2 <- cnd[[nm2]]
              if (all(colnames(slot(s2, nm)) != nm2)) stop(error_msg)
              # Character
              if (inherits(cnd2, c("character", "factor"))) {
                if (!inherits(cnd2, c("character", "factor"))) {
                  stop(error_msg)
                }
                for (i in seq(length.out = nrow(rst))) {
                  rst[i, "use"] <- any(
                    grep2(
                      cnd2,
                      slot(obj@data[[rst[i, 1]]]@data[[rst[i, 2]]], nm)[[nm2]],
                      FL2[nm2]
                    ),
                    na.rm = TRUE
                  )
                }
                rst <- rst[rst$use, , drop = FALSE]
              } else if (is(cnd2, "logical")) {
                # Logical
                if (!is(cnd2, "logical")) stop(error_msg)
                for (i in seq(length.out = nrow(rst))) {
                  rst[i, "use"] <-
                    any(
                      cnd == slot(
                        obj@data[[rst[i, 1]]]@data[[rst[i, 2]]],
                        nm
                      )[[nm2]],
                      na.rm = TRUE
                    )
                }
                rst <- rst[rst$use, , drop = FALSE]
              } else if (is(cnd2, "numeric")) {
                # Numeric
                if (!(class(slot(s2, nm)[[nm2]]) %in% c("integer", "numeric"))) {
                  stop(error_msg)
                }
                if (is.null(names(cnd2)) && length(cnd2) > 2) stop(error_msg)
                if (is.null(names(cnd2)) && length(cnd2) == 2) {
                  names(cnd2) <- c("ge", "le")
                }
                if (is.null(names(cnd2)) && length(cnd2) == 1) {
                  names(cnd2) <- "e"
                }
                if (any(!(names(cnd2) %in% c("l", "le", "e", "ge", "g", "ne")))) {
                  stop(error_msg)
                }
                if (any(names(cnd2) == "le")) {
                  for (i in seq(length.out = nrow(rst))) {
                    rst[i, "use"] <- any(
                      cnd2["le"] >=
                        slot(obj@data[[rst[i, 1]]]@data[[rst[i, 2]]], nm)[[nm2]],
                      na.rm = TRUE
                    )
                  }
                  rst <- rst[rst$use, , drop = FALSE]
                }
                if (any(names(cnd2) == "l")) {
                  for (i in seq(length.out = nrow(rst))) {
                    rst[i, "use"] <- any(
                      cnd2["l"] > slot(
                        obj@data[[rst[i, 1]]]@data[[rst[i, 2]]],
                        nm
                      )[[nm2]],
                      na.rm = TRUE
                    )
                  }
                  rst <- rst[rst$use, , drop = FALSE]
                }
                if (any(names(cnd2) == "e")) {
                  for (i in seq(length.out = nrow(rst))) {
                    rst[i, "use"] <- any(
                      cnd2["e"] == slot(
                        obj@data[[rst[i, 1]]]@data[[rst[i, 2]]],
                        nm
                      )[[nm2]],
                      na.rm = TRUE
                    )
                  }
                  rst <- rst[rst$use, , drop = FALSE]
                }
                if (any(names(cnd2) == "ge")) {
                  for (i in seq(length.out = nrow(rst))) {
                    rst[i, "use"] <- any(
                      cnd2["ge"] <= slot(
                        obj@data[[rst[i, 1]]]@data[[rst[i, 2]]],
                        nm
                      )[[nm2]],
                      na.rm = TRUE
                    )
                  }
                  rst <- rst[rst$use, , drop = FALSE]
                }
                if (any(names(cnd2) == "g")) {
                  for (i in seq(length.out = nrow(rst))) {
                    rst[i, "use"] <- any(
                      cnd2["g"] < slot(
                        obj@data[[rst[i, 1]]]@data[[rst[i, 2]]],
                        nm
                      )[[nm2]],
                      na.rm = TRUE
                    )
                  }
                  rst <- rst[rst$use, , drop = FALSE]
                }
                if (any(names(cnd2) == "ne")) {
                  for (i in seq(length.out = nrow(rst))) {
                    rst[i, "use"] <- any(
                      cnd2["ne"] != slot(
                        obj@data[[rst[i, 1]]]@data[[rst[i, 2]]],
                        nm
                      )[[nm2]],
                      na.rm = TRUE
                    )
                  }
                  rst <- rst[rst$use, , drop = FALSE]
                }
              } else {
                stop(error_msg)
              }
            }
          }
        }
      }
    }
    nn <- list()
    for (i in seq(length.out = nrow(rst))) {
      nn[[obj@data[[rst[i, 1]]]@data[[rst[i, 2]]]@name]] <-
        obj@data[[rst[i, 1]]]@data[[rst[i, 2]]]
    }
    nn
  }
}

getNames <- function(obj, class = c(), regex = NULL, ...) {
  names(.getNames(obj, cls = class, regex = regex, ...))
}

getObjects <- function(obj, class = c(), regex = NULL, ...) {
  .getNames(obj, cls = class, regex = regex, ...)
}


# ---------------------------------------------------------------------------
# (was R/get_object.R, merged 2026-09-20)
# ---------------------------------------------------------------------------

# get_object.R -- retrieve model *objects* (commodities, technologies, ...) from
# a repository / model / scenario, filtered by class, name, description and
# region. The object-level counterpart of getData() (which returns their data).
# Reuses the internal getObjects()/.getNames() engine (class/name/slot filters,
# R/get_data.R) and adds a class-agnostic region filter via get_region().

#' Retrieve model objects from a repository, model or scenario
#'
#' `getObject()` returns the model building-block objects held in a container --
#' commodities, technologies, supplies, storages, and so on -- selected by
#' **class**, **name**, **description**, **region** and/or any object **slot**.
#' It is the object-level counterpart of [getData()], which returns those
#' objects' parameter data.
#'
#' Class, name, description and any other slot are matched by the same engine
#' that powers the internal object accessors; the **region** filter is applied
#' with [get_region()], which reads regions uniformly from `@region` slots and
#' from the `region`/`src`/`dst` columns of data.frame slots, so it works for
#' every class (including `import`/`export`/`trade`, which store region
#' structurally). A scenario is unwrapped to its model automatically.
#'
#' @param x a `repository`, `model` or `scenario`.
#' @param class character vector of object classes to keep (e.g. `"technology"`,
#'   `c("supply", "import")`); `NULL` (default) keeps all classes.
#' @param name character, object name(s) to match against `@name`.
#' @param desc character, description pattern(s) to match against `@desc`.
#' @param region character, keep objects belonging to any of these regions.
#'   Region-agnostic objects (e.g. commodities, which carry no region) are kept
#'   for every region unless `region_agnostic = FALSE`.
#' @param ... additional per-slot filters forwarded to the matching engine, e.g.
#'   `timeframe = "HOUR"`. Character filters are exact by default; a data.frame
#'   slot is filtered by named columns. Filtering by a slot implicitly restricts
#'   results to classes that have it.
#' @param regex logical; treat `name`, `desc` and character `...` filters as
#'   regular expressions instead of exact matches (default `FALSE`).
#' @param ignore.case logical; case-insensitive matching for `regex = TRUE`
#'   (default `TRUE`).
#' @param region_agnostic logical; whether objects with no region information
#'   satisfy a `region` filter (default `TRUE`).
#' @param drop logical; if `TRUE` and exactly one object matches, return that
#'   object itself instead of a one-element list (default `FALSE`).
#' @param verbose logical; report progress (the `en_registry` method).
#'
#' @return A named list of model objects keyed by `@name` (empty list if none
#'   match); or a single object when `drop = TRUE` and exactly one matches.
#' @seealso [getData()], [get_region()], [find_in_model()]
#' @examples
#' \dontrun{
#' repo <- topia$modules$electricity$repo
#' getObject(repo, class = "technology")                 # all technologies
#' getObject(repo, class = c("supply", "commodity"))     # two classes
#' getObject(repo, region = "R1")                         # everything in R1
#' getObject(repo, name = "ECOA", drop = TRUE)            # the ECOA object
#' getObject(repo, desc = "coal", regex = TRUE)           # by description
#' getObject(repo, class = "technology", timeframe = "HOUR")  # slot filter
#' }
#' @export
getObject <- function(x, ...) UseMethod("getObject")

#' @rdname getObject
#' @export
getObject.default <- function(x, ...) {
  stop("getObject() is defined for 'repository', 'model' and 'scenario' ",
       "objects; got '", class(x)[1], "'.", call. = FALSE)
}

.getObject_container <- function(x, class = NULL, name = NULL, desc = NULL,
                                 region = NULL, ..., regex = FALSE,
                                 ignore.case = TRUE, region_agnostic = TRUE,
                                 drop = FALSE) {
  flt <- list(...)
  if (!is.null(name)) flt[["name"]] <- as.character(name)
  if (!is.null(desc)) flt[["desc"]] <- as.character(desc)
  rgx <- if (isTRUE(regex)) TRUE else NULL

  pull <- function(cl) do.call(getObjects, c(
    list(obj = x, class = cl, regex = rgx, ignore.case = ignore.case), flt))

  # getObjects()/.getNames() matches a single class at a time; loop for many.
  if (is.null(class) || length(class) == 0L) {
    objs <- pull(c())
  } else {
    objs <- list()
    for (cl in class) objs <- c(objs, pull(cl))
  }

  # class-agnostic region filter via get_region()
  if (!is.null(region) && length(objs)) {
    region <- as.character(region)
    keep <- vapply(objs, function(o) {
      r <- get_region(o)
      if (length(r) == 0L) isTRUE(region_agnostic) else
        length(intersect(region, r)) > 0L
    }, logical(1))
    objs <- objs[keep]
  }

  if (isTRUE(drop) && length(objs) == 1L) return(objs[[1L]])
  objs
}

#' @rdname getObject
#' @export
getObject.repository <- .getObject_container

#' @rdname getObject
#' @export
getObject.model <- .getObject_container

#' @rdname getObject
#' @export
getObject.scenario <- .getObject_container

# -- registry ------------------------------------------------------------------
# A registry indexes SAVED objects, so getObject() on one loads what the rows
# point at. Names are unique only within a type (a model and a scenario may
# both be "TOPIA", and run labels repeat across scenarios), so the returned
# list is keyed `type/name` rather than by bare name -- otherwise same-named
# objects of different types would overwrite each other.

#' @rdname getObject
#' @param type character, registry row type(s) to load: "model", "repository",
#'   "scenario", "dataset". `NULL` (default) loads every loadable type.
#' @param hash character, content hash (prefix) selecting a stored version.
#' @param parent,variant character, extra registry filters (run bookkeeping).
#' @param run which run to activate in loaded scenarios: `"main"` (default)
#'   keeps the scenario's own run, a character selects that run, and `NULL`,
#'   `NA` or `"ALL"` returns one entry per recorded run, keyed
#'   `scenario/<name>:<variant>/<label>`.
#' @export
getObject.en_registry <- function(x, type = NULL, name = NULL, hash = NULL,
                                  parent = NULL, variant = NULL,
                                  run = "main", drop = FALSE,
                                  verbose = FALSE, ...) {
  rows <- find_in_registry(x, type = type, name = name, hash = hash,
                           parent = parent, variant = variant)
  # run rows are bookkeeping inside a scenario, not standalone objects; they
  # are reachable through `run =` below, or scenario_runs()
  rows <- rows[rows$type != "run", , drop = FALSE]
  if (!nrow(rows)) {
    if (isTRUE(drop)) {
      stop("No registry entry matches; nothing to return with drop = TRUE.",
           call. = FALSE)
    }
    return(list())
  }

  all_runs <- .run_is_all(run)
  out <- list()
  for (i in seq_len(nrow(rows))) {
    r <- rows[i, , drop = FALSE]
    obj <- .registry_row_object(r, verbose = verbose)
    key <- paste0(r$type, "/", r$name)
    if (identical(r$type, "scenario") && !identical(run, "main")) {
      if (all_runs) {
        rr <- tryCatch(scenario_runs(obj), error = function(e) NULL)
        if (is.null(rr) || !nrow(rr)) { out[[key]] <- obj; next }
        for (j in seq_len(nrow(rr))) {
          sc <- tryCatch(read_solution(obj, run = rr$run[j], verbose = FALSE),
                         error = function(e) NULL)
          if (is.null(sc)) next
          out[[paste0(key, ":", rr$run[j])]] <- sc
        }
        next
      }
      obj <- read_solution(obj, run = run, verbose = FALSE)
      key <- paste0(key, ":", run)
    }
    out[[key]] <- obj
  }

  if (isTRUE(drop)) {
    if (length(out) != 1L) {
      stop("drop = TRUE needs exactly one match; got ", length(out),
           if (length(out)) paste0(": ", paste(names(out), collapse = ", "))
           else "", ".\n  Narrow with type=/name=/hash=, or use drop = FALSE.",
           call. = FALSE)
    }
    return(out[[1]])
  }
  out
}


# ---------------------------------------------------------------------------
# (was R/get_data_objects.R, merged 2026-09-21)
# ---------------------------------------------------------------------------

# =============================================================================#
# get_data_objects.R -- getData() methods for model *objects* (raw input data).
#
# getData(scenario) returns processed/solved data with a uniform (name, dims,
# value) schema (see get_data.R). The methods here return the RAW input stored
# in a model object's data.frame slots (pre-interpolation): technology,
# commodity, storage, supply, demand, trade, ... and, via containers, model and
# repository. With `interpolate = TRUE` the year-bearing slots are expanded over
# the target milestone years using each parameter's own default interpolation
# rule -- handy for quick plots that demonstrate what interpolation will do.
# =============================================================================#

# Object class -> its canonical set-dimension name (column that carries the
# object's name in the tidy output). Falls back to the class name itself.
.class_set_dim <- c(
  technology = "tech", commodity = "comm", storage = "stg",
  supply = "sup", demand = "dem", trade = "trade",
  import = "imp", export = "expp", weather = "weather",
  # both policy classes share one label space
  tax = "levy", subsidy = "levy"
)

# Known set (index) dimensions -- everything else numeric is a "value" column.
.known_set_dims <- c(
  "region", "regionp", "year", "yearp", "timeslice", "timeslicep", "comm", "commp",
  "acomm", "group", "tech", "techp", "stg", "sup", "dem", "trade", "expp",
  "imp", "impp", "weather", "src", "dst", "process", "levy"
)

# Reverse map (memoised): "class\rslot\rcolName" -> list(rule, defVal,
# param, dims). An index over `.col_param`, the baked column <-> parameter
# relation (built by data-raw/col_param.R): one row per value column of every
# slot, with the parameter that owns it already resolved through class
# inheritance (`settings` reads config's slots) and `colAlias` (`pDemand` is
# catalogued under colName "dem" while the column of `demand@demand` is
# "demand"). A `bounds` parameter contributes its three `<colName>.lo/.up/.fx`
# columns, each with its own rule and default (`.pack_bounds_long()` naming).
.param_interp_map_cache <- new.env(parent = emptyenv())

.param_interp_map <- function() {
  if (!is.null(.param_interp_map_cache$map)) {
    return(.param_interp_map_cache$map)
  }
  cp <- tryCatch(.col_param, error = function(e) NULL)
  if (!is.data.frame(cp)) return(list())
  # slot-level rows (a scalar slot like `technology@cap2act`) carry no
  # column, so they are not keys of this map
  cp <- cp[!is.na(cp$parameter) & !is.na(cp$column), , drop = FALSE]
  out <- stats::setNames(
    lapply(seq_len(nrow(cp)), function(i) {
      list(rule = cp$rule[i], defVal = cp$defVal[i],
           param = cp$parameter[i], dims = cp$dims[[i]])
    }),
    paste(cp$class, cp$slot, cp$column, sep = "\r")
  )
  .param_interp_map_cache$map <- out
  out
}

# All mapped value columns of one (class, slot): named list col -> entry.
# Used by autoplot(show_defaults = TRUE) to know which parameters CAN exist
# in a slot and what their defaults are.
.slot_param_cols <- function(cls, slot_name) {
  mp <- .param_interp_map()
  if (length(mp) == 0) return(list())
  keys <- strsplit(names(mp), "\r", fixed = TRUE)
  hit <- vapply(keys, function(k)
    length(k) == 3 && k[1] == cls && k[2] == slot_name, logical(1))
  out <- mp[hit]
  names(out) <- vapply(keys[hit], `[[`, "", 3L)
  out
}

# Apply the default (per-parameter) interpolation rule to every year-bearing
# value column of one slot data.frame, over `years` (target milestones). Value
# columns without a mapped rule fall back to "back.inter.forth"/default.
.interp_object_slot <- function(cls, slot_name, d, years, dim_nm) {
  if (!("year" %in% names(d)) || nrow(d) == 0) {
    return(d)
  }
  val_cols <- names(d)[vapply(d, is.numeric, logical(1))]
  val_cols <- setdiff(val_cols, .known_set_dims) # drops year + set dims
  # only interpolate GIVEN data -- an all-NA column would otherwise be
  # materialised at its registry default (that is show_defaults' job)
  val_cols <- val_cols[vapply(val_cols, function(vc) any(!is.na(d[[vc]])),
                              logical(1))]
  if (length(val_cols) == 0) {
    return(d)
  }
  yrs <- years
  if (is.null(yrs)) {
    yy <- suppressWarnings(as.integer(d$year))
    yy <- yy[!is.na(yy)]
    if (length(yy) == 0) {
      return(d)
    }
    yrs <- if (length(unique(yy)) > 1) seq(min(yy), max(yy)) else unique(yy)
  }
  mp <- .param_interp_map()
  set_cols <- setdiff(intersect(names(d), .known_set_dims), "year")
  res <- NULL
  for (vc in val_cols) {
    info <- mp[[paste(cls, slot_name, vc, sep = "\r")]]
    rule <- .interp_rule_token(if (!is.null(info)) info$rule else "back.inter.forth")
    dv <- if (!is.null(info)) info$defVal[1] else NA_real_
    raw <- d[, c(set_cols, "year", vc), drop = FALSE]
    names(raw)[names(raw) == vc] <- "value"
    raw$year <- suppressWarnings(as.integer(raw$year))
    ser <- tryCatch(
      .interp_one_series(raw, set_cols, yrs, rule, dv),
      error = function(e) NULL
    )
    if (is.null(ser) || nrow(ser) == 0) next
    names(ser)[names(ser) == "value"] <- vc
    res <- if (is.null(res)) {
      ser
    } else {
      suppressMessages(dplyr::full_join(res, ser))
    }
  }
  if (is.null(res)) {
    return(d)
  }
  if (!is.na(dim_nm) && dim_nm %in% names(d) && !(dim_nm %in% names(res))) {
    res[[dim_nm]] <- unique(d[[dim_nm]])[1]
  }
  as.data.frame(res)
}

# Filter a slot data.frame by `...` selectors: `col = values` (exact) and
# `col_ = pattern` (regex). Unknown columns are ignored. A NULL / empty
# selector means "no filter" -- it must never match nothing (a `year = NULL`
# passed through `...` used to drop EVERY row, which made all the object
# autoplots report "No year-indexed data").
.filter_object_df <- function(d, flt, ignore.case = TRUE) {
  if (length(flt) > 0) {
    flt <- flt[!vapply(flt, function(x) is.null(x) || length(x) == 0,
                       logical(1))]
  }
  if (length(flt) == 0 || nrow(d) == 0) {
    return(d)
  }
  keep <- rep(TRUE, nrow(d))
  for (k in names(flt)) {
    rgx <- grepl("_$", k)
    col <- sub("_$", "", k)
    if (!(col %in% names(d))) next
    if (rgx) {
      keep <- keep & grepl(flt[[k]], as.character(d[[col]]), ignore.case = ignore.case)
    } else {
      keep <- keep & (as.character(d[[col]]) %in% as.character(flt[[k]]))
    }
  }
  d[keep, , drop = FALSE]
}

# Pivot one slot data.frame to long tidy form: (set cols..., param, slot, value).
# Value columns that are entirely NA (unfilled sub-parameters) and NA-value rows
# are dropped so a merged/plotting frame carries only real data.
.slot_to_long <- function(d, slot_name) {
  set_cols <- intersect(names(d), c(.known_set_dims, "class"))
  val_cols <- setdiff(names(d)[vapply(d, is.numeric, logical(1))], .known_set_dims)
  val_cols <- val_cols[vapply(val_cols, function(vc) any(!is.na(d[[vc]])), logical(1))]
  if (length(val_cols) == 0) {
    return(NULL)
  }
  parts <- lapply(val_cols, function(vc) {
    x <- d[, c(set_cols, vc), drop = FALSE]
    names(x)[names(x) == vc] <- "value"
    x <- x[!is.na(x$value), , drop = FALSE]
    if (nrow(x) == 0) {
      return(NULL)
    }
    x$param <- vc
    x$slot <- slot_name
    x
  })
  dplyr::bind_rows(parts)
}

# Core object extractor shared by all brick methods.
.getData_object <- function(obj, name = NULL, ..., merge = FALSE,
                            interpolate = FALSE, years = NULL, process = FALSE,
                            asTibble = TRUE, ignore.case = TRUE, verbose = FALSE) {
  cls <- class(obj)[1]
  dim_nm <- unname(.class_set_dim[cls])
  if (is.na(dim_nm)) dim_nm <- cls
  obj_name <- tryCatch(obj@name, error = function(e) NA_character_)

  sl_types <- methods::getSlots(cls)
  df_slots <- names(sl_types)[sl_types == "data.frame"]
  if (!is.null(name)) df_slots <- df_slots[df_slots %in% name]
  flt <- list(...)
  # Same contract as the scenario method: a character `process` selects and
  # renames; TRUE only renames. Translated onto the object's own id column,
  # since that is what `.filter_object_df()` sees.
  if (is.character(process)) {
    if (!is.na(dim_nm)) flt[[dim_nm]] <- process
    process <- TRUE
  }
  if (!is.null(flt$process) && !is.na(dim_nm)) {
    flt[[dim_nm]] <- flt$process
    flt$process <- NULL
    process <- TRUE
  }
  if (!is.null(flt$process_) && !is.na(dim_nm)) {
    flt[[paste0(dim_nm, "_")]] <- flt$process_
    flt$process_ <- NULL
    process <- TRUE
  }

  out <- list()
  for (s in df_slots) {
    d <- tryCatch(as.data.frame(methods::slot(obj, s)), error = function(e) NULL)
    if (is.null(d)) next
    if (nrow(d) == 0 && !interpolate) next
    if (nrow(d) > 0 && !is.na(obj_name) && nzchar(obj_name) &&
        !(dim_nm %in% names(d))) {
      d[[dim_nm]] <- obj_name    # 0-row assignment would error (length 1 vs 0)
    }
    if (isTRUE(interpolate)) d <- .interp_object_slot(cls, s, d, years, dim_nm)
    d <- .filter_object_df(d, flt, ignore.case)
    if (is.null(d) || nrow(d) == 0) next
    d$class <- cls
    out[[s]] <- d
  }

  if (isTRUE(process) && !is.na(dim_nm)) {
    out <- lapply(out, function(d) {
      if (dim_nm %in% names(d)) names(d)[names(d) == dim_nm] <- "process"
      d
    })
  }

  if (!merge) {
    if (isTRUE(asTibble)) out <- lapply(out, tibble::as_tibble)
    return(out)
  }
  if (length(out) == 0) {
    return(if (asTibble) tibble::tibble() else data.frame())
  }
  long <- dplyr::bind_rows(lapply(names(out), function(s) .slot_to_long(out[[s]], s)))
  if (isTRUE(asTibble)) long <- tibble::as_tibble(long)
  long
}

# Container extractor: stack the requested slot(s) across all contained objects.
.getData_container <- function(obj, name = NULL, ..., merge = FALSE,
                               interpolate = FALSE, years = NULL, process = FALSE,
                               asTibble = TRUE, ignore.case = TRUE, verbose = FALSE) {
  objs <- tryCatch(getObjects(obj, class = c()), error = function(e) list())
  objs <- objs[!vapply(objs, is.null, logical(1))]
  if (length(objs) == 0) {
    return(if (merge) {
      if (asTibble) tibble::tibble() else data.frame()
    } else {
      list()
    })
  }
  res <- lapply(objs, function(o) {
    tryCatch(
      .getData_object(o,
        name = name, ..., merge = merge, interpolate = interpolate,
        years = years, process = process, asTibble = asTibble,
        ignore.case = ignore.case, verbose = verbose
      ),
      error = function(e) if (merge) NULL else list()
    )
  })
  if (merge) {
    res <- res[!vapply(res, is.null, logical(1))]
    long <- dplyr::bind_rows(res)
    if (isTRUE(asTibble)) long <- tibble::as_tibble(long)
    return(long)
  }
  # named list keyed by object name, dropping objects with no matching data
  res <- res[vapply(res, function(x) length(x) > 0, logical(1))]
  res
}

#' @description
#' Model-object methods (\code{technology}, \code{commodity}, \code{storage},
#' \code{supply}, \code{demand}, \code{trade}) return the object's raw input
#' slot data. \code{model}/\code{repository} stack that across all contained
#' objects. Use \code{name} to select slot(s) (e.g. \code{"invcost"}), \code{...}
#' to filter (\code{region=}, \code{year=}, \code{comm=}, or \code{col_=}regex),
#' \code{merge=TRUE} for one long tidy frame (\code{param}/\code{value}), and
#' \code{interpolate=TRUE} to expand year-bearing slots over \code{years} (or the
#' observed year range) with each parameter's default interpolation rule.
#'
#' @param interpolate if TRUE, expand year-bearing object slots over the target
#'   years using each parameter's default interpolation rule (for quick
#'   demonstration plots). Default FALSE (raw data as stored). Only meaningful
#'   for object/model/repository methods.
#' @param years integer vector of target milestone years for \code{interpolate}
#'   (default: the yearly range observed in the data).
#' @rdname getData
#' @method getData technology
#' @export
getData.technology <- .getData_object
#' @rdname getData
#' @method getData commodity
#' @export
getData.commodity <- .getData_object
#' @rdname getData
#' @method getData storage
#' @export
getData.storage <- .getData_object
#' @rdname getData
#' @method getData supply
#' @export
getData.supply <- .getData_object
#' @rdname getData
#' @method getData demand
#' @export
getData.demand <- .getData_object
#' @rdname getData
#' @method getData trade
#' @export
getData.trade <- .getData_object
#' @rdname getData
#' @method getData import
#' @export
getData.import <- .getData_object
#' @rdname getData
#' @method getData export
#' @export
getData.export <- .getData_object
#' @rdname getData
#' @method getData weather
#' @export
getData.weather <- .getData_object

#' @rdname getData
#' @method getData model
#' @export
getData.model <- .getData_container
#' @rdname getData
#' @method getData repository
#' @export
getData.repository <- .getData_container
