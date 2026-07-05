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
#' @param scen a \code{scenario} (or list of scenarios), a model object, a
#'   \code{model}, or a \code{repository}.
#' @param ... passed to the dispatched method (filters, \code{name}, \code{merge}, ...).
#' @seealso the per-class methods for the full argument list.
#' @export
getData <- function(scen, ...) UseMethod("getData")

#' Performs search for available data in _scenario_ object.
#'
#' @param scen object _scenario_ with model solution.
#' @param dataType type of data to lok for (currently only "parameters" and "variables").
#' @param dropEmpty logical, if TRUE drops parameters and variables with zero length.
#' @param valueColumn logical, if TRUE will return variables and parameters with 'value' column (to filter sets and mappings).
#' @param dfDim logical, if TRUE returns dimension _dim_.
#' @param dfNames logical, when TRUE returns names of the data frame column.
#' @param asMatrix return results as a matrix (not implemented).
#' @param setsNames_ regular expression pattern for names of sets which will be included in search.
#' @param allSets logical, if TRUE _and_ operator should be used in search the sets, _or_ will be used if FALSE.
#' @param ignore.case grepl parameter for matching names.
#'
#' @return list with variables and parameters name, each includes _dim_ and _names_ character vectors.
#'
#' @export
findData <- function(scen,
                     dataType = c("parameters", "variables"),
                     setsNames_ = NULL,
                     valueColumn = TRUE,
                     allSets = TRUE,
                     ignore.case = FALSE,
                     # anyOfTheSets = !allSets,
                     add_weights = "auto",
                     dropEmpty = TRUE,
                     dfDim = TRUE,
                     dfNames = TRUE,
                     asMatrix = FALSE) {
  ll <- lt <- list()
  # browser()
  # 1 Parameters
  ii <- dataType == "parameters"
  if (any(ii)) {
    dataType <- dataType[!ii]
    # dat <- scen@modInp@parameters
    lt <- lapply(scen@modInp@parameters, function(x) {
      # if (dim(x@data)[1] > 0 || !dropEmpty) {
      # browser()
      # cat(x@name, " ")
      # if (x@name == "meqLECActivity") browser()
      qu <- get_lazy_dim_names(x, slot = "data")
      # qu <- get_lazy_data(x, slot = "data")
      # if (nrow(qu) > 0 || !dropEmpty) {
      #     list(
      #     dim = dim(qu),
      #     names = names(qu)
      #   )
      # }
      qu
    })
  }
  # browser()
  ll <- c(ll, lt)

  # 2. Variables
  ii <- dataType == "variables"
  if (any(ii)) {
    lt <- list()
    dataType <- dataType[!ii]
    # dat <- scen@modOut@variables
    # lt <- lapply(dat, function(x) {
    #   if (dim(x)[1] > 0 || !dropEmpty) {
    #     list(
    #       dim = if (dfDim) dim(x) else NULL,
    #       names = if (dfNames) names(x) else NULL
    #     )
    #   }
    # })
    for (v in names(scen@modOut@variables)) {
      # if (v == "vObjective") browser()
      # cat(v, " ")
      qu <- get_lazy_dim_names(scen@modOut, slot = "variables", element = v)
      lt[[v]] <- list(
        dim = qu$dim,
        names = qu$names
      )
      # if (dim(x)[1] > 0 || !dropEmpty) {
      # qu <- get_lazy_data(scen@modOut, slot = "variables", element = v)
      # if (nrow(qu) > 0 || !dropEmpty) {
      #   lt[[v]] <- list(
      #     dim = if (dfDim) dim(qu) else NULL,
      #     names = if (dfNames) names(qu) else NULL
      #   )
      # }
      # })
    }
    ll <- c(ll, lt)
  }
  # browser()
  if (valueColumn) {
    ii <- sapply(ll, function(x) {
      any(grepl("^value$", x$names,
        ignore.case = ignore.case
      ))
    })
    ll <- ll[ii]
  }

  # browser()
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
#' @param scen Object scenario or list of scenarios.
#' @param ... filters for various sets (setname = c(val1, val2) or setname_ = "matching pattern"), see details.
#' @param name character vector with names of parameters and/or variables.
#' @param merge if TRUE, the search results will be merged in one dataframe; the named list will be returned if FALSE. When TRUE, a data.frame (empty if nothing matched) is always returned, never NULL.
#' @param timeframe controls sub-annual time aggregation of results that carry a `slice` column. One of `"lowest"` (default, aggregate/sum flows up to the coarsest level, normally `ANNUAL`), `"highest"` (native/finest, as stored), `"all"` (return every timeframe level stacked), or an explicit calendar level name (e.g. `"SEASON"`, `"YDAY"`) to aggregate to that level. Non-slice data, and state/level variables (e.g. `vStorageStore`) for which summing over slices is meaningless, are returned unchanged.
#' @param process if TRUE, dimensions "tech", "stg", "trade", "imp", "expp", "dem", and "sup" will be renamed with "process".
#' @param parameters if TRUE, parameters will be included in the search and returned if found.
#' @param variables if TRUE, variables will be included in the search and returned if found.
#' @param maps if TRUE, map-type parameters (membership mappings, no `value` column) are also returned.
#' @param na.rm if TRUE, NA values will be dropped.
#' @param digits if integer, indicates the number of decimal places for rounding, if NULL - no actions.
#' @param drop.zeros logical, should rows containing zero values be filtered out.
#' @param asTibble logical, if the data.frames should be converted into tibbles.
#' @param newNames renaming sets, named character vector or list with new names as values, and old names as names - the input parameter to renameSets function. The operation is performed before merging the data (merge parameter).
#' @param newValues revalue sets, named character vector or list with new values as values, and old values as names - the input parameter to revalueSets function. The operation is performed after merging the data (merge parameter).
#' @param ignore.case grepl parameter if regular expressions are used in '...' or 'name_'.
#' @param stringsAsFactors logical, should the sets values be converted to factors?
#' @param yearsAsFactors logical, should `year` be converted to factors? Set 'year' is integer by default.
#' @param scenNameInList logical, should the name of the scenarios be used if not provided in the list with several scenarios?
#' @param verbose logical, print progress and diagnostic messages.
#'
#' @aliases getData get_data
#'
#' @examples
#' \dontrun{
#' data("utopia_scen_BAU.RData")
#' getData(scen, name = "pDemand", year = 2015, merge = TRUE)
#' getData(scen, name = "vTechOut", comm = "ELC", merge = TRUE, year = 2015)
#' elc2050 <- getData(scen, parameters = FALSE, comm = "ELC", year = 2050)
#' names(elc2050)
#' elc2050$vBalance
#' }
#' @method getData scenario
#' @export
getData.scenario <- function(
    scen,
    name = NULL,
    ...,
    merge = FALSE,
    timeframe = c("lowest", "highest", "all"),
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
    # addGroups = list(), summarizeGroups = list(),
    add_weights = "auto",
    add_period_length = "auto",
    apply_weights = FALSE,
    apply_period_length = FALSE,
    asTibble = TRUE,
    as_data_table = FALSE,
    stringsAsFactors = FALSE,
    yearsAsFactors = FALSE,
    drop_duplicated_scenarios = TRUE,
    scenNameInList = as.logical(length(scen) - 1),
    unfold = TRUE,
    verbose = FALSE) {
  # if (name == "vObjective") browser()
  # browser()
  arg <- list(...)
  argnam <- names(arg)
  stopifnot(!any(duplicated(argnam)))
  if (process) {
    stopifnot(length(newNames) == length(unique(newNames)))
    newNamesDefault <- c(
      tech = "process", stg = "process",
      trade = "process", impp = "process",
      imp = "process", expp = "process",
      dem = "process", sup = "process"
    )
    if (!is.null(newNames)) {
      ii <- names(newNamesDefault) %in% names(newNames)
      newNames <- c(newNames, newNamesDefault[!ii])
    } else {
      newNames <- newNamesDefault
    }
  }
  # browser()
  # Select scenarios, check and add names if not provided
  if (!is.list(scen)) {
    scen <- list(scen)
    names(scen) <- scen[[1]]@name
  } else {
    ii <- sapply(scen, class) == "scenario"
    if (sum(ii) == 0) {
      message("Scenario object is not found")
      return(.getdata_empty(merge, asTibble))
    }
    scen <- scen[ii] # keep scenarios only
    nm <- names(scen)
    if (is.null(nm)) nm <- rep("", length(scen))
    ii <- nm == ""
    nm[ii] <- sapply(scen[ii], function(x) x@name) # work on names
    names(scen) <- nm
    ii <- duplicated(nm)
    if (any(ii)) {
      if (drop_duplicated_scenarios) {
        warning("Dropping duplicated scenarios: ", nm[ii])
        scen <- scen[!ii]
      } else {
        if (verbose) cat("Found scenarios with identical names: ", nm[ii], "\n")
      }
    }
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
  for (s in 1:length(scen)) { # loop over scenarios
    sc <- names(scen)[s]
    # Data availability: an un-interpolated scenario has no modInp parameters and
    # an unsolved one has no modOut variables. Skip gracefully instead of erroring.
    avail <- c(
      parameters = tryCatch(length(scen[[s]]@modInp@parameters) > 0,
        error = function(e) FALSE),
      variables = tryCatch(length(scen[[s]]@modOut@variables) > 0,
        error = function(e) FALSE)
    )
    if (verbose && !any(avail)) {
      message("Scenario '", sc, "' has no interpolated/solved data to extract.")
    }
    # if(is.null(scen[[sc]]@modInp@parameters$pDemand@data$comm)) {scen[[sc]] <- .addComm2pDemand(scen[[sc]])}
    for (datype in names(parvar)[parvar]) { # loop over data sources
      if (!isTRUE(avail[[datype]])) {
        if (verbose) cat("No", datype, "available in scenario '", sc, "'.\n")
        next
      }
      if (verbose) cat("Extracting data from", datype, "\n")
      if (length(nflt1) > 0) {
        sets_names <- paste0("^", nflt1, "$")
      } else {
        sets_names <- NULL
      }
      lt <- findData(scen[[s]],
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
          p <- scen[[s]]@modInp@parameters[[nm]]
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
      if (length(nflt1) > 0 & length(clNames) > 0) {
        # Check if provided sets/filters exist
        ii <- nflt1 %in% clNames
        if (!all(ii)) {
          warning(
            "Sets '", paste(nflt1, collapse = "', '"),
            "' have not been found in scenario '", sc, "',", datype, "'."
          )
        }
        # find all matching names of columns
        ii <- sapply(clNames, function(x) {
          any(grepl(x, nflt1, ignore.case = ignore.case))
        })
        clNames <- clNames[ii]
        # browser()
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
            # browser()
            dat <- get_lazy_data(scen[[s]]@modInp@parameters[[pv]],
              slot = "data"
            )
            if (!is.null(dat)) {
              dat <- collect(dat)
            }
            # Unfold wildcard (NA) rows of folded parameters back to explicit
            # members at read time, using the scenario's membership maps.
            if (isTRUE(unfold) && !is.null(dat) && nrow(dat) > 0) {
              fi <- scen[[s]]@modInp@parameters[[pv]]@misc[["fold_info"]]
              if (!is.null(fi) && isTRUE(fi[["folded"]])) {
                dat <- unfold_scenario_parameter(
                  scen[[s]], scen[[s]]@modInp@parameters[[pv]]
                )
              }
            }
            # temporary. ToDo: rewrite filter-algo for lazy-data
            # if (!is.null(scen[[sc]]@modInp@parameters[[pv]])) {
            # if (!is.null(qu) {
            # dat <- scen[[sc]]@modInp@parameters[[pv]]@data
            # if (verbose) cat("   ", pv, "\n")
            # } else {
            # warning("Parameter '", pv, "' was not found.")
            # }
          } else {
            # dat <- scen[[sc]]@modOut@variables[[pv]]
            # browser()
            dat <- get_lazy_data(scen[[s]]@modOut,
              slot = "variables",
              element = pv
            )
            if (!is.null(dat)) {
              # temporary. ToDo: rewrite filter-algo for lazy-data
              dat <- collect(dat)
            }
          }
          dim1 <- dim(dat)[1]
          if (is.null(dim1)) dim1 <- 0
          kk <- rep(TRUE, dim1)
          # browser()
          if (length(nflt1) > 0) { # the data should be filtered
            # browser()
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
              nkk <- sum(kk)
              dat <- dplyr::bind_cols(
                data.frame(
                  scenario = rep(sc, nkk),
                  name = rep(pv, nkk)
                ),
                dkk
              )
              le <- length(ll) + 1
              nm_ll <- names(ll)
              if (scenNameInList) nm_le <- paste(sc, pv, sep = ".") else nm_le <- pv
              ll[[le]] <- dat
              names(ll) <- c(nm_ll, nm_le)
            }
          }
        }
      }
    }
  }

  ## Temporary solution for non-mileStone period data in parameters
  # msy <- scen[[1]]@model@config@horizon@intervals$mid
  # if (length(ll) > 0) {
  #   for (i in 1:length(ll)) {
  #     if (!is.null(ll[[i]]$year)) {
  #       ii <- ll[[i]]$year %in% msy # temporary solution
  #       if (!all(ii)) ll[[i]] <- ll[[i]][ii,] # temporary solution
  #     }
  #   }
  # }

  # browser()
  force_format <- function(x) {
    # converts sets-columns to strings, year to integer
    cnames <- colnames(x)
    # ex <- grepl("value|year")
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
    # for (i in 1:length(ll)) {
    #   ll[[i]] <- renameSets(ll[[i]], newNames)
    # }
    ll <- lapply(ll, function(x) renameSets(x, newNames))
  }

  # Sub-annual time aggregation (slice roll-up), see `timeframe` argument.
  if (length(ll) > 0) {
    cal <- tryCatch(scen[[1]]@settings@calendar, error = function(e) NULL)
    if (!is.null(cal)) ll <- .apply_timeframe(ll, cal, timeframe)
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
          dat$year <- .crs2fct(dat$year)
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
              ll[[i]]$year <- .crs2fct(ll[[i]]$year)
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

#' @rdname getData
#' @method getData default
#' @export
getData.default <- function(scen, ...) {
  stop(
    "getData() has no method for class '", paste(class(scen), collapse = "/"),
    "'. Supported: scenario (or list of scenarios), model, repository, and ",
    "model objects (technology, commodity, storage, supply, demand, trade).",
    call. = FALSE
  )
}

#' @rdname getData
#' @export
get_data <- getData

# ---- getData helpers: empty result + timeframe (slice) aggregation ----------

# Uniform "nothing found" return: an (empty) data.frame when merge = TRUE,
# an empty list otherwise. Keeps `merge = TRUE` from ever returning NULL.
.getdata_empty <- function(merge, asTibble = TRUE) {
  if (!isTRUE(merge)) {
    return(list())
  }
  if (isTRUE(asTibble)) tibble::tibble() else data.frame()
}

# Canonical slice -> integer rank (1 = ANNUAL / coarsest; larger = finer),
# derived from the number of ancestors each slice has in the calendar.
.slice_rank_map <- function(calendar) {
  slices <- calendar@slice_share$slice
  anc <- calendar@slice_ancestry # columns: parent (ancestor), child
  if (is.null(anc) || nrow(anc) == 0 || length(slices) == 0) {
    return(stats::setNames(rep(1L, length(slices)), slices))
  }
  nanc <- table(factor(anc$child, levels = slices))
  stats::setNames(as.integer(nanc) + 1L, slices)
}

# Map every canonical slice to its representative at `target_rank`:
#   finer slices -> their ancestor at target_rank; at-or-coarser -> themselves.
.slice_target_map <- function(calendar, target_rank) {
  rk <- .slice_rank_map(calendar)
  anc <- calendar@slice_ancestry
  out <- character(0)
  if (!is.null(anc) && nrow(anc) > 0) {
    keep <- rk[anc$parent] == target_rank # ancestors sitting exactly at target
    out <- stats::setNames(anc$parent[keep], anc$child[keep])
  }
  self <- names(rk)[rk <= target_rank] # at or coarser than target: keep as-is
  c(out, stats::setNames(self, self))
}

# State/level variables whose slice dimension is a snapshot, not a flow: summing
# them over slices is meaningless, so timeframe roll-up leaves them at native
# resolution. Extend as needed.
.timeframe_state_vars <- c("vStorageStore")

# Aggregate one data.frame to `target_rank` by summing `value` over child slices.
.aggregate_timeframe_df <- function(df, calendar, target_rank) {
  if (is.na(target_rank)) {
    return(df)
  }
  if (!("slice" %in% names(df)) || !("value" %in% names(df))) {
    return(df)
  }
  # never sum a stored-level / state variable across slices
  if ("name" %in% names(df) && any(df$name %in% .timeframe_state_vars)) {
    return(df)
  }
  map <- .slice_target_map(calendar, target_rank)
  tgt <- unname(map[as.character(df$slice)])
  na <- is.na(tgt)
  tgt[na] <- as.character(df$slice)[na] # unknown slices: leave untouched
  if (all(tgt == as.character(df$slice))) {
    return(df) # nothing to roll up
  }
  df$slice <- tgt
  grp <- setdiff(names(df), "value")
  out <- df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(grp))) |>
    dplyr::summarise(value = sum(value), .groups = "drop")
  as.data.frame(out)
}

# Apply the requested `timeframe` to a list of result data.frames.
#   "highest" -> native (unchanged); "lowest" -> coarsest (ANNUAL);
#   "all"     -> native + every coarser aggregate, stacked;
#   <level>   -> aggregate to that named calendar level.
.apply_timeframe <- function(ll, calendar, timeframe) {
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
      if (!("slice" %in% names(df)) || !("value" %in% names(df))) {
        return(df)
      }
      pieces <- lapply(ranks, function(r) .aggregate_timeframe_df(df, calendar, r))
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
  lapply(ll, function(df) .aggregate_timeframe_df(df, calendar, target_rank))
}

if (F) { # test
  load("energyRt_tutorial/data/utopia_scen_BAU.RData")
  (dem <- getData(scen, name = "pDemand", year = 2015, merge = TRUE))
  (vTechOut <- getData(scen, name = "vTechOut", comm = "ELC", merge = TRUE, year = 2015))
  # Storage capacity
  getData(scen, name = "vStorageCap", merge = TRUE)
}

.crs2int <- function(x) {
  # coerce to integer from factor or character
  if (is(x, "factor")) x <- as.character(x)
  if (is(x, "character")) x <- as.integer(x)
  x
}

.crs2fct <- function(x, levels = NULL, ordered = TRUE) {
  # coerce to integer from factor or character
  if (is(x, "character")) {
    if (!is.null(levels)) {
      x <- factor(x, levels = levels)
    } else {
      x <- as.factor(x)
    }
    if (ordered) x <- as.ordered(x)
  }
  x
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
      nms <- plyr::revalue(nms, newNames, warn_missing = FALSE)
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
  # browser()
  jj <- xnms %in% nnms
  for (j in xnms[jj]) {
    x[[j]] <- plyr::revalue(x[[j]], newValues[[j]], warn_missing = FALSE)
  }
  x
}

if (F) { # Check
  library(tidyverse)
  # renameSets
  x <- tibble(a = letters, n = as.character(1:length(letters)))
  x
  renameSets(x, c(a = "A", n = "N"))

  d <- as.data.frame(x)
  renameSets(d, c(a = "A", n = "N"))

  # revalueSets
  nw1 <- LETTERS[1:10]
  names(nw1) <- letters[1:10]
  nw2 <- formatC(1:9, width = 3, flag = "0")
  names(nw2) <- 1:9
  newValues <- list(a = nw1, n = nw2)
  newValues
  revalueSets(x, newValues)
  revalueSets(d, newValues)
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

getNames_ <- function(obj, class = c(), ...) {
  names(.getNames(obj, cls = class, regex = TRUE, ...))
}

getObjects <- function(obj, class = c(), regex = NULL, ...) {
  .getNames(obj, cls = class, regex = regex, ...)
}

getObjects_ <- function(obj, class = c(), ...) {
  .getNames(obj, cls = class, regex = TRUE, ...)
}
