# Some commonly used functions


#' Countdown timer to use in R scripts
#'
#' @param seconds numeric, time in seconds to count down
#' @param warn_message character, warning message to display before the countdown
#' @param start_message character, message to display at the beginning of the countdown
#' @param count_message character, message to display during the countdown
#' @param final_message character, message to display at the end of the countdown
#'
#' @returns NULL
#' @export
#'
#' @examples
#' \dontrun{
#' countdown_timer(10)
#' countdown_timer(10, warn_message = "Something important is going to happen in 10 seconds.")
#' }
countdown_timer <- function(
    seconds,
    warn_message = NULL,
    start_message = "Press Esc (Ctrl+C in terminal) to interrupt the execution.\n",
    count_message = "\rTime remaining: %2d seconds",
    final_message = "-> resuming...\n"
    ) {
  if (!is.numeric(seconds) || seconds < 0) {
    stop("Input must be a positive numeric value.")
  } else if (seconds == 0) {
    return(invisible(NULL))
  }

  if (!is.null(warn_message)) {
    message(warn_message)
  }

  # message("Press Ctrl+C to interrupt the countdown.")
  cat(start_message)

  for (i in seq(seconds, 1, by = -1)) {
    # cat(sprintf("\rTime remaining: %2d seconds", i)) # Replace previous line
    cat(sprintf(count_message, i)) # Replace previous line
    flush.console() # Ensure the message is printed immediately
    Sys.sleep(1)
  }
  i <- 0
  # cat("\rTime remaining:  0 seconds\nCountdown complete!\n") # Final message
  # cat(sprintf("\rTime remaining:  0 seconds\n%s\n", end_message)) # Final message
  cat(sprintf(count_message, i), final_message) # Final message
}

# Example usage
# countdown_timer(10)


factors_in_params <- function(x) {
  # x - list
  # if (inherits(x, "list")) y <- lapply()
  # browser()
  y <- lapply(x, function(y) any(sapply(y@data, class) == "factors"))
  y[unlist(y)]
}

# factors_in_prams(scen_BASE_int@modInp@parameters)

nonchar_in_sets <- function(x) {
  # x - list
  # if (inherits(x, "list")) y <- lapply()
  # browser()
  y <- lapply(x, function(y) any(class(y) != "character"))
  y[unlist(y)]
}
# nonchar_in_sets(scen_BASE_int@modInp@set)
# scen_BASE_int@modInp@set$year |> class()


#' Size of an object
#'
#' @param x any R object
#' @param level1 logical, if TRUE, the function will return the size of the
#' object and its slots (if any)
#' @param units character, units to display the size, default is "auto"
#' @param sort logical, if TRUE, the function will sort the slots by size
#' @param decreasing logical, if TRUE, the function will sort the slots in
#' decreasing order
#' @param byteTol numeric, threshold in bytes to filter the slots
#' @param asNumeric logical, if TRUE, the function will return the size of the
#' object and its slots in bytes
#'
#' @return character value or vector, size of the object or its slots
#' @export
#'
#' @examples
#' size(1)
#' size(rep(1, 1e3))
#' size(rep(1L, 1e3))
size <- function(x, level1 = FALSE, units = "auto", sort = TRUE,
                 decreasing = FALSE, byteTol = 0, asNumeric = FALSE) {
  # browser()
  if (!level1) {
    format(object.size(x), units = units)
  } else {
    if (isS4(x)) { # S4
      slx <- slotNames(x)
      val <- lapply(slx, function(z) {
        object.size(slot(x, z))
      })
      names(val) <- slx
      # return(val)
    } else if (is.list(x)) {
      val <- lapply(x, function(z) {
        object.size(z)
      })
    } else {
      format(object.size(x), units = units)
    }
    vv <- lapply(val, as.numeric) # in bytes
    if (sort) {
      ii <- order(unlist(vv), decreasing = decreasing)
      val <- val[ii]
      vv <- vv[ii]
    }
    if (asNumeric) {
      val <- vv
    } else {
      val <- lapply(val, function(z) {
        format(z, units = units)
      })
    }
    # browser()
    ii <- vv >= byteTol
    val[ii]
  }
}

if (F) { # Check
  size(scen, 1, "Mb", byteTol = 1024)
  size(scen@modInp, 1, "Mb", byteTol = 1024)
  size(scen@modInp@parameters, 1, "Mb", byteTol = 1024 * 1000)
  size(scen@modInp@parameters$pTradeIrEff, 1, "Mb", byteTol = 1024 * 1000)
  size(scen@modInp@parameters$pTradeIrEff@data, 1, "Mb", byteTol = 0, asNumeric = TRUE)
  head(scen@modInp@parameters$pTradeIrEff@data)
}

dir_size <- function(path) {
  if (!dir.exists(path)) {
    stop("Directory '", path, "' does not exist")
  }
  files <- list.files(path, recursive = TRUE, full.names = TRUE)
  sizes <- file.size(files)
  # sum(file.info(list.files(".", all.files = TRUE, recursive = TRUE))$size)
  return(sum(sizes))
}

.fix_path <- function(x) {
  # gsub("[\\/]+", "/", paste0(x, "/"))
  gsub("[\\/]+", "/", x)
}

fp <- function(...) {
  file.path(...) |> .fix_path()
    # normalizePath(winslash = "/", mustWork = FALSE)
}


#' Check validity of object's names used in sets
#'
#' @param x character, name of an object of `energyRt`
#'
#' @return logical, TRUE if the name is valid.
#' @export
#'
#' @examples
#' check_name("name")
#' check_name("1name")
#' check_name("name1")
#' check_name("name_1")
#' check_name("name_1!")
check_name <- function(x) {
  (length(x) != 1 || !is.character(x) ||
    sub("^[[:alpha:]][[:alnum:]_]*$", "", x) == "")
}

#' Function to find duplicated values in interpolated scenario.
#'
#' @param x scenario or data.frame with data to check.
#'
#' @return data.frame with duplicated values.
#' @export
#'
#' @examples
#' \dontrun{
#' findDuplicates(scen_BASE)
#' }
findDuplicates <- function(x) {
  if (is(x, 'scenario')) {
    rs <- NULL
    for (pr in names(x@modInp@parameters))
      if (x@modInp@parameters[[pr]]@type %in% c('numpar', 'bounds')) {
        tmp <- x@modInp@parameters[[pr]]@data
        tmp <- tmp[, -ncol(tmp), drop = FALSE]
        fl <- duplicated(tmp)
        if (any(fl)) {
          tmp <- tmp[fl,, drop = FALSE]
          tmp$parameter <- pr
          tmp <- tmp[, c(ncol(tmp), 1:(ncol(tmp) - 1)), drop = FALSE]
          rs <- rbind(rs, tmp)
        }
      }
    if (!is.null(rs)) {
      cat(paste0("Found ", length(unique(rs$parameter)),
                 " tables with duplicates, ", nrow(rs),
                 " duplicated rows in total\n"))
      return(invisible(rs))
    }
  }
  findDuplicates0 <- function(x) {
    check_by_slots <- function(x, slt_name) {
      rs <- NULL
      for (i in slt_name) {
        slt <- slot(x, i)
        set_slot <- colnames(slt)[
          colnames(slt) %in% c('acomm',
                               .set_al[
                                 !(.set_al %in% c('dem'))
                               ])]
        value_slot <- colnames(slt)[!(colnames(slt) %in% set_slot)]
        fl <- !is.na(slt[, value_slot, drop = FALSE])
        if (any(fl)) {
          for (j in value_slot[apply(fl, 2, any)]) {
            f2 <- duplicated(slt[fl[, j], set_slot, drop = FALSE])
            if (any(f2)) {
              rs <- rbind(rs, data.frame(slot = i, parameter = j,
                                         value = sum(f2),
                                         stringsAsFactors = FALSE))
            }
          }
        }
      }
      return(rs)
    }
    res <- data.frame(repository = character(), object = character(),
                      slot = character(), parameter = character(),
                      stringsAsFactors = FALSE)
    if (is(x, 'model')) {
      rs <- NULL
      for (i in seq_along(x@data)) {
        tmp <- findDuplicates0(x@data[[i]])
        if (!is.null(tmp)) {
          tmp$repository <- x@data[[i]]@name
          rs <- rbind(rs, tmp)
        }
      }
      tmp <- findDuplicates0(x@config)
      if (!is.null(tmp)) {
        tmp$repository <- '-'
        tmp$object <- 'config'
        rs <- rbind(rs, tmp[, c(ncol(tmp), 2:ncol(tmp) - 1)])
      }
      if (is.null(rs)) return(NULL)
      return(rs[, c(ncol(rs), 1:(ncol(rs) - 1))])
    } else
      if (is(x, 'repository')) {
        rs <- NULL
        for (i in seq_along(x@data)) {
          tmp <- findDuplicates0(x@data[[i]])
          if (!is.null(tmp)) {
            tmp$object <- x@data[[i]]@name
            rs <- rbind(rs, tmp)
          }
        }
        if (is.null(rs)) return(NULL)
        return(rs[, c(ncol(rs), 1:(ncol(rs) - 1))])
      } else
        if (inherits(x, c('tax', 'sub', 'weather', 'supply',
                            'import', 'export', 'trade', 'technology',
                            'demand', 'storage'))) {
          slt_name <- getSlots(class(x))
          slt_name <- names(slt_name)[
            slt_name == 'data.frame' &
              !(names(slt_name) %in% c('input', 'output', 'aux'))]
          return(check_by_slots(x, slt_name))
        } else if (is(x, c('constraint'))) {
          tmp <- check_by_slots(x, c('rhs', 'for.each'))
          for (y in seq_along(x@lhs)) {
            nn <- check_by_slots(x@lhs[[y]], 'mult')
            if (!is.null(nn)) {
              nn$slot <- paste('lhs', y, nn$slot)
              tmp <- rbind(tmp, nn)
            }
          }
          return(tmp)
        } else if (is(x, "costs")) {
          tmp <- check_by_slots(x, c('for.sum', 'for.each', 'mult'))
          return(tmp)
        } else if (inherits(x, c('slice', 'commodity'))) {
        } else if (is(x, 'config')) {
          return(check_by_slots(x, c('debug', 'discount')))
        } else warning(paste0('Unknown class "', class(x), '"'))
    NULL
  }
  rs <- findDuplicates0(x)
  if (!is.null(rs)) {
    # cat(paste0("There are ", nrow(rs), " duplicates, sum of values: ", sum(rs$value), "\n"))
    cat(paste0("Found ", nrow(rs), " tables with duplicates,",
               sum(rs$value), "duplicated rows in total\n"))
    return(invisible(rs))
  }

}

fact2char <- function(df, asTibble = TRUE) {
  stopifnot(is.data.frame(df))
  jj <- sapply(df, is.factor)
  for (j in names(df)[jj]) {
    df[[j]] <- as.character(df[[j]])
  }
  if (asTibble) {df <- as_tibble(df)}
  df
}

#' Switch on/off and select/customize progress bar
#'
#' @param type character, type of the progress bar to display. Existing options:
#' "bw", "default", "cli", "progress".
#' @param show logical, the progress bar is visible if `TRUE`.
#' @param clear logical, sets `progressr.clear` global option. If `TRUE`, all outout from the progress bar will be cleared.
#'
#' @rdname progress
#' @return sets the progress bar and returns `NULL`
#' @export
#'
#' @examples
#' \dontrun{
#' set_progress_bar("bw")
#' set_progress_bar("default")
#' set_progress_bar("cli")
#' set_progress_bar("progress")
#' set_progress_bar("pbcol")
#' }
set_progress_bar <- function(type = "bw", show = TRUE, clear = FALSE) {
  if (interactive()) progressr::handlers(global = show) # results a warning
  options(progressr.clear = clear)
  if (is.null(type)) return(invisible(NULL))
  if (type == "bw") {
    progressr::handlers(
      progressr::handler_pbcol(
        # adjust = 1.0,
        # complete = function(s) cli::bg_br_green(cli::col_br_black(s)),
        complete = function(s) cli::bg_black(cli::col_white(s)),
        # complete = function(s) cli::bg_br_black(cli::col_silver(s)),
        incomplete = function(s) cli::bg_none(cli::col_grey(s))
        # incomplete = function(s) cli::bg_black(cli::col_white(s))
      )
    )
  } else if (type == "default") {
    progressr::handlers("txtprogressbar")
  } else if (type == "pbcol") {
    progressr::handlers(
      progressr::handler_pbcol(
        adjust = 1.0,
        complete = function(s) cli::bg_red(cli::col_black(s)),
        incomplete = function(s) cli::bg_cyan(cli::col_black(s))
      )
    )
  } else if (type == "cli") {
    progressr::handlers("cli")
  } else if (type == "progress") {
    progressr::handlers("progress")
  } else {
    warning(
      "Unrecognized 'type = ", type, "'\n",
      "See `https://progressr.futureverse.org/` for detailed customization.")
  }
}


#' @rdname progress
#' @export
#'
#' @examples
#' \dontrun{
#' show_progress_bar()
#' show_progress_bar(FALSE)
#' }
show_progress_bar <- function(show = TRUE) {
  if (interactive()) set_progress_bar(type = NULL, show = show)
}


#' Set or get directory for/with scenarios
#'
#' @param path character, path to the directory with scenarios,
#' default is `NULL`
#'
#' @family options
#' @return sets or gets the path to the directory with scenarios
#' @export
#' @rdname options
#'
#' @examples
#' \dontrun{
#' set_scenarios_path("path/to/scenarios")
#' get_scenarios_path()
#' }
set_scenarios_path <- function(path = NULL) {
  options::opt_set("scenarios_path", path)
  # options(en_scenarios_path = path)
}


#' @family options
#' @export
#' @rdname options
get_scenarios_path <- function() {
  options::opt("scenarios_path")
  # getOption("en_scenarios_path")
}

# merge_paths <- function(path1, path2)

#' Drop columns in a data.frame with all NA values
#'
#' @description
#' A wrapper with `dplyr` functions to drop columns with no information (all `NA` values)
#'
#' @param x data.frame
#' @param unique logical, if TRUE (default), `unique()` function will be applied to the result.
#'
#' @return data.frame with dropped columns
#' @export
#'
#' @examples
#' x <- data.frame(a = c(1, 2, NA), b = c(NA, NA, NA), c = c(NA, 2, 3))
#' drop_na_cols(x)
#'
drop_na_cols <- function(x, unique = TRUE) {
  x <- select(x, where(~ !all(is.na(.))))
  if (unique) x <- unique(x)
  x
}

#' Make a name for a scenario directory
#' @description A function to automate the creation of a scenario directory name.
#' Used internally in `solve*()` and `interpolate*()` functions.
#' Also can be used to amend the name of the scenario directory and explicitly
#' assign the directory name to save the scenario object.
#'
#' @param scen scenario object
#' @param name character, name of the scenario, default is `scen@name`
#' @param model_name character, name of the model, default is `scen@model@name`
#' @param calendar_name character, name of the calendar, default is `scen@settings@calendar@name`
#' @param horizon_name character, name of the horizon, default is `scen@settings@horizon@name`
#' @param prefix character, prefix to add to the name
#' @param suffix character, suffix to add to the name
#' @param sep character, separator, default is `_`
#'
#' @return character, name of the scenario directory
#' @export
#'
#' @examples
#' \dontrun{
#' make_scenario_dirname(scen_BASE)
#' make_scenario_dirname(scen_BASE, prefix = "prefix", suffix = "suffix")
#' }
#'
make_scenario_dirname <- function(
    scen,
    name = scen@name,
    model_name = scen@model@name,
    calendar_name = scen@settings@calendar@name,
    horizon_name = scen@settings@horizon@name,
    prefix = NULL,
    suffix = NULL,
    sep = "_"
  ) {

  if (isTRUE(nchar(prefix) > 0)) {
    name <- paste(prefix, name, sep = sep)
  }

  if (isTRUE(is.null(name) && nchar(name) == 0)) {
    warning("Scenario name is empty. Using 'scenario' as a default name.")
    name <- "scenario"
  }

  if (isTRUE(nchar(model_name) > 0)) {
    name <- paste(name, model_name, sep = sep)
  }

  if (isTRUE(nchar(calendar_name) > 0)) {
    name <- paste(name, calendar_name, sep = sep)
  }

  if (isTRUE(nchar(horizon_name) > 0)) {
    name <- paste(name, horizon_name, sep = sep)
  }

  if (isTRUE(nchar(suffix) > 0)) {
    name <- paste(name, suffix, sep = sep)
  }

  return(name)
}


fEAC <- function(invcost, discount, olife) {
  stopifnot(olife > 0)
  stopifnot(invcost > 0)
  if (round(discount, 7) == 0) {
    return(invcost/olife)
  }
  (invcost * discount) / (1 - (1 + discount) ^ (-olife))
}


#' Check if an element of a set is "ANY*" or NA
#'
#' @param x character, vector of a set elements
#' @param na logical, if TRUE, NA values are included
#' @param any_mask character, regular expression to match "ANY*" elements
#'
#' @returns logical vector, TRUE if an element of the set is "ANY*"
#' @export
#'
#' @examples
#' is_any(c("ANY", "ANYREGION", "ANYSLICE", "ANYYEAR", "A", "B"))
is_any <- function(x, na = TRUE, any_mask = "^ANY_?[A-Z]*$") {
  # x - vector
  # na - logical, if TRUE, NA values are included
  ii <- grepl(any_mask, x)
  if (na) {
    ii <- ii | is.na(x)
  }
  return(ii)
}

if (F) {
  # year as factor
  y <- seq(2000, 2020, 5)
  yy <- factor(y, ordered = TRUE)
  yy <- factor(y, levels = y, ordered = TRUE)
  yy <- factor(y, levels = y, labels = y, ordered = TRUE)
  yy
  labels(yy)
  levels(yy)
  as.character(yy)
  as.integer(yy)
  as.integer(levels(yy))

}

# Flatten a model's repository list into a single named list of its objects'
# `@data`. Used by `model` `[` indexing (R/class-model.R). Relocated here from the
# (retired) interpolate2.R so that file can be archived.
flatten_mod_data <- function(x) {
  ll <- list()
  for (i in seq_along(x)) {
    ll <- c(ll, x[[i]]@data)
  }
  ll
}


# Flatten a model / scenario / repository / object to a flat list of the leaf
# S4 model objects (the ones that carry data slots).
.collect_model_objects <- function(x) {
  if (methods::is(x, "scenario")) x <- x@model
  if (methods::is(x, "model")) {
    objs <- list()
    for (rp in x@data) {
      objs <- c(objs, if (methods::is(rp, "repository")) rp@data else list(rp))
    }
    return(objs)
  }
  if (methods::is(x, "repository")) return(x@data)
  list(x)
}

#' Find where value(s) are stored across a model's objects
#'
#' Reflectively walks every slot of every object in a `model` / `scenario` /
#' `repository` (or a single S4 model object) and reports where the given
#' value(s) appear: the object, its class, the slot, and -- for `data.frame`
#' slots -- the column. Handy for tracking down a stray label, e.g. an
#' undeclared region (`"ES_off"`) or a mistyped commodity.
#'
#' @param x a `model`, `scenario`, `repository`, or a single model object (S4).
#' @param pattern character vector of value(s) to look for.
#' @param fixed if `TRUE` (default) match exactly; if `FALSE`, treat `pattern`
#'   as a regular expression (matched with [grepl()], alternated over the vector).
#' @param slots optional character vector restricting which slot names to search.
#' @param classes optional character vector restricting which object classes to
#'   search (e.g. `"technology"`).
#'
#' @returns a `data.frame` with columns `object`, `class`, `slot`, `column`,
#'   `value`, `n` -- one row per (object, slot, column, matched value); `column`
#'   is `NA` for atomic slots and `n` counts the matching elements/rows. Empty
#'   (0-row) data.frame when nothing matches.
#'
#' @examples
#' \dontrun{
#' find_in_model(mod, c("ES_off", "PT_off"))   # locate stray regions
#' find_in_model(scen, "BIO", fixed = FALSE)    # regex over every object
#' find_in_model(mod, "ES_off", classes = "technology")
#' }
#' @family model
#' @export
find_in_model <- function(x, pattern, fixed = TRUE, slots = NULL,
                          classes = NULL) {
  objs <- .collect_model_objects(x)
  pattern <- as.character(pattern)
  rx <- paste(pattern, collapse = "|")
  matched <- function(v) {
    cv <- as.character(v)
    if (fixed) cv[!is.na(cv) & cv %in% pattern] else cv[!is.na(cv) & grepl(rx, cv)]
  }
  hits <- list()
  for (o in objs) {
    if (!isS4(o)) next
    if (!is.null(classes) && !inherits(o, classes)) next
    o_class <- class(o)[1]
    o_name <- if ("name" %in% methods::slotNames(o)) {
      as.character(methods::slot(o, "name"))[1]
    } else {
      NA_character_
    }
    for (sn in methods::slotNames(o)) {
      if (identical(sn, "misc")) next
      if (!is.null(slots) && !(sn %in% slots)) next
      v <- methods::slot(o, sn)
      cols <- if (is.data.frame(v)) colnames(v) else list(NULL)
      for (cc in cols) {
        vals <- matched(if (is.null(cc)) v else v[[cc]])
        if (length(vals) == 0) next
        tab <- table(vals)
        hits[[length(hits) + 1L]] <- data.frame(
          object = o_name, class = o_class, slot = sn,
          column = if (is.null(cc)) NA_character_ else cc,
          value = names(tab), n = as.integer(tab),
          stringsAsFactors = FALSE
        )
      }
    }
  }
  if (length(hits) == 0L) {
    return(data.frame(
      object = character(0), class = character(0), slot = character(0),
      column = character(0), value = character(0), n = integer(0),
      stringsAsFactors = FALSE
    ))
  }
  do.call(rbind, hits)
}



