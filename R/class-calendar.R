# calendar-class ####
#' An S4 class to represent sub-annual time resolution structure.
#'
#' @name class-calendar
#'
#' @description
#' Sub-annual time resolution is represented by nested, named
#' time-frames and time-slices.
#'
#' @md
#' @slot name `r get_slot_doc("calendar", "name")`
#' @slot desc `r get_slot_doc("calendar", "desc")`
#' @slot timeframes `r get_slot_doc("calendar", "timeframes")`
#' @slot year_fraction `r get_slot_doc("calendar", "year_fraction")`
#' @slot timetable `r get_slot_doc("calendar", "timetable")`
#' @slot slice_share `r get_slot_doc("calendar", "slice_share")`
#' @slot default_timeframe `r get_slot_doc("calendar", "default_timeframe")`
#' @slot timeframe_rank `r get_slot_doc("calendar", "timeframe_rank")`
#' @slot slices_in_frame `r get_slot_doc("calendar", "slices_in_frame")`
#' @slot slice_family `r get_slot_doc("calendar", "slice_family")`
#' @slot slice_ancestry `r get_slot_doc("calendar", "slice_ancestry")`
#' @slot next_in_timeframe `r get_slot_doc("calendar", "next_in_timeframe")`
#' @slot next_in_year `r get_slot_doc("calendar", "next_in_year")`
#' @slot misc `r get_slot_doc("calendar", "misc")`
#'
#' @include generics.R defaults.R
#' @rdname class-calendar
#' @export
setClass("calendar", # alt: timestructure, timescales, timescheme, timeframe, schedule
  representation(
    name = "character",
    desc = "character",
    timeframes = "list", # renamed `slice_map` // alt.names: hierarchy, nest, ..
    year_fraction = "numeric",
    timetable = "data.frame", # renames `levels`
    slice_share = "data.frame", # !!! rename to fraction?
    default_timeframe = "character", # renamed `default_slice_level`
    timeframe_rank = "integer", # renamed `misc$deep`
    slices_in_frame = "integer", # renamed `misc$nlevel`
    slice_family = "data.frame", # renamed `parent_child`
    slice_ancestry = "data.frame", # renamed `all_parent_child`
    next_in_timeframe = "data.frame", # renamed `misc$next_slice`
    next_in_year = "data.frame", # renamed `misc$fyear_next_slice`
    misc = "list"
    # full_set = "character", # renamed `all_slice` -> slice_share$slice
  ),
  prototype(
    name = character(),
    desc = character(),
    timeframes = list(), # Slices set by level
    year_fraction = as.numeric(1),
    timetable = data.frame(stringsAsFactors = FALSE),
    slice_share = data.frame(
      # year = integer(),
      slice = character(), # == time interval
      share = numeric(), # fraction of a year // rename?
      weight = numeric(),
      stringsAsFactors = FALSE
    ),
    slices_in_frame = integer(),
    slice_family = data.frame(
      # year = integer(),
      parent = character(),
      child = character(),
      stringsAsFactors = FALSE
    ),
    slice_ancestry = data.frame(
      # year = integer(),
      parent = character(),
      child = character(),
      stringsAsFactors = FALSE
    ),
    default_timeframe = character(), # Default slice
    timeframe_rank = c("ANNUAL" = 1L),
    next_in_timeframe = data.frame(
      slice = character(),
      slicep = character()
    ),
    next_in_year = data.frame(
      slice = character(),
      slicep = character()
    ),
    # all_slice = character(), -> slice_share$slice
    misc = list()
  ),
  S3methods = FALSE
)

setMethod("initialize", "calendar", function(.Object, ...) {
  .Object
})


#' Create timetable of time-slices from given structure as a list
#'
#' @param struct named list of timeframes with sets of timeslices and optional shares of every slice or frame in the nest
#' @param warn logical, if TRUE, warning will be issued if `ANNUAL` level does not exists in the given structure. The level will be auto-created to complete the time-structure.
#'
#' @return an data.frame with the specified structure.
#' @order 2
#' @export
#' @rdname calendar
#'
#' @examples
#' make_timetable()
#' make_timetable(list("SEASON" = c("WINTER", "SUMMER")))
#' make_timetable(list("SEASON" = c("WINTER" = .6, "SUMMER" = .4)))
#' make_timetable(list(
#'   "SEASON" = list(
#'     "WINTER" = list(.3, DAY = c("MORNING", "EVENING")),
#'     "SUMMER" = list(.7, DAY = c("MORNING", "EVENING"))
#'   )
#' ))
#'
#' make_timetable(list(
#'   "SEASON" = list("WINTER" = .3, "SUMMER" = .7),
#'   "DAY" = c("MORNING", "EVENING")
#' ))
#'
make_timetable <- function(struct = list(ANNUAL = "ANNUAL"),
                           year_fraction = 1, warn = FALSE) {
  # an old version, adjusted for data.table
  # class and content check
  if (inherits(struct, "list")) {
    # check/add ANNUAL
    if (is.null(struct$ANNUAL)) {
      if (warn) warning("Adding `ANNUAL` level to timeframes")
      struct <- c(ANNUAL = "ANNUAL", struct)
    }
    # arg <- unlist(struct)
  } else {
    stop(
      "`struct` should be a named nested list with timeframes and slices ",
      "(see examples)"
    )
  }
  # check for duplicates
  nms <- names(struct)
  if (anyDuplicated(nms)) {
    stop(paste('duplicated slice levels: "',
      paste(unique(nms[duplicated(nms)]), collapse = '", "'),
      '"',
      sep = ""
    ))
  }
  # create timetable
  dtf <- data.table(share = numeric(), stringsAsFactors = FALSE)
  if (length(struct) == 1 && is.character(struct[[1]]) && length(struct[[1]]) == 1) {
    dtf <- data.table(
      share = year_fraction,
      ANNUAL = struct[[1]],
      stringsAsFactors = FALSE
    )
    if (!is.null(names(struct))) colnames(dtf)[2] <- names(struct)[1]
  } else {
    # browser()
    dtf <- .slice_constructor(dtf, struct) # , year_fraction = year_fraction
    if (year_fraction != 1) dtf$share <- dtf$share * year_fraction
  }
  # dtf <- dtf[, c(2:ncol(dtf), 1), drop = FALSE] # arrange columns
  setcolorder(dtf, c(2:ncol(dtf)))
  if (abs(sum(dtf$share) - year_fraction) < 1e-10) {
    dtf$share <- (dtf$share / sum(dtf$share) * year_fraction)
  }

  x <- select(dtf, if_else(ncol(dtf) > 2, 2, 1):share, -share) |>
    tidyr::unite(slice)
  dtf <- mutate(dtf, slice = x$slice, .before = "share")
  # browser()
  .check_timetable(dtf, year_fraction = year_fraction) # check validity
  dtf <- dplyr::arrange(dtf, across(1:slice))
  dtf$weight <- 1./year_fraction
  return(dtf)
}

if (F) {
  ### tests ####
  make_timetable()
  make_timetable(list("SEASON" = c("WINTER", "SUMMER")))
  make_timetable(list("SEASON" = c("WINTER" = .6, "SUMMER" = .4)))
  make_timetable(list(
    "SEASON" = list(
      "WINTER" = list(.3, DAY = c("MORNING", "EVENING")),
      "SUMMER" = list(.7, DAY = c("MORNING", "EVENING"))
    )
  ))

  make_timetable(list(
    "SEASON" = list("WINTER" = .3, "SUMMER" = .7),
    "DAY" = c("MORNING", "EVENING")
  ))

  # from UTOPIA
  make_timetable(timeslices)
  make_timetable(timeslices1)
  make_timetable(timeslices2)
  make_timetable(timeslices2, year_fraction = .5)
  make_timetable(timeslices3)

  dtf <- make_timetable(timeslices2)
  # obj <- new("calendar")
  # obj@timetable <- dtf
}

#' Generate a new calendar object from
#'
#' @name newCalendar
#'
#'
#' @param name `r get_slot_doc("calendar", "name")`
#' @param desc `r get_slot_doc("calendar", "desc")`
#' @param timetable `r get_slot_doc("calendar", "timetable")`
#' @param year_fraction `r get_slot_doc("calendar", "year_fraction")`
#' @param default_timeframe `r get_slot_doc("calendar", "default_timeframe")`
#' @param misc `r get_slot_doc("calendar", "misc")`
#' @param ... ignored
#'
#' @rdname newCalendar
#' @return an object of class `calendar` with the specified structure.
#'
#' @description
#' Calendars are defined by the structure of timeframes and time-slices
#' with shares of time in a year. The structure is represented by a
#' `timetable` data.frame with levels of timeframes in the named columns,
#' and names of individual time-slices in every timeframe.
#' The number of rows in `timetable` is equal to the total number
#' of time-slices on the lowest level.
#' Every timeframe is a set of timeslices ("slices") - a named fragment
#' of time with a year-share. Timeframes have nested structure.
#' Currently, every "parent"-timeframe must have the same number of
#' elements as the "child"-timeframe. (This may change in the future.)
#' \describe{
#'   \item{ANNUAL}{character, annual, the top level of timeframes}
#'   \item{TIMEFRAME2}{character, (optional) first subannual level of timeframes}
#'   \item{TIMEFRAME3}{character, (optional) second subannual level of timeframes}
#'   \item{...}{character, (optional) further subannual levels of timeframes}
#'   \item{slice}{character, name of the time-slices used in sets to refer to the lowest level of timeframes. If not specified, will be auto-created with the formula: `{SLICE2}_{SLICE3}...`}
#' }
#'
#' @order 1
#' @export
#'
#' @examples
#' newCalendar()
newCalendar <- function(
    name = "",
    desc = "",
    timetable = NULL,
    year_fraction = 1,
    default_timeframe = NULL,
    misc = list(
      pSliceWeight = NULL
    ),
    ...) {
  obj <- .init_calendar(timetable = timetable, year_fraction = year_fraction)
  arg <- list(...)
  arg$name <- name
  arg$desc <- desc
  arg$misc <- misc
  if (!is.null(arg$name)) obj@name <- arg$name
  if (!is.null(arg$desc)) obj@desc <- arg$desc
  if (!is.null(arg$misc)) obj@misc <- arg$misc
  if (!is.null(arg$default_timeframe)) {
    if (!(obj@default_timeframe %in% names(obj@timeframes))) {
      stop(
        "The default_timeframe = ", default_timeframe,
        " is inconsistent with timeframes:\n       ",
        paste(names(obj@timeframes), collapse = " ")
      )
    }
    obj@default_timeframe <- arg$default_timeframe
  }
  obj
}

if (F) {
  ## tests ####
  newCalendar()
  newCalendar(timetable = make_timetable(timeslices))
  newCalendar(timetable = make_timetable(timeslices2),
    name = "WRSA_DN",
    desc = "Four Seasons, day-night"
  )
  newCalendar(make_timetable(timeslices3),
    name = "m12h24",
    desc = "One day per month, 24 hours per day"
  )

  cal <- make_timetable(timeslices3)
  cal_subset <- cal[grepl("h0[12]", HOUR)]
  cal$share |> sum()
  cal_subset$share |> sum()
  newCalendar(timetable = cal_subset, year_fraction = sum(cal_subset$share))
}

.print_if_not_empty <- function(x, pref = NULL, suff = NULL) {
  # browser()
  x <- as.character(x)
  msg <- paste0(pref, x, suff)
  if (length(x) != 0) cat(msg, "\n")
}

# print calendar ####
setMethod("print", "calendar", function(x, ...) {
  # browser()
  cat('An object of class "calendar"\n')
  .print_if_not_empty(x@name, "name: ")
  .print_if_not_empty(x@desc, "desc: ")
  printed_timeframes <- lapply(x@timeframes, function(y) {
    if (length(y) <= 10) return(y)
    y <- c(
      head(y, 5),
      # paste0(y[10], "... (", length(y), " total)")
      "...", tail(y, 5), paste0(
      "(", length(y), " total)"
    ))
    paste(y, collapse = ", ")
  })

})

if (F) {
  tmtbl <- make_timetable(timeslices::tsl_sets$d365_h24)
  calend <- newCalendar(tmtbl, name = "d365_h24")
  print(calend)
}

# internal functions ####
# validation of names of individual time-slices
.check_slice_name <- function(nm) {
  # check / optimize script
  if (any(grep("^[A-z]", nm, invert = TRUE)) ||
    any(gsub("[[:alnum:]]*", "", nm) != "") ||
    anyDuplicated(nm)) {
    n1 <- unique(c(
      grep("^[A-z]", nm, invert = TRUE, value = TRUE),
      nm[(gsub("[[:alnum:]]*", "", nm) != "")]
    ))
    ms1 <- NULL
    ms2 <- NULL
    if (length(n1) != 0) {
      ms1 <- paste('Check slice names "',
        paste(n1, collapse = '", "'), '". ',
        sep = ""
      )
    }
    n2 <- unique(nm[duplicated(nm)])
    if (length(n2) != 0) {
      ms2 <- paste('Check slice names "',
        paste(n2, collapse = '", "'), '"',
        sep = ""
      )
    }
    ms <- paste(ms1, ms2, sep = "")
    stop(ms)
  }
}

# validation of calendar@timetable
.check_timetable <- function(dtf, year_fraction = 1) {
  # adjusted for data.table & dtplyr
  # browser()
  sl <- select(dtf, slice)
  dtf <- select(dtf, -any_of("slice")) # to fit "old" check algo
  dtf <- select(dtf, -any_of("weight")) # !!! add check of weights
  # check / optimize the script
  if (ncol(dtf) < 2) {
    stop("time-slices data.table must have more than one columns")
  }
  if (colnames(dtf)[ncol(dtf)] != "share") {
    stop("The time-slices data.table must have `share` column")
  }
  # rcs <- colnames(dtf)[-ncol(dtf)]
  rcs <- select(dtf, -share) |> colnames()
  if (anyDuplicated(rcs)) {
    stop(paste('duplicated slice levels: "',
      paste(unique(rcs[duplicated(rcs)]), collapse = '", "'), '"',
      sep = ""
    ))
  }

  # check length
  fl <- apply(
    # dtf[, -c(1, ncol(dtf)), drop = FALSE],
    select(dtf, -1, -share),
    2, function(x) length(unique(x)) == 1
  )

  if (any(fl)) {
    stop(paste('all slice levels except "ANNUAL", ',
      'should have more than one elements, check: "',
      paste(colnames(dtf)[c(FALSE, fl, FALSE)], collapse = '", "'), '"',
      sep = ""
    ))
  }
  if (length(unique(dtf[[1]])) != 1) {
    stop("first slice should have only one 'ANNUAL' element")
  }
  rcs <- c(
    apply(select(dtf, -share), 2, function(x) unique(x)),
    recursive = TRUE
  )
  if (anyDuplicated(rcs)) {
    stop(paste('duplicated slice names in levels: "',
      paste(unique(rcs[duplicated(rcs)]), collapse = '", "'), '"',
      sep = ""
    ))
  }
  # Check sum == year_fraction
  if (round(sum(dtf$share) - year_fraction, 7) != 0) {
    stop(
      "Sum of slice shares must be equal to the given year_fraction = ",
      year_fraction, ", check: ", sum(dtf$share)
    )
  }
  # full year
  ll <- apply(select(dtf, -share), 1, paste, collapse = ".")
  if (anyDuplicated(ll)) {
    stop(paste('duplicated sets in time-slices. ("',
      paste(ll[duplicated(ll)], collapse = '", "'), '").',
      sep = ""
    ))
  }
  # check length
  if (length(ll) != prod(sapply(
    select(dtf, -share),
    function(x) length(unique(x))
  ))) {
    message("Irregular calendar / time series")
    # # error - investigate
    # dtf2 <- unique(dtf[[1]])
    # for (i in seq(length = ncol(dtf) - 2) + 1) {
    #   ln <- length(unique(dtf[[i]]))
    #   dtf2 <- paste(c(t(matrix(dtf2, length(dtf2), ln))), ".",
    #     unique(dtf[[i]]),
    #     sep = ""
    #   )
    # }
    # stop(paste('(empty?) time-slices. ("',
    #   paste(dtf2[!(dtf2 %in% ll)], collapse = '", "'), '").',
    #   sep = ""
    # ))
  }
}

# internal function to ??? create timeslices table from a given list with structure
# !!! ToDo: optimize/rewrite for data.table
.slice_constructor <- function(dtf, arg) {
  # browser()
  dtf <- as.data.frame(dtf) # doesn't work with data.table - debug/rewrite
  # check / optimize script
  if (is.null(names(arg)) || any(names(arg) == "")) {
    stop(paste("Unnamed arguments: ",
      paste(capture.output(print(arg)), collapse = "\n"),
      sep = "\n"
    ))
  }
  add_val <- function(dtf, val_sh, val_nm) {
    dtf0 <- dtf
    dtf <- dtf[0, , drop = FALSE]
    dtf[[lv]] <- character()
    if (nrow(dtf0) != 0) {
      dtf[1:(nrow(dtf0) * length(val_sh)), ] <- NA
      for (i in 2:ncol(dtf0)) dtf[[i]] <- dtf0[[i]]
      dtf[, lv] <- c(t(matrix(val_nm, length(val_sh), nrow(dtf0))))
      dtf[, "share"] <- dtf0[, "share"] * c(t(matrix(
        val_sh,
        length(val_sh),
        nrow(dtf0)
      )))
    } else {
      dtf[1:length(val_sh), ] <- NA
      dtf[, lv] <- val_nm
      dtf[, "share"] <- val_sh
    }
    dtf
  }
  while (length(arg) != 0) {
    lv <- names(arg)[1]
    dtf[, lv] <- rep(character(), nrow(dtf))
    if (is.character(arg[[1]]) ||
      (!is.null(names(arg[[1]])) &&
        is.numeric(arg[[1]]))) {
      if (is.character(arg[[1]])) {
        val_sh <- rep(1 / length(arg[[1]]), length(arg[[1]]))
        val_nm <- arg[[1]]
      } else {
        val_sh <- arg[[1]]
        val_nm <- names(arg[[1]])
      }
      .check_slice_name(val_nm)
      if (any(val_sh <= 0) || round(sum(val_sh), 10) != 1) { # avoiding precision issues on some systems (Mac/M2-Si)
        stop(paste(paste('Check time-slice data for level "', lv, '"\n',
          sep = ""
        ), paste(capture.output(print(arg[[1]])), collapse = "\n"), sep = "\n"))
      }
      arg <- arg[-1]
      dtf <- add_val(dtf, val_sh, val_nm)
    } else if (is.list(arg[[1]])) {
      arg2 <- arg[[1]] # arg <- arg[-1]
      if (is.null(names(arg2)) || any(names(arg2) == "")) {
        stop(paste(
          paste('Check time-slice data for level "', lv, '"\n',
            sep = ""
          ), paste(capture.output(print(arg[[1]])), collapse = "\n"),
          sep = "\n"
        ))
      }
      if (is.numeric(arg2[[1]])) {
        if (!all(sapply(arg2, is.numeric))) {
          stop(paste(
            paste('Check time-slice data for level "', lv, '"\n',
              sep = ""
            ), paste(capture.output(print(arg[[1]])), collapse = "\n"),
            sep = "\n"
          ))
        }
        dtf <- add_val(dtf, c(arg2, recursive = TRUE), names(arg2))
        arg <- arg[-1]
      } else {
        if (!all(sapply(arg2, is.list))) {
          stop(paste(
            paste('Check time-slice data for level "', lv, '"\n',
              sep = ""
            ), paste(capture.output(print(arg[[1]])), collapse = "\n"),
            sep = "\n"
          ))
        }
        dtf0 <- dtf
        dtf <- NULL
        arg2 <- arg[[1]]
        for (i in seq(length.out = length(arg2))) {
          dtf1 <- .slice_constructor(add_val(
            dtf0, arg2[[i]][[1]],
            names(arg2)[i]
          ), arg2[[i]][-1])
          if (i == 1) {
            dtf <- dtf1
          } else {
            if (ncol(dtf) != ncol(dtf1) || any(colnames(dtf) != colnames(dtf1))) {
              stop(paste("Set of slice have to be the same for all ",
                "(check list slice arguments).",
                sep = ""
              ))
            }
            dtf <- rbind(dtf, dtf1)
          }
        }
        arg <- arg[-1]
      }
    } else {
      stop(paste('Unknown type of argument for slice level "', lv, '"',
        sep = ""
      ))
    }
  }
  as.data.table(dtf)
}

# internal function to populate all slots of `calendar` from given calendar@timetable
.complete_calendar <- function(obj, year_fraction = 1) {
  # browser()
  if (nrow(obj@timetable) == 0) {
    warning('no slices desc, using default: "ANNUAL"')
    obj@timetable <- make_timetable()
  } else if (is.null(obj@timetable$weight)) {
    obj@timetable <- mutate(
      obj@timetable,
      weight = 1 / sum(obj@timetable$share)
    )
  }
  # validate the timetable
  .check_timetable(obj@timetable, year_fraction = year_fraction)
  # obj@misc <- list()
  dtf <- obj@timetable |> select(-any_of(c("slice")))

  # timeframe_rank / levels
  d <- select(dtf, -any_of(c("share", "year", "slice", "weight")))
  obj@timeframe_rank <- 1:ncol(d)
  names(obj@timeframe_rank) <- colnames(d)

  # number of slices on every level
  # browser()
  obj@slices_in_frame <- sapply(d, function(x) length(unique(x)))

  # share of every slice in a year
  # browser()
  obj@slice_share <- data.table(
    slice = rep(as.character(NA), sum(cumprod(obj@slices_in_frame))),
    share = as.numeric(NA),
    weight = 1.
  )
  obj@slice_share[1, "slice"] <- dtf[1, 1]
  obj@slice_share[1, "share"] <- year_fraction
  obj@slice_share[1, "weight"] <- 1/year_fraction
  k <- 1
  if (ncol(dtf) > 3) {
    # browser()
    for (i in 2:(ncol(dtf) - 2)) {
      # tmp <- apply(dtf[, 2:i, drop = FALSE], 1, paste, collapse = "_")
      slice_names <- apply(select(dtf, 2:i), 1, paste, collapse = "_")
      # tmp <- tapply(dtf[, ncol(dtf)], tmp, sum)
      wh <- tapply(dtf[, weight], slice_names, sum)
      # w <- 1/year_fraction
      sh <- tapply(dtf[, share], slice_names, sum)
      wh <- wh / sum(wh * sh)
      obj@slice_share$slice[k + seq(along = sh)] <- names(sh)
      obj@slice_share$share[k + seq(along = sh)] <- sh
      obj@slice_share$weight[k + seq(along = sh)] <- wh
      k <- (k + length(sh))
    }
  }

  # @structure
  # browser()
  nframes <- select(obj@timetable, 1:slice, -slice) |> ncol()
  fnames <- names(obj@slices_in_frame)
  if (nframes > 2) {
    tmp <- nchar(obj@slice_share$slice) -
      nchar(gsub("[_]", "", obj@slice_share$slice)) + 2
    names(tmp) <- obj@slice_share$slice
    tmp[obj@timetable[[1]][1]] <- 1
    obj@timeframes <- lapply(
      1:nframes,
      # 1:(ncol(obj@timetable) - 2),
      function(x) names(tmp)[tmp == x]
    )
  } else if (nframes == 1) {
    obj@timeframes <- list()
    obj@timeframes[fnames[1]] <- fnames[1]
  } else if (nframes == 2) {
    obj@timeframes <- list()
    obj@timeframes[[fnames[1]]] <- fnames[1]
    obj@timeframes[[fnames[2]]] <- obj@timetable[[fnames[2]]] #|> sort()
  } else {
    stop("Empty timeframes / timeslices")
  }
  # browser()
  names(obj@timeframes) <- colnames(d)

  obj@default_timeframe <- colnames(d)[ncol(d)]

  # @slice_share$slice
  # obj@slice_share$slice <- obj@slice_share$slice

  # @slice_family
  # browser()
  if (nrow(obj@timetable) == 1) {
    obj@slice_family <- obj@slice_family[0, , drop = FALSE]
    obj@slice_ancestry <- obj@slice_ancestry[0, , drop = FALSE]
  } else {
    obj@slice_family <- obj@slice_family[0, ] |> as.data.frame()
    # obj@slice_family$lev <- numeric()
    obj@slice_family[1:(nrow(obj@slice_share) - 1), ] <- NA
    i <- 1
    k <- 0
    z <- 1
    while (i != ncol(dtf) - 2) {
      l <- obj@slices_in_frame[i + 1]
      for (j in 1:obj@slices_in_frame[i]) {
        obj@slice_family$parent[k + 1:l] <- obj@slice_share$slice[z]
        obj@slice_family$child[k + 1:l] <- obj@slice_share$slice[1 + k + 1:l]
        # obj@slice_family[k + 1:l, 'lev'] <- i + 1
        k <- k + l
        z <- z + 1
      }
      i <- i + 1
    }
    # @slice_ancestry
    tmp <- obj@slice_family
    tmp$nlev <- NA
    for (i in seq_along(obj@timeframes)) {
      tmp$nlev[tmp$parent %in% obj@timeframes[[i]]] <- i
    }
    ll <- tmp[tmp$nlev + 1 == length(obj@timeframes), -3]
    for (i in rev(seq_along(obj@timeframes))[-(1:2)]) {
      gg <- tmp[tmp$nlev == i, -3]
      g3 <- gg
      colnames(gg)[2] <- "sht"
      l2 <- ll
      colnames(l2)[1] <- "sht"
      g2 <- merge(gg, l2)
      g2$sht <- NULL
      ll <- rbind(ll, g2, g3)
    }
    obj@slice_ancestry <- ll
  }
  # browser()
  # next slice in the same nest & in a year
  # obj@slices_in_frame <- NULL
  if (nrow(obj@timetable) != 1) {
    tmp <- obj@slice_family
    tmp$next_slice <- NA
    j <- 1
    for (i in 1:(nrow(tmp) - 1)) {
      if (tmp$parent[i] == tmp$parent[i + 1]) {
        tmp$next_slice[i] <- tmp$child[i + 1]
      } else {
        tmp$next_slice[i] <- tmp$child[j]
        j <- i + 1
      }
      # if (tmp[i, "parent"] == tmp[i + 1, "parent"]) {
      #   tmp[i, "next_slice"] <- tmp[i + 1, "child"]
      # } else {
      #   tmp[i, "next_slice"] <- tmp[j, "child"]
      #   j <- i + 1
      # }
    }
    tmp$next_slice[i + 1] <- tmp$child[j]
    obj@next_in_timeframe <- data.table(
      slice = tmp$child, slicep = tmp$next_slice,
      stringsAsFactors = FALSE
    )
    # browser()
    n1 <- c(lapply(obj@timeframes[-1], function(x) x), recursive = TRUE)
    names(n1) <- NULL
    n2 <- c(lapply(obj@timeframes[-1], function(x) c(x[-1], x[1])),
            recursive = TRUE)
    names(n2) <- NULL
    # browser()
    obj@next_in_year <- data.table(
      slice = n1,
      slicep = n2,
      stringsAsFactors = FALSE
    )
  }
  obj@year_fraction <- year_fraction
  obj@slice_family <- as.data.table(obj@slice_family)
  obj@slice_ancestry <- as.data.table(obj@slice_ancestry)
  obj@next_in_timeframe <- as.data.table(obj@next_in_timeframe)
  obj@next_in_year <- as.data.table(obj@next_in_year)
  obj
}

# initialize calendar object from given list ("struct") or "timetable"
.init_calendar <- function(struct = NULL, timetable = NULL, year_fraction = 1) {
  # browser()
  tsl <- new("calendar")
  if (is.null(timetable)) {
    if (is.null(struct)) {
      tsl@timetable <- make_timetable()
    } else {
      tsl@timetable <- make_timetable(struct)
    }
  } else if (!is.null(struct)) {
    stop("Only one parameter `struct` or `timetable` can be specified")
  } else {
    tsl@timetable <- timetable
  }
  .complete_calendar(tsl, year_fraction = year_fraction)
}

if (F) {
  ## tests ####
  .init_calendar()
  .init_calendar(struct = timeslices2)
  .init_calendar(struct = timeslices2, year_fraction = .5)
  .init_calendar(struct = timeslices3)
  make_timetable(timeslices2)
  make_timetable(timeslices3)
  .init_calendar(timetable = make_timetable(timeslices2))
  .init_calendar(timetable = make_timetable(timeslices3))
  # .init_calendar(timetable = tsl@timetable)
}

#### migrated from class-slice ####
# the functions below to be review/rewritten

# =============================================================================#
# Check if slice level exist ####
# =============================================================================#
# !!! superceded by .check_timeframe
.checkSliceLevel <- function(app, approxim) {
  # browser()
  timeframes <- names(approxim$calendar@timeframe_rank)
  # if (length(app@slice) != 0 &&
  #     all(app@slice != colnames(approxim$calendar@timetable)[
  #       -ncol(approxim$calendar@timetable)])) {
  if (.hasSlot(app, "timeframe")) {
    if (!is_empty(app@timeframe) && !any(app@timeframe %in% timeframes)) {
      stop(paste0(
        'Unrecognized timeframe level "', app@timeframe, '" in ',
        class(app), ': "', app@name, '"'
      ))
    }
  }
}

.check_timeframe <- function(obj, scen) {
  # the function checks if the given timeframe is valid
  # ...
}


# =============================================================================#
# Disaggregate slice ####
# e.g. from WINTER to WINTER_DAY and WINTER_NIGHT
# =============================================================================#
.disaggregateSliceLevel <- function(app, approxim) {
  # browser()
  slt <- getSlots(class(app))
  slt <- names(slt)[slt %in% c("data.frame", "data.table")]
  if (is(app, "technology")) slt <- slt[slt != "afs"]
  for (ss in slt) {
    if (any(colnames(slot(app, ss)) == "slice")) {
      tmp <- slot(app, ss) |> as.data.frame() # !!! rewrite
      fl <- (!is.na(tmp$slice) & !(tmp$slice %in% approxim$slice)) # !!! @calendar?
      if (any(fl)) {
        mark_col <- (sapply(tmp, is.character) | colnames(tmp) == "year")
        mark_coli <- colnames(tmp)[mark_col]
        t1 <- tmp[fl, , drop = FALSE] |> as.data.frame() # !!! rewrite
        t2 <- tmp[!fl, , drop = FALSE] |> as.data.frame() # !!! rewrite
        # Sort from lowest level to largest
        ff <- approxim$parent_child$parent[
          !duplicated(approxim$parent_child$parent)
        ]
        f1 <- seq_along(ff)
        names(f1) <- ff
        if (!all(t1$slice %in% ff)) {
          stop(paste0(
            'Unknown slice or slice is not parrent slice, for "',
            app@name, '" (class ', class(app), '), slot: "', ss, '", slice: "',
            paste0(t1$slice[!(t1$slice %in% ff)], collapse = '", "'), '"'
          ))
        }
        t1 <- t1[
          sort(f1[t1$slice], index.return = TRUE, decreasing = TRUE)$ix, ,
          drop = FALSE
        ]
        # browser()
        # Add child disaggregation
        for (i in seq_len(nrow(t1))) {
          # ll <- approxim$parent_child[
          #   approxim$parent_child$parent == t1[i, "slice"], "child"
          # ]
          ll <- approxim$parent_child |>
            filter(parent == t1$slice[i]) |>
            select(child) |> purrr::simplify()

          t0 <- t1[rep(i, length(ll)), , drop = FALSE]
          t0$slice <- ll
          # tes <- t0[, mark_coli, drop = FALSE]
          tes <- select(t0, all_of(mark_coli)) |> as.matrix()
          tes[is.na(tes)] <- "-"
          z1 <- apply(tes, 1, paste0, collapse = "##")
          # tes <- t2[, mark_coli, drop = FALSE]
          tes <- select(t2, all_of(mark_coli)) |> as.matrix()
          tes[is.na(tes)] <- "-"
          z2 <- apply(tes, 1, paste0, collapse = "##")
          # If there are the same row, after splititng
          if (any(z1 %in% z2)) {
            merge_col <- merge0(t0, t2, by = mark_coli)
            colnames(merge_col)[seq_len(ncol(t0))] <- colnames(t0)
            for (j in colnames(tmp)[!mark_col]) {
              merge_col[!is.na(merge_col[, paste0(j, ".y")]), j] <- NA
            }
            t0 <- rbind(t0[!((z1 %in% z2)), ], merge_col[, 1:ncol(t0)])
          }
          t2 <- rbind(t2, t0)
        }
        slot(app, ss) <- t2
      }
    }
  }
  app
}

# set Slice name vectors
# .setTimeSlices <- function(slice = NULL, ...) {
#   browser()
#   if (!is.null(slice) && length(list(...))) {
#     stop('setTimeSlices: only one argument could be used: "slice" or "..."')
#   }
#   if (!is.null(slice)) {
#     arg <- slice
#   } else {
#     arg <- list(...)
#   }
#   rcs <- names(arg)
#   if (anyDuplicated(rcs)) {
#     stop('Duplicated slice levels: "',
#          paste(unique(rcs[duplicated(rcs)]), collapse = '", "'),'"')
#   }
#   dtf <- data.frame(share = numeric(), stringsAsFactors = FALSE)
#   if (length(arg) == 1 && is.character(arg[[1]]) && length(arg[[1]]) == 1) {
#     dtf <- data.frame(share = 1, ANNUAL = arg[[1]], stringsAsFactors = FALSE)
#     if (!is.null(names(arg))) colnames(dtf)[2] <- names(arg)[1]
#   } else {
#     dtf <- .slice_constructor(dtf, arg)
#   }
#   dtf <- dtf[, c(2:ncol(dtf), 1), drop = FALSE]
#   if (length(unique(dtf[, 1])) != 1) {
#     warning('The first level should have only one element, ',
#             'add "ANNUAL"?')
#     if (any(colnames(dtf) == "ANNUAL") ||
#         any(c(dtf == "ANNUAL", recursive = TRUE))) {
#       browser()
#       stop('Error with adding "ANNUAL" slice')
#     }
#     dtf$ANNUAL <- rep("ANNUAL", nrow(dtf))
#     dtf <- dtf[, c(ncol(dtf), 2:ncol(dtf) - 1), drop = FALSE]
#   }
#   if (abs(sum(dtf$share) - 1) < 1e-10) dtf$share <- (dtf$share / sum(dtf$share))
#   .slice_check_data(dtf)
#   sl <- new("slice")
#   sl@levels <- dtf
#   sl <- .init_slice(sl)
#   sl
# }

# ToDo: write methods: ####
## `add` ####
## `update` ####

