# check <- function(...) UseMethod("check")

#' An S4 class to represent model/scenario planning horizon with intervals (year-steps)
#' @name class-horizon
#' 
#' @slot name `r get_slot_doc("horizon", "name")`
#' @slot desc `r get_slot_doc("horizon", "desc")`
#' @slot period `r get_slot_doc("horizon", "period")`
#' @slot intervals `r get_slot_doc("horizon", "intervals")`
#'
#' @rdname class-horizon
#' @family settings model scenario
#' @include generics.R
#' @export
setClass(
  "horizon",
  representation(
    name = "character",
    desc = "character",
    period = "integer",
    intervals = "data.table"
  ),
  prototype(
    name = character(),
    desc = character(),
    period = integer(),
    intervals = data.table(
      start = integer(),
      mid = integer(),
      end = integer()
    )
    # !!! Add misc
  )
)

# Functions and methods to define and set model horizon

#' Create a new object of class 'horizon'
#'
#' @description 
#' The function creates a new object of class 'horizon' that represents the planning horizon of the model/scenario.
#' 
#' @rdname newHorizon
#'
#' @param name `r get_slot_doc("horizon", "name")` 
#' @param period (optional) integer vector with a range or a sequence of years to define
#' the full period of the model/scenario. If not provided, the range of 'intervals' will be used. 
#' @param intervals (optional) either data.frame or integer vector. 
#' The data.frame must have `start`, `mid`, and `end` columns with modeled interval. 
#' The vector will be considered as lengths of each modeled interval in period.
#' @param desc `r get_slot_doc("horizon", "desc")`
#' @param force_BY_interval_to_1_year logical, if TRUE (default), the base-year (first) interval will be forced to one year.
#' @param mid_is_end logical, if TRUE, the mid-year will be set to the end of the interval.
#' @param mid_is_start logical, if TRUE, the mid-year will be set to the start of the interval.
#'
#' @family horizon config settings model scenario
#'
#' @return An object of class 'horizon'
#'
#' @examples
#' newHorizon(2020:2050)
#' newHorizon(2020:2030, desc = "One-year intervals")
#' newHorizon(2020:2030, c(1, 2, 5, 10), desc = "Different length intervals")
#' newHorizon(2020:2035, c(1, 2, 5, 5, 5))
#' newHorizon(2020:2050, c(1, 2, 5, 7, 1))
#'
#' newHorizon(intervals = data.frame(
#'   start = c(2030, 2031, 2034),
#'   mid =   c(2030, 2032, 2037),
#'   end =   c(2030, 2033, 2040)),
#'   desc = "Explicit assignment of intervals via data.frame"
#'   )
#'
#' newHorizon(period = 2020:2050,
#'            intervals = data.frame(
#'              start = c(2030, 2031, 2034),
#'              mid =   c(2030, 2032, 2037),
#'              end =   c(2030, 2033, 2040)),
#'              desc = "The period will be trimmed to the scope of intervals")
#'
#' newHorizon(2020:2050, c(3, 2, 5, 10),
#'            desc = "Pay attention to the length of the first interval")
#'
#' newHorizon(period = 2020:2040,
#'            intervals = data.frame(
#'              start = c(2030, 2032, 2035),
#'              mid =   c(2031, 2033, 2037),
#'              end =   c(2032, 2034, 2040)))
#' @export
newHorizon <- function(
    period = NULL,
    intervals = NULL,
    mid_is_end = FALSE,
    mid_is_start = FALSE,
    force_BY_interval_to_1_year = TRUE,
    desc = NULL,
    name = NULL
    ) {
  # browser()
  h <- new("horizon") # !!! update .data2slots for this class
  if (!is.null(desc)) {
    stopifnot(is.character(desc))
    h@desc <- as.character(desc)
  }
  if (!is.null(name)) {
    stopifnot(is.character(name))
    h@name <- as.character(name)
  }
  if (mid_is_end & mid_is_start) {
    stop("Only one of parameters 'mid_is_end' and  'mid_is_start' can be TRUE")
  }

  if (!is.null(period)) {
    .check_integer(period, ": period")
    period <- min(period):max(period) |> as.integer()
  }

  if (!is.null(intervals)) {
    if (is.data.frame(intervals)) {
      .check_intervals(intervals)
      intervals <- as.data.table(intervals)
      intervals <- intervals[order(start)]
      int_range <- as.list(intervals) |>
        unlist() |>
        range() |>
        as.integer()
      # next step: merge the data.frame with `period`
    } else if (is.numeric(intervals)) {
      if (is.null(period)) {
        stop(
          "When `intervals` is an integer vector with length of intervals, ",
          "`period` must be a vector (or range) of modeled period."
        )
      }
      .check_integer(intervals, ": intervals")
      intervals <- as.integer(intervals)
      if (force_BY_interval_to_1_year && intervals[1] != 1) {
        # adjusting BY length to 1
        intervals <- c(1L, intervals)
        if (length(intervals) > 1 && intervals[2] > 1) {
          intervals[2] <- intervals[2] - 1L
        }
      }
      intervals <- data.table(
        start = period[1] + cumsum(c(0, intervals[-length(intervals)])),
        mid = as.integer(rep(NA, length(intervals))),
        end = period[1] + cumsum(intervals) - 1
      )
      intervals$mid <- trunc(.5 * (intervals[, "start"] + intervals[, "end"]))

      intervals <- intervals[start >= period[1] & start <= max(period), ]
      nr <- nrow(intervals)
      if (intervals$end[nr] > max(period)) {
        intervals$end[nr] <- max(period)
        intervals$mid[nr] <- round(mean(intervals$start[nr], intervals$end[nr]))
      }
      int_range <- intervals |>
        as.list() |>
        unlist() |>
        as.vector() |>
        range() |>
        as.integer()
      period <- period[period >= min(int_range) & period <= max(int_range)]
      h@period <- period
      if (mid_is_end) intervals$mid <- intervals$end
      if (mid_is_start) intervals$mid <- intervals$start
      h@intervals <- intervals
      return(h)
    }
  } else if (!is.null(period)) { # intervals == NULL
    intervals <- data.table(
      start = as.integer(period),
      mid = as.integer(period),
      end = as.integer(period)
    )
    h@period <- period
    if (mid_is_end) intervals$mid <- intervals$end
    if (mid_is_start) intervals$mid <- intervals$start
    h@intervals <- intervals
    return(h) # one year steps
  } else if (is.null(period)) { # no data
    return(h) # empty
  }

  if (is.null(period)) {
    period <- min(int_range):max(int_range) |> as.integer()
  } else { # merge `period` vector with `intervals` data.table
    period <- seq(max(min(int_range), min(period)),
      min(max(int_range), max(period)),
      by = 1L
    ) |> as.integer()
    intervals <- intervals[start >= min(period) & end <= max(period), ]
  }

  # Check & fix BY interval
  if (force_BY_interval_to_1_year && nrow(intervals) > 0 &&
    (intervals$mid[1] != intervals$start[1] ||
      intervals$end[1] != intervals$start[1]) ||
    (nrow(intervals) > 1 && diff(intervals$mid[1:2] > 1))) {
    # warning("Adjusting base-year interval to be one-year.")
    int_BY <- intervals[1, ]
    int_BY[1, ] <- int_BY$start[1]
    intervals <- rbind(int_BY, intervals)
    intervals$start[2] <- intervals$end[1] + 1L
    intervals$mid[2] <- intervals$start[1] + 1L
    intervals <- data.table(
      start = as.integer(intervals$start),
      mid = as.integer(intervals$mid),
      end = as.integer(intervals$end)
    )
    intervals <- intervals[order(start)]
    .check_intervals(intervals) # double-check
  }
  h@period <- period
  if (mid_is_end) intervals$mid <- intervals$end
  if (mid_is_start) intervals$mid <- intervals$start
  h@intervals <- intervals
  # h <- .data2slots("period", x = "", period = period, intervals = intervals)
  return(h)
}

#' @family update horizon
#' @method update horizon
#' @rdname newHorizon
#' @export
setMethod("update", "horizon", function(object, ..., warn_nodata = TRUE) {
  # browser()
  # !!! add no-data check for warning
  # cf <- .data2slots("config", object, ..., warn_nodata = FALSE)
  object <- .data2slots("horizon", object, ...,
    # ignore_args = c("name", "desc"),
    warn_nodata = warn_nodata
  )
  object
})


.check_integer <- function(x, msg_end = NULL, skip_null = TRUE) {
  if (is.null(x) & skip_null) {
    return(invisible(NULL))
  }
  if (!is.numeric(x)) stop("Expecting integer values", msg_end)
  y <- x - as.integer(x)
  if (!all(y == 0)) {
    stop(
      "Non-integer values with fractions aassigned to integer parameter",
      msg_end
    )
  }
  return(invisible(NULL))
}

.check_intervals <- function(x, ...) {
  # class
  if (!is.data.frame(x)) stop("Expecting `data.frame`")
  # columns
  if (any(sapply(c("start", "mid", "end"), function(i) is.null(x[[i]])))) {
    stop('The `intervals` table must have "start", "mid", and "end" columns')
  }
  # columns' class
  for (i in c("start", "mid", "end")) {
    .check_integer(x[[i]], paste0(": intefvals$", i))
  }
  # NAs
  if (any(is.na(x))) stop("NA values are not allowed in `intervals` table")
  # consistency of the data
  if (any(x$mid - x$start < 0)) stop("intervals$mid must be >= intervals$start")
  if (any(x$end - x$mid < 0)) stop("intervals$end must be >= intervals$mid")
  if (any(diff(x$start <= 0), diff(x$mid <= 0), diff(x$end <= 0))) {
    stop("Data in `intervals` table mult be stricly ascending")
  }
  return(invisible(NULL))
}

## tests ####
if (F) {
  newHorizon()
  newHorizon(2020:2030)
  newHorizon(2020:2030, c(1, 2, 5, 10))
  newHorizon(2020:2035, c(1, 2, 5, 5, 5))
  newHorizon(2020:2050, c(1, 2, 5, 7, 1))

  newHorizon(
    intervals = data.frame(
      start = c(2030, 2031, 2034),
      mid =   c(2030, 2032, 2037),
      end =   c(2030, 2033, 2040)
    )
  )

  newHorizon(
    period = 2020:2050,
    intervals = data.frame(
      start = c(2030, 2031, 2034),
      mid =   c(2030, 2032, 2037),
      end =   c(2030, 2033, 2040)
    )
  )

  newHorizon(2020:2050, c(3, 2, 5, 10), desc = "")

  newHorizon(
    period = 2020:2040,
    intervals = data.frame(
      start = c(2030, 2032, 2035),
      mid =   c(2031, 2033, 2037),
      end =   c(2032, 2034, 2040)
    )
  )
}

# ToDo: write methods: ####
## `add` ####
## `update` ####
