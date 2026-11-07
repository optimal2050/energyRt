# =========================================================================== #
# Recasting a model onto another calendar: the time mirror of the spatial
# sampling in region.R.
#
# A model's objects carry their time-varying data on the calendar they were
# written against. Handing `newModel()` a different calendar therefore fails --
# the data still names the old timeslices -- so moving a model between
# resolutions is a transformation of the objects, not of the config.
#
# Three rules, each of which the model refuses without:
#
# 1. EVERY timeslice-indexed value is a RATE. The equations multiply it by
#    `pTimesliceWeight` to make an annual quantity (`eqSupCost`, `eqTechVarom`,
#    `eqTradeIrCost`, ... in glpk/energyRt.mod), so the aggregation rule is
#    `weighted_mean` and the weighted annual total is conserved exactly. Going
#    the other way the rule is `copy`: the same rate in every child timeslice.
#
# 2. Only rows AT the source calendar's leaf timeframe are recast. A row
#    declared at `ANNUAL` -- every fuel supply, import and fuel trade in TOPIA
#    -- passes through untouched; recasting it would invent sub-annual detail
#    the object never declared, which interpolation then refuses.
#
# 3. The DECLARATIONS move with the data. A `@timeframe` naming a timeframe the
#    target calendar does not have leaves the model referencing an undeclared
#    name, so it falls back to the finest timeframe that survives.
#
# What the result is NOT: a smaller answer to the same question. The variation
# inside each collapsed timeslice is gone, so dispatch, costs and the objective
# are the recast model's own.
# =========================================================================== #

#' @include class-calendar.R timeslices.R
NULL

# Numeric columns that index rather than measure, and so are never recast.
# `year` is the only one in the shipped objects; `vintage` is character there
# but integer in some user models, so it is guarded too.
.recast_id_cols <- c("year", "vintage")

#' Recast a model onto another calendar
#'
#' Moves a model's time-varying data from the calendar it was written against
#' onto `calendar`, so the model can be built and solved at another time
#' resolution. The time mirror of [subset_model_regions()] and
#' [aggregate_model_regions()].
#'
#' Every timeslice-indexed value in energyRt is a rate: the equations multiply
#' it by the timeslice weight to obtain an annual quantity. Aggregating
#' therefore takes the weighted mean, which conserves the weighted annual
#' total exactly, and disaggregating copies the rate into each child timeslice.
#' `rules` overrides this per column where a value is not a rate (a declared
#' quantity to be summed, a standard deviation).
#'
#' Rows are recast by their own timeframe, not by their table: only rows at the
#' source calendar's leaf timeframe move. A row declared at `ANNUAL`, or at any
#' timeframe `calendar` also has, is left exactly as it is.
#'
#' A `@timeframe` naming a timeframe `calendar` does not have falls back to the
#' finest timeframe that survives, and is reported -- otherwise the model
#' references a timeframe its calendar cannot resolve.
#'
#' @section Comparability:
#' A recast model is not a smaller answer to the same question. Collapsing
#' timeslices removes the variation inside them, which is what a model prices:
#' peaks flatten, variable resources become firmer than they are, and storage
#' loses the cycle it was built to serve. Its dispatch, costs and objective are
#' its own. Use it to run, plot and teach the shape of a solution, not to
#' report the source model's numbers.
#'
#' @param object a [model].
#' @param calendar the target [calendar][class-calendar]; coarser or finer than
#'   the model's own.
#' @param rules optional named character vector of
#'   [timescales::CALENDAR_RULES] keyed by value column, overriding the rate
#'   default for those columns (e.g. `c(demand = "sum")`).
#' @param year model year used to materialise both calendars on a shared
#'   datetime grid. `NULL` (default) takes the model's first horizon milestone.
#'   It matters only where the calendars align to real dates (days of the year,
#'   weekdays), and then only across a leap year.
#' @param verbose print per-table messages.
#'
#' @return the recast [model], carrying `calendar`.
#' @family interpolation
#' @export
#' @examples
#' mod <- newModel("TOPIA", data = topia$modules$electricity$repo,
#'                 calendar = topia$modules$calendars$s4_h24,
#'                 region = topia$modules$electricity$regions,
#'                 horizon = topia$modules$horizons$BY2025,
#'                 discount = 0.05)
#'
#' # 96 hourly timeslices collapse to 4 seasons: the same model, small enough
#' # to solve in a second
#' light <- recast_model_calendar(mod, calendar = calendars$s4)
#' getCalendar(light)
#'
#' # the hourly demand profile, and the four seasonal rates it becomes
#' dem <- getObject(light, name = "DEM_ELC", drop = TRUE)
#' head(dem@demand)
recast_model_calendar <- function(object, calendar, rules = NULL, year = NULL,
                                  verbose = isVerbose()) {
  stopifnot(inherits(object, "model"))
  if (!methods::is(calendar, "calendar")) {
    stop("`calendar` must be an energyRt calendar; see newCalendar() and ",
         "the `calendars` catalog.", call. = FALSE)
  }
  check_package("timescales")
  if (!is.null(rules)) {
    if (!is.character(rules) || is.null(names(rules)) ||
        any(!nzchar(names(rules)))) {
      stop("`rules` must be a named character vector, keyed by value column ",
           "(e.g. c(demand = \"sum\")).", call. = FALSE)
    }
    bad <- setdiff(unname(rules), timescales::CALENDAR_RULES)
    if (length(bad)) {
      stop("unknown rule(s): ", paste(bad, collapse = ", "),
           ". Available: ", paste(timescales::CALENDAR_RULES, collapse = ", "),
           ".", call. = FALSE)
    }
  }
  src <- object@config@calendar
  if (!nrow(src@timetable)) {
    stop("the model carries no calendar to recast from.", call. = FALSE)
  }

  ts_src <- .ts_calendar(src)
  ts_tgt <- .ts_calendar(calendar)
  if (is.null(ts_src) || is.null(ts_tgt)) {
    stop("recasting needs both calendars to carry a timeframe below ANNUAL; ",
         "a model at annual resolution has no timeslices to recast.",
         call. = FALSE)
  }

  src_leaves <- as.character(src@timetable$timeslice)
  tgt_tfs <- .calendar_timeframes(calendar)
  finest <- tgt_tfs[length(tgt_tfs)]
  n_src <- length(src_leaves)
  n_tgt <- nrow(calendar@timetable)
  if (identical(n_src, n_tgt) && setequal(src_leaves,
                                          as.character(calendar@timetable$timeslice))) {
    .say("recast: the model already carries this calendar; nothing to do.")
    return(object)
  }
  rule_default <- if (n_tgt < n_src) "weighted_mean" else "copy"
  if (is.null(year)) year <- .recast_year(object)

  st <- list(tables = 0L, rows_in = 0L, rows_out = 0L, kept = 0L,
             tf = character(0), dropped = character(0))

  recast_el <- function(el) {
    if (methods::is(el, "repository")) {
      for (nm in names(el@data)) el@data[[nm]] <- recast_el(el@data[[nm]])
      return(el)
    }
    if (!isS4(el)) return(el)
    nm_el <- tryCatch(el@name, error = function(e) "<unnamed>")
    for (sn in .instance_slots(el)) {
      if (identical(sn, "misc")) next
      v <- methods::slot(el, sn)
      if (is.data.frame(v) && nrow(v) > 0L && "timeslice" %in% colnames(v)) {
        r <- .recast_table(v, ts_src = ts_src, ts_tgt = ts_tgt,
                           src_leaves = src_leaves, rule_default = rule_default,
                           rules = rules, year = year,
                           where = paste0(nm_el, "@", sn))
        if (!is.null(r$data)) {
          st$tables <<- st$tables + 1L
          st$rows_in <<- st$rows_in + nrow(v)
          st$rows_out <<- st$rows_out + nrow(r$data)
          st$kept <<- st$kept + r$kept
          if (length(r$dropped)) st$dropped <<- c(st$dropped, r$dropped)
          if (isTRUE(verbose)) {
            .say(sprintf("  %-28s %6d -> %6d row(s)%s",
                            paste0(nm_el, "@", sn), nrow(v), nrow(r$data),
                            if (r$kept) paste0(" (", r$kept, " kept as declared)")
                            else ""))
          }
          methods::slot(el, sn) <- r$data
        }
        next
      }
      # the declarations: a timeframe the target does not have
      if (grepl("timeframe", sn, ignore.case = TRUE) && is.character(v) &&
          length(v) > 0L) {
        bad <- !is.na(v) & nzchar(v) & !v %in% c(tgt_tfs, "ANNUAL")
        if (any(bad)) {
          st$tf <<- c(st$tf, sprintf("%s@%s (%s)", nm_el, sn,
                                     paste(unique(v[bad]), collapse = ", ")))
          v[bad] <- finest
          methods::slot(el, sn) <- v
        }
      }
    }
    el
  }

  for (i in seq_along(object@data)) object@data[[i]] <- recast_el(object@data[[i]])

  object@config@calendar <- calendar
  # The recast is no longer the stored version it was built from, and
  # `save_model()` updates an entry in place BY NAME, so the parent's hash
  # would let it overwrite the source model's entry while claiming to be it.
  object@misc$hash <- NULL

  .say("recast onto calendar '", calendar@name, "' (", n_src, " -> ", n_tgt,
          " timeslices): ", st$tables, " table(s), ",
          format(st$rows_in, big.mark = ","), " -> ",
          format(st$rows_out, big.mark = ","), " row(s)",
          if (st$kept) paste0(", ", format(st$kept, big.mark = ","),
                              " row(s) kept at their declared timeframe")
          else "",
          if (length(st$tf)) paste0("; ", length(st$tf),
                                    " timeframe declaration(s) -> ", finest,
                                    if (isTRUE(verbose))
                                      paste0(": ", paste(st$tf, collapse = "; "))
                                    else " (verbose = TRUE lists them)")
          else "",
          ".\nDispatch, costs and the objective are the recast model's own, ",
          "not the source model's.")
  if (length(st$dropped)) {
    warning("the target calendar does not cover every source timeslice; ",
            "data was lost in ", length(unique(st$dropped)), " table(s): ",
            paste(unique(st$dropped), collapse = ", "), call. = FALSE)
  }
  object
}

# The timeframes of an energyRt calendar, coarsest first, ANNUAL dropped.
#' @noRd
.calendar_timeframes <- function(calendar) {
  tfs <- setdiff(names(calendar@timetable),
                 c("timeslice", "share", "weight", "ANNUAL"))
  if (!length(tfs)) "ANNUAL" else tfs
}

# The year both calendars are materialised on: the model's first milestone.
#' @noRd
.recast_year <- function(object) {
  h <- tryCatch(object@config@horizon, error = function(e) NULL)
  y <- tryCatch(as.integer(h@intervals$mid), error = function(e) integer())
  if (!length(y)) y <- tryCatch(as.integer(h@period), error = function(e) integer())
  if (!length(y) || all(is.na(y))) return(2025L)
  min(y, na.rm = TRUE)
}

# One table. Returns `NULL` data when there is nothing to do, so the caller can
# leave the slot alone and not count it.
#' @noRd
.recast_table <- function(d, ts_src, ts_tgt, src_leaves, rule_default, rules,
                          year, where) {
  num <- names(d)[vapply(d, is.numeric, logical(1))]
  values <- setdiff(num, c(.recast_id_cols, "timeslice"))
  if (!length(values)) return(list(data = NULL, kept = 0L, dropped = character(0)))
  tsl <- as.character(d[["timeslice"]])
  at_leaf <- !is.na(tsl) & tsl %in% src_leaves
  if (!any(at_leaf)) return(list(data = NULL, kept = 0L, dropped = character(0)))

  rule <- stats::setNames(rep(rule_default, length(values)), values)
  if (!is.null(rules)) {
    hit <- intersect(names(rules), values)
    rule[hit] <- unname(rules[hit])
  }

  src <- d[at_leaf, , drop = FALSE]
  src[["timeslice"]] <- tsl[at_leaf]
  dropped <- character(0)
  # `recast_calendar(rule=)` takes ONE rule for the whole call, so a table
  # whose columns need different rules is recast once per rule and joined back
  # on its identifier columns.
  cast <- function(vals, rl) {
    withCallingHandlers(
      as.data.frame(timescales::recast_calendar(
        src[, c(setdiff(colnames(src), values), vals), drop = FALSE],
        source = ts_src, target = ts_tgt, year = year,
        key = "timeslice", values = vals, rule = rl)),
      warning = function(w) {
        if (grepl("not covered|were ignored", conditionMessage(w))) {
          dropped <<- c(dropped, where)
        }
        invokeRestart("muffleWarning")
      })
  }
  groups <- split(values, unname(rule[values]))
  parts <- lapply(names(groups), function(rl) cast(groups[[rl]], rl))
  out <- parts[[1L]]
  if (length(parts) > 1L) {
    for (p in parts[-1L]) {
      on <- setdiff(intersect(colnames(out), colnames(p)), values)
      out <- merge(out, p, by = on, all = TRUE, sort = FALSE)
    }
  }
  # `recast_calendar()` returns the full target vocabulary, NA where the
  # target covers a slice the source never populated.
  out <- out[!is.na(out[["timeslice"]]), , drop = FALSE]
  keep_cols <- intersect(colnames(d), colnames(out))
  out <- out[, keep_cols, drop = FALSE]
  # A target timeslice the source never populated carries NA in every value
  # column and is not a row the object declared. A row with SOME values is
  # kept: energyRt tables routinely leave one bound of a pair empty.
  vcols <- intersect(values, keep_cols)
  if (length(vcols)) {
    has_value <- rowSums(!is.na(out[, vcols, drop = FALSE])) > 0L
    out <- out[has_value, , drop = FALSE]
  }
  if (all(at_leaf)) {
    return(list(data = out, kept = 0L, dropped = dropped))
  }
  rest <- d[!at_leaf, keep_cols, drop = FALSE]
  list(data = rbind(out, rest), kept = nrow(rest), dropped = dropped)
}
