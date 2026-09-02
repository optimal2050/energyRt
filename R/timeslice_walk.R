# =========================================================================== #
# timeslice_walk.R -- sequential, chronological access to a solved scenario.
#
# Walks the calendar's finest timeslices in order and hands back the rows of a
# caller-chosen set of variables and parameters for each one. Intended for
# per-slice work that has to see the year as a sequence: storage trajectories,
# dispatch profiles, and tests that assert slice-by-slice behaviour.
#
# Why not `getData()` in a loop: `getData()` collects the whole table and then
# filters in R (`get_data.R:424-432`), so a per-slice loop over it re-reads the
# entire dataset once per slice. The storage datasets are also unpartitioned --
# one directory per table, `part-0.<ext>` (`data2disk()`, arrow.R:405) -- so a
# per-slice predicate prunes no files. Both facts point the same way: open each
# table once, read in BLOCKS, and split in memory.
#
# Three properties of the stored data shape the contract:
#
#   * the solver writes only NON-ZERO rows (`glpk/energyRt.mod:1165`), so a
#     slice with no row is a structural zero, not the end of the series;
#   * a folded parameter carries NA as a WILDCARD in `timeslice` / `region`,
#     meaning "all of them". `%in%` does not match NA, so the block predicate
#     has to admit NA explicitly or every folded parameter silently vanishes;
#   * a table with no `timeslice` column at all (capacity, for instance) is
#     year-indexed and applies to every slice; it is read once and attached
#     unchanged to each step rather than re-read.
# =========================================================================== #

#' @include arrow.R
NULL

# Chronological finest-level slices of the scenario's calendar.
#
# `@timeframes[[leaf]]` is already in the timetable's row order, and the
# timetable's row order IS the calendar's chronology (class-calendar.R:586-592);
# `@next_in_year` is derived from it by wrapping the last slice onto the first,
# so the two agree by construction.
#
# NOT `.shape_slices()`: that anchors the walk at `sort(sl)[1]` because the
# cycle it generates shapes for has no canonical start. The rotation is
# harmless there and wrong here -- on `s4_h24` it starts the year at FAL rather
# than WIN, which a plotted profile or a storage trajectory would show.
.tw_slice_order <- function(scen) {
  cal <- tryCatch(scen@settings@calendar, error = function(e) NULL)
  if (is.null(cal) || length(cal@timeframes) == 0) {
    stop("The scenario carries no calendar to walk.", call. = FALSE)
  }
  lvl <- cal@default_timeframe
  sl <- as.character(cal@timeframes[[lvl]])
  if (!length(sl)) {
    stop("The calendar's finest timeframe ('", lvl, "') has no timeslices.",
         call. = FALSE)
  }
  sl
}

# The lazy query, columns and timeslice-awareness of one requested table.
.tw_source <- function(obj, nm, kind, filter) {
  if (is.null(obj)) return(NULL)
  cols <- tryCatch(obj@colNames, error = function(e) NULL)
  qu <- get_lazy_data(obj, slot = "data", collect_data = FALSE,
                      filter = filter, optional = TRUE)
  if (is.null(qu)) return(NULL)
  if (is.null(cols) || !length(cols)) cols <- .en_filter_cols(qu)
  list(name = nm, kind = kind, query = qu, cols = cols,
       has_ts = "timeslice" %in% (cols %||% character()))
}

# Collect a table that has no timeslice dimension. Read once, reused for every
# slice.
.tw_collect_static <- function(src) {
  d <- tryCatch(dplyr::collect(src$query), error = function(e) NULL)
  if (is.null(d)) return(NULL)
  force_cols_classes(as.data.frame(d))
}

# Rows of one timeslice-bearing table for a block of slices, keeping the NA
# wildcards that apply to every slice in it.
.tw_collect_block <- function(src, block) {
  q <- src$query
  d <- if (inherits(q, "data.frame")) {
    ts <- as.character(q$timeslice)
    q[ts %in% block | is.na(ts), , drop = FALSE]
  } else {
    # `%in%` alone drops NA, which is the wildcard a folded parameter uses to
    # mean "every timeslice" -- admit it explicitly.
    dplyr::filter(q, .data$timeslice %in% !!block | is.na(.data$timeslice)) |>
      dplyr::collect()
  }
  if (is.null(d)) return(NULL)
  force_cols_classes(as.data.frame(d))
}

#' Walk a solved scenario one timeslice at a time
#'
#' @description
#' Steps through the calendar's finest timeslices in chronological order,
#' returning the rows of the requested variables and parameters for each one.
#' Built for work that has to see the year as a sequence — storage
#' trajectories, dispatch profiles, and slice-by-slice tests.
#'
#' @details
#' Each table is opened once and read in blocks of `block` slices, with the
#' block pushed into the Arrow scanner; the block is then split in memory. That
#' is the opposite of calling [getData()] per slice, which collects the whole
#' table every time.
#'
#' Three conventions of the stored data are handled for you:
#'
#' * **Absent rows are zeros.** The solver writes only non-zero rows, so a
#'   slice with no row for a process is not the end of the series.
#' * **`NA` is a wildcard.** A folded parameter uses `NA` in `timeslice` to
#'   mean "every slice"; such rows are returned at every step.
#' * **Tables with no `timeslice` column** (capacity, lifetimes) are read once
#'   and attached unchanged to every step.
#'
#' @param scen a solved scenario.
#' @param vars character, solved variables to pull (e.g. `"vTechOut"`).
#' @param pars character, interpolated parameters to pull (e.g. `"pTechAf"`).
#' @param ... unused, for future extension.
#' @param slices character, the slices to walk and their order; defaults to the
#'   calendar's chronological order.
#' @param block integer, slices read from disk at a time. Larger is fewer,
#'   bigger reads.
#' @param filter named list of column = allowed values, pushed into the scanner
#'   (e.g. `list(region = "R1", year = 2030L)`).
#' @param fun optional function applied to each step; when given, the walk runs
#'   to completion and its results are returned as a list.
#'
#' @return With `fun`, a named list of its results, one per slice. Without, a
#'   `timeslice_walk` object: call `$next_slice()` for the next step (`NULL`
#'   when exhausted) or `$reset()` to start over.
#' @noRd
timeslice_walk <- function(scen, vars = NULL, pars = NULL, ...,
                           slices = NULL, block = 24L, filter = list(),
                           fun = NULL) {
  stopifnot(is(scen, "scenario"))
  vars <- as.character(vars %||% character())
  pars <- as.character(pars %||% character())
  if (!length(vars) && !length(pars)) {
    stop("Nothing to walk: give `vars` and/or `pars`.", call. = FALSE)
  }
  block <- max(1L, as.integer(block))

  order_all <- .tw_slice_order(scen)
  slices <- if (is.null(slices)) order_all else as.character(slices)
  unknown <- setdiff(slices, order_all)
  if (length(unknown)) {
    stop("Not timeslices of this scenario's calendar: ",
         paste(utils::head(unknown, 5), collapse = ", "),
         if (length(unknown) > 5) ", ..." else "", call. = FALSE)
  }

  srcs <- list()
  for (v in vars) {
    s <- .tw_source(scen@modOut@variables[[v]], v, "variable", filter)
    if (is.null(s)) {
      # an empty table is never written; that is normal, not an error
      warning("No stored data for variable '", v, "'; it will be empty at ",
              "every step.", call. = FALSE)
    } else {
      srcs[[v]] <- s
    }
  }
  for (p in pars) {
    s <- .tw_source(scen@modInp@parameters[[p]], p, "parameter", filter)
    if (is.null(s)) {
      warning("No stored data for parameter '", p, "'; it will be empty at ",
              "every step.", call. = FALSE)
    } else {
      srcs[[p]] <- s
    }
  }

  # year-indexed tables: read once, reused at every step
  static <- list()
  for (nm in names(srcs)) {
    if (!srcs[[nm]]$has_ts) static[[nm]] <- .tw_collect_static(srcs[[nm]])
  }
  dynamic <- names(srcs)[vapply(srcs, function(s) s$has_ts, logical(1))]

  share <- tryCatch(as.data.frame(scen@settings@calendar@timeslice_share),
                    error = function(e) NULL)
  share_of <- function(s) {
    if (is.null(share) || !"share" %in% names(share)) return(NA_real_)
    v <- share$share[as.character(share$timeslice) == s]
    if (length(v)) as.numeric(v[1]) else NA_real_
  }

  pos <- 0L
  cache <- list(block = character(), data = list())

  load_block <- function(i) {
    blk <- slices[seq(i, min(i + block - 1L, length(slices)))]
    d <- list()
    for (nm in dynamic) d[[nm]] <- .tw_collect_block(srcs[[nm]], blk)
    cache <<- list(block = blk, data = d)
  }

  step <- function() {
    if (pos >= length(slices)) return(NULL)
    pos <<- pos + 1L
    s <- slices[pos]
    if (!s %in% cache$block) load_block(pos)
    out <- list(timeslice = s, index = pos, share = share_of(s))
    for (nm in dynamic) {
      d <- cache$data[[nm]]
      out[[nm]] <- if (is.null(d) || !nrow(d)) d else {
        ts <- as.character(d$timeslice)
        d[ts == s | is.na(ts), , drop = FALSE]
      }
    }
    for (nm in names(static)) out[[nm]] <- static[[nm]]
    out
  }

  if (!is.null(fun)) {
    res <- vector("list", length(slices))
    names(res) <- slices
    for (i in seq_along(slices)) res[[i]] <- fun(step())
    return(res)
  }

  structure(
    list(next_slice = step,
         reset = function() { pos <<- 0L; invisible(NULL) },
         slices = slices, tables = names(srcs), block = block,
         scenario = scen@name),
    class = "timeslice_walk"
  )
}

# `print` is an S4 generic here whose default body is `UseMethod("print")`
# (print.R:11), so an S3 method has to be registered explicitly or dispatch
# falls through and dumps the raw list.
#' @exportS3Method print timeslice_walk
print.timeslice_walk <- function(x, ...) {
  cat("<timeslice_walk> scenario '", x$scenario, "'\n", sep = "")
  cat("  ", length(x$slices), " slices, blocks of ", x$block, "\n", sep = "")
  cat("  tables: ", paste(x$tables, collapse = ", "), "\n", sep = "")
  cat("  $next_slice() for the next step, $reset() to start over\n")
  invisible(x)
}
