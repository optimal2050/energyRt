# Predicate pushdown for on-disk data ------------------------------------------
#
# A filter is a named list mapping a column to the values to keep, e.g.
# `list(timeslice = c("d001_h00", "d001_h01"), region = c("ES111", "PT111"))`.
# Applied to an Arrow query it restricts what is read from disk; applied to a
# data frame it subsets in memory, so both branches of `get_lazy_data()` return
# the same rows.
#
# Columns named in the filter that the data does not have are ignored: a filter
# is a restriction on the dimensions a dataset happens to carry, and requiring
# every dataset to carry every dimension would make it unusable as a default.

# Apply a named-list filter to an Arrow query or a data frame.
.en_apply_filter <- function(x, filter) {
  if (is.null(x) || is.null(filter) || !length(filter)) return(x)
  if (!is.list(filter) || is.null(names(filter)) || any(!nzchar(names(filter)))) {
    stop("`filter` must be a named list of column = allowed values",
         call. = FALSE)
  }
  cols <- .en_filter_cols(x)
  if (is.null(cols)) return(x)
  for (nm in names(filter)) {
    vals <- filter[[nm]]
    if (!nm %in% cols || is.null(vals) || !length(vals)) next
    x <- if (inherits(x, "data.frame")) {
      x[as.character(x[[nm]]) %in% as.character(vals), , drop = FALSE]
    } else {
      # `filter()` on an arrow_dplyr_query is lazy; the restriction reaches the
      # scanner and limits what is read.
      dplyr::filter(x, !!rlang::sym(nm) %in% !!as.character(vals))
    }
  }
  x
}

# Column names of an Arrow query, Dataset or data frame; NULL when unknown.
.en_filter_cols <- function(x) {
  if (inherits(x, "data.frame")) return(names(x))
  out <- tryCatch(names(x), error = function(e) NULL)
  if (is.null(out)) out <- tryCatch(x$schema$names, error = function(e) NULL)
  out
}

# Build a filter list from the sets a scenario actually requested, dropping
# entries that would not restrict anything.
#
# `declared` is the full set of values present in the data and `wanted` the
# subset asked for; when they match, the column is left out so the query stays
# unfiltered rather than carrying a no-op `%in%` over every value.
.en_build_filter <- function(...) {
  spec <- list(...)
  spec <- spec[!vapply(spec, is.null, logical(1))]
  spec <- spec[vapply(spec, length, integer(1)) > 0]
  if (!length(spec)) return(NULL)
  spec
}
