# ========================================================================== #
# Lifespan / vintage helpers
#
# The `technology` class used to carry three single-purpose lifespan slots,
# `@start`, `@end` and `@olife`, each a `(region, value)` table. They are now
# columns of the single `@vintage` table, keyed on `(vintage, region, cluster)`.
#
# `.tech_lifespan_args()` translates the legacy `start=` / `end=` / `olife=`
# ARGUMENTS into a `vintage=` table, so existing model code keeps working. It is
# called from both `newTechnology()` and `update()`; the legacy names must never
# reach `.data2slots()`, which no longer knows them.
#
# Note there is deliberately NO upgrade path for objects serialised before the
# merge -- a saved model carrying the old slots must be rebuilt from source.
#
# Representation note: the merged table is a COLUMN-WISE union, not a row-wise
# join. Each of start/end/olife keeps its own region granularity and an NA in a
# column simply means "not specified by this row". This is the same idiom the
# `@ceff` slot already uses (one table, many value columns, each feeding its own
# parameter, NA rows dropped per parameter), and it is what makes the merge
# behaviour-preserving: a global `start` alongside a per-region `olife` does not
# force the global value to be materialised per region.
#
# Everything here is written with dplyr verbs (package convention) so the same
# code paths work over data.frame, data.table (via dtplyr) and arrow.
# ========================================================================== #

# Reserved token marking a group-aggregate (rather than per-variant) bound.
.VARIANT_TOTAL <- "TOTAL"

# Leading token of the constraint names generated for group-aggregate bounds.
# Not part of `config@variant_prefix`: those are infixes inside a process name,
# whereas this starts a constraint name and so must satisfy check_name()'s
# "begins with a letter" rule.
.VARIANT_GROUP_PREFIX <- "VG"

# Columns of the @vintage slot, in prototype order.
.vintage_cols <- c("vintage", "region", "cluster", "start", "end", "olife")

# The value columns of @vintage (as opposed to the selector columns).
.vintage_val_cols <- c("start", "end", "olife")

.empty_lifespan <- function() {
  data.frame(
    vintage = character(), region = character(), cluster = character(),
    start = integer(), end = integer(), olife = integer(),
    stringsAsFactors = FALSE
  )
}

# Pad a frame out to the full @vintage column set, typing added columns.
.pad_vintage_cols <- function(x) {
  miss <- setdiff(.vintage_cols, names(x))
  for (cc in miss) {
    x[[cc]] <- if (cc %in% .vintage_val_cols) NA_integer_ else NA_character_
  }
  x |> select(all_of(.vintage_cols))
}

# Normalise one legacy lifespan argument to a frame with the selector columns
# plus the single value column `value_col`. Accepts:
#   scalar / length-1 vector      olife = 30
#   named list                    olife = list(olife = 30)
#   data.frame, value only        start = data.frame(start = 2020)
#   data.frame, region + value    start = data.frame(region = ..., start = ...)
# Returns NULL when nothing was supplied.
.lifespan_one <- function(x, value_col) {
  if (is.null(x)) return(NULL)
  if (is.list(x) && !is.data.frame(x)) {
    if (length(x) == 0L) return(NULL)
    x <- as.data.frame(x, stringsAsFactors = FALSE)
  }
  if (!is.data.frame(x)) {
    # bare scalar / vector -> selector-agnostic rows
    x <- x[!is.na(x)]
    if (length(x) == 0L) return(NULL)
    x <- data.frame(value = x, stringsAsFactors = FALSE)
    names(x) <- value_col
  }
  if (nrow(x) == 0L) return(NULL)

  nms <- names(x)
  vcol <- intersect(c(value_col, "value"), nms)
  if (length(vcol) == 0L) {
    stop("The `", value_col, "` argument must contain a `", value_col,
         "` column; got: ", paste(nms, collapse = ", "))
  }
  unknown <- setdiff(nms, c("region", "vintage", "cluster", vcol[1]))
  if (length(unknown) > 0L) {
    stop("Unknown column(s) in `", value_col, "`: ",
         paste(unknown, collapse = ", "))
  }

  # NOTE do not coerce the value here. `storage`/`trade` may carry `olife = Inf`,
  # which `is.infinite()` checks downstream rely on (`.lifespan_olife_inf()`,
  # `map_mStorageOlifeInf()`); `as.integer(Inf)` would silently become NA.
  # Typing is applied by `.data2slots()` from the slot prototype instead.
  x |>
    rename(.val = all_of(vcol[1])) |>
    mutate(
      vintage = if ("vintage" %in% nms) as.character(vintage) else NA_character_,
      region  = if ("region"  %in% nms) as.character(region)  else NA_character_,
      cluster = if ("cluster" %in% nms) as.character(cluster) else NA_character_,
      "{value_col}" := .val
    ) |>
    filter(!is.na(.data[[value_col]])) |>
    select(all_of(c("vintage", "region", "cluster", value_col)))
}

# Combine normalised start/end/olife frames into one @vintage table, coalescing
# rows that share the same (vintage, region, cluster) key so the common case
# (all three given at the same granularity) yields one row per region.
.lifespan_combine <- function(parts) {
  parts <- parts[!vapply(parts, is.null, logical(1))]
  if (length(parts) == 0L) return(NULL)

  out <- parts |>
    lapply(.pad_vintage_cols) |>
    bind_rows() |>
    group_by(vintage, region, cluster) |>
    summarise(across(all_of(.vintage_val_cols), ~ {
      z <- .x[!is.na(.x)]
      # `.x[1]` keeps the column's own type for the all-NA case (a bare `NA`
      # would be logical); `z[1]` preserves Inf, which storage/trade rely on
      if (length(z) == 0L) .x[1] else z[1]
    }), .groups = "drop") |>
    arrange(vintage, region, cluster) |>
    as.data.frame()

  if (nrow(out) == 0L) NULL else out
}

# Translate legacy lifespan arguments in an argument list into `vintage`.
# Returns the argument list with start/end/olife removed.
.tech_lifespan_args <- function(args) {
  legacy <- c("start", "end", "olife")
  present <- intersect(legacy, names(args))
  # drop empty legacy args so `newTechnology()`'s data.frame() defaults do not
  # count as "supplied"
  nonempty <- present[vapply(present, function(n) {
    x <- args[[n]]
    !(is.null(x) || (is.data.frame(x) && nrow(x) == 0L) ||
        (is.list(x) && !is.data.frame(x) && length(x) == 0L) ||
        (!is.list(x) && length(x) == 0L))
  }, logical(1))]

  has_vintage <- "vintage" %in% names(args) &&
    is.data.frame(args$vintage) && nrow(args$vintage) > 0L

  if (length(nonempty) > 0L && has_vintage) {
    stop("Supply either `vintage` or the legacy `",
         paste(nonempty, collapse = "`/`"),
         "` argument(s), not both. `start`/`end`/`olife` are now columns of ",
         "the `vintage` slot.")
  }

  parts <- lapply(nonempty, function(n) .lifespan_one(args[[n]], n))
  args[present] <- NULL  # never let legacy names reach .data2slots()

  vin <- .lifespan_combine(parts)
  if (is.null(vin)) return(args)
  args$vintage <- vin
  args
}

# -------------------------------------------------------------------------- #
# Reading lifespans back out
#
# `technology` stores its lifespan in `@vintage`; `storage` and `trade` still
# use the three separate slots. These accessors hide that difference so every
# consumer works with one shape, whichever class it is handed.
# -------------------------------------------------------------------------- #

# Full lifespan table for a `technology` (which keeps everything in @vintage).
.proc_lifespan <- function(obj) {
  if (!.hasSlot(obj, "vintage")) return(.empty_lifespan())
  v <- as.data.frame(obj@vintage)
  if (nrow(v) == 0L) return(.empty_lifespan())
  .pad_vintage_cols(v)
}

.empty_lifespan_col <- function(col) {
  out <- data.frame(region = character(), stringsAsFactors = FALSE)
  out[[col]] <- integer()
  out
}

# One lifespan column in the shape the removed slot used to have: a frame with
# `region` plus the value column, NA values dropped. Drop-in replacement for
# `obj@start` / `obj@end` / `obj@olife`.
#
# `technology` reads the column out of `@vintage`; `storage`/`trade` still read
# their own slot. The legacy branch reads the raw slot with NO grouping or
# coalescing: `trade@olife` is keyed on `year` (not `region`) and may carry
# several rows, so collapsing them could drop a distinct value or an `Inf` that
# `.lifespan_olife_inf()` needs to see.
.lifespan_col <- function(obj, col) {
  src <- if (.hasSlot(obj, "vintage")) {
    as.data.frame(obj@vintage)
  } else if (.hasSlot(obj, col)) {
    as.data.frame(slot(obj, col))
  } else {
    return(.empty_lifespan_col(col))
  }
  if (nrow(src) == 0L || !col %in% names(src)) return(.empty_lifespan_col(col))
  if (!"region" %in% names(src)) src$region <- NA_character_
  src |>
    filter(!is.na(.data[[col]])) |>
    select(all_of(c("region", col)))
}
