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
# Representation note: start/end/olife are ordinary keyed slot columns --
# collected by (vintage, region, cluster) like any other slot, with the
# universal broadcast rule (an NA key applies to all members; a specific
# row overrides the broadcast). An NA in a VALUE
# column simply means "not specified by this row", so a global `start`
# row can sit beside per-region `olife` rows without materialising the
# global value per region. Reads go through `.lifespan_col()` (keys kept)
# and `.lifespan_resolve()` (one row per key, conflicts are errors).
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

# Leading token of the constraint names generated to TIE variant capacities to
# fixed proportions of one another (loss tranches). Distinct from the group
# prefix above because the two constrain different things: a group bound caps
# the SUM over variants, a share tie fixes their RATIO -- and a name collision
# between them would be silently overwritten at insertion.
.VARIANT_SHARE_PREFIX <- "VS"

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
# Returns the argument list with start/end/olife removed. Class-agnostic: used by
# `newTechnology`/`newStorage`/`newTrade` and their `update` methods.
.tech_lifespan_args <- function(args) {
  legacy <- c("start", "end", "olife")
  present <- intersect(legacy, names(args))
  # An all-infinite bound carries no information -- `newTrade()` defaults to
  # `start = -Inf` / `end = Inf` meaning "always" -- and `as.integer(Inf)` is NA
  # with a warning. Treat those as absent; interp already normalises Inf -> NA.
  .all_inf <- function(x) {
    v <- if (is.data.frame(x)) unlist(x[vapply(x, is.numeric, logical(1))]) else
      if (is.list(x)) unlist(x) else x
    v <- suppressWarnings(as.numeric(v))
    length(v) > 0L && all(is.infinite(v) | is.na(v))
  }
  # drop empty legacy args so the constructors' data.frame() defaults do not
  # count as "supplied"
  nonempty <- present[vapply(present, function(n) {
    x <- args[[n]]
    if (is.null(x)) return(FALSE)
    if (is.data.frame(x) && nrow(x) == 0L) return(FALSE)
    if (is.list(x) && !is.data.frame(x) && length(x) == 0L) return(FALSE)
    if (!is.list(x) && length(x) == 0L) return(FALSE)
    !.all_inf(x)
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

# `trade@capacityVariable` was removed: a trade's capacity is always a decision
# variable, as for technology and storage. In this version the slot gated nothing
# -- `mTradeCapacityVariable` was declared in the templates but referenced by
# none of them, and `vTradeCap`/`eqTradeCap`/`eqTradeCapFlow` were built either
# way -- so a `FALSE` trade still paid investment and fixed O&M. Fail loudly with
# the remedy rather than silently ignoring the argument.
# @noRd
.trade_removed_args <- function(args) {
  if (!"capacityVariable" %in% names(args)) return(args)
  stop("`capacityVariable` has been removed from the `trade` class: a trade's ",
       "capacity is always a decision variable, as for technology and storage. ",
       "It gated nothing in this version (the set it produced was declared but ",
       "never referenced by any solver template). For a fixed transfer limit ",
       "with no investment decision, leave `capacity`/`invcost` empty and set ",
       'the limit on the flow instead, e.g. `trade = data.frame(src = "R1", ',
       'dst = "R2", ava.up = 5)`.')
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

.lifespan_keys <- c("vintage", "region", "cluster")

.empty_lifespan_col <- function(col) {
  out <- data.frame(vintage = character(), region = character(),
                    cluster = character(), stringsAsFactors = FALSE)
  out[[col]] <- integer()
  out
}

# One lifespan column WITH its selector keys: a frame with `vintage`,
# `region`, `cluster` plus the value column, NA values dropped.
# `start`/`end`/`olife` are collected by (vintage, region, cluster) like
# any other slot column (user ruling 2026-08-13) -- the keys travel with
# the value instead of being silently discarded.
#
# `technology`/`storage`/`trade` read the column out of `@vintage`; the
# legacy branch reads a raw slot (dead for these classes) with NO
# grouping or coalescing.
.lifespan_col <- function(obj, col) {
  src <- if (.hasSlot(obj, "vintage")) {
    as.data.frame(obj@vintage)
  } else if (.hasSlot(obj, col)) {
    as.data.frame(slot(obj, col))
  } else {
    return(.empty_lifespan_col(col))
  }
  if (nrow(src) == 0L || !col %in% names(src)) return(.empty_lifespan_col(col))
  for (k in .lifespan_keys) {
    if (!k %in% names(src)) src[[k]] <- NA_character_
  }
  src |>
    filter(!is.na(.data[[col]])) |>
    select(all_of(c(.lifespan_keys, col)))
}

# Resolved lifespan column: at most ONE row per (vintage, region, cluster)
# key. Exact-duplicate keys carrying the SAME value are deduplicated;
# carrying DIFFERENT values they are a hard error naming the process --
# never silently averaged or first-row-picked (techspec policy applied to
# slots). Broadcast rows (NA in a key) legitimately coexist with specific
# rows: the standard interpolation rule applies downstream -- a specific
# row overrides the broadcast for its key, the broadcast fills the rest.
.lifespan_resolve <- function(obj, col) {
  nm <- tryCatch(obj@name, error = function(e) "?")
  .lifespan_resolve_df(.lifespan_col(obj, col), col, nm)
}

# Frame-level core of the resolver, for callers that already hold the
# `@vintage` table (e.g. the ob2mi parameter path). `src` may be the raw
# slot (keys added, NA values dropped) or an already-keyed column frame.
.lifespan_resolve_df <- function(src, col, name = "?") {
  d <- src
  if (!all(.lifespan_keys %in% names(d))) {
    for (k in setdiff(.lifespan_keys, names(d))) d[[k]] <- NA_character_
  }
  if (!col %in% names(d)) return(.empty_lifespan_col(col))
  d <- d[!is.na(d[[col]]), c(.lifespan_keys, col), drop = FALSE]
  if (nrow(d) <= 1L) {
    rownames(d) <- NULL
    return(d)
  }
  key <- do.call(paste, c(lapply(d[.lifespan_keys], as.character),
                          sep = "\r"))
  conflicting <- vapply(split(d[[col]], key),
                        function(v) length(unique(v)) > 1L, logical(1))
  if (any(conflicting)) {
    stop("conflicting `", col, "` values in `@vintage` of '", name,
         "' for the same (vintage, region, cluster) key; declare one ",
         "value per key", call. = FALSE)
  }
  out <- d[!duplicated(key), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# Fill-missing broadcast: per-region resolved values over a given region
# set. Region-specific rows win; the region-NA broadcast row supplies the
# regions without one. Assumes vintage/cluster are constant (the
# post-expansion cell shape); multiple broadcast rows are a conflict
# caught by .lifespan_resolve().
.lifespan_by_region <- function(obj, col, regions) {
  d <- .lifespan_resolve(obj, col)
  specific <- d[!is.na(d$region), , drop = FALSE]
  broadcast <- d[is.na(d$region), , drop = FALSE]
  out <- data.frame(region = regions, stringsAsFactors = FALSE)
  # typed NA of the value column when no broadcast row exists
  out[[col]] <- if (nrow(broadcast) > 0L) broadcast[[col]][1] else
    d[[col]][NA_integer_]
  m <- match(out$region, specific$region)
  hit <- !is.na(m)
  out[[col]][hit] <- specific[[col]][m[hit]]
  out
}
