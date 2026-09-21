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
    # `rep(, nrow(x))`, not a scalar: a 0-row frame rejects a length-1
    # replacement. Reachable since `trade@vintage` stopped carrying `region`,
    # which made `miss` non-empty for an empty trade lifespan.
    x[[cc]] <- rep(if (cc %in% .vintage_val_cols) NA_integer_ else
                   NA_character_, nrow(x))
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
.tech_lifespan_args <- function(args, cls = NULL) {
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
  # `trade@vintage` has no `region` column. The combine above works on the
  # common shape, so drop it here -- but only once it is empty: the legacy
  # `olife = data.frame(region = , olife = )` form reaches this point with a
  # populated column, and dropping that silently would discard user data.
  if (identical(cls, "trade")) {
    if ("region" %in% names(vin) && any(!is.na(vin$region))) .trade_region_stop()
    vin$region <- NULL
  }
  args$vintage <- vin
  args
}

# `trade@vintage` carries no `region`: a trade's lifespan belongs to the route.
# Reject a populated one here, where the remedy can be named, rather than let
# `.data2slots()` report an unknown column. An all-NA column is dropped
# silently -- that is what an object written before the column was removed, or
# built from the common shape, carries.
# @noRd
.trade_region_stop <- function() {
  stop("a trade's lifespan carries no `region`: it belongs to the route. ",
       "`pTradeOlife` is indexed by trade alone, and `mTradeSpan`/`mTradeNew` ",
       "are (trade, year) in every solver template, so `start`/`end`/`olife` ",
       "have nowhere to put one. Drop the region key. Per-region trade COSTS ",
       "are supported -- set `region` on `invcost`/`fixom` instead.",
       call. = FALSE)
}

.trade_vintage_region <- function(args) {
  v <- args$vintage
  if (!is.data.frame(v) || !"region" %in% names(v)) return(args)
  if (any(!is.na(v$region))) .trade_region_stop()
  args$vintage$region <- NULL
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
    # `rep(, nrow(d))`: a 0-row frame rejects a length-1 replacement. Reached
    # by a trade with an empty `@vintage`, whose slot has no `region` column.
    for (k in setdiff(.lifespan_keys, names(d))) {
      d[[k]] <- rep(NA_character_, nrow(d))
    }
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


# ---------------------------------------------------------------------------
# (was R/combine_vintages.R, merged 2026-09-20)
# ---------------------------------------------------------------------------

# =============================================================================
# combine_vintages() — merge per-vintage technology objects into one
# =============================================================================
# Workbooks and technology catalogs are often imported as one technology
# object PER vintage year (FDAC_S_VIN2030,
# FDAC_S_VIN2040, ...). The vintage machinery makes that one object with a
# `@vintage` table and vintage-keyed parameter rows; this is the
# combinator that gets you there from the per-vintage list.
#
# Policy (mirrors the techspec rules): nothing is invented — every value
# comes from one of the inputs, tagged with its vintage; rows identical
# across ALL vintages collapse to a single all-vintages row (vintage NA)
# so the combined tables stay readable.

#' Combine per-vintage technology objects into one vintaged technology
#'
#' Merges a list of `technology` objects, one per build-year vintage
#' (e.g. `FDAC_S_VIN2030`, `FDAC_S_VIN2040`, ...), into a single
#' technology with a [`@vintage`][newTechnology] table and vintage-keyed
#' parameter rows — the "one object, many vintages" form introduced with
#' the vintage machinery.
#'
#' * Ports (`input`/`output`/`aux`/`group`) are the union across
#'   vintages (a commodity present only in some vintages is fine: its
#'   `ceff` rows carry the vintage key).
#' * Structural attributes (`units`, `cap2act`, `timeframe`, `region`,
#'   `fullYear`) are taken from the first object; differences warn.
#' * Every parameter row is tagged with its object's vintage; rows that
#'   are identical across ALL vintages collapse to one all-vintages row
#'   (disable with `collapse_common = FALSE`).
#' * Investment windows: each vintage's `start` is kept (defaulting to
#'   the vintage year); with `close_windows = TRUE` a missing `end` is
#'   closed at the next vintage's start minus one (the last stays open).
#'
#' @param techs A named list of `technology` objects, one per vintage.
#'   Objects declaring clusters are not supported.
#' @param vintages Optional character vector of vintage labels (same
#'   length as `techs`). Default: parsed from `names(techs)`
#'   (`"..._VIN<label>"`), falling back to each object's
#'   `@vintage$start`.
#' @param name,desc Name/description of the combined technology.
#'   Defaults: the common `"..._VIN"` prefix of the input names, and the
#'   first object's description.
#' @param close_windows Close open-ended investment windows at the next
#'   vintage's start (default `TRUE`).
#' @param collapse_common Collapse rows identical across all vintages to
#'   a single all-vintages row (default `TRUE`).
#' @return A `technology` object with one `@vintage` row per input.
#' @examples
#' \dontrun{
#' dac <- make_dea_dac_techs()   # FDAC_S_VIN2020, FDAC_S_VIN2030, ...
#' fam <- split(dac, sub("_VIN[0-9]+$", "", names(dac)))
#' dac1 <- lapply(fam, combine_vintages)
#' draw(dac1$FDAC_S)             # vintage fan
#' }
#' @export
combine_vintages <- function(techs, vintages = NULL, name = NULL,
                             desc = NULL, close_windows = TRUE,
                             collapse_common = TRUE) {
  if (is(techs, "technology")) techs <- list(techs)
  if (!is.list(techs) || length(techs) == 0L ||
      !all(vapply(techs, is, logical(1), "technology"))) {
    stop("`techs` must be a non-empty list of technology objects",
         call. = FALSE)
  }
  for (tt in techs) {
    if (nrow(tt@cluster) > 0) {
      stop("combine_vintages() does not support clustered inputs (object '",
           tt@name, "' declares clusters)", call. = FALSE)
    }
  }

  # -- vintage labels ----------------------------------------------------------
  nms <- names(techs) %||% vapply(techs, function(t) t@name, character(1))
  if (is.null(nms) || any(!nzchar(nms))) {
    nms <- vapply(techs, function(t) t@name, character(1))
  }
  if (is.null(vintages)) {
    mm <- regexpr("_VIN([A-Za-z0-9]+)$", nms)
    vintages <- rep(NA_character_, length(nms))
    vintages[mm > 0] <- sub("^_VIN", "", regmatches(nms, mm))
    for (i in which(is.na(vintages))) {
      st <- techs[[i]]@vintage$start
      st <- st[!is.na(st)]
      if (length(st) > 0) vintages[i] <- as.character(st[1])
    }
    if (anyNA(vintages)) {
      stop("cannot infer vintage labels; give `vintages=` (no _VIN suffix ",
           "in names and no start year for: ",
           paste(nms[is.na(vintages)], collapse = ", "), ")",
           call. = FALSE)
    }
  }
  vintages <- as.character(vintages)
  if (length(vintages) != length(techs)) {
    stop("`vintages` must match `techs` in length", call. = FALSE)
  }
  if (anyDuplicated(vintages)) {
    stop("duplicate vintage labels: ",
         paste(unique(vintages[duplicated(vintages)]), collapse = ", "),
         call. = FALSE)
  }

  # sort by numeric label when possible
  ord <- order(suppressWarnings(as.numeric(vintages)), vintages)
  techs <- techs[ord]; vintages <- vintages[ord]; nms <- nms[ord]

  # -- identity ----------------------------------------------------------------
  base <- unique(sub("_VIN[A-Za-z0-9]+$", "", nms))
  if (is.null(name)) {
    if (length(base) != 1L) {
      stop("inputs have different base names (", paste(base, collapse = ", "),
           "); give `name=`", call. = FALSE)
    }
    name <- base
  }
  first <- techs[[1L]]
  if (is.null(desc)) {
    desc <- sub(",?\\s*vintage.*$", "", first@desc)
  }

  # -- structural attributes: first object, warn on differences ---------------
  .warn_differs <- function(what, differs) {
    if (differs) {
      warning("`", what, "` differs across vintages; using the first ",
              "object's value", call. = FALSE)
    }
  }
  for (tt in techs[-1L]) {
    .warn_differs("units", !identical(tt@units, first@units))
    .warn_differs("cap2act", !isTRUE(all.equal(tt@cap2act, first@cap2act)))
    .warn_differs("timeframe", !identical(tt@timeframe, first@timeframe))
    .warn_differs("region", !identical(tt@region, first@region))
    .warn_differs("fullYear", !identical(tt@fullYear, first@fullYear))
  }

  # -- ports: union across vintages -------------------------------------------
  .union_port <- function(slot_nm, key) {
    parts <- lapply(techs, function(t) methods::slot(t, slot_nm))
    parts <- parts[vapply(parts, nrow, integer(1)) > 0]
    if (length(parts) == 0L) return(NULL)
    all_rows <- do.call(rbind, parts)
    dup <- duplicated(all_rows[[key]])
    conflicting <- vapply(split(all_rows, all_rows[[key]]), function(g) {
      nrow(unique(g)) > 1L
    }, logical(1))
    if (any(conflicting)) {
      warning("port(s) with conflicting definitions across vintages ",
              "(keeping the first): ",
              paste(names(conflicting)[conflicting], collapse = ", "),
              call. = FALSE)
    }
    out <- all_rows[!dup, , drop = FALSE]
    rownames(out) <- NULL
    out
  }

  # -- vintage table -----------------------------------------------------------
  # each input's window/life is collected per REGION (region-specific rows
  # are kept, not flattened to the first value); the region column is
  # dropped again when no input uses it
  vt_parts <- lapply(seq_along(techs), function(i) {
    parts <- lapply(c("start", "end", "olife"), function(cc) {
      d <- .lifespan_resolve(techs[[i]], cc)
      d[, c("region", cc), drop = FALSE]
    })
    parts <- Filter(function(p) nrow(p) > 0L, parts)
    merged <- if (length(parts) > 0L) {
      Reduce(function(a, b) dplyr::full_join(a, b, by = "region"), parts)
    } else {
      data.frame(region = NA_character_, stringsAsFactors = FALSE)
    }
    for (cc in c("start", "end", "olife")) {
      if (!cc %in% names(merged)) merged[[cc]] <- NA_real_
    }
    merged$vintage <- vintages[i]
    merged
  })
  vt <- dplyr::bind_rows(vt_parts)[, c("vintage", "region", "start",
                                       "end", "olife")]
  num_lab <- suppressWarnings(as.numeric(vintages))
  lab_of <- match(vt$vintage, vintages)
  vt$start[is.na(vt$start)] <- num_lab[lab_of][is.na(vt$start)]
  if (isTRUE(close_windows) && length(techs) > 1L) {
    # next vintage's earliest start closes an open end
    nxt_start <- vapply(seq_along(vintages), function(i) {
      if (i == length(vintages)) return(NA_real_)
      s <- vt$start[vt$vintage == vintages[i + 1L]]
      s <- s[!is.na(s)]
      if (length(s) > 0) min(s) else num_lab[i + 1L]
    }, numeric(1))
    nxt <- nxt_start[lab_of] - 1
    fix <- is.na(vt$end) & !is.na(nxt)
    vt$end[fix] <- nxt[fix]
  }
  if (all(is.na(vt$region))) vt$region <- NULL
  rownames(vt) <- NULL

  # -- parameter slots: tag with vintage, rbind, collapse common rows ---------
  .combine_slot <- function(slot_nm) {
    parts <- list()
    for (i in seq_along(techs)) {
      df <- methods::slot(techs[[i]], slot_nm)
      if (!is.data.frame(df) || nrow(df) == 0L) next
      if ("vintage" %in% names(df) && any(!is.na(df$vintage))) {
        warning("object '", nms[i], "' already carries vintage keys in @",
                slot_nm, "; they are overwritten", call. = FALSE)
      }
      df$vintage <- vintages[i]
      parts[[length(parts) + 1L]] <- df
    }
    if (length(parts) == 0L) return(NULL)
    out <- do.call(rbind, parts)
    rownames(out) <- NULL
    if (isTRUE(collapse_common) && length(techs) > 1L) {
      rest <- setdiff(names(out), "vintage")
      key <- do.call(paste, c(lapply(out[rest], as.character), sep = "\r"))
      full <- names(table(key))[table(key) == length(techs)]
      is_full <- key %in% full
      shared <- out[is_full & !duplicated(key), , drop = FALSE]
      if (nrow(shared) > 0) shared$vintage <- NA_character_
      out <- rbind(shared, out[!is_full, , drop = FALSE])
      rownames(out) <- NULL
    }
    # drop all-NA columns; newTechnology re-adds the full prototypes
    keep <- vapply(out, function(x) any(!is.na(x)), logical(1))
    out <- out[, keep, drop = FALSE]
    if (ncol(out) == 0L || nrow(out) == 0L) NULL else out
  }

  # -- assemble ----------------------------------------------------------------
  args <- list(name = name, desc = desc, vintage = vt)
  for (p in list(c("input", "comm"), c("output", "comm"),
                 c("aux", "acomm"), c("group", "group"))) {
    u <- .union_port(p[1], p[2])
    if (!is.null(u)) args[[p[1]]] <- u
  }
  if (nrow(first@units) > 0) args$units <- first@units
  if (length(first@cap2act) == 1 && is.finite(first@cap2act)) {
    args$cap2act <- first@cap2act
  }
  if (length(first@timeframe) > 0 && nzchar(first@timeframe[1])) {
    args$timeframe <- first@timeframe
  }
  if (length(first@region) > 0) args$region <- first@region
  args$fullYear <- first@fullYear
  if (isTRUE(first@optimizeRetirement)) args$optimizeRetirement <- TRUE
  for (sl in c("ceff", "geff", "aeff", "af", "afs", "weather",
               "invcost", "fixom", "varom", "capacity")) {
    cs <- .combine_slot(sl)
    if (!is.null(cs)) args[[sl]] <- cs
  }
  misc <- first@misc
  misc$combined_from <- stats::setNames(as.list(nms), vintages)
  if (length(misc) > 0) args$misc <- misc

  do.call(newTechnology, args)
}


# ---------------------------------------------------------------------------
# (was R/stock_path.R, merged 2026-09-20)
# ---------------------------------------------------------------------------

# =========================================================================== #
# The exogenous stock PATH: commissioning stream + per-period survival
#
# `stock(y)` as a user writes it carries TWO meanings at once -- the original
# endowment and the fleet still standing -- and that conflation is what made
# requirement 1 (exogenous phase-out) and requirement 2 (endogenous early
# retirement) mutually exclusive. The old pair
#
#     retiredCum(y) <= stock(y)          # reads stock as the ORIGINAL endowment
#     retStock(y)*plen = cum(y)-cum(y-1) # flow >= 0, so cum is NON-DECREASING
#
# forced retiredCum to zero in EVERY year whenever the schedule declined to
# zero, because the cap in the final year bound the whole path. A fleet could
# be scheduled out, or optimised out, never both.
#
# Splitting the path separates them:
#
#     stockNew(y)  = max(0, stock(y) - stock(y-1)),   = stock(first) at the start
#     stockSurv(y) = (stock(y) - stockNew(y)) / stock(y-1),   1 when stock(y-1)=0
#
# so that the fleet obeys a RECURSIVE balance (eqXStockCap) with no cumulative
# variable, and therefore no non-decreasing trap:
#
#     stockCap(y) = stockSurv(y)*stockCap(y-1) + stockNew(y) - retStock(y)*plen(y)
#
# `stockSurv` thins whatever is ACTUALLY standing, so it can never demand more
# capacity than exists and can never make the model infeasible. It also uses
# only CONSECUTIVE milestones, so both parameters -- and hence the physics --
# are invariant under a change of base year. Retiring against a `stock0` taken
# from the first modelled year was not.
#
# A constant fleet gives stockSurv = 1 and stockNew = 0 after the first year,
# which reproduces the previous behaviour exactly.
# =========================================================================== #

.stock_path_one <- function(scen, key, stock_par, new_par, surv_par) {
  P <- scen@modInp@parameters
  if (is.null(P[[stock_par]]) || is.null(P[[new_par]]) || is.null(P[[surv_par]]))
    return(scen)
  st <- as.data.frame(get_data_slot(P[[stock_par]]))
  if (is.null(st) || nrow(st) == 0 || !("value" %in% names(st))) return(scen)
  names(st)[names(st) == "value"] <- "stock"
  st <- st[!is.na(st$stock), , drop = FALSE]
  if (nrow(st) == 0) return(scen)

  grp <- intersect(c(key, "region"), names(st))

  # `stock` is a numpar with defVal 0, so interpolation DROPS every row whose
  # value is zero -- including the year a declining schedule finally reaches
  # nothing. Read sparsely, a fleet scheduled 100 -> 80 -> 0 would look like
  # 100 -> 80 -> (unspecified, hence unchanged) and never leave. The equations
  # always read the parameter densely against its default, so the derivation
  # must too: rebuild the full milestone grid per key and fill the gaps with 0.
  yrs <- sort(unique(as.integer(scen@modInp@sets$year)))
  if (length(yrs) == 0) return(scen)
  st <- merge(unique(st[, grp, drop = FALSE]),
              data.frame(year = yrs), by = NULL) |>
    merge(st, by = c(grp, "year"), all.x = TRUE)
  st$stock[is.na(st$stock)] <- 0
  d <- dplyr::as_tibble(st) |>
    dplyr::arrange(dplyr::across(dplyr::all_of(c(grp, "year")))) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(grp))) |>
    dplyr::mutate(
      .prev = dplyr::lag(.data$stock),
      # first milestone of the span: the whole fleet is commissioned there
      .new  = ifelse(is.na(.data$.prev), .data$stock,
                     pmax(0, .data$stock - .data$.prev)),
      # nothing to survive from an empty (or absent) previous fleet
      .surv = ifelse(is.na(.data$.prev) | .data$.prev <= 0, 1,
                     (.data$stock - .data$.new) / .data$.prev)) |>
    dplyr::ungroup() |>
    as.data.frame()

  keys <- c(grp, "year")
  # A dense grid was needed to READ the schedule; writing it back dense would
  # bloat every model that has a stock. Both parameters carry the neutral value
  # as their default (0 commissioned, all of it surviving), so only departures
  # from it need to be written.
  for (spec in list(list(new_par, ".new", 0), list(surv_par, ".surv", 1))) {
    o <- d[, c(keys, spec[[2]]), drop = FALSE]
    names(o)[names(o) == spec[[2]]] <- "value"
    o <- o[!is.na(o$value) & o$value != spec[[3]], , drop = FALSE]
    scen@modInp@parameters[[spec[[1]]]] <-
      .fold_write_back(P[[spec[[1]]]], o)
  }
  scen
}

#' Derive the exogenous stock path parameters
#'
#' Runs after interpolation, next to `compute_eac_parameters()`, and turns each
#' class's `stock` level into a commissioning stream and a per-period survival
#' share. See the file header for why the split is necessary.
#'
#' @param scen scenario object.
#' @return the scenario with `pXStockNew` / `pXStockSurv` filled.
#' @keywords internal
compute_stock_parameters <- function(scen) {
  fam <- list(
    c("tech",  "pTechStock"),
    c("stg",   "pStorageOutStock"),
    c("stg",   "pStorageInpStock"),
    c("stg",   "pStorageStgStock"),
    c("trade", "pTradeStock"))
  for (f in fam) {
    scen <- .stock_path_one(scen, f[[1]], f[[2]],
                            paste0(f[[2]], "New"), paste0(f[[2]], "Surv"))
  }
  scen
}


# ---------------------------------------------------------------------------
# (was R/start_level.R, merged 2026-09-20)
# ---------------------------------------------------------------------------

# =========================================================================== #
# storage@startLevel -- place the endowment on the FIRST timeslice of each cycle
#
# `@startLevel` has no `timeslice` column on purpose: the slice is derived, not
# chosen. `ob2mi()` therefore writes `pStorageStartLevel` with `timeslice = NA`,
# which the usual wildcard would expand to EVERY timeslice -- the exact bug the
# old per-timeslice `@charge` had. This step replaces those NA rows with one
# explicit row per cycle, so the balance term (already present and defaulting to
# zero) fires once and only once per cycle.
#
# WHICH slice is the first depends on where the cycle closes, i.e. on
# `storage@fullYear` -- the same branch `.build_meqStorageLevel()` takes:
#   fullYear = TRUE   one cycle over the whole year  -> the first slice, once
#   fullYear = FALSE  one cycle per parent timeframe -> the first child of each
#
# `@startLevel` is ANNUAL, so each cycle receives its SHARE of it, not the whole
# value: `startLevel * sum(share of that cycle's slices)`. A year-long cycle has
# share 1 and is unchanged; 365 daily cycles get 1/365 each and still total
# `startLevel` over the year. Without the scaling, a daily-cycling store would be
# endowed 365 times over.
#
# Derived from the successor map rather than from calendar bookkeeping: each
# cycle is a closed loop in that map, and its "first" slice is the member that
# comes earliest in calendar order. That keeps this consistent with the balance
# by construction -- both read the same map.
#
# Runs after ob2mi/interpolation, next to compute_eac_parameters(), and follows
# the same pattern: read the interpolated parameter, rewrite it, write it back.
# =========================================================================== #

#' Cycle-first timeslices for one storage
#'
#' Returns one row per closed cycle: its first timeslice (earliest in calendar
#' order) and its SHARE of the year, which is the sum of its members' shares --
#' the cycle's parent share by construction. A year-long cycle sums to 1; each
#' of 365 daily cycles sums to 1/365; a calendar filtered to part of a year sums
#' to that `year_fraction`.
#'
#' @param succ data.frame with `timeslice` -> `timeslicep` (the SUCCESSOR map)
#' @param share named numeric: timeslice -> share of the year, in calendar order
#' @return data.frame with `timeslice` (the cycle's first) and `share`
#' @keywords internal
.cycle_first_slices <- function(succ, share) {
  empty <- data.frame(timeslice = character(), share = numeric(),
                      stringsAsFactors = FALSE)
  if (is.null(succ) || !nrow(succ) || !length(share)) return(empty)
  nxt <- stats::setNames(as.character(succ$timeslicep), as.character(succ$timeslice))
  # `share` is already in calendar order, so the first member reached is the
  # cycle's earliest.
  members <- intersect(names(share), names(nxt))
  seen <- character()
  firsts <- character()
  shares <- numeric()
  for (s in members) {
    if (s %in% seen) next
    cyc <- s
    nx <- nxt[[s]]
    while (!is.na(nx) && !(nx %in% cyc)) {
      cyc <- c(cyc, nx)
      nx <- if (nx %in% names(nxt)) nxt[[nx]] else NA_character_
    }
    seen <- c(seen, cyc)
    firsts <- c(firsts, s)
    shares <- c(shares, sum(share[intersect(cyc, names(share))], na.rm = TRUE))
  }
  data.frame(timeslice = firsts, share = shares, stringsAsFactors = FALSE)
}

#' Place `pStorageStartLevel` on the first timeslice of each cycle
#'
#' @param scen scenario after interpolation
#' @return scenario with `pStorageStartLevel` rewritten
#' @keywords internal
place_start_level <- function(scen) {
  pname <- "pStorageStartLevel"
  P <- scen@modInp@parameters[[pname]]
  if (is.null(P)) return(scen)
  d <- try(as.data.frame(get_data_slot(P)), silent = TRUE)
  if (inherits(d, "try-error") || is.null(d) || !nrow(d)) return(scen)
  vcol <- if ("value" %in% names(d)) "value" else utils::tail(names(d), 1)
  d <- d[!is.na(d[[vcol]]) & d[[vcol]] != 0, , drop = FALSE]
  if (!nrow(d)) return(scen)

  gdf <- function(nm) {
    p <- scen@modInp@parameters[[nm]]
    if (is.null(p)) return(NULL)
    x <- get_data_slot(p)
    if (is.null(x) || nrow(x) == 0) return(NULL)
    as.data.frame(x)
  }
  succ_pf <- gdf("mTimesliceNext")        # cycle closes in the parent timeframe
  succ_fy <- gdf("mTimesliceFYearNext")   # cycle closes over the whole year
  # `@timeslice_share`, NOT `@timetable`. The timetable holds LEAVES ONLY, so a
  # storage whose commodity sits at a coarse level (a reservoir on a YDAY water
  # commodity) would match no members and silently emit no rows -- dropping
  # startLevel without a word. `@timeslice_share` carries every level with its
  # share of the year, in calendar order, which is also what the annual scaling
  # below needs. One source for both.
  ts <- try(scen@settings@calendar@timeslice_share, silent = TRUE)
  if (inherits(ts, "try-error") || is.null(ts) || !nrow(ts)) return(scen)
  ts <- as.data.frame(ts)
  share <- stats::setNames(as.numeric(ts$share), as.character(ts$timeslice))
  if (!length(share)) return(scen)

  fy <- apply_to_scenario_data(
    scen = scen, classes = "storage", as_list = TRUE,
    func = function(obj) {
      out <- list()
      out[[obj@name]] <- data.frame(stg = obj@name,
                                    fullYear = isTRUE(obj@fullYear),
                                    stringsAsFactors = FALSE)
      out
    })
  fy <- if (length(fy) == 0) NULL else dplyr::bind_rows(fy)
  fy_stg <- if (is.null(fy)) character() else fy$stg[fy$fullYear]

  # Restrict to each storage's OWN timeslices. The successor maps carry cycles at
  # every timeframe (a DAY loop and an HOUR loop both exist), so an unrestricted
  # walk emits a row at every level and endows the store once per level. A
  # storage operates only at its commodity's resolution, which is what
  # `mvStorageLevel` records.
  dom <- gdf("mvStorageLevel")
  stg_slices <- if (is.null(dom)) NULL else split(as.character(dom$timeslice),
                                                  as.character(dom$stg))

  # `startLevel` is ANNUAL, so each cycle receives its own share of it. With the
  # year-long cycle that share is 1 and the value passes through unchanged; with
  # 365 daily cycles each gets 1/365 and the year still totals `startLevel`,
  # rather than endowing it 365 times.
  out <- lapply(split(d, d$stg), function(g) {
    stg <- g$stg[1]
    own <- if (is.null(stg_slices)) names(share) else unique(stg_slices[[stg]])
    if (is.null(own) || !length(own)) return(NULL)
    sh <- share[intersect(names(share), own)]   # keeps calendar order
    cyc <- .cycle_first_slices(if (stg %in% fy_stg) succ_fy else succ_pf, sh)
    if (!nrow(cyc)) return(NULL)
    g$timeslice <- NULL
    g <- unique(g)
    res <- do.call(rbind, lapply(seq_len(nrow(cyc)), function(i) {
      gi <- g
      gi$timeslice <- cyc$timeslice[i]
      gi[[vcol]] <- gi[[vcol]] * cyc$share[i]
      gi
    }))
    res[!is.na(res[[vcol]]) & res[[vcol]] != 0, , drop = FALSE]
  })
  out <- do.call(rbind, out[!vapply(out, is.null, logical(1))])
  if (is.null(out) || !nrow(out)) return(scen)
  out <- out[, intersect(c(P@dimSets, vcol), names(out)), drop = FALSE]
  scen@modInp@parameters[[pname]] <- .fold_write_back(P, out)
  scen
}
