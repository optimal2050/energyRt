# =========================================================================== #
# Region aggregation: a coarser model built from a finer one.
#
# The stage `interpolate_model()` names when it is handed a pruned geoscale.
# Every region-keyed slot is recast from the model's own regions onto a coarser
# geoframe: extensive quantities add up, intensive ones take a weighted mean,
# and paired-region slots (trade) drop the routes that become internal.
#
# Objects that span many regions -- a technology, a supply, a demand -- keep
# their identity and only their data moves. Trade objects are one per region
# pair, so those MERGE: several fine corridors become one coarse corridor.
# =========================================================================== #

#' @include geoscale.R sample_region.R
NULL

# Columns that identify a row rather than measure it. `year`, `vintage` and
# `timeslice` are grouping keys, not quantities to be averaged.
.AGG_ID_COLS <- c("region", "src", "dst", "year", "vintage", "cluster",
                  "timeslice", "comm", "acomm", "group", "desc")

# Extensive: the quantity of a parent is the total of its children. Anchored on
# both sides so `ncap2ainp` and `cap2ainp` -- emission coefficients -- do not
# match the capacity family.
.AGG_EXTENSIVE_RE <- paste0(
  "(^|\\.)(stock|cap|ncap|ret)(\\.|$)",
  "|(^|\\.)ava(\\.|$)",
  "|^res\\.",
  "|^demand$",
  "|^(imp|exp)\\."
)

# Structural: the same value for every region of the group, and a conflict is
# an error rather than something to average.
.AGG_COPY_COLS <- c("start", "end", "olife", "order")

#' Aggregation rule for each value column
#' @noRd
.agg_rules <- function(cols) {
  r <- ifelse(cols %in% .AGG_COPY_COLS, "copy",
              ifelse(grepl(.AGG_EXTENSIVE_RE, cols), "sum", "weighted_mean"))
  stats::setNames(r, cols)
}

#' Value columns of a slot: numeric, and not an identifier
#' @noRd
.agg_values <- function(df) {
  num <- names(df)[vapply(df, is.numeric, logical(1))]
  setdiff(num, .AGG_ID_COLS)
}

# One weight per region per object, as PyPSA weights a cluster by `p_nom`.
# A year- or vintage-varying weight would make an efficiency's weighting
# depend on which year it is read for; a single size per region does not.
#' @noRd
.agg_weight <- function(obj) {
  pick <- function(slot, cols) {
    if (!.hasSlot(obj, slot)) return(NULL)
    df <- methods::slot(obj, slot)
    if (!is.data.frame(df) || !nrow(df) || !"region" %in% names(df)) return(NULL)
    cols <- intersect(cols, names(df))
    if (!length(cols)) return(NULL)
    v <- rowSums(as.matrix(df[, cols, drop = FALSE]), na.rm = TRUE)
    keep <- !is.na(df$region)
    if (!any(keep)) return(NULL)
    agg <- stats::aggregate(list(.agg_w = v[keep]),
                            by = list(region = df$region[keep]),
                            FUN = function(z) sum(z, na.rm = TRUE))
    if (all(!is.finite(agg$.agg_w) | agg$.agg_w == 0)) return(NULL)
    agg
  }
  cl <- class(obj)[1]
  switch(
    cl,
    technology = pick("capacity", c("stock", "cap.up", "cap.fx")),
    storage = pick("capacity", c("out.stock", "out.cap.up", "out.cap.fx")),
    trade = pick("trade", c("ava.up", "ava.fx")),
    supply = pick("supply", c("ava.up", "ava.fx")),
    demand = pick("demand", "demand"),
    import = pick("import", c("imp.up", "imp.fx")),
    export = pick("export", c("exp.up", "exp.fx")),
    NULL
  )
}

# An NA region is the wildcard "every region". For an intensive quantity that
# survives aggregation unchanged -- the weighted mean of one value is that
# value -- so the wildcard is left alone. An extensive one does not: a parent's
# total is the sum over its children, which differs by how many it has, so
# those rows are written out per region first.
#' @noRd
.agg_expand_wildcards <- function(df, regions, ext_cols) {
  if (!"region" %in% names(df) || !length(ext_cols)) return(df)
  wc <- which(is.na(df$region))
  if (!length(wc)) return(df)
  has_ext <- vapply(wc, function(i)
    any(!is.na(unlist(df[i, ext_cols, drop = FALSE]))), logical(1))
  wc <- wc[has_ext]
  if (!length(wc)) return(df)
  ex <- df[rep(wc, each = length(regions)), , drop = FALSE]
  ex$region <- rep(regions, times = length(wc))
  rbind(df[-wc, , drop = FALSE], ex)
}

#' Recast one region-keyed slot onto the coarser geoframe
#' @noRd
.agg_slot <- function(df, gs, level, w, regions) {
  if (!is.data.frame(df) || !nrow(df)) return(df)
  vals <- .agg_values(df)
  rules <- .agg_rules(vals)

  # A slot with nothing to measure -- @routes -- is a set of keys: remap and
  # take the distinct rows.
  if (!length(vals)) {
    df$region <- .agg_map_codes(df$region, gs, level)
    return(unique(df[!is.na(df$region), , drop = FALSE]))
  }

  df <- .agg_expand_wildcards(df, regions, vals[rules == "sum"])
  wild <- df[is.na(df$region), , drop = FALSE]
  df <- df[!is.na(df$region), , drop = FALSE]
  if (!nrow(df)) return(wild)

  df$.agg_w <- if (is.null(w)) 1 else
    w$.agg_w[match(df$region, w$region)]
  df$.agg_w[is.na(df$.agg_w)] <- 0

  out <- geoscales::recast_from_geoatoms(
    df, gs, to = level, key = "region", values = vals, rule = rules,
    weight = ".agg_w", na_rm = TRUE, na_action = "drop")
  out <- as.data.frame(out)
  names(out)[names(out) == level] <- "region"
  out <- out[, intersect(names(df), names(out)), drop = FALSE]

  if (nrow(wild)) {
    wild$.agg_w <- NULL
    out <- rbind(out[, names(wild), drop = FALSE], wild)
  }
  rownames(out) <- NULL
  out
}

#' Map fine region codes to their code at `level`
#' @noRd
.agg_map_codes <- function(x, gs, level) {
  lt <- as.data.frame(geoscales::geoscale_leaftable(gs))
  fine <- geoscales::geoscale_geoframes(gs, finest = TRUE)
  as.character(lt[[level]])[match(as.character(x), as.character(lt[[fine]]))]
}

# -- trade ------------------------------------------------------------------- #
# A trade object is one region pair, so aggregation merges objects rather than
# moving rows inside one. Objects merge only when they agree on what they carry
# (commodity) and on how they are split (cluster names): corridors with
# different loss-tranche structures cannot share one set of tranche shares.

#' The object name with its `_<src>__<dst>` tail removed
#' @noRd
.agg_trade_prefix <- function(obj) {
  nm <- obj@name
  rt <- obj@routes
  if (is.data.frame(rt) && nrow(rt)) {
    tl <- paste0("_", rt$src[1], "__", rt$dst[1])
    if (endsWith(nm, tl)) return(substr(nm, 1L, nchar(nm) - nchar(tl)))
  }
  "TRD"
}

#' Coarse endpoints of a trade object, or NULL when it becomes internal
#' @noRd
.agg_trade_pair <- function(obj, gs, level) {
  rt <- obj@routes
  if (!is.data.frame(rt) || !nrow(rt)) return(NULL)
  s <- .agg_map_codes(rt$src, gs, level)
  d <- .agg_map_codes(rt$dst, gs, level)
  keep <- !is.na(s) & !is.na(d) & s != d
  if (!any(keep)) return(NULL)
  data.frame(src = pmin(s[keep], d[keep]), dst = pmax(s[keep], d[keep]),
             stringsAsFactors = FALSE) |> unique()
}

#' Group key: objects sharing one merge into a single coarse corridor
#' @noRd
.agg_trade_key <- function(obj, pair) {
  paste(c(.agg_trade_prefix(obj),
          paste(sort(as.character(obj@commodity)), collapse = ","),
          paste(sort(as.character(obj@cluster$cluster)), collapse = ","),
          sort(paste(pair$src, pair$dst, sep = "|"))),
        collapse = "::")
}

#' Recast one src/dst-keyed slot of the merged corridors
#' @noRd
.agg_pair_slot <- function(df, gs, level, w) {
  if (!is.data.frame(df) || !nrow(df)) return(df)
  vals <- .agg_values(df)
  if (!length(vals)) {
    df$src <- .agg_map_codes(df$src, gs, level)
    df$dst <- .agg_map_codes(df$dst, gs, level)
    df <- df[!is.na(df$src) & !is.na(df$dst) & df$src != df$dst, , drop = FALSE]
    return(unique(df))
  }
  df$.agg_w <- if (is.null(w)) 1 else
    w$.agg_w[match(paste(df$src, df$dst), paste(w$src, w$dst))]
  df$.agg_w[is.na(df$.agg_w)] <- 0
  out <- suppressWarnings(geoscales::recast_pairs(
    df, gs, to = level, src = "src", dst = "dst", values = vals,
    rule = .agg_rules(vals), weight = ".agg_w", na_rm = TRUE))
  out <- as.data.frame(out)
  out[, intersect(names(df), names(out)), drop = FALSE]
}

#' Merge a group of trade objects into one coarse corridor
#' @noRd
.agg_trade_merge <- function(objs, gs, level, pair) {
  base <- objs[[1L]]
  bind <- function(slot) {
    parts <- lapply(objs, function(o) {
      v <- methods::slot(o, slot)
      if (is.data.frame(v) && nrow(v)) v else NULL
    })
    parts <- parts[!vapply(parts, is.null, logical(1))]
    if (!length(parts)) return(methods::slot(base, slot))
    do.call(rbind, parts)
  }

  # Corridor capacity weights the intensive quantities, the way a cluster's
  # `p_nom` weights an aggregated line.
  tr <- bind("trade")
  w <- NULL
  if (is.data.frame(tr) && nrow(tr) && "ava.up" %in% names(tr)) {
    v <- tr$ava.up
    if (!"ava.up" %in% names(tr) || all(is.na(v))) v <- rep(NA_real_, nrow(tr))
    ok <- !is.na(tr$src) & !is.na(tr$dst)
    if (any(ok)) {
      w <- stats::aggregate(list(.agg_w = v[ok]),
                            by = list(src = tr$src[ok], dst = tr$dst[ok]),
                            FUN = function(z) sum(z, na.rm = TRUE))
    }
  }

  out <- base
  out@name <- paste0(.agg_trade_prefix(base), "_", pair$src[1], "__",
                     pair$dst[1])
  out@routes <- rbind(pair, stats::setNames(pair[, c("dst", "src")],
                                            c("src", "dst")))
  rownames(out@routes) <- NULL
  for (sl in c("trade", "aeff", "varom")) {
    methods::slot(out, sl) <- .agg_pair_slot(bind(sl), gs, level, w)
  }
  rw <- NULL
  for (sl in c("invcost", "fixom", "vintage")) {
    methods::slot(out, sl) <- .agg_slot(bind(sl), gs, level, rw,
                                        character(0))
  }
  if (length(objs) > 1L) {
    out@desc <- paste0(base@desc, " (merged from ", length(objs),
                       " corridors)")
  }
  out
}

# -- the driver -------------------------------------------------------------- #

#' Aggregate a model to a coarser set of regions
#'
#' Build a smaller model from a finer one by collapsing its regions onto a
#' coarser geoframe of a [geoscales::Geoscale]. The result is a model in its
#' own right: it can be interpolated, solved and reported like any other.
#'
#' Every region-keyed slot is recast. Extensive quantities -- capacities,
#' demand, supply availability, import and export limits -- are summed over the
#' regions of each parent. Intensive ones -- efficiencies, availability
#' factors, costs, weather values -- take a mean weighted by the object's own
#' size in each region, so an aggregated value stays within the range of the
#' values it came from. When the weights of a group are all zero the mean is
#' unweighted, which keeps a group of empty regions at their common value
#' rather than undefined.
#'
#' Trade is different: a trade object is one region pair, so corridors that
#' land inside a single parent region become internal and are dropped, and the
#' rest merge -- their capacities add, and their loss factors take the
#' capacity-weighted mean. Corridors merge only when they agree on commodity
#' and on cluster structure, so tranched and untranched routes stay apart.
#'
#' @param mod A `model`.
#' @param geoscale A [geoscales::Geoscale] whose finest geoframe contains the
#'   model's regions. `NULL` (default) uses the model's own.
#' @param level Name of the target geoframe, coarser than the model's regions.
#' @param name Name for the aggregated model. `NULL` (default) appends
#'   `level` to the source model's name.
#' @param verbose Report what was aggregated, merged and dropped.
#'
#' @return A `model` declared over the coarser regions, carrying the pruned
#'   geoscale.
#'
#' @seealso [subset_model_regions()], which takes a sub-territory at the
#'   model's own resolution instead of coarsening the whole of it.
#' @export
aggregate_model_regions <- function(mod, geoscale = NULL, level,
                                    name = NULL, verbose = isVerbose()) {
  stopifnot(inherits(mod, "model"))
  check_package("geoscales")
  gs <- geoscale
  if (is.null(gs)) gs <- tryCatch(mod@config@geoscale, error = function(e) NULL)
  if (is.null(gs) || !is_geoscale(gs)) {
    stop("no geoscale: pass `geoscale=`, or attach one to the model with ",
         "setGeoscale().", call. = FALSE)
  }
  frames <- geoscales::geoscale_geoframes(gs)
  if (!isTRUE(level %in% frames)) {
    stop("`level` = \"", level, "\" is not a geoframe of the geoscale; ",
         "one of: ", paste(frames, collapse = ", "), call. = FALSE)
  }
  fine <- geoscales::geoscale_geoframes(gs, finest = TRUE)
  if (identical(level, fine)) {
    stop("`level` = \"", level, "\" is the geoscale's finest geoframe; ",
         "there is nothing to aggregate.", call. = FALSE)
  }

  regions <- as.character(mod@config@region)
  regions <- regions[!is.na(regions) & nzchar(regions)]
  atoms <- as.character(geoscales::geoscale_regions(gs, fine))
  unknown <- setdiff(regions, atoms)
  if (length(unknown) > 0L) {
    stop(length(unknown), " of the model's ", length(regions), " region(s) ",
         "are not atoms of the geoscale, so their data would be dropped ",
         "silently: ", paste(utils::head(unknown, 6), collapse = ", "),
         if (length(unknown) > 6) ", ..." else "",
         ". The geoscale must be keyed the way the model is.", call. = FALSE)
  }

  coarse <- unique(stats::na.omit(.agg_map_codes(regions, gs, level)))
  if (isTRUE(verbose)) {
    message("Aggregating ", length(regions), " region(s) to ", length(coarse),
            " at `", level, "`")
  }

  objs <- mod@data[[1]]@data
  cls <- vapply(objs, function(o) class(o)[1], "")
  out <- list()

  # -- objects that span regions: the object stays, its data moves ------------
  for (i in which(cls != "trade")) {
    o <- objs[[i]]
    w <- .agg_weight(o)
    for (sl in methods::slotNames(o)) {
      v <- methods::slot(o, sl)
      if (is.data.frame(v) && nrow(v) && "region" %in% names(v)) {
        methods::slot(o, sl) <- .agg_slot(v, gs, level, w, regions)
      }
    }
    if (.hasSlot(o, "region")) {
      r <- methods::slot(o, "region")
      if (is.character(r) && length(r)) {
        methods::slot(o, "region") <-
          unique(stats::na.omit(.agg_map_codes(r, gs, level)))
      }
    }
    out[[o@name]] <- o
  }

  # -- trade: merge the corridors that survive -------------------------------
  tr <- objs[cls == "trade"]
  pairs <- lapply(tr, .agg_trade_pair, gs = gs, level = level)
  internal <- vapply(pairs, is.null, logical(1))
  keys <- rep(NA_character_, length(tr))
  for (i in which(!internal)) keys[i] <- .agg_trade_key(tr[[i]], pairs[[i]])

  n_merged <- 0L
  for (k in unique(stats::na.omit(keys))) {
    idx <- which(keys == k)
    merged <- .agg_trade_merge(tr[idx], gs, level, pairs[[idx[1L]]])
    if (length(idx) > 1L) n_merged <- n_merged + length(idx)
    out[[merged@name]] <- merged
  }
  if (isTRUE(verbose)) {
    message("Trade: ", length(tr), " corridor(s) -> ",
            sum(!is.na(unique(keys))), " (", sum(internal),
            " became internal, ", n_merged, " merged)")
  }

  res <- mod
  res@name <- name %||% paste0(mod@name, "_", level)
  res@data[[1]]@data <- out
  res@config@region <- coarse
  res@config@geoscale <- geoscales::prune_geoscale(gs, level)
  res
}
