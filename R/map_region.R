# =========================================================================== #
# map_region.R  —  region-hierarchy mapping builders
#
# The spatial twin of the timeslice maps in map_calendar.R. Both are built in the
# `calendar` recipe tier (see `mapping_spec.yml`), which runs before `filter`,
# so `mvBalance` can be restricted with `mCommRegion` once it exists.
#
#   mRegionFamily(region, regionp)  immediate parent -> child, one level only.
#                                   Mirrors mTimesliceFamily. Drives the
#                                   up-aggregation term in eqOutTot/eqInpTot.
#   mCommRegion(comm, region)       the regions each commodity is BALANCED at,
#                                   i.e. the members of its `@geoframe`.
#                                   Mirrors mCommTimeslice.
#
# Both are empty without a geoscale, which is what keeps a flat model bit-for-bit
# unchanged. `.set_cal()` skips empty frames, so nothing is emitted at all.
#
# Unlike the timeslice side there is no `pRegionAgg`: timeslice values are intensive
# rates and need renormalising by duration, whereas regional quantities are
# extensive and simply add up.
# =========================================================================== #

#' @include geoscale.R map_calendar.R
NULL

# The pruned region hierarchy for a scenario, or NULL when no geoscale is
# attached. Cheap enough to recompute at each call site.
#' @noRd
.scen_geo_hierarchy <- function(scen) {
  gs <- tryCatch(getGeoscale(scen@settings), error = function(e) NULL)
  .geo_hierarchy(gs, .model_regions(scen))
}

# Regions of each commodity = the members of the commodity's own geo-level.
# A commodity with no `@geoframe` sits at the finest level, which reproduces
# the flat behaviour exactly.
#' @noRd
.comm_region_df <- function(scen) {
  h <- .scen_geo_hierarchy(scen)
  cgl <- map_comm_geoframe(scen)
  if (is.null(h)) {
    # No hierarchy: a `@geoframe` cannot be honoured, and silently balancing
    # the commodity at the finest level would solve a different problem.
    named <- names(cgl)[vapply(cgl, function(v) {
      length(v) > 0 && !is.na(v[1]) && nzchar(v[1])
    }, logical(1))]
    if (length(named) > 0) {
      stop("Commodity ", paste0("'", utils::head(named, 5), "'",
                                collapse = ", "),
           if (length(named) > 5) ", ..." else "",
           " declare(s) `@geoframe`, but the model has no geoscale with ",
           "coarser levels. Attach one with `setGeoscale()`.", call. = FALSE)
    }
    return(NULL)
  }
  rows <- lapply(names(cgl), function(cm) {
    regs <- .geo_level_regions(h, cgl[[cm]])
    if (length(regs) == 0) return(NULL)
    data.frame(comm = cm, region = as.character(regs),
               stringsAsFactors = FALSE)
  })
  dplyr::bind_rows(rows)
}

# (comm, regionp, region): for each commodity, every ancestor `region` of a
# declared region `regionp`, walking up ONE LEVEL AT A TIME and stopping at the
# commodity's own `@geoframe`.
#
# Two things need this. `mvOutTot`/`mvInpTot` must be EXTENDED with the coarse
# cells -- `vOutTot[STEEL, IND, ...]` has to exist before eqOutTot can write the
# equation that sums the states into it -- and every INTERMEDIATE level must be
# present too, because mRegionFamily is adjacent-only and the accumulation
# chains level by level.
#
# Returns NULL when no commodity names a coarser level, which is what leaves a
# flat model untouched.
#' @noRd
.comm_region_chain <- function(scen) {
  h <- .scen_geo_hierarchy(scen)
  if (is.null(h) || nrow(h$family) == 0) return(NULL)

  levels <- h$levels
  rank <- stats::setNames(seq_along(levels), levels)   # 1 = coarsest
  finest <- h$finest

  cgl <- map_comm_geoframe(scen)
  target <- vapply(cgl, function(v) {
    if (length(v) == 0 || is.na(v[1]) || !nzchar(v[1])) finest else as.character(v[1])
  }, character(1))
  bad <- setdiff(unique(target), levels)
  if (length(bad) > 0) {
    stop("Unknown `@geoframe` ", paste0("'", bad, "'", collapse = ", "),
         ". The model's geoscale has: ", paste(levels, collapse = ", "), ".",
         call. = FALSE)
  }
  target <- target[target != finest]
  if (length(target) == 0) return(NULL)

  # Which level each region belongs to, so a walk can be cut at the right depth.
  lvl_of <- unlist(lapply(names(h$members), function(lv) {
    stats::setNames(rep(lv, length(h$members[[lv]])), h$members[[lv]])
  }))

  # Transitive ancestry, accumulated one adjacent step at a time. A join rather
  # than a parent lookup, so a region with more than one parent (levels that
  # cross-cut, which geoscales permits) is handled without special-casing.
  fam <- data.frame(child = as.character(h$family$regionp),
                    parent = as.character(h$family$region),
                    stringsAsFactors = FALSE)
  base <- h$members[[finest]]
  reach <- data.frame(base = base, node = base, stringsAsFactors = FALSE)
  acc <- list()
  for (i in seq_along(levels)) {          # bounded: at most one step per level
    step <- merge(reach, fam, by.x = "node", by.y = "child")
    if (nrow(step) == 0) break
    step <- unique(data.frame(base = step$base, node = step$parent,
                              stringsAsFactors = FALSE))
    acc[[length(acc) + 1L]] <- step
    reach <- step
  }
  if (length(acc) == 0) return(NULL)
  anc <- unique(do.call(rbind, acc))
  anc$lvl <- unname(lvl_of[anc$node])
  anc <- anc[!is.na(anc$lvl), , drop = FALSE]

  rows <- lapply(names(target), function(cm) {
    keep <- anc[rank[anc$lvl] >= rank[[target[[cm]]]], , drop = FALSE]
    if (nrow(keep) == 0) return(NULL)
    data.frame(comm = cm, regionp = keep$base, region = keep$node,
               stringsAsFactors = FALSE)
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (length(rows) == 0) return(NULL)
  unique(do.call(rbind, rows))
}

# Add the coarse cells a commodity's own level needs, keeping the fine ones.
#' @noRd
.extend_comm_region <- function(df, chain) {
  if (is.null(df) || nrow(df) == 0 || is.null(chain)) return(df)
  df <- as.data.frame(df)
  if (!all(c("comm", "region") %in% names(df))) return(df)
  add <- merge(df, chain, by.x = c("comm", "region"),
               by.y = c("comm", "regionp"))
  if (nrow(add) == 0) return(df)
  add$region <- add$region.y
  add$region.y <- NULL
  unique(rbind(df, add[, names(df), drop = FALSE]))
}

# Rewrite each (comm, region) pair to the commodity's OWN balancing level,
# leaving pairs that are already there untouched.
#
# Reachability has to be judged at the level the commodity is balanced at: steel
# produced in R1 and demanded nationally IS served, because the national balance
# pools both. Comparing per fine region would wrongly report infeasibility.
#' @noRd
.lift_to_comm_level <- function(df, scen) {
  if (is.null(df) || nrow(df) == 0) return(df)
  chain <- .comm_region_chain(scen)
  cr <- .comm_region_df(scen)
  if (is.null(chain) || is.null(cr)) return(df)
  # keep only the step that lands exactly on the commodity's own level
  cr$.at <- TRUE
  top <- merge(chain, cr, by = c("comm", "region"))
  if (nrow(top) == 0) return(df)
  top <- unique(top[, c("comm", "regionp", "region")])

  df <- as.data.frame(df)
  m <- merge(df, top, by.x = c("comm", "region"), by.y = c("comm", "regionp"),
             all.x = TRUE)
  m$region <- ifelse(is.na(m$region.y), m$region, m$region.y)
  m$region.y <- NULL
  unique(m[, names(df), drop = FALSE])
}

# Pin a domain to each commodity's own region level. The spatial twin of
# `.restrict_comm_timeslice()`; applied to mvBalance ONLY, never to the totals.
#' @noRd
.restrict_comm_region <- function(df, comm_region) {
  if (is.null(df) || nrow(df) == 0 || is.null(comm_region)) return(df)
  dplyr::distinct(as.data.frame(merge0(df, comm_region)))
}

# -- builders --------------------------------------------------------------- #

# Immediate parent->child region pairs (one level), from the geoscale's own
# adjacent-level family table. Transitive ancestry is deliberately NOT used:
# accumulation walks the levels one step at a time, exactly as mTimesliceFamily does.
map_mRegionFamily <- function(scen, fmp) {
  h <- .scen_geo_hierarchy(scen)
  if (is.null(h)) return(scen)
  .set_cal(scen, "mRegionFamily", h$family, fmp)
}

map_mCommRegion <- function(scen, fmp) {
  .set_cal(scen, "mCommRegion", .comm_region_df(scen), fmp)
}

# -- validation ------------------------------------------------------------- #

# A process must sit at a region level at least as FINE as every commodity it
# touches, because collection only ever flows fine -> coarse.
#
# The temporal rule is the same one `get_process_timeframe()` enforces (a
# process runs at the finest timeframe among its commodities). Spatially the
# failure is silent rather than noisy: a plant placed at the nation producing a
# state-balanced commodity would have its output stranded at a cell no balance
# equation reads, so it simply vanishes from the model.
#' @noRd
.assert_process_geoframe <- function(scen) {
  h <- .scen_geo_hierarchy(scen)
  if (is.null(h)) return(invisible(NULL))
  cr <- .comm_region_df(scen)
  if (is.null(cr)) return(invisible(NULL))

  rank <- stats::setNames(seq_along(h$levels), h$levels)   # 1 = coarsest
  lvl_of <- unlist(lapply(names(h$members), function(lv) {
    stats::setNames(rep(lv, length(h$members[[lv]])), h$members[[lv]])
  }))
  comm_rank <- vapply(split(cr$region, cr$comm), function(rg) {
    min(rank[lvl_of[unique(rg)]], na.rm = TRUE)
  }, numeric(1))

  preg <- named_list_to_df(scen@modInp@sets$process_region,
                           col_names = c("process", "region"))
  pio <- rbind(
    named_list_to_df(scen@modInp@sets$process_inputs,
                     col_names = c("process", "comm")),
    named_list_to_df(scen@modInp@sets$process_outputs,
                     col_names = c("process", "comm"))
  )
  pio <- pio[!is.na(pio$comm) & pio$comm %in% names(comm_rank), , drop = FALSE]
  if (nrow(pio) == 0 || nrow(preg) == 0) return(invisible(NULL))

  d <- merge(preg, pio, by = "process")
  d$preg_rank <- unname(rank[lvl_of[d$region]])
  d$comm_rank <- unname(comm_rank[d$comm])
  bad <- d[!is.na(d$preg_rank) & !is.na(d$comm_rank) &
             d$preg_rank < d$comm_rank, , drop = FALSE]
  if (nrow(bad) == 0) return(invisible(NULL))

  bad <- unique(bad[, c("process", "region", "comm")])
  stop("Process(es) placed at a region level COARSER than a commodity they ",
       "use, whose flows could never reach that commodity's balance:\n   ",
       paste(utils::capture.output(print(bad)), collapse = "\n   "),
       "\nMove the process to a finer region, or declare the commodity at a ",
       "coarser `@geoframe`.", call. = FALSE)
}

# -- engine support --------------------------------------------------------- #


# -- registry --------------------------------------------------------------- #

.region_builders <- list(
  mRegionFamily = map_mRegionFamily,
  mCommRegion   = map_mCommRegion
)
