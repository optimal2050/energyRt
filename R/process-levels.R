# =========================================================================== #
# Level algebra shared by the time and space axes.
#
# Both hierarchies number their levels COARSEST FIRST, so one comparator serves
# both: `calendar@timeframe_rank` is `1:ncol(d)` over the timetable's columns
# (R/class-calendar.R), and the geoscale's rank is the position in `h$levels`.
# A larger rank is therefore FINER on either axis.
# =========================================================================== #

#' Rank of each geoscale level, 1 = coarsest
#'
#' @param h a pruned hierarchy from `.scen_geo_hierarchy()` / `.geo_hierarchy()`.
#' @return named integer over `h$levels`, or `NULL` if `h` is `NULL`.
#' @keywords internal
#' @noRd
.geo_level_rank <- function(h) {
  if (is.null(h) || length(h$levels) == 0) return(NULL)
  stats::setNames(seq_along(h$levels), h$levels)
}

#' Which level each region belongs to
#'
#' @param h a pruned hierarchy.
#' @return named character, region -> level name; `NULL` if `h` is `NULL`.
#' @keywords internal
#' @noRd
.geo_level_of <- function(h) {
  if (is.null(h) || length(h$members) == 0) return(NULL)
  unlist(lapply(names(h$members), function(lv) {
    stats::setNames(rep(lv, length(h$members[[lv]])), h$members[[lv]])
  }))
}

#' Rank of each region's own level, 1 = coarsest
#'
#' `.geo_level_rank()` composed with `.geo_level_of()`: the lookup every caller
#' that compares two regions' depths actually performs.
#'
#' @param h a pruned hierarchy.
#' @return named integer, region -> rank; `NULL` if `h` is `NULL`.
#' @keywords internal
#' @noRd
.geo_region_rank <- function(h) {
  lo <- .geo_level_of(h)
  rk <- .geo_level_rank(h)
  if (is.null(lo) || is.null(rk)) return(NULL)
  stats::setNames(unname(rk[unname(lo)]), names(lo))
}

#' The level each process flow lives at
#'
#' One row per `(process, comm)` pair: the level of the process, the level of
#' the commodity, and `flow` -- the FINER of the two, which is where the flow
#' variable belongs. The two axes are returned separately because the time side
#' needs no region and crossing them multiplies rows for nothing.
#'
#' Today a process is always at-or-finer than every commodity it touches, so
#' `flow` equals `process` on every row. That is the reduction property the
#' tests assert, and it is what makes a coarse process additive rather than a
#' change in behaviour.
#'
#' @param scen a scenario with sets collected (needs `get_process_timeframe()`,
#'   `map_comm_timeframe()`, `get_process_comm()`; the region half additionally
#'   needs `scen@modInp@sets$process_region`).
#' @return data.frame `(process, comm, process_level, comm_level, flow)`, or
#'   `NULL` when the axis does not apply (no geoscale, for `region`).
#' @keywords internal
#' @noRd
.flow_levels <- function(scen, dim = c("timeslice", "region")) {
  dim <- match.arg(dim)
  pc <- get_process_comm(scen)
  if (length(pc) == 0) return(NULL)
  d <- named_list_to_df(pc, col_names = c("process", "comm"))
  d <- d[!is.na(d$comm) & nzchar(d$comm), , drop = FALSE]
  if (nrow(d) == 0) return(NULL)

  if (identical(dim, "timeslice")) {
    rank <- scen@settings@calendar@timeframe_rank
    plv <- get_process_timeframe(scen)
    clv <- map_comm_timeframe(scen)
  } else {
    h <- .scen_geo_hierarchy(scen)
    if (is.null(h)) return(NULL)
    rank <- .geo_level_rank(h)
    reg_rank <- .geo_region_rank(h)
    lvl_of <- .geo_level_of(h)
    # A process's own level is the FINEST of the regions it is declared in --
    # the same per-region reasoning `.assert_process_geoframe()` applies,
    # without materialising the (process, region, comm) cross.
    preg <- get_process_region(scen)
    plv <- lapply(preg, function(rg) {
      rg <- unique(as.character(rg[!is.na(rg)]))
      if (length(rg) == 0) return(NA_character_)
      rk <- reg_rank[rg]
      if (all(is.na(rk))) return(NA_character_)
      unname(lvl_of[rg[which.max(rk)]])
    })
    cr <- .comm_region_df(scen)
    clv <- if (is.null(cr)) {
      list()
    } else {
      # mirror: a commodity's balance level is the COARSEST of its regions
      lapply(split(as.character(cr$region), as.character(cr$comm)),
             function(rg) {
               rk <- reg_rank[unique(rg)]
               if (all(is.na(rk))) NA_character_
               else unname(lvl_of[unique(rg)[which.min(rk)]])
             })
    }
  }

  pick <- function(ll, keys) {
    vapply(keys, function(k) {
      v <- ll[[k]]
      if (is.null(v) || length(v) == 0) NA_character_ else as.character(v)[1]
    }, character(1), USE.NAMES = FALSE)
  }
  d$process_level <- pick(plv, d$process)
  d$comm_level <- pick(clv, d$comm)
  d$flow <- .finer_level(d$process_level, d$comm_level, rank)
  rownames(d) <- NULL
  d
}

# Axes on which the equations that sum a coarse process's finer flows exist in
# ALL FOUR templates. A coarse process on an unlisted axis is refused: accepted
# but unsummed, its flow lands in a cell no balance reads, which either strands
# the flow (infeasible -- measured on a NAT technology drawing per-region
# electricity) or, on the output side, frees it. Either way the model is not
# the one the user wrote.
#
# Grows one axis at a time as the template work lands; empty means a coarse
# process is refused everywhere, exactly as before this feature existed.
.coarse_axes_supported <- c("region", "timeslice")

# Classes whose coarse form is VERIFIED end to end. Storage is SPACE-only: it
# has no `@timeframe` slot, so the time-axis gate never sees one. A
# single-commodity storage cannot be coarse at all under the inputs-only rule
# -- `newStorage(commodity = )` makes the input, the stored commodity and the
# output the same commodity, so one flow level cannot be both finer (as the
# input) and equal (as the output and the level). Only a multi-commodity
# storage can be, and that is what the coarse-storage fixtures cover.
.coarse_classes_supported <- c("technology", "storage")

#' Is a coarse process supported on this axis yet?
#' @keywords internal
#' @noRd
.coarse_axis_ready <- function(dim) dim %in% .coarse_axes_supported

# Features a coarse process does not support yet, as (slot, columns) pairs. A
# column list of NULL means "any row in the slot at all". Each entry names the
# slice that will lift it, so the error can say what is missing rather than
# just refusing.
.coarse_unsupported <- list(
  list(slot = "aux", cols = NULL, what = "auxiliary flows"),
  list(slot = "aeff", cols = NULL, what = "auxiliary flows"),
  list(slot = "inp2out", cols = NULL, what = "storage inp2out coupling"),
  list(slot = "inp2stg", cols = NULL, what = "storage inp2stg coupling"),
  list(slot = "af", cols = c("rampup", "rampdown"), what = "ramp limits"),
  # ruled per-slice, blocked on `pTechAfc`'s interpolation level -- see above
  list(slot = "ceff", cols = c("afc.lo", "afc.up", "afc.fx"),
       what = "per-commodity availability (`afc`)"),
  # storage INPUT availability drives eqStorageInpUp/Lo, whose domain follows
  # the (now finer) input while the capacity it is bounded against stays at the
  # storage's own cell -- the bound would read a capacity that is not there and
  # silently become `inp <= 0`. The aggregate form is not written yet.
  list(slot = "af", cols = c("inp.af.lo", "inp.af.up", "inp.af.fx"),
       what = "storage input availability (`inp.af.*`)")
)

# Which unsupported features an object declares.
#' @noRd
.coarse_blockers <- function(obj) {
  out <- character()
  for (e in .coarse_unsupported) {
    if (!methods::.hasSlot(obj, e$slot)) next
    v <- methods::slot(obj, e$slot)
    if (!is.data.frame(v) || nrow(v) == 0) next
    if (is.null(e$cols)) {
      out <- c(out, e$what)
      next
    }
    present <- intersect(e$cols, colnames(v))
    if (length(present) == 0) next
    if (any(vapply(present, function(cl) any(!is.na(v[[cl]])), logical(1)))) {
      out <- c(out, e$what)
    }
  }
  unique(out)
}

# THE NO-DISAGGREGATION RULE
#
#   A commodity FINER than the process may appear only as an INPUT.
#
# A process may be declared coarser than a commodity it CONSUMES: the input is
# drawn from the finer cells beneath it and summed back
# (mTechProcAggRegion / mStorageProcAggRegion). It may never be coarser than a
# commodity it PRODUCES or STORES -- that would require disaggregating one
# coarse decision into several fine ones, which energyRt does not do. Without
# spreading, such an output lands at the process's cell where the commodity has
# no balance row, so the production enters no balance and is silently
# discarded.
#
# `afc` (per-commodity availability) is RULED to bind PER SLICE / PER REGION --
# it bounds a flow against capacity, so it belongs where its operand lives
# (user ruling 2026-10-11). `eqTechAfc*`'s domain already follows the flow, so
# the equation needs nothing. It is BLOCKED anyway, because the DOMAIN is only
# half of it: `pTechAfc` is interpolated at the PROCESS's frame, so for a
# coarse process the bound exists at the commodity's finer slices while the
# parameter has no value there. The writer omits the `default` clause for an
# infinite default, so GLPK refuses to translate the model and names the tuple
# (`no value for pTechAfcUp[MILL,ELC,...]`). Lifting it needs `pTechAfc`
# expanded to the flow's slices, the same parameter-interpolation work the
# group shares are waiting on.
#
# Inputs are unrestricted in both directions: a COARSER input already works via
# `.commreg_reach()` (draw anywhere in the commodity's cell), a FINER one
# through the maps above.
#' @noRd
.coarse_output_blockers <- function(scen) {
  lv <- .flow_levels(scen, "region")
  if (is.null(lv) || nrow(lv) == 0) return(character())
  coarse <- lv[!is.na(lv$flow) & !is.na(lv$process_level) &
                 lv$flow != lv$process_level, , drop = FALSE]
  if (nrow(coarse) == 0) return(character())
  outs <- get_process_outputs(scen)
  # a storage's STORED commodity is the level, which lives at the storage's own
  # cell, so it must match too -- only the charging input may be finer
  stored <- tryCatch(get_process_stored(scen), error = function(e) list())
  bad <- vapply(seq_len(nrow(coarse)), function(i) {
    p <- coarse$process[i]
    cm <- coarse$comm[i]
    o <- outs[[p]]
    st <- stored[[p]]
    (!is.null(o) && cm %in% as.character(o)) ||
      (!is.null(st) && cm %in% as.character(st))
  }, logical(1))
  if (!any(bad)) return(character())
  unique(paste0(coarse$process[bad], " (`", coarse$comm[bad],
                "` is an output or stored commodity, and is finer than the ",
                "process)"))
}

#' Coarse-process region pairs: (process region, the flow's finer regions)
#'
#' The spatial mirror of `mTechOutCommAggTimeslice`. Where
#' `.flow_levels()` says a flow sits FINER than its process -- i.e. the process
#' is coarser than the commodity, the case this feature adds -- the process's
#' own equations must sum the flow over the fine regions inside the process's
#' cell. This is that enumeration.
#'
#' Rows exist ONLY for the coarse-process case (`flow != process_level`), so
#' the map is empty in every model that is legal today and the `Agg` branch of
#' each equation generates nothing. That is what makes the branch free.
#'
#' @param scen a scenario with sets collected and a geoscale.
#' @return data.frame `(process, comm, region, regionp)` -- `region` is the
#'   process's coarse region, `regionp` a commodity region beneath it -- or
#'   `NULL` when there is no geoscale or no coarse process.
#' @keywords internal
#' @noRd
.flow_region_agg <- function(scen) {
  lv <- .flow_levels(scen, "region")
  if (is.null(lv) || nrow(lv) == 0) return(NULL)
  coarse <- lv[!is.na(lv$flow) & !is.na(lv$process_level) &
                 lv$flow != lv$process_level, , drop = FALSE]
  if (nrow(coarse) == 0) return(NULL)

  anc <- .region_ancestry(scen)          # (regionp = node, region = ancestor)
  if (is.null(anc) || nrow(anc) == 0) return(NULL)
  h <- .scen_geo_hierarchy(scen)
  lvl_of <- .geo_level_of(h)
  preg <- get_process_region(scen)
  cr <- .comm_region_df(scen)
  if (is.null(cr)) return(NULL)
  comm_regions <- split(as.character(cr$region), as.character(cr$comm))

  # descendants of a coarse region, at the level the flow actually sits at
  desc <- split(as.character(anc$regionp), as.character(anc$region))

  out <- lapply(seq_len(nrow(coarse)), function(i) {
    p <- coarse$process[i]
    cm <- coarse$comm[i]
    flow_lv <- coarse$flow[i]
    pr <- unique(as.character(preg[[p]]))
    pr <- pr[!is.na(pr)]
    cmr <- unique(comm_regions[[cm]])
    if (length(pr) == 0 || length(cmr) == 0) return(NULL)
    rows <- lapply(pr, function(r) {
      # the commodity's regions strictly beneath this process region, kept to
      # the flow's level so a three-level hierarchy does not pick up the
      # intermediate cells as well
      cand <- intersect(desc[[r]], cmr)
      cand <- cand[!is.na(lvl_of[cand]) & lvl_of[cand] == flow_lv]
      if (length(cand) == 0) return(NULL)
      data.frame(process = p, comm = cm, region = r, regionp = cand,
                 stringsAsFactors = FALSE)
    })
    do.call(rbind, rows[!vapply(rows, is.null, logical(1))])
  })
  out <- out[!vapply(out, is.null, logical(1))]
  if (length(out) == 0) return(NULL)
  res <- do.call(rbind, out)
  rownames(res) <- NULL
  unique(res)
}

#' Move a coarse process's flow rows down to the commodity's regions
#'
#' The mirror of `.commreg_reach()`, which widens a coarse COMMODITY's reach
#' down to a fine process's regions. Here the PROCESS is the coarse one, so its
#' flow rows -- carried at its own region by `merge0(mvTechAct, mTech*Comm)` --
#' are replaced by one row per commodity region beneath it. The activity domain
#' itself stays coarse; only the flows move.
#'
#' A no-op until the axis is enabled, and a no-op in any model with no coarse
#' process, because `.flow_region_agg()` is then empty.
#'
#' @param df a flow domain carrying `key`, `comm` and `region`.
#' @param key the process column name (`"tech"`, `"stg"`, ...).
#' @keywords internal
#' @noRd
.flow_region_spread <- function(scen, df, key) {
  if (!.coarse_axis_ready("region")) return(df)
  if (is.null(df) || nrow(df) == 0) return(df)
  df <- as.data.frame(df)
  if (!all(c(key, "comm", "region") %in% names(df))) return(df)
  ag <- .flow_region_agg(scen)
  if (is.null(ag) || nrow(ag) == 0) return(df)

  m <- data.frame(ag$process, ag$comm, ag$region, ag$regionp,
                  stringsAsFactors = FALSE)
  names(m) <- c(key, "comm", "region", ".regionp")
  by <- c(key, "comm", "region")
  hit <- merge(df, m, by = by)
  if (nrow(hit) == 0) return(df)
  # the coarse rows that were rewritten drop out; the fine ones replace them
  keep <- dplyr::anti_join(df, unique(m[, by, drop = FALSE]), by = by)
  hit$region <- hit$.regionp
  hit$.regionp <- NULL
  unique(rbind(as.data.frame(keep), hit[, names(df), drop = FALSE]))
}

#' Lift a coarse process's flow rows back up to its own region
#'
#' The inverse of `.flow_region_spread()`, for the EQUATION domains rather than
#' the flow domains. A process equation is indexed at the process's own cell --
#' `vTechAct[t,r,y,s]`, `vTechCap[t,r,y]` live there -- so a domain built by
#' joining flow maps has to be collapsed back onto that cell, or the equation
#' references a region its own variables do not have.
#'
#' Collapsing is what makes the input/output join work again: a coarse process's
#' input (at the commodity's regions) and output (possibly at another level)
#' share no region, but they share the process's cell.
#'
#' A no-op until the axis is enabled and in any model with no coarse process.
#'
#' @param df a domain carrying `key` and `region` (and `comm`, when present, is
#'   used to resolve the pairing precisely).
#' @param key the process column name.
#' @keywords internal
#' @noRd
.lift_flow_to_proc_region <- function(scen, df, key) {
  if (!.coarse_axis_ready("region")) return(df)
  if (is.null(df) || nrow(df) == 0) return(df)
  df <- as.data.frame(df)
  if (!all(c(key, "region") %in% names(df))) return(df)
  ag <- .flow_region_agg(scen)
  if (is.null(ag) || nrow(ag) == 0) return(df)

  # (process, fine region) -> coarse region. Keyed without `comm` so a domain
  # that has already dropped the commodity column still lifts; a process has
  # one region level, so the mapping is unambiguous either way.
  m <- unique(data.frame(ag$process, ag$regionp, ag$region,
                         stringsAsFactors = FALSE))
  names(m) <- c(key, "region", ".coarse")
  m <- m[!duplicated(m[, c(key, "region")]), , drop = FALSE]
  out <- merge(df, m, by = c(key, "region"), all.x = TRUE)
  hit <- !is.na(out$.coarse)
  if (any(hit)) out$region[hit] <- out$.coarse[hit]
  out$.coarse <- NULL
  unique(out[, names(df), drop = FALSE])
}

#' The finer of two level names
#'
#' Vectorised and axis-agnostic: pass `calendar@timeframe_rank` for the time
#' axis or `.geo_level_rank()` for space. Ties return the first argument.
#'
#' `NA` and `""` mean "not stated" and lose to a stated level, so a caller that
#' has already resolved its defaults gets the stated one back, and one that has
#' not gets `NA` only when neither side states anything. An unknown level NAME
#' is an error, not an `NA`: silently dropping it is the failure mode the level
#' checks exist to prevent.
#'
#' @param a,b character level names; length 1 is recycled.
#' @param rank named integer, 1 = coarsest (larger = finer).
#' @return character, the finer level of each pair.
#' @keywords internal
#' @noRd
.finer_level <- function(a, b, rank) {
  if (length(rank) == 0 || is.null(names(rank))) {
    stop("`rank` must be a named integer vector of levels.", call. = FALSE)
  }
  a <- as.character(a)
  b <- as.character(b)
  n <- max(length(a), length(b))
  if (length(a) == 1L) a <- rep(a, n)
  if (length(b) == 1L) b <- rep(b, n)
  if (length(a) != n || length(b) != n) {
    stop("`a` and `b` must have the same length (or be length 1).",
         call. = FALSE)
  }
  if (n == 0L) return(character())

  stated <- function(x) !is.na(x) & nzchar(x)
  unknown <- setdiff(unique(c(a[stated(a)], b[stated(b)])), names(rank))
  if (length(unknown) > 0) {
    stop("unknown level(s) ", paste0('"', sort(unknown), '"', collapse = ", "),
         ". Known: ", paste(names(rank), collapse = ", "), ".", call. = FALSE)
  }

  ra <- unname(rank[a])
  rb <- unname(rank[b])
  out <- ifelse(is.na(rb) | (!is.na(ra) & ra >= rb), a, b)
  out[is.na(ra) & is.na(rb)] <- NA_character_
  out
}

#' The (process, comm) pairs a `*ProcAggRegion` map records
#'
#' Reads one of the coarse-process maps back off a built scenario, with the
#' process column renamed, so a caller can test the roles those commodities
#' play without knowing which class the map belongs to.
#' @keywords internal
#' @noRd
.pl_agg_pairs <- function(scen, nm, key) {
  p <- tryCatch(scen@modInp@parameters[[nm]], error = function(e) NULL)
  if (is.null(p)) return(NULL)
  d <- tryCatch(get_data_slot(p), error = function(e) NULL)
  if (is.null(d) || nrow(d) == 0) return(NULL)
  d <- as.data.frame(d)
  if (!all(c(key, "comm") %in% names(d))) return(NULL)
  unique(data.frame(process = as.character(d[[key]]),
                    comm = as.character(d$comm),
                    stringsAsFactors = FALSE))
}

#' Coarse-process timeslice pairs: (process slice, the flow's finer slices)
#'
#' The time twin of `.flow_region_agg()`. Where `.flow_levels()` says a flow
#' sits FINER than its process, the process's own equations must sum the flow
#' over the finer slices inside its own -- weighted by `pTimesliceAgg`, because
#' timeslice values are intensive RATES and summing them needs renormalising by
#' duration (unlike regions, which are extensive and simply add).
#'
#' Rows exist ONLY for the coarse-process case, so the map is empty in every
#' model that is legal without this feature.
#'
#' @return data.frame `(process, comm, timeslice, timeslicep)` -- `timeslice` is
#'   the process's own (coarse) slice, `timeslicep` a commodity slice beneath
#'   it -- or `NULL` when no process is coarser.
#' @keywords internal
#' @noRd
.flow_timeslice_agg <- function(scen) {
  lv <- .flow_levels(scen, "timeslice")
  if (is.null(lv) || nrow(lv) == 0) return(NULL)
  coarse <- lv[!is.na(lv$flow) & !is.na(lv$process_level) &
                 lv$flow != lv$process_level, , drop = FALSE]
  if (nrow(coarse) == 0) return(NULL)

  cal <- scen@settings@calendar
  anc <- as.data.frame(cal@timeslice_ancestry)   # (parent, child), transitive
  if (is.null(anc) || nrow(anc) == 0) return(NULL)
  frames <- cal@timeframes
  # slice -> its level, so a three-level calendar does not also pick up the
  # intermediate cells
  lvl_of <- unlist(lapply(names(frames), function(lv2)
    stats::setNames(rep(lv2, length(frames[[lv2]])), frames[[lv2]])))
  desc <- split(as.character(anc$child), as.character(anc$parent))
  ptf <- get_process_timeframe(scen)

  out <- lapply(seq_len(nrow(coarse)), function(i) {
    p <- coarse$process[i]
    cm <- coarse$comm[i]
    flow_lv <- coarse$flow[i]
    own <- frames[[as.character(ptf[[p]])]]
    if (length(own) == 0) return(NULL)
    rows <- lapply(as.character(own), function(sl) {
      cand <- desc[[sl]]
      cand <- cand[!is.na(lvl_of[cand]) & lvl_of[cand] == flow_lv]
      if (length(cand) == 0) return(NULL)
      data.frame(process = p, comm = cm, timeslice = sl, timeslicep = cand,
                 stringsAsFactors = FALSE)
    })
    do.call(rbind, rows[!vapply(rows, is.null, logical(1))])
  })
  out <- out[!vapply(out, is.null, logical(1))]
  if (length(out) == 0) return(NULL)
  res <- do.call(rbind, out)
  rownames(res) <- NULL
  unique(res)
}

#' Move a coarse process's flow rows down to the commodity's timeslices
#'
#' The time twin of `.flow_region_spread()`. The activity domain stays at the
#' process's own slice; only the flows move.
#' @keywords internal
#' @noRd
.flow_timeslice_spread <- function(scen, df, key) {
  if (!.coarse_axis_ready("timeslice")) return(df)
  if (is.null(df) || nrow(df) == 0) return(df)
  df <- as.data.frame(df)
  if (!all(c(key, "comm", "timeslice") %in% names(df))) return(df)
  ag <- .flow_timeslice_agg(scen)
  if (is.null(ag) || nrow(ag) == 0) return(df)

  m <- data.frame(ag$process, ag$comm, ag$timeslice, ag$timeslicep,
                  stringsAsFactors = FALSE)
  names(m) <- c(key, "comm", "timeslice", ".tsp")
  by <- c(key, "comm", "timeslice")
  hit <- merge(df, m, by = by)
  if (nrow(hit) == 0) return(df)
  keep <- dplyr::anti_join(df, unique(m[, by, drop = FALSE]), by = by)
  hit$timeslice <- hit$.tsp
  hit$.tsp <- NULL
  unique(rbind(as.data.frame(keep), hit[, names(df), drop = FALSE]))
}

#' Lift a coarse process's flow rows back up to its own timeslice
#' @keywords internal
#' @noRd
.lift_flow_to_proc_timeslice <- function(scen, df, key) {
  if (!.coarse_axis_ready("timeslice")) return(df)
  if (is.null(df) || nrow(df) == 0) return(df)
  df <- as.data.frame(df)
  if (!all(c(key, "timeslice") %in% names(df))) return(df)
  ag <- .flow_timeslice_agg(scen)
  if (is.null(ag) || nrow(ag) == 0) return(df)
  m <- unique(data.frame(ag$process, ag$timeslicep, ag$timeslice,
                         stringsAsFactors = FALSE))
  names(m) <- c(key, "timeslice", ".coarse")
  m <- m[!duplicated(m[, c(key, "timeslice")]), , drop = FALSE]
  out <- merge(df, m, by = c(key, "timeslice"), all.x = TRUE)
  hit <- !is.na(out$.coarse)
  if (any(hit)) out$timeslice[hit] <- out$.coarse[hit]
  out$.coarse <- NULL
  unique(out[, names(df), drop = FALSE])
}

#' Lift a share domain to the COARSEST flow level among its group's members
#'
#' A share relates flows to flows, so it belongs at the level the flows live
#' at -- not at the process's level, where the ratio and capacity equations
#' live because those relate activity and capacity. The level is the COARSEST
#' among the group's member flows: the finest level at which every member can
#' be compared without disaggregating any of them (the no-disaggregation rule).
#'
#' Consequences, both intended:
#' * a group whose flows are all finer than the process keeps a PER-SLICE (or
#'   per-region) share, so coarsening the process does not loosen it;
#' * a group with a member at the process's own level falls back to that level,
#'   which is the only level where the comparison is expressible.
#'
#' Never coarser than the flows and never finer than the coarsest member, so it
#' is always at-or-finer than the process's level -- i.e. never looser than
#' lifting all the way to the process.
#'
#' @param df a share domain carrying `key`, `group`, `comm` and `dim`.
#' @param dim `"timeslice"` or `"region"`.
#' @keywords internal
#' @noRd
.lift_to_group_level <- function(scen, df, key, dim) {
  if (!.coarse_axis_ready(dim)) return(df)
  if (is.null(df) || nrow(df) == 0) return(df)
  df <- as.data.frame(df)
  if (!all(c(key, "group", "comm", dim) %in% names(df))) return(df)
  lv <- .flow_levels(scen, dim)
  if (is.null(lv) || nrow(lv) == 0) return(df)

  if (identical(dim, "timeslice")) {
    cal <- scen@settings@calendar
    rank <- cal@timeframe_rank
    frames <- cal@timeframes
    lvl_of <- unlist(lapply(names(frames), function(l)
      stats::setNames(rep(l, length(frames[[l]])), frames[[l]])))
    anc <- as.data.frame(cal@timeslice_ancestry)
    pairs <- data.frame(node = as.character(anc$child),
                        up = as.character(anc$parent),
                        stringsAsFactors = FALSE)
  } else {
    h <- .scen_geo_hierarchy(scen)
    rank <- .geo_level_rank(h)
    lvl_of <- .geo_level_of(h)
    anc <- .region_ancestry(scen)
    if (is.null(anc) || nrow(anc) == 0) return(df)
    pairs <- data.frame(node = as.character(anc$regionp),
                        up = as.character(anc$region),
                        stringsAsFactors = FALSE)
  }
  if (is.null(rank) || length(rank) == 0) return(df)

  # flow level of each (process, comm)
  fl <- stats::setNames(lv$flow, paste(lv$process, lv$comm, sep = "\r"))
  # target level per (process, group): the COARSEST member flow level
  gk <- paste(df[[key]], df$group, sep = "\r")
  mem <- fl[paste(df[[key]], df$comm, sep = "\r")]
  target <- tapply(unname(mem), gk, function(z) {
    z <- z[!is.na(z)]
    if (length(z) == 0) return(NA_character_)
    z[which.min(unname(rank[z]))]
  })

  # ancestor-or-self of `node` at `want`
  at_level <- function(node, want) {
    if (is.na(node) || is.na(want)) return(node)
    if (!is.na(lvl_of[node]) && identical(unname(lvl_of[node]), want)) return(node)
    cand <- pairs$up[pairs$node == node]
    cand <- cand[!is.na(lvl_of[cand]) & lvl_of[cand] == want]
    if (length(cand) == 0) node else cand[1]
  }
  tg <- unname(target[gk])
  cur <- as.character(df[[dim]])
  df[[dim]] <- vapply(seq_along(cur), function(i) at_level(cur[i], tg[i]),
                      character(1))
  unique(df)
}
