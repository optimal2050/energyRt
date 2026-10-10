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
#' @param h a pruned hierarchy from [.scen_geo_hierarchy()] / `.geo_hierarchy()`.
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
#' [.geo_level_rank()] composed with [.geo_level_of()]: the lookup every caller
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
.coarse_axes_supported <- "region"      # "timeslice" follows

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
  list(slot = "group", cols = NULL, what = "commodity groups"),
  list(slot = "geff", cols = NULL, what = "commodity groups"),
  list(slot = "ceff",
       cols = c("cinp2ginp", "grp.share.lo", "grp.share.up", "grp.share.fx"),
       what = "commodity groups and shares"),
  list(slot = "ceff", cols = c("afc.lo", "afc.up", "afc.fx"),
       what = "per-commodity availability (`afc`)"),
  list(slot = "af", cols = c("rampup", "rampdown"), what = "ramp limits")
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
  # single-in / single-out only, so a second commodity on either side blocks
  n_io <- function(sn) {
    if (!methods::.hasSlot(obj, sn)) return(0L)
    v <- methods::slot(obj, sn)
    if (!is.data.frame(v) || is.null(v$comm)) return(0L)
    length(unique(v$comm[!is.na(v$comm)]))
  }
  if (n_io("input") > 1L || n_io("output") > 1L) {
    out <- c(out, "more than one input or output commodity")
  }
  unique(out)
}

# An OUTPUT finer than the process is refused: each side's conversion
# coefficients must be read at its own region, which the GAMS form cannot do
# without division (it multiplies pTechUse2cact * pTechCact2cout deliberately).
# Inputs finer than the process are the supported direction, and the case the
# feature exists for -- a national plant drawing a regionally-balanced input.
#' @noRd
.coarse_output_blockers <- function(scen) {
  lv <- .flow_levels(scen, "region")
  if (is.null(lv) || nrow(lv) == 0) return(character())
  coarse <- lv[!is.na(lv$flow) & !is.na(lv$process_level) &
                 lv$flow != lv$process_level, , drop = FALSE]
  if (nrow(coarse) == 0) return(character())
  outs <- get_process_outputs(scen)
  bad <- vapply(seq_len(nrow(coarse)), function(i) {
    o <- outs[[coarse$process[i]]]
    !is.null(o) && coarse$comm[i] %in% as.character(o)
  }, logical(1))
  if (!any(bad)) return(character())
  unique(paste0(coarse$process[bad], " (output `", coarse$comm[bad],
                "` is finer than the process)"))
}

#' Coarse-process region pairs: (process region, the flow's finer regions)
#'
#' The spatial mirror of `mTechOutCommAggTimeslice`. Where
#' [.flow_levels()] says a flow sits FINER than its process -- i.e. the process
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
#' The mirror of [.commreg_geo_spreader()], which spreads a coarse COMMODITY
#' down to a fine process's regions. Here the PROCESS is the coarse one, so its
#' flow rows -- carried at its own region by `merge0(mvTechAct, mTech*Comm)` --
#' are replaced by one row per commodity region beneath it. The activity domain
#' itself stays coarse; only the flows move.
#'
#' A no-op until the axis is enabled, and a no-op in any model with no coarse
#' process, because [.flow_region_agg()] is then empty.
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
#' The inverse of [.flow_region_spread()], for the EQUATION domains rather than
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
#' axis or [.geo_level_rank()] for space. Ties return the first argument.
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

# A coarse process (one declared at a region level coarser than a commodity it
# uses) needs the mTechProcAggRegion branch in the process equations. GLPK and
# GAMS carry it; this backend does not yet, and without it the flow is read at
# a cell that holds no variable -- the model solves with the flow free. Refuse
# rather than return a wrong answer.
#' @noRd
.assert_no_coarse_process <- function(scen, backend) {
  p <- tryCatch(scen@modInp@parameters[["mTechProcAggRegion"]],
                error = function(e) NULL)
  if (is.null(p)) return(invisible(NULL))
  d <- tryCatch(get_data_slot(p), error = function(e) NULL)
  if (is.null(d) || nrow(d) == 0) return(invisible(NULL))
  stop("Process(es) ", paste0('"', sort(unique(as.character(d$tech))), '"',
                              collapse = ", "),
       " sit at a region level COARSER than a commodity they use, which the ",
       backend, " backend does not support yet (GLPK and GAMS do). Solve ",
       "with `lang = \"GLPK\"` or `\"GAMS\"`, or place the process at the ",
       "commodity's region level.", call. = FALSE)
}
