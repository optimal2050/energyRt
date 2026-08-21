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
