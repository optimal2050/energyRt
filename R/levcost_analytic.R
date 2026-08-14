# levcost_analytic.R
# Analytic (no-solve) levelized-cost path for levcost(method = "analytic"/"auto").
#
# The levcost mini-model (levcost.R) is a unit-demand, single-technology,
# annual-calendar LP with capacity bounds stripped and every year a milestone.
# That LP has a closed-form optimum, mirrored here equation by equation from
# the GLPK template: eqTechActSng/Grp (activity <-> output), eqTechSng2Sng /
# eqTechGrp2Sng (input <-> use, per pair -- ungrouped inputs are COMPLEMENTS,
# each carries the full use; only inputs inside a @group substitute),
# eqTechAf/Afs (annual availability sizes capacity), eqTechCap (capacity =
# surviving builds; greedy build-when-needed is LP-optimal because deferral
# is weakly cheaper at a constant invcost), eqTechEac (invcost x CRF charged
# over payback|olife years from each build), eqTechFixom/Varom, eqSupCost,
# free disposal (commodity limtype defaults to "LO"), and the backstop
# (unserved years produce and cost nothing).
#
# Group shares make the LP choose a mix; the optimum is a VERTEX of the share
# polytope {sum(s) = 1, lo <= s <= up}, every vertex of which has at most one
# fractional coordinate. `.levcost_share_vertices()` enumerates them all, the
# base result takes the cost-minimal vertex, and `$frontier_vertices` reports
# every corner (user request 2026-08-13: all corner solutions, not just the
# minimum).
#
# Anything the closed form cannot represent is REFUSED -- never approximated
# (no-auto-invention policy): cheap static checks live in
# `.levcost_analytic_blockers()`; conditions discovered mid-run raise a
# `levcost_analytic_unsupported` condition, which method = "auto" catches to
# fall back to the solver.
#
# Parity note: solved quantities are exact-zero-dropped like the GLPK csv
# writers (`<> 0` filters), because the extraction arithmetic keys its
# normaliser and NPV sums off row PRESENCE, not values.

.la_unsupported <- function(...) {
  stop(errorCondition(paste0(...),
                      class = c("levcost_analytic_unsupported", "error",
                                "condition")))
}

# ── year / region series readers ───────────────────────────────────────────────

# One value column of a slot data.frame as a per-year series, mirroring the
# model's back.inter.forth interpolation (linear inside, constant beyond) and
# region precedence (a region-specific row beats a region-NA broadcast).
.la_series <- function(df, val, years, region, default = NA_real_, what = val) {
  if (is.null(df) || nrow(df) == 0 || !val %in% names(df))
    return(rep(as.numeric(default), length(years)))
  d <- df[!is.na(df[[val]]), , drop = FALSE]
  if (nrow(d) == 0) return(rep(as.numeric(default), length(years)))
  if ("region" %in% names(d)) {
    reg <- as.character(d$region)
    d <- d[is.na(reg) | reg == region, , drop = FALSE]
    if (nrow(d) == 0) return(rep(as.numeric(default), length(years)))
    reg <- as.character(d$region)
    if (any(!is.na(reg))) d <- d[!is.na(reg), , drop = FALSE]
  }
  has_year <- "year" %in% names(d) && any(!is.na(d$year))
  if (!has_year) {
    v <- unique(as.numeric(d[[val]]))
    if (length(v) > 1)
      .la_unsupported("conflicting values of ", what)
    return(rep(v, length(years)))
  }
  if (any(is.na(d$year)))
    .la_unsupported("mixed year-specific and year-free rows for ", what)
  agg <- unique(data.frame(year = as.integer(d$year),
                           v = as.numeric(d[[val]])))
  if (anyDuplicated(agg$year) > 0)
    .la_unsupported("conflicting year rows for ", what)
  if (nrow(agg) == 1) return(rep(agg$v, length(years)))
  as.numeric(stats::approx(agg$year, agg$v, xout = years, rule = 2)$y)
}

# `.la_series` over the rows of `df` selected by one key column (comm / acomm /
# group); rows with an NA key broadcast when no keyed row exists.
.la_series_key <- function(df, key_col, key, val, years, region,
                           default = NA_real_, what = val) {
  if (is.null(df) || nrow(df) == 0 || !val %in% names(df) ||
      !key_col %in% names(df))
    return(rep(as.numeric(default), length(years)))
  d <- df[!is.na(df[[val]]), , drop = FALSE]
  k <- as.character(d[[key_col]])
  keyed <- d[!is.na(k) & k == key, , drop = FALSE]
  if (nrow(keyed) == 0) keyed <- d[is.na(k), , drop = FALSE]
  .la_series(keyed, val, years, region, default = default,
             what = paste0(what, "[", key, "]"))
}

# Per-year price of a supply object's commodity. Levcost supplies are spread
# verbatim across the (possibly artificial per-variant) regions, so the price
# is read region-agnostically; regionally differing prices are refused.
.la_supply_price <- function(sup, years, cm) {
  if (!isS4(sup) || !.hasSlot(sup, "supply") || nrow(sup@supply) == 0 ||
      !"cost" %in% names(sup@supply))
    return(rep(0, length(years)))
  d <- sup@supply[!is.na(sup@supply$cost), , drop = FALSE]
  if (nrow(d) == 0) return(rep(0, length(years)))
  has_year <- "year" %in% names(d) && any(!is.na(d$year))
  if (has_year && any(is.na(d$year)))
    .la_unsupported("mixed year rows in the supply price of '", cm, "'")
  if (!has_year) {
    v <- unique(as.numeric(d$cost))
    if (length(v) > 1)
      .la_unsupported("regionally differing supply price for '", cm, "'")
    return(rep(v, length(years)))
  }
  agg <- unique(data.frame(year = as.integer(d$year),
                           v = as.numeric(d$cost)))
  if (anyDuplicated(agg$year) > 0)
    .la_unsupported("regionally differing supply price for '", cm, "'")
  if (nrow(agg) == 1) return(rep(agg$v, length(years)))
  as.numeric(stats::approx(agg$year, agg$v, xout = years, rule = 2)$y)
}

# ── lifespan / availability ────────────────────────────────────────────────────

# start / end / olife of one (possibly devintaged) process as scalars for the
# pinned region. Post-expansion cells resolve to at most one value per region;
# anything else is not representable in a single mini-model process.
.la_lifespan <- function(cell, region) {
  out <- list()
  for (cc in .vintage_val_cols) {
    d <- .lifespan_resolve(cell, cc)
    if (nrow(d) > 0) {
      reg <- as.character(d$region)
      d <- d[is.na(reg) | reg == region, , drop = FALSE]
      reg <- as.character(d$region)
      if (any(!is.na(reg))) d <- d[!is.na(reg), , drop = FALSE]
    }
    v <- unique(d[[cc]])
    if (length(v) > 1)
      .la_unsupported("ambiguous @vintage ", cc, " for '", cell@name, "'")
    out[[cc]] <- if (length(v) == 0) NA_real_ else as.numeric(v)
  }
  out
}

# Effective annual availability: af.fx wins over the min of the upper bounds
# (af.up, afs.fx/up on the ANNUAL slice); default 1. Lower bounds and
# sub-annual rows are blocked upstream.
.la_af <- function(cell, years, region) {
  ub <- rep(1, length(years))
  fx <- rep(NA_real_, length(years))
  for (src in list(list(cell@af, "af.fx", TRUE),  list(cell@af, "af.up", FALSE),
                   list(cell@afs, "afs.fx", TRUE), list(cell@afs, "afs.up", FALSE))) {
    v <- .la_series(src[[1]], src[[2]], years, region, default = NA_real_,
                    what = paste0("@af ", src[[2]]))
    if (all(is.na(v))) next
    if (src[[3]]) fx <- ifelse(is.na(fx), v, pmin(fx, v))
    else          ub <- pmin(ub, ifelse(is.na(v), Inf, v))
  }
  has_fx <- any(!is.na(fx))
  if (has_fx) {
    if (length(unique(round(fx[!is.na(fx)], 12))) > 1)
      .la_unsupported("year-varying af.fx")
    if (any(fx > ub + 1e-9))
      .la_unsupported("af.fx above an upper availability bound")
    return(ifelse(is.na(fx), ub, fx))
  }
  ub
}

# ── share polytope vertices ────────────────────────────────────────────────────

# All vertices of {s: sum(s) = 1, lo <= s <= up}. Each vertex has at most one
# coordinate strictly between its bounds; the rest sit at lo or up.
.levcost_share_vertices <- function(lo, up) {
  nm <- names(lo)
  lo <- pmax(ifelse(is.na(lo), 0, lo), 0)
  up <- pmin(ifelse(is.na(up), 1, up), 1)
  if (any(lo > up + 1e-9))
    stop("share.lo above share.up for: ",
         paste(nm[lo > up + 1e-9], collapse = ", "), call. = FALSE)
  if (sum(lo) > 1 + 1e-9 || sum(up) < 1 - 1e-9)
    stop("infeasible share bounds: sum(lo) = ", round(sum(lo), 4),
         ", sum(up) = ", round(sum(up), 4), call. = FALSE)
  n <- length(lo)
  if (n > 12)
    .la_unsupported("share group with more than 12 commodities")
  seen <- new.env(parent = emptyenv())
  out <- list()
  add <- function(v) {
    key <- paste(round(v, 10), collapse = "|")
    if (is.null(seen[[key]])) {
      seen[[key]] <- TRUE
      out[[length(out) + 1L]] <<- v
    }
  }
  for (free in c(NA_integer_, seq_len(n))) {
    fixed <- if (is.na(free)) seq_len(n) else setdiff(seq_len(n), free)
    combos <- if (length(fixed) == 0) list(integer(0)) else {
      g <- rep(list(c(FALSE, TRUE)), length(fixed))
      as.list(as.data.frame(t(as.matrix(expand.grid(g)))))
    }
    for (hi in combos) {
      v <- numeric(n)
      v[fixed] <- ifelse(unlist(hi), up[fixed], lo[fixed])
      if (is.na(free)) {
        if (abs(sum(v) - 1) > 1e-9) next
      } else {
        sf <- 1 - sum(v[fixed])
        if (sf < lo[free] - 1e-9 || sf > up[free] + 1e-9) next
        v[free] <- sf
      }
      add(pmin(pmax(v, 0), 1))
    }
  }
  m <- do.call(rbind, out)
  colnames(m) <- nm
  m
}

# ── one-cell evaluation ────────────────────────────────────────────────────────

# Evaluate one process cell against a demand vector, optionally at forced
# output / input-group share vertices (NULL = pick the LP optimum). Returns
# the per-year solved series; all quantities per the template equations.
.la_eval <- function(cell, ctx, dem_vals, out_v = NULL, in_v = NULL) {
  years  <- ctx$hor_years
  ny     <- length(years)
  region <- if (length(cell@region) > 0) cell@region[1] else ctx$region

  lf <- .la_lifespan(cell, region)
  ws <- if (is.finite(lf$start)) lf$start else -Inf
  we <- if (is.finite(lf$end))   lf$end   else  Inf
  olife <- if (is.finite(lf$olife) && lf$olife > 0) lf$olife else Inf

  cap2act <- if (length(cell@cap2act) == 1 && is.finite(cell@cap2act))
    cell@cap2act else 1
  af <- .la_af(cell, years, region)

  outs_all <- unique(as.character(cell@output$comm))
  outs     <- ctx$all_out_comms
  byprod   <- setdiff(outs_all, outs)

  cc <- sapply(outs_all, function(cm)
    .la_series_key(cell@ceff, "comm", cm, "cact2cout", years, region,
                   default = 1, what = "cact2cout"))
  cc <- matrix(cc, nrow = ny, dimnames = list(NULL, outs_all))

  # `use` is eliminated against each output with the same use2cact factor;
  # differing factors would make the pair equations inconsistent.
  u2c_m <- sapply(outs, function(cm)
    .la_series_key(cell@ceff, "comm", cm, "use2cact", years, region,
                   default = 1, what = "use2cact"))
  u2c_m <- matrix(u2c_m, nrow = ny, dimnames = list(NULL, outs))
  if (length(ctx$in_comms) > 0 && ncol(u2c_m) > 1 &&
      any(abs(u2c_m - u2c_m[, 1]) > 1e-12))
    .la_unsupported("use2cact differs across output commodities")
  u2c <- u2c_m[, 1]

  dv <- pmax(unlist(dem_vals[outs]), 1e-9)          # mirror make_demands_()

  # ── output allocation ──
  # grouped >= 2: served total T* maximises backstop displacement
  # (lexicographic 1e6 >> own cost), then the mix is a cost-minimal vertex.
  if (ctx$has_grouped_output && length(outs) >= 2) {
    if (!setequal(ctx$out_comms, ctx$all_out_comms))
      .la_unsupported("comm subset of a grouped output")
    lo <- pmax(ifelse(is.na(ctx$share_lo[outs]), 0, ctx$share_lo[outs]), 0)
    up <- pmin(ifelse(is.na(ctx$share_up[outs]), 1, ctx$share_up[outs]), 1)
    # T* : largest T with sum(min(dv, up*T)) >= T and lo*T <= dv
    t_hi <- min(sum(dv), min(ifelse(lo > 0, dv / lo, Inf)))
    f <- function(T) sum(pmin(dv, up * T)) - T
    if (f(t_hi) < -1e-12) {
      bps <- sort(unique(c(dv / pmax(up, 1e-12), t_hi)))
      bps <- bps[bps <= t_hi + 1e-12]
      t_star <- 0
      prev <- 0
      for (b in bps) {
        if (f(b) >= -1e-12) { prev <- b; next }
        # root in (prev, b]: f is linear on the segment
        slope <- (f(b) - f(prev)) / (b - prev)
        t_star <- prev - f(prev) / slope
        break
      }
      if (t_star == 0 && f(prev) >= -1e-12) t_star <- prev
    } else t_star <- t_hi
    # feasible out range at T*: [max(lo*T), min(dv, up*T)], sum == T*
    o_lo <- lo * t_star
    o_up <- pmin(dv, up * t_star)
    # vertices of the out polytope, expressed as share vertices of s = out/T*
    verts <- .levcost_share_vertices(
      setNames(o_lo / t_star, outs), setNames(o_up / t_star, outs))
    if (!is.null(out_v)) {
      verts <- matrix(out_v[outs], nrow = 1, dimnames = list(NULL, outs))
    }
    out_choices <- lapply(seq_len(nrow(verts)), function(i) verts[i, ] * t_star)
  } else {
    out_choices <- list(setNames(dv, outs))
    verts <- matrix(rep(1, length(outs)) / length(outs), nrow = 1,
                    dimnames = list(NULL, outs))
  }

  # ── input-group vertex choice (independent of the output mix) ──
  prices <- ctx$prices
  in_alloc <- list()      # per group: list(sigma, totinp_per_G)
  for (g in ctx$in_groups) {
    gc <- ctx$in_group_comms[[g]]
    vlo <- ctx$in_share_lo[[g]][gc]
    vup <- ctx$in_share_up[[g]][gc]
    gverts <- .levcost_share_vertices(setNames(ifelse(is.na(vlo), 0, vlo), gc),
                                      setNames(ifelse(is.na(vup), 1, vup), gc))
    if (!is.null(in_v) && !is.null(in_v[[g]]))
      gverts <- matrix(in_v[[g]][gc], nrow = 1, dimnames = list(NULL, gc))
    c2g <- sapply(gc, function(cm)
      .la_series_key(cell@ceff, "comm", cm, "cinp2ginp", years, region,
                     default = 1, what = "cinp2ginp"))
    c2g <- matrix(c2g, nrow = ny, dimnames = list(NULL, gc))
    unit_cost <- sapply(gc, function(cm) {
      pr <- prices[[cm]]
      cv <- .la_series_key(cell@varom, "comm", cm, "cvarom", years, region,
                           default = 0, what = "cvarom")
      ax <- rep(0, ny)
      ae <- cell@aeff
      if (nrow(ae) > 0 && "cinp2ainp" %in% names(ae)) {
        rows <- ae[!is.na(ae$cinp2ainp) & !is.na(ae$comm) &
                     as.character(ae$comm) == cm, , drop = FALSE]
        for (a in unique(as.character(rows$acomm))) {
          co <- .la_series(rows[as.character(rows$acomm) == a, , drop = FALSE],
                           "cinp2ainp", years, region, default = 0)
          av <- .la_series_key(cell@varom, "acomm", a, "avarom", years, region,
                               default = 0, what = "avarom")
          pa <- if (!is.null(prices[[a]])) prices[[a]] else rep(0, ny)
          ax <- ax + co * (pa + av)
        }
      }
      (if (is.null(pr)) rep(0, ny) else pr) + cv + ax
    })
    unit_cost <- matrix(unit_cost, nrow = ny, dimnames = list(NULL, gc))
    # cost of delivering one unit of group input G at vertex sigma:
    # totinp = 1 / sum(sigma * cinp2ginp); cost = totinp * sum(sigma * unit)
    best <- NULL
    for (i in seq_len(nrow(gverts))) {
      sg   <- gverts[i, ]
      conv <- as.numeric(c2g %*% sg)
      if (any(conv <= 1e-12)) next
      cpg  <- as.numeric(unit_cost %*% sg) / conv
      tot  <- sum(cpg)          # constant-year enforced by blocker with groups
      if (is.null(best) || tot < best$tot - 1e-12)
        best <- list(sigma = sg, conv = conv, tot = tot)
    }
    if (is.null(best))
      .la_unsupported("no feasible share vertex in input group '", g, "'")
    in_alloc[[g]] <- best
  }

  g2u <- lapply(ctx$in_groups, function(g)
    .la_series_key(cell@geff, "group", g, "ginp2use", years, region,
                   default = 1, what = "ginp2use"))
  names(g2u) <- ctx$in_groups

  grouped_in <- unlist(ctx$in_group_comms, use.names = FALSE)
  free_in <- setdiff(ctx$in_comms, grouped_in)
  c2u <- lapply(free_in, function(cm)
    .la_series_key(cell@ceff, "comm", cm, "cinp2use", years, region,
                   default = 1, what = "cinp2use"))
  names(c2u) <- free_in

  # ── evaluate each output choice over the horizon, keep the cheapest ──
  eval_out <- function(out_vec) {
    outm <- matrix(0, ny, length(outs_all), dimnames = list(NULL, outs_all))
    grouped <- ctx$has_grouped_output && length(outs) >= 2
    if (grouped) {
      for (cm in outs) outm[, cm] <- out_vec[[cm]]
      act_dem <- rowSums(outm[, outs, drop = FALSE] / cc[, outs, drop = FALSE])
    } else {
      # ungrouped outputs are joint (eqTechActSng per commodity: out_c =
      # act * cact2cout_c): activity is driven by the largest per-commodity
      # requirement, the joint surplus is freely disposed
      act_dem <- do.call(pmax, lapply(outs, function(cm)
        out_vec[[cm]] / cc[, cm]))
    }

    # capacity requirement and greedy build schedule
    denom <- cap2act * af
    if (any(act_dem > 1e-12 & denom <= 1e-12))
      .la_unsupported("zero availability with positive demand")
    r_des <- ifelse(denom > 0, act_dem / denom, 0)
    cap <- numeric(ny); newcap <- numeric(ny)
    builds <- data.frame(yb = integer(), amt = numeric())
    for (i in seq_len(ny)) {
      y <- years[i]
      alive <- builds$amt[y < builds$yb + olife]
      inst <- sum(alive)
      if (r_des[i] > inst + 1e-12 && y >= ws && y <= we) {
        amt <- r_des[i] - inst
        builds <- rbind(builds, data.frame(yb = y, amt = amt))
        newcap[i] <- newcap[i] + amt
        inst <- r_des[i]
      }
      cap[i] <- inst
    }
    cap_ok <- cap * denom
    act <- pmin(act_dem, cap_ok)
    partial <- act < act_dem - 1e-9 & act > 1e-12
    if (any(partial) && grouped)
      .la_unsupported("partially served year with a grouped output")
    if (grouped) {
      scale <- ifelse(act_dem > 1e-12, act / act_dem, 0)
      for (cm in outs) outm[, cm] <- outm[, cm] * scale
    } else {
      # production, not demand: the joint surplus stays in vTechOut
      for (cm in outs) outm[, cm] <- act * cc[, cm]
    }
    for (cm in byprod) outm[, cm] <- act * cc[, cm]     # disposed surplus

    use_req <- act / u2c

    inpm <- matrix(0, ny, length(ctx$in_comms),
                   dimnames = list(NULL, ctx$in_comms))
    for (cm in free_in) inpm[, cm] <- use_req / c2u[[cm]]
    for (g in ctx$in_groups) {
      G <- use_req / g2u[[g]]
      al <- in_alloc[[g]]
      tot <- G / al$conv
      for (cm in names(al$sigma)) inpm[, cm] <- inpm[, cm] + tot * al$sigma[cm]
    }

    # auxiliary flows (eqTechAInp / eqTechAOut)
    ax_in  <- matrix(0, ny, length(ctx$aux_comms),
                     dimnames = list(NULL, ctx$aux_comms))
    ax_out <- ax_in
    ae <- cell@aeff
    if (nrow(ae) > 0 && length(ctx$aux_comms) > 0) {
      for (a in ctx$aux_comms) {
        for (spec in list(c("act2ainp", "act"), c("cap2ainp", "cap"),
                          c("ncap2ainp", "ncap"), c("act2aout", "act"),
                          c("cap2aout", "cap"), c("ncap2aout", "ncap"))) {
          col <- spec[1]
          if (!col %in% names(ae)) next
          co <- .la_series_key(ae, "acomm", a, col, years, region,
                               default = 0, what = col)
          if (all(co == 0)) next
          base <- switch(spec[2], act = act, ncap = newcap,
                         cap = cap / cap2act)
          tgt <- if (grepl("ainp$", col)) "in" else "out"
          if (tgt == "in") ax_in[, a] <- ax_in[, a] + co * base
          else             ax_out[, a] <- ax_out[, a] + co * base
        }
        for (spec in list(c("cinp2ainp", "in"), c("cinp2aout", "out_from_in"),
                          c("cout2ainp", "in_from_out"), c("cout2aout", "out"))) {
          col <- spec[1]
          if (!col %in% names(ae)) next
          rows <- ae[!is.na(ae[[col]]) & !is.na(ae$acomm) &
                       as.character(ae$acomm) == a, , drop = FALSE]
          for (cm in unique(as.character(rows$comm))) {
            co <- .la_series(rows[as.character(rows$comm) == cm, , drop = FALSE],
                             col, years, region, default = 0)
            flow <- if (grepl("^cinp", col)) {
              if (cm %in% colnames(inpm)) inpm[, cm] else rep(0, ny)
            } else {
              if (cm %in% colnames(outm)) outm[, cm] else rep(0, ny)
            }
            if (grepl("ainp$", col)) ax_in[, a] <- ax_in[, a] + co * flow
            else                     ax_out[, a] <- ax_out[, a] + co * flow
          }
        }
      }
    }

    # ── costs (eqCost terms the mini-model can generate) ──
    eac <- numeric(ny)
    if (nrow(builds) > 0) {
      for (b in seq_len(nrow(builds))) {
        yb <- builds$yb[b]; amt <- builds$amt[b]
        inv <- .la_series(cell@invcost, "invcost", yb, region, default = 0,
                          what = "invcost")
        if (inv == 0) next
        rate <- .la_series(cell@invcost, "wacc", yb, region,
                           default = NA_real_, what = "wacc")
        if (is.na(rate)) rate <- ctx$discount
        pb <- .la_series(cell@invcost, "payback", yb, region,
                         default = NA_real_, what = "payback")
        if (!is.na(pb) && pb == 0) pb <- NA_real_
        life <- if (!is.na(pb)) pb else olife
        ueac <- .la_series(cell@invcost, "eac", yb, region,
                           default = NA_real_, what = "eac")
        unit <- if (!is.na(ueac)) ueac else inv * .crf(rate, life)
        win  <- years >= yb & (is.infinite(life) | years < yb + life)
        eac[win] <- eac[win] + unit * amt
      }
    }
    fixom <- .la_series(cell@fixom, "fixom", years, region, default = 0,
                        what = "fixom") * cap
    varom <- .la_series(cell@varom, "varom", years, region, default = 0,
                        what = "varom") * act
    for (cm in ctx$in_comms)
      varom <- varom + .la_series_key(cell@varom, "comm", cm, "cvarom", years,
                                      region, default = 0) * inpm[, cm]
    for (cm in outs_all)
      varom <- varom + .la_series_key(cell@varom, "comm", cm, "cvarom", years,
                                      region, default = 0) * outm[, cm]
    for (a in ctx$aux_comms)
      varom <- varom + .la_series_key(cell@varom, "acomm", a, "avarom", years,
                                      region, default = 0) *
        (ax_in[, a] + ax_out[, a])

    supply <- rep(0, ny)
    for (cm in ctx$in_comms)
      if (!is.null(prices[[cm]])) supply <- supply + prices[[cm]] * inpm[, cm]
    for (a in ctx$aux_inp_comms)
      if (!is.null(prices[[a]])) supply <- supply + prices[[a]] * ax_in[, a]

    total <- eac + fixom + varom + supply
    # a year is backstop-assisted when any demanded commodity is under-served
    unserved <- rep(FALSE, ny)
    for (cm in outs)
      unserved <- unserved | (outm[, cm] < dv[[cm]] - 1e-9)

    list(years = years, act = act, cap = cap, newcap = newcap,
         out = outm, inp = inpm, ax_in = ax_in, ax_out = ax_out,
         eac = eac, fixom = fixom, varom = varom, supply = supply,
         total = total, unserved = unserved, out_vec = out_vec,
         in_alloc = in_alloc)
  }

  dsc <- (1 + ctx$discount)^(years - as.integer(ctx$base_year))
  best <- NULL
  for (ov in out_choices) {
    s <- eval_out(ov)
    npv <- sum(s$total / dsc)
    if (is.null(best) || npv < best$npv - 1e-12) best <- list(npv = npv, s = s)
  }
  best$s
}

# ── extraction (mirror of levcost_extract_ arithmetic) ─────────────────────────

# Drop exact zeros the way the GLPK csv writers do; return NULL when nothing
# remains, so component presence matches the solve path row for row.
.la_nz <- function(years, values) {
  keep <- abs(values) > 0
  if (!any(keep)) return(NULL)
  data.frame(year = as.integer(years[keep]), value = values[keep])
}

.la_extract <- function(s, ctx, dem_vals, primary_comm = NULL,
                        tech_label = ctx$tech_name,
                        region_label = ctx$region) {
  years <- s$years
  use_act_norm <- is.null(primary_comm) && ctx$has_grouped_output
  comm_lbl <- if (!is.null(primary_comm)) primary_comm else
    if (use_act_norm) "activity" else ctx$comm_label
  group_val <- if (!is.null(ctx$group)) ctx$group else NA_character_
  base_year <- as.integer(ctx$base_year)
  discount  <- ctx$discount

  # own total; a year appears when the model's vTotalCost row would be
  # nonzero -- i.e. own cost nonzero OR backstop active that year
  raw_present <- abs(s$total) > 0 | s$unserved
  if (!any(raw_present)) {
    lc_tbl <- data.frame(tech = tech_label, group = group_val, comm = comm_lbl,
                         region = region_label, year = NA_integer_,
                         levcost = NA_real_, stringsAsFactors = FALSE)
    return(list(levcost = lc_tbl, levcost_npv = NA_real_,
                cost_breakdown = NULL, cost_breakdown_npv = NULL,
                cost_yearly = NULL, levcost_per_act = NULL, scen = NULL))
  }
  tc_df <- data.frame(year = as.integer(years[raw_present]),
                      value = s$total[raw_present])

  out_sum <- rowSums(s$out[, ctx$out_comms, drop = FALSE])
  norm_yr <- NULL
  prim_dem <- 1
  if (use_act_norm) {
    act_ag <- .la_nz(years, s$act)
    if (!is.null(act_ag)) norm_yr <- setNames(act_ag$value,
                                              as.character(act_ag$year))
  } else if (!is.null(primary_comm)) {
    prim_dem <- max(dem_vals[[primary_comm]], 1e-9)
  } else {
    out_ag <- .la_nz(years, out_sum)
    if (!is.null(out_ag) && all(is.finite(out_ag$value)) &&
        all(out_ag$value > 0))
      norm_yr <- setNames(out_ag$value, as.character(out_ag$year))
    else
      prim_dem <- max(sum(unlist(dem_vals)), 1e-9)
  }
  normalise_ <- function(yr_int, val) {
    if (!is.null(norm_yr)) {
      nrm <- norm_yr[as.character(yr_int)]
      ifelse(is.finite(nrm) & nrm > 0, val / nrm, NA_real_)
    } else val / prim_dem
  }

  lc_tbl <- data.frame(tech = tech_label, group = group_val, comm = comm_lbl,
                       region = region_label, year = tc_df$year,
                       levcost = normalise_(tc_df$year, tc_df$value),
                       stringsAsFactors = FALSE)

  yr_int <- tc_df$year
  dsc_tc <- (1 + discount)^(yr_int - base_year)
  npv_num <- sum(ifelse(is.finite(dsc_tc) & dsc_tc > 0,
                        tc_df$value / dsc_tc, 0))
  npv_act_for_act <- NULL
  if (!is.null(norm_yr)) {
    qty_yr <- norm_yr[as.character(yr_int)]
    npv_den <- sum(ifelse(is.finite(dsc_tc) & dsc_tc > 0 & is.finite(qty_yr),
                          qty_yr / dsc_tc, 0))
    npv_act_for_act <- npv_den
  } else if (!is.null(primary_comm)) {
    cact2cout_val <- NA_real_
    ce <- ctx$object@ceff
    if (nrow(ce) > 0 && "cact2cout" %in% names(ce)) {
      r <- ce$cact2cout[ce$comm == primary_comm & !is.na(ce$cact2cout)]
      if (length(r) > 0) cact2cout_val <- as.numeric(r[1])
    }
    act_fr <- .la_nz(years, s$act)
    if (!is.null(act_fr) && is.finite(cact2cout_val) && cact2cout_val > 0) {
      dsc_fr <- (1 + discount)^(act_fr$year - base_year)
      npv_act <- sum(ifelse(is.finite(dsc_fr) & dsc_fr > 0 &
                              is.finite(act_fr$value),
                            act_fr$value / dsc_fr, 0))
      npv_den <- npv_act * cact2cout_val
      npv_act_for_act <- npv_act
    } else {
      npv_den <- prim_dem * sum(ifelse(is.finite(dsc_tc) & dsc_tc > 0,
                                       1 / dsc_tc, 0))
    }
  } else {
    npv_den <- prim_dem * sum(ifelse(is.finite(dsc_tc) & dsc_tc > 0,
                                     1 / dsc_tc, 0))
  }
  lc_npv <- if (is.finite(npv_den) && npv_den > 0) npv_num / npv_den else NA_real_

  cmp_raw <- list(eac    = .la_nz(years, s$eac),
                  fixom  = .la_nz(years, s$fixom),
                  varom  = .la_nz(years, s$varom),
                  supply = .la_nz(years, s$supply))

  cbd <- do.call(rbind, Filter(Negate(is.null), lapply(names(cmp_raw), function(nm) {
    d <- cmp_raw[[nm]]; if (is.null(d)) return(NULL)
    data.frame(tech = tech_label, group = group_val, comm = comm_lbl,
               region = region_label, year = d$year, component = nm,
               value = normalise_(d$year, d$value), stringsAsFactors = FALSE)
  })))
  if (!is.null(cbd)) rownames(cbd) <- NULL

  cost_yearly <- data.frame(tech = tech_label, group = group_val,
                            region = region_label, year = tc_df$year,
                            total = tc_df$value, stringsAsFactors = FALSE)
  for (cnm in names(cmp_raw)) {
    d <- cmp_raw[[cnm]]
    if (!is.null(d) && nrow(d) > 0) {
      dd <- data.frame(year = d$year, v__ = d$value)
      names(dd)[2] <- cnm
      cost_yearly <- merge(cost_yearly, dd, by = "year", all.x = TRUE)
    }
  }
  act_raw <- .la_nz(years, s$act)
  if (!is.null(act_raw))
    cost_yearly <- merge(cost_yearly,
                         data.frame(year = act_raw$year, activity = act_raw$value),
                         by = "year", all.x = TRUE)
  cap_raw <- .la_nz(years, s$cap)
  if (!is.null(cap_raw))
    cost_yearly <- merge(cost_yearly,
                         data.frame(year = cap_raw$year, capacity = cap_raw$value),
                         by = "year", all.x = TRUE)
  for (cm in colnames(s$out)) {
    oa <- .la_nz(years, s$out[, cm])
    if (is.null(oa)) next
    names(oa)[2] <- paste0("out_", cm)
    cost_yearly <- merge(cost_yearly, oa, by = "year", all.x = TRUE)
  }
  for (cm in colnames(s$inp)) {
    ia <- .la_nz(years, s$inp[, cm])
    if (is.null(ia)) next
    names(ia)[2] <- paste0("inp_", cm)
    cost_yearly <- merge(cost_yearly, ia, by = "year", all.x = TRUE)
  }
  cy_front <- c("tech", "group", "region", "year")
  cost_yearly <- cost_yearly[, c(cy_front, setdiff(names(cost_yearly), cy_front)),
                             drop = FALSE]
  rownames(cost_yearly) <- NULL

  cbd_npv <- NULL
  if (is.finite(lc_npv) && is.finite(npv_den) && npv_den > 0) {
    npv_parts <- Filter(Negate(is.null), lapply(names(cmp_raw), function(nm) {
      d <- cmp_raw[[nm]]; if (is.null(d) || nrow(d) == 0) return(NULL)
      dsc_c <- (1 + discount)^(d$year - base_year)
      vld <- is.finite(d$value) & is.finite(dsc_c) & dsc_c > 0
      pv <- if (any(vld)) sum(d$value[vld] / dsc_c[vld]) else NA_real_
      data.frame(tech = tech_label, group = group_val, comm = comm_lbl,
                 component = nm, value = pv / npv_den, stringsAsFactors = FALSE)
    }))
    if (length(npv_parts) > 0) {
      cbd_npv <- do.call(rbind, npv_parts)
      rownames(cbd_npv) <- NULL
    }
  }

  cbd_npv_per_act <- NULL
  if (!is.null(npv_act_for_act) && is.finite(npv_act_for_act) &&
      npv_act_for_act > 0) {
    if (isTRUE(all.equal(npv_act_for_act, npv_den, tolerance = 1e-9))) {
      cbd_npv_per_act <- cbd_npv
    } else {
      pa <- Filter(Negate(is.null), lapply(names(cmp_raw), function(nm) {
        d <- cmp_raw[[nm]]; if (is.null(d) || nrow(d) == 0) return(NULL)
        dsc_c <- (1 + discount)^(d$year - base_year)
        vld <- is.finite(d$value) & is.finite(dsc_c) & dsc_c > 0
        pv <- if (any(vld)) sum(d$value[vld] / dsc_c[vld]) else NA_real_
        data.frame(tech = tech_label, group = group_val, comm = comm_lbl,
                   component = nm, value = pv / npv_act_for_act,
                   stringsAsFactors = FALSE)
      }))
      if (length(pa) > 0) {
        cbd_npv_per_act <- do.call(rbind, pa)
        rownames(cbd_npv_per_act) <- NULL
      }
    }
  }

  lc_act <- NULL
  if (ctx$has_grouped_output) {
    act_ag2 <- if (!is.null(norm_yr))
      data.frame(year = as.integer(names(norm_yr)), value = unname(norm_yr))
    else .la_nz(years, s$act)
    if (!is.null(act_ag2) && nrow(act_ag2) > 0) {
      mrg <- merge(tc_df, act_ag2, by = "year", suffixes = c("_cost", "_act"))
      mrg$lca <- ifelse(is.finite(mrg$value_act) & mrg$value_act != 0,
                        mrg$value_cost / mrg$value_act, NA_real_)
      lc_act <- data.frame(
        tech = tech_label, group = group_val, comm = comm_lbl,
        region = region_label, year = mrg$year, activity = mrg$value_act,
        cost = mrg$value_cost, levcost_per_act = mrg$lca,
        stringsAsFactors = FALSE)
    }
  }

  list(levcost = lc_tbl, levcost_npv = lc_npv,
       cost_breakdown = cbd, cost_breakdown_npv = cbd_npv,
       cost_breakdown_npv_per_act = cbd_npv_per_act,
       cost_yearly = cost_yearly, levcost_per_act = lc_act,
       scen = NULL, series = s)
}

# ── applicability ──────────────────────────────────────────────────────────────

# Cheap static checks; empty result = the analytic path may be attempted.
# Conditions found mid-run raise `levcost_analytic_unsupported` instead.
.levcost_analytic_blockers <- function(ctx) {
  blk <- character(0)
  say <- function(...) blk <<- c(blk, paste0(...))
  if (isTRUE(ctx$as_scenario)) say("as_scenario = TRUE needs a solved scenario")
  if (!identical(ctx$timeframe, "ANNUAL")) say("timeframe = \"native\"")
  if (length(ctx$hor_years) > 1 && any(diff(ctx$hor_years) != 1L))
    say("non-yearly horizon grid")
  slice_ok <- function(x) all(is.na(x) | x == "ANNUAL")
  for (nm in names(ctx$tech_objects)) {
    t <- ctx$tech_objects[[nm]]
    if (isTRUE(t@optimizeRetirement)) say("optimizeRetirement")
    if (nrow(t@weather) > 0) say("unresolved @weather rows")
    ce <- t@ceff
    if (nrow(ce) > 0) {
      for (cc in c("afc.lo", "afc.up", "afc.fx"))
        if (cc %in% names(ce) && any(!is.na(ce[[cc]]))) say("@ceff ", cc)
    }
    for (sl in c("ceff", "aeff", "af", "afs", "varom", "invcost", "fixom")) {
      d <- methods::slot(t, sl)
      if (nrow(d) > 0 && "timeslice" %in% names(d) &&
          !slice_ok(as.character(d$timeslice)))
        say("sub-annual timeslice rows in @", sl)
      if (!ctx$has_variants && nrow(d) > 0) {
        for (k in c("vintage", "cluster"))
          if (k %in% names(d) && any(!is.na(d[[k]])))
            say(k, "-keyed rows in @", sl, " of an unexpanded process")
      }
    }
    for (cc in c("af.lo", "afs.lo")) {
      d <- if (cc == "af.lo") t@af else t@afs
      if (nrow(d) > 0 && cc %in% names(d) && any(!is.na(d[[cc]])))
        say("@af lower bound (", cc, ")")
    }
    inv <- t@invcost
    if (nrow(inv) > 0 && "year" %in% names(inv) &&
        length(unique(inv$year[!is.na(inv$invcost)])) > 1)
      say("year-varying invcost (build-timing choice)")
  }
  # supplies must be pure constant-or-interpolated prices
  for (cm in names(ctx$supply_objects)) {
    sup <- ctx$supply_objects[[cm]]
    if (!isS4(sup)) next
    if (.hasSlot(sup, "weather") && nrow(sup@weather) > 0)
      say("weather-dependent supply '", cm, "'")
    if (.hasSlot(sup, "reserve") && nrow(sup@reserve) > 0)
      say("reserve-constrained supply '", cm, "'")
    if (.hasSlot(sup, "supply") && nrow(sup@supply) > 0) {
      av <- sup@supply
      for (cc in intersect(c("ava.lo", "ava.up", "ava.fx"), names(av)))
        if (any(is.finite(av[[cc]]))) say("availability-bounded supply '", cm, "'")
      if ("timeslice" %in% names(av) &&
          !slice_ok(as.character(av$timeslice)))
        say("sub-annual supply price for '", cm, "'")
    }
  }
  # group substitution: the vertex choice is made once for the whole horizon,
  # which equals the LP only when the ranking cannot flip between years
  has_groups <- length(ctx$in_groups) > 0 ||
    (ctx$has_grouped_output && length(ctx$all_out_comms) >= 2)
  if (has_groups) {
    varies <- function(x) length(unique(round(x, 12))) > 1
    for (cm in names(ctx$prices)) if (varies(ctx$prices[[cm]]))
      say("year-varying price with group substitution ('", cm, "')")
    for (nm in names(ctx$tech_objects)) {
      t <- ctx$tech_objects[[nm]]
      for (sl in c("ceff", "geff", "varom")) {
        d <- methods::slot(t, sl)
        if (nrow(d) > 0 && "year" %in% names(d) && any(!is.na(d$year)))
          say("year-keyed @", sl, " rows with group substitution")
      }
    }
  }
  unique(blk)
}

# ── driver ─────────────────────────────────────────────────────────────────────

.levcost_analytic_run <- function(ctx) {
  base_dvals <- setNames(rep(1, length(ctx$all_out_comms)), ctx$all_out_comms)

  if (ctx$has_variants) {
    per_variant <- list()
    for (nm in names(ctx$tech_objects)) {
      cell <- ctx$tech_objects[[nm]]
      s <- .la_eval(cell, ctx, base_dvals)
      res <- .la_extract(s, ctx, base_dvals[ctx$out_comms], tech_label = nm)
      per_variant[[nm]] <- .levcost_variant_result(
        res, name = nm, object = cell, out_comms = ctx$out_comms,
        discount = ctx$discount, base_year = ctx$base_year, scen = NULL)
      per_variant[[nm]]$method <- "analytic"
    }
    if (length(per_variant) == 0)
      stop("No variant of '", ctx$tech_name, "' produced a levelized cost.",
           call. = FALSE)
    out <- .levcost_variants_object(per_variant, ctx$vprov, ctx$tech_name)
    attr(out, "method") <- "analytic"
    return(out)
  }

  cell <- ctx$tech_objects[[1]]
  s_base <- .la_eval(cell, ctx, base_dvals)
  base_res <- .la_extract(s_base, ctx, base_dvals[ctx$out_comms])

  out_comms <- ctx$out_comms
  frontier_raw <- list()
  if (ctx$do_frontier) {
    for (prim in out_comms) {
      si <- if (is.finite(ctx$share_up[[prim]])) ctx$share_up[[prim]] else
        1 / length(out_comms)
      n_other <- length(out_comms) - 1
      dv_oth <- if (n_other > 0) (1 - si) / n_other else 0
      dem_fr <- setNames(rep(max(dv_oth, 1e-6), length(out_comms)), out_comms)
      dem_fr[[prim]] <- si
      s_fr <- .la_eval(cell, ctx, dem_fr)
      fr_res <- .la_extract(s_fr, ctx, dem_fr, primary_comm = prim)
      frontier_raw[[paste0("max_", prim)]] <- c(fr_res,
        list(primary_comm = prim, primary_input = NA_character_,
             dem_vals = dem_fr))
    }
    if (length(ctx$in_groups) > 0) {
      inp_groups <- Filter(function(g) length(ctx$in_group_comms[[g]]) >= 2,
                           ctx$in_groups)
      ce0 <- ctx$object@ceff
      for (prim_out in out_comms) {
        si_out <- if (is.finite(ctx$share_up[[prim_out]]))
          ctx$share_up[[prim_out]] else 1 / length(out_comms)
        n_oth <- length(out_comms) - 1
        dv_oth <- if (n_oth > 0) (1 - si_out) / n_oth else 0
        dem_ifr <- setNames(rep(max(dv_oth, 1e-6), length(out_comms)), out_comms)
        dem_ifr[[prim_out]] <- si_out
        for (grp in inp_groups) {
          grp_comms <- ctx$in_group_comms[[grp]]
          already_fixed <- character(0)
          if ("share.fx" %in% names(ce0)) {
            for (cm in grp_comms)
              if (any(ce0$comm == cm & !is.na(ce0$share.fx)))
                already_fixed <- c(already_fixed, cm)
          }
          vary_comms <- setdiff(grp_comms, already_fixed)
          if (length(vary_comms) < 2) next
          for (prim_inp in vary_comms) {
            inp_su <- ctx$in_share_up[[grp]][[prim_inp]]
            fx_val <- if (is.finite(inp_su)) inp_su else 1.0
            other_comms <- setdiff(grp_comms, prim_inp)
            other_fx <- if (length(other_comms) > 0)
              (1 - fx_val) / length(other_comms) else 0
            iv <- setNames(rep(other_fx, length(grp_comms)), grp_comms)
            iv[[prim_inp]] <- fx_val
            s_ifr <- .la_eval(cell, ctx, dem_ifr,
                              in_v = setNames(list(iv), grp))
            ifr_res <- .la_extract(s_ifr, ctx, dem_ifr,
                                   primary_comm = prim_out)
            frontier_raw[[paste0("max_", prim_out, "_max_", prim_inp)]] <-
              c(ifr_res, list(primary_comm = prim_out,
                              primary_input = prim_inp, dem_vals = dem_ifr))
            if (ctx$verbose)
              message("Input frontier: max_", prim_out, "_max_", prim_inp)
          }
        }
      }
    }
  }

  # frontier assembly (mirror of levcost.R section 13)
  frontier_df <- NULL
  levcost_by_comm <- NULL
  levcost_by_act <- NULL
  levcost_by_act_cbd <- NULL
  if (length(frontier_raw) > 0) {
    bc <- lapply(names(frontier_raw), function(nm) {
      fr <- frontier_raw[[nm]]
      d <- fr$cost_breakdown_npv
      if (is.null(d)) return(NULL)
      d$scenario <- nm
      d$comm <- fr$primary_comm
      d$primary_input <- fr$primary_input
      d
    })
    levcost_by_comm <- do.call(rbind, Filter(Negate(is.null), bc))
    if (!is.null(levcost_by_comm)) rownames(levcost_by_comm) <- NULL

    act_rows <- lapply(names(frontier_raw), function(nm) {
      fr <- frontier_raw[[nm]]
      lca <- fr$levcost_per_act
      if (is.null(lca) || nrow(lca) == 0) return(NULL)
      dsc <- (1 + ctx$discount)^(lca$year - as.integer(ctx$base_year))
      vc <- is.finite(lca$cost) & is.finite(dsc) & dsc > 0
      va <- is.finite(lca$activity) & is.finite(dsc) & dsc > 0
      npv_cost <- if (any(vc)) sum(lca$cost[vc] / dsc[vc]) else NA_real_
      npv_act <- if (any(va)) sum(lca$activity[va] / dsc[va]) else NA_real_
      data.frame(tech = lca$tech[1], group = lca$group[1], scenario = nm,
                 primary_comm = fr$primary_comm,
                 primary_input = fr$primary_input,
                 npv_act = if (is.finite(npv_act) && npv_act > 0)
                   npv_cost / npv_act else NA_real_,
                 stringsAsFactors = FALSE)
    })
    levcost_by_act <- do.call(rbind, Filter(Negate(is.null), act_rows))
    if (!is.null(levcost_by_act)) rownames(levcost_by_act) <- NULL

    acbd <- lapply(names(frontier_raw), function(nm) {
      fr <- frontier_raw[[nm]]
      d <- fr$cost_breakdown_npv_per_act
      if (is.null(d) || nrow(d) == 0) return(NULL)
      d$scenario <- nm
      d$primary_comm <- fr$primary_comm
      d$primary_input <- fr$primary_input
      d
    })
    levcost_by_act_cbd <- do.call(rbind, Filter(Negate(is.null), acbd))
    if (!is.null(levcost_by_act_cbd)) rownames(levcost_by_act_cbd) <- NULL

    if (length(out_comms) == 2) {
      c1 <- out_comms[1]; c2 <- out_comms[2]
      out_only <- names(frontier_raw)[vapply(frontier_raw, function(fr)
        is.na(fr$primary_input), logical(1))]
      fr_pts <- do.call(rbind, lapply(out_only, function(nm) {
        fr <- frontier_raw[[nm]]
        s <- fr$series
        keep <- abs(s$total) > 0 | s$unserved
        yrs <- s$years[keep]
        if (length(yrs) == 0) return(NULL)
        p1 <- s$out[keep, c1]; p2 <- s$out[keep, c2]
        data.frame(scenario = nm, year = as.integer(yrs),
                   prod_comm1 = ifelse(abs(p1) > 0, p1, NA_real_),
                   prod_comm2 = ifelse(abs(p2) > 0, p2, NA_real_),
                   total_cost = ifelse(abs(s$total[keep]) > 0,
                                       s$total[keep], NA_real_),
                   stringsAsFactors = FALSE)
      }))
      if (!is.null(fr_pts) && nrow(fr_pts) > 0) {
        fr_pts$tech <- ctx$tech_name
        fr_pts$comm1 <- c1
        fr_pts$comm2 <- c2
        fr_pts$share_up_comm1 <- ctx$share_up[[c1]]
        fr_pts$share_up_comm2 <- ctx$share_up[[c2]]
        frontier_df <- fr_pts
        rownames(frontier_df) <- NULL
      }
    }
  }

  # all corner solutions of every share polytope (user request: not just the
  # cost-minimal one); per-activity NPV metrics per vertex
  frontier_vertices <- NULL
  if (isTRUE(ctx$frontier)) {
    vx <- list()
    dsc <- (1 + ctx$discount)^(ctx$hor_years - as.integer(ctx$base_year))
    eval_vertex <- function(direction, shares, out_v = NULL, in_v = NULL) {
      sv <- tryCatch(.la_eval(cell, ctx, base_dvals, out_v = out_v, in_v = in_v),
                     levcost_analytic_unsupported = function(e) NULL)
      if (is.null(sv)) return(NULL)
      npv_act <- sum(sv$act / dsc)
      if (npv_act <= 0) return(NULL)
      comp <- vapply(list(sv$eac, sv$fixom, sv$varom, sv$supply),
                     function(v) sum(v / dsc) / npv_act, numeric(1))
      data.frame(direction = direction, comm = names(shares),
                 share = unname(shares),
                 levcost_npv_act = sum(sv$total / dsc) / npv_act,
                 eac = comp[1], fixom = comp[2], varom = comp[3],
                 supply = comp[4], stringsAsFactors = FALSE)
    }
    if (ctx$has_grouped_output && length(ctx$all_out_comms) >= 2) {
      ov <- .levcost_share_vertices(ctx$share_lo[ctx$all_out_comms],
                                    ctx$share_up[ctx$all_out_comms])
      for (i in seq_len(nrow(ov))) {
        r <- eval_vertex("output", ov[i, ], out_v = ov[i, ])
        if (!is.null(r)) { r$vertex <- paste0("out_v", i); vx[[length(vx) + 1L]] <- r }
      }
    }
    for (g in ctx$in_groups) {
      gc <- ctx$in_group_comms[[g]]
      if (length(gc) < 2) next
      gv <- .levcost_share_vertices(ctx$in_share_lo[[g]][gc],
                                    ctx$in_share_up[[g]][gc])
      for (i in seq_len(nrow(gv))) {
        r <- eval_vertex(paste0("input:", g), gv[i, ],
                         in_v = setNames(list(gv[i, ]), g))
        if (!is.null(r)) {
          r$vertex <- paste0(g, "_v", i)
          vx[[length(vx) + 1L]] <- r
        }
      }
    }
    if (length(vx) > 0) {
      frontier_vertices <- do.call(rbind, vx)
      best <- tapply(frontier_vertices$levcost_npv_act,
                     frontier_vertices$direction, min, na.rm = TRUE)
      frontier_vertices$optimal <-
        abs(frontier_vertices$levcost_npv_act -
              best[frontier_vertices$direction]) < 1e-9
      cf <- c("vertex", "direction", "comm", "share")
      frontier_vertices <- frontier_vertices[
        , c(cf, setdiff(names(frontier_vertices), cf)), drop = FALSE]
      rownames(frontier_vertices) <- NULL
    }
  }

  result <- list(
    levcost            = base_res$levcost,
    levcost_npv        = setNames(base_res$levcost_npv, ctx$tech_name),
    cost_breakdown     = base_res$cost_breakdown,
    cost_breakdown_npv = base_res$cost_breakdown_npv,
    levcost_per_act    = base_res$levcost_per_act,
    cost_yearly        = base_res$cost_yearly,
    frontier           = frontier_df,
    levcost_by_comm    = levcost_by_comm,
    levcost_by_act     = levcost_by_act,
    levcost_by_act_cbd = levcost_by_act_cbd,
    input_frontier     = tech_share_frontier(ctx$object),
    frontier_vertices  = frontier_vertices,
    discount           = ctx$discount,
    base_year          = as.integer(ctx$base_year),
    units              = .levcost_tech_units(ctx$object, ctx$out_comms),
    scenario           = NULL,
    method             = "analytic"
  )
  class(result) <- c("levcost", "list")
  result
}
