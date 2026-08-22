# =========================================================================== #
# Shared fixtures for the trade suite, and for `energyRt-book/trade.qmd`.
#
# A trade object's identity is a PAIR of regions, and almost everything that
# surprises people follows from that:
#
#   * capacity is region-free -- one `vTradeCap{trade, year}` per object, shared
#     by every route on it, and by both directions of a two-way corridor;
#   * costs are region-INDEXED but are a rate per ENDPOINT, not a corridor total;
#   * `@varom` is the exception: per ROUTE, so it can differ by direction. Its
#     `varom` column is a REAL cost borne at `src`; its `markup` column is a
#     transfer that leaves the objective alone;
#   * flows have two kinds of bound: `ava.*` is ABSOLUTE, in commodity units,
#     and `af.*` is RELATIVE, a fraction of the object's own capacity.
#
# These builders are deliberately tiny and their objectives are exactly
# readable, so every number quoted in the chapter is one an assertion pins.
# =========================================================================== #

tr_cal <- function() newCalendar(
  timetable = make_timetable(struct = list(ANNUAL = "ANNUAL")), name = "tr_cal")

# One year, one slice: the objective is then a plain sum with no period or
# timeslice weighting to unpick.
tr_hor <- function() newHorizon(2020, intervals = 1)

# A corridor carrying ELC from R1 to R2 (and back, if `routes` says so).
#
# Supply is PRICED by region rather than restricted to one -- a region-restricted
# `newSupply` used to break `eqSupOutTot` in the Julia and Pyomo emitters, and
# pricing keeps the trade route competing on price rather than availability.
# Capacity is PINNED with `cap.fx`, not merely given as `stock`. With `stock`
# alone and no `invcost`, building more capacity is FREE, so the corridor simply
# expands to whatever the flow wants and the rating never binds -- which is easy
# to mistake for a broken capacity constraint. `cap.fx` removes the investment
# decision so a test about flow limits is about flow limits.
tr_model <- function(routes,
                     cap     = 100,
                     trade   = NULL,   # ava.* / teff, per route
                     varom   = NULL,   # varom / markup, per route
                     demand  = data.frame(region = "R2", demand = 10),
                     supply  = data.frame(region = c("R1", "R2"), cost = c(1, 100)),
                     regions = c("R1", "R2"),
                     name    = "tr") {
  TRD <- newTrade(
    "TRD", commodity = "ELC", cap2act = 1,
    routes = routes,
    capacity = data.frame(cap.fx = cap),
    vintage = data.frame(olife = 50L),
    trade = if (is.null(trade)) data.frame() else trade,
    varom = if (is.null(varom)) data.frame() else varom)
  newModel(
    name = name, region = regions, horizon = tr_hor(), discount = 0,
    calendar = tr_cal(),
    repo = newRepository("tr_repo",
      newCommodity("ELC", timeframe = "ANNUAL"),
      newSupply("SUP", commodity = "ELC", supply = supply),
      TRD,
      newDemand("DEM", commodity = "ELC", demand = demand)))
}

tr_solve <- function(mod) suppressMessages(
  solve_scen(interpolate_model(mod, name = mod@name)))

tr_obj <- function(mod) getData(tr_solve(mod), "vObjective", merge = TRUE)$value[1]

# Non-zero flows as a named vector keyed "src->dst", so an assertion reads as
# the physical statement it is.
tr_flows <- function(sol) {
  d <- as.data.frame(getData(sol, "vTradeIr", merge = TRUE))
  d <- d[d$value != 0, ]
  if (!nrow(d)) return(setNames(numeric(0), character(0)))
  setNames(round(d$value, 6), paste0(d$src, "->", d$dst))
}

TR_ONE_WAY <- data.frame(src = "R1", dst = "R2")
TR_TWO_WAY <- data.frame(src = c("R1", "R2"), dst = c("R2", "R1"))

# An infeasible solve does NOT error: `solve_scen()` returns a scenario whose
# results are empty (the solver writes no output files and the reader gives up).
# So infeasibility is detected by absence, not by a condition.
tr_infeasible <- function(mod) {
  s <- suppressWarnings(try(tr_solve(mod), silent = TRUE))
  if (inherits(s, "try-error")) return(TRUE)
  !length(suppressWarnings(getData(s, "vObjective", merge = TRUE)$value))
}

# Regional cost allocation. `@varom$markup` shows up here and nowhere else: it
# is a transfer, so it moves these and leaves the objective alone. `$varom` is a
# real cost and moves both.
tr_regional_cost <- function(sol, regions = c("R1", "R2")) {
  d <- as.data.frame(getData(sol, "vTotalCost", merge = TRUE))
  # A region whose cost is zero has no row at all, so fill the full set rather
  # than subtracting NA out of a partial one.
  out <- setNames(rep(0, length(regions)), regions)
  if (nrow(d)) out[as.character(d$region)] <- round(d$value, 6)
  out
}

# =========================================================================== #
# Multi-region fixtures.
#
# A whole network fits in one trade object -- `@routes` is a table, not a pair.
# The catch is that ONE object has ONE `vTradeCap`, and `eqTradeCapFlow` sums
# EVERY route against it. So a mesh in one object is not a network of lines with
# independent ratings; it is a single shared throughput budget that every leg
# draws on. Routing through an intermediate region therefore costs the budget
# TWICE per unit delivered, once on each hop.
#
# `tr_mesh_one()` and `tr_mesh_many()` are the same topology under the two
# packagings, so the tests can show exactly where the answers part company.
# =========================================================================== #

TR_MESH <- data.frame(src = c("R1", "R2", "R1"),
                      dst = c("R2", "R3", "R3"))

# Demand sits at R3, the only region with no cheap supply of its own. R1 is
# cheapest, so the interesting question is always how much of R3's demand can
# reach it and by which path.
.mesh_supply <- data.frame(region = c("R1", "R2", "R3"), cost = c(1, 5, 100))
.mesh_demand <- data.frame(region = "R3", demand = 10)

.mesh_repo <- function(trades) do.call(newRepository, c(
  list("mesh_repo",
       newCommodity("ELC", timeframe = "ANNUAL"),
       newSupply("SUP", commodity = "ELC", supply = .mesh_supply),
       newDemand("DEM", commodity = "ELC", demand = .mesh_demand)),
  trades))

.mesh_model <- function(trades, name) newModel(
  name = name, region = c("R1", "R2", "R3"), horizon = tr_hor(), discount = 0,
  calendar = tr_cal(), repo = .mesh_repo(trades))

# The whole mesh as ONE object: three routes, one shared capacity.
tr_mesh_one <- function(cap = 10, trade = NULL, name = "mesh1") .mesh_model(
  list(newTrade("NET", commodity = "ELC", cap2act = 1, routes = TR_MESH,
                capacity = data.frame(cap.fx = cap),
                vintage = data.frame(olife = 50L),
                trade = if (is.null(trade)) data.frame() else trade)),
  name)

# The same mesh as THREE objects, one route each: three independent capacities.
# `trade` is applied to whichever object carries the matching route, so the same
# frame drives both builders and the comparison is like for like.
tr_mesh_many <- function(cap = 10, trade = NULL, name = "mesh3") {
  objs <- lapply(seq_len(nrow(TR_MESH)), function(i) {
    rt <- TR_MESH[i, , drop = FALSE]
    tp <- data.frame()
    if (!is.null(trade)) {
      keep <- (is.na(trade$src) | trade$src == rt$src) &
              (is.na(trade$dst) | trade$dst == rt$dst)
      if (any(keep)) tp <- trade[keep, , drop = FALSE]
    }
    newTrade(paste0("L_", rt$src, rt$dst), commodity = "ELC", cap2act = 1,
             routes = rt, capacity = data.frame(cap.fx = cap),
             vintage = data.frame(olife = 50L), trade = tp)
  })
  .mesh_model(objs, name)
}

# Total flow on every leg, keyed "src->dst", summed across objects so the two
# packagings are directly comparable.
tr_mesh_flows <- function(sol) {
  d <- as.data.frame(getData(sol, "vTradeIr", merge = TRUE))
  d <- d[d$value > 1e-9, ]
  if (!nrow(d)) return(setNames(numeric(0), character(0)))
  k <- paste0(d$src, "->", d$dst)
  v <- tapply(d$value, k, sum)
  setNames(round(as.numeric(v), 6), names(v))
}
