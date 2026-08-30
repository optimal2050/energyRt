# =============================================================================
# levcost() for `trade` -- levelized cost of transmission (LCOT)
# =============================================================================
# A corridor moves a commodity between two regions and loses `teff` on the way.
# Its levelized cost is per unit ARRIVING, so it is comparable with a
# technology's LCOE and with LCOS.
#
# Two things about `trade` shape the arithmetic, and both are easy to get wrong:
#
# 1. Capacity is corridor-wide, costs are region-indexed. `vTradeNewCap` has no
#    region index while `eqTradeEac` / `eqTradeFixom` do
#    (glpk/energyRt.mod:964-966), so BOTH endpoints pay their own rate on the
#    WHOLE capacity -- "a corridor whose two endpoints each pay 100 therefore
#    costs 200" (data-raw/classes.yml:1879-1890). The numerator sums over
#    endpoints; picking one would halve the answer.
#
# 2. `@varom` is inert. `pTradeVarom` was removed from the model because no
#    equation read it, it was absent from Julia and Pyomo, and it is
#    unregistered in modInp.yml (glpk/energyRt.mod:535). Pricing it here would
#    make the analytic engine disagree with every solver, so it is excluded --
#    and warned about, because silently dropping a cost the user typed is worse
#    than refusing it.
#
# On a unit-capacity basis:
#
#   sent      = cap2act * utilisation      (utilisation is `@trade$af.up`)
#   delivered = sent * teff
#   LCOT      = sum_endpoints(eac + fixom) / delivered
#
# Note `cap2act` on a trade is a SCALAR defaulting to 1, not the 8760 that a
# storage part carries -- with the default, capacity and annual activity are
# the same number and `utilisation` is the whole of the denominator.
# =============================================================================

#' The two regions a corridor's costs are charged in
#' @noRd
.lct_endpoints <- function(object) {
  r <- object@routes
  if (NROW(r) == 0) return(character(0))
  unique(c(as.character(r$src), as.character(r$dst)))
}

#' One direction of the corridor: the route being priced
#' @noRd
.lct_route <- function(object, route = NULL) {
  r <- object@routes
  if (NROW(r) == 0)
    stop("trade ", object@name, " declares no routes.", call. = FALSE)
  if (is.null(route))
    return(list(src = as.character(r$src)[1], dst = as.character(r$dst)[1]))
  if (length(route) != 2)
    stop("`route` must be c(src, dst).", call. = FALSE)
  hit <- as.character(r$src) == route[1] & as.character(r$dst) == route[2]
  if (!any(hit))
    stop("route ", route[1], " -> ", route[2],
         " is not among the trade's routes.", call. = FALSE)
  list(src = route[1], dst = route[2])
}

#' A src/dst-indexed series for one direction (`teff`, `af.up`)
#' @noRd
.lct_dir_series <- function(df, val, years, src, dst, default) {
  if (NROW(df) == 0 || !val %in% names(df)) return(rep(default, length(years)))
  d <- df[!is.na(df[[val]]), , drop = FALSE]
  if (NROW(d) == 0) return(rep(default, length(years)))
  if (all(c("src", "dst") %in% names(d))) {
    keep <- (is.na(d$src) | as.character(d$src) == src) &
            (is.na(d$dst) | as.character(d$dst) == dst)
    d <- d[keep, , drop = FALSE]
  }
  if (NROW(d) == 0) return(rep(default, length(years)))
  v <- unique(as.numeric(d[[val]]))
  if (length(v) > 1)
    .la_unsupported(val, " varies within the priced direction; ",
                    "levcost needs one value")
  rep(v, length(years))
}

#' Closed-form levelized cost of a corridor
#' @noRd
.levcost_trade_analytic <- function(ctx) {
  object <- ctx$object
  years  <- ctx$hor_years
  ny     <- length(years)
  ends   <- ctx$endpoints

  life  <- .la_lifespan(object, ends[1])
  olife <- if (is.na(life$olife)) Inf else life$olife

  eac <- rep(0, ny); fixom <- rep(0, ny)
  for (rg in ends) {
    rate <- .la_series(object@invcost, "wacc", years, rg, default = NA_real_,
                       what = "wacc")
    if (all(is.na(rate))) rate <- rep(ctx$discount, ny)
    pb <- .la_series(object@invcost, "payback", years, rg, default = NA_real_,
                     what = "payback")
    span <- if (all(!is.na(pb)) && all(pb > 0)) pb[1] else olife
    inv <- .la_series(object@invcost, "invcost", years, rg, default = 0,
                      what = "invcost")
    ue  <- .la_series(object@invcost, "eac", years, rg, default = NA_real_,
                      what = "eac")
    eac   <- eac + ifelse(!is.na(ue), ue, inv * .crf(rate, rep(span, ny)))
    fixom <- fixom + .la_series(object@fixom, "fixom", years, rg, default = 0,
                                what = "fixom")
  }

  delivered <- rep(ctx$cap2act * ctx$utilisation * ctx$teff, ny)

  list(years = years, total = eac + fixom, annual_out = delivered,
       components = list(eac = eac, fixom = fixom))
}

#' Build and solve the corridor mini-model
#'
#' Two regions, a supply in the sending one and the demand in the receiving one,
#' and the corridor as the only link between them.
#'
#' The demand is what ONE unit of capacity delivers in a year, not one unit, so
#' the model builds `vTradeCap == 1` and its reported `vTradeEac` / `vTradeFixom`
#' ARE the per-capacity rates. A unit demand would size the corridor at
#' `1 / (cap2act * utilisation * teff)` -- for a `cap2act` of 8760 that is 2e-4,
#' which GLPK writes with about three significant digits, and dividing the costs
#' by it turned a 1e-6 write error into a 1e-3 error in the answer.
#' @noRd
.levcost_trade_solve <- function(ctx) {
  object <- ctx$object
  years  <- ctx$hor_years
  src <- ctx$src; dst <- ctx$dst
  cm  <- ctx$comm_moved

  # pin the priced direction and its utilisation, so the solver is answering
  # the same question the closed form is
  tr <- object@trade
  if (NROW(tr) > 0) {
    keep <- (is.na(tr$src) | as.character(tr$src) == src) &
            (is.na(tr$dst) | as.character(tr$dst) == dst)
    tr <- tr[keep, , drop = FALSE]
  }
  if (NROW(tr) == 0)
    tr <- object@trade[NA_integer_, , drop = FALSE]
  tr <- tr[1, , drop = FALSE]
  tr$src <- src; tr$dst <- dst
  tr$af.up <- ctx$utilisation
  tr$teff  <- ctx$teff
  for (cc in c("ava.up", "ava.lo", "ava.fx", "af.lo", "af.fx"))
    if (cc %in% names(tr)) tr[[cc]] <- NA_real_
  object@trade  <- tr
  object@routes <- data.frame(src = src, dst = dst, stringsAsFactors = FALSE)

  # the supply must exist in the SENDING region only: pinned by the @region
  # slot, not just by the row's region column. Left unpinned it broadcasts to
  # every model region, the demand is met locally, and the corridor -- the
  # thing being priced -- is never built at all (objective 0).
  sup <- newSupply(
    name = ".LCT_SUP", commodity = cm, region = src,
    supply = data.frame(region = src, ava.up = 1e6, cost = ctx$price[1],
                        stringsAsFactors = FALSE))
  unit_del <- ctx$cap2act * ctx$utilisation * ctx$teff
  dem <- newDemand(
    name = ".LCT_DEM", commodity = cm, region = dst,
    demand = data.frame(region = dst, year = years, demand = unit_del,
                        stringsAsFactors = FALSE))
  repo <- newRepository(name = ".LCT_REPO", newCommodity(name = cm, timeframe = "ANNUAL"),
                        sup, dem, object)
  mdl <- newModel(name = ".LCT", region = c(src, dst), discount = ctx$discount,
                  horizon = ctx$horizon, data = repo)
  sc <- solve(mdl, name = ".LCT_RUN", solver = ctx$solver, transient = TRUE,
              path = .levcost_scratch_path(".LCT_RUN"), verbose = FALSE)
  if (!isTRUE(sc@status$optimal))
    stop("levcost(): the trade mini-model did not solve to optimality.",
         call. = FALSE)
  sc
}

#' Read the corridor mini-model back on the analytic engine's unit basis
#'
#' The mini-model sized itself to deliver one unit; the closed form works per
#' unit of CAPACITY. Dividing the solved costs by `vTradeCap` puts the two on
#' the same basis, so the comparison is of rates and not of sizes.
#' @noRd
.levcost_trade_extract <- function(sc, ctx) {
  years <- ctx$hor_years
  get <- function(v) tryCatch({
    d <- getData(sc, name = v, merge = TRUE, drop.zeros = FALSE,
                 timeframe = "lowest")
    if (is.null(d) || nrow(d) == 0) return(NULL)
    as.data.frame(d)
  }, error = function(e) NULL)
  by_year <- function(d) {
    if (is.null(d) || !"year" %in% names(d))
      return(setNames(rep(0, length(years)), years))
    a <- tapply(d$value, as.integer(d$year), sum, na.rm = TRUE)
    v <- as.numeric(a[as.character(years)]); v[is.na(v)] <- 0
    setNames(v, years)
  }
  # the mini-model was sized so that vTradeCap == 1; costs are already rates
  cap <- by_year(get("vTradeCap"))
  if (any(abs(cap - 1) > 1e-3))
    warning("levcost(): the corridor mini-model built ",
            paste(signif(unname(cap), 4), collapse = ", "),
            " units of capacity where 1 was expected; the LCOT is scaled by ",
            "it, but something else is binding.", call. = FALSE)
  norm <- ifelse(cap > 0, 1 / cap, NA_real_)
  eac   <- unname(by_year(get("vTradeEac"))   * norm)
  fixom <- unname(by_year(get("vTradeFixom")) * norm)

  list(years = years, total = eac + fixom,
       annual_out = rep(ctx$cap2act * ctx$utilisation * ctx$teff,
                        length(years)),
       components = list(eac = eac, fixom = fixom), scen = sc)
}

# =============================================================================
# Driver
# =============================================================================

#' Levelized cost of transmission
#'
#' Called through [levcost()] on a `trade` object. The metric is cost per unit
#' ARRIVING in the receiving region, summed over both endpoints' capital and
#' fixed charges -- see the header of this file for why both endpoints pay.
#'
#' `@varom` and `@markup` are NOT priced: no equation in any backend reads them
#' (glpk/energyRt.mod:535). A trade carrying them gets a warning rather than a
#' number that no solver would reproduce.
#'
#' @param object A `trade` object.
#' @param comm Label for the priced commodity. Defaults to `@commodity`.
#' @param route The direction to price, as `c(src, dst)`. Defaults to the first
#'   row of `@routes`. Capacity is shared by both directions, so the full
#'   corridor capex is charged to the direction being priced.
#' @param utilisation Fraction of the corridor's capacity actually used, over
#'   the year. Defaults to `@trade$af.up` for the priced direction, then `1`.
#' @param price Price of the commodity in the sending region, per unit sent.
#'   Defaults to `0`, which makes the result the pure wheeling cost -- the
#'   conventional reading of a transmission tariff, and deliberately unlike
#'   [levcost_storage_()], where the charging price is taken from the container
#'   because round-trip loss is intrinsic to what a store costs. Set it (or
#'   `fuel_costs`) to get the delivered cost instead.
#' @param repo Accepted for signature compatibility with the container path and
#'   otherwise unused: a corridor moves its own commodity rather than consuming
#'   a different one, so there is nothing in a repository for it to be priced
#'   against. Pass `price` or `fuel_costs` to charge for the energy sent.
#' @param fuel_costs,discount,base_year,horizon,solver,method,verbose As in
#'   [levcost()] for a technology.
#' @param ... Ignored, for signature compatibility with the technology method.
#' @return A `levcost` object -- the same shape the technology method returns.
#' @export
levcost_trade_ <- function(
    object,
    comm        = NULL,
    route       = NULL,
    utilisation = NULL,
    price       = 0,
    repo        = NULL,
    fuel_costs  = NULL,
    discount    = 0.05,
    base_year   = NULL,
    horizon     = NULL,
    solver      = solver_options$glpk,
    method      = c("auto", "analytic", "solve"),
    verbose     = TRUE,
    ...
) {
  method <- match.arg(method)
  if (!inherits(object, "trade"))
    stop("`object` must be an energyRt 'trade' object.", call. = FALSE)

  name <- if (nzchar(object@name)) object@name else "TRADE"
  cm   <- as.character(object@commodity)[1]
  if (is.na(cm) || !nzchar(cm))
    stop("trade ", name, " declares no commodity.", call. = FALSE)
  if (is.null(comm)) comm <- cm

  # `@varom` is dead in every backend; refusing to price it is the only answer
  # that both engines and every solver agree on
  vo <- object@varom
  if (NROW(vo) > 0) {
    live <- intersect(c("varom", "markup"), names(vo))
    if (any(vapply(live, function(k) any(!is.na(vo[[k]])), logical(1))))
      warning("levcost(): trade ", name, " sets @varom/@markup, which no ",
              "backend reads (see glpk/energyRt.mod:535). It is excluded ",
              "from the levelized cost.", call. = FALSE)
  }

  rt   <- .lct_route(object, route)
  ends <- .lct_endpoints(object)

  if (is.null(horizon))
    horizon <- .levcost_auto_horizon(object, base_year = base_year,
                                     verbose = verbose)
  hor_years <- as.integer(horizon@period)
  if (is.null(base_year)) base_year <- min(hor_years)

  # ── vintage / cluster variants ────────────────────────────────────────────
  # A trade cluster is a LOSS TRANCHE (see R/variants.R:35), so a fan-out here
  # prices each tranche separately -- which is the whole point of declaring
  # them. `.levcost_devintage()` blanks only `vintage`/`cluster`, never the
  # `src`/`dst` of `@routes`. The route is passed on explicitly because a cell
  # must be priced in the same direction as its parent, and the horizon is
  # resolved from the BASE object above so every cell is discounted on the same
  # span and base year -- otherwise the fan-out compares vintages priced on
  # different horizons, which is not a comparison at all.
  vfan <- .tech_variants(object)
  if (!is.null(vfan)) {
    if (verbose)
      message("levcost(): '", name, "' has ", length(vfan$objects),
              " vintage/cluster cells; pricing each.")
    cells <- lapply(lapply(vfan$objects, .levcost_devintage, region = ends[1]),
                    .levcost_unwindow)
    names(cells) <- vapply(cells, function(o) o@name, character(1))
    per <- lapply(cells, function(o)
      levcost_trade_(o, comm = comm, route = c(rt$src, rt$dst),
                     utilisation = utilisation, price = price, repo = repo,
                     fuel_costs = fuel_costs, discount = discount,
                     base_year = base_year, horizon = horizon,
                     solver = solver, method = method, verbose = FALSE))
    return(.levcost_variants_object(per, vfan$prov, name))
  }

  object <- .levcost_strip_capacity(object, verbose = verbose,
                                    what = "LCOT mini-model")


  teff <- .lct_dir_series(object@trade, "teff", hor_years, rt$src, rt$dst,
                          default = 1)[1]
  if (!is.finite(teff) || teff <= 0 || teff > 1)
    stop("`teff` for ", rt$src, " -> ", rt$dst, " must be in (0, 1].",
         call. = FALSE)

  if (is.null(utilisation))
    utilisation <- .lct_dir_series(object@trade, "af.up", hor_years,
                                   rt$src, rt$dst, default = 1)[1]
  if (!is.finite(utilisation) || utilisation <= 0 || utilisation > 1)
    stop("`utilisation` must be in (0, 1].", call. = FALSE)

  cap2act <- if (length(object@cap2act) && is.finite(object@cap2act[1]))
    object@cap2act[1] else 1

  if (is.null(price) || !length(price)) price <- 0
  if (!is.null(fuel_costs) && cm %in% names(fuel_costs) && identical(price, 0))
    price <- as.numeric(fuel_costs[[cm]])

  ctx <- list(object = object, hor_years = hor_years, discount = discount,
              endpoints = ends, src = rt$src, dst = rt$dst, comm_moved = cm,
              teff = teff, utilisation = utilisation, cap2act = cap2act,
              price = price, horizon = horizon, solver = solver)

  run_solve <- function() .levcost_trade_extract(.levcost_trade_solve(ctx), ctx)
  res <- switch(
    method,
    analytic = .levcost_trade_analytic(ctx),
    solve    = run_solve(),
    auto     = tryCatch(.levcost_trade_analytic(ctx),
                        levcost_analytic_unsupported = function(e) {
                          if (verbose)
                            message("levcost(): ", conditionMessage(e),
                                    " -- falling back to the solver.")
                          run_solve()
                        }))

  .levcost_assemble(res, name = name, comm = comm,
                    region = paste(rt$src, rt$dst, sep = "->"),
                    discount = discount, base_year = base_year,
                    units = NULL, scen = res$scen)
}
