# =============================================================================
# levcost() for `storage` -- levelized cost of storage (LCOS)
# =============================================================================
# A store creates no energy: it buys at one time, sells at another, and loses
# `inpeff * outeff` on the way. So its levelized cost is per unit DISCHARGED,
# which makes it directly comparable with a technology's LCOE, and it depends on
# something no slot carries -- how often the store cycles. `cycles` supplies it.
#
# The unit basis is one unit delivered per cycle. Everything else follows:
#
#   level drawn per cycle = 1 / outeff        (vStorageOut is already post-outeff)
#   stgCap                = 1 / outeff        (the peak level)
#   charged per cycle     = 1 / (outeff * inpeff)
#   outCap                = stgCap / duration (energy-to-power ratio)
#   inpCap                = outCap * inp2out
#
#   LCOS = ( sum_part eac_part * cap_part
#          + sum_part fixom_part * cap_part
#          + cycles * (inpcost*charged + outcost*1 + stgcost*stgCap)
#          + cycles * price * charged )              <- the charging term
#          / (cycles * 1)
#
# Both engines are implemented and must agree: `.levcost_storage_analytic()`
# evaluates the closed form above, `.levcost_storage_solve()` builds a
# representative-CYCLE mini-model (see below) and reads the solution back. The
# parity test in test-levcost-storage.R is the guard that keeps them honest.
#
# The mini-model is one cycle, weighted up to a year, rather than 8760
# timeslices: a calendar of two timeslices (CHG, DIS) with
# `year_fraction = 1/cycles` gives `weight = cycles` and `share * weight == 1`,
# so one modelled cycle IS a year. `fullYear = FALSE` closes the cycle inside
# the representative period. Note `year_fraction` must be passed to BOTH
# `make_timetable()` and `newCalendar()` -- the latter validates the shares
# against its own default of 1 and errors otherwise.
#
# Flows in the solution are per-timeslice while costs are annualised by the
# timeslice weight, so annual delivered energy is `vStorageOut * weight`.
# =============================================================================

#' Scalar value of a storage ratio slot (`duration`, `inp2out`)
#'
#' A levelized cost needs a definite sizing, so a genuine range is refused: the
#' model would pick a point on it, and which point depends on the rest of the
#' system, which is exactly what a single-process metric cannot see.
#' @noRd
.lcs_ratio <- function(df, base, years, region, default = 1) {
  fx <- .la_series(df, paste0(base, ".fx"), years, region, default = NA_real_,
                   what = paste0(base, ".fx"))
  if (all(!is.na(fx))) return(fx)
  bare <- .la_series(df, base, years, region, default = NA_real_, what = base)
  if (all(!is.na(bare))) return(bare)
  lo <- .la_series(df, paste0(base, ".lo"), years, region, default = NA_real_)
  up <- .la_series(df, paste0(base, ".up"), years, region, default = NA_real_)
  if (all(!is.na(lo)) && all(!is.na(up))) {
    if (isTRUE(all.equal(lo, up))) return(lo)
    .la_unsupported(base, " is a range (", base, ".lo != ", base,
                    ".up); levcost needs a fixed ratio")
  }
  if (all(!is.na(lo)) || all(!is.na(up))) {
    .la_unsupported("one-sided ", base, " bound; levcost needs a fixed ratio")
  }
  rep(as.numeric(default), length(years))
}

#' Does this storage have a capacity variable for the given part?
#'
#' Structure follows data: a part carrying nothing but a commodity gets no
#' capacity variable, so its `invcost`/`fixom` never enter the objective. The
#' analytic path must mirror that or it would price capacity the solver never
#' builds. `out` always exists.
#' @noRd
.lcs_has_part <- function(object, part) {
  if (identical(part, "out")) return(TRUE)
  cost_cols <- paste0(part, c(".invcost", ".fixom"))
  for (sl in c("invcost", "fixom")) {
    d <- methods::slot(object, sl)
    for (cc in intersect(cost_cols, names(d)))
      if (nrow(d) > 0 && any(!is.na(d[[cc]]))) return(TRUE)
  }
  cap <- object@capacity
  pref <- paste0(part, ".")
  cc <- grep(paste0("^", pref), names(cap), value = TRUE)
  if (nrow(cap) > 0 && length(cc) > 0 &&
      any(vapply(cc, function(k) any(!is.na(cap[[k]])), logical(1)))) return(TRUE)
  # A declared part slot with more than a bare DECLARATION also materialises it.
  # `comm`, `unit` and `cap2act` are declaration: `cap2act` is populated on
  # every part by the constructor, so counting it would make every part look
  # priced and defeat the check.
  sl <- switch(part, inp = "input", stg = "storage")
  d <- methods::slot(object, sl)
  if (nrow(d) > 0) {
    extra <- setdiff(names(d), c("comm", "unit", "cap2act"))
    if (any(vapply(extra, function(k) any(!is.na(d[[k]])), logical(1))))
      return(TRUE)
  }
  FALSE
}

#' Closed-form levelized cost of a storage
#' @noRd
.levcost_storage_analytic <- function(ctx) {
  object <- ctx$object
  years  <- ctx$hor_years
  region <- ctx$region
  ny     <- length(years)

  eff <- function(k) {
    v <- .la_series(object@seff, k, years, region, default = 1, what = k)
    if (any(!is.finite(v)) || any(v <= 0 | v > 1))
      .la_unsupported("@seff$", k, " outside (0, 1]")
    v
  }
  inpeff <- eff("inpeff")
  outeff <- eff("outeff")

  duration <- .lcs_ratio(object@duration, "duration", years, region, 1)
  inp2out  <- .lcs_ratio(object@inp2out,  "inp2out",  years, region, 1)
  if (any(duration <= 0)) .la_unsupported("duration must be positive")

  # unit basis: one unit delivered per cycle
  delivered <- rep(1, ny)
  stgCap    <- delivered / outeff
  charged   <- delivered / (outeff * inpeff)
  outCap    <- stgCap / duration
  inpCap    <- outCap * inp2out

  caps <- list(out = outCap, stg = stgCap, inp = inpCap)
  for (p in c("stg", "inp"))
    if (!.lcs_has_part(object, p)) caps[[p]] <- rep(0, ny)

  life <- .la_lifespan(object, region)
  olife <- if (is.na(life$olife)) Inf else life$olife

  # annuity, per part, on that part's own capacity. All three share one
  # lifetime and one wacc/payback -- see the note in R/eac.R:251-253; the
  # per-part wacc/payback columns exist but the solver does not read them.
  rate <- .la_series(object@invcost, "out.wacc", years, region,
                     default = NA_real_, what = "out.wacc")
  if (all(is.na(rate))) rate <- rep(ctx$discount, ny)
  pb <- .la_series(object@invcost, "out.payback", years, region,
                   default = NA_real_, what = "out.payback")
  span <- if (all(!is.na(pb)) && all(pb > 0)) pb[1] else olife

  eac <- rep(0, ny)
  eac_parts <- list()
  for (p in c("out", "stg", "inp")) {
    inv <- .la_series(object@invcost, paste0(p, ".invcost"), years, region,
                      default = 0, what = paste0(p, ".invcost"))
    ue <- .la_series(object@invcost, paste0(p, ".eac"), years, region,
                     default = NA_real_, what = paste0(p, ".eac"))
    unit <- ifelse(!is.na(ue), ue, inv * .crf(rate, rep(span, ny)))
    eac_parts[[p]] <- unit * caps[[p]]
    eac <- eac + eac_parts[[p]]
  }

  fixom <- rep(0, ny)
  for (p in c("out", "stg", "inp")) {
    fx <- .la_series(object@fixom, paste0(p, ".fixom"), years, region,
                     default = 0, what = paste0(p, ".fixom"))
    fixom <- fixom + fx * caps[[p]]
  }

  # varom is charged per unit flowed / held, once per cycle
  inpcost <- .la_series(object@varom, "inpcost", years, region, default = 0,
                        what = "inpcost")
  outcost <- .la_series(object@varom, "outcost", years, region, default = 0,
                        what = "outcost")
  stgcost <- .la_series(object@varom, "stgcost", years, region, default = 0,
                        what = "stgcost")
  varom <- ctx$cycles * (inpcost * charged + outcost * delivered +
                           stgcost * stgCap)

  # the charging term: the commodity the store consumes, priced like a
  # technology's fuel. Round-trip loss shows up here AND in the denominator.
  price <- ctx$price %||% rep(0, ny)
  supply <- ctx$cycles * price * charged

  annual_out <- ctx$cycles * delivered
  total <- eac + fixom + varom + supply

  list(years = years, total = total, annual_out = annual_out,
       components = list(eac = eac, fixom = fixom, varom = varom,
                         supply = supply),
       eac_parts = eac_parts, caps = caps,
       charged = ctx$cycles * charged)
}

#' Build and solve the representative-cycle mini-model for a storage
#'
#' The store is the only path from the charging slice to the discharging one:
#' the supply is available in `CHG` only (`ava.up = 0` in `DIS`), so a unit of
#' demand placed in `DIS` can be met by nothing else. Without that restriction
#' the model would serve the demand straight from the supply and the store would
#' never be built -- a zero-cost answer that looks plausible and is wrong.
#' @noRd
.levcost_storage_solve <- function(ctx) {
  object <- ctx$object
  years  <- ctx$hor_years
  region <- ctx$region
  cycles <- ctx$cycles

  out_comm <- as.character(object@output$comm)[1]
  inp_comm <- if (nrow(object@input) > 0 && !is.na(object@input$comm[1]))
    as.character(object@input$comm)[1] else out_comm

  tt  <- make_timetable(struct = list(CYCLE = c(CHG = 0.5, DIS = 0.5)),
                        year_fraction = 1 / cycles)
  cal <- newCalendar(timetable = tt, year_fraction = 1 / cycles,
                     name = "levcost_cycle")

  # a store cannot close its cycle across a representative period that is not
  # the whole year, so pin fullYear off whatever the object declared
  object@fullYear <- FALSE
  object@region   <- region

  big <- 1e6
  price <- ctx$price %||% 0
  sup <- newSupply(
    name = ".LCS_SUP", commodity = inp_comm,
    supply = data.frame(
      region = region,
      timeslice = c("CHG", "DIS"),
      ava.up = c(big, 0),
      cost = c(price[1], price[1]),
      stringsAsFactors = FALSE))

  # one unit delivered per cycle, placed in the discharging slice
  dem <- newDemand(
    name = ".LCS_DEM", commodity = out_comm, region = region,
    demand = data.frame(region = region, year = rep(years, each = 1L),
                        timeslice = "DIS", demand = 1,
                        stringsAsFactors = FALSE))

  comms <- lapply(unique(c(inp_comm, out_comm)), function(cm)
    newCommodity(name = cm, timeframe = "CYCLE"))

  repo <- do.call(newRepository, c(list(name = ".LCS_REPO"), comms,
                                   list(sup, dem, object)))
  mdl <- newModel(name = ".LCS", region = region, discount = ctx$discount,
                  calendar = cal, horizon = ctx$horizon, data = repo)
  sc <- solve(mdl, name = ".LCS_RUN", solver = ctx$solver, transient = TRUE,
              path = .levcost_scratch_path(".LCS_RUN"), verbose = FALSE)
  if (!isTRUE(sc@status$optimal))
    stop("levcost(): the storage mini-model did not solve to optimality.",
         call. = FALSE)
  sc
}

#' Pull the solved mini-model apart into the same shape the analytic path returns
#'
#' Flows are per-timeslice, costs are already annual: the cost equations carry
#' the timeslice weight, the flow variables do not. So annual delivered energy is
#' `sum(vStorageOut) * cycles`, and multiplying the costs by anything would
#' double-count.
#' @noRd
.levcost_storage_extract <- function(sc, ctx) {
  years <- ctx$hor_years
  get <- function(v) tryCatch({
    d <- getData(sc, name = v, merge = TRUE, drop.zeros = FALSE,
                 timeframe = "lowest")
    if (is.null(d) || nrow(d) == 0) return(NULL)
    as.data.frame(d)
  }, error = function(e) NULL)
  by_year <- function(d) {
    if (is.null(d) || !"year" %in% names(d)) return(setNames(rep(0, length(years)), years))
    a <- tapply(d$value, as.integer(d$year), sum, na.rm = TRUE)
    v <- as.numeric(a[as.character(years)]); v[is.na(v)] <- 0
    setNames(v, years)
  }

  eac   <- by_year(get("vStorageEac"))
  fixom <- by_year(get("vStorageFixom"))
  varom <- by_year(get("vStorageVarom"))
  total <- by_year(get("vTotalCost"))
  # everything the store did not pay for is the charging energy
  supply <- total - eac - fixom - varom
  annual_out <- by_year(get("vStorageOut")) * ctx$cycles

  list(years = years, total = unname(total), annual_out = unname(annual_out),
       components = list(eac = unname(eac), fixom = unname(fixom),
                         varom = unname(varom), supply = unname(supply)),
       scen = sc)
}

# =============================================================================
# Shared plumbing -- used by `levcost_storage_()` and `levcost_trade_()` alike
# =============================================================================

#' Blank the capacity bounds that would distort a unit-demand mini-model
#'
#' The technology path matched these names literally, which silently did nothing
#' for a storage: its columns are `out.cap.up` / `stg.stock` / `inp.ncap.fx`, so
#' `intersect()` came back empty and the bounds leaked into the mini-model. Match
#' on the suffix after an optional `part.` prefix instead.
#' @noRd
.levcost_strip_capacity <- function(object, verbose = TRUE, what = "mini-model") {
  cap <- object@capacity
  if (NROW(cap) == 0) return(object)
  bounds <- c("stock", "cap.lo", "cap.up", "cap.fx",
              "ncap.lo", "ncap.up", "ncap.fx",
              "ret.lo", "ret.up", "ret.fx")
  hit <- names(cap)[vapply(names(cap), function(nm) {
    nm %in% bounds || sub("^[a-z]+[.]", "", nm) %in% bounds
  }, logical(1))]
  if (!length(hit)) return(object)
  for (col in hit) cap[[col]] <- NA_real_
  object@capacity <- cap
  if (verbose) message("Capacity constraints stripped for the ", what, ".")
  object
}

#' Drop a devintaged cell's build window, keeping its lifetime
#'
#' `.levcost_devintage()` carries `start`/`end` through, and for a levelized
#' cost they are actively wrong: the question is what a unit of THIS vintage
#' costs, not whether it could have been built inside the priced horizon. Left
#' in, the two engines disagree -- the closed form never reads the window and
#' returns a number, while the mini-model honours it and a vintage whose window
#' falls outside the horizon goes INFEASIBLE. `olife` is untouched; it is what
#' drives the annuity.
#' @noRd
.levcost_unwindow <- function(obj) {
  v <- obj@vintage
  if (NROW(v) == 0) return(obj)
  for (cc in intersect(c("start", "end"), names(v))) v[[cc]] <- NA_integer_
  obj@vintage <- v
  obj
}

#' Turn per-year totals and an output series into a `levcost` object
#'
#' One assembler for both classes, and deliberately the same list shape as
#' `levcost_technology_()` returns -- every existing accessor, `autoplot()`
#' method and `report()` template reads that shape and none of them should need
#' to know which class produced it.
#' @noRd
.levcost_assemble <- function(res, name, comm, region, discount, base_year,
                              units = NULL, scen = NULL) {
  years <- as.integer(res$years)
  lc <- ifelse(is.finite(res$annual_out) & res$annual_out != 0,
               res$total / res$annual_out, NA_real_)

  levcost_tbl <- data.frame(
    tech = name, group = NA_character_, comm = comm, region = region,
    year = years, levcost = lc, stringsAsFactors = FALSE)

  te  <- years - as.integer(base_year)
  dsc <- (1 + discount)^te
  pv  <- function(x) {
    ok <- is.finite(x) & is.finite(dsc) & dsc > 0
    if (any(ok)) sum(x[ok] / dsc[ok]) else NA_real_
  }
  npv_den <- pv(res$annual_out)
  levcost_npv <- if (is.finite(npv_den) && npv_den > 0) pv(res$total) / npv_den
                 else NA_real_

  cmp <- res$components
  cost_breakdown <- do.call(rbind, lapply(names(cmp), function(nm)
    data.frame(tech = name, group = NA_character_, comm = comm, region = region,
               year = years, component = nm,
               value = ifelse(is.finite(res$annual_out) & res$annual_out != 0,
                              cmp[[nm]] / res$annual_out, NA_real_),
               stringsAsFactors = FALSE)))
  rownames(cost_breakdown) <- NULL

  cost_breakdown_npv <- if (is.finite(npv_den) && npv_den > 0)
    do.call(rbind, lapply(names(cmp), function(nm)
      data.frame(tech = name, group = NA_character_, comm = comm,
                 component = nm, value = pv(cmp[[nm]]) / npv_den,
                 stringsAsFactors = FALSE))) else NULL
  if (!is.null(cost_breakdown_npv)) rownames(cost_breakdown_npv) <- NULL

  cost_yearly <- data.frame(tech = name, group = NA_character_, region = region,
                            year = years, total = res$total,
                            stringsAsFactors = FALSE)
  for (nm in names(cmp)) cost_yearly[[nm]] <- cmp[[nm]]
  cost_yearly$activity <- res$annual_out

  result <- list(
    levcost            = levcost_tbl,
    levcost_npv        = setNames(levcost_npv, name),
    cost_breakdown     = cost_breakdown,
    cost_breakdown_npv = cost_breakdown_npv,
    levcost_per_act    = NULL,
    cost_yearly        = cost_yearly,
    frontier           = NULL,
    levcost_by_comm    = NULL,
    levcost_by_act     = NULL,
    levcost_by_act_cbd = NULL,
    input_frontier     = NULL,
    discount           = discount,
    base_year          = as.integer(base_year),
    units              = units,
    scenario           = scen
  )
  class(result) <- c("levcost", "list")
  result
}

# =============================================================================
# Driver
# =============================================================================

#' Levelized cost of storage
#'
#' Called through [levcost()] on a `storage` object. See
#' `vignette("levcost")` for the metric; the header of this file for the algebra.
#'
#' @param object A `storage` object.
#' @param comm Commodity to price the output in. Defaults to the discharged
#'   commodity.
#' @param cycles Full charge/discharge cycles per year. A store's levelized cost
#'   is inversely proportional to how hard it is worked, and no slot carries
#'   that, so it is an argument. `365` is one cycle a day.
#' @param price Price of the charging commodity, per unit. Taken from
#'   `fuel_costs`, then from a matching `supply` in `repo`, then `0`.
#' @param repo,fuel_costs,discount,base_year,horizon,region,solver,method,verbose
#'   As in [levcost()] for a technology.
#' @param ... Ignored, for signature compatibility with the technology method.
#' @return A `levcost` object -- the same shape the technology method returns.
#' @keywords internal
levcost_storage_ <- function(
    object,
    comm       = NULL,
    cycles     = 365,
    price      = NULL,
    repo       = NULL,
    fuel_costs = NULL,
    discount   = 0.05,
    base_year  = NULL,
    horizon    = NULL,
    region     = NULL,
    solver     = solver_options$glpk,
    method     = c("auto", "analytic", "solve"),
    verbose    = TRUE,
    ...
) {
  method <- match.arg(method)
  if (!inherits(object, "storage"))
    stop("`object` must be an energyRt 'storage' object.", call. = FALSE)
  if (!is.numeric(cycles) || length(cycles) != 1 || !is.finite(cycles) ||
      cycles <= 0)
    stop("`cycles` must be one positive number (cycles per year).", call. = FALSE)

  name <- if (nzchar(object@name)) object@name else "STORAGE"
  out_comm <- as.character(object@output$comm)[1]
  if (is.na(out_comm) || !nzchar(out_comm))
    stop("storage '", name, "' declares no output commodity.", call. = FALSE)
  inp_comm <- if (NROW(object@input) > 0 && !is.na(object@input$comm[1]))
    as.character(object@input$comm)[1] else out_comm
  if (is.null(comm)) comm <- out_comm

  # ── region ────────────────────────────────────────────────────────────────
  if (is.null(region)) {
    cand <- .levcost_tech_regions(object)
    region <- if (length(cand)) cand[1] else "REGION"
    if (length(cand) > 1 && verbose)
      message("Multiple regions in storage; using first: '", region, "'.")
  } else region <- region[1]
  if (length(.levcost_tech_regions(object)) > 1)
    object <- .levcost_subset_tech_region(object, region)

  # ── horizon ───────────────────────────────────────────────────────────────
  if (is.null(horizon))
    horizon <- .levcost_auto_horizon(object, base_year = base_year,
                                     verbose = verbose)
  hor_years <- as.integer(horizon@period)
  if (is.null(base_year)) base_year <- min(hor_years)

  # ── vintage / cluster variants ────────────────────────────────────────────
  # One cell per (vintage, cluster), each priced by a fresh call. The technology
  # path instead rides every cell in ONE model pinned to artificial regions --
  # worth it there because a technology mini-model is expensive, pointless here
  # because a storage mini-model is two timeslices. `price` is deliberately
  # still NULL at this point, so each cell resolves its own charging price.
  #
  # The horizon is resolved from the BASE object ABOVE and passed down, so it
  # spans every vintage window. Letting each cell auto-size its own would price
  # the vintages on different horizons and different discounting base years,
  # which is exactly the comparison a variant fan-out exists to make.
  vfan <- .tech_variants(object)
  if (!is.null(vfan)) {
    if (verbose)
      message("levcost(): '", name, "' has ", length(vfan$objects),
              " vintage/cluster cells; pricing each.")
    cells <- lapply(lapply(vfan$objects, .levcost_devintage, region = region),
                    .levcost_unwindow)
    names(cells) <- vapply(cells, function(o) o@name, character(1))
    per <- lapply(cells, function(o)
      levcost_storage_(o, comm = comm, cycles = cycles, price = price,
                       repo = repo, fuel_costs = fuel_costs,
                       discount = discount, base_year = base_year,
                       horizon = horizon, region = region, solver = solver,
                       method = method, verbose = FALSE))
    return(.levcost_variants_object(per, vfan$prov, name))
  }

  object <- .levcost_strip_capacity(object, verbose = verbose,
                                    what = "LCOS mini-model")

  # ── price of the charging commodity ───────────────────────────────────────
  if (is.null(price)) {
    price <- if (!is.null(fuel_costs) && inp_comm %in% names(fuel_costs))
      rep(as.numeric(fuel_costs[[inp_comm]]), length(hor_years))
    else {
      sups <- Filter(function(o) inherits(o, "supply") &&
                       inp_comm %in% as.character(o@commodity), repo %||% list())
      if (length(sups)) .la_supply_price(sups[[1]], hor_years, inp_comm)
      else rep(0, length(hor_years))
    }
  }
  if (length(price) == 1L) price <- rep(as.numeric(price), length(hor_years))

  # Both engines price ONE cycle and scale it by `cycles`, so a seasonal store
  # -- `fullYear = TRUE`, cycling across the whole year rather than within its
  # parent timeframe -- is being asked a different question from the one it was
  # built to answer. `cycles` is then the user's declaration of how often it
  # actually cycles, which is the only sizing a single-process metric can have.
  if (isTRUE(object@fullYear) && verbose)
    message("levcost(): '", name, "' has fullYear = TRUE; LCOS prices one ",
            "cycle scaled by cycles = ", cycles, ", not the annual cycle.")

  ctx <- list(object = object, hor_years = hor_years, region = region,
              discount = discount, cycles = cycles, price = price,
              horizon = horizon, solver = solver)

  # ── engines ───────────────────────────────────────────────────────────────
  run_solve <- function() {
    sc <- .levcost_storage_solve(ctx)
    .levcost_storage_extract(sc, ctx)
  }
  res <- switch(
    method,
    analytic = .levcost_storage_analytic(ctx),
    solve    = run_solve(),
    auto     = tryCatch(.levcost_storage_analytic(ctx),
                        levcost_analytic_unsupported = function(e) {
                          if (verbose)
                            message("levcost(): ", conditionMessage(e),
                                    " -- falling back to the solver.")
                          run_solve()
                        }))

  units <- data.frame(comm = comm,
                      unit = {
                        u <- object@output$unit
                        if (length(u) && !is.na(u[1])) as.character(u[1]) else NA_character_
                      }, stringsAsFactors = FALSE)

  .levcost_assemble(res, name = name, comm = comm, region = region,
                    discount = discount, base_year = base_year,
                    units = units, scen = res$scen)
}
