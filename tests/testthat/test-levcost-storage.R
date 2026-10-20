# Levelized cost of storage.
#
# The core guard is the same one test-levcost-analytic.R applies to a
# technology: the closed form and the solver must agree. They reach the answer
# by completely different routes -- one evaluates algebra over the slots, the
# other builds a representative-cycle model and reads the solution back -- so
# agreement to six figures is strong evidence that both are right, and any
# disagreement localises to whichever one the hand-checks below disown.

.lcs_tol <- 5e-5

.lcs_battery <- function(...) {
  args <- list(
    name = "STG_BTR", commodity = "ELC", region = "R1",
    input = list(comm = "ELC", cap2act = 8760),
    storage = list(comm = "ELC"),
    invcost = data.frame(stg.invcost = 100, out.invcost = 50,
                         inp.invcost = 20),
    fixom = data.frame(stg.fixom = 1, out.fixom = 2, inp.fixom = 3),
    varom = data.frame(inpcost = 0.1, outcost = 0.2, stgcost = 0.05),
    seff = data.frame(inpeff = 0.9, outeff = 0.95),
    duration = 4, vintage = data.frame(olife = 10L))
  # a plain replace, NOT modifyList(): a data.frame is a list, so modifyList()
  # would MERGE the columns of an override into the default and quietly keep
  # the very costs a test is trying to remove
  ov <- list(...)
  for (nm in names(ov)) args[[nm]] <- ov[[nm]]
  do.call(newStorage, args)
}

.lcs_pair <- function(stg, ...) {
  a <- levcost(stg, ..., method = "analytic", verbose = FALSE)
  s <- suppressMessages(suppressWarnings(
    levcost(stg, ..., method = "solve", verbose = FALSE)))
  list(a = a, s = s)
}

test_that("analytic equals solve on a lossy battery, per year and NPV", {
  skip_if_no_solver()
  p <- .lcs_pair(.lcs_battery(), cycles = 365, price = 10, discount = 0.05,
                 horizon = newHorizon(period = 2020:2029))

  expect_equal(unname(p$a$levcost_npv), unname(p$s$levcost_npv),
               tolerance = .lcs_tol)
  expect_equal(p$a$levcost$year, p$s$levcost$year)
  expect_equal(p$a$levcost$levcost, p$s$levcost$levcost, tolerance = .lcs_tol)

  # every component, not just the total: a compensating pair of errors would
  # pass on the total alone
  m <- merge(p$a$cost_breakdown_npv, p$s$cost_breakdown_npv, by = "component")
  expect_equal(nrow(m), 4L)
  expect_setequal(m$component, c("eac", "fixom", "varom", "supply"))
  expect_equal(m$value.x, m$value.y, tolerance = .lcs_tol)
})

test_that("a lossless store's LCOS is annualised capex over cycles", {
  # No efficiency loss, no varom, no charging price, one hour of duration and
  # only the reservoir priced. Then the whole answer is capex / cycles, which
  # is checkable without either engine.
  stg <- newStorage(
    "STG_PLAIN", commodity = "ELC", region = "R1",
    storage = list(comm = "ELC"),
    invcost = data.frame(stg.invcost = 100),
    duration = 1, vintage = data.frame(olife = 10L))
  lc <- levcost(stg, cycles = 365, discount = 0, method = "analytic",
                horizon = newHorizon(period = 2020L), verbose = FALSE)
  # crf(0, 10) = 1/10, one unit of reservoir per delivered unit
  expect_equal(lc$levcost$levcost[1], 100 * 0.1 / 365, tolerance = 1e-12)
  expect_equal(unname(lc$levcost_npv), 100 * 0.1 / 365, tolerance = 1e-12)
})

test_that("cycles divide the capex share and leave the charging term alone", {
  base <- .lcs_battery()
  h <- newHorizon(period = 2020L)
  a1 <- levcost(base, cycles = 200, price = 10, discount = 0.05, horizon = h,
                method = "analytic", verbose = FALSE)
  a2 <- levcost(base, cycles = 400, price = 10, discount = 0.05, horizon = h,
                method = "analytic", verbose = FALSE)
  cmp <- function(x, nm) x$cost_breakdown$value[x$cost_breakdown$component == nm]

  # capex and fixed O&M are per YEAR, so working the store twice as hard halves
  # them per unit delivered
  expect_equal(cmp(a2, "eac"),   cmp(a1, "eac") / 2,   tolerance = 1e-10)
  expect_equal(cmp(a2, "fixom"), cmp(a1, "fixom") / 2, tolerance = 1e-10)
  # the energy terms are per CYCLE, so they are untouched
  expect_equal(cmp(a2, "supply"), cmp(a1, "supply"), tolerance = 1e-10)
  expect_equal(cmp(a2, "varom"),  cmp(a1, "varom"),  tolerance = 1e-10)
})

test_that("round-trip efficiency enters twice", {
  h <- newHorizon(period = 2020L)
  eff <- .lcs_battery(seff = data.frame(inpeff = 0.9, outeff = 0.95))
  per <- .lcs_battery(seff = data.frame(inpeff = 1, outeff = 1))
  a <- levcost(eff, cycles = 365, price = 10, discount = 0, horizon = h,
               method = "analytic", verbose = FALSE)
  b <- levcost(per, cycles = 365, price = 10, discount = 0, horizon = h,
               method = "analytic", verbose = FALSE)
  sup <- function(x) x$cost_breakdown$value[x$cost_breakdown$component == "supply"]
  # charging cost per unit delivered is price / (inpeff * outeff)
  expect_equal(sup(a), 10 / (0.9 * 0.95), tolerance = 1e-10)
  expect_equal(sup(b), 10, tolerance = 1e-10)
})

test_that("capacity bounds no longer leak into the mini-model", {
  # The technology path matched capacity columns literally, which never hit a
  # storage's `stg.cap.up` / `out.stock`; the bound then survived into the
  # mini-model. Setting one must not move the answer.
  skip_if_no_solver()
  free  <- .lcs_battery()
  bound <- .lcs_battery(capacity = data.frame(stg.cap.up = 0.5,
                                              out.cap.up = 0.1))
  args <- list(cycles = 365, price = 10, discount = 0.05,
               horizon = newHorizon(period = 2020:2024), verbose = FALSE)
  for (m in c("analytic", "solve")) {
    l1 <- suppressMessages(suppressWarnings(
      do.call(levcost, c(list(free),  args, method = m))))
    l2 <- suppressMessages(suppressWarnings(
      do.call(levcost, c(list(bound), args, method = m))))
    expect_equal(unname(l1$levcost_npv), unname(l2$levcost_npv),
                 tolerance = .lcs_tol, info = m)
  }
})

test_that(".levcost_strip_capacity() matches prefixed and bare columns alike", {
  stg <- .lcs_battery(capacity = data.frame(stg.cap.up = 0.5, out.stock = 2,
                                            inp.ncap.fx = 1))
  out <- energyRt:::.levcost_strip_capacity(stg, verbose = FALSE)
  for (cc in c("stg.cap.up", "out.stock", "inp.ncap.fx"))
    expect_true(all(is.na(out@capacity[[cc]])), info = cc)
})

test_that("an unpriced part gets no capacity, and a priced one does", {
  # `inp.invcost` was silently free before the ratio maps became structural:
  # with no explicit `inp2out`, no equation tied vStorageInpCap to anything and
  # it stayed at zero, taking its capital charge with it.
  priced   <- .lcs_battery()
  unpriced <- .lcs_battery(
    invcost = data.frame(stg.invcost = 100, out.invcost = 50),
    fixom   = data.frame(stg.fixom = 1, out.fixom = 2))
  expect_true(energyRt:::.lcs_has_part(priced, "inp"))
  expect_false(energyRt:::.lcs_has_part(unpriced, "inp"))

  h <- newHorizon(period = 2020L)
  lp <- levcost(priced, cycles = 365, discount = 0.05, horizon = h,
                method = "analytic", verbose = FALSE)
  lu <- levcost(unpriced, cycles = 365, discount = 0.05, horizon = h,
                method = "analytic", verbose = FALSE)
  expect_gt(unname(lp$levcost_npv), unname(lu$levcost_npv))
})

test_that("a genuine duration range is refused rather than guessed", {
  stg <- .lcs_battery(duration = data.frame(duration.lo = 2, duration.up = 8))
  expect_error(
    levcost(stg, cycles = 365, method = "analytic",
            horizon = newHorizon(period = 2020L), verbose = FALSE),
    "range")
})

test_that("cycles must be one positive number", {
  stg <- .lcs_battery()
  expect_error(levcost(stg, cycles = 0, verbose = FALSE), "positive")
  expect_error(levcost(stg, cycles = c(1, 2), verbose = FALSE), "positive")
})

test_that("a container prices a storage by name and takes its fuel price", {
  ELC <- newCommodity("ELC", timeframe = "ANNUAL")
  SUP <- newSupply("SUP_ELC", commodity = "ELC", region = "R1",
                   supply = data.frame(region = "R1", ava.up = 1e6, cost = 10))
  repo <- newRepository("REPO", ELC, SUP, .lcs_battery())
  h <- newHorizon(period = 2020L)

  from_repo <- levcost(repo, name = "STG_BTR", cycles = 365, horizon = h,
                       discount = 0.05, verbose = FALSE)
  direct <- levcost(.lcs_battery(), cycles = 365, price = 10, horizon = h,
                    discount = 0.05, method = "analytic", verbose = FALSE)
  # the supply's cost is what the container hands the storage as `price`
  expect_equal(unname(from_repo$levcost_npv), unname(direct$levcost_npv),
               tolerance = 1e-10)
})

# ── ex-post: levcost() on a SOLVED scenario ──────────────────────────────────
# One solve, all three classes. This is the path that reads the realised
# operation rather than a mini-model, so its arithmetic is entirely separate
# from everything above and is checked by hand against the solved variables.

.lc_scen <- local({
  cache <- NULL
  function() {
    if (!is.null(cache)) return(cache)
    cal <- newCalendar(timetable = make_timetable(struct = list(
      SEASON = c(WIN = .25, SPR = .25, SUM = .25, AUT = .25))))
    repo <- newRepository(
      "R",
      newCommodity("ELC", timeframe = "SEASON"),
      newCommodity("GAS", timeframe = "ANNUAL"),
      newSupply("SUP_GAS", commodity = "GAS", region = "R1",
                supply = data.frame(region = "R1", ava.up = 1e6, cost = 5)),
      # capped generator: enough ENERGY for the year, nowhere near enough POWER
      # for a demand that all falls in one season, so the store must carry it
      newTechnology(
        "PP", region = "R1", input = list(comm = "GAS"),
        output = list(comm = "ELC"),
        ceff = data.frame(comm = c("GAS", "ELC"), cinp2use = c(0.5, NA),
                          cact2cout = c(NA, 1)),
        invcost = data.frame(invcost = 700), fixom = data.frame(fixom = 20),
        varom = data.frame(varom = 1),
        capacity = data.frame(cap.up = 0.012),
        vintage = data.frame(olife = 25L), cap2act = 8760),
      newStorage(
        "STG_BTR", commodity = "ELC", region = "R1",
        input = list(comm = "ELC", cap2act = 8760),
        storage = list(comm = "ELC"),
        invcost = data.frame(stg.invcost = 0.05, out.invcost = 0.5),
        fixom = data.frame(stg.fixom = 0.001, out.fixom = 0.01),
        varom = data.frame(outcost = 0.05),
        seff = data.frame(inpeff = 0.9, outeff = 0.95),
        duration = 500, vintage = data.frame(olife = 12L)),
      newTrade(
        "TRD_ELC", commodity = "ELC",
        routes = data.frame(src = c("R1", "R2"), dst = c("R2", "R1")),
        trade = data.frame(src = c("R1", "R2"), dst = c("R2", "R1"),
                           teff = 0.95, af.up = 0.9),
        invcost = data.frame(invcost = 300), fixom = data.frame(fixom = 5),
        vintage = data.frame(olife = 40L), cap2act = 8760),
      newDemand("DEM_ELC", commodity = "ELC",
                demand = data.frame(region = c("R1", "R2"), year = 2020,
                                    timeslice = "SUM", demand = c(50, 30))))
    mdl <- newModel("M", region = c("R1", "R2"), calendar = cal,
                    horizon = newHorizon(period = 2020:2024), data = repo)
    cache <<- suppressMessages(suppressWarnings(
      solve(mdl, name = "RUN", solver = solver_options$glpk, verbose = FALSE)))
    cache
  }
})

test_that("a solved scenario prices technology, storage and trade alike", {
  skip_if_no_solver()
  sc <- .lc_scen()
  skip_if_not(isTRUE(sc@status$optimal), "fixture did not solve")

  # the fixture is only meaningful if all three actually operate
  for (v in c("vTechOut", "vStorageOut", "vTradeIr")) {
    d <- getData(sc, v, merge = TRUE, timeframe = "lowest")
    expect_gt(NROW(d), 0)
  }
  for (nm in c("PP", "STG_BTR", "TRD_ELC")) {
    lc <- suppressMessages(levcost(sc, name = nm))
    expect_s3_class(lc, "levcost")
    expect_true(is.finite(unname(lc$levcost_npv)), info = nm)
    expect_gt(unname(lc$levcost_npv), 0)
  }
})

test_that("a corridor's ex-post denominator is what ARRIVES, not what is sent", {
  skip_if_no_solver()
  sc <- .lc_scen()
  skip_if_not(isTRUE(sc@status$optimal), "fixture did not solve")
  yr <- function(d, y) sum(d$value[as.integer(d$year) == y])
  ir  <- getData(sc, "vTradeIr",    merge = TRUE, timeframe = "lowest")
  eac <- getData(sc, "vTradeEac",   merge = TRUE, timeframe = "lowest")
  fx  <- getData(sc, "vTradeFixom", merge = TRUE, timeframe = "lowest")
  lc  <- suppressMessages(levcost(sc, name = "TRD_ELC"))
  got <- lc$levcost$levcost[lc$levcost$year == 2020]
  expect_equal(got, (yr(eac, 2020) + yr(fx, 2020)) / (yr(ir, 2020) * 0.95),
               tolerance = 1e-9)
})

test_that("a store's ex-post varom per unit discharged is its outcost", {
  skip_if_no_solver()
  sc <- .lc_scen()
  skip_if_not(isTRUE(sc@status$optimal), "fixture did not solve")
  lc <- suppressMessages(levcost(sc, name = "STG_BTR"))
  bd <- lc$cost_breakdown_npv
  # tolerance is the GLPK csv write precision, not the arithmetic's
  expect_equal(bd$value[bd$component == "varom"], 0.05, tolerance = 1e-6)
  # ELC has no supply object, so the charging energy is charged at zero here --
  # documented, and the reason this is the store's OWN cost and not its
  # delivered cost
  expect_equal(bd$value[bd$component == "supply"], 0, tolerance = 1e-12)
})

test_that("a name that is not a priceable process is reported, not crashed", {
  skip_if_no_solver()
  sc <- .lc_scen()
  skip_if_not(isTRUE(sc@status$optimal), "fixture did not solve")
  expect_message(x <- levcost(sc, name = "SUP_GAS"), "not a technology")
  expect_null(x)
})

# ── vintage / cluster variants ───────────────────────────────────────────────
# A vintaged store fans out to one priced cell per (vintage, cluster), reusing
# the same `levcost_variants` container the technology path returns -- so
# `levcost(by_variant = )` and `autoplot()` work on all three classes alike.

.lcs_vintaged <- function() {
  newStorage(
    "STG_BTR", commodity = "ELC", region = "R1",
    input = list(comm = "ELC", cap2act = 8760),
    storage = list(comm = "ELC"),
    invcost = data.frame(vintage = c("v2020", "v2030"),
                         stg.invcost = c(100, 60), out.invcost = c(50, 40)),
    seff = data.frame(inpeff = 0.9, outeff = 0.95),
    duration = 4,
    vintage = data.frame(vintage = c("v2020", "v2030"),
                         start = c(2020L, 2030L), end = c(2029L, 2040L),
                         olife = 10L))
}

test_that("a vintaged storage fans out to one levcost per cell", {
  x <- levcost(.lcs_vintaged(), cycles = 365, price = 10, discount = 0.05,
               horizon = newHorizon(period = 2020:2029),
               method = "analytic", verbose = FALSE)
  expect_s3_class(x, "levcost_variants")
  expect_s3_class(x, "levcost_list")
  expect_length(x, 2L)

  npv <- levcost(x, by_variant = "npv")
  expect_equal(nrow(npv), 2L)
  expect_setequal(npv$vintage, c("v2020", "v2030"))
  expect_equal(unique(npv$tech), "STG_BTR")
  expect_true(all(is.finite(npv$levcost_npv)))
  # the cheaper vintage is cheaper
  expect_lt(npv$levcost_npv[npv$vintage == "v2030"],
            npv$levcost_npv[npv$vintage == "v2020"])

  for (what in c("levcost", "components"))
    expect_true(all(c("tech", "variant", "vintage", "cluster") %in%
                      names(levcost(x, by_variant = what))), info = what)
})

test_that("each cell equals pricing that cell on its own", {
  # the fan-out must be exactly `devintage then price`, with nothing of the
  # other cells leaking in
  stg <- .lcs_vintaged()
  h <- newHorizon(period = 2020:2029)
  x <- levcost(stg, cycles = 365, price = 10, discount = 0.05, horizon = h,
               method = "analytic", verbose = FALSE)
  vf <- energyRt:::.tech_variants(stg)
  for (o in vf$objects) {
    cell <- energyRt:::.levcost_devintage(o, "R1")
    direct <- levcost(cell, cycles = 365, price = 10, discount = 0.05,
                      horizon = h, method = "analytic", verbose = FALSE)
    expect_equal(unname(x[[cell@name]]$levcost_npv),
                 unname(direct$levcost_npv), tolerance = 1e-12,
                 info = cell@name)
  }
})

test_that("the solve engine fans out the same way as the closed form", {
  skip_if_no_solver()
  stg <- .lcs_vintaged()
  h <- newHorizon(period = 2020:2029)
  a <- levcost(stg, cycles = 365, price = 10, discount = 0.05, horizon = h,
               method = "analytic", verbose = FALSE)
  s <- suppressMessages(suppressWarnings(
    levcost(stg, cycles = 365, price = 10, discount = 0.05, horizon = h,
            method = "solve", verbose = FALSE)))
  expect_s3_class(s, "levcost_variants")
  expect_setequal(names(s), names(a))
  for (nm in names(a))
    expect_equal(unname(a[[nm]]$levcost_npv), unname(s[[nm]]$levcost_npv),
                 tolerance = .lcs_tol, info = nm)
})

test_that("an un-vintaged storage still returns a plain levcost", {
  x <- levcost(.lcs_battery(), cycles = 365, price = 10, discount = 0.05,
               horizon = newHorizon(period = 2020L), method = "analytic",
               verbose = FALSE)
  expect_s3_class(x, "levcost")
  expect_false(inherits(x, "levcost_variants"))
  # no variants to stack -- the error says why (levcost_by_variant() is the
  # deprecated spelling of `by_variant =`)
  expect_error(levcost(x, by_variant = TRUE), "vintages or clusters")
})
