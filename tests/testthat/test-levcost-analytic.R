# Analytic (no-solve) levcost: closed-form mirror of the unit-demand annual
# mini-model. The core guard is PARITY -- method = "analytic" must reproduce
# method = "solve" to within the GLPK csv write precision (%f, 6 decimals).

.lc_tol <- 5e-5

.lc_pair <- function(tech, ...) {
  la <- levcost(tech, ..., method = "analytic", verbose = FALSE)
  sv <- suppressMessages(suppressWarnings(
    levcost(tech, ..., method = "solve", verbose = FALSE)))
  list(a = la, s = sv)
}

test_that("analytic equals solve on a plain technology, per year and NPV", {
  skip_if_no_solver()
  PP <- newTechnology(
    "PP", input = list(comm = "GAS"), output = list(comm = "ELC"),
    ceff = data.frame(comm = c("GAS", "ELC"), cinp2use = c(0.55, NA),
                      cact2cout = c(NA, 1)),
    invcost = data.frame(invcost = 800),
    fixom = data.frame(fixom = 20), varom = data.frame(varom = 2),
    af = data.frame(af.up = 0.85),
    vintage = data.frame(olife = 25L), cap2act = 1)
  p <- .lc_pair(PP, fuel_costs = c(GAS = 15), discount = 0.06,
                base_year = 2025)
  expect_identical(p$a$method, "analytic")
  expect_null(p$a$scenario)
  expect_equal(as.numeric(p$a$levcost_npv), as.numeric(p$s$levcost_npv),
               tolerance = .lc_tol)
  expect_equal(nrow(p$a$levcost), nrow(p$s$levcost))
  expect_equal(p$a$levcost$levcost, p$s$levcost$levcost, tolerance = .lc_tol)
  m <- merge(p$a$cost_breakdown_npv[, c("component", "value")],
             p$s$cost_breakdown_npv[, c("component", "value")],
             by = "component")
  expect_equal(m$value.x, m$value.y, tolerance = .lc_tol)
  # same wide table shape: components + activity/capacity + flows
  expect_true(all(c("eac", "fixom", "varom", "supply", "activity",
                    "capacity", "out_ELC", "inp_GAS") %in%
                    names(p$a$cost_yearly)))
})

test_that("wacc and payback drive the analytic annuity like the solver's", {
  skip_if_no_solver()
  T2 <- newTechnology(
    "T2", input = list(comm = "GAS"), output = list(comm = "ELC"),
    ceff = data.frame(comm = "GAS", cinp2use = 0.5),
    invcost = data.frame(invcost = 1000, wacc = 0.09, payback = 15),
    fixom = data.frame(fixom = 10), af = data.frame(af.up = 0.9),
    vintage = data.frame(olife = 30L), cap2act = 1)
  p <- .lc_pair(T2, fuel_costs = c(GAS = 10), discount = 0.05,
                base_year = 2025)
  expect_equal(as.numeric(p$a$levcost_npv), as.numeric(p$s$levcost_npv),
               tolerance = .lc_tol)
  # eac charged over the payback window only: nonzero eac years == 15
  eac_years <- p$a$cost_breakdown$year[p$a$cost_breakdown$component == "eac"]
  expect_length(eac_years, 15L)
})

test_that("a weather profile collapses to the same annual CF on both paths", {
  skip_if_no_solver()
  WTH <- newWeather("wnd", weather = data.frame(wval = 0.31))
  WI <- newTechnology(
    "WI", output = list(comm = "ELC"),
    weather = data.frame(weather = "wnd", waf.fx = 1),
    invcost = data.frame(invcost = 1400), fixom = data.frame(fixom = 35),
    vintage = data.frame(olife = 22L), cap2act = 1)
  p <- .lc_pair(WI, discount = 0.05, base_year = 2025, weather = WTH)
  expect_equal(as.numeric(p$a$levcost_npv), as.numeric(p$s$levcost_npv),
               tolerance = .lc_tol)
  # capacity sized to serve unit demand at CF = 0.31 (the solve path used to
  # drop the collapsed CF on the floor -- fixed alongside the analytic path)
  expect_equal(unique(round(p$a$cost_yearly$capacity, 4)), round(1 / 0.31, 4))
  expect_equal(unique(round(p$s$cost_yearly$capacity, 4)), round(1 / 0.31, 4))
})

test_that("auxiliary commodity costs are priced identically", {
  skip_if_no_solver()
  T4 <- newTechnology(
    "T4", input = list(comm = "GAS"), output = list(comm = "ELC"),
    aux = list(acomm = "WATER"),
    aeff = data.frame(acomm = "WATER", act2ainp = 0.3),
    ceff = data.frame(comm = "GAS", cinp2use = 0.6),
    invcost = data.frame(invcost = 700),
    vintage = data.frame(olife = 20L), cap2act = 1)
  p <- .lc_pair(T4, fuel_costs = c(GAS = 12, WATER = 4), discount = 0.05,
                base_year = 2025)
  expect_equal(as.numeric(p$a$levcost_npv), as.numeric(p$s$levcost_npv),
               tolerance = .lc_tol)
})

test_that("grouped input with share.fx matches the solver", {
  skip_if_no_solver()
  T5 <- newTechnology(
    "T5", input = data.frame(comm = c("COA", "BIO"), group = "FUEL"),
    output = list(comm = "ELC"),
    geff = data.frame(group = "FUEL", ginp2use = 0.4),
    ceff = data.frame(comm = c("COA", "BIO"), share.fx = c(0.7, 0.3)),
    invcost = data.frame(invcost = 900),
    vintage = data.frame(olife = 30L), cap2act = 1)
  p <- .lc_pair(T5, fuel_costs = c(COA = 8, BIO = 20), discount = 0.05,
                base_year = 2025)
  expect_equal(as.numeric(p$a$levcost_npv), as.numeric(p$s$levcost_npv),
               tolerance = .lc_tol)
})

test_that("free shares resolve to the solver's merit-order corner", {
  skip_if_no_solver()
  T6 <- newTechnology(
    "T6", input = data.frame(comm = c("COA", "BIO"), group = "FUEL"),
    output = list(comm = "ELC"),
    geff = data.frame(group = "FUEL", ginp2use = 0.4),
    ceff = data.frame(comm = c("COA", "BIO"),
                      share.lo = c(0.2, 0.1), share.up = c(0.9, 0.8)),
    invcost = data.frame(invcost = 900),
    vintage = data.frame(olife = 30L), cap2act = 1)
  p <- .lc_pair(T6, fuel_costs = c(COA = 8, BIO = 20), discount = 0.05,
                base_year = 2025)
  expect_equal(as.numeric(p$a$levcost_npv), as.numeric(p$s$levcost_npv),
               tolerance = .lc_tol)
  # ALL corner solutions are reported, the cheap corner flagged optimal
  fv <- p$a$frontier_vertices
  expect_false(is.null(fv))
  inp <- fv[fv$direction == "input:FUEL", ]
  expect_gte(length(unique(inp$vertex)), 2L)
  best <- inp[inp$optimal & inp$comm == "COA", ]
  expect_equal(unique(best$share), 0.9)   # cheap COA at its upper bound
})

test_that("grouped output with share caps matches the solver", {
  skip_if_no_solver()
  T7 <- newTechnology(
    "T7", input = list(comm = "GAS"),
    output = data.frame(comm = c("ELC", "HEAT"), group = "COGEN"),
    geff = data.frame(group = "COGEN"),
    ceff = data.frame(comm = c("GAS", "ELC", "HEAT"),
                      cinp2use = c(0.8, NA, NA),
                      cact2cout = c(NA, 0.4, 0.5),
                      share.up = c(NA, 0.6, 0.7)),
    invcost = data.frame(invcost = 1100),
    vintage = data.frame(olife = 25L), cap2act = 1)
  p <- .lc_pair(T7, fuel_costs = c(GAS = 9), discount = 0.05,
                base_year = 2025)
  expect_equal(as.numeric(p$a$levcost_npv), as.numeric(p$s$levcost_npv),
               tolerance = .lc_tol)
  # the frontier corner runs are mirrored too
  expect_false(is.null(p$a$levcost_by_comm))
  expect_setequal(unique(p$a$levcost_by_comm$scenario),
                  unique(p$s$levcost_by_comm$scenario))
})

test_that("vintaged technology: per-cell parity without artificial regions", {
  skip_if_no_solver()
  T8 <- newTechnology(
    "T8", input = list(comm = "GAS"), output = list(comm = "ELC"),
    ceff = data.frame(comm = "GAS", cinp2use = 0.5),
    vintage = data.frame(vintage = c("2025", "2035"),
                         start = c(2025L, 2035L), end = c(2034L, NA),
                         olife = c(20L, 25L)),
    invcost = data.frame(vintage = c("2025", "2035"), invcost = c(1000, 700)),
    fixom = data.frame(fixom = 15), af = data.frame(af.up = 0.8),
    cap2act = 1)
  p <- .lc_pair(T8, fuel_costs = c(GAS = 10), discount = 0.05,
                base_year = 2025)
  expect_s3_class(p$a, "levcost_variants")
  a <- levcost_by_variant(p$a, "npv")
  s <- levcost_by_variant(p$s, "npv")
  m <- merge(a[, c("variant", "levcost_npv")], s[, c("variant", "levcost_npv")],
             by = "variant")
  expect_equal(nrow(m), 2L)
  expect_equal(m$levcost_npv.x, m$levcost_npv.y, tolerance = .lc_tol)
})

# ── share-polytope vertex enumeration ─────────────────────────────────────────

test_that(".levcost_share_vertices enumerates every corner", {
  v <- energyRt:::.levcost_share_vertices(c(a = 0, b = 0), c(a = 1, b = 1))
  expect_equal(nrow(v), 2L)                       # (1,0) and (0,1)
  v2 <- energyRt:::.levcost_share_vertices(c(a = 0.2, b = 0.1),
                                           c(a = 0.9, b = 0.8))
  expect_equal(nrow(v2), 2L)                      # (0.9,0.1) and (0.2,0.8)
  expect_true(all(abs(rowSums(v2) - 1) < 1e-9))
  v3 <- energyRt:::.levcost_share_vertices(c(a = 0.1, b = 0.1, c = 0.1),
                                           c(a = 0.6, b = 0.6, c = 0.6))
  expect_true(all(abs(rowSums(v3) - 1) < 1e-9))
  expect_gte(nrow(v3), 6L)                        # hexagonal slice
  # at most one coordinate strictly inside its bounds at every vertex
  inner <- apply(v3, 1, function(s)
    sum(s > 0.1 + 1e-9 & s < 0.6 - 1e-9))
  expect_true(all(inner <= 1))
  # degenerate lo = up pins the vertex
  v4 <- energyRt:::.levcost_share_vertices(c(a = 0.7, b = 0.3),
                                           c(a = 0.7, b = 0.3))
  expect_equal(nrow(v4), 1L)
  expect_error(energyRt:::.levcost_share_vertices(c(a = 0.8, b = 0.5),
                                                  c(a = 0.9, b = 0.6)),
               "infeasible")
})

# ── refusals: never approximate ───────────────────────────────────────────────

test_that("non-representable features are refused, not approximated", {
  base_args <- list(
    input = list(comm = "GAS"), output = list(comm = "ELC"),
    invcost = data.frame(invcost = 800),
    vintage = data.frame(olife = 20L), cap2act = 1)

  t_afc <- do.call(newTechnology, c(list("TA"), base_args,
    list(ceff = data.frame(comm = "ELC", afc.up = 0.5))))
  expect_error(levcost(t_afc, fuel_costs = c(GAS = 10), method = "analytic",
                       verbose = FALSE), "afc")

  t_ret <- do.call(newTechnology, c(list("TR"), base_args,
    list(optimizeRetirement = TRUE)))
  expect_error(levcost(t_ret, fuel_costs = c(GAS = 10), method = "analytic",
                       verbose = FALSE), "optimizeRetirement")

  t_inv <- do.call(newTechnology, c(list("TI"),
    base_args[names(base_args) != "invcost"],
    list(invcost = data.frame(year = c(2025L, 2035L),
                              invcost = c(1000, 600)))))
  expect_error(levcost(t_inv, fuel_costs = c(GAS = 10), method = "analytic",
                       verbose = FALSE), "year-varying invcost")

  t_lo <- do.call(newTechnology, c(list("TL"), base_args,
    list(af = data.frame(af.lo = 0.3))))
  expect_error(levcost(t_lo, fuel_costs = c(GAS = 10), method = "analytic",
                       verbose = FALSE), "lower bound")

  # chains need the solver (the list path is reached via levcost_technology_)
  t_a <- do.call(newTechnology, c(list("TC1"), base_args))
  expect_error(
    energyRt:::levcost_technology_(list(t_a, t_a), method = "analytic",
                                   verbose = FALSE),
    "chain")
})

test_that("method = \"auto\" falls back to the solver with a message", {
  skip_if_no_solver()
  t_ret <- newTechnology(
    "TR", input = list(comm = "GAS"), output = list(comm = "ELC"),
    invcost = data.frame(invcost = 800),
    vintage = data.frame(olife = 20L), cap2act = 1,
    optimizeRetirement = TRUE)
  expect_message(
    lc <- suppressWarnings(levcost(t_ret, fuel_costs = c(GAS = 10),
                                   discount = 0.05, base_year = 2025)),
    "analytic path unavailable")
  expect_s3_class(lc, "levcost")
  expect_false(is.null(lc$scenario))    # solved, not analytic
})

test_that("the analytic path needs no solver at all", {
  # simulate a solver-less machine: analytic must succeed without touching
  # GLPK, which is the designer's no-solver path
  PP <- newTechnology(
    "PP", input = list(comm = "GAS"), output = list(comm = "ELC"),
    ceff = data.frame(comm = "GAS", cinp2use = 0.55),
    invcost = data.frame(invcost = 800),
    vintage = data.frame(olife = 25L), cap2act = 1)
  lc <- levcost(PP, fuel_costs = c(GAS = 15), discount = 0.06,
                base_year = 2025, method = "analytic",
                solver = list(name = "no_such_solver"), verbose = FALSE)
  expect_s3_class(lc, "levcost")
  expect_identical(lc$method, "analytic")
  expect_true(is.finite(lc$levcost_npv))
})
