# =========================================================================== #
# check_feasibility(): emission caps.
#
# A cap bounds a sum over DISPATCH, which no declaration fixes, so the check
# reasons through a chain instead: demand must be served, the emission-free
# fleet has a ceiling, the residue must be burnt, so emissions are at least
# residue x the cleanest intensity. A cap below that is contradictory.
#
# The chain is a proof only while every link is bounded, so most of this file
# is about ABSTAINING. A cap that cannot be disproved must not be reported,
# and the suite checks that it is not -- and that the abstention is visible
# rather than silent.
# =========================================================================== #

# One region, one demand, one emitting technology and optionally a clean one.
# `clean_cap` bounds the clean technology's new capacity: with a bound the
# chain closes, without one it cannot.
ec_model <- function(cap = 1e6, demand = 100, clean_cap = NA_real_,
                     clean = TRUE, eff = 0.5, factor = 2, name = "EC",
                     land = NA_real_, land_per_gw = 1, no_inv = FALSE,
                     cap_lo = NA_real_, coa_cap_lo = NA_real_) {
  cal <- newCalendar(
    timetable = make_timetable(struct = list(ANNUAL = "ANNUAL")),
    name = paste0("cal_", name))
  objs <- list(
    newCommodity("COA", timeframe = "ANNUAL",
                 emis = data.frame(comm = "CO2", emis = factor)),
    newCommodity("CO2", timeframe = "ANNUAL"),
    newCommodity("ELC", timeframe = "ANNUAL"),
    newSupply("SUP_COA", commodity = "COA", unit = "PJ",
              supply = data.frame(region = "R1", cost = 1, ava.up = 1e6)),
    newDemand("DEM_ELC", commodity = "ELC", unit = "GWh",
              demand = data.frame(region = "R1", year = 2025,
                                  demand = demand)),
    newTechnology("ECOA",
      input = data.frame(comm = "COA", unit = "PJ"),
      output = data.frame(comm = "ELC", unit = "GWh"),
      ceff = data.frame(comm = "COA", cinp2use = eff),
      cap2act = 1,
      capacity = if (is.na(coa_cap_lo)) {
        data.frame(region = "R1", year = 2025, stock = 1e4)
      } else {
        data.frame(region = "R1", year = 2025, stock = 1e4,
                   cap.lo = coa_cap_lo)
      }),
    newConstraint(
      name = "CO2_CAP", desc = "cap", eq = "<=",
      for.each = data.frame(year = 2025L, comm = "CO2"),
      term1 = list(variable = "vEmsFuelTot"),
      rhs = data.frame(year = 2025L, rhs = cap),
      defVal = 1e10, replace_zerros = NULL))
  if (clean) {
    sol <- list(
      output = data.frame(comm = "ELC", unit = "GWh"),
      cap2act = 1,
      capacity = if (is.na(clean_cap)) {
        data.frame(region = "R1", year = 2025, stock = 0)
      } else {
        data.frame(region = "R1", year = 2025, stock = 0,
                   ncap.up = clean_cap)
      })
    if (!is.na(land)) {
      objs <- c(objs, list(
        newCommodity("LAND", timeframe = "ANNUAL"),
        newSupply("RES_LAND", commodity = "LAND", unit = "km2",
                  supply = data.frame(region = "R1", cost = 0,
                                      ava.up = land))))
      sol$aux <- data.frame(acomm = "LAND", unit = "km2")
      sol$aeff <- data.frame(acomm = "LAND", cap2ainp = land_per_gw)
    }
    objs <- c(objs, list(do.call(newTechnology, c(list("ESOL"), sol))))
  }
  if (no_inv) {
    objs <- c(objs, list(newConstraint(
      name = "NO_INV", desc = "nothing may be built", eq = "<=",
      for.each = data.frame(year = 2025L),
      term1 = list(variable = "vTechNewCap"),
      rhs = data.frame(year = 2025L, rhs = 0),
      defVal = 1e10, replace_zerros = NULL)))
  }
  if (!is.na(cap_lo)) {
    objs <- c(objs, list(newConstraint(
      name = "CAP_LIMIT", desc = "total capacity ceiling", eq = "<=",
      for.each = data.frame(year = 2025L),
      term1 = list(variable = "vTechCap"),
      rhs = data.frame(year = 2025L, rhs = cap_lo),
      defVal = 1e10, replace_zerros = NULL)))
  }
  newModel(name = name, desc = "", calendar = cal, region = "R1",
           horizon = newHorizon(2025), discount = 0,
           repo = newRepository(paste0("repo_", name), objs))
}

ec_scen <- function(...) {
  pr_local_model()
  suppressMessages(interpolate_model(ec_model(...), name = "ECS",
                                     verbose = FALSE))
}

# --------------------------------------------------------------------------- #
# The chain closes
# --------------------------------------------------------------------------- #

test_that("a cap below the unavoidable emission floor is reported", {
  # 100 GWh demanded, no clean capacity at all and none permitted, coal at
  # efficiency 0.5 and factor 2 -> 4 units of CO2 per GWh -> a floor of 400,
  # against a cap of 10
  s <- ec_scen(cap = 10, demand = 100, clean = FALSE, eff = 0.5, factor = 2)
  f <- check_feasibility(s, verbose = FALSE)
  e <- f[f$check == "emission-cap", , drop = FALSE]
  expect_equal(nrow(e), 1L)
  expect_equal(e$commodity, "CO2")
  expect_equal(e$year, 2025L)
  expect_equal(e$available, 10)
  expect_equal(e$required, 400)
  expect_equal(e$shortfall, 390)
  expect_equal(e$forced_by, "CO2_CAP")
})

test_that("a cap the floor fits under is not reported", {
  s <- ec_scen(cap = 1e6, demand = 100, clean = FALSE)
  f <- check_feasibility(s, verbose = FALSE)
  expect_equal(nrow(f[f$check == "emission-cap", , drop = FALSE]), 0L)
})

test_that("clean capacity that covers the demand removes the floor", {
  # 100 GWh demanded and 100 GWh of permitted clean build: no residue
  s <- ec_scen(cap = 10, demand = 100, clean_cap = 100)
  f <- check_feasibility(s, verbose = FALSE)
  expect_equal(nrow(f[f$check == "emission-cap", , drop = FALSE]), 0L)
})

test_that("partial clean capacity leaves a proportionate floor", {
  # 100 demanded, 75 clean permitted -> 25 residue at 4 per unit -> 100
  s <- ec_scen(cap = 10, demand = 100, clean_cap = 75)
  f <- check_feasibility(s, verbose = FALSE)
  e <- f[f$check == "emission-cap", , drop = FALSE]
  expect_equal(nrow(e), 1L)
  expect_equal(e$required, 100)
})

test_that("the floor follows efficiency and the emission factor", {
  # efficiency 0.25 and factor 1 -> 4 per unit, same floor as 0.5 and 2
  s <- ec_scen(cap = 10, demand = 100, clean = FALSE, eff = 0.25,
               factor = 1)
  e <- check_feasibility(s, verbose = FALSE)
  e <- e[e$check == "emission-cap", , drop = FALSE]
  expect_equal(e$required, 400)
})

# --------------------------------------------------------------------------- #
# The chain does not close: abstain, and say so
# --------------------------------------------------------------------------- #

test_that("unbounded clean capacity makes the check abstain, not fire", {
  # ESOL may be built without limit, so no floor can be proved even though
  # the cap is absurdly tight
  s <- ec_scen(cap = 10, demand = 100, clean_cap = NA_real_)
  f <- check_feasibility(s, verbose = FALSE)
  expect_equal(nrow(f[f$check == "emission-cap", , drop = FALSE]), 0L)
  ab <- attr(f, "abstained")
  expect_true(!is.null(ab) && nrow(ab) > 0)
  expect_equal(unique(ab$constraint), "CO2_CAP")
  expect_match(ab$reason[1], "unbounded capacity")
})

test_that("an abstention is reported, not passed over in silence", {
  s <- ec_scen(cap = 10, demand = 100, clean_cap = NA_real_)
  expect_message(check_feasibility(s), "ABSTAINED")
})

test_that("a model with no emission cap abstains over nothing", {
  s <- ec_scen(cap = 1e9, demand = 100, clean = FALSE)
  f <- check_feasibility(s, verbose = FALSE)
  expect_null(attr(f, "abstained"))
})

# --------------------------------------------------------------------------- #
# Shape, so the rows sit beside the other checks
# --------------------------------------------------------------------------- #

test_that("emission rows carry the retirement column like any other", {
  s <- ec_scen(cap = 10, demand = 100, clean = FALSE)
  f <- check_feasibility(s, verbose = FALSE)
  expect_true("retirement" %in% names(f))
  expect_equal(unique(f$retirement), "off")
  expect_equal(attr(f, "retirement_actual"), "off")
})

# --------------------------------------------------------------------------- #
# Capacity bounded by a RESOURCE rather than by its own ncap bound. This is
# the link a VRE model needs: solar may be freely investable and still be
# capped by the land it consumes per unit of capacity.
# --------------------------------------------------------------------------- #

test_that("a bounded per-capacity resource bounds the clean fleet", {
  # ESOL is freely investable but each GW needs 1 km2 and only 60 km2 exist,
  # so at most 60 GWh of the 100 demanded can be clean -> 40 x 4 = 160
  s <- ec_scen(cap = 10, demand = 100, clean_cap = NA_real_, land = 60,
               land_per_gw = 1)
  f <- check_feasibility(s, verbose = FALSE)
  e <- f[f$check == "emission-cap", , drop = FALSE]
  expect_equal(nrow(e), 1L)
  expect_equal(e$required, 160)
  expect_null(attr(f, "abstained"))
})

test_that("ample land leaves nothing to prove", {
  s <- ec_scen(cap = 10, demand = 100, clean_cap = NA_real_, land = 1000,
               land_per_gw = 1)
  f <- check_feasibility(s, verbose = FALSE)
  expect_equal(nrow(f[f$check == "emission-cap", , drop = FALSE]), 0L)
})

test_that("land per GW scales the bound", {
  # 60 km2 at 2 km2/GW -> 30 GW clean -> 70 residue -> 280
  s <- ec_scen(cap = 10, demand = 100, clean_cap = NA_real_, land = 60,
               land_per_gw = 2)
  e <- check_feasibility(s, verbose = FALSE)
  e <- e[e$check == "emission-cap", , drop = FALSE]
  expect_equal(e$required, 280)
})

# --------------------------------------------------------------------------- #
# Bounds DERIVED from a constraint. A sum of non-negative variables held at
# zero pins every member to zero -- a bound no per-process parameter carries,
# and the one the emission chain needs when a clean fleet is freely
# investable.
# --------------------------------------------------------------------------- #

test_that("a sum-to-zero constraint bounds new capacity for the chain", {
  # ESOL has no ncap bound and no land limit, so on its own the chain
  # abstains; NO_INV pins total new capacity to zero, which closes it
  loose <- ec_scen(cap = 10, demand = 100, clean_cap = NA_real_)
  expect_true(nrow(attr(check_feasibility(loose, verbose = FALSE),
                        "abstained")) > 0)
  tight <- ec_scen(cap = 10, demand = 100, clean_cap = NA_real_,
                   no_inv = TRUE)
  f <- check_feasibility(tight, verbose = FALSE)
  e <- f[f$check == "emission-cap", , drop = FALSE]
  expect_equal(nrow(e), 1L)
  expect_equal(e$required, 400)      # no clean stock, nothing may be built
  expect_null(attr(f, "abstained"))
})

test_that(".feas_derived_zero reads the constraint, not the parameters", {
  s <- ec_scen(cap = 1e9, demand = 100, clean_cap = NA_real_, no_inv = TRUE)
  dz <- energyRt:::.feas_derived_zero(s@modInp)
  expect_true("vTechNewCap" %in% names(dz))
  expect_equal(as.integer(dz$vTechNewCap$year), 2025L)
  expect_true(energyRt:::.feas_zero_at(dz, "vTechNewCap", 2025L))
  expect_false(energyRt:::.feas_zero_at(dz, "vTechNewCap", 2035L))
  expect_false(energyRt:::.feas_zero_at(dz, "vTechCap", 2025L))
})

test_that("a positive right-hand side derives no bound", {
  s <- ec_scen(cap = 1e9, demand = 100, cap_lo = 50)
  dz <- energyRt:::.feas_derived_zero(s@modInp)
  expect_false(energyRt:::.feas_zero_at(dz, "vTechCap", 2025L))
})

# --------------------------------------------------------------------------- #
# constraint-vs-forced: a right-hand side under the total its own terms are
# already forced to reach.
# --------------------------------------------------------------------------- #

test_that("a cap below a forced capacity total is reported", {
  # `cap.lo = 400` forces vTechCap to at least 400, against a ceiling of 50
  # declared on the same variable by CAP_LIMIT
  s <- ec_scen(cap = 1e9, demand = 100, clean = FALSE, cap_lo = 50,
               coa_cap_lo = 400)
  f <- check_feasibility(s, verbose = FALSE)
  e <- f[f$check == "constraint-vs-forced", , drop = FALSE]
  expect_equal(nrow(e), 1L)
  expect_equal(e$available, 50)
  expect_equal(e$required, 400)
  expect_equal(e$shortfall, 350)
  expect_equal(e$forced_by, "CAP_LIMIT")
  expect_equal(e$commodity, "vTechCap")
})

test_that("a ceiling the forced total fits under is not reported", {
  s <- ec_scen(cap = 1e9, demand = 100, clean = FALSE, cap_lo = 500,
               coa_cap_lo = 400)
  f <- check_feasibility(s, verbose = FALSE)
  expect_equal(nrow(f[f$check == "constraint-vs-forced", , drop = FALSE]), 0L)
})
