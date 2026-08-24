# =========================================================================== #
# Stepped price curves for supply, export and import.
#
# Each class carries a SINGLE price per (region, year, timeslice). A real supply
# curve is stepped -- cheap grades exhausted first, the next ones dearer -- and a
# real export market is the mirror image, paying less as more volume is pushed
# into it.
#
# A curve is CLUSTERS: one step per price, expanded into ordinary objects before
# interpolation, all summing into the same commodity balance. No new variable, no
# new equation, no solver-template change.
#
# `range` describes a continuous linear curve and each step is priced at its band
# MIDPOINT, which reproduces the area under that curve exactly:
#     price_t = lo + (hi - lo) * (alpha_{t-1} + alpha_t) / 2
#     sum_t w_t * price_t == mean(range)      <- identically, for ANY shares
#
# The ordering needs no integer variables: a cheaper step is strictly better per
# unit, so cost minimisation fills them in order. That is why the DIRECTION is
# enforced -- rising for supply/import, falling for export. A rising export curve
# is non-convex, and the solver would jump to the top step and skip the rest.
# =========================================================================== #

sc_hor <- function() newHorizon(2020, intervals = 1)
sc_cal <- function() newCalendar(
  timetable = make_timetable(struct = list(ANNUAL = "ANNUAL")), name = "sc_cal")

# One region, one year, one timeslice, so the arithmetic is readable.
sc_model <- function(name, objs, demand = 0) newModel(
  name = name, region = "R1", horizon = sc_hor(), discount = 0,
  calendar = sc_cal(),
  repo = do.call(newRepository, c(list("sc_repo",
    newCommodity("COA", timeframe = "ANNUAL"),
    newDemand("DEM", commodity = "COA",
              demand = data.frame(demand = demand))), objs)))

sc_solve <- function(m, nm) suppressMessages(
  solve_scen(suppressMessages(interpolate_model(m, name = nm))))

sc_obj <- function(m, nm) getData(sc_solve(m, nm), "vObjective",
                                  merge = TRUE)$value[1]

# Non-zero rows of a variable, keyed by the object name.
sc_by <- function(s, v, key) {
  d <- as.data.frame(getData(s, v, merge = TRUE))
  d <- d[abs(d$value) > 1e-9, ]
  setNames(round(d$value, 6), as.character(d[[key]]))
}

# --------------------------------------------------------------------------- #
# The arithmetic, no solver
# --------------------------------------------------------------------------- #

test_that("steps are priced at their band midpoints", {
  SUP <- newSupply("SUPC", commodity = "COA", supply = data.frame(ava.up = 100))
  S <- asSupplyCurve(SUP, range = c(10, 50), nsteps = 5)
  expect_equal(S@supply$cost, c(14, 22, 30, 38, 46))
  expect_equal(S@supply$ava.up, rep(20, 5))
  expect_equal(sum(S@supply$ava.up), 100)

  # The calibration identity: the quantity-weighted mean price IS the mean of
  # the range, for ANY shares. This is what fails silently if the prices are put
  # at the band edges instead of their midpoints.
  for (w in list(c(0.5, 0.5), c(0.5, 0.3, 0.2), c(0.1, 0.2, 0.3, 0.4))) {
    x <- asSupplyCurve(SUP, range = c(10, 50), shares = w)
    expect_equal(sum(w * x@supply$cost), 30)
  }

  # One step is the plain single-price object, exactly.
  one <- asSupplyCurve(SUP, range = c(30, 30), nsteps = 1)
  expect_equal(one@supply$cost, 30)
  expect_equal(one@supply$ava.up, 100)

  # An explicit price vector bypasses `range` entirely.
  ex <- asSupplyCurve(SUP, price = c(5, 9, 11))
  expect_equal(ex@supply$cost, c(5, 9, 11))
  expect_equal(ex@supply$ava.up, rep(100 / 3, 3))
})

test_that("the curve direction is enforced, in both directions", {
  SUP <- newSupply("SUPC", commodity = "COA", supply = data.frame(ava.up = 100))
  EXP <- newExport("EXPC", commodity = "COA", export = data.frame(exp.up = 100))
  IMP <- newImport("IMPC", commodity = "COA", import = data.frame(imp.up = 100))

  # Supply and import must RISE: the solver fills the cheapest step first, so a
  # cheaper step placed after a dearer one is never reached.
  expect_error(asSupplyCurve(SUP, range = c(50, 10), nsteps = 3), "do not rise")
  expect_error(asImportCurve(IMP, range = c(50, 10), nsteps = 3), "do not rise")
  expect_error(asSupplyCurve(SUP, price = c(10, 30, 20)), "do not rise")

  # Export must FALL. This is the dangerous one: export revenue is a negative
  # cost, so a rising curve is non-convex and the solver skips steps.
  expect_error(asExportCurve(EXP, range = c(10, 50), nsteps = 3), "do not fall")
  expect_error(asExportCurve(EXP, range = c(10, 50), nsteps = 3), "negative cost")

  # ... and the right way round is accepted.
  expect_equal(asExportCurve(EXP, range = c(50, 10), nsteps = 4)@export$price,
               c(45, 35, 25, 15))
})

test_that("a curve over an unbounded quantity is refused", {
  # Nothing limits the cheapest step, so the dearer ones could never be reached
  # and the "curve" would be a single price wearing five labels.
  expect_error(asSupplyCurve(newSupply("SUPC", commodity = "COA"),
                             range = c(10, 50), nsteps = 3),
               "no finite quantity bound")
  # A finite RESERVE alone is enough, even with no per-period bound.
  r <- asSupplyCurve(newSupply("SUPC", commodity = "COA",
                               reserve = data.frame(res.up = 80)),
                     range = c(10, 50), nsteps = 4)
  expect_equal(sum(r@reserve$res.up), 80)
})

test_that("bad inputs are refused, naming the problem", {
  SUP <- newSupply("SUPC", commodity = "COA", supply = data.frame(ava.up = 100))
  expect_error(asSupplyCurve(SUP, range = c(10, 50), shares = c(0.5, 0.4)),
               "sum to 0.9, not 1")
  expect_error(asSupplyCurve(SUP, range = c(10, 50), price = c(1, 2)), "not both")
  expect_error(asSupplyCurve(SUP, range = c(10, 50)), "no default number")
  expect_error(asSupplyCurve(SUP, range = c(10, 50), nsteps = 3,
                             labels = c("A", "A", "B")), "must be unique")
  # Wrong class in, named alternative out.
  expect_error(asSupplyCurve(newExport("E", commodity = "COA"),
                             range = c(10, 50), nsteps = 2), "asExportCurve")
})

test_that("the legacy scalar `reserve` still works", {
  # `@reserve` was a bare number before 0.85. Accepting one keeps every existing
  # export/import model building.
  e <- newExport("E", commodity = "COA", reserve = 1000)
  expect_s3_class(e@reserve, "data.frame")
  expect_equal(e@reserve$res.up, 1000)
  expect_equal(nrow(newImport("I", commodity = "COA")@reserve), 0)
})

# --------------------------------------------------------------------------- #
# The solve
# --------------------------------------------------------------------------- #

test_that("supply steps are used cheapest first", {
  skip_if_no_solver()
  cv <- function() asSupplyCurve(
    newSupply("SUPC", commodity = "COA", supply = data.frame(ava.up = 100)),
    range = c(10, 50), nsteps = 5)          # 20 units each at 14/22/30/38/46

  # Below the first breakpoint only the cheapest step runs.
  s <- sc_solve(sc_model("sc1", list(cv()), demand = 20), "sc1")
  expect_equal(sc_by(s, "vSupOut", "sup"), c(SUPC_CLS1 = 20))
  expect_equal(getData(s, "vObjective", merge = TRUE)$value[1], 20 * 14)

  # Past it, the next step starts -- and not before.
  s2 <- sc_solve(sc_model("sc2", list(cv()), demand = 50), "sc2")
  f <- sc_by(s2, "vSupOut", "sup")
  expect_equal(unname(f[c("SUPC_CLS1", "SUPC_CLS2", "SUPC_CLS3")]), c(20, 20, 10))
  expect_equal(getData(s2, "vObjective", merge = TRUE)$value[1],
               20 * 14 + 20 * 22 + 10 * 30)

  # Fully loaded, the total cost is the area under the curve: quantity x mean.
  expect_equal(sc_obj(sc_model("sc3", list(cv()), demand = 100), "sc3"),
               100 * 30)
})

test_that("a curve equals the same steps written as separate objects", {
  skip_if_no_solver()
  # The test that proves the helper adds no accidental economics: identical
  # objective to the hand-built family-of-objects idiom.
  curve <- asSupplyCurve(
    newSupply("SUPX", commodity = "COA", supply = data.frame(ava.up = 100)),
    range = c(10, 50), nsteps = 5)
  family <- lapply(1:5, function(k) newSupply(
    paste0("SUPX_", k), commodity = "COA",
    supply = data.frame(ava.up = 20, cost = 10 + 40 * ((k - 1) / 5 + k / 5) / 2)))

  expect_equal(sc_obj(sc_model("sc4", list(curve), demand = 70), "sc4"),
               sc_obj(sc_model("sc5", family, demand = 70), "sc5"))
})

test_that("export fills the BEST-PAID step first", {
  skip_if_no_solver()
  # Export revenue is a negative cost (the minus is inside eqExportRowCost), so
  # the ordering is the mirror image of supply's and needs no extra machinery.
  EXP <- asExportCurve(
    newExport("EXPC", commodity = "COA", export = data.frame(exp.up = 100)),
    range = c(50, 10), nsteps = 4)          # 25 units each at 45/35/25/15
  m <- sc_model("sc6", list(
    newSupply("SUPC", commodity = "COA",
              supply = data.frame(ava.up = 60, cost = 1)), EXP), demand = 0)
  s <- sc_solve(m, "sc6")
  f <- sc_by(s, "vExportRow", "expp")
  # 60 available, so the top two steps fill and the third takes the remainder;
  # the cheapest-paid step is left empty.
  expect_equal(unname(f[c("EXPC_CLS1", "EXPC_CLS2", "EXPC_CLS3")]), c(25, 25, 10))
  expect_true(is.na(f["EXPC_CLS4"]))
  # 60 bought at 1, sold at 45/35/25.
  expect_equal(getData(s, "vObjective", merge = TRUE)$value[1],
               60 * 1 - (25 * 45 + 25 * 35 + 10 * 25))
})

test_that("a split reserve does NOT multiply the resource", {
  skip_if_no_solver()
  # The silent trap: leave `reserve` out of the variant slots and every step
  # inherits the FULL cumulative cap, so an n-step curve holds n times the
  # deposit -- feasible, and wrong. Asserted on the SOLVED cumulative, not on
  # the parameter.
  hor2 <- newHorizon(c(2020, 2021), intervals = c(1, 1))
  mk <- function(nm, nst) {
    SUP <- newSupply("SUPR", commodity = "COA",
                     supply = data.frame(ava.up = 1000),
                     reserve = data.frame(res.up = 100))
    if (nst > 1) SUP <- asSupplyCurve(SUP, range = c(10, 50), nsteps = nst)
    else SUP@supply$cost <- 30
    newModel(name = nm, region = "R1", horizon = hor2, discount = 0,
             calendar = sc_cal(),
             repo = newRepository("r", newCommodity("COA", timeframe = "ANNUAL"),
               SUP,
               newSupply("BACK", commodity = "COA",
                         supply = data.frame(cost = 1000)),
               newDemand("DEM", commodity = "COA",
                         demand = data.frame(demand = 200))))
  }
  tot <- function(nm, nst) {
    d <- as.data.frame(getData(sc_solve(mk(nm, nst), nm), "vSupReserve",
                               merge = TRUE))
    sum(d$value[grepl("^SUPR", d$sup)])
  }
  expect_equal(tot("sc7", 1), 100)
  expect_equal(tot("sc8", 4), 100)     # not 400
})

test_that("stepped results carry base/cluster provenance", {
  skip_if_no_solver()
  # `.attach_variants()` keys on the class's own id column, which had to gain
  # `sup` / `expp` / `imp` -- without that these results come back silently
  # unannotated.
  s <- sc_solve(sc_model("sc9", list(asSupplyCurve(
    newSupply("SUPC", commodity = "COA", supply = data.frame(ava.up = 100)),
    range = c(10, 50), nsteps = 3)), demand = 40), "sc9")
  d <- as.data.frame(getData(s, "vSupOut", merge = TRUE))
  expect_true(all(c("base", "cluster") %in% names(d)))
  expect_setequal(unique(d$base), "SUPC")
  expect_true(all(d$cluster %in% c("S1", "S2", "S3")))
})
