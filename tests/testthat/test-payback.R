# `@invcost$payback` is the COST-RECOVERY life, distinct from the technical life
# in `@vintage$olife`. It shortens the annuity period AND the years over which
# the annuity is charged (the eqXEac window); capacity keeps operating for its
# full `olife` after the annuity stops.
#
# The GLPK equations take pXPayback where the user set one and fall back to
# pXOlife otherwise, so a model without payback is unchanged by construction --
# there is no R-side fill whose gaps could zero a technology's capital cost.

test_that("no payback leaves the parameter empty and the model unchanged", {
  skip_if_no_solver()
  sc <- rt_interp(rt_model(name = "p0", discount = 0.05), "p0")
  expect_length(rt_val(sc, "pTechPayback"), 0L)
  expect_equal(rt_val(sc, "pTechEac"), round(1000 * rt_crf(0.05, 40), 6))
})

test_that("payback shortens the annuity period", {
  skip_if_no_solver()
  sc <- rt_interp(rt_model(
    invcost = data.frame(invcost = 1000, payback = 10),
    name = "p1", discount = 0.05), "p1")
  expect_equal(rt_val(sc, "pTechPayback"), 10)
  expect_equal(rt_val(sc, "pTechEac"), round(1000 * rt_crf(0.05, 10), 6))
  # Strictly more per year than recovering over the full 40-year life.
  expect_gt(rt_val(sc, "pTechEac"), 1000 * rt_crf(0.05, 40))
})

test_that("the annuity stops at the end of the window, the capacity does not", {
  skip_if_no_solver()
  # `build_first_only` forbids new capacity after 2020, so every unit is the
  # 2020 vintage and the charging window is readable straight off vTechEac.
  # Milestones are 2020, 2025, 2035, 2045, 2051.
  pb <- rt_solve(rt_interp(rt_model(
    invcost = data.frame(invcost = 1000, payback = 10),
    build_first_only = TRUE, name = "p2", discount = 0.05), "p2"))
  base <- rt_solve(rt_interp(rt_model(
    build_first_only = TRUE, name = "p3", discount = 0.05), "p3"))

  eac_pb <- rt_by_year(pb, "vTechEac")
  eac_bs <- rt_by_year(base, "vTechEac")
  # Without payback the annuity runs the whole 40-year life: all 5 milestones.
  expect_setequal(names(eac_bs), c("2020", "2025", "2035", "2045", "2051"))
  expect_true(all(eac_bs > 0))
  # With a 10-year payback only the milestones inside the window are charged.
  expect_setequal(names(eac_pb), c("2020", "2025"))

  # ... while the capacity itself is untouched: same in every year, and present
  # long after the annuity has stopped.
  cap_pb <- rt_by_year(pb, "vTechCap")
  expect_setequal(names(cap_pb), c("2020", "2025", "2035", "2045", "2051"))
  expect_true(all(cap_pb > 0))
  expect_equal(unname(cap_pb), unname(rt_by_year(base, "vTechCap")))

  # Per charged year the payback annuity is the 10-year CRF, not the 40-year one.
  expect_equal(unname(eac_pb["2020"] / cap_pb["2020"]),
               round(1000 * rt_crf(0.05, 10), 4), tolerance = 1e-4)
  expect_equal(unname(eac_bs["2020"] / cap_pb["2020"]),
               round(1000 * rt_crf(0.05, 40), 4), tolerance = 1e-4)
})

test_that("payback must be positive and no longer than olife", {
  expect_error(
    rt_interp(rt_model(invcost = data.frame(invcost = 1000, payback = 50),
                       olife = 40L, name = "p4", discount = 0.05), "p4"),
    "cannot exceed `olife`")
  expect_error(
    rt_interp(rt_model(invcost = data.frame(invcost = 1000, payback = -5),
                       name = "p5", discount = 0.05), "p5"),
    "must be positive")
})

test_that("a payback of zero reads as unset, not as an instant recovery", {
  skip_if_no_solver()
  # 0 is the parameter default, so it cannot mean "recover immediately"; it is
  # treated as absent and the annuity falls back to the operational life.
  sc <- rt_interp(rt_model(invcost = data.frame(invcost = 1000, payback = 0),
                           name = "p6", discount = 0.05), "p6")
  expect_equal(rt_val(sc, "pTechEac"), round(1000 * rt_crf(0.05, 40), 6))
})

test_that("a payback off the milestone grid is reported, not silently rounded", {
  skip_if_no_solver()
  # Periods here are 1, 10, 10, 10, 9 years, so a 10-year payback from the 2020
  # vintage is charged over 1 + 10 = 11 years. Saying so is the whole point:
  # otherwise it looks like an exact recovery.
  expect_message(
    interpolate_model(rt_model(
      invcost = data.frame(invcost = 1000, payback = 10),
      name = "p7", discount = 0.05), name = "p7", fold = TRUE, verbose = FALSE),
    "does not land on a milestone boundary")
})

test_that("payback is refused by the engines that do not implement it", {
  skip_if_no_solver()
  sc <- rt_interp(rt_model(invcost = data.frame(invcost = 1000, payback = 10),
                           name = "p8", discount = 0.05), "p8")
  guard <- getFromNamespace(".assert_payback_supported", "energyRt")
  expect_error(guard(sc, "GAMS"), "GLPK only")
  # ... and accepted where there is none set.
  ok <- rt_interp(rt_model(name = "p9", discount = 0.05), "p9")
  expect_null(guard(ok, "GAMS"))
})

test_that("payback reaches storage and trade, and their equations compile", {
  skip_if_no_solver()
  # eqStorageEac and eqTradeEac carry their own copy of the window condition, so
  # each needs its own exercise: a malformed MathProg condition does not produce
  # a wrong answer, it fails to compile. Solving is the assertion.
  BATT <- newStorage(
    "BATT", commodity = "ELC",
    invcost = data.frame(invcost = 300, payback = 5),
    vintage = data.frame(olife = 15L), cap2stg = 4)
  TBD <- newTrade(
    "TBD_ELC", commodity = "ELC",
    routes = data.frame(src = c("R1", "R2"), dst = c("R2", "R1")),
    invcost = data.frame(invcost = 200, payback = 5),
    vintage = data.frame(olife = 20L), cap2act = 1)

  sc <- vt_interp(vt_model(BATT, TBD, name = "pbst"), "pbst")
  expect_equal(unique(rt_val(sc, "pStoragePayback")), 5)
  expect_equal(unique(rt_val(sc, "pTradePayback")), 5)
  expect_equal(rt_val(sc, "pStorageEac"), round(300 * rt_crf(0.05, 5), 6))
  expect_equal(rt_val(sc, "pTradeEac"), round(200 * rt_crf(0.05, 5), 6))

  sol <- vt_solve(sc)
  expect_equal(sol@modOut@stage, "solved")

  # Both classes validate the same way as technology.
  expect_error(vt_interp(vt_model(
    newStorage("BATT2", commodity = "ELC",
               invcost = data.frame(invcost = 300, payback = 40),
               vintage = data.frame(olife = 15L), cap2stg = 4),
    name = "pbs2"), "pbs2"), "cannot exceed `olife`")
})
