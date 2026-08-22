# The exogenous stock path: `pXStockNew` / `pXStockSurv` and the recursive
# `eqXStockCap` balance that replaced the cumulative retirement form.

test_that("a fixed fleet is untouched: surv = 1, new only in the first year", {
  skip_if_no_solver()
  sc <- sp_interp(sp_tech(c(100, 100, 100), optret = FALSE, name = "spb1"))
  # Both parameters carry their neutral value as the default and are written
  # sparsely, so a constant fleet leaves `Surv` empty entirely.
  expect_equal(unname(sp_by_year(sc, "pTechStockNew")), c(100, 0, 0))
  expect_length(rt_val(sc, "pTechStockSurv"), 0L)

  sol <- solve_scen(sc)
  # ... and the balance reproduces the declared stock exactly, which is what
  # backward compatibility means here.
  expect_equal(unname(sp_by_year(sol, "vTechStockCap")), c(100, 100, 100))
  expect_equal(unname(sp_by_year(sol, "vTechStockPhaseOut")), c(0, 0, 0))
})

test_that("a declining schedule is read off consecutive milestones", {
  skip_if_no_solver()
  # 100 -> 80 -> 0.  surv(2021) = 80/100, surv(2022) = 0/80.  Using only
  # neighbouring years is what makes both parameters invariant under a change
  # of base year; a share of the FIRST year's stock would not be.
  sc <- sp_interp(sp_tech(c(100, 80, 0), optret = FALSE, name = "spb2"))
  expect_equal(unname(sp_by_year(sc, "pTechStockNew")),  c(100, 0, 0))
  expect_equal(unname(sp_by_year(sc, "pTechStockSurv")), c(0, 0.8, 0))

  sol <- solve_scen(sc)
  expect_equal(unname(sp_by_year(sol, "vTechStockCap")), c(100, 80, 0))
  # the departures are now TRACKED, not merely implied by a falling stock
  expect_equal(unname(sp_by_year(sol, "vTechStockPhaseOut")), c(0, 20, 80))
})

test_that("a rising schedule is commissioning, not a negative phase-out", {
  skip_if_no_solver()
  sc <- sp_interp(sp_tech(c(50, 80, 120), optret = FALSE, name = "spb3"))
  expect_equal(unname(sp_by_year(sc, "pTechStockNew")), c(50, 30, 40))
  expect_length(rt_val(sc, "pTechStockSurv"), 0L)
  sol <- solve_scen(sc)
  expect_equal(unname(sp_by_year(sol, "vTechStockCap")), c(50, 80, 120))
  expect_equal(unname(sp_by_year(sol, "vTechStockPhaseOut")), c(0, 0, 0))
})

test_that("a declining schedule and early retirement now coexist", {
  skip_if_no_solver()
  # THE regression this redesign exists for. Under the cumulative form
  # `retiredCum(y) <= stock(y)` with a non-decreasing cum, stock(2022) = 0
  # forced retiredCum to zero in every year, so `ret.fx` was infeasible.
  # Now the schedule thins whatever is actually standing:
  #   2021: 0.8 * 100 - 10 = 70,  and 20 + 10 = 30 units leave that year.
  sol <- sp_solve(sp_tech(c(100, 80, 0), ret = c(0, 10, 0), name = "spc1"))
  expect_equal(unname(sp_by_year(sol, "vTechStockCap")), c(100, 70, 0))
  expect_equal(unname(sp_by_year(sol, "vTechRetiredStock")), c(0, 10, 0))
  # phase-out thins the POST-retirement fleet, so 2022 loses 70, not 80
  expect_equal(unname(sp_by_year(sol, "vTechStockPhaseOut")), c(0, 20, 70))
})

test_that("storage carries the path on every part", {
  skip_if_no_solver()
  sol <- sp_solve(sp_storage(c(100, 80, 0), ret = c(0, 10, 0), name = "spc2"))
  expect_equal(unname(sp_by_year(sol, "vStorageOutStockCap")), c(100, 70, 0))
  expect_equal(unname(sp_by_year(sol, "vStorageInpStockCap")), c(100, 70, 0))
  # the reservoir is four hours of power, and stays so through the whole path
  expect_equal(unname(sp_by_year(sol, "vStorageStgStockCap")), c(400, 280, 0))
  # phase-out is a storage-level quantity, rated on the discharging part -- the
  # same convention as vStoragePhaseOut and the pho2a* coefficients
  expect_equal(unname(sp_by_year(sol, "vStorageStockPhaseOut")), c(0, 20, 70))
})

test_that("trade carries the path on region-free capacity", {
  skip_if_no_solver()
  sol <- sp_solve(sp_trade(c(100, 80, 0), ret = c(0, 10, 0), name = "spc3"))
  expect_equal(unname(sp_by_year(sol, "vTradeStockCap")), c(100, 70, 0))
  expect_equal(unname(sp_by_year(sol, "vTradeRetiredStock")), c(0, 10, 0))
})

test_that("the cumulative retirement form is gone from every template", {
  # `vXRetiredStockCum` and its two equations are what made requirements 1 and 2
  # mutually exclusive. A template that still carries them would keep solving,
  # and keep being wrong, so assert their absence directly.
  code <- getFromNamespace(".modelCode", "energyRt")
  engines <- c("GLPK", "GAMS", "GAMS_output", "JuMP", "JuMPOutput",
               "PYOMOConcrete", "PYOMOConcreteOutput")
  # Names, not just contents: a typo here would make the loop assert nothing.
  expect_true(all(engines %in% names(code)))
  for (eng in engines) {
    expect_false(grepl("RetiredStockCum", paste(code[[eng]], collapse = "\n"),
                       fixed = TRUE), info = eng)
  }
})

test_that("every backend agrees on the schedule-plus-retirement case", {
  skip_if_no_solver()
  # The combination is new, so backend parity has to be re-established for it.
  # GAMS is excluded: it needs a dense scenario and a NEOS submission.
  sc <- sp_interp(sp_tech(c(100, 80, 0), ret = c(0, 10, 0), name = "spd1"))
  ref <- getData(solve_scen(sc), "vObjective", merge = TRUE)$value[1]
  have <- function(f) { p <- tryCatch(f(), error = function(e) NULL)
                        !is.null(p) && nzchar(p) }
  engines <- c(if (have(get_julia_path))  "julia_highs",
               if (have(get_python_path)) "pyomo_glpk")
  for (eng in engines) {
    sol <- solve_scen(sc, solver = solver_options[[eng]])
    expect_equal(getData(sol, "vObjective", merge = TRUE)$value[1], ref,
                 tolerance = 1e-6, info = eng)
    expect_equal(unname(sp_by_year(sol, "vTechStockCap")), c(100, 70, 0),
                 info = eng)
  }
})
