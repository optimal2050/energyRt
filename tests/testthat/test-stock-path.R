# The exogenous stock path: `pXStockNew` / `pXStockSurv` and the recursive
# `eqXStockCap` balance that replaced the cumulative retirement form.

test_that("a fixed fleet is untouched: surv = 1, new only in the first year", {
  skip_if_no_solver()
  sc <- sp_interp(sp_tech(c(100, 100, 100), optret = FALSE, name = "spb1"))
  # Both parameters carry their neutral value as the default and are written
  # sparsely, so a constant fleet leaves `Surv` empty entirely.
  expect_equal(unname(sp_by_year(sc, "pTechStockNew")), c(100, 0, 0))
  expect_length(rt_val(sc, "pTechStockSurv"), 0L)

  sol <- solve_scenario(sc)
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

  sol <- solve_scenario(sc)
  expect_equal(unname(sp_by_year(sol, "vTechStockCap")), c(100, 80, 0))
  # the departures are now TRACKED, not merely implied by a falling stock
  expect_equal(unname(sp_by_year(sol, "vTechStockPhaseOut")), c(0, 20, 80))
})

test_that("a rising schedule is commissioning, not a negative phase-out", {
  skip_if_no_solver()
  sc <- sp_interp(sp_tech(c(50, 80, 120), optret = FALSE, name = "spb3"))
  expect_equal(unname(sp_by_year(sc, "pTechStockNew")), c(50, 30, 40))
  expect_length(rt_val(sc, "pTechStockSurv"), 0L)
  sol <- solve_scenario(sc)
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
  # guard: the fixture's demand must actually BE in the model. Its shape
  # (region-scoped demand + wildcard row) used to interpolate to ZERO pDemand
  # rows, and every assertion below passed on a demand-less LP because the
  # stock variables are exogenous-schedule-driven.
  flows <- suppressMessages(getData(sol, "vTradeIr", merge = TRUE))
  expect_gt(sum(flows$value), 0)
  expect_equal(unname(sp_by_year(sol, "vTradeStockCap")), c(100, 70, 0))
  expect_equal(unname(sp_by_year(sol, "vTradeRetiredStock")), c(0, 10, 0))
  # Trade departures are tracked too, on the same footing as the other two
  # classes: the schedule thins what survives early retirement, so 2022 loses
  # 70, not 80. Identical to the storage case above -- that equality is the
  # point, and it is what "the third class behaves like the other two" means.
  expect_equal(unname(sp_by_year(sol, "vTradeStockPhaseOut")), c(0, 20, 70))
  expect_equal(unname(sp_by_year(sol, "vTradePhaseOut")), c(0, 20, 70))
})

test_that("a trade schedule alone retires the whole fleet", {
  skip_if_no_solver()
  # No early retirement: the schedule accounts for all 100 units.
  sol <- sp_solve(sp_trade(c(100, 80, 0), optret = FALSE, name = "spc4"))
  expect_equal(unname(sp_by_year(sol, "vTradeStockCap")), c(100, 80, 0))
  expect_equal(unname(sp_by_year(sol, "vTradeStockPhaseOut")), c(0, 20, 80))
  # per-year rates on 1-year periods, so they total the fleet exactly
  expect_equal(sum(sp_by_year(sol, "vTradeStockPhaseOut")), 100)
})

test_that("GAMS declares the same variables non-negative as GLPK", {
  # `vTradeRetiredStock` and the storage retirement family were declared in a
  # FREE `variable` block in GAMS while GLPK had them `>= 0`. A free retirement
  # variable can go negative -- un-retiring capacity, i.e. creating it from
  # nothing. Latent (the tested optima never wanted more capacity) but real.
  # Written with fixed-string splits rather than regex: the point is the set
  # comparison, and escaping backslashes through the build is its own hazard.
  code <- getFromNamespace(".modelCode", "energyRt")

  glpk  <- grep("^var v", code[["GLPK"]], value = TRUE)
  parts <- strsplit(sub("^var ", "", glpk), "{", fixed = TRUE)
  gnm   <- vapply(parts, function(x) x[1], "")
  gtail <- vapply(parts, function(x) paste(x[-1], collapse = "{"), "")
  glpk_pos <- gnm[grepl(">=", gtail, fixed = TRUE)]
  expect_gt(length(glpk_pos), 50)

  gams <- code[["GAMS"]]
  blk  <- character(length(gams))
  cur  <- NA_character_
  for (i in seq_along(gams)) {
    t <- tolower(trimws(gams[i]))
    if (t %in% c("positive variables", "positive variable")) cur <- "pos"
    else if (t %in% c("variables", "variable",
                      "free variables", "free variable")) cur <- "free"
    else if (t == ";") cur <- NA_character_
    blk[i] <- cur
  }
  isvar <- startsWith(gams, "v") & grepl("(", gams, fixed = TRUE) & !is.na(blk)
  anm   <- vapply(strsplit(gams, "(", fixed = TRUE), function(x) x[1], "")
  gams_free <- unique(anm[isvar & blk == "free"])

  expect_setequal(intersect(glpk_pos, gams_free), character())
})
