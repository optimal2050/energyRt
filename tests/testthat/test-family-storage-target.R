# =========================================================================== #
# Feature family: storage-target (soft floors on the storage level, by tier).
#
#   eqStorageTarget[tier]: vStorageLevel + vStorageShort[tier] >= target[tier] * capacity
#   eqStorageShortCost:    vStorageShortCost = sum(cost[tier] * weight * vStorageShort[tier])
#
# `storage@target` rows are tiers: `target` is a share of storing capacity (the
# base `af.lo` uses) and `cost` prices each unit short, on the annual basis of
# `stgcost`. Below several targets the prices add up, so a deeper tier makes
# draining dearer the further it goes. A missing or NA `tier` is the tier "1".
# No target, no rows: a model without one is unchanged.
#
# Fixture: a 10-unit reservoir over eight equal slices, cheap supply only in
# s1-s2, demand of 3 in s3-s8. Unconstrained, the store empties (objective
# 810); a hard floor `af.lo = 0.5` holds 5 throughout (objective 1305).
# =========================================================================== #

st_slices <- paste0("s", 1:8)

st_build <- function(target = data.frame(), af = data.frame()) {
  newModel("st",
    repo = newRepository("st_repo",
      newCommodity("ELC", timeframe = "SL"),
      newSupply("SUP", commodity = "ELC",
                supply = data.frame(timeslice = st_slices, ava.up = 1000,
                                    cost = c(1, 1, rep(100, 6)))),
      newStorage("STG", commodity = "ELC",
                 input = list(comm = "ELC", cap2act = 1),
                 output = list(comm = "ELC", cap2act = 1),
                 olife = data.frame(olife = 30),
                 capacity = data.frame(stg.cap.fx = 10, inp.cap.fx = 80,
                                       out.cap.fx = 80),
                 invcost = data.frame(stg.invcost = 0),
                 target = target, af = af,
                 seff = data.frame(inpeff = 1, outeff = 1)),
      newDemand("DEM", commodity = "ELC",
                demand = data.frame(timeslice = st_slices[3:8], demand = 3))),
    calendar = newCalendar(
      timetable = make_timetable(struct = list(ANNUAL = "ANNUAL", SL = st_slices)),
      name = "st_cal"),
    region = "R1", horizon = newHorizon(2030), discount = 0)
}

st_interp <- function(nm, ...) suppressMessages(suppressWarnings(
  interpolate_model(st_build(...), name = nm, overwrite = TRUE)))

st_rows <- function(scen, nm) {
  d <- ff_param(scen, nm)
  if (is.null(d)) 0L else nrow(d)
}

st_var <- function(scen, nm) {
  v <- scen@modOut@variables[[nm]]
  d <- if (is.null(v)) NULL else get_data_slot(v, optional = TRUE)
  if (is.null(d)) data.frame(value = numeric()) else as.data.frame(d)
}

# the level in every slice: a zero level is not written by the solver
st_level <- function(scen) {
  lv <- st_var(scen, "vStorageLevel")
  out <- stats::setNames(rep(0, length(st_slices)), st_slices)
  if (nrow(lv)) out[as.character(lv$timeslice)] <- lv$value
  out
}

st_vs_target <- function(scen) {
  verify_solution(scen, checks = "storage_target")$checks$storage_target$status
}

st_one <- function(cost) data.frame(target = 0.5, cost = cost)
# a unit drained saves 99 (100 bought vs 1 stored), so below 0.3 the stacked
# price (1 + 200 per slice) is never worth paying, while above it (1) always is
st_two <- data.frame(tier = c("t1", "t2"), target = c(0.6, 0.3), cost = c(1, 200))

# @covers pStorageTarget pStorageTargetCost meqStorageTarget mStorageShortCost depth=I backends=glpk
test_that("family storage-target: no target, no rows", {
  scen <- st_interp("st_none")
  expect_equal(st_rows(scen, "meqStorageTarget"), 0L)
  expect_equal(st_rows(scen, "mStorageShortCost"), 0L)
  expect_length(scen@modInp@sets$tier, 0L)
})

# @covers pStorageTarget meqStorageTarget mStorageShortCost depth=I backends=glpk
test_that("family storage-target: a target builds one row per tier and level cell", {
  scen <- st_interp("st_rows", target = st_one(50))
  expect_equal(st_rows(scen, "meqStorageTarget"), length(st_slices))
  expect_equal(st_rows(scen, "mStorageShortCost"), 1L)
  # an untiered row is the tier "1"
  expect_equal(scen@modInp@sets$tier, "1")
  expect_equal(unique(as.character(ff_param(scen, "meqStorageTarget")$tier)), "1")
  # two tiers, two rows per cell
  scen <- st_interp("st_rows2", target = st_two)
  expect_equal(st_rows(scen, "meqStorageTarget"), 2L * length(st_slices))
  expect_setequal(scen@modInp@sets$tier, c("t1", "t2"))
  # an end-of-season target names its slice
  scen <- st_interp("st_rows_s8",
                    target = data.frame(timeslice = "s8", target = 0.5, cost = 50))
  expect_equal(as.character(ff_param(scen, "meqStorageTarget")$timeslice), "s8")
})

# @covers pStorageTarget depth=I backends=glpk
test_that("family storage-target: a tier without a price warns", {
  expect_warning(
    suppressMessages(interpolate_model(
      st_build(target = data.frame(tier = c("t1", "t2"), target = c(0.6, 0.3),
                                   cost = c(10, NA))),
      name = "st_noprice", overwrite = TRUE)),
    "t2")
})

# @covers vObjective depth=S backends=glpk
test_that("family storage-target: without a target the model is unchanged", {
  skip_if_no_solver()
  scen <- .fork_solve(st_interp("st_base"), solver_options$glpk)
  expect_equal(.fork_objective(scen), 810, tolerance = 1e-6)
})

# @covers eqStorageTarget vStorageShort vStorageShortCost depth=S backends=glpk
test_that("family storage-target: a high price holds the target", {
  skip_if_no_solver()
  scen <- .fork_solve(st_interp("st_high", target = st_one(1000)),
                      solver_options$glpk)
  expect_true(verify_solution(scen)$ok)
  expect_equal(st_vs_target(scen), "ok")
  expect_gte(min(st_level(scen)), 5 - 1e-6)
  expect_equal(sum(st_var(scen, "vStorageShort")$value), 0, tolerance = 1e-9)
  # the same as the hard floor at that level
  expect_equal(.fork_objective(scen), 1305, tolerance = 1e-6)
})

# @covers eqStorageShortCost vStorageShortCost eqCost depth=S backends=glpk
test_that("family storage-target: a low price is paid for the shortfall", {
  skip_if_no_solver()
  scen <- .fork_solve(st_interp("st_low", target = st_one(1)),
                      solver_options$glpk)
  expect_true(verify_solution(scen)$ok)
  expect_equal(st_vs_target(scen), "ok")
  sh <- st_var(scen, "vStorageShort")
  expect_gt(sum(sh$value), 0)
  # pTimesliceWeight is 1 on this calendar
  expect_equal(sum(st_var(scen, "vStorageShortCost")$value), sum(sh$value),
               tolerance = 1e-6)
  obj <- .fork_objective(scen)
  expect_gt(obj, 810)
  expect_lt(obj, 1305)
})

# @covers eqStorageTarget eqStorageShortCost vStorageShort depth=S backends=glpk
test_that("family storage-target: tier prices stack, so a deeper drain costs more", {
  skip_if_no_solver()
  one <- .fork_solve(st_interp("st_t1", target = st_two[1, ]), solver_options$glpk)
  two <- .fork_solve(st_interp("st_t12", target = st_two), solver_options$glpk)
  expect_true(verify_solution(two)$ok)
  expect_equal(st_vs_target(two), "ok")
  sh <- st_var(two, "vStorageShort")
  # the cost line is the price-weighted sum over both tiers
  price <- c(t1 = 1, t2 = 200)[as.character(sh$tier)]
  expect_equal(sum(st_var(two, "vStorageShortCost")$value), sum(price * sh$value),
               tolerance = 1e-6)
  # the deep tier is dear enough that the level never drops below it ...
  expect_gte(min(st_level(two)), 3 - 1e-6)
  expect_equal(sum(sh$value[sh$tier == "t2"]), 0, tolerance = 1e-9)
  # ... while the cheap tier alone lets it drain further
  expect_lt(min(st_level(one)), 3 - 1e-6)
  expect_gt(.fork_objective(two), .fork_objective(one))
})

# @covers eqStorageTarget vStorageShort depth=S backends=glpk
test_that("family storage-target: an unattainable target is a cost, not an infeasibility", {
  skip_if_no_solver()
  scen <- .fork_solve(st_interp("st_over",
                                target = data.frame(target = 1.2, cost = 1000)),
                      solver_options$glpk)
  # the level cannot pass capacity (10), so each slice is at least 2 short
  sh <- st_var(scen, "vStorageShort")
  expect_equal(nrow(sh), length(st_slices))
  expect_gte(min(sh$value), 2 - 1e-6)
  expect_false(is.na(.fork_objective(scen)))
})

# @covers vStorageShort depth=S backends=glpk
test_that("family storage-target: a coarser timeframe reads the closing shortfall", {
  skip_if_no_solver()
  scen <- .fork_solve(st_interp("st_close", target = st_one(1)),
                      solver_options$glpk)
  fine <- as.data.frame(getData(scen, "vStorageShort", merge = TRUE))
  coarse <- as.data.frame(getData(scen, "vStorageShort", merge = TRUE,
                                  timeframe = "lowest"))
  last <- fine$value[as.character(fine$timeslice) == "s8"]
  if (!length(last)) last <- 0
  expect_equal(sum(coarse$value), last, tolerance = 1e-9)
})

# @covers eqStorageTarget eqStorageShortCost depth=X backends=julia_highs,pyomo_cbc,mm_highs
test_that("family storage-target: the backends agree on tiered shortfalls", {
  skip_if_no_solver()
  skip_if_tier_below("cross")
  ref <- .fork_objective(.fork_solve(st_interp("st_x_glpk", target = st_two),
                                     solver_options$glpk))
  for (b in c("julia_highs", "pyomo_cbc", "mm_highs")) {
    ok <- switch(b,
      julia_highs = has_julia(), pyomo_cbc = has_python(), mm_highs = has_multimod())
    if (!isTRUE(ok)) next
    scen <- .fork_solve(st_interp(paste0("st_x_", b), target = st_two),
                        solver_options[[b]])
    expect_equal(.fork_objective(scen), ref, tolerance = 1e-6, label = b)
  }
})
