# =========================================================================== #
# Feature family: storage-aux-caps (PER-PART aux capacity couplings).
#
# A storage has three capacities; each `<part>.cap2a*` / `<part>.ncap2a*`
# multiplies its OWN part's capacity variable. The pre-part-aware bare names
# (`cap2ainp` ...) coupled only the discharger and are REFUSED with a rename
# hint. A coupling on a part with no capacity variable (not priced/bounded)
# emits no map rows -- dropped with a warning at interpolation.
#
# Shift model (supply at s1 only, cost 1; demand 10 at s3), WAT aux at cost 2,
# storing part priced (stg.invcost 30, olife 30, discount 0 -> reservoir EAC
# 1/unit; level 10 held s1->s3 -> StgCap = StgNewCap = 10):
#   base                                   obj = 10 + 10          = 20
#   stg.ncap2ainp 0.1 @ s1: aux 0.1*10=1,  obj = 20 + 2*1         = 22
#   stg.cap2ainp 0.05 @ s2: aux 0.5,       obj = 20 + 2*0.5       = 21
# (couplings name ONE timeslice: the term carries no share, so a wildcard
# would fire in every slice -- the documented aeff timeslice warning.)
# =========================================================================== #

sc_slices <- paste0("s", 1:4)

sc_build <- function(aeff = data.frame(), stg_priced = TRUE) {
  invcost <- if (stg_priced) data.frame(stg.invcost = 30) else
    data.frame(out.invcost = 0)
  newModel("sc",
    repo = newRepository("sc_repo",
      newCommodity("ELC", timeframe = "SL"),
      newCommodity("WAT", timeframe = "SL"),
      newSupply("SUP", commodity = "ELC",
                supply = data.frame(timeslice = sc_slices, cost = 1,
                                    ava.up = c(1000, 0, 0, 0))),
      newSupply("SWAT", commodity = "WAT", supply = data.frame(cost = 2)),
      newStorage("STG", commodity = "ELC", olife = data.frame(olife = 30),
                 invcost = invcost,
                 aux = data.frame(acomm = "WAT"), aeff = aeff),
      newDemand("DEM", commodity = "ELC",
                demand = data.frame(timeslice = "s3", demand = 10))),
    calendar = newCalendar(
      timetable = make_timetable(struct = list(ANNUAL = "ANNUAL", SL = sc_slices)),
      name = "sc_cal"),
    region = "R1", horizon = newHorizon(2025), discount = 0)
}

test_that("family storage-aux-caps: the bare legacy names are refused with a hint", {
  expect_error(
    newStorage("S_LEG", commodity = "ELC",
               aux = data.frame(acomm = "WAT"),
               aeff = data.frame(acomm = "WAT", cap2ainp = 0.1)),
    "out\\.cap2ainp")
  expect_error(
    newStorage("S_LEG2", commodity = "ELC",
               aux = data.frame(acomm = "WAT"),
               aeff = data.frame(acomm = "WAT", ncap2aout = 0.1)),
    "renamed")
})

# @covers pStorageStgNCap2AInp pStorageStgCap2AInp depth=I backends=glpk
test_that("family storage-aux-caps: stg couplings land iff the part is materialised", {
  scen <- suppressMessages(suppressWarnings(interpolate_model(
    sc_build(data.frame(acomm = "WAT", timeslice = "s1", stg.ncap2ainp = 0.1)),
    name = "sc_i", overwrite = TRUE)))
  expect_equal(unique(ff_param(scen, "pStorageStgNCap2AInp")$value), 0.1)
  expect_gte(nrow(ff_param(scen, "mStorageStgNCap2AInp")), 1)
  # the storing part unpriced: no vStorageStgCap, so the coupling is dropped
  # (with a warning) and no map rows are emitted
  expect_warning(
    scen2 <- suppressMessages(interpolate_model(
      sc_build(data.frame(acomm = "WAT", timeslice = "s1",
                          stg.ncap2ainp = 0.1),
               stg_priced = FALSE),
      name = "sc_i2", overwrite = TRUE)),
    "no capacity variable")
  expect_equal(nrow(ff_param(scen2, "mStorageStgNCap2AInp")), 0)
})

# @covers pStorageStgNCap2AInp vStorageAInp eqStorageAInp depth=S backends=glpk
test_that("family storage-aux-caps: build-time material scales with the reservoir", {
  skip_if_no_solver()
  base <- suppressMessages(suppressWarnings(interpolate_model(
    sc_build(), name = "sc_base", overwrite = TRUE)))
  base <- .fork_solve(base, solver_options$glpk)
  expect_true(verify_solution(base)$ok)
  expect_equal(ff_solution_sum(base, "vStorageStgCap"), 10, tolerance = 1e-6)
  expect_equal(.fork_objective(base), 20, tolerance = 1e-6)

  scen <- suppressMessages(suppressWarnings(interpolate_model(
    sc_build(data.frame(acomm = "WAT", timeslice = "s1", stg.ncap2ainp = 0.1)),
    name = "sc_ncap", overwrite = TRUE)))
  scen <- .fork_solve(scen, solver_options$glpk)
  expect_true(verify_solution(scen)$ok)
  expect_equal(ff_solution_sum(scen, "vStorageAInp"), 1, tolerance = 1e-6)
  expect_equal(.fork_objective(scen), 22, tolerance = 1e-6)
})

# @covers pStorageStgCap2AInp depth=S backends=glpk
test_that("family storage-aux-caps: standing-capacity coupling recurs on the stock", {
  skip_if_no_solver()
  scen <- suppressMessages(suppressWarnings(interpolate_model(
    sc_build(data.frame(acomm = "WAT", timeslice = "s2", stg.cap2ainp = 0.05)),
    name = "sc_cap", overwrite = TRUE)))
  scen <- .fork_solve(scen, solver_options$glpk)
  expect_true(verify_solution(scen)$ok)
  expect_equal(ff_solution_sum(scen, "vStorageAInp"), 0.5, tolerance = 1e-6)
  expect_equal(.fork_objective(scen), 21, tolerance = 1e-6)
})

# @covers eqStorageAInp depth=X backends=julia_highs forks=default
test_that("family storage-aux-caps: the reservoir coupling agrees on julia", {
  skip_if_no_solver()
  skip_if_no_julia_highs()
  scen <- suppressMessages(suppressWarnings(interpolate_model(
    sc_build(data.frame(acomm = "WAT", timeslice = "s1", stg.ncap2ainp = 0.1)),
    name = "sc_jl", overwrite = TRUE)))
  scen <- .fork_solve(scen, solver_options$julia_highs)
  expect_true(verify_solution(scen)$ok)
  expect_equal(.fork_objective(scen), 22, tolerance = 1e-6)
})
