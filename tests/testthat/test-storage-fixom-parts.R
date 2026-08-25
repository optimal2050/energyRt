# =========================================================================== #
# Fixed O&M on a storage part must reach the objective.
#
# `eqStorageFixom` sums all three parts into one `vStorageFixom`, with the
# storing and charging terms as guarded sums INSIDE the equation. Its outer
# domain `mStorageFixom` was sourced from `pStorageOutFixom` alone, so a storage
# priced only on its reservoir got no equation at all and its fixed O&M left the
# objective silently, with an OPTIMAL solve.
#
# `out.fixom = 0` was not a workaround either: zero is the parameter default, so
# it contributes no data and the domain stays empty. Only a NON-ZERO out.fixom
# rescued the row -- which also prices the discharge capacity, so there was no
# clean way to charge a reservoir for its upkeep.
#
# Same family as test-storage-eac-parts.R, one level up: there a PART's capacity
# domain was missing a source, here the EQUATION's domain was.
#
# Found while checking whether a storage can be a pure accumulator (reservoir
# sized, no power decision), which is exactly the shape that declares `stg.fixom`
# and has no reason to declare `out.fixom`.
# =========================================================================== #

sf_slices <- paste0("s", 1:4)

# Supply only in s1, demand of 10 in s3: the storage must carry the energy, so
# its reservoir is pinned at 10 and the fixed O&M owed is exactly 5 * 10 = 50.
sf_build <- function(fixom, name = "SF") {
  newModel(name,
    repo = newRepository(paste0(name, "_repo"),
      newCommodity("ELC", timeframe = "SL"),
      newSupply("SUP", commodity = "ELC",
                supply = data.frame(timeslice = sf_slices, cost = 1,
                                    ava.up = c(1000, 0, 0, 0))),
      newStorage("STG", commodity = "ELC",
                 olife = data.frame(olife = 30),
                 invcost = data.frame(stg.eac = 10),
                 duration = data.frame(duration.lo = 0),
                 fixom = fixom),
      newDemand("DEM", commodity = "ELC",
                demand = data.frame(timeslice = "s3", demand = 10))),
    calendar = newCalendar(
      timetable = make_timetable(struct = list(ANNUAL = "ANNUAL", SL = sf_slices)),
      name = paste0(name, "_cal")),
    region = "R1", horizon = newHorizon(2025), discount = 0)
}

sf_solve <- function(fixom, nm) {
  scen <- suppressMessages(suppressWarnings(solve_model(
    sf_build(fixom), name = nm, solver = solver_options$glpk, overwrite = TRUE)))
  getData(scen, "vObjective", merge = TRUE)$value
}

# @covers mStorageFixom depth=I backends=glpk
test_that("fixed O&M on any storage part instantiates the fixom equation", {
  for (col in c("out.fixom", "inp.fixom", "stg.fixom")) {
    fx <- data.frame(5); names(fx) <- col
    scen <- suppressMessages(suppressWarnings(interpolate_model(
      sf_build(fx), name = paste0("sf_i_", sub("[.]", "_", col)),
      overwrite = TRUE)))
    expect_gte(nrow(ff_param(scen, "mStorageFixom")), 1,
               label = paste("mStorageFixom rows with", col))
  }
})

# @covers mStorageFixom depth=S backends=glpk
test_that("a reservoir pays its own fixed O&M without an out-side price", {
  skip_if_no_solver()
  base <- sf_solve(data.frame(), "sf_base")
  expect_equal(base, 110, tolerance = 1e-9)   # supply 10 + reservoir 10 * 10

  # The reservoir is 10 units and stg.fixom is 5, so 50 is owed. Before the fix
  # this returned the bare 110: no equation, no charge, no warning.
  expect_equal(sf_solve(data.frame(stg.fixom = 5), "sf_stg"), base + 50,
               tolerance = 1e-9)

  # `out.fixom = 0` must behave exactly like declaring nothing on the out side --
  # it neither rescues nor suppresses the storing-side charge.
  expect_equal(sf_solve(data.frame(stg.fixom = 5, out.fixom = 0), "sf_stg0"),
               base + 50, tolerance = 1e-9)
})

# @covers mStorageFixom depth=S backends=glpk
test_that("the three parts are charged independently and additively", {
  skip_if_no_solver()
  base <- sf_solve(data.frame(), "sf_base2")
  # The accumulator has no discharge capacity, so an out-side rate costs nothing.
  expect_equal(sf_solve(data.frame(out.fixom = 1), "sf_out"), base,
               tolerance = 1e-9)
  # Adding it alongside the storing rate must not change the storing charge.
  expect_equal(sf_solve(data.frame(stg.fixom = 5, out.fixom = 1), "sf_both"),
               base + 50, tolerance = 1e-9)
})
