# =========================================================================== #
# A storage part exists wherever the OBJECT declares a column on it.
#
# The charger and the discharger are "structure follows data", and the data
# used to be read from the interpolated parameters -- AFTER the default-value
# rule had removed every row equal to its parameter default. So
# `inp.invcost = 0` and `inp.cap.up = Inf`, both deliberate declarations,
# created no part, and `interpolate_model(sparse = )` decided which parts a
# model had rather than how many rows it carried.
#
# Since 2026-11-18 the existence test reads the object: any `inp.` / `out.`
# column in any slot that carries one, plus the ratio links, whose two ends are
# both parts. `comm` / `unit` / `cap2act` are excluded -- the constructor fills
# `cap2act` on every part. A part nothing SIZES (declared only by an `af`, a
# weather factor, a flow cost, an aux coupling, a retirement bound or an
# annuity rate) is sized freely by the LP, which `validate_scenario_parameters()`
# reports as `unsized_part`.
# =========================================================================== #

pr_slices <- paste0("s", 1:4)

pr_build <- function(storage_args = list(), regions = "R1",
                     periods = 2025L, ...) {
  stg <- do.call(newStorage, c(list(
    name = "STG", commodity = "ELC",
    input = list(comm = "ELC", cap2act = 1),
    output = list(comm = "ELC", cap2act = 1),
    olife = data.frame(olife = 30)), storage_args))
  newModel("pr",
    repo = newRepository("pr_repo",
      newCommodity("ELC", timeframe = "SL"),
      newSupply("SUP", commodity = "ELC",
                supply = data.frame(timeslice = pr_slices, ava.up = 1000,
                                    cost = c(1, 100, 100, 100))),
      stg,
      newDemand("DEM", commodity = "ELC",
                demand = data.frame(timeslice = "s3", demand = 8))),
    calendar = newCalendar(timetable = make_timetable(
      struct = list(ANNUAL = "ANNUAL", SL = pr_slices)), name = "pr_cal"),
    region = regions, horizon = newHorizon(period = periods), discount = 0)
}

pr_interp <- function(nm, storage_args, regions = "R1", periods = 2025L, ...)
  suppressMessages(suppressWarnings(interpolate_model(
    pr_build(storage_args, regions = regions, periods = periods),
    name = nm, overwrite = TRUE, ...)))

pr_rows <- function(scen, map) {
  d <- tryCatch(energyRt:::.gds(scen, map), error = function(e) NULL)
  if (is.null(d)) 0L else nrow(d)
}
pr_unsized <- function(scen, map) {
  d <- validate_scenario_parameters(scen, action = "silent")
  if (is.null(d) || !NROW(d)) return(0L)
  d <- as.data.frame(d)
  nrow(d[d$check == "unsized_part" & d$parameter == map, , drop = FALSE])
}

# the reservoir is the anchor, so every case below needs it determined
pr_rsv <- data.frame(stg.invcost = 0)

# @covers mStorageInpCap mStorageOutCap depth=I backends=glpk
test_that("storage-part-rule: a part with nothing declared on it does not exist", {
  scen <- pr_interp("pr_none", list(invcost = pr_rsv))
  expect_equal(pr_rows(scen, "mStorageInpCap"), 0L)
  expect_equal(pr_rows(scen, "mStorageOutCap"), 0L)
  expect_equal(pr_unsized(scen, "mStorageInpCap"), 0L)
})

# @covers mStorageInpCap depth=I backends=glpk
test_that("storage-part-rule: a declared ZERO price creates the part", {
  # `inp.invcost = 0` is a deliberately free charger. Its parameter row equals
  # the default and is removed before any map is built, so only the object can
  # say it was written.
  scen <- pr_interp("pr_zero",
                    list(invcost = data.frame(stg.invcost = 0, inp.invcost = 0)))
  expect_gte(pr_rows(scen, "mStorageInpCap"), 1)
  # a price SIZES the part, even at zero: no advisory
  expect_equal(pr_unsized(scen, "mStorageInpCap"), 0L)
})

# @covers mStorageInpCap depth=I backends=glpk
test_that("storage-part-rule: a declared INFINITE bound creates the part", {
  scen <- pr_interp("pr_inf",
                    list(invcost = pr_rsv,
                         capacity = data.frame(inp.cap.up = Inf)))
  expect_gte(pr_rows(scen, "mStorageInpCap"), 1)
  expect_equal(pr_unsized(scen, "mStorageInpCap"), 0L)
})

# @covers mStorageInpCap mStorageOutCap depth=I backends=glpk
test_that("storage-part-rule: `sparse` no longer decides which parts exist", {
  # the regression this rule exists for: the default-value drop is a SIZE
  # setting, and must not change the model's structure
  args <- list(invcost = data.frame(stg.invcost = 0, inp.invcost = 0),
               capacity = data.frame(out.cap.up = Inf))
  a <- pr_interp("pr_sparse_t", args, sparse = TRUE)
  b <- pr_interp("pr_sparse_f", args, sparse = FALSE)
  for (m in c("mStorageInpCap", "mStorageInpNew",
              "mStorageOutCap", "mStorageOutNew")) {
    expect_equal(pr_rows(a, m), pr_rows(b, m), info = m)
    expect_gte(pr_rows(a, m), 1)
  }
})

# @covers mStorageInpCap depth=I backends=glpk
test_that("storage-part-rule: a part only scaled is built, and named as unsized", {
  scen <- pr_interp("pr_scaling",
                    list(invcost = pr_rsv, af = data.frame(inp.af.up = 0.5)))
  expect_gte(pr_rows(scen, "mStorageInpCap"), 1)
  expect_equal(pr_unsized(scen, "mStorageInpCap"), 1L)
  # the discharger was never mentioned, so it is neither built nor named
  expect_equal(pr_rows(scen, "mStorageOutCap"), 0L)
  expect_equal(pr_unsized(scen, "mStorageOutCap"), 0L)
})

# @covers mStorageInpCap mStorageOutCap depth=I backends=glpk
test_that("storage-part-rule: a link sizes both of its ends", {
  # the reservoir may be determined BY `inp2stg`, so refusing `inp2stg` until
  # the charger exists was circular
  scen <- pr_interp("pr_link",
                    list(invcost = pr_rsv,
                         inp2stg = data.frame(inp2stg.fx = 1)))
  expect_gte(pr_rows(scen, "mStorageInpCap"), 1)
  expect_equal(pr_unsized(scen, "mStorageInpCap"), 0L)
})

pr_cells <- function(scen, map) {
  d <- tryCatch(energyRt:::.gds(scen, map), error = function(e) NULL)
  if (is.null(d)) character(0) else sort(unique(paste0(d$region, ":", d$year)))
}

# @covers mStorageInpCap depth=I backends=glpk
test_that("storage-part-rule: a declaration names its own cells, not every cell", {
  # one rule per axis, the same one every key column follows: NA means every
  # member, a concrete value means that one. `R1` does not declare `R2`.
  reg <- pr_interp("pr_reg", list(
    invcost = pr_rsv,
    capacity = data.frame(region = "R1", inp.cap.up = 5)),
    regions = c("R1", "R2"))
  expect_true(all(startsWith(pr_cells(reg, "mStorageInpCap"), "R1:")))
  expect_false(any(startsWith(pr_cells(reg, "mStorageInpCap"), "R2:")))

  yr <- pr_interp("pr_yr", list(
    invcost = pr_rsv,
    capacity = data.frame(year = 2035L, inp.cap.up = 5)),
    periods = c(2025L, 2035L))
  expect_equal(pr_cells(yr, "mStorageInpCap"), "R1:2035")

  # the same declaration without a key is a wildcard and covers the window
  all_ <- pr_interp("pr_wild", list(invcost = pr_rsv,
                                    capacity = data.frame(inp.cap.up = 5)))
  expect_equal(length(pr_cells(all_, "mStorageInpCap")),
               length(pr_cells(all_, "mStorageSpan")))
})

# @covers mStorageInpCap depth=I backends=glpk
test_that("storage-part-rule: the advisory is per cell, not per storage", {
  # sized in R1, declared everywhere by the af: R2's charger is free, and a
  # per-object test would have let R1's bound answer for it
  scen <- pr_interp("pr_mixed", list(
    invcost = pr_rsv,
    af = data.frame(inp.af.up = 0.5),
    capacity = data.frame(region = "R1", inp.cap.up = 5)),
    regions = c("R1", "R2"))
  expect_true(any(startsWith(pr_cells(scen, "mStorageInpCap"), "R2:")))
  expect_equal(pr_unsized(scen, "mStorageInpCap"), 1L)
  d <- as.data.frame(validate_scenario_parameters(scen, action = "silent"))
  expect_match(d$detail[d$check == "unsized_part"], "cell\\(s\\) of 1 object")
})

# @covers mStorageInpCap depth=I backends=glpk
test_that("storage-part-rule: the part prefix is the whole rule", {
  o <- newStorage(name = "X", commodity = "ELC",
                  input = list(comm = "ELC", cap2act = 1),
                  output = list(comm = "ELC", cap2act = 1),
                  seff = data.frame(stg.eff = 0.99),
                  invcost = data.frame(stg.invcost = 0))
  # `cap2act` is filled by the constructor on every part and must not count
  expect_equal(energyRt:::.storage_part_decl(o, "inp"), "")
  expect_equal(energyRt:::.storage_part_decl(o, "out"), "")
  expect_equal(energyRt:::.storage_part_decl(
    newStorage(name = "Y", commodity = "ELC",
               input = list(comm = "ELC"),
               seff = data.frame(inp.eff = 0.9)), "inp"), "scaling")
  expect_equal(energyRt:::.storage_part_decl(
    newStorage(name = "Z", commodity = "ELC",
               input = list(comm = "ELC"),
               capacity = data.frame(inp.cap.up = 5)), "inp"), "sized")
})
