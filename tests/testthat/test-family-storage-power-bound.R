# =========================================================================== #
# Feature family: storage-power-bound (flow-to-capacity link on the converters).
#
# eqStorageInpUp / eqStorageOutUp are the ONLY equations tying a storage's
# charge / discharge FLOW to its converter CAPACITY:
#
#   vStorageInp <= InpCap * cap2act * share * inp.af.up   (and Out alike)
#
# They come from `pStorageInpAf` / `pStorageOutAf`, whose upper default was
# `Inf`. An infinite upper bound deletes the equation, so a storage with no
# declared availability had an unbounded charge / discharge RATE while its
# capacity was still built and paid for: the whole reservoir could be cycled
# inside one timeslice. Only the level chain (`eqStorageLevel`) and the
# reservoir ceiling (`eqStorageAfUp`) bound it, so energy was conserved and
# nothing looked wrong in the results.
#
# Semantics now:
#   - both upper defaults are 1, as `pTechAf` has always had for a technology,
#     so a flow in a timeslice cannot exceed `capacity * cap2act * share`;
#   - both converters are OPTIONAL (the reservoir is the anchor, 2026-10-17):
#     `meqStorageInpUp` / `meqStorageOutUp` are `structural_in` their part's
#     map (`mStorageInpCap` / `mStorageOutCap`), structural wherever the part
#     exists. A storage declaring nothing on `inp.` / `out.` keeps no flow
#     bound beyond its level;
#   - a part exists wherever the OBJECT declares ANY `inp.` / `out.` column
#     (2026-11-18), read from the object rather than from the interpolated
#     parameters, so a deliberate `inp.invcost = 0` or `inp.cap.up = Inf`
#     counts. `inp.eff` counts too, being an `inp.` column; `stg.eff` speaks
#     for the reservoir alone. A part nothing SIZES -- declared only by an
#     `af`, a weather factor, a flow cost, an aux coupling, a retirement bound
#     or an annuity rate -- is sized freely by the LP, which the
#     `unsized_part` advisory names;
#   - `inp.af.up = Inf` / `out.af.up = Inf` declared explicitly still remove the
#     equation (the open-side anti-join in `.build_constraint_join_map`), and
#     the `free_meal` advisory then names it.
# =========================================================================== #

pb_slices <- paste0("s", 1:4)

pb_cal <- function() newCalendar(
  timetable = make_timetable(struct = list(ANNUAL = "ANNUAL", SL = pb_slices)),
  name = "pb_cal")

# cap2act = 1 keeps the arithmetic readable: the per-slice allowance is
# capacity * share = capacity / 4.
pb_build <- function(..., capacity = data.frame(), invcost = data.frame(stg.invcost = 0),
                     af = data.frame(), inp2out = data.frame(),
                     seff = data.frame(inp.eff = 1, out.eff = 1)) {
  newModel("pb",
    repo = newRepository("pb_repo",
      newCommodity("ELC", timeframe = "SL"),
      newSupply("SUP", commodity = "ELC",
                supply = data.frame(timeslice = pb_slices, ava.up = 1000,
                                    cost = c(1, 100, 100, 100))),
      newStorage("STG", commodity = "ELC",
                 input = list(comm = "ELC", cap2act = 1),
                 output = list(comm = "ELC", cap2act = 1),
                 olife = data.frame(olife = 30),
                 capacity = capacity, invcost = invcost, af = af,
                 inp2out = inp2out, seff = seff),
      newDemand("DEM", commodity = "ELC",
                demand = data.frame(timeslice = "s3", demand = 8))),
    calendar = pb_cal(), region = "R1", horizon = newHorizon(2025),
    discount = 0)
}

pb_interp <- function(nm, ...) suppressMessages(suppressWarnings(
  interpolate_model(pb_build(...), name = nm, overwrite = TRUE)))

# `ff_param()` / `ff_bound()` return NULL for an empty parameter
pb_rows <- function(scen, nm) {
  d <- ff_param(scen, nm)
  if (is.null(d)) 0L else nrow(d)
}
pb_bound_rows <- function(scen, nm, side) {
  d <- ff_bound(scen, nm, side)
  if (is.null(d)) 0L else nrow(d)
}
pb_issues <- function(scen) {
  d <- validate_scenario_parameters(scen, action = "silent")
  if (is.null(d) || !is.data.frame(d) || nrow(d) == 0)
    data.frame(check = character(0), parameter = character(0),
               detail = character(0), severity = character(0))
  else as.data.frame(d)
}
pb_max <- function(scen, variable) {
  v <- scen@modOut@variables[[variable]]
  d <- if (is.null(v)) NULL else get_data_slot(v, optional = TRUE)
  if (is.null(d) || nrow(d) == 0) 0 else max(as.data.frame(d)$value)
}

test_that("family storage-power-bound: the flow availabilities default to 1", {
  mi <- getFromNamespace(".modInp", "energyRt")
  expect_equal(as.numeric(unlist(mi$pStorageInpAf$defVal)), c(0, 1))
  expect_equal(as.numeric(unlist(mi$pStorageOutAf$defVal)), c(0, 1))
  # both flow maps are conditional outside their part's membership, so their
  # declared default-valued rows must survive the `sparse` prune -- the
  # `structural_in` contract at R/mapping-engine.R requires it of both
  expect_false(isTRUE(mi$pStorageInpAf$dropIfEmpty))
  expect_false(isTRUE(mi$pStorageOutAf$dropIfEmpty))
  expect_identical(mi$pStorageInpAf$dropIfEmpty, mi$pStorageOutAf$dropIfEmpty)
})

# @covers pStorageInpAf pStorageOutAf eqStorageInpUp eqStorageOutUp depth=I backends=glpk
test_that("family storage-power-bound: a priced charger is rated without any `af`", {
  scen <- pb_interp("pb_priced", invcost = data.frame(inp.invcost = 10,
                                                      stg.invcost = 0))
  # the charging part exists, so both flow bounds are instantiated
  expect_gte(pb_rows(scen, "mStorageInpCap"), 1)
  expect_gte(pb_rows(scen, "meqStorageInpUp"), 1)
  # the discharger is declared too -- `out.eff` is an `out.` column, and any
  # declaration on a part brings that part into being
  expect_gte(pb_rows(scen, "meqStorageOutUp"), 1)
  # nothing was written into the parameter: the declared default carries the 1
  expect_equal(pb_bound_rows(scen, "pStorageInpAf", "up"), 0L)
  # and the coverage check does not read that as a missing value
  iss <- pb_issues(scen)
  expect_equal(nrow(iss[iss$severity == "structural", , drop = FALSE]), 0L)
})

# @covers pStorageInpAf eqStorageInpUp depth=I backends=glpk
test_that("family storage-power-bound: `inp.af.up = Inf` is the explicit opt-out", {
  scen <- pb_interp("pb_open", invcost = data.frame(inp.invcost = 10,
                                                    out.invcost = 10,
                                                    stg.invcost = 0),
                    af = data.frame(inp.af.up = Inf))
  expect_equal(pb_rows(scen, "meqStorageInpUp"), 0L)
  # the discharging side is untouched by the charging opt-out
  expect_gte(pb_rows(scen, "meqStorageOutUp"), 1)
  # ... and the free meal is now named rather than silent
  iss <- pb_issues(scen)
  fm <- iss[iss$check == "free_meal" & iss$parameter == "meqStorageInpUp", ]
  expect_equal(nrow(fm), 1L)
  expect_match(fm$detail, "inp.af.up = Inf")
})

# @covers pStorageInpAf depth=I backends=glpk
test_that("family storage-power-bound: a storage declaring nothing on a part keeps no flow bound", {
  # `seff` is what declared the parts in every other case here; with it empty
  # the storage says nothing about either converter
  scen <- pb_interp("pb_rsv", seff = data.frame())
  expect_equal(pb_rows(scen, "mStorageInpCap"), 0L)
  expect_gte(pb_rows(scen, "mvStorageInp"), 1)      # the flow does exist
  expect_equal(pb_rows(scen, "meqStorageInpUp"), 0L)
  expect_equal(pb_rows(scen, "mStorageOutCap"), 0L)
  expect_equal(pb_rows(scen, "meqStorageOutUp"), 0L)
  # nothing was declared, so nothing is missing -- no advisory either
  iss <- pb_issues(scen)
  expect_equal(nrow(iss[iss$parameter == "meqStorageInpUp", , drop = FALSE]), 0L)
  expect_equal(nrow(iss[iss$check == "unsized_part", , drop = FALSE]), 0L)
})

# @covers pStorageInpAf eqStorageInpUp depth=I backends=glpk
test_that("family storage-power-bound: a part declared but not sized builds, and is named", {
  # an `af` is a declaration on the part, so the part exists -- but nothing
  # fixes its size, so the LP sets it freely and the bound constrains nothing
  scen <- pb_interp("pb_declared", af = data.frame(inp.af.up = 0.5))
  expect_gte(pb_rows(scen, "mStorageInpCap"), 1)
  expect_gte(pb_rows(scen, "meqStorageInpUp"), 1)
  iss <- pb_issues(scen)
  un <- iss[iss$check == "unsized_part" & iss$parameter == "mStorageInpCap", ]
  expect_equal(nrow(un), 1L)
  expect_match(un$detail, "charger capacity is declared but nothing sizes it")
  expect_equal(nrow(iss[iss$severity == "structural", , drop = FALSE]), 0L)

  # a price sizes it, and the advisory goes quiet
  sized <- pb_interp("pb_sized", af = data.frame(inp.af.up = 0.5),
                     invcost = data.frame(inp.invcost = 10, stg.invcost = 0))
  iss2 <- pb_issues(sized)
  expect_equal(nrow(iss2[iss2$check == "unsized_part" &
                           iss2$parameter == "mStorageInpCap", , drop = FALSE]), 0L)
})

# @covers pStorageInpEff depth=I backends=glpk
test_that("family storage-power-bound: `stg.eff` speaks for the reservoir alone", {
  # the part prefix is the whole rule: a reservoir efficiency must not mint a
  # charger or a discharger
  scen <- pb_interp("pb_stgeff", seff = data.frame(stg.eff = 0.99))
  expect_equal(pb_rows(scen, "mStorageInpCap"), 0L)
  expect_equal(pb_rows(scen, "mStorageOutCap"), 0L)
})

# @covers pStorageInpAf vStorageInp eqStorageInpUp depth=S backends=glpk
test_that("family storage-power-bound: the rating limits the per-slice flow", {
  skip_if_no_solver()
  # charger fixed at 4, cap2act 1, four equal slices -> 1 unit per slice. The
  # discharger is left roomy (40 -> 10 per slice) so that only the CHARGING
  # rate is in question.
  cap <- data.frame(inp.cap.fx = 4, out.cap.fx = 40)
  allow <- 4 * 1 * 0.25

  bounded <- .fork_solve(pb_interp("pb_bound", capacity = cap),
                         solver_options$glpk)
  expect_true(verify_solution(bounded)$ok)
  expect_lte(pb_max(bounded, "vStorageInp"), allow + 1e-6)

  open <- .fork_solve(pb_interp("pb_unbound", capacity = cap,
                                af = data.frame(inp.af.up = Inf)),
                      solver_options$glpk)
  # without the equation the whole demand is charged in the cheap slice, well
  # past the charger's rating
  expect_gt(pb_max(open, "vStorageInp"), 4 * allow)
  expect_lt(.fork_objective(open), .fork_objective(bounded))
})
