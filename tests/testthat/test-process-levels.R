# =========================================================================== #
# Feature family: process-levels -- the level algebra shared by the time and
# space axes, used to place a flow at the FINER of (process level, commodity
# level).
#
# Both hierarchies number levels coarsest-first, which is what lets one
# comparator serve both. These tests pin that shared property: if either axis
# ever flips its rank direction, `.finer_level()` silently starts returning the
# coarser level and every flow built on it lands where no balance reads it.
# =========================================================================== #

pl_rank <- c(ANNUAL = 1L, SEASON = 2L, HOUR = 3L)

test_that("family process-levels: the finer level wins on either axis", {
  f <- function(a, b) energyRt:::.finer_level(a, b, pl_rank)

  expect_identical(f("ANNUAL", "HOUR"), "HOUR")
  expect_identical(f("HOUR", "ANNUAL"), "HOUR")
  expect_identical(f("SEASON", "HOUR"), "HOUR")
  expect_identical(f("ANNUAL", "SEASON"), "SEASON")
  # a tie returns the level itself, not a copy of the other argument
  expect_identical(f("SEASON", "SEASON"), "SEASON")
})

test_that("family process-levels: it is vectorised and recycles length 1", {
  f <- function(a, b) energyRt:::.finer_level(a, b, pl_rank)

  expect_identical(f(c("ANNUAL", "HOUR", "SEASON"),
                     c("SEASON", "SEASON", "ANNUAL")),
                   c("SEASON", "HOUR", "SEASON"))
  # length-1 recycling, which is how a single process level is compared against
  # a column of commodity levels
  expect_identical(f("SEASON", c("ANNUAL", "SEASON", "HOUR")),
                   c("SEASON", "SEASON", "HOUR"))
  expect_identical(f(c("ANNUAL", "HOUR"), "SEASON"), c("SEASON", "HOUR"))
  expect_identical(f(character(), character()), character())
})

test_that("family process-levels: 'not stated' loses to a stated level", {
  f <- function(a, b) energyRt:::.finer_level(a, b, pl_rank)

  # NA/"" mean the caller has not resolved a default yet; a stated level wins
  expect_identical(f(NA_character_, "SEASON"), "SEASON")
  expect_identical(f("SEASON", NA_character_), "SEASON")
  expect_identical(f("", "ANNUAL"), "ANNUAL")
  expect_identical(f("ANNUAL", ""), "ANNUAL")
  # neither side states anything -> NA, never a silent ""
  expect_identical(f(NA_character_, NA_character_), NA_character_)
  expect_identical(f("", ""), NA_character_)
})

test_that("family process-levels: an unknown level name is an error", {
  # the whole point: a typo'd or stale level must not resolve to NA and place
  # the flow somewhere no equation reads
  expect_error(energyRt:::.finer_level("ANNUAL", "WEEK", pl_rank),
               "unknown level")
  expect_error(energyRt:::.finer_level("ANNUAL", "WEEK", pl_rank), "WEEK")
  # and the message says what IS known, so the fix is obvious
  expect_error(energyRt:::.finer_level("NOPE", "HOUR", pl_rank), "Known:")
  expect_error(energyRt:::.finer_level("ANNUAL", "HOUR", c(a = 1L)),
               "unknown level")
  expect_error(energyRt:::.finer_level("A", "B", stats::setNames(1L, NULL)),
               "named integer")
})

test_that("family process-levels: mismatched lengths are refused", {
  expect_error(energyRt:::.finer_level(c("ANNUAL", "HOUR"),
                                       c("ANNUAL", "HOUR", "SEASON"), pl_rank),
               "same length")
})

# --------------------------------------------------------------------------- #
# The geoscale accessors. These replace an idiom duplicated four times in
# R/mapping-builders.R (:1273, :1413, :1430, :1659); the tests assert the
# contract those call sites rely on -- 1 = coarsest, and every region mapped.
# --------------------------------------------------------------------------- #

test_that("family process-levels: geo rank is coarsest-first", {
  skip_if_not_installed("geoscales")
  h <- list(levels = c("nation", "zone", "region"),
            members = list(nation = "NAT", zone = c("Z1", "Z2"),
                           region = c("R1", "R2", "R3", "R4")))
  rk <- energyRt:::.geo_level_rank(h)
  expect_identical(rk, c(nation = 1L, zone = 2L, region = 3L))
  # the direction that makes one comparator serve both axes
  expect_lt(rk[["nation"]], rk[["region"]])
  expect_identical(energyRt:::.finer_level("nation", "region", rk), "region")

  lo <- energyRt:::.geo_level_of(h)
  expect_identical(lo[["NAT"]], "nation")
  expect_identical(lo[["R3"]], "region")
  expect_identical(lo[["Z1"]], "zone")
  expect_setequal(names(lo), c("NAT", "Z1", "Z2", "R1", "R2", "R3", "R4"))
})

test_that("family process-levels: region rank composes level-of and rank", {
  h <- list(levels = c("nation", "zone", "region"),
            members = list(nation = "NAT", zone = c("Z1", "Z2"),
                           region = c("R1", "R2", "R3", "R4")))
  rr <- energyRt:::.geo_region_rank(h)
  expect_identical(rr[["NAT"]], 1L)
  expect_identical(rr[["Z2"]], 2L)
  expect_identical(rr[["R4"]], 3L)
  expect_setequal(names(rr), c("NAT", "Z1", "Z2", "R1", "R2", "R3", "R4"))
  # it must equal the composed lookup the five call sites used to write by hand
  lo <- energyRt:::.geo_level_of(h)
  rk <- energyRt:::.geo_level_rank(h)
  expect_identical(unname(rr[names(lo)]), unname(rk[unname(lo)]))
  # an unknown region is NA, not an error -- every call site indexes with a
  # single bracket and guards on NA itself (`reg_rank[d$region]`,
  # `rk[cand$regionp]`), so that is the contract pinned here
  expect_true(is.na(rr["ZZ"]))
  expect_true(all(is.na(rr[c("ZZ", "QQ")])))
})

test_that("family process-levels: the geo accessors tolerate no geoscale", {
  # a flat model has no hierarchy at all, and every caller of these must stay a
  # no-op there rather than erroring
  expect_null(energyRt:::.geo_level_rank(NULL))
  expect_null(energyRt:::.geo_level_of(NULL))
  expect_null(energyRt:::.geo_level_rank(list(levels = character())))
  expect_null(energyRt:::.geo_level_of(list(members = list())))
})

# --------------------------------------------------------------------------- #
# The flow-level table. The property under test is REDUCTION: today a process
# is always at-or-finer than every commodity it touches, so the finer of the
# two is always the process's own level. Every row must therefore satisfy
# `flow == process_level`. When a coarse process becomes legal that stops being
# universally true -- and this test is what proves the generalization did not
# change any existing model on the way there.
# --------------------------------------------------------------------------- #

pl_time_model <- function() {
  newModel("plt",
    repo = newRepository("r",
      newCommodity("COA", timeframe = "ANNUAL"),
      newCommodity("ELC", timeframe = "SEASON"),
      newSupply("SUP", commodity = "COA", supply = data.frame(cost = 1)),
      newTechnology("ECOA", input = list(comm = "COA"),
                    output = list(comm = "ELC"),
                    invcost = data.frame(invcost = 900),
                    vintage = data.frame(olife = 30L), cap2act = 1),
      newDemand("DE", commodity = "ELC",
                demand = data.frame(timeslice = c("WIN", "SUM"), demand = 30))),
    calendar = newCalendar(timetable = make_timetable(
      struct = list(ANNUAL = "ANNUAL", SEASON = c("WIN", "SUM"))),
      name = "pltc"),
    region = "R1", horizon = newHorizon(2020:2030, intervals = 10),
    discount = 0)
}

pl_geo_model <- function(mill_region = "R1") {
  gs <- geoscales::geoscale_from_leaftable(
    data.frame(nation = "NAT", region = c("R1", "R2")),
    geoframes = c("nation", "region"), key = "region", name = "plg")
  newModel("plr",
    repo = newRepository("r",
      newCommodity("COA", timeframe = "ANNUAL"),
      newCommodity("ELC", timeframe = "ANNUAL"),
      newCommodity("STEEL", timeframe = "ANNUAL", geoframe = "nation"),
      newSupply("SUP", commodity = "COA",
                supply = data.frame(region = c("R1", "R2"), cost = 1)),
      newTechnology("ECOA", input = list(comm = "COA"),
                    output = list(comm = "ELC"),
                    invcost = data.frame(invcost = 900),
                    vintage = data.frame(olife = 30L), cap2act = 1),
      newTechnology("MILL", input = list(comm = "ELC"),
                    output = list(comm = "STEEL"), region = mill_region,
                    invcost = data.frame(invcost = 50),
                    vintage = data.frame(olife = 30L), cap2act = 1),
      newDemand("DS", commodity = "STEEL", region = "NAT",
                demand = data.frame(demand = 5)),
      newDemand("DE", commodity = "ELC",
                demand = data.frame(region = c("R1", "R2"), demand = 30))),
    calendar = newCalendar(timetable = make_timetable(
      struct = list(ANNUAL = "ANNUAL")), name = "plrc"),
    geoscale = gs, region = c("R1", "R2"),
    horizon = newHorizon(2020:2030, intervals = 10), discount = 0)
}

pl_interp <- function(mod, tag) {
  suppressMessages(suppressWarnings(
    interpolate_model(mod, name = tag, ondisk = FALSE, overwrite = TRUE)))
}

test_that("family process-levels: flow levels reduce to the process level (time)", {
  d <- energyRt:::.flow_levels(pl_interp(pl_time_model(), "pl_t"), "timeslice")
  expect_true(all(c("process", "comm", "process_level", "comm_level", "flow")
                  %in% names(d)))
  expect_gt(nrow(d), 0L)
  expect_false(any(is.na(d$flow)))
  # THE reduction property
  expect_identical(d$flow, d$process_level)

  # and the shape is the documented one: ECOA runs at SEASON (the finest of its
  # commodities) so its ANNUAL coal flow sits at SEASON and aggregates up --
  # the case that already works today
  ecoa_coa <- d[d$process == "ECOA" & d$comm == "COA", ]
  expect_identical(ecoa_coa$comm_level, "ANNUAL")
  expect_identical(ecoa_coa$flow, "SEASON")
  # a process matching its commodity is unchanged
  sup <- d[d$process == "SUP", ]
  expect_identical(sup$flow, "ANNUAL")
})

test_that("family process-levels: flow levels reduce to the process level (space)", {
  skip_if_not_installed("geoscales")
  d <- energyRt:::.flow_levels(pl_interp(pl_geo_model(), "pl_g"), "region")
  expect_gt(nrow(d), 0L)
  expect_false(any(is.na(d$flow)))
  expect_identical(d$flow, d$process_level)

  # MILL sits at R1 and produces nationally-balanced STEEL, so its flow is at
  # the region level and rolls up -- the spatial twin of the ECOA case
  mill <- d[d$process == "MILL" & d$comm == "STEEL", ]
  expect_identical(mill$comm_level, "nation")
  expect_identical(mill$flow, "region")
  # a nationally-declared demand for a nationally-balanced commodity matches
  ds <- d[d$process == "DS", ]
  expect_identical(ds$process_level, "nation")
  expect_identical(ds$flow, "nation")
})

test_that("family process-levels: the region axis is a no-op without a geoscale", {
  s <- pl_interp(pl_time_model(), "pl_flat")
  expect_null(energyRt:::.flow_levels(s, "region"))
  # ... while the time axis still works there
  expect_gt(nrow(energyRt:::.flow_levels(s, "timeslice")), 0L)
})

# --------------------------------------------------------------------------- #
# The coarse-process gate. The machinery below (flow levels, the region-pair
# map, the per-feature blockers) is built, but no axis is ENABLED until the
# equations that sum a coarse process's finer flows exist in all four
# templates. Measured consequence of enabling it early: a NAT technology
# drawing per-region electricity strands its input in a cell no balance reads
# and the model is INFEASIBLE -- loud, but for the wrong reason.
# --------------------------------------------------------------------------- #

test_that("family process-levels: both axes are enabled", {
  expect_true(energyRt:::.coarse_axis_ready("region"))
  expect_true(energyRt:::.coarse_axis_ready("timeslice"))
  # storage is space-only by decision: its chronology and C-rate logic are
  # written against its own time resolution
  expect_setequal(energyRt:::.coarse_classes_supported,
                  c("technology", "storage"))
})

test_that("family process-levels: a coarse technology in space is accepted", {
  skip_if_not_installed("geoscales")
  # MILL at the nation while ELC stays per-region: a COARSE process, which the
  # region axis now supports
  s <- pl_interp(pl_geo_model(mill_region = "NAT"), "pl_coarse")
  expect_s4_class(s, "scenario")

  # the flow moved DOWN to the commodity's regions ...
  inp <- as.data.frame(get_data_slot(s@modInp@parameters[["mvTechInp"]]))
  inp <- inp[inp$tech == "MILL", , drop = FALSE]
  expect_setequal(unique(inp$region), c("R1", "R2"))
  # ... while the output stayed at NAT, because STEEL is balanced there and so
  # process and commodity levels already agree
  out <- as.data.frame(get_data_slot(s@modInp@parameters[["mvTechOut"]]))
  expect_setequal(unique(out$region[out$tech == "MILL"]), "NAT")

  # and the Agg map pairs the process cell with the regions beneath it
  ag <- as.data.frame(get_data_slot(
    s@modInp@parameters[["mTechProcAggRegion"]]))
  expect_setequal(ag$regionp, c("R1", "R2"))
  expect_setequal(ag$region, "NAT")
  expect_setequal(ag$comm, "ELC")
})

test_that("family process-levels: a coarse process on the TIME axis is refused", {
  # the axis that is not built yet must still refuse, with the reason
  m <- pl_time_model()
  err <- tryCatch({
    # ECOA runs at SEASON (from ELC); pinning it to ANNUAL makes it coarser
    pl_interp(newModel("pltc2",
      repo = newRepository("r",
        newCommodity("COA", timeframe = "ANNUAL"),
        newCommodity("ELC", timeframe = "SEASON"),
        newSupply("SUP", commodity = "COA", supply = data.frame(cost = 1)),
        newTechnology("ECOA", input = list(comm = "COA"),
                      output = list(comm = "ELC"), timeframe = "ANNUAL",
                      invcost = data.frame(invcost = 900),
                      vintage = data.frame(olife = 30L), cap2act = 1),
        newDemand("DE", commodity = "ELC",
                  demand = data.frame(timeslice = c("WIN", "SUM"),
                                      demand = 30))),
      calendar = m@config@calendar, region = "R1",
      horizon = newHorizon(2020:2030, intervals = 10), discount = 0),
      "pl_coarse_t")
    NA_character_
  }, error = function(e) conditionMessage(e))
  expect_false(is.na(err))
  expect_match(err, "COARSER")
})

test_that("family process-levels: a coarse technology solves like the fine one", {
  skip_if_no_solver()
  skip_if_not_installed("geoscales")
  # R1 and R2 are symmetric here, so moving MILL up to the nation must not
  # change the optimum -- it only lets the plant pick where it draws. Before
  # the Agg branches existed this gave 18683.33 (steel from free electricity).
  obj <- function(tag, reg) {
    s <- suppressMessages(suppressWarnings(solve_model(
      pl_geo_model(mill_region = reg), name = tag, ondisk = FALSE,
      overwrite = TRUE)))
    suppressWarnings(getData(s, "vObjective", merge = TRUE)$value)
  }
  fine <- obj("pl_sv_fine", "R1")
  coarse <- obj("pl_sv_coarse", "NAT")
  expect_length(coarse, 1L)
  expect_equal(coarse, fine, tolerance = 1e-6)
})

test_that("family process-levels: blockers detect each unsupported feature", {
  base <- newTechnology("T", input = list(comm = "A"),
                        output = list(comm = "B"),
                        invcost = data.frame(invcost = 1),
                        vintage = data.frame(olife = 10L), cap2act = 1)
  expect_identical(energyRt:::.coarse_blockers(base), character())

  blk <- function(x) energyRt:::.coarse_blockers(x)
  # SEVERAL commodities are fine: summing the finer inputs works per commodity,
  # and a different input/output commodity is the configuration that makes a
  # coarse storage possible at all (ELC in, H2 out). This used to be refused.
  t2 <- newTechnology("T", input = list(comm = c("A", "C")),
                      output = list(comm = "B"),
                      invcost = data.frame(invcost = 1),
                      vintage = data.frame(olife = 10L), cap2act = 1)
  expect_identical(blk(t2), character())

  # ramp limits
  tr <- base
  tr@af <- data.frame(rampup = 0.5)
  expect_match(paste(blk(tr), collapse = "|"), "ramp")

  # a slot present but all-NA in the guarded columns is NOT a blocker: every
  # technology carries these columns, so testing presence would refuse
  # everything
  tn <- base
  tn@af <- data.frame(af.up = 1, rampup = NA_real_)
  expect_false(any(grepl("ramp", blk(tn))))
})

test_that("family process-levels: the region-pair map is empty when legal", {
  skip_if_not_installed("geoscales")
  # every process is at-or-finer than its commodities, so there is nothing to
  # sum and the Agg branch of each equation generates no rows
  expect_null(energyRt:::.flow_region_agg(pl_interp(pl_geo_model(), "pl_agg")))
})

test_that("family process-levels: the real hierarchy satisfies the contract", {
  # the synthetic `h` above must not drift from what .geo_hierarchy() returns
  skip_if_no_fixtures()
  skip_if_not_installed("geoscales")
  gs <- geoscales::geoscale_from_leaftable(
    data.frame(nation = "NAT", region = c("R1", "R2")),
    geoframes = c("nation", "region"), key = "region", name = "pl")
  h <- energyRt:::.geo_hierarchy(gs, c("R1", "R2"))
  rk <- energyRt:::.geo_level_rank(h)
  lo <- energyRt:::.geo_level_of(h)
  expect_true(rk[["nation"]] < rk[["region"]])
  expect_true(all(c("R1", "R2") %in% names(lo)))
  expect_identical(unname(lo[["R1"]]), "region")
  # and the composite lookup the four call sites actually perform
  expect_identical(unname(rk[lo[["NAT"]]]), 1L)
})

# --------------------------------------------------------------------------- #
# Coarsening is a MODEL CHANGE, not a re-representation. A coarse process's
# own constraints bind at its own frame, so they are weaker than the fine
# process's, and the coarse model is CHEAPER. These tests pin that difference
# rather than asserting equivalence -- asserting equality here would assert
# something false.
#
# Two sources, both measured:
#   capacity  availability is `cap * pTimesliceShare[s]`, so a coarse process is
#             sized against its own (longer) slice. On a SAMPLED calendar the
#             shares do not sum to 1 and the gap is visible.
#   relaxation the input/output ratio -- and, once groups are unblocked, the
#             `@ceff` shares -- bind in aggregate over the coarse cell.
#
# Equivalence (fine == coarse) holds only where neither bites: a full calendar
# with symmetric demand. That is what the cross-backend tests assert.
# --------------------------------------------------------------------------- #

pl_sampled_model <- function(mill_tf = character()) {
  newModel("pls", repo = newRepository("r",
    newCommodity("COA", timeframe = "ANNUAL"),
    newCommodity("ELC", timeframe = "MONTH"),
    newCommodity("STEEL", timeframe = "ANNUAL"),
    newSupply("SUP", commodity = "COA", supply = data.frame(cost = 1)),
    newTechnology("ECOA", input = list(comm = "COA"),
                  output = list(comm = "ELC"),
                  invcost = data.frame(invcost = 900),
                  vintage = data.frame(olife = 30L), cap2act = 1),
    newTechnology("MILL", input = list(comm = "ELC"),
                  output = list(comm = "STEEL"), timeframe = mill_tf,
                  invcost = data.frame(invcost = 50),
                  vintage = data.frame(olife = 30L), cap2act = 1),
    newDemand("DS", commodity = "STEEL", demand = data.frame(demand = 4)),
    newDemand("DE", commodity = "ELC",
              demand = data.frame(timeslice = c("m01", "m02", "m03"),
                                  demand = 20))),
    calendar = calendars$m12_subset_q1, region = "R1",
    horizon = newHorizon(2020:2030, intervals = 10), discount = 0)
}

test_that("family process-levels: a coarse process is sized on its own slice", {
  skip_if_no_solver()
  solve_it <- function(tf, tag) {
    s <- suppressMessages(suppressWarnings(solve_model(
      pl_sampled_model(tf), name = tag, ondisk = FALSE, overwrite = TRUE)))
    cap <- suppressWarnings(getData(s, "vTechCap", merge = TRUE))
    list(obj = suppressWarnings(getData(s, "vObjective", merge = TRUE)$value),
         cap = unique(cap$value[cap$tech == "MILL"]))
  }
  fine <- solve_it(character(), "pl_s_fine")
  coarse <- solve_it("ANNUAL", "pl_s_coarse")
  expect_length(fine$obj, 1L)
  expect_length(coarse$obj, 1L)

  # Measured 2026-10-10 on `m12_subset_q1`: fine 80784.39 / cap 5.8065,
  # coarse 80754.29 / cap 4.0. The DIRECTION is the contract; the exact values
  # would move with unrelated cost changes, so they are recorded, not asserted.
  expect_lt(coarse$obj, fine$obj)
  expect_lt(max(coarse$cap), max(fine$cap))
  # ... and the coarse capacity is the annual activity itself (pTimesliceShare
  # is 1 at the top slice), which is what makes it smaller
  expect_equal(max(coarse$cap), 4, tolerance = 1e-6)
})

test_that("family process-levels: equivalence holds where nothing relaxes", {
  skip_if_no_solver()
  # full calendar, symmetric demand, no shares: the coarse declaration changes
  # the model's SHAPE but not its optimum, which is what makes the generalization
  # safe to default on
  mk <- function(tf) newModel("ple", repo = newRepository("r",
      newCommodity("COA", timeframe = "ANNUAL"),
      newCommodity("ELC", timeframe = "SEASON"),
      newCommodity("STEEL", timeframe = "ANNUAL"),
      newSupply("SUP", commodity = "COA", supply = data.frame(cost = 1)),
      newTechnology("ECOA", input = list(comm = "COA"),
                    output = list(comm = "ELC"),
                    invcost = data.frame(invcost = 900),
                    vintage = data.frame(olife = 30L), cap2act = 1),
      newTechnology("MILL", input = list(comm = "ELC"),
                    output = list(comm = "STEEL"), timeframe = tf,
                    invcost = data.frame(invcost = 50),
                    vintage = data.frame(olife = 30L), cap2act = 1),
      newDemand("DS", commodity = "STEEL", demand = data.frame(demand = 4)),
      newDemand("DE", commodity = "ELC",
                demand = data.frame(timeslice = c("WIN", "SUM"), demand = 20))),
    calendar = newCalendar(timetable = make_timetable(
      struct = list(ANNUAL = "ANNUAL", SEASON = c("WIN", "SUM"))),
      name = "plec"),
    region = "R1", horizon = newHorizon(2020:2030, intervals = 10),
    discount = 0)
  obj <- function(tf, tag) suppressWarnings(getData(
    suppressMessages(suppressWarnings(solve_model(mk(tf), name = tag,
      ondisk = FALSE, overwrite = TRUE))), "vObjective", merge = TRUE)$value)
  expect_equal(obj("ANNUAL", "ple_coarse"), obj(character(), "ple_fine"),
               tolerance = 1e-6)
})

pl_group_model <- function(mill_tf = character(), shares = FALSE) {
  ceff <- data.frame(comm = c("ELC", "GAS"), cinp2ginp = c(2, 3))
  if (shares) ceff$grp.share.up <- c(NA, 0.1)
  newModel("plg", repo = newRepository("r",
    newCommodity("COA", timeframe = "ANNUAL"),
    newCommodity("ELC", timeframe = "SEASON"),
    newCommodity("GAS", timeframe = "SEASON"),
    newCommodity("STEEL", timeframe = "ANNUAL"),
    newSupply("SUP", commodity = "COA", supply = data.frame(cost = 1)),
    newTechnology("ECOA", input = list(comm = "COA"),
                  output = list(comm = "ELC"),
                  invcost = data.frame(invcost = 900),
                  vintage = data.frame(olife = 30L), cap2act = 1),
    newTechnology("EGAS", input = list(comm = "COA"),
                  output = list(comm = "GAS"),
                  invcost = data.frame(invcost = 800),
                  vintage = data.frame(olife = 30L), cap2act = 1),
    newTechnology("MILL",
                  input = list(comm = c("ELC", "GAS"),
                               group = c("FUEL", "FUEL")),
                  output = list(comm = "STEEL"),
                  geff = data.frame(group = "FUEL", ginp2use = 1),
                  ceff = ceff, timeframe = mill_tf,
                  invcost = data.frame(invcost = 50),
                  vintage = data.frame(olife = 30L), cap2act = 1),
    newDemand("DS", commodity = "STEEL", demand = data.frame(demand = 4)),
    newDemand("DE", commodity = "ELC",
              demand = data.frame(timeslice = c("WIN", "SUM"),
                                  demand = c(2, 18))),
    newDemand("DG", commodity = "GAS",
              demand = data.frame(timeslice = c("WIN", "SUM"),
                                  demand = c(18, 2)))),
    calendar = newCalendar(timetable = make_timetable(
      struct = list(ANNUAL = "ANNUAL", SEASON = c("WIN", "SUM"))),
      name = "plgc"),
    region = "R1", horizon = newHorizon(2020:2030, intervals = 10),
    discount = 0)
}

test_that("family process-levels: a coarse process's @ceff share relaxes", {
  skip_if_no_solver()
  obj <- function(tf, sh, tag) suppressWarnings(getData(
    suppressMessages(suppressWarnings(solve_model(
      pl_group_model(tf, sh), name = tag, ondisk = FALSE, overwrite = TRUE))),
    "vObjective", merge = TRUE)$value)

  # Even WITHOUT a share the coarse model is cheaper: the group input/output
  # RATIO binds in aggregate over the year, so the coarse process may vary its
  # fuel mix across seasons in a way the fine one cannot -- intra-period
  # "banking". Measured 2026-10-10: fine 20883.33, coarse 20880.00.
  fine0 <- obj(character(), FALSE, "plg_f0")
  coarse0 <- obj("ANNUAL", FALSE, "plg_c0")
  expect_lt(coarse0, fine0)

  # WITH a binding share and asymmetric seasonal demand, the coarse process's
  # share binds in AGGREGATE over the year and is therefore WEAKER than the
  # fine process's per-season share -- so the coarse model is CHEAPER. This is
  # the documented model change, not a defect. Measured 2026-10-10:
  # fine 20953.33, coarse 20885.71.
  fine <- obj(character(), TRUE, "plg_f1")
  coarse <- obj("ANNUAL", TRUE, "plg_c1")
  expect_length(fine, 1L)
  expect_length(coarse, 1L)
  expect_lt(coarse, fine)
  # the share must genuinely bind on the fine model, or the comparison above
  # would be vacuous
  expect_gt(fine, fine0)
  # and the share relaxation is LARGER than the ratio relaxation alone
  expect_lt(coarse - coarse0, fine - fine0)
})

test_that("family process-levels: group conversion coefficients are read at the process cell", {
  # `@ceff` coefficients are per-process properties defined at the process's own
  # cell. The Agg branches read the VARIABLE at the finer cell and the
  # COEFFICIENT at the process's -- reading the coefficient finer instead gets
  # the backend default (GLPK/Julia/Pyomo) or 0 (GAMS dense), which was
  # silently wrong until the declared value differed from the default. Hence the
  # non-default 2 / 3 in the fixture: with 1 / 1 the bug was invisible.
  ce <- pl_group_model("ANNUAL", TRUE)@data[[1]]@data
  mill <- ce[[which(vapply(ce, function(o) identical(o@name, "MILL"), logical(1)))]]
  expect_setequal(mill@ceff$cinp2ginp, c(2, 3))
  expect_false(any(mill@ceff$cinp2ginp == 1))
})

# --------------------------------------------------------------------------- #
# `afc` on an OUTPUT: the availability bound must scale with the slice, as its
# three siblings do. eqTechAfcOutUp was the only one of the four missing
# `pTimesliceShare[s]`, consistently in all four templates, which made the
# bound too loose by 1/share -- 2x on two equal seasons. Measured before the
# fix: 5.0 per slice against a correct cap of 2.5.
# --------------------------------------------------------------------------- #

test_that("family process-levels: afc.up on an output scales with the slice", {
  skip_if_no_solver()
  mod <- newModel("plafc", repo = newRepository("r",
      newCommodity("COA", timeframe = "ANNUAL"),
      newCommodity("ELC", timeframe = "SEASON"),
      newSupply("SUP", commodity = "COA", supply = data.frame(cost = 1)),
      newTechnology("PP", input = list(comm = "COA"),
        output = list(comm = "ELC"),
        ceff = data.frame(comm = "ELC", afc.up = 0.5),
        capacity = data.frame(cap.up = 10), cap2act = 1,
        invcost = data.frame(invcost = 1),
        vintage = data.frame(olife = 30L)),
      # an expensive backstop keeps the model feasible once PP is capped
      newImport("IMP", commodity = "ELC", import = data.frame(price = 500)),
      newDemand("DE", commodity = "ELC",
                demand = data.frame(timeslice = c("WIN", "SUM"), demand = 50))),
    calendar = newCalendar(timetable = make_timetable(
      struct = list(ANNUAL = "ANNUAL", SEASON = c("WIN", "SUM"))),
      name = "plafcc"),
    region = "R1", horizon = newHorizon(2025), discount = 0)
  s <- suppressMessages(suppressWarnings(solve_model(
    mod, name = "pl_afc", ondisk = FALSE, overwrite = TRUE)))
  out <- suppressWarnings(getData(s, "vTechOut", merge = TRUE))
  cap <- suppressWarnings(getData(s, "vTechCap", merge = TRUE))
  shares <- as.data.frame(s@settings@calendar@timeslice_share)

  pp_cap <- max(cap$value[cap$tech == "PP"])
  sh <- shares$share[shares$timeslice == "WIN"]
  # cact2cout * afc.up * cap2act * cap * share
  expected <- 1 * 0.5 * 1 * pp_cap * sh
  expect_equal(max(out$value[out$tech == "PP"]), expected, tolerance = 1e-6)
  # and the bound must actually bite, or the test proves nothing
  expect_lt(expected, pp_cap * 0.5)
})

# --------------------------------------------------------------------------- #
# A MIXED-LEVEL group on a coarse process: one member AT the process's level
# (not spread) and one FINER (spread). The group sum must therefore mix a
# direct term with an Agg-summed one in the same equation -- the combination
# most likely to expose a formulation error, and the one the earlier audit left
# unverified.
#
# Two different weightings are in play, and getting them crossed is the trap:
#   the RATIO is in group-input units      -> cinp2ginp-weighted
#   the SHARE is in raw commodity units    -> NOT weighted
# --------------------------------------------------------------------------- #

pl_mixed_group_model <- function(mill_tf = "ANNUAL") {
  newModel("plmg", repo = newRepository("r",
    newCommodity("COA", timeframe = "ANNUAL"),     # == the process's level
    newCommodity("GAS", timeframe = "SEASON"),     # FINER -> spread
    newCommodity("STEEL", timeframe = "ANNUAL"),
    newSupply("SCOA", commodity = "COA", supply = data.frame(cost = 9)),
    newSupply("SGAS", commodity = "GAS", supply = data.frame(cost = 1)),
    newTechnology("MILL",
      input = list(comm = c("COA", "GAS"), group = c("FUEL", "FUEL")),
      output = list(comm = "STEEL"),
      geff = data.frame(group = "FUEL", ginp2use = 1),
      ceff = data.frame(comm = c("COA", "GAS"), cinp2ginp = c(2, 3),
                        grp.share.up = c(NA, 0.6)),
      timeframe = mill_tf, invcost = data.frame(invcost = 50),
      vintage = data.frame(olife = 30L), cap2act = 1),
    newDemand("DS", commodity = "STEEL", demand = data.frame(demand = 12))),
    calendar = newCalendar(timetable = make_timetable(
      struct = list(ANNUAL = "ANNUAL", SEASON = c("WIN", "SUM"))),
      name = "plmgc"),
    region = "R1", horizon = newHorizon(2025), discount = 0)
}

test_that("family process-levels: a mixed-level group sums correctly", {
  skip_if_no_solver()
  solved <- function(tf, tag) {
    s <- suppressMessages(suppressWarnings(solve_model(
      pl_mixed_group_model(tf), name = tag, ondisk = FALSE, overwrite = TRUE)))
    i <- suppressWarnings(getData(s, "vTechInp", merge = TRUE))
    o <- suppressWarnings(getData(s, "vTechOut", merge = TRUE))
    i <- i[i$tech == "MILL", ]
    f <- function(cm) sum(i$value[i$comm == cm])
    list(obj = suppressWarnings(getData(s, "vObjective", merge = TRUE))$value,
         coa = f("COA"), gas = f("GAS"),
         out = sum(o$value[o$tech == "MILL"]),
         slices = sort(unique(as.character(i$timeslice[i$comm == "GAS"]))))
  }
  co <- solved("ANNUAL", "pl_mg_c")
  fi <- solved(character(), "pl_mg_f")

  # the COARSE process's finer member really is spread to the commodity's slices
  expect_true(all(co$slices %in% c("WIN", "SUM")))

  for (r in list(co, fi)) {
    # ratio, in GROUP-INPUT units: ginp2use * sum(cinp2ginp * inp) == out
    expect_equal(2 * r$coa + 3 * r$gas, r$out, tolerance = 1e-6)
    # share, in RAW commodity units -- the share equation does NOT apply
    # cinp2ginp, unlike the ratio
    expect_equal(r$gas / (r$coa + r$gas), 0.6, tolerance = 1e-6)
  }
  # nothing here lets the coarse process exploit its freedom (annual demand,
  # symmetric conditions), so the two must agree exactly
  expect_equal(co$obj, fi$obj, tolerance = 1e-6)
})

test_that("family process-levels: coarse storage holds on a SAMPLED calendar", {
  skip_if_no_solver()
  skip_if_not_installed("geoscales")
  # Storage is coarse in SPACE only, so `pTimesliceAgg` should not enter its
  # Agg branch at all. On a sampled calendar the month weight is 4.0556 rather
  # than 1, so a weight leaking in through the level chain would show here and
  # nowhere else -- the same blind spot that hid the time-axis weight until a
  # sampled fixture was used.
  gs <- geoscales::geoscale_from_leaftable(
    data.frame(nation = "NAT", region = c("R1", "R2")),
    geoframes = c("nation", "region"), key = "region", name = "plssg")
  mod <- newModel("plss", repo = newRepository("r",
      newCommodity("COA", timeframe = "ANNUAL"),
      newCommodity("ELC"),
      newCommodity("H2", geoframe = "nation"),
      newSupply("SUP", commodity = "COA", region = c("R1", "R2"),
                supply = data.frame(cost = 1)),
      newTechnology("ECOA", input = list(comm = "COA"),
                    output = list(comm = "ELC"),
                    invcost = data.frame(invcost = 900),
                    vintage = data.frame(olife = 30L), cap2act = 1),
      # charges REGIONAL electricity, stores and releases NATIONAL hydrogen:
      # the only shape a coarse storage can take (a single-commodity storage
      # would need its own commodity both finer and not-finer than itself)
      newStorage("P2G", input = list(comm = "ELC"),
                 storage = list(comm = "H2"), output = list(comm = "H2"),
                 region = "NAT",
                 invcost = data.frame(out.invcost = 20, stg.invcost = 5),
                 vintage = data.frame(olife = 20L)),
      newDemand("DH2", commodity = "H2", region = "NAT",
                demand = data.frame(demand = 3)),
      newDemand("DE", commodity = "ELC", demand = data.frame(demand = 30))),
    calendar = calendars$m12_subset_q1, geoscale = gs,
    region = c("R1", "R2"), horizon = newHorizon(2025), discount = 0)
  s <- suppressMessages(suppressWarnings(solve_model(
    mod, name = "pl_ss", ondisk = FALSE, overwrite = TRUE)))

  inp <- suppressWarnings(getData(s, "vStorageInp", merge = TRUE))
  out <- suppressWarnings(getData(s, "vStorageOut", merge = TRUE))
  lvl <- suppressWarnings(getData(s, "vStorageLevel", merge = TRUE))
  # charging spread to the regions; discharge and reservoir at the storage's own
  # cell (outputs are never spread -- the no-disaggregation rule)
  expect_setequal(unique(as.character(inp$region)), c("R1", "R2"))
  expect_setequal(unique(as.character(out$region)), "NAT")
  expect_setequal(unique(as.character(lvl$region)), "NAT")
  # and the energy balances: what is charged is what is released. A sampled
  # weight leaking into the Agg branch would break this by ~4x.
  expect_equal(sum(inp$value), sum(out$value), tolerance = 1e-6)
})
