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

test_that("family process-levels: region is enabled, timeslice is not", {
  expect_true(energyRt:::.coarse_axis_ready("region"))
  expect_false(energyRt:::.coarse_axis_ready("timeslice"))
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
  # two inputs -> not single-in/single-out
  t2 <- newTechnology("T", input = list(comm = c("A", "C")),
                      output = list(comm = "B"),
                      invcost = data.frame(invcost = 1),
                      vintage = data.frame(olife = 10L), cap2act = 1)
  expect_match(paste(blk(t2), collapse = "|"), "more than one input")

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
