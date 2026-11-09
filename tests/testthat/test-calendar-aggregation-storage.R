# =========================================================================== #
# AGGREGATED calendars and storage.
#
# Two reductions share the word "fewer timeslices" and are not the same thing:
#
#   sampled     keep m of G days. Each surviving slice is still a real day's
#               hour -- its `share` is unchanged -- and `weight` = 1/year_fraction
#               annualises the costs. Covered by test-subset_slices.R, which
#               shows the objective is reproduced exactly, storage included.
#
#   aggregated  keep m REPRESENTATIVE days, each standing for G/m real ones.
#               `year_fraction` is 1 and the slice's `share` is scaled up, so a
#               slice is G/m days long.
#
# `pTimesliceWeight` never enters the commodity balance -- it appears only in
# the cost and cumulative equations -- so the two reductions differ only in
# `share`, and `share` is what every per-slice flow bound is scaled by:
#
#     vStorageOut[s] <= cap * cap2act * pTimesliceShare[s]
#
# eqStorageLevel then accumulates those flows, so under an aggregated calendar
# one charge step moves G/m cycles' worth of energy and the reservoir has to
# hold all of it. Everything WITHOUT inter-slice memory is unaffected, which is
# why the no-storage control below is identical across all three calendars.
#
# The repetition factor is already derivable from the calendar -- it is
# share[s] * 8760 / (canonical hours of the leaf frame), and equals 1 for every
# chronological calendar including a sampled one.
#
# @covers vStorageStgCap vStorageLevel pTimesliceShare depth=S backend=glpk
# =========================================================================== #

G <- 6L; H <- 6L; M <- 2L                 # 6 days of 6 hours; keep 2
REP <- G / M                              # each representative day stands for 3

.agg_day <- paste0("d", seq_len(G))
.agg_hour <- sprintf("h%02d", seq_len(H) - 1L)
# tie-free peaky shape, IDENTICAL on every day: the correct answer is then one
# repeating daily cycle, so every calendar that keeps the 6 hours must agree
.agg_shape <- c(2, 2.1, 4, 8, 6, 3)

.agg_model <- function(cal, dem_df, storage = TRUE) {
  objs <- list(
      newCommodity("COA", timeframe = "ANNUAL"),
      newCommodity("ELC", timeframe = "HOUR"),
      newSupply("SUP_COA", commodity = "COA",
                supply = data.frame(region = "R1", cost = 5)),
      newTechnology("ECOA", input = list(comm = "COA"), output = list(comm = "ELC"),
                    invcost = data.frame(region = "R1", invcost = 800),
                    olife = list(olife = 30), cap2act = 1,
                    af = data.frame(af.up = 1)),
      newDemand("DEM_ELC", commodity = "ELC", demand = dem_df))
  if (storage) {
    objs <- c(objs, list(newStorage("STG_ELC", commodity = "ELC",
      invcost = list(out.invcost = 20, stg.invcost = 40),
      duration = data.frame(duration.lo = 0.01, duration.up = 100),
      olife = list(olife = 15))))
  }
  newModel("aggstg", repo = do.call(newRepository, c(list("r"), objs)),
           calendar = cal, region = "R1", horizon = newHorizon(2020),
           discount = 0.05)
}

# full / sampled / aggregated built from ONE timetable, so nothing but the
# calendar differs between the three models
.agg_case <- function(kind, storage = TRUE) {
  tt <- make_timetable(struct = list(ANNUAL = "ANNUAL", DAY = .agg_day,
                                     HOUR = .agg_hour))
  dem <- do.call(rbind, lapply(.agg_day, function(d)
    data.frame(region = "R1", timeslice = paste0(d, "_", .agg_hour),
               demand = .agg_shape)))
  tt1 <- tt[tt$DAY %in% .agg_day[seq_len(M)], ]
  cal <- switch(kind,
    full = newCalendar(timetable = tt, name = "aggfull"),
    sampled = newCalendar(timetable = tt1, name = "aggsmp",
                          year_fraction = sum(tt1$share)),
    aggregated = {
      tt2 <- tt1; tt2$share <- tt2$share * REP
      newCalendar(timetable = tt2, name = "aggagg", year_fraction = 1)
    })
  d <- dem[dem$timeslice %in% as.character(cal@timetable$timeslice), ]
  # a representative slice carries the energy of the REP days it stands for
  if (identical(kind, "aggregated")) d$demand <- d$demand * REP
  mod <- .agg_model(cal, d, storage)
  scen <- suppressMessages(interpolate_model(
    mod, paste0("AGGSTG_", kind, if (storage) "_S" else "_N"),
    ondisk = FALSE, overwrite = TRUE))
  scen <- suppressWarnings(suppressMessages(
    solve_scenario(scen, solver = solver_options$glpk)))
  get1 <- function(v) {
    x <- try(getData(scen, name = v, merge = TRUE), silent = TRUE)
    if (inherits(x, "try-error") || is.null(x) || !nrow(x)) NA_real_ else sum(x$value)
  }
  list(obj = get1("vObjective"), gw = get1("vStorageOutCap"),
       pj = get1("vStorageStgCap"))
}

test_that("a calendar reduction leaves a model WITHOUT storage unchanged", {
  skip_if_no_solver()
  full <- .agg_case("full", storage = FALSE)
  expect_false(is.na(full$obj))
  for (k in c("sampled", "aggregated")) {
    expect_equal(.agg_case(k, storage = FALSE)$obj, full$obj, tolerance = 1e-6,
                 info = k)
  }
})

test_that("a SAMPLED calendar reproduces the full model, storage included", {
  skip_if_no_solver()
  full <- .agg_case("full")
  smp  <- .agg_case("sampled")
  expect_equal(smp$obj, full$obj, tolerance = 1e-6)
  expect_equal(smp$gw,  full$gw,  tolerance = 1e-6)
  expect_equal(smp$pj,  full$pj,  tolerance = 1e-6)
})

test_that("an AGGREGATED calendar inflates the reservoir by the repetition factor", {
  skip_if_no_solver()
  # Current behaviour, pinned so the magnitude cannot drift unnoticed. This is
  # the defect, not the intent: the level is expressed in REP cycles' units
  # while the capacity it is compared against is not, so `vStorageStgCap` comes
  # out REP times too large and the storage looks that much dearer.
  full <- .agg_case("full")
  agg  <- .agg_case("aggregated")
  expect_equal(agg$pj / full$pj, REP, tolerance = 1e-6)
  expect_gt(agg$obj, full$obj)
})

test_that("an AGGREGATED calendar reproduces the full model once cycles are applied", {
  skip("pending: per-slice repetition factor (timescales) and the rep-scaled
        capacity bound in eqStorageLevel's companions")
  full <- .agg_case("full")
  agg  <- .agg_case("aggregated")
  expect_equal(agg$obj, full$obj, tolerance = 1e-6)
  expect_equal(agg$gw,  full$gw,  tolerance = 1e-6)
  expect_equal(agg$pj,  full$pj,  tolerance = 1e-6)
})
