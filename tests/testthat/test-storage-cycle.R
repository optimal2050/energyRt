# =========================================================================== #
# Where a storage cycle closes, and whether it crosses a sampling gap.
#
# Two settings, one equation. `storage_cycle` overrides every object's own
# `storage@fullYear` for one build -- the cycle closes over the year or at each
# parent timeframe boundary. `bridge_storage_gaps` answers a different
# question, and only on a SAMPLED calendar: a sample keeps blocks of the year
# and drops the rest, so its successor relation chains blocks that are months
# apart as if they were adjacent. `FALSE` (the default) closes each contiguous
# block onto its own first timeslice instead.
#
# The two compose rather than conflict. `@fullYear` states the physics and
# holds in every milestone; the gap question is about where a year's calendar
# stops supporting that claim, so a reservoir on a model with one hourly
# milestone and one sampled one keeps its year cycle where the chronology is
# real and closes per block where it is not.
#
# A gap is never found by reading a timeslice label: the reference calendar's
# own successor relation is the chronology.
# =========================================================================== #

source(test_path("fixtures", "testing-models.R"))

# A three-level calendar, so the parent-timeframe cycle and the year cycle are
# genuinely different relations (with one sub-annual level they coincide).
.sg_cal <- function() {
  newCalendar(name = "sg_cal", timetable = make_timetable(list(
    ANNUAL = "ANNUAL",
    SEASON = c("WIN", "SPR", "SUM", "AUT"),
    DAYNITE = c("DAY", "NGT"))))
}

# One region, one plant, one store, electricity balanced at the leaf level.
.sg_model <- function(cal = .sg_cal()) {
  newModel("sg", calendar = cal, region = "R1", discount = 0.05,
           horizon = newHorizon(period = 2030:2039, intervals = 5),
           repo = newRepository(
             "sg",
             newCommodity("COA", timeframe = "ANNUAL"),
             newCommodity("ELC", timeframe = "DAYNITE"),
             newSupply("SUP_COA", commodity = "COA",
                       supply = data.frame(region = "R1", cost = 1)),
             newTechnology("ECOA", input = list(comm = "COA"),
                           output = list(comm = "ELC"), cap2act = 1,
                           invcost = data.frame(region = "R1", invcost = 1000),
                           olife = list(olife = 30)),
             newDemand("DEM_ELC", commodity = "ELC",
                       demand = data.frame(
                         timeslice = cal@timeframes$DAYNITE,
                         demand = rep(c(10, 4), times = 4))),
             newStorage("STG_ELC", commodity = "ELC", duration = 1,
                        invcost = list(out.invcost = 50),
                        olife = list(olife = 15))))
}

# The storage balance's index, as `(year, slice, predecessor)`.
.sg_rel <- function(scen, year = NULL) {
  d <- as.data.frame(get_data_slot(
    scen@modInp@parameters[["meqStorageLevel"]]))
  d <- unique(d[, c("year", "timeslice", "timeslicep")])
  if (!is.null(year)) d <- d[d$year == year, , drop = FALSE]
  d <- d[order(d$year, d$timeslice), , drop = FALSE]
  row.names(d) <- NULL
  d
}

.sg_pairs <- function(cal, slot_nm) {
  d <- as.data.frame(methods::slot(cal, slot_nm))
  paste(d$timeslice, d$timeslicep)
}


# --------------------------------------------------------------------------- #
# 1. the block algorithm
# --------------------------------------------------------------------------- #

test_that("a gap-free cycle is returned untouched", {
  f <- energyRt:::.block_cyclic_one
  ref <- paste(paste0("s", 1:8), paste0("s", c(2:8, 1)))
  # the full year
  expect_equal(f(paste0("s", 1:8), paste0("s", c(2:8, 1)), ref),
               paste0("s", c(2:8, 1)))
  # one unbroken block already closes on itself
  expect_equal(f(c("s3", "s4", "s5"), c("s4", "s5", "s3"), ref),
               c("s4", "s5", "s3"))
})

test_that("each contiguous block closes onto its own first timeslice", {
  f <- energyRt:::.block_cyclic_one
  ref <- paste(paste0("s", 1:8), paste0("s", c(2:8, 1)))
  # two blocks of two: s2 -> s1 and s6 -> s5, not s2 -> s5
  expect_equal(f(c("s1", "s2", "s5", "s6"), c("s2", "s5", "s6", "s1"), ref),
               c("s2", "s1", "s6", "s5"))
  # three blocks, two of them single timeslices
  expect_equal(f(c("s1", "s3", "s5", "s6"), c("s3", "s5", "s6", "s1"), ref),
               c("s1", "s3", "s6", "s5"))
  # a lone timeslice is its own cycle
  expect_equal(f(c("s1", "s5"), c("s5", "s1"), ref), c("s1", "s5"))
})

test_that("a block spanning the year's turn stays one block", {
  # the sample keeps the year's last two timeslices and its first two, which
  # ARE contiguous across the turn: one block s7 -> s8 -> s1 -> s2 -> s7
  f <- energyRt:::.block_cyclic_one
  ref <- paste(paste0("s", 1:8), paste0("s", c(2:8, 1)))
  expect_equal(f(c("s1", "s2", "s7", "s8"), c("s2", "s7", "s8", "s1"), ref),
               c("s2", "s7", "s8", "s1"))
})

test_that("every level of a sampled calendar is rewritten on its own", {
  cal <- .sg_cal()
  smp <- sample_calendar(cal, grep("^(AUT|SUM)", cal@timetable$timeslice,
                                   value = TRUE), name = "two")
  out <- energyRt:::.block_cyclic_next(as.data.frame(smp@next_in_year),
                                       .sg_pairs(cal, "next_in_year"))
  got <- stats::setNames(out$timeslicep, out$timeslice)
  # SEASON: AUT and SUM are not adjacent, so each is its own cycle
  expect_equal(got[["AUT"]], "AUT")
  expect_equal(got[["SUM"]], "SUM")
  # DAYNITE: two blocks of two, each closing inside its own season
  expect_equal(got[["AUT_NGT"]], "AUT_DAY")  # was SUM_DAY
  expect_equal(got[["SUM_NGT"]], "SUM_DAY")  # was AUT_DAY
  expect_equal(got[["AUT_DAY"]], "AUT_NGT")
  expect_equal(got[["SUM_DAY"]], "SUM_NGT")
})


# --------------------------------------------------------------------------- #
# 2. storage_cycle: the override
# --------------------------------------------------------------------------- #

test_that("storage_cycle picks the relation, leaving @fullYear alone", {
  mod <- .sg_model()
  tf <- interpolate_model(mod, name = "sgTF", storage_cycle = "timeframe",
                          overwrite = TRUE, verbose = FALSE)
  yr <- interpolate_model(mod, name = "sgYR", storage_cycle = "year",
                          overwrite = TRUE, verbose = FALSE)
  rtf <- .sg_rel(tf, 2030)
  ryr <- .sg_rel(yr, 2030)
  # the parent-timeframe cycle closes inside each season: every predecessor
  # shares its slice's season
  expect_true(all(sub("_.*$", "", rtf$timeslice) ==
                    sub("_.*$", "", rtf$timeslicep)))
  # the year cycle crosses season boundaries
  expect_false(all(sub("_.*$", "", ryr$timeslice) ==
                     sub("_.*$", "", ryr$timeslicep)))
  # `@fullYear` is TRUE by default, so "year" is what the objects already ask
  # for -- and the model itself is untouched by either call
  plain <- interpolate_model(mod, name = "sgPL", overwrite = TRUE,
                             verbose = FALSE)
  expect_equal(.sg_rel(plain, 2030), ryr)
  expect_true(mod@data[[1]]@data$STG_ELC@fullYear)
})

test_that("storage_cycle refuses a value that is not a cycle", {
  expect_error(interpolate_model(.sg_model(), name = "sgX",
                                 storage_cycle = "block", overwrite = TRUE,
                                 verbose = FALSE))
})


# --------------------------------------------------------------------------- #
# 3. bridge_storage_gaps on a milestone with its own sampled calendar
# --------------------------------------------------------------------------- #

# AUT and SUM are not adjacent in `cal_test` (whose chronology is
# AUT -> SPR -> SUM -> WIN), so this sample is two blocks, not one.
.sg_sampled_year <- function() {
  mod <- tm_core()
  mid <- as.integer(mod@config@horizon@intervals$mid)
  smp <- sample_calendar(getCalendar(mod), c("AUT", "SUM"), name = "half")
  list(model = setCalendar(mod, smp, years = mid[2]),
       full = mid[1], sampled = mid[2])
}

test_that("the default closes a sampled year's blocks; bridge = TRUE does not", {
  k <- suppressWarnings(.sg_sampled_year())
  br <- suppressWarnings(interpolate_model(
    k$model, name = "sgB", bridge_storage_gaps = TRUE, overwrite = TRUE,
    verbose = FALSE))
  cy <- suppressWarnings(interpolate_model(
    k$model, name = "sgC", overwrite = TRUE, verbose = FALSE))
  rb <- .sg_rel(br, k$sampled)
  rc <- .sg_rel(cy, k$sampled)
  # bridged: the two blocks chain into each other
  expect_equal(rb$timeslicep[rb$timeslice == "AUT"], "SUM")
  expect_equal(rb$timeslicep[rb$timeslice == "SUM"], "AUT")
  # closed: each block is its own cycle
  expect_equal(rc$timeslicep[rc$timeslice == "AUT"], "AUT")
  expect_equal(rc$timeslicep[rc$timeslice == "SUM"], "SUM")
  # the FULL-calendar year has no gaps and is identical either way
  expect_equal(.sg_rel(cy, k$full), .sg_rel(br, k$full))
})

test_that("closing the gaps cannot make the model cheaper", {
  # block-cyclic storage shrinks the feasible set -- every block-cyclic
  # solution is feasible in the bridged model and not conversely -- so the
  # objective may only rise. A cheaper closed run is a defect, not a result.
  k <- suppressWarnings(.sg_sampled_year())
  br <- suppressWarnings(interpolate_model(
    k$model, name = "sgBS", bridge_storage_gaps = TRUE, overwrite = TRUE,
    verbose = FALSE))
  cy <- suppressWarnings(interpolate_model(
    k$model, name = "sgCS", overwrite = TRUE, verbose = FALSE))
  obj <- function(s) {
    s <- tryCatch(
      suppressMessages(solve_scenario(s, solver = "glpk", verbose = FALSE)),
      error = function(e) NULL)
    if (is.null(s)) return(NA_real_)
    as.numeric(get_variable(s, "vObjective", data = TRUE)$value[1])
  }
  ob <- obj(br)
  oc <- obj(cy)
  skip_if(is.na(ob) || is.na(oc), "glpk not available")
  expect_gte(oc, ob - 1e-6)
})

test_that("a model whose own calendar is the sample closes its gaps too", {
  mod <- tm_core()
  smp <- sample_calendar(getCalendar(mod), c("AUT", "SUM"), name = "half")
  scen <- suppressWarnings(interpolate_model(
    mod, smp, name = "sgA", overwrite = TRUE, verbose = FALSE))
  expect_null(getYearCalendars(scen))
  r <- .sg_rel(scen)
  expect_true(all(r$timeslice == r$timeslicep))
})

test_that("a full calendar keeps the year cycle", {
  mod <- tm_core()
  scen <- suppressWarnings(interpolate_model(mod, name = "sgF",
                                             overwrite = TRUE, verbose = FALSE))
  r <- .sg_rel(scen)
  # every predecessor is a different timeslice: nothing was closed
  expect_true(all(r$timeslice != r$timeslicep))
})

test_that("a sample with no recorded source warns rather than guessing", {
  mod <- tm_core()
  smp <- sample_calendar(getCalendar(mod), c("AUT", "SUM"), name = "half")
  smp@misc$sample_of <- NULL     # as if sampled by hand
  expect_warning(
    interpolate_model(mod, smp, name = "sgN", overwrite = TRUE,
                      verbose = FALSE),
    "gaps cannot be located")
})


# --------------------------------------------------------------------------- #
# 4. the reference a sample records
# --------------------------------------------------------------------------- #

test_that("sample_calendar records the chronology it sampled", {
  cal <- .sg_cal()
  smp <- sample_calendar(cal, grep("^(AUT|SUM)", cal@timetable$timeslice,
                                   value = TRUE), name = "two")
  expect_equal(smp@misc$sample_of$name, "sg_cal")
  expect_equal(.sg_pairs(cal, "next_in_year"),
               paste(smp@misc$sample_of$next_in_year$timeslice,
                     smp@misc$sample_of$next_in_year$timeslicep))
  # a sample of a sample keeps the DEEPEST reference: the intermediate already
  # has gaps of its own, and judging against it would call them contiguous
  # (three seasons first, since a sample keeps >= 2 members at every level)
  sm1 <- sample_calendar(cal, grep("^(AUT|SPR|SUM)", cal@timetable$timeslice,
                                   value = TRUE), name = "three")
  sm2 <- sample_calendar(sm1, grep("^(AUT|SUM)", sm1@timetable$timeslice,
                                   value = TRUE), name = "two_of_three")
  expect_equal(sm2@misc$sample_of$name, "sg_cal")
})


# --------------------------------------------------------------------------- #
# 5. the record, and the base-reuse refusal
# --------------------------------------------------------------------------- #

test_that("both settings are recorded, and refuse to reuse a base", {
  mod <- .sg_model()
  scen <- interpolate_model(mod, name = "sgR", storage_cycle = "timeframe",
                            overwrite = TRUE, verbose = FALSE)
  expect_equal(scen@misc$storage$cycle, "timeframe")
  expect_false(scen@misc$storage$bridge_gaps)
  # a default build records nothing: there is nothing to reproduce
  plain <- interpolate_model(mod, name = "sgR0", overwrite = TRUE,
                             verbose = FALSE)
  expect_null(plain@misc$storage)
  # either setting rewrites the storage maps, so neither can be layered onto
  # an existing base
  expect_true(all(c("storage_cycle", "bridge_storage_gaps") %in%
                    energyRt:::.base_full_args))
})
