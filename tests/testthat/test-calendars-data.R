# =========================================================================== #
# The shipped `calendars` dataset: every entry well-formed, the timescales
# imports present with their expected shapes, sampled entries solver-correct.
# Tests the SHIPPED objects -- no timescales needed at test time.
# =========================================================================== #

test_that("every shipped calendar is internally consistent", {
  for (nm in names(calendars)) {
    x <- calendars[[nm]]
    expect_s4_class(x, "calendar")
    expect_lt(abs(sum(x@timetable$share) - x@year_fraction), 1e-7)
    ss <- as.data.frame(x@timeslice_share)
    expect_equal(ss$share[1], x@year_fraction, tolerance = 1e-9,
                 label = paste0(nm, ": top share == year_fraction"))
    expect_equal(ss$weight[1], 1 / x@year_fraction, tolerance = 1e-9,
                 label = paste0(nm, ": top weight == 1/year_fraction"))
    expect_identical(x@name, nm)
  }
})

test_that("the timescales imports ship with their catalog shapes", {
  rows <- c(m12 = 12, m12a = 12, q4 = 4, s4 = 4, s4_h24 = 96,
            m12_h24 = 288, wd7_h24 = 168, w52_h24 = 1248)
  for (nm in names(rows)) {
    x <- calendars[[nm]]
    expect_false(is.null(x), label = paste0(nm, " present"))
    expect_equal(nrow(x@timetable), unname(rows[nm]),
                     label = paste0(nm, " row count"))
    expect_equal(x@year_fraction, 1)
  }
  # day-proportional shares, not equal splits
  s4 <- as.data.frame(calendars$s4@timetable)
  expect_equal(sort(round(s4$share * 365)), c(90, 91, 92, 92))
  m12 <- as.data.frame(calendars$m12@timetable)
  expect_equal(round(m12$share * 365)[1:3], c(31, 28, 31))
  # unified season vocabulary, calendar order
  expect_identical(s4$SEASON, c("WIN", "SPR", "SUM", "FAL"))
})

test_that("sampled calendars carry the surviving year fraction", {
  expect_equal(calendars$m12_subset_q1@year_fraction, 90 / 365,
               tolerance = 1e-9)
  expect_equal(calendars$s4_h24_subset_2seasons@year_fraction,
               (90 + 92) / 365, tolerance = 1e-9)
  expect_equal(calendars$m12_h24_subset_4months@year_fraction,
               (31 + 30 + 31 + 31) / 365, tolerance = 1e-9)
  expect_lt(nrow(calendars$s4_h24_subset_2seasons@timetable),
            nrow(calendars$s4_h24@timetable))
})

test_that("calendar chronology follows the year, not the alphabet", {
  ny <- as.data.frame(calendars$s4_h24@next_in_year)
  succ <- function(x) ny[ny[[1]] == x, 2][1]
  expect_identical(succ("WIN_h23"), "SPR_h00")
  expect_identical(succ("SUM_h23"), "FAL_h00")
  expect_identical(succ("FAL_h23"), "WIN_h00")   # wraps the year
  nm <- as.data.frame(calendars$m12_h24@next_in_year)
  expect_identical(nm[nm[[1]] == "m01_h23", 2][1], "m02_h00")
})

# @covers pTimesliceShare depth=S backends=glpk forks=sampled
test_that("a shipped sampled calendar reproduces the full objective", {
  skip_if_no_solver()
  # grouped-identical INTENSITY: per-slice demand proportional to the
  # slice share (constant load), so sampling must reproduce the full
  # objective exactly (the test-subset_slices.R ground truth) even with
  # day-proportional shares -- here with the SHIPPED s4_h24 /
  # s4_h24_subset_2seasons pair
  mk <- function(cal) {
    tt <- as.data.frame(cal@timetable)
    sl <- tt$timeslice
    newModel("caldata",
      repo = newRepository("r",
        newCommodity("COA", timeframe = "ANNUAL"),
        newCommodity("ELC", timeframe = "HOUR"),
        newSupply("SUP_COA", commodity = "COA",
                  supply = data.frame(region = "R1", cost = 5)),
        newTechnology("ECOA", input = list(comm = "COA"),
                      output = list(comm = "ELC"),
                      af = data.frame(af.up = 0.9),
                      invcost = data.frame(invcost = 1000),
                      olife = list(olife = 30), cap2act = 1),
        newDemand("DEM_ELC", commodity = "ELC",
                  demand = data.frame(region = "R1", timeslice = sl,
                                      demand = 960 * tt$share))),
      calendar = cal, region = "R1", horizon = newHorizon(2020),
      discount = 0.05)
  }
  obj <- function(cal, tag) {
    td <- file.path(tempdir(), paste0("caldata_", tag))
    on.exit(unlink(td, recursive = TRUE), add = TRUE)
    s <- suppressWarnings(suppressMessages(solve_scenario(
      interpolate_model(mk(cal), tag, ondisk = FALSE),
      solver = solver_options$glpk, tmp.dir = td, tmp.del = TRUE,
      force = TRUE)))
    getData(s, "vObjective", merge = TRUE)$value
  }
  o_full <- obj(calendars$s4_h24, "full")
  o_smpl <- obj(calendars$s4_h24_subset_2seasons, "smpl")
  expect_equal(o_smpl, o_full, tolerance = 1e-6)
})
