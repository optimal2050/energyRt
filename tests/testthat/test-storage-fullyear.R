# =========================================================================== #
# storage@fullYear -- where the state-of-charge cycle closes.
#
# `meqStorageStore` pairs each storing timeslice with its PRECEDING one, and
# which predecessor that is depends on @fullYear:
#
#   TRUE  (default)  the cycle spans the whole year. On a calendar of days over
#                    hours the predecessor of `d2_h1` is `d1_h4`, and only the
#                    very last timeslice wraps back to the first.
#   FALSE            the cycle closes inside each parent timeframe. The
#                    predecessor of `d1_h1` is `d1_h4`, so every day is an
#                    independent loop and no energy crosses a day boundary.
#
# The flag was silently ignored for years: `.build_meqStorageStore()` joined
# `mTimesliceNext` unconditionally, so every storage behaved as `FALSE`
# regardless -- which made multi-day and seasonal storage unrepresentable and,
# because the default is TRUE, did so without any user asking for it. It
# surfaced when a converted PyPSA-Eur battery came out 2.1x oversized and idle
# 6,624 hours of 8,760 against PyPSA's cyclic-over-the-year equivalent.
#
# These tests pin the map itself (no solver) and then the behaviour it buys.
# =========================================================================== #

# Two days of four hours: the smallest calendar on which "within the parent
# timeframe" and "through the year" differ.
fy_calendar <- function() {
  newCalendar(
    timetable = make_timetable(
      struct = list(ANNUAL = "ANNUAL", DAY = c("d1", "d2"), HOUR = paste0("h", 1:4))
    ),
    name = "fy_d2h4"
  )
}

fy_hours <- function(day) paste0(day, "_h", 1:4)

# Demand falls entirely on day 2; the cheap plant runs only on day 1. Storage is
# therefore the ONLY way to serve day 2 cheaply, and only if it may carry energy
# across the day boundary. `EEXP` is an always-available backstop so the model
# stays feasible when it may not.
fy_model <- function(fullYear = TRUE, name = "fy") {
  newModel(
    name = name, desc = "", calendar = fy_calendar(), region = "R1",
    horizon = newHorizon(2020), discount = 0,
    repo = newRepository(
      paste0("repo_", name),
      newCommodity("COA", timeframe = "ANNUAL"),
      newCommodity("ELC", timeframe = "HOUR"),
      newSupply("SUP_COA", commodity = "COA", supply = data.frame(cost = 1)),
      newTechnology(
        "ECHEAP", input = list(comm = "COA"), output = list(comm = "ELC"),
        invcost = data.frame(invcost = 10),
        af = data.frame(timeslice = fy_hours("d2"), af.up = 0),
        vintage = data.frame(olife = 30L), cap2act = 1
      ),
      newTechnology(
        "EEXP", input = list(comm = "COA"), output = list(comm = "ELC"),
        invcost = data.frame(invcost = 5000),
        vintage = data.frame(olife = 30L), cap2act = 1
      ),
      newStorage(
        "STG", commodity = "ELC", invcost = list(invcost = 1),
        vintage = data.frame(olife = 30L), fullYear = fullYear
      ),
      newDemand("DEM_ELC", commodity = "ELC",
                demand = data.frame(timeslice = fy_hours("d2"), demand = 10))
    )
  )
}

fy_interp <- function(fullYear) {
  suppressMessages(suppressWarnings(
    interpolate_model(fy_model(fullYear), name = "fy", ondisk = FALSE)
  ))
}

# (timeslicep, timeslice) pairs of the storage balance, as "prev>curr" strings.
fy_pairs <- function(scen) {
  gds <- getFromNamespace("get_data_slot", "energyRt")
  d <- as.data.frame(gds(scen@modInp@parameters[["meqStorageStore"]]))
  paste(d$timeslicep, d$timeslice, sep = ">")
}

test_that("fullYear = TRUE closes the storage cycle over the whole year", {
  pairs <- fy_pairs(fy_interp(TRUE))

  # The day boundary is crossed: d1_h4 precedes d2_h1.
  expect_true("d1_h4>d2_h1" %in% pairs)
  # ... and NOT recycled inside day 1.
  expect_false("d1_h4>d1_h1" %in% pairs)
  # Only the final timeslice wraps, and it wraps to the very first.
  expect_true("d2_h4>d1_h1" %in% pairs)
  expect_false("d2_h4>d2_h1" %in% pairs)

  # One closed chain over every hourly timeslice, each appearing exactly once as
  # a predecessor and once as a successor.
  expect_equal(length(pairs), 8L)
  expect_equal(anyDuplicated(pairs), 0L)
})

test_that("fullYear = FALSE closes the storage cycle within each parent timeframe", {
  pairs <- fy_pairs(fy_interp(FALSE))

  # Each day is an independent loop.
  expect_true("d1_h4>d1_h1" %in% pairs)
  expect_true("d2_h4>d2_h1" %in% pairs)
  # Nothing crosses the boundary in either direction.
  expect_false("d1_h4>d2_h1" %in% pairs)
  expect_false("d2_h4>d1_h1" %in% pairs)

  expect_equal(length(pairs), 8L)
})

test_that("the two settings differ only at the parent-timeframe boundaries", {
  # Everything strictly inside a day is identical; the flag must be a branch,
  # not a wholesale change of the balance.
  interior <- c("d1_h1>d1_h2", "d1_h2>d1_h3", "d1_h3>d1_h4",
                "d2_h1>d2_h2", "d2_h2>d2_h3", "d2_h3>d2_h4")
  expect_true(all(interior %in% fy_pairs(fy_interp(TRUE))))
  expect_true(all(interior %in% fy_pairs(fy_interp(FALSE))))
})

test_that("fullYear = TRUE lets storage carry energy across a day boundary", {
  skip_if_no_solver()

  solve_fy <- function(fullYear, nm) {
    suppressMessages(suppressWarnings(
      solve_model(fy_model(fullYear, name = nm), name = nm)
    ))
  }
  s_fy <- solve_fy(TRUE,  "fy_on")
  s_pf <- solve_fy(FALSE, "fy_off")

  got <- function(scen, v) {
    d <- try(as.data.frame(getData(scen, v, merge = TRUE, drop.zeros = FALSE)),
             silent = TRUE)
    if (inherits(d, "try-error") || is.null(d) || !NROW(d)) return(0)
    sum(d$value)
  }

  # Day-2 demand is served from storage when the cycle spans the year, and not
  # at all when it does not -- day 1 has the only cheap generation and day 2 the
  # only demand.
  expect_gt(got(s_fy, "vStorageOut"), 0)
  expect_equal(got(s_pf, "vStorageOut"), 0, tolerance = 1e-6)

  # Storing across the boundary displaces the expensive backstop, so it must be
  # strictly cheaper. This is the half that makes it an economics test rather
  # than a bookkeeping one.
  obj <- function(scen) getData(scen, "vObjective", merge = TRUE)$value[1]
  expect_lt(obj(s_fy), obj(s_pf))
})
