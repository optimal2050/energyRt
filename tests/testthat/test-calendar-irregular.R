# =========================================================================== #
# Irregular calendars: a level whose parents have different numbers of
# children (months of 28-31 days). The calendar is built from the
# timetable's rows; regular calendars come out as before.
# =========================================================================== #

# three months of 31, 28 and 31 days, two 12-hour slices a day
.irr_timetable <- function() {
  days <- c(m01 = 31, m02 = 28, m03 = 31)
  mon <- rep(names(days), days)
  yday <- sprintf("d%03d", seq_along(mon))
  tt <- data.frame(
    ANNUAL = "ANNUAL",
    MONTH = rep(mon, each = 2),
    YDAY = rep(yday, each = 2),
    HOUR = rep(c("h00", "h12"), length(yday))
  )
  tt$timeslice <- paste(tt$YDAY, tt$HOUR, sep = "_")
  tt$share <- 1 / nrow(tt)
  tt
}

test_that("an irregular calendar has the right structure", {
  cal <- suppressMessages(newCalendar(name = "irr", timetable = .irr_timetable()))
  expect_identical(lengths(cal@timeframes),
                   c(ANNUAL = 1L, MONTH = 3L, YDAY = 90L, HOUR = 180L))
  # names: the shortest unique label; leaves keep the given timeslice names
  expect_identical(cal@timeframes$MONTH, c("m01", "m02", "m03"))
  expect_identical(cal@timeframes$YDAY[1:2], c("d001", "d002"))
  expect_identical(cal@timeframes$HOUR[1:2], c("d001_h00", "d001_h12"))
  # shares follow the days, weights do not depend on the number of children
  ss <- as.data.frame(cal@timeslice_share)
  sh <- setNames(ss$share, ss$timeslice)
  expect_equal(unname(sh[c("m01", "m02", "m03")]), c(31, 28, 31) / 90)
  expect_equal(ss$weight, rep(1, nrow(ss)))
  # 31, 28 and 31 days under the months
  fam <- as.data.frame(cal@timeslice_family)
  expect_identical(as.vector(table(fam$parent[fam$child %in% cal@timeframes$YDAY])),
                   c(31L, 28L, 31L))
  anc <- as.data.frame(cal@timeslice_ancestry)
  expect_equal(sum(anc$parent == "m02" & anc$child %in% cal@timeframes$HOUR), 56)
  expect_equal(sum(anc$parent == "ANNUAL"), 3 + 90 + 180)
  # cycles close within each parent; the year runs through the months
  nx <- as.data.frame(cal@next_in_timeframe)
  succ <- function(d, x) d[d[[1]] == x, 2]
  expect_identical(succ(nx, "d059"), "d032")       # Feb 28 -> Feb 1
  expect_identical(succ(nx, "d031"), "d001")       # Jan 31 -> Jan 1
  expect_identical(succ(nx, "d001_h12"), "d001_h00")
  expect_identical(succ(nx, "m03"), "m01")
  ny <- as.data.frame(cal@next_in_year)
  expect_identical(succ(ny, "d031"), "d032")
  expect_identical(succ(ny, "d090"), "d001")
})

test_that("shipped regular calendars rebuild unchanged from their timetables", {
  for (nm in names(calendars)) {
    x <- calendars[[nm]]
    y <- suppressMessages(newCalendar(name = x@name, desc = x@desc,
                                      timetable = x@timetable,
                                      year_fraction = x@year_fraction))
    for (sl in c("timeframes", "timeslice_share", "timeslice_family",
                 "timeslice_ancestry", "next_in_timeframe", "next_in_year")) {
      a <- methods::slot(x, sl)
      b <- methods::slot(y, sl)
      if (is.data.frame(a)) {
        a <- as.data.frame(a)
        b <- as.data.frame(b)
        rownames(a) <- rownames(b) <- NULL
      }
      expect_equal(b, a, tolerance = 1e-12, label = paste(nm, sl))
    }
  }
})

test_that("a ragged sample keeps the full calendar's names", {
  cal <- suppressMessages(newCalendar(name = "irr", timetable = .irr_timetable()))
  # an uneven pick across days and months
  pick <- c("d001_h00", "d001_h12", "d002_h12", "d032_h00", "d033_h12")
  sc <- .sample_calendar(cal, pick)
  expect_identical(sc@timeframes$HOUR, pick)
  expect_identical(sc@timeframes$YDAY, c("d001", "d002", "d032", "d033"))
  expect_identical(sc@timeframes$MONTH, c("m01", "m02"))
  fam <- as.data.frame(sc@timeslice_family)
  expect_identical(fam$parent[fam$child == "d002_h12"], "d002")
})

test_that("a timetable out of time order is refused", {
  tt <- .irr_timetable()
  tt <- tt[c(2, 1, 3:nrow(tt)), ]
  expect_silent(suppressMessages(newCalendar(name = "ok", timetable = tt)))
  tt <- .irr_timetable()
  tt <- tt[order(tt$HOUR, seq_len(nrow(tt))), ]
  expect_error(suppressMessages(newCalendar(name = "bad", timetable = tt)),
               "chronology")
})

test_that("timeslice names may not repeat across levels", {
  tt <- .irr_timetable()
  tt$timeslice <- tt$YDAY
  tt$timeslice[seq(2, nrow(tt), 2)] <- paste0(tt$YDAY[seq(2, nrow(tt), 2)], "x")
  expect_error(suppressMessages(newCalendar(name = "bad", timetable = tt)),
               "repeat across timeframes")
})

test_that("a monthly commodity balances over months of 28-31 days", {
  skip_if_no_solver()
  tt <- .irr_timetable()
  cal <- suppressMessages(newCalendar(name = "irr", timetable = tt))
  mod <- newModel(
    "IRR", region = "R1", calendar = cal, horizon = newHorizon(2025),
    repo = newRepository(
      "r",
      newCommodity("ELC", timeframe = "HOUR"),
      newCommodity("GAS", timeframe = "ANNUAL"),
      newCommodity("WAT", timeframe = "MONTH"),
      newSupply("SUP_GAS", commodity = "GAS", supply = data.frame(cost = 30)),
      newSupply("SUP_WAT", commodity = "WAT",
                supply = data.frame(timeslice = c("m01", "m02", "m03"),
                                    ava.up = c(2e5, 3e4, 2e5), cost = 0.1)),
      newTechnology("PP", input = list(comm = "GAS"), output = list(comm = "ELC"),
                    ceff = data.frame(comm = "GAS", cinp2use = 2),
                    cap2act = 8760,
                    aux = data.frame(acomm = "WAT"),
                    aeff = data.frame(acomm = "WAT", act2ainp = 2),
                    capacity = data.frame(stock = 100)),
      newDemand("DEM", commodity = "ELC",
                demand = data.frame(timeslice = tt$timeslice, demand = 720)),
      newImport("IMP_ELC", commodity = "ELC", import = data.frame(price = 500))
    ))
  td <- file.path(tempdir(), "cal_irregular")
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  s <- suppressWarnings(suppressMessages(solve_scenario(
    interpolate_model(mod, "IRR", ondisk = FALSE),
    solver = solver_options$glpk, tmp.dir = td, tmp.del = TRUE, force = TRUE)))
  # Feb water caps generation at 15,000 MWh against 40,320 demanded
  w <- as.data.frame(getData(s, "vTechAInp", merge = TRUE))
  w <- setNames(w$value, w$timeslice)
  expect_equal(unname(w[c("m01", "m02", "m03")]), c(89280, 30000, 89280))
  # gas 52,140 x 30 + water 208,560 x 0.1 + import 25,320 x 500
  expect_equal(getData(s, "vObjective", merge = TRUE)$value, 14245056,
               tolerance = 1e-8)
})
