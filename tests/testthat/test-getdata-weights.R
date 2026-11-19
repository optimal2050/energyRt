# =========================================================================== #
# getData(add_weights =, apply_weights =, add_period_length =,
#         apply_period_length =)
#
# The timeslice weight is `pTimesliceWeight(year, timeslice)`: how many times a
# slice occurs in its year -- 1 on a full calendar, 1/year_fraction on a
# sampled one, and 1 at the top (ANNUAL) slice always. The property that
# matters on a SAMPLED scenario is therefore
#
#   sum(value * weight) at native resolution == value at timeframe = "lowest"
#
# and applying the weights after that roll-up must multiply by 1 rather than
# annualise twice.
#
# What is multiplied is gated by the declared catalogue, not by the presence of
# a `timeslice` column: only a `sum` on the time axis is a quantity to weight.
# =========================================================================== #

.gw_K <- 12L
.gw_m <- 4L
.gw_sl <- sprintf("s%02d", seq_len(.gw_K))

.gw_timetable <- function() {
  make_timetable(struct = list(ANNUAL = "ANNUAL", TIMESLICE = .gw_sl))
}

# `slices` are the timeslices the calendar actually carries, which is also
# where the demand is declared.
.gw_model <- function(cal, slices) {
  newModel(
    "gw",
    repo = newRepository(
      "r",
      newCommodity("COA", timeframe = "ANNUAL"),
      newCommodity("ELC", timeframe = "TIMESLICE"),
      newSupply("SUP_COA", commodity = "COA",
                supply = data.frame(region = "R1", cost = 5)),
      newTechnology("ECOA", input = list(comm = "COA"),
                    output = list(comm = "ELC"),
                    af = data.frame(af.up = 0.5),
                    invcost = data.frame(region = "R1", invcost = 1000),
                    olife = list(olife = 30), cap2act = 1),
      newStorage("STG_ELC", commodity = "ELC", duration = 1,
                 invcost = list(out.invcost = 50),
                 olife = list(olife = 15)),
      # a demand that differs between slices, so storage has something to earn
      newDemand("DEM_ELC", commodity = "ELC",
                demand = data.frame(
                  region = "R1", timeslice = slices,
                  demand = 10 + seq_along(slices) %% 3L))
    ),
    calendar = cal, region = "R1",
    horizon = newHorizon(2021:2040, 10), discount = 0.05)
}

.gw_solve <- function(mod, name) {
  suppressMessages(suppressWarnings(
    solve_model(mod, name = name, solver = solver_options$glpk,
                tmp.del = TRUE, wait = TRUE)))
}

# one solve each, reused by every test below
.gw_scen <- local({
  cache <- list()
  function(which = c("sampled", "full")) {
    which <- match.arg(which)
    if (!is.null(cache[[which]])) return(cache[[which]])
    tt <- .gw_timetable()
    if (identical(which, "sampled")) {
      tt <- tt[tt$TIMESLICE %in% .gw_sl[seq_len(.gw_m)], ]
      # the sample's shares sum to m/K, so newCalendar normalises the declared
      # weights to K/m -- the warning IS the mechanism under test
      cal <- suppressWarnings(
        newCalendar(timetable = tt, year_fraction = sum(tt$share),
                    name = "gw_sub"))
      sl <- .gw_sl[seq_len(.gw_m)]
    } else {
      cal <- newCalendar(timetable = tt, name = "gw_full")
      sl <- .gw_sl
    }
    s <- .gw_solve(.gw_model(cal, sl), paste0("gw_", which))
    cache[[which]] <<- s
    s
  }
})

.gw_get <- function(scen, nm, ...) {
  suppressMessages(as.data.frame(getData(scen, nm, merge = TRUE, ...)))
}

# --------------------------------------------------------------------------- #

# @covers getData pTimesliceWeight depth=S backends=glpk
test_that("on a full calendar every weight is 1 and apply_weights is identity", {
  skip_if_no_solver()
  scen <- .gw_scen("full")
  d0 <- .gw_get(scen, "vTechOut")
  skip_if(nrow(d0) == 0, "no vTechOut rows")
  d1 <- .gw_get(scen, "vTechOut", add_weights = TRUE)
  expect_true(all(d1$weight == 1))
  d2 <- .gw_get(scen, "vTechOut", apply_weights = TRUE)
  expect_equal(d2$value, d0$value)
})

# @covers getData pTimesliceWeight pYearFraction depth=S backends=glpk forks=sampled
test_that("a sampled calendar weights each slice by 1/year_fraction", {
  skip_if_no_solver()
  scen <- .gw_scen("sampled")
  d <- .gw_get(scen, "vTechOut", add_weights = TRUE)
  skip_if(nrow(d) == 0, "no vTechOut rows")
  # the sample keeps m of K slices, so every kept slice stands for K/m of them
  expect_setequal(unique(d$weight), .gw_K / .gw_m)
  # and the weight is exactly the LP's own pTimesliceWeight
  w <- as.data.frame(energyRt:::get_data_slot(
    scen@modInp@parameters[["pTimesliceWeight"]]))
  key <- paste(d$year, d$timeslice)
  expect_equal(d$weight,
               w$value[match(key, paste(w$year, w$timeslice))])
})

# @covers getData depth=S backends=glpk forks=sampled
test_that("sum(value * weight) is the annual total on a sampled calendar", {
  skip_if_no_solver()
  scen <- .gw_scen("sampled")
  hi <- .gw_get(scen, "vTechOut", apply_weights = TRUE)
  lo <- .gw_get(scen, "vTechOut", timeframe = "lowest")
  skip_if(nrow(hi) == 0 || nrow(lo) == 0, "no vTechOut rows")
  grp <- setdiff(names(lo), c("timeslice", "value"))
  agg <- hi |>
    dplyr::group_by(dplyr::across(dplyr::all_of(grp))) |>
    dplyr::summarise(value = sum(value), .groups = "drop") |>
    as.data.frame()
  cmp <- merge(agg, lo[, c(grp, "value")], by = grp,
               suffixes = c(".hi", ".lo"))
  expect_equal(nrow(cmp), nrow(lo))
  expect_equal(cmp$value.hi, cmp$value.lo, tolerance = 1e-9)
})

# @covers getData depth=S backends=glpk forks=sampled
test_that("weighting after the roll-up to ANNUAL does not annualise twice", {
  skip_if_no_solver()
  scen <- .gw_scen("sampled")
  lo <- .gw_get(scen, "vTechOut", timeframe = "lowest")
  skip_if(nrow(lo) == 0, "no vTechOut rows")
  lo_w <- .gw_get(scen, "vTechOut", timeframe = "lowest", apply_weights = TRUE)
  # the top slice is full-year magnitude, so its weight is 1
  expect_true(all(lo_w$weight == 1))
  expect_equal(lo_w$value, lo$value)
})

# @covers getData depth=S backends=glpk forks=sampled
test_that("a rate is not weighted, and the skip is named", {
  skip_if_no_solver()
  scen <- .gw_scen("sampled")
  # every INPUT parameter is declared `weighted_mean` on the time axis -- a
  # rate, not a quantity -- so none of them is scaled
  p0 <- .gw_get(scen, "pDemand")
  skip_if(nrow(p0) == 0, "no pDemand rows")
  expect_warning(p1 <- .gw_get(scen, "pDemand", apply_weights = TRUE),
                 "unweighted")
  expect_equal(p1$value, p0$value)
  # the factor still rides along, so the reader sees why it was not applied
  expect_true("weight" %in% names(p1))
  expect_setequal(unique(p1$weight), .gw_K / .gw_m)
})

# @covers getData vStorageLevel depth=S backends=glpk forks=sampled
test_that("a level is a snapshot and is never weighted", {
  skip_if_no_solver()
  # the catalogue is what decides, and it declares the level `last`
  expect_identical(
    unname(energyRt:::.get_variable_recast_rule("vStorageLevel", "time")),
    "last")
  scen <- .gw_scen("sampled")
  s0 <- .gw_get(scen, "vStorageLevel")
  skip_if(nrow(s0) == 0, "storage not built in this solution")
  expect_warning(s1 <- .gw_get(scen, "vStorageLevel", apply_weights = TRUE),
                 "unweighted")
  expect_equal(s1$value, s0$value)
})

# @covers getData pPeriodLen depth=S backends=glpk
test_that("period length multiplies a flow and never a capacity", {
  skip_if_no_solver()
  scen <- .gw_scen("sampled")
  pl <- as.data.frame(energyRt:::get_data_slot(
    scen@modInp@parameters[["pPeriodLen"]]))
  expect_true(nrow(pl) > 0)
  d0 <- .gw_get(scen, "vTechOut")
  skip_if(nrow(d0) == 0, "no vTechOut rows")
  d1 <- .gw_get(scen, "vTechOut", apply_period_length = TRUE)
  f <- pl$value[match(as.integer(d0$year), as.integer(pl$year))]
  expect_equal(d1$period_length, as.numeric(f))
  expect_equal(d1$value, d0$value * as.numeric(f))

  # a capacity is a stock: the years a milestone stands for do not multiply it
  c0 <- .gw_get(scen, "vTechCap")
  skip_if(nrow(c0) == 0, "no vTechCap rows")
  expect_warning(c1 <- .gw_get(scen, "vTechCap", apply_period_length = TRUE),
                 "unscaled")
  expect_equal(c1$value, c0$value)
})

# @covers getData depth=S backends=glpk
test_that("the defaults leave the result shape unchanged", {
  skip_if_no_solver()
  scen <- .gw_scen("sampled")
  d0 <- .gw_get(scen, "vTechOut")
  skip_if(nrow(d0) == 0, "no vTechOut rows")
  expect_false("weight" %in% names(d0))
  expect_false("period_length" %in% names(d0))
  # "auto" attaches the column when the factor was applied
  expect_true("weight" %in% names(.gw_get(scen, "vTechOut",
                                          apply_weights = TRUE)))
  expect_false("weight" %in% names(.gw_get(scen, "vTechOut",
                                           apply_weights = TRUE,
                                           add_weights = FALSE)))
  # and TRUE attaches it without touching the values
  d1 <- .gw_get(scen, "vTechOut", add_weights = TRUE)
  expect_true("weight" %in% names(d1))
  expect_equal(d1$value, d0$value)
  expect_error(.gw_get(scen, "vTechOut", add_weights = "sometimes"),
               "TRUE, FALSE or")
})

# --------------------------------------------------------------------------- #
# The per-year lookup, without a model: a milestone on its own calendar takes
# that calendar's weights, which is the year split the roll-up also makes.
# --------------------------------------------------------------------------- #

# @covers getData depth=XS
test_that("each year takes the weights of its own calendar", {
  tt <- .gw_timetable()
  cal_full <- newCalendar(timetable = tt, name = "gw_f")
  tt_sub <- tt[tt$TIMESLICE %in% .gw_sl[seq_len(.gw_m)], ]
  cal_sub <- suppressWarnings(
    newCalendar(timetable = tt_sub, year_fraction = sum(tt_sub$share),
                name = "gw_s"))
  df <- data.frame(
    year = c(2020L, 2020L, 2030L, 2030L),
    timeslice = c(.gw_sl[1], .gw_sl[2], .gw_sl[1], .gw_sl[2]),
    value = 1)
  w <- energyRt:::.timeslice_weight_vec(df, cal_full,
                                        list("2030" = cal_sub))
  expect_equal(w, c(1, 1, .gw_K / .gw_m, .gw_K / .gw_m))
  # no per-year calendars: the model calendar answers for every row
  expect_equal(energyRt:::.timeslice_weight_vec(df, cal_sub), rep(.gw_K / .gw_m, 4))
  # an unknown slice has no occurrence count, so it stands for itself
  df2 <- data.frame(year = 2020L, timeslice = NA_character_, value = 1)
  expect_equal(energyRt:::.timeslice_weight_vec(df2, cal_full), 1)
})
