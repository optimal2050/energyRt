# =========================================================================== #
# Recast rules for the 95 solution variables: how a RESULT combines when it is
# rolled up, and what the roll-up records rather than discards.
#
# The rules are declared (`recast` in data-raw/variables.yml, defaulting from
# `role`) and resolved into `.variables`. `getData(timeframe =, geoframe =)`
# reads them, so these tests cover both the declaration and the two roll-ups
# that consume it.
#
# The invariant that matters most is conservation: for a quantity, rolling up
# must not change `Sum(value * weight)`, because a per-slice solved value is
# already scaled by `share` and `Sum(share * weight) == 1`.
# =========================================================================== #

vr_rule <- function(nm, axis) unname(energyRt:::.get_variable_recast_rule(nm, axis))
vr_spec <- function() energyRt:::.variables


# --------------------------------------------------------------------------- #
# 1. the declaration
# --------------------------------------------------------------------------- #

test_that("every variable carries a rule on every axis it is indexed on", {
  v <- vr_spec()
  for (nm in names(v)) {
    dims <- v[[nm]]$dimSets
    sp <- vr_rule(nm, "space")
    tm <- vr_rule(nm, "time")
    # indexed on the axis -> a rule; not indexed -> NA. `vObjective` has
    # neither region nor timeslice.
    if ("region" %in% dims || grepl("^vTrade", nm)) {
      # corridor-keyed trade variables have no region column but DO aggregate
      expect_false(is.na(sp), label = paste0(nm, ": space rule"))
    }
    if ("timeslice" %in% dims) {
      expect_false(is.na(tm), label = paste0(nm, ": time rule"))
    } else {
      expect_true(is.na(tm), label = paste0(nm, ": no time rule"))
    }
  }
})

test_that("the rules are drawn from the declared vocabulary", {
  v <- vr_spec()
  sp <- vapply(names(v), vr_rule, "", axis = "space")
  tm <- vapply(names(v), vr_rule, "", axis = "time")
  expect_true(all(is.na(sp) | sp %in% energyRt:::.VARIABLE_RECAST_RULES$space))
  expect_true(all(is.na(tm) | tm %in% energyRt:::.VARIABLE_RECAST_RULES$time))
})

test_that("sum_external is declared for exactly the eight that need it", {
  v <- vr_spec()
  sp <- vapply(names(v), vr_rule, "", axis = "space")
  # vTradeIr carries two region columns; the other seven are keyed by corridor
  # and reach their endpoints through mTradeRoutes.
  expect_setequal(
    names(v)[!is.na(sp) & sp == "sum_external"],
    c("vTradeIr", "vTradeCap", "vTradeNewCap", "vTradeStockCap",
      "vTradePhaseOut", "vTradeStockPhaseOut", "vTradeRetiredStock",
      "vTradeRetiredNewCap"))
})

test_that("last is declared for the one level that has a timeslice", {
  v <- vr_spec()
  tm <- vapply(names(v), vr_rule, "", axis = "time")
  expect_setequal(names(v)[!is.na(tm) & tm == "last"], "vStorageLevel")
  # the other three stocks carry no timeslice, so the role default cannot apply
  for (nm in c("vExportRowCum", "vImportRowCum", "vSupReserve")) {
    expect_equal(v[[nm]]$role, "stock")
    expect_true(is.na(vr_rule(nm, "time")), label = nm)
  }
})

test_that("the role table and the generator agree", {
  # `data-raw/DATASET.R` carries a copy of `.VARIABLE_RECAST_BY_ROLE`, because
  # it runs before the package is loadable. This is what catches a drift: every
  # variable NOT overridden in variables.yml must resolve to its role's entry.
  v <- vr_spec()
  tbl <- energyRt:::.VARIABLE_RECAST_BY_ROLE
  differs <- character()
  for (nm in names(v)) {
    role <- v[[nm]]$role
    want <- tbl[[role]]
    got_sp <- vr_rule(nm, "space")
    if (!identical(unname(want["space"]), got_sp) && !is.na(got_sp)) {
      differs <- c(differs, nm)
    }
  }
  # the only declared overrides are the seven corridor-keyed capacities
  expect_setequal(differs,
    c("vTradeCap", "vTradeNewCap", "vTradeStockCap", "vTradePhaseOut",
      "vTradeStockPhaseOut", "vTradeRetiredStock", "vTradeRetiredNewCap"))
})

test_that("the accessor refuses the down direction", {
  # `.recast_rules()`'s contract is bidirectional for INPUTS; a solution cannot
  # be split into a finer calendar or geometry.
  expect_error(energyRt:::.get_variable_recast_rule("vTechOut", "down"),
               "aggregation-only")
  expect_error(energyRt:::.get_variable_recast_rule("vTechOut", "split"),
               "aggregation-only")
  expect_true(is.na(vr_rule("not_a_variable", "space")))
})


# --------------------------------------------------------------------------- #
# 2. the roll-ups
# --------------------------------------------------------------------------- #

test_that("a quantity rolls up conserving the annual total", {
  cal <- calendars$s4_h24
  rank <- cal@timeframe_rank
  season <- unname(rank[names(rank) == "SEASON"])
  skip_if(is.na(season) || !length(season))
  sl <- as.data.frame(cal@timetable)$timeslice
  w <- stats::setNames(as.numeric(cal@timeslice_share$weight),
                       as.character(cal@timeslice_share$timeslice))
  q <- data.frame(name = "vTechOut", tech = "E", comm = "ELC", region = "R1",
                  year = 2030L, timeslice = sl, value = 1,
                  stringsAsFactors = FALSE)
  got <- energyRt:::.aggregate_timeframe_df(q, cal, season)
  expect_lt(nrow(got), nrow(q))
  expect_equal(sum(got$value * w[as.character(got$timeslice)]),
               sum(q$value * w[as.character(q$timeslice)]), tolerance = 1e-9)
})

test_that("a level takes its closing value and records the swing", {
  cal <- calendars$s4_h24
  rank <- cal@timeframe_rank
  season <- unname(rank[names(rank) == "SEASON"])
  skip_if(is.na(season) || !length(season))
  sl <- as.data.frame(cal@timetable)$timeslice
  lv <- data.frame(name = "vStorageLevel", stg = "BATT", comm = "ELC",
                   region = "R1", year = 2030L, timeslice = sl,
                   value = seq_along(sl), stringsAsFactors = FALSE)
  got <- energyRt:::.aggregate_timeframe_df(lv, cal, season)
  expect_lt(nrow(got), nrow(lv))
  # a level is never summed: every reported value is one of the fine values
  expect_true(all(got$value %in% lv$value))
  expect_false(any(got$value > max(lv$value)))
  ab <- attr(got, "absorbed")
  expect_equal(ab$rule, "last")
  sw <- ab$in_slice_swing
  expect_true(all(c("level_min", "level_max", "slices") %in% names(sw)))
  expect_equal(nrow(sw), nrow(got))
  expect_true(all(sw$level_min <= sw$level_max))
  # the closing value is the largest here, since the fixture rises monotonically
  expect_equal(sort(got$value), sort(sw$level_max))
})

test_that("an exchange is summed only where it crosses, and the rest recorded", {
  map <- c(R1 = "Z1", R2 = "Z1", R3 = "Z2")
  tr <- data.frame(name = "vTradeIr", trade = "T", comm = "ELC",
                   src = c("R1", "R1", "R2"), dst = c("R2", "R3", "R3"),
                   year = 2030L, timeslice = "WIN_h00",
                   value = c(5, 7, 11), stringsAsFactors = FALSE)
  got <- energyRt:::.rollup_sum_external_df(tr, map)
  expect_equal(nrow(got), 1L)
  expect_equal(got$src, "Z1")
  expect_equal(got$dst, "Z2")
  expect_equal(got$value, 18)          # 7 + 11, the two that cross
  ab <- attr(got, "absorbed")
  expect_equal(ab$rule, "sum_external")
  ir <- ab$in_region_flow
  expect_equal(nrow(ir), 1L)
  expect_equal(ir$value, 5)            # R1 -> R2, now inside Z1
  # nothing vanishes
  expect_equal(sum(got$value) + sum(ir$value), sum(tr$value))
})

test_that("a corridor survives if any of its routes still crosses", {
  map <- c(R1 = "Z1", R2 = "Z1", R3 = "Z2")
  routes <- data.frame(
    trade = c("T_in", "T_out", "T_mix", "T_mix"),
    src   = c("R1",   "R1",    "R1",    "R2"),
    dst   = c("R2",   "R3",    "R2",    "R3"), stringsAsFactors = FALSE)
  cap <- data.frame(name = "vTradeCap", trade = c("T_in", "T_out", "T_mix"),
                    year = 2030L, value = c(100, 200, 300),
                    stringsAsFactors = FALSE)
  got <- energyRt:::.rollup_sum_external_trade(cap, map, routes)
  # T_mix is internal on one route and crossing on the other: it keeps its
  # whole capacity, since the capacity is one number for the corridor
  expect_setequal(got$trade, c("T_out", "T_mix"))
  expect_equal(got$value[got$trade == "T_mix"], 300)
  ab <- attr(got, "absorbed")
  expect_equal(ab$in_region_capacity$trade, "T_in")
  expect_equal(sum(got$value) + sum(ab$in_region_capacity$value),
               sum(cap$value))
})

test_that("a corridor absent from the route table is left alone", {
  # not "assumed internal": a corridor the routes do not mention is unknown,
  # and dropping it would lose capacity silently
  map <- c(R1 = "Z1", R2 = "Z1", R3 = "Z2")
  routes <- data.frame(trade = "T_in", src = "R1", dst = "R2",
                       stringsAsFactors = FALSE)
  cap <- data.frame(name = "vTradeCap", trade = c("T_in", "T_other"),
                    year = 2030L, value = c(100, 50),
                    stringsAsFactors = FALSE)
  got <- energyRt:::.rollup_sum_external_trade(cap, map, routes)
  expect_true("T_other" %in% got$trade)
  expect_false("T_in" %in% got$trade)
})

test_that("a frame holding several variables is left alone", {
  # the rule is per variable; a mixed frame has no single answer
  cal <- calendars$s4_h24
  rank <- cal@timeframe_rank
  season <- unname(rank[names(rank) == "SEASON"])
  skip_if(is.na(season) || !length(season))
  sl <- as.data.frame(cal@timetable)$timeslice
  mixed <- rbind(
    data.frame(name = "vTechOut", region = "R1", year = 2030L,
               timeslice = sl, value = 1, stringsAsFactors = FALSE),
    data.frame(name = "vStorageLevel", region = "R1", year = 2030L,
               timeslice = sl, value = 2, stringsAsFactors = FALSE))
  got <- energyRt:::.aggregate_timeframe_df(mixed, cal, season)
  expect_equal(nrow(got), nrow(mixed))
})
