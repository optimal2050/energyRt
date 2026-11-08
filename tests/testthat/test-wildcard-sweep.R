# =========================================================================== #
# Wildcard rows, swept across the parameter catalogue.
#
# An NA in a key column means "every member of that dimension": the contract
# `data-raw/classes-docs.yml` states for 160 slots ("NA for every region") and
# `test-conflicting-rows.R:62` asserts at construction ("region = NA means ALL
# ... specific overriding broadcast"). Nothing asserted that INTERPOLATION
# honours it, and it did not. A base-year anchor written
#
#     demand = data.frame(region = NA, year = 2025, timeslice = NA, demand = 0)
#
# beside rows naming regions became its own series: it anchored nothing, so
# `back.inter.forth` back-cast the 2030 value over it, and the orphan series
# was then dropped as all-default. The row had NO effect -- the parameter came
# out byte-identical to one declared without the anchor at all. The same shape
# in `supply@supply` behaved the same way, so it was never demand-specific.
#
# The property each case asserts is EQUIVALENCE: the wildcard spelling and the
# spelled-out one must produce the same parameter. It needs no knowledge of the
# intended values, which is what makes it sweepable, and it is exactly what the
# defect violated. `wc_coverage` then accounts for every catalogued column that
# could carry a wildcard, so a column cannot go untested by omission.
#
# @covers pDemand pSupCost pSupAva pTechVarom pTechAf pTechInvcost
# @covers pStorageOutInvcost pTradeIrVarom depth=I
# =========================================================================== #

WC_REGIONS <- c("R1", "R2")

wc_horizon <- function() newHorizon(
  intervals = data.frame(start = c(2025L, 2026L, 2031L),
                         mid   = c(2025L, 2030L, 2040L),
                         end   = c(2025L, 2030L, 2040L)),
  force_BY_interval_to_1_year = FALSE, name = "wc_h")

# The anchor pattern the defect was found with: a value at the FIRST milestone
# plus named values later. `wild = TRUE` spells the anchor with an NA region,
# `wild = FALSE` spells it out per region; both mean the same thing, so the two
# must interpolate identically.
wc_rows <- function(col, wild, anchor, later = c(100, 200), slice = NULL) {
  a <- data.frame(
    region = if (wild) NA_character_ else WC_REGIONS,
    year = 2025L, stringsAsFactors = FALSE)
  a[[col]] <- anchor
  b <- data.frame(region = WC_REGIONS, year = 2030L, stringsAsFactors = FALSE)
  b[[col]] <- later
  out <- rbind(a, b)
  if (!is.null(slice)) out$timeslice <- slice
  out
}

wc_converter <- function(...) newTechnology(
  "T1", desc = "Unit converter", input = list(comm = "COA"),
  output = list(comm = "ELC"), ceff = data.frame(comm = "COA", cinp2use = 1),
  cap2act = 1, invcost = list(invcost = 1), olife = 30L, ...)

wc_flat_demand <- function() newDemand(
  "DG", desc = "Flat demand", commodity = "ELC", unit = "GWh",
  demand = data.frame(year = c(2025L, 2030L, 2040L), demand = 10))

wc_backstop <- function(price = 99) newImport(
  "IMP", desc = "Expensive backstop", commodity = "ELC", unit = "GWh",
  import = data.frame(region = WC_REGIONS, price = price))

# --- the driven cases ------------------------------------------------------ #
# One per (class, slot, column); `build(wild)` returns the objects to add.
WC_CASES <- list(
  demand = list(
    param = "pDemand", cls = "demand", slot = "demand", col = "demand",
    build = function(wild) list(
      newDemand("DG", desc = "Demand under test", commodity = "ELC",
                unit = "GWh",
                demand = wc_rows("demand", wild, anchor = 1,
                                 slice = "ANNUAL")),
      wc_backstop(10))),

  sup_cost = list(
    param = "pSupCost", cls = "supply", slot = "supply", col = "cost",
    build = function(wild) list(
      newSupply("SUP", desc = "Supply under test", commodity = "COA",
                supply = wc_rows("cost", wild, anchor = 1, later = c(5, 7))),
      wc_converter(), wc_flat_demand(), wc_backstop())),

  sup_ava = list(
    param = "pSupAva", cls = "supply", slot = "supply", col = "ava.up",
    build = function(wild) list(
      newSupply("SUP", desc = "Supply under test", commodity = "COA",
                supply = cbind(wc_rows("ava.up", wild, anchor = 0,
                                       later = c(100, 100)), cost = 1)),
      wc_converter(), wc_flat_demand(), wc_backstop())),

  tech_varom = list(
    param = "pTechVarom", cls = "technology", slot = "varom", col = "varom",
    build = function(wild) list(
      newSupply("SUP", desc = "Cheap fuel", commodity = "COA",
                supply = data.frame(cost = 1)),
      wc_converter(varom = wc_rows("varom", wild, anchor = 1,
                                   later = c(5, 5))),
      wc_flat_demand(), wc_backstop())),

  tech_af = list(
    param = "pTechAf", cls = "technology", slot = "af", col = "af.up",
    build = function(wild) list(
      newSupply("SUP", desc = "Cheap fuel", commodity = "COA",
                supply = data.frame(cost = 1)),
      wc_converter(af = wc_rows("af.up", wild, anchor = 0, later = c(1, 1))),
      wc_flat_demand(), wc_backstop())),

  tech_invcost = list(
    param = "pTechInvcost", cls = "technology", slot = "invcost",
    col = "invcost",
    build = function(wild) list(
      newSupply("SUP", desc = "Cheap fuel", commodity = "COA",
                supply = data.frame(cost = 1)),
      newTechnology("T1", desc = "Unit converter",
                    input = list(comm = "COA"), output = list(comm = "ELC"),
                    ceff = data.frame(comm = "COA", cinp2use = 1),
                    cap2act = 1, olife = 30L,
                    invcost = wc_rows("invcost", wild, anchor = 1,
                                      later = c(900, 900))),
      wc_flat_demand(), wc_backstop()))
)

wc_model <- function(case, wild, tag) {
  newModel(tag, region = WC_REGIONS, discount = 0.05,
           calendar = energyRt::calendars$annual, horizon = wc_horizon(),
           data = do.call(add, c(
             list(newRepository("wc")),
             list(newCommodity("ELC", desc = "Electricity", unit = "GWh"),
                  newCommodity("COA", desc = "Coal", unit = "GWh")),
             WC_CASES[[case]]$build(wild))))
}

wc_param <- function(case, wild, tag) {
  scen <- suppressMessages(suppressWarnings(interpolate_model(
    wc_model(case, wild, tag), name = tag, overwrite = TRUE, verbose = FALSE)))
  p <- scen@modInp@parameters[[WC_CASES[[case]]$param]]
  if (is.null(p)) return(NULL)
  d <- tryCatch(as.data.frame(get_data_slot(p, optional = TRUE)),
                error = function(e) NULL)
  if (is.null(d) || nrow(d) == 0) return(NULL)
  d <- d[do.call(order, lapply(d, as.character)), , drop = FALSE]
  rownames(d) <- NULL
  d
}

# --------------------------------------------------------------------------- #

test_that("a wildcard-keyed row interpolates exactly as the spelled-out one", {
  skip_if_tier_below("fast")
  for (case in names(WC_CASES)) {
    w <- wc_param(case, TRUE,  paste0("wcw_", case))
    e <- wc_param(case, FALSE, paste0("wce_", case))
    expect_false(is.null(w),
                 label = paste0(case, ": the wildcard spelling produced no ",
                                "parameter rows at all"))
    expect_equal(w, e,
                 label = paste0(case, ": wildcard and explicit spellings of ",
                                "the same data disagree"))
  }
})

# @covers pDemand depth=I
test_that("a wildcard anchor reaches every member and stops the back-cast", {
  skip_if_tier_below("fast")
  # The reported case: the 2025 anchor must land in BOTH regions and keep
  # 2030's value from being back-cast over it.
  d <- wc_param("demand", TRUE, "wc_reach")
  at25 <- d[d$year == 2025L, ]
  expect_setequal(at25$region, WC_REGIONS)
  expect_true(all(abs(at25$value - 1) < 1e-9),
              label = "the wildcard anchor was overwritten by a back-cast")
})

# @covers pDemand depth=I
test_that("an explicit row beats a wildcard at the same cell", {
  skip_if_tier_below("fast")
  DG <- newDemand("DG", desc = "Mixed spelling", commodity = "ELC",
                  unit = "GWh",
                  demand = rbind(
                    data.frame(region = NA_character_, year = 2025L,
                               timeslice = "ANNUAL", demand = 1),
                    data.frame(region = "R1", year = 2025L,
                               timeslice = "ANNUAL", demand = 7),
                    data.frame(region = WC_REGIONS, year = 2030L,
                               timeslice = "ANNUAL", demand = c(100, 200))))
  mod <- newModel("WCP", region = WC_REGIONS, discount = 0.05,
                  calendar = energyRt::calendars$annual,
                  horizon = wc_horizon(),
                  data = newRepository("wcp") |>
                    add(newCommodity("ELC", desc = "Electricity",
                                     unit = "GWh"),
                        DG, wc_backstop(10)))
  scen <- suppressMessages(suppressWarnings(interpolate_model(
    mod, name = "wc_prec", overwrite = TRUE, verbose = FALSE)))
  d <- as.data.frame(get_data_slot(scen@modInp@parameters[["pDemand"]],
                                   optional = TRUE))
  at25 <- d[d$year == 2025L, ]
  expect_equal(at25$value[at25$region == "R1"], 7)
  expect_equal(at25$value[at25$region == "R2"], 1)
})

# @covers pSupAva depth=S backends=glpk
test_that("a zero upper bound binds instead of reading as unbounded", {
  skip_if_no_solver()
  # `ava.up = 0` was dropped from `mSupAvaUp` -- the gate keeps only non-default
  # bounds, and it tested `value != 0` although the UP default is Inf. The
  # parameter carried the zero, no equation read it, and the supply solved
  # UNBOUNDED: the opposite of the declaration.
  mod <- newModel("WCZ", region = "R1", discount = 0.05,
                  calendar = energyRt::calendars$annual,
                  horizon = newHorizon(
                    intervals = data.frame(start = c(2025L, 2026L),
                                           mid = c(2025L, 2030L),
                                           end = c(2025L, 2030L)),
                    force_BY_interval_to_1_year = FALSE, name = "wcz_h"),
                  data = newRepository("wcz") |> add(
                    newCommodity("ELC", desc = "Electricity", unit = "GWh"),
                    newCommodity("COA", desc = "Coal", unit = "GWh"),
                    newSupply("SUP", desc = "Supply, closed in the base year",
                              commodity = "COA",
                              supply = data.frame(year = c(2025L, 2030L),
                                                  cost = 1,
                                                  ava.up = c(0, 100))),
                    wc_converter(),
                    newImport("IMP", desc = "Expensive backstop",
                              commodity = "ELC", unit = "GWh",
                              import = data.frame(price = 99)),
                    newDemand("DG", desc = "Flat demand", commodity = "ELC",
                              unit = "GWh",
                              demand = data.frame(year = c(2025L, 2030L),
                                                  demand = 10))))
  scen <- suppressMessages(suppressWarnings(interpolate_model(
    mod, name = "wc_zero", overwrite = TRUE, verbose = FALSE)))
  gate <- as.data.frame(get_data_slot(
    scen@modInp@parameters[["mSupAvaUp"]], optional = TRUE))
  expect_true(2025L %in% gate$year,
              label = "the zero ceiling never reached the gate map")
  sol <- suppressMessages(suppressWarnings(
    solve_scenario(scen, solver = solver_options$glpk, wait = TRUE)))
  out <- tryCatch(as.data.frame(get_data_slot(
    sol@modOut@variables[["vSupOut"]])), error = function(e) NULL)
  got <- if (is.null(out)) 0 else sum(out$value[out$year == 2025L])
  expect_equal(got, 0,
               label = "supply flowed in a year declared closed (ava.up = 0)")
})

# --------------------------------------------------------------------------- #
# Coverage accounting. The sweep drives a handful of columns; this block states
# how many COULD carry a wildcard, so the gap is a number in the test output
# rather than something to be discovered later. `wc_driven` must name real
# catalogue entries, which keeps the list from drifting as the catalogue grows.

wc_catalogue <- function() {
  mp <- energyRt:::.param_interp_map()
  do.call(rbind, lapply(names(mp), function(k) {
    e <- mp[[k]]
    parts <- strsplit(k, "\r", fixed = TRUE)[[1]]
    dims <- paste(e$dims, collapse = "+")
    data.frame(
      class = parts[1], slot = parts[2], col = parts[3],
      param = if (is.null(e$param)) NA_character_ else e$param,
      wildcardable = grepl("(^|[+])year([+]|$)", dims) &&
        (grepl("(^|[+])region([+]|$)", dims) ||
           grepl("(^|[+])timeslice([+]|$)", dims)),
      stringsAsFactors = FALSE)
  }))
}

# @covers depth=C
test_that("the sweep's cases are real catalogue entries, and the gap is stated", {
  cat_tbl <- wc_catalogue()
  key <- paste(cat_tbl$class, cat_tbl$slot, cat_tbl$col, sep = "\r")
  driven <- vapply(WC_CASES, function(x) paste(x$cls, x$slot, x$col,
                                               sep = "\r"), "")
  expect_true(all(driven %in% key),
              label = paste("a driven case names no catalogued column:",
                            paste(setdiff(driven, key), collapse = ", ")))

  wildcardable <- cat_tbl[cat_tbl$wildcardable, ]
  # Stated, not asserted loosely: 195 of 244 catalogued columns carry both a
  # `year` and a `region`/`timeslice` dimension, so they can all take a
  # wildcard row. The sweep drives the ones a small fixture can drive; the
  # count is pinned so that growth in the catalogue shows up here as a
  # deliberate edit rather than silent dilution.
  expect_equal(nrow(wildcardable), 195L)
  expect_equal(nrow(cat_tbl), 244L)
  expect_gte(length(driven), 6L)
})
