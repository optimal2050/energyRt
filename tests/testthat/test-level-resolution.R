# =========================================================================== #
# Feature family: level-resolution -- how `summand@timeframe` / `@geoframe`
# resolve a named time or geoscale level to the indices of a variable.
#
# The decision is made PER GROUP (the distinct combinations of the variable's
# other dimensions), because the pooled domain column cannot tell two different
# situations apart:
#
#   real aggregation  one commodity with rows at several levels, the coarse row
#                     being the roll-up of the fine ones (`mvOutTot`). PIN; a
#                     cross-level sum would double count.
#   heterogeneity     different objects at different levels sharing one column
#                     (`mvTechOut` with an annual and a seasonal commodity).
#                     Nothing aggregates anything, so pinning drops the finer
#                     object -- silently, before this family existed.
#
# Only groups with a row AT the level OR FINER take part. "Finer" is deeper in
# the hierarchy: with ANNUAL < SEASON < HOUR, the set for SEASON is the seasons
# and their hours, not the annual slice. A group sitting entirely COARSER than
# the level has nothing at that resolution and correctly contributes nothing --
# which is what keeps `timeframe = "SEASON"` legal on `tm_core`, whose coal is
# annual-only.
#
#   every participating group has a row AT the level -> PIN
#   no group holds rows at TWO levels under it       -> SUM; each group
#                                                       contributes its own
#                                                       rows exactly once
#   some group holds rows at two levels under it     -> ERROR naming it
#
# The middle branch is why heterogeneity alone is not an error: groups that
# disagree about which level they occupy still each contribute once, so the sum
# is right. Only a group spanning two levels can be counted twice, and that is
# the one case a flat `for.sum` cannot express -- pinning drops the finer-only
# groups, summing double-counts the spanning one.
#
# A STATE is refused outright where a flow would be summed: the sum of hourly
# storage levels is not an annual level (`role: stock`).
# =========================================================================== #

lr_cns <- function(variable, tf = NULL, gf = NULL, name = "LR") {
  term <- list(variable = variable)
  if (!is.null(tf)) term$timeframe <- tf
  if (!is.null(gf)) term$geoframe <- gf
  newConstraint(name, eq = "<=", for.each = data.frame(year = 2020),
                t1 = term, defVal = 1e6)
}

lr_interp <- function(mod, tag) {
  suppressMessages(suppressWarnings(
    interpolate_model(mod, name = tag, ondisk = FALSE, overwrite = TRUE)))
}

# timeslices the compiled constraint resolved to
lr_timeslices <- function(scen) {
  out <- character()
  for (n in grep("^mCns", names(scen@modInp@parameters), value = TRUE)) {
    d <- tryCatch(get_data_slot(scen@modInp@parameters[[n]]),
                  error = function(e) NULL)
    if (!is.null(d) && "timeslice" %in% names(d)) {
      out <- c(out, as.character(d$timeslice))
    }
  }
  sort(unique(out))
}

# --------------------------------------------------------------------------- #
# PIN: vOutTot genuinely aggregates (eqOutTot is one of only two equations with
# a family roll-up term), so a level request takes that level's own rows.
# --------------------------------------------------------------------------- #

# @covers vOutTot depth=I backends=glpk
test_that("family level-resolution: an aggregating variable pins to the level", {
  skip_if_no_fixtures()
  env <- .mapping_fixture_env()
  # every commodity has an ANNUAL row, so every participating group has one
  s <- lr_interp(add(env$tm_core(), lr_cns("vOutTot", tf = "ANNUAL")), "lr_pin_a")
  expect_identical(lr_timeslices(s), "ANNUAL")
})

# @covers vOutTot depth=I backends=glpk
test_that("family level-resolution: a group above the level is not required to have a row", {
  skip_if_no_fixtures()
  env <- .mapping_fixture_env()
  # tm_core's COA is ANNUAL-only -- COARSER than SEASON -- so it has nothing at
  # SEASON or finer and must not make the request ambiguous. ELC does, and pins.
  s <- lr_interp(add(env$tm_core(), lr_cns("vOutTot", tf = "SEASON")), "lr_pin_s")
  got <- lr_timeslices(s)
  expect_false("ANNUAL" %in% got)
  expect_setequal(got, env$tm_core()@config@calendar@timeframes$SEASON)
})

# --------------------------------------------------------------------------- #
# SUM: a per-object variable has no coarse row, so the level resolves to
# everything beneath it.
# --------------------------------------------------------------------------- #

# The SUM branch's control lives in `test-level-checks.R:180-230`, which owns the
# geoscale fixture helpers (`lc_geo_model()`, `lc_con_regions()`): `geoframe` on
# `vTechOut` must keep resolving to the zone's own regions, because no group has
# a zone row. It is a control for this change, not a new case, so it is not
# duplicated here.

# --------------------------------------------------------------------------- #
# HETEROGENEITY: the case that used to under-count in silence, and then briefly
# errored. It sums.
# --------------------------------------------------------------------------- #

# One ANNUAL commodity and one sub-annual one, a technology on each. No single
# (tech, comm) pair spans levels, but the POOLED timeslice column does -- which
# is what the original test read, so it pinned to ANNUAL and dropped the
# seasonal technology's output from the sum without saying so.
lr_mixed <- function(...) {
  newModel("lrm",
    repo = newRepository("lrm_repo",
      newCommodity("HEAT", timeframe = "ANNUAL"),
      newCommodity("ELC", timeframe = "SL"),
      newCommodity("COA", timeframe = "ANNUAL"),
      newSupply("SUP", commodity = "COA", supply = data.frame(cost = 1)),
      newTechnology("THEAT", input = list(comm = "COA"),
                    output = list(comm = "HEAT"),
                    invcost = list(invcost = 10),
                    vintage = data.frame(olife = 30L), cap2act = 1),
      newTechnology("TELC", input = list(comm = "COA"),
                    output = list(comm = "ELC"),
                    invcost = list(invcost = 10),
                    vintage = data.frame(olife = 30L), cap2act = 1),
      newDemand("DH", commodity = "HEAT", demand = data.frame(demand = 10)),
      newDemand("DE", commodity = "ELC",
                demand = data.frame(timeslice = "s3", demand = 10)), ...),
    calendar = newCalendar(timetable = make_timetable(
      struct = list(ANNUAL = "ANNUAL", SL = paste0("s", 1:4))),
      name = "lrm_cal"),
    region = "R1", horizon = newHorizon(2025), discount = 0)
}

# @covers vTechOut depth=I backends=glpk
test_that("family level-resolution: heterogeneous levels sum, they do not error", {
  # No (tech, comm) pair spans two levels here -- THEAT is annual-only and TELC
  # seasonal-only -- so each contributes its own rows once and the sum is the
  # true total. The request must therefore resolve, not be refused: refusing it
  # was the over-strict first cut of this family.
  cns <- newConstraint("LRM", eq = "<=", for.each = data.frame(year = 2025),
                       out = list(variable = "vTechOut", timeframe = "ANNUAL"),
                       rhs = 1e6, defVal = 0)
  expect_s4_class(lr_interp(lr_mixed(cns), "lr_mixed"), "scenario")
})

# @covers vTechOut depth=I backends=glpk
test_that("family level-resolution: the automatic sum equals the explicit for.sum", {
  # the escape hatch the error message used to offer is the reference answer:
  # resolving `timeframe` automatically must produce the same compiled
  # constraint as naming every timeslice by hand
  mk <- function(term) newConstraint("LRM", eq = "<=",
                                     for.each = data.frame(year = 2025),
                                     out = term, rhs = 1e6, defVal = 0)
  auto <- mk(list(variable = "vTechOut", timeframe = "ANNUAL"))
  manual <- mk(list(variable = "vTechOut",
                    for.sum = list(timeslice = c("ANNUAL", paste0("s", 1:4)))))
  cns_params <- function(scen) {
    ps <- sort(grep("Cns", names(scen@modInp@parameters), value = TRUE))
    lapply(setNames(ps, ps), function(n)
      tryCatch(as.data.frame(get_data_slot(scen@modInp@parameters[[n]])),
               error = function(e) NULL))
  }
  expect_identical(cns_params(lr_interp(lr_mixed(auto), "lr_eq_auto")),
                   cns_params(lr_interp(lr_mixed(manual), "lr_eq_man")))
})

# --------------------------------------------------------------------------- #
# The resolver itself. The three branches are decided per group, which no model
# fixture can exercise exhaustively -- a commodity's parent cells are built up
# to the top of the hierarchy (`.extend_comm_timeslice`), so a model variable
# either has every ancestor row or none, and the SPANNING case only becomes
# reachable once `parent_levels` builds them for a subset. So the branches are
# pinned directly on the function.
# --------------------------------------------------------------------------- #

test_that("family level-resolution: the resolver decides per group", {
  mem <- list(ANNUAL = "ANNUAL", SEASON = c("WIN", "SUM"),
              HOUR = c("h1", "h2", "h3", "h4"))
  desc <- c(mem$SEASON, mem$HOUR)          # SEASON's own members and below
  dom <- c("ANNUAL", desc)
  res <- function(rows) energyRt:::.resolve_level_indices(
    dom, "SEASON", mem, desc, rows = rows, dim = "timeslice")

  # every participating group has a SEASON row -> pin
  r <- res(data.frame(g = c("A", "A", "B"), timeslice = c("WIN", "h1", "SUM")))
  expect_identical(r$mode, "pin")
  expect_setequal(r$values, mem$SEASON)

  # no group does -> sum everything at or under SEASON
  r <- res(data.frame(g = c("A", "B"), timeslice = c("h1", "h2")))
  expect_identical(r$mode, "sum")
  expect_setequal(r$values, desc)

  # groups at DIFFERENT single levels still sum: each contributes once
  r <- res(data.frame(g = c("A", "B"), timeslice = c("WIN", "h2")))
  expect_identical(r$mode, "sum")
  expect_setequal(r$values, desc)

  # a group COARSER than the level has nothing at that resolution and must not
  # make the request ambiguous (this is what keeps SEASON legal on tm_core)
  r <- res(data.frame(g = c("A", "B"), timeslice = c("ANNUAL", "WIN")))
  expect_identical(r$mode, "pin")
})

test_that("family level-resolution: a group spanning two levels is refused", {
  mem <- list(ANNUAL = "ANNUAL", SEASON = c("WIN", "SUM"),
              HOUR = c("h1", "h2", "h3", "h4"))
  desc <- c(mem$SEASON, mem$HOUR)
  # A holds WIN *and* h1 -- summing counts its winter twice; B holds only h2 --
  # pinning drops it. No flat `for.sum` can do both, so this is the error.
  r <- energyRt:::.resolve_level_indices(
    c("ANNUAL", desc), "SEASON", mem, desc,
    rows = data.frame(g = c("A", "A", "B"),
                      timeslice = c("WIN", "h1", "h2")),
    dim = "timeslice")
  expect_identical(r$mode, "mixed")
  expect_identical(r$missing, "A")          # the SPANNING group, not B

  # and the message names it, says why, and offers the two ways out
  err <- tryCatch(energyRt:::.stop_level_mixed(r, "timeframe", "SEASON",
                                              "vTechOut", stop),
                  error = function(e) conditionMessage(e))
  expect_match(err, "is ambiguous for variable")
  expect_match(err, '"A"')
  expect_match(err, "double-count")
  expect_match(err, "for.sum")
  expect_match(err, "parent_levels")
})

# @covers vTechOut depth=I backends=glpk
test_that("family level-resolution: naming the timeslices is the documented way out", {
  # the escape the error message offers must actually work
  cns <- newConstraint("LRM2", eq = "<=", for.each = data.frame(year = 2025),
                       out = list(variable = "vTechOut",
                                  for.sum = list(timeslice = c("ANNUAL", paste0("s", 1:4)))),
                       rhs = 1e6, defVal = 0)
  expect_s4_class(lr_interp(lr_mixed(cns), "lr_escape"), "scenario")
})

# --------------------------------------------------------------------------- #
# STATE: refused where a flow would be summed.
# --------------------------------------------------------------------------- #

test_that("family level-resolution: a state variable refuses to be aggregated", {
  skip_if_no_fixtures()
  env <- .mapping_fixture_env()
  expect_error(
    lr_interp(add(env$tm_flows(), lr_cns("vStorageLevel", tf = "ANNUAL")), "lr_state"),
    "declared `role: stock`")
})

test_that("family level-resolution: states come from `role: stock`, not a list", {
  # The classification is DECLARED in data-raw/variables.yml and read by
  # `.is_state_var()`. Naming a variable literally here would re-create the
  # hard-coded list that `role: stock` exists to replace, so assert the
  # DERIVATION: every declared stock is recognised and nothing else is.
  spec <- energyRt:::.variables
  stocks <- names(spec)[vapply(spec, function(x)
    identical(x$role, "stock"), logical(1))]
  expect_gt(length(stocks), 0L)
  expect_true(all(stocks %in% names(energyRt:::.variable_mapping)))
  expect_true(all(energyRt:::.is_state_var(stocks)),
              label = "every `role: stock` variable is a state")
  flows <- setdiff(names(energyRt:::.variable_mapping), stocks)
  expect_false(any(energyRt:::.is_state_var(flows)),
               label = "no non-stock variable is a state")
})

test_that("family level-resolution: a stock still sums over SPACE", {
  # `role: stock` means "never summed over timeslices" -- it says nothing about
  # regions, and the total stored across a zone is meaningful. This is the
  # complement that pins the time-axis-only decision: if the refusal ever widens
  # to every dimension, this fails.
  sum_res <- list(mode = "sum", values = character(), missing = character())
  expect_null(energyRt:::.stop_level_state(
    sum_res, "geoframe", "zone", "vStorageLevel", "region",
    function(x) stop("geoframe must not be refused for a stock: ", x)))
  # ... while the time axis is
  expect_error(energyRt:::.stop_level_state(
    sum_res, "timeframe", "ANNUAL", "vStorageLevel", "timeslice",
    function(x) stop(x)), "role: stock")
})
