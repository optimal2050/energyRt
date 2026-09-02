# `mSupSpan` — the (sup, region) operational span of each supply object.
#
# The span decides which (sup, region) cells the solver may draw from. A cell
# outside the object's real span has no `pSupCost` row, and `pSupCost` defaults
# to 0, so a widened span hands the optimiser free energy: it takes it in
# preference to every real cell, `vSupCost` is zero everywhere (and therefore
# written as no rows at all), and the objective comes back 0 on a feasible,
# optimal-looking solve.
#
# The span has now been wrong in BOTH directions. It once intersected with the
# atoms and DELETED a coarse-level supply (see test-nested-regions.R); it then
# widened a data-scoped supply onto every region. Same signature, opposite
# mechanism — so both directions are pinned here.

ss_cal <- function() {
  newCalendar(timetable = make_timetable(
    struct = list(ANNUAL = "ANNUAL", SEASON = c("WIN", "SUM"))),
    name = "ss2")
}

# regs: the model's regions. sups: list of (name, region, cost); `region = NA`
# scopes nothing. `on_slot` also sets the @region declaration slot.
ss_mod <- function(sups, regs = c("R1", "R2"), on_slot = FALSE,
                   name = "ss", demand = 2) {
  cal <- ss_cal()
  sl <- as.character(cal@timeframes[[length(cal@timeframes)]])
  objs <- list(newCommodity("ELC", timeframe = "SEASON"))
  for (s in sups) {
    args <- list(s$name, commodity = "ELC",
                 supply = data.frame(region = s$region, cost = s$cost,
                                     stringsAsFactors = FALSE))
    if (on_slot && !all(is.na(s$region))) {
      args$region <- unique(s$region[!is.na(s$region)])
    }
    objs <- c(objs, list(do.call(newSupply, args)))
  }
  for (r in regs) {
    objs <- c(objs, list(newDemand(paste0("D_", r), commodity = "ELC",
      demand = data.frame(region = r, year = 2020L, timeslice = sl,
                          demand = demand))))
  }
  newModel(name, region = regs, discount = 0, calendar = cal,
           horizon = newHorizon(2020),
           repo = do.call(newRepository, c(list("ssr"), objs)))
}

ss_span <- function(scen) {
  d <- get_data_slot(scen@modInp@parameters[["mSupSpan"]])
  as.data.frame(d)[, c("sup", "region")]
}

ss_interp <- function(mod, name) {
  suppressMessages(suppressWarnings(
    interpolate_model(mod, name = name, overwrite = TRUE)))
}

# --- the span itself (no solver needed) -------------------------------------- #

test_that("a supply scoped by its DATA spans only those regions", {
  # REGRESSION: the span read the @region slot only, so a supply scoped through
  # the `region` column of @supply -- the column the class prototype provides
  # -- spanned every region in the model.
  mod <- ss_mod(list(list(name = "S1", region = "R1", cost = 15),
                     list(name = "S2", region = "R2", cost = 20)))
  sp <- ss_span(ss_interp(mod, "ss_data"))
  expect_identical(nrow(sp), 2L)
  expect_setequal(paste(sp$sup, sp$region), c("S1 R1", "S2 R2"))
})

test_that("the @region slot still wins when it is set", {
  mod <- ss_mod(list(list(name = "S1", region = "R1", cost = 15),
                     list(name = "S2", region = "R2", cost = 20)),
                on_slot = TRUE)
  sp <- ss_span(ss_interp(mod, "ss_slot"))
  expect_setequal(paste(sp$sup, sp$region), c("S1 R1", "S2 R2"))
})

test_that("an unscoped supply still spans every region", {
  # no region anywhere: the object genuinely applies everywhere
  mod <- ss_mod(list(list(name = "S1", region = NA_character_, cost = 15)))
  sp <- ss_span(ss_interp(mod, "ss_none"))
  expect_setequal(sp$region, c("R1", "R2"))

  # one object per-region row set covering both regions
  mod2 <- ss_mod(list(list(name = "S", region = c("R1", "R2"),
                           cost = c(15, 20))))
  sp2 <- ss_span(ss_interp(mod2, "ss_both"))
  expect_setequal(paste(sp2$sup, sp2$region), c("S R1", "S R2"))
})

test_that("an NA region row means everywhere, even beside named rows", {
  # a mixed table: one row names R1, another leaves region NA. The NA row is a
  # wildcard, so the object still spans the model.
  mod <- ss_mod(list(list(name = "S1", region = c("R1", NA_character_),
                          cost = c(15, 15))))
  sp <- ss_span(ss_interp(mod, "ss_na"))
  expect_setequal(sp$region, c("R1", "R2"))
})

test_that("two supplies on the same region span only that region", {
  mod <- ss_mod(list(list(name = "S1", region = "R1", cost = 15),
                     list(name = "S2", region = "R1", cost = 20)))
  sp <- ss_span(ss_interp(mod, "ss_same"))
  expect_setequal(paste(sp$sup, sp$region), c("S1 R1", "S2 R1"))
})

# --- what the span costs when it is wrong ------------------------------------ #

test_that("region-partitioned supply is charged, not free", {
  skip_if_no_solver()
  old <- set_scenarios_path(
    gsub("[\\/]+", "/", file.path(tempdir(), "supply-span")))
  on.exit(set_scenarios_path(old), add = TRUE)

  # 2 seasons x demand 2 = 4 units per region; R1 at 15, R2 at 20 -> 140
  mod <- ss_mod(list(list(name = "S1", region = "R1", cost = 15),
                     list(name = "S2", region = "R2", cost = 20)))
  s <- solve_scenario(ss_interp(mod, "ss_cost"), echo = FALSE)

  expect_true(isTRUE(s@status$optimal))
  obj <- as.numeric(getData(s, "vObjective", merge = TRUE)$value[1])
  expect_equal(obj, 4 * 15 + 4 * 20)

  # the cost variable must exist at all: a zero-cost solve writes no rows
  cost <- getData(s, "vSupCost", merge = TRUE)
  expect_gt(nrow(cost), 0)

  # and every unit must come from a (sup, region) pair that HAS a cost row
  out <- as.data.frame(getData(s, "vSupOut", merge = TRUE))
  priced <- unique(as.data.frame(
    getData(s, "pSupCost", merge = TRUE))[, c("sup", "region")])
  drawn <- unique(data.frame(sup = as.character(out$sup),
                             region = as.character(out$region)))
  expect_true(all(paste(drawn$sup, drawn$region) %in%
                    paste(priced$sup, priced$region)))
})
