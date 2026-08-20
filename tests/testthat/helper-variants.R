# =========================================================================== #
# Helpers for the technology-variant tests (vintage / cluster / variant_prefix /
# group-aggregate bounds).
#
# Structural assertions (slots, expansion, validation messages) need nothing but
# the package. Behavioural assertions need a solver, so they are gated with
# `skip_if_no_solver()`, mirroring `skip_if_no_fixtures()` in helper-mapping.R.
# =========================================================================== #

skip_if_no_solver <- function() {
  testthat::skip_on_cran()
  # energyRt finds glpsol either on PATH or via ~/.energyRt/config.yml, so
  # checking PATH alone skips on machines where the solver IS configured.
  # Sturdier form promoted from test-subset_slices.R:34-36.
  cfg <- tryCatch(get_glpk_path(), error = function(e) NULL)
  if (!nzchar(Sys.which("glpsol")) && (is.null(cfg) || !nzchar(cfg))) {
    testthat::skip("glpsol not available (PATH or ~/.energyRt/config.yml)")
  }
}

# A minimal two-region electricity model used across the variant tests.
# `...` receives the technology object(s) under test.
vt_model <- function(..., name = "vt", regions = c("R1", "R2"),
                     years = 2020:2030, interval = 10) {
  cal <- newCalendar(
    timetable = make_timetable(
      struct = list(ANNUAL = "ANNUAL", SEASON = c("WIN", "SPR", "SUM", "AUT"))
    ),
    name = paste0("cal_", name)
  )
  newModel(
    name = name, desc = "", calendar = cal, region = regions,
    horizon = newHorizon(years, intervals = interval), discount = 0.05,
    repo = newRepository(
      paste0("repo_", name),
      newCommodity("COA", timeframe = "ANNUAL"),
      newCommodity("ELC", timeframe = "SEASON"),
      newSupply("SUP_COA", commodity = "COA",
                supply = data.frame(region = regions, cost = 1)),
      newDemand("DEM_ELC", commodity = "ELC",
                demand = data.frame(region = rep(regions, each = 2),
                                 timeslice = c("WIN", "SUM"), demand = 30)),
      # A conventional plant is always present: a model whose only producer is
      # an output-only technology fails the commodity-closure check (pre-existing
      # behaviour, unrelated to variants). Expensive, so variants are preferred.
      newTechnology("ECOA", input = list(comm = "COA"),
                    output = list(comm = "ELC"),
                    invcost = data.frame(invcost = 9000),
                    vintage = data.frame(olife = 30L), cap2act = 1),
      ...
    )
  )
}

# Storage and trade variants of `vt_model`. Both were file-scoped in
# test-storage-variants.R / test-trade-variants.R; testthat sources helper-*.R
# before test files but does NOT cross-load between test files, so any new test
# needing them had to redefine them. Moved here verbatim.
#
# Both add an EWIN backstop so the model has a producer besides the object under
# test -- the commodity-closure check rejects a model whose only producer is
# output-only, and an expensive backstop makes objective comparisons meaningful.
vt_stg_model <- function(stg, name = "st") {
  vt_model(
    newTechnology("EWIN", output = list(comm = "ELC"),
                  invcost = data.frame(invcost = 150),
                  afs = data.frame(timeslice = "ANNUAL", afs.up = 0.4),
                  vintage = data.frame(olife = 25L), cap2act = 1),
    stg, name = name)
}

vt_trade_model <- function(trd, name = "tr") {
  vt_model(
    newTechnology("EWIN", output = list(comm = "ELC"),
                  invcost = data.frame(invcost = 150),
                  afs = data.frame(timeslice = "ANNUAL", afs.up = 0.4),
                  vintage = data.frame(olife = 25L), cap2act = 1),
    trd, name = name)
}

vt_interp <- function(mod, name = "vt") {
  suppressMessages(suppressWarnings(
    interpolate_model(mod, name = name, fold = TRUE)
  ))
}

vt_solve <- function(scen) {
  suppressMessages(suppressWarnings(
    solve_scen(scen, solver = solver_options$glpk, wait = TRUE)
  ))
}

# Peak total capacity of the variants of `base` in one region.
vt_cap <- function(sol, base, region) {
  d <- suppressMessages(getData(sol, "vTechCap", merge = TRUE))
  d <- d[grepl(paste0("^", base), d$tech) & d$region == region, , drop = FALSE]
  if (nrow(d) == 0) return(0)
  max(stats::aggregate(list(v = d$value), by = list(y = d$year), FUN = sum)$v)
}

# Internal helpers under test.
vt_expand_one <- getFromNamespace(".expand_one_tech", "energyRt")
vt_levels     <- getFromNamespace(".variant_levels", "energyRt")
vt_prefix     <- getFromNamespace(".variant_prefix", "energyRt")
vt_prefix_def <- getFromNamespace(".variant_prefix_default", "energyRt")
vt_prefix_mrg <- getFromNamespace(".variant_prefix_merge", "energyRt")
vt_cfg2set    <- getFromNamespace(".config_to_settings", "energyRt")
