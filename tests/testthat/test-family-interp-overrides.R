# =========================================================================== #
# Feature family: interp overrides -- the `code =` template-override fork of
# interpolate_model(), plus a KNOWN-GAP pin on the config-level per-column
# defVal / interpolation overrides.
#
# `code = list(GLPK = <lines>)` replaces the baked solver template for that
# backend; keys follow `.modelCode` (GLPK, GAMS, JuMP, PYOMOConcrete, ...).
#
# KNOWN GAP (pinned): `config@defVal` / `config@interpolation` are initialised
# from config_default_values.yml / config_default_interpolation.yml
# (class-config.R:146-147, with an unfinished "add import" comment) but are
# never read anywhere in the pipeline -- parameter defaults come exclusively
# from `.modInp` (class-modInp.R:76). Editing the config slots is a silent
# no-op. When the override is wired up, replace the pin with a real test
# (e.g. defVal af.up = 0.5 must double the required capacity).
# =========================================================================== #

io_build <- function() {
  newModel("io",
    repo = newRepository("io_repo",
      newCommodity("COA", timeframe = "ANNUAL"),
      newCommodity("ELC", timeframe = "ANNUAL"),
      newSupply("SUP", commodity = "COA", supply = data.frame(cost = 1)),
      newTechnology("ECOA", input = list(comm = "COA"), output = list(comm = "ELC"),
                    invcost = list(invcost = 1000), olife = list(olife = 30),
                    cap2act = 1),
      newDemand("DEM", commodity = "ELC", demand = data.frame(demand = 40))),
    calendar = newCalendar(
      timetable = make_timetable(struct = list(ANNUAL = "ANNUAL")),
      name = "io_cal"),
    region = "R1", horizon = newHorizon(2025), discount = 0)
}

io_base_obj <- 40 + 40 * 1000 / 30

# @covers pTechInvcost depth=S backends=glpk forks=code
test_that("family interp-overrides: code= replaces the solver template", {
  skip_if_no_solver()
  tpl <- get(".modelCode", envir = asNamespace("energyRt"))$GLPK
  marker <- "# __ENERGYRT_TEST_CODE_OVERRIDE__"
  tpl2 <- c(marker, tpl)
  scen <- suppressMessages(suppressWarnings(interpolate_model(
    io_build(), name = "io_code", overwrite = TRUE,
    code = list(GLPK = tpl2))))
  scen <- .fork_solve(scen, solver_options$glpk)
  expect_true(verify_solution(scen)$ok)
  expect_equal(.fork_objective(scen), io_base_obj, tolerance = 1e-6)
  # the written model file carries the injected marker
  mod_file <- file.path(scen@misc$tmp.dir %||% "", "energyRt.mod")
  if (!file.exists(mod_file)) {
    cand <- list.files(scen@path, pattern = "^energyRt\\.mod$",
                       recursive = TRUE, full.names = TRUE)
    skip_if(length(cand) == 0, "written model file not found")
    mod_file <- cand[[1]]
  }
  expect_true(any(grepl(marker, readLines(mod_file, warn = FALSE), fixed = TRUE)))
})

test_that("family interp-overrides: KNOWN-GAP config defVal/interpolation overrides are inert", {
  skip_if_no_solver()
  # base: af defaults to 1 -> cap 40, obj 1373.333
  base <- .fork_solve(suppressMessages(suppressWarnings(interpolate_model(
    io_build(), name = "io_dv_base", overwrite = TRUE))), solver_options$glpk)
  ob <- .fork_objective(base)
  # editing config@defVal (af.up -> 0.5) SHOULD double the capacity need; today
  # the slot is never consumed, so the objective must be unchanged. A fix that
  # wires the override up will break this pin -- replace it with the real
  # assertion (obj rises by 40 * 1000/30).
  mod <- io_build()
  expect_true("af.up" %in% colnames(mod@config@defVal))
  mod@config@defVal$af.up <- 0.5
  over <- .fork_solve(suppressMessages(suppressWarnings(interpolate_model(
    mod, name = "io_dv_over", overwrite = TRUE))), solver_options$glpk)
  expect_equal(.fork_objective(over), ob, tolerance = 1e-9,
               label = "KNOWN-GAP pin: config@defVal override ignored (wiring pending)")
})

`%||%` <- function(a, b) if (is.null(a)) b else a
