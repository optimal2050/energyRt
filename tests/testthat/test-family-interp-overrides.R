# =========================================================================== #
# Feature family: interp overrides -- the `code =` template-override fork of
# interpolate_model(), plus a KNOWN-GAP pin on the config-level per-column
# defVal / interpolation overrides.
#
# `code = list(GLPK = <lines>)` replaces the baked solver template for that
# backend; keys follow `.modelCode` (GLPK, GAMS, JuMP, PYOMOConcrete, ...).
#
# Per-column defVal / interpolation overrides (wired 2026-08-25, previously
# initialised-but-never-consumed): `settings@defVal` / `settings@interpolation`
# override the `.modInp` catalog per column at modInp init
# (.apply_settings_param_overrides), keyed by colName provenance -- a bounds
# parameter like pTechAf (colName "af") reads `af.lo`/`af.up`, a numpar like
# pDemand reads `dem`. Model-level `config` flows in via .config_to_settings().
# An override applies ONLY where the value differs from the baked baseline, so
# an untouched table is a guaranteed no-op (mirror drift cannot clobber
# modInp.yml -- the tm goldens pin this).
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

# @covers pTechAf depth=S backends=glpk forks=default
test_that("family interp-overrides: a config defVal override changes the effective default", {
  skip_if_no_solver()
  # base: af.up defaults to 1 -> cap 40, obj 1373.333
  base <- .fork_solve(suppressMessages(suppressWarnings(interpolate_model(
    io_build(), name = "io_dv_base", overwrite = TRUE))), solver_options$glpk)
  expect_equal(.fork_objective(base), io_base_obj, tolerance = 1e-6)
  # config-level override af.up -> 0.5: no af declared on the tech, so the
  # DEFAULT governs -- capacity need doubles (cap 80)
  mod <- io_build()
  expect_true("af.up" %in% colnames(mod@config@defVal))
  mod@config@defVal$af.up <- 0.5
  over <- suppressMessages(suppressWarnings(interpolate_model(
    mod, name = "io_dv_over", overwrite = TRUE)))
  p <- scen_par <- over@modInp@parameters[["pTechAf"]]
  expect_equal(p@defVal[2], 0.5, label = "pTechAf up-default overridden")
  over <- .fork_solve(over, solver_options$glpk)
  expect_true(verify_solution(over)$ok)
  expect_equal(ff_solution_sum(over, "vTechCap"), 80, tolerance = 1e-6)
  expect_equal(.fork_objective(over), 40 + 80 * 1000 / 30, tolerance = 1e-6)
})

test_that("family interp-overrides: an interpolation-rule override stops extrapolation", {
  # demand declared for 2030 only, horizon 2025..2035: the default
  # back.inter.forth extrapolates it to every milestone; overriding the `dem`
  # rule to `inter` leaves the un-covered years without demand rows
  io_build2 <- function() {
    newModel("io2",
      repo = newRepository("io2_repo",
        newCommodity("COA", timeframe = "ANNUAL"),
        newCommodity("ELC", timeframe = "ANNUAL"),
        newSupply("SUP", commodity = "COA", supply = data.frame(cost = 1)),
        newTechnology("ECOA", input = list(comm = "COA"),
                      output = list(comm = "ELC"),
                      invcost = list(invcost = 1000), olife = list(olife = 30),
                      cap2act = 1),
        newDemand("DEM", commodity = "ELC",
                  demand = data.frame(year = 2030, demand = 40))),
      calendar = newCalendar(
        timetable = make_timetable(struct = list(ANNUAL = "ANNUAL")),
        name = "io2_cal"),
      region = "R1", horizon = newHorizon(2025:2035, intervals = 5),
      discount = 0)
  }
  scen0 <- suppressMessages(suppressWarnings(interpolate_model(
    io_build2(), name = "io_int_base", overwrite = TRUE)))
  yrs0 <- sort(unique(ff_param(scen0, "pDemand")$year))
  expect_gte(length(yrs0), 2)   # extrapolated back/forth across milestones

  mod <- io_build2()
  expect_true("dem" %in% colnames(mod@config@interpolation))
  mod@config@interpolation$dem <- "inter"
  scen1 <- suppressMessages(suppressWarnings(interpolate_model(
    mod, name = "io_int_over", overwrite = TRUE)))
  expect_equal(unique(scen1@modInp@parameters[["pDemand"]]@interpolation), "inter")
  yrs1 <- sort(unique(ff_param(scen1, "pDemand")$year))
  expect_lt(length(yrs1), length(yrs0))
})

`%||%` <- function(a, b) if (is.null(a)) b else a
