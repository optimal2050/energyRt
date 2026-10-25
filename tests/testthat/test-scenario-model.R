# A scenario keeps the model as given: `scen@model` holds the objects as
# defined, and the variants, tiers and transform clones interpolation built from
# them are rebuilt on demand (.scen_build_model()). A sealed model is therefore
# referenced by a saved scenario rather than copied into it.

.sm_model <- function() {
  cal <- newCalendar(timetable = make_timetable(
    struct = list(ANNUAL = "ANNUAL", SEASON = c("WIN", "SUM"))), name = "smcal")
  rows <- function(cl, win, sum) {
    data.frame(cluster = cl, region = "R1", year = 2025L,
               timeslice = c("WIN", "SUM"), wval = c(win, sum))
  }
  w <- newWeather(
    name = "WWIN", timeframe = "SEASON",
    cluster = data.frame(cluster = c("T1", "T2"), desc = c("good", "poor"),
                         region = NA_character_, order = 1:2),
    weather = rbind(rows("T1", 0.9, 0.6), rows("T2", 0.45, 0.3)))
  cl <- c("T1", "T2")
  tech <- newTechnology(
    "EWIN", output = list(comm = "ELC"),
    cluster = data.frame(cluster = cl, region = NA_character_),
    capacity = data.frame(cluster = cl, cap.up = 10),
    invcost = data.frame(cluster = cl, invcost = 1),
    weather = data.frame(cluster = cl, weather = "WWIN", waf.up = 1),
    vintage = data.frame(olife = 30L), cap2act = 1)
  newModel("SM", calendar = cal, region = "R1",
           horizon = newHorizon(2025), discount = 0,
           repo = newRepository("sm_repo",
             newCommodity("ELC", timeframe = "SEASON"),
             newDemand("DE", commodity = "ELC",
                       demand = data.frame(timeslice = c("WIN", "SUM"),
                                           demand = 3)),
             newImport("IMP", commodity = "ELC",
                       import = data.frame(price = 100)),
             w, tech))
}

.sm_interp <- function(mod, name = "SM_S") {
  suppressMessages(suppressWarnings(
    interpolate_model(mod, name = name, verbose = FALSE)))
}

.sm_names <- function(m, cls) names(getObjects(m, cls))

test_that("a scenario keeps the model as given and rebuilds the expanded one", {
  mod <- .sm_model()
  scen <- .sm_interp(mod)
  expect_setequal(.sm_names(scen@model, "technology"), "EWIN")
  expect_setequal(.sm_names(scen@model, "weather"), "WWIN")
  expect_identical(model_hash(scen@model), model_hash(mod))
  built <- .scen_build_model(scen)
  expect_setequal(.sm_names(built, "technology"),
                  as.character(scen@modInp@sets$tech))
  expect_setequal(.sm_names(built, "weather"),
                  as.character(scen@modInp@sets$weather))
  expect_false("EWIN" %in% .sm_names(built, "technology"))
})

test_that("re-interpolating a scenario gives the same inputs", {
  scen <- .sm_interp(.sm_model())
  again <- .sm_interp(scen)
  ci <- compare_inputs(again, scen)
  expect_equal(sum(ci$summary$status != "identical"), 0)
})

test_that("the solved scenario verifies against the rebuilt objects", {
  skip_if_no_solver()
  scen <- .sm_interp(.sm_model())
  sol <- suppressMessages(suppressWarnings(
    solve_scenario(scen, solver = solver_options$glpk, wait = TRUE)))
  v <- verify_solution(sol)
  expect_true(v$ok)
})

test_that("a saved scenario references the sealed model and loads", {
  withr::local_options(energyRt.save_mode = "ref")
  tmp <- withr::local_tempdir()
  suppressMessages(open_project(tmp))
  mod <- .sm_model()
  suppressMessages(save_model(mod, verbose = FALSE))
  suppressMessages(seal_model(mod, verbose = FALSE))
  scen <- .sm_interp(mod)
  suppressMessages(save_scenario(scen, verbose = FALSE))
  mf <- yaml::read_yaml(file.path(scen@path, "scenario.yml"))
  expect_identical(mf$model$source, "ref")
  expect_identical(mf$model$hash, model_hash(mod))
  expect_false(dir.exists(file.path(scen@path, "model")))
  ld <- suppressMessages(load_scenario(scen@path, env = NULL, verbose = FALSE))
  expect_setequal(.sm_names(ld@model, "technology"), "EWIN")
  ci <- suppressWarnings(compare_inputs(ld, scen))
  expect_equal(sum(ci$summary$status != "identical"), 0)
  expect_setequal(.sm_names(.scen_build_model(ld), "weather"),
                  as.character(scen@modInp@sets$weather))
})

test_that("a missing pWeather table is recomputed from the rebuilt tiers", {
  withr::local_options(energyRt.save_mode = "ref",
                       energyRt.ondisk_min_rows = 1,
                       energyRt.param_ref_min_rows = 1)
  tmp <- withr::local_tempdir()
  suppressMessages(open_project(tmp))
  scen <- .sm_interp(.sm_model())
  want <- as.data.frame(get_data_slot(scen@modInp@parameters$pWeather))
  suppressWarnings(suppressMessages(save_scenario(scen, verbose = FALSE)))
  ref <- yaml::read_yaml(file.path(scen@path, "scenario.yml"))$datasets
  pw <- Filter(function(d) identical(d$object, "pWeather"), ref)
  skip_if(!length(pw), "pWeather was not referenced")
  entry <- file.path(tmp, pw[[1]]$path)
  expect_true(dir.exists(entry))
  unlink(entry, recursive = TRUE)
  expect_false(dir.exists(entry))
  ld <- suppressMessages(suppressWarnings(
    load_scenario(scen@path, env = NULL, verbose = FALSE)))
  got <- as.data.frame(suppressWarnings(get_data_slot(ld@modInp@parameters$pWeather)))
  key <- function(d) d[do.call(order, d[setdiff(names(d), "value")]), ]
  expect_equal(nrow(got), nrow(want))
  expect_equal(key(got)$value, key(want)$value)
})

test_that("a spatial sample is rebuilt from the model as given", {
  skip_if_not_installed("geoscales")
  withr::local_options(energyRt.save_mode = "ref")
  tmp <- withr::local_tempdir()
  suppressMessages(open_project(tmp))
  regions <- c("R1", "R2", "R3")
  gs <- geoscales::geoscale_from_leaftable(
    data.frame(zone = c("Z12", "Z12", "Z3"), region = regions, km2 = 1),
    geoframes = c("zone", "region"), name = "g3")
  mod <- newModel("SMR", region = regions, horizon = newHorizon(2020),
    discount = 0.05,
    repo = newRepository("r",
      newCommodity("COA", timeframe = "ANNUAL"),
      newCommodity("ELC", timeframe = "ANNUAL"),
      newSupply("SUP_COA", commodity = "COA",
                supply = data.frame(region = regions, cost = 5)),
      newTechnology("ECOA", input = list(comm = "COA"),
                    output = list(comm = "ELC"),
                    invcost = data.frame(invcost = 1000),
                    olife = list(olife = 30), cap2act = 1),
      newDemand("DEM_ELC", commodity = "ELC",
                demand = data.frame(region = regions, demand = c(10, 20, 30))),
      newTrade("TR23", commodity = "ELC", cap2act = 1,
               routes = data.frame(src = "R2", dst = "R3"),
               trade = data.frame(src = "R2", dst = "R3", ava.up = 4),
               invcost = data.frame(invcost = 10),
               capacity = data.frame(year = 2020, cap.up = 100))))
  suppressMessages(save_model(mod, verbose = FALSE))
  suppressMessages(seal_model(mod, verbose = FALSE))
  bp <- data.frame(src = "R2", dst = "R3", price = 50, cap.up = 4)
  scen <- suppressMessages(interpolate_model(
    mod, "S12", geoscales::filter_geoscale(gs, "region", c("R1", "R2"),
                                           drop_empty_geoframes = TRUE),
    boundary_prices = bp, verbose = FALSE))
  expect_false("boundary_stubs" %in% names(scen@model@data))
  expect_identical(scen@misc$spatial$regions, c("R1", "R2"))
  built <- .scen_build_model(scen)
  expect_true("EXP_TR23_R22R3" %in% names(getObjects(built, "export")))
  suppressMessages(save_scenario(scen, verbose = FALSE))
  mf <- yaml::read_yaml(file.path(scen@path, "scenario.yml"))
  expect_identical(mf$model$source, "ref")
  ld <- suppressMessages(load_scenario(scen@path, env = NULL, verbose = FALSE))
  expect_true("EXP_TR23_R22R3" %in%
                names(getObjects(.scen_build_model(ld), "export")))
})
