# Dataset references from the repository store (R/repo_store.R +
# R/dataset_store.R): a weather/demand table already in the dataset store is
# saved as a {name, hash} ref — stored once however many repository (and,
# above them, model) versions use it. Loading resolves refs eagerly.

dr_root <- function(...) {
  gsub("[\\/]+", "/", file.path(tempdir(), "dataset-refs-suite", ...))
}

dr_local <- function(env = parent.frame()) {
  old_dp <- set_datasets_path(dr_root("datasets"))
  old_rp <- set_repositories_path(dr_root("repositories"))
  old_rf <- set_registry_file(dr_root("reg.csv"))
  expr <- bquote({
    set_datasets_path(.(old_dp))
    set_repositories_path(.(old_rp))
    set_registry_file(.(old_rf))
  })
  do.call(on.exit, list(expr, add = TRUE), envir = env)
  invisible(NULL)
}

dr_slices <- sprintf("s%02d", 1:4)

dr_repo <- function(wvals = c(1, 0.5, 0.25, 0.2)) {
  newRepository(
    "drr",
    newCommodity("ELC", timeframe = "SL"),
    newWeather("WCF", timeframe = "SL",
               weather = data.frame(timeslice = dr_slices, wval = wvals)),
    newTechnology("WIND", output = list(comm = "ELC"),
                  weather = data.frame(weather = "WCF", waf.up = 1),
                  invcost = list(invcost = 1000), olife = list(olife = 30),
                  cap2act = 1),
    newDemand("DEM", commodity = "ELC",
              demand = data.frame(timeslice = dr_slices, demand = 10)))
}

test_that("a stored weather table is saved by reference and loads back full", {
  dr_local()
  unlink(dr_root(), recursive = TRUE)
  repo <- dr_repo()
  w_full <- repo@data[["WCF"]]@weather

  # the tables go to the dataset store ONCE; auto mode then references them
  save_dataset(w_full, "wcf_series", verbose = FALSE)
  save_dataset(repo@data[["DEM"]]@demand, "dem_series", verbose = FALSE)
  saved <- suppressMessages(save_repository(repo, verbose = FALSE))

  # manifest records the refs
  mf <- yaml::read_yaml(fp(saved@misc$path, "repository.yml"))
  expect_length(mf$datasets, 2L)
  mf$datasets <- Filter(function(e) e$object == "WCF", mf$datasets)
  expect_identical(mf$datasets[[1]]$object, "WCF")
  expect_identical(mf$datasets[[1]]$slot, "weather")
  expect_identical(mf$datasets[[1]]$name, "wcf_series")
  expect_identical(mf$datasets[[1]]$source, "ref")

  # no duplicate weather parquet inside the repo entry
  expect_false(dir.exists(fp(saved@misc$path, "data", "WCF", "weather")))

  # the raw store object is a stub: 0-row slot + the ref on @misc
  e <- new.env(); load(fp(saved@misc$path, "repo.RData"), envir = e)
  raw <- get(ls(e), envir = e)
  expect_identical(nrow(raw@data[["WCF"]]@weather), 0L)
  expect_identical(raw@data[["WCF"]]@misc$dataset_ref$weather$name,
                   "wcf_series")

  # the RETURNED object stays fully loaded, with no stale ref
  expect_identical(saved@data[["WCF"]]@weather, w_full)
  expect_null(saved@data[["WCF"]]@misc$dataset_ref)

  # load resolves the ref back to the identical table
  ld <- load_repository("drr", verbose = FALSE)
  expect_identical(dataset_hash(ld@data[["WCF"]]@weather),
                   dataset_hash(w_full))
  expect_null(ld@data[["WCF"]]@misc$dataset_ref)

  # content identity is storage-independent: with every big table restored
  # from the dataset store, the loaded repository hashes as the original
  expect_identical(repository_hash(ld), repository_hash(repo))
})

test_that("embed_datasets = TRUE embeds; FALSE requires a store hit", {
  dr_local()
  unlink(dr_root(), recursive = TRUE)
  repo <- dr_repo(c(0.9, 0.4, 0.3, 0.1))

  # unstored + FALSE errors, naming the remedy
  expect_error(
    suppressMessages(save_repository(repo, embed_datasets = FALSE,
                                     verbose = FALSE)),
    "save_dataset")

  # unstored + auto embeds (current behavior, parquet in the entry)
  saved <- suppressMessages(save_repository(repo, verbose = FALSE))
  mf <- yaml::read_yaml(fp(saved@misc$path, "repository.yml"))
  expect_null(mf$datasets)
  expect_true(dir.exists(fp(saved@misc$path, "data", "WCF", "weather")))

  # stored + TRUE still embeds
  save_dataset(repo@data[["WCF"]]@weather, "wcf2", verbose = FALSE)
  saved2 <- suppressMessages(save_repository(repo, embed_datasets = TRUE,
                                             overwrite = TRUE,
                                             verbose = FALSE))
  mf2 <- yaml::read_yaml(fp(saved2@misc$path, "repository.yml"))
  expect_null(mf2$datasets)
})

dr_cal <- function() {
  newCalendar(timetable = make_timetable(
    struct = list(ANNUAL = "ANNUAL", SL = dr_slices)), name = "drc")
}

dr_model <- function(name = "drm", ...) {
  newModel(name, region = "R1", discount = 0, calendar = dr_cal(),
           horizon = newHorizon(2025), repo = dr_repo(), ...)
}

dr_local_model <- function(env = parent.frame()) {
  dr_local(env = env)
  old_mp <- set_models_path(dr_root("models"))
  old_sp <- set_scenarios_path(dr_root("scenarios"))
  expr <- bquote({
    set_models_path(.(old_mp))
    set_scenarios_path(.(old_sp))
  })
  do.call(on.exit, list(expr, add = TRUE), envir = env)
  invisible(NULL)
}

test_that("save_model references the geoscale map and repo tables; load restores", {
  skip_if_not_installed("geoscales")
  dr_local_model()
  unlink(dr_root(), recursive = TRUE)
  gs <- utopia_geoscale()
  mod <- dr_model(geoscale = gs)
  h_full <- model_hash(mod)

  save_dataset(gs, "umap", verbose = FALSE)
  save_dataset(mod@data[[1]]@data[["WCF"]]@weather, "wcf_m", verbose = FALSE)
  saved <- suppressMessages(save_model(mod, verbose = FALSE))
  expect_identical(saved@misc$hash, h_full)   # hash-before-stub invariant

  mf <- yaml::read_yaml(fp(saved@misc$path, "model.yml"))
  slots <- vapply(mf$datasets, `[[`, "", "slot")
  expect_setequal(slots, c("geoscale", "weather"))

  # the raw store object holds stubs
  e <- new.env(); load(fp(saved@misc$path, "mod.RData"), envir = e)
  raw <- get(ls(e), envir = e)
  expect_null(raw@config@geoscale)
  expect_identical(raw@config@misc$dataset_ref$geoscale$name, "umap")

  # the RETURNED object is full
  expect_identical(dataset_hash(saved@config@geoscale), dataset_hash(gs))

  # load restores both payloads
  ld <- load_model("drm", verbose = FALSE)
  expect_identical(dataset_hash(ld@config@geoscale), dataset_hash(gs))
  expect_identical(dataset_hash(ld@data[[1]]@data[["WCF"]]@weather),
                   dataset_hash(mod@data[[1]]@data[["WCF"]]@weather))
})

test_that("save_scenario references the settings geoscale; load restores", {
  skip_if_not_installed("geoscales")
  dr_local_model()
  unlink(dr_root(), recursive = TRUE)
  gs <- utopia_geoscale()
  save_dataset(gs, "umap_s", verbose = FALSE)

  scen <- suppressMessages(interpolate_model(
    dr_model(name = "drs"), name = "drs",
    path = dr_root("scenarios", "drs")))
  scen@settings@geoscale <- gs
  saved <- suppressMessages(save_scenario(scen, verbose = FALSE))

  mf <- yaml::read_yaml(fp(saved@path, "scenario.yml"))
  expect_identical(mf$datasets[[1]]$object, "settings")
  expect_identical(mf$datasets[[1]]$slot, "geoscale")

  # the returned object keeps the map; the loaded one restores it
  expect_identical(dataset_hash(saved@settings@geoscale), dataset_hash(gs))
  ld <- suppressMessages(load_scenario(saved@path, env = NULL,
                                       verbose = FALSE))
  expect_identical(dataset_hash(ld@settings@geoscale), dataset_hash(gs))
  expect_null(ld@settings@misc$dataset_ref)
})

test_that("a ref-thinned model solves identically to an embedded one", {
  skip_if_no_solver()
  dr_local_model()
  unlink(dr_root(), recursive = TRUE)
  mod <- dr_model(name = "drv")

  # baseline: embedded save (the fixture covers demand with the WIND tech
  # alone, which the pre-solve supply heuristic does not count — harmless)
  base <- suppressWarnings(suppressMessages(solve_model(
    mod, name = "drv_base", path = dr_root("scenarios", "drv_base"),
    echo = FALSE)))
  obj_base <- get_variable(base, "vObjective", data = TRUE)$value[1]

  # referenced save -> load -> solve
  save_dataset(mod@data[[1]]@data[["WCF"]]@weather, "wcf_v", verbose = FALSE)
  save_dataset(mod@data[[1]]@data[["DEM"]]@demand, "dem_v", verbose = FALSE)
  suppressMessages(save_model(mod, verbose = FALSE))
  ld <- load_model("drv", verbose = FALSE)
  sol <- suppressWarnings(suppressMessages(solve_model(
    ld, name = "drv_ref", path = dr_root("scenarios", "drv_ref"),
    echo = FALSE)))
  obj_ref <- get_variable(sol, "vObjective", data = TRUE)$value[1]
  expect_equal(as.numeric(obj_ref), as.numeric(obj_base), tolerance = 1e-9)
})

test_that("a missing dataset store fails the load loudly", {
  dr_local()
  unlink(dr_root(), recursive = TRUE)
  repo <- dr_repo()
  save_dataset(repo@data[["WCF"]]@weather, "wcf3", verbose = FALSE)
  suppressMessages(save_repository(repo, verbose = FALSE))
  unlink(dr_root("datasets"), recursive = TRUE)
  unlink(dr_root("reg.csv"))
  expect_error(load_repository("drr", verbose = FALSE),
               "dataset store")
})
