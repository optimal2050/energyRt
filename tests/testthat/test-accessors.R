# Name-based access (R/accessors.R + .scenario_resolve + getData widening):
# open_project() anchors the session; getScenario() serves cached thin
# shells out of .scen with manifest-stamp freshness; getData() accepts
# names and environments. Fixtures from helper-stock-path.R.

acc_root <- function(...) {
  gsub("[\\/]+", "/", file.path(tempdir(), "accessors-suite", ...))
}

# open a throwaway project; restore all options on exit
acc_project <- function(env = parent.frame()) {
  dir.create(acc_root(), recursive = TRUE, showWarnings = FALSE)
  old <- suppressMessages(open_project(acc_root(), verbose = FALSE))
  expr <- bquote({
    old <- .(old)
    set_registry_file(old$registry_file)
    set_scenarios_path(old$scenarios_path)
    set_models_path(old$models_path)
    set_repositories_path(old$repositories_path)
    set_datasets_path(old$datasets_path)
    set_reports_path(old$reports_path)
    set_levcost_cache_path(old$levcost_cache_path)
  })
  do.call(on.exit, list(expr, add = TRUE), envir = env)
  invisible(NULL)
}

# one solved + saved scenario under the open project
acc_scen <- function(name, stock = c(100, 100, 100)) {
  sc <- interpolate_model(sp_tech(stock, optret = FALSE, name = name),
                          name = name)
  sol <- solve_scenario(sc)
  suppressMessages(save_scenario(sol, verbose = FALSE))
}

acc_clear_bank <- function(...) {
  bank <- energyRt:::.scen_bank()
  rm(list = intersect(c(...), ls(bank, all.names = TRUE)), envir = bank)
}

test_that("open_project points the five options and reports them back", {
  acc_project()
  expect_identical(get_registry_file(),
                   fp(acc_root(), "energyRt_registry.csv"))
  expect_identical(get_scenarios_path(), fp(acc_root(), "scenarios"))
  expect_identical(get_models_path(), fp(acc_root(), "models"))
  expect_identical(get_repositories_path(), fp(acc_root(), "repositories"))
  expect_identical(get_datasets_path(), fp(acc_root(), "datasets"))
  expect_identical(get_reports_path(), fp(acc_root(), "reports"))
  expect_identical(get_levcost_cache_path(), fp(acc_root(), "levcosts"))
})

test_that("load_scenario resolves names via registry and via dir scan", {
  skip_if_no_solver()
  acc_project()
  unlink(acc_root("scenarios"), recursive = TRUE)
  unlink(get_registry_file())
  acc_scen("acc_a")

  ld <- load_scenario("acc_a", env = NULL, verbose = FALSE)
  expect_s4_class(ld, "scenario")
  expect_identical(ld@name, "acc_a")

  # registry gone -> the scenarios-store scan still finds it
  unlink(get_registry_file())
  ld2 <- load_scenario("acc_a", env = NULL, verbose = FALSE)
  expect_identical(ld2@name, "acc_a")

  # unknown name errors with the remedies
  expect_error(load_scenario("acc_nope", env = NULL, verbose = FALSE),
               "refresh_registry")
})

test_that("two folders claiming one name is an explicit error", {
  acc_project()
  dir.create(fp(get_scenarios_path(), "dup1"), recursive = TRUE)
  dir.create(fp(get_scenarios_path(), "dup2"), recursive = TRUE)
  yaml::write_yaml(list(layout = 3L, name = "DUP"),
                   fp(get_scenarios_path(), "dup1", "scenario.yml"))
  yaml::write_yaml(list(layout = 3L, name = "DUP"),
                   fp(get_scenarios_path(), "dup2", "scenario.yml"))
  unlink(get_registry_file())
  expect_error(energyRt:::.scenario_resolve("DUP"), "matches 2 folders")
})

test_that("getScenario caches in .scen and reloads on manifest drift", {
  skip_if_no_solver()
  acc_project()
  unlink(acc_root("scenarios"), recursive = TRUE)
  unlink(get_registry_file())
  acc_clear_bank("acc_c")
  on.exit(acc_clear_bank("acc_c"), add = TRUE)
  sol <- acc_scen("acc_c")

  s1 <- getScenario("acc_c")
  expect_s4_class(s1, "scenario")
  bank <- energyRt:::.scen_bank()
  expect_true(exists("acc_c", envir = bank, inherits = FALSE))

  # a second call serves the cached shell (same stamp, same object)
  s2 <- getScenario("acc_c")
  expect_identical(s2@misc$.accessor_stamp, s1@misc$.accessor_stamp)

  # re-saving moves the manifest stamp -> automatic reload
  Sys.sleep(1.1)   # `updated` has 1s resolution
  suppressMessages(save_scenario(sol, verbose = FALSE))
  s3 <- getScenario("acc_c")
  expect_false(identical(s3@misc$.accessor_stamp, s1@misc$.accessor_stamp))

  # refresh = TRUE always reloads
  expect_s4_class(getScenario("acc_c", refresh = TRUE), "scenario")
})

test_that("getModel / getRepository / getDataset resolve store names", {
  acc_project()
  m <- suppressMessages(save_model(sp_tech(c(80, 80, 80), optret = FALSE,
                                           name = "acc_m"), verbose = FALSE))
  gm <- getModel("acc_m")
  expect_s4_class(gm, "model")
  # the loaded model is thinned (lazy slots), so compare the RECORDED store
  # hash, not a recomputed one (see ?model_hash)
  expect_identical(gm@misc$hash, m@misc$hash)

  repo <- m@data[[1]]
  suppressMessages(save_repository(repo, verbose = FALSE))
  gr <- getRepository(repo@name)
  expect_s4_class(gr, "repository")

  save_dataset(data.frame(a = 1:3), "acc_d", verbose = FALSE)
  expect_identical(getDataset("acc_d")$a, 1:3)
})

test_that("getData takes names and environments directly", {
  skip_if_no_solver()
  acc_project()
  unlink(acc_root("scenarios"), recursive = TRUE)
  unlink(get_registry_file())
  acc_clear_bank("acc_x", "acc_y")
  on.exit(acc_clear_bank("acc_x", "acc_y"), add = TRUE)
  acc_scen("acc_x", c(100, 100, 100))
  acc_scen("acc_y", c(50, 50, 50))

  d <- getData(c("acc_x", "acc_y"), name = "vObjective", merge = TRUE)
  expect_true(all(c("acc_x", "acc_y") %in% d$scenario))
  expect_identical(nrow(d), 2L)

  # the shells landed in the bank; an environment reads like a list
  e <- new.env()
  assign("acc_x", getScenario("acc_x"), envir = e)
  assign("acc_y", getScenario("acc_y"), envir = e)
  d2 <- getData(e, name = "vObjective", merge = TRUE)
  expect_setequal(d2$scenario, d$scenario)
  expect_equal(sort(d2$value), sort(d$value))

  # batch loader returns a ready named list
  scens <- load_scenarios(verbose = FALSE)
  expect_setequal(names(scens), c("acc_x", "acc_y"))
  d3 <- getData(scens, name = "vObjective", merge = TRUE)
  expect_identical(nrow(d3), 2L)
})
