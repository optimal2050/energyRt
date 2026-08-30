# Layout 3 run folders: runs/<solve>/ (base) with run.yml provenance and a
# solver/ working directory (R/runs.R + R/solve.R resolver). Fixtures from
# helper-stock-path.R: a one-tech, three-year GLPK model.

rn_scen_path <- function(name) {
  p <- file.path(tempdir(), "runs-suite", name)
  gsub("[\\/]+", "/", p)
}

rn_solved <- local({
  cache <- new.env()
  function() {
    # one shared solved scenario for the read-only assertions
    if (!is.null(cache$scen)) return(cache$scen)
    sc <- interpolate_model(sp_tech(c(100, 100, 100), optret = FALSE,
                                    name = "rnbase"),
                            name = "rnbase", path = rn_scen_path("rnbase"))
    cache$scen <- solve_scenario(sc)
    cache$scen
  }
})

test_that("a solve lands in runs/<solver>/ with a run.yml record", {
  skip_if_no_solver()
  sol <- rn_solved()
  expect_true(isTRUE(sol@status$optimal))
  expect_identical(sol@misc$variant %||% "", "")  # base problem, no variant
  solve_lbl <- sol@misc$run
  expect_true(nzchar(solve_lbl))
  expect_null(sol@misc$tmp.dir)     # recorded runs store no dir path
  expect_null(sol@misc$solver.dir)

  run_dir <- file.path(sol@path, "runs", solve_lbl)   # FLAT: no default/
  expect_true(dir.exists(file.path(run_dir, "solver", "output")))
  expect_true(file.exists(file.path(run_dir, "run.yml")))

  rec <- scenario_run_info(sol, solve_lbl)
  expect_identical(rec$status, "solved")
  expect_identical(rec$variant, "")
  expect_identical(rec$scenario, "rnbase")
  expect_true(nzchar(rec$started))
  expect_true(is.numeric(rec$duration_sec))
  expect_true(is.numeric(rec$objective) && is.finite(rec$objective))
  expect_identical(rec$modinp, "shared")

  runs <- scenario_runs(sol)
  expect_identical(nrow(runs), 1L)
  expect_identical(runs$run, solve_lbl)   # run id == solve label for base runs
  expect_identical(runs$variant, "")
  expect_identical(runs$status, "solved")
  expect_true(runs$active)
})

test_that("a labeled second run coexists and read_solution(run=) switches", {
  skip_if_no_solver()
  sol <- rn_solved()
  first_lbl <- sol@misc$run
  obj1 <- scenario_run_info(sol, first_lbl)$objective

  sol2 <- solve_scenario(sol, force = TRUE, run = "glpk-b")
  expect_identical(sol2@misc$run, "glpk-b")
  expect_true(dir.exists(file.path(sol2@path, "runs", "glpk-b",
                                   "solver", "output")))
  runs <- scenario_runs(sol2)
  expect_identical(nrow(runs), 2L)
  expect_setequal(runs$run, c(first_lbl, "glpk-b"))
  expect_identical(runs$run[runs$active], "glpk-b")
  # same problem, same solver -> same objective in both records
  expect_equal(scenario_run_info(sol2, "glpk-b")$objective, obj1)

  # switch the active run back
  sol3 <- read_solution(sol2, run = first_lbl, echo = FALSE)
  expect_identical(sol3@misc$run, first_lbl)
  expect_identical(sol3@misc$variant %||% "", "")
  expect_true(isTRUE(sol3@status$optimal))
  v <- get_variable(sol3, "vObjective", data = TRUE)
  expect_equal(as.numeric(v$value[1]), obj1)

  # unknown run errors and names the available ones
  expect_error(read_solution(sol2, run = "nope"), "Available runs")
})

test_that("run.conflict suffixes or errors on an existing recorded run", {
  skip_if_no_solver()
  sol <- rn_solved()
  sol5 <- solve_scenario(sol, force = TRUE, run = "cf", run.conflict = "overwrite")
  expect_identical(sol5@misc$run, "cf")
  sol6 <- solve_scenario(sol5, force = TRUE, run = "cf", run.conflict = "suffix")
  expect_identical(sol6@misc$run, "cf-2")
  expect_true(file.exists(file.path(sol6@path, "runs", "cf-2", "run.yml")))
  expect_error(
    solve_scenario(sol6, force = TRUE, run = "cf", run.conflict = "error"),
    "already exists")
})

test_that("a transient solve leaves no run record", {
  skip_if_no_solver()
  sc <- interpolate_model(sp_tech(c(50, 50, 50), optret = FALSE,
                                  name = "rntmp"),
                          name = "rntmp", path = rn_scen_path("rntmp"))
  sol <- solve_scenario(sc, transient = TRUE)
  expect_true(isTRUE(sol@status$optimal))
  expect_false(dir.exists(file.path(sol@path, "runs")))
  expect_identical(nrow(scenario_runs(sol)[scenario_runs(sol)$status !=
                                             "legacy", ]), 0L)
  # the throwaway solver dir (and its now-empty solver/ wrapper) is deleted
  expect_null(sol@misc$solver.dir)
  expect_false(dir.exists(file.path(sol@path, "solver")))
})

test_that("an explicit solver.dir is external mode: honored verbatim, no record", {
  skip_if_no_solver()
  td <- gsub("[\\/]+", "/", file.path(tempdir(), "runs-suite", "ext-dir"))
  sc <- interpolate_model(sp_tech(c(50, 50, 50), optret = FALSE,
                                  name = "rnext"),
                          name = "rnext", path = rn_scen_path("rnext"))
  sol <- solve_scenario(sc, solver.dir = td)
  expect_true(isTRUE(sol@status$optimal))
  expect_identical(gsub("[\\/]+", "/", sol@misc$solver.dir), td)
  expect_false(dir.exists(file.path(sol@path, "runs")))
})

test_that("deprecated tmp.dir / tmp.del aliases still work, with a warning", {
  skip_if_no_solver()
  td <- gsub("[\\/]+", "/", file.path(tempdir(), "runs-suite", "ext-alias"))
  sc <- interpolate_model(sp_tech(c(50, 50, 50), optret = FALSE,
                                  name = "rnali"),
                          name = "rnali", path = rn_scen_path("rnali"))
  # rlang::warn(.frequency = "once") warns once per session; don't assert the
  # warning object itself, only that the alias functions
  sol <- suppressWarnings(solve_scenario(sc, tmp.dir = td))
  expect_true(isTRUE(sol@status$optimal))
  expect_identical(gsub("[\\/]+", "/", sol@misc$solver.dir), td)

  sc2 <- interpolate_model(sp_tech(c(40, 40, 40), optret = FALSE,
                                   name = "rnali2"),
                           name = "rnali2", path = rn_scen_path("rnali2"))
  sol2 <- suppressWarnings(solve_scenario(sc2, tmp.del = TRUE))
  expect_true(isTRUE(sol2@status$optimal))
  expect_false(dir.exists(file.path(sol2@path, "runs")))
})

test_that("save_scenario writes the manifest (default run), per-run modOut, registry", {
  skip_if_no_solver()
  reg_file <- file.path(tempdir(), "runs-suite", "reg.csv")
  old_reg <- set_registry_file(reg_file)
  on.exit(set_registry_file(old_reg), add = TRUE)

  sc <- interpolate_model(sp_tech(c(80, 80, 80), optret = FALSE,
                                  name = "rnsave"),
                          name = "rnsave", path = rn_scen_path("rnsave"))
  sol <- solve_scenario(sc)
  saved <- save_scenario(sol, verbose = FALSE)

  # layout marker and manifest
  expect_identical(readLines(file.path(saved@path, "layout"), warn = FALSE)[1],
                   "3")
  mf <- yaml::read_yaml(file.path(saved@path, "scenario.yml"))
  expect_identical(mf$layout, 3L)
  expect_identical(mf$name, "rnsave")
  expect_identical(mf$default, saved@misc$run)  # default: <run-id>, flat

  # solution store lives under the run (flat), not at the top level
  run_modout <- file.path(saved@path, "runs", saved@misc$run,
                          "modOut", "variables")
  expect_true(dir.exists(run_modout))
  expect_true(dir.exists(file.path(run_modout, "vObjective", "data")))
  expect_false(dir.exists(file.path(saved@path, "modOut", "variables")))

  # registry rows: the scenario and its run (flat run id)
  reg <- load_registry(reg_file)
  expect_identical(find_in_registry(reg, type = "scenario", name = "rnsave") |>
                     nrow(), 1L)
  run_rows <- find_in_registry(reg, type = "run", parent = "rnsave")
  expect_identical(nrow(run_rows), 1L)
  expect_identical(run_rows$name, saved@misc$run)

  # the saved dir loads back
  back <- load_scenario(saved@path, env = NULL, verbose = FALSE)
  expect_s4_class(back, "scenario")
  expect_identical(back@name, "rnsave")
  expect_identical(back@misc$run, saved@misc$run)
})

test_that("save_scenario on an ondisk-interpolated scenario is not a no-op", {
  skip_if_no_solver()
  sc <- interpolate_model(sp_tech(c(60, 60, 60), optret = FALSE,
                                  name = "rnod"),
                          name = "rnod", path = rn_scen_path("rnod"),
                          ondisk = TRUE)
  sol <- solve_scenario(sc)
  saved <- save_scenario(sol, verbose = FALSE)
  # the trap was: early return -> no scen.RData -> load_scenario fails
  expect_true(file.exists(file.path(saved@path, "scen.RData")))
  expect_true(file.exists(file.path(saved@path, "scenario.yml")))
  back <- load_scenario(saved@path, env = NULL, verbose = FALSE)
  expect_s4_class(back, "scenario")
})

test_that("interim S2/S3-era trees (runs/default/<solve>/script) are readable", {
  skip_if_no_solver()
  # simulate the retired shape from a fresh solve
  sc <- interpolate_model(sp_tech(c(30, 30, 30), optret = FALSE,
                                  name = "rnint"),
                          name = "rnint", path = rn_scen_path("rnint"))
  sol <- solve_scenario(sc)
  lbl <- sol@misc$run
  # transform: runs/<lbl>/{solver,run.yml} -> runs/default/<lbl>/{script,run.yml}
  old_dir <- file.path(sol@path, "runs", lbl)
  new_dir <- file.path(sol@path, "runs", "default", lbl)
  dir.create(dirname(new_dir), recursive = TRUE)
  stopifnot(file.rename(old_dir, new_dir))
  stopifnot(file.rename(file.path(new_dir, "solver"),
                        file.path(new_dir, "script")))

  runs <- scenario_runs(sol)
  expect_identical(nrow(runs), 1L)
  expect_identical(runs$variant, "default")   # read as a variant of that name
  expect_identical(runs$run, paste0("default/", lbl))
  sol2 <- read_solution(sol, run = paste0("default/", lbl), echo = FALSE)
  expect_true(isTRUE(sol2@status$optimal))
})
