# Switching between a scenario's runs (read_solution(run = )).
#
# Every switching path in the package goes through read_solution(): getScenario
# (run = ), load_scenarios(run = ), compare_scenarios(runs = ), getObject and
# report(run = ). It used to return the ORIGINAL object when a run's output/
# could not be read, so a failed switch silently served the previously active
# run's solution -- reachable simply by cleaning up a run that had been
# imported, or by sharing a scenario, since prepare_for_sharing() drops solver
# scratch.

# Covers the R API: read_solution(), drop_solver_outputs(), solved on glpk.
# NOT an `@covers` tag -- see the note in test-getdata-run.R.

rs_root <- function(...) {
  gsub("[\\/]+", "/", file.path(tempdir(), "run-switch-suite", ...))
}

rs_local <- function(env = parent.frame()) {
  old_sp <- set_scenarios_path(rs_root("scenarios"))
  old_mp <- set_models_path(rs_root("models"))
  old_rf <- set_registry_file(rs_root("reg.csv"))
  expr <- bquote({
    set_scenarios_path(.(old_sp)); set_models_path(.(old_mp))
    set_registry_file(.(old_rf))
  })
  do.call(on.exit, list(expr, add = TRUE), envir = env)
  invisible(NULL)
}

rs_two_runs <- function(name) {
  mod <- sp_tech(c(100, 100, 100), optret = FALSE, name = "rs")
  sc <- interpolate_model(mod, name = name, path = rs_root("scenarios", name))
  a <- suppressMessages(save_scenario(suppressMessages(solve_scenario(
    sc, solver = solver_options$glpk, run = "runA", echo = FALSE)),
    verbose = FALSE))
  b <- suppressMessages(save_scenario(suppressMessages(solve_scenario(
    sc, solver = solver_options$glpk, run = "runB", echo = FALSE)),
    verbose = FALSE))
  list(a = a, b = b)
}

test_that("a cleaned-up run reads back from its imported store", {
  skip_if_no_solver()
  rs_local()
  # drop_solver_outputs() removes output/ precisely BECAUSE a run was
  # imported. Before this, switching to such a run returned the PREVIOUSLY
  # ACTIVE run's solution, with no error -- the store it was cleared against
  # was never consulted.
  mod <- sp_tech(c(100, 100, 100), optret = FALSE, name = "ro")
  sc <- interpolate_model(mod, name = "ro_two", path = rs_root("scenarios", "ro_two"))
  a <- suppressMessages(save_scenario(suppressMessages(solve_scenario(
    sc, solver = solver_options$glpk, run = "runA", echo = FALSE)),
    verbose = FALSE))
  ref <- sum(as.data.frame(getData(a, "vTechOut", merge = TRUE))$value)
  b <- suppressMessages(save_scenario(suppressMessages(solve_scenario(
    sc, solver = solver_options$glpk, run = "runB", echo = FALSE)),
    verbose = FALSE))
  invisible(suppressMessages(
    drop_solver_outputs(b, runs = "runA", dry_run = FALSE, verbose = FALSE)))
  expect_false(dir.exists(fp(.run_dir(b, "", "runA"), "output")))

  x <- suppressMessages(read_solution(b, run = "runA", echo = FALSE))
  expect_identical(x@misc$run, "runA")          # not runB
  d <- as.data.frame(getData(x, "vTechOut", merge = TRUE))
  expect_gt(nrow(d), 0L)
  expect_equal(sum(d$value), ref)

})

test_that("a run with neither copy left says so, rather than falling back", {
  skip_if_no_solver()
  rs_local()
  mod <- sp_tech(c(100, 100, 100), optret = FALSE, name = "ro")
  sc <- interpolate_model(mod, name = "ro_gone", path = rs_root("scenarios", "ro_gone"))
  a <- suppressMessages(save_scenario(suppressMessages(solve_scenario(
    sc, solver = solver_options$glpk, run = "runA", echo = FALSE)),
    verbose = FALSE))
  b <- suppressMessages(save_scenario(suppressMessages(solve_scenario(
    sc, solver = solver_options$glpk, run = "runB", echo = FALSE)),
    verbose = FALSE))

  # strip runA of BOTH copies before reading, so the refusal is about the run
  # having no solution left rather than about a half-open one
  unlink(fp(.run_dir(b, "", "runA"), "output"), recursive = TRUE)
  unlink(fp(.run_dir(b, "", "runA"), "modOut"), recursive = TRUE)

  expect_error(suppressMessages(read_solution(b, run = "runA", echo = FALSE)),
               "cannot be read")
  # and the object is NOT silently left reporting runB's numbers as runA's
  expect_identical(b@misc$run, "runB")
})

