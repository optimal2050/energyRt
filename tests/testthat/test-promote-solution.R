# Promoting a run's solution to the scenario level (R/promote_solution.R).
#
# Until a solution is promoted it lives only inside the run that produced it,
# and the shell points there -- so deleting `runs/` leaves every read failing
# with "On-disk data expected but not found". Promotion is what makes the run
# tree disposable, and the test of it is exactly that: delete runs/ and the
# scenario still works.
#
# The other properties that would break silently:
#   * `run.yml` is the only home of the solve's provenance, so it has to be
#     copied into the store before the run can be dropped;
#   * a variant's problem lives inside runs/, so its solution cannot be lifted
#     out on its own;
#   * upgrade_scenario_layout() migrates a top-level modOut/ INTO a run -- it
#     must not undo a deliberate promotion.

# @covers promote_solution import_solution drop_scenario_run depth=S backends=glpk

pr_root <- function(...) {
  gsub("[\\\\/]+", "/", file.path(tempdir(), "promote-suite", ...))
}

pr_local <- function(env = parent.frame()) {
  old_sp <- set_scenarios_path(pr_root("scenarios"))
  old_mp <- set_models_path(pr_root("models"))
  old_rf <- set_registry_file(pr_root("reg.csv"))
  expr <- bquote({
    set_scenarios_path(.(old_sp)); set_models_path(.(old_mp))
    set_registry_file(.(old_rf))
  })
  do.call(on.exit, list(expr, add = TRUE), envir = env)
  invisible(NULL)
}

pr_solved <- function(name, run = "glpk") {
  mod <- sp_tech(c(100, 100, 100), optret = FALSE, name = "pr")
  sc <- interpolate_model(mod, name = name, path = pr_root("scenarios", name))
  suppressMessages(save_scenario(suppressMessages(solve_scenario(
    sc, solver = solver_options$glpk, run = run, echo = FALSE)),
    verbose = FALSE))
}

test_that("a promoted solution survives deleting runs/ entirely", {
  skip_if_no_solver()
  pr_local()
  s <- pr_solved("pr_del")
  ref <- sum(as.data.frame(getData(s, "vTechOut", merge = TRUE))$value)
  p <- s@path

  s <- suppressMessages(promote_solution(s, verbose = FALSE))
  expect_true(dir.exists(fp(p, "modOut", "variables")))
  expect_identical(s@misc$run, "")

  # the whole point
  unlink(fp(p, "runs"), recursive = TRUE)
  expect_false(dir.exists(fp(p, "runs")))

  back <- suppressMessages(load_scenario(p, env = NULL, verbose = FALSE))
  d <- as.data.frame(getData(back, "vTechOut", merge = TRUE))
  expect_gt(nrow(d), 0L)
  expect_equal(sum(d$value), ref)
})

test_that("the solve's provenance travels with the store", {
  skip_if_no_solver()
  pr_local()
  s <- suppressMessages(promote_solution(pr_solved("pr_prov"), verbose = FALSE))
  unlink(fp(s@path, "runs"), recursive = TRUE)

  # run.yml is gone; everything below would be irrecoverable without this
  mf <- yaml::read_yaml(fp(s@path, "modOut", "modOut.yml"))
  expect_identical(mf$from_run, "glpk")
  expect_identical(mf$stage, "solved")
  expect_equal(as.numeric(mf$objective), 300)
  expect_true(nzchar(mf$solver$name))
  expect_true(nzchar(mf$solved$started))
  expect_gt(length(mf$variables), 0L)
})

test_that("a variant run is refused, with the reason", {
  skip_if_no_solver()
  pr_local()
  mod <- sp_tech(c(100, 100, 100), optret = FALSE, name = "pr")
  sc <- interpolate_model(mod, name = "pr_var", path = pr_root("scenarios", "pr_var"))
  s <- suppressMessages(save_scenario(suppressMessages(solve_scenario(
    sc, solver = solver_options$glpk, variant = "low", echo = FALSE)),
    verbose = FALSE))
  # a variant carries its own modInp, so lifting only its solution would pair
  # variant results with the base problem's parameters
  expect_error(promote_solution(s, verbose = FALSE), "own problem")
})

test_that("a run with no imported solution is refused", {
  skip_if_no_solver()
  pr_local()
  s <- pr_solved("pr_noimp")
  unlink(fp(.run_dir(s, "", "glpk"), "modOut"), recursive = TRUE)
  expect_error(promote_solution(s, run = "glpk", verbose = FALSE),
               "import_solution")
})

test_that("dropping the active run refuses while it is the only copy", {
  skip_if_no_solver()
  pr_local()
  s <- pr_solved("pr_drop")
  # before promotion this run IS the scenario's solution
  expect_error(drop_scenario_run(s, "glpk"), "only copy")

  s <- suppressMessages(promote_solution(s, verbose = FALSE))
  # after promotion nothing is active and the run is scratch
  expect_no_error(drop_scenario_run(s, "glpk"))
  expect_false(dir.exists(fp(s@path, "runs", "glpk")))
  expect_gt(nrow(as.data.frame(getData(s, "vTechOut", merge = TRUE))), 0L)
})

test_that("the upgrader leaves a promoted store where it is", {
  skip_if_no_solver()
  pr_local()
  s <- suppressMessages(promote_solution(pr_solved("pr_up"), verbose = FALSE))
  p <- s@path
  # the promoted shape looks exactly like layout 2 -- a top-level modOut/ with
  # no active run -- so only modOut.yml tells the upgrader not to migrate it
  suppressMessages(upgrade_scenario_layout(p, verbose = FALSE))
  expect_true(dir.exists(fp(p, "modOut", "variables")))
  expect_true(file.exists(fp(p, "modOut", "modOut.yml")))
  back <- suppressMessages(load_scenario(p, env = NULL, verbose = FALSE))
  expect_gt(nrow(as.data.frame(getData(back, "vTechOut", merge = TRUE))), 0L)
})

test_that("a failed promotion leaves the previous store intact", {
  skip_if_no_solver()
  pr_local()
  s <- suppressMessages(promote_solution(pr_solved("pr_fail"), verbose = FALSE))
  ref <- sum(as.data.frame(getData(s, "vTechOut", merge = TRUE))$value)

  # a source with no solution cannot promote; the good store must survive
  s@misc$run <- "nope"
  expect_error(promote_solution(s, verbose = FALSE))
  expect_true(file.exists(fp(s@path, "modOut", "modOut.yml")))
  back <- suppressMessages(load_scenario(s@path, env = NULL, verbose = FALSE))
  expect_equal(sum(as.data.frame(getData(back, "vTechOut", merge = TRUE))$value),
               ref)
})

test_that("scenario_artifacts lists the solution, and never as scratch", {
  skip_if_no_solver()
  pr_local()
  s <- suppressMessages(promote_solution(pr_solved("pr_art"), verbose = FALSE))
  a <- scenario_artifacts(s)
  row <- a[a$kind == "solution", ]
  expect_identical(nrow(row), 1L)
  expect_gt(row$solution_mb, 0)
  expect_identical(row$scratch_mb, 0)
  expect_identical(row$suggest, "")   # never a clean-up candidate

  # and the run cleaner does not touch it
  suppressMessages(drop_solver_outputs(s, dry_run = FALSE, verbose = FALSE))
  expect_true(dir.exists(fp(s@path, "modOut", "variables")))
})

test_that("re-promoting a different run replaces the store", {
  skip_if_no_solver()
  pr_local()
  s <- pr_solved("pr_re", run = "runA")
  s <- suppressMessages(save_scenario(suppressMessages(solve_scenario(
    interpolate_model(sp_tech(c(100, 100, 100), optret = FALSE, name = "pr"),
                      name = "pr_re", path = pr_root("scenarios", "pr_re")),
    solver = solver_options$glpk, run = "runB", echo = FALSE)),
    verbose = FALSE))

  s <- suppressMessages(promote_solution(s, run = "runA", verbose = FALSE))
  expect_identical(
    yaml::read_yaml(fp(s@path, "modOut", "modOut.yml"))$from_run, "runA")

  s <- suppressMessages(promote_solution(s, run = "runB", verbose = FALSE))
  mf <- yaml::read_yaml(fp(s@path, "modOut", "modOut.yml"))
  expect_identical(mf$from_run, "runB")
  # replaced, not merged: no staging or set-aside directory survives
  expect_false(dir.exists(paste0(fp(s@path, "modOut"), ".incoming")))
  expect_false(dir.exists(paste0(fp(s@path, "modOut"), ".prev")))
  expect_gt(nrow(as.data.frame(getData(s, "vTechOut", merge = TRUE))), 0L)
})

test_that("sharing scrubs the promoted manifest, not just run records", {
  skip_if_no_solver()
  pr_local()
  s <- suppressMessages(promote_solution(pr_solved("pr_share"), verbose = FALSE))
  out <- pr_root("shared")
  unlink(out, recursive = TRUE)
  res <- suppressMessages(prepare_for_sharing(s, path = out, verbose = FALSE))

  # modOut.yml copies the solve's host/user so they survive the run folder
  # being deleted -- which is exactly what would carry them past a scrub
  # aimed only at run.yml
  mf <- yaml::read_yaml(fp(out, "modOut", "modOut.yml"))
  expect_identical(as.character(mf$user %||% ""), "")
  expect_identical(as.character(mf$hostname %||% ""), "")
  expect_false(any(grepl("modOut[.]yml", res$remaining)))
  # and the provenance that is NOT identifying is untouched
  expect_true(nzchar(mf$from_run))
  expect_identical(mf$stage, "solved")
})

test_that("import_solution(promote = TRUE) chains both steps", {
  skip_if_no_solver()
  pr_local()
  s <- pr_solved("pr_chain")
  # start from an unimported run, the situation the pair exists for
  unlink(fp(.run_dir(s, "", "glpk"), "modOut"), recursive = TRUE)
  s2 <- suppressMessages(import_solution(s, "glpk", promote = TRUE,
                                         verbose = FALSE))
  expect_identical(s2@misc$run, "")
  expect_true(file.exists(fp(s2@path, "modOut", "modOut.yml")))
  expect_gt(nrow(as.data.frame(getData(s2, "vTechOut", merge = TRUE))), 0L)
})
