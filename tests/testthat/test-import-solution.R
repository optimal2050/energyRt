# Choosing a solver attempt and making it stick
# (scenario_solutions() / import_solution(), R/import_solution.R).
#
# The properties that would break silently:
#   * run.yml's `objective` is NA for exactly the runs that were never
#     imported, so the table that is supposed to help you choose is blank
#     where it matters -- the peek is the whole point;
#   * listing must write NOTHING, output/ and run.yml included;
#   * a read that produced no variables must never be saved: an empty modOut/
#     flips the run to "imported" and lets the solution's only copy be
#     cleaned up;
#   * importing must not touch output/.

# Covers the R API: scenario_solutions(), import_solution(), solved on glpk.
# NOT an `@covers` tag -- see the note in test-getdata-run.R.

is_root <- function(...) {
  gsub("[\\/]+", "/", file.path(tempdir(), "import-solution-suite", ...))
}

is_local <- function(env = parent.frame()) {
  old_sp <- set_scenarios_path(is_root("scenarios"))
  old_mp <- set_models_path(is_root("models"))
  old_rf <- set_registry_file(is_root("reg.csv"))
  expr <- bquote({
    set_scenarios_path(.(old_sp)); set_models_path(.(old_mp))
    set_registry_file(.(old_rf))
  })
  do.call(on.exit, list(expr, add = TRUE), envir = env)
  invisible(NULL)
}

# A scenario solved but never imported: run.yml records `finished` with no
# objective, and output/ is the only copy of the solution.
is_unimported <- function(name) {
  mod <- sp_tech(c(100, 100, 100), optret = FALSE, name = "is")
  sc <- interpolate_model(mod, name = name, path = is_root("scenarios", name))
  s <- suppressMessages(solve_scenario(sc, solver = solver_options$glpk,
                                       read.solution = FALSE, echo = FALSE))
  sv <- suppressMessages(save_scenario(s, verbose = FALSE))
  suppressMessages(load_scenario(sv@path, env = NULL, verbose = FALSE))
}

is_outdir <- function(scen, run) {
  d <- .run_dir(scen, .parse_run_id(run, scen)$variant,
                .parse_run_id(run, scen)$solve)
  .art_output_dir(.run_solver_dir(d), d)
}

is_snap <- function(d) {
  ff <- sort(list.files(d, recursive = TRUE, all.files = TRUE, no.. = TRUE))
  data.frame(f = ff, size = file.size(file.path(d, ff)),
             stringsAsFactors = FALSE)
}

test_that("the objective of an un-imported run is recovered from output/", {
  skip_if_no_solver()
  is_local()
  s <- is_unimported("is_peek")

  # the record cannot help: it is written from the in-memory solution
  expect_true(is.na(scenario_runs(s)$objective[1]))

  t <- scenario_solutions(s)
  expect_identical(nrow(t), 1L)
  expect_identical(t$status, "finished")        # what the driver recorded
  expect_identical(t$solver_status, "optimal")  # what the solver said
  expect_equal(t$objective, 300)
  expect_identical(t$objective_src, "output")
  expect_false(t$imported)
  expect_true(t$has_output)
  expect_gt(t$output_mb, 0)
})

test_that("a recorded objective is used as-is, and peek = FALSE skips output/", {
  skip_if_no_solver()
  is_local()
  mod <- sp_tech(c(100, 100, 100), optret = FALSE, name = "is")
  sc <- interpolate_model(mod, name = "is_rec", path = is_root("scenarios", "is_rec"))
  s <- suppressMessages(solve_scenario(sc, solver = solver_options$glpk,
                                       echo = FALSE))
  t <- scenario_solutions(s)
  expect_identical(t$objective_src, "record")
  expect_equal(t$objective, 300)

  t2 <- scenario_solutions(s, peek = FALSE)
  expect_true(is.na(t2$solver_status))
})

test_that("listing writes nothing", {
  skip_if_no_solver()
  is_local()
  s <- is_unimported("is_ro")
  out <- is_outdir(s, "glpk")
  before_out <- is_snap(out)
  ry <- fp(.run_dir(s, "", "glpk"), "run.yml")
  before_yml <- c(file.size(ry), as.numeric(file.mtime(ry)))

  invisible(scenario_solutions(s))

  expect_identical(is_snap(out), before_out)
  expect_identical(c(file.size(ry), as.numeric(file.mtime(ry))), before_yml)
  expect_false(dir.exists(fp(.run_dir(s, "", "glpk"), "modOut")))
})

test_that("import_solution imports, saves, and leaves output/ alone", {
  skip_if_no_solver()
  is_local()
  s <- is_unimported("is_imp")
  out <- is_outdir(s, "glpk")
  before <- is_snap(out)

  imp <- suppressMessages(import_solution(s, "glpk"))
  expect_equal(get_variable(imp, "vObjective")$value[1], 300)

  t <- scenario_solutions(imp)
  expect_true(t$imported)
  expect_identical(t$objective_src, "record")  # back-filled on import
  expect_identical(is_snap(out), before)       # untouched

  # and it survives a reload, which is the point of the verb
  back <- suppressMessages(load_scenario(imp@path, env = NULL, verbose = FALSE))
  expect_gt(nrow(as.data.frame(getData(back, "vTechOut", merge = TRUE))), 0L)
})

test_that("a read that yields no variables is refused, not saved", {
  skip_if_no_solver()
  is_local()
  s <- is_unimported("is_guard")
  out <- is_outdir(s, "glpk")
  # make the dump unreadable the way a truncated transfer would
  unlink(fp(out, "variable_list.csv"))

  # Either guard may speak first -- read_solution() now refuses an unreadable
  # run outright, and import_solution() still checks that a read produced
  # variables before saving. What must hold is the OUTCOME.
  expect_error(suppressMessages(import_solution(s, "glpk")),
               "cannot be read|Nothing was imported")
  # the guard's reason for existing: no empty store was written, so the run
  # is still "not imported" and output/ is still protected
  expect_false(dir.exists(fp(.run_dir(s, "", "glpk"), "modOut", "variables")))
  expect_false(scenario_solutions(s)$imported)
})

test_that("the solver identity is restored, never the command line", {
  skip_if_no_solver()
  is_local()
  s <- is_unimported("is_solv")
  s@settings@solver$name <- "wrong"
  s@settings@solver$cmdline <- "C:/Users/someone/bin/glpsol --lp"

  imp <- suppressMessages(import_solution(s, "glpk", save = FALSE))
  expect_identical(imp@settings@solver$name, "glpk")
  expect_null(imp@settings@solver$cmdline)

  s2 <- is_unimported("is_solv2")
  s2@settings@solver$name <- "wrong"
  imp2 <- suppressMessages(import_solution(s2, "glpk", restore_solver = FALSE,
                                           save = FALSE))
  expect_identical(imp2@settings@solver$name, "wrong")
})

test_that("an unknown run errors with the available runs", {
  skip_if_no_solver()
  is_local()
  s <- is_unimported("is_bad")
  expect_error(import_solution(s, "nope"), "Available runs")
  expect_error(import_solution(s), "required")
})
