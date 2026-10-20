# upgrade_scenario_layout(): layout 2 -> 3 migration and interim-3 cleanup
# (R/paths_rebase.R). Fixtures built by transforming a CURRENT save into the
# old shapes, so the tests need no checked-in binary fixtures.

# Move a flat run's solver files into `dest`, leaving the run-level records
# (`run.yml`, `modOut/`) in place -- the reshaping every legacy-layout fixture
# below needs, since a run folder no longer has a `solver/` subdirectory to
# rename.
ul_move_solver <- function(run_dir, dest) {
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)
  keep <- c("run.yml", "modOut", basename(dest))
  mv <- setdiff(list.files(run_dir, all.files = TRUE, no.. = TRUE), keep)
  stopifnot(length(mv) > 0,
            all(file.rename(file.path(run_dir, mv), file.path(dest, mv))))
  invisible(dest)
}

ul_path <- function(name) {
  gsub("[\\/]+", "/", file.path(tempdir(), "upgrade-suite", name))
}

# solve + save, then reshape the folder into layout 2:
#   runs/<lbl>/solver -> script/<lbl>;  runs/<lbl>/modOut -> modOut;
#   drop runs/ + scenario.yml; layout marker "2"; scen.RData loses the run
#   markers and records the old-style misc$tmp.dir.
ul_make_layout2 <- function(name) {
  p <- ul_path(name)
  unlink(p, recursive = TRUE)
  sc <- interpolate_model(sp_tech(c(100, 100, 100), optret = FALSE,
                                  name = name),
                          name = name, path = p)
  sol <- solve_scenario(sc)
  saved <- suppressMessages(save_scenario(sol, verbose = FALSE))
  lbl <- saved@misc$run

  run_dir <- file.path(p, "runs", lbl)
  stopifnot(dir.create(file.path(p, "script", lbl), recursive = TRUE))
  stopifnot(file.rename(file.path(run_dir, "modOut"),
                        file.path(p, "modOut")))
  # A run folder is flat now: its solver files sit beside `run.yml`, where
  # layout 2 kept them under `script/<lbl>`.
  ul_move_solver(run_dir, file.path(p, "script", lbl))
  unlink(file.path(p, "runs"), recursive = TRUE)
  unlink(file.path(p, "scenario.yml"))
  write("2", file.path(p, "layout"), append = FALSE)

  # rewrite scen.RData the way an old version would have left it
  e <- new.env()
  nm <- load(file.path(p, "scen.RData"), envir = e)
  scen <- get(nm, envir = e)
  scen@misc$run <- NULL
  scen@misc$variant <- NULL
  scen@misc$solver.dir <- NULL
  scen@misc$tmp.dir <- gsub("[\\/]+", "/", file.path(p, "script", lbl))
  save(scen, file = file.path(p, "scen.RData"))
  list(path = p, label = lbl)
}

test_that("a layout-2 folder upgrades to flat runs/ with a backfilled record", {
  skip_if_no_solver()
  fx <- ul_make_layout2("ul2")

  up <- suppressWarnings(suppressMessages(
    upgrade_scenario_layout(fx$path, verbose = FALSE)))

  expect_identical(readLines(file.path(fx$path, "layout"), warn = FALSE)[1],
                   "3")
  run_dir <- file.path(fx$path, "runs", fx$label)
  expect_true(dir.exists(file.path(run_dir, "solver", "output")))
  expect_true(file.exists(file.path(run_dir, "run.yml")))
  rec <- yaml::read_yaml(file.path(run_dir, "run.yml"))
  expect_identical(rec$status, "legacy")
  expect_identical(rec$variant, "")
  expect_true(nzchar(rec$solver_name))
  # modOut moved under the run; no top-level copies remain
  expect_true(dir.exists(file.path(run_dir, "modOut", "variables")))
  expect_false(dir.exists(file.path(fx$path, "modOut")))
  expect_false(dir.exists(file.path(fx$path, "script")))
  # manifest names the migrated run as the default
  mf <- yaml::read_yaml(file.path(fx$path, "scenario.yml"))
  expect_identical(mf$default, fx$label)

  # the upgraded folder loads and reads data
  back <- suppressMessages(load_scenario(fx$path, env = NULL,
                                         verbose = FALSE))
  expect_identical(back@misc$run, fx$label)
  d <- suppressMessages(getData(back, "vTechStockCap", merge = TRUE))
  expect_gt(nrow(d), 0L)
  runs <- scenario_runs(back)
  expect_identical(nrow(runs), 1L)
  expect_identical(runs$status, "legacy")

  # idempotent
  up2 <- suppressWarnings(suppressMessages(
    upgrade_scenario_layout(fx$path, verbose = FALSE)))
  expect_s4_class(up2, "scenario")
  expect_identical(nrow(scenario_runs(up2)), 1L)
  unlink(fx$path, recursive = TRUE)
})

test_that("an interim runs/default/<solve>/script tree is flattened and renamed", {
  skip_if_no_solver()
  p <- ul_path("uli")
  unlink(p, recursive = TRUE)
  sc <- interpolate_model(sp_tech(c(60, 60, 60), optret = FALSE,
                                  name = "uli"),
                          name = "uli", path = p)
  sol <- solve_scenario(sc)
  saved <- suppressMessages(save_scenario(sol, verbose = FALSE))
  lbl <- saved@misc$run

  # reshape into the retired interim form
  old_dir <- file.path(p, "runs", lbl)
  new_dir <- file.path(p, "runs", "default", lbl)
  dir.create(dirname(new_dir), recursive = TRUE)
  stopifnot(file.rename(old_dir, new_dir))
  ul_move_solver(new_dir, file.path(new_dir, "script"))
  e <- new.env()
  nm <- load(file.path(p, "scen.RData"), envir = e)
  scen <- get(nm, envir = e)
  scen@misc$variant <- "default"
  save(scen, file = file.path(p, "scen.RData"))

  up <- suppressWarnings(suppressMessages(
    upgrade_scenario_layout(p, verbose = FALSE)))

  expect_true(dir.exists(file.path(p, "runs", lbl, "solver", "output")))
  expect_false(dir.exists(file.path(p, "runs", "default")))
  rec <- yaml::read_yaml(file.path(p, "runs", lbl, "run.yml"))
  expect_identical(rec$variant, "")
  runs <- scenario_runs(up)
  expect_identical(runs$run, lbl)
  expect_identical(runs$variant, "")
  unlink(p, recursive = TRUE)
})
