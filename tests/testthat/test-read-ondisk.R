# read_solution(ondisk = ): streaming the solution straight into the run's
# modOut store instead of holding it in memory (R/read.R).
#
# The properties that would break silently:
#   * an IN-MEMORY scenario must behave exactly as before -- this is a hot
#     path and the default must not move;
#   * the container's `onDisk` bookkeeping must be recorded, not just the
#     path: every rebase is guarded on `length(get_ondisk_slots(x))`, so
#     without it a moved folder reads back as ZERO ROWS with no error;
#   * a store already written must not be walked again by save_scenario(),
#     which would record dim-0 bookkeeping over a complete store;
#   * streaming needs a run folder; an external solver.dir has none.

# @covers read_solution save_scenario depth=S backends=glpk

ro_root <- function(...) {
  gsub("[\\/]+", "/", file.path(tempdir(), "read-ondisk-suite", ...))
}

ro_local <- function(env = parent.frame()) {
  old_sp <- set_scenarios_path(ro_root("scenarios"))
  old_mp <- set_models_path(ro_root("models"))
  old_rf <- set_registry_file(ro_root("reg.csv"))
  expr <- bquote({
    set_scenarios_path(.(old_sp)); set_models_path(.(old_mp))
    set_registry_file(.(old_rf))
  })
  do.call(on.exit, list(expr, add = TRUE), envir = env)
  invisible(NULL)
}

ro_solve <- function(name, ondisk = FALSE) {
  mod <- sp_tech(c(100, 100, 100), optret = FALSE, name = "ro")
  sc <- interpolate_model(mod, name = name, path = ro_root("scenarios", name),
                          ondisk = ondisk)
  suppressMessages(solve_scenario(sc, solver = solver_options$glpk,
                                  echo = FALSE))
}

test_that("an in-memory scenario does not stream", {
  skip_if_no_solver()
  ro_local()
  sol <- ro_solve("ro_mem")
  expect_true(isInMemory(sol))
  expect_false(isOnDisk(sol@modOut))
  expect_gt(nrow(get_variable(sol, "vTechOut")), 0L)
})

test_that("an on-disk scenario streams into the run's modOut store", {
  skip_if_no_solver()
  ro_local()
  sol <- ro_solve("ro_disk", ondisk = TRUE)

  expect_true(isOnDisk(sol@modOut))
  # the bookkeeping, not just the path -- an empty one makes every rebase a
  # silent no-op
  expect_identical(get_ondisk_slots(sol@modOut), "variables")
  mp <- getObjPath(sol@modOut)
  expect_true(dir.exists(file.path(mp, "variables")))
  expect_identical(basename(dirname(mp)), "glpk")
  # and it reads back through the lazy path
  expect_gt(nrow(get_variable(sol, "vTechOut")), 0L)
})

test_that("streamed and in-memory reads give the same values", {
  skip_if_no_solver()
  ro_local()
  a <- as.data.frame(getData(ro_solve("ro_a"), "vTechOut", merge = TRUE))
  b <- as.data.frame(getData(ro_solve("ro_b", ondisk = TRUE), "vTechOut",
                             merge = TRUE))
  expect_identical(nrow(a), nrow(b))
  expect_equal(sum(a$value), sum(b$value))
})

test_that("a streamed solution survives a folder move", {
  skip_if_no_solver()
  ro_local()
  sol <- ro_solve("ro_move", ondisk = TRUE)
  before <- sum(as.data.frame(getData(sol, "vTechOut", merge = TRUE))$value)
  sv <- suppressMessages(save_scenario(sol, verbose = FALSE))

  moved <- ro_root("moved")
  unlink(moved, recursive = TRUE)
  expect_true(file.rename(sv@path, moved))
  back <- suppressMessages(load_scenario(moved, env = NULL, verbose = FALSE))
  d <- as.data.frame(getData(back, "vTechOut", merge = TRUE))
  # rows, not just "no error": the failure mode is silence
  expect_gt(nrow(d), 0L)
  expect_equal(sum(d$value), before)
})

test_that("save_scenario does not rewalk an already streamed store", {
  skip_if_no_solver()
  ro_local()
  sol <- ro_solve("ro_twice", ondisk = TRUE)
  mp <- getObjPath(sol@modOut)
  n_before <- length(list.files(mp, recursive = TRUE))
  sv <- suppressMessages(save_scenario(sol, verbose = FALSE))
  expect_identical(length(list.files(mp, recursive = TRUE)), n_before)
  expect_identical(get_ondisk_slots(sv@modOut), "variables")
  # still readable after the save
  expect_gt(nrow(as.data.frame(getData(sv, "vTechOut", merge = TRUE))), 0L)
})
