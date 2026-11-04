# Detached solving (`wait = FALSE`) and the run-state machinery
# (R/solve.R, R/run_state.R).
#
# Two claims are worth pinning. First, that a detached solve changes nothing
# but timing: the objective collected afterwards must equal the one a blocking
# solve produces, or the feature is a different answer rather than the same
# answer sooner. Second, that a run still being written is protected — the
# permissive behaviour was not merely untidy, it could serve a half-written
# output/ as a solution, overwrite a live run's record, or delete its working
# files during a routine cleanup.
#
# Liveness is tested through fabricated records rather than real crashed
# processes: killing a solver mid-flight is not reproducible, while the record
# is exactly what the code reads.

ds_root <- function(...) {
  gsub("[\\/]+", "/", file.path(tempdir(), "detached-suite", ...))
}

ds_local <- function(tag, env = parent.frame()) {
  unlink(ds_root(tag), recursive = TRUE)
  old <- list(
    set_models_path(ds_root(tag, "models")),
    set_scenarios_path(ds_root(tag, "scenarios")),
    set_repositories_path(ds_root(tag, "repositories")),
    set_registry_file(ds_root(tag, "reg.csv")))
  expr <- bquote({
    set_models_path(.(old[[1]]))
    set_scenarios_path(.(old[[2]]))
    set_repositories_path(.(old[[3]]))
    set_registry_file(.(old[[4]]))
  })
  do.call(on.exit, list(expr, add = TRUE), envir = env)
  invisible(NULL)
}

# a solvable one-technology scenario; `pr_model()` is helper-param-refs.R's
ds_scen <- function(name) {
  mod <- pr_model("dsm")
  suppressMessages(save_model(mod, verbose = FALSE))
  s <- suppressMessages(interpolate_model(mod, name = name))
  suppressMessages(save_scenario(s, verbose = FALSE))
  s
}

ds_solve <- function(s, ...) {
  suppressMessages(solve_scenario(s, solver = solver_options$glpk,
                                  echo = FALSE, ...))
}

ds_rec <- function(s, run) {
  yaml::read_yaml(fp(s@path, "runs", run, "run.yml"))
}

ds_set <- function(s, run, ...) {
  ry <- fp(s@path, "runs", run, "run.yml")
  r <- yaml::read_yaml(ry)
  new <- list(...)
  for (k in names(new)) r[[k]] <- new[[k]]
  yaml::write_yaml(r, ry)
  invisible(r)
}

ds_now_stamp <- function() {
  energyRt:::.run_stamp_fmt(ps::ps_create_time(ps::ps_handle()))
}

ds_live <- function(s, run) {
  ds_set(s, run, status = "running", pid = Sys.getpid(),
         pid_started = ds_now_stamp(),
         hostname = unname(Sys.info()["nodename"]))
}

# --- stage 1: wait = FALSE -------------------------------------------------- #

test_that("a detached solve leaves the run open for its own process to close", {
  ds_local("detach")
  s <- ds_scen("DSDET")
  s <- ds_solve(s, wait = FALSE)

  rr <- scenario_runs(s)
  expect_identical(nrow(rr), 1L)
  # the record must NOT be stamped: the solver was only just launched, and
  # "finished" here would mark a solve complete at the moment it started
  expect_identical(rr$status[1], "running")
  expect_true(is.na(rr$objective[1]))
})

test_that("a detached solve returns the same answer, collected later", {
  ds_local("same")
  blocking <- ds_solve(ds_scen("DSBLOCK"))
  obj_b <- getData(blocking, "vObjective", merge = TRUE)$value
  expect_false(is.na(obj_b))

  d <- ds_solve(ds_scen("DSSAME"), wait = FALSE)
  run <- scenario_runs(d)$run[1]
  vl <- fp(d@path, "runs", run, "output", "variable_list.csv")
  for (i in 1:400) {               # the fixture solves in well under a second
    if (file.exists(vl)) break
    Sys.sleep(0.05)
  }
  expect_true(file.exists(vl))

  d <- suppressMessages(import_solution(d, run = run, verbose = FALSE))
  expect_equal(getData(d, "vObjective", merge = TRUE)$value, obj_b)
  expect_identical(scenario_runs(d)$status[1], "solved")
})

test_that("wait = FALSE refuses what it cannot honour", {
  ds_local("refuse")
  s <- ds_scen("DSREF")
  expect_error(ds_solve(s, wait = FALSE, read.solution = TRUE),
               "cannot be honoured")
  expect_error(ds_solve(s, wait = FALSE, transient = TRUE),
               "deletes the solver directory")
  expect_error(ds_solve(s, wait = FALSE, solver.dir = ds_root("refuse", "ext")),
               "cannot be tracked")
})

test_that("invisible no longer tracks wait", {
  ds_local("invis")
  s <- ds_solve(ds_scen("DSINV"), wait = TRUE)
  # it used to be forced to `wait`, tying "don't block" to "show the console"
  expect_true(isTRUE(s@settings@solver$invisible))
})

# --- stage 2: pid state ------------------------------------------------------ #

test_that("the run record identifies the process that wrote it", {
  ds_local("stamp")
  s <- ds_solve(ds_scen("DSPID"))
  rec <- ds_rec(s, scenario_runs(s)$run[1])
  expect_identical(as.integer(rec$pid), Sys.getpid())
  # a whole-second UTC string, not a number: yaml::write_yaml() writes a double
  # with eight significant digits, which lost 38 seconds and read a live
  # process as dead
  expect_match(as.character(rec$pid_started),
               "^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}Z$")
})

test_that("the pid state separates alive, dead and not-checkable", {
  ds_local("live")
  skip_if_not_installed("ps")
  s <- ds_solve(ds_scen("DSLIVE"))
  run <- scenario_runs(s)$run[1]
  host <- unname(Sys.info()["nodename"])

  expect_identical(energyRt:::.get_pid_state(ds_live(s, run)), "alive")

  # a pid that cannot be running
  r <- ds_set(s, run, status = "running", pid = 999999L,
              pid_started = ds_now_stamp(), hostname = host)
  expect_identical(energyRt:::.get_pid_state(r), "dead")

  # pid REUSE: the right pid, a different process. This is what the start time
  # is recorded for -- on the pid alone this case reads alive.
  r <- ds_set(s, run, status = "running", pid = Sys.getpid(),
              pid_started = "2020-01-01T00:00:00Z", hostname = host)
  expect_identical(energyRt:::.get_pid_state(r), "dead")

  # another machine: not checkable from here
  r <- ds_set(s, run, status = "running", pid = Sys.getpid(),
              pid_started = ds_now_stamp(), hostname = "SOME-OTHER-HOST")
  expect_identical(energyRt:::.get_pid_state(r), "unknown")
  expect_match(energyRt:::.run_unknown_reason(r), "SOME-OTHER-HOST")

  # a stamp that cannot be read is not evidence of being alive
  r <- ds_set(s, run, status = "running", pid = Sys.getpid(),
              pid_started = "rubbish", hostname = host)
  expect_identical(energyRt:::.get_pid_state(r), "unknown")

  # a record that already closed itself is never alive
  r <- ds_set(s, run, status = "solved")
  expect_identical(energyRt:::.get_pid_state(r), "dead")
})

test_that("a dead running record is a crash, which no record can self-report", {
  ds_local("crash")
  skip_if_not_installed("ps")
  s <- ds_solve(ds_scen("DSCRASH"))
  run <- scenario_runs(s)$run[1]
  r <- ds_set(s, run, status = "running", pid = 999999L,
              pid_started = ds_now_stamp(),
              hostname = unname(Sys.info()["nodename"]))
  expect_identical(energyRt:::.get_run_state(r, has_solution = FALSE), "crashed")
  expect_identical(energyRt:::.get_run_state(r, has_solution = TRUE), "finished")

  # a crash leaves no objective and no complete output/: strip both, since
  # this fixture solved first in order to have a run at all
  ds_set(s, run, objective = NULL)
  unlink(fp(s@path, "runs", run, "output"), recursive = TRUE)
  st <- get_status(s)
  expect_identical(st$crashed, 1L)
  expect_identical(st$running, 0L)
  expect_identical(st$solved, 0L)
})

# --- stage 2: the guards ---------------------------------------------------- #

test_that("a run still being written refuses to be read", {
  ds_local("g_read")
  skip_if_not_installed("ps")
  s <- ds_solve(ds_scen("DSGREAD"))
  run <- scenario_runs(s)$run[1]
  ds_live(s, run)
  # the output has to look unfinished too: a complete output/ is collectable
  # whatever the record says, which is what makes a detached run readable
  unlink(fp(s@path, "runs", run, "output"), recursive = TRUE)
  expect_error(suppressMessages(read_solution(s, run = run)), "still running")
})

test_that("a run still being written refuses to be dropped", {
  ds_local("g_drop")
  skip_if_not_installed("ps")
  s <- ds_solve(ds_scen("DSGDROP"))
  run <- scenario_runs(s)$run[1]
  ds_live(s, run)
  unlink(fp(s@path, "runs", run, "output"), recursive = TRUE)
  expect_error(drop_scenario_run(s, run), "still running")
  # force is the documented override
  expect_message(drop_scenario_run(s, run, force = TRUE), "Dropped run")
})

test_that("a run still being written refuses an output seal", {
  ds_local("g_seal")
  skip_if_not_installed("ps")
  s <- ds_solve(ds_scen("DSGSEAL"))
  run <- scenario_runs(s)$run[1]
  ds_live(s, run)
  unlink(fp(s@path, "runs", run, "output"), recursive = TRUE)
  expect_error(suppressMessages(seal_scenario(s, inputs = TRUE,
                                              outputs = TRUE,
                                              verbose = FALSE)),
               "still running")
  # and the inputs were not sealed on the way out
  expect_false(get_status(s)$sealed)
})

test_that("a re-solve into a live run is refused, not overwritten", {
  ds_local("g_solve")
  skip_if_not_installed("ps")
  s <- ds_solve(ds_scen("DSGSOLVE"))
  run <- scenario_runs(s)$run[1]
  ds_live(s, run)
  # run.conflict defaults to "overwrite" and tests only whether run.yml
  # exists, never its status, so this used to clobber a live record
  expect_error(ds_solve(s, force = TRUE), "still running")
  # a different label is the way forward and is not blocked
  s2 <- ds_solve(s, force = TRUE, run = "second")
  expect_true("second" %in% scenario_runs(s2)$run)
})

test_that("a cleanup does not select a run that is still being written", {
  ds_local("cleanup")
  skip_if_not_installed("ps")
  s <- ds_solve(ds_scen("DSCLEAN"))
  run <- scenario_runs(s)$run[1]
  ds_live(s, run)

  # .art_suggest() called ANY running record stale without checking the process, and
  # drop_solver_outputs(runs = NULL) unlinks exactly the suggested rows -- so
  # this could delete the generated model and logs of a solve in flight
  a <- suppressMessages(drop_solver_outputs(s, dry_run = TRUE, verbose = FALSE))
  expect_false(any(grepl("running", a$suggest %||% "")))
})

test_that("a run on another host refuses, naming it", {
  ds_local("foreign")
  s <- ds_solve(ds_scen("DSFGN"))
  run <- scenario_runs(s)$run[1]
  unlink(fp(s@path, "runs", run, "output"), recursive = TRUE)
  ds_set(s, run, status = "running", pid = Sys.getpid(),
         pid_started = ds_now_stamp(), hostname = "SOME-OTHER-HOST")
  expect_error(suppressMessages(read_solution(s, run = run)),
               "SOME-OTHER-HOST")
})

test_that("a finished output is collectable whatever the record says", {
  ds_local("complete")
  s <- ds_solve(ds_scen("DSCOMP"))
  run <- scenario_runs(s)$run[1]
  obj <- getData(s, "vObjective", merge = TRUE)$value
  # the detached case: nothing closed the record, because the process that
  # could is the solver and it knows nothing about run.yml. A complete
  # output/ is what settles it -- otherwise a detached run could never be
  # collected at all.
  ds_set(s, run, status = "running", detached = TRUE, pid = 999999L,
         pid_started = ds_now_stamp(), hostname = "SOME-OTHER-HOST")
  s2 <- suppressMessages(read_solution(s, run = run))
  expect_equal(getData(s2, "vObjective", merge = TRUE)$value, obj)
})

test_that("clear_run releases the guard without touching the output", {
  ds_local("clear")
  skip_if_not_installed("ps")
  s <- ds_solve(ds_scen("DSCLR"))
  run <- scenario_runs(s)$run[1]
  out <- fp(s@path, "runs", run, "output")
  files_before <- length(list.files(out, recursive = TRUE))
  expect_gt(files_before, 0L)

  # an incomplete output is what makes the guard the deciding factor; a
  # foreign host is the case the pid state cannot resolve on its own
  unlink(fp(out, "variable_list.csv"))
  ds_set(s, run, status = "running", pid = Sys.getpid(),
         pid_started = ds_now_stamp(), hostname = "SOME-OTHER-HOST")
  expect_error(energyRt:::.run_busy_guard(s, run, "TEST"), "still running")

  invisible(suppressMessages(clear_run(s, run, verbose = FALSE)))
  expect_identical(ds_rec(s, run)$status, "abandoned")
  expect_silent(energyRt:::.run_busy_guard(s, run, "TEST"))
  # nothing but the record changed
  expect_identical(length(list.files(out, recursive = TRUE)),
                   files_before - 1L)
})

test_that("clear_run says so rather than acting on a run it need not touch", {
  ds_local("clear2")
  s <- ds_solve(ds_scen("DSCLR2"))
  run <- scenario_runs(s)$run[1]
  expect_message(clear_run(s, run), "not marked running")
  expect_identical(ds_rec(s, run)$status, "solved")
  expect_error(clear_run(s, "no-such-run"), "No run record")
})

test_that("clear_run warns when the process it releases is alive", {
  ds_local("clear3")
  skip_if_not_installed("ps")
  s <- ds_solve(ds_scen("DSCLR3"))
  run <- scenario_runs(s)$run[1]
  ds_live(s, run)
  expect_warning(suppressMessages(clear_run(s, run, verbose = FALSE)),
                 "LIVE process")
})


# A run is addressed by its directory, so the cases `clear_run()` exists for --
# a record from another machine, or a store in a layout this version no longer
# loads -- can be cleared without a loadable scenario.
test_that("clear_run() takes a path, and mirrors the status into the registry", {
  root <- file.path(tempdir(), paste0("cr-", as.integer(runif(1, 1e5, 9e5))))
  dir.create(file.path(root, "scenarios"), recursive = TRUE)
  # only the registry file is needed, and it is restored from the value read
  # back rather than from open_project()'s return, which is empty on a fresh
  # session -- so the project cannot leak into the test files that follow
  orf <- get_registry_file()
  on.exit(set_registry_file(orf), add = TRUE)
  set_registry_file(file.path(root, "energyRt_registry.csv"))
  save_registry(newRegistry())

  # no scenario.yml, so nothing here can be loaded as a scenario
  scen_dir <- file.path(root, "scenarios", "LEGACY-mod-cal-Y2025")
  run_dir <- file.path(scen_dir, "runs", "v1", "glpk")
  dir.create(run_dir, recursive = TRUE)
  yaml::write_yaml(list(solver = "glpk", status = "running",
                        hostname = "SOME-OTHER-BOX",
                        started = "2026-09-09T10:00:00Z"),
                   file.path(run_dir, "run.yml"))
  expect_error(load_scenario("LEGACY-mod-cal-Y2025"))

  expect_silent(clear_run(scen_dir, "v1/glpk", verbose = FALSE))
  rec <- yaml::read_yaml(file.path(run_dir, "run.yml"))
  expect_identical(rec$status, "abandoned")
  expect_true(nzchar(rec$abandoned_at))
  # nothing else is rewritten
  expect_identical(rec$solver, "glpk")
  expect_identical(rec$started, "2026-09-09T10:00:00Z")

  reg <- load_registry()
  r <- reg[reg$type == "run", ]
  expect_equal(nrow(r), 1L)
  expect_identical(r$name, "glpk")
  expect_identical(r$variant, "v1")
  expect_identical(r$status, "abandoned")
  expect_identical(r$parent, "LEGACY-mod-cal-Y2025")

  expect_error(clear_run(42, "x"), "must be a scenario")
  expect_error(clear_run(file.path(root, "nope"), "x"), "no directory at")
})
