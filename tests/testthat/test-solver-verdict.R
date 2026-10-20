# Did the solver conclude there is NO solution, or did it never run?
#
# `read_solution()` used to raise "Solution files not found" for both. An
# infeasible model is an answer a caller may want to test for; a missing
# toolchain is not. `.solver_verdict()` (R/read.R) separates them, and the
# separation only works if it stays silent on a healthy run -- a false positive
# would turn every successful solve into a warning and an empty result.
#
# The backends report differently, which is why there are two sources:
#   log.csv      -- Julia writes a termination status, Pyomo and GAMS a numeric
#                   "solution status"
#   solver.log   -- glpsol's own terminal copy, because MathProg aborts on a
#                   failed `solve` and glpsol exits 0 either way
#
# Solver-free: the helper reads files, so the files are written by hand.

# Covers the R API: .solver_verdict().
# NOT an `@covers` tag -- see the note in test-getdata-run.R.

.sv <- function(...) getFromNamespace(".solver_verdict", "energyRt")(...)

# a run directory with whatever output/ files the case needs
.sv_dir <- function(log_csv = NULL, solver_log = NULL) {
  d <- file.path(tempfile("sv_"), "")
  dir.create(file.path(d, "output"), recursive = TRUE, showWarnings = FALSE)
  if (!is.null(log_csv))
    writeLines(log_csv, file.path(d, "output", "log.csv"))
  if (!is.null(solver_log))
    writeLines(solver_log, file.path(d, "output", "solver.log"))
  d
}

test_that("a healthy run yields NO verdict", {
  # the guard that matters most: silence on success
  ok_glpk <- c("GLPK Simplex Optimizer 5.0", "*   246: obj = 1.5e+03 inf = 0.0e+00 (0)",
               "OPTIMAL LP SOLUTION FOUND", "Time used:   0.0 secs")
  ok_csv  <- c("parameter,value,time",
               '"solution status",1,"2026-10-06 18:28:18Z"')
  expect_null(.sv(.sv_dir(solver_log = ok_glpk)))
  expect_null(.sv(.sv_dir(log_csv = ok_csv)))
  expect_null(.sv(.sv_dir(log_csv = ok_csv, solver_log = ok_glpk)))
})

test_that("no evidence at all yields no verdict, so the caller still errors", {
  expect_null(.sv(.sv_dir()))
  expect_null(.sv(NULL))
  expect_null(.sv(""))
})

test_that("glpsol's own verdicts are recognised", {
  inf <- .sv(.sv_dir(solver_log = c("Constructing initial basis...",
                                    "LP HAS NO PRIMAL FEASIBLE SOLUTION")))
  expect_match(inf, "INFEASIBLE")
  expect_match(inf, "NO PRIMAL FEASIBLE SOLUTION")   # quotes the solver

  unb <- .sv(.sv_dir(solver_log = "LP HAS UNBOUNDED SOLUTION"))
  expect_match(unb, "UNBOUNDED")
})

test_that("log.csv statuses are recognised", {
  # Julia: an explicit no-primal-solution row, plus the termination status
  jl <- .sv(.sv_dir(log_csv = c("parameter,value,time",
                                '"termination status","INFEASIBLE","t"',
                                '"no primal solution","INFEASIBLE","t"')))
  expect_match(jl, "no primal solution")
  expect_match(jl, "INFEASIBLE")

  # Pyomo writes 0 when the solver status is not ok
  py <- .sv(.sv_dir(log_csv = c("parameter,value,time",
                                '"solution status",0,"t"')))
  expect_match(py, "solution status 0")

  # GAMS writes its Modelstat; 4 is INFEASIBLE
  gm <- .sv(.sv_dir(log_csv = c("parameter,value,time",
                                '"solution status",4,"t"')))
  expect_match(gm, "solution status 4")
})

test_that("a malformed log.csv is ignored rather than fatal", {
  # the helper runs on a failure path; it must not add a second failure
  expect_null(.sv(.sv_dir(log_csv = c("not,a,status,table", "1,2,3,4"))))
  expect_null(.sv(.sv_dir(log_csv = "")))
})
