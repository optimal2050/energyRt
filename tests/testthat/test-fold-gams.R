# GAMS folding end to end: a DENSE folded scenario written for GAMS compiles and
# solves to the same objective as its unfolded twin, which is the objective
# every other backend returns on this fixture. Local GAMS is unlicensed here, so
# the solve goes through NEOS (opt-in: ENERGYRT_TEST_NEOS=true and NEOS_EMAIL
# set); the text-data route inlines sets.gms and data.gms into one job.

test_that("dense folded and unfolded solve identically on GAMS via NEOS", {
  skip_if_no_neos()
  skip_if(is.null(get_neos_email()), "NEOS_EMAIL not set")
  obj <- function(fold, tag) {
    scen <- interpolate_model(fold_model(), name = tag, fold = fold,
                              sparse = FALSE, verbose = FALSE)
    d <- withr::local_tempdir()
    s <- solve_scenario(scen, solver = solver_options$neos_gams_cplex,
                        solver.dir = d, force = TRUE)
    expect_true(verify_solution(s)$ok, label = paste0(tag, " invariants"))
    fold_objective(s)
  }
  fo <- obj(TRUE, "ng_fo")
  uf <- obj(FALSE, "ng_uf")
  expect_equal(fo, uf, tolerance = 1e-6)
  expect_equal(fo, 75.0342, tolerance = 1e-4)
})
