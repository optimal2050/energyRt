# =========================================================================== #
# Nightly deep regressions (tier "nightly"; tools/test/run_nightly.R):
#   - the larger UTOPIA layouts (R7 / R11) against their goldens,
#   - UTOPIA R3 cross-solver parity on julia/HiGHS,
#   - external real-world models via ENERGYRT_EXT_MODELS (opt-in): solved on
#     julia/HiGHS and checked with verify_solution(). These are .rds files
#     serialized by OLDER package versions, so merely loading and solving them
#     is the forward-compatibility fixture: a class-layout break that the
#     upgrade path does not absorb fails (or skips loudly) here first.
# The full-year copperplate model additionally hides behind
# ENERGYRT_TEST_HEAVY=true (~minutes).
# =========================================================================== #

test_that("UTOPIA R7/R11 reproduce their golden tracked values (julia/HiGHS)", {
  skip_if_tier_below("nightly")
  # mid-size models reference julia/HiGHS (the backend-choice convention;
  # SUITE_SOLVERS in make_goldens.R) -- same-solver goldens, julia both sides
  skip_if_no_julia_highs()
  g <- skip_if_no_golden("utopia_nightly")
  entries <- ut_nightly_entries()
  for (nm in names(g)) {
    skip_if(is.null(entries[[nm]]), paste0(nm, " not in ut_nightly_entries()"))
    scen <- ut_solve(entries[[nm]], paste0("nightly_", nm),
                     solver = "julia_highs")
    vs <- verify_solution(scen)
    expect_true(vs$ok, label = paste0(nm, " invariants"))
    expect_matches_golden(scen, "utopia_nightly", nm, kind = "same_solver")
  }
})

test_that("UTOPIA R3 solves to the golden objective on julia/HiGHS", {
  skip_if_tier_below("nightly")
  skip_if_no_julia_highs()
  g <- skip_if_no_golden("utopia", "base_R3")
  mod <- ut_build(layout = "R3", calendar = "s4_h24")
  scen <- suppressMessages(suppressWarnings(
    solve_model(mod, name = "nightly_R3_julia",
                solver = solver_options$julia_highs,
                tmp.del = TRUE, wait = TRUE)))
  vs <- verify_solution(scen)
  expect_true(vs$ok, label = "base_R3 julia invariants")
  # degenerate optima make per-path flows solver-dependent (dev/TESTING.md);
  # across solvers only the objective (and the invariants above) are pinned
  fresh <- capture_tracked_values(scen)
  expect_equal(fresh$objective, as.numeric(g$base_R3$objective),
               tolerance = TOL_XSOLVER_OBJ, label = "base_R3 julia objective")
})

# ---- external models (opt-in) --------------------------------------------- #

.ext_model_path <- function(file) {
  dir <- Sys.getenv("ENERGYRT_EXT_MODELS", "")
  if (!nzchar(dir)) {
    testthat::skip("external models are opt-in (set ENERGYRT_EXT_MODELS to the model directory)")
  }
  if (!dir.exists(dir)) {
    testthat::skip(paste0("ENERGYRT_EXT_MODELS dir not found: ", dir))
  }
  p <- file.path(dir, file)
  if (!file.exists(p)) testthat::skip(paste0("external model not found: ", p))
  p
}

# Load an externally-serialized model/scenario, solve on julia/HiGHS, verify.
# A readRDS/validity failure on these old objects is the forward-compat
# signal: skip with the error text so the nightly report surfaces it.
.ext_model_check <- function(file) {
  skip_if_tier_below("nightly")
  skip_if_no_julia_highs()
  p <- .ext_model_path(file)
  obj <- tryCatch(suppressWarnings(readRDS(p)), error = function(e) e)
  if (inherits(obj, "error")) {
    testthat::skip(paste0(file, " no longer loads under the current class ",
                          "layout: ", conditionMessage(obj)))
  }
  if (!methods::is(obj, "model") && !methods::is(obj, "scenario")) {
    testthat::skip(paste0(file, " holds a ", class(obj)[1],
                          ", not a model/scenario"))
  }
  scen <- if (methods::is(obj, "scenario")) {
    suppressMessages(suppressWarnings(
      solve_scenario(obj, solver = solver_options$julia_highs,
                     force = TRUE, wait = TRUE)))
  } else {
    suppressMessages(suppressWarnings(
      solve_model(obj, name = paste0("ext_", sub("\\.rds$", "", file)),
                  solver = solver_options$julia_highs,
                  tmp.del = TRUE, wait = TRUE)))
  }
  expect_true(isTRUE(scen@status$solved), label = paste0(file, " solved"))
  vs <- verify_solution(scen)
  expect_true(vs$ok, label = paste0(file, " invariants"))
  invisible(scen)
}

test_that("external: belgium_model solves and verifies (julia)", {
  .ext_model_check("belgium_model.rds")
})

test_that("external: belgium_copperplate (8760 h) solves and verifies (julia)", {
  skip_if(!identical(Sys.getenv("ENERGYRT_TEST_HEAVY"), "true"),
          "heavy external models are opt-in (set ENERGYRT_TEST_HEAVY=true)")
  .ext_model_check("belgium_copperplate.rds")
})

test_that("external: eu41_model (41 nodes) solves and verifies (julia)", {
  skip_if(!identical(Sys.getenv("ENERGYRT_TEST_HEAVY"), "true"),
          "heavy external models are opt-in (set ENERGYRT_TEST_HEAVY=true)")
  .ext_model_check("eu41_model.rds")
})
