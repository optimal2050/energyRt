# =============================================================================
# lang = "mm": a preset's `highs_options` reach HiGHS
# =============================================================================
#
# The matrix route solves in process through multimod::solve_mps(); the
# preset's optional `highs_options` (a named list) is its `control`.

skip_if_no_multimod <- function() skip_if_not_installed("multimod")

# .solve_matrix() reads only scen@settings@solver and scen@name
setClass("mm_mock_settings", representation(solver = "list"), where = environment())
setClass("mm_mock_scen", representation(settings = "mm_mock_settings", name = "character"), where = environment())
.mm_scen <- function(solver) {
  methods::new("mm_mock_scen", settings = methods::new("mm_mock_settings", solver = solver), name = "mock")
}
.mm_arg <- function() {
  d <- tempfile("mm_opts_")
  dir.create(d)
  writeLines("", file.path(d, "model.mps"))
  list(solver.dir = d, echo = FALSE)
}

test_that("highs_options are passed to solve_mps() as its control", {
  skip_if_no_multimod()
  seen <- NULL
  local_mocked_bindings(
    solve_mps = function(file, sol_file = NULL, control = list(), ...) {
      seen <<- control
      stop("captured")
    },
    .package = "multimod")
  s <- solver_options$mm_highs
  s$highs_options <- list(presolve = "off", time_limit = 60)
  expect_error(.solve_matrix(.mm_arg(), .mm_scen(s)), "captured")
  expect_identical(seen, list(presolve = "off", time_limit = 60))
})

test_that("a preset without highs_options passes an empty control", {
  skip_if_no_multimod()
  seen <- NULL
  local_mocked_bindings(
    solve_mps = function(file, sol_file = NULL, control = list(), ...) {
      seen <<- control
      stop("captured")
    },
    .package = "multimod")
  expect_error(.solve_matrix(.mm_arg(), .mm_scen(solver_options$mm_highs)), "captured")
  expect_identical(seen, list())
})

test_that("unnamed highs_options are refused before the solve", {
  skip_if_no_multimod()
  s <- solver_options$mm_highs
  s$highs_options <- list("off")
  expect_error(.solve_matrix(.mm_arg(), .mm_scen(s)), "named list")
})
