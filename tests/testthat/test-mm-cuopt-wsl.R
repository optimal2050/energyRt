# =============================================================================
# lang = "mm", mm_engine = "cuopt_wsl": NVIDIA cuOpt on a local GPU via WSL
# =============================================================================
#
# The multimod matrix is solved by `cuopt_cli` inside WSL; the `.sol` it writes
# is decoded as on the cloud route. The command is built by a pure function
# (tested without a GPU); the solve itself runs only where WSL and cuOpt are
# reachable (ENERGYRT_WSL_USER / ENERGYRT_CUOPT_CLI locate a non-default install).

test_that("Windows paths map to WSL's /mnt/<drive>/", {
  expect_identical(.wsl_path("C:/Users/x/model.mps"), "/mnt/c/Users/x/model.mps")
  expect_identical(.wsl_path("D:/a b/c.sol"), "/mnt/d/a b/c.sol")
  expect_identical(.wsl_path("/already/posix"), "/already/posix")
})

test_that("without a user or path set, WSL's default user and cuopt_cli on PATH", {
  withr::local_envvar(ENERGYRT_WSL_USER = NA, ENERGYRT_CUOPT_CLI = NA)
  a <- .mm_cuopt_wsl_args("C:/r/model.mps", "C:/r/model.sol", solver_options$mm_cuopt_wsl)
  expect_identical(a[1:2], c("-e", "cuopt_cli"))
  expect_identical(a[3:5], c("/mnt/c/r/model.mps", "--solution-file", "/mnt/c/r/model.sol"))
  expect_identical(a[6:9], c("--method", "1", "--time-limit", "7200"))
  x <- .mm_cuopt_wsl_args("C:/r/m.mps", "C:/r/m.sol", solver_options$mm_cuopt_wsl_crossover)
  expect_identical(tail(x, 2), c("--crossover", "1"))
})

test_that("the environment, then the preset, locate the install", {
  withr::local_envvar(ENERGYRT_WSL_USER = "envuser", ENERGYRT_CUOPT_CLI = "/env/cuopt_cli")
  s <- solver_options$mm_cuopt_wsl
  expect_identical(.mm_cuopt_wsl_args("C:/r/m.mps", "C:/r/m.sol", s)[1:4],
                   c("-u", "envuser", "-e", "/env/cuopt_cli"))
  s$wsl_user <- "me"; s$cuopt_cli <- "/usr/local/bin/cuopt_cli"; s$gpu_time_limit <- NULL
  y <- .mm_cuopt_wsl_args("C:/r/m.mps", "C:/r/m.sol", s)
  expect_identical(y[1:4], c("-u", "me", "-e", "/usr/local/bin/cuopt_cli"))
  expect_false("--time-limit" %in% y)
})

skip_if_no_cuopt_wsl <- function() {
  skip_on_cran()
  skip_if_not_installed("multimod")
  if (.Platform$OS.type != "windows") skip("WSL route: Windows only")
  s <- solver_options$mm_cuopt_wsl
  ok <- tryCatch(suppressWarnings(system2("wsl", c(.wsl_user_args(s), "-e", .cuopt_cli_path(s), "--version"),
                                          stdout = FALSE, stderr = FALSE)) == 0,
                 error = function(e) FALSE)
  if (!isTRUE(ok)) skip("cuopt_cli not reachable through WSL")
}

# DEM 100 met by a supply at 2 and an import at 3 capped at 40:
# supply 100 x 2 = 200 (the import is dearer and never used)
test_that("cuOpt through WSL solves a small model to the HiGHS objective", {
  skip_if_no_cuopt_wsl()
  cal <- newCalendar(timetable = make_timetable(struct = list(ANNUAL = "ANNUAL")), name = "cw_cal")
  mod <- newModel("cw",
    repo = newRepository("cw_repo",
      newCommodity("ELC", timeframe = "ANNUAL"),
      newDemand("DEM", commodity = "ELC", demand = data.frame(demand = 100)),
      newSupply("SUP", commodity = "ELC", supply = data.frame(cost = 2)),
      newImport("IMP", commodity = "ELC", import = data.frame(price = 3, imp.up = 40))),
    calendar = cal, region = "R1", horizon = newHorizon(2025), discount = 0)
  scen <- suppressMessages(suppressWarnings(interpolate_model(mod, name = "cw")))
  scen <- .fork_solve(scen, solver_options$mm_cuopt_wsl)
  expect_equal(.fork_objective(scen), 200, tolerance = 1e-3)
  sup <- as.data.frame(getData(scen, "vSupOut", merge = TRUE))
  expect_equal(sum(sup$value), 100, tolerance = 1e-3)
})
