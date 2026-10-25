# =============================================================================
# backend = "mm": the model solved through an MPS matrix by multimod::solve_mps()
# =============================================================================
#
# The preset is mapped to solve_mps()'s solver, place and options by a pure
# function (tested without a solver). Running the solvers is multimod's and is
# tested there; here the small model goes through the routes this machine can
# reach.

test_that("each matrix preset maps to its solver and place", {
  m <- function(n) .mm_solve_args(solver_options[[n]], "C:/r", "s")[c("solver", "where")]
  expect_identical(m("mm_highs"), list(solver = "highs", where = "local"))
  expect_identical(m("mm_julia_highs_barrier"), list(solver = "julia_highs", where = "local"))
  expect_identical(m("mm_cuopt_wsl"), list(solver = "cuopt", where = "wsl"))
  expect_identical(m("mm_cuopt_cloud"), list(solver = "cuopt", where = "remote"))
  expect_identical(m("pyomo_cuopt_cloud"), list(solver = "cuopt", where = "remote"))
})

test_that("cuOpt options and the WSL install come from the preset, then ENERGYRT_*", {
  withr::local_envvar(ENERGYRT_WSL_USER = NA, ENERGYRT_CUOPT_CLI = NA)
  a <- .mm_solve_args(solver_options$mm_cuopt_wsl_crossover, "C:/r")
  expect_identical(a$control, list(method = 1L, time_limit = 7200,
                                   options = "--crossover 1"))
  expect_identical(a$sol_file, file.path("C:/r", "model.sol"))
  withr::local_envvar(ENERGYRT_WSL_USER = "envuser", ENERGYRT_CUOPT_CLI = "/env/cuopt_cli")
  a <- .mm_solve_args(solver_options$mm_cuopt_wsl, "C:/r")
  expect_identical(a$control$wsl_user, "envuser")
  expect_identical(a$control$cuopt_cli, "/env/cuopt_cli")
  s <- solver_options$mm_cuopt_wsl
  s$wsl_user <- "me"
  expect_identical(.mm_solve_args(s, "C:/r")$control$wsl_user, "me")
})

test_that("a cloud preset carries the transport settings and a compressed solution", {
  s <- solver_options$mm_cuopt_cloud
  s$cloud_namespace <- "org"
  s$cloud_flavor <- "h200"
  a <- .mm_solve_args(s, "C:/r", "BY25")
  expect_identical(a$remote$namespace, "org")
  expect_identical(a$remote$flavor, "h200")
  expect_identical(a$remote$provider, "hf_jobs")
  expect_identical(a$remote$name, "energyrt-BY25")
  expect_identical(a$remote$handle_file, file.path("C:/r", "cloud_job.yml"))
  expect_identical(a$sol_file, file.path("C:/r", "model.sol.gz"))
  expect_false("time_limit" %in% names(a$control))
})

test_that("settings stored by earlier versions map to the same solver and place", {
  old_cloud <- list(name = "mmcloud", lang = "mm", backend = "mmcloud", gpu_method = 1L)
  expect_identical(.mm_solve_args(old_cloud, "C:/r")[c("solver", "where")],
                   list(solver = "cuopt", where = "remote"))
  old_wsl <- list(name = "mm_cuopt_wsl", lang = "mm", backend = "mm", mm_engine = "cuopt_wsl")
  expect_identical(.mm_solve_args(old_wsl, "C:/r")[c("solver", "where")],
                   list(solver = "cuopt", where = "wsl"))
  old_julia <- list(name = "x", lang = "mm", backend = "mm", mm_engine = "julia")
  expect_identical(.mm_solve_args(old_julia, "C:/r")$solver, "julia_highs")
})

test_that("a renamed preset stops and names its replacement", {
  expect_error(.check_renamed_preset(solver_options$mmcloud),
               "'mmcloud' was renamed to 'mm_cuopt_cloud'")
  expect_error(.check_renamed_preset(solver_options$mmcloud_pyomo),
               "'mmcloud_pyomo' was renamed to 'pyomo_cuopt_cloud'")
  expect_silent(.check_renamed_preset(solver_options$mm_cuopt_cloud))
})

test_that("hf_* fields are refused with their new names", {
  expect_error(.reject_legacy_solver_fields(list(hf_flavor = "h200")),
               "hf_flavor -> cloud_flavor")
})

test_that("the output templates give every variable's dimensions in order", {
  d <- .template_dim_names(.modelCode[["PYOMOConcreteOutput"]])
  expect_identical(d$dim[d$symbol == "vTechCap"], c("tech", "region", "year"))
  expect_identical(d$dim[d$symbol == "vTradeIr"],
                   c("trade", "comm", "src", "dst", "year", "timeslice"))
  expect_false("vObjective" %in% d$symbol)   # a scalar: no dimensions
  expect_true(all(tapply(d$position, d$symbol, function(p) identical(p, seq_along(p)))))
  expect_error(.template_dim_names(c('f = open("output/vX.csv", "w")', 'g()')),
               "Unexpected output template")
  # the JuMP template gives the same dimensions
  j <- .template_dim_names(.modelCode[["JuMPOutput"]])
  common <- intersect(unique(d$symbol), unique(j$symbol))
  expect_gt(length(common), 80)
  key <- function(x) paste(x$symbol, x$position, x$dim)
  expect_setequal(key(d[d$symbol %in% common, ]), key(j[j$symbol %in% common, ]))
})

test_that("each script writer leaves its matrix and index files", {
  expect_identical(.matrix_writer(list(lang = "JuMP")), "julia")
  expect_identical(.matrix_writer(list(lang = "PYOMO")), "pyomo")
  expect_identical(.matrix_writer_files("julia"),
                   c("model.mps", "model_col_index.arrow", "model_row_index.arrow"))
  expect_match(solver_options$julia_cuopt_cloud$inc3, 'write_to_file(model, "model.mps")', fixed = TRUE)
  expect_identical(.mm_solve_args(solver_options$julia_cuopt_cloud, "C:/r")$where, "remote")
})

test_that("the Pyomo matrix preset writes the index tables beside the matrix", {
  hook <- solver_options$pyomo_cuopt_cloud$inc4
  expect_match(hook, "model_col_index.parquet", fixed = TRUE)
  expect_match(hook, "model_row_index.parquet", fixed = TRUE)
  expect_match(hook, "'c_e_' + _lab + '_'", fixed = TRUE)
})

test_that("the definitions substituted at build time come from the preset", {
  skip_if_not("substitute" %in% names(formals(multimod::write_mps)))
  m <- list(equations = list(eqTechActSng = 1, eqTechAfUp = 1))
  expect_identical(.mm_substitute(list(), m), "eqTechActSng")
  m$equations$eqTechAfFx <- 1
  expect_identical(.mm_substitute(list(), m), c("eqTechAfFx", "eqTechActSng"))
  expect_identical(.mm_substitute(list(substitute = character()), m), character())
  expect_identical(.mm_substitute(list(substitute = "eqTechAfFx"), m), "eqTechAfFx")
})

test_that("highs_options must be a named list", {
  s <- solver_options$mm_highs
  s$highs_options <- list("ipm")
  expect_error(.mm_solve_args(s, "C:/r"), "named list of HiGHS options")
})

# DEM 100 met by a supply at 2 and an import at 3 capped at 40:
# supply 100 x 2 = 200 (the import is dearer and never used)
small_model <- function() {
  cal <- newCalendar(timetable = make_timetable(struct = list(ANNUAL = "ANNUAL")), name = "sm_cal")
  newModel("sm",
    repo = newRepository("sm_repo",
      newCommodity("ELC", timeframe = "ANNUAL"),
      newDemand("DEM", commodity = "ELC", demand = data.frame(demand = 100)),
      newSupply("SUP", commodity = "ELC", supply = data.frame(cost = 2)),
      newImport("IMP", commodity = "ELC", import = data.frame(price = 3, imp.up = 40))),
    calendar = cal, region = "R1", horizon = newHorizon(2025), discount = 0)
}

test_that("the small model solves to the same objective through HiGHS", {
  skip_on_cran()
  skip_if_not_installed("multimod")
  skip_if_not_installed("highs")
  scen <- suppressMessages(suppressWarnings(interpolate_model(small_model(), name = "sm")))
  scen <- .fork_solve(scen, solver_options$mm_highs)
  expect_equal(.fork_objective(scen), 200, tolerance = 1e-6)
})

skip_if_no_cuopt_wsl <- function() {
  skip_on_cran()
  skip_if_not_installed("multimod")
  if (.Platform$OS.type != "windows") skip("WSL route: Windows only")
  a <- .mm_solve_args(solver_options$mm_cuopt_wsl, tempdir())$control
  user <- if (is.null(a$wsl_user)) character(0) else c("-u", a$wsl_user)
  ok <- tryCatch(suppressWarnings(system2("wsl", c(user, "-e", a$cuopt_cli %||% "cuopt_cli",
                                                   "--version"),
                                          stdout = FALSE, stderr = FALSE)) == 0,
                 error = function(e) FALSE)
  if (!isTRUE(ok)) skip("cuopt_cli not reachable through WSL")
}

test_that("cuOpt through WSL solves the small model to the HiGHS objective", {
  skip_if_no_cuopt_wsl()
  scen <- suppressMessages(suppressWarnings(interpolate_model(small_model(), name = "sm")))
  scen <- .fork_solve(scen, solver_options$mm_cuopt_wsl)
  expect_equal(.fork_objective(scen), 200, tolerance = 1e-3)
  sup <- as.data.frame(getData(scen, "vSupOut", merge = TRUE))
  expect_equal(sum(sup$value), 100, tolerance = 1e-3)
})
