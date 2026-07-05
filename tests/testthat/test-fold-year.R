# Folding the `year` dimension collapses parameters that are constant across
# milestone years (e.g. pWeather when a single weather year is repeated across a
# multi-year horizon) to one wildcard row -- a large saving for weather-heavy
# multi-year models. This MUST be solution-invariant.
#
# The JuMP fix (R/fold_artificial.R) has two parts, both year/JuMP specific:
#   1. `.fold_member_literal` emits the year wildcard as the bare integer `0`
#      (JuMP keys the year slot of its parameter Dicts numerically; a quoted "0"
#      never matches and silently returns the default).
#   2. `apply_fold_artificial` does NOT add the wildcard to the looped `year` set
#      for JuMP (its gate conditions hard-index year params, e.g.
#      `ordYear[(yp)] for yp in year`, so a spurious `0` -> KeyError).
# GLPK (numeric set, defaulting access) and Pyomo (string keys, `.get` defaults)
# are unaffected and were already correct.

.fy_find <- function(rel) {
  for (cand in c(rel, file.path("..", "..", rel))) {
    if (file.exists(cand)) return(cand)
  }
  NULL
}

test_that("year-fold is solution-invariant across GLPK / JuMP / Pyomo", {
  tm_file <- .fy_find("data-raw/testing-models.R")
  so_file <- .fy_find("data-raw/solver_options.R")
  skip_if(is.null(tm_file) || is.null(so_file),
          "data-raw/ builders not available (installed package)")
  source(tm_file, local = TRUE)
  so_env <- new.env(); sys.source(so_file, envir = so_env)
  solver_options <- so_env$solver_options
  mod <- tm_weather()

  # Sanity: multi-year horizon + pWeather constant across years (so year folds).
  base <- suppressWarnings(suppressMessages(
    interp_mod(mod, name = "fy_base", ondisk = FALSE, fold = FALSE)))
  yrs <- sort(unique(as.data.frame(get_data_slot(base@modInp@parameters$year))$year))
  expect_gt(length(yrs), 1)

  obj <- function(s) tryCatch(getData(s, "vObjective", merge = TRUE)$value[1],
                              error = function(e) NA_real_)
  solve_fb <- function(solver, fold, tag) {
    td <- file.path(tempdir(), paste0("fy_", tag)); unlink(td, recursive = TRUE)
    on.exit(unlink(td, recursive = TRUE), add = TRUE)
    s <- tryCatch(suppressWarnings(suppressMessages(
      solve_mod(mod, name = tag, solver = solver, fold = fold,
                tmp.dir = td, force = TRUE))),
      error = function(e) NULL)
    obj(s)
  }
  FOLD <- c("region", "slice", "year")

  # --- GLPK anchor (required) ------------------------------------------------ #
  skip_if(is.null(get_glpk_path()) || !nzchar(get_glpk_path()),
          "GLPK path not configured")
  g_uf <- solve_fb(solver_options$glpk, FALSE, "g_uf")
  g_fo <- solve_fb(solver_options$glpk, FOLD,  "g_fo")
  expect_false(is.na(g_uf))
  expect_equal(g_fo, g_uf, tolerance = 1e-6)

  # --- JuMP (the case the fix targets) --------------------------------------- #
  if (!is.null(get_julia_path()) && nzchar(get_julia_path())) {
    j_uf <- solve_fb(solver_options$julia_highs, FALSE, "j_uf")
    j_fo <- solve_fb(solver_options$julia_highs, FOLD,  "j_fo")
    expect_false(is.na(j_uf))
    expect_equal(j_fo, j_uf, tolerance = 1e-6)  # FAILS pre-fix (KeyError), PASSES post-fix
    expect_equal(j_uf, g_uf, tolerance = 1e-6)  # cross-backend sanity
  }

  # --- Pyomo (already correct; verify it stays so) --------------------------- #
  if (!is.null(get_python_path()) && nzchar(get_python_path())) {
    p_uf <- solve_fb(solver_options$pyomo_cbc, FALSE, "p_uf")
    p_fo <- solve_fb(solver_options$pyomo_cbc, FOLD,  "p_fo")
    expect_false(is.na(p_uf))
    expect_equal(p_fo, p_uf, tolerance = 1e-6)
  }
})

test_that("JuMP year wildcard: clean `year` set + bare integer 0 in lookups", {
  # Solver-free structural check of the two-part fix.
  tm_file <- .fy_find("data-raw/testing-models.R")
  skip_if(is.null(tm_file), "data-raw/ builders not available")
  source(tm_file, local = TRUE)
  mod <- tm_weather()

  sc <- suppressWarnings(suppressMessages(
    interp_mod(mod, name = "fy_struct", ondisk = FALSE,
               fold = c("region", "slice", "year"))))
  sc <- energyRt:::.finalize_interp(sc)
  sc2 <- apply_fold_artificial(sc, backends = "JuMP")

  # (2) the `year` set the JuMP loops iterate must NOT contain the wildcard 0.
  yset <- as.data.frame(get_data_slot(sc2@modInp@parameters$year))$year
  expect_false(0 %in% yset)

  # pWeather DATA must carry the wildcard 0 (so the Dict key exists).
  pw_data <- as.data.frame(get_data_slot(sc2@modInp@parameters$pWeather))
  expect_true(0 %in% pw_data$year)

  # (1) JuMP pWeather lookups index the bare integer 0, never the string "0".
  code <- sc2@settings@sourceCode[["JuMP"]]
  pw <- grep("pWeather", code, value = TRUE)
  expect_true(length(pw) > 0)
  expect_true(any(grepl("pWeather[^)]*,0,", pw)))   # ...,0,... (unquoted)
  expect_false(any(grepl('pWeather[^)]*"0"', pw)))  # never ..."0"...
})
