# Apache Arrow (IPC/feather) data exchange for the JuMP / Pyomo solvers: model
# data (input) and the solution (output) are exchanged as compressed Arrow files
# instead of RData/SQLite (in) and CSV (out), selected via the solver's
# export_format / import_format. The solve must be format-invariant.

.ax_find <- function(rel) {
  for (cand in c(rel, file.path("..", "..", rel))) if (file.exists(cand)) return(cand)
  NULL
}

test_that(".write/.read_exchange_table round-trips type-safely (feather + parquet)", {
  df <- data.frame(tech = rep(c("A", "B"), 50), region = "R1",
                   year = rep(2020:2024L, 20), slice = NA_character_,
                   value = runif(100), stringsAsFactors = FALSE)
  d <- file.path(tempdir(), "axrt"); dir.create(d, showWarnings = FALSE)
  for (fmt in c("feather", "parquet")) {
    p <- .write_exchange_table(df, file.path(d, paste0("t_", fmt)), format = fmt)
    expect_true(file.exists(p))
    back <- .read_exchange_table(p)
    expect_equal(back, df, ignore_attr = TRUE)
    expect_true(is.integer(back$year))           # integer year preserved
    expect_true(is.character(back$slice))         # all-NA char survives (CSV loses it)
  }
})

test_that("arrow exchange solves identically to the legacy format (Julia + Pyomo)", {
  tm <- .ax_find("data-raw/testing-models.R")
  so <- .ax_find("data-raw/solver_options.R")
  skip_if(is.null(tm) || is.null(so), "data-raw/ builders not available")
  source(tm, local = TRUE)
  so_env <- new.env(); sys.source(so, envir = so_env)
  solver_options <- so_env$solver_options
  mod <- tm_core()

  obj <- function(s) tryCatch(getData(s, "vObjective", merge = TRUE)$value[1],
                              error = function(e) NA_real_)
  solve1 <- function(sv, tag) {
    td <- file.path(tempdir(), paste0("ax_", tag)); unlink(td, recursive = TRUE)
    on.exit(unlink(td, recursive = TRUE), add = TRUE)
    suppressWarnings(suppressMessages(
      solve_mod(mod, name = tag, solver = sv, tmp.dir = td, force = TRUE)))
  }

  skip_if(is.null(get_glpk_path()) || !nzchar(get_glpk_path()), "GLPK not configured")
  g <- obj(solve1(solver_options$glpk, "g"))
  expect_false(is.na(g))

  # JuMP: input feather (Arrow.jl) + solution feather. FAILS without the arrow path.
  if (!is.null(get_julia_path()) && nzchar(get_julia_path())) {
    sj <- solver_options$julia_highs
    sj$export_format <- "feather"; sj$import_format <- "feather"
    expect_equal(obj(solve1(sj, "jl")), g, tolerance = 1e-6)
  }
  # Pyomo: input feather (pyarrow) + solution feather.
  if (!is.null(get_python_path()) && nzchar(get_python_path())) {
    sp <- solver_options$pyomo_cbc
    sp$export_format <- "feather"; sp$import_format <- "feather"
    expect_equal(obj(solve1(sp, "py")), g, tolerance = 1e-6)
  }
})
