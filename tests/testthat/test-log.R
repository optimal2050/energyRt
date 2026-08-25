# Optional operation log (R/log.R): off by default; set_log_file() makes
# interpolate_model / solve_scenario / solve_myopic append one CSV line each;
# read_log() reads the sequence back. Fixtures from helper-stock-path.R.

lg_root <- function(...) {
  gsub("[\\/]+", "/", file.path(tempdir(), "log-suite", ...))
}

lg_local <- function(env = parent.frame()) {
  dir.create(lg_root(), recursive = TRUE, showWarnings = FALSE)
  old_lf <- set_log_file(lg_root("oplog.csv"))
  old_sp <- set_scenarios_path(lg_root("scenarios"))
  expr <- bquote({
    set_log_file(.(old_lf))
    set_scenarios_path(.(old_sp))
  })
  do.call(on.exit, list(expr, add = TRUE), envir = env)
  invisible(NULL)
}

test_that("logging is off by default and read_log returns an empty tibble", {
  expect_identical(get_log_file(), "")
  expect_false(energyRt:::.en_log("noop", "x"))
  d <- read_log()
  expect_s3_class(d, "tbl_df")
  expect_identical(nrow(d), 0L)
  expect_identical(names(d), energyRt:::.en_log_header)
})

test_that("interpolate and solve append parseable rows in sequence", {
  skip_if_no_solver()
  lg_local()
  unlink(lg_root("oplog.csv"))

  sc <- interpolate_model(sp_tech(c(50, 50, 50), optret = FALSE, name = "lg"),
                          name = "lg", path = lg_root("scenarios", "lg"))
  sol <- solve_scenario(sc, transient = TRUE)
  expect_true(isTRUE(sol@status$optimal))

  d <- read_log()
  expect_identical(nrow(d), 2L)
  expect_identical(d$op, c("interpolate", "solve"))
  expect_identical(d$object, c("lg", "lg"))
  expect_identical(d$status[2], "optimal")
  expect_true(all(is.finite(d$duration_sec)) && all(d$duration_sec >= 0))
  expect_true(inherits(d$timestamp, "POSIXct"))
  expect_match(d$details[1], "model=lg")
  expect_match(d$details[2], "objective=")
  expect_match(d$details[2], "transient=TRUE")

  # appends accumulate across calls
  solve_scenario(sol, transient = TRUE, force = TRUE)
  expect_identical(nrow(read_log()), 3L)
})

test_that("levcost mini-solves (dot-prefixed scratch) stay out of the log", {
  skip_if_no_solver()
  lg_local()
  unlink(lg_root("oplog.csv"))
  tech <- newTechnology(
    "LGT", input = list(comm = "GAS"), output = list(comm = "ELC"),
    ceff = data.frame(comm = "GAS", cinp2use = 0.55),
    invcost = data.frame(invcost = 800), fixom = data.frame(fixom = 20),
    vintage = data.frame(olife = 25L), cap2act = 1)
  suppressMessages(suppressWarnings(
    levcost(tech, fuel_costs = c(GAS = 15), discount = 0.06,
            base_year = 2025, method = "solve", cache = FALSE,
            verbose = FALSE)))
  expect_false(file.exists(lg_root("oplog.csv")))
})

test_that("an unwritable log path warns but never fails the operation", {
  lg_local()
  # a path whose parent cannot be created (file where a dir is needed)
  block <- lg_root("blocker")
  writeLines("x", block)
  set_log_file(file.path(block, "sub", "oplog.csv"))
  expect_warning(
    sc <- interpolate_model(sp_tech(c(50, 50, 50), optret = FALSE,
                                    name = "lgw"),
                            name = "lgw", path = lg_root("scenarios", "lgw")),
    "Operation log")
  expect_s4_class(sc, "scenario")
})
