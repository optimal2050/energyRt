# Path rebasing on load (R/paths_rebase.R + the get_lazy_data loud-error
# branch): a moved scenario folder must load and read data; the stale object
# whose paths point at the old location must fail loudly, not with 0 rows.

pr_path <- function(name) {
  gsub("[\\/]+", "/", file.path(tempdir(), "rebase-suite", name))
}

test_that("a moved scenario folder loads and reads data after the rebase", {
  skip_if_no_solver()
  p1 <- pr_path("pr1")
  p2 <- pr_path("pr1-moved")
  unlink(c(p1, p2), recursive = TRUE)

  sc <- interpolate_model(sp_tech(c(100, 100, 100), optret = FALSE,
                                  name = "pr1"),
                          name = "pr1", path = p1)
  sol <- solve_scenario(sc)
  saved <- suppressMessages(save_scenario(sol, verbose = FALSE))
  n_cap <- nrow(suppressMessages(getData(sol, "vTechStockCap", merge = TRUE)))
  expect_gt(n_cap, 0L)

  stopifnot(file.rename(p1, p2))

  back <- suppressMessages(load_scenario(p2, env = NULL, verbose = FALSE))
  expect_identical(gsub("[\\/]+", "/", back@path), p2)
  # parameters and the solution read from the NEW location
  d <- suppressMessages(getData(back, "vTechStockCap", merge = TRUE))
  expect_identical(nrow(d), n_cap)
  # the active run still resolves (solver dir is derived, never stored)
  expect_true(nzchar(back@misc$run))
  expect_true(dir.exists(file.path(p2, "runs", back@misc$run, "solver")))

  # the STALE object (paths at the old location) fails loudly, not silently
  expect_error(
    suppressMessages(getData(saved, "vTechStockCap", merge = TRUE)),
    "moved")
  unlink(p2, recursive = TRUE)
})

test_that("a bare read_solution on a loaded scenario uses the manifest default", {
  skip_if_no_solver()
  p <- pr_path("pr2")
  unlink(p, recursive = TRUE)
  sc <- interpolate_model(sp_tech(c(50, 50, 50), optret = FALSE,
                                  name = "pr2"),
                          name = "pr2", path = p)
  sol <- solve_scenario(sc)
  suppressMessages(save_scenario(sol, verbose = FALSE))

  back <- suppressMessages(load_scenario(p, env = NULL, verbose = FALSE))
  # simulate an object that lost its active-run marker
  back@misc$run <- NULL
  back@misc$variant <- NULL
  back2 <- read_solution(back, echo = FALSE)
  expect_true(isTRUE(back2@status$optimal))
  expect_true(nzchar(back2@misc$run))
  unlink(p, recursive = TRUE)
})
