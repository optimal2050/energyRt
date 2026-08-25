# Levcost mini-models must be side-effect free on the project layout:
# scratch scenarios anchor under tempdir()/energyRt-levcost/ instead of the
# scenarios store, solves are transient (solver dirs deleted, R/solve.R), and
# refresh_registry() skips dot-prefixed scenario dirs.

.lcx_tech <- function() {
  newTechnology(
    "PPX", input = list(comm = "GAS"), output = list(comm = "ELC"),
    ceff = data.frame(comm = c("GAS", "ELC"), cinp2use = c(0.55, NA),
                      cact2cout = c(NA, 1)),
    invcost = data.frame(invcost = 800),
    fixom = data.frame(fixom = 20), varom = data.frame(varom = 2),
    af = data.frame(af.up = 0.85),
    vintage = data.frame(olife = 25L), cap2act = 1)
}

test_that("levcost(method = 'solve') leaves the scenarios store untouched", {
  skip_if_no_solver()
  root <- file.path(tempdir(), "lc-scratch-proj")
  scen_root <- file.path(root, "scenarios")
  dir.create(scen_root, recursive = TRUE, showWarnings = FALSE)
  old <- set_scenarios_path(scen_root)
  on.exit(set_scenarios_path(old), add = TRUE)

  before <- list.files(scen_root, recursive = TRUE, all.files = TRUE)
  lc <- suppressMessages(suppressWarnings(
    levcost(.lcx_tech(), fuel_costs = c(GAS = 15), discount = 0.06,
            base_year = 2025, method = "solve", verbose = FALSE)))
  after <- list.files(scen_root, recursive = TRUE, all.files = TRUE)

  expect_s3_class(lc, "levcost")
  expect_true(is.finite(as.numeric(lc$levcost_npv)))
  expect_identical(after, before)
})

test_that("levcost storage/trade mini-solves leave the store untouched", {
  skip_if_no_solver()
  root <- file.path(tempdir(), "lc-scratch-proj2")
  scen_root <- file.path(root, "scenarios")
  dir.create(scen_root, recursive = TRUE, showWarnings = FALSE)
  old <- set_scenarios_path(scen_root)
  on.exit(set_scenarios_path(old), add = TRUE)

  stg <- newStorage(
    "BAT", commodity = "ELC", region = "R1",
    storage = list(comm = "ELC"),
    invcost = data.frame(stg.invcost = 300),
    duration = 1, vintage = data.frame(olife = 15L))
  before <- list.files(scen_root, recursive = TRUE, all.files = TRUE)
  lc <- suppressMessages(suppressWarnings(
    levcost(stg, cycles = 365, price = 10, discount = 0.05,
            horizon = newHorizon(period = 2020L),
            method = "solve", verbose = FALSE)))
  after <- list.files(scen_root, recursive = TRUE, all.files = TRUE)
  expect_s3_class(lc, "levcost")
  expect_identical(after, before)
})

test_that("refresh_registry skips dot-prefixed scenario dirs", {
  root <- file.path(tempdir(), paste0("lc-reg", as.integer(runif(1, 1, 1e6))))
  scen_root <- file.path(root, get_scenarios_path())
  reg_file <- file.path(root, "energyRt_registry.csv")

  dir.create(file.path(scen_root, "REAL"), recursive = TRUE)
  writeLines("scenario", file.path(scen_root, "REAL", "class"))
  dir.create(file.path(scen_root, ".lc_scratch"), recursive = TRUE)
  writeLines("scenario", file.path(scen_root, ".lc_scratch", "class"))

  reg <- refresh_registry(root = root, file = reg_file, write = FALSE)
  expect_identical(find_registry(reg, type = "scenario")$name, "REAL")
  unlink(root, recursive = TRUE)
})
