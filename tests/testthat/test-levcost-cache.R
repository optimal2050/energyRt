# On-disk levcost cache (R/levcost_cache.R): keyed on object content hash +
# normalized assumptions; artifacts live under the owner's levcost/ for saved
# objects and under the project-level get_levcost_cache_path() (temporary
# tier) for in-memory objects.

lcc_root <- function(...) {
  gsub("[\\/]+", "/", file.path(tempdir(), "lcc-suite", ...))
}

lcc_local <- function(env = parent.frame()) {
  old_lc <- set_levcost_cache_path(lcc_root("levcosts"))
  old_mp <- set_models_path(lcc_root("models"))
  old_rf <- set_registry_file(lcc_root("reg.csv"))
  expr <- bquote({
    set_levcost_cache_path(.(old_lc))
    set_models_path(.(old_mp))
    set_registry_file(.(old_rf))
  })
  do.call(on.exit, list(expr, add = TRUE), envir = env)
  invisible(NULL)
}

lcc_tech <- function(invcost = 800, name = "PPC") {
  newTechnology(
    name, input = list(comm = "GAS"), output = list(comm = "ELC"),
    ceff = data.frame(comm = c("GAS", "ELC"), cinp2use = c(0.55, NA),
                      cact2cout = c(NA, 1)),
    invcost = data.frame(invcost = invcost),
    fixom = data.frame(fixom = 20), varom = data.frame(varom = 2),
    af = data.frame(af.up = 0.85),
    vintage = data.frame(olife = 25L), cap2act = 1)
}

test_that("cache key is deterministic and content/assumption sensitive", {
  k <- function(obj, ...) {
    energyRt:::.levcost_cache_key("technology", obj, list(...))
  }
  expect_identical(k(lcc_tech(), discount = 0.06), k(lcc_tech(), discount = 0.06))
  # misc noise is invisible
  noisy <- lcc_tech(); noisy@misc$scratch <- "x"
  expect_identical(k(noisy, discount = 0.06), k(lcc_tech(), discount = 0.06))
  # content and assumptions are not
  expect_false(identical(k(lcc_tech(900), discount = 0.06),
                         k(lcc_tech(800), discount = 0.06)))
  expect_false(identical(k(lcc_tech(), discount = 0.05),
                         k(lcc_tech(), discount = 0.06)))
  # cache/verbosity knobs are excluded
  expect_identical(k(lcc_tech(), discount = 0.06, verbose = FALSE, force = TRUE),
                   k(lcc_tech(), discount = 0.06))
})

test_that("an in-memory object caches in the temporary tier and hits", {
  lcc_local()
  unlink(lcc_root("levcosts"), recursive = TRUE)

  a1 <- levcost(lcc_tech(), fuel_costs = c(GAS = 15), discount = 0.06,
                base_year = 2025, method = "analytic", verbose = FALSE)
  rds <- list.files(lcc_root("levcosts"), pattern = "\\.rds$", full.names = TRUE)
  expect_length(rds, 1L)
  expect_true(startsWith(basename(rds), "PPC@"))
  expect_true(file.exists(sub("\\.rds$", ".yml", rds)))

  # a rebuilt-identical object hits the cache
  expect_message(
    a2 <- levcost(lcc_tech(), fuel_costs = c(GAS = 15), discount = 0.06,
                  base_year = 2025, method = "analytic"),
    "cached result is current")
  expect_s3_class(a2, "levcost")
  expect_equal(as.numeric(a2$levcost_npv), as.numeric(a1$levcost_npv))

  # force recomputes (no hit message), same single entry remains
  expect_no_message(
    levcost(lcc_tech(), fuel_costs = c(GAS = 15), discount = 0.06,
            base_year = 2025, method = "analytic", force = TRUE,
            verbose = FALSE))
  expect_length(list.files(lcc_root("levcosts"), pattern = "\\.rds$"), 1L)

  # a changed assumption is a second entry
  levcost(lcc_tech(), fuel_costs = c(GAS = 15), discount = 0.07,
          base_year = 2025, method = "analytic", verbose = FALSE)
  expect_length(list.files(lcc_root("levcosts"), pattern = "\\.rds$"), 2L)

  # cache = FALSE writes nothing
  unlink(lcc_root("levcosts"), recursive = TRUE)
  levcost(lcc_tech(), fuel_costs = c(GAS = 15), discount = 0.06,
          base_year = 2025, method = "analytic", cache = FALSE,
          verbose = FALSE)
  expect_false(dir.exists(lcc_root("levcosts")))
})

test_that("solve-path results cache without $scenario; hits skip the solver", {
  skip_if_no_solver()
  lcc_local()
  unlink(lcc_root("levcosts"), recursive = TRUE)

  s1 <- suppressMessages(suppressWarnings(
    levcost(lcc_tech(), fuel_costs = c(GAS = 15), discount = 0.06,
            base_year = 2025, method = "solve", verbose = FALSE)))
  expect_s4_class(s1$scenario, "scenario")   # fresh compute keeps the scenario
  rds <- list.files(lcc_root("levcosts"), pattern = "\\.rds$", full.names = TRUE)
  expect_length(rds, 1L)
  cached <- readRDS(rds)
  expect_null(cached$scenario)               # the artifact does not

  expect_message(
    s2 <- levcost(lcc_tech(), fuel_costs = c(GAS = 15), discount = 0.06,
                  base_year = 2025, method = "solve"),
    "cached result is current")
  expect_equal(as.numeric(s2$levcost_npv), as.numeric(s1$levcost_npv))
})

test_that("a saved model owns its levcost cache under the store entry", {
  skip_if_no_solver()
  lcc_local()
  unlink(lcc_root("levcosts"), recursive = TRUE)
  m <- suppressMessages(save_model(sp_tech(c(100, 100, 100), optret = FALSE,
                                           name = "lcm"), verbose = FALSE))
  own <- m@misc$path
  expect_true(dir.exists(own))

  lc <- suppressMessages(suppressWarnings(
    levcost(m, name = "TECH", verbose = FALSE)))
  expect_s3_class(lc, "levcost")
  expect_length(list.files(file.path(own, "levcost"), pattern = "\\.rds$"), 1L)
  # nothing in the temporary tier
  expect_false(dir.exists(lcc_root("levcosts")))
})
