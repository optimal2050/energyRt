# `interpolate_slot()` (R/interp_shared.R) fills a constraint's rhs / for.each
# over the years between the declared ones. It used to interpolate the whole
# column at once: with two regions the tied years collapsed and both regions
# received the cross-region AVERAGE (R1 6/8 and R2 1/3 became 3.5/5.5), and a
# single declared year over two regions errored ("need at least two non-NA
# values"). Interpolation is per group of the non-year keys.

.ci_model <- function(rhs) {
  skip_if_no_fixtures()
  env <- .mapping_fixture_env()
  mod <- env$tm_core()
  add(mod, newConstraint(
    name = "CAPX", eq = "<=", defVal = 1e6,
    for.each = unique(rhs[, c("region", "year")]),
    term1 = list(variable = "vTechCap"),
    rhs = rhs))
}

.ci_rhs <- function(scen) {
  d <- as.data.frame(get_data_slot(scen@modInp@parameters$pCnsRhsCAPX))
  d <- d[order(d$region, d$year), ]
  rownames(d) <- NULL
  d
}

test_that("a region-varying constraint rhs keeps each region's values", {
  mod <- .ci_model(data.frame(region = rep(c("R1", "R2"), each = 2),
                              year = rep(c(2020L, 2030L), 2),
                              rhs = c(6, 8, 1, 3)))
  yrs <- sort(unique(as.integer(mod@config@horizon@intervals$mid)))
  scen <- suppressWarnings(suppressMessages(
    interpolate_model(mod, name = "ci_var", ondisk = FALSE)))
  d <- .ci_rhs(scen)
  expect_true(all(c("region", "year", "value") %in% names(d)))
  expect_setequal(unique(d$region), c("R1", "R2"))
  # linear in year, per region: R1 runs 6 -> 8, R2 runs 1 -> 3
  for (y in intersect(yrs, 2020:2030)) {
    w <- (y - 2020) / 10
    expect_equal(d$value[d$region == "R1" & d$year == y], 6 + 2 * w, label = paste("R1", y))
    expect_equal(d$value[d$region == "R2" & d$year == y], 1 + 2 * w, label = paste("R2", y))
  }
})

test_that("a single declared year over two regions interpolates", {
  mod <- .ci_model(data.frame(region = c("R1", "R2"), year = 2020L, rhs = c(6, 1)))
  scen <- suppressWarnings(suppressMessages(
    interpolate_model(mod, name = "ci_one", ondisk = FALSE)))
  d <- .ci_rhs(scen)
  expect_equal(d$value[d$region == "R1" & d$year == 2020L], 6)
  expect_equal(d$value[d$region == "R2" & d$year == 2020L], 1)
})
