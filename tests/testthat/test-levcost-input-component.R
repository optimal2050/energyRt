# =========================================================================== #
# `input` is what a process pays for what it CONSUMES, and it is reported
# whenever the process consumes anything -- at zero when nothing prices the
# commodity.
#
# A free input is a price, not a missing number. `cost = 0` equals `pSupCost`'s
# default, so the row is pruned before the maps are built and no `vSupCost`
# cell remains to read back; the analytic engine drops an all-zero component
# through `.la_nz()` for the same reason. Both engines therefore left the
# component out, and `levcost()` on a bare technology read as a complete cost.
#
# The component was named `supply` until 2026-11-18. A process's inputs are
# not necessarily fuels, and the engine cannot tell.
# =========================================================================== #

lci_tech <- function() newTechnology(
  "PP", input = list(comm = "GAS"), output = list(comm = "ELC"),
  ceff = data.frame(comm = c("GAS", "ELC"), cinp2use = c(0.55, NA),
                    cact2cout = c(NA, 1)),
  invcost = data.frame(invcost = 800),
  fixom = data.frame(fixom = 20),
  af = data.frame(af.up = 0.85),
  vintage = data.frame(olife = 25L), cap2act = 1)

lci_npv <- function(x) {
  d <- as.data.frame(x$cost_breakdown_npv)
  stats::setNames(d$value, d$component)
}

test_that("levcost: a free input is reported at zero, not omitted", {
  skip_if_no_solver()
  tech <- lci_tech()
  for (m in c("solve", "analytic")) {
    x <- suppressMessages(suppressWarnings(
      levcost(tech, method = m, discount = 0.06, base_year = 2025,
              verbose = FALSE)))
    cmp <- lci_npv(x)
    expect_true("input" %in% names(cmp), info = m)
    expect_equal(unname(cmp[["input"]]), 0, info = m)
    expect_true("input" %in% names(x$cost_yearly), info = m)
    # the capital term is unaffected: the zero is an addition, not a rescale
    expect_gt(unname(cmp[["eac"]]), 0)
  }
})

test_that("levcost: a priced input lands in the same component", {
  skip_if_no_solver()
  x <- suppressMessages(suppressWarnings(
    levcost(lci_tech(), method = "solve", fuel_costs = c(GAS = 15),
            discount = 0.06, base_year = 2025, verbose = FALSE)))
  cmp <- lci_npv(x)
  expect_gt(unname(cmp[["input"]]), 0)
  # the old name is gone from every table the result carries
  expect_false("supply" %in% names(cmp))
  expect_false("supply" %in% names(x$cost_yearly))
  expect_false("supply" %in% as.character(x$cost_breakdown$component))
})
