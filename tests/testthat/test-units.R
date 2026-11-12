# getUnits() resolution -------------------------------------------------------

.units_tech <- function() {
  suppressWarnings(newTechnology(
    "TT", desc = "test",
    units = list(capacity = "GW", activity = "PJ", costs = "MUSD"),
    input = data.frame(comm = "GAS", unit = "PJ"),
    output = data.frame(comm = "ELC", unit = "GWh"),
    aux = data.frame(acomm = "CO2", unit = "kt"),
    ceff = data.frame(comm = "GAS", cinp2use = 1),
    aeff = data.frame(acomm = "CO2", comm = "GAS", cinp2ainp = 55)
  ))
}

test_that("aeff units resolve both comm and acomm from the row", {
  u <- as.data.frame(getUnits(.units_tech(), slots = "aeff"))
  r <- u[u$parameter == "cinp2ainp", ]
  expect_equal(nrow(r), 1L)
  expect_equal(r$unit, "PJ/kt")
  expect_equal(r$comm, "GAS/CO2")
})

test_that("complete = TRUE reports empty parameters with resolved tokens", {
  u <- as.data.frame(getUnits(.units_tech(), complete = TRUE))
  # af has no data at all, but its formula-known bounds are reported
  expect_true(all(c("af.lo", "af.up", "af.fx") %in%
                    u$parameter[u$slot == "af"]))
  expect_equal(unique(u$unit[u$slot == "af"]), "fraction")
  # avarom is empty; its {acomm} resolves because CO2 is the only aux port
  expect_equal(u$unit[u$slot == "varom" & u$parameter == "avarom"],
               "MUSD/kt")
  # cvarom is empty and input/output units differ -> token stays visible
  expect_equal(u$unit[u$slot == "varom" & u$parameter == "cvarom"],
               "MUSD/{comm}")
  # default (complete = FALSE) keeps the populated-only behaviour
  u0 <- as.data.frame(getUnits(.units_tech()))
  expect_false("af" %in% u0$slot)
})


# The model's currency, at construction ---------------------------------------
#
# A cost priced in another currency is reconciled where it can be and refused
# where it cannot. Up to 0.92 any mismatch was an error -- there was no
# exchange rate in the package to reconcile one with; since 0.93 there is
# sourced annual data behind `convert()`, so a mismatch it can answer is work
# for `normalize_model_currency()` (test-currency-normalize.R) and only a pair
# with no conversion at all stops construction. The check runs at
# `newModel()`, not only at `interpolate_model()`, which used to let a
# mismatched object sit in a model until something much later failed.

.cur_repo <- function(costs = "MUSD") {
  sup <- newSupply("SUP_COA", commodity = "COA", unit = "PJ",
                   units = data.frame(activity = "PJ", costs = costs),
                   supply = data.frame(region = "R1", cost = 2.5, ava.up = 10))
  newRepository("cur") |> add(newCommodity("COA", unit = "PJ"), sup)
}

test_that("newModel() refuses a currency it cannot convert from", {
  # `bnEUR` is not a unit at all: no factor exists, so the costs in the
  # objective would be a sum of numbers that do not share a measure.
  expect_error(
    newModel("CUR", currency = "MUSD", region = "R1",
             data = .cur_repo(costs = "bnEUR")),
    "cannot be reached from")
})

test_that("newModel() accepts a mismatch convert() can answer", {
  # crore rupees against millions of dollars is an exchange rate, and one the
  # shipped tables carry. The conversion itself happens at interpolation.
  expect_s4_class(
    newModel("CUR1", currency = "cr.INR", region = "R1", data = .cur_repo()),
    "model")
})

test_that("a matching currency, and none at all, both construct", {
  expect_s4_class(
    newModel("CUR2", currency = "MUSD", region = "R1", data = .cur_repo()),
    "model")
  expect_s4_class(newModel("CUR3", region = "R1", data = .cur_repo()), "model")
  # an object claiming nothing is not compared
  expect_s4_class(
    newModel("CUR4", currency = "cr.INR", region = "R1",
             data = .cur_repo(costs = NA_character_)),
    "model")
})
