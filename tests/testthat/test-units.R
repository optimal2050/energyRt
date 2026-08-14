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
