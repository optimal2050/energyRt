# =========================================================================== #
# A levy on a storage PART charges only where that part exists.
#
# `eqTaxCost` / `eqSubCost` sum each part's rate over the part's own capacity
# map, so `inp.cap` on a storage with no charger multiplies nothing and the
# levy is silently free. `.levy_check_stg_cap()` runs on the object and sees
# only its rate columns; which parts exist is known after the maps are built,
# so the report is an advisory from `validate_scenario_parameters()`.
#
# Advisory rather than an error: a levy selecting every storage legitimately
# sweeps reservoir-only ones, and refusing would block a correct model.
# =========================================================================== #

lsp_build <- function(..., storage_args = list()) {
  stg <- do.call(newStorage, c(list(
    name = "STG", commodity = "ELC",
    input = list(comm = "ELC", cap2act = 1),
    output = list(comm = "ELC", cap2act = 1),
    invcost = data.frame(stg.invcost = 1),
    olife = data.frame(olife = 30)), storage_args))
  newModel("lsp",
    repo = newRepository("lsp_repo",
      newCommodity("ELC", timeframe = "SL"),
      newSupply("SUP", commodity = "ELC",
                supply = data.frame(timeslice = paste0("s", 1:4),
                                    ava.up = 1000, cost = c(1, 100, 100, 100))),
      stg,
      newDemand("DEM", commodity = "ELC",
                demand = data.frame(timeslice = "s3", demand = 8)),
      ...),
    calendar = newCalendar(timetable = make_timetable(
      struct = list(ANNUAL = "ANNUAL", SL = paste0("s", 1:4))), name = "lsp_cal"),
    region = "R1", horizon = newHorizon(2025), discount = 0)
}

lsp_interp <- function(nm, ...) suppressMessages(suppressWarnings(
  interpolate_model(lsp_build(...), name = nm, overwrite = TRUE)))

lsp_issues <- function(scen, check = "levy_no_part") {
  d <- validate_scenario_parameters(scen, action = "silent")
  if (is.null(d) || !NROW(d)) return(data.frame())
  d <- as.data.frame(d)
  d[d$check == check, , drop = FALSE]
}

test_that("levy-storage-parts: a charger levy on a storage with no charger is named", {
  scen <- lsp_interp("lsp_gap",
                     newTax("TX", classes = "storage",
                            tax = data.frame(inp.cap = 5)))
  iss <- lsp_issues(scen)
  expect_equal(nrow(iss), 1L)
  expect_equal(iss$parameter, "pTaxProcInpCap")
  expect_match(iss$detail, "charger that does not exist")
  expect_match(iss$detail, "STG")
  expect_match(iss$detail, "levy TX")
  # advisory only: the model still interpolates, and nothing is structural
  all_iss <- as.data.frame(validate_scenario_parameters(scen, action = "silent"))
  expect_equal(nrow(all_iss[all_iss$severity == "structural", , drop = FALSE]), 0L)
})

test_that("levy-storage-parts: the levy is quiet once the part exists", {
  scen <- lsp_interp("lsp_ok",
                     newTax("TX", classes = "storage",
                            tax = data.frame(inp.cap = 5)),
                     storage_args = list(
                       capacity = data.frame(inp.cap.up = 50)))
  expect_equal(nrow(lsp_issues(scen)), 0L)
})

test_that("levy-storage-parts: a reservoir levy is never reported", {
  # the reservoir is the anchor, so `stg.cap` always has a variable to charge
  scen <- lsp_interp("lsp_stg",
                     newTax("TX", classes = "storage",
                            tax = data.frame(stg.cap = 5)))
  expect_equal(nrow(lsp_issues(scen)), 0L)
})

test_that("levy-storage-parts: a subsidy is reported the same way", {
  scen <- lsp_interp("lsp_sub",
                     newSubsidy("SB", classes = "storage",
                                subsidy = data.frame(out.ncap = 3)))
  iss <- lsp_issues(scen)
  expect_equal(nrow(iss), 1L)
  expect_equal(iss$parameter, "pSubProcOutNcap")
  expect_match(iss$detail, "discharger that does not exist")
})
