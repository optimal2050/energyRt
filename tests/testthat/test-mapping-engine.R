# =========================================================================== #
# Regression tests for the new mapping pipeline (interp_mod + mapping_engine).
#
# Strategy:
#   * Comprehensive guard - snapshot the full (map -> nrow) table for every
#     cumulative tier (tm_core .. tm_weather). Any map appearing, disappearing,
#     or changing size is caught by the snapshot. Snapshots are recorded on
#     first run and reviewed in the PR diff.
#   * Intent guard - explicit assertions for the maps each tier was built to
#     exercise (trade-Ir chain, aux conversion, emissions, aggregate, import /
#     export, supply availability, tax / subsidy, weather). These document the
#     validated behaviour and cover the empty Lo-bound weather maps that the
#     snapshot (non-empty maps only) does not list.
#
# The tm_*() tier builders come from data-raw/testing-models.R (sourced by the
# helper); tests skip when that catalog is unavailable.
# =========================================================================== #

# --------------------------------------------------------------------------- #
# Comprehensive snapshot guard, one per cumulative tier.
# --------------------------------------------------------------------------- #
test_that("tm_core mapping-parameter counts are stable", {
  skip_if_no_fixtures()
  scen <- interp_tier("tm_core")
  expect_snapshot_value(mapping_counts(scen), style = "json2")
})

test_that("tm_flows mapping-parameter counts are stable", {
  skip_if_no_fixtures()
  scen <- interp_tier("tm_flows")
  expect_snapshot_value(mapping_counts(scen), style = "json2")
})

test_that("tm_io mapping-parameter counts are stable", {
  skip_if_no_fixtures()
  scen <- interp_tier("tm_io")
  expect_snapshot_value(mapping_counts(scen), style = "json2")
})

test_that("tm_policy mapping-parameter counts are stable", {
  skip_if_no_fixtures()
  scen <- interp_tier("tm_policy")
  expect_snapshot_value(mapping_counts(scen), style = "json2")
})

test_that("tm_weather mapping-parameter counts are stable", {
  skip_if_no_fixtures()
  scen <- interp_tier("tm_weather")
  expect_snapshot_value(mapping_counts(scen), style = "json2")
})

# --------------------------------------------------------------------------- #
# Intent guards: the deliverable maps of each tier (P2-P6).
# --------------------------------------------------------------------------- #
test_that("tm_core builds the trade-Ir chain and core value maps", {
  skip_if_no_fixtures()
  scen <- interp_tier("tm_core")
  # Trade interregional routing chain.
  expect_equal(map_nrow(scen, "mTradeRoutes"), 1)
  expect_equal(map_nrow(scen, "mTradeIr"), 8)
  expect_equal(map_nrow(scen, "mvTradeIr"), 8)
  # Interregional trade cost maps (region = dst for import, src for export).
  expect_equal(map_nrow(scen, "mImportIrCost"), 2)
  expect_equal(map_nrow(scen, "mExportIrCost"), 2)
  # Core value maps.
  expect_equal(map_nrow(scen, "mTechInv"), 4)
  expect_equal(map_nrow(scen, "mvSupCost"), 2)
  expect_equal(map_nrow(scen, "mSupSpan"), 2)
})

test_that("tm_flows builds aux-conversion, emission and aggregate maps", {
  skip_if_no_fixtures()
  scen <- interp_tier("tm_flows")
  # Auxiliary-commodity conversion chain.
  expect_equal(map_nrow(scen, "mTechAInp"), 1)
  expect_equal(map_nrow(scen, "mTechCap2AInp"), 2)
  expect_equal(map_nrow(scen, "mStorageStg2AInp"), 16)
  expect_equal(map_nrow(scen, "mTradeIrAInp"), 1)
  expect_equal(map_nrow(scen, "mTradeIrCsrc2Ainp"), 8)
  # Emission-fuel maps.
  expect_equal(map_nrow(scen, "mTechEmsFuel"), 2)
  expect_equal(map_nrow(scen, "mEmsFuelTot"), 2)
  # Aggregate commodity maps.
  expect_equal(map_nrow(scen, "mAggOut"), 4)
  expect_equal(map_nrow(scen, "mAggregateFactor"), 1)
  # Input / output subsidy-support maps.
  expect_equal(map_nrow(scen, "mInpSub"), 8)
  expect_equal(map_nrow(scen, "mOutSub"), 8)
})

test_that("tm_io builds import / export and supply-availability maps", {
  skip_if_no_fixtures()
  scen <- interp_tier("tm_io")
  expect_equal(map_nrow(scen, "mImportRow"), 4)
  expect_equal(map_nrow(scen, "mImportRowUp"), 2)
  expect_equal(map_nrow(scen, "mExportRow"), 16)
  expect_equal(map_nrow(scen, "mExportRowUp"), 8)
  expect_equal(map_nrow(scen, "mImportRowCost"), 4)
  expect_equal(map_nrow(scen, "mExportRowCost"), 4)
  # Capped coal supply -> upper-bound availability map.
  expect_equal(map_nrow(scen, "mSupAvaUp"), 2)
})

test_that("tm_policy builds tax and subsidy cost maps", {
  skip_if_no_fixtures()
  scen <- interp_tier("tm_policy")
  expect_equal(map_nrow(scen, "mTaxCost"), 4)
  expect_equal(map_nrow(scen, "mSubCost"), 2)
})

test_that("tm_weather builds weather maps split by bound type", {
  skip_if_no_fixtures()
  scen <- interp_tier("tm_weather")
  # Up maps (sources carry an upper / fixed bound).
  expect_equal(map_nrow(scen, "mTechWeatherAfUp"), 1)
  expect_equal(map_nrow(scen, "mTechWeatherAfsUp"), 1)
  expect_equal(map_nrow(scen, "mTechWeatherAfcUp"), 1)
  expect_equal(map_nrow(scen, "mStorageWeatherAfUp"), 1)
  expect_equal(map_nrow(scen, "mStorageWeatherCinpUp"), 1)
  expect_equal(map_nrow(scen, "mStorageWeatherCoutUp"), 1)
  expect_equal(map_nrow(scen, "mSupWeatherUp"), 1)
  # Only the technology af bound carries a lower bound in the fixture.
  expect_equal(map_nrow(scen, "mTechWeatherAfLo"), 1)
  # All other Lo maps stay empty (no lower bound supplied).
  for (nm in c("mTechWeatherAfsLo", "mTechWeatherAfcLo",
               "mStorageWeatherAfLo", "mStorageWeatherCinpLo",
               "mStorageWeatherCoutLo", "mSupWeatherLo")) {
    expect_equal(map_nrow(scen, nm), 0)
  }
})

# --------------------------------------------------------------------------- #
# Content guard: the supply weather map melts the wide bounds slot correctly
# (regression for the pSupWeather wide-vs-long bug fixed in obj2modInp.R).
# --------------------------------------------------------------------------- #
test_that("mSupWeatherUp carries the expected weather/supply key", {
  skip_if_no_fixtures()
  scen <- interp_tier("tm_weather")
  gds <- getFromNamespace("get_data_slot", "energyRt")
  d <- as.data.frame(gds(scen@modInp@parameters[["mSupWeatherUp"]]))
  expect_true("WWIN" %in% d$weather)
  expect_true("SUP_COA" %in% d$sup)
})
