# =========================================================================== #
# The v0.90 deprecation sunset.
#
# `R/legacy_api_shims.R` held every deprecated public name and nothing else, so
# the sunset was a single-file delete. This file used to assert that the layer
# EXISTED -- that each name was exported, warned naming itself and v0.90, and
# forwarded to its replacement. It now asserts the opposite.
#
# The failure mode worth guarding is not "the old name survived": it is a
# removal that also loses the REPLACEMENT, leaving callers with no migration
# target and only a silent "object not found" to go on. Both directions below.
# =========================================================================== #

# removed name -> the function that replaces it
removed <- c(
  solve_mod             = "solve_model",
  solve_scen            = "solve_scenario",
  interp_mod            = "interpolate_model",   # was internal, not exported
  register              = "add_to_registry",
  get_registry          = "load_registry",
  get_entry             = "find_in_registry",
  get_entry_object      = "getScenario",
  find_registry         = "find_in_registry",
  read_procspec         = "read_process_spec",
  levcost_by_variant    = "levcost",
  registry_exists       = "get_registry_file",
  registry.exists       = "get_registry_file",
  set_default_registry  = "set_registry_file",
  use_registry          = "set_registry_file",
  which_registry        = "get_registry_file",
  tech_designer         = "process_designer",
  tech_from_spec        = "process_from_spec",
  tech_to_spec          = "process_to_spec",
  tech_spec_code        = "process_spec_code",
  tech_spec_issues      = "process_spec_issues",
  read_techspec         = "read_process_spec",
  write.sc              = "write_sc",
  make_scenario_dirname = "set_path_builder",
  get_data              = "getData",
  get_units             = "getUnits"
)

test_that("no removed name is exported", {
  expect_equal(intersect(names(removed), getNamespaceExports("energyRt")),
               character())
})

test_that("no removed name survives inside the namespace either", {
  # `energyRt:::solve_mod()` must fail too -- an unexported leftover would keep
  # working for anyone who had reached for `:::`, and would keep the shim file's
  # contents alive in a package that claims to have dropped them
  ns <- asNamespace("energyRt")
  still <- names(removed)[vapply(names(removed),
                                 function(n) exists(n, envir = ns, inherits = FALSE),
                                 logical(1))]
  expect_equal(still, character())
})

test_that("every replacement named by the removed API still exists", {
  # the point of the table: a removal that also loses the migration target
  # leaves callers with nothing but "object not found"
  ex <- getNamespaceExports("energyRt")
  expect_equal(setdiff(unname(removed), ex), character())
})

test_that("the shim file and its Collate entry are gone", {
  root <- testthat::test_path("..", "..")
  skip_if_not(file.exists(file.path(root, "DESCRIPTION")),
              "not running from the source tree")
  expect_false(file.exists(file.path(root, "R", "legacy_api_shims.R")))
  expect_false(any(grepl("legacy_api_shims",
                         readLines(file.path(root, "DESCRIPTION"), warn = FALSE))))
})

test_that("the UTOPIA satellite datasets are gone, their content is not", {
  sats <- c("utopia_weather", "utopia_demand", "utopia_stock", "utopia_modules")
  # removed from the package
  expect_equal(intersect(sats, utils::data(package = "energyRt")$results[, "Item"]),
               character())
  expect_equal(intersect(sats, getNamespaceExports("energyRt")), character())
  # ... and present in the combined list, which is where they went
  expect_true(all(c("map", "geo", "weather", "demand", "stock", "modules") %in%
                    names(utopia)))
  expect_s3_class(utopia$weather, "data.frame")
  expect_s3_class(utopia$demand, "data.frame")
  expect_s3_class(utopia$stock, "data.frame")
  expect_type(utopia$modules, "list")
  expect_gt(nrow(utopia$weather), 0L)
})
