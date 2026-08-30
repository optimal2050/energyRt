# =========================================================================== #
# The deprecation layer (R/legacy_api_shims.R).
#
# One table drives everything: every deprecated name must be exported, must
# warn naming ITSELF and the removal version, and must forward to its
# replacement. When the sunset arrives, this file and the shim file are
# deleted together -- if one goes without the other, these tests say so.
# =========================================================================== #

# deprecated name -> the replacement named in its warning
dep_pairs <- c(
  solve_mod             = "solve_model",
  solve_scen            = "solve_scenario",
  register              = "add_to_registry",
  get_registry          = "load_registry",
  get_entry             = "find_in_registry",
  get_entry_object      = "getScenario",
  find_registry         = "find_in_registry",
  read_procspec         = "read_process_spec",
  levcost_by_variant    = "levcost(by_variant = )",
  # these now report whether the project HAS a registry; `name` is ignored
  registry_exists       = "file.exists(get_registry_file())",
  registry.exists       = "file.exists(get_registry_file())",
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

test_that("every deprecated name is still exported", {
  expect_setequal(intersect(names(dep_pairs),
                            getNamespaceExports("energyRt")),
                  names(dep_pairs))
})

test_that("every shim warns naming itself, its replacement, and v0.90", {
  ns <- asNamespace("energyRt")
  for (old in names(dep_pairs)) {
    # read the body rather than calling: some shims launch a Shiny app or
    # need a solved scenario, and the contract under test is the warning
    src <- paste(deparse(get(old, envir = ns)), collapse = " ")
    expect_match(src, paste0('.dep090("', old, '"'), fixed = TRUE,
                 label = paste0(old, " warns with its own name"))
    expect_match(src, dep_pairs[[old]], fixed = TRUE,
                 label = paste0(old, " names ", dep_pairs[[old]]))
  }
})

test_that(".dep090 states the sunset version once, in the house wording", {
  w <- tryCatch(energyRt:::.dep090("old_fn", "new_fn"),
                warning = conditionMessage)
  expect_match(w, "`old_fn()` is deprecated", fixed = TRUE)
  expect_match(w, "won't be available starting energyRt v0.90", fixed = TRUE)
  expect_match(w, "use `new_fn` instead", fixed = TRUE)
  # no replacement named -> no dangling "use"
  w2 <- tryCatch(energyRt:::.dep090("lonely_fn"), warning = conditionMessage)
  expect_false(grepl("use `", w2, fixed = TRUE))
})

test_that("forwarding shims return exactly what their replacement returns", {
  f <- tempfile(fileext = ".csv")
  old_file <- set_registry_file(f)
  on.exit(set_registry_file(old_file), add = TRUE)
  save_registry(newRegistry(), f)   # load_registry() no longer invents one

  expect_warning(r1 <- get_registry(), "v0.90")
  expect_identical(r1, load_registry())

  cm <- newCommodity("ELC", unit = "PJ")
  expect_warning(u1 <- get_units(cm), "v0.90")
  expect_identical(u1, getUnits(cm))

  # registry_exists() answers "does this project have a registry file?"
  expect_warning(e1 <- registry_exists("ignored_name"), "v0.90")
  expect_true(e1)                                    # saved above
  gone <- set_registry_file(file.path(tempdir(), "no_such_reg.csv"))
  expect_warning(e2 <- registry_exists("ignored_name"), "v0.90")
  expect_false(e2)
  set_registry_file(gone)

  expect_warning(fr <- find_registry(newRegistry(), type = "model"), "v0.90")
  expect_identical(nrow(fr), 0L)
})

test_that("the shim file holds NO live API", {
  # Live methods (interpolate/solve) were moved to interp.R / solve.R so the
  # file can be deleted wholesale at the sunset. Guard the invariant.
  src <- readLines(testthat::test_path("..", "..", "R",
                                       "legacy_api_shims.R"), warn = FALSE)
  expect_false(any(grepl("^setMethod\\(", src)),
               label = "no setMethod() in the deprecation file")
  expect_false(any(grepl("^setGeneric\\(", src)),
               label = "no setGeneric() in the deprecation file")
})
