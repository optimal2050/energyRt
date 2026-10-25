# Derived scenarios: interpolate_model(..., base = REF) adds constraints, cost
# terms, taxes and subsidies to an interpolated base and must equal a full
# interpolation of the model with the base's additions and the new ones.
# Solver-free except the objective check.

.ib_ref_add <- function() {
  newRepository("ref_policy",
    newTax("TCO2", comm = "CO2", tax = data.frame(out = 10)),
    newCosts("FEEGAS", variable = "vTechAct", mult = 2,
             subset = list(tech = "EGAS")))
}

.ib_new <- function() {
  list(
    newConstraint(name = "CAPCOA", eq = "<=",
                  for.each = data.frame(year = 2030),
                  term1 = list(variable = "vTechNewCap",
                               for.sum = list(tech = "ECOA")),
                  defVal = 1),
    newCosts("FEECOA", variable = "vTechAct", mult = 3,
             subset = list(tech = "ECOA")),
    newTax("TCOA", comm = "COA", tax = data.frame(inp = 2)),
    newSubsidy("SSOL", process = "ESOL", subsidy = data.frame(ncap = 5)))
}

.ib_quiet <- function(expr) suppressMessages(suppressWarnings(expr))

# full interpolation and the derived one, from the same model
.ib_pair <- function(mod, fold = FALSE) {
  ref <- .ib_quiet(interpolate_model(mod, .ib_ref_add(), name = "REF",
                                     fold = fold, verbose = FALSE))
  full <- .ib_quiet(do.call(interpolate_model,
    c(list(mod, .ib_ref_add()), .ib_new(),
      list(name = "CAP", fold = fold, verbose = FALSE))))
  der <- .ib_quiet(do.call(interpolate_model,
    c(list(mod), .ib_new(), list(name = "CAP", base = ref, verbose = FALSE))))
  list(ref = ref, full = full, der = der)
}

test_that("a derived scenario equals the full interpolation", {
  p <- .ib_pair(ut_build("R1", "s4_h24"))
  expect_true(isTRUE(p$der@status$interpolated))
  expect_false(isTRUE(p$der@status$solved))
  expect_identical(p$der@misc$base$name, "REF")
  expect_true(all(c("ref_policy", "CAP_repo") %in% names(p$der@model@data)))
  ci <- compare_inputs(p$der, p$full)
  expect_equal(sum(ci$summary$status != "identical"), 0)
  expect_identical(p$der@modInp@user_costs, p$full@modInp@user_costs)
  expect_setequal(names(p$der@modInp@user_constraints),
                  names(p$full@modInp@user_constraints))
  # the base itself is untouched
  expect_false(any(c("TCOA", "SSOL") %in% p$ref@modInp@sets$levy))
})

test_that("a folded base gives a folded derived scenario equal to the full one", {
  skip_if_tier_below("fast")
  p <- .ib_pair(ut_build("R3", "s4_h24"), fold = TRUE)
  expect_setequal(p$der@status$folded, p$full@status$folded)
  ci <- compare_inputs(p$der, p$full)
  expect_equal(sum(ci$summary$status != "identical"), 0)
})

test_that("the base passed as `object` is its own model", {
  mod <- ut_build("R1", "s4_h24")
  ref <- .ib_quiet(interpolate_model(mod, .ib_ref_add(), name = "REF",
                                     verbose = FALSE))
  der <- .ib_quiet(do.call(interpolate_model,
    c(list(ref), .ib_new(), list(name = "CAP", base = ref, verbose = FALSE))))
  full <- .ib_quiet(do.call(interpolate_model,
    c(list(mod, .ib_ref_add()), .ib_new(), list(name = "CAP", verbose = FALSE))))
  ci <- compare_inputs(der, full)
  expect_equal(sum(ci$summary$status != "identical"), 0)
})

test_that("anything beyond the supported additions is interpolated in full", {
  mod <- ut_build("R1", "s4_h24")
  ref <- .ib_quiet(interpolate_model(mod, .ib_ref_add(), name = "REF",
                                     verbose = FALSE))
  msg <- function(...) {
    m <- character()
    withCallingHandlers(
      suppressWarnings(interpolate_model(..., base = ref, verbose = FALSE)),
      message = function(e) {
        m <<- c(m, conditionMessage(e))
        invokeRestart("muffleMessage")
      })
    paste(m, collapse = "")
  }
  expect_match(msg(mod, newTax("TCO2", comm = "CO2", tax = data.frame(out = 3)),
                   name = "X1"), "replaces an object")
  expect_match(msg(mod, newHorizon(2030), name = "X2"), "class horizon")
  expect_match(msg(mod, .ib_new()[[3]], name = "X3", fold = TRUE),
               "`fold` differs")
  expect_match(msg(mod, .ib_ref_add(), name = "X6"), "already in the base")
  mod2 <- add(mod, newTechnology("ENEW", input = list(comm = "GAS"),
                                 output = list(comm = "ELC")))
  expect_match(msg(mod2, .ib_new()[[3]], name = "X4"), "model differs")
  expect_error(interpolate_model(mod, .ib_new()[[3]], name = "X5", base = mod),
               "interpolated scenario")
})

test_that("a saved base is read, never written, and the result saves", {
  skip_if_tier_below("fast")
  withr::local_options(energyRt.ondisk_min_rows = 1,
                       energyRt.param_ref_min_rows = 40,
                       energyRt.save_mode = "ref")
  tmp <- withr::local_tempdir()
  .ib_quiet(open_project(tmp))
  mod <- ut_build("R1", "s4_h24")
  ref <- .ib_quiet(interpolate_model(mod, .ib_ref_add(), name = "REF",
                                     verbose = FALSE))
  .ib_quiet(save_scenario(ref, verbose = FALSE))
  snap <- function(d) {
    f <- sort(list.files(d, recursive = TRUE, all.files = TRUE, full.names = TRUE))
    i <- file.info(f)
    data.frame(f = f, size = i$size, mtime = as.numeric(i$mtime))
  }
  before <- snap(ref@path)
  refl <- .ib_quiet(load_scenario(ref@path, env = NULL, verbose = FALSE))
  expect_true(any(vapply(refl@modInp@parameters,
                         function(p) !is.null(p@misc$param_ref), logical(1))))
  der <- .ib_quiet(do.call(interpolate_model,
    c(list(mod), .ib_new(), list(name = "CAP", base = refl, verbose = FALSE))))
  full <- .ib_quiet(do.call(interpolate_model,
    c(list(mod, .ib_ref_add()), .ib_new(), list(name = "CAP", verbose = FALSE))))
  # on-disk tables are read through arrow's deprecated "feather" format name
  ci <- suppressWarnings(compare_inputs(der, full))
  expect_equal(sum(ci$summary$status != "identical"), 0)
  expect_identical(snap(ref@path), before)
  .ib_quiet(save_scenario(der, verbose = FALSE))
  mf <- yaml::read_yaml(file.path(der@path, "scenario.yml"))
  expect_identical(mf$base$name, "REF")
  derl <- .ib_quiet(load_scenario(der@path, env = NULL, verbose = FALSE))
  ci <- suppressWarnings(compare_inputs(derl, full))
  expect_equal(sum(ci$summary$status != "identical"), 0)
})

test_that("a derived scenario solves to the full scenario's objective", {
  skip_if_tier_below("fast")
  skip_if_no_solver()
  p <- .ib_pair(ut_build("R1", "s4_h24"))
  a <- .fork_solve(p$der, solver_options$glpk)
  b <- .fork_solve(p$full, solver_options$glpk)
  expect_equal(.fork_objective(a), .fork_objective(b), tolerance = 1e-8)
})

test_that("the model hash ignores the S7 class object of a geoscale", {
  gs <- topia$modules$electricity$R3$geoscale
  x <- .strip_volatile(gs)
  expect_null(attr(x, "S7_class"))
  expect_identical(class(x), class(gs))
})

# -- phase B: new processes and commodities on a base --------------------------

.ib_build_new <- function(repo) {
  stg <- repo@data$STG_ELC
  stg@name <- "STG_NEW"
  list(
    newTechnology("EGAS2", input = list(comm = "GAS"), output = list(comm = "ELC"),
                  ceff = data.frame(comm = "GAS", cinp2use = 0.6),
                  invcost = data.frame(invcost = 900),
                  vintage = data.frame(olife = 30L), cap2act = 8.76),
    stg,
    newCommodity("H2", timeframe = "ANNUAL"),
    newSupply("SUP_H2", commodity = "H2", supply = data.frame(cost = 50)),
    newTechnology("EH2", input = list(comm = "H2"), output = list(comm = "ELC"),
                  invcost = data.frame(invcost = 700),
                  vintage = data.frame(olife = 25L), cap2act = 8.76))
}

test_that("new processes and commodities on a base equal the full interpolation", {
  skip_if_tier_below("fast")
  for (fold in c(FALSE, TRUE)) {
    mod <- ut_build("R3", "s4_h24")
    new <- .ib_build_new(topia$modules$electricity$R3$repo)
    ref <- .ib_quiet(interpolate_model(mod, .ib_ref_add(), name = "REF",
                                       fold = fold, verbose = FALSE))
    full <- .ib_quiet(do.call(interpolate_model,
      c(list(mod, .ib_ref_add()), new, list(name = "CAP", fold = fold, verbose = FALSE))))
    der <- .ib_quiet(do.call(interpolate_model,
      c(list(mod), new, list(name = "CAP", base = ref, verbose = FALSE))))
    ci <- compare_inputs(der, full)
    expect_equal(sum(ci$summary$status != "identical"), 0, label = paste("fold", fold))
    expect_identical(der@misc$base$name, "REF")
    expect_true("EGAS2" %in% as.character(der@modInp@sets$tech))
  }
})

test_that("a bound declared per region and one for all regions both bind", {
  # one storage's duration names its regions, the other's applies to all: the
  # NA of the second is a wildcard for its own rows
  repo <- topia$modules$electricity$R3$repo
  stg <- repo@data$STG_ELC
  stg@name <- "STG_REG"
  stg@duration <- data.frame(vintage = NA_character_, cluster = NA_character_,
                             region = c("W1", "W2", "C1"), year = NA_integer_,
                             duration = NA_real_, duration.lo = NA_real_,
                             duration.up = NA_real_, duration.fx = 0.0144)
  scen <- .ib_quiet(interpolate_model(add(ut_build("R3", "s4_h24"), stg),
                                      name = "MIX", verbose = FALSE))
  fx <- as.data.frame(get_data_slot(scen@modInp@parameters$mStorageDurationFx))
  expect_setequal(unique(as.character(fx$stg)), c("STG_ELC", "STG_REG"))
})
