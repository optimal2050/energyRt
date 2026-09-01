# A user constraint whose `for.each` years all fall outside the solved horizon
# has an empty index domain. Such a constraint cannot bind and must be dropped
# with a warning: keeping it emitted `eqCns<name>(year)` declared over a domain
# that was never defined (the `mCnsForEach<name>` guard is only written for a
# non-empty domain), which JuMP rejected at parse time with
#   "Unexpected error parsing reference set: eqCns<name>".
# The complementary invariant -- an INDEXED equation always carries its domain
# map -- is asserted below; it is what would have caught the original bug.

.cns_over <- function(year, name = "OUT_OF_HORIZON") {
  newConstraint(
    name = name,
    eq = "<=",
    for.each = data.frame(year = year),
    term1 = list(variable = "vTechNewCap"),
    rhs = data.frame(year = year, rhs = 1e-20),
    defVal = 1e10
  )
}

test_that("a constraint with no year inside the horizon is dropped with a warning", {
  skip_if_no_fixtures()
  env <- .mapping_fixture_env()
  mod <- env$tm_core()

  yrs <- sort(unique(as.integer(mod@config@horizon@intervals$mid)))
  out_year <- min(yrs) - 100L
  mod <- add(mod, .cns_over(out_year))

  expect_warning(
    scen <- suppressMessages(
      interpolate_model(mod, name = "cns_out", ondisk = FALSE)
    ),
    "OUT_OF_HORIZON"
  )

  # nothing about the constraint reaches modInp: no equation ...
  expect_false("OUT_OF_HORIZON" %in% names(scen@modInp@user_constraints))
  # ... and no orphan supporting parameters either
  expect_false("mCnsForEachOUT_OF_HORIZON" %in% names(scen@modInp@parameters))
  expect_false("pCnsRhsOUT_OF_HORIZON" %in% names(scen@modInp@parameters))
})

test_that("the dropped-constraint warning names the requested and available years", {
  skip_if_no_fixtures()
  env <- .mapping_fixture_env()
  mod <- env$tm_core()

  yrs <- sort(unique(as.integer(mod@config@horizon@intervals$mid)))
  out_year <- min(yrs) - 100L
  mod <- add(mod, .cns_over(out_year))

  w <- tryCatch(
    suppressMessages(interpolate_model(mod, name = "cns_msg", ondisk = FALSE)),
    warning = function(w) conditionMessage(w)
  )
  expect_match(w, as.character(out_year), fixed = TRUE)
  expect_match(w, as.character(min(yrs)), fixed = TRUE)
})

test_that("an indexed constraint inside the horizon is kept, with its domain map", {
  skip_if_no_fixtures()
  env <- .mapping_fixture_env()
  mod <- env$tm_core()

  yrs <- sort(unique(as.integer(mod@config@horizon@intervals$mid)))
  mod <- add(mod, .cns_over(min(yrs), name = "IN_HORIZON"))

  scen <- suppressWarnings(suppressMessages(
    interpolate_model(mod, name = "cns_in", ondisk = FALSE)
  ))

  expect_true("IN_HORIZON" %in% names(scen@modInp@user_constraints))
  eq <- scen@modInp@user_constraints[["IN_HORIZON"]]$equation
  # the invariant: an equation declared over indices must carry the map that
  # defines them, or the backends have nothing to index over
  expect_match(eq, "eqCnsIN_HORIZON(year)", fixed = TRUE)
  expect_match(eq, "$mCnsForEachIN_HORIZON(", fixed = TRUE)
  expect_true("mCnsForEachIN_HORIZON" %in% names(scen@modInp@parameters))

  # and the JuMP translation indexes over that map, not over the equation name
  jl <- paste(.equation.from.gams.to.julia(eq), collapse = "\n")
  expect_match(jl, "mCnsForEachIN_HORIZON", fixed = TRUE)
})

test_that("`for.each` year = NA still expands to all milestone years", {
  skip_if_no_fixtures()
  env <- .mapping_fixture_env()
  mod <- env$tm_core()

  yrs <- sort(unique(as.integer(mod@config@horizon@intervals$mid)))
  cns <- newConstraint(
    name = "ALL_YEARS",
    eq = "<=",
    for.each = data.frame(year = NA),
    term1 = list(variable = "vTechNewCap"),
    rhs = data.frame(year = yrs, rhs = 1e6),
    defVal = 1e6
  )
  mod <- add(mod, cns)

  scen <- suppressWarnings(suppressMessages(
    interpolate_model(mod, name = "cns_na", ondisk = FALSE)
  ))

  expect_true("ALL_YEARS" %in% names(scen@modInp@user_constraints))
  expect_true("mCnsForEachALL_YEARS" %in% names(scen@modInp@parameters))
  expect_setequal(
    as.integer(scen@modInp@parameters[["mCnsForEachALL_YEARS"]]@data$year),
    yrs
  )
})

test_that("a constraint with no `for.each` at all is unaffected by the guard", {
  skip_if_no_fixtures()
  env <- .mapping_fixture_env()
  mod <- env$tm_core()

  # no for.each => scalar equation `eqCnsSCALAR..` with no index domain; the
  # empty-domain guard must not fire on it
  cns <- newConstraint(
    name = "SCALAR",
    eq = "<=",
    term1 = list(variable = "vTechNewCap"),
    defVal = 1e6
  )
  mod <- add(mod, cns)

  scen <- suppressWarnings(suppressMessages(
    interpolate_model(mod, name = "cns_scalar", ondisk = FALSE)
  ))

  expect_true("SCALAR" %in% names(scen@modInp@user_constraints))
  eq <- scen@modInp@user_constraints[["SCALAR"]]$equation
  expect_match(eq, "eqCnsSCALAR", fixed = TRUE)
  expect_false(grepl("mCnsForEachSCALAR", eq, fixed = TRUE))
})
