# Regression test for user-defined `constraint` objects.
#
# A `constraint` added to the model must be compiled by ob2mi("constraint")
# (run after the mapping pipeline in interp_mod) into the solver-agnostic
# GAMS-string IR (`scen@modInp@gams.equation`) plus its supporting pCns*/mCns*
# parameters, and must translate to each backend. This is solver-independent:
# it exercises the pure-R IR translators, so no Julia/Python/GLPK binary is
# needed (mirrors the catalog-skip pattern of the mapping-engine tests).

test_that("a user constraint compiles to IR and translates to all backends", {
  skip_if_no_fixtures()
  env <- .mapping_fixture_env()
  mod <- env$tm_core()

  yrs <- sort(unique(as.integer(mod@config@horizon@intervals$mid)))
  # Upper bound on total new technology capacity per year, summed over all
  # technologies and regions. defVal RHS is large => never binding; the point
  # is that it COMPILES and TRANSLATES, not that it changes the solution.
  cns <- newConstraint(
    name = "MAXNEWCAP",
    eq = "<=",
    for.each = data.frame(year = yrs),
    term1 = list(variable = "vTechNewCap"),
    rhs = data.frame(year = yrs, rhs = 1e6),
    defVal = 1e6
  )
  mod <- add(mod, cns)

  scen <- suppressWarnings(suppressMessages(
    interp_mod(mod, name = "uc", ondisk = FALSE)
  ))

  # IR present and well-formed
  expect_true("MAXNEWCAP" %in% names(scen@modInp@gams.equation))
  eq <- scen@modInp@gams.equation[["MAXNEWCAP"]]$equation
  expect_match(eq, "eqCnsMAXNEWCAP")
  expect_match(eq, "vTechNewCap")
  expect_match(eq, "=l=", fixed = TRUE) # "<=" renders as GAMS =l=

  # supporting for-each membership map built
  expect_true("mCnsForEachMAXNEWCAP" %in% names(scen@modInp@parameters))

  # translates to each backend (pure-R translators; no solver binary needed)
  translators <- list(
    GLPK  = .equation.from.gams.to.glpk,
    JuMP  = .equation.from.gams.to.julia,
    Pyomo = .equation.from.gams.to.pyomo
  )
  for (nm in names(translators)) {
    out <- paste(translators[[nm]](eq), collapse = "\n")
    expect_true(nzchar(out), info = nm)
    # The constraint name appears per-backend either as the equation name
    # (eqCnsMAXNEWCAP, GLPK/Pyomo) or via its for-each map (mCnsForEachMAXNEWCAP,
    # JuMP @constraint), so assert on the constraint name itself.
    expect_match(out, "MAXNEWCAP", info = nm)
    expect_match(out, "vTechNewCap", info = nm)
  }
})

test_that("a constraint with only `defVal` (no rhs data.frame) uses it as a constant RHS", {
  skip_if_no_fixtures()
  env <- .mapping_fixture_env()
  mod <- env$tm_core()

  yrs <- sort(unique(as.integer(mod@config@horizon@intervals$mid)))
  cns <- newConstraint(
    name = "MAXINV2",
    eq = "<=",
    for.each = data.frame(year = yrs),
    term1 = list(variable = "vTechNewCap"),
    defVal = 500 # no `rhs` data.frame -> constant RHS from defVal
  )
  mod <- add(mod, cns)

  scen <- suppressWarnings(suppressMessages(
    interp_mod(mod, name = "dv", ondisk = FALSE)
  ))
  eq <- scen@modInp@gams.equation[["MAXINV2"]]$equation
  expect_match(eq, "=l= 500", fixed = TRUE) # defVal becomes the literal RHS
  # no pCnsRhs parameter is built for a constant RHS
  expect_false("pCnsRhsMAXINV2" %in% names(scen@modInp@parameters))
})

test_that("a summand `timeframe` restricts the variable to that slice level (retires *RY)", {
  skip_if_no_fixtures()
  env <- .mapping_fixture_env()

  build <- function(tf) {
    mod <- env$tm_core() # calendar: ANNUAL + 4 seasons
    yrs <- sort(unique(as.integer(mod@config@horizon@intervals$mid)))
    cns <- newConstraint(
      name = "TFLIM", eq = "<=",
      for.each = data.frame(year = yrs),
      term1 = list(variable = "vOutTot", timeframe = tf),
      defVal = 1e6
    )
    mod <- add(mod, cns)
    suppressWarnings(suppressMessages(interp_mod(mod, name = "tf", ondisk = FALSE)))
  }
  slice_map_values <- function(scen) {
    m <- grep("^mCnsTFLIM_", names(scen@modInp@parameters), value = TRUE)
    expect_length(m, 1)
    sort(unique(unlist(get_data_slot(scen@modInp@parameters[[m]]))))
  }
  seasons <- sort(env$tm_core()@config@calendar@timeframes$SEASON)

  # ANNUAL -> the variable is taken only at the single ANNUAL slice
  expect_identical(slice_map_values(build("ANNUAL")), "ANNUAL")

  # SEASON -> only the season slices (no ANNUAL => no cross-level double count)
  vs <- slice_map_values(build("SEASON"))
  expect_false("ANNUAL" %in% vs)
  expect_identical(vs, seasons)

  # an unknown timeframe is reported clearly
  modx <- env$tm_core()
  yrs <- sort(unique(as.integer(modx@config@horizon@intervals$mid)))
  cx <- newConstraint(
    name = "TFBAD", eq = "<=", for.each = data.frame(year = yrs),
    term1 = list(variable = "vOutTot", timeframe = "NOPE"), defVal = 1
  )
  modx <- add(modx, cx)
  expect_error(
    suppressWarnings(suppressMessages(interp_mod(modx, name = "b", ondisk = FALSE))),
    "unknown timeframe"
  )
})
