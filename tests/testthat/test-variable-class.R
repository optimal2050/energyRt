# The `variable` class: `modOut@variables` is a named list of typed objects,
# mirroring `modInp@parameters`.
#
# Several assertions here guard failure modes that are SILENT rather than loud,
# which is why they are worth pinning:
#   * `get_lazy_dim_names()` returning NULL column names for an S4 element makes
#     `findData()`'s value-column filter drop every variable, so `getData()`
#     comes back empty with no error.
#   * a missing `.save_slots$variable` makes `save_scenario()` report success
#     having written no results at all.
#   * the on-disk layout moved a variable's data one level down, and reading an
#     older directory returns NULL rather than erroring.

vc_model <- function(name = "vc") {
  cal <- newCalendar(timetable = make_timetable(
    struct = list(ANNUAL = "ANNUAL", SEASON = c("WIN", "SPR", "SUM", "AUT"))),
    name = paste0("cal_", name))
  newModel(
    name = name, desc = "", calendar = cal, region = c("R1", "R2"),
    discount = 0.05,
    horizon = newHorizon(2020:2039, intervals = c(1, rep(10, 2))),
    repo = newRepository(
      paste0("repo_", name),
      newCommodity("COA", timeframe = "ANNUAL"),
      newCommodity("ELC", timeframe = "SEASON"),
      newSupply("S", commodity = "COA",
                supply = data.frame(region = c("R1", "R2"), cost = 1)),
      newDemand("D", commodity = "ELC",
                demand = data.frame(region = rep(c("R1", "R2"), each = 2),
                                 timeslice = c("WIN", "SUM"), demand = 30)),
      newTechnology("ECOA", input = list(comm = "COA"),
                    output = list(comm = "ELC"),
                    invcost = data.frame(invcost = 1000),
                    vintage = data.frame(olife = 30L), cap2act = 1),
      newTrade("TBD", commodity = "ELC",
               routes = data.frame(src = "R1", dst = "R2"),
               invcost = data.frame(invcost = 100),
               vintage = data.frame(olife = 30L), cap2act = 1)))
}
vc_interp <- function(m, n) suppressMessages(suppressWarnings(
  interpolate_model(m, name = n, fold = TRUE)))
vc_solve <- function(s) suppressMessages(suppressWarnings(
  solve_scen(s, solver = solver_options$glpk, wait = TRUE)))

# ---------------------------------------------------------------------------- #
# the specification
# ---------------------------------------------------------------------------- #

test_that("the spec covers every declared variable and nothing invented", {
  V  <- getFromNamespace(".variables", "energyRt")
  vm <- getFromNamespace(".variable_mapping", "energyRt")
  expect_true(all(names(vm) %in% names(V)))
  # Anything beyond the declared set must say it is computed in R.
  extra <- setdiff(names(V), names(vm))
  expect_true(all(vapply(V[extra], function(x) x$origin == "computed",
                         logical(1))))
})

test_that("@dimSets is byte-identical to .variable_set", {
  # Not a preference: `newCosts()` and `newConstraint()` consume these
  # positionally against the GAMS declaration string, duplicates and all.
  V  <- getFromNamespace(".variables", "energyRt")
  vs <- getFromNamespace(".variable_set", "energyRt")
  for (v in names(vs)) expect_identical(V[[v]]$dimSets, vs[[v]], info = v)
})

test_that("@colNames matches what the GLPK writer actually emits", {
  V <- getFromNamespace(".variables", "energyRt")
  mc <- getFromNamespace(".modelCode", "energyRt")
  ln <- mc$GLPK
  i <- grep('^printf "[^"]*value\\\\n" > "output/v[A-Za-z0-9]+[.]csv";$', ln)
  hd <- sub('^printf "([^"]*)\\\\n".*$', "\\1", ln[i])
  nm <- sub('^.*output/(v[A-Za-z0-9]+)[.]csv.*$', "\\1", ln[i])
  for (k in seq_along(nm)) {
    expect_identical(V[[nm[k]]]$colNames,
                     setdiff(strsplit(hd[k], ",", fixed = TRUE)[[1]], "value"),
                     info = nm[k])
  }
  # The only two that differ from the model dims are the duplicate-dim cases.
  differs <- names(V)[vapply(V, function(x)
    !identical(x$colNames, x$dimSets), logical(1))]
  expect_setequal(differs, c("vTradeIr", "vTechRetiredNewCap"))
})

test_that("a scalar variable has no dimensions", {
  V <- getFromNamespace(".variables", "energyRt")
  # `.variable_set` derives dims by stripping what is outside the parentheses,
  # and a scalar has none -- which used to yield the variable's own NAME as its
  # dimension, and would now fail every `new("modOut")`.
  expect_length(V$vObjective$dimSets, 0L)
  expect_equal(V$vObjective$gate, "")
})

# ---------------------------------------------------------------------------- #
# the class
# ---------------------------------------------------------------------------- #

test_that("modOut pre-populates one typed variable per declared variable", {
  m <- new("modOut")
  V <- getFromNamespace(".variables", "energyRt")
  expect_length(m@variables, length(V))
  expect_true(all(vapply(m@variables, function(x) inherits(x, "variable"),
                         logical(1))))
  expect_true(all(vapply(m@variables, function(x) nrow(x@data) == 0,
                         logical(1))))
})

test_that("parameter and variable share the modelData contract", {
  p <- new("parameter", "pTechCap2act", "tech", type = "numpar")
  v <- new("modOut")@variables$vTechCap
  expect_true(is(p, "modelData"))
  expect_true(is(v, "modelData"))
  # the existing `inherits(x, "parameter")` call sites must keep working
  expect_true(inherits(p, "parameter"))
  expect_false(inherits(v, "parameter"))
})

test_that("the data skeleton disambiguates repeated dimensions", {
  v <- new("modOut")@variables$vTradeIr
  expect_identical(v@dimSets,
                   c("trade", "comm", "region", "region", "year", "timeslice"))
  expect_identical(names(v),
                   c("trade", "comm", "src", "dst", "year", "timeslice", "value"))
  y <- new("modOut")@variables$vTechRetiredNewCap
  expect_identical(names(y), c("tech", "region", "year", "yearp", "value"))
  # `yearp` stands for a `year` dimension, so it must be integer, not character
  expect_true(is.integer(get_data_slot(y)$yearp))
})

test_that("newVariable validates its inputs", {
  nv <- getFromNamespace("newVariable", "energyRt")
  expect_error(nv("vX", dimSets = "nosuchdim"), "Unknown dimSets")
  expect_error(nv("vX", dimSets = "tech", role = "nonsense"), "unknown role")
  expect_error(nv("vX", dimSets = "tech", unit = "furlongs"), "unknown unit")
  expect_error(nv("vX", dimSets = "tech", origin = "magic"), "solver")
  # a repeated dim is disambiguated by default
  expect_identical(nv("vX", dimSets = c("year", "year"))@colNames,
                   c("year", "year2"))
})

test_that("print names the class it is printing", {
  v <- new("modOut")@variables$vTechCap
  expect_output(print(v), 'variable "vTechCap" is empty')
  p <- new("parameter", "pTechCap2act", "tech", type = "numpar")
  expect_output(print(p), 'parameter "pTechCap2act" is empty')
})

# ---------------------------------------------------------------------------- #
# end to end
# ---------------------------------------------------------------------------- #

test_that("a solved scenario fills its variables and getData reads them", {
  skip_if_no_solver()
  sol <- vc_solve(vc_interp(vc_model("vc1"), "vc1"))
  expect_equal(sol@modOut@stage, "solved")

  # THE trap: NULL column names would empty this without any error.
  d <- suppressMessages(getData(sol, "vTechCap", merge = TRUE))
  expect_true(!is.null(d) && nrow(d) > 0)
  expect_gt(length(suppressMessages(findData(sol, dataType = "variables"))), 10L)

  v <- sol@modOut@variables$vTechCap
  expect_s4_class(v, "variable")
  expect_equal(v@role, "capacity")
  expect_equal(v@gate, "mTechSpan")
  expect_true(v@positive)

  # compatibility shim for `...$vX$value`
  expect_true(is.numeric(v$value) && length(v$value) > 0)
  expect_gt(nrow(v), 0L)
  expect_true(is.data.frame(as.data.frame(v)))

  # A variable the solver skipped keeps its typed, empty skeleton -- so its
  # columns are known even with no rows, which a bare list could not express.
  sk <- sol@modOut@variables$vStorageOutCap
  expect_equal(nrow(get_data_slot(sk)), 0L)
  expect_identical(names(sk), c("stg", "region", "year", "value"))
})

test_that("saving and loading preserves a variable's rows", {
  skip_if_no_solver()
  sol <- vc_solve(vc_interp(vc_model("vc2"), "vc2"))
  n <- nrow(suppressMessages(getData(sol, "vTechCap", merge = TRUE)))

  p <- file.path(tempdir(), "vc_save")
  unlink(p, recursive = TRUE)
  on.exit(unlink(p, recursive = TRUE), add = TRUE)
  sol@path <- p
  suppressMessages(suppressWarnings(save_scenario(sol, path = p,
                                                  verbose = FALSE)))
  # Without `.save_slots$variable` nothing is written and this directory is absent
  expect_true(dir.exists(file.path(p, "modOut", "variables", "vTechCap",
                                   "data")))
  expect_equal(readLines(file.path(p, "layout"), warn = FALSE)[1], "2")

  l <- suppressMessages(suppressWarnings(
    load_scenario(p, env = NULL, verbose = FALSE)))
  expect_equal(nrow(suppressMessages(getData(l, "vTechCap", merge = TRUE))), n)

  # An older directory stores the data one level up; reading it would return
  # NULL rather than erroring, so the layout marker must refuse it.
  unlink(file.path(p, "layout"))
  expect_error(
    suppressMessages(load_scenario(p, env = NULL, verbose = FALSE)),
    "layout")
})

test_that("an unsolved scenario still reports as unsolved", {
  skip_if_no_solver()
  # `modOut` now always holds a full set of (empty) variables, so the old
  # `length(@variables) > 0` test would call a failed solve "solved".
  sc <- vc_interp(vc_model("vc3"), "vc3")
  d <- suppressMessages(getData(sc, "vTechCap", merge = TRUE))
  expect_true(is.null(d) || nrow(d) == 0)
  expect_null(suppressMessages(levcost(sc, name = "ECOA")))
})
