# =========================================================================== #
# A variable that indexes one set twice must still be usable in a constraint.
#
# Six variables do: `vTradeIr` carries two `region` dimensions (the endpoints of
# a route) and the five `*RetiredNewCap` variables carry two `year`s (the
# milestone capacity was built in, and the one it is retired in).
#
# The generator named an index after its SET, so both positions came out with
# the same name:
#
#   sum((trade, comm, region, region, timeslice)$..., vTradeIr(...))
#
# GLPK refuses that -- "duplicate dummy index r not allowed" -- so none of the
# six could appear in a custom constraint AT ALL. And `for.sum`, being a named
# list keyed the same way, had no spelling that meant "this endpoint but not
# that one".
#
# `.variable_dim` now gives every position a distinct name, reusing the
# vocabulary the rest of the package already uses for it: `src` / `dst` for a
# route's endpoints, `vintage` for a build milestone.
# =========================================================================== #

# The three-region mesh from helper-trade.R: routes R1->R2, R2->R3, R1->R3 on a
# single object whose capacity is 10, demand 10 at R3, supply costing 1 at R1,
# 5 at R2 and 100 at R3. Unconstrained it ships 10 direct on R1->R3 for an
# objective of 10, so every number below is a departure from that.
dim_scen <- function(name, ...) suppressMessages(interpolate_model(
  add(tr_mesh_one(name = name),
      newConstraint(name = "DIMT",
                    for.each = data.frame(comm = "ELC", year = 2020L), ...)),
  name = name))

dim_eq <- function(name, ...) dim_scen(name, ...)@modInp@gams.equation[["DIMT"]]$equation

dim_obj <- function(name, ...) getData(
  suppressMessages(solve_scen(dim_scen(name, ...))), "vObjective",
  merge = TRUE)$value[1]

test_that("the two endpoints of `vTradeIr` get distinct index names", {
  eq <- dim_eq("dm1", eq = "<=", defVal = 6, t1 = list(variable = "vTradeIr"))
  # The sum tuple names each position once...
  expect_match(eq, "sum((trade, src, dst, timeslice)", fixed = TRUE)
  # ... and the variable and its gating map agree on the order.
  expect_match(eq, "vTradeIr(trade, comm, src, dst, year, timeslice)", fixed = TRUE)
  expect_match(eq, "mvTradeIr(trade, comm, src, dst, year, timeslice)", fixed = TRUE)
  # The defect this replaces, spelled out so a regression is recognisable.
  expect_false(grepl("region, region", eq, fixed = TRUE))
})

test_that("`for.sum` addresses one endpoint without the other", {
  eq <- dim_eq("dm2", eq = "<=", defVal = 6,
               t1 = list(variable = "vTradeIr",
                         for.sum = list(src = "R1", dst = "R3")))
  # TWO maps, one per endpoint -- a single map applied twice was the old bug.
  expect_match(eq, "mCnsDIMT_1(src)", fixed = TRUE)
  expect_match(eq, "mCnsDIMT_2(dst)", fixed = TRUE)

  # And they hold the region each was given, not the same set twice.
  pars <- dim_scen("dm3", eq = "<=", defVal = 6,
                   t1 = list(variable = "vTradeIr",
                             for.sum = list(src = "R1", dst = "R3")))@modInp@parameters
  expect_equal(as.character(pars[["mCnsDIMT_1"]]@data$region), "R1")
  expect_equal(as.character(pars[["mCnsDIMT_2"]]@data$region), "R3")
})

test_that("naming the SET rather than the position is refused, with the names", {
  # `region` is genuinely ambiguous here, so it is an error rather than a guess
  # -- and the message lists what to write instead.
  expect_error(
    newConstraint(name = "X", eq = "<=", defVal = 1,
                  for.each = data.frame(year = 2020L),
                  t1 = list(variable = "vTradeIr",
                            for.sum = list(region = "R1"))),
    "indexes \"region\" more than once")
  expect_error(
    newConstraint(name = "X", eq = "<=", defVal = 1,
                  for.each = data.frame(year = 2020L),
                  t1 = list(variable = "vTradeIr",
                            for.sum = list(region = "R1"))),
    "src, dst")
  # A variable with no repeated set is unaffected: `region` still means region.
  expect_silent(newConstraint(name = "X", eq = "<=", defVal = 1,
                              for.each = data.frame(year = 2020L),
                              t1 = list(variable = "vExportTot",
                                        for.sum = list(region = "R1"))))
})

test_that("the constraint reaches the solver and binds", {
  skip_if_no_solver()
  # Unconstrained.
  expect_equal(getData(tr_solve(tr_mesh_one(name = "dm4")), "vObjective",
                       merge = TRUE)$value[1], 10)

  # Total flow over every route <= 6: R1 ships 6 at cost 1 and R3 buys the
  # remaining 4 from itself at 100.
  expect_equal(dim_obj("dm5", eq = "<=", defVal = 6,
                       t1 = list(variable = "vTradeIr")),
               6 * 1 + 4 * 100)

  # Only the DIRECT leg <= 6. The other endpoints are untouched, so R2 sells 4
  # of its own on the mesh's remaining budget instead of R3 self-supplying --
  # a cheaper answer, reachable only if the restriction landed on one route.
  expect_equal(dim_obj("dm6", eq = "<=", defVal = 6,
                       t1 = list(variable = "vTradeIr",
                                 for.sum = list(src = "R1", dst = "R3"))),
               6 * 1 + 4 * 5)

  # Two terms, each on its own route, with different coefficients:
  # f(R1->R2) + 2 f(R1->R3) <= 6. The direct leg is worth two, so it carries 3
  # and R2 sells the remaining 7 of the shared budget of 10.
  expect_equal(dim_obj("dm7", eq = "<=", defVal = 6,
                       t1 = list(variable = "vTradeIr",
                                 for.sum = list(src = "R1", dst = "R2"), mult = 1),
                       t2 = list(variable = "vTradeIr",
                                 for.sum = list(src = "R1", dst = "R3"), mult = 2)),
               3 * 1 + 7 * 5)
})

test_that("the back-ends agree on a two-endpoint constraint", {
  skip_if_no_solver()
  # The index names are written independently into each template, so a
  # transcription error shows up as one back-end disagreeing. GAMS needs a dense
  # re-interpolation and a NEOS round trip and is checked in the manual sweep.
  scen <- dim_scen("dm8", eq = "<=", defVal = 6,
                   t1 = list(variable = "vTradeIr",
                             for.sum = list(src = "R1", dst = "R3")))
  got <- vapply(c("glpk", "julia_highs", "pyomo_glpk"), function(e) {
    r <- try(suppressMessages(solve_scen(scen, solver = solver_options[[e]])),
             silent = TRUE)
    if (inherits(r, "try-error")) NA_real_
    else getData(r, "vObjective", merge = TRUE)$value[1]
  }, numeric(1))
  expect_equal(unname(got), rep(26, 3))
})

test_that("the vintaging variables get `vintage` and `year`", {
  # The same defect, and the same fix, for the five *RetiredNewCap variables:
  # the first year is the milestone the capacity was built in, the second the
  # one it is retired in.
  expect_equal(energyRt:::.variable_dim[["vTradeRetiredNewCap"]],
               c("trade", "vintage", "year"))
  expect_equal(energyRt:::.variable_dim[["vTechRetiredNewCap"]],
               c("tech", "region", "vintage", "year"))
  # No variable may still repeat a dimension NAME -- that is the invariant the
  # whole registry exists to hold.
  expect_false(any(vapply(energyRt:::.variable_dim,
                          function(x) any(duplicated(x)), logical(1))))
  # ... while `.variable_set` keeps reporting the SET, which domain lookups need.
  expect_equal(energyRt:::.variable_set[["vTradeIr"]],
               c("trade", "comm", "region", "region", "year", "timeslice"))
})
