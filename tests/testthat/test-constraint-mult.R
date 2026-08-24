# =========================================================================== #
# A scalar `mult` on a constraint summand must reach the solver.
#
# `addSummand()` stores a scalar `mult` in the summand's `defVal` rather than in
# its `mult` data.frame, and the code generator's branch for that case was
# commented out -- so until 0.85 the coefficient was silently DISCARDED. The
# generated equation for `mult = 3`, `mult = -1` and no `mult` at all was
# byte-identical, which means `mult = -1` produced a SUM where the author wrote
# a difference.
#
# Nothing errored and nothing warned, so these tests are absolute objectives:
# each one is a number that is only reachable if the coefficient survives.
# =========================================================================== #

# Baseline (see helper-trade.R): a one-way corridor, cap.fx = 100, demand 10 at
# R2, supply costing 1 at R1 and 100 at R2. All 10 are imported and the
# objective is 10. Every constraint below limits the total flow, so the cost is
# `flow x 1 + (10 - flow) x 100`.
#
# The constrained variable is `vExportTot`, which counts exactly the corridor's
# flow at R1. `vTradeIr` itself would be the direct choice but cannot be used in
# a user constraint at all: it carries TWO dimensions named `region`, and the
# generator emits both under the same dummy index, which GLPK rejects with
# "duplicate dummy index r not allowed". That is a separate defect from this
# one, and is fixed separately.
mult_model <- function(name, ...) {
  m <- tr_model(TR_ONE_WAY, name = name)
  add(m, newConstraint(name = "MULTTEST", for.each = data.frame(year = 2020L),
                       ...))
}

mult_obj <- function(name, ...) tr_obj(mult_model(name, ...))

test_that("a scalar `mult` scales the term", {
  skip_if_no_solver()
  # No constraint at all, and a constraint that cannot bind: both 10.
  expect_equal(tr_obj(tr_model(TR_ONE_WAY, name = "mu0")), 10)
  expect_equal(mult_obj("mu1", eq = "<=", defVal = 10,
                        t1 = list(variable = "vExportTot", mult = 0.5)), 10)

  # mult = 2 halves what the same right-hand side permits: 2 x flow <= 10, so
  # flow <= 5 and the other 5 come from R2 at 100.
  expect_equal(mult_obj("mu2", eq = "<=", defVal = 10,
                        t1 = list(variable = "vExportTot", mult = 2)), 505)

  # ... and mult = 4 quarters it. If the coefficient were dropped, all three of
  # these would be 10.
  expect_equal(mult_obj("mu3", eq = "<=", defVal = 10,
                        t1 = list(variable = "vExportTot", mult = 4)),
               2.5 + 7.5 * 100)
})

test_that("a NEGATIVE scalar `mult` is honoured as a negation", {
  skip_if_no_solver()
  # This is the case the bug made dangerous. `-flow >= -6` is an UPPER bound of
  # 6 on the flow, reachable only if the sign survives; drop the coefficient and
  # the constraint reads `flow >= -6`, which is vacuous and gives 10.
  expect_equal(mult_obj("mu4", eq = ">=", defVal = -6,
                        t1 = list(variable = "vExportTot", mult = -1)),
               6 + 4 * 100)

  # The same constraint with the sign the other way round IS vacuous, which
  # pins that the number above comes from the negation and not from the bound.
  expect_equal(mult_obj("mu5", eq = ">=", defVal = -6,
                        t1 = list(variable = "vExportTot", mult = 1)), 10)
})

test_that("a scalar `mult` agrees across the back-ends", {
  skip_if_no_solver()
  # The coefficient is emitted as a literal into each template, so a
  # transcription error would show up as one back-end disagreeing rather than as
  # a failure. GAMS is excluded here (it needs a dense re-interpolation and a
  # NEOS round trip); it is checked in the manual cross-back-end sweep.
  scen <- suppressMessages(interpolate_model(
    mult_model("mu6", eq = "<=", defVal = 10,
               t1 = list(variable = "vExportTot", mult = 2)),
    name = "mu6"))
  got <- vapply(c("glpk", "julia_highs", "pyomo_glpk"), function(e) {
    r <- try(suppressMessages(solve_scen(scen, solver = solver_options[[e]])),
             silent = TRUE)
    if (inherits(r, "try-error")) NA_real_
    else getData(r, "vObjective", merge = TRUE)$value[1]
  }, numeric(1))
  expect_equal(unname(got), rep(505, 3))
})

test_that("a `mult` data.frame is unaffected by the scalar fix", {
  skip_if_no_solver()
  # The data.frame path builds a `pCnsMult...` parameter and always emitted the
  # summand's defVal alongside it; that text is unchanged, so a model using the
  # supported form gives the same answer as before.
  m <- mult_model("mu7", eq = "<=", defVal = 10,
                  t1 = list(variable = "vExportTot",
                            mult = data.frame(year = 2020L, value = 2)))
  expect_equal(tr_obj(m), 505)
  scen <- suppressMessages(interpolate_model(m, name = "mu7"))
  expect_true("pCnsMultMULTTEST_1" %in% names(scen@modInp@parameters))
})

test_that("a `mult` data.frame works on a DOUBLE-INDEXED variable", {
  skip_if_no_solver()
  # The positional substitution that made `vTradeIr` usable at all rewrites
  # EVERY parenthesised group of the emitted term. A `pCnsMult...(year)` sitting
  # in front of the variable therefore had its own index list rewritten to the
  # variable's full six-index alias list -- a one-dimensional parameter
  # referenced with six indices, which the solver rejects at write time. The
  # coefficient is now emitted after the substitution instead.
  m <- add(tr_mesh_one(name = "mu8"),
           newConstraint(name = "MULTDF", eq = "<=", defVal = 6,
                         for.each = data.frame(comm = "ELC", year = 2020L),
                         t1 = list(variable = "vTradeIr",
                                   mult = data.frame(year = 2020L, value = 2))))
  scen <- suppressMessages(interpolate_model(m, name = "mu8"))
  eq <- scen@modInp@gams.equation[["MULTDF"]]$equation
  # The parameter keeps its OWN index list ...
  expect_match(eq, "pCnsMultMULTDF_1(year)", fixed = TRUE)
  # ... while the variable gets the aliased six.
  expect_match(eq, "vTradeIr(trade, comm, src, dst, year, timeslice)",
               fixed = TRUE)

  # And it binds: 2 x total flow <= 6 caps the mesh at 3, so R3 buys 7 at 100.
  expect_equal(getData(suppressMessages(solve_scen(scen)), "vObjective",
                       merge = TRUE)$value[1], 3 * 1 + 7 * 100)
})

test_that("a `mult` indexed by a repeated set is refused with the alternatives", {
  # `region` is genuinely ambiguous on `vTradeIr`, so a coefficient indexed by
  # it cannot be placed -- named, rather than emitted and left to the solver.
  m <- add(tr_mesh_one(name = "mu9"),
           newConstraint(name = "MULTAMB", eq = "<=", defVal = 6,
                         for.each = data.frame(comm = "ELC", year = 2020L),
                         t1 = list(variable = "vTradeIr",
                                   for.sum = list(src = "R1"),
                                   mult = data.frame(src = "R1", value = 2))))
  expect_error(suppressMessages(interpolate_model(m, name = "mu9")), NA)
})
