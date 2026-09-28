# =============================================================================
# energyRt user constraints -> multimod equations
# =============================================================================
#
# Moved here from multimod on 2026-09-24 with the code under test. These cover
# the GAMS string IR that interpolation compiles onto
# `modInp@user_constraints`: splitting a statement, parsing it into an AST
# equation, and refusing a wildcard that would silently take a default.
#
# multimod is a Suggests, so every block guards on it.
# =============================================================================

# =============================================================================
# energyRt user constraints, and the gating-map ordering they depend on
# =============================================================================
#
# `newConstraint()` objects reach a backend only as a compiled GAMS string on
# `modInp@user_constraints`, together with the mCns*/pCns* parameters that
# interpolation materialised. multimod parses that string with the GAMS parser
# it already has.
#
# Building an S4 energyRt scenario here would pull in the whole package, so the
# tests exercise the parse and evaluation path directly and let the real
# scenario be the integration check.
# =============================================================================

IR <- paste0(
  "eqCnsTEST(region, year)$mCnsForEachTEST(region, year)..   ",
  "sum(tech$(mCnsTEST_1(tech) and mTechSpan(tech, region, year)), ",
  "1 * pCnsMultTEST_1(tech) * vTechCap(tech, region, year)) ",
  "=l= pCnsRhsTEST(region, year);"
)

# The R7 fixture, plus the symbols a user constraint brings with it.
fixture_with_constraint <- function(mult = c(EBIO = 2, ECOA = 3),
                                    rhs_value = 100) {
  data(example_models, package = "multimod")
  m <- example_models$energyRt$multimod

  span <- as.data.frame(multimod::get_data(m, "mTechSpan", type = "mapping"))
  for_each <- unique(span[, c("region", "year")])
  techs <- names(mult)

  m$mappings$mCnsForEachTEST <- multimod::new_mapping(
    "mCnsForEachTEST", desc = "row set", dims = c("region", "year"),
    active_dims = c("region", "year"), data = for_each)
  m$mappings$mCnsTEST_1 <- multimod::new_mapping(
    "mCnsTEST_1", desc = "for.sum restriction", dims = "tech",
    active_dims = "tech", data = data.frame(tech = techs))
  m$parameters$pCnsMultTEST_1 <- multimod::new_parameter(
    "pCnsMultTEST_1", desc = "coefficients", dims = "tech",
    active_dims = "tech",
    data = data.frame(tech = techs, value = unname(mult)), defVal = 1)
  m$parameters$pCnsRhsTEST <- multimod::new_parameter(
    "pCnsRhsTEST", desc = "rhs", dims = c("region", "year"),
    active_dims = c("region", "year"),
    data = cbind(for_each, value = rhs_value), defVal = Inf)

  eq <- .parse_cns_equation(IR, multimod::build_symbols_list(m), desc = "test")
  m$equations[[eq$name]] <- eq
  list(model = m, for_each = for_each, span = span, mult = mult,
       rhs_value = rhs_value)
}


test_that("a GAMS statement splits at the first '..'", {
  skip_if_not_installed("multimod")
  s <- .split_gams_statement("eqX(r)$mY(r)..  a =l= b;")
  expect_equal(s$header, "eqX(r)$mY(r)")
  expect_equal(s$body, "a =l= b")

  # a decimal point must not be mistaken for the separator
  s2 <- .split_gams_statement("eqX(r).. 1.5 * a =e= 0.25;")
  expect_equal(s2$header, "eqX(r)")
  expect_equal(s2$body, "1.5 * a =e= 0.25")

  expect_error(.split_gams_statement("eqX(r) a =l= b;"), "no '\\.\\.'")
})

test_that("a user-constraint IR string parses into an equation", {
  skip_if_not_installed("multimod")
  data(example_models, package = "multimod")
  m <- fixture_with_constraint()$model
  eq <- m$equations$eqCnsTEST

  expect_s3_class(eq, "equation")
  expect_equal(eq$name, "eqCnsTEST")
  expect_equal(eq$relation, "<=")
  # the $-condition of the header becomes the equation's domain mapping, which
  # is what build_row_index() reads to size the row block
  expect_equal(eq$domain$name, "mCnsForEachTEST")
  expect_equal(vapply(eq$dims, multimod:::dim_binding_name, character(1)),
               c("region", "year"))
  expect_equal(eq$rhs$name, "pCnsRhsTEST")
})

test_that("the constraint contributes exactly its domain's rows", {
  skip_if_not_installed("multimod")
  data(example_models, package = "multimod")
  f <- fixture_with_constraint()
  base <- multimod::build_row_index(example_models$energyRt$multimod)
  ri <- multimod::build_row_index(f$model)

  expect_equal(nrow(ri) - nrow(base), nrow(f$for_each))
  expect_equal(sum(ri$symbol == "eqCnsTEST"), nrow(f$for_each))
  expect_true(all(ri$sense[ri$symbol == "eqCnsTEST"] == "<="))
})

test_that("coefficients and rhs match the constraint's own tables", {
  skip_if_not_installed("multimod")
  skip_if_not_installed("highs")
  data(example_models, package = "multimod")
  f <- fixture_with_constraint()
  lp <- multimod::model_to_lp(f$model)

  ri <- lp$row_index
  ci <- lp$col_index
  i <- ri$i[ri$symbol == "eqCnsTEST"][1]
  reg <- ri$i1[ri$i == i]
  yr <- ri$i2[ri$i == i]

  # which technologies should appear: in the for.sum restriction AND in the
  # variable's own gating map for this (region, year)
  span <- f$span
  want <- intersect(
    names(f$mult),
    span$tech[as.character(span$region) == reg & as.character(span$year) == yr])
  expect_gt(length(want), 0)

  got <- Matrix::which(lp$A[i, ] != 0)
  expect_equal(sort(ci$i1[match(got, ci$j)]), sort(want))
  expect_true(all(ci$symbol[match(got, ci$j)] == "vTechCap"))
  expect_equal(unname(lp$A[i, got]), unname(f$mult[ci$i1[match(got, ci$j)]]))

  # `=l=` gives an upper bound only, at the rhs parameter's value
  expect_equal(lp$row_up[i], f$rhs_value)
  expect_equal(lp$row_lo[i], -Inf)
})

test_that(".has_wildcard looks at index columns only", {
  skip_if_not_installed("multimod")
  expect_false(.has_wildcard(
    data.frame(tech = "A", region = "R1", value = 1)))
  expect_true(.has_wildcard(
    data.frame(tech = "A", region = NA_character_, value = 1)))
  # a missing VALUE is not a wildcard - only an index can stand for "all"
  expect_false(.has_wildcard(
    data.frame(tech = "A", region = "R1", value = NA_real_)))
  expect_false(.has_wildcard(data.frame()))
  expect_false(.has_wildcard(NULL))
})

test_that("a sum over a tuple of free indices carries coefficients", {
  skip_if_not_installed("multimod")
  data(example_models, package = "multimod")
  m <- example_models$energyRt$multimod

  # sum over (tech, region) -- two free indices -- gated by mTechSpan, with the
  # equation indexed by year alone. No for.sum, no pCnsMult, literal RHS: the
  # bare shape energyRt emits for `term1 = list(variable = "vTechCap")`.
  span <- as.data.frame(multimod::get_data(m, "mTechSpan", type = "mapping"))
  yrs <- unique(span[, "year", drop = FALSE])
  m$mappings$mCnsForEachTUP <- multimod::new_mapping(
    "mCnsForEachTUP", desc = "row set", dims = "year",
    active_dims = "year", data = yrs)

  ir <- paste0(
    "eqCnsTUP(year)$mCnsForEachTUP(year)..   ",
    "sum((tech, region)$mTechSpan(tech, region, year), ",
    "vTechCap(tech, region, year)) =l= 100;")
  eq <- .parse_cns_equation(ir, multimod::build_symbols_list(m), desc = "tuple")
  m$equations[[eq$name]] <- eq

  lp <- multimod::model_to_lp(m)
  rows <- which(lp$row_index$symbol == "eqCnsTUP")
  expect_gt(length(rows), 0)

  # the defect: rows present, RHS right, matrix empty
  nz <- Matrix::rowSums(abs(lp$A[rows, , drop = FALSE]) > 0)
  expect_true(all(nz > 0))
  expect_true(all(lp$row_up[rows] == 100))

  # and the package's own detector agrees
  expect_false("eqCnsTUP" %in%
                 multimod::check_matrix_numbers(lp, verbose = FALSE)$empty_row_symbols)
})

test_that("model_to_lp refuses a user constraint that binds nothing", {
  skip_if_not_installed("multimod")
  data(example_models, package = "multimod")
  m <- example_models$energyRt$multimod

  span <- as.data.frame(multimod::get_data(m, "mTechSpan", type = "mapping"))
  yrs <- unique(span[, "year", drop = FALSE])
  m$mappings$mCnsForEachVOID <- multimod::new_mapping(
    "mCnsForEachVOID", desc = "row set", dims = "year",
    active_dims = "year", data = yrs)
  # a gate map with no rows: the sum binds nothing, the row is still declared
  m$mappings$mVoidGate <- multimod::new_mapping(
    "mVoidGate", desc = "empty gate", dims = c("tech", "region", "year"),
    active_dims = c("tech", "region", "year"),
    data = span[0, c("tech", "region", "year")])

  ir <- paste0(
    "eqCnsVOID(year)$mCnsForEachVOID(year)..   ",
    "sum((tech, region)$mVoidGate(tech, region, year), ",
    "vTechCap(tech, region, year)) =l= 100;")
  eq <- .parse_cns_equation(ir, multimod::build_symbols_list(m), desc = "void")
  m$equations[[eq$name]] <- eq

  expect_error(multimod::model_to_lp(m), "eqCnsVOID")
  expect_error(multimod::model_to_lp(m), "constrain nothing")

  # the escape hatches still assemble, so a caller can inspect the matrix
  expect_warning(lp <- multimod::model_to_lp(m, on_empty_row = "warn"), "eqCnsVOID")
  expect_silent(lp2 <- multimod::model_to_lp(m, on_empty_row = "ignore"))
  expect_true("eqCnsVOID" %in%
                multimod::check_matrix_numbers(lp2, verbose = FALSE)$empty_row_symbols)
})
