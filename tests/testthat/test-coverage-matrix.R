# =========================================================================== #
# Coverage-matrix integrity: the generator assembles from package metadata and
# every `# @covers` tag in the test suite resolves to a known row name.
# The generator lives in tools/coverage/build_matrix.R (Rbuildignored), so
# these tests skip on an installed package without the source tree.
# =========================================================================== #

.matrix_tool <- (function() {
  candidates <- c(
    testthat::test_path("..", "..", "tools", "coverage", "build_matrix.R"),
    file.path("tools", "coverage", "build_matrix.R")
  )
  for (p in candidates) if (file.exists(p)) return(normalizePath(p))
  NULL
})()

skip_if_no_matrix_tool <- function() {
  if (is.null(.matrix_tool)) {
    testthat::skip("tools/coverage/build_matrix.R not available (installed package)")
  }
}

.matrix_env <- new.env(parent = globalenv())
.matrix_loaded <- FALSE
load_matrix_tool <- function() {
  if (!.matrix_loaded) {
    sys.source(.matrix_tool, envir = .matrix_env)
    assign(".matrix_loaded", TRUE, envir = parent.env(environment()))
  }
  .matrix_env
}

test_that("matrix rows assemble from package metadata with expected arithmetic", {
  skip_if_no_matrix_tool()
  env <- load_matrix_tool()
  rows <- env$assemble_rows()
  counts <- table(rows$kind)
  # 503 .modInp entries (15 set + 309 map + 141 numpar + 38 bounds)
  # + 150 equations + 95 variables = 748
  # (15 sets = 13 + `process`, the union of the seven process classes, declared
  # on every backend and the GAMS parent of tech/stg/sup/dem/expp/imp/trade)
  # (297 = 285 + the 12 mStorage*Comm{SameTimeslice,Agg,AggTimeslice} maps
  # added with the storage coarse-timeframe aggregation fix, 2026-08-25;
  # 308/141/38/150 = + inp2stg (1 bound, 3 maps, 2 equations) + the per-part
  # aux capacity couplings (12 params and maps replacing 4), 2026-09-03;
  # 309 maps = + mWeatherRegionAt, which shares a parent-level weather series
  # down to its child regions -- read by the af/afs/afc equations on all four
  # backends, 2026-09-21)
  # (15 sets = + `levy`, the shared index of tax and subsidy instruments; it
  # re-dimensions the six pTaxCost*/pSubCost* parameters rather than adding any,
  # so numpar stays 141 -- 2026-10-08)
  expect_equal(unname(counts[["set"]]), 15)
  expect_equal(unname(counts[["map"]]), 309)
  expect_equal(unname(counts[["numpar"]]), 141)
  expect_equal(unname(counts[["bounds"]]), 38)
  expect_equal(unname(counts[["equation"]]), 150)
  expect_gte(unname(counts[["variable"]]), 93)
  expect_false(any(duplicated(rows$name[rows$kind != "variable"])))
  # every bounds row expands to a Lo/Up pXxx pair
  b <- rows[rows$kind == "bounds", ]
  expect_true(all(grepl("Lo,.*Up$", b$pXxx_expanded)))
  # no numeric parameter falls through to a fallback family (maps/eqs/vars may)
  np <- rows[rows$kind %in% c("numpar", "bounds"), ]
  expect_false(any(grepl("^other", np$family)))
})

test_that("all @covers tags in the suite resolve to known names", {
  skip_if_no_matrix_tool()
  env <- load_matrix_tool()
  expect_true(isTRUE(env$check_tags(dir = testthat::test_path("."))))
})
