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
  # 480 .modInp entries (13 set + 297 map + 133 numpar + 37 bounds)
  # + 148 equations + 95 variables = 723
  # (297 = 285 + the 12 mStorage*Comm{SameTimeslice,Agg,AggTimeslice} maps
  # added with the storage coarse-timeframe aggregation fix, 2026-08-25)
  expect_equal(unname(counts[["set"]]), 13)
  expect_equal(unname(counts[["map"]]), 297)
  expect_equal(unname(counts[["numpar"]]), 133)
  expect_equal(unname(counts[["bounds"]]), 37)
  expect_equal(unname(counts[["equation"]]), 148)
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
