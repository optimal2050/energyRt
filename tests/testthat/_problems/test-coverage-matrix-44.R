# Extracted from test-coverage-matrix.R:44

# prequel ----------------------------------------------------------------------
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

# test -------------------------------------------------------------------------
skip_if_no_matrix_tool()
env <- load_matrix_tool()
rows <- env$assemble_rows()
counts <- table(rows$kind)
expect_equal(unname(counts[["set"]]), 13)
expect_equal(unname(counts[["map"]]), 297)
expect_equal(unname(counts[["numpar"]]), 133)
