# =============================================================================
# Importing an energyRt scenario into a multimod model
# =============================================================================
#
# Moved here from multimod on 2026-09-24 with collect_scenario_parameter_data().
# The companion assertions about multimod's own path/format helpers stayed in
# multimod's test-import-empty-path.R.
# =============================================================================

skip_if_no_multimod <- function() skip_if_not_installed("multimod")

# An empty table whose recorded misc$path resolves nowhere must import as
# empty, not error: an EMPTY table legitimately gets no on-disk file when a
# scenario is saved (e.g. an unused `group` set), while its shell may still
# carry the path the writer would have used. The import's no-silent-default
# hard stop is reserved for the aggregate case (nothing reachable at all,
# .report_rows) -- per-object emptiness is data, not failure.

test_that("an empty parameter with a dangling path imports as empty", {
  skip_if_no_multimod()
  setClass("mm_mock_param",
           representation(data = "ANY", misc = "list", name = "character",
                          dimSets = "character"),
           where = environment())
  p <- new("mm_mock_param",
           data = data.frame(),
           misc = list(path = file.path(tempdir(), "does-not-exist-anywhere")),
           name = "group", dimSets = character())

  out <- collect_scenario_parameter_data(p)
  expect_s3_class(out, "data.frame")
  expect_identical(nrow(out), 0L)
})

test_that("the unfold guard only fires for a wildcard fold", {
  # .scenario_is_folded() decides whether energyRt data needs materialising.
  # A drop-mode fold has no column left to expand.
  f <- .scenario_is_folded
  mk <- function(mode) {
    p <- methods::new("parameter", name = "pX", dimSets = "region",
                      type = "numpar")
    p@misc <- list(fold_info = c(list(folded = TRUE),
                                 if (!is.null(mode)) list(mode = mode)))
    mi <- methods::new("modInp")
    mi@parameters <- list(pX = p)
    s <- methods::new("scenario")
    s@modInp <- mi
    s
  }

  expect_true(f(mk("wildcard")))
  expect_false(f(mk("drop")))
  # a missing mode predates the field; every energyRt that wrote one wrote a
  # wildcard, so it must still expand rather than silently skip
  expect_true(f(mk(NULL)))
})
