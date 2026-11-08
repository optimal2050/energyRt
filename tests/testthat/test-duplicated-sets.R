# =========================================================================== #
# Duplicate set columns in a materialised table.
#
# A parameter whose dimensions repeat a set -- a trade's `src`/`dst` both
# resolving to `region`, an aggregation keyed on two levels of one family --
# arrives with two columns of the same name, which neither Arrow nor a join can
# address. `rename_duplicated_sets()` makes them unique, and
# `anyDuplicatedSets()` decides whether it has to run
# (`arrow.R`, `get_data.R`).
#
# The bug this pins: suffixes started at 2 unconditionally, so a table that
# ALREADY held `b2` got a second `b2` from renaming its duplicate `b` columns
# -- the function creating the very thing it exists to remove. It only showed
# when the taken name came FIRST; later, it was reprocessed and self-healed.
# =========================================================================== #

ds_tbl <- function(nm) {
  x <- as.data.frame(matrix(seq_along(nm), nrow = 1))
  colnames(x) <- nm
  x
}

ds_rename <- function(nm) colnames(energyRt:::rename_duplicated_sets(ds_tbl(nm)))

test_that("duplicate columns are renamed and the first keeps its name", {
  expect_equal(ds_rename(c("a", "b", "c", "b", "b", "a", "a", "a")),
               c("a", "b", "c", "b2", "b3", "a2", "a3", "a4"))
  # nothing to do, and the table comes back untouched
  expect_equal(ds_rename(c("region", "year", "value")),
               c("region", "year", "value"))
})

test_that("a suffix that is already a column name is skipped", {
  # the taken name FIRST: this returned c("b2", "b", "b2") before
  expect_equal(ds_rename(c("b2", "b", "b")), c("b2", "b", "b3"))
  expect_equal(ds_rename(c("a2", "a", "a")), c("a2", "a", "a3"))
  # and after the duplicates, where the old code happened to recover
  expect_equal(ds_rename(c("b", "b", "b2")), c("b", "b3", "b2"))
})

test_that("renaming never leaves a duplicate", {
  cases <- list(
    c("a", "b", "c", "b2", "b", "a", "a5", "a"),
    c("b2", "b3", "b", "b", "b"),
    c("region", "region", "region2", "region3"),
    c("src", "dst", "region", "region", "region2"),
    rep("x", 5L),
    c("x2", "x2", "x", "x")
  )
  for (nm in cases) {
    out <- ds_rename(nm)
    expect_false(any(duplicated(out)),
                 label = paste0("unique names for {", paste(nm, collapse = ","),
                                "} -> {", paste(out, collapse = ","), "}"))
    expect_length(out, length(nm))
  }
})

test_that("anyDuplicatedSets() reports what rename_duplicated_sets() fixes", {
  expect_true(energyRt:::anyDuplicatedSets(ds_tbl(c("a", "a"))))
  expect_false(energyRt:::anyDuplicatedSets(ds_tbl(c("a", "a2"))))
  # not a data.frame: NULL, so the callers' `if` short-circuits
  expect_null(energyRt:::anyDuplicatedSets(1:3))
})
