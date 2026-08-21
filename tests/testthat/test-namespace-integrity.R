# =========================================================================== #
# NAMESPACE integrity
#
# Inserting a helper between a roxygen block and the function it documents makes
# roxygen attach that block -- and its `@export` -- to the HELPER, and then keep
# consuming the following prose as tags. This has happened twice:
#
#   1. `.storage_deprecated_args()` placed above `newStorage()` produced
#      `export(.storage_inflow_args)` and NO `export(newStorage)`.
#   2. `get_process_stored()` placed above `get_process_comm()` grew NAMESPACE to
#      287 entries including `export(a)`, `export(and)`, `export(H2)` and
#      `export("(hydrogen:")` -- ordinary words lifted out of a sentence -- while
#      `get_process_comm` silently lost its own export.
#
# Neither is visible under `devtools::load_all()`, which sets `export_all = TRUE`
# and puts every object in scope regardless of what NAMESPACE says. So the file
# has to be read from disk and checked directly.
#
# The first attempt at a guard listed the constructors and checked they were
# still exported; case 2 slipped past it because the constructors were fine and
# the damage was elsewhere. Hence the check below is on the whole file: an export
# that does not name a real object in the namespace is the shape ALL of this
# garbage takes, whether or not the token happens to look like an identifier.
# =========================================================================== #

.ns_exports <- function() {
  ns_file <- file.path(find.package("energyRt"), "NAMESPACE")
  skip_if_not(file.exists(ns_file), "NAMESPACE not found")
  ns <- readLines(ns_file, warn = FALSE)
  ex <- grep("^export[(]", ns, value = TRUE)
  nms <- sub("^export[(]", "", ex)
  nms <- sub("[)]$", "", nms)
  # Quote characters are stripped by code point rather than by an escaped
  # regex class -- the class is unreadable and easy to mis-escape.
  nms <- gsub(intToUtf8(34), "", nms, fixed = TRUE)
  gsub(intToUtf8(39), "", nms, fixed = TRUE)
}

test_that("every NAMESPACE export names a real object in the package", {
  nms <- .ns_exports()
  expect_gt(length(nms), 100L)   # the file is populated at all

  ns <- asNamespace("energyRt")
  missing <- nms[!vapply(nms, exists, logical(1), envir = ns, inherits = FALSE)]
  # `export(a)`, `export(and)`, `export("(hydrogen:")` all land here.
  expect_equal(missing, character(0))
})

test_that("no NAMESPACE export is a syntactically invalid name", {
  nms <- .ns_exports()
  # make.names() is a no-op exactly on already-valid names.
  bad <- nms[make.names(nms) != nms]
  expect_equal(bad, character(0))
})

test_that("the object constructors are exported", {
  # The narrower half of the guard, kept: these are the entry points a user calls
  # first, and case 1 above removed one of them without any other symptom.
  ctors <- c("newCommodity", "newSupply", "newDemand", "newTechnology",
             "newStorage", "newTrade", "newModel", "newRepository",
             "newCalendar", "newHorizon")
  nms <- .ns_exports()
  expect_equal(setdiff(ctors, nms), character(0))
})
