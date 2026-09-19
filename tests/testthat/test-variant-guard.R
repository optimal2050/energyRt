# Refusing to write over a variant that a different configuration produced
# (.variant_guard() in R/runs.R, wired into the four solve drivers).
#
# Variant labels are unit-derived -- s01, R1_R2, s02-2030 -- so a second run of
# the same driver with another seed, grouping or window lands on exactly the
# same folders. Before this, the first sequence's results were silently
# replaced and nothing in the tree said so.
#
# The guard must NOT fire on a legitimate redo: same method, same settings.

# @covers solve_myopic solve_by_sample solve_by_region solve_guided depth=S

vg_yml <- function(dir, vlab, type, params, sequence = "seq") {
  d <- file.path(dir, "runs", vlab)
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  yaml::write_yaml(list(layout = 3L, class = "variant", name = vlab,
                        type = type, sequence = sequence, params = params),
                   file.path(d, "variant.yml"))
  invisible(d)
}

test_that("nothing to protect passes", {
  d <- file.path(tempdir(), "vg-none"); unlink(d, recursive = TRUE)
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  # no variant.yml at all
  expect_true(.variant_guard(d, "s01", "calendar_sample", list(seed = 1)))
  # a folder with an unreadable manifest is not a reason to refuse a solve
  dd <- file.path(d, "runs", "s02"); dir.create(dd, recursive = TRUE)
  writeLines("{{ not yaml", file.path(dd, "variant.yml"))
  expect_true(.variant_guard(d, "s02", "calendar_sample", list(seed = 1)))
})

test_that("the same method with the same settings is a redo, not a collision", {
  d <- file.path(tempdir(), "vg-same"); unlink(d, recursive = TRUE)
  vg_yml(d, "s01", "calendar_sample", list(seed = 1L, n = 3L))
  expect_true(.variant_guard(d, "s01", "calendar_sample",
                             list(seed = 1L, n = 3L)))
  # a yaml round-trip turns 1L into 1; refusing over that would make the
  # guard useless in practice
  expect_true(.variant_guard(d, "s01", "calendar_sample",
                             list(seed = 1, n = 3)))
})

test_that("different settings are refused, naming what differs", {
  d <- file.path(tempdir(), "vg-diff"); unlink(d, recursive = TRUE)
  vg_yml(d, "s01", "calendar_sample", list(seed = 1L, n = 3L), sequence = "first")
  expect_error(.variant_guard(d, "s01", "calendar_sample",
                              list(seed = 2L, n = 3L)),
               "seed \\(1 vs 2\\)")
  # the message has to say whose results are at stake, and how to proceed
  err <- tryCatch(.variant_guard(d, "s01", "calendar_sample",
                                 list(seed = 2L, n = 3L)),
                  error = function(e) conditionMessage(e))
  expect_match(err, "first")
  expect_match(err, "overwrite = TRUE")
  # an argument present on one side only is a difference, not a match
  expect_error(.variant_guard(d, "s01", "calendar_sample", list(seed = 1L)),
               "n \\(3 vs <unset>\\)")
})

test_that("a different method on the same label is refused", {
  d <- file.path(tempdir(), "vg-type"); unlink(d, recursive = TRUE)
  vg_yml(d, "s01", "calendar_sample", list(seed = 1L))
  expect_error(.variant_guard(d, "s01", "myopic_step", list(seed = 1L)),
               "different method")
})

test_that("overwrite = TRUE proceeds", {
  d <- file.path(tempdir(), "vg-ow"); unlink(d, recursive = TRUE)
  vg_yml(d, "s01", "calendar_sample", list(seed = 1L))
  expect_true(.variant_guard(d, "s01", "myopic_step", list(seed = 99L),
                             overwrite = TRUE))
})

test_that("solve_myopic refuses a re-run with different settings", {
  skip_if_no_solver()
  p <- file.path(tempdir(), "vg-myo")
  unlink(p, recursive = TRUE)
  old <- set_scenarios_path(p); on.exit(set_scenarios_path(old), add = TRUE)
  m <- my_mod(years = 2020:2022, name = "vg")

  run <- function(...) {
    utils::capture.output(suppressMessages(suppressWarnings(r <- list(...))))
    r[[1]]
  }
  expect_no_error(run(solve_myopic(m, name = "vg", step = 1L, verbose = FALSE)))
  # same settings: a redo, allowed
  expect_no_error(run(solve_myopic(m, name = "vg", step = 1L, verbose = FALSE)))
  # different window: would overwrite the first sequence
  expect_error(run(solve_myopic(m, name = "vg", step = 1L, overlap = 1L,
                                verbose = FALSE)),
               "overlap")
  # explicitly asked for, allowed
  expect_no_error(run(solve_myopic(m, name = "vg", step = 1L, overlap = 1L,
                                   overwrite = TRUE, verbose = FALSE)))
})
