# getData(run = ): reading several runs in one call (R/get_data.R).
#
# The properties that would break silently:
#   * `run = NULL` must return EXACTLY today's frame -- getData() is called
#     from almost everything, and an extra column or a moved one breaks
#     callers that index positionally or join on a column set;
#   * a variant is a DIFFERENT problem, so its PARAMETERS must come from its
#     own modInp, not the base -- results from one and parameters from the
#     other would look plausible and be wrong;
#   * the tag column is `run`, never `variant`: `variant` already means a
#     technology's vintage/cluster provenance in this very function;
#   * asking for runs must not WRITE anything.

# Covers the R API: getData(), read_solution(), solved on glpk.
# NOT an `@covers` tag -- that vocabulary is the model coverage matrix
# (sets/maps/parameters/equations/variables), which has no row for an exported
# function, so such a tag can never resolve. See tests/README.md.

gr_root <- function(...) {
  gsub("[\\/]+", "/", file.path(tempdir(), "getdata-run-suite", ...))
}

gr_local <- function(env = parent.frame()) {
  old_sp <- set_scenarios_path(gr_root("scenarios"))
  old_mp <- set_models_path(gr_root("models"))
  old_rf <- set_registry_file(gr_root("reg.csv"))
  expr <- bquote({
    set_scenarios_path(.(old_sp)); set_models_path(.(old_mp))
    set_registry_file(.(old_rf))
  })
  do.call(on.exit, list(expr, add = TRUE), envir = env)
  invisible(NULL)
}

gr_two <- function(name) {
  mod <- sp_tech(c(100, 100, 100), optret = FALSE, name = "gr")
  sc <- interpolate_model(mod, name = name, path = gr_root("scenarios", name))
  suppressMessages(save_scenario(suppressMessages(solve_scenario(
    sc, solver = solver_options$glpk, run = "runA", echo = FALSE)),
    verbose = FALSE))
  suppressMessages(save_scenario(suppressMessages(solve_scenario(
    sc, solver = solver_options$glpk, run = "runB", echo = FALSE)),
    verbose = FALSE))
}

test_that("run = NULL returns exactly today's frame", {
  skip_if_no_solver()
  gr_local()
  s <- gr_two("gr_compat")
  d <- as.data.frame(getData(s, "vTechOut", merge = TRUE))
  # no new column, and the column ORDER is part of the contract
  expect_false("run" %in% names(d))
  expect_identical(names(d)[1:2], c("scenario", "name"))
  expect_gt(nrow(d), 0L)
})

test_that("a run id tags the rows and keeps them stacked", {
  skip_if_no_solver()
  gr_local()
  s <- gr_two("gr_tag")
  one <- as.data.frame(getData(s, "vTechOut", merge = TRUE, run = "runA"))
  expect_true("run" %in% names(one))
  expect_identical(unique(one$run), "runA")

  two <- as.data.frame(getData(s, "vTechOut", merge = TRUE,
                               run = c("runA", "runB")))
  # stacked, NOT joined: a full_join on the shared keys would have produced
  # value.x / value.y instead of twice the rows
  expect_identical(nrow(two), nrow(one) * 2L)
  expect_setequal(unique(two$run), c("runA", "runB"))
  expect_false(any(grepl("^value[.]", names(two))))
})

test_that("run = 'all' covers every run", {
  skip_if_no_solver()
  gr_local()
  s <- gr_two("gr_all")
  d <- as.data.frame(getData(s, "vTechOut", merge = TRUE, run = "all"))
  expect_setequal(unique(d$run), scenario_runs(s)$run)
})

test_that("merge = FALSE names elements by run, not just by scenario", {
  skip_if_no_solver()
  gr_local()
  s <- gr_two("gr_list")
  l <- getData(s, "vTechOut", merge = FALSE, run = c("runA", "runB"))
  expect_identical(length(l), 2L)
  # without the run in the name the two elements would be indistinguishable
  expect_false(anyDuplicated(names(l)) > 0)
  expect_true(all(grepl("runA|runB", names(l))))
})

test_that("asking for runs writes nothing", {
  skip_if_no_solver()
  gr_local()
  s <- gr_two("gr_ro")
  snap <- function(d) {
    ff <- sort(list.files(d, recursive = TRUE, all.files = TRUE, no.. = TRUE))
    data.frame(f = ff, size = file.size(file.path(d, ff)),
               stringsAsFactors = FALSE)
  }
  before <- snap(s@path)
  invisible(getData(s, "vTechOut", merge = TRUE, run = "all"))
  expect_identical(snap(s@path), before)
})

test_that("an unreadable run errors rather than returning another run", {
  skip_if_no_solver()
  gr_local()
  s <- gr_two("gr_bad")
  expect_error(getData(s, "vTechOut", run = "nope"), "cannot read run")
})
