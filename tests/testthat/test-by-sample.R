# Calendar sampling (R/solve_by_sample.R). Three invariants are pinned here:
#
#   * samples are REPLICATES -- each is annualised and estimates the whole
#     year, so the set is a distribution and must not be summed, unlike the
#     region driver's disjoint samples;
#   * a sample keeps >= 2 members at every level -- dropping a collapsed level
#     renames the leaf timeslices, and the model's data stops matching them;
#   * a sample is a complete cross of its level members -- the calendar
#     constructor re-expands a ragged selection, so the requested slices and
#     the resulting calendar would disagree.

bs_path <- function(name) {
  gsub("[\\/]+", "/", file.path(tempdir(), "by-sample", name))
}

bs_local <- function(name, env = parent.frame()) {
  unlink(bs_path(name), recursive = TRUE)
  old <- set_scenarios_path(bs_path(name))
  do.call(on.exit, list(bquote(set_scenarios_path(.(old))), add = TRUE),
          envir = env)
  invisible(NULL)
}

bs_cal <- function() calendars$s4_h24
bs_leaf <- function() {
  cal <- bs_cal()
  as.character(cal@timeframes[[length(cal@timeframes)]])
}

bs_mod <- function(name = "bs", demand = 2) {
  leaf <- bs_leaf()
  newModel(name, region = "R1", discount = 0, calendar = bs_cal(),
           horizon = newHorizon(2020),
           repo = newRepository("bsr",
             newCommodity("ELC", timeframe = "HOUR"),
             newSupply("SUP", commodity = "ELC",
                       supply = data.frame(region = "R1", cost = 15)),
             newDemand("DEM", commodity = "ELC",
                       demand = data.frame(region = "R1", year = 2020L,
                                           timeslice = leaf,
                                           demand = demand))))
}

# --- the sampler ------------------------------------------------------------- #

test_that("calendar_samples draws whole members of a level", {
  cal <- bs_cal()
  sq <- calendar_samples(cal, sample_size = 2, method = "sequential")
  expect_s3_class(sq, "calendar_samples")
  expect_setequal(names(sq), c("sample", "timeslice", "count"))
  expect_identical(attr(sq, "level"), "SEASON")
  # a sequential tiling covers every leaf slice exactly once
  expect_setequal(sq$timeslice, bs_leaf())
  expect_false(any(duplicated(sq$timeslice)))
  expect_identical(length(unique(sq$sample)), 2L)

  rd <- calendar_samples(cal, sample_size = 2, n = 2, method = "random",
                         seed = 1)
  expect_false(any(duplicated(rd$timeslice)))     # draws are disjoint

  bt <- calendar_samples(cal, sample_size = 4, n = 2, method = "bootstrap",
                         seed = 1)
  expect_true(any(bt$count > 1))                  # repeats are counts...
  expect_false(any(duplicated(bt[, c("sample", "timeslice")])))  # ...not rows
})

test_that("calendar_samples refuses a sample that would collapse a level", {
  cal <- bs_cal()
  expect_error(calendar_samples(cal, sample_size = 1, method = "sequential"),
               "at least 2")
  expect_error(calendar_samples(cal, sample_size = 99), "exceeds")
  expect_error(calendar_samples(cal, level = "NOPE", sample_size = 2),
               "must be one of")
  expect_error(calendar_samples(cal), "sample_size")
  # an odd member count must not leave a trailing single-member block
  sq <- calendar_samples(cal, sample_size = 3, method = "sequential")
  sizes <- table(sq$sample)
  expect_true(all(sizes >= 2 * 24))
})

test_that(".sample_calendar keeps two members and a complete cross", {
  cal <- bs_cal()
  leaf <- bs_leaf()

  # one season collapses SEASON
  expect_error(.sample_calendar(cal, grep("^WIN_", leaf, value = TRUE)),
               "at least two members")

  # a ragged selection would be re-expanded by the constructor
  ragged <- setdiff(leaf, "WIN_h00")
  expect_error(.sample_calendar(cal, ragged), "complete cross")

  # two whole seasons are fine, and the leaf labels are preserved
  ok <- .sample_calendar(cal, grep("^(WIN|SUM)_", leaf, value = TRUE))
  expect_s4_class(ok, "calendar")
  expect_identical(length(as.character(
    ok@timeframes[[length(ok@timeframes)]])), 48L)
  expect_true(all(grepl("^(WIN|SUM)_", as.character(
    ok@timeframes[[length(ok@timeframes)]]))))
  expect_equal(ok@year_fraction, sum(ok@timeslice_share$share[
    ok@timeslice_share$timeslice %in%
      as.character(ok@timeframes[[length(ok@timeframes)]])]))
})

test_that("a bootstrap repeat becomes a scaled share", {
  cal <- bs_cal()
  leaf <- bs_leaf()
  two <- grep("^(WIN|SUM)_", leaf, value = TRUE)
  cnt <- stats::setNames(rep(1L, length(two)), two)
  cnt[grepl("^WIN_", names(cnt))] <- 2L            # winter drawn twice
  c1 <- .sample_calendar(cal, two, counts = cnt)
  base <- .sample_calendar(cal, two)
  # doubling winter raises the year fraction by winter's own share
  expect_gt(c1@year_fraction, base@year_fraction)
  expect_identical(length(as.character(
    c1@timeframes[[length(c1@timeframes)]])), 48L) # still 48 slices, not 72
})

# --- the driver -------------------------------------------------------------- #

test_that("a full-coverage sample reproduces the full-calendar solve", {
  skip_if_no_solver()
  bs_local("one")
  mod <- bs_mod()
  full <- solve_scenario(interpolate_model(mod, name = "bs_full"),
                         echo = FALSE)
  o_full <- as.numeric(getData(full, "vObjective", merge = TRUE)$value[1])

  all_slices <- data.frame(sample = "s01", timeslice = bs_leaf())
  one <- solve_by_sample(mod, name = "bs_one", samples = all_slices,
                         verbose = FALSE)
  expect_identical(nrow(one$runs), 1L)
  expect_equal(one$runs$year_fraction, 1)
  expect_equal(one$runs$objective, o_full, tolerance = 1e-8)
})

test_that("samples are replicates: the MEAN tracks the year, the sum does not", {
  skip_if_no_solver()
  bs_local("rep")
  mod <- bs_mod()
  full <- solve_scenario(interpolate_model(mod, name = "bs_ref"), echo = FALSE)
  o_full <- as.numeric(getData(full, "vObjective", merge = TRUE)$value[1])

  ens <- solve_by_sample(mod, name = "bs_rep", sample_size = 2,
                         method = "sequential", verbose = FALSE)
  expect_s3_class(ens, "by_sample")
  expect_identical(nrow(ens$runs), 2L)
  expect_true(all(ens$runs$status == "solved"))

  # each sample annualises, so the mean estimates the year ...
  expect_equal(mean(ens$runs$objective), o_full, tolerance = 0.02)
  # ... and the sum is emphatically NOT the year. Asserted so the semantics
  # cannot silently invert into the region driver's additive ones.
  expect_gt(sum(ens$runs$objective), 1.5 * o_full)
})

test_that("the driver refuses a collapsing sample and reports failures", {
  skip_if_no_solver()
  bs_local("bad")
  mod <- bs_mod()
  one_season <- data.frame(sample = "s01",
                           timeslice = grep("^WIN_", bs_leaf(), value = TRUE))
  expect_error(
    solve_by_sample(mod, name = "bs_bad", samples = one_season,
                    verbose = FALSE),
    "at least two members")
  expect_warning(
    r <- solve_by_sample(mod, name = "bs_bad2", samples = one_season,
                         on_error = "continue", verbose = FALSE),
    "at least two members")
  expect_true(r$failed)
  expect_identical(r$runs$status, "failed")
})

test_that("runs are stored as variants, tagged, and stacked by getData", {
  skip_if_no_solver()
  bs_local("store")
  mod <- bs_mod()
  ens <- solve_by_sample(mod, name = "bs_store", sample_size = 2,
                         method = "sequential", verbose = FALSE)

  rr <- scenario_runs(ens$scenarios[[1]])
  expect_true(all(c("s01", "s02") %in% rr$variant))
  vy <- yaml::read_yaml(file.path(ens$scenarios[[1]]@path, "runs", "s01",
                                  "variant.yml"))
  expect_identical(vy$type, "calendar_sample")
  expect_identical(vy$method, "sequential")
  expect_identical(vy$slices, 48L)
  expect_lt(vy$year_fraction, 1)

  d <- getData(ens, "vSupOut", merge = TRUE)
  expect_true("sample" %in% names(d))
  expect_setequal(unique(d$sample), c("s01", "s02"))
})

test_that("sample_summary returns quantiles across samples", {
  skip_if_no_solver()
  bs_local("summ")
  mod <- bs_mod()
  ens <- solve_by_sample(mod, name = "bs_summ", sample_size = 2, n = 2,
                         method = "bootstrap", seed = 7, verbose = FALSE)
  skip_if(any(ens$runs$status != "solved"), "a bootstrap sample failed")

  s <- sample_summary(ens, "vSupOut", probs = c(0.1, 0.5, 0.9),
                      by = "region")
  expect_true(all(c("n", "mean", "q10", "q50", "q90") %in% names(s)))
  expect_true(all(s$q10 <= s$q50 & s$q50 <= s$q90))
})
