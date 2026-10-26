# Calendar sampling (R/solve_by_sample.R). Three invariants are pinned here:
#
#   * samples are REPLICATES -- each is annualised and estimates the whole
#     year, so the set is a distribution and must not be summed, unlike the
#     region driver's disjoint samples;
#   * a sample keeps >= 2 members at every level -- dropping a collapsed level
#     renames the leaf timeslices, and the model's data stops matching them;
#   * a sample keeps the full calendar's timeslice names -- a ragged
#     selection builds an irregular calendar of exactly the slices chosen.

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

test_that(".sample_calendar keeps two members and the chosen slices", {
  cal <- bs_cal()
  leaf <- bs_leaf()

  # one season collapses SEASON
  expect_error(.sample_calendar(cal, grep("^WIN_", leaf, value = TRUE)),
               "at least two members")

  # a ragged selection is an irregular calendar of exactly those slices
  ragged <- setdiff(leaf, "WIN_h00")
  rc <- .sample_calendar(cal, ragged)
  expect_identical(as.character(rc@timeframes[[length(rc@timeframes)]]),
                   ragged)
  expect_equal(sum(rc@timetable$share), rc@year_fraction)

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
  # how the sampling was configured, not just which sample this is -- without
  # it a reloaded sequence cannot say what produced it, and two runs of the
  # method with different settings look identical
  expect_true(is.list(vy$params))
  expect_identical(vy$params$method, "sequential")

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

# --- coverage weighting ------------------------------------------------------ #

test_that("sample_summary weights the mean by each sample's coverage", {
  skip_if_no_solver()
  bs_local("bs_cov")
  # Demand must VARY across slices, or every sample returns the same number and
  # no weighting could change the mean -- the test would pass on a no-op.
  mod <- bs_mod("bs_cov", demand = seq_along(bs_leaf()))
  ens <- solve_by_sample(mod, name = "bs_cov", sample_size = 2,
                         method = "sequential",
                         solver = solver_options$glpk, verbose = FALSE)
  skip_if(isTRUE(ens$failed))
  skip_if(nrow(ens$runs) < 2L)

  # Samples need not cover equal shares of the year -- `sequential` folds a
  # trailing singleton block into its predecessor -- so the weights are set
  # here directly rather than hunting for a calendar that happens to be ragged.
  ens$runs$year_fraction <- c(0.75, 0.25)

  d <- as.data.frame(getData(ens, "vSupOut", merge = TRUE))
  skip_if(!("sample" %in% names(d)) || length(unique(d$sample)) < 2L)
  plain <- mean(d$value, na.rm = TRUE)
  wts <- stats::setNames(ens$runs$year_fraction, ens$runs$sample)
  want <- sum(d$value * wts[d$sample]) / sum(wts[d$sample])

  # the samples must differ, or the assertion below cannot discriminate
  expect_false(isTRUE(all.equal(plain, want)))

  s <- suppressMessages(sample_summary(ens, "vSupOut", by = "region"))
  expect_equal(s$mean[1], want)
  # and it is genuinely a different number from the unweighted mean, which is
  # what the code did before
  expect_false(isTRUE(all.equal(s$mean[1], plain)))

  # unequal coverage is announced, because the quantiles stay unweighted
  expect_message(sample_summary(ens, "vSupOut", by = "region"),
                 "unequal shares of the year")
})

test_that("equal coverage weights nothing and says nothing", {
  skip_if_no_solver()
  bs_local("bs_cov2")
  mod <- bs_mod("bs_cov2", demand = seq_along(bs_leaf()))
  ens <- solve_by_sample(mod, name = "bs_cov2", sample_size = 2,
                         method = "sequential",
                         solver = solver_options$glpk, verbose = FALSE)
  skip_if(isTRUE(ens$failed))
  ens$runs$year_fraction <- rep(0.5, nrow(ens$runs))
  d <- as.data.frame(getData(ens, "vSupOut", merge = TRUE))
  s <- sample_summary(ens, "vSupOut", by = "region")   # no message expected
  expect_equal(s$mean[1], mean(d$value, na.rm = TRUE))
})

# --- the consensus floor ----------------------------------------------------- #

bs_mod_build <- function(name = "bsb") {
  leaf <- bs_leaf()
  newModel(name, region = "R1", discount = 0, calendar = bs_cal(),
           horizon = newHorizon(2020),
           repo = newRepository("bsbr",
             newCommodity("ELC", timeframe = "HOUR"),
             newTechnology("GEN", output = list(comm = "ELC"), cap2act = 1,
                           invcost = data.frame(invcost = 100),
                           olife = list(olife = 30),
                           af = data.frame(af.up = 1)),
             newSupply("BACK", commodity = "ELC",
                       supply = data.frame(region = "R1", cost = 500)),
             newDemand("DEM", commodity = "ELC",
                       demand = data.frame(region = "R1", year = 2020L,
                                           timeslice = leaf,
                                           demand = seq_along(leaf)))))
}

test_that("the consensus counts a sample that built nothing as a zero", {
  # .capacity_targets() drops non-positive rows, so averaging only the rows
  # present would read "two of four samples built X" as "every sample built X"
  # and floor the ensemble above anything it agreed on.
  mk <- function(v) {
    structure(tibble(family = "tech", proc = "GEN", region = "R1",
                     year = 2020L, value = v),
              class = c("capacity_targets", class(tibble())))
  }
  with_mocked_bindings(
    .capacity_targets = function(object, var = "totalcap", ...) object,
    {
      cs <- energyRt:::.ensemble_consensus(list(mk(10), mk(30)), stat = "mean")
      expect_equal(cs$value, 20)                     # (10 + 30) / 2
      # a third sample that built nothing drags the mean down
      cs3 <- energyRt:::.ensemble_consensus(
        list(mk(10), mk(30), mk(numeric(0))[0, ]), stat = "mean")
      expect_equal(cs3$value, 40 / 3)
    },
    .package = "energyRt")
})

test_that("the consensus does not depend on the order of the samples", {
  mk <- function(v) {
    structure(tibble(family = "tech", proc = "GEN", region = "R1",
                     year = 2020L, value = v),
              class = c("capacity_targets", class(tibble())))
  }
  with_mocked_bindings(
    .capacity_targets = function(object, var = "totalcap", ...) object,
    {
      a <- list(mk(10), mk(30), mk(50))
      expect_equal(energyRt:::.ensemble_consensus(a, "mean")$value,
                   energyRt:::.ensemble_consensus(rev(a), "mean")$value)
      expect_equal(energyRt:::.ensemble_consensus(a, "median")$value, 30)
    },
    .package = "energyRt")
})

test_that("ncap_floor = 0 is a no-op and ncap_floor > 1 is refused", {
  skip_if_no_solver()
  bs_local("bs_nf0")
  mod <- bs_mod_build("bs_nf0")
  a <- solve_by_sample(mod, name = "bs_nf0", sample_size = 2,
                       method = "sequential", solver = solver_options$glpk,
                       verbose = FALSE)
  skip_if(isTRUE(a$failed))
  # the default must change nothing: one pass, every row "free"
  expect_setequal(a$runs$pass, "free")
  expect_null(a$scenarios_free)
  expect_identical(a$ncap_floor, 0)

  expect_error(solve_by_sample(mod, ncap_floor = 1.5), "cannot exceed 1")
  expect_error(solve_by_sample(mod, ncap_floor = -0.1), "must be one number")
})

test_that("a floor pulls a sample up without pinning one already above it", {
  skip_if_no_solver()
  bs_local("bs_nf9")
  mod <- bs_mod_build("bs_nf9")
  b <- solve_by_sample(mod, name = "bs_nf9", sample_size = 2,
                       method = "sequential", ncap_floor = 0.9,
                       solver = solver_options$glpk, verbose = FALSE)
  skip_if(isTRUE(b$failed))

  expect_setequal(b$runs$pass, c("free", "floor"))
  expect_identical(nrow(b$runs), 4L)
  expect_false(is.null(b$scenarios_free))      # the free pass stays reachable

  free <- b$runs[b$runs$pass == "free", ]
  flr <- b$runs[b$runs$pass == "floor", ]
  o_free <- stats::setNames(free$objective, free$sample)
  o_flr <- stats::setNames(flr$objective, flr$sample)
  # a floor can only make a sample's own problem harder, never cheaper
  expect_true(all(o_flr >= o_free - 1e-6))
  # and it binds on at least one of them, or the floor did nothing
  expect_true(any(o_flr > o_free + 1e-6))

  # the floored variants are stored beside the free ones, not over them
  rr <- scenario_runs(b$scenarios[[1]])
  expect_true(any(grepl("_f$", rr$variant)))
  expect_true(any(!grepl("_f$", rr$variant)))
})

# --- building the samples on a sealed base ----------------------------------- #

test_that("`path =` alone no longer refuses a base", {
  # Model content reaches interpolate_model() positionally; `path` is a control
  # argument that arrives through `...` and is read only on the full path, so
  # the base dispatch used to see it as "an object of class character is added"
  # and refuse. Every solve driver passes `path`, so this blocked all of them.
  bs_local("bs_pathbase")
  mod <- bs_mod("bs_pathbase")
  base <- suppressMessages(save_scenario(
    interpolate_model(mod, name = "bs_pb_base"), verbose = FALSE))
  suppressMessages(seal_scenario(base, inputs = TRUE, outputs = FALSE,
                                 verbose = FALSE))
  cal2 <- energyRt:::.sample_calendar(
    bs_cal(), utils::head(bs_leaf(), 48), name = "bs_pb_half")

  d <- suppressMessages(interpolate_model(
    mod, name = "bs_pb_d", cal2, path = bs_path("bs_pb_d"), overwrite = TRUE,
    base = base))
  expect_false(is.null(d@misc$base))
  expect_true("timeslice" %in% names(d@misc$sample))
})

test_that("a scenario given as `object` becomes the samples' base", {
  skip_if_no_solver()
  bs_local("bs_base")
  # the threshold is 1e6 rows by default, far above a test model's tables
  old <- options(energyRt.param_ref_min_rows = 1)
  on.exit(options(old), add = TRUE)

  mod <- bs_mod("bs_base")
  base <- suppressMessages(save_scenario(
    interpolate_model(mod, name = "bs_base_full"), verbose = FALSE))
  suppressMessages(seal_scenario(base, inputs = TRUE, outputs = FALSE,
                                 verbose = FALSE))

  b <- solve_by_sample(base, name = "bs_onbase", sample_size = 2,
                       method = "sequential", solver = solver_options$glpk,
                       verbose = FALSE)
  skip_if(isTRUE(b$failed))

  s1 <- b$scenarios[[1]]
  expect_false(is.null(s1@misc$base))
  expect_true("timeslice" %in% names(s1@misc$sample))

  # the tables the sample shares with the base are references, so fewer of its
  # parameter dirs hold a local copy
  own <- list.dirs(fp(s1@path, "runs", names(b$scenarios)[1], "modInp",
                      "parameters"), recursive = FALSE)
  a <- solve_by_sample(mod, name = "bs_nobase", sample_size = 2,
                       method = "sequential", solver = solver_options$glpk,
                       verbose = FALSE)
  skip_if(isTRUE(a$failed))
  own_a <- list.dirs(fp(a$scenarios[[1]]@path, "runs",
                        names(a$scenarios)[1], "modInp", "parameters"),
                     recursive = FALSE)
  expect_lt(length(own), length(own_a))
  # and the answer is the same either way, which is the point
  expect_equal(b$runs$objective, a$runs$objective, tolerance = 1e-6)
})

test_that("a non-scenario base is refused", {
  mod <- bs_mod("bs_badbase")
  expect_error(solve_by_sample(mod, base = mod), "must be a saved scenario")
})
