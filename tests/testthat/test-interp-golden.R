# =========================================================================== #
# Interpolation goldens (nightly, solver-free): each tm tier model's modInp
# must reproduce its tracked baseline in tests/testthat/goldens/interp/
# (frozen by tools/test/interp_guard.R / make_goldens.R --suite=interp).
# Solved goldens pin what the OPTIMIZER returns; this pins what the
# interpolation pipeline HANDS the optimizer -- a representation change that
# happens to solve to the same objective still fails here.
# =========================================================================== #

test_that("tm tier modInp reproduces the tracked interp baselines", {
  skip_if_tier_below("nightly")
  skip_if_no_fixtures()
  dir <- testthat::test_path("goldens", "interp")
  skip_if(!dir.exists(dir), paste(
    "no interp baselines -- run: Rscript tools/test/interp_guard.R baseline"))
  env <- .mapping_fixture_env()
  for (tier in c("tm_core", "tm_flows", "tm_io", "tm_policy", "tm_weather")) {
    f <- file.path(dir, paste0(tier, ".rds"))
    if (!file.exists(f)) {      # report, don't abort the remaining tiers
      fail(paste0(tier, ": baseline missing -- re-run interp_guard.R baseline"))
      next
    }
    base <- readRDS(f)          # compare_inputs upgrades old-layout objects
    scen <- suppressMessages(suppressWarnings(interpolate_model(
      env[[tier]](), name = paste0("ig_", tier),
      ondisk = FALSE, overwrite = TRUE)))
    res <- compare_inputs(base, scen@modInp, names = c("baseline", "current"))
    clean <- inputs_cmp_identical(res)
    if (!clean) print(res)      # the full diff, next to the failure
    expect_true(clean, label = paste0(tier, " modInp identical to baseline"))
  }
})
