# =========================================================================== #
# Test-tier resolution and per-backend skip guards.
#
# Tiers (check < fast < cross < nightly), selected via ENERGYRT_TEST_TIER:
#   check   - solver-free; what R CMD check / CRAN runs
#   fast    - + glpsol; the default for local devtools::test()
#   cross   - + julia / pyomo / gams cross-backend parity tests
#   nightly - + deep regression, external models, full fork grids
#
# When ENERGYRT_TEST_TIER is unset the tier defaults to "check" under
# R CMD check (detected via _R_CHECK_PACKAGE_NAME_) and to "fast" everywhere
# else. This deliberately REPLACES skip_on_cran() as the solver gate: a bare
# `Rscript -e "testthat::test_file(...)"` (no NOT_CRAN set) used to skip every
# solver test silently -- now it solves whenever glpsol is present.
#
# See tests/README.md and dev/TESTING.md.
# =========================================================================== #

.tier_levels <- c(check = 0L, fast = 1L, cross = 2L, nightly = 3L)

en_test_tier <- function() {
  t <- tolower(Sys.getenv("ENERGYRT_TEST_TIER", ""))
  if (nzchar(t)) {
    if (!t %in% names(.tier_levels)) {
      stop("ENERGYRT_TEST_TIER must be one of: ",
           paste(names(.tier_levels), collapse = ", "), " (got '", t, "')")
    }
    return(t)
  }
  if (nzchar(Sys.getenv("_R_CHECK_PACKAGE_NAME_")) ||
      identical(Sys.getenv("NOT_CRAN"), "false")) {
    return("check")
  }
  "fast"
}

skip_if_tier_below <- function(tier) {
  stopifnot(tier %in% names(.tier_levels))
  if (.tier_levels[[en_test_tier()]] < .tier_levels[[tier]]) {
    testthat::skip(paste0("tier '", en_test_tier(), "' < required '", tier,
                          "' (set ENERGYRT_TEST_TIER=", tier, " to run)"))
  }
}

# --------------------------------------------------------------------------- #
# Toolchain availability, memoised once per test run. Guards are deliberately
# light (path presence, via ~/.energyRt/config.yml or PATH) -- deep probes like
# en_check_julia_pkgs() start a Julia session and are too slow for a skip.
# --------------------------------------------------------------------------- #

.toolchain <- local({
  cache <- new.env(parent = emptyenv())
  function(key, probe) {
    if (is.null(cache[[key]])) cache[[key]] <- isTRUE(tryCatch(probe(), error = function(e) FALSE))
    cache[[key]]
  }
})

.path_found <- function(getter, exe) {
  cfg <- tryCatch(getter(), error = function(e) NULL)
  (!is.null(cfg) && nzchar(cfg) && file.exists(cfg)) || nzchar(Sys.which(exe))
}

has_glpsol <- function() .toolchain("glpk", function() .path_found(get_glpk_path, "glpsol"))
has_julia  <- function() .toolchain("julia", function() .path_found(get_julia_path, "julia"))
has_python <- function() .toolchain("python", function() .path_found(get_python_path, "python"))
has_gams   <- function() .toolchain("gams", function() .path_found(get_gams_path, "gams"))

skip_if_no_julia_highs <- function() {
  skip_if_tier_below("cross")
  if (!has_julia()) testthat::skip("julia not available (PATH or ~/.energyRt/config.yml)")
}

skip_if_no_pyomo <- function() {
  skip_if_tier_below("cross")
  if (!has_python()) testthat::skip("python not available (PATH or ~/.energyRt/config.yml)")
}

skip_if_no_gams <- function() {
  skip_if_tier_below("cross")
  if (!has_gams()) testthat::skip("gams not available (PATH or ~/.energyRt/config.yml)")
  if (!has_gams_license()) {
    testthat::skip("gams found but unlicensed (no gamslice.txt next to the executable)")
  }
}

# A gams binary without a license terminates every job ("Terminated due to a
# licensing error"), so an unlicensed install must skip, not fail.
has_gams_license <- function() {
  .toolchain("gams_license", function() {
    cfg <- tryCatch(get_gams_path(), error = function(e) NULL)
    exe <- if (!is.null(cfg) && nzchar(cfg) && file.exists(cfg)) cfg else Sys.which("gams")
    if (!nzchar(exe)) return(FALSE)
    file.exists(file.path(dirname(exe), "gamslice.txt"))
  })
}

skip_if_no_neos <- function() {
  skip_if_tier_below("cross")
  if (!identical(Sys.getenv("ENERGYRT_TEST_NEOS"), "true")) {
    testthat::skip("NEOS tests are opt-in (set ENERGYRT_TEST_NEOS=true)")
  }
}
