# =========================================================================== #
# Test-tier resolution and per-backend skip guards.
#
# Tiers (check < fast < cross < full), selected via ENERGYRT_TEST_TIER:
#   check   - solver-free; what R CMD check / CRAN runs
#   fast    - + glpsol; the default for local devtools::test()
#   cross   - + julia / pyomo / gams cross-backend parity tests
#   full    - + deep regression, external models, complete fork grids
#
# When ENERGYRT_TEST_TIER is unset the tier defaults to "check" under
# R CMD check (detected via _R_CHECK_PACKAGE_NAME_) and to "fast" everywhere
# else. This deliberately REPLACES skip_on_cran() as the solver gate: a bare
# `Rscript -e "testthat::test_file(...)"` (no NOT_CRAN set) used to skip every
# solver test silently -- now it solves whenever glpsol is present.
#
# See tests/README.md and tests/TESTING.md.
# =========================================================================== #

.tier_levels <- c(check = 0L, fast = 1L, cross = 2L, full = 3L)

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
# Toolchain availability, memoised once per test run. The package's own
# `en_check_*()` backend checks are the single source of truth -- they resolve
# `~/.energyRt/config.yml` before `PATH` and know the quirks (the Windows
# `python.exe` stub that prints "Python was not found" and exits 0; GAMS is
# detected by presence only, since it has no safe non-interactive version
# flag). Deeper probes like en_check_julia_pkgs() start a Julia session and are
# too slow for a skip.
#
# Whether a configured backend can actually SOLVE is a separate question,
# answered once per suite run by the preamble (`en_toolchains(solve = )` in
# tools/test/lib/runner.R, which calls `en_check_solve()`), and published as
# JSON for the parallel shard workers. `.probe_verdict()` reads it here: a
# backend the probe found broken skips with that reason, so a skip always cites
# a failed solve rather than a guess. With no verdict file -- a bare
# `test_file()`, or any run without the preamble -- the guards fall back to
# presence only and nothing is suppressed.
# --------------------------------------------------------------------------- #

.toolchain <- local({
  cache <- new.env(parent = emptyenv())
  function(key, probe) {
    if (is.null(cache[[key]])) cache[[key]] <- isTRUE(tryCatch(probe(), error = function(e) FALSE))
    cache[[key]]
  }
})

has_glpsol <- function() .toolchain("glpk", function() en_check_glpk()$found)
has_julia  <- function() .toolchain("julia", function() en_check_julia()$found)
has_python <- function() .toolchain("python", function() en_check_python()$found)
has_gams   <- function() .toolchain("gams", function() en_check_gams()$found)

# The preamble's verdict, read once per process. A file older than an hour is
# ignored: it may describe a machine that has since changed.
.PROBE_TTL_SECS <- 3600

# The preamble exports an absolute `ENERGYRT_TEST_PROBE`; without it, look for
# the default file both from the package root and from tests/testthat, which is
# where testthat puts the working directory.
.probe_default_path <- function() {
  env <- Sys.getenv("ENERGYRT_TEST_PROBE")
  if (nzchar(env)) return(env)
  rel <- file.path("tools", "test", ".backend-probe.json")
  cand <- c(rel, file.path("..", "..", rel))
  hit <- cand[file.exists(cand)]
  if (length(hit)) hit[[1]] else rel
}

.probe_verdict <- local({
  cache <- new.env(parent = emptyenv())
  function(path = NULL) {
    if (is.null(path)) {
      path <- .probe_default_path()
      key <- "default"
    } else {
      key <- path
    }
    if (is.null(cache[[key]])) {
      cache[[key]] <- list(v = tryCatch({
        if (!file.exists(path) ||
            !requireNamespace("jsonlite", quietly = TRUE)) {
          NULL
        } else {
          age <- as.numeric(difftime(Sys.time(), file.mtime(path), units = "secs"))
          if (age > .PROBE_TTL_SECS) NULL else
            jsonlite::read_json(path, simplifyVector = FALSE)$backends
        }
      }, error = function(e) NULL))
    }
    cache[[key]]$v
  }
})

# Skip when the preamble probed this backend and it failed. Silent when there is
# no verdict, when the backend was not probed, or when every level passed.
# The two failure modes get different reasons: a backend that cannot solve the
# tiny model is broken, while one that solves tiny but not the full model is
# size-limited (a GAMS demo or CPLEX community licence), which is the case that
# otherwise looks like a mysterious mid-suite failure.
skip_if_probe_failed <- function(backend, path = NULL) {
  v <- .probe_verdict(path)[[backend]]
  if (is.null(v) || !isTRUE(v$probed) || isTRUE(v$ok)) return(invisible(FALSE))
  note <- v$note %||% "no detail"
  if (identical(v$failed_level, "full") && isTRUE(v$tiny)) {
    testthat::skip(paste0(backend, " solves only small models (size-limited ",
                          "licence?): ", note))
  }
  testthat::skip(paste0(backend, " configured but failed the backend probe: ",
                        note))
}

skip_if_no_julia_highs <- function() {
  skip_if_tier_below("cross")
  if (!has_julia()) testthat::skip("julia not available (PATH or ~/.energyRt/config.yml)")
  skip_if_probe_failed("julia")
}

skip_if_no_pyomo <- function() {
  skip_if_tier_below("cross")
  if (!has_python()) testthat::skip("python not available (PATH or ~/.energyRt/config.yml)")
  skip_if_probe_failed("pyomo")
}

skip_if_no_gams <- function() {
  skip_if_tier_below("cross")
  if (!has_gams()) testthat::skip("gams not available (PATH or ~/.energyRt/config.yml)")
  skip_if_probe_failed("gams")
}

skip_if_no_neos <- function() {
  skip_if_tier_below("cross")
  if (!identical(Sys.getenv("ENERGYRT_TEST_NEOS"), "true")) {
    testthat::skip("NEOS tests are opt-in (set ENERGYRT_TEST_NEOS=true)")
  }
}

# `lang = "mm"` needs no external binary -- the matrix is built in R and solved
# by HiGHS in process -- so the guard is a package check, not a path check.
has_multimod <- function() {
  .toolchain("multimod", function() {
    requireNamespace("multimod", quietly = TRUE) &&
      requireNamespace("highs", quietly = TRUE)
  })
}

# Unlike the other probes this one runs the reader, because a version check
# cannot tell these builds apart: the GAMS template declares `sup(process)` and
# friends as SUBSETS, and a multimod whose reader registers a subset only as a
# mapping fails to convert every equation that sums over one. The failure is at
# model build, far from its cause, so a stale dependency must skip rather than
# fail. Milliseconds on a 6-symbol model, and memoised like the rest.
has_multimod_subsets <- function() {
  .toolchain("multimod_subsets", function() {
    if (!has_multimod()) return(FALSE)
    gms <- c("sets", "a  alpha", "p  parent", "s(p)  subset", ";",
             "sets", "m(s, a)  map", ";",
             "parameters", "pv(s, a)  val", ";",
             "variables", "vx(a)  x", ";",
             "equations", "eq(a)  e", ";",
             "eq(a).. vx(a) =e= sum(s$m(s, a), pv(s, a));")
    ms <- multimod::read_gams(gms, include = FALSE, verbose = FALSE)
    "s" %in% names(ms$sets) && !is.null(multimod::as_multimod(ms))
  })
}

skip_if_no_multimod <- function() {
  skip_if_tier_below("cross")
  if (!has_multimod()) {
    testthat::skip("multimod and/or highs not installed")
  }
  if (!has_multimod_subsets()) {
    testthat::skip("installed multimod does not resolve GAMS subsets")
  }
  skip_if_probe_failed("mm")
}
