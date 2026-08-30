# =========================================================================== #
# interp_guard.R -- tracked interpolation goldens: the tm tier models' modInp
# must reproduce a frozen baseline bit-for-bit (order/rounding-insensitive).
# Successor of dev-scripts/refactor_guard.R, now on energyRt::compare_inputs()
# and with baselines TRACKED in git (tests/testthat/goldens/interp/<tier>.rds,
# xz-compressed; Rbuildignored -- nightly-tier only, not in the tarball).
#
#   Rscript tools/test/interp_guard.R baseline   # (re)freeze the baselines
#   Rscript tools/test/interp_guard.R check      # rebuild + diff vs baselines
#
# make_goldens.R --suite=interp delegates to `baseline`; the nightly tier
# replays `check` through tests/testthat/test-interp-golden.R.
#
# Policy (same as the solved goldens, dev/TESTING.md §4): re-freeze only for
# an INTENDED interpolation change, in the same commit, with a NEWS bullet.
# =========================================================================== #

IG_MODELS <- c("tm_core", "tm_flows", "tm_io", "tm_policy", "tm_weather")
IG_DIR <- file.path("tests", "testthat", "goldens", "interp")

.ig_fixture_env <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) {
      e <- new.env(parent = globalenv())
      sys.source(file.path("tests", "testthat", "fixtures", "testing-models.R"),
                 envir = e)
      cache <<- e
    }
    cache
  }
})

# Build one tier's modInp with the CURRENT code (in-memory, deterministic).
ig_build <- function(nm) {
  mod <- .ig_fixture_env()[[nm]]()
  scen <- NULL
  invisible(utils::capture.output(   # swallow the build's progress banners
    scen <- suppressWarnings(suppressMessages(
      interpolate_model(mod, name = nm, ondisk = FALSE, overwrite = TRUE)))))
  scen@modInp
}

ig_baseline <- function(models = IG_MODELS) {
  dir.create(IG_DIR, recursive = TRUE, showWarnings = FALSE)
  for (nm in models) {
    f <- file.path(IG_DIR, paste0(nm, ".rds"))
    saveRDS(ig_build(nm), f, compress = "xz")
    sz <- file.size(f)
    cat(sprintf("  frozen %-10s -> %s (%.0f KB)\n", nm, f, sz / 1024))
    if (sz > 2e6) {
      warning("baseline ", f, " exceeds 2 MB -- consider a digest manifest ",
              "instead of a full modInp (dev/TESTING.md)", call. = FALSE)
    }
  }
  invisible(TRUE)
}

ig_check <- function(models = IG_MODELS) {
  ok <- TRUE
  for (nm in models) {
    f <- file.path(IG_DIR, paste0(nm, ".rds"))
    if (!file.exists(f)) {
      cat(nm, ": NO BASELINE (run: Rscript tools/test/interp_guard.R baseline)\n")
      ok <- FALSE
      next
    }
    base <- readRDS(f)            # compare_inputs upgrades old-layout objects
    cur <- ig_build(nm)
    res <- compare_inputs(base, cur, names = c("base", "cur"))
    clean <- inputs_cmp_identical(res)
    nbad <- sum(res$summary$status != "identical")
    cat(sprintf("%-10s params=%3d differing=%3d only_base=%d only_cur=%d%s\n",
                nm, nrow(res$summary), nbad,
                length(res$params$only_A), length(res$params$only_B),
                if (clean) "" else "  <-- DIFFERS"))
    if (!clean) {
      ok <- FALSE
      print(res)                  # full report incl. sets / constraints / misc
    }
  }
  cat("\n=== INTERP GOLDENS", if (ok) "PASS" else "FAIL", "===\n")
  invisible(ok)
}

if (sys.nframe() == 0L) {
  if (!file.exists("DESCRIPTION")) stop("Run from the package root.")
  suppressMessages(pkgload::load_all(".", quiet = TRUE))
  source(file.path("tests", "testthat", "helper-goldens.R"))
  mode <- commandArgs(trailingOnly = TRUE)
  mode <- if (length(mode)) mode[1] else "check"
  ok <- switch(mode,
    baseline = ig_baseline(),
    check = ig_check(),
    stop("Usage: Rscript tools/test/interp_guard.R [baseline|check]")
  )
  quit(status = if (isTRUE(ok)) 0L else 1L)
}
