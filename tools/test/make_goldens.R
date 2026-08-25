# =========================================================================== #
# make_goldens.R -- capture or refresh golden benchmarks (the ONLY writer of
# tests/testthat/goldens/*.json).
#
# Usage:
#   Rscript tools/test/make_goldens.R --suite=tm [--dry-run]
#   Rscript tools/test/make_goldens.R --suite=all
#
# Suites: tm (fast tier), utopia (fast/cross), utopia_nightly (R7/R11, only
# read at the nightly tier), interp (solver-free modInp baselines -- delegated
# to tools/test/interp_guard.R, stored as goldens/interp/*.rds).
#
# For every scenario in a suite: build, solve with the reference backend
# (GLPK), run verify_solution() -- capture is REFUSED if any identity is
# violated -- then store the objective and tracked values at the aggregation
# levels defined in tests/testthat/helper-goldens.R. An existing golden is
# diffed against the fresh capture before overwriting.
#
# Policy: regenerate goldens only for an INTENDED behavior change, in the same
# commit as the change, with a NEWS bullet. See dev/TESTING.md §4.
# =========================================================================== #

suppressMessages({
  if (!file.exists("DESCRIPTION")) stop("Run from the package root.")
  pkgload::load_all(".", quiet = TRUE)
  library(data.table)
  library(jsonlite)
})

# helper-goldens.R provides capture_tracked_values() etc.; helper-utopia.R the
# UTOPIA suite builders. Helper files are self-contained enough to source
# directly (helper-utopia's testthat::test_path fallback is bypassed by running
# from the package root).
source(file.path("tests", "testthat", "helper-goldens.R"))
source(file.path("tests", "testthat", "helper-utopia.R"))

# --------------------------------------------------------------------------- #
# suites: name -> list of entries; each entry builds an UNSOLVED model
# --------------------------------------------------------------------------- #

.fixture_env <- local({
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

SUITES <- list(
  tm = {
    tiers <- c("tm_core", "tm_flows", "tm_io", "tm_policy", "tm_weather")
    setNames(lapply(tiers, function(t) function() .fixture_env()[[t]]()), tiers)
  },
  utopia = lapply(ut_entries(), function(a) function() do.call(ut_build, a)),
  utopia_nightly = lapply(ut_nightly_entries(),
                          function(a) function() do.call(ut_build, a))
)

# --------------------------------------------------------------------------- #

solve_entry <- function(build_fn, name) {
  mod <- build_fn()
  suppressMessages(suppressWarnings(
    solve_model(mod, name = paste0("golden_", name),
              solver = solver_options$glpk, tmp.del = TRUE, wait = TRUE)
  ))
}

diff_entry <- function(old, new, name) {
  if (is.null(old)) {
    cat("  ", name, ": NEW entry (objective ", format(new$objective, digits = 12),
        ")\n", sep = "")
    return(invisible())
  }
  d_obj <- new$objective - as.numeric(old$objective)
  if (abs(d_obj) > TOL_SAME_SOLVER * max(1, abs(new$objective))) {
    cat("  ", name, ": objective ", format(as.numeric(old$objective), digits = 12),
        " -> ", format(new$objective, digits = 12),
        "  (delta ", format(d_obj, digits = 6), ")\n", sep = "")
  } else {
    cat("  ", name, ": objective unchanged\n", sep = "")
  }
  for (v in union(names(old$values), names(new$values))) {
    if (is.null(old$values[[v]])) cat("    +", v, "(new tracked variable)\n")
    else if (is.null(new$values[[v]])) cat("    -", v, "(no longer captured)\n")
  }
}

main <- function(args = commandArgs(trailingOnly = TRUE)) {
  suite_arg <- sub("^--suite=", "", grep("^--suite=", args, value = TRUE))
  dry <- "--dry-run" %in% args
  all_suites <- c(names(SUITES), "interp")
  if (!length(suite_arg)) stop("Pass --suite=<", paste(c(all_suites, "all"),
                                                       collapse = "|"), ">")
  suites <- if (identical(suite_arg, "all")) all_suites else suite_arg
  bad <- setdiff(suites, all_suites)
  if (length(bad)) stop("Unknown suite(s): ", paste(bad, collapse = ", "))

  # "interp" is solver-free and stores .rds baselines, not tracked-value JSON:
  # delegate to the interp guard (tools/test/interp_guard.R).
  if ("interp" %in% suites) {
    cat("== suite: interp ==\n")
    source(file.path("tools", "test", "interp_guard.R"))
    if (dry) cat("  (dry-run: nothing written)\n") else ig_baseline()
    suites <- setdiff(suites, "interp")
    if (!length(suites)) return(invisible())
  }

  dir.create(file.path("tests", "testthat", "goldens"),
             recursive = TRUE, showWarnings = FALSE)

  for (suite in suites) {
    cat("== suite:", suite, "==\n")
    path <- file.path("tests", "testthat", "goldens", paste0(suite, ".json"))
    old <- if (file.exists(path)) jsonlite::fromJSON(path, simplifyVector = FALSE) else list()
    fresh <- list()
    for (name in names(SUITES[[suite]])) {
      scen <- solve_entry(SUITES[[suite]][[name]], name)
      vs <- verify_solution(scen)
      if (!vs$ok) {
        print(vs)
        stop("REFUSED: '", name, "' fails solution invariants -- fix the model ",
             "or the code before freezing a benchmark.")
      }
      cap <- capture_tracked_values(scen)
      cap$captured <- list(
        date = format(Sys.Date()),
        version = as.character(utils::packageVersion("energyRt")),
        solver = "glpk"
      )
      diff_entry(old[[name]], cap, name)
      fresh[[name]] <- cap
    }
    if (dry) {
      cat("  (dry-run: nothing written)\n")
    } else {
      jsonlite::write_json(fresh, path, auto_unbox = TRUE, digits = NA,
                           pretty = TRUE)
      cat("  wrote", path, "\n")
    }
  }
}

if (sys.nframe() == 0L) main()
