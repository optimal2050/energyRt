# energyRt one-line installer (bootstrap)
# -----------------------------------------------------------------------------
# Works BEFORE energyRt is installed. Source it, then call install_energyRt():
#
#   source("https://raw.githubusercontent.com/optimal2050/energyRt/master/inst/install.R")
#   install_energyRt()                     # release (the repository default branch)
#   install_energyRt(branch = "dev")       # development version
#
# NOTE the URL says `master`: that is energyRt's default branch, not `main`.
#
# It installs `pak`, detects the OS, reports (does not run) any Linux system
# libraries to install, pre-installs energyRt's CRAN dependencies one by one
# with a pass/fail report, then installs energyRt itself. Finish with:
#
#   library(energyRt); en_setup()
#
# Self-contained: it depends on nothing but base R (+ pak, which it installs).
# -----------------------------------------------------------------------------

#' @param ref  Package source for pak, e.g. "optimal2050/energyRt". A branch may
#'   be given here directly as "optimal2050/energyRt@dev"; `branch` is the
#'   friendlier way to say the same thing.
#' @param branch Git branch to install from, e.g. "dev". `NULL` (default) takes
#'   the repository's default branch. Ignored with a warning if `ref` already
#'   carries an explicit "@ref".
#' @param deps_one_by_one Install the dependency layer package by package so a
#'   single failure is isolated and named rather than aborting the whole run.
install_energyRt <- function(ref = "optimal2050/energyRt",
                             branch = NULL,
                             deps_one_by_one = TRUE) {

  # Resolve `branch` into pak's "<repo>@<ref>" syntax.
  if (!is.null(branch)) {
    if (!is.character(branch) || length(branch) != 1L || !nzchar(branch)) {
      stop("`branch` must be a single non-empty string, e.g. branch = \"dev\".",
           call. = FALSE)
    }
    if (grepl("@", ref, fixed = TRUE)) {
      warning("`ref` already pins a branch (", ref, "); ignoring branch = \"",
              branch, "\".", call. = FALSE)
    } else {
      ref <- paste0(ref, "@", branch)
    }
  }

  os <- {
    s <- tolower(Sys.info()[["sysname"]])
    if (grepl("windows", s)) "windows" else if (grepl("darwin", s)) "macos" else "linux"
  }
  message("energyRt installer — detected OS: ", os)
  message("Source: ", ref)

  # 1. Ensure pak.
  if (!requireNamespace("pak", quietly = TRUE)) {
    message("Installing 'pak' ...")
    utils::install.packages("pak")
  }

  # 2. Linux system libraries — report only, never run sudo.
  if (os == "linux") {
    sq <- tryCatch(pak::pkg_sysreqs(ref), error = function(e) NULL)
    cmds <- if (!is.null(sq)) c(sq$pre_install, sq$install_scripts) else character(0)
    cmds <- cmds[nzchar(cmds)]
    if (length(cmds)) {
      message("\nSystem libraries may be required. Run in a terminal:")
      for (cmd in cmds) message("  sudo ", cmd)
      message("")
    }
  }

  # 3. Pre-install energyRt's direct CRAN imports, one by one, with a report.
  #    (Base packages are omitted; a single failure is isolated, not fatal.)
  deps <- c(
    "generics", "data.table", "DBI", "RSQLite", "tibble", "tidyr", "dplyr",
    "rlang", "stringr", "lubridate", "purrr", "arrow", "progressr", "tictoc",
    "cli", "zoo", "registry", "options", "glue", "plyr",
    # suggested -- plots and reports (optional but recommended):
    "ggplot2", "patchwork", "knitr", "rmarkdown", "tinytex", "sf",
    # optional -- the process-designer GUI (process_designer()):
    "shiny", "DT",
    # optional -- the NEOS client, which solves remotely with no local solver
    "httr2", "xml2", "base64enc"
  )

  failed <- character(0)
  if (isTRUE(deps_one_by_one)) {
    for (pkg in deps) {
      ok <- tryCatch(
        {
          pak::pkg_install(pkg, ask = FALSE)
          message("  [ok]   ", pkg)
          TRUE
        },
        error = function(e) {
          message("  [FAIL] ", pkg, ": ", conditionMessage(e))
          FALSE
        }
      )
      if (!ok) failed <- c(failed, pkg)
    }
  }

  # 4. Install energyRt itself (only if the dependency layer is clean).
  if (length(failed) == 0) {
    message("\nDependencies ready — installing energyRt (", ref, ") ...")
    pak::pkg_install(ref, ask = FALSE)
    message("\nDone. Next, in a fresh session run:  library(energyRt); en_setup()")
  } else {
    message("\nFailed packages: ", paste(failed, collapse = ", "))
    message("Install their system libraries (above), then re-run install_energyRt().")
  }

  invisible(list(os = os, failed = failed))
}
