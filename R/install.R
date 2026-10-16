# Functions to install external packages and libraries

# The optimal2050 siblings and the bundled solver binary: every energyRt
# dependency that is NOT on CRAN. `geoscales` and `timescales` are in Imports,
# the rest in Suggests. Two consumers read this: `en_install_deps()`, which
# installs them and subtracts them from the CRAN set, and `en_check_packages()`.
# `inst/install.R` carries the same list because it must run BEFORE energyRt
# exists -- keep the two in step.
.en_sibling_refs <- function() {
  c("optimal2050/nestedscales", "optimal2050/timescales",
    "optimal2050/geoscales", "optimal2050/glpsol",
    "optimal2050/multimod", "optimal2050/mmcloud")
}

.en_sibling_pkgs <- function() sub(".*/", "", .en_sibling_refs())

#' Install Julia packages
#'
#' @param pkgs A character vector of Julia packages to install. The default is
#'  \code{c("JuMP", "HiGHS", "GLPK", "Cbc", "Arrow", "RData", "DataFrames")}:
#'  the solvers the shipped `julia_*` presets name, plus `Arrow` for the default
#'  data exchange (see [get_exchange_format()]), `RData` for the
#'  `julia_highs_rdata` preset, and `DataFrames`, which the generated code uses.
#'  The `julia_cplex*` and `julia_cuopt*` presets need `CPLEX` / `cuOpt`, which
#'  are licensed or GPU-specific and so are not installed by default -- add them
#'  to `pkgs` yourself. `Dates` is a Julia standard library and needs no install.
#'
#' @param update logical; also run `Pkg.update()` after adding the packages.
#' @return NULL if the completion is successful. The verification of the installation is
#' done by the user or by the function \code{en_check_julia()}.
#' @export
#'
#' @examples
#' \dontrun{
#' en_install_julia_pkgs()
#' }
en_install_julia_pkgs <- function(pkgs = NULL, update = FALSE) {

  if (is.null(pkgs)) {
    # One entry per Julia package the shipped presets actually name: HiGHS,
    # GLPK and Cbc are the solvers behind `julia_highs*`, `julia_glpk` and
    # `julia_cbc`. `Arrow` reads the model data and writes the solution (the
    # default exchange); `RData` serves the `julia_highs_rdata` preset;
    # `DataFrames` is used by the generated code. Neither `CSV` nor `SQLite` is
    # used by the Julia path -- the generated code writes csv with plain
    # `println` and never opens a database.
    pkgs <- c("JuMP", "HiGHS", "GLPK", "Cbc", "Arrow", "RData", "DataFrames")
  }

  # Locate the julia executable. `.en_which()` is the same resolver
  # `en_check_dependencies()` uses: it tries the energyRt-configured path
  # (which carries a trailing /) with and without the `.exe` suffix, then the
  # system PATH. Resolving it by hand here used to miss `julia.exe` on Windows
  # and, with no path configured, tested the RELATIVE name `julia` -- which
  # matches any file or directory of that name in the working directory.
  jp <- .en_which("julia", get_julia_path())
  if (is.na(jp)) {
    stop("Cannot locate the julia executable on the path. If Julia is not ",
         "installed, download it from https://julialang.org/downloads/",
         call. = FALSE)
  }

  message("Using Julia at ", jp, "\n")
  
  # create a temporary file to install Julia packages
  tmp_file <- tempfile("julia_install_", fileext = ".jl")
  # tmp <- file("tmp/julia_install.jl", open = "wt")
  tmp_con <- file(tmp_file, open = "wt")

  # write the script to install Julia packages
  writeLines("using Pkg", tmp_con)
  for (pkg in pkgs) {
    writeLines(paste0("Pkg.add(\"", pkg, "\")"), tmp_con, sep = "\n")
  }
  if (update) {
    writeLines("Pkg.update()", tmp_con)
  }
  close(tmp_con)

  # run Julia with the script. The exit status is the only signal that
  # `Pkg.add()` worked: discarding it let this function report success for a
  # failed install, which its own @return promised it would not.
  status <- system2(jp, c("--color=yes", tmp_file))

  # remove the temporary file
  unlink(tmp_file)

  if (!identical(as.integer(status), 0L)) {
    stop("Julia exited with status ", status,
         " -- the packages were not installed. The Julia output above says why.",
         call. = FALSE)
  }

  return(invisible())
}


#' Install Python/Pyomo dependencies (deprecated)
#'
#' Superseded by `multimod::mm_setup_python()`, which owns the one conda
#' environment shared by energyRt and multimod. This forwards to it and will be
#' removed.
#'
#' `solver`, `channel` and `use_conda` are no longer honoured: the replacement
#' installs with `pip` inside a conda environment, so it cannot supply the CBC
#' binary. Install a shell solver separately if you still need one; `highspy`
#' is part of the default package set.
#'
#' @param env character. Forwarded as `env_name`.
#' @param packages character vector of Python packages to install.
#' @param solver,channel,use_conda Ignored, with a warning when set.
#'
#' @return NULL, invisibly. Verify with [en_check_pyomo()].
#' @family solver
#' @export
#' @examples
#' \dontrun{
#' # create (or top up) the python environment the Pyomo backend uses
#' en_install_python_deps()
#'
#' # a named environment and an explicit solver
#' en_install_python_deps(env = "energyRt", solver = "highs")
#' }
en_install_python_deps <- function(env = "multimod",
                                   packages = NULL,
                                   solver = NULL,
                                   channel = NULL,
                                   use_conda = NULL) {
  lifecycle_msg <- paste0(
    "en_install_python_deps() is deprecated. Use ",
    "multimod::mm_setup_python() instead - energyRt and multimod now share ",
    "one conda environment.")
  warning(lifecycle_msg, call. = FALSE)

  ignored <- c("solver", "channel", "use_conda")[
    !vapply(list(solver, channel, use_conda), is.null, logical(1))]
  if (length(ignored) > 0) {
    warning("Ignored by mm_setup_python(): ", paste(ignored, collapse = ", "),
            ". It installs with pip and cannot supply a solver binary.",
            call. = FALSE)
  }

  if (!requireNamespace("multimod", quietly = TRUE)) {
    stop("This needs the 'multimod' package.
",
         '  pak::pak("optimal2050/multimod")', call. = FALSE)
  }

  args <- list(env_name = env)
  # NULL would mean "install nothing"; omit so mm_setup_python()'s list wins.
  if (!is.null(packages)) args$packages <- packages
  do.call(multimod::mm_setup_python, args)
  invisible(NULL)
}


#' Install the energyRt dependency library layer
#'
#' @description
#' Orchestrator that auto-installs the *safe* library layer: optional R packages,
#' Julia solver packages (via [en_install_julia_pkgs()]) when Julia is present,
#' and Python/Pyomo packages (via [multimod::mm_setup_python()]) when Python or
#' conda is present. System software that is missing (Julia, Python, conda, GAMS)
#' is *not* auto-installed --- the function reports it and points to the
#' platform-specific instructions printed by [en_check_dependencies()].
#'
#' @param julia,python,r logical. Whether to install each layer.
#' @param recheck logical. Run [en_check_dependencies()] before and after.
#'
#' @return NULL, invisibly.
#' @family solver
#' @export
#'
#' @examples
#' \dontrun{
#' en_install_deps()
#' }
en_install_deps <- function(julia = TRUE, python = TRUE, r = TRUE, recheck = TRUE) {
  if (recheck) en_check_dependencies()

  if (r) {
    # gdxtools is not on CRAN (GAMS GDX bridge); others are optional Suggests.
    if (!requireNamespace("gdxtools", quietly = TRUE)) {
      if (requireNamespace("pak", quietly = TRUE)) {
        message("Installing gdxtools (GDX bridge) ...")
        try(pak::pkg_install("lolow/gdxtools"), silent = TRUE)
      } else {
        cli::cli_alert_info(
          "gdxtools missing; install pak then pak::pkg_install('lolow/gdxtools').")
      }
    }
    # The optional CRAN layer is read off Suggests rather than hard-coded, so it
    # cannot drift from DESCRIPTION the way a copied list does. Dev-only tools
    # and the siblings (handled below, none of which is on CRAN) are excluded.
    sug <- tryCatch(
      trimws(gsub("[(][^)]*[)]", "", strsplit(
        utils::packageDescription("energyRt", fields = "Suggests"), ",")[[1]])),
      error = function(e) character(0))
    sug <- sug[nzchar(sug)]
    r_opt <- setdiff(sug, c("pak", "testthat", "devtools", "withr",
                            .en_sibling_pkgs()))
    r_missing <- r_opt[!vapply(r_opt, requireNamespace, logical(1), quietly = TRUE)]
    if (length(r_missing)) {
      message("Installing optional R packages: ", paste(r_missing, collapse = ", "))
      tryCatch(utils::install.packages(r_missing),
               error = function(e) message("  failed: ", conditionMessage(e)))
      # requireNamespace() is the verdict, not install.packages()' own return:
      # it warns rather than errors for a package it cannot reach.
      still <- r_missing[!vapply(r_missing, requireNamespace, logical(1), quietly = TRUE)]
      if (length(still)) {
        cli::cli_alert_warning(
          "Still missing after install: {paste(still, collapse = ', ')}")
      }
    }

    # The optimal2050 siblings and the bundled solver binary -- GitHub, not CRAN.
    for (sib in .en_sibling_refs()) {
      nm <- sub(".*/", "", sib)
      if (requireNamespace(nm, quietly = TRUE)) next
      if (!requireNamespace("pak", quietly = TRUE)) {
        cli::cli_alert_info(
          "{nm} missing; install pak then pak::pkg_install('{sib}').")
        next
      }
      message("Installing ", nm, " ...")
      tryCatch(pak::pkg_install(sib),
               error = function(e)
                 cli::cli_alert_warning("{nm}: {conditionMessage(e)}"))
    }
  }

  if (julia) {
    if (!is.na(.en_which("julia", get_julia_path()))) {
      message("Installing Julia packages ...")
      en_install_julia_pkgs()
    } else {
      cli::cli_alert_warning(
        "Julia not found; skipping Julia packages. Install Julia (juliaup) first.")
    }
  }

  if (python) {
    has_py <- !is.na(.en_which("python", get_python_path())) ||
      !is.na(.en_which("python3", get_python_path()))
    has_conda <- !is.na(.en_which("mamba")) || !is.na(.en_which("conda"))
    if (!requireNamespace("multimod", quietly = TRUE)) {
      cli::cli_alert_warning(paste(
        "Python setup is owned by multimod, which is not installed; skipping.",
        "Install it with pak::pkg_install('optimal2050/multimod')."))
    } else if (has_py || has_conda) {
      message("Installing Python/Pyomo dependencies ...")
      tryCatch(multimod::mm_setup_python(),
               error = function(e)
                 cli::cli_alert_warning("mm_setup_python(): {conditionMessage(e)}"))
    } else {
      cli::cli_alert_warning(
        "Python/conda not found; skipping. Install miniforge first.")
    }
  }

  if (recheck) {
    cli::cli_h2("Re-checking dependencies")
    en_check_dependencies()
  }
  return(invisible())
}


#' Set up energyRt after installation
#'
#' @description
#' A one-call "am I ready?" entry point to run *after* energyRt is installed. It
#' reports the operating system, prints the system libraries to install on Linux
#' (report only --- it never runs `sudo`), and prints the status tables from
#' [en_check_dependencies()] (solver backends) and [en_check_packages()] (R
#' packages, training extras, and external tools). It installs nothing itself:
#' use the
#' `install_energyRt()` bootstrap (see the Installation article) to install
#' energyRt and its dependencies, and [en_install_deps()] to set up the
#' solver-backend library layer.
#'
#' @param ref character. Package reference used to query system requirements on
#'   Linux (default `"optimal2050/energyRt"`).
#'
#' @return A list, invisibly, with elements `os` (character), `deps` (the tibble
#'   from [en_check_dependencies()]) and `packages` (the tibble from
#'   [en_check_packages()]).
#' @family solver
#' @export
#'
#' @examples
#' \dontrun{
#' en_setup()
#' }
en_setup <- function(ref = "optimal2050/energyRt") {
  os <- .en_os()
  cli::cli_h2("energyRt setup")
  cli::cli_alert_info("Operating system: {os}")

  # System libraries: report only --- never run sudo on the user's behalf.
  if (os == "linux") {
    if (requireNamespace("pak", quietly = TRUE)) {
      sq <- tryCatch(pak::pkg_sysreqs(ref), error = function(e) NULL)
      cmds <- if (!is.null(sq)) c(sq$pre_install, sq$install_scripts) else character(0)
      cmds <- cmds[nzchar(cmds)]
      if (length(cmds)) {
        cli::cli_alert_info(paste0(
          "System libraries may be required. Run these commands in a terminal ",
          "(you will be prompted for your password):"))
        cli::cli_code(paste("sudo", cmds))
      } else {
        cli::cli_alert_success("No additional system libraries reported.")
      }
    } else {
      cli::cli_alert_info(
        'Install {.pkg pak} to list required system libraries: install.packages("pak")')
    }
  } else {
    cli::cli_alert_success(
      "No system libraries to install on {os} (on Windows, GLPK ships with Rtools).")
  }

  # Dependency status: reuse the existing read-only reports.
  deps <- en_check_dependencies()
  pkgs <- en_check_packages()

  cli::cli_alert_info(
    "Next: install a solver backend (see the Installation article), or run {.code en_install_deps()} for the solver library layer.")

  invisible(list(os = os, deps = deps, packages = pkgs))
}

