# Functions t install external packages and libraries

#' Install Julia packages
#'
#' @param pkgs A character vector of Julia packages to install. The default is
#'  \code{c("JuMP", "HiGHS", "Cbc", "Clp", "RData", "RCall", "CodecBzip2",
#'  "Gadfly", "DataFrames", "CSV", "SQLite", "Dates")}.
#'  If you have pre-installed CPLEX or Gurobi, you can add them to the list.
#'
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
    pkgs <- c("JuMP", "HiGHS", "Cbc", "Clp", "RData", "RCall", "CodecBzip2",
              "Gadfly", "DataFrames", "CSV", "SQLite", "Dates")
  }

  # check if Julia is installed and available on the path
  
  jp <- get_julia_path() |> paste0("julia")
  if (!file.exists(jp)) {
    # try default path
    jp <- Sys.which("julia")
  }
  if (!file.exists(jp)) {
    stop("\nCannot locate julia executable on the path.", 
         "In Julia is not installed, download and install from https://julialang.org/downloads/")
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

  # run Julia with the script
  system2(jp, c("--color=yes", tmp_file))

  # remove the temporary file
  unlink(tmp_file)

  return(invisible())
}


#' Install Python/Pyomo dependencies
#'
#' @description
#' Installs the Python library layer energyRt needs for the Pyomo backend. When
#' a conda/mamba executable is available (the recommended route) it creates or
#' reuses a named environment and installs `pyomo`, a solver (`coincbc`), and the
#' result-IO helpers from conda-forge. Otherwise it falls back to `pip` into the
#' configured Python (see [set_python_path()]); note that `pip` cannot supply the
#' CBC solver binary, which must then be installed separately.
#'
#' This installs *libraries only*. It does not install Python or conda
#' themselves --- see [en_check_dependencies()] for guidance on the system layer.
#'
#' @param env character. Name of the conda environment to create/use.
#' @param packages character vector of Python packages to install.
#' @param solver character. Conda solver package to install (e.g. `"coincbc"`).
#' @param channel character. Conda channel.
#' @param use_conda logical or NULL. Force conda (`TRUE`) or pip (`FALSE`); `NULL`
#'   auto-detects (conda if found, else pip).
#'
#' @return NULL, invisibly. Verify with [en_check_pyomo()].
#' @family solver
#' @export
#'
#' @examples
#' \dontrun{
#' en_install_python_deps()                 # conda env "energyRt" with pyomo + cbc
#' en_install_python_deps(use_conda = FALSE) # pip into current python
#' }
en_install_python_deps <- function(env = "energyRt",
                                   packages = c("pyomo", "pandas", "pyarrow"),
                                   solver = "coincbc",
                                   channel = "conda-forge",
                                   use_conda = NULL) {
  conda <- .en_which("mamba")
  if (is.na(conda)) conda <- .en_which("conda")
  want_conda <- if (is.null(use_conda)) !is.na(conda) else isTRUE(use_conda)

  if (want_conda) {
    if (is.na(conda)) {
      stop("conda/mamba not found on PATH. Install miniforge ",
           "(https://conda-forge.org/) or call with use_conda = FALSE for pip.",
           call. = FALSE)
    }
    message("Using ", conda, " to install into conda env '", env, "'")
    envs <- tryCatch(system2(conda, c("env", "list"), stdout = TRUE),
                     error = function(e) character(0))
    has_env <- any(grepl(paste0("^", env, "([ ]|$)"), trimws(envs)))
    if (!has_env) {
      message("Creating conda env '", env, "' ...")
      system2(conda, c("create", "-y", "-n", env, "python"))
    }
    system2(conda, c("install", "-y", "-n", env, "-c", channel,
                     unique(c(packages, solver))))
  } else {
    py <- .en_which("python", get_python_path())
    if (is.na(py)) py <- .en_which("python3", get_python_path())
    if (is.na(py)) {
      stop("python not found. Call set_python_path() or install Python first.",
           call. = FALSE)
    }
    message("Using pip via ", py)
    system2(py, c("-m", "pip", "install", "--upgrade", packages))
    message("Note: pip cannot provide the CBC solver binary. Install a ",
            "Pyomo-compatible solver (e.g. CBC) separately, or use conda.")
  }
  return(invisible())
}


#' Install the energyRt dependency library layer
#'
#' @description
#' Orchestrator that auto-installs the *safe* library layer: optional R packages,
#' Julia solver packages (via [en_install_julia_pkgs()]) when Julia is present,
#' and Python/Pyomo packages (via [en_install_python_deps()]) when Python or
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
    r_opt <- c("jsonlite", "readxl", "openxlsx")
    r_missing <- r_opt[!vapply(r_opt, requireNamespace, logical(1), quietly = TRUE)]
    if (length(r_missing)) {
      message("Installing optional R packages: ", paste(r_missing, collapse = ", "))
      try(utils::install.packages(r_missing), silent = TRUE)
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
    if (has_py || has_conda) {
      message("Installing Python/Pyomo dependencies ...")
      try(en_install_python_deps(), silent = TRUE)
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

