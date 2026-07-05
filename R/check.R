#' Find and replace special characters
#'
#' @include plot.R
#'
#' @param x character vector
#' @param pattern regular expression pattern to match special characters
#' @param repl replacement character
#'
#' @returns character vector with special characters replaced
#' @export
#'
#' @examples
#' en_replace_specials(c("valid", "invalid!", "in-valid", "valid_1", "invalid.2"))
#' en_replace_specials(c("valid", "invalid!"), "[\\.\\^\\$\\*\\+\\?\\!]", "_fixed")
en_replace_specials <- function(x, pattern = "[^[:alnum:]]", repl = "_") {
  # x - character
  gsub(pattern, repl, x)
}

#' @describeIn en_replace_specials Return `TRUE` if any element contains a special character.
#' @export
en_has_specials <- function(x, pattern = "[^[:alnum:]]") {
  # x - character
  if (!is.character(x)) stop("x must be a character vector")
  any(grepl(pattern, x))
}

#' @describeIn en_replace_specials Return the match positions of special characters (via [gregexpr()]).
#' @export
en_find_specials <- function(x, pattern = "[^[:alnum:]]") {
  # x - character
  if (!is.character(x)) stop("x must be a character vector")
  gregexpr(pattern, x)
}


check_package <- function(pkg) {
  # Check if the package is installed
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("Package '", pkg, "' is not installed.")
  }

}


# Dependency detection ---------------------------------------------------------
# Helpers and `en_check_*()` detectors for the external software energyRt can
# use as solver backends (GLPK, Julia/JuMP, Python/Pyomo, GAMS) plus the GDX
# bridge. Each detector returns a one-row tibble with a common schema so that
# `en_check_dependencies()` can stack them into a status table. Detectors are
# read-only: they locate an executable and (cheaply) probe its version; they
# never install anything. See `en_install_deps()` for the install side.

# Locate an executable, preferring an energyRt-configured path (which carries a
# trailing "/"), then the system PATH. Returns the resolved file path or NA.
.en_which <- function(exe, path = NULL) {
  cand <- character(0)
  if (!is.null(path) && nzchar(path)) {
    cand <- c(paste0(path, exe), paste0(path, exe, ".exe"))
  }
  sw <- Sys.which(exe)
  if (nzchar(sw)) cand <- c(cand, unname(sw))
  cand <- cand[file.exists(cand)]
  if (length(cand)) unname(cand[1]) else NA_character_
}

# Probe an executable: locate it and, optionally, capture a version string.
# `run = FALSE` skips execution (e.g. GAMS, which has no safe non-interactive
# version flag) and reports presence only.
.en_probe <- function(exe, version_args = "--version", path = NULL, run = TRUE) {
  bin <- .en_which(exe, path)
  if (is.na(bin)) {
    return(list(found = FALSE, path = NA_character_, version = NA_character_))
  }
  ver <- NA_character_
  if (run) {
    out <- tryCatch(
      suppressWarnings(system2(bin, version_args, stdout = TRUE, stderr = TRUE)),
      error = function(e) character(0)
    )
    out <- out[nzchar(out)]
    if (length(out)) ver <- out[1]
  }
  list(found = TRUE, path = bin, version = ver)
}

# Build a one-row status tibble with the schema shared by all detectors.
.en_status_row <- function(component, required, probe, note = NA_character_) {
  tibble::tibble(
    component = component,
    required  = required,
    found     = probe$found,
    version   = probe$version %||% NA_character_,
    path      = probe$path %||% NA_character_,
    note      = note
  )
}

# Normalise the operating system to "windows" | "macos" | "linux".
.en_os <- function() {
  s <- tolower(Sys.info()[["sysname"]])
  if (grepl("windows", s)) {
    "windows"
  } else if (grepl("darwin", s)) {
    "macos"
  } else {
    "linux"
  }
}

# Probe an installed R package: found?, version, library path.
.en_pkg_probe <- function(pkg) {
  ok <- requireNamespace(pkg, quietly = TRUE)
  list(
    found   = ok,
    path    = if (ok) find.package(pkg)[1] else NA_character_,
    version = if (ok) as.character(utils::packageVersion(pkg)) else NA_character_
  )
}

#' Detect external solver software and dependencies
#'
#' @description
#' Read-only detectors that report whether each external backend energyRt can
#' use is installed and runnable. `en_check_dependencies()` runs all of them and
#' prints a status table; the individual `en_check_*()` functions probe a single
#' backend and return a one-row tibble (`component`, `required`, `found`,
#' `version`, `path`, `note`). None of these functions install anything --- see
#' [en_install_deps()].
#'
#' Each detector honours the path configured via the corresponding
#' `set_*_path()` (e.g. [set_julia_path()]) and falls back to the system `PATH`.
#'
#' @return A one-row (`en_check_*`) or multi-row (`en_check_dependencies`) tibble,
#'   returned invisibly for `en_check_dependencies()` which also prints a table.
#' @family solver
#' @name en_check
#' @examples
#' \dontrun{
#' en_check_dependencies()
#' en_check_julia()
#' }
NULL

#' @rdname en_check
#' @export
en_check_glpk <- function() {
  pr <- .en_probe("glpsol", "--version", get_glpk_path())
  .en_status_row("GLPK (glpsol)", required = FALSE, pr,
    note = if (!pr$found) "open-source LP/MILP solver; see set_glpk_path()" else NA_character_)
}

#' @rdname en_check
#' @export
en_check_julia <- function() {
  pr <- .en_probe("julia", "--version", get_julia_path())
  .en_status_row("Julia", required = FALSE, pr,
    note = if (!pr$found) "install via juliaup; then en_install_julia_pkgs()" else
      "run en_check_julia_pkgs() to verify JuMP/HiGHS")
}

#' @rdname en_check
#' @export
en_check_python <- function() {
  pr <- .en_probe("python", "--version", get_python_path())
  if (!pr$found || !.en_is_real_python(pr$version)) {
    pr <- .en_probe("python3", "--version", get_python_path())
  }
  # Guard against the Windows Store "App execution alias" stub, which is a fake
  # python.exe that prints "Python was not found ..." and exits 0.
  if (pr$found && !.en_is_real_python(pr$version)) {
    return(.en_status_row("Python", required = FALSE,
      list(found = FALSE, path = NA_character_, version = NA_character_),
      note = paste0("found a non-functional 'python' (e.g. Windows Store alias); ",
                    "install real Python via miniforge/conda")))
  }
  .en_status_row("Python", required = FALSE, pr,
    note = if (!pr$found) "install via miniforge/conda; then en_install_python_deps()" else NA_character_)
}

# A working python --version prints "Python X.Y.Z"; the Store-alias stub does not.
.en_is_real_python <- function(version) {
  !is.na(version) && grepl("^Python\\s+\\d", version)
}

#' @rdname en_check
#' @export
en_check_pyomo <- function() {
  pyc <- en_check_python()
  if (!isTRUE(pyc$found)) {
    return(.en_status_row("Pyomo", required = FALSE,
      list(found = FALSE, path = NA_character_, version = NA_character_),
      note = "needs a working Python first"))
  }
  py <- pyc$path
  ver <- tryCatch(
    suppressWarnings(system2(py,
      c("-c", shQuote("import pyomo; print(pyomo.version.version)")),
      stdout = TRUE, stderr = TRUE)),
    error = function(e) character(0)
  )
  ver <- ver[nzchar(ver)]
  # A real success prints a bare version number (e.g. "6.7.3").
  ok <- length(ver) > 0 && grepl("^[0-9]+\\.[0-9]+", ver[length(ver)])
  cbc <- .en_which("cbc")
  .en_status_row("Pyomo", required = FALSE,
    list(found = ok, path = if (ok) py else NA_character_,
         version = if (ok) ver[length(ver)] else NA_character_),
    note = if (!ok) "en_install_python_deps()" else
      if (is.na(cbc)) "Pyomo ok; no 'cbc' solver on PATH" else
        paste0("solver cbc: ", cbc))
}

#' @rdname en_check
#' @export
en_check_gams <- function() {
  # GAMS has no safe non-interactive version flag; detect presence only.
  pr <- .en_probe("gams", path = get_gams_path(), run = FALSE)
  .en_status_row("GAMS", required = FALSE, pr,
    note = if (!pr$found) "proprietary; license required; see set_gams_path()" else
      "present (version not probed)")
}

#' @rdname en_check
#' @export
en_check_gdx <- function() {
  ok <- requireNamespace("gdxtools", quietly = TRUE)
  ver <- if (ok) as.character(utils::packageVersion("gdxtools")) else NA_character_
  .en_status_row("gdxtools (GDX bridge)", required = FALSE,
    list(found = ok, path = NA_character_, version = ver),
    note = if (!ok) "for GAMS GDX I/O: pak::pkg_install('lolow/gdxtools')" else NA_character_)
}

# Verify Julia solver packages are installed (slow: starts a Julia session).
#' @rdname en_check
#' @param pkgs character vector of Julia package names to verify.
#' @export
en_check_julia_pkgs <- function(pkgs = c("JuMP", "HiGHS")) {
  jp <- .en_which("julia", get_julia_path())
  if (is.na(jp)) {
    return(.en_status_row("Julia packages", required = FALSE,
      list(found = FALSE, path = NA_character_, version = NA_character_),
      note = "Julia not found"))
  }
  expr <- sprintf(
    "import Pkg; ks = keys(Pkg.project().dependencies); for p in [%s]; println(p, '=', in(p, ks)); end",
    paste0('"', pkgs, '"', collapse = ", "))
  out <- tryCatch(
    suppressWarnings(system2(jp, c("--startup-file=no", "-e", shQuote(expr)),
      stdout = TRUE, stderr = TRUE)),
    error = function(e) character(0)
  )
  missing <- pkgs[!vapply(pkgs, function(p) any(grepl(paste0("^", p, "=true"), out)), logical(1))]
  ok <- length(missing) == 0
  .en_status_row("Julia packages", required = FALSE,
    list(found = ok, path = jp, version = paste(pkgs, collapse = ",")),
    note = if (ok) "JuMP/HiGHS present" else
      paste0("missing: ", paste(missing, collapse = ", "), " -> en_install_julia_pkgs()"))
}

#' @rdname en_check
#' @param solver_pkgs verify Julia solver packages too (slower; starts Julia).
#' @export
en_check_dependencies <- function(solver_pkgs = FALSE) {
  rows <- list(
    en_check_glpk(),
    en_check_julia(),
    en_check_python(),
    en_check_pyomo(),
    en_check_gams(),
    en_check_gdx()
  )
  if (solver_pkgs) rows <- c(rows, list(en_check_julia_pkgs()))
  tab <- dplyr::bind_rows(rows)

  # Report --------------------------------------------------------------------
  cli::cli_h2("energyRt dependency check")
  for (i in seq_len(nrow(tab))) {
    r <- tab[i, ]
    sym <- if (isTRUE(r$found)) cli::col_green(cli::symbol$tick) else
      cli::col_red(cli::symbol$cross)
    ver <- if (!is.na(r$version)) cli::col_silver(paste0(" (", r$version, ")")) else ""
    cli::cli_text("{sym} {.strong {r$component}}{ver}")
    if (!is.na(r$note)) cli::cli_text("    {cli::col_silver(r$note)}")
  }

  backends <- c("GLPK (glpsol)", "Julia", "Python", "GAMS")
  have <- tab$found[match(backends, tab$component)]
  if (any(have, na.rm = TRUE)) {
    cli::cli_alert_success(
      "At least one solver backend is available ({paste(backends[which(have)], collapse = ', ')}).")
  } else {
    cli::cli_alert_danger("No solver backend found. Install at least one:")
    cli::cli_ul(c(
      "GLPK   - open-source; https://www.gnu.org/software/glpk/ then set_glpk_path()",
      "Julia  - https://github.com/JuliaLang/juliaup then en_install_julia_pkgs()",
      "Python - https://conda-forge.org/ (miniforge) then en_install_python_deps()",
      "GAMS   - proprietary; https://www.gams.com/ then set_gams_path()"
    ))
  }
  cli::cli_alert_info("Auto-install the library layer with en_install_deps().")

  invisible(tab)
}


#' Check R package and external-tool dependencies
#'
#' @description
#' Companion to [en_check_dependencies()] (which covers the solver *backends*).
#' `en_check_packages()` reports whether energyRt's own R package dependencies and
#' the extra packages used in the training course are installed, plus the external
#' tools some of them need: a LaTeX engine for PDF reports (via `tinytex`) and
#' MuseScore for the `gm` music output. Like [en_check_dependencies()] it prints a
#' status table and returns the underlying tibble invisibly.
#'
#' @param extras character vector of optional / training R packages to check.
#' @param external logical; also probe external tools (LaTeX engine, MuseScore).
#'
#' @return A tibble (`component`, `required`, `found`, `version`, `path`, `note`),
#'   invisibly.
#' @family solver
#' @export
#' @examples
#' \dontrun{
#' en_check_packages()
#' }
en_check_packages <- function(
    extras = c("ggplot2", "patchwork", "knitr", "rmarkdown", "tinytex", "sf", "gm"),
    external = TRUE) {

  # energyRt's own CRAN imports (from DESCRIPTION), minus base/recommended pkgs.
  imports <- tryCatch(
    strsplit(utils::packageDescription("energyRt", fields = "Imports"), ",")[[1]],
    error = function(e) character(0))
  imports <- trimws(gsub("\\(.*?\\)", "", imports))     # drop version constraints
  imports <- imports[nzchar(imports)]
  base_pkgs <- rownames(utils::installed.packages(priority = "base"))
  imports <- setdiff(imports, base_pkgs)

  mk_rows <- function(pkgs, required) {
    lapply(pkgs, function(p) {
      pr <- .en_pkg_probe(p)
      .en_status_row(p, required = required, pr,
        note = if (!pr$found) paste0("install: pak::pkg_install('", p, "')") else NA_character_)
    })
  }

  rows <- c(mk_rows(imports, TRUE), mk_rows(extras, FALSE))

  if (isTRUE(external)) {
    # LaTeX engine (for PDF reports; `tinytex` can provide one).
    latex_bin <- Sys.which("pdflatex")
    latex_ok  <- nzchar(latex_bin) ||
      (requireNamespace("tinytex", quietly = TRUE) && isTRUE(tinytex::is_tinytex()))
    rows <- c(rows, list(.en_status_row("LaTeX engine", required = FALSE,
      list(found = latex_ok,
           path = if (nzchar(latex_bin)) unname(latex_bin) else NA_character_,
           version = NA_character_),
      note = if (!latex_ok) "for PDF reports: tinytex::install_tinytex()" else NA_character_)))

    # MuseScore (needed by the `gm` package to render scores).
    ms <- .en_which("mscore")
    for (exe in c("MuseScore4", "MuseScore3", "musescore")) if (is.na(ms)) ms <- .en_which(exe)
    rows <- c(rows, list(.en_status_row("MuseScore", required = FALSE,
      list(found = !is.na(ms), path = ms, version = NA_character_),
      note = if (is.na(ms)) "for gm music output: https://musescore.org" else NA_character_)))
  }

  tab <- dplyr::bind_rows(rows)

  # Report --------------------------------------------------------------------
  cli::cli_h2("energyRt package check")
  for (i in seq_len(nrow(tab))) {
    r <- tab[i, ]
    sym <- if (isTRUE(r$found)) {
      cli::col_green(cli::symbol$tick)
    } else if (isTRUE(r$required)) {
      cli::col_red(cli::symbol$cross)
    } else {
      cli::col_yellow(cli::symbol$cross)
    }
    ver <- if (!is.na(r$version)) cli::col_silver(paste0(" (", r$version, ")")) else ""
    cli::cli_text("{sym} {.strong {r$component}}{ver}")
    if (!is.na(r$note)) cli::cli_text("    {cli::col_silver(r$note)}")
  }

  miss_req <- tab$component[tab$required & !tab$found]
  miss_opt <- tab$component[!tab$required & !tab$found]
  if (length(miss_req)) {
    cli::cli_alert_danger("Missing required packages: {paste(miss_req, collapse = ', ')}.")
  } else {
    cli::cli_alert_success("All required energyRt packages are installed.")
  }
  if (length(miss_opt)) {
    cli::cli_alert_info(
      "Optional / training items missing: {paste(miss_opt, collapse = ', ')}.")
  }

  invisible(tab)
}
