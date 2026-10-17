#' Find and replace special characters
#'
#' @include plot.R
#'
#' @param x character vector
#' @param pattern regular expression pattern to match special characters
#' @param repl replacement character
#'
#' @returns character vector with special characters replaced
#'
#' @examples
#' energyRt:::en_replace_specials(c("valid", "invalid!", "in-valid", "valid_1", "invalid.2"))
#' energyRt:::en_replace_specials(c("valid", "invalid!"), "[\\.\\^\\$\\*\\+\\?\\!]", "_fixed")
#' @keywords internal
en_replace_specials <- function(x, pattern = "[^[:alnum:]]", repl = "_") {
  # x - character
  gsub(pattern, repl, x)
}

#' @describeIn en_replace_specials Return `TRUE` if any element contains a special character.
#' @keywords internal
en_has_specials <- function(x, pattern = "[^[:alnum:]]") {
  # x - character
  if (!is.character(x)) stop("x must be a character vector")
  any(grepl(pattern, x))
}

#' @describeIn en_replace_specials Return the match positions of special characters (via [gregexpr()]).
#' @keywords internal
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
#' `en_check_solve()` is the one exception to "read-only": it SOLVES a tiny
#' model (objective 8) on each configured backend, which is the only way to tell
#' a backend that is installed from one that can produce a number. A backend
#' that is not configured is not probed at all.
#'
#' @return A one-row (`en_check_*`) or multi-row (`en_check_dependencies`) tibble,
#'   returned invisibly for `en_check_dependencies()` which also prints a table.
#' @family solver
#' @name en_check
#' @examples
#' \dontrun{
#' en_check_dependencies()
#' en_check_julia()
#' en_check_solve()                      # solves on every configured backend
#' en_check_solve(backends = "gams")     # proprietary: ask for it by name
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
    note = if (!pr$found) "install via miniforge/conda; then multimod::mm_setup_python()" else NA_character_)
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
  # pyarrow reads the model data in the default exchange; without it a solve
  # fails inside the generated script, where the message is easy to miss.
  pa <- if (ok) {
    out <- tryCatch(
      suppressWarnings(system2(py,
        c("-c", shQuote("import pyarrow; print(pyarrow.__version__)")),
        stdout = TRUE, stderr = TRUE)),
      error = function(e) character(0)
    )
    any(grepl("^[0-9]+\\.[0-9]+", out))
  } else {
    FALSE
  }
  .en_status_row("Pyomo", required = FALSE,
    list(found = ok, path = if (ok) py else NA_character_,
         version = if (ok) ver[length(ver)] else NA_character_),
    note = if (!ok) "multimod::mm_setup_python()" else
      if (!pa) "Pyomo ok; pyarrow MISSING -> multimod::mm_setup_python()" else
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
en_check_julia_pkgs <- function(pkgs = c("JuMP", "HiGHS", "Arrow")) {
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
    note = if (ok) paste0(paste(pkgs, collapse = "/"), " present") else
      paste0("missing: ", paste(missing, collapse = ", "), " -> en_install_julia_pkgs()"))
}

# --------------------------------------------------------------------------- #
# Solve probe: the only check that proves a backend can produce a number.
#
# TWO LEVELS, because a backend can be installed, run, and still be unable to
# carry a real model -- a size-limited licence (GAMS demo, CPLEX community) is
# the common case, and it fails nowhere near the entry point.
#
#   tiny  the unit kit U1 (`topia$modules$unit$U1`): every input is 1 on the
#         symmetric `unit_s4` calendar, one region, one year, `discount = 0`, so
#         the objective is the hand-computed integer `.EN_PROBE_OBJECTIVE` --
#         arithmetic in `data-raw/topia_modules.R`, pinned by
#         `tests/testthat/test-unit-model.R`. A constant to assert means no
#         golden file and no per-backend tolerance. A dozen rows: inside any
#         demo cap, so this level proves the chain (writer, data exchange,
#         solver, reader) and nothing about capacity.
#   full  the R1 electricity kit on `s4_h24`, the calendar its data is built
#         for: the size the suite's own fixtures run at, and above the
#         classic 300x300 demo cap. Its objective is not hand-computable, so the
#         level asserts agreement with GLPK on the same model -- the same parity
#         the cross tier checks, at probe scale.
#
# Only `gams` carries the `full` level by default; the other presets use
# unlimited solvers (CBC, HiGHS, GLPK).
# --------------------------------------------------------------------------- #

.EN_PROBE_OBJECTIVE <- 8

# backend -> (solver preset, toolchain gate, levels). `mm` needs no external
# binary; `dense` marks a backend whose writer refuses a sparse scenario.
.en_probe_backends <- function() {
  list(
    glpk  = list(preset = "glpk",           gate = function() en_check_glpk()$found),
    julia = list(preset = "julia_highs",    gate = function() en_check_julia()$found),
    pyomo = list(preset = "pyomo_cbc",      gate = function() en_check_pyomo()$found),
    mm    = list(preset = "mm_highs",
                 gate = function() requireNamespace("multimod", quietly = TRUE)),
    gams  = list(preset = "gams_gdx_cplex", dense = TRUE,
                 levels = c("tiny", "full"),
                 gate = function() en_check_gams()$found)
  )
}

# The probe models, built from package data only (no test fixtures). The `tiny`
# one mirrors `un_build()` in tests/testthat/helper-topia.R -- keep them in step.
.en_probe_model <- function(level = c("tiny", "full")) {
  level <- match.arg(level)
  if (level == "tiny") {
    kit <- topia$modules$unit$U1
    return(newModel("EN_PROBE", data = kit$repo,
                    calendar = topia$modules$unit$calendars[[kit$calendar]],
                    region   = kit$regions,
                    horizon  = topia$modules$horizons$unit, discount = 0))
  }
  kit <- topia$modules$electricity$R1
  newModel("EN_PROBE_FULL", data = kit$repo,
           calendar = topia$modules$calendars$s4_h24,
           region   = kit$regions,
           horizon  = topia$modules$horizons$unit, discount = 0)
}

#' @rdname en_check
#' @param backends character vector naming the backends to probe: any of
#'   `"glpk"`, `"julia"`, `"pyomo"`, `"mm"`, `"gams"`. `NULL` (default) probes
#'   every one whose toolchain is configured EXCEPT `"gams"`, which is
#'   proprietary and must be asked for by name.
#' @param timeout seconds allowed per backend before the probe gives up.
#' @export
en_check_solve <- function(backends = NULL, timeout = 180) {
  defs <- .en_probe_backends()
  if (is.null(backends)) backends <- setdiff(names(defs), "gams")
  backends <- intersect(backends, names(defs))
  if (length(backends) == 0) {
    return(.en_status_row("solve", required = FALSE,
      list(found = FALSE, path = NA_character_, version = NA_character_),
      note = "no backend named"))
  }

  # A backend that is not configured is NOT probed: no solver starts and the row
  # is informational, not a failure of this machine.
  gated <- vapply(backends, function(b) {
    isTRUE(tryCatch(defs[[b]]$gate(), error = function(e) FALSE))
  }, logical(1))

  # One interpolation per (level, sparsity), built on first use: the GAMS writer
  # is the only reader of `status$sparse` and refuses a sparse scenario, so
  # probing it with the sparse one would report the PROBE's limitation as a
  # broken backend.
  cache <- new.env(parent = emptyenv())
  probe_scen <- function(level, dense) {
    key <- paste0(level, if (dense) "/dense" else "/sparse")
    if (is.null(cache[[key]])) {
      cache[[key]] <- tryCatch(
        suppressMessages(suppressWarnings(interpolate_model(
          .en_probe_model(level), name = paste0("en_probe_", level),
          ondisk = FALSE, sparse = !dense))),
        error = function(e) e)
    }
    cache[[key]]
  }

  # The `full` level has no hand-computable objective, so it is checked against
  # GLPK on the same model -- computed once, only if some backend needs it.
  full_ref <- function(dense) {
    key <- paste0("ref", if (dense) "/dense" else "/sparse")
    if (is.null(cache[[key]])) {
      sc <- probe_scen("full", dense)
      cache[[key]] <- if (inherits(sc, "error") ||
                          !isTRUE(tryCatch(en_check_glpk()$found,
                                           error = function(e) FALSE))) {
        NA_real_
      } else {
        r <- .en_probe_one(sc, "glpk", "glpk", timeout, expected = NA_real_)
        if (isTRUE(r$found)) as.numeric(sub("^obj ", "", r$version)) else NA_real_
      }
    }
    cache[[key]]
  }

  rows <- list()
  for (b in backends) {
    preset <- defs[[b]]$preset
    dense <- isTRUE(defs[[b]]$dense)
    levels <- defs[[b]]$levels %||% "tiny"
    if (!gated[[b]]) {
      rows[[length(rows) + 1L]] <- .en_probe_row(b, "tiny", preset,
        found = FALSE, version = NA_character_, note = "No backend")
      next
    }
    for (lv in levels) {
      scen <- probe_scen(lv, dense)
      if (inherits(scen, "error")) {
        rows[[length(rows) + 1L]] <- .en_probe_row(b, lv, preset,
          found = FALSE, version = NA_character_,
          note = paste0("probe model failed to interpolate: ",
                        .en_first_line(conditionMessage(scen))))
        next
      }
      expected <- if (lv == "tiny") .EN_PROBE_OBJECTIVE else full_ref(dense)
      r <- .en_probe_one(scen, b, preset, timeout, expected = expected,
                         level = lv)
      rows[[length(rows) + 1L]] <- r
      # a backend that cannot solve the tiny model will not solve the big one,
      # and its error is already reported
      if (lv == "tiny" && !isTRUE(r$found)) break
    }
  }
  dplyr::bind_rows(rows)
}

# One row of the probe table, carrying `backend` and `level` so the caller can
# group without parsing `component`.
.en_probe_row <- function(backend, level, preset, found, version, note) {
  row <- .en_status_row(
    paste0("solve: ", backend, if (!is.null(level)) paste0(" (", level, ")")),
    required = FALSE,
    list(found = found, path = preset, version = version), note = note)
  row$backend <- backend
  row$level <- level
  row
}

# Solve the probe scenario on one backend. Never throws: a backend that cannot
# solve is a reported row, not an error in the caller.
.en_probe_one <- function(scen, backend, preset, timeout,
                          expected = .EN_PROBE_OBJECTIVE, level = "tiny") {
  wd <- getwd()
  dir <- tempfile(paste0("en_probe_", backend, "_"))
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  on.exit({
    setwd(wd)
    unlink(dir, recursive = TRUE, force = TRUE)
  }, add = TRUE)
  setwd(dir)                       # solves land here, never in the user's project
  t0 <- Sys.time()
  # `echo = FALSE` silences the solver's own console output; the writers print
  # a little on top of that, so swallow both streams and keep the table clean
  out <- NULL
  invisible(utils::capture.output(type = "message", suppressMessages(
    invisible(utils::capture.output(type = "output", {
      out <- tryCatch(
        withCallingHandlers(
          suppressMessages(suppressWarnings(
            solve_scenario(scen, name = paste0("en_probe_", backend),
                           solver = solver_options[[preset]],
                           solver.dir = file.path(dir, "run"),
                           transient = TRUE, wait = TRUE, echo = FALSE))),
          warning = function(w) invokeRestart("muffleWarning")),
        error = function(e) e)
    })))))
  secs <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  if (inherits(out, "error")) {
    return(.en_probe_row(backend, level, preset, found = FALSE,
      version = NA_character_, note = .en_first_line(conditionMessage(out))))
  }
  obj <- tryCatch({
    d <- getData(out, "vObjective", merge = TRUE)
    if (is.null(d) || nrow(d) == 0) NA_real_ else as.numeric(d$value[1])
  }, error = function(e) NA_real_)
  # `expected = NA` means "any finite objective" -- used when computing the
  # reference the other backends are then compared against
  tol <- if (identical(level, "tiny")) 1e-9 else 1e-6 * max(1, abs(expected))
  ok <- if (is.na(obj)) FALSE else if (is.na(expected)) TRUE else
    abs(obj - expected) < tol
  .en_probe_row(backend, level, preset, found = ok,
    version = if (is.na(obj)) NA_character_ else paste0("obj ", format(obj)),
    note = if (ok) sprintf("solved in %.1fs", secs) else
      if (is.na(obj)) "solved but returned no objective" else
      paste0("solved to ", format(obj), ", expected ", format(expected)))
}

# First non-empty line of a message, for a one-line status note.
.en_first_line <- function(msg) {
  ls <- trimws(strsplit(paste(msg, collapse = "
"), "
")[[1]])
  ls <- ls[nzchar(ls)]
  if (length(ls) == 0) "failed" else substr(ls[1], 1, 160)
}


#' @rdname en_check
#' @param solver_pkgs verify Julia solver packages too (slower; starts Julia).
#' @param solve also run [en_check_solve()], which SOLVES a tiny model on each
#'   configured backend. `FALSE` by default: the plain dependency check starts no
#'   solver.
#' @export
en_check_dependencies <- function(solver_pkgs = FALSE, solve = FALSE) {
  rows <- list(
    en_check_glpk(),
    en_check_julia(),
    en_check_python(),
    en_check_pyomo(),
    en_check_gams(),
    en_check_gdx()
  )
  if (solver_pkgs) rows <- c(rows, list(en_check_julia_pkgs()))
  if (isTRUE(solve)) rows <- c(rows, list(en_check_solve()))
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
      "Python - https://conda-forge.org/ (miniforge) then multimod::mm_setup_python()",
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
#' @param extras character vector of optional R packages to check. `NULL`
#'   (default) reads them off the package's own `Suggests`, so the list cannot
#'   drift from DESCRIPTION; dev-only tooling is left out. `gm` is appended
#'   because the (parked) score export uses it and it is not a dependency.
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
en_check_packages <- function(extras = NULL, external = TRUE) {

  if (is.null(extras)) {
    sug <- tryCatch(
      trimws(gsub("[(][^)]*[)]", "", strsplit(
        utils::packageDescription("energyRt", fields = "Suggests"), ",")[[1]])),
      error = function(e) character(0))
    sug <- sug[nzchar(sug)]
    # Dev-only tooling is not something a user needs to be told is missing.
    extras <- c(setdiff(sug, c("pak", "testthat", "devtools", "withr")), "gm")
  }

  # energyRt's own CRAN imports (from DESCRIPTION), minus base/recommended pkgs.
  imports <- tryCatch(
    strsplit(utils::packageDescription("energyRt", fields = "Imports"), ",")[[1]],
    error = function(e) character(0))
  imports <- trimws(gsub("\\(.*?\\)", "", imports))     # drop version constraints
  imports <- imports[nzchar(imports)]
  base_pkgs <- rownames(utils::installed.packages(priority = "base"))
  imports <- setdiff(imports, base_pkgs)

  # The non-CRAN siblings need their GitHub ref in the hint; a bare
  # `pak::pkg_install('glpsol')` would send the user to CRAN, where none of
  # them is published.
  sib_ref <- stats::setNames(.en_sibling_refs(), .en_sibling_pkgs())

  mk_rows <- function(pkgs, required) {
    lapply(pkgs, function(p) {
      pr <- .en_pkg_probe(p)
      src <- if (p %in% names(sib_ref)) sib_ref[[p]] else p
      .en_status_row(p, required = required, pr,
        note = if (!pr$found) paste0("install: pak::pkg_install('", src, "')") else NA_character_)
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
