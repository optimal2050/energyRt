# =============================================================================
# process_designer() — launcher for the process designer Shiny app
# =============================================================================

# A run that dies without unwinding (a force-stop, or an error thrown by a
# shutdown callback) can leave shiny's "an app is running" state behind; every
# later launch then fails with "Can't call `runApp()` from within `runApp()`".
# If shiny claims to be running but there is no runApp() on the call stack —
# i.e. we are being called from the console, which would be impossible while
# an app truly runs — that state is provably stale and safe to clear.
#' @noRd
.reset_stale_shiny_state <- function() {
  if (!shiny::isRunning()) return(invisible(FALSE))
  on_stack <- any(vapply(sys.calls(), function(cl) {
    is.call(cl) && grepl("runApp", deparse(cl[[1]])[1], fixed = TRUE)
  }, logical(1)))
  if (on_stack) return(invisible(FALSE))
  ns <- asNamespace("shiny")
  if (exists("clearCurrentAppState", envir = ns, inherits = FALSE)) {
    get("clearCurrentAppState", envir = ns)()
    message("Cleared stale shiny app state left by an interrupted run.")
    return(invisible(TRUE))
  }
  invisible(FALSE)
}

#' Launch the process designer
#'
#' An interactive editor for [process spec][process_from_spec] YAML
#' specifications: ports and parameter blocks as forms, a live [draw()]
#' schematic, a validation report enforcing the spec policy (nothing
#' auto-invented; partial vintage coverage is reported), and YAML / R-code
#' export.
#'
#' The underlying spec layer reads, validates, draws and exports
#' `technology`, `storage` and `trade` specs alike (see
#' [process_from_spec()]). The editing **forms** are technology-shaped for
#' now: opening a storage or trade spec works, and every tab except the
#' parameter forms is fully functional, but its blocks are edited as YAML.
#' The remaining process classes (supply, demand, import, export) are
#' reserved.
#' Requires the suggested packages `shiny` and `DT`.
#'
#' `tech_designer()` is the deprecated former name.
#'
#' @param spec Optional process spec to open — a path to a `.yml` file or
#'   a spec list (see [read_procspec()]).
#' @param dir Optional directory of process-spec `.yml` files (e.g. specs
#'   exported from a workbook import) added to the app's template gallery
#'   alongside the built-in examples.
#' @param launch.browser Where the app opens. `NULL` (default) uses the
#'   standard shiny behaviour for your session — in RStudio that is the
#'   Shiny app window / viewer. `TRUE` opens the system web browser
#'   instead (no RStudio window chrome, e.g. its "Publish" button).
#'   `FALSE` starts the server without opening anything (the URL is
#'   printed to the console).
#' @param ... Passed to [shiny::runApp()] (e.g. `port`).
#' @return The value of [shiny::runApp()] (invisibly; blocks while the app
#'   runs).
#' @examples
#' \dontrun{
#' process_designer()
#' process_designer(system.file("techspec", "examples", "battery.yml",
#'                              package = "energyRt"))
#' process_designer(launch.browser = TRUE)  # system browser, no IDE chrome
#' }
#' @export
process_designer <- function(spec = NULL, dir = NULL, launch.browser = NULL,
                             ...) {
  for (p in c("shiny", "DT")) {
    if (!requireNamespace(p, quietly = TRUE)) {
      stop("process_designer() requires the '", p, "' package; install ",
           "it with install.packages(\"", p, "\")", call. = FALSE)
    }
  }
  app_dir <- system.file("designer", package = "energyRt")
  if (!nzchar(app_dir) || !file.exists(file.path(app_dir, "app.R"))) {
    stop("designer app not found; reinstall energyRt", call. = FALSE)
  }
  if (!is.null(spec)) {
    old <- options(energyRt.designer.spec = read_procspec(spec))
    on.exit(options(old), add = TRUE)
  }
  if (!is.null(dir)) {
    if (!dir.exists(dir)) {
      stop("`dir` does not exist: ", dir, call. = FALSE)
    }
    old_dir <- options(energyRt.designer.dir = normalizePath(dir))
    on.exit(options(old_dir), add = TRUE)
  }
  .reset_stale_shiny_state()
  if (is.null(launch.browser)) {
    shiny::runApp(app_dir, ...)
  } else {
    shiny::runApp(app_dir, launch.browser = launch.browser, ...)
  }
}

#' @rdname process_designer
#' @export
tech_designer <- function(spec = NULL, dir = NULL, launch.browser = NULL,
                          ...) {
  .Deprecated("process_designer")
  process_designer(spec = spec, dir = dir, launch.browser = launch.browser,
                   ...)
}
