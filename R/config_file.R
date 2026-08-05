# config_file.R ###############################################################
# Persisted configuration.
#
# The options registry (`R/options.R`) covers a single R session. This file adds
# the layer underneath it: a YAML file that survives restarts, so a user
# configures their solver paths once instead of on every session.
#
# Resolution order, highest priority first:
#
#   1. R option              options(en.gams_path = "...")
#   2. environment variable  ENERGYRT_GAMS_PATH
#   3. project config        ./.energyRt.yml
#   4. global config         ~/.energyRt/config.yml
#   5. package default       see ?energyRt-options
#
# Levels 1-2 and 5 are handled by the `options` package itself. Levels 3-4 are
# applied by `.en_apply_config()` from `.onLoad()`, which only fills options the
# user has not already set at levels 1-2 -- so the ordering above holds.

#' @include zzz.R
NULL

#' The energyRt configuration file
#'
#' @description
#' Solver paths and other options can be persisted to a YAML file so they are
#' restored in every session: `~/.energyRt/config.yml` for the user, or
#' `./.energyRt.yml` for a single project. energyRt applies them when the
#' package loads, without overriding anything the user has already set through
#' an R option or an environment variable.
#'
#' Use [en_config_show()] to see which options are in effect and where each
#' value came from.
#'
#' @param global logical. `TRUE` for the user-wide config
#'   (`~/.energyRt/config.yml`), `FALSE` for the project-local one
#'   (`./.energyRt.yml`).
#'
#' @return character, the path. The file need not exist.
#'
#' @family options
#' @rdname en_config
#' @export
#' @examples
#' en_config_path()
en_config_path <- function(global = TRUE) {
  if (global) {
    file.path(path.expand("~"), ".energyRt", "config.yml")
  } else {
    file.path(getwd(), ".energyRt.yml")
  }
}

# Read one YAML file into a named list, or an empty list if it is missing,
# empty, or unreadable. Never throws: a broken config must not stop the package
# from loading.
.en_config_read_file <- function(path) {
  if (is.null(path) || !file.exists(path)) {
    return(list())
  }
  if (!requireNamespace("yaml", quietly = TRUE)) {
    return(list())
  }
  cfg <- tryCatch(yaml::read_yaml(path), error = function(e) {
    cli::cli_warn("Could not read energyRt config {.path {path}}: {e$message}")
    NULL
  })
  if (is.null(cfg) || !is.list(cfg) || is.null(names(cfg))) {
    return(list())
  }
  cfg
}

#' @description
#' `en_config_read()` reads the project config (`./.energyRt.yml`) merged over
#' the global one (`~/.energyRt/config.yml`). It reports what is *on disk*; use
#' [en_config_show()] for what is actually in effect.
#'
#' @param global logical or `NULL`. `NULL` (default) merges both files, `TRUE`
#'   reads only the global one, `FALSE` only the project one.
#'
#' @return a named list of option values, possibly empty.
#'
#' @family options
#' @rdname en_config
#' @export
en_config_read <- function(global = NULL) {
  if (isTRUE(global)) {
    return(.en_config_read_file(en_config_path(global = TRUE)))
  }
  if (isFALSE(global)) {
    return(.en_config_read_file(en_config_path(global = FALSE)))
  }
  cfg <- .en_config_read_file(en_config_path(global = TRUE))
  prj <- .en_config_read_file(en_config_path(global = FALSE))
  utils::modifyList(cfg, prj)
}

#' @description
#' `en_config_write()` persists options so they are restored in later sessions.
#' Called with no arguments it writes every option whose current value differs
#' from the package default -- so the usual workflow is to set paths
#' interactively, check them with [en_config_show()], then call
#' `en_config_write()` once.
#'
#' @param config named list of option values, using names *without* the `en.`
#'   prefix (e.g. `list(gams_path = "C:/GAMS/48")`). `NULL` (default) collects
#'   the currently non-default options.
#' @param global logical. `TRUE` writes `~/.energyRt/config.yml`, `FALSE` writes
#'   `./.energyRt.yml`.
#'
#' @return the written config, invisibly.
#'
#' @family options
#' @rdname en_config
#' @export
#' @examples
#' \dontrun{
#' set_solver_path("gams", "C:/GAMS/win64/48.1")
#' en_config_write()
#' }
en_config_write <- function(config = NULL, global = TRUE) {
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("Writing the energyRt config file requires the 'yaml' package.",
         call. = FALSE)
  }
  if (is.null(config)) {
    config <- .en_nondefault_options()
  }
  if (!is.list(config) || (length(config) && is.null(names(config)))) {
    stop("`config` must be a named list.", call. = FALSE)
  }
  unknown <- setdiff(names(config), .en_option_names())
  if (length(unknown)) {
    cli::cli_warn(c(
      "Unknown energyRt option{?s} in the config: {.val {unknown}}.",
      i = "See {.code ?energyRt-options} for the available names."
    ))
  }

  path <- en_config_path(global = global)
  dir <- dirname(path)
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }
  yaml::write_yaml(config, path)
  cli::cli_alert_success("Wrote {length(config)} option{?s} to {.path {path}}.")
  invisible(config)
}

# Names of every option in the registry.
#
# NOT `names(options::opts())`: that builds a list by assigning each value, and
# assigning NULL to a list element drops it -- so every option whose value is
# NULL (all six toolchain paths, plus `neos_email`) would silently disappear.
# Reading the registry environment directly is exact.
.en_option_names <- function() {
  ls(get(".options", envir = topenv(environment()), inherits = FALSE),
     all.names = TRUE)
}

# Options whose value the user (or a config file) has changed from the package
# default. `opt_source()` reports "default" only while nothing has touched it,
# which is exactly the test we want -- and it avoids reaching into the `options`
# package's internals to recover the declared default.
.en_nondefault_options <- function() {
  nms <- .en_option_names()
  keep <- vapply(nms, function(n) {
    !identical(.en_opt_source(n), "default")
  }, logical(1))
  res <- lapply(nms[keep], function(n) options::opt(n, env = "energyRt"))
  names(res) <- nms[keep]
  res
}

# Apply the config files to options the user has not already set through an R
# option or an environment variable. The project file is applied first so it
# wins over the global one: once an option has been set its source is no longer
# "default", and the global pass skips it.
.en_apply_config <- function() {
  applied <- list()
  known <- .en_option_names()
  for (global in c(FALSE, TRUE)) {
    path <- en_config_path(global = global)
    cfg <- .en_config_read_file(path)
    if (!length(cfg)) next
    for (nm in intersect(names(cfg), known)) {
      if (!identical(.en_opt_source(nm), "default")) {
        next
      }
      options::opt_set(nm, cfg[[nm]], env = "energyRt")
      applied[[nm]] <- path
    }
  }
  .en_state$config_applied <- applied
  invisible(applied)
}

# Render an option value as one short line for `en_config_show()`.
.en_format_value <- function(x) {
  if (is.null(x)) {
    return("NULL")
  }
  if (is.list(x)) {
    inner <- vapply(seq_along(x), function(i) {
      paste0(names(x)[i], "=", .en_format_value(x[[i]]))
    }, character(1))
    return(paste0("list(", paste(inner, collapse = ", "), ")"))
  }
  if (is.character(x)) {
    x <- paste0('"', x, '"')
  }
  out <- paste(format(x, trim = TRUE), collapse = ", ")
  if (nchar(out) > 48) out <- paste0(substr(out, 1, 45), "...")
  out
}

#' Show the effective energyRt configuration
#'
#' @description
#' Prints every registered option with its current value and where that value
#' came from -- an R option, an environment variable, a config file, or the
#' package default. This is the first thing to run when a solver is not found.
#'
#' @return a data frame with columns `option`, `value`, `source` and `origin`,
#'   invisibly. `origin` names the environment variable or config file the value
#'   came from, where applicable.
#'
#' @family options
#' @seealso [en_config_write()] to persist the current settings.
#' @rdname en_config_show
#' @export
#' @examples
#' en_config_show()
en_config_show <- function() {
  nms <- sort(.en_option_names())
  applied <- .en_state$config_applied

  rows <- lapply(nms, function(n) {
    src <- .en_opt_source(n)
    origin <- NA_character_
    # `.en_apply_config()` sets config values as R options, so `opt_source()`
    # reports "option" for them. Correct that here using what was recorded at
    # load time, unless the option has since been changed.
    if (identical(src, "option") && !is.null(applied[[n]])) {
      src <- "config"
      origin <- applied[[n]]
    } else if (identical(src, "envvar")) {
      origin <- toupper(paste0("ENERGYRT_", n))
      if (identical(n, "neos_email")) origin <- "NEOS_EMAIL"
    } else if (identical(src, "option")) {
      origin <- paste0("en.", n)
    }
    data.frame(
      option = n,
      value = .en_format_value(options::opt(n, env = "energyRt")),
      source = src,
      origin = origin,
      stringsAsFactors = FALSE
    )
  })
  res <- do.call(rbind, rows)

  cli::cli_h1("energyRt configuration")
  width <- max(nchar(res$option))
  for (i in seq_len(nrow(res))) {
    tag <- switch(res$source[i],
      option = "option",
      envvar = "envvar",
      config = "config",
      "default"
    )
    cli::cli_verbatim(paste0(
      "  ", formatC(res$option[i], width = width, flag = "-"),
      "  ", formatC(res$value[i], width = 26, flag = "-"),
      "  [", tag, "]",
      if (!is.na(res$origin[i]) && !identical(tag, "option")) {
        paste0(" ", res$origin[i])
      } else {
        ""
      }
    ))
  }
  cli::cli_text("")
  cli::cli_alert_info(
    "Priority: R option > env var > {.path ./.energyRt.yml} > {.path ~/.energyRt/config.yml} > default"
  )
  invisible(res)
}
