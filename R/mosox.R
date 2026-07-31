#' Set the path to the mosox executable
#'
#' @param path character. Path to a directory containing the mosox executable.
#'
#' @return NULL, invisibly.
#' @family solver
#' @export
set_mosox_path <- function(path = NULL) {
  if (!is.null(path) && nzchar(path)) {
    if (!dir.exists(path)) {
      stop("The mosox path '", path, "' does not exist.", call. = FALSE)
    }
    if (!grepl("/$", path)) {
      path <- paste0(path, "/")
    }
  }
  options::opt_set("mosox_path", path, env = "energyRt")
  invisible(NULL)
}

#' Get the configured mosox executable path
#'
#' @return Configured mosox path, if any.
#' @family solver
#' @export
get_mosox_path <- function() {
  res <- tryCatch(options::opt("mosox_path", env = "energyRt"),
                  error = function(e) NULL)
  if (is.null(res) || identical(res, "")) {
    NULL
  } else {
    res
  }
}

# Resolve the mosox executable, honouring the configured path first.
.find_mosox <- function() {
  mp <- get_mosox_path()
  if (!is.null(mp) && nzchar(mp)) {
    cand <- c(file.path(mp, "mosox"), file.path(mp, "mosox.exe"))
    cand <- cand[file.exists(cand)]
    if (length(cand)) {
      return(normalizePath(cand[1], winslash = "/"))
    }
  }

  found <- Sys.which("mosox")
  if (nzchar(found)) {
    return(unname(found))
  }

  if (.Platform$OS.type == "windows") {
    cand <- Sys.glob(c(
      "C:/Program Files/mosox/bin/mosox.exe",
      "C:/Program Files (x86)/mosox/bin/mosox.exe"
    ))
    cand <- cand[file.exists(cand)]
    if (length(cand)) {
      return(normalizePath(cand[1], winslash = "/"))
    }
  }

  "mosox"
}

#' Run mosox on a GMPL model or data file
#'
#' @param model character. Path to the GMPL model file.
#' @param data character. Optional path to the GMPL data file.
#' @param output character. Optional output path for compile or solve output.
#' @param command character. Either "compile" or "solve".
#' @param extra_args character. Additional CLI arguments to pass to mosox.
#' @param check logical. If TRUE, stop on a non-zero exit status.
#'
#' @return A list with the execution result.
#' @family solver
#' @export
en_mosox_run <- function(model, data = NULL, output = NULL,
                         command = c("compile", "solve"),
                         extra_args = character(), check = TRUE) {
  command <- match.arg(command)

  if (missing(model) || !nzchar(model)) {
    stop("`model` must be a non-empty path.", call. = FALSE)
  }

  model_path <- normalizePath(model, winslash = "/", mustWork = FALSE)
  data_path <- if (!is.null(data) && nzchar(data)) {
    normalizePath(data, winslash = "/", mustWork = FALSE)
  } else {
    NULL
  }

  if (is.null(output) || !nzchar(output)) {
    output <- tempfile(fileext = if (command == "compile") ".mps" else ".txt")
  }

  outdir <- dirname(output)
  if (!dir.exists(outdir)) {
    dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  }

  exe <- .find_mosox()
  args <- c(command, model_path)

  if (!is.null(data_path) && nzchar(data_path)) {
    args <- c(args, data_path)
  }

  if (!is.null(output) && nzchar(output)) {
    args <- c(args, "-o", output)
  }

  if (length(extra_args)) {
    args <- c(args, extra_args)
  }

  res <- tryCatch(
    system2(exe, args, stdout = TRUE, stderr = TRUE),
    error = function(e) {
      list(status = 127L, stdout = character(), stderr = e$message)
    }
  )

  if (is.list(res)) {
    status <- res$status
    stdout <- res$stdout
    stderr <- res$stderr
  } else {
    status <- attr(res, "status")
    stdout <- res
    stderr <- character(0)
  }
  status <- as.integer(status %||% 0L)

  if (check && !identical(status, 0L)) {
    if (identical(status, 127L) && !file.exists(exe)) {
      stop("mosox executable was not found. Install mosox or set_mosox_path().",
           call. = FALSE)
    }
    stop("mosox failed with exit status ", status, call. = FALSE)
  }

  list(
    success = identical(status, 0L),
    command = command,
    exe = exe,
    args = args,
    output = output,
    stdout = stdout,
    stderr = stderr
  )
}

#' Run mosox on a GMPL model or data file
#'
#' @param ... Arguments passed to [en_mosox_run()].
#'
#' @return A list with the execution result.
#' @family solver
#' @export
mosox_run <- function(...) {
  en_mosox_run(...)
}
