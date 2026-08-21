# mosox: a runner, NOT a solver backend ######################################
#
# mosox is a standalone GMPL matrix generator with HiGHS attached (`compile` ->
# MPS, `solve` -> HiGHS, plus `normalize`/`compare` for diffing two matrices).
# energyRt shells out to it through `en_mosox_run()` and nothing more: it is
# deliberately absent from `solver_options`, so `solve_scen()` cannot reach it.
#
# Assessed against mosox 0.6.2 (2026-08). Four defects, in the order they bite;
# each is pinned with a minimal reproducer in tests/testthat/test-mosox.R and
# flips green on a fixed mosox.
#
# 1. SILENTLY WRONG MATRIX -- the blocker. mosox ignores set-membership
#    conditions, `sum{j in J: (i,j) in M}(...)`, and sums over the whole of `J`.
#    Numeric conditions (`k[j] > 1`) ARE honoured, so nothing errors or warns.
#    energyRt gates almost every sum with a membership map, so the generated LP
#    is wrong wherever a gating map is not the full cross-product: on the
#    one-technology unit model mosox builds 6357 rows (right) but 19681 columns
#    and 106520 nonzeros (glpsol: 5987 / 13401) and reports objective 0 instead
#    of 4510. Nothing else on this list matters until this is fixed.
#
# 2. NO RESULT READ-BACK. energyRt reads GLPK results from the post-solve
#    `printf`/`for` block, 234 statements writing output/v*.csv. mosox parses
#    that block and never executes it -- no files appear, no error is raised.
#    Its own `-f csv` output is the only route back, and see 4.
#
# 3. MATHPROG SUBSET. `glpk/energyRt.mod` does not parse: no `prod` aggregate,
#    no implicit dummy index (`sum{FORIF: ...}`), no scalar set membership
#    (`t1 in mTradeOlifeInf`), no leading unary plus, no constant-minus-variable
#    (`pTechStock[...] - sum{...}(vFoo)`), and the data writer's trailing comma
#    after the last map tuple is rejected. All six are worked around by
#    data-raw/build-mosox-template.R, which derives glpk/energyRt-mosox.mod from
#    the GLPK template; glpsol runs that variant to the same objective on both
#    a plain (4510) and a weather (135773.813654) model, so the rewrites are
#    sound -- they just cannot rescue defect 1.
#
# 4. SHIFTED VARIABLE VALUES. `solve`'s txt and csv output report a spurious
#    leading 1.000 and then each variable one slot late, dropping the last.
#    Objective and constraint duals are correct.
#
# Also worth knowing when wiring anything: `compare` prints its differences but
# exits 0, so a mismatch is invisible in the exit status.
#
# The weather rewrite in 3 replaces `prod(B)` with `1 + sum(B - 1)`, which is
# exact only for AT MOST ONE weather factor per object. Whoever registers mosox
# as a backend must refuse a model with several -- every other backend keeps the
# real product (see NEWS.md, "prod() over several weather factors").
##############################################################################

#' Set the path to the mosox executable
#'
#' @param path character. Path to a directory containing the mosox executable.
#'
#' @return NULL, invisibly.
#' @family solver
#' @export
set_mosox_path <- function(path = NULL) {
  set_solver_path("mosox", path)
  invisible(NULL)
}

#' Get the configured mosox executable path
#'
#' @return Configured mosox path, if any.
#' @family solver
#' @export
get_mosox_path <- function() {
  get_solver_path("mosox")
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
