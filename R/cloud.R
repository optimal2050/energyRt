# Cloud solve backend: build here, solve on a rented machine ##################
#
# The transport -- credentials, upload, job submission, polling, fetching --
# lives in the `mmcloud` package, which knows nothing about scenarios and is
# shared with multimod. What stays here is the part that is specific to
# energyRt: producing the matrix, and turning the returned solution into the
# `output/` directory that `read_solution()` already reads.
#
# Modelled on the NEOS backend (`R/neos.R` plus `.neos_call_solver()` in
# `R/solve.R`), whose good idea was to have no result parser of its own: it
# lands files where a local solve would have left them and lets the ordinary
# reader take over.

# Ensure the optional deps are present, with an actionable message.
.cloud_check_deps <- function() {
  if (!requireNamespace("mmcloud", quietly = TRUE)) {
    stop("Cloud solving needs the 'mmcloud' package.\n",
         '  pak::pak("optimal2050/mmcloud")\n',
         "It carries the transport (job submission, upload, polling) and is ",
         "shared with multimod.", call. = FALSE)
  }
}

#' Hugging Face access token
#'
#' Re-exported from \pkg{mmcloud} so cloud solving can be configured without
#' loading that package explicitly. See [mmcloud::get_hf_token()] for the
#' resolution order.
#'
#' @param token Access token with write scope, or `NULL` to clear it.
#' @return `get_hf_token()` returns the token or `NULL`; `set_hf_token()`
#'   returns it invisibly.
#' @examples
#' \dontrun{
#' set_hf_token("hf_xxx")
#' }
#' @rdname hf_token
#' @export
get_hf_token <- function() {
  .cloud_check_deps()
  mmcloud::get_hf_token()
}

#' @rdname hf_token
#' @export
set_hf_token <- function(token = NULL) {
  .cloud_check_deps()
  mmcloud::set_hf_token(token)
}

# Solve a locally written matrix in the cloud and stage the results.
#
# Returns 0 on success, non-zero otherwise -- the same contract as
# `.neos_call_solver()`, which `.call_solver()` turns into a `stop()`.
.cloud_call_solver <- function(arg, scen) {
  .cloud_check_deps()
  s <- scen@settings@solver
  .cloud_reject_legacy_fields(s)

  # Who wrote the matrix decides how the returned solution is read back: Pyomo
  # emits symbolic row and column names, multimod emits positional ones that
  # only the index files resolve. The index files stay local -- the GPU is sent
  # the matrix alone.
  writer <- if (identical(toupper(s$lang), "PYOMO")) {
    "pyomo"
  } else if (any(grep("^(mm|multimod)$", s$lang, ignore.case = TRUE))) {
    "mm"
  } else {
    stop("backend = 'mmcloud' ships a matrix written by Pyomo (lang = ",
         "'PYOMO') or by multimod (lang = 'mm'). Got lang = ", s$lang, ".",
         call. = FALSE)
  }

  mps <- if (identical(writer, "mm")) .mm_cloud_matrix(arg) else {
    .cloud_matrix(arg, scen)
  }
  gz <- .cloud_compress(mps, arg)

  if (arg$echo) {
    cat(sprintf("Uploading %.2f GB ...\n", file.size(gz) / 1e9))
  }
  job <- mmcloud::cloud_run(
    inputs    = gz,
    outputs   = "model.sol",
    script    = .cloud_worker_script(s),
    flavor    = s$cloud_flavor %||% "a10g-large",
    timeout   = s$cloud_timeout %||% "3h",
    namespace = s$cloud_namespace,
    image     = s$cloud_image %||% "python:3.12",
    name      = paste0("energyrt-", scen@name),
    echo      = isTRUE(arg$echo))

  if (arg$echo) cat("  job ", job$id, " (", job$flavor, ")\n", sep = "")
  # Keep the handle on the scenario's run record: it outlives this call, so a
  # killed R session can still poll or cancel the job.
  .cloud_note_job(arg, job)

  stage <- mmcloud::cloud_wait(job, poll = s$cloud_poll %||% 15,
                               max_wait = s$cloud_max_wait %||% Inf,
                               echo = isTRUE(arg$echo))

  ok <- tryCatch({
    mmcloud::cloud_fetch(job, arg$solver.dir, echo = isTRUE(arg$echo))
    TRUE
  }, error = function(e) {
    # A job can finish cleanly and still return nothing -- an infeasible or
    # unbounded model, a time limit, a cancellation. Report the solver's own
    # conclusion rather than blaming the transport for a missing file.
    v <- tryCatch(mmcloud::cloud_verdict(job), error = function(e2) NULL)
    if (!is.null(v) && !identical(v$verdict, "ok")) {
      warning("mmcloud: ", v$message, " (job ", job$id, ").",
              if (!is.na(v$evidence)) paste0("\n  ", v$evidence) else "",
              call. = FALSE)
    } else {
      warning("mmcloud: no solution returned (job ", job$id,
              " ended ", stage, "): ", conditionMessage(e), call. = FALSE)
    }
    FALSE
  })
  if (!ok || !identical(stage, "COMPLETED")) return(1L)

  if (identical(writer, "mm")) {
    # cuOpt returns primals only: the duals in the decoded solution are NA.
    .mm_decode_solution(arg$solver.dir, fp(arg$solver.dir, "model.sol"),
                        solver = "cuopt", echo = isTRUE(arg$echo))
  } else {
    .cloud_decode_solution(arg$solver.dir, echo = isTRUE(arg$echo))
  }
  0L
}

# The `hf_*` option names are gone: transport settings are `cloud_*` and solver
# settings are `gpu_*`, because `hf_method` and friends were never Hugging Face
# settings at all -- they are cuOpt flags. An unrecognised field on a solver
# spec is SILENTLY IGNORED, and the defaults it falls back to are not benign:
# an unset namespace resolves to the token's personal account (HTTP 402), an
# unset time limit lets cuopt_cli stop after its own 60 s, and the default
# flavor is 24 GB of VRAM against the ~63 GB a full-year case needs. So a stale
# name is refused rather than dropped.
.cloud_legacy_fields <- c(
  hf_flavor = "cloud_flavor", hf_namespace = "cloud_namespace",
  hf_timeout = "cloud_timeout", hf_image = "cloud_image",
  hf_poll = "cloud_poll", hf_max_wait = "cloud_max_wait",
  hf_reuse_matrix = "cloud_reuse_matrix", hf_method = "gpu_method",
  hf_time_limit = "gpu_time_limit", hf_solver_options = "gpu_solver_options"
)

.cloud_reject_legacy_fields <- function(s) {
  found <- intersect(names(.cloud_legacy_fields), names(s))
  if (!length(found)) return(invisible())
  stop("solver spec carries renamed field(s):\n",
       paste0("  ", found, " -> ", .cloud_legacy_fields[found],
              collapse = "\n"),
       "\n`hf_*` became `cloud_*` for transport and `gpu_*` for solver ",
       "options. An unknown field would be ignored and the job would run at ",
       "the defaults, so it is refused instead.", call. = FALSE)
}

# The matrix for the mm route is already on disk: `.write_model_MM()` wrote it,
# together with the index files the solution is decoded against.
.mm_cloud_matrix <- function(arg) {
  mps <- fp(arg$solver.dir, "model.mps")
  if (!file.exists(mps)) {
    stop("mmcloud: no model.mps in ", arg$solver.dir,
         " - was the model written?", call. = FALSE)
  }
  mps
}

# The matrix, built here unless one is already on disk and `cloud_reuse_matrix`
# says to keep it. Re-solving with different solver options should not pay for
# the build again.
.cloud_matrix <- function(arg, scen) {
  s <- scen@settings@solver
  mps <- file.path(arg$solver.dir, "model.mps")

  if (isTRUE(s$cloud_reuse_matrix) && file.exists(mps)) {
    if (arg$echo) {
      cat(sprintf("Reusing the matrix already built (%.2f GB, %s)\n",
                  file.size(mps) / 1e9,
                  format(file.mtime(mps), "%Y-%m-%d %H:%M")))
    }
    return(mps)
  }

  unlink(mps)
  if (arg$echo) cat("Building the matrix locally ...\n")
  rs <- .cloud_build_matrix(arg, scen)
  if (!identical(as.integer(rs), 0L)) {
    stop("mmcloud: the local build failed with code ", rs, call. = FALSE)
  }
  if (!file.exists(mps)) {
    stop("mmcloud: the local build produced no 'model.mps' in ",
         arg$solver.dir, ". The preset must carry the inc4 hook that writes it ",
         "(see solver_options$pyomo_mps).", call. = FALSE)
  }
  mps
}

# Run the written model locally so its inc4 hook writes the matrix. Restores
# the working directory on every exit path.
.cloud_build_matrix <- function(arg, scen) {
  home <- getwd()
  on.exit(setwd(home), add = TRUE)
  setwd(arg$solver.dir)
  system(paste(scen@settings@solver$cmdline),
         wait = TRUE, ignore.stdout = !isTRUE(arg$echo))
}

# Compressing 10 GB takes minutes; keep the archive so a resubmit can reuse it,
# and skip the work when it is already newer than the matrix it was made from.
.cloud_compress <- function(mps, arg) {
  gz <- paste0(mps, ".gz")
  if (file.exists(gz) && file.mtime(gz) >= file.mtime(mps)) {
    if (arg$echo) cat("Reusing model.mps.gz\n")
    return(gz)
  }
  if (arg$echo) cat("Compressing model.mps ...\n")
  con_in <- file(mps, "rb")
  con_out <- gzfile(gz, "wb")
  on.exit({close(con_in); close(con_out)}, add = TRUE)
  repeat {
    buf <- readBin(con_in, "raw", n = 64 * 1024 * 1024)
    if (!length(buf)) break
    writeBin(buf, con_out)
  }
  gz
}

# The container script. The solver's stdout is deliberately NOT redirected: it
# is the only live progress signal, and cuOpt's own --log-file is buffered for
# tens of minutes.
.cloud_worker_script <- function(s) {
  opts <- c(sprintf("--method %s", s$gpu_method %||% 1L))
  if (!is.null(s$gpu_time_limit)) {
    opts <- c(opts, sprintf("--time-limit %s", s$gpu_time_limit))
  }
  if (!is.null(s$gpu_solver_options)) opts <- c(opts, s$gpu_solver_options)
  c("set -e",
    "nvidia-smi || true",
    # cuOpt's wheels bundle their own CUDA runtime, so a plain python image and
    # the driver are enough -- no CUDA devel image, no apt.
    "pip install -q --no-cache-dir cuopt-cu12",
    # cuopt_cli dispatches on the extension and reads .gz directly, so the
    # matrix is never unpacked inside the container.
    # GPU memory sampled every second during the solve; the peak goes to the log
    "(nvidia-smi --query-gpu=memory.used --format=csv,noheader,nounits -l 1 > /tmp/vram.log 2>/dev/null &) || true",
    paste("cuopt_cli /job/model.mps.gz --solution-file /out/model.sol",
          paste(opts, collapse = " ")),
    "echo VRAM_PEAK_MIB $(sort -n /tmp/vram.log 2>/dev/null | tail -1)",
    "ls -l /out")
}

# Record the job on the run so it can be found after the session ends. The
# COMPLETE handle is persisted (mmcloud::cloud_handle_write): the output
# bucket reference is randomised at submit and cannot be recovered from the
# job id, so without it a finished job's model.sol is unreachable.
.cloud_note_job <- function(arg, job) {
  f <- file.path(arg$solver.dir, "cloud_job.yml")
  tryCatch(mmcloud::cloud_handle_write(job, f),
           error = function(e) invisible(NULL))
  invisible(f)
}

# Turn the solver's .sol into <solver.dir>/output/.
#
# The decoder is python because it rebuilds energyRt's own output contract from
# the generated `output.py` and `input/*.arrow` the write phase left in the
# solver directory -- column names, order, the meta files. Doing that again in
# R would be a second implementation of the same contract, and a reader fault
# here yields plausible wrong numbers rather than an error.
.cloud_decode_solution <- function(solver.dir, python = NULL, echo = FALSE) {
  script <- system.file("python", "sol_to_energyrt.py", package = "energyRt")
  if (!nzchar(script)) {
    stop("sol_to_energyrt.py not found in the installed package.", call. = FALSE)
  }
  py <- mmcloud::cloud_python(python)
  sol <- file.path(solver.dir, "model.sol")
  if (!file.exists(sol)) {
    stop("mmcloud: no model.sol to decode in ", solver.dir, call. = FALSE)
  }
  if (isTRUE(echo)) cat("Decoding solution ...\n")
  out <- suppressWarnings(system2(
    py, c(shQuote(script), shQuote(solver.dir), shQuote(sol)),
    stdout = TRUE, stderr = TRUE))
  st <- attr(out, "status") %||% 0L
  if (isTRUE(echo)) cat(out, sep = "\n")
  if (!identical(as.integer(st), 0L)) {
    stop("Decoding the solution failed:\n",
         paste(utils::tail(out, 25), collapse = "\n"), call. = FALSE)
  }
  invisible(TRUE)
}
