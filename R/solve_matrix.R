# Solving the model through an MPS matrix (`backend = "mm"`).
#
# The matrix is a file in the solver directory, written by multimod during the
# write phase (`lang = "mm"`) or by Pyomo's writer here (`lang = "PYOMO"`). It is
# solved by `multimod::solve_mps()` -- HiGHS in this process or through Julia,
# cuOpt on this machine, in WSL or on a rented GPU -- and the solution is
# decoded into `<solver.dir>/output/` so `read_solution()` is unchanged. Which
# solver and where come from the preset (`mm_solver`, `where`); everything
# about running it lives in multimod, the transport in mmcloud.

.solve_matrix <- function(arg, scen) {
  .need_multimod("a matrix solver preset (backend = 'mm')")
  s <- scen@settings@solver
  .reject_legacy_solver_fields(s)
  writer <- .matrix_writer(s)
  mps <- switch(writer,
    mm = .mm_matrix_file(arg$solver.dir),
    .write_matrix_script(arg, scen, writer))
  a <- .mm_solve_args(s, arg$solver.dir, scen@name)

  # multimod resolves Julia itself; a path set through energyRt's options is
  # handed over for this call
  jpath <- get_julia_path()
  if (identical(a$solver, "julia_highs") && !is.null(jpath) && nzchar(jpath)) {
    op <- options(multimod.julia_path = fp(jpath, "julia"))
    on.exit(options(op), add = TRUE)
  }

  r <- multimod::solve_mps(mps, sol_file = a$sol_file, control = a$control,
                           require_optimal = FALSE, verbose = isTRUE(arg$echo),
                           solver = a$solver, where = a$where,
                           remote = a$remote)
  if (isTRUE(arg$echo)) {
    cat("  ", a$solver, " (", a$where, "): ", r$status_message, ", objective ",
        format(r$objective, digits = 10), "\n", sep = "")
  }
  # every writer leaves index tables beside the matrix: multimod's place the
  # solution by position, Pyomo's and JuMP's carry each label and place it by name
  .mm_decode_solution(arg$solver.dir, r$sol_file,
                      solver = if (identical(a$solver, "cuopt")) "cuopt" else "highs",
                      echo = isTRUE(arg$echo))
  # the matrix is the largest file of a run and is not read again once the
  # solution is decoded; the solution file and the index tables stay, so the
  # decode can be repeated. `keep_mps` (or `cloud_reuse_matrix`) keeps it.
  if (!isTRUE(s$keep_mps) && !isTRUE(s$cloud_reuse_matrix)) {
    unlink(paste0(mps, c("", ".gz")))
  }
  0L
}

# Who wrote the matrix decides how it is produced: multimod writes it in the
# write phase with positional names; Pyomo and JuMP write it by running the
# model script, whose hook names it and writes the index tables.
.matrix_writer <- function(s) {
  lang <- s$lang %||% ""
  if (identical(toupper(lang), "PYOMO")) return("pyomo")
  if (identical(toupper(lang), "JUMP")) return("julia")
  if (any(grepl("^(mm|multimod)$", lang, ignore.case = TRUE))) return("mm")
  stop("A matrix solver preset needs a matrix written by multimod (lang = 'mm'), ",
       "Pyomo (lang = 'PYOMO') or JuMP (lang = 'JuMP'). Got lang = ", lang, ".",
       call. = FALSE)
}

# The files each script writer leaves: the index format is what the language
# writes natively (pyarrow: parquet; Arrow.jl: IPC).
.matrix_writer_files <- function(writer) {
  ext <- c(pyomo = "parquet", julia = "arrow")[[writer]]
  c("model.mps", paste0("model_col_index.", ext), paste0("model_row_index.", ext))
}

# The preset as `multimod::solve_mps()` arguments. Pure, so the mapping can be
# checked without a solver. Settings stored by earlier versions (`mm_engine`,
# `backend = "mmcloud"`) map to the same solver and place.
.mm_solve_args <- function(s, solver.dir, scen_name = "scenario") {
  legacy_remote <- identical(s$backend, "mmcloud")
  solver <- s$mm_solver %||% switch(s$mm_engine %||% "",
    julia = "julia_highs",
    cuopt_wsl = "cuopt",
    if (legacy_remote) "cuopt" else "highs")
  where <- s$where %||% if (identical(s$mm_engine, "cuopt_wsl")) "wsl" else
    if (legacy_remote) "remote" else "local"

  if (identical(solver, "cuopt")) {
    env <- function(v) { x <- Sys.getenv(v, ""); if (nzchar(x)) x else NULL }
    control <- list(method = s$gpu_method %||% 1L,
                    time_limit = s$gpu_time_limit,
                    presolve = s$gpu_presolve,
                    options = s$gpu_solver_options,
                    wsl_user = s$wsl_user %||% env("ENERGYRT_WSL_USER"),
                    cuopt_cli = s$cuopt_cli %||% env("ENERGYRT_CUOPT_CLI"))
    control <- control[!vapply(control, is.null, logical(1))]
  } else {
    # `highs_options`: named HiGHS options, e.g. list(presolve = "off")
    control <- s$highs_options %||% list()
    if (length(control) && (!is.list(control) || is.null(names(control)) ||
                            any(!nzchar(names(control))))) {
      stop("solver `highs_options` must be a named list of HiGHS options",
           call. = FALSE)
    }
  }

  remote <- list()
  if (identical(where, "remote")) {
    remote <- list(flavor = s$cloud_flavor %||% "a10g-large",
                   timeout = s$cloud_timeout %||% "3h",
                   namespace = s$cloud_namespace,
                   image = s$cloud_image %||% "python:3.12",
                   provider = s$cloud_provider %||% "hf_jobs",
                   poll = s$cloud_poll %||% 15,
                   max_wait = s$cloud_max_wait %||% Inf,
                   # the handle outlives the session: a killed R session can
                   # still poll, cancel or fetch the job
                   handle_file = fp(solver.dir, "cloud_job.yml"),
                   name = paste0("energyrt-", scen_name))
  }
  list(solver = solver, where = where, control = control, remote = remote,
       sol_file = fp(solver.dir, if (identical(where, "remote")) "model.sol.gz" else "model.sol"))
}

# The `hf_*` option names are gone: transport settings are `cloud_*` and solver
# settings are `gpu_*`, because `hf_method` and friends were never platform
# settings at all -- they are solver flags. An unrecognised field on a solver
# spec is SILENTLY IGNORED, and the defaults it falls back to are not benign:
# an unset namespace resolves to the token's personal account (HTTP 402), an
# unset time limit lets cuopt_cli stop after its own 60 s, and the default
# flavor is 24 GB of VRAM against the ~63 GB a full-year case needs. So a stale
# name is refused rather than dropped.
.legacy_solver_fields <- c(
  hf_flavor = "cloud_flavor", hf_timeout = "cloud_timeout",
  hf_namespace = "cloud_namespace", hf_image = "cloud_image",
  hf_poll = "cloud_poll", hf_max_wait = "cloud_max_wait",
  hf_reuse_matrix = "cloud_reuse_matrix", hf_method = "gpu_method",
  hf_time_limit = "gpu_time_limit", hf_solver_options = "gpu_solver_options")

.reject_legacy_solver_fields <- function(s) {
  found <- intersect(names(.legacy_solver_fields), names(s))
  if (!length(found)) return(invisible())
  stop("solver spec carries renamed field(s):\n",
       paste0("  ", found, " -> ", .legacy_solver_fields[found],
              collapse = "\n"),
       "\n`hf_*` became `cloud_*` for transport and `gpu_*` for solver ",
       "options. An unknown field would be ignored and the job would run at ",
       "the defaults, so it is refused instead.", call. = FALSE)
}

# A preset renamed without an alias: its stub names the replacement, and a
# solve with it stops here rather than falling back to some default.
.check_renamed_preset <- function(s) {
  if (is.list(s) && !is.null(s$renamed_to)) {
    stop("solver preset '", s$name, "' was renamed to '", s$renamed_to,
         "': use solver_options$", s$renamed_to, ".", call. = FALSE)
  }
  invisible()
}

# The matrix multimod wrote in the write phase.
.mm_matrix_file <- function(solver.dir) {
  mps <- fp(solver.dir, "model.mps")
  if (!file.exists(mps)) {
    stop("lang = 'mm': no model.mps in ", solver.dir,
         " - was the model written?", call. = FALSE)
  }
  mps
}

# Pyomo or JuMP writes the matrix by running the written model script: the
# preset's hook (Pyomo `inc4`, JuMP `inc3`) serialises the model, writes the
# index tables (label, symbol, index tuple) and exits before solving (see
# solver_options$pyomo_mps, $julia_mps). The dimension names come from
# energyRt's own output template. A matrix already there is reused when
# `cloud_reuse_matrix` is set: re-solving with other options should not pay for
# the build again.
.write_matrix_script <- function(arg, scen, writer) {
  s <- scen@settings@solver
  files <- fp(arg$solver.dir, .matrix_writer_files(writer))
  mps <- files[1]
  idx <- files[-1]
  .write_template_dim_names(scen, arg$solver.dir, writer)
  if (isTRUE(s$cloud_reuse_matrix) && all(file.exists(files))) {
    if (isTRUE(arg$echo)) {
      cat(sprintf("Reusing the matrix already built (%.2f GB, %s)\n",
                  file.size(mps) / 1e9,
                  format(file.mtime(mps), "%Y-%m-%d %H:%M")))
    }
    return(mps)
  }
  unlink(files)
  if (isTRUE(arg$echo)) cat("Writing the matrix with", writer, "...\n")
  home <- getwd()
  on.exit(setwd(home), add = TRUE)
  setwd(arg$solver.dir)
  rs <- system(paste(s$cmdline), wait = TRUE, ignore.stdout = !isTRUE(arg$echo))
  if (!identical(as.integer(rs), 0L)) {
    stop(writer, " could not write the matrix (exit ", rs, ").", call. = FALSE)
  }
  if (!all(file.exists(files))) {
    stop(writer, " wrote no 'model.mps' with its index tables in ", arg$solver.dir,
         ". The preset must carry the hook that writes them (see ",
         "solver_options$pyomo_mps, $julia_mps).", call. = FALSE)
  }
  mps
}

# The dimension names of each variable, in index order, from energyRt's output
# template: each variable's block opens `output/<v>.csv`, writes its header
# (`tech,region,year,value`) and loops over its index in that order. Pyomo:
# `f = open(...)` then `f.write("...\n")`; JuMP: `fvX = open(...);` then
# `println(fvX, "...");`.
.template_dim_names <- function(src) {
  s <- unlist(strsplit(paste(src, collapse = "\n"), "\n", fixed = TRUE))
  open_re <- '^\\s*f[A-Za-z0-9_]* = open\\("output/(v[A-Z][A-Za-z0-9_]*)\\.csv", "w"\\);?\\s*$'
  hdr_re <- '^\\s*(f\\.write\\(|println\\(f[A-Za-z0-9_]*, )"([A-Za-z0-9_,]+)(\\\\n)?"\\);?\\s*$'
  at <- grep(open_re, s)
  out <- lapply(at, function(k) {
    sym <- sub(open_re, "\\1", s[k])
    nxt <- s[(k + 1L):min(k + 3L, length(s))]
    hdr <- nxt[nzchar(trimws(nxt))][1]
    # a Pyomo scalar writes its header and value in one call: no dimensions
    if (grepl('^\\s*f\\.write\\("value\\\\n" \\+', hdr)) return(NULL)
    if (!grepl(hdr_re, hdr)) {
      stop("Unexpected output template after `", trimws(s[k]), "`: ",
           trimws(hdr), call. = FALSE)
    }
    cols <- setdiff(strsplit(sub(hdr_re, "\\2", hdr), ",")[[1]], "value")
    if (!length(cols)) return(NULL)
    data.frame(symbol = sym, position = seq_along(cols), dim = cols)
  })
  do.call(rbind, out)
}

.write_template_dim_names <- function(scen, solver.dir, writer) {
  key <- c(pyomo = "PYOMOConcreteOutput", julia = "JuMPOutput")[[writer]]
  src <- scen@settings@sourceCode[[key]] %||% .modelCode[[key]]
  utils::write.csv(.template_dim_names(src), fp(solver.dir, "model_dim_names.csv"),
                   row.names = FALSE)
}

# Decode a solver's `.sol` against multimod's index files and write the result
# as the `output/` tree `read_solution()` expects. Shared by every solver and
# place, which differ only in who produced the `.sol`.
.mm_decode_solution <- function(solver.dir, sol, solver = c("auto", "highs",
                                                            "cuopt"),
                                echo = FALSE) {
  solver <- match.arg(solver)
  if (!file.exists(sol)) {
    stop("lang = 'mm': no solution file to decode at ", sol, call. = FALSE)
  }
  if (isTRUE(echo)) cat("Decoding solution ...\n")
  # variables substituted at build time are written from their definitions
  # one at a time, never held together with the solved columns
  args <- list(sol, index_dir = solver.dir, format = "parquet", solver = solver)
  if ("substituted" %in% names(formals(multimod::read_mps_solution))) {
    args$substituted <- "defer"
  }
  s <- do.call(multimod::read_mps_solution, args)
  multimod::write_energyrt_output(s, fp(solver.dir, "output"),
                                  format = "arrow")
  # the decoded columns are released before read_solution() loads output/
  rm(s)
  gc(FALSE)
  invisible(NULL)
}
