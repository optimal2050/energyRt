# Write the model as an MPS matrix built by multimod (`lang = "mm"`).
#
# Unlike the other writers this emits no model script: multimod assembles the
# LP straight from the AST and `highs` writes the matrix. The row and column
# names in that MPS are POSITIONAL (`c0`, `r0`), which is why it is small, so
# the `_col_index` / `_row_index` / `_dim_names` files written beside it are the
# only way back to (symbol, index tuple) and must travel with the `.mps`.
#
# Both the local and the cloud route consume those files, so the matrix is
# handed on as files rather than as an in-memory LP: a scenario is serialised
# into the store, and a large LP on `@misc` would be written out with it.

.write_model_MM <- function(arg, scen) {
  .need_multimod("solver lang = 'mm'")

  # `read_solution()` derives the per-variable extension from `import_format`
  # and falls back to "csv" when it is unset (R/read.R:307-315, :428-430), while
  # `multimod::write_energyrt_output()` writes `.arrow`. Recording the format
  # here is what keeps the reader and the writer agreeing.
  scen@settings@solver <- .resolve_exchange_formats(scen@settings@solver)
  scen@settings@solver$export_format <- "feather"
  scen@settings@solver$import_format <- "feather"

  dir.create(fp(arg$solver.dir, "output"), recursive = TRUE,
             showWarnings = FALSE)

  # The artificial-member substitution must not have run: it replaces the fold
  # wildcards with `ANY*` / year `0` labels in the DATA while the equations read
  # from the GAMS template still gate on the real dims, so the folded lookups
  # match nothing. The symptom is a smaller matrix that solves to Optimal at
  # objective 0 rather than an error, which is why this is checked rather than
  # trusted.
  if (length(scen@misc$fold_artificial) > 0) {
    stop("This scenario carries artificial fold members (",
         paste(scen@misc$fold_artificial, collapse = ", "),
         "), which the 'mm' route cannot read: its equations come from the ",
         "GAMS template and index the real dimensions.\n",
         "  `.fold_code_block()` returns NA for 'mm' so the substitution is ",
         "skipped; something re-enabled it.", call. = FALSE)
  }

  m <- multimod_from_energyRt(scen, verbose = isTRUE(arg$echo))

  # `on_empty_row` defaults to "user_constraints": a `^eqCns` row with no
  # coefficients reads `0 <op> rhs`, binds nothing, and still solves to
  # Optimal, so it is refused rather than written.
  lp <- multimod::model_to_lp(m, verbose = isTRUE(arg$echo))

  if (isTRUE(arg$echo)) {
    cat("  matrix: ", nrow(lp$A), " rows x ", ncol(lp$A), " cols, ",
        length(lp$A@x), " nonzeros\n", sep = "")
  }

  multimod::write_mps(m, fp(arg$solver.dir, "model.mps"),
                      index_dir = arg$solver.dir, format = "parquet",
                      lp = lp, verbose = isTRUE(arg$echo))

  # The matrix is solved by `.call_solver()`'s `mm` branch, in this process or
  # on a rented GPU. There is no external command.
  scen@settings@solver$cmdline <- ""
  scen@settings@solver$code <- .mm_matrix_files()
  scen
}

# The matrix and the three files that decode it.
.mm_matrix_files <- function() {
  c("model.mps", "model_col_index.parquet", "model_row_index.parquet",
    "model_dim_names.csv")
}

# Decode a solver's `.sol` against the index files and write the result as the
# `output/` tree `read_solution()` expects. Shared by the local and cloud
# routes, which differ only in who produced the `.sol`.
.mm_decode_solution <- function(solver.dir, sol, solver = c("auto", "highs",
                                                            "cuopt"),
                                echo = FALSE) {
  solver <- match.arg(solver)
  if (!file.exists(sol)) {
    stop("lang = 'mm': no solution file to decode at ", sol, call. = FALSE)
  }
  if (isTRUE(echo)) cat("Decoding solution ...\n")
  s <- multimod::read_mps_solution(sol, index_dir = solver.dir,
                                   format = "parquet", solver = solver)
  multimod::write_energyrt_output(s, fp(solver.dir, "output"),
                                  format = "arrow")
  invisible(s)
}

# Local solve: HiGHS reads the MPS we just wrote and emits a `.sol` beside it.
# Going back through the file keeps this path identical to the cloud one.
.mm_call_solver <- function(arg, scen) {
  .need_multimod("solver lang = 'mm'")
  mps <- fp(arg$solver.dir, "model.mps")
  if (!file.exists(mps)) {
    stop("lang = 'mm': no model.mps in ", arg$solver.dir,
         " - was the model written?", call. = FALSE)
  }
  # `highs_options` on the preset: named HiGHS options, e.g. list(presolve = "off")
  control <- scen@settings@solver$highs_options
  if (is.null(control)) control <- list()
  if (length(control) && (!is.list(control) || is.null(names(control)) || any(!nzchar(names(control))))) {
    stop("solver `highs_options` must be a named list of HiGHS options", call. = FALSE)
  }
  # `mm_engine = "julia"`: the MPS is solved by Julia's HiGHS (HiGHS.jl), whose
  # build carries the HiPO interior point solver and writes the `.sol` itself
  if (identical(scen@settings@solver$mm_engine, "julia")) {
    rs <- .mm_julia_solve(arg, mps, control)
    if (rs != 0) return(rs)
    .mm_decode_solution(arg$solver.dir, fp(arg$solver.dir, "model.sol"),
                        solver = "highs", echo = isTRUE(arg$echo))
    return(0L)
  }
  r <- multimod::solve_mps(mps, control = control, require_optimal = FALSE,
                           verbose = isTRUE(arg$echo))
  if (isTRUE(arg$echo)) {
    cat("  HiGHS: ", r$status_message, ", objective ",
        format(r$objective, digits = 10), "\n", sep = "")
  }
  .mm_decode_solution(arg$solver.dir, r$sol_file, solver = "highs",
                      echo = isTRUE(arg$echo))
  0L
}

# Solve model.mps with HiGHS through HiGHS.jl's C API: no JuMP model is built.
# Writes solve_mps.jl into the solver directory (the options typed from R) and
# runs it with the Julia found by get_julia_path(); HiGHS writes model.sol.
.mm_julia_solve <- function(arg, mps, control) {
  setter <- function(nm, v) {
    if (is.logical(v)) sprintf('HiGHS.Highs_setBoolOptionValue(h, "%s", %s)', nm, if (isTRUE(v)) "1" else "0")
    else if (is.integer(v)) sprintf('HiGHS.Highs_setIntOptionValue(h, "%s", %d)', nm, v)
    else if (is.numeric(v)) sprintf('HiGHS.Highs_setDoubleOptionValue(h, "%s", %s)', nm, format(v, digits = 17))
    else sprintf('HiGHS.Highs_setStringOptionValue(h, "%s", "%s")', nm, as.character(v))
  }
  jl <- c(
    "using HiGHS",
    "h = HiGHS.Highs_create()",
    "t0 = time()",
    "HiGHS.Highs_readModel(h, ARGS[1]) == 0 || error(\"HiGHS could not read \" * ARGS[1])",
    vapply(names(control), function(nm) setter(nm, control[[nm]]), ""),
    "HiGHS.Highs_run(h)",
    "t1 = time()",
    "HiGHS.Highs_writeSolution(h, ARGS[2])",
    "st = HiGHS.Highs_getModelStatus(h)",
    "println(\"HiGHS (Julia): model status \", st, \", objective \", HiGHS.Highs_getObjectiveValue(h), \", \", round(t1 - t0, digits = 1), \" s\")",
    "HiGHS.Highs_destroy(h)")
  writeLines(jl, fp(arg$solver.dir, "solve_mps.jl"))
  jpath <- get_julia_path()
  julia <- if (is.null(jpath) || !nzchar(jpath)) "julia" else fp(jpath, "julia")
  rs <- system2(julia, c(shQuote(fp(arg$solver.dir, "solve_mps.jl")), shQuote(mps),
                         shQuote(fp(arg$solver.dir, "model.sol"))),
                stdout = if (isTRUE(arg$echo)) "" else FALSE)
  if (!file.exists(fp(arg$solver.dir, "model.sol"))) {
    stop("lang = 'mm', mm_engine = 'julia': HiGHS wrote no solution (Julia exit ", rs, ")", call. = FALSE)
  }
  as.integer(rs)
}
