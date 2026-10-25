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

  # folded numeric parameters stay folded when this multimod can look them up;
  # the tables are not copied into the model: multimod reads them from the
  # scenario's store or the dataset store as the matrix is built, through a
  # memo of `data_cache_gb` gigabytes (a preset field; 32 by default)
  sv0 <- scen@settings@solver
  op <- options(energyRt.mm_keep_wildcards = .mm_reads_wildcards(),
                multimod.data_cache_gb = sv0$data_cache_gb %||% 32)
  m <- multimod_from_energyRt(scen, inMemory = isTRUE(sv0$import_in_memory),
                              verbose = isTRUE(arg$echo))
  options(op)

  # The matrix is built block by block and streamed to the file, so the LP
  # is never held in memory. The solver preset may carry `engine` ("native",
  # the default, or "r"), `presolve` (default TRUE: the doubleton
  # substitution applied as the entries pass through) and `threads` for
  # multimod's writer. `on_empty_row` defaults to "user_constraints": a
  # `^eqCns` row with no coefficients reads `0 <op> rhs`, binds nothing, and
  # still solves to Optimal, so it is refused rather than written.
  # `substitute` names the definition equations substituted while the matrix
  # is built (their rows and the defined columns are never written); by
  # default the activity and output definitions and the per-process totals,
  # `character()` for none.
  sv <- scen@settings@solver
  op <- options(multimod.engine = sv$engine %||% "native")
  if (!is.null(sv$threads)) op <- c(op, options(multimod.threads = sv$threads))
  on.exit(options(op), add = TRUE)
  wargs <- list(m, fp(arg$solver.dir, "model.mps"),
                index_dir = arg$solver.dir, format = "parquet",
                stream = TRUE, presolve = isTRUE(sv$presolve %||% TRUE),
                verbose = isTRUE(arg$echo))
  sub <- .mm_substitute(sv, m)
  if (!is.null(sub)) wargs$substitute <- sub
  do.call(multimod::write_mps, wargs)

  # The matrix is on disk: nothing of it is needed through the solve. The
  # model, multimod's memo and the scenario's tables that are persisted
  # elsewhere go, and the heap the build used is collected.
  rm(m)
  if ("clear_data_cache" %in% getNamespaceExports("multimod")) {
    multimod::clear_data_cache()
  }
  scen <- .shed_persisted_tables(scen)
  gc(FALSE)

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

# Does the installed multimod look parameters up on their explicit columns when
# some index columns are wildcards (NA)?
.mm_reads_wildcards <- function() {
  exists(".mx_param_wildcard", envir = asNamespace("multimod"), inherits = FALSE)
}

#' Definition equations to substitute while multimod builds the matrix
#'
#' The preset's `substitute` field, or by default those of the activity and
#' output definitions and the per-process totals the model holds;
#' `character()` turns it off. NULL when the installed multimod's `write_mps()` has no `substitute`
#' argument (it then builds every row).
#' @keywords internal
#' @noRd
.mm_substitute <- function(sv, m) {
  if (!"substitute" %in% names(formals(multimod::write_mps))) {
    if (length(sv$substitute)) {
      warning("The installed multimod cannot substitute equations; `substitute` is ignored.",
              call. = FALSE)
    }
    return(NULL)
  }
  if (!is.null(sv$substitute)) return(as.character(sv$substitute))
  # each total is used by one other equation, so substituting it moves its
  # entries there and adds none
  intersect(c("eqTechAfFx", "eqTechActSng", "eqTechOutTot", "eqTechInpTot",
              "eqStorageInpTot", "eqStorageOutTot", "eqSupOutTot", "eqImportTot",
              "eqExportTot", "eqDemInp"), names(m$equations))
}
