# =========================================================================== #
# exchange.R  --  single-file table exchange with the JuMP / Pyomo solvers.
#
# Each model parameter (input) and solution variable (output) is written as ONE
# self-contained file in the run's solver directory (`<scenario>/runs/<run>/
# solver/input|output/`), which Arrow.jl (Julia) and pyarrow (Python) read
# directly.
# This is distinct from the partitioned `arrow::write_dataset` used for on-disk
# scenario STORAGE (R/arrow.R): here a single data.frame maps to a single file.
#
# Format / compression come from the EXCHANGE options (R/options.R):
# `exchange_format` (feather | parquet | csv), `exchange_compression` (zstd |
# lz4 | uncompressed), `exchange_compression_level` (ZSTD: 1-22). These are
# separate from the storage options: an exchange file is written and read
# within one solve, so it favours a cheap codec. The solver's
# `export_format` / `import_format` overrides the option at write / read time.
# =========================================================================== #

# Legacy INPUT containers: one bundle written by R and read by a backend
# library (RData.jl on the Julia side, sqlite3 on the Python side). Kept
# reachable, but no longer the default -- see `exchange_format`.
.exchange_legacy_in <- c("sqlite", "rdata")

# Resolve a JuMP/Pyomo solver's exchange formats before writing. A preset that
# names neither follows the `exchange_format` option; one that asks for a
# legacy input keeps the historical CSV output unless it says otherwise. The
# resolved values are written back onto the solver list so read_solution()
# sees a concrete format instead of guessing.
.resolve_exchange_formats <- function(solver) {
  ex <- solver$export_format
  if (is.null(ex) || !nzchar(ex)) ex <- get_exchange_format()
  im <- solver$import_format
  if (is.null(im) || !nzchar(im)) {
    im <- if (tolower(ex) %in% .exchange_legacy_in) "csv" else ex
  }
  solver$export_format <- ex
  solver$import_format <- im
  solver
}

# Does this format name select the Arrow exchange (as opposed to a legacy
# container or csv)?
.is_arrow_exchange <- function(fmt) {
  !is.null(fmt) && tolower(fmt) %in% c("feather", "ipc", "arrow", "parquet")
}

# Drop the tables with no rows. The generated model code never reads them --
# an empty set becomes a literal `set()` / `[]` and an empty parameter a
# default-only lookup -- so writing them is pure overhead. On a UTOPIA-size
# model roughly two thirds of the tables are empty.
.drop_empty_tables <- function(dat) {
  keep <- vapply(dat, function(x) NROW(x) > 0L, logical(1))
  dat[keep]
}

# Map an exchange format name to its file extension.
.exchange_ext <- function(format) {
  switch(tolower(format),
    feather = "arrow", ipc = "arrow", arrow = "arrow",
    parquet = "parquet",
    csv = "csv",
    stop("Unknown arrow exchange format: '", format, "'")
  )
}

# Windows refuses a path of 260 characters or more, and reports it as "cannot
# find the path specified" -- indistinguishable from a missing directory. The
# exchange writes one file per symbol, and a generated variant constraint has a
# long name, so a deep scenarios directory overruns the limit on some solvers
# and not others (the run directory carries the solver's name).
#
# Checked before anything is written, so the failure names the cause instead of
# surfacing from inside an Arrow writer half way through.
#' @noRd
.check_exchange_path_len <- function(in_dir, symbols, format, limit = 259L) {
  if (.Platform$OS.type != "windows") return(invisible(NULL))
  if (length(symbols) == 0L) return(invisible(NULL))
  root <- if (grepl("^([A-Za-z]:|[\\/])", in_dir)) in_dir
          else file.path(getwd(), in_dir)
  files <- paste0(symbols, ".", .exchange_ext(format))
  n <- nchar(root) + 1L + nchar(files)
  if (max(n) <= limit) return(invisible(NULL))
  worst <- which.max(n)
  stop("the solver exchange needs a ", max(n), "-character path and Windows ",
       "allows ", limit + 1L, ".
",
       "  directory     : ", root, " (", nchar(root), " chars)
",
       "  longest symbol: ", files[worst], " (", nchar(files[worst]),
       " chars)
",
       "  Shorten set_scenarios_path(), or use a single-file exchange ",
       "(solver_options$julia_highs_rdata, solver_options$pyomo_cbc_sqlite).",
       call. = FALSE)
}

# Write ONE data.frame to ONE file `<path_noext>.<ext>`. Returns the file path.
.write_exchange_table <- function(df, path_noext,
                                  format = get_exchange_format(),
                                  compression = get_exchange_compression(),
                                  level = get_exchange_compression_level()) {
  df  <- as.data.frame(df)
  ext <- .exchange_ext(format)
  path <- paste0(path_noext, ".", ext)
  compression <- tolower(compression)
  # `compression_level` is only meaningful for zstd; arrow errors when it is
  # supplied alongside any other codec (lz4 included).
  args <- c(list(df, path, compression = compression),
            if (identical(compression, "zstd")) {
              list(compression_level = as.integer(level))
            })
  if (ext == "arrow") {
    do.call(arrow::write_feather, args)
  } else if (ext == "parquet") {
    do.call(arrow::write_parquet, args)
  } else {
    data.table::fwrite(df, path)
  }
  path
}

# Read ONE exchange file into a data.frame (dispatched on extension).
.read_exchange_table <- function(path) {
  ext <- tolower(tools::file_ext(path))
  d <- switch(ext,
    arrow   = arrow::read_feather(path),
    feather = arrow::read_feather(path),
    parquet = arrow::read_parquet(path),
    csv     = data.table::fread(path, stringsAsFactors = FALSE),
    stop("Unknown exchange file extension: '", path, "'")
  )
  as.data.frame(d)
}
