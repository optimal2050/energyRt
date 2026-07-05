# =========================================================================== #
# exchange.R  --  single-file table exchange with the JuMP / Pyomo solvers.
#
# Each model parameter (input) and solution variable (output) is written as ONE
# self-contained file in the solver run-folder (`<scenario>/script/<solver>/
# input|output/`), which Arrow.jl (Julia) and pyarrow (Python) read directly.
# This is distinct from the partitioned `arrow::write_dataset` used for on-disk
# scenario STORAGE (R/arrow.R): here a single data.frame maps to a single file.
#
# Format / compression come from the package options (R/options.R):
# `arrow_format` (feather | parquet | csv), `arrow_compression` (zstd | lz4 |
# uncompressed), `arrow_compression_level` (ZSTD: 1-22). The solver's
# `export_format` / `import_format` selects this at write / read time.
# =========================================================================== #

# Map an exchange format name to its file extension.
.exchange_ext <- function(format) {
  switch(tolower(format),
    feather = "arrow", ipc = "arrow", arrow = "arrow",
    parquet = "parquet",
    csv = "csv",
    stop("Unknown arrow exchange format: '", format, "'")
  )
}

# Write ONE data.frame to ONE file `<path_noext>.<ext>`. Returns the file path.
.write_exchange_table <- function(df, path_noext,
                                  format = get_arrow_format(),
                                  compression = get_arrow_compression(),
                                  level = get_arrow_compression_level()) {
  df  <- as.data.frame(df)
  ext <- .exchange_ext(format)
  path <- paste0(path_noext, ".", ext)
  uncompressed <- identical(tolower(compression), "uncompressed")
  if (ext == "arrow") {
    if (uncompressed) {
      arrow::write_feather(df, path, compression = "uncompressed")
    } else {
      arrow::write_feather(df, path, compression = compression,
                           compression_level = as.integer(level))
    }
  } else if (ext == "parquet") {
    if (uncompressed) {
      arrow::write_parquet(df, path, compression = "uncompressed")
    } else {
      arrow::write_parquet(df, path, compression = compression,
                           compression_level = as.integer(level))
    }
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
