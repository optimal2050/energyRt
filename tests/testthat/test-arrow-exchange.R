# Apache Arrow (IPC/feather) data exchange for the JuMP / Pyomo solvers: model
# data (input) and the solution (output) are exchanged as compressed Arrow files
# -- the DEFAULT since the exchange/storage option split -- with RData (Julia)
# and SQLite (Pyomo) reachable as the legacy fallback via the solver's
# export_format / import_format. The solve must be format-invariant.
#
# Two option families govern this and must not be confused: `storage_*` is the
# codec of tables that outlive the session, `exchange_*` the codec of the files
# handed to a solver for one solve.

.ax_find <- function(rel) {
  cands <- c(testthat::test_path("fixtures", basename(rel)),
             rel, file.path("..", "..", rel))
  for (cand in cands) if (file.exists(cand)) return(cand)
  NULL
}

test_that(".write/.read_exchange_table round-trips type-safely (feather + parquet)", {
  df <- data.frame(tech = rep(c("A", "B"), 50), region = "R1",
                   year = rep(2020:2024L, 20), timeslice = NA_character_,
                   value = runif(100), stringsAsFactors = FALSE)
  d <- file.path(tempdir(), "axrt"); dir.create(d, showWarnings = FALSE)
  for (fmt in c("feather", "parquet")) {
    p <- .write_exchange_table(df, file.path(d, paste0("t_", fmt)), format = fmt)
    expect_true(file.exists(p))
    back <- .read_exchange_table(p)
    expect_equal(back, df, ignore_attr = TRUE)
    expect_true(is.integer(back$year))           # integer year preserved
    expect_true(is.character(back$timeslice))         # all-NA char survives (CSV loses it)
  }
})

test_that("arrow exchange solves identically to the legacy format (Julia + Pyomo)", {
  tm <- .ax_find("data-raw/testing-models.R")
  so <- .ax_find("data-raw/solver_options.R")
  skip_if(is.null(tm) || is.null(so), "data-raw/ builders not available")
  source(tm, local = TRUE)
  so_env <- new.env(); sys.source(so, envir = so_env)
  solver_options <- so_env$solver_options
  mod <- tm_core()

  obj <- function(s) tryCatch(getData(s, "vObjective", merge = TRUE)$value[1],
                              error = function(e) NA_real_)
  solve1 <- function(sv, tag) {
    td <- file.path(tempdir(), paste0("ax_", tag)); unlink(td, recursive = TRUE)
    on.exit(unlink(td, recursive = TRUE), add = TRUE)
    suppressWarnings(suppressMessages(
      solve_model(mod, name = tag, solver = sv, tmp.dir = td, force = TRUE)))
  }

  skip_if(is.null(get_glpk_path()) || !nzchar(get_glpk_path()), "GLPK not configured")
  g <- obj(solve1(solver_options$glpk, "g"))
  expect_false(is.na(g))

  # JuMP: input feather (Arrow.jl) + solution feather. FAILS without the arrow path.
  if (!is.null(get_julia_path()) && nzchar(get_julia_path())) {
    sj <- solver_options$julia_highs
    sj$export_format <- "feather"; sj$import_format <- "feather"
    expect_equal(obj(solve1(sj, "jl")), g, tolerance = 1e-6)
  }
  # Pyomo: input feather (pyarrow) + solution feather.
  if (!is.null(get_python_path()) && nzchar(get_python_path())) {
    sp <- solver_options$pyomo_cbc
    sp$export_format <- "feather"; sp$import_format <- "feather"
    expect_equal(obj(solve1(sp, "py")), g, tolerance = 1e-6)
  }
})

# --- option split: storage vs exchange ------------------------------------- #

ax_local_opts <- function(env = parent.frame()) {
  old <- list(
    sf = get_storage_format(), sc = get_storage_compression(),
    xf = get_exchange_format(), xc = get_exchange_compression())
  expr <- bquote({
    old <- .(old)
    set_storage_format(old$sf); set_storage_compression(old$sc)
    set_exchange_format(old$xf); set_exchange_compression(old$xc)
  })
  do.call(on.exit, list(expr, add = TRUE), envir = env)
  invisible(NULL)
}

test_that("storage and exchange codecs are independent options", {
  ax_local_opts()
  set_storage_format("parquet"); set_exchange_format("feather")
  expect_identical(get_storage_format(), "parquet")
  expect_identical(get_exchange_format(), "feather")

  # each writer reads its own family
  expect_identical(formals(save_scenario)$format, quote(get_storage_format()))
  expect_identical(formals(data2disk)$format, quote(get_storage_format()))
  expect_identical(formals(.write_exchange_table)$format,
                   quote(get_exchange_format()))

  # and the exchange writer follows the exchange option end to end
  d <- file.path(tempdir(), "ax_split"); dir.create(d, showWarnings = FALSE)
  set_exchange_format("parquet")
  p <- .write_exchange_table(data.frame(a = "x", value = 1), file.path(d, "t1"))
  expect_identical(tools::file_ext(p), "parquet")
})

test_that("a non-zstd codec never receives a compression level", {
  ax_local_opts()
  # arrow errors on `compression_level` unless the codec is zstd; the lz4
  # exchange default used to trip that.
  d <- file.path(tempdir(), "ax_cmp"); dir.create(d, showWarnings = FALSE)
  for (codec in c("lz4", "uncompressed", "zstd")) {
    set_exchange_compression(codec)
    p <- expect_silent(
      .write_exchange_table(data.frame(a = "x", value = 1),
                            file.path(d, paste0("c_", codec))))
    expect_equal(.read_exchange_table(p)$value, 1)
  }
})

# --- format resolution ------------------------------------------------------ #

test_that("a preset naming no format follows the exchange option", {
  ax_local_opts()
  set_exchange_format("feather")
  r <- .resolve_exchange_formats(list(lang = "JuMP", solver = "HiGHS"))
  expect_identical(r$export_format, "feather")
  expect_identical(r$import_format, "feather")

  # a legacy INPUT keeps the historical csv OUTPUT
  r <- .resolve_exchange_formats(list(export_format = "SQLite"))
  expect_identical(r$import_format, "csv")
  r <- .resolve_exchange_formats(list(export_format = "RData"))
  expect_identical(r$import_format, "csv")

  # an explicit pair is never overridden
  r <- .resolve_exchange_formats(list(export_format = "feather",
                                      import_format = "csv"))
  expect_identical(r$import_format, "csv")
})

test_that("the shipped presets default to arrow, with a legacy escape hatch", {
  for (nm in c("julia_highs", "pyomo_cbc")) {
    r <- .resolve_exchange_formats(solver_options[[nm]])
    expect_true(.is_arrow_exchange(r$export_format), info = nm)
    expect_true(.is_arrow_exchange(r$import_format), info = nm)
  }
  expect_identical(
    .resolve_exchange_formats(solver_options$pyomo_cbc_sqlite)$export_format,
    "SQLite")
  expect_identical(
    .resolve_exchange_formats(solver_options$julia_highs_rdata)$export_format,
    "RData")
  # GAMS is untouched by the exchange options
  expect_identical(
    .resolve_exchange_formats(solver_options$gams_gdx_cplex)$export_format,
    "GDX")
})

test_that("parquet is refused on the Julia path instead of silently downgraded", {
  # Arrow.jl has no parquet reader. Served as feather, R would then look for
  # output/<var>.parquet, find nothing, and read the scenario back EMPTY.
  expect_error(.assert_julia_arrow_format("parquet", "export_format"),
               "cannot exchange parquet")
  expect_true(.assert_julia_arrow_format("feather", "export_format"))
})

# --- generated solver code -------------------------------------------------- #

test_that("the emitted output writers follow the exchange options", {
  ax_local_opts()
  set_exchange_compression("lz4")
  jl <- grep("Arrow.write", .jump_arrow_output_helpers(), value = TRUE)
  expect_match(jl, "compress = :lz4", fixed = TRUE)
  py <- grep("write_feather", .pyomo_arrow_output_helpers(), value = TRUE)
  expect_match(py, 'compression="lz4"', fixed = TRUE)
  expect_false(grepl("compression_level", py))   # zstd-only argument

  set_exchange_compression("zstd"); set_exchange_compression_level(7L)
  py <- grep("write_feather", .pyomo_arrow_output_helpers(), value = TRUE)
  expect_match(py, "compression_level=7", fixed = TRUE)
  jl <- grep("Arrow.write", .jump_arrow_output_helpers(), value = TRUE)
  expect_match(jl, "compress = :zstd", fixed = TRUE)

  # Pyomo can write parquet; the extension and the writer both switch
  py <- .pyomo_arrow_output_helpers(format = "parquet")
  expect_true(any(grepl("import pyarrow.parquet", py, fixed = TRUE)))
  expect_true(any(grepl('".parquet"', py, fixed = TRUE)))
  expect_false(any(grepl("write_feather", py, fixed = TRUE)))

  set_exchange_compression("uncompressed")
  jl <- grep("Arrow.write", .jump_arrow_output_helpers(), value = TRUE)
  expect_match(jl, "compress = nothing", fixed = TRUE)
})

test_that("the generated loaders name the missing dependency", {
  # a bare "Package Arrow not found" / ModuleNotFoundError says nothing about
  # how to proceed, and the solve fails inside the backend
  jl <- .jump_arrow_output_helpers()
  expect_true(any(grepl("Arrow", jl)))
  py_tpl <- energyRt:::.modelCode$PYOMOConcrete
  expect_true(any(grepl("en_install_python_deps", py_tpl)))
  expect_true(any(grepl("pyomo_cbc_sqlite", py_tpl)))
  expect_true(any(grepl('_DATA_FORMAT == "parquet"', py_tpl, fixed = TRUE)))
})

test_that("empty tables are not written", {
  dat <- list(a = data.frame(x = 1), b = data.frame(x = numeric(0)),
              c = data.frame())
  expect_identical(names(.drop_empty_tables(dat)), "a")
})

# --- storage format: the store's own codec wins ----------------------------- #

test_that(".store_format prefers the files, then the manifest, then the option", {
  ax_local_opts()
  set_storage_format("parquet")
  d <- file.path(tempdir(), "ax_sf"); unlink(d, recursive = TRUE)
  dir.create(file.path(d, "data"), recursive = TRUE)

  # empty store -> manifest, then option
  mf <- file.path(d, "scenario.yml")
  expect_identical(.store_format(file.path(d, "data")), "parquet")  # option
  yaml::write_yaml(list(format = "feather"), mf)
  expect_identical(.store_format(file.path(d, "data"), mf), "feather")

  # files present -> they decide, whatever the manifest or option says
  writeLines("x", file.path(d, "data", "part-0.csv"))
  expect_identical(.store_format(file.path(d, "data"), mf), "csv")

  # `arrow`/`ipc` spellings normalise onto feather
  yaml::write_yaml(list(format = "ipc"), mf)
  unlink(file.path(d, "data", "part-0.csv"))
  expect_identical(.store_format(file.path(d, "data"), mf), "feather")
})

test_that("en_open_dataset verifies a declared format against the files", {
  d <- file.path(tempdir(), "ax_od"); unlink(d, recursive = TRUE)
  dir.create(d, recursive = TRUE)
  arrow::write_dataset(data.frame(a = 1:2), d, format = "feather")
  expect_s3_class(en_open_dataset(d, format = "feather"), "Dataset")
  # reading feather files as parquet used to fail deep inside arrow
  expect_error(en_open_dataset(d, format = "parquet"), "recorded format")
  expect_error(en_open_dataset(d, format = "nonsense"), "Unknown dataset format")

  # an EMPTY directory is a legitimate empty dataset, not an error: a slot with
  # no rows writes no files at all
  e <- file.path(tempdir(), "ax_od_empty"); unlink(e, recursive = TRUE)
  dir.create(e, recursive = TRUE)
  expect_error(en_open_dataset(e), NA)
})

test_that("an interpolation write-back keeps the store's codec", {
  # REGRESSION: the write-back used to recognise only parquet-or-csv, so on a
  # feather store it wrote CSV beside the .arrow files -- and a directory that
  # mixes codecs is not a readable dataset.
  ax_local_opts()
  skip_if_not(dir.exists(testthat::test_path("fixtures")), "fixtures missing")
  tm <- .ax_find("data-raw/testing-models.R")
  skip_if(is.null(tm), "fixtures/testing-models.R not available")
  source(tm, local = TRUE)

  set_storage_format("feather")
  store <- file.path(tempdir(), "ax_wb"); unlink(store, recursive = TRUE)
  on.exit(unlink(store, recursive = TRUE), add = TRUE)
  sc <- suppressWarnings(suppressMessages(
    interpolate_model(tm_core(), name = "ax_wb", ondisk = TRUE, path = store,
                      overwrite = TRUE)))

  pdirs <- list.dirs(file.path(store, "modInp", "parameters"),
                     recursive = FALSE)
  expect_gt(length(pdirs), 0)
  exts <- unique(unlist(lapply(pdirs, function(p)
    tools::file_ext(list.files(file.path(p, "data"))))))
  expect_identical(exts, "arrow")          # storage_format honoured, not csv

  # every parameter still reads (a mixed directory would stop() here)
  md <- .materialize_modInp(sc)
  expect_true(all(vapply(md@modInp@parameters,
                         function(p) !is.null(get_data_slot(p)), logical(1))))
})
