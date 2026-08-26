# Content-addressed dataset store (R/dataset_store.R): tables as parquet,
# objects (Geoscale maps) as rds, functional datasets as recorded calls with
# a materialized snapshot. The fourth storage tier next to models/,
# repositories/, scenarios/.

ds_root <- function(...) {
  gsub("[\\/]+", "/", file.path(tempdir(), "dataset-store-suite", ...))
}

ds_local <- function(env = parent.frame()) {
  old_dp <- set_datasets_path(ds_root("datasets"))
  old_rf <- set_registry_file(ds_root("reg.csv"))
  expr <- bquote({
    set_datasets_path(.(old_dp))
    set_registry_file(.(old_rf))
  })
  do.call(on.exit, list(expr, add = TRUE), envir = env)
  invisible(NULL)
}

ds_table <- function(scale = 1) {
  data.frame(
    region = rep(c("R1", "R2"), each = 4L),
    year = rep(2025L, 8L),
    timeslice = rep(sprintf("t%02d", 1:4), 2L),
    wval = scale * c(0.1, 0.5, 0.8, 0.3, 0.2, 0.6, 0.7, 0.2),
    stringsAsFactors = FALSE)
}

test_that("dataset_hash is content-based and attribute-blind", {
  d1 <- ds_table()
  d2 <- data.table::as.data.table(ds_table())
  data.table::setindexv(d2, "region")   # session noise
  expect_false(identical(dataset_hash(d1), dataset_hash(ds_table(2))))
  # NOTE: data.frame vs data.table wrap differ by class attr — same wrapper
  # must be compared; the store records the wrapper in payload_class
  expect_identical(dataset_hash(data.table::as.data.table(ds_table())),
                   dataset_hash(d2))
})

test_that("a table round-trips through the store with column classes intact", {
  ds_local()
  info <- save_dataset(ds_table(), "wtest", verbose = FALSE)
  expect_identical(info$kind, "table")
  expect_true(dir.exists(fp(info$path, "data")))

  # identical content again is a no-op
  expect_message(save_dataset(ds_table(), "wtest"), "already in the store")

  d <- load_dataset("wtest", verbose = FALSE)
  expect_identical(class(d$year), "integer")   # parquet round-trip pinned
  expect_identical(d[order(d$region, d$timeslice), ],
                   ds_table()[order(ds_table()$region,
                                    ds_table()$timeslice), ],
                   ignore_attr = TRUE)
  expect_identical(dataset_hash(d), info$hash)

  # name@hash8 and path= addressing
  d2 <- load_dataset(paste0("wtest@", substr(info$hash, 1, 8)),
                     verbose = FALSE)
  expect_identical(dataset_hash(d2), info$hash)
  d3 <- load_dataset(info$path, verbose = FALSE)
  expect_identical(dataset_hash(d3), info$hash)

  # changed content is a SECOND version side by side
  info2 <- save_dataset(ds_table(2), "wtest", verbose = FALSE)
  expect_false(identical(info2$path, info$path))
  expect_length(list.dirs(ds_root("datasets"), recursive = FALSE), 2L)
})

test_that("a non-tabular object (Geoscale map) stores as rds", {
  skip_if_not_installed("geoscales")
  ds_local()
  gs <- utopia_geoscale()
  info <- save_dataset(gs, "utopia_map", verbose = FALSE)
  expect_identical(info$kind, "object")
  expect_true(file.exists(fp(info$path, "payload.rds")))
  gs2 <- load_dataset("utopia_map", verbose = FALSE)
  expect_identical(dataset_hash(gs2), info$hash)
})

test_that("missing datasets error loudly, naming the remedies", {
  ds_local()
  expect_error(load_dataset("nope", verbose = FALSE), "refresh_registry")
})

test_that("the registry indexes datasets and refresh_registry rescans them", {
  ds_local()
  unlink(ds_root(), recursive = TRUE)
  info <- save_dataset(ds_table(), "wreg", verbose = FALSE)
  reg <- load_registry()
  row <- find_registry(reg, type = "dataset", name = "wreg")
  expect_identical(nrow(row), 1L)
  expect_identical(row$hash, info$hash)

  # rebuild from disk alone
  unlink(ds_root("reg.csv"))
  reg2 <- refresh_registry(root = ds_root(), file = ds_root("reg.csv"),
                           write = FALSE)
  expect_identical(find_registry(reg2, type = "dataset")$name, "wreg")
})

test_that("functional dataset: snapshot identity, drift check, call-hash mode", {
  ds_local()
  args <- list(regions = c("R1", "R2"), calendar = "s4_h24")
  info <- save_dataset(name = "utp", fun = "energyRt::utopia_profiles",
                       args = args, verbose = FALSE)
  expect_identical(info$kind, "function")
  mf <- yaml::read_yaml(fp(info$path, "dataset.yml"))
  expect_identical(mf$hash_of, "content")
  expect_true(isTRUE(mf$snapshot))
  expect_identical(mf$fun, "energyRt::utopia_profiles")

  # snapshot equals a fresh evaluation, and identity is the RESULT hash
  snap <- load_dataset("utp", verbose = FALSE)
  fresh <- load_dataset("utp", evaluate = TRUE, verbose = FALSE)
  expect_identical(dataset_hash(snap), info$hash)
  expect_identical(dataset_hash(fresh), info$hash)

  # a tampered result_hash warns on evaluate, errors under strict
  mf$result_hash <- "deadbeef"
  yaml::write_yaml(mf, fp(info$path, "dataset.yml"))
  expect_warning(load_dataset("utp", evaluate = TRUE, verbose = FALSE),
                 "no longer reproduces")
  expect_error(load_dataset("utp", evaluate = TRUE, strict = TRUE,
                            verbose = FALSE),
               "no longer reproduces")

  # materialize = FALSE records the call only and hashes the call
  info2 <- save_dataset(name = "utp_call", fun = "energyRt::utopia_profiles",
                        args = args, materialize = FALSE, verbose = FALSE)
  mf2 <- yaml::read_yaml(fp(info2$path, "dataset.yml"))
  expect_identical(mf2$hash_of, "call")
  expect_false(isTRUE(mf2$snapshot))
  expect_false(identical(info2$hash, info$hash))
  ev <- load_dataset("utp_call", verbose = FALSE)
  expect_identical(dataset_hash(ev), info$hash)   # evaluates to same content

  # a bad generator spec is rejected up front
  expect_error(save_dataset(name = "bad", fun = "no_namespace_fun"),
               "namespaced")
})
