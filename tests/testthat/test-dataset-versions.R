# The dataset store keeps versions by content under one name: the manifest
# names the current version and lists every version present; saving other
# content under a name adds a version, nothing is overwritten; a scenario
# or a caller pins a version by its hash; the references table records who
# points at which version.

dv_root <- function(...) {
  gsub("[\\/]+", "/", file.path(tempdir(), "dataset-versions-suite", ...))
}

dv_local <- function(env = parent.frame()) {
  old_dp <- set_datasets_path(dv_root("datasets"))
  old_rf <- set_registry_file(dv_root("reg.csv"))
  expr <- bquote({
    set_datasets_path(.(old_dp))
    set_registry_file(.(old_rf))
  })
  do.call(on.exit, list(expr, add = TRUE), envir = env)
  invisible(NULL)
}

dv_table <- function(scale = 1) {
  data.frame(
    region = rep(c("R1", "R2"), each = 4L),
    year = rep(2025L, 8L),
    timeslice = rep(sprintf("t%02d", 1:4), 2L),
    wval = scale * c(0.1, 0.5, 0.8, 0.3, 0.2, 0.6, 0.7, 0.2),
    stringsAsFactors = FALSE)
}

sorted <- function(d) {
  d <- as.data.frame(d)
  d[order(d$region, d$timeslice), ]
}

test_that("a version lives under the name as parquet parts, and the manifest lists it", {
  dv_local()
  info <- save_dataset(dv_table(), "wver", verbose = FALSE)
  vdir <- fp(info$path, info$hash)
  expect_true(dir.exists(vdir))
  expect_true(length(list.files(vdir, pattern = "[.](parquet|feather|arrow)$")) > 0L)
  expect_false(dir.exists(fp(info$path, "data")))
  mf <- yaml::read_yaml(fp(info$path, "dataset.yml"))
  expect_identical(mf$hash, info$hash)
  expect_identical(names(mf$versions), info$hash)
  expect_identical(mf$versions[[info$hash]]$kind, "table")
  d <- load_dataset("wver", verbose = FALSE)
  expect_identical(sorted(d), sorted(dv_table()), ignore_attr = TRUE)
})

test_that("other content under the same name adds a version and moves the current marker", {
  dv_local()
  v1 <- save_dataset(dv_table(5), "wver2", verbose = FALSE)
  v2 <- save_dataset(dv_table(6), "wver2", verbose = FALSE)
  expect_false(identical(v1$hash, v2$hash))
  expect_true(dir.exists(fp(v1$path, v1$hash)))
  expect_true(dir.exists(fp(v2$path, v2$hash)))
  mf <- yaml::read_yaml(fp(v2$path, "dataset.yml"))
  expect_identical(mf$hash, v2$hash)
  expect_setequal(names(mf$versions), c(v1$hash, v2$hash))
  # the current version by name, the older one by hash (full or prefix)
  expect_identical(sorted(load_dataset("wver2", verbose = FALSE)), sorted(dv_table(6)),
                   ignore_attr = TRUE)
  expect_identical(sorted(load_dataset("wver2", hash = v1$hash, verbose = FALSE)),
                   sorted(dv_table(5)), ignore_attr = TRUE)
  expect_identical(sorted(load_dataset(paste0("wver2@", substr(v1$hash, 1, 8)), verbose = FALSE)),
                   sorted(dv_table(5)), ignore_attr = TRUE)
  # one registry row per version, both found by hash
  reg <- .registry_open()
  rows <- find_in_registry(reg, type = "dataset", name = "wver2")
  expect_equal(nrow(rows), 2L)
  expect_setequal(rows$hash, c(v1$hash, v2$hash))
  expect_identical(.dataset_store_find_hash(v1$hash)$name, "wver2")
  expect_identical(.dataset_store_find_hash(v2$hash)$name, "wver2")
  # saving the older content again makes it current without a write
  before <- file.info(list.files(fp(v1$path, v1$hash), full.names = TRUE))$mtime
  v3 <- save_dataset(dv_table(5), "wver2", verbose = FALSE)
  expect_identical(v3$hash, v1$hash)
  expect_identical(yaml::read_yaml(fp(v1$path, "dataset.yml"))$hash, v1$hash)
  expect_identical(file.info(list.files(fp(v1$path, v1$hash), full.names = TRUE))$mtime, before)
})

test_that("a legacy entry with data/ under the name reads as its single version", {
  dv_local()
  root <- get_datasets_path()
  path <- fp(root, "wlegacy")
  x <- dv_table(3)
  h <- dataset_hash(x)
  data2disk(x, path = fp(path, "data"), format = "parquet")
  yaml::write_yaml(list(layout = 3L, class = "dataset", name = "wlegacy", hash = h,
                        hash_of = "content", kind = "table", format = "parquet",
                        payload_class = list("data.frame"), dim = as.list(dim(x)),
                        cols = lapply(as.list(x), function(col) class(col)[1]),
                        created = .registry_now(), updated = .registry_now()),
                   fp(path, "dataset.yml"))
  expect_identical(sorted(load_dataset("wlegacy", verbose = FALSE)), sorted(x), ignore_attr = TRUE)
  expect_identical(sorted(load_dataset("wlegacy", hash = h, verbose = FALSE)), sorted(x), ignore_attr = TRUE)
  expect_identical(.dataset_store_find_hash(h)$name, "wlegacy")
  # a new version beside it leaves the legacy payload where it is
  v2 <- save_dataset(dv_table(4), "wlegacy", verbose = FALSE)
  expect_true(dir.exists(fp(path, "data")))
  expect_true(dir.exists(fp(path, v2$hash)))
  expect_identical(sorted(load_dataset("wlegacy", hash = h, verbose = FALSE)), sorted(x), ignore_attr = TRUE)
  expect_identical(sorted(load_dataset("wlegacy", verbose = FALSE)), sorted(dv_table(4)), ignore_attr = TRUE)
})

test_that("the references table records who points at which version", {
  dv_local()
  f <- .refs_file()
  unlink(f)
  expect_equal(nrow(.refs_open(f)), 0L)
  rows <- data.frame(object = c("pWeather", "pDemand"), slot = "data",
                     name = c("pWeather", "pDemand"), hash = c("aaaa", "bbbb"),
                     path = c("datasets/pWeather", "datasets/pDemand"),
                     stringsAsFactors = FALSE)
  .refs_replace("scenA", rows, file = f)
  .refs_replace("scenB", rows[1, ], file = f)
  expect_equal(nrow(.refs_open(f)), 3L)
  dep <- .refs_dependents("pWeather", "aa", file = f)
  expect_setequal(dep$scenario, c("scenA", "scenB"))
  expect_equal(nrow(.refs_dependents("pDemand", "bbbb", file = f)), 1L)
  # replacing a scenario's references drops its old rows
  .refs_replace("scenA", rows[2, ], file = f)
  expect_setequal(.refs_dependents("pWeather", file = f)$scenario, "scenB")
  .refs_drop("scenB", file = f)
  expect_equal(nrow(.refs_dependents("pWeather", file = f)), 0L)
  expect_equal(nrow(.refs_open(f)), 1L)
})
