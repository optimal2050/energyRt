# interp_mod(ondisk = TRUE) is the default storage mode but was historically only
# exercised in-memory. This test proves an on-disk build is equivalent to an
# in-memory one: identical parameter data (after the parquet/csv round-trip is
# class-normalised) and, where a GLPK binary is available, an identical objective.

test_that("on-disk interpolation equals in-memory (data + solve)", {
  # Reuse the guard's test-model builders (data-raw is present in the source tree
  # but not in an installed package, so skip cleanly when absent).
  tm_file <- NULL
  for (cand in c("data-raw/testing-models.R",
                 file.path("..", "..", "data-raw", "testing-models.R"))) {
    if (file.exists(cand)) { tm_file <- cand; break }
  }
  skip_if(is.null(tm_file), "data-raw/testing-models.R not available")
  source(tm_file, local = TRUE)
  mod <- tm_weather()

  mem <- suppressWarnings(suppressMessages(
    interp_mod(mod, name = "ondisk_mem", ondisk = FALSE,
               fold = c("region", "slice", "year", "comm", "tech", "stg", "trade"),
               sparse = TRUE)))

  store <- file.path(tempdir(), "ondisk_store")
  unlink(store, recursive = TRUE)
  disk <- suppressWarnings(suppressMessages(
    interp_mod(mod, name = "ondisk_disk", ondisk = TRUE, path = store,
               fold = c("region", "slice", "year", "comm", "tech", "stg", "trade"),
               sparse = TRUE, overwrite = TRUE)))
  on.exit(unlink(store, recursive = TRUE), add = TRUE)

  expect_true(isOnDisk(disk@modInp))
  expect_false(isOnDisk(mem@modInp))

  # --- parameter-data identity (no solver needed) ---------------------------- #
  md <- .materialize_modInp(disk)
  sorted <- function(p) {
    d <- as.data.frame(get_data_slot(p))
    if (is.null(d) || nrow(d) == 0) return(NULL)   # 0-row params carry no data
    d[do.call(order, lapply(d, as.character)), , drop = FALSE]
  }
  divergent <- character(0)
  for (nm in names(mem@modInp@parameters)) {
    a <- sorted(mem@modInp@parameters[[nm]])
    b <- sorted(md@modInp@parameters[[nm]])
    if (!isTRUE(all.equal(a, b, check.attributes = FALSE))) {
      divergent <- c(divergent, nm)
    }
  }
  expect_identical(divergent, character(0))

  # --- solve identity (requires a GLPK binary) ------------------------------- #
  skip_if(is.null(get_glpk_path()) || !nzchar(get_glpk_path()),
          "GLPK path not configured")
  getobj <- function(s) tryCatch(s@modOut@variables$vObjective$value[1],
                                 error = function(e) NA_real_)
  solve1 <- function(s, tag) {
    td <- file.path(tempdir(), paste0("ondisk_solve_", tag))
    unlink(td, recursive = TRUE)
    on.exit(unlink(td, recursive = TRUE), add = TRUE)
    suppressWarnings(suppressMessages(
      solve_scen(s, name = tag, solver = "GLPK", tmp.dir = td, force = TRUE)))
  }
  om <- getobj(solve1(mem, "mem"))
  od <- getobj(solve1(disk, "disk"))
  # The invariant is storage-independence: the on-disk build must solve to the
  # SAME objective as the in-memory build (the absolute value is model-specific).
  expect_false(is.na(od))
  expect_equal(om, od)
})
