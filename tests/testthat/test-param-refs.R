# Interpolated tables and their references (R/save-params.R): eligibility,
# hashing a modInp's large tables, references to a sealed base scenario's
# tables, resolving references back (and those into the dataset store that
# older scenarios hold), the data directory a reader uses, and clearing a
# reference before a write.

test_that("only large numpar, bounds and map tables are referenced", {
  p <- pr_param("pTechAf", pr_af())
  expect_false(.param_ref_eligible(p, 8L))
  withr::local_options(energyRt.param_ref_min_rows = 1)
  expect_true(.param_ref_eligible(p, 8L))
  expect_false(.param_ref_eligible(p, NA_integer_))
  # a source table follows the on-disk threshold
  w <- pr_param("pWeather", pr_weather())
  withr::local_options(energyRt.ondisk_min_rows = 1e9)
  expect_false(.param_ref_eligible(w, 8L))
  withr::local_options(energyRt.ondisk_min_rows = 1)
  expect_true(.param_ref_eligible(w, 8L))
})

test_that("large tables are hashed and kept; without a sealed base none is referenced", {
  pr_local()
  withr::local_options(energyRt.param_ref_min_rows = 1, energyRt.ondisk_min_rows = 1)
  scen <- pr_scen(pWeather = pr_param("pWeather", pr_weather()),
                  pTechAf = pr_param("pTechAf", pr_af()))
  th <- .thin_param_tables(scen@modInp, scen, format = "parquet", verbose = FALSE)
  expect_length(th$entries, 0L)
  expect_setequal(names(th$tables), c("pWeather", "pTechAf"))
  expect_identical(th$tables$pWeather$hash,
                   dataset_hash(force_cols_classes(pr_param("pWeather", pr_weather())@data)))
  expect_null(th$modInp@parameters[["pWeather"]]@misc$param_ref)
  expect_false(dir.exists(get_datasets_path()))
})

test_that("dataset references resolve at load, a local table wins, a missing derived version errors", {
  pr_local()
  scen <- pr_scen(pTechAf = pr_param("pTechAf", pr_af(2)))
  # a reference into the dataset store, as scenarios saved before base
  # references hold
  info <- save_dataset(force_cols_classes(pr_af(2)), "pTechAf", verbose = FALSE)
  p <- scen@modInp@parameters[["pTechAf"]]
  p@misc$param_ref <- list(name = "pTechAf", hash = info$hash, kind = "table",
                           source = "ref", path = .registry_rel_path(info$path),
                           origin = list(kind = "derived"))
  # as a loaded scenario: no table, a local path that holds nothing
  p@data <- p@data[0, ]
  p@misc$path <- pr_root("scen", "modInp", "parameters", "pTechAf")
  scen@modInp@parameters[["pTechAf"]] <- p
  out <- .resolve_param_refs(scen, verbose = FALSE)
  expect_s4_class(out, "scenario")
  r <- out@modInp@parameters[["pTechAf"]]@misc$param_ref
  expect_true(dir.exists(r$resolved))
  expect_identical(.en_param_data_dir(out@modInp@parameters[["pTechAf"]]), r$resolved)
  # a table written locally after the reference wins and the reference goes
  local <- file.path(p@misc$path, "data")
  data2disk(data.table::as.data.table(pr_af(9)), path = local, format = "parquet")
  out2 <- .resolve_param_refs(scen, verbose = FALSE)
  expect_null(out2@modInp@parameters[["pTechAf"]]@misc$param_ref)
  expect_identical(.en_param_data_dir(out2@modInp@parameters[["pTechAf"]]), local)
  unlink(p@misc$path, recursive = TRUE)
  # the version gone: a derived table cannot be rebuilt
  unlink(file.path(get_datasets_path(), "pTechAf"), recursive = TRUE)
  unlink(get_registry_file())
  expect_error(.resolve_param_refs(scen, verbose = FALSE), class = "energyRt_param_ref_missing")
  expect_false(.resolve_param_refs(scen, ignore_errors = TRUE, verbose = FALSE))
  # clearing a reference before a write
  expect_null(.param_ref_clear(p)@misc$param_ref)
})

# --- a scenario on a sealed base references the base's tables ---------------

# REF saved and sealed, CAP built on it with a user cost term and saved
pr_ref_cap <- function(tag) {
  ref <- suppressMessages(interpolate_model(pr_model(), name = paste0(tag, "_ref"),
                                            verbose = FALSE))
  ref <- suppressMessages(save_scenario(ref, verbose = FALSE))
  suppressMessages(seal_scenario(ref, verbose = FALSE))
  fee <- newCosts(paste0(tag, "FEE"), variable = "vTechAct", mult = 2,
                  subset = list(tech = "WIND"))
  cap <- suppressMessages(interpolate_model(pr_model(), fee, name = paste0(tag, "_cap"),
                                            base = ref, verbose = FALSE))
  cap <- suppressMessages(save_scenario(cap, verbose = FALSE))
  list(ref = ref, cap = cap)
}

test_that("a scenario on a sealed base references its identical tables and loads them", {
  pr_local_model()
  unlink(pr_root(), recursive = TRUE)
  withr::local_options(energyRt.param_ref_min_rows = 1, energyRt.ondisk_min_rows = 1)
  rc <- pr_ref_cap("prs")
  ref <- rc$ref
  cap <- rc$cap
  # the base keeps its tables and records their hashes
  mr <- yaml::read_yaml(fp(ref@path, "scenario.yml"))
  expect_length(Filter(function(e) identical(e$via, "parameter"), mr$datasets), 0L)
  expect_true(length(mr$tables) > 0L)
  for (nm in names(mr$tables)) {
    expect_true(dir.exists(fp(ref@path, "modInp", "parameters", nm, "data")), info = nm)
  }
  expect_false(dir.exists(get_datasets_path()))

  # CAP references every table identical to the base's and stores none of them
  mc <- yaml::read_yaml(fp(cap@path, "scenario.yml"))
  pe <- Filter(function(e) identical(e$source, "scenario"), mc$datasets)
  expect_true(length(pe) > 0L)
  for (e in pe) {
    expect_identical(e$scenario, ref@name)
    expect_identical(e$hash, mr$tables[[e$object]]$hash, info = e$object)
    expect_false(dir.exists(fp(cap@path, "modInp", "parameters", e$object, "data")),
                 info = e$object)
  }
  # the references table knows them; the base lists CAP as a dependent and
  # cannot be unsealed while CAP references it
  refs <- .refs_open()
  expect_true(all(vapply(pe, function(e) e$object, "") %in% refs$object))
  dep <- list_dependents(ref)
  expect_identical(dep$name, cap@name)
  expect_error(unseal_scenario(ref, verbose = FALSE), cap@name)

  # loading resolves the references into the base's folder; every table reads back identical
  ld <- suppressMessages(load_scenario(cap@path, env = NULL, verbose = FALSE))
  for (e in pe) {
    p <- ld@modInp@parameters[[e$object]]
    expect_true(startsWith(p@misc$param_ref$resolved, gsub("[\\/]+", "/", ref@path)),
                info = e$object)
    d <- get_data_slot(p)
    expect_identical(dataset_hash(force_cols_classes(d)), e$hash, info = e$object)
  }

  # copy-on-write: a changed table drops its reference and is written locally
  w <- get_data_slot(ld@modInp@parameters[["pWeather"]])
  w$value <- w$value * 2
  ld2 <- update_parameter(ld, "pWeather", w)
  expect_null(ld2@modInp@parameters[["pWeather"]]@misc$param_ref)
  saved3 <- suppressMessages(save_scenario(ld2, overwrite = TRUE, verbose = FALSE))
  mf3 <- yaml::read_yaml(fp(saved3@path, "scenario.yml"))
  expect_length(Filter(function(e) identical(e$object, "pWeather"), mf3$datasets), 0L)
  expect_false(identical(mf3$tables$pWeather$hash, mr$tables$pWeather$hash))
  expect_true(dir.exists(fp(saved3@path, "modInp", "parameters", "pWeather", "data")))
})

test_that("the mm import reads referenced tables in place and builds the same LP", {
  skip_if_not_installed("multimod")
  skip_if_not_installed("highs")
  pr_local_model()
  unlink(pr_root(), recursive = TRUE)
  withr::local_options(energyRt.param_ref_min_rows = 1, energyRt.ondisk_min_rows = 1,
                       energyRt.mm_keep_wildcards = TRUE)
  saved <- pr_ref_cap("prs_mm")$cap
  ld <- suppressMessages(load_scenario(saved@path, env = NULL, verbose = FALSE))
  full <- suppressMessages(suppressWarnings(multimod_from_energyRt(ld, inMemory = TRUE, verbose = FALSE)))
  lazy <- suppressMessages(suppressWarnings(multimod_from_energyRt(ld, inMemory = FALSE, verbose = FALSE)))
  # referenced parameters carry the store directory and the hash, no table
  pw <- lazy$parameters[["pWeather"]]
  expect_identical(pw$misc$ref$hash, ld@modInp@parameters[["pWeather"]]@misc$param_ref$hash)
  expect_true(dir.exists(pw$misc$path))
  expect_equal(NROW(pw$data), 0L)
  # multimod reads the same rows
  a <- multimod::get_data(full, "pWeather", type = "parameter")
  b <- multimod::get_data(lazy, "pWeather", type = "parameter")
  expect_identical(dataset_hash(force_cols_classes(as.data.frame(b))),
                   dataset_hash(force_cols_classes(as.data.frame(a))))
  # and the same LP
  lf <- multimod::model_to_lp(full)
  ll <- multimod::model_to_lp(lazy)
  expect_identical(dim(ll$A), dim(lf$A))
  expect_equal(sum(abs(ll$A@x)), sum(abs(lf$A@x)))
  sf <- multimod::solve_highs(full, attach = FALSE)
  sl <- multimod::solve_highs(lazy, attach = FALSE)
  expect_equal(sl$objective, sf$objective, tolerance = 1e-9)
})

test_that("tables persisted on disk or in a base scenario are shed from memory and read back lazily", {
  pr_local_model()
  unlink(pr_root(), recursive = TRUE)
  withr::local_options(energyRt.param_ref_min_rows = 1, energyRt.ondisk_min_rows = 1)
  saved <- pr_ref_cap("prs_shed")$cap
  ld <- suppressMessages(load_scenario(saved@path, env = NULL, verbose = FALSE))
  # a table brought into memory while on disk
  p <- ld@modInp@parameters[["pWeather"]]
  full <- get_data_slot(p)
  p@data <- data.table::as.data.table(full)
  ld@modInp@parameters[["pWeather"]] <- p
  expect_equal(nrow(ld@modInp@parameters[["pWeather"]]@data), nrow(full))
  shed <- .shed_persisted_tables(ld)
  expect_equal(nrow(shed@modInp@parameters[["pWeather"]]@data), 0L)
  expect_identical(dataset_hash(force_cols_classes(get_data_slot(shed@modInp@parameters[["pWeather"]]))),
                   dataset_hash(force_cols_classes(full)))
  # a referenced table read back and flagged as in memory: shed, and its
  # readers resolve the reference again
  p@misc$inMemory <- TRUE
  ld@modInp@parameters[["pWeather"]] <- p
  expect_false(isOnDisk(ld@modInp@parameters[["pWeather"]]))
  shed <- .shed_persisted_tables(ld)
  ps <- shed@modInp@parameters[["pWeather"]]
  expect_equal(nrow(ps@data), 0L)
  expect_true(isOnDisk(ps))
  expect_identical(dataset_hash(force_cols_classes(get_data_slot(ps))),
                   dataset_hash(force_cols_classes(full)))
  # the scenario shell written over a stored scenario carries no such table
  suppressMessages(save_scenario(ld, verbose = FALSE))
  e <- new.env()
  load(fp(saved@path, "scen.RData"), envir = e)
  sh <- get(ls(e)[1], envir = e)
  expect_equal(NROW(sh@modInp@parameters[["pWeather"]]@data), 0L)
  # a table that is nowhere else stays
  mem <- pr_scen(pTechAf = pr_param("pTechAf", pr_af()))
  expect_equal(nrow(.shed_persisted_tables(mem)@modInp@parameters[["pTechAf"]]@data), 8L)
})
