# Interpolated tables as references into the dataset store (R/store-params.R):
# eligibility, thinning a modInp into references, resolving them back, the
# data directory a reader uses, and clearing a reference before a write.

pr_root <- function(...) {
  gsub("[\\/]+", "/", file.path(tempdir(), "param-refs-suite", ...))
}

pr_local <- function(env = parent.frame()) {
  old_dp <- set_datasets_path(pr_root("datasets"))
  old_rf <- set_registry_file(pr_root("reg.csv"))
  expr <- bquote({
    set_datasets_path(.(old_dp))
    set_registry_file(.(old_rf))
  })
  do.call(on.exit, list(expr, add = TRUE), envir = env)
  invisible(NULL)
}

pr_param <- function(name, data) {
  p <- new("modInp")@parameters[[name]]
  stopifnot(!is.null(p))
  p@data <- data.table::as.data.table(data)
  p
}

pr_weather <- function(scale = 1) {
  data.frame(weather = "WSOL", region = rep(c("R1", "R2"), each = 4L),
             year = 2025L, timeslice = rep(sprintf("t%02d", 1:4), 2L),
             value = scale * c(0.1, 0.5, 0.8, 0.3, 0.2, 0.6, 0.7, 0.2),
             stringsAsFactors = FALSE)
}

pr_af <- function(scale = 1) {
  data.frame(tech = "TPV", region = rep(c("R1", "R2"), each = 4L), year = 2025L,
             timeslice = rep(sprintf("t%02d", 1:4), 2L), type = "up",
             value = scale * c(0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2),
             stringsAsFactors = FALSE)
}

pr_scen <- function(...) {
  mi <- new("modInp")
  mi@parameters <- list(...)
  mi <- mark_inMemory(mi)
  scen <- new("scenario", name = "prtest")
  scen@modInp <- mi
  scen@status$sparse <- TRUE
  scen
}

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

test_that("a modInp's large tables become versions in the store and references on the parameters", {
  pr_local()
  withr::local_options(energyRt.param_ref_min_rows = 1, energyRt.ondisk_min_rows = 1)
  scen <- pr_scen(pWeather = pr_param("pWeather", pr_weather()),
                  pTechAf = pr_param("pTechAf", pr_af()))
  th <- .thin_param_tables(scen@modInp, scen, format = "parquet", verbose = FALSE)
  expect_length(th$entries, 2L)
  for (e in th$entries) {
    expect_identical(e$slot, "data")
    expect_identical(e$via, "parameter")
    expect_identical(e$source, "ref")
    expect_true(dir.exists(file.path(get_datasets_path(), e$name, e$hash)))
  }
  w <- th$modInp@parameters[["pWeather"]]
  ref <- w@misc$param_ref
  expect_identical(ref$name, "pWeather")
  expect_identical(ref$hash, dataset_hash(force_cols_classes(w@data)))
  expect_identical(ref$origin$kind, "source")
  expect_identical(ref$origin$class, "weather")
  expect_identical(th$modInp@parameters[["pTechAf"]]@misc$param_ref$origin$kind, "derived")
  # the table is still on the parameter: the caller decides what to drop
  expect_equal(nrow(w@data), 8L)
  # the reference resolves to the version directory holding the rows
  d <- .param_ref_data_dir(w)
  expect_true(dir.exists(d))
  back <- as.data.frame(en_open_dataset(d) |> dplyr::collect())
  expect_identical(back[order(back$region, back$timeslice), "value"],
                   pr_weather()[order(pr_weather()$region, pr_weather()$timeslice), "value"])
  # the same content again is found, not stored twice
  scen2 <- pr_scen(pWeather = pr_param("pWeather", pr_weather()))
  th2 <- .thin_param_tables(scen2@modInp, scen2, format = "parquet", verbose = FALSE)
  expect_identical(th2$modInp@parameters[["pWeather"]]@misc$param_ref$hash, ref$hash)
  mf <- yaml::read_yaml(file.path(get_datasets_path(), "pWeather", "dataset.yml"))
  expect_length(mf$versions, 1L)
  # other content under the same parameter adds a version
  scen3 <- pr_scen(pWeather = pr_param("pWeather", pr_weather(3)))
  th3 <- .thin_param_tables(scen3@modInp, scen3, format = "parquet", verbose = FALSE)
  mf <- yaml::read_yaml(file.path(get_datasets_path(), "pWeather", "dataset.yml"))
  expect_length(mf$versions, 2L)
  expect_false(identical(th3$modInp@parameters[["pWeather"]]@misc$param_ref$hash, ref$hash))
  # a referenced parameter is left alone on a second pass: its entry is
  # listed again, nothing is hashed or stored
  th4 <- .thin_param_tables(th$modInp, scen, format = "parquet", verbose = FALSE)
  expect_length(th4$entries, 2L)
  expect_setequal(vapply(th4$entries, function(e) e$hash, ""),
                  vapply(th$entries, function(e) e$hash, ""))
})

test_that("references resolve at load, a local table wins, a missing derived version errors", {
  pr_local()
  withr::local_options(energyRt.param_ref_min_rows = 1)
  scen <- pr_scen(pTechAf = pr_param("pTechAf", pr_af(2)))
  th <- .thin_param_tables(scen@modInp, scen, format = "parquet", verbose = FALSE)
  scen@modInp <- th$modInp
  p <- scen@modInp@parameters[["pTechAf"]]
  ref <- p@misc$param_ref
  # as a loaded scenario: no table, a local path that holds nothing
  p@data <- p@data[0, ]
  p@misc$param_ref$resolved <- NULL
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

# --- a scenario saved and loaded with its tables referenced -------------------

pr_slices <- sprintf("s%02d", 1:4)

pr_repo <- function(wvals = c(1, 0.5, 0.25, 0.2)) {
  newRepository(
    "prr",
    newCommodity("ELC", timeframe = "SL"),
    newWeather("WCF", timeframe = "SL",
               weather = data.frame(timeslice = pr_slices, wval = wvals)),
    newTechnology("WIND", output = list(comm = "ELC"),
                  weather = data.frame(weather = "WCF", waf.up = 1),
                  invcost = list(invcost = 1000), olife = list(olife = 30),
                  cap2act = 1),
    newDemand("DEM", commodity = "ELC",
              demand = data.frame(timeslice = pr_slices, demand = 10)))
}

pr_model <- function(name = "prm") {
  cal <- newCalendar(timetable = make_timetable(
    struct = list(ANNUAL = "ANNUAL", SL = pr_slices)), name = "prc")
  newModel(name, region = "R1", discount = 0, calendar = cal,
           horizon = newHorizon(2025), repo = pr_repo())
}

pr_local_model <- function(env = parent.frame()) {
  pr_local(env = env)
  old_mp <- set_models_path(pr_root("models"))
  old_sp <- set_scenarios_path(pr_root("scenarios"))
  old_rp <- set_repositories_path(pr_root("repositories"))
  expr <- bquote({
    set_models_path(.(old_mp))
    set_scenarios_path(.(old_sp))
    set_repositories_path(.(old_rp))
  })
  do.call(on.exit, list(expr, add = TRUE), envir = env)
  invisible(NULL)
}

test_that("a scenario's large tables are saved once in the store and load back through references", {
  pr_local_model()
  unlink(pr_root(), recursive = TRUE)
  withr::local_options(energyRt.param_ref_min_rows = 1, energyRt.ondisk_min_rows = 1)
  scen <- suppressMessages(interpolate_model(pr_model(), name = "prs",
                                             path = pr_root("scenarios", "prs")))
  before <- lapply(scen@modInp@parameters, function(p) {
    d <- get_data_slot(p); if (is.null(d) || !nrow(d)) NULL else dataset_hash(force_cols_classes(d))
  })
  before <- before[!vapply(before, is.null, logical(1))]
  saved <- suppressMessages(save_scenario(scen, verbose = FALSE))

  # the manifest references every referenced table; none is in the scenario's own store
  mf <- yaml::read_yaml(fp(saved@path, "scenario.yml"))
  pe <- Filter(function(e) identical(e$via, "parameter"), mf$datasets)
  expect_true(length(pe) > 0L)
  for (e in pe) {
    expect_identical(e$hash, before[[e$object]], info = e$object)
    expect_true(dir.exists(fp(get_datasets_path(), e$name, e$hash)), info = e$object)
    expect_false(dir.exists(fp(saved@path, "modInp", "parameters", e$object, "data")), info = e$object)
  }
  # the references table knows them
  refs <- .refs_open()
  expect_setequal(refs$object, vapply(pe, function(e) e$object, ""))
  dep <- .refs_dependents(pe[[1]]$name, pe[[1]]$hash)
  expect_equal(length(unique(dep$scenario)), 1L)

  # loading resolves the references; every table reads back identical
  ld <- suppressMessages(load_scenario(saved@path, env = NULL, verbose = FALSE))
  for (e in pe) {
    p <- ld@modInp@parameters[[e$object]]
    expect_true(dir.exists(p@misc$param_ref$resolved), info = e$object)
    d <- get_data_slot(p)
    expect_identical(dataset_hash(force_cols_classes(d)), e$hash, info = e$object)
  }
  # the same content saved again under another scenario shares the versions
  scen2 <- suppressMessages(interpolate_model(pr_model(), name = "prs2",
                                              path = pr_root("scenarios", "prs2")))
  saved2 <- suppressMessages(save_scenario(scen2, verbose = FALSE))
  mf2 <- yaml::read_yaml(fp(saved2@path, "scenario.yml"))
  pe2 <- Filter(function(e) identical(e$via, "parameter"), mf2$datasets)
  for (e in pe2) {
    mfd <- yaml::read_yaml(fp(get_datasets_path(), e$name, "dataset.yml"))
    expect_length(mfd$versions, 1L)
  }
  expect_equal(length(unique(.refs_dependents(pe[[1]]$name, pe[[1]]$hash)$scenario)), 2L)

  # copy-on-write: a changed table drops its reference and is written locally
  w <- get_data_slot(ld@modInp@parameters[["pWeather"]])
  w$value <- w$value * 2
  ld2 <- update_parameter(ld, "pWeather", w)
  expect_null(ld2@modInp@parameters[["pWeather"]]@misc$param_ref)
  saved3 <- suppressMessages(save_scenario(ld2, overwrite = TRUE, verbose = FALSE))
  mf3 <- yaml::read_yaml(fp(saved3@path, "scenario.yml"))
  pe3 <- Filter(function(e) identical(e$via, "parameter") && e$object == "pWeather", mf3$datasets)
  # the new content became a second version of pWeather, referenced again
  expect_length(pe3, 1L)
  expect_false(identical(pe3[[1]]$hash, before[["pWeather"]]))
  expect_length(yaml::read_yaml(fp(get_datasets_path(), "pWeather", "dataset.yml"))$versions, 2L)
})
