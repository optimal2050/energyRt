# Content-addressed model store + scenario model references + sourceCode
# dedup (R/model_store.R and the save/load_scenario changes; layout 3, S3).
# Fixtures from helper-stock-path.R.

ms_root <- function(...) {
  gsub("[\\/]+", "/", file.path(tempdir(), "model-store-suite", ...))
}

# point the model store + registry into tempdir for every test in this file;
# restores the previous options when the calling test frame exits
ms_local_store <- function(env = parent.frame()) {
  old_mp <- set_models_path(ms_root("models"))
  old_rf <- set_registry_file(ms_root("reg.csv"))
  expr <- bquote({
    set_models_path(.(old_mp))
    set_registry_file(.(old_rf))
  })
  do.call(on.exit, list(expr, add = TRUE), envir = env)
  invisible(NULL)
}

test_that("object_hash generalizes model_hash to any S4 object", {
  m <- sp_tech(c(100, 100, 100), optret = FALSE, name = "oh")
  expect_identical(object_hash(m), model_hash(m))
  expect_identical(object_hash(m@data[[1]]), repository_hash(m@data[[1]]))

  t1 <- newTechnology(
    "OHT", output = data.frame(comm = "ELC"),
    invcost = data.frame(invcost = 500), cap2act = 1)
  t2 <- t1
  t2@misc$scratch <- "noise"
  expect_identical(object_hash(t1), object_hash(t2))   # misc-blind
  t3 <- t1
  t3@invcost$invcost <- 600
  expect_false(identical(object_hash(t1), object_hash(t3)))  # content-sensitive
  expect_error(object_hash(list()), "isS4")
})

test_that("model_hash is stable across rebuilds and blind to @misc noise", {
  m1 <- sp_tech(c(100, 100, 100), optret = FALSE, name = "msh")
  m2 <- sp_tech(c(100, 100, 100), optret = FALSE, name = "msh")
  expect_identical(model_hash(m1), model_hash(m2))

  m3 <- m1
  m3@misc$scratch <- "noise"
  m3@data[[1]]@misc$scratch <- "more noise"
  expect_identical(model_hash(m3), model_hash(m1))

  # content changes change the hash
  m4 <- sp_tech(c(100, 100, 90), optret = FALSE, name = "msh")
  expect_false(identical(model_hash(m4), model_hash(m1)))
  # so does the name
  m5 <- sp_tech(c(100, 100, 100), optret = FALSE, name = "msh2")
  expect_false(identical(model_hash(m5), model_hash(m1)))
})

test_that("save_model / load_model round-trips; identical content is a no-op", {
  ms_local_store()
  mod <- sp_tech(c(100, 100, 100), optret = FALSE, name = "msm")
  h8 <- substr(model_hash(mod), 1, 8)

  saved <- save_model(mod, verbose = FALSE)
  store_dir <- ms_root("models", paste0("msm@", h8))
  expect_true(file.exists(file.path(store_dir, "mod.RData")))
  mf <- yaml::read_yaml(file.path(store_dir, "model.yml"))
  expect_identical(mf$name, "msm")
  expect_identical(substr(mf$hash, 1, 8), h8)

  # registry row
  reg <- load_registry()
  expect_identical(nrow(find_registry(reg, type = "model", name = "msm")), 1L)

  # re-saving identical content is a no-op with a message
  expect_message(save_model(mod), "already in the store")

  # load by name, by name@hash8, and by path
  back <- load_model("msm", verbose = FALSE)
  expect_s4_class(back, "model")
  expect_identical(back@name, "msm")
  expect_identical(sort(names(back@data)), sort(names(mod@data)))
  back2 <- load_model(paste0("msm@", h8), verbose = FALSE)
  expect_identical(back2@name, "msm")
  back3 <- load_model("ignored", path = store_dir, verbose = FALSE)
  expect_identical(back3@name, "msm")

  # a missing model errors with guidance
  expect_error(load_model("no-such-model", verbose = FALSE), "refresh_registry")
})

test_that("a stored model is referenced, not embedded; load resolves it", {
  skip_if_no_solver()
  ms_local_store()
  mod <- sp_tech(c(70, 70, 70), optret = FALSE, name = "msref")
  save_model(mod, verbose = FALSE)

  sc <- interpolate_model(mod, name = "msref", path = ms_root("scen-ref"))
  sol <- solve_scenario(sc)
  saved <- suppressMessages(save_scenario(sol, verbose = FALSE))

  mf <- yaml::read_yaml(file.path(saved@path, "scenario.yml"))
  expect_identical(mf$model$source, "ref")
  expect_identical(mf$model$name, "msref")
  expect_true(nzchar(mf$model$hash))

  # the SAVED copy holds a stub...
  e <- new.env()
  load(file.path(saved@path, "scen.RData"), envir = e)
  raw <- get(ls(e)[1], envir = e)
  expect_false(is.null(raw@model@misc$model_ref))
  expect_identical(length(raw@model@data), 0L)
  # ...while the returned object keeps the full model
  expect_gt(length(saved@model@data), 0L)

  # loading resolves the reference back to the full model
  back <- suppressMessages(load_scenario(saved@path, env = NULL,
                                         verbose = FALSE))
  expect_identical(back@model@name, "msref")
  expect_gt(length(back@model@data), 0L)

  # with the store and registry gone, loading fails loudly
  unlink(ms_root("models"), recursive = TRUE)
  unlink(ms_root("reg.csv"))
  expect_error(
    suppressMessages(load_scenario(saved@path, env = NULL, verbose = FALSE)),
    "model store")
})

test_that("embed_model = TRUE embeds even when the store has the model", {
  skip_if_no_solver()
  ms_local_store()
  mod <- sp_tech(c(40, 40, 40), optret = FALSE, name = "msemb")
  save_model(mod, verbose = FALSE)
  sc <- interpolate_model(mod, name = "msemb", path = ms_root("scen-emb"))
  sol <- solve_scenario(sc)
  saved <- suppressMessages(save_scenario(sol, embed_model = TRUE,
                                          verbose = FALSE))
  mf <- yaml::read_yaml(file.path(saved@path, "scenario.yml"))
  expect_identical(mf$model$source, "embedded")
  e <- new.env()
  load(file.path(saved@path, "scen.RData"), envir = e)
  raw <- get(ls(e)[1], envir = e)
  expect_null(raw@model@misc$model_ref)
  # loads without any store
  unlink(ms_root("models"), recursive = TRUE)
  back <- suppressMessages(load_scenario(saved@path, env = NULL,
                                         verbose = FALSE))
  expect_identical(back@model@name, "msemb")
})

test_that("embed_model = FALSE errors when the model is not stored", {
  skip_if_no_solver()
  ms_local_store()
  mod <- sp_tech(c(30, 30, 30), optret = FALSE, name = "msreq")
  sc <- interpolate_model(mod, name = "msreq", path = ms_root("scen-req"))
  sol <- solve_scenario(sc)
  expect_error(
    suppressMessages(save_scenario(sol, embed_model = FALSE, verbose = FALSE)),
    "save_model")
})

test_that("default sourceCode blocks are dropped on save and restored on load", {
  skip_if_no_solver()
  ms_local_store()
  mod <- sp_tech(c(20, 20, 20), optret = FALSE, name = "mssrc")
  sc <- interpolate_model(mod, name = "mssrc", path = ms_root("scen-src"))
  sol <- solve_scenario(sc)
  # override ONE backend's code; the others stay identical to the package
  sol@settings@sourceCode[["GAMS"]] <-
    c("* custom override", sol@settings@sourceCode[["GAMS"]])
  saved <- suppressMessages(save_scenario(sol, verbose = FALSE))

  e <- new.env()
  load(file.path(saved@path, "scen.RData"), envir = e)
  raw <- get(ls(e)[1], envir = e)
  # the saved copy dropped the default blocks and kept the override
  expect_true("GLPK" %in% raw@misc$sourceCode_default)
  expect_null(raw@settings@sourceCode[["GLPK"]])
  expect_identical(raw@settings@sourceCode[["GAMS"]][1], "* custom override")
  # the returned object is untouched
  expect_identical(saved@settings@sourceCode[["GLPK"]],
                   energyRt:::.modelCode[["GLPK"]])

  # loading restores the dropped blocks from the package and keeps overrides
  back <- suppressMessages(load_scenario(saved@path, env = NULL,
                                         verbose = FALSE))
  expect_identical(back@settings@sourceCode[["GLPK"]],
                   energyRt:::.modelCode[["GLPK"]])
  expect_identical(back@settings@sourceCode[["GAMS"]][1], "* custom override")
})

test_that("read_solution keeps solver code file NAMES, not their text", {
  skip_if_no_solver()
  ms_local_store()
  mod <- sp_tech(c(10, 10, 10), optret = FALSE, name = "mscode")
  sc <- interpolate_model(mod, name = "mscode", path = ms_root("scen-code"))
  sol <- solve_scenario(sc)
  expect_null(sol@settings@solver$code1)
  expect_true("energyRt.mod" %in% sol@settings@solver$code_files)
})
