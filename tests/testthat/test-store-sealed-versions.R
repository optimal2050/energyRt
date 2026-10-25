# Sealed versions of model and repository entries: sealing freezes the payload
# as `<entry>/<hash>/`; unsealed saves update the entry in place and never
# touch a sealed version; references and pinned loads reach it exactly.
# Fixtures from helper-stock-path.R; store helpers as in test-seal.R.

sv_root <- function(...) {
  gsub("[\\/]+", "/", file.path(tempdir(), "sealed-versions-suite", ...))
}

sv_local <- function(env = parent.frame()) {
  dir.create(sv_root(), recursive = TRUE, showWarnings = FALSE)
  old <- suppressMessages(open_project(sv_root(), verbose = FALSE))
  expr <- bquote({
    old <- .(old)
    set_registry_file(old$registry_file)
    set_scenarios_path(old$scenarios_path)
    set_models_path(old$models_path)
    set_repositories_path(old$repositories_path)
    set_datasets_path(old$datasets_path)
    set_reports_path(old$reports_path)
    set_levcost_cache_path(old$levcost_cache_path)
  })
  do.call(on.exit, list(expr, add = TRUE), envir = env)
  invisible(NULL)
}

sv_mod <- function(name, fixom = 1) {
  m <- sp_tech(c(100, 100, 100), optret = FALSE, name = name)
  m@data[[1]]@data[["TECH"]]@fixom$fixom[1] <- fixom
  m
}
sv_fixom <- function(x) {
  repo <- if (is(x, "model")) x@data[[1]] else x
  repo@data[["TECH"]]@fixom$fixom[1]
}

test_that("sealing a repository moves its payload into a version folder", {
  sv_local()
  unlink(sv_root(), recursive = TRUE)
  repo <- sv_mod("svr", fixom = 1)@data[[1]]
  st <- save_repository(repo, verbose = FALSE)
  h1 <- st@misc$hash
  entry <- st@misc$path
  expect_true(file.exists(fp(entry, "repo.RData")))

  suppressMessages(seal_repository(repo@name))
  expect_false(file.exists(fp(entry, "repo.RData")))
  expect_true(file.exists(fp(entry, h1, "repo.RData")))
  mf <- yaml::read_yaml(fp(entry, "repository.yml"))
  expect_true(isTRUE(mf$sealed))
  expect_true(h1 %in% names(mf$versions))

  # loads by name and by hash reach the sealed version
  expect_identical(sv_fixom(load_repository(repo@name, verbose = FALSE)), 1)
  expect_identical(sv_fixom(load_repository(repo@name, hash = substr(h1, 1, 8),
                                            verbose = FALSE)), 1)
  # other content is refused while sealed; the same content is a no-op
  expect_error(save_repository(sv_mod("svr", fixom = 2)@data[[1]], verbose = FALSE),
               "sealed")
  expect_no_error(suppressMessages(save_repository(repo, verbose = FALSE)))
  # sealing again changes nothing
  expect_no_error(suppressMessages(seal_repository(repo@name)))
  expect_true(file.exists(fp(entry, h1, "repo.RData")))
})

test_that("after unsealing, new content goes to the entry and the sealed version stays", {
  sv_local()
  unlink(sv_root(), recursive = TRUE)
  repo1 <- sv_mod("svu", fixom = 1)@data[[1]]
  rn <- repo1@name
  h1 <- save_repository(repo1, verbose = FALSE)@misc$hash
  suppressMessages(seal_repository(rn))
  suppressMessages(unseal_repository(rn))
  # still the sealed version by name until something new is saved
  expect_identical(sv_fixom(load_repository(rn, verbose = FALSE)), 1)

  st2 <- save_repository(sv_mod("svu", fixom = 2)@data[[1]], verbose = FALSE)
  h2 <- st2@misc$hash
  entry <- st2@misc$path
  expect_true(file.exists(fp(entry, "repo.RData")))
  expect_true(file.exists(fp(entry, h1, "repo.RData")))
  mf <- yaml::read_yaml(fp(entry, "repository.yml"))
  expect_identical(mf$hash, h2)
  expect_true(h1 %in% names(mf$versions))
  expect_identical(sv_fixom(load_repository(rn, verbose = FALSE)), 2)
  expect_identical(sv_fixom(load_repository(rn, hash = h1, verbose = FALSE)), 1)

  # unsealed saves overwrite the entry, never the sealed version
  save_repository(sv_mod("svu", fixom = 3)@data[[1]], verbose = FALSE)
  expect_identical(sv_fixom(load_repository(rn, verbose = FALSE)), 3)
  expect_identical(sv_fixom(load_repository(rn, hash = h1, verbose = FALSE)), 1)
})

test_that("a model referencing a sealed repository version keeps it exactly", {
  sv_local()
  unlink(sv_root(), recursive = TRUE)
  mod <- sv_mod("svm", fixom = 1)
  save_repository(mod@data[[1]], verbose = FALSE)
  suppressMessages(seal_repository(mod@data[[1]]@name))
  suppressMessages(save_model(mod, verbose = FALSE))      # references the sealed repository

  suppressMessages(unseal_repository(mod@data[[1]]@name))
  save_repository(sv_mod("svm", fixom = 9)@data[[1]], verbose = FALSE)

  expect_no_warning(back <- load_model("svm", verbose = FALSE))
  expect_identical(sv_fixom(back), 1)
})

test_that("a sealed model version loads by name, by hash and from a scenario", {
  sv_local()
  unlink(sv_root(), recursive = TRUE)
  m1 <- save_model(sv_mod("svs", fixom = 1), verbose = FALSE)
  h1 <- m1@misc$hash
  suppressMessages(seal_model("svs"))
  expect_true(file.exists(fp(m1@misc$path, h1, "mod.RData")))
  expect_identical(sv_fixom(load_model("svs", verbose = FALSE)), 1)

  sc <- interpolate_model(sv_mod("svs", fixom = 1), name = "svs_scen",
                          path = sv_root("scenarios", "svs_scen"))
  saved <- suppressMessages(save_scenario(sc, verbose = FALSE))
  expect_identical(yaml::read_yaml(fp(saved@path, "scenario.yml"))$model$source, "ref")

  suppressMessages(unseal_model("svs"))
  save_model(sv_mod("svs", fixom = 5), verbose = FALSE)
  expect_identical(sv_fixom(load_model("svs", verbose = FALSE)), 5)
  expect_identical(sv_fixom(load_model("svs", hash = h1, verbose = FALSE)), 1)
  back <- expect_no_warning(suppressMessages(
    load_scenario(saved@path, env = NULL, verbose = FALSE)))
  expect_identical(sv_fixom(back@model), 1)
})
