# Content-addressed repository store + repository references in the model
# store (R/repo_store.R + save_model(embed_repos=); layout 3, S4c).
# Fixtures from helper-stock-path.R (sp_tech's model carries one repo "spr").

rs_root <- function(...) {
  gsub("[\\/]+", "/", file.path(tempdir(), "repo-store-suite", ...))
}

rs_local_store <- function(env = parent.frame()) {
  old_mp <- set_models_path(rs_root("models"))
  old_rp <- set_repositories_path(rs_root("repositories"))
  old_rf <- set_registry_file(rs_root("reg.csv"))
  expr <- bquote({
    set_models_path(.(old_mp))
    set_repositories_path(.(old_rp))
    set_registry_file(.(old_rf))
  })
  do.call(on.exit, list(expr, add = TRUE), envir = env)
  invisible(NULL)
}

rs_repo <- function(stock = c(100, 100, 100), name = "rsr") {
  mod <- sp_tech(stock, optret = FALSE, name = name)
  mod@data[[1]]
}

test_that("repository_hash is stable, misc-blind, and content-sensitive", {
  r1 <- rs_repo()
  r2 <- rs_repo()
  expect_identical(repository_hash(r1), repository_hash(r2))
  r3 <- r1
  r3@misc$scratch <- "noise"
  r3@data[[1]]@misc$scratch <- "more"
  expect_identical(repository_hash(r3), repository_hash(r1))
  r4 <- rs_repo(stock = c(100, 100, 90))
  expect_false(identical(repository_hash(r4), repository_hash(r1)))
})

test_that("save_repository / load_repository round-trips; identical content is a no-op", {
  rs_local_store()
  repo <- rs_repo()
  h8 <- substr(repository_hash(repo), 1, 8)

  save_repository(repo, verbose = FALSE)
  # default layout: the folder is the NAME; the hash lives in the manifest
  store_dir <- rs_root("repositories", repo@name)
  expect_true(file.exists(file.path(store_dir, "repo.RData")))
  mf <- yaml::read_yaml(file.path(store_dir, "repository.yml"))
  expect_identical(mf$class, "repository")
  expect_identical(substr(mf$hash, 1, 8), h8)

  expect_message(save_repository(repo), "already in the store")

  back <- load_repository(repo@name, verbose = FALSE)
  expect_s4_class(back, "repository")
  expect_identical(back@name, repo@name)
  expect_setequal(names(back@data), names(repo@data))
  back2 <- load_repository(paste0(repo@name, "@", h8), verbose = FALSE)
  expect_identical(back2@name, repo@name)

  # registry row + refresh rescan
  reg <- load_registry()
  expect_identical(
    nrow(find_registry(reg, type = "repository", name = repo@name)), 1L)
  reg2 <- refresh_registry(root = rs_root(), file = rs_root("reg.csv"),
                           write = FALSE)
  expect_identical(
    nrow(find_registry(reg2, type = "repository", name = repo@name)), 1L)

  expect_error(load_repository("no-such-repo", verbose = FALSE),
               "refresh_registry")
})

test_that("save_model references a stored repository; load_model resolves it", {
  rs_local_store()
  mod <- sp_tech(c(80, 80, 80), optret = FALSE, name = "rsm")
  repo_name <- names(mod@data)[1]
  h_full <- model_hash(mod)

  save_repository(mod@data[[1]], verbose = FALSE)
  saved <- save_model(mod, verbose = FALSE)

  # manifest records the reference; the hash is of the FULL model
  store_dir <- rs_root("models", "rsm")
  mf <- yaml::read_yaml(file.path(store_dir, "model.yml"))
  expect_identical(mf$hash, h_full)
  expect_identical(length(mf$repositories), 1L)
  expect_identical(mf$repositories[[1]]$source, "ref")
  expect_identical(mf$repositories[[1]]$name, mod@data[[1]]@name)

  # the stored RData holds a stub; the returned object keeps the full repo
  e <- new.env()
  load(file.path(store_dir, "mod.RData"), envir = e)
  raw <- get(ls(e)[1], envir = e)
  expect_false(is.null(raw@data[[repo_name]]@misc$repo_ref))
  expect_identical(length(raw@data[[repo_name]]@data), 0L)
  expect_gt(length(saved@data[[repo_name]]@data), 0L)

  # loading resolves the reference back to a full repository
  back <- load_model("rsm", verbose = FALSE)
  expect_gt(length(back@data[[repo_name]]@data), 0L)
  expect_null(back@data[[repo_name]]@misc$repo_ref)

  # with the repository store gone, load_model fails loudly
  unlink(rs_root("repositories"), recursive = TRUE)
  unlink(rs_root("reg.csv"))
  expect_error(load_model("rsm", verbose = FALSE), "repository store")
})

test_that("embed_repos = TRUE embeds; FALSE requires a store hit", {
  rs_local_store()
  mod <- sp_tech(c(60, 60, 60), optret = FALSE, name = "rse")
  save_repository(mod@data[[1]], verbose = FALSE)

  save_model(mod, embed_repos = TRUE, verbose = FALSE)
  store_dir <- rs_root("models", "rse")
  mf <- yaml::read_yaml(file.path(store_dir, "model.yml"))
  expect_identical(mf$repositories[[1]]$source, "embedded")
  # loads without the repository store
  unlink(rs_root("repositories"), recursive = TRUE)
  back <- load_model("rse", verbose = FALSE)
  expect_gt(length(back@data[[1]]@data), 0L)

  mod2 <- sp_tech(c(55, 55, 55), optret = FALSE, name = "rse2")
  expect_error(save_model(mod2, embed_repos = FALSE, verbose = FALSE),
               "save_repository")
})

test_that("the full chain resolves: scenario -> model ref -> repository ref", {
  skip_if_no_solver()
  rs_local_store()
  mod <- sp_tech(c(40, 40, 40), optret = FALSE, name = "rsc")
  save_repository(mod@data[[1]], verbose = FALSE)
  save_model(mod, verbose = FALSE)

  sc <- interpolate_model(mod, name = "rsc", path = rs_root("scen"))
  sol <- solve_scenario(sc)
  saved <- suppressMessages(save_scenario(sol, verbose = FALSE))
  mf <- yaml::read_yaml(file.path(saved@path, "scenario.yml"))
  expect_identical(mf$model$source, "ref")

  back <- suppressMessages(load_scenario(saved@path, env = NULL,
                                         verbose = FALSE))
  expect_identical(back@model@name, "rsc")
  expect_gt(length(back@model@data[[1]]@data), 0L)
  expect_null(back@model@data[[1]]@misc$repo_ref)
})
