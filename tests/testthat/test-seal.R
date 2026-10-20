# Name-addressed stores + entry lifecycle (R/seal.R): in-place updates,
# seal/unseal, reference verification warnings, mark-for-deletion queue.
# Fixtures from helper-stock-path.R.

sl_root <- function(...) {
  gsub("[\\/]+", "/", file.path(tempdir(), "seal-suite", ...))
}

sl_local <- function(env = parent.frame()) {
  dir.create(sl_root(), recursive = TRUE, showWarnings = FALSE)
  old <- suppressMessages(open_project(sl_root(), verbose = FALSE))
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

# a model whose one technology's fixom we can turn like a dial
sl_mod <- function(name, fixom = 1) {
  m <- sp_tech(c(100, 100, 100), optret = FALSE, name = name)
  m@data[[1]]@data[["TECH"]]@fixom$fixom[1] <- fixom
  m
}

test_that("a changed model UPDATES IN PLACE: same folder, new hash", {
  sl_local()
  unlink(sl_root("models"), recursive = TRUE)
  m1 <- save_model(sl_mod("ipm", fixom = 1), verbose = FALSE)
  expect_identical(basename(m1@misc$path), "ipm")   # readable, no hash

  # a derived-artifact cache beside the entry survives the update
  dir.create(fp(m1@misc$path, "reports"), showWarnings = FALSE)
  writeLines("keep me", fp(m1@misc$path, "reports", "r.html"))

  m2 <- save_model(sl_mod("ipm", fixom = 2), verbose = FALSE)
  expect_identical(m2@misc$path, m1@misc$path)      # SAME folder
  expect_false(identical(m2@misc$hash, m1@misc$hash))
  expect_length(list.dirs(sl_root("models"), recursive = FALSE), 1L)
  mf <- yaml::read_yaml(fp(m2@misc$path, "model.yml"))
  expect_identical(mf$hash, m2@misc$hash)
  expect_true(nzchar(mf$updated))
  expect_true(file.exists(fp(m2@misc$path, "reports", "r.html")))

  # re-saving a LOADED model is a genuine no-op (rehydrate-before-hash)
  back <- load_model("ipm", verbose = FALSE)
  expect_message(save_model(back), "already in the store")
  expect_length(list.dirs(sl_root("models"), recursive = FALSE), 1L)
})

test_that("an outdated reference warns and loads the current version", {
  sl_local()
  unlink(sl_root(), recursive = TRUE)
  mod <- sl_mod("rwm", fixom = 1)
  repo <- mod@data[[1]]
  save_repository(repo, verbose = FALSE)
  suppressMessages(save_model(mod, verbose = FALSE))   # repo saved as ref

  # the repository moves on: same name, new content, updated in place
  repo2 <- sl_mod("rwm", fixom = 9)@data[[1]]
  save_repository(repo2, verbose = FALSE)

  expect_warning(back <- load_model("rwm", verbose = FALSE),
                 "loading the current version")
  expect_identical(
    back@data[[1]]@data[["TECH"]]@fixom$fixom[1], 9)
})

test_that("seal freezes an entry; unseal reopens it", {
  sl_local()
  unlink(sl_root("models"), recursive = TRUE)
  m <- save_model(sl_mod("slm", fixom = 1), verbose = FALSE)

  expect_message(seal_model("slm"), "sealed")
  # identical content: still a silent no-op
  expect_message(save_model(sl_mod("slm", fixom = 1)),
                 "already in the store")
  # changed content: refused, naming the unseal verb
  expect_error(save_model(sl_mod("slm", fixom = 2), verbose = FALSE),
               "unseal_model")
  # sealed content cannot drift -> no mismatch warning is ever possible
  mf <- yaml::read_yaml(fp(m@misc$path, "model.yml"))
  expect_true(isTRUE(mf$sealed))
  expect_identical(mf$sealed_hash, m@misc$hash)

  expect_message(unseal_model("slm"), "unsealed")
  m2 <- save_model(sl_mod("slm", fixom = 2), verbose = FALSE)
  expect_false(identical(m2@misc$hash, m@misc$hash))
})

test_that("a sealed scenario is an archive: loads, but refuses changes", {
  sl_local()
  unlink(sl_root("scenarios"), recursive = TRUE)
  sc <- interpolate_model(sp_tech(c(50, 50, 50), optret = FALSE,
                                  name = "sls"), name = "sls")
  sc <- suppressMessages(save_scenario(sc, verbose = FALSE))
  seal_scenario("sls", verbose = FALSE)

  expect_error(suppressMessages(save_scenario(sc, verbose = FALSE)),
               "unseal_scenario")
  expect_error(drop_scenario_run(sc, "any"), "unseal_scenario")
  # reading stays free (suppress the pre-existing unsolved-shell rebase note)
  ld <- suppressWarnings(load_scenario("sls", env = NULL, verbose = FALSE))
  expect_s4_class(ld, "scenario")

  unseal_scenario("sls", verbose = FALSE)
  sc2 <- interpolate_model(sp_tech(c(50, 50, 50), optret = FALSE,
                                   name = "sls"), name = "sls")
  expect_s4_class(suppressMessages(save_scenario(sc2, verbose = FALSE)),
                  "scenario")
})

test_that("mark_delete queues; delete_marked honors importance and seals", {
  sl_local()
  unlink(sl_root(), recursive = TRUE)
  save_model(sl_mod("del0", fixom = 1), verbose = FALSE)
  save_model(sl_mod("del5", fixom = 1), verbose = FALSE)
  save_model(sl_mod("keep", fixom = 1), verbose = FALSE)
  save_dataset(data.frame(a = 1:3), "dds", verbose = FALSE)

  mark_delete("del0", type = "model", importance = 0, verbose = FALSE)
  mark_delete("del5", type = "model", importance = 5, verbose = FALSE)
  mark_delete("dds", type = "dataset", importance = 0, verbose = FALSE)
  mark_delete("keep", type = "model", importance = 0, verbose = FALSE)
  seal_model("keep", verbose = FALSE)     # sealed wins over the mark

  # loading a marked entry says so
  expect_message(load_model("del5", verbose = FALSE), "marked for deletion")

  # dry run: nothing deleted, actions classified
  out <- suppressMessages(delete_marked(verbose = FALSE))
  expect_setequal(out$name, c("del0", "del5", "dds", "keep"))
  expect_identical(out$action[out$name == "del0"], "would_delete")
  expect_identical(out$action[out$name == "del5"], "kept_importance")
  expect_identical(out$action[out$name == "keep"], "skipped_sealed")
  expect_true(dir.exists(sl_root("models", "del0")))

  # real run at threshold 0: del0 + dds go; del5 and sealed keep stay
  res <- suppressMessages(suppressWarnings(
    delete_marked(dry_run = FALSE, verbose = FALSE)))
  expect_false(dir.exists(sl_root("models", "del0")))
  expect_false(dir.exists(sl_root("datasets", "dds")))
  expect_true(dir.exists(sl_root("models", "del5")))
  expect_true(dir.exists(sl_root("models", "keep")))
  reg <- load_registry()
  expect_identical(nrow(find_in_registry(reg, type = "model", name = "del0")), 0L)

  # unmark clears the queue
  unmark_delete("del5", type = "model", verbose = FALSE)
  out2 <- suppressMessages(delete_marked(verbose = FALSE))
  expect_false("del5" %in% out2$name)

  # dispatch on the OBJECT works too
  m <- load_model("del5", verbose = FALSE)
  mark_delete(m, importance = 1, verbose = FALSE)
  out3 <- suppressMessages(delete_marked(max_importance = 1,
                                         verbose = FALSE))
  expect_identical(out3$action[out3$name == "del5"], "would_delete")
})

test_that("pinned-version load in plain layout errors when superseded", {
  sl_local()
  unlink(sl_root("models"), recursive = TRUE)
  m1 <- save_model(sl_mod("pin", fixom = 1), verbose = FALSE)
  h8_old <- substr(m1@misc$hash, 1, 8)
  save_model(sl_mod("pin", fixom = 2), verbose = FALSE)
  expect_error(load_model(paste0("pin@", h8_old), verbose = FALSE),
               "updated in place")
})
