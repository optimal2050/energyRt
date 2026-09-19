# The reference graph over the four stores (R/store_deps.R) and the delete
# guard it feeds (delete_marked()).
#
# What would break silently without these:
#   * referencing is the DEFAULT since save_scenario(embed_model = NULL) stores
#     the model, so deleting one entry can break many -- and only at load time;
#   * an EMBEDDED copy is not a dependency, and confusing the two would either
#     block harmless deletions or permit harmful ones;
#   * the prompt must never run in a non-interactive session: menu() reading
#     EOF would abort or hang a scripted sweep.

# Covers the R API store_dependents(), delete_marked() and mark_delete().
# Deliberately NOT an `@covers` tag -- see the note in test-report-clear.R:
# that vocabulary is the model coverage matrix, not exported function names.

sd_root <- function(...) {
  gsub("[\\/]+", "/", file.path(tempdir(), "store-deps-suite", ...))
}

sd_local_store <- function(env = parent.frame()) {
  old_mp <- set_models_path(sd_root("models"))
  old_sp <- set_scenarios_path(sd_root("scenarios"))
  old_rf <- set_registry_file(sd_root("reg.csv"))
  expr <- bquote({
    set_models_path(.(old_mp))
    set_scenarios_path(.(old_sp))
    set_registry_file(.(old_rf))
  })
  do.call(on.exit, list(expr, add = TRUE), envir = env)
  invisible(NULL)
}

# an interpolated (unsolved) scenario is enough: the reference is recorded on
# save, not on solve
sd_scen <- function(mod, name) {
  sc <- interpolate_model(mod, name = name, path = sd_root("scenarios", name))
  suppressMessages(save_scenario(sc, verbose = FALSE))
}

test_that("scenarios referencing a model are its dependents; embedded are not", {
  sd_local_store()
  mod <- sp_tech(c(100, 100, 100), optret = FALSE, name = "sdm")
  h <- model_hash(mod)

  sd_scen(mod, "sd_a")
  sd_scen(mod, "sd_b")

  dep <- store_dependents("sdm", type = "model")
  expect_setequal(dep$name, c("sd_a", "sd_b"))
  expect_true(all(dep$type == "scenario"))
  expect_true(all(dep$via == "model"))
  expect_true(all(dep$current))
  expect_identical(dep$hash, c(h, h))

  # an embedded copy is not a dependency
  sc <- interpolate_model(mod, name = "sd_emb",
                          path = sd_root("scenarios", "sd_emb"))
  suppressMessages(save_scenario(sc, embed_model = TRUE, verbose = FALSE))
  expect_false("sd_emb" %in% store_dependents("sdm", type = "model")$name)
})

test_that("an unreferenced entry has no dependents", {
  sd_local_store()
  mod <- sp_tech(c(90, 90, 90), optret = FALSE, name = "sdorphan")
  suppressMessages(save_model(mod, verbose = FALSE))
  expect_identical(nrow(store_dependents("sdorphan", type = "model")), 0L)
})

test_that("a dependent pinned to a superseded version reports current = FALSE", {
  sd_local_store()
  mod <- sp_tech(c(80, 80, 80), optret = FALSE, name = "sdver")
  sd_scen(mod, "sd_pin")
  expect_true(store_dependents("sdver", type = "model")$current)

  # the entry updates IN PLACE, so the scenario's recorded hash goes stale
  mod2 <- sp_tech(c(80, 80, 70), optret = FALSE, name = "sdver")
  suppressMessages(save_model(mod2, overwrite = TRUE, verbose = FALSE))
  expect_false(store_dependents("sdver", type = "model")$current)
})

test_that("delete_marked refuses a referenced entry and never prompts in batch", {
  sd_local_store()
  mod <- sp_tech(c(70, 70, 70), optret = FALSE, name = "sdkeep")
  sd_scen(mod, "sd_dep")

  suppressMessages(mark_delete("sdkeep", type = "model"))
  dry <- suppressMessages(delete_marked(types = "model"))
  row <- dry[dry$name == "sdkeep", ]
  expect_identical(row$action, "skipped_referenced")
  expect_identical(row$dependents, 1L)

  # testthat runs non-interactively, so this must skip rather than ask
  suppressMessages(delete_marked(types = "model", dry_run = FALSE))
  expect_true(dir.exists(sd_root("models", "sdkeep")))
})

test_that("ignore_refs deletes anyway, and the dependent then fails loudly", {
  sd_local_store()
  mod <- sp_tech(c(60, 60, 60), optret = FALSE, name = "sdgone")
  s <- sd_scen(mod, "sd_orphaned")

  suppressMessages(mark_delete("sdgone", type = "model"))
  suppressMessages(delete_marked(types = "model", dry_run = FALSE,
                                 ignore_refs = TRUE))
  expect_false(dir.exists(sd_root("models", "sdgone")))

  # the failure the guard exists to prevent
  expect_error(
    suppressMessages(load_scenario(s@path, env = NULL, verbose = FALSE)),
    "model store")
})

test_that("sealed still wins over referenced", {
  sd_local_store()
  mod <- sp_tech(c(50, 50, 50), optret = FALSE, name = "sdseal")
  sd_scen(mod, "sd_sdep")
  suppressMessages(seal_model("sdseal"))
  suppressMessages(mark_delete("sdseal", type = "model"))

  dry <- suppressMessages(delete_marked(types = "model"))
  expect_identical(dry$action[dry$name == "sdseal"], "skipped_sealed")
})

test_that("mark_delete reports the dependents but does not refuse", {
  sd_local_store()
  mod <- sp_tech(c(40, 40, 40), optret = FALSE, name = "sdnote")
  sd_scen(mod, "sd_note_dep")
  expect_message(mark_delete("sdnote", type = "model"), "reference it")
  mf <- yaml::read_yaml(sd_root("models", "sdnote", "model.yml"))
  expect_true(isTRUE(mf$marked_delete))
})
