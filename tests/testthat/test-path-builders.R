# User-overridable path creation (R/options.R set_path_builder/get_path_builder
# + R/solve.R .path_hook seams): four hookable kinds — scenario_dir,
# store_entry, run_label, slug. Hooks are session options; discovery stays
# manifest/registry-based, so custom folder names must load back by NAME.
# Fixtures from helper-stock-path.R.

pb_root <- function(...) {
  gsub("[\\/]+", "/", file.path(tempdir(), "path-builders-suite", ...))
}

# clear every hook when the calling test frame exits
pb_reset <- function(env = parent.frame()) {
  do.call(on.exit, list(quote(set_path_builder(
    scenario_dir = FALSE, store_entry = FALSE,
    run_label = FALSE, slug = FALSE)), add = TRUE), envir = env)
  invisible(NULL)
}

# point scenarios + models + registry into tempdir for one test
pb_local_store <- function(env = parent.frame()) {
  old_sp <- set_scenarios_path(pb_root("scenarios"))
  old_mp <- set_models_path(pb_root("models"))
  old_rf <- set_registry_file(pb_root("reg.csv"))
  expr <- bquote({
    set_scenarios_path(.(old_sp))
    set_models_path(.(old_mp))
    set_registry_file(.(old_rf))
  })
  do.call(on.exit, list(expr, add = TRUE), envir = env)
  invisible(NULL)
}

test_that("set_path_builder merges, clears, and validates at set time", {
  pb_reset()
  f <- function(parts) paste(parts, collapse = "+")
  set_path_builder(slug = f)
  expect_identical(get_path_builder("slug"), f)
  expect_null(get_path_builder("scenario_dir"))

  g <- function(name, model, calendar, horizon) name
  set_path_builder(scenario_dir = g)          # NULL leaves `slug` untouched
  expect_identical(get_path_builder("slug"), f)
  expect_identical(get_path_builder("scenario_dir"), g)
  expect_setequal(names(get_path_builder()), c("slug", "scenario_dir"))

  set_path_builder(slug = FALSE)              # FALSE removes one hook
  expect_null(get_path_builder("slug"))
  expect_identical(get_path_builder("scenario_dir"), g)

  expect_error(set_path_builder(slug = "not a function"), "must be a function")
})

test_that("a slug hook replaces the sanitize/join primitive; clearing restores", {
  pb_reset()
  slug <- energyRt:::.path_slug
  set_path_builder(slug = function(parts) paste(tolower(parts), collapse = "."))
  expect_identical(slug("BASE", "UTOPIA_R11"), "base.utopia_r11")
  # nothing to slug -> "" without consulting the hook (callers have fallbacks)
  expect_identical(slug("", NA), "")
  set_path_builder(slug = FALSE)
  expect_identical(slug("BASE", "UTOPIA_R11"), "BASE-UTOPIA_R11")
})

test_that("an invalid hook return errors naming the contract, never silently", {
  pb_reset()
  slug <- energyRt:::.path_slug
  set_path_builder(slug = function(parts) c("a", "b"))
  expect_error(slug("x"), "`slug` path builder")
  set_path_builder(slug = function(parts) "")
  expect_error(slug("x"), "`slug` path builder")
  set_path_builder(slug = function(parts) "a/b")
  expect_error(slug("x"), "filesystem-safe")
  set_path_builder(slug = function(parts) "a b")   # space is not path-safe
  expect_error(slug("x"), "filesystem-safe")
})

test_that("scenario_dir hook names the folder; the scenario loads back by name", {
  pb_reset()
  pb_local_store()
  set_path_builder(scenario_dir = function(name, model, calendar, horizon) {
    paste("proj", name, sep = ".")
  })
  sc <- interpolate_model(sp_tech(c(50, 50, 50), optret = FALSE,
                                  name = "pbm"), name = "PB_A")
  expect_identical(basename(sc@path), "proj.PB_A")
  suppressMessages(save_scenario(sc, verbose = FALSE))

  # resolution is by manifest/registry name — the folder name is irrelevant,
  # even with the hook cleared
  set_path_builder(scenario_dir = FALSE)
  ld <- load_scenario("PB_A", env = NULL, verbose = FALSE)
  expect_identical(ld@name, "PB_A")
  expect_identical(basename(ld@path), "proj.PB_A")

  # a broken hook is heard at interpolation time
  set_path_builder(scenario_dir = function(name, model, calendar, horizon) NULL)
  expect_error(
    interpolate_model(sp_tech(c(50, 50, 50), optret = FALSE, name = "pbm"),
                      name = "PB_B"),
    "`scenario_dir` path builder")
})

test_that("store_entry hook names store folders; name resolution still works", {
  pb_reset()
  pb_local_store()
  set_path_builder(store_entry = function(type, name) {
    paste(substr(type, 1, 3), name, sep = "_")
  })
  mod <- sp_tech(c(100, 100, 100), optret = FALSE, name = "pbsm")
  saved <- suppressMessages(save_model(mod, verbose = FALSE))
  expect_identical(basename(saved@misc$path), "mod_pbsm")
  expect_true(file.exists(fp(saved@misc$path, "model.yml")))

  # registry route
  back <- load_model("pbsm", verbose = FALSE)
  expect_identical(back@name, "pbsm")

  # manifest-sweep route: no registry, hook cleared -> the slug fast-path
  # misses and the sweep matches the manifest's name field
  set_path_builder(store_entry = FALSE)
  unlink(get_registry_file())
  back2 <- load_model("pbsm", verbose = FALSE)
  expect_identical(basename(back2@misc$path), "mod_pbsm")

  # in-place no-op and update keep working in the custom folder
  set_path_builder(store_entry = function(type, name) {
    paste(substr(type, 1, 3), name, sep = "_")
  })
  expect_message(save_model(mod), "already in the store")
  mod2 <- sp_tech(c(100, 100, 90), optret = FALSE, name = "pbsm")
  up <- suppressMessages(save_model(mod2, verbose = FALSE))
  expect_identical(basename(up@misc$path), "mod_pbsm")
  expect_identical(
    yaml::read_yaml(fp(up@misc$path, "model.yml"))$hash %||% "",
    model_hash(mod2))
})

test_that("run_label hook sets the default solve label", {
  pb_reset()
  set_path_builder(run_label = function(solver) "lab_x")
  expect_identical(energyRt:::.solver_dir_name(list(name = "glpk")), "lab_x")
  set_path_builder(run_label = FALSE)
  expect_identical(energyRt:::.solver_dir_name(list(name = "glpk")), "glpk")
})

test_that("a solve without run= lands under the hooked label", {
  skip_if_no_solver()
  pb_reset()
  pb_local_store()
  set_path_builder(run_label = function(solver) "pb_run")
  sc <- interpolate_model(sp_tech(c(100, 100, 100), optret = FALSE,
                                  name = "pbr"), name = "pbr")
  sol <- solve_scenario(sc)
  expect_identical(sol@misc$run, "pb_run")
  expect_true(file.exists(file.path(sol@path, "runs", "pb_run", "run.yml")))
  runs <- scenario_runs(sol)
  expect_identical(runs$run, "pb_run")
  # an explicit run= label still wins over the hook
  sol2 <- solve_scenario(sol, force = TRUE, run = "explicit")
  expect_identical(sol2@misc$run, "explicit")
})
