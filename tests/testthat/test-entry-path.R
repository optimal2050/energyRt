# An entry is addressable by its FOLDER, not only by its registered name
# (R/save-objects.R, `.entry_path_resolve()`), so the lifecycle verbs reach a
# folder outside the project's store and a name the registry cannot resolve --
# the three forms `load_scenario()` takes.

ep_root <- function(...) {
  gsub("[\\/]+", "/", file.path(tempdir(), "entry-path-suite", ...))
}

# a scenario folder, hand-made: `.entry_resolve()` reads the manifest and
# nothing else, so no solve is needed to exercise it
ep_scen_dir <- function(dir, name, ...) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  yaml::write_yaml(c(list(name = name), list(...)), fp(dir, "scenario.yml"))
  dir
}

test_that("a folder path resolves to its entry, and the NAME comes from the manifest", {
  d <- ep_scen_dir(ep_root("run-2050"), "ep_named", sealed = TRUE,
                   outputs_sealed = TRUE)
  e <- energyRt:::.entry_resolve("scenario", d)
  expect_identical(e$name, "ep_named")
  expect_true(dir.exists(e$path))
  # the registry row is keyed on this name, so it must never be the path
  expect_false(grepl("[/]", e$name))

  # a manifest with no name falls back to the folder's own
  d2 <- ep_scen_dir(ep_root("ep_unnamed"), NULL)
  expect_identical(energyRt:::.entry_resolve("scenario", d2)$name, "ep_unnamed")
})

test_that("unseal_scenario() by path clears both parts; outputs-only keeps the inputs", {
  d <- ep_scen_dir(ep_root("both"), "ep_both", sealed = TRUE,
                   outputs_sealed = TRUE)
  unseal_scenario(d, verbose = FALSE)
  mf <- yaml::read_yaml(fp(d, "scenario.yml"))
  expect_false(isTRUE(mf$sealed))
  expect_false(isTRUE(mf$outputs_sealed))
  expect_identical(mf$name, "ep_both")

  d2 <- ep_scen_dir(ep_root("outs"), "ep_outs", sealed = TRUE,
                    outputs_sealed = TRUE)
  unseal_scenario(d2, inputs = FALSE, outputs = TRUE, verbose = FALSE)
  mf2 <- yaml::read_yaml(fp(d2, "scenario.yml"))
  expect_true(isTRUE(mf2$sealed))
  expect_false(isTRUE(mf2$outputs_sealed))
})

test_that("a folder is an entry only with the kind's manifest; layout 2 says so", {
  plain <- ep_root("not-an-entry")
  dir.create(plain, recursive = TRUE, showWarnings = FALSE)
  expect_null(energyRt:::.entry_path_resolve("scenario", plain))
  expect_error(energyRt:::.entry_resolve("scenario", plain),
               "no 'scenario.yml' in it")

  old2 <- ep_root("layout-2")
  dir.create(old2, recursive = TRUE, showWarnings = FALSE)
  writeLines("scenario", fp(old2, "class"))
  expect_error(energyRt:::.entry_resolve("scenario", old2),
               "upgrade_scenario_layout")

  # a bare unknown name keeps pointing at the store, not at the filesystem
  expect_error(energyRt:::.entry_resolve("scenario", "ep_no_such_name"),
               "was not found in its store")
})

test_that("a scenario carries its own path, so the object form agrees with it", {
  d <- ep_scen_dir(ep_root("obj"), "ep_obj", sealed = TRUE)
  sc <- new("scenario")        # the constructors refuse slot values in new()
  sc@name <- "ep_obj"
  sc@path <- d
  expect_identical(energyRt:::.entry_resolve("scenario", sc)$path,
                   energyRt:::.entry_resolve("scenario", d)$path)
})
