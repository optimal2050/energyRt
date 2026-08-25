# Persisted project registry (R/registry.R, storage layout 3 stage S1)

test_that("load_registry returns a typed empty tibble when the file is missing", {
  reg <- load_registry(file.path(tempdir(), "no-such-registry.csv"))
  expect_s3_class(reg, "tbl_df")
  expect_identical(nrow(reg), 0L)
  expect_identical(names(reg), energyRt:::.registry_cols)
})

test_that("registry add/save/load round-trips", {
  f <- tempfile(fileext = ".csv")
  reg <- load_registry(f)
  reg <- add_to_registry(reg, "scenario", "BASE", path = "scenarios/BASE",
                      model_hash = "abc123", memo = "first")
  reg <- add_to_registry(reg, "model", "UTOPIA", path = "models/UTOPIA@abc12345",
                      hash = "abc12345ffff")
  reg <- add_to_registry(reg, "run", "default/glpk", path = "scenarios/BASE/runs/default/glpk",
                      parent = "BASE")
  save_registry(reg, f)
  reg2 <- load_registry(f)
  expect_identical(nrow(reg2), 3L)
  expect_setequal(reg2$type, c("scenario", "model", "run"))
  expect_identical(
    find_registry(reg2, type = "run", parent = "BASE")$name, "default/glpk")
  # all-character round-trip
  expect_true(all(vapply(reg2, is.character, logical(1))))
})

test_that("add_to_registry upserts on (type, name, parent), preserving created/memo", {
  reg <- load_registry(tempfile(fileext = ".csv"))
  reg <- add_to_registry(reg, "scenario", "A", path = "p1", memo = "keep me")
  created1 <- reg$created[1]
  Sys.sleep(1.1)  # timestamps have 1s resolution
  reg <- add_to_registry(reg, "scenario", "A", path = "p2")
  expect_identical(nrow(reg), 1L)
  expect_identical(reg$path, "p2")
  expect_identical(reg$created, created1)   # preserved
  expect_identical(reg$memo, "keep me")     # empty new memo keeps the old
  expect_false(identical(reg$updated, created1))  # updated moved forward
  # different parent -> separate row
  reg <- add_to_registry(reg, "scenario", "A", path = "p3", parent = "X")
  expect_identical(nrow(reg), 2L)
})

test_that("add() dispatches on the registry tibble and upserts a row", {
  reg <- load_registry(tempfile(fileext = ".csv"))
  expect_s3_class(reg, "ert_registry")
  reg <- add(reg, "scenario", "BASE", path = "scenarios/BASE")
  expect_s3_class(reg, "ert_registry")
  expect_identical(nrow(reg), 1L)
  expect_identical(reg$name, "BASE")
  # upsert, not append
  reg <- add(reg, "scenario", "BASE", path = "scenarios/BASE2")
  expect_identical(nrow(reg), 1L)
  expect_identical(reg$path, "scenarios/BASE2")
  # the subclass survives filtering, so dispatch keeps working downstream
  expect_s3_class(find_registry(reg, type = "scenario"), "ert_registry")
})

test_that("find_registry filters by type, name, hash prefix, parent", {
  reg <- load_registry(tempfile(fileext = ".csv"))
  reg <- add_to_registry(reg, "model", "M1", path = "models/M1@11112222",
                      hash = "1111222233334444")
  reg <- add_to_registry(reg, "model", "M2", path = "models/M2@aaaabbbb",
                      hash = "aaaabbbbccccdddd")
  reg <- add_to_registry(reg, "scenario", "S1", path = "scenarios/S1")
  expect_identical(find_registry(reg, type = "model") |> nrow(), 2L)
  expect_identical(find_registry(reg, hash = "aaaabbbb")$name, "M2")   # short hash
  expect_identical(find_registry(reg, hash = "1111222233334444")$name, "M1")
  expect_identical(find_registry(reg, type = "scenario", name = "S1") |> nrow(), 1L)
  expect_identical(find_registry(reg, name = "nope") |> nrow(), 0L)
})

test_that("refresh_registry rebuilds from on-disk markers and manifests", {
  root <- file.path(tempdir(), paste0("regproj", as.integer(runif(1, 1, 1e6))))
  scen_root <- file.path(root, get_scenarios_path())
  mod_root <- file.path(root, get_models_path())
  reg_file <- file.path(root, "energyRt_registry.csv")

  # layout-2 style scenario: `class` marker only
  dir.create(file.path(scen_root, "OLD2"), recursive = TRUE)
  writeLines("scenario", file.path(scen_root, "OLD2", "class"))

  # layout-3 style scenario with manifest + a base run and a variant run
  dir.create(file.path(scen_root, "NEW3", "runs", "glpk"), recursive = TRUE)
  dir.create(file.path(scen_root, "NEW3", "runs", "cal-d24", "glpk"),
             recursive = TRUE)
  yaml::write_yaml(
    list(layout = 3L, name = "NEW3", model = list(hash = "cafe0123deadbeef")),
    file.path(scen_root, "NEW3", "scenario.yml"))
  yaml::write_yaml(
    list(label = "glpk", variant = "", status = "solved"),
    file.path(scen_root, "NEW3", "runs", "glpk", "run.yml"))
  yaml::write_yaml(
    list(label = "glpk", variant = "cal-d24", status = "solved"),
    file.path(scen_root, "NEW3", "runs", "cal-d24", "glpk", "run.yml"))

  # model store entry
  dir.create(file.path(mod_root, "UTOPIA@cafe0123"), recursive = TRUE)
  yaml::write_yaml(
    list(layout = 3L, name = "UTOPIA", hash = "cafe0123deadbeef"),
    file.path(mod_root, "UTOPIA@cafe0123", "model.yml"))

  # a directory that is NOT a scenario (no marker) must be skipped
  dir.create(file.path(scen_root, "random_dir"))

  reg <- refresh_registry(root = root, file = reg_file, write = TRUE)
  expect_true(file.exists(reg_file))
  expect_setequal(find_registry(reg, type = "scenario")$name, c("OLD2", "NEW3"))
  expect_identical(find_registry(reg, type = "model")$name, "UTOPIA")
  expect_identical(find_registry(reg, type = "model")$hash, "cafe0123deadbeef")
  expect_identical(find_registry(reg, type = "scenario", name = "NEW3")$model_hash,
                   "cafe0123deadbeef")
  run <- find_registry(reg, type = "run")
  expect_setequal(run$name, c("glpk", "cal-d24/glpk"))
  expect_setequal(run$parent, "NEW3")
  # paths are relative to the registry file's directory
  expect_false(any(grepl("^([A-Za-z]:|/)", find_registry(reg, type = "scenario")$path)))

  # refresh again preserves created stamps and stays stable
  created <- find_registry(reg, type = "scenario", name = "NEW3")$created
  Sys.sleep(1.1)
  reg2 <- refresh_registry(root = root, file = reg_file, write = FALSE)
  expect_identical(
    find_registry(reg2, type = "scenario", name = "NEW3")$created, created)
  expect_identical(nrow(reg2), nrow(reg))
  unlink(root, recursive = TRUE)
})

test_that("deprecated registry shims warn and delegate", {
  f <- tempfile(fileext = ".csv")
  old <- set_registry_file(f)
  on.exit(set_registry_file(old), add = TRUE)

  reg <- add_to_registry(load_registry(f), "scenario", "S", path = "scenarios/S")
  save_registry(reg, f)

  expect_warning(r <- get_registry(), "deprecated")
  expect_identical(nrow(r), 1L)
  expect_warning(ex <- registry_exists("S"), "deprecated")
  expect_true(ex)
  expect_warning(e <- get_entry("S"), "deprecated")
  expect_identical(e$name, "S")
  expect_warning(w <- which_registry(), "deprecated")
  expect_identical(w$file, f)
  expect_warning(set_default_registry(), "deprecated")
})

test_that("newScenario returns the object and ignores registry args with a message", {
  s <- newScenario("TESTSCEN", path = NULL)
  expect_s4_class(s, "scenario")
  expect_identical(s@name, "TESTSCEN")
  expect_message(s2 <- newScenario("TESTSCEN2", path = NULL, registry = "x"),
                 "deprecated")
  expect_s4_class(s2, "scenario")
})
