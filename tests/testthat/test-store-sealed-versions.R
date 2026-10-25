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
  entry <- st@misc$path
  expect_true(file.exists(fp(entry, "repo.RData")))
  # an unsealed save records a save id, not a content hash
  expect_false(isTRUE(yaml::read_yaml(fp(entry, "repository.yml"))$hashed))

  suppressMessages(seal_repository(repo@name))
  h1 <- yaml::read_yaml(fp(entry, "repository.yml"))$hash
  expect_identical(h1, repository_hash(repo))
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
  e1 <- save_repository(repo1, verbose = FALSE)@misc$path
  suppressMessages(seal_repository(rn))
  h1 <- yaml::read_yaml(fp(e1, "repository.yml"))$hash
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

  suppressMessages(unseal_repository(mod@data[[1]]@name, force = TRUE))
  save_repository(sv_mod("svm", fixom = 9)@data[[1]], verbose = FALSE)

  expect_no_warning(back <- load_model("svm", verbose = FALSE))
  expect_identical(sv_fixom(back), 1)
})

test_that("a sealed model version loads by name, by hash and from a scenario", {
  sv_local()
  unlink(sv_root(), recursive = TRUE)
  m1 <- save_model(sv_mod("svs", fixom = 1), verbose = FALSE)
  suppressMessages(seal_model("svs"))
  h1 <- yaml::read_yaml(fp(m1@misc$path, "model.yml"))$hash
  expect_identical(h1, model_hash(sv_mod("svs", fixom = 1)))
  expect_true(file.exists(fp(m1@misc$path, h1, "mod.RData")))
  expect_identical(sv_fixom(load_model("svs", verbose = FALSE)), 1)

  sc <- interpolate_model(sv_mod("svs", fixom = 1), name = "svs_scen",
                          path = sv_root("scenarios", "svs_scen"))
  saved <- suppressMessages(save_scenario(sc, verbose = FALSE))
  expect_identical(yaml::read_yaml(fp(saved@path, "scenario.yml"))$model$source, "ref")

  suppressMessages(unseal_model("svs", force = TRUE))
  save_model(sv_mod("svs", fixom = 5), verbose = FALSE)
  expect_identical(sv_fixom(load_model("svs", verbose = FALSE)), 5)
  expect_identical(sv_fixom(load_model("svs", hash = h1, verbose = FALSE)), 1)
  back <- expect_no_warning(suppressMessages(
    load_scenario(saved@path, env = NULL, verbose = FALSE)))
  expect_identical(sv_fixom(back@model), 1)
})

test_that("sealed versions record their dependents; unsealing shows and guards them", {
  sv_local()
  unlink(sv_root(), recursive = TRUE)
  mod <- sv_mod("svd", fixom = 1)
  rn <- mod@data[[1]]@name
  save_repository(mod@data[[1]], verbose = FALSE)
  suppressMessages(seal_repository(rn))
  m <- suppressMessages(save_model(mod, verbose = FALSE))  # references the sealed repository
  suppressMessages(seal_model("svd"))
  sc <- interpolate_model(mod, name = "svd_scen", path = sv_root("scenarios", "svd_scen"))
  saved <- suppressMessages(save_scenario(sc, verbose = FALSE))

  r <- list_dependents(rn, type = "repository", recursive = TRUE)
  expect_setequal(paste(r$level, r$type, r$name),
                  c("1 model svd", "2 scenario svd_scen"))
  expect_true(all(r$exists & r$references))
  expect_identical(nrow(list_dependents("svd", type = "model")), 1L)

  # unsealing refuses and names the dependents; forced, the version stays
  expect_error(unseal_repository(rn), "svd_scen")
  expect_error(unseal_model("svd"), "svd_scen")
  suppressMessages(unseal_model("svd", force = TRUE))
  save_model(sv_mod("svd", fixom = 4), verbose = FALSE)
  back <- suppressMessages(load_scenario(saved@path, env = NULL, verbose = FALSE))
  expect_identical(sv_fixom(back@model), 1)

  # a deleted dependent shows as gone, and prune drops the record
  unlink(saved@path, recursive = TRUE)
  r <- list_dependents("svd", type = "model", details = TRUE)
  expect_false(any(r$exists))
  expect_identical(nrow(list_dependents("svd", type = "model")), 0L)
  r <- list_dependents("svd", type = "model", prune = TRUE, details = TRUE)
  expect_identical(nrow(r), 0L)
})

test_that("a model built from objects of a sealed repository references the selection", {
  sv_local()
  unlink(sv_root(), recursive = TRUE)
  base <- sv_mod("svsel", fixom = 1)
  repo <- base@data[[1]]
  expect_gt(length(repo@data), 1L)
  save_repository(repo, verbose = FALSE)
  suppressMessages(seal_repository(repo@name))
  mf <- yaml::read_yaml(fp(sv_root("repositories"), repo@name, "repository.yml"))
  expect_setequal(names(mf$versions[[1]]$objects), names(repo@data))

  # select from the sealed version: every object but the last
  sealed <- load_repository(repo@name, verbose = FALSE)
  keep <- names(sealed@data)[-length(sealed@data)]
  sel <- do.call(newRepository, c(list(name = "svsel_part"), unname(sealed@data[keep])))
  mod <- base
  mod@data <- list(svsel_part = sel)
  saved <- save_model(mod, verbose = FALSE)
  mm <- yaml::read_yaml(fp(saved@misc$path, "model.yml"))
  re <- mm$repositories[[1]]
  expect_identical(re$source, "ref")
  expect_identical(re$name, repo@name)
  expect_identical(re$as, "svsel_part")
  expect_setequal(unlist(re$objects), keep)

  back <- load_model(mod@name, verbose = FALSE)
  expect_identical(names(back@data), "svsel_part")
  expect_identical(back@data[[1]]@name, "svsel_part")
  expect_setequal(names(back@data[[1]]@data), keep)
  expect_identical(object_hash(back@data[[1]]@data[[keep[1]]]),
                   object_hash(repo@data[[keep[1]]]))
  # the sealed version records the model as a dependent
  expect_true("svsel" %in% list_dependents(repo@name, type = "repository")$name)

  # a changed object is not the sealed one: the repository is copied in
  sel2 <- sel
  o <- sel2@data[[keep[1]]]
  o@desc <- paste(o@desc, "changed")
  sel2@data[[keep[1]]] <- o
  mod2 <- base
  mod2@name <- "svsel2"
  mod2@data <- list(svsel_part = sel2)
  s2 <- save_model(mod2, verbose = FALSE)
  expect_identical(yaml::read_yaml(fp(s2@misc$path, "model.yml"))$repositories[[1]]$source,
                   "embedded")
})

test_that("unsealed saves record a save id; sealing computes the content hash", {
  sv_local()
  unlink(sv_root(), recursive = TRUE)
  m <- sv_mod("svlazy", fixom = 1)
  s1 <- save_model(m, verbose = FALSE)
  mf <- yaml::read_yaml(fp(s1@misc$path, "model.yml"))
  expect_false(isTRUE(mf$hashed))
  expect_match(mf$hash, "^u[0-9]{14}")
  # an identical re-save is written again under a new save id
  s2 <- save_model(m, verbose = FALSE)
  expect_false(identical(s2@misc$hash, s1@misc$hash))
  suppressMessages(seal_model("svlazy"))
  mf <- yaml::read_yaml(fp(s1@misc$path, "model.yml"))
  expect_true(isTRUE(mf$hashed))
  expect_identical(mf$hash, model_hash(m))
  # a sealed entry is hashed on save: identical content is a no-op
  expect_message(save_model(m), "already in the store")
})

test_that("list_versions shows a status; marked unused versions are deleted on request", {
  sv_local()
  unlink(sv_root(), recursive = TRUE)
  m1 <- sv_mod("svgc", fixom = 1)
  save_model(m1, verbose = FALSE)
  suppressMessages(seal_model("svgc"))
  sc <- interpolate_model(m1, name = "svgc_scen", path = sv_root("scenarios", "svgc_scen"))
  saved <- suppressMessages(save_scenario(sc, verbose = FALSE))
  suppressMessages(unseal_model("svgc", force = TRUE))
  save_model(sv_mod("svgc", fixom = 2), verbose = FALSE)

  v <- list_versions("svgc", type = "model")
  expect_identical(names(v), c("status", "name", "type", "version", "sealed_at",
                               "refs", "size_mb"))
  expect_setequal(v$status, c("current", "in use"))
  expect_s3_class(v$sealed_at, "POSIXct")
  expect_identical(v$version[v$status == "current"], "(unsealed)")
  vd <- list_versions("svgc", type = "model", details = TRUE)
  expect_identical(vd$dependents[vd$status == "in use"], "scenario svgc_scen")
  expect_true("svgc" %in% list_versions()$name)

  # a referenced version: marked, it is still kept, and the entry too
  suppressMessages(mark_delete("svgc", type = "model", version = "unused"))
  expect_identical(list_versions("svgc", type = "model")$status, c("current", "in use"))
  h_old <- vd$hash[vd$status == "in use"]
  suppressMessages(mark_delete("svgc", type = "model", version = substr(h_old, 1, 8)))
  dm <- suppressMessages(delete_marked(types = "model"))
  expect_identical(dm$action[!is.na(dm$version)], "skipped_referenced")
  suppressMessages(mark_delete("svgc", type = "model"))
  dm <- suppressMessages(delete_marked(types = "model"))
  expect_identical(dm$action[is.na(dm$version) & dm$name == "svgc"], "skipped_referenced")
  suppressMessages(unmark_delete("svgc", type = "model"))

  # the dependent gone: unused but still kept until deleted from the marks
  unlink(saved@path, recursive = TRUE)
  suppressMessages(unmark_delete("svgc", type = "model", version = substr(h_old, 1, 8)))
  expect_identical(list_versions("svgc", type = "model")$status, c("current", "unused"))
  expect_identical(nrow(subset(suppressMessages(delete_marked(types = "model")),
                               !is.na(version))), 0L)
  suppressMessages(mark_delete("svgc", type = "model", version = "unused"))
  expect_identical(list_versions("svgc", type = "model")$status, c("current", "marked"))
  dm <- suppressMessages(delete_marked(types = "model"))
  expect_identical(dm$action[!is.na(dm$version)], "would_delete")
  sealed_dir <- fp(sv_root("models"), "svgc", h_old)
  expect_true(dir.exists(sealed_dir))
  suppressMessages(delete_marked(types = "model", dry_run = FALSE))
  expect_false(dir.exists(sealed_dir))
  expect_identical(list_versions("svgc", type = "model")$status, "current")
  expect_identical(sv_fixom(load_model("svgc", verbose = FALSE)), 2)
})

test_that("a scenario on a sealed model keeps only the repositories it adds", {
  sv_local()
  unlink(sv_root(), recursive = TRUE)
  m <- sv_mod("svown", fixom = 1)
  save_model(m, verbose = FALSE)
  suppressMessages(seal_model("svown"))
  h <- yaml::read_yaml(fp(sv_root("models"), "svown", "model.yml"))$hash

  extra <- newCommodity("XTRA", unit = "MWh")
  sc <- suppressMessages(interpolate_model(m, extra, name = "svown_cap",
                                           path = sv_root("scenarios", "svown_cap")))
  expect_true(length(sc@misc$added) == 1L)
  saved <- suppressMessages(save_scenario(sc, verbose = FALSE))
  mf <- yaml::read_yaml(fp(saved@path, "scenario.yml"))
  expect_identical(mf$model$source, "ref")
  expect_identical(mf$model$hash, h)
  expect_identical(unlist(mf$model$added), sc@misc$added)
  # the shell holds the scenario's repository only
  e <- new.env()
  load(fp(saved@path, "scen.RData"), envir = e)
  raw <- get(ls(e)[1], envir = e)
  expect_identical(names(raw@model@data), sc@misc$added)

  back <- suppressMessages(load_scenario(saved@path, env = NULL, verbose = FALSE))
  expect_setequal(names(back@model@data), names(sc@model@data))
  expect_true("XTRA" %in% names(back@model@data[[sc@misc$added]]@data))
  expect_identical(sv_fixom(back@model), 1)
  # recorded on the sealed base model
  expect_true(any(grepl("scenario svown_cap",
                        list_versions("svown", type = "model", details = TRUE)$dependents,
                        fixed = TRUE)))

  # a scenario on an unsealed model copies it in
  save_model(sv_mod("svown2", fixom = 1), verbose = FALSE)
  sc2 <- suppressMessages(interpolate_model(sv_mod("svown2", fixom = 1), extra,
                                            name = "svown2_cap",
                                            path = sv_root("scenarios", "svown2_cap")))
  s2 <- suppressMessages(save_scenario(sc2, verbose = FALSE))
  expect_identical(yaml::read_yaml(fp(s2@path, "scenario.yml"))$model$source, "embedded")
})
