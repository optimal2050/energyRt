# get_status() (R/status.R): the lifecycle reader.
#
# What is worth asserting is that it agrees with the machinery it reports on --
# the registry's view of the same flags, and list_dependents()' view of the
# same references -- and that it keeps provenance apart from coupling. A
# scenario built on a base whose tables were all copied has a `base` and no
# `shares`, and nothing about it blocks unsealing that base; a scenario that
# borrows tables does block it. Reporting those two as one state would make the
# verb useless for deciding what may be unsealed or deleted.
#
# Its own root, not the shared param-refs project: the whole-project form
# counts every saved object, so a sibling test file's leftovers would be
# counted too.

st_root <- function(...) {
  gsub("[\\/]+", "/", file.path(tempdir(), "status-suite", ...))
}

st_local <- function(tag, env = parent.frame()) {
  unlink(st_root(tag), recursive = TRUE)
  old <- list(
    set_models_path(st_root(tag, "models")),
    set_scenarios_path(st_root(tag, "scenarios")),
    set_repositories_path(st_root(tag, "repositories")),
    set_datasets_path(st_root(tag, "datasets")),
    set_registry_file(st_root(tag, "reg.csv")))
  expr <- bquote({
    set_models_path(.(old[[1]]))
    set_scenarios_path(.(old[[2]]))
    set_repositories_path(.(old[[3]]))
    set_datasets_path(.(old[[4]]))
    set_registry_file(.(old[[5]]))
  })
  do.call(on.exit, list(expr, add = TRUE), envir = env)
  invisible(NULL)
}

# `pr_model()` / `pr_repo()` come from helper-param-refs.R: a one-technology
# model with four timeslices, already the fixture for the reference machinery.
st_save_all <- function() {
  repo <- pr_repo()
  mod <- pr_model("stm")
  suppressMessages(save_repository(repo, verbose = FALSE))
  suppressMessages(save_model(mod, verbose = FALSE))
  suppressMessages(save_dataset(data.frame(a = 1:3), name = "std",
                               verbose = FALSE))
  scen <- suppressMessages(interpolate_model(mod, name = "STREF"))
  suppressMessages(save_scenario(scen, verbose = FALSE))
  list(repo = repo, mod = mod, scen = scen)
}

test_that("an empty project is a zero-row tibble, not an error", {
  st_local("empty")
  st <- get_status()
  expect_s3_class(st, "en_status")
  expect_identical(nrow(st), 0L)
  expect_true(all(c("type", "name", "sealed", "marked", "base", "shares",
                    "used_by") %in% names(st)))
})

test_that("every entry kind is reported, and the flags follow the verbs", {
  st_local("flags")
  o <- st_save_all()

  st <- get_status()
  expect_setequal(st$type, c("model", "repository", "dataset", "scenario"))
  expect_false(any(st$sealed))
  expect_false(any(st$marked))
  # outputs_sealed is a scenario-only idea
  expect_true(all(is.na(st$outputs_sealed[st$type != "scenario"])))

  suppressMessages(seal_model(o$mod, verbose = FALSE))
  suppressMessages(seal_repository(o$repo, verbose = FALSE))
  suppressMessages(seal_dataset("std", verbose = FALSE))
  suppressMessages(seal_scenario(o$scen, verbose = FALSE))
  suppressMessages(mark_delete("std", importance = 2, type = "dataset",
                               verbose = FALSE))

  st <- get_status()
  expect_true(all(st$sealed))
  expect_true(all(nzchar(st$sealed_at)))
  expect_true(st$marked[st$type == "dataset"])
  expect_identical(st$importance[st$type == "dataset"], 2)
  expect_false(any(st$marked[st$type != "dataset"]))
})

test_that("an unmarked entry reports no importance, whatever the yaml holds", {
  st_local("unmark")
  st_save_all()
  suppressMessages(mark_delete("std", importance = 2, type = "dataset",
                               verbose = FALSE))
  suppressMessages(unmark_delete("std", type = "dataset", verbose = FALSE))

  # unmark_delete() writes marked_delete = FALSE and leaves the importance and
  # the timestamp behind (R/save-objects.R:2658), so a verb reading the raw
  # manifest would report an importance for an unmarked entry and contradict
  # load_registry() on the same object
  # unmark_delete() clears the importance and the timestamp, so the manifest
  # is readable on its own: an entry with no mark and an importance beside it
  # reads as marked-at-some-tier to anything that does not also test
  # `marked_delete`
  mf <- yaml::read_yaml(fp(get_datasets_path(), "std", "dataset.yml"))
  expect_false(isTRUE(mf$marked_delete))
  expect_null(mf$delete_importance)
  expect_null(mf$marked_at)

  s <- get_status("std", type = "dataset")
  expect_false(s$marked)
  expect_true(is.na(s$importance))
  expect_false(nzchar(s$marked_at))
})

test_that("a mark still round-trips through the manifest", {
  st_local("markrt")
  st_save_all()
  suppressMessages(mark_delete("std", importance = 3, type = "dataset",
                               verbose = FALSE))
  mf <- yaml::read_yaml(fp(get_datasets_path(), "std", "dataset.yml"))
  expect_true(isTRUE(mf$marked_delete))
  expect_identical(as.numeric(mf$delete_importance), 3)
  expect_true(nzchar(as.character(mf$marked_at)))
  s <- get_status("std", type = "dataset")
  expect_true(s$marked)
  expect_identical(s$importance, 3)
})

test_that("ondisk is read from the store, so a name answers it too", {
  st_local("ondisk")
  mod <- pr_model("stm")
  suppressMessages(save_model(mod, verbose = FALSE))
  mem <- suppressMessages(interpolate_model(mod, name = "MEMS"))
  suppressMessages(save_scenario(mem, verbose = FALSE))
  ond <- suppressMessages(interpolate_model(mod, name = "ONDS",
                                            ondisk = TRUE))
  suppressMessages(save_scenario(ond, verbose = FALSE))

  # by name, with no object in hand
  expect_false(get_status("MEMS")$ondisk)
  expect_true(get_status("ONDS")$ondisk)
  # and project-wide
  st <- get_status()
  expect_false(st$ondisk[st$name == "MEMS"])
  expect_true(st$ondisk[st$name == "ONDS"])

  # where isOnDisk() can also be asked the two agree; the store is read
  # because isOnDisk() needs the loaded object and so cannot answer for a name
  # or during the project walk.
  # load_scenario() assigns into an environment and returns invisible(TRUE),
  # so load into this one rather than expecting the object back.
  suppressMessages(load_scenario(ond@path, name = "back_od",
                                 env = environment(), overwrite = TRUE,
                                 verbose = FALSE))
  suppressMessages(load_scenario(mem@path, name = "back_mem",
                                 env = environment(), overwrite = TRUE,
                                 verbose = FALSE))
  expect_true(isOnDisk(back_od))
  expect_true(get_status(back_od)$ondisk)
  expect_false(isOnDisk(back_mem))
  expect_false(get_status(back_mem)$ondisk)
})

test_that("a scenario's input and output seals are reported apart", {
  st_local("split")
  o <- st_save_all()

  suppressMessages(seal_scenario(o$scen, inputs = TRUE, outputs = TRUE,
                                 verbose = FALSE))
  s <- get_status(o$scen)
  expect_true(s$sealed)
  expect_true(s$outputs_sealed)

  suppressMessages(unseal_scenario(o$scen, inputs = FALSE, outputs = TRUE,
                                   verbose = FALSE))
  s <- get_status(o$scen)
  expect_true(s$sealed)
  expect_false(s$outputs_sealed)
})

test_that("the flags agree with the registry's cached copy", {
  st_local("registry")
  o <- st_save_all()
  suppressMessages(seal_scenario(o$scen, verbose = FALSE))
  suppressMessages(mark_delete("std", importance = 1, type = "dataset",
                               verbose = FALSE))

  st <- get_status()
  reg <- suppressMessages(refresh_registry(root = st_root("registry")))
  reg <- reg[reg$type != "run", , drop = FALSE]
  m <- merge(as.data.frame(st[, c("type", "name", "sealed", "marked")]),
             as.data.frame(reg[, c("type", "name", "sealed", "marked_delete")]),
             by = c("type", "name"), suffixes = c(".st", ".reg"))
  expect_identical(nrow(m), nrow(st))
  expect_identical(m$sealed.st, m$sealed.reg)
  expect_identical(m$marked, m$marked_delete)
})

# --- lineage ---------------------------------------------------------------- #

st_base_pair <- function(min_rows) {
  old <- options(energyRt.param_ref_min_rows = min_rows,
                 energyRt.ondisk_min_rows = min_rows)
  on.exit(options(old), add = TRUE)
  mod <- pr_model("stm")
  suppressMessages(save_model(mod, verbose = FALSE))
  base <- suppressMessages(interpolate_model(mod, name = "STREF"))
  suppressMessages(save_scenario(base, verbose = FALSE))
  suppressMessages(seal_scenario(base, verbose = FALSE))
  sub <- suppressMessages(interpolate_model(mod, name = "STSUB", base = base))
  suppressMessages(save_scenario(sub, verbose = FALSE))
  list(base = base, sub = sub)
}

test_that("borrowed tables are coupling and block unsealing the base", {
  st_local("coupled")
  # a threshold of one row makes every table eligible, so the reference path
  # engages on a fixture instead of needing a million-row table
  st_base_pair(1)

  st <- get_status()
  d <- st[st$name == "STSUB", ]
  b <- st[st$name == "STREF", ]
  expect_identical(d$base, "STREF")
  expect_gt(d$shares, 0L)
  expect_identical(b$used_by, 1L)
  expect_identical(b$shares, 0L)
  expect_false(nzchar(d$lineage_warn))

  # the same edges list_dependents() walks
  dep <- list_dependents("STREF", type = "scenario")
  expect_identical(as.integer(b$used_by), nrow(dep))
  expect_true("STSUB" %in% dep$name)

  # and what the coupling actually costs: the base cannot be unsealed
  expect_error(suppressMessages(unseal_scenario("STREF", verbose = FALSE)),
               "is the base of")
})

test_that("a base whose tables were copied is provenance, not coupling", {
  st_local("provenance")
  # the default threshold: nothing in a four-timeslice fixture qualifies, so
  # the tables are copied and the base is only recorded
  st_base_pair(1e6)

  st <- get_status()
  d <- st[st$name == "STSUB", ]
  b <- st[st$name == "STREF", ]
  expect_identical(d$base, "STREF")
  expect_identical(d$shares, 0L)
  expect_identical(b$used_by, 0L)
  expect_false(nzchar(d$lineage_warn))

  # nothing is at risk, so the base unseals cleanly -- the distinction the
  # `shares` column exists to make
  expect_silent(suppressMessages(unseal_scenario("STREF", verbose = FALSE)))
})

test_that("a reference to a scenario that is gone is flagged", {
  st_local("broken")
  p <- st_base_pair(1)
  unlink(p$base@path, recursive = TRUE)

  d <- get_status()
  d <- d[d$name == "STSUB", ]
  expect_true(nzchar(d$lineage_warn))
  expect_match(d$lineage_warn, "missing scenario")
  expect_match(d$lineage_warn, "base folder missing")
})

test_that("used_by counts dependents of every kind, not only scenarios", {
  st_local("usedby")
  repo <- pr_repo()
  mod <- pr_model("stm")
  suppressMessages(save_repository(repo, verbose = FALSE))
  suppressMessages(save_model(mod, verbose = FALSE))
  # `embed_model = FALSE` makes the scenario REFERENCE the model store entry
  # instead of embedding a copy, and only a reference is a dependency edge --
  # .dep_edges() skips an entry whose `source` is "embedded". A reference is
  # allowed only to a SEALED store version, so the seal comes first.
  suppressMessages(seal_model(mod, verbose = FALSE))
  scen <- suppressMessages(interpolate_model(mod, name = "STREF"))
  suppressMessages(save_scenario(scen, embed_model = FALSE, verbose = FALSE))

  st <- get_status()
  expect_identical(st$used_by[st$type == "model"], 1L)
  # filtering the edges on via == "scenario" would report 0 here and
  # contradict the refusal that unsealing the model earns
  expect_identical(as.integer(st$used_by[st$type == "model"]),
                   nrow(list_dependents(mod)))
  expect_true("STREF" %in% list_dependents(mod)$name)

  # and that non-zero used_by is exactly what the refusal cites
  expect_error(suppressMessages(unseal_model("stm", verbose = FALSE)),
               "STREF")
})

test_that("lineage = FALSE reports no used_by rather than a wrong zero", {
  st_local("nolineage")
  st_base_pair(1)
  st <- get_status(lineage = FALSE)
  expect_true(all(is.na(st$used_by)))
  expect_true(all(!nzchar(st$lineage_warn)))
  # shares comes off the object's own manifest, so it survives
  expect_gt(st$shares[st$name == "STSUB"], 0L)
})

# --- dispatch --------------------------------------------------------------- #

test_that("every no-argument form reaches the whole-project view", {
  st_local("dispatch")
  st_save_all()
  expect_identical(nrow(get_status()), 4L)
  expect_identical(nrow(get_status(NULL)), 4L)
  # `x` missing with only later arguments named: UseMethod() sends a missing
  # first argument to the default method, so the generic routes this itself
  expect_identical(nrow(get_status(sizes = TRUE)), 4L)
  expect_identical(nrow(get_status(type = "scenario")), 1L)
})

test_that("sizes are measured only when asked", {
  st_local("sizes")
  st_save_all()
  expect_true(all(is.na(get_status()$size_mb)))
  expect_false(any(is.na(get_status(sizes = TRUE)$size_mb)))
  # one object measures by default: it is a single folder
  expect_false(is.na(get_status("STREF")$size_mb))
})

test_that("an ambiguous or unknown name is refused, not guessed", {
  st_local("refuse")
  st_save_all()
  expect_error(get_status("nope"), "not found in any store")
  expect_error(get_status(42), "takes a model, repository or scenario")
  expect_error(get_status(c("a", "b")), "one name at a time")
  # a dataset has no class to dispatch on, so it is named
  expect_s3_class(get_status("std", type = "dataset"), "en_status")
})
