# Lifecycle status cached in the registry CSV (R/registry.R).
#
# The mark and the seal live in each entry's own manifest; the registry caches
# them so the CSV answers "sealed?" and "marked for deletion?" on its own, for
# any reader. A cache is only as good as the rule that refreshes it, so most of
# this file is about `lazy = TRUE` -- the path that can silently serve a stale
# table.
#
# The two cases worth stating plainly, because modification time alone cannot
# see either: an entry whose directory was DELETED leaves nothing newer than
# the CSV, and a directory RENAMED or copied carries its old manifest mtime.
# Both are caught by comparing the set of manifest paths, not the timestamps.

# Covers the R API: refresh_registry(), load_registry(), add_to_registry(),
# summary.en_registry(). No model entity is exercised, so there is no @covers
# tag -- the coverage matrix indexes model vocabulary only (tests/README.md).

# A minimal project on disk: scenarios and models with manifests only. No
# solver and no interpolation, so these stay fast and backend-independent.
rl_project <- function(env = parent.frame()) {
  root <- file.path(tempdir(), paste0("reglife-", basename(tempfile("p"))))
  dir.create(file.path(root, "scenarios"), recursive = TRUE)
  dir.create(file.path(root, "models"), recursive = TRUE)
  old <- list(
    r = set_registry_file(file.path(root, "energyRt_registry.csv")),
    s = set_scenarios_path(file.path(root, "scenarios")),
    m = set_models_path(file.path(root, "models")))
  expr <- bquote({
    set_registry_file(.(old$r)); set_scenarios_path(.(old$s))
    set_models_path(.(old$m))
  })
  do.call(on.exit, list(expr, add = TRUE), envir = env)
  root
}

rl_scenario <- function(root, name, extra = list()) {
  d <- file.path(root, "scenarios", name)
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  yaml::write_yaml(c(list(class = "scenario", name = name), extra),
                   file.path(d, "scenario.yml"))
  d
}

rl_model <- function(root, name, hash = "abc12345", extra = list()) {
  d <- file.path(root, "models", paste0(name, "@", hash))
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  yaml::write_yaml(c(list(class = "model", name = name, hash = hash), extra),
                   file.path(d, "model.yml"))
  d
}

rl_run <- function(scen_dir, solve, status = "solved") {
  d <- file.path(scen_dir, "runs", solve)
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  yaml::write_yaml(list(status = status, solver_name = "glpk", lang = "GLPK"),
                   file.path(d, "run.yml"))
  d
}

test_that("the lifecycle of each manifest reaches the CSV", {
  root <- rl_project()
  rl_scenario(root, "PLAIN")
  rl_scenario(root, "MARKED",
              list(marked_delete = TRUE, delete_importance = 5,
                   marked_at = "2026-10-11T00:00:00Z"))
  # sealed AND marked: nothing guards a mark on a sealed entry, which is why
  # delete_marked() has a skipped_sealed action -- two columns, not one enum
  rl_scenario(root, "BOTH",
              list(sealed = TRUE, sealed_at = "2026-10-01T00:00:00Z",
                   marked_delete = TRUE, delete_importance = 0))
  rl_model(root, "MOD", extra = list(sealed = TRUE))

  reg <- refresh_registry(root = root)
  g <- function(nm) reg[reg$name == nm, ]

  expect_false(g("PLAIN")$marked_delete)
  expect_false(g("PLAIN")$sealed)
  # never marked => importance is NA, not 0, which would read as "marked at
  # the least valuable tier"
  expect_true(is.na(g("PLAIN")$delete_importance))

  expect_true(g("MARKED")$marked_delete)
  expect_identical(g("MARKED")$delete_importance, 5)
  expect_identical(g("MARKED")$marked_at, "2026-10-11T00:00:00Z")

  expect_true(g("BOTH")$sealed)
  expect_true(g("BOTH")$marked_delete)
  expect_identical(g("BOTH")$delete_importance, 0)
  expect_true(g("MOD")$sealed)

  # typed, so the table can be filtered rather than string-matched
  expect_type(reg$marked_delete, "logical")
  expect_identical(sort(reg$name[reg$marked_delete]), c("BOTH", "MARKED"))
})

test_that("run rows carry their solve status", {
  root <- rl_project()
  d <- rl_scenario(root, "S")
  rl_run(d, "glpk", status = "solved")
  rl_run(d, "highs", status = "failed")
  reg <- refresh_registry(root = root)
  runs <- find_in_registry(reg, type = "run")
  expect_setequal(runs$status, c("solved", "failed"))
  # seal and mark act on entries, never on a run
  expect_false(any(runs$sealed))
  expect_false(any(runs$marked_delete))
})

test_that("the CSV is readable without R: literal TRUE/FALSE, no quotes needed", {
  root <- rl_project()
  rl_scenario(root, "M", list(marked_delete = TRUE, delete_importance = 2))
  refresh_registry(root = root)
  txt <- readLines(get_registry_file())
  hdr <- strsplit(gsub('"', "", txt[1]), ",")[[1]]
  row <- strsplit(gsub('"', "", txt[2]), ",")[[1]]
  expect_identical(row[match("marked_delete", hdr)], "TRUE")
  expect_identical(row[match("sealed", hdr)], "FALSE")
  expect_identical(row[match("delete_importance", hdr)], "2")
})

test_that("a registry written before these columns existed still loads", {
  f <- tempfile(fileext = ".csv")
  # exactly the pre-change column set
  utils::write.csv(data.frame(
    type = "scenario", name = "OLD", hash = "", model_hash = "",
    path = "scenarios/OLD", parent = "", variant = "",
    created = "2026-01-01T00:00:00Z", updated = "2026-01-01T00:00:00Z",
    energyRt_version = "0.89.0", memo = ""), f, row.names = FALSE)

  reg <- load_registry(f)
  expect_identical(nrow(reg), 1L)
  expect_identical(names(reg), energyRt:::.registry_cols)
  # a back-filled column must read FALSE -- NA would make every old project
  # look marked-or-unknown, and TRUE would queue it for deletion
  expect_false(reg$marked_delete)
  expect_false(reg$sealed)
  expect_true(is.na(reg$delete_importance))
})

test_that("the default always rescans", {
  root <- rl_project()
  rl_scenario(root, "A")
  refresh_registry(root = root)
  f <- get_registry_file()
  t0 <- file.mtime(f)
  Sys.sleep(1.1)              # 1 s filesystem timestamp resolution
  reg <- refresh_registry(root = root)
  expect_gt(as.numeric(file.mtime(f)), as.numeric(t0))
  expect_identical(nrow(reg), 1L)
})

test_that("lazy = TRUE skips the scan while nothing has changed", {
  root <- rl_project()
  rl_scenario(root, "A")
  refresh_registry(root = root)
  f <- get_registry_file()
  Sys.sleep(1.1)
  t0 <- file.mtime(f)
  reg <- suppressMessages(refresh_registry(root = root, lazy = TRUE))
  expect_identical(as.numeric(file.mtime(f)), as.numeric(t0))
  expect_identical(nrow(reg), 1L)
})

test_that("lazy = TRUE still rescans after a manifest is touched", {
  root <- rl_project()
  rl_scenario(root, "A")
  refresh_registry(root = root)
  Sys.sleep(1.1)
  rl_scenario(root, "A", list(marked_delete = TRUE, delete_importance = 1))
  reg <- suppressMessages(refresh_registry(root = root, lazy = TRUE))
  expect_true(reg$marked_delete[reg$name == "A"])
})

test_that("lazy = TRUE rescans when an entry was deleted", {
  # mtime alone cannot see this: nothing on disk is newer than the CSV, so a
  # timestamp-only check keeps listing a folder that is gone
  root <- rl_project()
  rl_scenario(root, "A")
  rl_scenario(root, "B")
  expect_identical(nrow(refresh_registry(root = root)), 2L)
  Sys.sleep(1.1)
  unlink(file.path(root, "scenarios", "B"), recursive = TRUE)
  reg <- suppressMessages(refresh_registry(root = root, lazy = TRUE))
  expect_identical(reg$name, "A")
})

test_that("lazy = TRUE rescans when a directory appears with an old mtime", {
  # a move or an archive-preserving copy carries the manifest's old timestamp,
  # so the new entry would look older than the CSV
  root <- rl_project()
  rl_scenario(root, "A")
  refresh_registry(root = root)
  f <- get_registry_file()
  Sys.sleep(1.1)
  d <- rl_scenario(root, "B")
  old <- as.POSIXct("2020-01-01 00:00:00", tz = "UTC")
  Sys.setFileTime(file.path(d, "scenario.yml"), old)
  expect_lt(as.numeric(file.mtime(file.path(d, "scenario.yml"))),
            as.numeric(file.mtime(f)))
  reg <- suppressMessages(refresh_registry(root = root, lazy = TRUE))
  expect_setequal(reg$name, c("A", "B"))
})

test_that("a scenario with no runs refreshes to zero run rows", {
  # the promoted shape: the solution sits at scenario level and runs/ is gone
  root <- rl_project()
  d <- rl_scenario(root, "PROMOTED")
  dir.create(file.path(d, "runs"), showWarnings = FALSE)
  dir.create(file.path(d, "modOut"), showWarnings = FALSE)
  reg <- refresh_registry(root = root)
  expect_identical(nrow(find_in_registry(reg, type = "run")), 0L)
  expect_identical(nrow(find_in_registry(reg, type = "scenario")), 1L)
})

test_that("mark_delete reaches the registry with no refresh in between", {
  root <- rl_project()
  rl_model(root, "MOD")
  refresh_registry(root = root)

  suppressMessages(mark_delete("MOD", importance = 3, type = "model"))
  # deliberately NOT calling refresh_registry(): the verb updates its own row
  reg <- load_registry()
  expect_true(reg$marked_delete[reg$name == "MOD"])
  expect_identical(reg$delete_importance[reg$name == "MOD"], 3)
  expect_true(nzchar(reg$marked_at[reg$name == "MOD"]))

  # unmarking clears the flag but leaves the record of the previous mark --
  # mark_delete() writes marked_delete = FALSE, it does not strip the fields
  suppressMessages(unmark_delete("MOD", type = "model"))
  reg <- load_registry()
  expect_false(reg$marked_delete[reg$name == "MOD"])
  expect_true(nzchar(reg$marked_at[reg$name == "MOD"]))
  mf <- yaml::read_yaml(file.path(root, "models", "MOD@abc12345", "model.yml"))
  expect_identical(as.numeric(mf$delete_importance), 3)

  # sealing lands the same way
  suppressMessages(seal_model("MOD"))
  expect_true(load_registry()$sealed[reg$name == "MOD"])
})

test_that("marking in a project with no registry does not create one", {
  # creating a registry is newRegistry(), never a side effect of marking
  root <- rl_project()
  rl_model(root, "MOD")
  expect_false(file.exists(get_registry_file()))
  suppressMessages(mark_delete("MOD", type = "model"))
  expect_false(file.exists(get_registry_file()))
  # and the mark still took effect where it actually lives
  mf <- yaml::read_yaml(file.path(root, "models", "MOD@abc12345", "model.yml"))
  expect_true(isTRUE(mf$marked_delete))
})

test_that("summary() reports counts, lifecycle and drift", {
  root <- rl_project()
  rl_scenario(root, "A")
  rl_scenario(root, "M", list(marked_delete = TRUE, delete_importance = 5))
  rl_scenario(root, "S", list(sealed = TRUE))
  rl_run(rl_scenario(root, "R"), "glpk", status = "solved")
  reg <- refresh_registry(root = root)

  out <- capture.output(res <- summary(reg))
  txt <- paste(out, collapse = "\n")
  expect_match(txt, "Entries: 5")
  expect_match(txt, "1 sealed")
  expect_match(txt, "1 marked for deletion")
  expect_match(txt, "importance 5: 1")
  expect_match(txt, "1 solved")
  # fresh registry: no drift warning
  expect_false(grepl("have changed since", txt))
  expect_s3_class(res, "tbl_df")

  # and it says so once the stores move on
  Sys.sleep(1.1)
  rl_scenario(root, "NEW")
  expect_match(paste(capture.output(summary(reg)), collapse = "\n"),
               "have changed since")
})
