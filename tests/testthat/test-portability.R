# Portability: what a solve left on disk, dropping what regenerates, removing
# the machine, and preparing a scenario to share (R/portability.R).
#
# The properties that would fail silently:
#
#   * `output/` is deletable only once `modOut/` exists -- `output/` is what
#     read_solution() reads, `modOut/` is the imported copy, and
#     `run.yml$status == "solved"` means the solution reached MEMORY, not disk;
#   * the reported scratch size must equal what deletion frees, or the two
#     definitions have drifted;
#   * preparing a copy must change the COPY -- an early version cleaned the
#     ORIGINAL, because getScenario() resolves by NAME through the registry and
#     the store, both of which still point at the source;
#   * @misc$onDisk must survive stripping: every path rebase is guarded on it
#     being non-empty, so clearing it makes the rebase a no-op AND destroys the
#     "moved vs never written" discriminator, after which results come back as
#     zero rows with no error;
#   * the shared folder must actually load elsewhere, which none of the above
#     implies.

# @covers vSupOut depth=S backends=glpk

af_local <- function(name, env = parent.frame()) {
  root <- gsub("[\\\\/]+", "/", file.path(tempdir(), "artifacts", name))
  unlink(root, recursive = TRUE)
  old_s <- set_scenarios_path(file.path(root, "scenarios"))
  old_r <- set_registry_file(file.path(root, "energyRt_registry.csv"))
  do.call(on.exit, list(bquote({
    set_scenarios_path(.(old_s)); set_registry_file(.(old_r))
  }), add = TRUE), envir = env)
  invisible(root)
}

af_solved <- function(name) {
  cal <- newCalendar(timetable = make_timetable(struct = list(ANNUAL = "ANNUAL")),
                     name = "af1")
  mod <- newModel(name, region = "R1", discount = 0, calendar = cal,
    horizon = newHorizon(2020),
    repo = newRepository("afr",
      newCommodity("ELC", timeframe = "ANNUAL"),
      newSupply("SUP", commodity = "ELC", region = "R1",
                supply = data.frame(region = "R1", cost = 5)),
      newDemand("DEM", commodity = "ELC", region = "R1",
                demand = data.frame(region = "R1", year = 2020L,
                                    timeslice = "ANNUAL", demand = 2))))
  suppressMessages(suppressWarnings(
    solve_scenario(interpolate_model(mod, name = name), echo = FALSE)))
}

af_save <- function(s) {
  suppressMessages(suppressWarnings(save_scenario(s, verbose = FALSE)))
}

# --- the listing --------------------------------------------------------- #

test_that("a solved-but-unsaved run reports its solution as NOT imported", {
  skip_if_no_solver()
  af_local("unsaved")
  s <- af_solved("af_unsaved")

  a <- scenario_artifacts(s)
  expect_identical(nrow(a), 1L)
  expect_false(a$imported[1])
  # status says "solved" -- that is exactly the trap: it means the solution
  # reached memory, not disk
  expect_identical(a$status[1], "solved")
})

test_that("saving imports the solution and the scratch becomes redundant", {
  skip_if_no_solver()
  af_local("saved")
  s <- af_save(af_solved("af_saved"))

  a <- scenario_artifacts(s)
  expect_true(a$imported[1])
  expect_gt(a$solution_mb[1], 0)
  expect_gt(a$scratch_mb[1], 0)
  expect_match(a$suggest[1], "redundant")
})

test_that("the reported scratch size is what deletion actually frees", {
  skip_if_no_solver()
  af_local("size")
  s <- af_save(af_solved("af_size"))

  before <- scenario_artifacts(s)$scratch_mb[1]
  d <- drop_solver_outputs(s, dry_run = FALSE, verbose = FALSE)
  expect_equal(d$freed_mb[1], before)
  # and nothing is left to clean
  expect_equal(scenario_artifacts(s)$scratch_mb[1], 0)
  expect_identical(scenario_artifacts(s)$suggest[1], "")
})

# --- the rule ------------------------------------------------------------- #

test_that("output/ is never offered while the solution is not imported", {
  skip_if_no_solver()
  af_local("rule")
  s <- af_solved("af_rule")           # solved, NOT saved

  a <- scenario_artifacts(s)
  expect_false(a$imported[1])
  out <- energyRt:::.art_output_dir(energyRt:::.run_solver_dir(a$path[1]),
                                    a$path[1])
  skip_if(!dir.exists(out), "no solver output directory")

  paths <- energyRt:::.art_scratch_paths(a$path[1], imported = FALSE)
  expect_false(out %in% paths)

  # and after a real deletion the solver output is still there
  drop_solver_outputs(s, runs = a$run[1], dry_run = FALSE, verbose = FALSE)
  expect_true(dir.exists(out))
})

test_that("output/ IS offered once the solution is imported", {
  skip_if_no_solver()
  af_local("rule2")
  s <- af_save(af_solved("af_rule2"))

  a <- scenario_artifacts(s)
  out <- energyRt:::.art_output_dir(energyRt:::.run_solver_dir(a$path[1]),
                                    a$path[1])
  skip_if(!dir.exists(out), "no solver output directory")
  expect_true(out %in% energyRt:::.art_scratch_paths(a$path[1],
                                                     imported = TRUE))
})

test_that("cleanup never costs the solution", {
  skip_if_no_solver()
  af_local("keep")
  s <- af_save(af_solved("af_keep"))
  p <- s@path

  invisible(load_scenario(p, overwrite = TRUE, verbose = FALSE))
  before <- as.data.frame(getData(getScenario("af_keep"), "vSupOut",
                                  merge = TRUE))

  drop_solver_outputs(s, dry_run = FALSE, verbose = FALSE)

  invisible(load_scenario(p, overwrite = TRUE, verbose = FALSE))
  after <- as.data.frame(getData(getScenario("af_keep"), "vSupOut",
                                 merge = TRUE))

  expect_identical(nrow(after), nrow(before))
  expect_equal(after$value, before$value)
})

# --- guards --------------------------------------------------------------- #

test_that("dry run is the default and removes nothing", {
  skip_if_no_solver()
  af_local("dry")
  s <- af_save(af_solved("af_dry"))
  before <- scenario_artifacts(s)$scratch_mb[1]

  d <- drop_solver_outputs(s, verbose = FALSE)
  expect_identical(d$action[1], "would_delete")
  expect_equal(scenario_artifacts(s)$scratch_mb[1], before)
})

test_that("a sealed scenario is skipped", {
  skip_if_no_solver()
  af_local("sealed")
  s <- af_save(af_solved("af_sealed"))
  suppressMessages(seal_scenario(s, verbose = FALSE))
  on.exit(suppressMessages(unseal_scenario(s, verbose = FALSE)), add = TRUE)

  a <- scenario_artifacts(s)
  expect_true(a$sealed[1])
  expect_identical(a$suggest[1], "")
  d <- drop_solver_outputs(s, runs = a$run[1], dry_run = FALSE,
                           verbose = FALSE)
  expect_identical(d$action[1], "skipped_sealed")
  expect_gt(scenario_artifacts(s)$scratch_mb[1], 0)
})

test_that("an unknown run is refused, naming the available ones", {
  skip_if_no_solver()
  af_local("unknown")
  s <- af_save(af_solved("af_unknown"))
  expect_error(drop_solver_outputs(s, runs = "nope"), "No such run")
})

test_that("dir_size counts dotfiles and survives a vanished file", {
  d <- file.path(tempdir(), "dsz"); unlink(d, recursive = TRUE)
  dir.create(d, recursive = TRUE)
  writeLines("x", file.path(d, "visible.txt"))
  writeLines("yy", file.path(d, ".hidden"))
  # both files count -- the old implementation missed the dotfile
  expect_gt(energyRt:::dir_size(d), 2)
  expect_equal(energyRt:::dir_size(file.path(d, "gone"), missing = "zero"), 0)
  expect_error(energyRt:::dir_size(file.path(d, "gone")), "does not exist")
})

su_local <- function(name, env = parent.frame()) {
  root <- gsub("[\\\\/]+", "/", file.path(tempdir(), "strip-ui", name))
  unlink(root, recursive = TRUE)
  old_s <- set_scenarios_path(file.path(root, "scenarios"))
  old_r <- set_registry_file(file.path(root, "energyRt_registry.csv"))
  do.call(on.exit, list(bquote({
    set_scenarios_path(.(old_s)); set_registry_file(.(old_r))
  }), add = TRUE), envir = env)
  root
}

su_scen <- function(name) {
  cal <- newCalendar(timetable = make_timetable(struct = list(ANNUAL = "ANNUAL")),
                     name = "su1")
  mod <- newModel(name, region = "R1", discount = 0, calendar = cal,
    horizon = newHorizon(2020),
    repo = newRepository("sur",
      newCommodity("ELC", timeframe = "ANNUAL"),
      newSupply("SUP", commodity = "ELC", region = "R1",
                supply = data.frame(region = "R1", cost = 5)),
      newDemand("DEM", commodity = "ELC", region = "R1",
                demand = data.frame(region = "R1", year = 2020L,
                                    timeslice = "ANNUAL", demand = 2))))
  s <- suppressMessages(suppressWarnings(
    solve_scenario(interpolate_model(mod, name = name), echo = FALSE)))
  suppressMessages(suppressWarnings(save_scenario(s, verbose = FALSE)))
}

su_obj <- function(dir) {
  e <- new.env(parent = emptyenv())
  nm <- load(file.path(dir, "scen.RData"), envir = e)
  get(nm[1], envir = e)
}

su_run_yml <- function(dir) {
  f <- list.files(dir, "^run\\.yml$", recursive = TRUE, full.names = TRUE)
  if (!length(f)) return(NULL)
  yaml::read_yaml(f[1])
}

# --- the leak inventory --------------------------------------------------- #

test_that("a saved scenario carries the machine before stripping", {
  skip_if_no_solver()
  su_local("before")
  s <- su_scen("su_before")

  o <- su_obj(s@path)
  expect_true(nzchar(o@path))
  # the one absolute path the load-time rebase does not reconstruct
  expect_true(nzchar(paste(o@settings@solver$cmdline, collapse = "")))
  y <- su_run_yml(s@path)
  expect_true(nzchar(y$user %||% ""))
  expect_true(nzchar(y$hostname %||% ""))
})

test_that("a cleaned copy has no paths, command line or provenance", {
  skip_if_no_solver()
  root <- su_local("copy")
  s <- su_scen("su_copy")
  out <- file.path(root, "share", "su_copy")

  r <- strip_user_info(s, path = out, verbose = FALSE)
  expect_false(r$dry_run)

  o <- su_obj(out)
  expect_identical(o@path, "")
  expect_identical(paste(o@settings@solver$cmdline, collapse = ""), "")
  expect_identical(o@modInp@misc$path, "")

  y <- su_run_yml(out)
  expect_identical(y$user, "")
  expect_identical(y$hostname, "")
  expect_identical(y$cmdline, "")

  expect_false(file.exists(file.path(out, "logfile.csv")))
  expect_identical(r$remaining, character(0))
})

test_that("scope = 'store' keeps provenance and still fixes the paths", {
  skip_if_no_solver()
  root <- su_local("store")
  s <- su_scen("su_store")
  out <- file.path(root, "share", "su_store")

  strip_user_info(s, path = out, scope = "store", verbose = FALSE)
  y <- su_run_yml(out)
  # who ran it is the point on a shared drive
  expect_true(nzchar(y$user %||% ""))
  expect_true(nzchar(y$hostname %||% ""))
  # but the machine-local path is gone either way
  expect_identical(y$cmdline, "")
  expect_identical(su_obj(out)@path, "")
})

test_that("structural bookkeeping survives the strip", {
  skip_if_no_solver()
  root <- su_local("keep")
  s <- su_scen("su_keep")
  out <- file.path(root, "share", "su_keep")
  strip_user_info(s, path = out, verbose = FALSE)

  o <- su_obj(out)
  p <- o@modInp@parameters[[1]]
  # onDisk is not user-identifying, and every rebase is guarded on it
  expect_false(is.null(p@misc$onDisk))
  expect_false(is.null(o@modInp@misc$onDisk))
  # and the fields the loader derives its roots from
  expect_false(is.null(o@misc$sourceCode_default))
})

# --- the guards ----------------------------------------------------------- #

test_that("in place is a dry run unless confirmed", {
  skip_if_no_solver()
  su_local("dry")
  s <- su_scen("su_dry")
  before <- su_obj(s@path)

  r <- strip_user_info(s, verbose = FALSE)
  expect_true(r$dry_run)
  expect_identical(su_obj(s@path)@path, before@path)

  strip_user_info(s, confirm = TRUE, verbose = FALSE)
  expect_identical(su_obj(s@path)@path, "")
})

# --- sharing -------------------------------------------------------------- #

test_that("prepare_for_sharing leaves the ORIGINAL untouched", {
  skip_if_no_solver()
  root <- su_local("orig")
  s <- su_scen("su_orig")
  n_before <- length(list.files(s@path, recursive = TRUE))

  out <- file.path(root, "outbox", "su_orig")
  prepare_for_sharing(s, path = out, verbose = FALSE)

  # the bug this pins: cleaning "the copy" through getScenario() resolves the
  # name back to the source and empties it instead
  expect_identical(length(list.files(s@path, recursive = TRUE)), n_before)
  expect_lt(length(list.files(out, recursive = TRUE)), n_before)
})

test_that("prepare_for_sharing refuses to write in place", {
  skip_if_no_solver()
  su_local("inplace")
  s <- su_scen("su_inplace")
  expect_error(prepare_for_sharing(s), "`path` is required")
})

test_that("the shared copy loads in a different project and reads the same", {
  skip_if_no_solver()
  root <- su_local("trip")
  s <- su_scen("su_trip")
  ref <- as.data.frame(getData(s, "vSupOut", merge = TRUE))

  out <- file.path(root, "outbox", "su_trip")
  r <- prepare_for_sharing(s, path = out, verbose = FALSE)
  expect_identical(r$remaining, character(0))

  # a recipient: different store root, no registry row
  root2 <- gsub("[\\\\/]+", "/", file.path(tempdir(), "strip-ui", "recipient"))
  unlink(root2, recursive = TRUE)
  dir.create(file.path(root2, "scenarios"), recursive = TRUE)
  file.copy(out, file.path(root2, "scenarios"), recursive = TRUE)
  old_s <- set_scenarios_path(file.path(root2, "scenarios"))
  old_r <- set_registry_file(file.path(root2, "energyRt_registry.csv"))
  on.exit({ set_scenarios_path(old_s); set_registry_file(old_r) }, add = TRUE)

  p2 <- file.path(root2, "scenarios", "su_trip")
  invisible(load_scenario(p2, overwrite = TRUE, verbose = FALSE))
  got <- as.data.frame(getData(getScenario("su_trip"), "vSupOut",
                               merge = TRUE))
  expect_identical(nrow(got), nrow(ref))
  expect_equal(got$value, ref$value)
})
