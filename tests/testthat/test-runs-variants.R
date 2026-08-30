# Own-problem variants (layout 3, S5): a named variant owns its interpolated
# problem at runs/<variant>/{variant.yml, modInp/, variant.RData}, shared by
# all its solves; read_solution(run=) swaps the in-memory problem when
# crossing a problem boundary. Fixtures from helper-stock-path.R — the two
# problems differ in the exogenous stock (100 vs 50), so their objectives
# (fixom on capacity) and pTechStock values differ observably.

vr_path <- gsub("[\\/]+", "/", file.path(tempdir(), "variants-suite", "vr"))

vr_obj <- function(scen) {
  as.numeric(get_variable(scen, "vObjective", data = TRUE)$value[1])
}
vr_stock <- function(scen) {
  sort(unique(suppressMessages(
    getData(scen, "pTechStock", merge = TRUE))$value))
}

vr_state <- local({
  cache <- new.env()
  function() {
    if (!is.null(cache$st)) return(cache$st)
    unlink(vr_path, recursive = TRUE)
    # base problem: stock 100
    base <- solve_scenario(interpolate_model(
      sp_tech(c(100, 100, 100), optret = FALSE, name = "vr"),
      name = "vr", path = vr_path))
    base_saved <- suppressMessages(save_scenario(base, verbose = FALSE))
    # variant problem: stock 50, interpolated into the same scenario folder
    alt <- solve_scenario(
      interpolate_model(sp_tech(c(50, 50, 50), optret = FALSE, name = "vr"),
                        name = "vr", path = vr_path, overwrite = TRUE),
      variant = "low")
    alt_saved <- suppressMessages(save_scenario(alt, verbose = FALSE))
    cache$st <- list(base = base_saved, alt = alt_saved,
                     obj_base = vr_obj(base), obj_alt = vr_obj(alt),
                     solve_lbl = base_saved@misc$run)
    cache$st
  }
})

test_that("a variant's solve and problem land under runs/<variant>/", {
  skip_if_no_solver()
  st <- vr_state()
  expect_identical(st$alt@misc$variant, "low")
  vdir <- file.path(vr_path, "runs", "low")
  expect_true(dir.exists(file.path(vdir, st$solve_lbl, "solver", "output")))
  expect_true(file.exists(file.path(vdir, st$solve_lbl, "run.yml")))
  rec <- yaml::read_yaml(file.path(vdir, st$solve_lbl, "run.yml"))
  expect_identical(rec$variant, "low")
  expect_identical(rec$modinp, "own")

  # the variant owns its problem: manifest + modInp store + swap file
  expect_true(file.exists(file.path(vdir, "variant.yml")))
  vmf <- yaml::read_yaml(file.path(vdir, "variant.yml"))
  expect_identical(vmf$name, "low")
  expect_identical(vmf$modinp, "own")
  expect_true(dir.exists(file.path(vdir, "modInp", "parameters")))
  expect_true(file.exists(file.path(vdir, "variant.RData")))

  # the base problem is untouched at the scenario level; its one home is
  # scen.RData (problem.RData is retired)
  expect_true(dir.exists(file.path(vr_path, "modInp", "parameters")))
  expect_false(file.exists(file.path(vr_path, "problem.RData")))

  # manifest default follows the last save (the variant's run)
  mf <- yaml::read_yaml(file.path(vr_path, "scenario.yml"))
  expect_identical(mf$default, paste0("low/", st$solve_lbl))

  # the two problems solved to different objectives
  expect_false(isTRUE(all.equal(st$obj_base, st$obj_alt)))
})

test_that("scenario_runs lists base and variant runs with modinp kind", {
  skip_if_no_solver()
  st <- vr_state()
  runs <- scenario_runs(st$alt)
  expect_setequal(runs$run,
                  c(st$solve_lbl, paste0("low/", st$solve_lbl)))
  expect_identical(runs$modinp[runs$variant == "low"], "own")
  expect_identical(runs$modinp[runs$variant == ""], "shared")
})

test_that("a second solve of the variant reuses its single problem store", {
  skip_if_no_solver()
  st <- vr_state()
  sol2 <- solve_scenario(st$alt, force = TRUE, run = "glpk-b")
  expect_identical(sol2@misc$variant, "low")
  expect_true(dir.exists(file.path(vr_path, "runs", "low", "glpk-b",
                                   "solver", "output")))
  expect_equal(vr_obj(sol2), st$obj_alt)
  saved2 <- suppressMessages(save_scenario(sol2, verbose = FALSE))
  # still exactly one modInp store for the variant
  expect_true(dir.exists(file.path(vr_path, "runs", "low", "modInp")))
  expect_false(dir.exists(file.path(vr_path, "runs", "low", "glpk-b",
                                    "modInp")))
})

test_that("read_solution(run=) swaps the problem across the boundary", {
  skip_if_no_solver()
  st <- vr_state()
  fresh <- suppressMessages(load_scenario(vr_path, env = NULL,
                                          verbose = FALSE))
  # last save was the variant: it is the active problem
  expect_identical(fresh@misc$variant, "low")
  expect_equal(vr_obj(fresh), st$obj_alt)
  expect_identical(vr_stock(fresh), 50)

  # switch to the BASE run: problem (settings + modInp) swaps with it
  to_base <- read_solution(fresh, run = st$solve_lbl, echo = FALSE)
  expect_null(to_base@misc$variant)
  expect_equal(vr_obj(to_base), st$obj_base)
  expect_identical(vr_stock(to_base), 100)
  # solver code restored by the swap (sourceCode dedup round-trip)
  expect_identical(to_base@settings@sourceCode[["GLPK"]],
                   energyRt:::.modelCode[["GLPK"]])

  # and back to the variant
  to_alt <- read_solution(to_base, run = paste0("low/", st$solve_lbl),
                          echo = FALSE)
  expect_identical(to_alt@misc$variant, "low")
  expect_equal(vr_obj(to_alt), st$obj_alt)
  expect_identical(vr_stock(to_alt), 50)
})

test_that("switching to a never-saved problem errors with guidance", {
  skip_if_no_solver()
  st <- vr_state()
  broken <- st$alt
  expect_error(energyRt:::.variant_swap(broken, "no-such-variant"),
               "no saved")
})
