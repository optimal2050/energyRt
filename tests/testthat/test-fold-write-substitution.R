# A folded scenario keeps its wildcards as NA; the generated model source must
# index the artificial set member (ANYREGION) instead. That substitution used
# to live in solve_scenario() only, so `write_script()` emitted a model whose
# every folded lookup missed and silently took the parameter default -- 0 for
# costs and discount rates, Inf for the afs upper bound (which surfaces under
# JuMP as `-Inf * vTechCap`, and under GLPK as a technology forced to zero).
# It now happens on the common write path, so both entry points get it.

# `fold_model()` / `written_source()` are shared with the other fold suites
# through helper-fold.R.

test_that("the map outruns the folded parameter (the condition being guarded)", {
  scen <- interpolate_model(fold_model(), name = "f", fold = TRUE,
                            verbose = FALSE)
  p <- as.data.frame(get_data_slot(scen@modInp@parameters$pTechAfs))
  m <- as.data.frame(get_data_slot(scen@modInp@parameters$meqTechAfsUp))
  # one wildcard row, but a map entry per region
  expect_true(all(is.na(p$region)))
  expect_setequal(m$region, c("R1", "R2"))
})

test_that("write_script substitutes the artificial member on every backend", {
  scen <- interpolate_model(fold_model(), name = "f", fold = TRUE,
                            verbose = FALSE)
  for (sv in c("glpk", "julia_highs_barrier", "pyomo_highs_barrier")) {
    src <- written_source(scen, solver_options[[sv]])
    expect_gt(sum(grepl("ANYREGION", src)), 0L, label = sv)
  }
})

test_that("an unfolded scenario needs no substitution", {
  scen <- interpolate_model(fold_model(), name = "u", fold = FALSE,
                            verbose = FALSE)
  for (sv in c("glpk", "julia_highs_barrier")) {
    src <- written_source(scen, solver_options[[sv]])
    expect_equal(sum(grepl("ANYREGION", src)), 0L, label = sv)
  }
})

test_that("folded and unfolded solve to the same objective", {
  skip_on_cran()
  obj <- function(fold) {
    scen <- interpolate_model(fold_model(), name = paste0("o", fold),
                              fold = fold, verbose = FALSE)
    d <- withr::local_tempdir()
    s <- solve_scenario(scen, solver = solver_options$glpk, solver.dir = d)
    sum(as.data.frame(getData(s, "vObjective", merge = TRUE))$value, na.rm = TRUE)
  }
  expect_equal(obj(TRUE), obj(FALSE), tolerance = 1e-6)
})

# The GAMS writer requires a DENSE scenario -- that is about the missing native
# default, not about folding -- so a sparse scenario, folded or not, is refused
# on density grounds.
test_that("a sparse folded scenario reaches the GAMS writer's density error", {
  scen <- interpolate_model(fold_model(), name = "g", fold = TRUE,
                            sparse = TRUE, verbose = FALSE)
  d <- withr::local_tempdir()
  expect_error(
    write_script(scen, solver.dir = d, solver = solver_options$gams_gdx_cplex),
    "requires a dense scenario"
  )
})

# Densify materialises every default over the domain and the fold collapses
# exactly that back, so a dense build is where folding pays most.
test_that("folding survives a dense interpolation", {
  scen <- interpolate_model(fold_model(), name = "d", fold = TRUE,
                            sparse = FALSE, verbose = FALSE)
  expect_gt(length(unlist(energyRt:::.folded_params(scen))), 0L)
  expect_equal(scen@status$folded, c("region", "timeslice"))
})

# GAMS spells a parameter's declaration and its use identically, so the rewrite
# is confined to non-declaration lines, and GAMS domain-checks a quoted label at
# compile time, so the set members must be included ahead of the equations.
test_that("a dense folded scenario writes GAMS with the member substituted", {
  scen <- interpolate_model(fold_model(), name = "gd", fold = TRUE,
                            sparse = FALSE, verbose = FALSE)
  d <- withr::local_tempdir()
  write_script(scen, solver.dir = d, solver = solver_options$gams_csv_cplex)
  gms <- readLines(file.path(d, "energyRt.gms"), warn = FALSE)
  hits <- grep("'ANYREGION'", gms, fixed = TRUE)
  expect_gt(length(hits), 0L)
  # never inside a declaration block
  expect_false(any(energyRt:::.gams_decl_lines(gms)[hits]))
  # the set include precedes the first equation definition
  inc <- grep("^[$]include[[:space:]]+sets[.]gms", gms)
  eqs <- grep("^eq[A-Za-z0-9_]+.*[.][.]", gms)
  expect_length(inc, 1L)
  expect_lt(inc, eqs[1])
  # set members (with the artificial one) come through sets.gms, not data.gms
  sets <- readLines(file.path(d, "sets.gms"), warn = FALSE)
  expect_true(any(grepl("input/region.gms", sets, fixed = TRUE)))
  dat <- readLines(file.path(d, "data.gms"), warn = FALSE)
  expect_false(any(grepl("input/region.gms", dat, fixed = TRUE)))
  reg <- readLines(file.path(d, "input", "region.gms"), warn = FALSE)
  expect_true(any(grepl("ANYREGION", reg, fixed = TRUE)))
  # the year wildcard is materialised for GAMS: no `'0'` label anywhere
  expect_false(any(grepl("'0'", gms, fixed = TRUE)))
})

# The artificial member belongs to the written files: the scenario handed back
# by write_script() / solve_scenario() still carries the NA wildcard.
test_that("the written scenario keeps its NA wildcards", {
  scen <- interpolate_model(fold_model(), name = "w", fold = TRUE,
                            verbose = FALSE)
  d <- withr::local_tempdir()
  out <- write_script(scen, solver.dir = d, solver = solver_options$glpk)
  reg <- as.data.frame(get_data_slot(out@modInp@parameters$region))$region
  expect_false("ANYREGION" %in% reg)
  p <- as.data.frame(get_data_slot(out@modInp@parameters$pTechAfs))
  expect_true(all(is.na(p$region)))
  # the substitution still fires on the next write
  src <- written_source(out, solver_options$glpk)
  expect_gt(sum(grepl("ANYREGION", src)), 0L)
})
