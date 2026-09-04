# The artificial set member (ANYREGION / 0) is a property of the WRITTEN files.
# It used to be persisted into the solved scenario: the region set read
# `R1 R2 ANYREGION`, the year set `2020 0`, and getData() expanded the region
# wildcard over ANYREGION too while leaving the year wildcard 0 unexpanded.

test_that("a folded solve returns a scenario without the artificial members", {
  skip_if_no_solver()
  mod <- fold_model()
  fo <- interpolate_model(mod, name = "rb_fo", fold = FOLD_ALL, verbose = FALSE)
  uf <- interpolate_model(mod, name = "rb_uf", fold = FALSE, verbose = FALSE)
  expect_true("year" %in% unlist(lapply(fold_folded(fo), `[[`, "wildcard_dims")))

  d <- withr::local_tempdir()
  s <- solve_scenario(fo, solver = solver_options$glpk, solver.dir = d,
                      force = TRUE)
  expect_true(isTRUE(s@status$optimal))
  for (dim in c("region", "year", "timeslice", "tech", "comm")) {
    members <- as.data.frame(get_data_slot(s@modInp@parameters[[dim]]))[[dim]]
    expect_false(energyRt:::.fold_any[[dim]]$member %in% members, label = dim)
  }
  # still folded: the substitution re-applies on the next write
  expect_gt(length(unlist(energyRt:::.folded_params(s))), 0L)

  # every folded parameter reads back as the unfolded build has it
  for (nm in names(fold_folded(s))) {
    a <- fold_norm(getData(s, nm, merge = TRUE)[, -(1:2)])
    b <- fold_norm(getData(uf, nm, merge = TRUE)[, -(1:2)])
    expect_equal(a, b, label = nm, ignore_attr = TRUE)
  }

  # and the returned scenario re-solves to the same objective
  s2 <- solve_scenario(s, solver = solver_options$glpk,
                       solver.dir = withr::local_tempdir(), force = TRUE)
  expect_equal(fold_objective(s2), fold_objective(s), tolerance = 1e-9)
  expect_equal(fold_objective(s2), 75.0342, tolerance = 1e-4)
})
