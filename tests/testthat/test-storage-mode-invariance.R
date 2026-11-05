# =========================================================================== #
# Storage-mode invariance: a reader must not be able to tell whether a
# scenario's tables are in memory or in its on-disk store.
#
# This is the suite the demand defect slipped past. `check_feasibility()` read
# `@data` directly, so on a saved scenario it saw no demand at all and
# reported "no contradiction" -- the failure was silent, and no test compared
# the two storage modes. Fixtures: helper-storage-mode.R.
# =========================================================================== #

# --------------------------------------------------------------------------- #
# The fixture itself. If the on-disk scenario still held its tables in the
# slot, every test below would pass while proving nothing, so the guard comes
# first and is an assertion, not a skip.
# --------------------------------------------------------------------------- #

test_that("the on-disk fixture really has empty slots", {
  p <- sm_pair()
  params <- sm_params(p$mem)
  expect_gt(length(params), 10L)
  # on disk NOTHING is left in a slot, which is what makes the rest of this
  # suite meaningful; in memory most tables are in their slot, the exceptions
  # being the rule-defined maps covered by the next test
  expect_equal(sm_n_in_slot(p$disk, params), 0L)
  expect_gt(sm_n_in_slot(p$mem, params), length(params) * 0.8)
})

test_that("a map defined by a rule has no table in EITHER mode", {
  # the third thing `get_data_slot()` resolves and a slot read cannot: these
  # maps hold no table at all, their tuples are enumerated on demand
  p <- sm_pair()
  params <- sm_params(p$mem)
  ruled <- params[vapply(params, function(k) {
    NROW(p$mem@modInp@parameters[[k]]@data) == 0L
  }, logical(1))]
  expect_gt(length(ruled), 0L)
  for (k in ruled) {
    expect_equal(NROW(p$disk@modInp@parameters[[k]]@data), 0L, info = k)
    expect_gt(NROW(get_data_slot(p$mem@modInp@parameters[[k]],
                                 optional = TRUE)), 0L)
    expect_equal(sm_norm(get_data_slot(p$mem@modInp@parameters[[k]],
                                       optional = TRUE)),
                 sm_norm(get_data_slot(p$disk@modInp@parameters[[k]],
                                       optional = TRUE)), info = k)
  }
})

test_that("a bare @data read DIFFERS between the modes", {
  # the hazard this suite exists for: if this ever stops differing, the
  # accessors are no longer load-bearing and the rest of the suite is moot
  p <- sm_pair()
  n_mem <- NROW(p$mem@modInp@parameters[["pDemand"]]@data)
  n_disk <- NROW(p$disk@modInp@parameters[["pDemand"]]@data)
  expect_gt(n_mem, 0L)
  expect_equal(n_disk, 0L)
})

# --------------------------------------------------------------------------- #
# The accessors
# --------------------------------------------------------------------------- #

test_that("get_data_slot() returns the same table in both modes", {
  p <- sm_pair()
  params <- sm_params(p$mem)
  diffs <- Filter(Negate(is.null), lapply(params, function(k) {
    a <- sm_norm(get_data_slot(p$mem@modInp@parameters[[k]], optional = TRUE))
    b <- sm_norm(get_data_slot(p$disk@modInp@parameters[[k]], optional = TRUE))
    if (isTRUE(all.equal(a, b, check.attributes = FALSE))) NULL else k
  }))
  expect_equal(unlist(diffs), NULL)
})

test_that("getData() returns the same table in both modes", {
  p <- sm_pair()
  for (k in c("pDemand", "pSupAva", "pTechStock", "pTechCap2AInp")) {
    a <- sm_norm(getData(p$mem, name = k, merge = TRUE))
    b <- sm_norm(getData(p$disk, name = k, merge = TRUE))
    expect_equal(a, b, info = k)
  }
})

# --------------------------------------------------------------------------- #
# The readers built on them
# --------------------------------------------------------------------------- #

test_that("check_feasibility() agrees in both modes when clean", {
  p <- sm_pair()
  fm <- check_feasibility(p$mem, verbose = FALSE)
  fd <- check_feasibility(p$disk, verbose = FALSE)
  expect_equal(sm_norm(fm), sm_norm(fd))
  expect_equal(attr(fm, "retirement_actual"), attr(fd, "retirement_actual"))
})

test_that("check_feasibility() finds the same contradiction in both modes", {
  # land is now too small for the standing capacity, so there IS something to
  # find -- a check that only agrees on "nothing" agrees too cheaply
  p <- local({
    pr_local()
    old_sp <- set_scenarios_path(pr_root("scenarios"))
    on.exit(set_scenarios_path(old_sp), add = TRUE)
    mem <- suppressMessages(interpolate_model(
      sm_model(name = "SMC", land = 0.5, per_gw = 2), name = "SMC",
      verbose = FALSE))
    disk <- withr::with_options(
      list(energyRt.ondisk_min_rows = 1L,
           energyRt.param_ref_min_rows = 1e12), {
        s <- suppressMessages(save_scenario(mem, verbose = FALSE))
        suppressMessages(load_scenario(s@path, env = NULL, verbose = FALSE))
      })
    list(mem = mem, disk = disk)
  })
  fm <- check_feasibility(p$mem, verbose = FALSE)
  fd <- check_feasibility(p$disk, verbose = FALSE)
  expect_gt(nrow(fm), 0L)
  expect_equal(sm_norm(fm), sm_norm(fd))
  expect_true(all(fm$check == "aux-from-stock"))
})

test_that("a user constraint is scoped the same from either base", {
  # `interpolate_model(base =)` is where an on-disk parameter reaches the
  # constraint builder: its milestone reads decide the constraint's rows
  p <- sm_pair()
  sm <- suppressMessages(interpolate_model(sm_model(), sm_cap(),
                                           name = "SMB_MEM",
                                           base = p$mem, verbose = FALSE))
  sd <- suppressMessages(interpolate_model(sm_model(), sm_cap(),
                                           name = "SMB_DISK",
                                           base = p$disk, verbose = FALSE))
  for (k in c("pCnsRhsSM_CAP", "mCnsForEachSM_CAP")) {
    a <- sm_norm(get_data_slot(sm@modInp@parameters[[k]], optional = TRUE))
    b <- sm_norm(get_data_slot(sd@modInp@parameters[[k]], optional = TRUE))
    expect_equal(a, b, info = k)
    expect_gt(NROW(a), 0L)
  }
})
