# =========================================================================== #
# Feature family: parent-levels -- the `interpolate_model(parent_levels = )`
# switch that gives a variable rows at the levels COARSER than the one it
# occupies, each defined by a roll-up of its finer rows.
#
# This file covers the SWITCH. The per-variable roll-up equations, the ON/OFF
# solution equivalence and the size benchmark arrive with each variable as its
# equation lands; until then `.parent_level_vars` is empty and the switch is a
# guaranteed no-op, which is what these tests pin.
#
# Why a supported SET rather than "any variable": a parent row with no equation
# to define it is a FREE variable. The model still solves, and is silently
# wrong. So naming a variable whose equation does not exist is refused.
# =========================================================================== #

test_that("family parent-levels: the argument resolves to a variable set", {
  res <- function(x) energyRt:::.parent_levels_resolve(x)
  sup <- energyRt:::.parent_level_vars

  expect_identical(res(FALSE), character())
  expect_identical(res(NULL), character())
  expect_identical(res(TRUE), sup)          # every SUPPORTED variable
  expect_identical(res(character()), character())
})

test_that("family parent-levels: a variable with no roll-up equation is refused", {
  # the whole point of the gate -- parent rows with no defining equation would
  # be free variables, and the model would solve anyway
  expect_error(energyRt:::.parent_levels_resolve("vTechOut"),
               "no roll-up equation yet")
  expect_error(energyRt:::.parent_levels_resolve("vTechOut"),
               "free variable")
})

test_that("family parent-levels: an unknown variable name is refused", {
  expect_error(energyRt:::.parent_levels_resolve("vNotAVariable"),
               "unknown variable")
})

test_that("family parent-levels: the argument is type-checked", {
  expect_error(energyRt:::.parent_levels_resolve(NA), "must be TRUE, FALSE")
  expect_error(energyRt:::.parent_levels_resolve(c(TRUE, FALSE)),
               "must be TRUE, FALSE")
  expect_error(energyRt:::.parent_levels_resolve(1), "got numeric")
})

test_that("family parent-levels: every supported variable really is supported", {
  # drift guard: a name in `.parent_level_vars` with no equation behind it is
  # the exact failure the set exists to prevent, so assert the name is a real
  # variable and that it has a timeslice or region dimension to have parents in
  for (v in energyRt:::.parent_level_vars) {
    expect_true(v %in% names(energyRt:::.variable_mapping),
                label = paste0("`", v, "` is a known variable"))
    dims <- energyRt:::.variable_set[[v]]
    expect_true(any(c("timeslice", "region") %in% dims),
                label = paste0("`", v, "` has a level dimension"))
  }
  # and `vOutTot`/`vInpTot` are NOT governed by the switch: they carry their
  # parent rows unconditionally because eqOutTot/eqInpTot are written against
  # them, so listing them here would imply the switch could take them away
  expect_false("vOutTot" %in% energyRt:::.parent_level_vars)
  expect_false("vInpTot" %in% energyRt:::.parent_level_vars)
})

test_that("family parent-levels: interpolation accepts it and records it", {
  skip_if_no_fixtures()
  env <- .mapping_fixture_env()
  s <- suppressMessages(suppressWarnings(interpolate_model(
    env$tm_core(), name = "pl_off", ondisk = FALSE, overwrite = TRUE,
    parent_levels = FALSE)))
  expect_identical(s@status$parent_levels, character())

  # TRUE is accepted and, with an empty supported set, resolves to the same
  s2 <- suppressMessages(suppressWarnings(interpolate_model(
    env$tm_core(), name = "pl_on", ondisk = FALSE, overwrite = TRUE,
    parent_levels = TRUE)))
  expect_identical(s2@status$parent_levels, energyRt:::.parent_level_vars)
})

test_that("family parent-levels: the switch is a no-op while nothing is supported", {
  skip_if_no_fixtures()
  env <- .mapping_fixture_env()
  rows <- function(pl) {
    s <- suppressMessages(suppressWarnings(interpolate_model(
      env$tm_core(), name = paste0("pl_n_", isTRUE(pl)), ondisk = FALSE,
      overwrite = TRUE, parent_levels = pl)))
    vapply(c("mvTechOut", "mvTechAct", "mvOutTot"), function(m) {
      d <- tryCatch(get_data_slot(s@modInp@parameters[[m]]),
                    error = function(e) NULL)
      if (is.null(d)) 0L else nrow(d)
    }, integer(1))
  }
  expect_identical(rows(FALSE), rows(TRUE))
})

test_that("family parent-levels: the option supplies the default", {
  # so a whole run or test suite can be switched in one place
  withr::with_options(list(energyRt.parent_levels = TRUE), {
    expect_identical(energyRt:::.parent_levels_active(),
                     energyRt:::.parent_level_vars)
  })
  withr::with_options(list(energyRt.parent_levels = FALSE), {
    expect_identical(energyRt:::.parent_levels_active(), character())
  })
})

test_that("family parent-levels: interpolation restores the option it borrowed", {
  skip_if_no_fixtures()
  env <- .mapping_fixture_env()
  withr::with_options(list(energyRt.parent_levels = FALSE), {
    suppressMessages(suppressWarnings(interpolate_model(
      env$tm_core(), name = "pl_restore", ondisk = FALSE, overwrite = TRUE,
      parent_levels = TRUE)))
    # the call publishes its own setting for the mapping layer; leaking it would
    # make every later interpolation in the session build parent rows silently
    expect_false(isTRUE(getOption("energyRt.parent_levels")))
  })
})
