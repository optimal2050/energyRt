# =========================================================================== #
# PARTIAL wildcard columns must never reach a writer as a raw NA.
#
# `.folded_params()` registers a parameter only when a dimension column is
# ENTIRELY wildcard, because the code rewrite is per-PARAMETER: pointing every
# `pX[...]` lookup at 'ANYREGION' would strand the explicit rows. `fold.R`
# never creates a partial column for the same reason.
#
# But one can still arrive from the SOURCE data -- a wildcard that
# `unfold_scenario_parameters()` could not materialise and that the fold pass
# then declined to fold. Nothing converted it, nothing rewrote it, and
# `validate_scenario_parameters()` exempts the trimmable dims from its NA
# check, so the raw NA reached the solver. NA is not a set member, so every
# lookup that should hit it missed and silently took the parameter's default.
#
# Measured on IB_PTL50_CU50_P10 (fold = TRUE): pTechEac carried NA in 106 of
# 208 region rows, only 3 of 141 mTechNew tuples found a value, capital cost
# effectively vanished and the model returned a NEGATIVE objective
# (-7.52e9) having built 2,201 GW against 122.5 GW unfolded. pTechVarom,
# pStorageDurationLo and pStorageDurationUp were hit the same way.
# =========================================================================== #

# a folded scenario from the shared two-region fixture (helper-fold.R)
.pw_scen <- function() {
  suppressWarnings(suppressMessages(
    interpolate_model(fold_model(), name = "pw", ondisk = FALSE, fold = TRUE)))
}

# Pick a numpar that actually carries rows and a `region` column in this
# fixture, then make that column PARTIAL: wildcard in some rows, explicit in
# others. Chosen at runtime so the test does not depend on which parameters the
# fixture happens to populate.
.pw_pick <- function(scen, dim = "region") {
  for (nm in names(scen@modInp@parameters)) {
    p <- scen@modInp@parameters[[nm]]
    if (is.null(p) || p@type %in% c("set", "map")) next
    d <- tryCatch(as.data.frame(get_data_slot(p)), error = function(e) NULL)
    if (is.null(d) || nrow(d) < 2) next
    if (!dim %in% names(d)) next
    return(nm)
  }
  NULL
}

.pw_make_partial <- function(scen, param, dim = "region") {
  p <- scen@modInp@parameters[[param]]
  d <- as.data.frame(get_data_slot(p))
  d[[dim]] <- as.character(d[[dim]])
  d[[dim]] <- rep(c(NA_character_, "R1"), length.out = nrow(d))
  scen@modInp@parameters[[param]] <- energyRt:::.fold_write_back(p, d)
  scen
}

test_that("a partial wildcard column is detected", {
  scen <- .pw_scen()
  nm <- .pw_pick(scen)
  skip_if(is.null(nm), "fixture has no populated region parameter")

  scen <- .pw_make_partial(scen, nm)
  d <- as.data.frame(get_data_slot(scen@modInp@parameters[[nm]]))
  expect_gt(sum(is.na(d$region)), 0)
  expect_lt(sum(is.na(d$region)), nrow(d))

  expect_true(nm %in% unlist(energyRt:::.partial_wildcard_params(scen)))
})

test_that("apply_fold_artificial leaves no raw NA behind", {
  scen <- .pw_scen()
  nm <- .pw_pick(scen)
  skip_if(is.null(nm), "fixture has no populated region parameter")
  scen <- .pw_make_partial(scen, nm)

  out <- apply_fold_artificial(scen, backends = "PYOMOConcrete")
  d <- as.data.frame(get_data_slot(out@modInp@parameters[[nm]]))

  # expanded to explicit members - NOT to the artificial member, which cannot
  # represent a partial column
  expect_equal(sum(is.na(d$region)), 0L)
  expect_equal(sum(grepl("^ANY", as.character(d$region))), 0L)
  expect_gt(nrow(d), 0)
})

test_that("a raw NA that cannot be expanded is refused, not written", {
  scen <- .pw_scen()
  nm <- .pw_pick(scen)
  skip_if(is.null(nm), "fixture has no populated region parameter")
  scen <- .pw_make_partial(scen, nm)

  # the assert sees the raw NA before anything converts it
  expect_error(energyRt:::.assert_no_raw_wildcards(scen), "raw NA")
  expect_error(energyRt:::.assert_no_raw_wildcards(scen), "wrong answer")
})

test_that("whole-column folding is unchanged", {
  scen <- .pw_scen()
  whole <- energyRt:::.folded_params(scen)
  skip_if(all(lengths(whole) == 0), "fixture folded nothing")

  out <- apply_fold_artificial(scen, backends = "PYOMOConcrete")
  nm <- unlist(whole)[1]
  dim <- names(whole)[lengths(whole) > 0][1]
  d <- as.data.frame(get_data_slot(out@modInp@parameters[[nm]]))
  # a whole column still becomes the artificial member, and round-trips back
  expect_true(any(grepl("^ANY|^0$", as.character(d[[dim]]))))
  back <- revert_fold_artificial(out)
  expect_true(all(is.na(
    as.data.frame(get_data_slot(back@modInp@parameters[[nm]]))[[dim]])))
})

test_that("an unfolded scenario is unaffected", {
  scen <- suppressWarnings(suppressMessages(
    interpolate_model(fold_model(), name = "pw_unf", ondisk = FALSE, fold = FALSE)))
  expect_length(unlist(energyRt:::.partial_wildcard_params(scen)), 0)
  expect_silent(energyRt:::.assert_no_raw_wildcards(scen))
  expect_no_error(apply_fold_artificial(scen, backends = "PYOMOConcrete"))
})
