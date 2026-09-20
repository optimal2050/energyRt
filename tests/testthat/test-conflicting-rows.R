# =========================================================================== #
# Conflicting rows in an object's data slots (R/assert_conflicting_rows.R).
#
# An NA in a key column means "all members of that dimension", so two rows
# with the same key both claim the same parameter cell. Nothing downstream
# resolves that: `.interp_one_series()` left-joins the value table onto the
# key grid, so a repeated key MULTIPLIES rows and the solver silently receives
# a duplicated parameter. The construction-time check is what makes it loud.
#
# The property that is easy to get wrong, and the reason this file exists:
# keys are per PARAMETER, not per slot. Two columns of ONE slot can have
# different key sets, so a pair of rows can be legal for one column and a
# conflict for another.
# =========================================================================== #

cr_tech <- function(...) {
  newTechnology("CRT", input = list(comm = "GAS"), output = list(comm = "ELC"),
                ceff = data.frame(comm = "ELC", cact2cout = 1),
                cap2act = 1, ...)
}

# @covers pTechCap depth=C
test_that("two rows setting one column at one key are refused", {
  expect_error(
    cr_tech(capacity = data.frame(region = c("R1", "R1"), year = c(2020L, 2020L),
                                  cap.up = c(5, 8))),
    "'cap.up' is set 2 times for the same key")
  # the message carries what is needed to fix it: the key and both values
  err <- tryCatch(
    cr_tech(capacity = data.frame(region = c("R1", "R1"), year = c(2020L, 2020L),
                                  cap.up = c(5, 8))),
    error = conditionMessage)
  expect_match(err, "region=R1")
  expect_match(err, "5, 8")
})

test_that("equal values are refused too, not silently deduplicated", {
  # ruled 2026-09-19: a repeated key is a defect whatever the values. Two
  # identical rows fan out in a join exactly as two conflicting ones do -- they
  # are harmless only at the point of resolution, not downstream.
  expect_error(
    cr_tech(capacity = data.frame(region = c("R1", "R1"), year = c(2020L, 2020L),
                                  cap.up = c(5, 5))),
    "'cap.up' is set 2 times")
})

test_that("rows at genuinely different keys are fine", {
  expect_s4_class(
    cr_tech(capacity = data.frame(region = c("R1", "R2"), year = 2020L,
                                  cap.up = c(5, 8))), "technology")
  expect_s4_class(
    cr_tech(capacity = data.frame(region = "R1", year = c(2020L, 2030L),
                                  cap.up = c(5, 8))), "technology")
  # vintage and cluster are keys as well -- variants expand by suffixing the
  # process NAME, so they never appear in a parameter's own dims
  expect_s4_class(
    cr_tech(cluster = c("a", "b"),
            capacity = data.frame(cluster = c("a", "b"), cap.up = c(5, 8))),
    "technology")
})

test_that("a broadcast row coexists with a specific one", {
  # region = NA means ALL, but it is still a DIFFERENT key from region = "R1":
  # the standard interpolation rule applies, specific overriding broadcast
  expect_s4_class(
    cr_tech(capacity = data.frame(region = c(NA, "R1"), year = 2020L,
                                  cap.up = c(5, 8))), "technology")
  # two broadcasts, however, are the same cell claimed twice
  expect_error(
    cr_tech(capacity = data.frame(region = rep(NA_character_, 2), year = 2020L,
                                  cap.up = c(5, 8))),
    "cap.up.*same key")
  # ... as is omitting the key column entirely
  expect_error(
    cr_tech(capacity = data.frame(year = c(2020L, 2020L), cap.up = c(5, 8))),
    "cap.up.*same key")
})

test_that("different columns at the same key do not collide", {
  # each column is checked on its own, and only where it is set
  expect_s4_class(
    cr_tech(capacity = data.frame(region = "R1", year = 2020L,
                                  cap.up = c(8, NA), cap.lo = c(NA, 5))),
    "technology")
})

# The per-parameter keying proof. Within `technology@aeff`, `act2ainp` is keyed
# [vintage, cluster, acomm, region, year, timeslice] while `cinp2ainp` keys on
# `comm` as well -- so ONE pair of rows differing only by `comm` is legal for
# one column and a conflict for the other. A slot-level key model gets this
# wrong in one direction or the other.
# @covers pTechAct2AInp pTechCinp2AInp depth=C
test_that("keys are per parameter, not per slot", {
  aux <- data.frame(acomm = "H2O")
  # `comm` IS a key of cinp2ainp: two rows, two commodities, no conflict
  expect_s4_class(
    cr_tech(aux = aux,
            aeff = data.frame(acomm = "H2O", comm = c("GAS", "ELC"),
                              cinp2ainp = c(1, 2))), "technology")
  # `comm` is NOT a key of act2ainp: the same two rows claim one cell
  expect_error(
    cr_tech(aux = aux,
            aeff = data.frame(acomm = "H2O", comm = c("GAS", "ELC"),
                              act2ainp = c(1, 2))),
    "'act2ainp' is set 2 times")
  # and acomm, which IS a key of both, separates them again
  expect_s4_class(
    cr_tech(aux = data.frame(acomm = c("H2O", "CO2")),
            aeff = data.frame(acomm = c("H2O", "CO2"), act2ainp = c(1, 2))),
    "technology")
})

test_that("columns that are not catalogued parameters are not checked", {
  # declaration columns, annotations and transform arguments carry no parameter,
  # so repeats there mean nothing. `@aux` is pure declaration.
  expect_null(energyRt:::.slot_col_keys("technology", "aux", "unit",
                                        c("acomm", "unit")))
  expect_s4_class(
    cr_tech(aux = data.frame(acomm = c("H2O", "CO2"), unit = c("t", "t"))),
    "technology")
})

test_that("the checker is a no-op below two rows", {
  d <- data.frame(region = "R1", cap.up = 5)
  expect_true(energyRt:::.assert_no_conflicting_rows(d, "technology",
                                                     "capacity", "CRT"))
})
