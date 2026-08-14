# Group-aggregate ("TOTAL") capacity bounds. A bound row whose vintage/cluster is
# the reserved token "TOTAL" constrains the SUM over that dimension instead of
# each variant separately, compiled into an ordinary generated `newConstraint()`.
#
# The per-region case below is a regression test for a silent-data-loss bug:
# the generated constraint used to be named from (technology, bound column,
# level) only, so two TOTAL rows differing just by region minted the SAME name
# and the second replaced the first in a named list. One cap simply vanished --
# in the fixture below R1 built 120 against a declared cap of 5.

mk_clustered <- function(nm = "EWIN", capacity) {
  newTechnology(
    nm, output = list(comm = "ELC"),
    cluster  = c("best", "mid"),
    invcost  = data.frame(cluster = c("best", "mid"), invcost = c(100, 110)),
    afs      = data.frame(cluster = c("best", "mid"), timeslice = "ANNUAL",
                          afs.up = c(0.9, 0.9)),
    capacity = capacity,
    vintage  = data.frame(olife = 25L), cap2act = 1)
}

test_that("a per-variant bound (cluster = NA) caps each cluster separately", {
  skip_if_no_solver()
  W <- mk_clustered(capacity = data.frame(cap.up = 5))
  sc <- vt_interp(vt_model(W, name = "pv"), "pv")
  expect_length(sc@modInp@gams.equation, 0L)   # no generated constraint
  sol <- vt_solve(sc)
  # each of the two clusters can reach 5, so the total exceeds 5
  expect_gt(vt_cap(sol, "EWIN", "R1"), 5 + 1e-4)
})

test_that("cluster = TOTAL caps the SUM across clusters", {
  skip_if_no_solver()
  W <- mk_clustered(capacity = data.frame(cluster = "TOTAL", cap.up = 5))
  sc <- vt_interp(vt_model(W, name = "gt"), "gt")
  expect_length(sc@modInp@gams.equation, 1L)
  sol <- vt_solve(sc)
  expect_lte(vt_cap(sol, "EWIN", "R1"), 5 + 1e-6)
  expect_lte(vt_cap(sol, "EWIN", "R2"), 5 + 1e-6)
})

test_that("per-region group caps BOTH bind (regression: one used to vanish)", {
  skip_if_no_solver()
  W <- mk_clustered(capacity = data.frame(
    cluster = c("TOTAL", "TOTAL"), region = c("R1", "R2"),
    cap.up  = c(5, 8)))
  sc <- vt_interp(vt_model(W, name = "pr"), "pr")

  cns <- names(sc@modInp@gams.equation)
  expect_length(cns, 2L)                 # region is restricted -> part of the id
  expect_false(anyDuplicated(cns) > 0L)  # ... and the names are distinct

  sol <- vt_solve(sc)
  # Before the fix: R1 = 120 against a cap of 5, because its constraint had been
  # replaced by R2's. Both must now bind exactly.
  expect_equal(vt_cap(sol, "EWIN", "R1"), 5, tolerance = 1e-6)
  expect_equal(vt_cap(sol, "EWIN", "R2"), 8, tolerance = 1e-6)
})

test_that("contradictory bounds for the same cell are rejected", {
  W <- mk_clustered(capacity = data.frame(
    cluster = c("TOTAL", "TOTAL"), region = c("R1", "R1"), cap.up = c(5, 8)))
  expect_error(vt_interp(vt_model(W, name = "cd"), "cd"),
               "Contradictory group-aggregate")
})

test_that("technologies whose names collapse to one token are rejected", {
  # "E_WIN" and "EWIN" both reduce to "EWIN", so both mint the same constraint
  # name; `add(overwrite = TRUE)` would silently drop one.
  cap <- data.frame(cluster = "TOTAL", cap.up = 5)
  mod <- vt_model(mk_clustered("E_WIN", cap), mk_clustered("EWIN", cap),
                  name = "cc")
  expect_error(vt_interp(mod, "cc"), "collide on name")
})

test_that("TOTAL is refused where it has no meaning", {
  # only the bound columns of @capacity support it
  W <- newTechnology(
    "EWIN", output = list(comm = "ELC"), cap2act = 1,
    cluster = c("best", "mid"),
    invcost = data.frame(cluster = c("TOTAL", "mid"), invcost = c(100, 110)),
    vintage = data.frame(olife = 25L))
  expect_error(vt_expand_one(W), "only supported on the bound columns")

  # ret.* carries a second year index on the retirement variables. The refusal
  # lives in the group-constraint builder, which runs at model level.
  W2 <- mk_clustered(capacity = data.frame(cluster = "TOTAL", ret.up = 5))
  expect_error(vt_interp(vt_model(W2, name = "rt"), "rt"), "retirement")
})

test_that("the TOTAL token matches a hand-written newConstraint exactly", {
  skip_if_no_solver()
  cap <- data.frame(cluster = "TOTAL", cap.up = 5)
  s_tok <- vt_interp(vt_model(mk_clustered(capacity = cap), name = "tk"), "tk")

  plain <- newTechnology(
    "EWIN", output = list(comm = "ELC"), cluster = c("best", "mid"),
    invcost = data.frame(cluster = c("best", "mid"), invcost = c(100, 110)),
    afs = data.frame(cluster = c("best", "mid"), timeslice = "ANNUAL",
                     afs.up = c(0.9, 0.9)),
    vintage = data.frame(olife = 25L), cap2act = 1)
  hand <- newConstraint(
    name = "HAND", eq = "<=",
    for.each = expand.grid(region = c("R1", "R2"), year = c(2020L, 2030L),
                           stringsAsFactors = FALSE),
    term1 = list(variable = "vTechCap",
                 for.sum = list(tech = c("EWIN_CLbest", "EWIN_CLmid"))),
    defVal = 5)
  s_hand <- vt_interp(vt_model(plain, hand, name = "hd"), "hd")

  o_tok  <- suppressMessages(getData(vt_solve(s_tok),  "vObjective", merge = TRUE))
  o_hand <- suppressMessages(getData(vt_solve(s_hand), "vObjective", merge = TRUE))
  expect_equal(sum(o_tok$value), sum(o_hand$value), tolerance = 1e-8)
})
