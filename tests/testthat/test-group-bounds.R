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
  expect_length(sc@modInp@user_constraints, 0L)   # no generated constraint
  sol <- vt_solve(sc)
  # each of the two clusters can reach 5, so the total exceeds 5
  expect_gt(vt_cap(sol, "EWIN", "R1"), 5 + 1e-4)
})

test_that("cluster = TOTAL caps the SUM across clusters", {
  skip_if_no_solver()
  W <- mk_clustered(capacity = data.frame(cluster = "TOTAL", cap.up = 5))
  sc <- vt_interp(vt_model(W, name = "gt"), "gt")
  expect_length(sc@modInp@user_constraints, 1L)
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

  cns <- names(sc@modInp@user_constraints)
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

# =========================================================================== #
# Every capacity PART, not just the discharger
# =========================================================================== #

test_that("a storage group bound reaches inp.* and stg.*, not only out.*", {
  # `.variant_group_constraints()` used to derive its scan list from a single
  # `bound_prefix` ("out."), so `inp.cap.up = "TOTAL"` and `stg.cap.up = "TOTAL"`
  # matched no column, the loop body never ran, and the row did NOTHING -- no
  # variant (the token is filtered out of the level set) and no bound. Silently.
  mk <- function(col, val) {
    cap <- data.frame(cluster = c("a", "b", "TOTAL"),
                      out.cap.up = c(50, 50, NA))
    cap[[col]] <- c(NA, NA, val)
    if (col == "out.cap.up") cap$out.cap.up <- c(50, 50, val)
    newStorage("BATT", commodity = "ELC", cluster = c("a", "b"),
               invcost = data.frame(cluster = c("a", "b"),
                                    out.invcost = c(10, 12)),
               capacity = cap, vintage = data.frame(olife = 20L), duration = 4)
  }
  for (col in c("out.cap.up", "inp.cap.up", "stg.cap.up")) {
    sc <- vt_interp(vt_stg_model(mk(col, 7), name = paste0("gp", substr(col, 1, 3))),
                    paste0("gp", substr(col, 1, 3)))
    expect_length(sc@modInp@user_constraints, 1L)
  }
})

test_that("a group bound on the storing part actually binds", {
  skip_if_no_solver()
  # Per-cluster `stg.cap.lo` puts the storage in EXPLICIT storing-capacity mode
  # (with only `duration`, `vStorageStgCap` does not exist and the bound would be
  # vacuous), and forces the summed capacity to 20. The group cap then decides
  # feasibility -- which is the sharpest way to show it is enforced at all.
  mk <- function(tot) newStorage(
    "BATT", commodity = "ELC", cluster = c("a", "b"),
    invcost = data.frame(cluster = c("a", "b"), out.invcost = c(10, 12),
                         stg.invcost = c(1, 1)),
    capacity = data.frame(cluster = c("a", "b", "TOTAL"),
                          stg.cap.lo = c(10, 10, NA),
                          stg.cap.up = c(NA, NA, tot)),
    vintage = data.frame(olife = 20L))

  # 25 >= the forced 20: feasible, and the sum is the forced 20.
  sol <- vt_solve(vt_interp(vt_stg_model(mk(25), name = "gsok"), "gsok"))
  d <- suppressMessages(getData(sol, "vStorageStgCap", merge = TRUE))
  expect_gt(nrow(d), 0)
  expect_equal(sum(d$value[d$year == min(d$year) & d$region == d$region[1]]), 20)

  # 15 < 20: infeasible. Before the fix this solved happily, because the bound
  # was never generated.
  bad <- vt_interp(vt_stg_model(mk(15), name = "gsbad"), "gsbad")
  sol2 <- suppressWarnings(try(vt_solve(bad), silent = TRUE))
  got <- if (inherits(sol2, "try-error")) 0L else
    nrow(suppressWarnings(try(getData(sol2, "vObjective", merge = TRUE),
                              silent = TRUE)))
  expect_equal(got, 0L)
})

# =========================================================================== #
# Group AVAILABILITY FACTORS: activity against capacity
# =========================================================================== #
#
# A capacity group bound constrains a stock with one summand. An availability
# factor relates the summed ACTIVITY of a group to its summed CAPACITY:
#
#     sum_k act_k  <op>  af * cap2act * share_s * sum_k cap_k
#
# Two summands and a timeslice-varying coefficient. Note what this is NOT: it is
# not the sum of the per-variant limits, because those carry each variant's own
# weather factor. It is a bound in its own right -- "however good the weather,
# the fleet cannot exceed X% of nameplate" -- and the per-variant constraints
# still apply alongside it.

gb_af <- function(nm, slot, row) {
  W <- newTechnology("EWIN", output = list(comm = "ELC"),
                     cluster = c("best", "mid"),
                     invcost = data.frame(cluster = c("best", "mid"),
                                          invcost = c(100, 110)),
                     capacity = data.frame(cap.up = 100),
                     vintage = data.frame(olife = 25L), cap2act = 1)
  slot(W, slot) <- row
  vt_interp(vt_model(W, name = nm), nm)
}

test_that("a group afs.up caps the fleet's annual utilisation", {
  skip_if_no_solver()
  sc <- gb_af("gafs", "afs", data.frame(cluster = "TOTAL", timeslice = "ANNUAL",
                                        afs.up = 0.3))
  expect_length(sc@modInp@user_constraints, 1L)
  sol <- vt_solve(sc)
  a <- suppressMessages(getData(sol, "vTechAct", merge = TRUE))
  a <- a[grepl("^EWIN", a$tech) & a$region == "R1", ]
  act <- sum(a$value[a$year == min(a$year)])
  cap <- vt_cap(sol, "EWIN", "R1")
  # Annual activity is exactly 30% of the summed nameplate. The model builds MORE
  # capacity than it otherwise would, precisely because the cap bites.
  expect_equal(act / cap, 0.3, tolerance = 1e-6)
})

test_that("a group af.up caps every timeslice against summed capacity", {
  skip_if_no_solver()
  peak <- function(nm, v) {
    sc <- gb_af(nm, "af", data.frame(cluster = "TOTAL", af.up = v))
    # one constraint per timeslice of the calendar (ANNUAL + four seasons)
    expect_gt(length(sc@modInp@user_constraints), 1L)
    sol <- vt_solve(sc)
    a <- suppressMessages(getData(sol, "vTechAct", merge = TRUE))
    a <- a[grepl("^EWIN", a$tech) & a$region == "R1", ]
    a <- a[a$year == min(a$year), ]
    p <- stats::aggregate(list(v = a$value), by = list(s = a$timeslice), FUN = sum)
    max(p$v) / (vt_cap(sol, "EWIN", "R1") * 0.25)   # 0.25 = a season's share
  }
  expect_equal(peak("gaf5", 0.5), 0.5, tolerance = 1e-6)
  expect_equal(peak("gaf9", 0.9), 0.9, tolerance = 1e-6)
})

test_that("a TOTAL row with no af value is refused", {
  # The token is filtered out of the variant levels, so such a row would create
  # neither a variant nor a bound.
  expect_error(gb_af("gafbad", "afs",
                     data.frame(cluster = "TOTAL", timeslice = "ANNUAL",
                                afs.lo = NA_real_, afs.up = NA_real_)),
               "carries no group-boundable value")
})
