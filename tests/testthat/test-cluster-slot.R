# The `@cluster` slot DECLARES a technology's clusters -- what they are and
# where they exist -- mirroring how `@group` declares input groups whose data
# lives in `@input$group`. It is optional: empty means labels are harvested from
# the variant-varying slots, populated makes it authoritative.
#
# The reason it exists: a mistyped label used to produce a phantom technology
# carrying whatever data held the misspelling and DEFAULTS for everything else --
# i.e. no investment cost and no capacity bound, a free unlimited generator --
# while stripping the real cluster of its availability limit. Silently.

mk_win <- function(...) {
  newTechnology("EWIN", output = list(comm = "ELC"),
                vintage = data.frame(olife = 25L), cap2act = 1, ...)
}

test_that("@cluster slot exists with the declaration columns", {
  expect_true("cluster" %in% slotNames("technology"))
  expect_identical(names(new("technology")@cluster),
                   c("cluster", "desc", "region", "order"))
})

test_that("declaring clusters is enough to create the variants, in `order`", {
  t1 <- mk_win(cluster = data.frame(cluster = c("best", "mid", "poor"),
                                    order = 1:3))
  r1 <- vt_expand_one(t1)
  expect_equal(length(r1$objects), 3L)
  expect_identical(r1$provenance$cluster, c("best", "mid", "poor"))
  # each variant keeps only its own declaration row, so the expanded model still
  # satisfies the "one cell per technology" guard
  expect_true(all(vapply(r1$objects, function(o) nrow(o@cluster) == 1L,
                         logical(1))))
  expect_true(all(vapply(r1$objects,
                         function(o) length(vt_levels(o, "cluster")) == 1L,
                         logical(1))))
})

test_that("bare-vector shorthand declares clusters", {
  expect_equal(nrow(mk_win(cluster = c("best", "mid"))@cluster), 2L)
})

test_that("an undeclared (mistyped) label is rejected, naming the slot", {
  t3 <- mk_win(
    cluster = c("best", "mid"),
    invcost = data.frame(cluster = c("best", "mid"), invcost = c(1500, 1300)),
    afs     = data.frame(cluster = c("bset", "mid"), timeslice = "ANNUAL",
                         afs.up = c(0.45, 0.32)))            # typo: bset
  expect_error(vt_expand_one(t3), "Undeclared cluster")
  expect_error(vt_expand_one(t3), "afs")
})

test_that("without a declaration, cross-slot inconsistency is still caught", {
  t4 <- mk_win(
    invcost = data.frame(cluster = c("best", "mid"), invcost = c(1500, 1300)),
    afs     = data.frame(cluster = c("bset", "mid"), timeslice = "ANNUAL",
                         afs.up = c(0.45, 0.32)))
  expect_error(vt_expand_one(t4), "Inconsistent cluster")
})

test_that("consistent labels without a declaration are fine (no false positive)", {
  t5 <- mk_win(
    invcost = data.frame(cluster = c("best", "mid"), invcost = c(1500, 1300)),
    afs     = data.frame(cluster = c("best", "mid"), timeslice = "ANNUAL",
                         afs.up = c(0.45, 0.32)))
  expect_equal(length(vt_expand_one(t5)$objects), 2L)
})

test_that("labels that collapse under sanitisation are rejected", {
  # "best-1" and "best 1" both reduce to "best1": one variant would silently
  # replace the other.
  t6 <- mk_win(cluster = c("best-1", "best 1"))
  expect_error(vt_expand_one(t6), "collapse to the same variant-name token")
})

test_that("region-scoped clusters restrict the variant to those regions", {
  t7 <- mk_win(region = c("R1", "R2", "R3"),
               cluster = data.frame(cluster = c("onshore", "offshore"),
                                    region  = c(NA, "R1"), order = 1:2))
  r7 <- vt_expand_one(t7)
  nms <- vapply(r7$objects, function(o) o@name, character(1))
  onsh <- r7$objects[[which(nms == "EWIN_CLonshore")]]
  offs <- r7$objects[[which(nms == "EWIN_CLoffshore")]]
  expect_setequal(onsh@region, c("R1", "R2", "R3"))   # NA -> all
  expect_identical(offs@region, "R1")                  # named -> only those
})

test_that("an empty region intersection is an error, not a dead technology", {
  t8 <- mk_win(region = "R2",
               cluster = data.frame(cluster = "offshore", region = "R1"))
  expect_error(vt_expand_one(t8), "do not overlap")
})

test_that("@vintage may not contradict @cluster existence", {
  t9 <- newTechnology(
    "EWIN", output = list(comm = "ELC"), cap2act = 1,
    cluster = data.frame(cluster = "offshore", region = "R1"),
    vintage = data.frame(vintage = "2030", cluster = "offshore",
                         region = "R2", olife = 25L))
  expect_error(vt_expand_one(t9), "restricts that cluster")
})
