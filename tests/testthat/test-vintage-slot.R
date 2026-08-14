# The `technology` lifespan lives in the merged `@vintage` slot; the former
# `@start` / `@end` / `@olife` slots are gone but their ARGUMENTS still work via
# `.tech_lifespan_args()`. The merge must be behaviour-preserving, which hinges
# on the table being a COLUMN-WISE union: a region-agnostic `start` beside a
# per-region `olife` must keep each column's own granularity rather than
# broadcasting the global value across only the regions named by the other.

test_that("@vintage replaces @start/@end/@olife", {
  sn <- slotNames("technology")
  expect_true("vintage" %in% sn)
  expect_false(any(c("start", "end", "olife") %in% sn))

  p <- new("technology")
  expect_identical(names(p@vintage),
                   c("vintage", "region", "cluster", "start", "end", "olife"))
  # selector columns are character so the reserved "TOTAL" token fits
  expect_true(is.character(p@vintage$vintage))
  expect_true(is.character(p@vintage$cluster))
  expect_true(is.integer(p@vintage$start))
  expect_true(is.integer(p@vintage$end))
  expect_true(is.integer(p@vintage$olife))
})

test_that("variant-varying slots gained vintage/cluster; topology slots did not", {
  p <- new("technology")
  for (s in c("capacity", "ceff", "geff", "aeff", "af", "afs", "weather",
              "fixom", "varom", "invcost")) {
    expect_true(all(c("vintage", "cluster") %in% names(slot(p, s))),
                info = paste("slot", s))
  }
  for (s in c("input", "output", "aux", "units", "group")) {
    expect_false(any(c("vintage", "cluster") %in% names(slot(p, s))),
                 info = paste("slot", s))
  }
})

test_that("legacy start/end/olife arguments fold into @vintage", {
  t1 <- newTechnology(
    "ECOA", input = list(comm = "COA"), output = list(comm = "ELC"),
    start = data.frame(region = c("R1", "R2"), start = c(2020L, 2030L)),
    end   = data.frame(region = c("R1", "R2"), end   = c(2040L, 2050L)),
    olife = data.frame(region = c("R1", "R2"), olife = c(30L, 20L))
  )
  # same granularity on all three -> coalesced to one row per region
  expect_equal(nrow(t1@vintage), 2L)
  r1 <- t1@vintage[t1@vintage$region == "R1", ]
  r2 <- t1@vintage[t1@vintage$region == "R2", ]
  expect_equal(c(r1$start, r1$end, r1$olife), c(2020L, 2040L, 30L))
  expect_equal(c(r2$start, r2$end, r2$olife), c(2030L, 2050L, 20L))
  # the other arguments must survive (the .data2slots early-return trap)
  expect_equal(nrow(t1@input), 1L)
  expect_equal(nrow(t1@output), 1L)
})

test_that("scalar and named-list lifespan forms work", {
  t2 <- newTechnology("EGAS", input = list(comm = "COA"),
                      output = list(comm = "ELC"),
                      start = 2020L, end = 2050L, olife = list(olife = 25))
  expect_equal(nrow(t2@vintage), 1L)
  expect_true(is.na(t2@vintage$region))
  expect_equal(c(t2@vintage$start, t2@vintage$end, t2@vintage$olife),
               c(2020L, 2050L, 25L))
})

test_that("mixed granularity stays column-wise (no false broadcast)", {
  t3 <- newTechnology("EMIX", input = list(comm = "COA"),
                      output = list(comm = "ELC"),
                      start = 2020L, end = 2050L,
                      olife = data.frame(region = c("R1", "R2"),
                                         olife = c(25L, 15L)))
  expect_equal(nrow(t3@vintage), 3L)
  na_row <- t3@vintage[is.na(t3@vintage$region), ]
  expect_equal(c(na_row$start, na_row$end), c(2020L, 2050L))
  expect_true(is.na(na_row$olife))          # global start/end, NOT a global olife
  r1 <- t3@vintage[!is.na(t3@vintage$region) & t3@vintage$region == "R1", ]
  expect_true(is.na(r1$start))
  expect_equal(r1$olife, 25L)
})

test_that("update() keeps other slots and rejects mixing vintage with legacy args", {
  t1 <- newTechnology("ECOA", input = list(comm = "COA"),
                      output = list(comm = "ELC"),
                      olife = list(olife = 30))
  t5 <- update(t1, olife = data.frame(region = c("R1", "R2"), olife = c(40L, 40L)))
  expect_equal(nrow(t5@input), 1L)
  expect_true(all(t5@vintage$olife == 40L))

  expect_error(
    newTechnology("X", vintage = data.frame(start = 2020L), start = 2020L),
    "not both"
  )
})

test_that(".data2slots errors on an unknown slot instead of silently emptying", {
  # It used to warn and return an object with NONE of the supplied data applied.
  expect_error(newTechnology("X", nosuchslot = 1), "Unidentified slot")
})

test_that("bare-vector shorthand fills one row per element", {
  t6 <- newTechnology("EVIN", input = list(comm = "COA"),
                      output = list(comm = "ELC"),
                      vintage = c(2030, 2040))   # numeric -> character labels
  expect_equal(nrow(t6@vintage), 2L)
  expect_identical(t6@vintage$vintage, c("2030", "2040"))
})
