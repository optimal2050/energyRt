# Keyed lifespan collection: start/end/olife by (vintage, region, cluster)
# with the universal broadcast/override rule (user ruling 2026-08-13) -----

.vl_tech <- function(vintage_df) {
  newTechnology("VLT", input = list(comm = "GAS"),
                output = list(comm = "ELC"),
                ceff = data.frame(comm = "ELC", cact2cout = 1),
                vintage = vintage_df, cap2act = 1)
}

test_that(".lifespan_col keeps the (vintage, region, cluster) keys", {
  t1 <- .vl_tech(data.frame(vintage = c("2020", "2030"),
                            start = c(2020L, 2030L), olife = 20L))
  d <- energyRt:::.lifespan_col(t1, "start")
  expect_named(d, c("vintage", "region", "cluster", "start"))
  expect_equal(d$vintage, c("2020", "2030"))
  expect_equal(d$start, c(2020L, 2030L))
})

test_that(".lifespan_resolve dedups equal keys and errors on conflicts", {
  # `newTechnology()` now refuses a repeated key outright, so these fixtures
  # cannot be built through the constructor. `@<-` does not route through
  # .data2slots(), which is also how the objects this layer defends arrive:
  # loaded from a store or built by a converter, never via new*().
  .vl_dup <- function(second_olife) {
    t <- .vl_tech(data.frame(vintage = "2020", olife = 20L))
    d <- rbind(t@vintage, t@vintage)
    d$olife[2] <- second_olife
    t@vintage <- d
    t
  }
  # duplicate key with the SAME value: deduplicated silently
  expect_equal(nrow(energyRt:::.lifespan_resolve(.vl_dup(20L), "olife")), 1L)
  # duplicate key with DIFFERENT values: hard error naming the process
  expect_error(energyRt:::.lifespan_resolve(.vl_dup(30L), "olife"),
               "conflicting.*olife.*VLT")
  # broadcast + specific keys legitimately coexist
  t3 <- .vl_tech(data.frame(region = c(NA, "R1"), olife = c(20L, 30L)))
  expect_equal(nrow(energyRt:::.lifespan_resolve(t3, "olife")), 2L)
})

test_that(".lifespan_by_region: specific overrides broadcast, fills rest", {
  t1 <- .vl_tech(data.frame(region = c(NA, "R1"), olife = c(20L, 30L)))
  br <- energyRt:::.lifespan_by_region(t1, "olife", c("R1", "R2", "R3"))
  expect_equal(br$olife[br$region == "R1"], 30L)
  expect_equal(br$olife[br$region == "R2"], 20L)
  expect_equal(br$olife[br$region == "R3"], 20L)
  # no broadcast: uncovered regions get NA
  t2 <- .vl_tech(data.frame(region = "R1", olife = 30L))
  br2 <- energyRt:::.lifespan_by_region(t2, "olife", c("R1", "R2"))
  expect_equal(br2$olife[br2$region == "R1"], 30L)
  expect_true(is.na(br2$olife[br2$region == "R2"]))
})

test_that("the post-expansion cell shape resolves cleanly", {
  # broadcast olife row + vintage-keyed window rows: the shape expansion
  # legitimately produces; each column resolves independently
  t1 <- .vl_tech(data.frame(vintage = c(NA, "2020", "2030"),
                            start = c(NA, 2020L, 2030L),
                            end = c(NA, 2029L, NA),
                            olife = c(25L, NA, NA)))
  ex <- energyRt:::.expand_one_tech(t1)
  for (o in ex$objects) {
    for (cc in c("start", "end", "olife")) {
      d <- energyRt:::.lifespan_resolve(o, cc)
      expect_lte(nrow(d), 1L)
    }
  }
  o20 <- ex$objects[[which(ex$provenance$vintage == "2020")]]
  expect_equal(energyRt:::.lifespan_resolve(o20, "olife")$olife, 25L)
  expect_equal(energyRt:::.lifespan_resolve(o20, "start")$start, 2020L)
  expect_equal(energyRt:::.lifespan_resolve(o20, "end")$end, 2029L)
})

test_that("devintage keeps both window and life from a multi-row cell", {
  t1 <- .vl_tech(data.frame(vintage = c(NA, "2020", "2030"),
                            start = c(NA, 2020L, 2030L),
                            end = c(NA, 2029L, NA),
                            olife = c(25L, NA, NA)))
  ex <- energyRt:::.expand_one_process(t1)
  dv <- energyRt:::.levcost_devintage(ex$objects[[1]], "R1_VIN2020")
  expect_equal(nrow(dv@vintage), 1L)
  expect_equal(dv@vintage$start, 2020L)
  expect_equal(dv@vintage$end, 2029L)
  expect_equal(dv@vintage$olife, 25L)   # the broadcast life survives
})

# A trade's lifespan belongs to the route, not to a region: `pTradeOlife` is
# `{trade}` and `mTradeSpan`/`mTradeNew` are (trade, year) in every solver
# template. `trade@vintage` therefore carries no `region` column at all, so a
# per-region lifespan is refused at construction with the remedy named, rather
# than collapsing silently or surfacing as a duplicate-key complaint.
.vl_trade <- function(...) {
  newTrade("TRD", commodity = "ELC", cap2act = 1,
           routes = data.frame(src = "R1", dst = "R2"),
           invcost = data.frame(invcost = 10), ...)
}

test_that("trade@vintage has no region column", {
  expect_false("region" %in% colnames(methods::new("trade")@vintage))
  # technology and storage keep theirs: their olife IS region-indexed
  expect_true("region" %in% colnames(methods::new("technology")@vintage))
  expect_true("region" %in% colnames(methods::new("storage")@vintage))
})

test_that("a per-region trade lifespan is refused, whichever way it arrives", {
  msg <- "lifespan carries no .region."
  # the `vintage =` table
  expect_error(.vl_trade(vintage = data.frame(region = "R1", olife = 20L)), msg)
  # two rows differing only by region: the duplicate-key checker must not get
  # there first -- this is what it looked like before the column was removed
  expect_error(
    .vl_trade(vintage = data.frame(region = c("R1", "R2"), olife = c(20L, 30L))),
    msg)
  # and the legacy `olife = data.frame(region = , olife = )` form, which
  # reaches the vintage table by a different route
  expect_error(.vl_trade(olife = data.frame(region = "R1", olife = 20L)), msg)
  # update() takes the same path
  expect_error(update(.vl_trade(vintage = data.frame(olife = 20L)),
                      vintage = data.frame(region = "R1", olife = 30L)), msg)
})

test_that("an all-NA region column is dropped, not refused", {
  # objects written before the column was removed, and frames built to the
  # common technology/storage shape, carry an empty `region`
  tr <- .vl_trade(vintage = data.frame(region = NA_character_, olife = 20L))
  expect_false("region" %in% colnames(tr@vintage))
  expect_equal(tr@vintage$olife, 20L)
})

test_that("trade lifespans still build and read back", {
  expect_equal(.vl_trade(vintage = data.frame(olife = 20L))@vintage$olife, 20L)
  expect_equal(.vl_trade(olife = 20L)@vintage$olife, 20L)
  tr <- .vl_trade(start = 2020L, end = 2040L, olife = 20L)
  expect_equal(c(tr@vintage$start, tr@vintage$end), c(2020L, 2040L))
  # reads come back on the common shape, region supplied as NA
  d <- energyRt:::.lifespan_col(tr, "olife")
  expect_named(d, c("vintage", "region", "cluster", "olife"))
  expect_true(is.na(d$region))
})
