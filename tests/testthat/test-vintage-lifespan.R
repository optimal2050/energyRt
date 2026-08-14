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
  # duplicate key with the SAME value: deduplicated silently
  t1 <- .vl_tech(data.frame(vintage = c("2020", "2020"), olife = c(20L, 20L)))
  expect_equal(nrow(energyRt:::.lifespan_resolve(t1, "olife")), 1L)
  # duplicate key with DIFFERENT values: hard error naming the process
  t2 <- .vl_tech(data.frame(vintage = c("2020", "2020"), olife = c(20L, 30L)))
  expect_error(energyRt:::.lifespan_resolve(t2, "olife"),
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
