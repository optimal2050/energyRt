# Vintage / cluster expansion. A vintaged or clustered `technology` is replaced
# at interpolation time by one ordinary technology per (vintage, cluster) cell,
# so the solver core -- equations, mappings, templates -- is untouched. The link
# back to the base technology is the provenance table, never the mangled name.

test_that("clusters expand into one technology each, keeping per-cluster data", {
  WIN <- newTechnology(
    "EWIN", output = list(comm = "ELC"),
    invcost  = data.frame(cluster = c("best", "mid", "poor"),
                          invcost = c(1500, 1300, 1100)),
    afs      = data.frame(cluster = c("best", "mid", "poor"), timeslice = "ANNUAL",
                          afs.up = c(0.45, 0.32, 0.20)),
    capacity = data.frame(cluster = c("best", "mid", "poor"),
                          cap.up = c(4, 6, 100)),
    vintage  = data.frame(olife = 25L), cap2act = 1)

  sc <- vt_interp(vt_model(WIN, name = "clu"), "clu")
  prov <- sc@modInp@sets$tech_variant

  expect_setequal(sc@modInp@sets$tech,
                  c("ECOA", "EWIN_CLbest", "EWIN_CLmid", "EWIN_CLpoor"))
  expect_false("EWIN" %in% sc@modInp@sets$tech)   # base replaced, not kept
  expect_true(all(prov$base == "EWIN"))
  expect_true(all(is.na(prov$vintage)))

  inv <- suppressMessages(getData(sc, "pTechInvcost", merge = TRUE))
  inv <- inv[grepl("^EWIN", inv$tech), ]
  expect_setequal(round(unique(inv$value)), c(1100, 1300, 1500))
  expect_equal(nrow(unique(inv[, c("tech", "value")])), 3L)
})

test_that("vintages are investable only in their own period and keep their data", {
  GAS <- newTechnology(
    "EGAS", input = list(comm = "COA"), output = list(comm = "ELC"),
    # windows are USER-DEFINED (NA side = unbounded), so point windows
    # must be declared explicitly
    vintage = data.frame(vintage = c("2020", "2030"),
                         start = c(2020L, 2030L), end = c(2020L, 2030L),
                         olife = c(30L, 30L)),
    invcost = data.frame(vintage = c("2020", "2030"), invcost = c(1200, 1000)),
    ceff    = data.frame(vintage = c("2020", "2030"), comm = "COA",
                         cinp2use = c(0.35, 0.45)),
    cap2act = 1)

  sv <- vt_interp(vt_model(GAS, name = "vin"), "vin")
  pv <- sv@modInp@sets$tech_variant
  expect_setequal(pv$tech, c("EGAS_VIN2020", "EGAS_VIN2030"))

  win <- sv@modInp@sets$process_invest_window
  w20 <- win[win$process == "EGAS_VIN2020", ]
  expect_true(all(w20$start == 2020) && all(w20$end == 2020))
  w30 <- win[win$process == "EGAS_VIN2030", ]
  expect_true(all(w30$start == 2030) && all(w30$end == 2030))

  eff <- suppressMessages(getData(sv, "pTechCinp2use", merge = TRUE))
  expect_setequal(round(unique(eff$value), 2), c(0.35, 0.45))
})

test_that("bare vintages (no start/end) keep unbounded windows", {
  # user ruling 2026-08-13: windows are user-defined and may overlap;
  # NA start = all years up to end, NA end = all years from start on,
  # both NA = the full model horizon. Nothing defaults to the vintage
  # year any more.
  GAS <- newTechnology(
    "EGAS", input = list(comm = "COA"), output = list(comm = "ELC"),
    vintage = data.frame(vintage = c("2020", "2030"), olife = 30L),
    invcost = data.frame(vintage = c("2020", "2030"),
                         invcost = c(1200, 1000)),
    cap2act = 1)
  ex <- vt_expand_one(GAS)
  for (o in ex$objects) {
    expect_true(all(is.na(o@vintage$start)))
    expect_true(all(is.na(o@vintage$end)))
  }
  # one-sided windows stay one-sided
  GAS2 <- newTechnology(
    "EGA2", input = list(comm = "COA"), output = list(comm = "ELC"),
    vintage = data.frame(vintage = c("2020", "2030"),
                         start = c(NA, 2030L), end = c(2029L, NA),
                         olife = 30L),
    invcost = data.frame(vintage = c("2020", "2030"),
                         invcost = c(1200, 1000)),
    cap2act = 1)
  ex2 <- vt_expand_one(GAS2)
  v20 <- ex2$objects[[which(ex2$provenance$vintage == "2020")]]@vintage
  expect_true(is.na(v20$start) && v20$end == 2029)
  v30 <- ex2$objects[[which(ex2$provenance$vintage == "2030")]]@vintage
  expect_true(v30$start == 2030 && is.na(v30$end))
})

test_that("vintage x cluster produces the full grid", {
  BOTH <- newTechnology(
    "ESOL", output = list(comm = "ELC"),
    vintage = data.frame(vintage = c("2020", "2030"), olife = c(25L, 25L)),
    invcost = data.frame(vintage = rep(c("2020", "2030"), each = 2),
                         cluster = rep(c("a", "b"), 2),
                         invcost = c(2000, 2200, 1500, 1700)),
    cap2act = 1)
  sb <- vt_interp(vt_model(BOTH, name = "both"), "both")
  pb <- sb@modInp@sets$tech_variant
  expect_equal(nrow(pb), 4L)
  expect_true("ESOL_VIN2030_CLb" %in% pb$tech)

  ib <- suppressMessages(getData(sb, "pTechInvcost", merge = TRUE))
  ib <- ib[grepl("^ESOL", ib$tech), ]
  expect_setequal(round(unique(ib$value)), c(1500, 1700, 2000, 2200))
})

test_that("an un-vintaged technology passes through untouched", {
  PLAIN <- newTechnology("EPLAIN", input = list(comm = "COA"),
                         output = list(comm = "ELC"),
                         invcost = data.frame(invcost = 1000),
                         vintage = data.frame(olife = 30L), cap2act = 1)
  sp <- vt_interp(vt_model(PLAIN, name = "plain"), "plain")
  expect_setequal(sp@modInp@sets$tech, c("ECOA", "EPLAIN"))
  expect_null(sp@modInp@sets$tech_variant)
})

test_that("results carry base/vintage/cluster and roll up by default", {
  skip_if_no_solver()
  WIN <- newTechnology(
    "EWIN", output = list(comm = "ELC"),
    invcost  = data.frame(cluster = c("best", "mid"), invcost = c(150, 130)),
    afs      = data.frame(cluster = c("best", "mid"), timeslice = "ANNUAL",
                          afs.up = c(0.45, 0.32)),
    capacity = data.frame(cluster = c("best", "mid"), cap.up = c(4, 6)),
    vintage  = data.frame(olife = 25L), cap2act = 1)
  sol <- vt_solve(vt_interp(vt_model(WIN, name = "rep"), "rep"))

  d <- suppressMessages(getData(sol, "vTechCap", merge = TRUE))
  expect_true(all(c("base", "vintage", "cluster") %in% names(d)))
  expect_true(all(d$base[grepl("^EWIN", d$tech)] == "EWIN"))
  expect_true(all(is.na(d$base[d$tech == "ECOA"])))  # non-variant -> NA

  mix <- getMix(sol, "capacity")
  expect_true("EWIN" %in% mix$process)          # rolled up to the base
  expect_false(any(grepl("_CL", mix$process)))
  mix2 <- getMix(sol, "capacity", by = "cluster")
  expect_true("cluster" %in% names(mix2))
  expect_true(all(c("best", "mid") %in% mix2$cluster))
})

test_that("broadcast olife + vintage-keyed windows yield clean model input", {
  # the cell shape that used to corrupt input: a broadcast olife row plus
  # per-vintage window rows -- windows must stay per-variant (no union)
  # and pTechOlife must hold exactly one row per (tech, region)
  GAS <- newTechnology(
    "EGAS", input = list(comm = "COA"), output = list(comm = "ELC"),
    vintage = data.frame(vintage = c(NA, "2020", "2030"),
                         start = c(NA, 2020L, 2030L),
                         end = c(NA, 2029L, NA),
                         olife = c(30L, NA, NA)),
    invcost = data.frame(vintage = c("2020", "2030"),
                         invcost = c(1200, 1000)),
    ceff = data.frame(comm = "COA", cinp2use = 0.4),
    cap2act = 1)
  sv <- vt_interp(vt_model(GAS, name = "vw"), "vw")

  win <- sv@modInp@sets$process_invest_window
  w20 <- win[win$process == "EGAS_VIN2020", ]
  expect_true(nrow(w20) > 0)
  expect_true(all(w20$start == 2020 & w20$end == 2029))
  w30 <- win[win$process == "EGAS_VIN2030", ]
  expect_true(all(w30$start == 2030 & is.na(w30$end)))
  # one window row per (process, region): no cartesian duplicates
  expect_false(any(duplicated(win[, c("process", "region")])))

  ol <- sv@modInp@parameters[["pTechOlife"]]@data
  for (tt in c("EGAS_VIN2020", "EGAS_VIN2030")) {
    rows <- ol[ol$tech == tt, ]
    expect_true(nrow(rows) > 0)
    expect_false(any(duplicated(rows[, c("tech", "region")])))
    expect_true(all(rows$value == 30))
  }
})
