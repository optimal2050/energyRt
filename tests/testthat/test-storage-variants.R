# Vintage and cluster variants for `storage`.
#
# Storage has the strongest vintage case in the model -- battery capex is its
# dominant time-varying cost. Its cluster case is site-constrained capacity
# (pumped-hydro head classes, CAES caverns) plus duration classes expressed as a
# per-cluster `duration`.

# `vt_stg_model()` now lives in helper-variants.R so other test files can use it.

test_that("storage keeps its lifespan in @vintage", {
  sn <- slotNames("storage")
  expect_true(all(c("vintage", "cluster") %in% sn))
  expect_false(any(c("start", "end", "olife") %in% sn))
  p <- new("storage")
  expect_identical(names(p@vintage),
                   c("vintage", "region", "cluster", "start", "end", "olife"))
  expect_identical(names(p@cluster), c("cluster", "desc", "region", "order"))
})

test_that("legacy start/end/olife arguments still work on storage", {
  s <- newStorage("BATT", commodity = "ELC",
                  start = 2020L, end = 2050L, olife = list(olife = 15))
  expect_equal(nrow(s@vintage), 1L)
  expect_equal(c(s@vintage$start, s@vintage$end, s@vintage$olife),
               c(2020L, 2050L, 15L))
  expect_equal(nrow(update(s, olife = list(olife = 12))@vintage), 1L)
})

test_that("duration accepts a scalar and varies by cluster", {
  s1 <- newStorage("BATT", commodity = "ELC", duration = 4)
  expect_equal(nrow(s1@duration), 1L)
  expect_equal(s1@duration$duration, 4)

  s2 <- newStorage("BATT", commodity = "ELC",
                   cluster = c("h1", "h4"),
                   duration = data.frame(cluster = c("h1", "h4"),
                                        duration = c(1, 4)))
  r <- vt_expand_one(s2)
  expect_equal(length(r$objects), 2L)
  vals <- vapply(r$objects, function(o) o@duration$duration, numeric(1))
  expect_setequal(vals, c(1, 4))
})

test_that("storage vintages expand with per-vintage capex and windows", {
  BATT <- newStorage(
    "BATT", commodity = "ELC",
    # explicit point windows: NA sides are unbounded, nothing defaults
    # to the vintage year
    vintage = data.frame(vintage = c("2020", "2030"),
                         start = c(2020L, 2030L), end = c(2020L, 2030L),
                         olife = c(15L, 15L)),
    invcost = data.frame(vintage = c("2020", "2030"), invcost = c(300, 180)),
    duration = 4)

  sc <- vt_interp(vt_stg_model(BATT, "sv"), "sv")
  prov <- getVariants(sc, class = "storage")
  expect_setequal(prov$name, c("BATT_VIN2020", "BATT_VIN2030"))
  expect_true(all(prov$class == "storage"))
  expect_setequal(sc@modInp@sets$stg, c("BATT_VIN2020", "BATT_VIN2030"))

  win <- sc@modInp@sets$process_invest_window
  w20 <- win[win$process == "BATT_VIN2020", ]
  expect_true(all(w20$start == 2020) && all(w20$end == 2020))

  inv <- suppressMessages(getData(sc, "pStorageInvcost", merge = TRUE))
  expect_setequal(round(unique(inv$value)), c(180, 300))
  # per-vintage olife reaches the solver through pStorageOlife (stg, region)
  ol <- suppressMessages(getData(sc, "pStorageOlife", merge = TRUE))
  expect_true(all(ol$value == 15))
})

test_that("storage clusters expand with per-cluster potential", {
  PHS <- newStorage(
    "PHS", commodity = "ELC",
    cluster = data.frame(cluster = c("siteA", "siteB"), order = 1:2),
    invcost = data.frame(cluster = c("siteA", "siteB"), invcost = c(900, 1400)),
    capacity = data.frame(cluster = c("siteA", "siteB"), cap.up = c(2, 6)),
    vintage = data.frame(olife = 40L), duration = 8)

  sc <- vt_interp(vt_stg_model(PHS, "sc"), "sc")
  prov <- getVariants(sc, class = "storage")
  expect_setequal(prov$name, c("PHS_CLsiteA", "PHS_CLsiteB"))
  expect_identical(prov$cluster, c("siteA", "siteB"))    # declared order
  cap <- suppressMessages(getData(sc, "pStorageCap", merge = TRUE))
  expect_setequal(round(unique(cap$value)), c(2, 6))
})

test_that("an undeclared storage cluster label is rejected", {
  BAD <- newStorage("PHS", commodity = "ELC",
                    cluster = c("siteA", "siteB"),
                    invcost = data.frame(cluster = c("siteA", "siteB"),
                                         invcost = c(900, 1400)),
                    capacity = data.frame(cluster = c("siteX", "siteB"),
                                          cap.up = c(2, 6)))
  expect_error(vt_expand_one(BAD), "Undeclared cluster")
})

test_that("storage group bounds cap the sum across clusters", {
  skip_if_no_solver()
  PHS <- newStorage(
    "PHS", commodity = "ELC",
    cluster = c("siteA", "siteB"),
    invcost = data.frame(cluster = c("siteA", "siteB"), invcost = c(20, 25)),
    capacity = data.frame(cluster = "TOTAL", cap.up = 3),
    vintage = data.frame(olife = 40L), duration = 4)
  sc <- vt_interp(vt_stg_model(PHS, "sg"), "sg")
  expect_length(sc@modInp@gams.equation, 1L)

  sol <- vt_solve(sc)
  d <- suppressMessages(getData(sol, "vStorageCap", merge = TRUE))
  d <- d[grepl("^PHS", d$stg), ]
  if (nrow(d) > 0) {
    tot <- stats::aggregate(list(v = d$value),
                            by = list(r = d$region, y = d$year), FUN = sum)
    expect_lte(max(tot$v), 3 + 1e-6)
  }
})

test_that("ret.* group bounds are refused for storage", {
  S <- newStorage("PHS", commodity = "ELC", cluster = c("a", "b"),
                  invcost = data.frame(cluster = c("a", "b"), invcost = c(20, 25)),
                  capacity = data.frame(cluster = "TOTAL", ret.up = 3))
  expect_error(vt_interp(vt_stg_model(S, "sr"), "sr"), "retirement")
})

test_that("storage results carry provenance keyed on `stg`", {
  BATT <- newStorage(
    "BATT", commodity = "ELC",
    cluster = c("h1", "h4"),
    invcost = data.frame(cluster = c("h1", "h4"), invcost = c(20, 30)),
    duration = data.frame(cluster = c("h1", "h4"), duration = c(1, 4)),
    vintage = data.frame(olife = 15L))
  sc <- vt_interp(vt_stg_model(BATT, "sp"), "sp")

  # `.attach_variants()` used to look only for a `tech`/`process` column, so a
  # storage-keyed frame silently got no provenance.
  d <- suppressMessages(getData(sc, "pStorageInvcost", merge = TRUE))
  expect_true("stg" %in% names(d))
  expect_true(all(c("base", "class", "vintage", "cluster") %in% names(d)))
  expect_true(all(d$base == "BATT"))
  expect_true(all(d$class == "storage"))
  expect_setequal(unique(d$cluster), c("h1", "h4"))

  # and the unified table can be filtered by class
  expect_equal(nrow(getVariants(sc, class = "storage")), 2L)
  expect_null(getVariants(sc, class = "trade"))
})
