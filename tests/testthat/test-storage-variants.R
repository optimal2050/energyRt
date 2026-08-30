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
  # `duration` is a BOUND now. An unnamed number means the ratio is FIXED, so
  # the scalar shorthand normalises to `duration.fx` -- `.pack_bounds_long()`
  # reads only the .lo/.up/.fx suffixes, and a bare column would be read as
  # nothing at all, silently untying the store's energy from its power.
  s1 <- newStorage("BATT", commodity = "ELC", duration = 4)
  expect_equal(nrow(s1@duration), 1L)
  expect_equal(s1@duration$duration.fx, 4)
  expect_true(is.na(s1@duration$duration.lo))
  expect_true(is.na(s1@duration$duration.up))

  s2 <- newStorage("BATT", commodity = "ELC",
                   cluster = c("h1", "h4"),
                   duration = data.frame(cluster = c("h1", "h4"),
                                        duration = c(1, 4)))
  r <- vt_expand_one(s2)
  expect_equal(length(r$objects), 2L)
  vals <- vapply(r$objects, function(o) o@duration$duration.fx, numeric(1))
  expect_setequal(vals, c(1, 4))
})

test_that("a one-sided duration range leaves the other side open", {
  # `pStorageDuration` defaults to [1, 1] so a storage that says nothing keeps
  # the legacy tie. That default must NOT leak into a deliberate range: an
  # `up`-only spec would otherwise also inherit `lo = 1` and forbid anything
  # shorter than an hour, which is not what "up to 8 hours" means.
  up_only <- newStorage("B1", commodity = "ELC",
                        duration = data.frame(duration.up = 8))@duration
  expect_equal(up_only$duration.lo, 0)
  expect_equal(up_only$duration.up, 8)

  lo_only <- newStorage("B2", commodity = "ELC",
                        duration = data.frame(duration.lo = 2))@duration
  expect_equal(lo_only$duration.lo, 2)
  expect_equal(lo_only$duration.up, Inf)

  both <- newStorage("B3", commodity = "ELC",
                     duration = data.frame(duration.lo = 2,
                                           duration.up = 8))@duration
  expect_equal(both$duration.lo, 2)
  expect_equal(both$duration.up, 8)

  # nothing supplied -> empty slot; the [1, 1] parameter default does the tying
  expect_equal(nrow(newStorage("B4", commodity = "ELC")@duration), 0L)
})

test_that("the deprecated cap2stg reaches @duration as a fixed ratio", {
  # The formal default used to be a bare `1`, so `.given(args$duration)` was
  # always TRUE and this path could only ever raise "supply either ... not
  # both". It could not have worked for anyone.
  s <- suppressWarnings(newStorage("OLD", commodity = "ELC", cap2stg = 4))
  expect_equal(s@duration$duration.fx, 4)
  expect_error(
    suppressWarnings(newStorage("OLD", commodity = "ELC",
                                cap2stg = 4, duration = 6)),
    "not both"
  )
})

test_that("storage vintages expand with per-vintage capex and windows", {
  BATT <- newStorage(
    "BATT", commodity = "ELC",
    # explicit point windows: NA sides are unbounded, nothing defaults
    # to the vintage year
    vintage = data.frame(vintage = c("2020", "2030"),
                         start = c(2020L, 2030L), end = c(2020L, 2030L),
                         olife = c(15L, 15L)),
    invcost = data.frame(vintage = c("2020", "2030"), out.invcost = c(300, 180)),
    duration = 4)

  sc <- vt_interp(vt_stg_model(BATT, "sv"), "sv")
  prov <- getVariants(sc, class = "storage")
  expect_setequal(prov$name, c("BATT_VIN2020", "BATT_VIN2030"))
  expect_true(all(prov$class == "storage"))
  expect_setequal(sc@modInp@sets$stg, c("BATT_VIN2020", "BATT_VIN2030"))

  win <- sc@modInp@sets$process_invest_window
  w20 <- win[win$process == "BATT_VIN2020", ]
  expect_true(all(w20$start == 2020) && all(w20$end == 2020))

  inv <- suppressMessages(getData(sc, "pStorageOutInvcost", merge = TRUE))
  expect_setequal(round(unique(inv$value)), c(180, 300))
  # per-vintage olife reaches the solver through pStorageOlife (stg, region)
  ol <- suppressMessages(getData(sc, "pStorageOlife", merge = TRUE))
  expect_true(all(ol$value == 15))
})

test_that("storage clusters expand with per-cluster potential", {
  PHS <- newStorage(
    "PHS", commodity = "ELC",
    cluster = data.frame(cluster = c("siteA", "siteB"), order = 1:2),
    invcost = data.frame(cluster = c("siteA", "siteB"), out.invcost = c(900, 1400)),
    capacity = data.frame(cluster = c("siteA", "siteB"), out.cap.up = c(2, 6)),
    vintage = data.frame(olife = 40L), duration = 8)

  sc <- vt_interp(vt_stg_model(PHS, "sc"), "sc")
  prov <- getVariants(sc, class = "storage")
  expect_setequal(prov$name, c("PHS_CLsiteA", "PHS_CLsiteB"))
  expect_identical(prov$cluster, c("siteA", "siteB"))    # declared order
  cap <- suppressMessages(getData(sc, "pStorageOutCap", merge = TRUE))
  expect_setequal(round(unique(cap$value)), c(2, 6))
})

test_that("an undeclared storage cluster label is rejected", {
  BAD <- newStorage("PHS", commodity = "ELC",
                    cluster = c("siteA", "siteB"),
                    invcost = data.frame(cluster = c("siteA", "siteB"),
                                         out.invcost = c(900, 1400)),
                    capacity = data.frame(cluster = c("siteX", "siteB"),
                                          out.cap.up = c(2, 6)))
  expect_error(vt_expand_one(BAD), "Undeclared cluster")
})

test_that("storage group bounds cap the sum across clusters", {
  skip_if_no_solver()
  PHS <- newStorage(
    "PHS", commodity = "ELC",
    cluster = c("siteA", "siteB"),
    invcost = data.frame(cluster = c("siteA", "siteB"), out.invcost = c(20, 25)),
    capacity = data.frame(cluster = "TOTAL", out.cap.up = 3),
    vintage = data.frame(olife = 40L), duration = 4)
  sc <- vt_interp(vt_stg_model(PHS, "sg"), "sg")
  expect_length(sc@modInp@user_constraints, 1L)

  sol <- vt_solve(sc)
  d <- suppressMessages(getData(sol, "vStorageOutCap", merge = TRUE))
  d <- d[grepl("^PHS", d$stg), ]
  if (nrow(d) > 0) {
    tot <- stats::aggregate(list(v = d$value),
                            by = list(r = d$region, y = d$year), FUN = sum)
    expect_lte(max(tot$v), 3 + 1e-6)
  }
})

test_that("ret.* group bounds are refused for storage", {
  S <- newStorage("PHS", commodity = "ELC", cluster = c("a", "b"),
                  invcost = data.frame(cluster = c("a", "b"), out.invcost = c(20, 25)),
                  capacity = data.frame(cluster = "TOTAL", out.ret.up = 3))
  expect_error(vt_interp(vt_stg_model(S, "sr"), "sr"), "retirement")
})

test_that("storage results carry provenance keyed on `stg`", {
  BATT <- newStorage(
    "BATT", commodity = "ELC",
    cluster = c("h1", "h4"),
    invcost = data.frame(cluster = c("h1", "h4"), out.invcost = c(20, 30)),
    duration = data.frame(cluster = c("h1", "h4"), duration = c(1, 4)),
    vintage = data.frame(olife = 15L))
  sc <- vt_interp(vt_stg_model(BATT, "sp"), "sp")

  # `.attach_variants()` used to look only for a `tech`/`process` column, so a
  # storage-keyed frame silently got no provenance.
  d <- suppressMessages(getData(sc, "pStorageOutInvcost", merge = TRUE))
  expect_true("stg" %in% names(d))
  expect_true(all(c("base", "class", "vintage", "cluster") %in% names(d)))
  expect_true(all(d$base == "BATT"))
  expect_true(all(d$class == "storage"))
  expect_setequal(unique(d$cluster), c("h1", "h4"))

  # and the unified table can be filtered by class
  expect_equal(nrow(getVariants(sc, class = "storage")), 2L)
  expect_null(getVariants(sc, class = "trade"))
})


test_that("each seff coefficient is filed under its OWN role's commodity", {
  # `@seff` feeds THREE parameters -- pStorageInpEff, pStorageOutEff and
  # pStorageStgEff -- and the slot has no `comm` column, so `ob2mi()` fills one
  # per parameter from the matching role. It used to assign back into the shared
  # `slot_data`, so whichever parameter was processed first stamped `comm` and
  # the other two silently inherited it.
  #
  # Invisible while all three roles held the same commodity, and wrong the moment
  # they differ: an EV storing kWh and selling kilometres had its km-per-kWh
  # `outeff` filed under electricity, where nothing reads it, so the motor ran at
  # the default efficiency of 1 and the car drew six times too much from the grid.
  skip_if_no_solver()
  cal <- newCalendar(
    timetable = make_timetable(struct = list(
      ANNUAL = "ANNUAL", HOUR = sprintf("h%02d", 0:5))),
    name = "seff_cal")
  V <- data.frame(olife = 10L)
  mod <- newModel(
    name = "seff_roles", region = "R1", horizon = newHorizon(2020),
    discount = 0, calendar = cal,
    repo = newRepository("seff_repo",
      newCommodity("ELC", timeframe = "HOUR"),
      newCommodity("BAT", timeframe = "HOUR"),
      newCommodity("KM",  timeframe = "HOUR"),
      newSupply("GRID", commodity = "ELC", supply = data.frame(cost = 1)),
      newStorage("CAR",
        input    = list(comm = "ELC"),
        storage  = list(comm = "BAT"),
        output   = list(comm = "KM"),
        seff     = data.frame(inpeff = 0.90, outeff = 6),
        duration = data.frame(duration.lo = 0, duration.up = 1e6),
        vintage  = V),
      newDemand("TRIPS", commodity = "KM",
                demand = data.frame(timeslice = "h03", demand = 60))))

  scen <- suppressMessages(suppressWarnings(
    interpolate_model(mod, name = "seff_roles", ondisk = FALSE)))
  gds <- getFromNamespace("get_data_slot", "energyRt")
  comm_of <- function(p) {
    d <- as.data.frame(gds(scen@modInp@parameters[[p]]))
    unique(as.character(d$comm))
  }
  expect_equal(comm_of("pStorageInpEff"), "ELC")
  expect_equal(comm_of("pStorageOutEff"), "KM")   # NOT "ELC"

  # and it shows up in the answer: 60 km / 6 km per kWh / 0.9 = 11.11 kWh
  sol <- suppressMessages(suppressWarnings(solve_model(mod, name = "seff_roles")))
  inp <- as.data.frame(getData(sol, "vStorageInp", merge = TRUE, drop.zeros = TRUE))
  expect_equal(sum(inp$value), 60 / 6 / 0.9, tolerance = 1e-6)
})
