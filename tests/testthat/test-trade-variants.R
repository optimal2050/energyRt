# Vintaging for `trade`, plus the removal of `@capacityVariable`.
#
# Trade gets vintage but NOT cluster: `@routes` (src/dst) already provides
# multiplicity, so a cluster axis would be a redundant second one. Several
# vintages of one corridor therefore mean several capacities on the SAME
# (src, dst) route, with their flows summed in the commodity balance.
#
# `@capacityVariable` was removed. In this version it gated nothing --
# `mTradeCapacityVariable` was declared in the templates but referenced by none
# of them, and vTradeCap/eqTradeCap/eqTradeCapFlow were built either way, so a
# `FALSE` trade still paid investment and fixed O&M. Its only live effect made
# the flow domain LARGER, contrary to its stated purpose.

vt_trade_model <- function(trd, name = "tr") {
  vt_model(
    newTechnology("EWIN", output = list(comm = "ELC"),
                  invcost = data.frame(invcost = 150),
                  afs = data.frame(timeslice = "ANNUAL", afs.up = 0.4),
                  vintage = data.frame(olife = 25L), cap2act = 1),
    trd, name = name)
}

test_that("trade keeps its lifespan in @vintage and lost capacityVariable", {
  sn <- slotNames("trade")
  expect_true("vintage" %in% sn)
  expect_false(any(c("start", "end", "olife", "capacityVariable") %in% sn))
  expect_identical(names(new("trade")@vintage),
                   c("vintage", "region", "cluster", "start", "end", "olife"))
})

test_that("capacityVariable errors with a migration message", {
  rt <- data.frame(src = "R1", dst = "R2")
  expect_error(newTrade("TBD", commodity = "ELC", routes = rt,
                        capacityVariable = TRUE), "has been removed")
  expect_error(newTrade("TBD", commodity = "ELC", routes = rt,
                        capacityVariable = FALSE), "ava.up")
  trd <- newTrade("TBD", commodity = "ELC", routes = rt)
  expect_error(update(trd, capacityVariable = FALSE), "has been removed")
})

test_that("the -Inf/Inf constructor defaults produce no vintage row", {
  # newTrade() defaults to start = -Inf / end = Inf meaning "no restriction";
  # folding those in verbatim would give every trade a spurious vintage row and
  # an as.integer(Inf) warning.
  trd <- newTrade("TBD", commodity = "ELC",
                  routes = data.frame(src = "R1", dst = "R2"))
  expect_equal(nrow(trd@vintage), 0L)
  expect_length(vt_expand_one(trd)$objects, 1L)
  expect_null(vt_expand_one(trd)$provenance)
})

test_that("legacy start/end/olife arguments still work on trade", {
  trd <- newTrade("TBD", commodity = "ELC",
                  routes = data.frame(src = "R1", dst = "R2"),
                  start = 2020L, end = 2050L, olife = list(olife = 40))
  expect_equal(nrow(trd@vintage), 1L)
  expect_equal(c(trd@vintage$start, trd@vintage$end, trd@vintage$olife),
               c(2020L, 2050L, 40L))
})

test_that("a vintaged corridor expands, keeping routes and per-vintage capex", {
  rt <- data.frame(src = c("R1", "R2"), dst = c("R2", "R1"))
  TBD <- newTrade(
    "TBD_ELC", commodity = "ELC", routes = rt,
    trade = data.frame(src = c("R1", "R2"), dst = c("R2", "R1"), teff = 0.95),
    # explicit point windows: NA sides are unbounded, nothing defaults
    # to the vintage year
    vintage = data.frame(vintage = c("2020", "2030"),
                         start = c(2020L, 2030L), end = c(2020L, 2030L),
                         olife = c(40L, 40L)),
    invcost = data.frame(vintage = c("2020", "2030"), invcost = c(200, 140)),
    cap2act = 1)

  r <- vt_expand_one(TBD)
  expect_length(r$objects, 2L)
  expect_setequal(r$provenance$name, c("TBD_ELC_VIN2020", "TBD_ELC_VIN2030"))
  expect_true(all(r$provenance$class == "trade"))
  expect_true(all(is.na(r$provenance$cluster)))   # trade has no cluster axis
  # @routes is structural: identical on every variant
  for (o in r$objects) expect_equal(o@routes[, c("src", "dst")], rt)

  sc <- vt_interp(vt_trade_model(TBD, "tv"), "tv")
  expect_setequal(sc@modInp@sets$trade,
                  c("TBD_ELC_VIN2020", "TBD_ELC_VIN2030"))
  win <- sc@modInp@sets$process_invest_window
  w20 <- win[win$process == "TBD_ELC_VIN2020", ]
  expect_true(all(w20$start == 2020) && all(w20$end == 2020))

  inv <- suppressMessages(getData(sc, "pTradeInvcost", merge = TRUE))
  expect_setequal(round(unique(inv$value)), c(140, 200))
  # per-vintage olife reaches the solver through pTradeOlife (trade), because
  # each variant is its own `trade` set member -- no new dimension needed
  ol <- suppressMessages(getData(sc, "pTradeOlife", merge = TRUE))
  expect_true(all(ol$value == 40))
  expect_equal(nrow(ol), 2L)
})

test_that("trade results carry provenance keyed on `trade`", {
  TBD <- newTrade(
    "TBD_ELC", commodity = "ELC",
    routes = data.frame(src = "R1", dst = "R2"),
    vintage = data.frame(vintage = c("2020", "2030"), olife = c(40L, 40L)),
    invcost = data.frame(vintage = c("2020", "2030"), invcost = c(200, 140)),
    cap2act = 1)
  sc <- vt_interp(vt_trade_model(TBD, "tp"), "tp")

  d <- suppressMessages(getData(sc, "pTradeInvcost", merge = TRUE))
  expect_true("trade" %in% names(d))
  expect_true(all(c("base", "class", "vintage") %in% names(d)))
  expect_true(all(d$base == "TBD_ELC"))
  expect_true(all(d$class == "trade"))
  expect_equal(nrow(getVariants(sc, class = "trade")), 2L)
})

test_that("a trade group bound spans years, and a region on it errors", {
  rt <- data.frame(src = "R1", dst = "R2")
  ok <- newTrade(
    "TBD_ELC", commodity = "ELC", routes = rt,
    vintage = data.frame(vintage = c("2020", "2030"), olife = c(40L, 40L)),
    invcost = data.frame(vintage = c("2020", "2030"), invcost = c(200, 140)),
    capacity = data.frame(vintage = "TOTAL", cap.up = 4), cap2act = 1)
  sc <- vt_interp(vt_trade_model(ok, "tg"), "tg")
  expect_length(sc@modInp@gams.equation, 1L)

  # `vTradeCap{trade, year}` has no region index, so a region-restricted group
  # bound is not expressible: an unmatched for.each dim is auto-summed by
  # newConstraint(), which would silently replicate the same constraint per
  # region. `trade@capacity` therefore has no `region` column at all (it is
  # commented out in the prototype), which rejects the case at the schema level
  # -- earlier and more cheaply than a runtime guard could.
  expect_false("region" %in% names(new("trade")@capacity))
  expect_error(
    newTrade("TBD_ELC", commodity = "ELC", routes = rt,
             capacity = data.frame(vintage = "TOTAL", region = "R1",
                                   cap.up = 4)),
    "Unknown column")
})

test_that("mTradeCapacityVariable is gone from the R side", {
  expect_false("mTradeCapacityVariable" %in%
                 names(getFromNamespace(".modInp", "energyRt")))
  expect_false("mTradeCapacityVariable" %in%
                 names(getFromNamespace(".mapping_spec", "energyRt")))
})

test_that("per-region trade lifespan is refused (pTradeOlife has no region)", {
  rt <- data.frame(src = "R1", dst = "R2")
  TRG <- newTrade(
    "TRG_ELC", commodity = "ELC", routes = rt,
    trade = data.frame(src = "R1", dst = "R2", teff = 0.95),
    vintage = data.frame(region = "R1", olife = 40L),
    cap2act = 1)
  expect_error(vt_interp(vt_trade_model(TRG, "trg"), "trg"),
               "region")
})
