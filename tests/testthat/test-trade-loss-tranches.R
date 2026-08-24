# =========================================================================== #
# Piecewise-linear transmission losses, by capacity tranches.
#
# `teff` loses a FIXED fraction of the flow. Real losses are QUADRATIC --
# `loss = r*f^2` -- so the loss FRACTION should rise with loading, and under a
# single `teff` a half-loaded and a fully-loaded line lose the same proportion.
#
# Splitting the line's capacity into tranches with rising loss rates makes the
# loss curve piecewise-linear and CONVEX, and convexity is the whole trick: a
# lossier tranche delivers strictly less per unit sent, so cost minimisation
# fills the cheap one first without any integer variable.
#
# With shares `w_t`, cumulative `alpha_t`:
#     lambda_t = loss_full * (alpha_{t-1} + alpha_t),   teff_t = 1 - lambda_t
# which is the SECANT slope across the tranche -- so the fit is exact at every
# breakpoint and overestimates in between (a chord of a convex function lies
# above it). PyPSA's tangent envelope errs the other way; neither is "more
# correct" at the same segment count.
# =========================================================================== #

lt_link <- function(nm, tr, cap = NULL, invcost = NULL, demand = 30,
                    dst_cost = 1e6) {
  newModel(
    name = nm, region = c("R1", "R2"), horizon = tr_hor(), discount = 0,
    calendar = tr_cal(),
    repo = newRepository("lt_repo",
      newCommodity("ELC", timeframe = "ANNUAL"),
      newSupply("SUP", commodity = "ELC",
                supply = data.frame(region = c("R1", "R2"),
                                    cost = c(1, dst_cost))),
      newDCLink("L", "ELC", "R1", "R2", cap2act = 1,
                vintage = data.frame(olife = 50L),
                trade = tr$trade, cluster = tr$cluster,
                capacity = if (!is.null(cap)) cap else tr$capacity,
                invcost = if (is.null(invcost)) data.frame() else
                  data.frame(invcost = invcost)),
      newDemand("DEM", commodity = "ELC",
                demand = data.frame(region = "R2", demand = demand))))
}

lt_solve <- function(m, nm) suppressMessages(
  solve_scen(suppressMessages(interpolate_model(m, name = nm))))

lt_flows <- function(s) {
  d <- as.data.frame(getData(s, "vTradeIr", merge = TRUE))
  d <- d[d$value > 1e-9, ]
  setNames(round(d$value, 6), as.character(d$trade))
}

# --------------------------------------------------------------------------- #
# The arithmetic, with no solver involved
# --------------------------------------------------------------------------- #

test_that("the tranche loss rates are the secant slopes", {
  # 4% at rated load, two equal halves: lambda = 0.04*(0+0.5) and 0.04*(0.5+1).
  tr <- lossTranches(shares = c(0.5, 0.5), loss = 0.04)
  expect_equal(tr$trade$teff, c(0.98, 0.94))

  # The RATIO of the two loss rates is 3, not 2 -- and 2 is unreachable with
  # equal halves, since (c + F)/c = 2 needs F = c. Worth pinning: "twice as
  # lossy for the upper half" is the intuitive-but-wrong guess.
  lam <- attr(tr, "lambda")
  expect_equal(lam[2] / lam[1], 3)

  # Calibration: the flow-weighted loss at full load IS the loss at full load.
  # This identity is what fails silently if the shares do not sum to 1.
  for (w in list(c(0.5, 0.5), c(0.4, 0.35, 0.25), c(0.1, 0.2, 0.3, 0.4))) {
    t2 <- lossTranches(shares = w, loss = 0.03)
    expect_equal(sum(w * attr(t2, "lambda")), 0.03)
  }

  # One tranche is the ordinary flat-`teff` line, exactly.
  expect_equal(lossTranches(1, loss = 0.03)$trade$teff, 0.97)

  # `resistance` x `capacity` is the same input by another route.
  expect_equal(lossTranches(c(0.5, 0.5), resistance = 0.02, capacity = 2)$trade$teff,
               lossTranches(c(0.5, 0.5), loss = 0.04)$trade$teff)
})

test_that("`lossTranches()` refuses what it cannot calibrate", {
  expect_error(lossTranches(c(0.5, 0.4), loss = 0.03), "sum to 0.9, not 1")
  expect_error(lossTranches(c(0.5, -0.5), loss = 0.03), "greater than zero")
  # Both inputs is an error, not a preference: silently picking one would hide
  # a disagreement between them.
  expect_error(lossTranches(c(0.5, 0.5), loss = 0.03, resistance = 0.01,
                            capacity = 2), "not both")
  expect_error(lossTranches(c(0.5, 0.5)), "does not determine a loss fraction")
  # A resistance/capacity pair in mismatched units gives a nonsense fraction.
  expect_error(lossTranches(c(0.5, 0.5), resistance = 0.01, capacity = 500),
               "not a loss fraction")
  # A loss so large the last tranche would have zero or negative efficiency.
  expect_error(lossTranches(c(0.5, 0.5), loss = 0.7), "must be below")
  expect_error(lossTranches(c(0.5, 0.5), loss = 0.03, labels = c("A", "A")),
               "must be unique")
})

test_that("the class carries the tranches, and refuses a region on them", {
  expect_true("cluster" %in% slotNames("trade"))
  expect_equal(names(new("trade")@cluster),
               c("cluster", "desc", "share", "order"))
  # No `region` column, deliberately: a trade object has no `@region` slot -- its
  # scope comes from the route endpoints -- so a region here could restrict
  # nothing. Its ABSENCE rejects the mistake at construction, which is earlier
  # and cheaper than a runtime guard inside variant expansion.
  expect_false("region" %in% names(new("trade")@cluster))
  expect_error(
    newDCLink("L", "ELC", "R1", "R2",
              cluster = data.frame(cluster = "A", region = "R1")),
    "Unknown column")
  # `@vintage`'s column set and order are unchanged.
  expect_equal(names(new("trade")@vintage),
               c("vintage", "region", "cluster", "start", "end", "olife"))
})

# --------------------------------------------------------------------------- #
# Expansion and the solve
# --------------------------------------------------------------------------- #

test_that("tranches expand into one trade object each", {
  tr <- lossTranches(c(0.5, 0.5), loss = 0.04, fix = 100)
  scen <- suppressMessages(interpolate_model(lt_link("lt1", tr), name = "lt1"))
  expect_setequal(scen@modInp@sets$trade, c("L_CLT1", "L_CLT2"))
  # `@routes` is structural, so every tranche keeps BOTH directed routes -- a
  # tranche that lost one would fail KVL's "declared in one direction only".
  v <- scen@modInp@sets$variant
  expect_setequal(v$base[v$name %in% c("L_CLT1", "L_CLT2")], "L")
  expect_setequal(v$cluster[v$name %in% c("L_CLT1", "L_CLT2")], c("T1", "T2"))
})

test_that("the low-loss tranche fills first", {
  skip_if_no_solver()
  # Demand 30, tranche capacities 50/50: tranche 1 alone can carry it.
  tr <- lossTranches(c(0.5, 0.5), loss = 0.04, fix = 100)
  f <- lt_flows(lt_solve(lt_link("lt2", tr), "lt2"))
  expect_equal(unname(f["L_CLT1"]), round(30 / 0.98, 6))
  expect_true(is.na(f["L_CLT2"]))   # the lossy tranche carries nothing
})

test_that("the loss curve is EXACT at every breakpoint", {
  skip_if_no_solver()
  # 4% at F = 100 means r = 0.0004, so loss(f) = 0.0004 f^2. The breakpoints are
  # f = 50 (alpha_1 * F) and f = 100 (F).
  tr <- lossTranches(c(0.5, 0.5), loss = 0.04, fix = 100)
  sent <- function(dem, nm) sum(lt_flows(lt_solve(lt_link(nm, tr, demand = dem), nm)))

  s50 <- sent(49, "lt3")     # 50 sent, 1 lost
  expect_equal(s50, 50)
  expect_equal(s50 - 49, 0.0004 * s50^2)

  s100 <- sent(96, "lt4")    # 100 sent, 4 lost
  expect_equal(s100, 100)
  expect_equal(s100 - 96, 0.0004 * s100^2)

  # BETWEEN breakpoints the secant lies ABOVE the quadratic, so the model
  # overestimates losses. That is the honest direction of the error and the
  # opposite of PyPSA's tangent envelope; it is not a defect to be "fixed".
  s <- sent(20, "lt5")
  expect_gt(s - 20, 0.0004 * s^2)
})

test_that("tranche loss rates are invariant under parallel expansion", {
  skip_if_no_solver()
  # A second circuit halves r and doubles F, so `loss_full = r*F` is unchanged
  # and the SAME tranche efficiencies apply. This is what lets losses work when
  # capacity is a decision, with no outer loop -- PyPSA's tangent formulation
  # cannot do it because its tangent points are absolute.
  tr1 <- lossTranches(c(0.5, 0.5), loss = 0.04, fix = 100)
  tr2 <- lossTranches(c(0.5, 0.5), loss = 0.04, fix = 200)   # r halved, F doubled
  expect_equal(tr1$trade$teff, tr2$trade$teff)

  # At the same RELATIVE loading (half of rating) the loss fraction matches.
  s1 <- sum(lt_flows(lt_solve(lt_link("lt6", tr1, demand = 49), "lt6")))
  s2 <- sum(lt_flows(lt_solve(lt_link("lt7", tr2, demand = 98), "lt7")))
  expect_equal((s1 - 49) / s1, (s2 - 98) / s2)
})

# --------------------------------------------------------------------------- #
# Capacity tying
# --------------------------------------------------------------------------- #

test_that("investable tranche capacities are tied to the declared shares", {
  skip_if_no_solver()
  # Without a tie the LP builds the WHOLE corridor as the lowest-loss tranche --
  # same capex, better efficiency -- and the piecewise curve collapses back to a
  # single low-loss straight line, leaving the model more optimistic about
  # losses than the flat-`teff` model it replaced. A "TOTAL" group bound does
  # not help: it caps the SUM and says nothing about the split.
  tr <- lossTranches(c(0.5, 0.5), loss = 0.04)
  scen <- suppressMessages(interpolate_model(
    lt_link("lt8", tr, cap = data.frame(cap.up = 100), invcost = 10),
    name = "lt8"))
  expect_true(any(grepl("^VS", names(scen@modInp@gams.equation))))

  cp <- as.data.frame(getData(suppressMessages(solve_scen(scen)), "vTradeCap",
                              merge = TRUE))
  caps <- setNames(round(cp$value, 6), as.character(cp$trade))
  expect_equal(unname(caps["L_CLT1"]), unname(caps["L_CLT2"]))
  expect_gt(unname(caps["L_CLT1"]), 0)
})

test_that("fixed capacities are checked against the shares, not tied", {
  # Proportional: nothing to add, and no tie stacked on two fixed bounds.
  tr <- lossTranches(c(0.5, 0.5), loss = 0.04)
  ok <- suppressMessages(interpolate_model(
    lt_link("lt9", tr, cap = data.frame(cluster = c("T1", "T2"),
                                        cap.fx = c(50, 50))), name = "lt9"))
  expect_false(any(grepl("^VS", names(ok@modInp@gams.equation))))

  # Mis-proportioned is the commonest silent error: edit the shares, forget the
  # capacities, and the ratings no longer match the efficiencies derived from
  # them. Named, with both sets of numbers.
  expect_error(suppressMessages(interpolate_model(
    lt_link("lt10", tr, cap = data.frame(cluster = c("T1", "T2"),
                                         cap.fx = c(70, 30))), name = "lt10")),
    "the declared shares")

  # Partly fixed has no defensible reading.
  expect_error(suppressMessages(interpolate_model(
    lt_link("lt11", tr, cap = data.frame(cluster = "T1", cap.fx = 50)),
    name = "lt11")), "some clusters have a fixed capacity")
})

test_that("shares that do not sum to 1 are refused at interpolation", {
  # `lossTranches()` cannot produce these, but a hand-written declaration can --
  # and the derived efficiencies would then be silently mis-calibrated.
  bad <- newDCLink("L", "ELC", "R1", "R2",
                   cluster = data.frame(cluster = c("A", "B"), share = c(0.5, 0.4),
                                        order = 1:2),
                   trade = data.frame(cluster = c("A", "B"), teff = c(0.99, 0.97)),
                   vintage = data.frame(olife = 50L),
                   capacity = data.frame(cap.up = 10))
  m <- newModel(name = "ltbad", region = c("R1", "R2"), horizon = tr_hor(),
                discount = 0, calendar = tr_cal(),
                repo = newRepository("r", newCommodity("ELC", timeframe = "ANNUAL"),
                                     bad))
  expect_error(suppressMessages(interpolate_model(m, name = "ltbad")),
               "sum to 0.9, not 1")
})

# =========================================================================== #
# An absolute flow limit on a TRANCHED line
# =========================================================================== #

test_that("`ava.*` on a tranched line needs TOTAL, and splitting is wrong", {
  skip_if_no_solver()
  # A tranched line has its CAPACITY split, so an absolute flow bound cannot be
  # handled per tranche:
  #
  #   broadcast (cluster = NA) -- each tranche gets the full value, so a 50
  #     limit on two tranches permits 100. Correct per the NA convention, and
  #     not what a line-level limit means.
  #   split 25/25 -- the right total, but it forces flow onto the DEAR tranche
  #     before the cheap one is full, doubling the modelled loss.
  #   TOTAL -- the aggregate. Merit order survives, so the loss stays exact.
  #
  # Line of 100 in two equal tranches, 4% loss at rated load => r = 0.0004.
  mk <- function(nm, avarows) {
    tt <- lossTranches(c(0.5, 0.5), loss = 0.04, fix = 100)
    L <- newACLine("L", "ELC", "R1", "R2", reactance = 0.1, cap2act = 1,
                   vintage = data.frame(olife = 50L),
                   trade = rbind(cbind(tt$trade, ava.up = NA_real_), avarows),
                   cluster = tt$cluster, capacity = tt$capacity)
    newModel(name = nm, region = c("R1", "R2"), horizon = tr_hor(),
             discount = 0, calendar = tr_cal(),
             repo = newRepository("r", newCommodity("ELC", timeframe = "ANNUAL"),
               newSupply("SUP", commodity = "ELC",
                         supply = data.frame(region = c("R1", "R2"),
                                             cost = c(1, 1000))),
               L, newDemand("DEM", commodity = "ELC",
                            demand = data.frame(region = "R2", demand = 200))))
  }
  flows <- function(nm, avarows) {
    s <- suppressMessages(solve_scen(suppressMessages(
      interpolate_model(mk(nm, avarows), name = nm))))
    d <- as.data.frame(getData(s, "vTradeIr", merge = TRUE))
    d <- d[d$value > 1e-9, ]
    setNames(round(d$value, 6), as.character(d$trade))
  }
  eff <- c(L_CLT1 = 0.98, L_CLT2 = 0.94)
  loss <- function(f) sum(f) - sum(f * eff[names(f)])

  # TOTAL: the line limit holds AND the cheap tranche is filled first, so the
  # modelled loss equals the quadratic exactly.
  ft <- flows("lta", data.frame(cluster = "TOTAL", teff = NA, ava.up = 50))
  expect_equal(sum(ft), 50)
  expect_equal(unname(ft["L_CLT1"]), 50)
  expect_true(is.na(ft["L_CLT2"]))
  expect_equal(loss(ft), 0.0004 * 50^2)          # == 1, the true r*f^2

  # Broadcast still means "each", per the NA convention -- 50 on each tranche.
  fb <- flows("ltb", data.frame(cluster = NA, teff = NA, ava.up = 50))
  expect_equal(sum(fb), 100)

  # Splitting reaches the right total but the WRONG loss: 2 against a true 1.
  fs <- flows("ltc", data.frame(cluster = c("T1", "T2"), teff = NA,
                                ava.up = c(25, 25)))
  expect_equal(sum(fs), 50)
  expect_equal(loss(fs), 2)
  expect_gt(loss(fs), 0.0004 * 50^2)
})

test_that("a TOTAL row with nothing group-boundable is refused", {
  # The token is filtered out of the variant levels, so a TOTAL row that names
  # no boundable column would create neither a variant nor a bound -- it would
  # do nothing at all. Loud instead.
  tt <- lossTranches(c(0.5, 0.5), loss = 0.04, fix = 100)
  L <- newACLine("L", "ELC", "R1", "R2", reactance = 0.1, cap2act = 1,
                 vintage = data.frame(olife = 50L),
                 trade = rbind(cbind(tt$trade, ava.up = NA_real_),
                               data.frame(cluster = "TOTAL", teff = 0.9,
                                          ava.up = NA_real_)),
                 cluster = tt$cluster, capacity = tt$capacity)
  m <- newModel(name = "ltbad", region = c("R1", "R2"), horizon = tr_hor(),
                discount = 0, calendar = tr_cal(),
                repo = newRepository("r",
                  newCommodity("ELC", timeframe = "ANNUAL"), L))
  expect_error(suppressMessages(interpolate_model(m, name = "ltbad")),
               "carries no group-boundable value")
})

test_that("`af.*` on a tranched line has the same problem, and the same fix", {
  skip_if_no_solver()
  # A RELATIVE bound distorts exactly as the absolute one does, and for the same
  # reason: the capacity is split, so `af.up = 0.5` per tranche means half of
  # HALF the line each, forcing a 25/25 split instead of 50 on the cheap tranche.
  # Same total, double the loss.
  mk <- function(nm, afrows) {
    tt <- lossTranches(c(0.5, 0.5), loss = 0.04, fix = 100)
    L <- newACLine("L", "ELC", "R1", "R2", reactance = 0.1, cap2act = 1,
                   vintage = data.frame(olife = 50L),
                   trade = rbind(cbind(tt$trade, af.up = NA_real_), afrows),
                   cluster = tt$cluster, capacity = tt$capacity)
    newModel(name = nm, region = c("R1", "R2"), horizon = tr_hor(),
             discount = 0, calendar = tr_cal(),
             repo = newRepository("r", newCommodity("ELC", timeframe = "ANNUAL"),
               newSupply("SUP", commodity = "ELC",
                         supply = data.frame(region = c("R1", "R2"),
                                             cost = c(1, 1000))),
               L, newDemand("DEM", commodity = "ELC",
                            demand = data.frame(region = "R2", demand = 200))))
  }
  flows <- function(nm, afrows) {
    s <- suppressMessages(solve_scen(suppressMessages(
      interpolate_model(mk(nm, afrows), name = nm))))
    d <- as.data.frame(getData(s, "vTradeIr", merge = TRUE))
    d <- d[d$value > 1e-9, ]
    setNames(round(d$value, 6), as.character(d$trade))
  }
  eff <- c(L_CLT1 = 0.98, L_CLT2 = 0.94)
  loss <- function(f) sum(f) - sum(f * eff[names(f)])

  ft <- flows("lafa", data.frame(cluster = "TOTAL", teff = NA, af.up = 0.5))
  expect_equal(sum(ft), 50)
  expect_equal(unname(ft["L_CLT1"]), 50)
  expect_equal(loss(ft), 0.0004 * 50^2)        # == 1

  fb <- flows("lafb", data.frame(cluster = NA, teff = NA, af.up = 0.5))
  expect_equal(sum(fb), 50)                    # same total ...
  expect_equal(loss(fb), 2)                    # ... double the loss
})
