# =========================================================================== #
# Milestone-granularity invariance (pPeriodLen contract).
#
# With time-constant unit parameters and discount = 0, the undiscounted total
# of every cost component over 2021-2030 must be the same whether the horizon
# is solved in 1-, 5- or 10-year milestones. The objective aggregates annual
# costs as vTotalCost x pPeriodLen, so each component equation must express
# an ANNUAL quantity; a component that carries (or misses) a pPeriodLen of
# its own fails these tests by an integer factor equal to the period length.
#
# Contract pinned here (from eqTechCap): vTechNewCap is an ANNUAL BUILD RATE
# (capacity/yr); standing capacity accumulates pPeriodLen[yp] * vTechNewCap.
# Every per-MW-standing charge (eac, fixom) must therefore apply to the
# accumulated quantity, and every one-off per-MW-built charge (invcost) to
# rate x pPeriodLen -- which the objective's own pPeriodLen supplies.
# =========================================================================== #

# ---------------------------------------------------------------------------
test_that("varom total is granularity-invariant", {
  skip_if_no_solver()
  # capacity pinned, demand 10/yr, varom 2 -> 10 yr x 10 x 2 = 200
  ob <- iv_sweep(function(g) iv_model(
    iv_tech(varom = data.frame(varom = 2),
            capacity = data.frame(cap.fx = IV_DEMAND)),
    g = g, name = "ivvo"), "ivvo")
  expect_equal(unname(ob), rep(200, 3), tolerance = 1e-9)
})

# ---------------------------------------------------------------------------
test_that("fuel-cost total is granularity-invariant", {
  skip_if_no_solver()
  # eff 1, fuel cost 1 -> 10 yr x 10 = 100
  ob <- iv_sweep(function(g) iv_model(
    iv_tech(capacity = data.frame(cap.fx = IV_DEMAND)),
    g = g, name = "ivfu", fuel_cost = 1), "ivfu")
  expect_equal(unname(ob), rep(100, 3), tolerance = 1e-9)
})

# ---------------------------------------------------------------------------
test_that("fixom total is granularity-invariant", {
  skip_if_no_solver()
  # fixom 3 on 10 units standing -> 10 yr x 10 x 3 = 300
  ob <- iv_sweep(function(g) iv_model(
    iv_tech(fixom = data.frame(fixom = 3),
            capacity = data.frame(cap.fx = IV_DEMAND)),
    g = g, name = "ivfx"), "ivfx")
  expect_equal(unname(ob), rep(300, 3), tolerance = 1e-9)
})

# ---------------------------------------------------------------------------
test_that("invcost total is granularity-invariant", {
  skip_if_no_solver()
  # invcost is annuitized at the model rate over olife (the eac-parity
  # design): charge = invcost x crf(0, 100) x 10 units x 10 yr = 7.
  ob <- iv_sweep(function(g) iv_model(
    iv_tech(invcost = data.frame(invcost = 7),
            capacity = data.frame(cap.fx = IV_DEMAND)),
    g = g, name = "ivin"), "ivin")
  expect_equal(unname(ob), rep(7, 3), tolerance = 1e-9)
})

# ---------------------------------------------------------------------------
test_that("eac total is granularity-invariant", {
  skip_if_no_solver()
  # eac 1 on 10 units standing over the whole horizon -> 10 yr x 10 x 1 = 100.
  # The annuity must apply to standing capacity (periodLen x build rate);
  # a rate-only charge fails here by the period length.
  ob <- iv_sweep(function(g) iv_model(
    iv_tech(invcost = data.frame(eac = 1),
            capacity = data.frame(cap.fx = IV_DEMAND)),
    g = g, name = "ivea"), "ivea")
  expect_equal(unname(ob), rep(100, 3), tolerance = 1e-9)
})

# ---------------------------------------------------------------------------
test_that("invcost and eac routes agree at discount 0 (multi-year parity)", {
  skip_if_no_solver()
  # olife 10 spans the horizon exactly; crf(0, 10) = 1/10, so invcost 30
  # <=> eac 3. Both routes must charge 30 x 10 units = 300 in total, at
  # every granularity.
  for (g in c(1, 5, 10)) {
    o_inv <- iv_obj(iv_solve(iv_model(
      iv_tech(olife = 10L, invcost = data.frame(invcost = 30),
              capacity = data.frame(cap.fx = IV_DEMAND)),
      g = g, name = "ivpi"), paste0("ivpi_g", g)))
    o_eac <- iv_obj(iv_solve(iv_model(
      iv_tech(olife = 10L, invcost = data.frame(eac = 3),
              capacity = data.frame(cap.fx = IV_DEMAND)),
      g = g, name = "ivpe"), paste0("ivpe_g", g)))
    expect_equal(o_inv, 300, tolerance = 1e-9,
                 info = sprintf("invcost route, g=%d", g))
    expect_equal(o_eac, o_inv, tolerance = 1e-9,
                 info = sprintf("eac == invcost route, g=%d", g))
  }
})

# ---------------------------------------------------------------------------
test_that("ncap.up bounds the ANNUAL build rate at every granularity", {
  skip_if_no_solver()
  # ncap.up 1/yr, olife 100: at most 10 units can stand by 2030 regardless
  # of milestone granularity. A varom-100 backstop absorbs early-year demand
  # so the bound, not feasibility, shapes the build.
  for (g in c(1, 5, 10)) {
    sol <- iv_solve(iv_model(
      iv_tech(invcost = data.frame(invcost = 1),
              capacity = data.frame(ncap.up = 1)),
      iv_tech(name = "BST", input_comm = "BIO",
              varom = data.frame(varom = 100),
              capacity = data.frame(cap.fx = IV_DEMAND)),
      g = g, name = "ivnc"), paste0("ivnc_g", g))
    d <- suppressMessages(getData(sol, "vTechCap", merge = TRUE))
    d <- d[d$tech == "E1", , drop = FALSE]
    cap_final <- sum(d$value[d$year == max(d$year)])
    expect_equal(cap_final, 10, tolerance = 1e-9,
                 info = sprintf("standing 2030 under ncap.up=1/yr, g=%d", g))
    # total capacity built = sum(rate x periodLen) must also be 10
    nc <- suppressMessages(getData(sol, "vTechNewCap", merge = TRUE))
    nc <- nc[nc$tech == "E1", , drop = FALSE]
    expect_equal(sum(nc$value) * g, 10, tolerance = 1e-9,
                 info = sprintf("built total = sum(rate) x len, g=%d", g))
  }
})

# ---------------------------------------------------------------------------
test_that("an annual CO2 cap binds identically at every granularity", {
  skip_if_no_solver()
  # dirty (GAS, fuel 1, emis 1) vs clean (BIO, varom 5). Uncapped optimum is
  # all-dirty; a 4/yr cap forces the split dirty 4 / clean 6:
  # annual cost 4x1 + 6x5 = 34 -> 340 total; emissions 4/yr -> 40 total.
  cap <- newConstraint(
    name = "CO2CAP", eq = "<=",
    for.each = data.frame(year = IV_YEARS, comm = "CO2"),
    term1 = list(variable = "vEmsFuelTot"),
    rhs = data.frame(year = IV_YEARS, rhs = 4),
    defVal = Inf)
  for (g in c(1, 5, 10)) {
    sol <- iv_solve(iv_model(
      iv_tech(capacity = data.frame(cap.fx = IV_DEMAND)),
      iv_tech(name = "ECLN", input_comm = "BIO",
              varom = data.frame(varom = 5),
              capacity = data.frame(cap.fx = IV_DEMAND)),
      cap, g = g, name = "ivcc", fuel_cost = 1), paste0("ivcc_g", g))
    expect_equal(iv_obj(sol), 340, tolerance = 1e-9,
                 info = sprintf("objective under 4/yr cap, g=%d", g))
    expect_equal(iv_total(sol, "vEmsFuelTot", plen = g), 40,
                 tolerance = 1e-9,
                 info = sprintf("total emissions = 4 x 10 yr, g=%d", g))
  }
})

# ---------------------------------------------------------------------------
test_that("constant-stock totals are granularity-invariant", {
  skip_if_no_solver()
  # exogenous stock 10, fixom 3, no investment -> 300; served demand equal.
  ob <- iv_sweep(function(g) iv_model(
    iv_tech(fixom = data.frame(fixom = 3),
            capacity = data.frame(stock = IV_DEMAND)),
    g = g, name = "ivst"), "ivst")
  expect_equal(unname(ob), rep(300, 3), tolerance = 1e-9)
})
