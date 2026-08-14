# The model has TWO rates and no fallback between them:
#   wacc -> annuitises investment into pXEac (R/eac.R)
#   sdr  -> discounts the objective (pDiscountFactor)
# `discount` survives as an ARGUMENT only, the shorthand that sets both. No class
# carries a `discount` column and no parameter carries a `discount` value, so
# there is nothing left to establish a precedence between.

test_that("`discount` is an argument, not a column", {
  expect_setequal(names(new("config")@discount),
                  c("region", "year", "wacc", "sdr"))
  expect_false("pDiscount" %in% names(getFromNamespace(".modInp", "energyRt")))
  expect_true(all(c("pWacc", "pSdr") %in%
                    names(getFromNamespace(".modInp", "energyRt"))))
})

test_that("the `discount` shorthand sets both rates", {
  d <- update(new("config"), discount = 0.05)@discount
  expect_equal(d$wacc, 0.05)
  expect_equal(d$sdr, 0.05)

  # A table may use it too, per region.
  d <- update(new("config"),
              discount = data.frame(region = c("R1", "R2"),
                                    discount = c(0.05, 0.08)))@discount
  expect_equal(d$wacc, c(0.05, 0.08))
  expect_equal(d$sdr, c(0.05, 0.08))
})

test_that("wacc and sdr can be set independently, by region and year", {
  d <- update(new("config"),
              discount = data.frame(region = c("R1", "R2"),
                                    year = c(2020L, 2030L),
                                    wacc = c(0.08, 0.06),
                                    sdr = c(0.03, 0.03)))@discount
  expect_equal(d$wacc, c(0.08, 0.06))
  expect_equal(d$sdr, c(0.03, 0.03))
  expect_equal(d$region, c("R1", "R2"))
  expect_equal(d$year, c(2020L, 2030L))
})

test_that("a partial pair or a mixed form is refused", {
  expect_error(update(new("config"), discount = data.frame(wacc = 0.08)),
               "must both be supplied")
  expect_error(update(new("config"), discount = data.frame(sdr = 0.03)),
               "missing `wacc`")
  expect_error(
    update(new("config"), discount = data.frame(discount = 0.05, sdr = 0.03)),
    "cannot be combined")
  expect_error(update(new("config"), discount = "0.05"), "must be a number")
})

test_that("re-updating a config does not re-trigger the mixed-form error", {
  # The expansion clears nothing to re-expand: `discount` never reaches a slot,
  # so normalisation is idempotent by construction.
  cfg <- update(new("config"), discount = 0.05)
  expect_equal(update(cfg, name = "again")@discount$wacc, 0.05)
})

test_that("the shorthand and the explicit pair give the same model", {
  skip_if_no_solver()
  a <- rt_solve(rt_interp(rt_model(name = "r1", discount = 0.05), "r1"))
  b <- rt_solve(rt_interp(
    rt_model(name = "r2", discount = data.frame(wacc = 0.05, sdr = 0.05)), "r2"))
  expect_equal(rt_val(a, "vObjective"), rt_val(b, "vObjective"))
})

test_that("wacc annuitises and sdr discounts, independently", {
  skip_if_no_solver()
  sc <- rt_interp(rt_model(name = "r3",
                           discount = data.frame(wacc = 0.08, sdr = 0.03)), "r3")
  expect_equal(rt_val(sc, "pWacc"), 0.08)
  expect_equal(rt_val(sc, "pSdr"), 0.03)
  # The annuity uses wacc only ...
  expect_equal(rt_val(sc, "pTechEac"), round(1000 * rt_crf(0.08, 40), 6))
  # ... and the discount factor uses sdr only: 1/(1+sdr) compounded per period.
  df <- suppressMessages(getData(sc, "pDiscountFactor", merge = TRUE))
  expect_true(all(df$value <= 1))
  expect_lt(max(df$value[df$year > min(df$year)]), 1 / (1 + 0.03)^4)
})

test_that("sdr = 0 leaves the objective undiscounted", {
  skip_if_no_solver()
  sc <- rt_interp(rt_model(name = "r4",
                           discount = data.frame(wacc = 0.05, sdr = 0)), "r4")
  # An all-default parameter is pruned to zero rows; 1.0 IS pDiscountFactor's
  # default, so "no rows" and "every row 1" both mean undiscounted.
  expect_true(all(rt_val(sc, "pDiscountFactor") == 1))
  # The annuity is unaffected by sdr.
  expect_equal(rt_val(sc, "pTechEac"), round(1000 * rt_crf(0.05, 40), 6))
})

test_that("a per-process wacc overrides the model-wide one", {
  skip_if_no_solver()
  sc <- rt_interp(rt_model(
    invcost = data.frame(invcost = 1000, wacc = 0.08),
    name = "r5", discount = 0.05), "r5")
  expect_equal(rt_val(sc, "pTechWacc"), 0.08)
  expect_equal(rt_val(sc, "pTechEac"), round(1000 * rt_crf(0.08, 40), 6))
  # It is a WACC override only -- the objective still discounts at the sdr.
  expect_equal(rt_val(sc, "pSdr"), 0.05)
})

test_that("an explicit rate of zero is kept, not read as absent", {
  skip_if_no_solver()
  # 0 is both the parameter default and a legitimate zero-interest rate, so it
  # must survive `drop_default = TRUE` (hence `dropIfEmpty: false`). A dropped
  # zero would be indistinguishable from "unset" -- except that here there is
  # nothing to fall back TO, so it would silently annuitise at 0 anyway; the
  # observable guarantee is that the row is still reported.
  sc <- suppressMessages(suppressWarnings(interpolate_model(
    rt_model(name = "r6", discount = data.frame(wacc = 0, sdr = 0.04)),
    name = "r6", fold = TRUE, drop_default = TRUE)))
  expect_equal(rt_val(sc, "pWacc"), 0)
  # wacc = 0 -> straight-line annuity, invcost / olife.
  expect_equal(rt_val(sc, "pTechEac"), 1000 / 40)
})

test_that("no rate at all still builds (both default to zero)", {
  skip_if_no_solver()
  sc <- rt_interp(rt_model(name = "r7"), "r7")
  expect_equal(rt_val(sc, "pTechEac"), 1000 / 40)
  # An all-default parameter is pruned to zero rows; 1.0 IS pDiscountFactor's
  # default, so "no rows" and "every row 1" both mean undiscounted.
  expect_true(all(rt_val(sc, "pDiscountFactor") == 1))
})
