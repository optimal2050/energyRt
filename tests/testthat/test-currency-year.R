# =========================================================================== #
# Year-tagged money: `MEUR2015`, `MUSD2020`, and the two routes between them.
#
# `convert()` is a ratio of two per-unit scalars. A year cannot live in such a
# scalar, because moving money between currency-years needs a PIVOT -- the year
# whose exchange rate is used -- and the pivot belongs to the pair, not to
# either side. So the per-unit coefficients stay what they are (the rate at the
# base year) and a pairwise factor corrects them.
#
# The property that matters is that it collapses correctly. One currency and
# two years must be a pure deflator ratio; two currencies and one year must be
# a pure exchange rate; nothing tagged must be exactly what it was before any
# of this existed. The tests below pin all three, then the route algebra.
# =========================================================================== #

fxr <- function(code, year) {
  currency_fx$lcu_per_usd[currency_fx$code == code & currency_fx$year == year]
}
dfl <- function(code, year, basis = "gdp") {
  currency_deflator$index[currency_deflator$code == code &
                            currency_deflator$year == year &
                            currency_deflator$basis == basis]
}


# --------------------------------------------------------------------------- #
# 1. the collapses
# --------------------------------------------------------------------------- #

test_that("untagged money is exactly what it was", {
  # The base-year rate, and nothing else: no deflator is consulted and no
  # warning is raised. Every vignette and every shipped model relies on this.
  y <- currency_base_year()
  expect_silent(v <- convert("MEUR", "MUSD"))
  expect_equal(v, fxr("USD", y) / fxr("EUR", y))
  expect_silent(convert("MWh", "kWh"))
  expect_equal(convert("MWh", "kWh"), 1000)
  expect_equal(convert("EUR/kW", "MEUR/GW"), 1)
  expect_equal(convert("cents/kWh", "USD/MWh"), 10)
})

test_that("one currency and two years is the deflator ratio alone", {
  # No exchange rate can enter: the currency is the same on both sides, so
  # both routes must give the identical number.
  expect_equal(convert(1, "MUSD2015", "MUSD2020"),
               dfl("USD", 2020) / dfl("USD", 2015))
  r <- convert(1, "MUSD2015", "MUSD2020", currency_route = "both")
  expect_length(r, 2L)
  expect_equal(r[[1]], r[[2]])
  # and in the other direction
  expect_equal(convert(1, "MUSD2020", "MUSD2015"),
               dfl("USD", 2015) / dfl("USD", 2020))
})

test_that("two currencies and one year is the exchange rate alone", {
  # No deflator can enter: the year is the same on both sides, so again both
  # routes agree. The anchor is a published figure -- the 2020 euro averaged
  # 1.1422 USD -- so a reciprocal inversion cannot pass.
  expect_equal(convert(1, "MEUR2020", "MUSD2020"),
               fxr("USD", 2020) / fxr("EUR", 2020))
  expect_equal(convert(1, "MEUR2020", "MUSD2020"), 1.1422, tolerance = 1e-3)
  r <- convert(1, "MEUR2020", "MUSD2020", currency_route = "both")
  expect_equal(r[[1]], r[[2]])
})

test_that("an untagged side is read at the base year", {
  y <- currency_base_year()
  expect_equal(convert(1, "MUSD2015", "MUSD"),
               convert(1, "MUSD2015", paste0("MUSD", y)))
  expect_equal(convert(1, "MEUR", "MUSD2020"),
               convert(1, paste0("MEUR", y), "MUSD2020"))
})


# --------------------------------------------------------------------------- #
# 2. the route algebra
# --------------------------------------------------------------------------- #

test_that("each route is the formula it claims to be", {
  # deflate_then_fx pivots on the TARGET year: deflate in the source currency
  # to that year, then convert. fx_then_deflate pivots on the SOURCE year.
  d1 <- suppressWarnings(
    convert(1, "MEUR2015", "MUSD2020", currency_route = "deflate_then_fx"))
  d2 <- suppressWarnings(
    convert(1, "MEUR2015", "MUSD2020", currency_route = "fx_then_deflate"))
  expect_equal(d1, (dfl("EUR", 2020) / dfl("EUR", 2015)) *
                     (fxr("USD", 2020) / fxr("EUR", 2020)))
  expect_equal(d2, (fxr("USD", 2015) / fxr("EUR", 2015)) *
                     (dfl("USD", 2020) / dfl("USD", 2015)))
  expect_false(isTRUE(all.equal(d1, d2)))
})

test_that("the default is deflate_then_fx", {
  expect_equal(suppressWarnings(convert(1, "MEUR2015", "MUSD2020")),
               suppressWarnings(convert(1, "MEUR2015", "MUSD2020",
                                        currency_route = "deflate_then_fx")))
})

test_that("both routes agree exactly when purchasing power parity holds", {
  # The gap between routes IS the change in the real exchange rate, so it is
  # zero when the nominal rate moved by exactly the inflation differential.
  # Constructed against the real US deflator so only one side is synthetic.
  on.exit(energyRt:::.currency_reset(), add = TRUE)
  p0 <- dfl("USD", 2015)
  p1 <- dfl("USD", 2020)
  add_currency_fx("EUR", 1801, 10)
  add_currency_fx("EUR", 1802, 12)
  add_currency_fx("USD", 1801, 1)
  add_currency_fx("USD", 1802, 1)
  add_currency_deflator("USD", 1801, p0)
  add_currency_deflator("USD", 1802, p1)
  # P_EUR(y) proportional to lcu(EUR, y) * P_USD(y) makes the real rate flat
  add_currency_deflator("EUR", 1801, 10 * p0)
  add_currency_deflator("EUR", 1802, 12 * p1)
  r <- convert(1, "MEUR1801", "MUSD1802", currency_route = "both")
  expect_equal(r[[1]], r[[2]])
})

test_that("a material disagreement between routes warns", {
  expect_warning(convert(1, "MJPY2012", "MUSD2023"), "differ by")
  # and a trivial one does not
  expect_silent(convert(1, "MUSD2015", "MUSD2020"))
  # the threshold is settable
  withr::with_options(list(energyRt.currency_route_tol = 10),
                      expect_silent(convert(1, "MJPY2012", "MUSD2023")))
})

test_that("both returns one value per route, named, and refuses a vector", {
  r <- suppressWarnings(
    convert(1, "MJPY2012", "MUSD2023", currency_route = "both"))
  expect_named(r, c("deflate_then_fx", "fx_then_deflate"))
  expect_error(convert(c(1, 2), "MJPY2012", "MUSD2023",
                       currency_route = "both"), "one number")
  expect_error(convert(1, "MEUR2015", "MUSD2020", currency_route = "sideways"),
               "must be one of")
})

test_that("the deflator basis is selectable and changes the answer", {
  a <- suppressWarnings(convert(1, "MJPY2012", "MUSD2023", basis = "gdp"))
  b <- suppressWarnings(convert(1, "MJPY2012", "MUSD2023", basis = "cpi"))
  expect_true(is.finite(a) && is.finite(b))
  expect_false(isTRUE(all.equal(a, b)))
  expect_error(convert(1, "MUSD2015", "MUSD2020", basis = "vibes"),
               "no vibes deflator")
})


# --------------------------------------------------------------------------- #
# 3. the grammar
# --------------------------------------------------------------------------- #

test_that("a year survives a compound unit", {
  # `.split_to_unit()` reduces a compound to its base-unit signature, which
  # keeps no trace of either the currency or the year, so both are carried
  # alongside. Without that, a cost per capacity would convert as a bare rate.
  expect_equal(suppressWarnings(convert(1, "MEUR2015/GW", "MUSD2020/GW")),
               suppressWarnings(convert(1, "MEUR2015", "MUSD2020")))
  expect_equal(suppressWarnings(convert(1, "MEUR2015/GW", "MUSD2020/MW")),
               suppressWarnings(convert(1, "MEUR2015", "MUSD2020")) / 1000)
})

test_that("the year can be an argument instead of a suffix", {
  expect_equal(
    suppressWarnings(convert(1, "MEUR", "MUSD", from_year = 2015,
                             to_year = 2020)),
    suppressWarnings(convert(1, "MEUR2015", "MUSD2020")))
  # an argument wins over a suffix, so one can be overridden
  expect_equal(
    suppressWarnings(convert(1, "MEUR2015", "MUSD2020", to_year = 2015)),
    convert(1, "MEUR2015", "MUSD2015"))
})

test_that("only a currency may carry a four-digit year", {
  # No spelling in the database ends in four digits, so the suffix collides
  # with nothing -- but the stem must still resolve to Currency, or a stray
  # number would silently become a year.
  expect_error(convert("MWh2020", "kWh"), "Unknown unit MWh2020")
  expect_error(convert("t2020", "kg"), "Unknown unit")
  # a bare number is not an unknown unit: `.split_to_unit()` drops numeric
  # tokens, so it reaches the dimension check as a literal
  expect_error(convert("2020", "USD"), "Different unit type")
})

test_that("units whose names end in digits are untouched", {
  # These values are what the resolver returned before year parsing existed
  # and must still be returned. Two of them are WRONG, and pre-existing: the
  # database holds `km2 = 1e6` and `km3 = 1e9` as explicit columns, but the
  # column scan reaches `m2`/`m3` first, matches them as `k` + the base unit
  # (both carry SI_prefixes = 1) and returns 1e3. The correct value is in the
  # data and unreachable -- the same first-match-wins defect as `day` reading
  # as deca-year. `cm3 = 0.001` is likewise wrong for a cubic centimetre and
  # likewise untouched here.
  expect_equal(convert("cm3", "m3"), 1e-3)
  expect_equal(convert("ha", "m2"), 1e4)
  expect_equal(convert("km3", "m3"), 1e3)   # FIXME: 1e9, shadowed by m3
  expect_equal(convert("km2", "m2"), 1e3)   # FIXME: 1e6, shadowed by m2
})

test_that("a scaled spelling still refuses an SI prefix", {
  expect_error(convert("Mcents", "USD"), "Unknown unit")
  expect_error(convert("Mcents2020", "USD2020"), "Unknown unit")
})

test_that("a year the table cannot answer is refused, not extrapolated", {
  # The euro did not exist in 1995, so `MEUR1995` is a question with no answer.
  expect_error(convert(1, "MEUR1995", "MUSD2020"),
               "no exchange rate for EUR in 1995")
  # one currency needs no rate at all, so the deflator is what runs out
  expect_error(convert(1, "MUSD1700", "MUSD2020"), "no gdp deflator")
})


# --------------------------------------------------------------------------- #
# 4. the registry
# --------------------------------------------------------------------------- #

test_that("a user rate extends the table and then clears", {
  on.exit(energyRt:::.currency_reset(), add = TRUE)
  expect_error(convert(1, "MEUR2099", "MUSD2099"), "no exchange rate")
  add_currency_fx("EUR", 2099, 0.8)
  add_currency_fx("USD", 2099, 1)
  expect_equal(convert(1, "MEUR2099", "MUSD2099"), 1 / 0.8)
  energyRt:::.currency_reset()
  expect_error(convert(1, "MEUR2099", "MUSD2099"), "no exchange rate")
})

test_that("a user rate replaces a shipped one rather than duplicating it", {
  on.exit(energyRt:::.currency_reset(), add = TRUE)
  add_currency_fx("EUR", 2020, 0.5)
  expect_equal(convert(1, "MEUR2020", "MUSD2020"), 2)
  d <- energyRt:::.currency_data("fx")
  expect_equal(sum(d$code == "EUR" & d$year == 2020), 1L)
})

test_that("a nonsensical rate is refused", {
  expect_error(add_currency_fx("EUR", 2099, 0), "must be positive")
  expect_error(add_currency_fx("EUR", 2099, -1), "must be positive")
  expect_error(add_currency_deflator("EUR", 2099, 0), "must be positive")
})

test_that("the base year is readable and settable", {
  y <- currency_base_year()
  expect_true(is.integer(y) && y > 1960L)
  on.exit(currency_base_year(y), add = TRUE)
  currency_base_year(2020)
  expect_equal(currency_base_year(), 2020L)
  # untagged now resolves at 2020
  expect_equal(convert("MEUR", "MUSD"), fxr("USD", 2020) / fxr("EUR", 2020))
  expect_error(currency_base_year("not a year"), "four-digit year")
})
