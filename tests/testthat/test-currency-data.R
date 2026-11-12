# =========================================================================== #
# The three currency datasets, and the contract between them and
# `convert_data$base$Currency`.
#
# Money is the one unit whose value moves in time, so a currency needs a year.
# `currency_fx` carries exchange rates per currency and year, `currency_deflator`
# carries price indices for moving within one currency, and `currency_codes`
# resolves a spelling to an ISO code and a scale so a rate is stored once per
# currency rather than once per spelling.
#
# Before this, `convert_data$base$Currency` held undated rates typed in by hand
# -- which is how it came to carry a euro rate from one year beside a yen rate
# from another, with nothing able to detect it. These tests pin the properties
# that make that impossible to repeat.
# =========================================================================== #

test_that("the datasets exist with their documented columns", {
  expect_true(is.data.frame(currency_codes))
  expect_true(is.data.frame(currency_fx))
  expect_true(is.data.frame(currency_deflator))
  expect_named(currency_codes,
               c("spelling", "code", "scale", "si_prefixes"))
  expect_named(currency_fx, c("code", "year", "lcu_per_usd", "source"))
  expect_named(currency_deflator,
               c("code", "area", "year", "basis", "index", "source"))
})

test_that("currency_codes matches the Currency array exactly, in order", {
  # Order is load-bearing: unit lookup scans spellings and the first match
  # wins, so a reordering silently changes what a string resolves to.
  cu <- convert_data$base$Currency
  expect_identical(currency_codes$spelling, colnames(cu))
  expect_identical(as.numeric(currency_codes$si_prefixes),
                   unname(cu["SI_prefixes", ]))
  expect_identical(colnames(cu)[1], "USD")
  expect_equal(unname(cu["coef", "USD"]), 1)
})

test_that("the base year is declared and is a year the table covers", {
  y <- attr(currency_fx, "base_year")
  expect_true(is.numeric(y) && length(y) == 1L)
  expect_true(y %in% currency_fx$year)
  # every spelling's currency must have a rate there, or the generated
  # coefficient row would carry an NA
  at <- currency_fx[currency_fx$year == y, ]
  expect_true(all(currency_codes$code %in% at$code))
})

test_that("keys are unique", {
  expect_false(any(duplicated(currency_fx[, c("code", "year")])))
  expect_false(any(duplicated(
    currency_deflator[, c("code", "basis", "year")])))
})

test_that("the numeraire is the numeraire and every rate is usable", {
  usd <- currency_fx[currency_fx$code == "USD", ]
  expect_gt(nrow(usd), 0L)
  expect_true(all(usd$lcu_per_usd == 1))
  expect_true(all(is.finite(currency_fx$lcu_per_usd)))
  expect_true(all(currency_fx$lcu_per_usd > 0))
  expect_true(all(is.finite(currency_deflator$index)))
  expect_true(all(currency_deflator$index > 0))
})

test_that("the euro does not predate the euro", {
  # The rate comes from the euro area itself, which has one from 1999 and not
  # before. A `MEUR1995` is a question with no answer rather than one to
  # extrapolate, and the absence of the rows is what enforces that.
  eur <- currency_fx$year[currency_fx$code == "EUR"]
  expect_gt(length(eur), 0L)
  expect_gte(min(eur), 1999L)
})

test_that("a currency with no deflator of its own borrows a declared one", {
  # There is no euro-area deflator in these series and no WDI aggregate
  # carries one, so EUR deflates on a member state. The substitution is
  # recorded in `area` rather than implied, which is the whole reason that
  # column exists.
  a <- unique(currency_deflator$area[currency_deflator$code == "EUR"])
  expect_length(a, 1L)
  expect_false(a %in% c("EMU", "XC"))
  expect_true(nchar(a) == 3L)
  # the currencies used in the shipped models all resolve to their own country
  own <- c(USD = "USA", JPY = "JPN", CNY = "CHN", INR = "IND")
  for (k in names(own)) {
    expect_equal(unique(currency_deflator$area[currency_deflator$code == k]),
                 own[[k]], info = k)
  }
})

test_that("both deflator bases are carried for the currencies in use", {
  for (k in unique(currency_codes$code)) {
    for (b in c("gdp", "cpi")) {
      n <- sum(currency_deflator$code == k & currency_deflator$basis == b)
      expect_gt(n, 0L)
    }
  }
})

test_that("a deflator ratio is what moves money between years", {
  # The index base is arbitrary and only ratios are ever used, so a same-year
  # ratio must be exactly 1 and the series must be monotone-free of zeros.
  d <- currency_deflator[currency_deflator$code == "USD" &
                           currency_deflator$basis == "gdp", ]
  i2015 <- d$index[d$year == 2015]
  i2020 <- d$index[d$year == 2020]
  expect_length(i2015, 1L)
  expect_length(i2020, 1L)
  expect_equal(i2015 / i2015, 1)
  # US prices rose between 2015 and 2020; a reversed sign here means the
  # series was read upside down
  expect_gt(i2020, i2015)
})
