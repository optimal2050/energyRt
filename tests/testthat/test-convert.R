# Safety net for the convert() rewrite ########################################
#
# These tests pin the behaviour of `convert()` as it stands BEFORE the unit
# registry and parser are rewritten. They exist so the rewrite can be proven
# not to change any answer it did not intend to change.
#
# Two groups, deliberately separated:
#
#   "documented behaviour"  correct today and must never regress.
#   "KNOWN-WRONG"           wrong today. Each is pinned to the wrong value with
#                           the right one in a FIXME, so the data and parser
#                           fixes flip them in one obvious diff. A failure here
#                           after those land is the goal, not a regression.
#
# Values were measured, not derived. Do not "tidy" a number without re-measuring.
#
# Non-ASCII unit names are written as \u escapes so this file stays ASCII, which
# R CMD check requires:
#   € EUR sign   ₹ INR sign   円 yen   元 yuan
##############################################################################

# -- A. correct today, must never regress ------------------------------------

test_that("simple within-dimension conversions are stable", {
  expect_equal(convert("MWh", "kWh"), 1000)
  expect_equal(convert("kWh", "MJ"), 3.6)
  expect_equal(convert("PJ", "GWh"), 1e6 / 3600, tolerance = 1e-12)
  expect_equal(convert("GWh", "PJ"), 0.0036)
  expect_equal(convert("PJ", "TWh"), 1e3 / 3600, tolerance = 1e-12)
  expect_equal(convert("kt", "t"), 1000)
  expect_equal(convert("toe", "GJ"), 41.868)
  expect_equal(convert("tce", "GJ"), 29.308)
  expect_equal(convert("W", "J/s"), 1)
  expect_equal(convert("W*s", "J"), 1)
  expect_equal(convert("t/d", "t/y"), 365)
})

test_that("compound units and the currency-per-capacity idiom are stable", {
  # the shape used throughout vignettes/utopia-build.Rmd
  expect_equal(convert("EUR/kW", "MEUR/GW"), 1)
  expect_equal(convert("MEUR/GW", "EUR/kW"), 1)
  expect_equal(convert("EUR/kWh", "MEUR/PJ"), 1e6 / 3600, tolerance = 1e-12)
  expect_equal(convert("cents/kWh", "USD/MWh"), 10)
  expect_equal(convert("USD/tce", "MUSD/PJ"), 0.0341203766889586,
               tolerance = 1e-12)
  expect_equal(convert("Wh/kg", "GWh/kt"), 1e-3)
  expect_equal(convert("MMBtu", "MWh"), 0.293071111111111, tolerance = 1e-12)
})

test_that("multi-word and punctuated unit names resolve", {
  # the names the regex-based parser is fragile around; the ones that work
  # today must keep working
  expect_equal(convert("metric tonne", "kg"), 1000)
  expect_equal(convert("gal (US)", "L"), 3.785411784, tolerance = 1e-12)
  expect_equal(convert("MJ/t", "MJ/metric tonne"), 1)
  expect_equal(convert("cu ft", "m3"), 0.028316846592, tolerance = 1e-12)
})

test_that("every calling convention in the wild is honoured", {
  expect_equal(convert("EUR/kW", "MEUR/GW", 2000), 2000) # value 3rd, chr method
  expect_equal(convert("kWh/kg", "MJ/t", 1e-3), 3.6)   # ditto, from the man page
  expect_equal(convert(1000, "kWh", "MWh"), 1)           # value 1st, num method
  expect_equal(convert(1, "kWh/kg", "MJ/t"), 3600)
  expect_equal(convert(5, "cents/kWh", "USD/MWh"), 50)
  expect_equal(convert(4 * 250, "Wh/kg", "GWh/kt"), 1)   # R/draw.R:1374
  expect_equal(convert("kWh", "MJ", c(1, 2, 3)), c(3.6, 7.2, 10.8))
  # a bare numeric, no attributes -- vignettes assign this into data.frame cols
  expect_null(attributes(convert("MWh", "kWh")))
})

test_that("currency spellings all resolve to their documented rate", {
  expect_equal(convert("USD", "USD"), 1)
  expect_equal(convert("cents", "USD"), 0.01)
  expect_equal(convert("mills", "USD"), 0.001)
  expect_equal(convert("EUR", "USD"), 1.1)
  expect_equal(convert("Euro", "USD"), 1.1)
  expect_equal(convert("INR", "USD"), 0.0121)
  expect_equal(convert("JPY", "USD"), 1 / 140, tolerance = 1e-12)
  expect_equal(convert("Japanese yen", "USD"), 1 / 140, tolerance = 1e-12)
  expect_equal(convert("CNY", "USD"), 1 / 7.15, tolerance = 1e-12)
  expect_equal(convert("RMB", "USD"), 1 / 7.15, tolerance = 1e-12)
  expect_equal(convert("Chinese yuan", "USD"), 1 / 7.15, tolerance = 1e-12)
  expect_equal(convert("MUSD", "USD"), 1e6)
  expect_equal(convert("MEUR", "MUSD"), 1.1)
  expect_equal(convert("€", "USD"), 1.1)
  expect_equal(convert("₹", "USD"), 0.0121)
  expect_equal(convert("円", "USD"), 1 / 140, tolerance = 1e-12)
  expect_equal(convert("元", "USD"), 1 / 7.15, tolerance = 1e-12)
})

test_that("the crore-INR family is internally consistent", {
  # 1 crore = 1e7, so every spelling must equal 1e7 x the INR rate. Today that
  # holds only by hand-arithmetic in the shipped data -- nothing enforces it,
  # which is the bug the alias_of/scale migration is meant to make impossible.
  crore <- c("crore_INR", "cr.INR", "cr. INR", "crore INR", "INR (in cr.)",
             "cr. ₹", "cr.₹", "crore ₹")
  for (u in crore) {
    expect_equal(convert(u, "USD"), 121000, info = u)
    expect_equal(convert(u, "USD"), 1e7 * convert("INR", "USD"), info = u)
  }
  expect_equal(convert("crore INR", "MUSD"), 0.121)
})

test_that("vignette call sites produce their published values", {
  # vignettes/utopia-build.Rmd :435 :482 :531 :578 :607 :617 :665 :720 and
  # data-raw/utopia_modules.R :45 :178 -- the latter BAKES results into
  # data/utopia_modules.rda, so drift here silently changes shipped data.
  for (v in c(2000, 900, 8000, 650, 1300, 3000, 2200)) {
    expect_equal(convert("EUR/kW", "MEUR/GW", v), v, info = v)
  }
  expect_equal(convert("EUR/kWh", "MEUR/PJ", 200), 200 * 1e6 / 3600,
               tolerance = 1e-12)
  # vignettes/articles/model-bricks.Rmd:219
  expect_equal(convert("PJ", "GWh", 1), 1e6 / 3600, tolerance = 1e-12)
})

# -- B. wrong today; these are the deliverable of the data + parser fixes -----

test_that("KNOWN-WRONG: SI prefixes shadow explicit unit entries", {
  # .prefix_find_unit() scans a type's columns in order and matches prefix+base
  # before reaching an explicit entry further along. Fixed by matching the
  # longest exact spelling first.
  expect_equal(convert("day", "d"), 3650)  # FIXME: 1; `day` parses as deca-year
  expect_equal(convert("t/day", "t/d"), 1 / 3650, tolerance = 1e-12) # FIXME: 1
  expect_equal(convert("km2", "m2"), 1000) # FIXME: 1e6
  expect_equal(convert("km3", "m3"), 1000) # FIXME: 1e9
})

test_that("KNOWN-WRONG: shipped coefficients disagree with NIST", {
  expect_equal(convert("cm3", "m3"), 1e-3)       # FIXME: 1e-6
  expect_equal(convert("ft3", "m3"), 2.832e-4)   # FIXME: 0.028316846592
                                                 #        (`cu ft` is right)
  expect_equal(convert("in3", "m3"), 1.408e-5)   # FIXME: 1.6387064e-5
  expect_equal(convert("bu", "m3"), 4.731e-4)    # FIXME: 0.03523907
  expect_equal(convert("pck", "m3"), 1.118e-4)   # FIXME: 0.0088098
  expect_equal(convert("cal", "J"), 4.187e6)     # FIXME: 4.1868 (`Cal` is right)
  expect_equal(convert("Cal", "J"), 4187)
  expect_equal(convert("in", "m"), 0.025)        # FIXME: 0.0254
  expect_equal(convert("atm", "Pa"), 101317)     # FIXME: 101325
  expect_equal(convert("psi", "Pa"), 6896.5)     # FIXME: 6894.757
  expect_equal(convert("lb", "kg"), 0.4535147)   # FIXME: 0.45359237
  expect_equal(convert("ft-lb/s", "W"), 1.35501) # FIXME: 1.3558179
  expect_equal(convert("mm3", "m3"), 1e-6)       # FIXME: 1e-9 read as (mm)^3;
                                                 #        a design choice
})

test_that("the Btu family: Energy entries are correct and self-consistent", {
  # `Btu` is the International Table Btu, and it is exact.
  expect_equal(convert("Btu", "J"), 1055.05585262, tolerance = 1e-12)
  expect_equal(convert("MMBtu", "J"), 1e6 * convert("Btu", "J"),
               tolerance = 1e-6)
  expect_equal(convert("therm", "J"), 1e5 * convert("Btu", "J"),
               tolerance = 1e-6)
  expect_equal(convert("decatherm", "J"), 10 * convert("therm", "J"))
  expect_equal(convert("decatherm", "J"), convert("MMBtu", "J"))
})

test_that("KNOWN-WRONG: `therm` is the EC therm, and nothing says so", {
  # NIST lists two: therm (EC) = 1.05506e8 J, therm (U.S.) = 1.054804e8 J.
  # energyRt's `therm` is 1e5 x Btu_IT, i.e. the EC one -- but UDUNITS' bare
  # `therm` is the *US* one, so an oracle cross-check disagrees by 0.024%
  # unless this is declared. Not a coefficient bug; a missing declaration.
  expect_equal(convert("therm", "J"), 1.055056e8, tolerance = 1e-9)
  expect_false(isTRUE(all.equal(convert("therm", "J"), 1.054804e8)))
  # FIXME: ship `EC_therm` and `US_therm` explicitly, keep bare `therm` = EC
  # (no behaviour change), and whitelist the UDUNITS mismatch with the reason.
  expect_error(convert("US_therm", "J"), "Unknown unit")
  expect_error(convert("EC_therm", "J"), "Unknown unit")
})

test_that("KNOWN-WRONG: the Power Btu entries have drifted from Energy::Btu", {
  # W = J/s, so these are the SAME physical fact as Energy::Btu expressed per
  # unit time -- but each carries its own independently-rounded number.
  btu <- 1055.05585262
  expect_equal(convert("Btu/s", "W"), 1055.03678, tolerance = 1e-9)
  expect_false(isTRUE(all.equal(convert("Btu/s", "W"), btu)))       # FIXME
  expect_equal(convert("Btu/hr", "W"), 0.29306, tolerance = 1e-9)
  expect_false(isTRUE(all.equal(convert("Btu/hr", "W"), btu / 3600))) # FIXME
  expect_equal(convert("Btu/min", "W"), 17.58394, tolerance = 1e-9)
  expect_false(isTRUE(all.equal(convert("Btu/min", "W"), btu / 60)))  # FIXME

  # The sharpest demonstration: two spellings of one quantity, two answers.
  # `Btu/hr` hits the explicit Power atom; `Btu/h` composes Btu / h.
  expect_equal(convert("Btu/hr", "W") / convert("Btu/h", "W"),
               0.999962227004, tolerance = 1e-9)   # FIXME: exactly 1
})

test_that("KNOWN-WRONG: `hr` is not a time unit, so it only works by accident", {
  # Time has s, h, y, d, second, hour, year, minute, day -- but no `hr`.
  # `Btu/hr` resolves ONLY because it is an explicit Power atom; the moment
  # `hr` has to compose, it fails.
  expect_equal(convert("MMBtu/h", "MW"), 0.293071111111, tolerance = 1e-9)
  expect_error(convert("MMBtu/hr", "MW"), "Unknown unit hr")   # FIXME: works
  expect_error(convert("GJ/hr", "MW"), "Unknown unit hr")      # FIXME: works
})

test_that("KNOWN-WRONG: common Btu spellings from real data do not resolve", {
  for (u in c("BTU", "mmBtu", "MMbtu", "kBtu")) {
    expect_error(convert(u, "J"), "Unknown unit", info = u)  # FIXME: aliases
  }
  # `MBtu` must stay unresolvable rather than be guessed at: in oil and gas
  # M is the Roman thousand, so MBtu = 1e3 Btu, while SI M is mega = 1e6 Btu.
  # The two conventions differ by 1000x. `Btu` carrying SI_prefixes = 0 is
  # what currently prevents the silent mega- reading, and that must become a
  # DELIBERATE, documented choice rather than an accident of the data.
  expect_error(convert("MBtu", "J"), "Unknown unit")
})

test_that("KNOWN-WRONG: derived types do not reconcile with their compounds", {
  # Density is both its own type AND Mass/Volume, and its column is misspelled
  # `metric tonnem3`, so `t/m3` never resolves against it.
  expect_error(convert("kg/L", "t/m3"), "Different unit type")   # FIXME: 1
  expect_error(convert("g/cm3", "t/m3"), "Different unit type")  # FIXME: 1
  expect_error(convert("m2", "m^2"), "Different unit type")      # FIXME: 1
})

test_that("KNOWN-WRONG: the parser treats unit names as regex and evals them", {
  # .split_to_unit() gsub-substitutes coefficients into the input string and
  # eval(parse())s it, so any name with a regex metacharacter detonates.
  expect_error(convert("USD/gal (US)", "USD/L"), "could not find function")
                                                 # FIXME: 1 / 3.785411784
  expect_error(convert("INR (in cr.)/PJ", "MUSD/PJ"), "unexpected") # FIXME: 0.121
  # ...while the same spelling resolves standalone
  expect_equal(convert("INR (in cr.)", "USD"), 121000)
})

test_that("KNOWN-WRONG: prefix/unit collisions and missing prefixes", {
  expect_equal(convert("min", "in"), 1e-6)  # FIXME: a dimension error;
                                            #        `min` matches milli-inch
  expect_error(convert("min", "s"), "Different unit type")  # FIXME: 60
  expect_error(convert("hPa", "Pa"), "Unknown unit")        # FIXME: 100
  expect_error(convert("cL", "L"), "Unknown unit")          # FIXME: 0.01
  expect_error(convert("EUR/kW/yr", "MEUR/GW/y"), "Unknown unit") # FIXME: 1
})

test_that("KNOWN-WRONG: named-argument dispatch depends on where you call it", {
  # The generic's first formal is `x` but convert.character()'s is `from`, so a
  # named-only call cannot dispatch positionally. What happens instead depends
  # on the CALLING ENVIRONMENT, which is the real defect and the reason it is
  # easy to miss: the package's own code and vignettes run inside the namespace
  # and see it work, while a user at the console gets an error.
  ns <- function() convert(from = "MWh", to = "kWh")
  environment(ns) <- asNamespace("energyRt")
  expect_equal(ns(), 1000)                       # inside the package: works

  glb <- function() convert(from = "MWh", to = "kWh")
  environment(glb) <- globalenv()
  expect_error(glb(), "no applicable method")    # FIXME: 1000 from anywhere
})

# -- C. registry state -------------------------------------------------------

test_that("KNOWN-WRONG: a global convert_data hijacks the registry", {
  # .prefix_find_unit() reads a bare `convert_data`; the lookup chain from the
  # namespace reaches .GlobalEnv, so a user object of that name shadows the
  # package data and every conversion fails.
  #
  # `inherits = FALSE` matters here: the package's own lazy-loaded copy is
  # reachable from globalenv through the search path, so an inheriting exists()
  # always reports TRUE and would wrongly skip this test.
  skip_if(exists("convert_data", envir = globalenv(), inherits = FALSE),
          "a convert_data already exists in the global env")
  assign("convert_data", list(base = list()), envir = globalenv())
  on.exit(rm("convert_data", envir = globalenv()), add = TRUE)
  expect_error(convert("t", "kg"), "Unknown unit")  # FIXME: 1000, unhijackable
})
