# =========================================================================== #
# Normalising a model's money.
#
# A model has one currency whether it says so or not: the objective adds every
# cost together with coefficient 1. Where objects were priced in another one,
# `normalize_model_currency()` converts them and reports every factor.
#
# The two properties that matter are opposite in kind. One is that NOTHING
# moves unless something was declared -- a model that tags no currency must
# interpolate exactly as it did before any of this existed. The other is that
# where everything moves by one factor, the objective moves by exactly that
# factor and by nothing else: a scaling of all money is a scaling of the sum.
# =========================================================================== #

.cn_sup <- function(cur = NULL, cost = 2.5) {
  u <- data.frame(activity = "PJ")
  if (!is.null(cur)) u$currency <- cur
  newSupply("SUP_COA", commodity = "COA", unit = "PJ", units = u,
            supply = data.frame(region = "R1", cost = cost, ava.up = 10))
}

.cn_tec <- function(cur = NULL) {
  u <- data.frame(capacity = "GW", activity = "GWh")
  if (!is.null(cur)) u$currency <- cur
  newTechnology("PP", input = data.frame(comm = "COA", unit = "PJ"),
                output = data.frame(comm = "ELC", unit = "GWh"),
                units = u, invcost = data.frame(invcost = 100),
                varom = data.frame(varom = 3))
}

.cn_model <- function(cur = NULL, sup_cur = NULL, tec_cur = NULL) {
  repo <- add(newRepository("cn"), newCommodity("COA", unit = "PJ"),
              newCommodity("ELC", unit = "GWh"),
              .cn_sup(sup_cur), .cn_tec(tec_cur))
  args <- list(name = "CN", region = "R1", data = repo)
  if (!is.null(cur)) args$currency <- cur
  suppressWarnings(do.call(newModel, args))
}

.cn_obj <- function(mod, nm) getObject(mod, name = nm)[[1]]


# --------------------------------------------------------------------------- #
# 1. nothing declared, nothing moves
# --------------------------------------------------------------------------- #

test_that("a model that declares no currency is not normalised", {
  mod <- .cn_model()
  expect_null(energyRt:::.normalize_currency(mod, verbose = FALSE))
  expect_identical(normalize_model_currency(mod, verbose = FALSE), mod)
})

test_that("objects already in the model's currency are not rewritten", {
  mod <- .cn_model("MUSD2020", "MUSD2020", "MUSD2020")
  expect_null(energyRt:::.normalize_currency(mod, verbose = FALSE))
})

test_that("an object that declares nothing is left alone at its own target", {
  # It is in the MODEL's currency -- that is what a model-level declaration
  # means -- so converting to the model's own currency is a factor of 1.
  mod <- .cn_model("MUSD2020")
  expect_null(energyRt:::.normalize_currency(mod, verbose = FALSE))
})


# --------------------------------------------------------------------------- #
# 2. what counts as money
# --------------------------------------------------------------------------- #

test_that("the money columns are derived from the parameter catalogue", {
  # Not a list in this file: a cost column added to a class is normalised the
  # day its unit is declared in data-raw/parameters.yml.
  cp <- energyRt:::.col_param
  for (cls in c("technology", "supply", "trade", "storage", "tax")) {
    want <- cp[cp$class == cls & !is.na(cp$unit) &
                 grepl("{currency}", cp$unit, fixed = TRUE), ]
    keys <- ifelse(is.na(want$column), want$slot,
                   paste(want$slot, want$column, sep = "."))
    expect_setequal(energyRt:::.currency_money_keys(cls), keys)
    expect_gt(length(keys), 0L)
  }
  # a commodity has no cost of its own, so it can never be converted
  expect_length(energyRt:::.currency_money_keys("commodity"), 0L)
})

test_that("only the money columns move", {
  mod <- .cn_model("MUSD2020", "MEUR2015", "MUSD2020")
  m2 <- suppressWarnings(normalize_model_currency(mod, verbose = FALSE))
  f <- suppressWarnings(convert(1, "MEUR2015", "MUSD2020"))
  s <- .cn_obj(m2, "SUP_COA")
  expect_equal(s@supply$cost, 2.5 * f)
  expect_equal(s@supply$ava.up, 10)        # a quantity, not a price
  expect_equal(s@units$activity, "PJ")     # physical units are not touched
  expect_equal(s@units$currency, "MUSD2020")
  # the technology was already in the target
  expect_equal(.cn_obj(m2, "PP")@invcost$invcost, 100)
  expect_equal(.cn_obj(m2, "PP")@varom$varom, 3)
})

test_that("the model as given is not modified", {
  mod <- .cn_model("MUSD2020", "MEUR2015", "MUSD2020")
  suppressWarnings(normalize_model_currency(mod, verbose = FALSE))
  expect_equal(.cn_obj(mod, "SUP_COA")@supply$cost, 2.5)
  expect_equal(.cn_obj(mod, "SUP_COA")@units$currency, "MEUR2015")
})


# --------------------------------------------------------------------------- #
# 3. the report and its provenance
# --------------------------------------------------------------------------- #

test_that("the report is one row per object and one factor per declaration", {
  mod <- .cn_model("MUSD2020", "MEUR2015", "MUSD2015")
  m2 <- suppressWarnings(normalize_model_currency(mod, verbose = FALSE))
  d <- m2@misc$currency$objects
  expect_named(d, c("object", "class", "from", "declared", "factor", "note",
                    "converted"))
  expect_setequal(d$object, c("SUP_COA", "PP"))
  expect_true(all(d$converted))
  expect_equal(d$factor[d$object == "SUP_COA"],
               suppressWarnings(convert(1, "MEUR2015", "MUSD2020")))
  expect_equal(d$factor[d$object == "PP"], convert(1, "MUSD2015", "MUSD2020"))
  # a conversion within one currency cannot depend on the route, and says so
  expect_equal(d$note[d$object == "PP"], "USD gdp deflator")
  expect_equal(d$note[d$object == "SUP_COA"], "deflate_then_fx")
  expect_equal(m2@misc$currency$target, "MUSD2020")
  expect_equal(m2@misc$currency$basis, "gdp")
})

test_that("the report prints the factors it applied", {
  mod <- .cn_model("MUSD2020", "MEUR2015", "MUSD2020")
  expect_message(suppressWarnings(normalize_model_currency(mod)),
                 "Currency normalisation -> MUSD2020")
  expect_message(suppressWarnings(normalize_model_currency(mod)), "MEUR2015")
  expect_silent(suppressWarnings(
    normalize_model_currency(mod, verbose = FALSE)))
})

test_that("provenance carries the rows the applied route read, and no others", {
  mod <- .cn_model("MUSD2020", "MEUR2015", "MUSD2020")
  m2 <- suppressWarnings(normalize_model_currency(mod, verbose = FALSE))
  p <- m2@misc$currency
  gen <- energyRt:::.currency_generated_at()
  # deflate_then_fx pivots on the TARGET year: the euro deflator moves
  # 2015 -> 2020 and the exchange rate is read at 2020, plus the generation
  # year the unit coefficients already carry.
  expect_setequal(paste(p$fx$code, p$fx$year),
                  c(paste("EUR", 2020), paste("USD", 2020),
                    paste("EUR", gen), paste("USD", gen)))
  expect_setequal(paste(p$deflator$code, p$deflator$year),
                  c(paste("EUR", 2015), paste("EUR", 2020)))
  # the US deflator belongs to the route NOT taken and must not appear
  expect_false("USD" %in% p$deflator$code)
  expect_true(all(p$fx$lcu_per_usd > 0))
})

test_that("a different route changes the factor and the rows", {
  mod <- .cn_model("MUSD2020", "MEUR2015", "MUSD2020")
  a <- suppressWarnings(normalize_model_currency(
    mod, verbose = FALSE))@misc$currency
  b <- suppressWarnings(normalize_model_currency(
    mod, currency_route = "fx_then_deflate", verbose = FALSE))@misc$currency
  expect_false(isTRUE(all.equal(a$objects$factor, b$objects$factor)))
  # fx_then_deflate pivots on the SOURCE year, so now it is the US deflator
  # that runs and the euro's that does not
  expect_true("USD" %in% b$deflator$code)
  expect_false("EUR" %in% b$deflator$code)
})

test_that("the basis is recorded and changes the answer", {
  mod <- .cn_model("MUSD2020", "MUSD2015", "MUSD2020")
  a <- normalize_model_currency(mod, verbose = FALSE)@misc$currency
  b <- normalize_model_currency(mod, basis = "cpi",
                                verbose = FALSE)@misc$currency
  expect_equal(b$basis, "cpi")
  expect_false(isTRUE(all.equal(a$objects$factor[1], b$objects$factor[1])))
  expect_equal(b$objects$note[1], "USD cpi deflator")
})


# --------------------------------------------------------------------------- #
# 4. an explicit target
# --------------------------------------------------------------------------- #

test_that("an explicit target moves the untagged side too, and the model", {
  # An object declaring nothing is in the model's currency, so asking for a
  # different target converts it: it is not "already in" a currency nobody
  # claimed for it.
  mod <- .cn_model("MUSD2020")
  m2 <- normalize_model_currency(mod, "MUSD2015", verbose = FALSE)
  f <- convert(1, "MUSD2020", "MUSD2015")
  expect_equal(.cn_obj(m2, "SUP_COA")@supply$cost, 2.5 * f)
  expect_equal(.cn_obj(m2, "PP")@invcost$invcost, 100 * f)
  # once the costs are in the target, the model counts in it
  expect_equal(m2@config@units$currency, "MUSD2015")
  expect_equal(.cn_obj(m2, "SUP_COA")@units$currency, "MUSD2015")
  expect_false(m2@misc$currency$objects$declared[1])
})

test_that("a target year the tables cannot answer is refused", {
  # The euro did not exist in 1995. A model priced in it never gets past
  # `newModel()` (below), so the case that reaches here is a target asked for
  # after the fact -- refused rather than extrapolated.
  mod <- .cn_model("MUSD2020")
  expect_error(normalize_model_currency(mod, "MEUR1995", verbose = FALSE),
               "no exchange rate for EUR in 1995")
})

test_that("a declaration that is not a currency is refused by name", {
  mod <- .cn_model("MUSD2020")
  expect_error(normalize_model_currency(mod, "bnEUR", verbose = FALSE),
               "not a currency unit")
})


# --------------------------------------------------------------------------- #
# 5. the model-level check
# --------------------------------------------------------------------------- #

test_that("newModel() accepts a mismatch convert() can answer", {
  # It used to refuse any mismatch. There is sourced annual data behind
  # `convert()` now, so this is a conversion with a factor and a provenance,
  # carried out on the interpolation copy and reported.
  expect_s4_class(.cn_model("MUSD2020", "MEUR2015", "MUSD2020"), "model")
  expect_s4_class(.cn_model("MUSD", "cr.INR"), "model")
})

test_that("newModel() still refuses a mismatch with no conversion at all", {
  expect_error(.cn_model("MUSD2020", "bnEUR"), "cannot be reached from")
  expect_error(.cn_model("MUSD2020", "bnEUR"), "bnEUR")
  # the model's own declaration can be the unreachable one
  expect_error(.cn_model("dollars", "MEUR2015"), "cannot be reached from")
})

test_that("a year outside the tables is refused at construction", {
  expect_error(.cn_model("MUSD2020", "MEUR1995"), "cannot be reached from")
})


# --------------------------------------------------------------------------- #
# 6. through interpolation and a solve
# --------------------------------------------------------------------------- #

# The U1 unit kit: every input is 1 and the objective is exactly 8 by hand
# (see test-unit-model.R). Re-pricing every object in one other currency-year
# must therefore scale the objective by exactly that factor -- the strongest
# statement available, because it goes through the whole pipeline.
.cn_kit <- function(cur = NULL, obj_cur = NULL) {
  kit <- topia$modules$unit$U1
  repo <- kit$repo
  if (!is.null(obj_cur)) {
    for (o in getObject(repo)) {
      u <- tryCatch(o@units, error = function(e) NULL)
      if (!is.data.frame(u) || !("currency" %in% names(u))) next
      if (!nrow(u)) u[1, ] <- NA
      u$currency[1] <- obj_cur
      o@units <- u
      repo <- add(repo, o, overwrite = TRUE)
    }
  }
  args <- list(name = "UN_CUR", data = repo,
               calendar = topia$modules$unit$calendars[[kit$calendar]] %||%
                 topia$modules$calendars[[kit$calendar]],
               region = kit$regions,
               horizon = topia$modules$horizons$BY2025, discount = 0)
  if (!is.null(cur)) args$currency <- cur
  do.call(newModel, args)
}

# @covers vObjective depth=S backends=glpk
test_that("one factor on every cost scales the objective by exactly that", {
  skip_if_no_fixtures()
  skip_if_no_solver()
  f <- convert(1, "MUSD2020", "MUSD2015")
  scen <- suppressMessages(suppressWarnings(solve_model(
    .cn_kit("MUSD2015", "MUSD2020"), name = "cn_scaled",
    solver = solver_options$glpk, tmp.del = TRUE, wait = TRUE)))
  obj <- capture_tracked_values(scen)$objective
  expect_equal(obj, 8 * f, tolerance = 1e-9)
  expect_equal(nrow(scen@misc$currency$objects), 2L)
  expect_true(all(scen@misc$currency$objects$converted))
})

test_that("the scenario carries converted costs, the model authored ones", {
  skip_if_no_fixtures()
  scen <- suppressMessages(suppressWarnings(interpolate_model(
    .cn_kit("MUSD2015", "MUSD2020"), "cn_auth", ondisk = FALSE,
    overwrite = TRUE)))
  f <- convert(1, "MUSD2020", "MUSD2015")
  # authored, through scen@model
  sup <- getObject(scen@model, name = "SUP_PRM")[[1]]
  expect_equal(sup@supply$cost, 1)
  expect_equal(sup@units$currency, "MUSD2020")
  # normalised, in the interpolated tables
  v <- scen@modInp@parameters$pSupCost@data
  expect_equal(unique(v$value), f)
})

test_that("a kit with no currency interpolates with no currency provenance", {
  skip_if_no_fixtures()
  scen <- suppressMessages(suppressWarnings(interpolate_model(
    .cn_kit(), "cn_plain", ondisk = FALSE, overwrite = TRUE)))
  expect_null(scen@misc$currency)
})
