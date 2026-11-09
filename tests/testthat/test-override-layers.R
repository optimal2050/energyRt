# =========================================================================== #
# Three override layers: the catalogue, the model, one object.
#
# A parameter's default and interpolation rule come from `parameters.yml`.
# `config@defVal` / `@interpolation` override them model-wide, keyed by
# PARAMETER name. An object's own `@defVal` / `@interpolation` override them
# for that object's rows only, keyed by the short COLUMN name the value is
# written under -- the two levels differ because the information differs: on an
# object the class is known, while at model level a bare column is ambiguous
# (46 of 190 column names are shared across classes).
#
# The property that matters most is the LAST test: a model that sets no
# override must interpolate exactly as it did before the feature existed, which
# is why an override equal to the layer above it records nothing.
# =========================================================================== #

ol_tech <- function(nm) newTechnology(
  name = nm, desc = nm,
  input = data.frame(comm = "COA", unit = "PJ"),
  output = data.frame(comm = "ELC", unit = "GWh"),
  cap2act = 8760,
  # ONE anchor, at the middle milestone, so the rule alone decides which years
  # the series covers
  af = data.frame(year = 2030, af.up = 0.8))

ol_model <- function(..., techs = list(ol_tech("A"), ol_tech("B"))) {
  repo <- newRepository("ol")
  repo <- add(repo,
              newCommodity("COA", desc = "Coal", unit = "PJ"),
              newCommodity("ELC", desc = "Electricity", unit = "GWh"),
              newSupply("SC", commodity = "COA", unit = "PJ",
                        supply = data.frame(cost = 1)),
              newDemand("D", commodity = "ELC",
                        demand = data.frame(year = 2030, demand = 10)))
  for (t in techs) repo <- add(repo, t)
  newModel("ol", data = repo, region = "R1",
           calendar = calendars$d365_ANNUAL,
           horizon = newHorizon(period = 2020:2040, intervals = rep(10, 2)),
           ...)
}

ol_af <- function(m) {
  sc <- suppressMessages(interpolate(m, name = "ol"))
  d <- as.data.frame(
    energyRt:::get_data_slot(sc@modInp@parameters[["pTechAf"]]))
  list(up = d[d$type == "up", c("tech", "year", "value")],
       ov = sc@modInp@parameters[["pTechAf"]]@misc$override)
}

ol_years <- function(d, t) sort(d$year[d$tech == t])


# --------------------------------------------------------------------------- #
# 1. the keys
# --------------------------------------------------------------------------- #

test_that("the column name is the key, and the parameter name is an alias", {
  r <- function(k, cls) energyRt:::.resolve_override_key(k, cls)
  expect_equal(r("af.up", "technology"), "af.up")
  expect_equal(r("pTechAf.up", "technology"), "af.up")
  expect_equal(r("wval", "weather"), "wval")
  expect_equal(r("pWeather", "weather"), "wval")
  # a bare parameter name is the only way to write "every bound at once"
  expect_setequal(r("pTechAf", "technology"), c("af.lo", "af.up", "af.fx"))
})

test_that("an ambiguous column needs its slot, and says so", {
  # the ONE column name that occurs in two slots of one class:
  # storage@input$cap2act -> pStorageInpCap2act, @output$cap2act -> ...Out...
  expect_error(energyRt:::.resolve_override_key("cap2act", "storage"),
               "ambiguous")
  expect_equal(energyRt:::.resolve_override_key("input.cap2act", "storage"),
               "input.cap2act")
  expect_equal(
    energyRt:::.resolve_override_key("pStorageInpCap2act", "storage"),
    "input.cap2act")
})

test_that("a key that names nothing of this class is an error", {
  expect_error(energyRt:::.resolve_override_key("zzz", "technology"),
               "unknown")
  # a real key, but of another class
  expect_error(energyRt:::.resolve_override_key("af.up", "commodity"),
               "unknown")
  te <- ol_tech("A")
  te@interpolation$zzz <- "inter"
  expect_error(energyRt:::.check_param_overrides(te), "unknown")
})

test_that("any accepted rule spelling works and is normalised on read", {
  te <- ol_tech("A")
  te@interpolation$af.up <- "bwd.mid.fwd"
  ov <- energyRt:::.object_overrides(te)
  expect_equal(ov$interpolation[ov$key == "af.up"], "back.inter.forth")
  te@interpolation$af.up <- "nonsense"
  expect_error(energyRt:::.object_overrides(te))
})

test_that("a default must be one finite number", {
  te <- ol_tech("A")
  te@defVal$af.up <- Inf
  expect_error(energyRt:::.check_param_overrides(te), "finite")
})


# --------------------------------------------------------------------------- #
# 2. scope -- the object that carries it, and nothing else
# --------------------------------------------------------------------------- #

test_that("an override moves its own object's series and no other", {
  base <- ol_af(ol_model())
  expect_null(base$ov)                       # nothing recorded, nothing to do
  expect_equal(ol_years(base$up, "A"), ol_years(base$up, "B"))

  A <- ol_tech("A")
  A@interpolation$af.up <- "inter"
  got <- ol_af(ol_model(techs = list(A, ol_tech("B"))))

  # the override is recorded against the member, for that bound only
  expect_equal(nrow(got$ov), 1L)
  expect_equal(got$ov$member, "A")
  expect_equal(got$ov$dim, "tech")
  expect_equal(got$ov$bound, "up")
  expect_equal(got$ov$rule, "inter")

  # B is untouched: same years, same values as the un-overridden run
  expect_equal(ol_years(got$up, "B"), ol_years(base$up, "B"))
  expect_equal(got$up$value[got$up$tech == "B"],
               base$up$value[base$up$tech == "B"])
  # and A is not
  expect_false(identical(ol_years(got$up, "A"), ol_years(base$up, "A")))
})

test_that("both spellings give the same recorded override", {
  A <- ol_tech("A"); A@interpolation$af.up <- "inter"
  B <- ol_tech("A"); B@interpolation$pTechAf.up <- "inter"
  expect_equal(energyRt:::.object_overrides(A),
               energyRt:::.object_overrides(B))
})


# --------------------------------------------------------------------------- #
# 3. the layers, and the fast path
# --------------------------------------------------------------------------- #

test_that("an override equal to the catalogue records nothing", {
  # `pTechAf`'s own rule, written out on the object: the layers agree, so
  # there is nothing to carry into the engine and the fast path survives
  p <- energyRt:::.modInp[["pTechAf"]]
  A <- ol_tech("A")
  A@interpolation$af.up <- as.character(p$interpolation)[
    min(2L, length(p$interpolation))]
  got <- ol_af(ol_model(techs = list(A, ol_tech("B"))))
  expect_null(got$ov)
})

test_that("a model with no overrides interpolates as it always did", {
  # the property the goldens check globally, asserted here directly: the
  # override machinery is invisible until something is set
  a <- ol_af(ol_model())
  b <- ol_af(ol_model())
  expect_equal(a$up, b$up)
  expect_null(a$ov)
})

test_that("the numeric defVal argument folds into the table", {
  # `weather`/`tax`/`subsidy` carried a numeric `defVal` slot until 0.92; the
  # argument keeps working and writes the column the class owns
  w0 <- newWeather("WX", defVal = 0,
                   weather = data.frame(timeslice = "ANNUAL", wval = 1))
  w3 <- newWeather("WX", defVal = 3,
                   weather = data.frame(timeslice = "ANNUAL", wval = 1))
  # 0 IS the catalogue default for pWeather, so it claims nothing
  expect_equal(ncol(w0@defVal), 0L)
  expect_equal(w3@defVal$wval, 3)

  # a levy's bare number covers its three cost columns
  t <- newTax("T", comm = "CO2", defVal = 0.05)
  expect_setequal(names(t@defVal), c("inp", "out", "bal"))
  expect_true(all(unlist(t@defVal[1, ]) == 0.05))
  expect_equal(ncol(newTax("T", comm = "CO2", defVal = 0)@defVal), 0L)
})
