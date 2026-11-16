mk_sup <- function() {
  newSupply("SUP_X", commodity = "COA",
            supply = data.frame(region = "R1", year = 2030,
                                ava.up = 10, cost = 2))
}

# @covers is_divisible depth=U
test_that("a rate is carried; a quantity asks for a weight", {
  d <- is_divisible(column = c("cap.up", "demand", "eff", "cost"))
  expect_true(all(d$needs[d$column %in% c("cap.up", "demand")] == "weight"))
  expect_false(any(d$ready[d$column %in% c("cap.up", "demand")]))
  expect_true(all(d$needs[d$column %in% c("eff", "cost")] == ""))
  expect_true(all(d$ready[d$column %in% c("eff", "cost")]))
  expect_false(attr(d, "ready"))
})

# @covers is_divisible depth=U
test_that("every quantity asks, bound or not, and says why", {
  d <- is_divisible(column = c("cap.fx", "cap.lo", "cap.up", "stock",
                               "ava.fx"))
  expect_true(all(d$needs == "weight"))
  expect_match(d$reason[d$column == "stock"], "name the weight")
})

# @covers is_divisible depth=U
test_that("an identifier is not a value column", {
  d <- is_divisible(column = c("region", "year", "comm", "cap.up"))
  expect_equal(d$column, "cap.up")
})

# @covers is_divisible depth=U
test_that("the declared contract is reported, and where each rule came from", {
  d <- is_divisible()
  expect_equal(nrow(d), nrow(.defRecast))
  expect_true(all(d$source == "declared"))
  # a quantity is a quantity whether or not its column is a bound
  expect_equal(d$needs == "weight", d$rule == "sum")
  expect_equal(sum(d$needs == "weight"), 59L)
  expect_equal(sum(d$needs == ""), 198L)
  # the levies are rates per unit and keep the common value
  lev <- d[d$class %in% c("tax", "subsidy"), ]
  expect_true(all(lev$ready))
  expect_false(attr(d, "ready"))
})

# @covers is_divisible depth=U
test_that("a named weight settles a quantity, and the most specific key wins", {
  sup <- mk_sup()
  w1 <- is_divisible(sup, split_weight = list(supply = "area"))
  expect_equal(w1$weight[w1$column == "ava.up"], "area")
  expect_true(all(w1$ready))
  expect_true(attr(w1, "ready"))
  # a named character is one weight per column; the object key beats the class
  w2 <- is_divisible(sup, split_weight = list(supply = c(ava.up = "area"),
                                              SUP_X = c(ava.up = "km2")))
  expect_equal(w2$weight[w2$column == "ava.up"], "km2")
  # the flat shorthand reads the same
  w3 <- is_divisible(sup, split_weight = c("SUP_X:ava.up" = "area"))
  expect_equal(w3$weight[w3$column == "ava.up"], "area")
  # a rate is never given a weight
  expect_true(is.na(w1$weight[w1$column == "cost"]))
})

# @covers is_divisible depth=U
test_that("explicit shares are a weight too", {
  sup <- mk_sup()
  s1 <- is_divisible(sup, split_weight = list(supply = c(R1 = 0.6, R2 = 0.4)))
  expect_equal(s1$weight[s1$column == "ava.up"], "shares")
  expect_true(attr(s1, "ready"))
  s2 <- is_divisible(sup, split_weight =
                       list(SUP_X = list(ava.up = c(R1 = 0.6, R2 = 0.4))))
  expect_true(attr(s2, "ready"))
})

# @covers is_divisible depth=U
test_that("keep and drop settle a column without a weight", {
  sup <- mk_sup()
  k <- is_divisible(sup, keep = "SUP_X")
  expect_match(k$reason[k$column == "ava.up"], "whole in every part")
  expect_true(is.na(k$weight[k$column == "ava.up"]))
  expect_true(attr(k, "ready"))
  d <- is_divisible(sup, drop = "SUP_X:ava.up")
  expect_match(d$reason[d$column == "ava.up"], "left out")
  expect_true(attr(d, "ready"))
})

# @covers is_divisible depth=U
test_that("what the caller gave is checked, and what matched nothing is named", {
  sup <- mk_sup()
  u <- is_divisible(sup, split_weight = list(NOPE = "area", supply = "area"))
  expect_equal(attr(u, "unmatched"), "NOPE")
  expect_error(is_divisible(sup, split_weight = "area"), "must be named")
})

# @covers is_divisible depth=U
test_that("an object reports what it carries, not the whole contract", {
  d <- is_divisible(mk_sup())
  expect_setequal(d$column[d$slot == "supply"], c("ava.up", "cost"))
  expect_equal(d$name, c("SUP_X", "SUP_X"))
  expect_equal(d$needs[d$column == "ava.up"], "weight")
  expect_equal(d$needs[d$column == "cost"], "")
})

# @covers is_divisible depth=U
test_that("the time axis asks for rules, never for a weight", {
  t <- is_divisible(axis = "time")
  expect_false(any(t$needs == "weight"))
  # no time rule is `sum`: a timeslice value is a rate
  expect_false(any(t$rule == "sum", na.rm = TRUE))
  expect_true(sum(t$needs == "rule") > 0L)
})

# @covers is_divisible depth=U
test_that("the model shape keeps only declarations above its own regions", {
  skip_if_not_installed("geoscales")
  k <- topia$modules$electricity
  mod <- newModel("T", data = k$repo, calendar = calendars[["d365_h24_1dps"]],
                  region = k$regions,
                  horizon = topia$modules$horizons$BY2025_50_by10,
                  discount = 0.05)
  mod <- setGeoscale(mod, k$geoscale)
  d <- is_divisible(mod)
  # TOPIA declares fuels at the zone: the two fuel commodities, two supplies,
  # two imports, three corridor costs -- and nothing else sits above the
  # provinces.
  expect_setequal(unique(d$name),
                  c("COA", "GAS", "SUP_COA", "SUP_GAS", "IMP_COA", "IMP_GAS",
                    "TBD_COA_Z4_Z3", "TBD_COA_Z4_Z5", "TBD_GAS_Z2_Z3"))
  # `@geoframe = "province"` is the model's own finest frame, not a coarse
  # declaration, so LANDW and its kin stay out
  expect_false("LANDW" %in% d$name)
  # the whole model asks about exactly two columns: the two zone resources
  ask <- d[!d$ready, c("name", "column")]
  expect_setequal(ask$name, c("SUP_COA", "SUP_GAS"))
  expect_true(all(ask$column == "ava.up"))
  expect_false(attr(d, "ready"))
  # the corridor costs are rates, so a split zone does not divide them
  expect_true(all(d$ready[d$slot == "invcost"]))
  # every technology is region-free or province-level: none appears
  expect_false(any(c("ECOA", "EWIN", "ENUC") %in% d$name))
  w <- is_divisible(mod, split_weight = list(supply = "area"))
  expect_true(attr(w, "ready"))
  # a weight the geoscale does not declare is refused, naming what it has
  expect_error(is_divisible(mod, split_weight = list(supply = "population")),
               "not a weight column")
})

# @covers is_divisible depth=U
test_that("a model with no declared regions falls back to its geoscale", {
  skip_if_not_installed("geoscales")
  k <- topia$modules$electricity
  bare <- newModel(name = "T", repository = k$repo, geoscale = k$geoscale)
  expect_length(bare@config@region, 0L)
  full <- newModel("T", data = k$repo, region = k$regions,
                   calendar = calendars[["d365_h24_1dps"]],
                   horizon = topia$modules$horizons$BY2025_50_by10,
                   discount = 0.05)
  full <- setGeoscale(full, k$geoscale)
  expect_setequal(unique(is_divisible(bare)$name),
                  unique(is_divisible(full)$name))
})
