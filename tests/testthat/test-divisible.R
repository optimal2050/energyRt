# @covers is_divisible depth=U
test_that("the rule decides: a quantity splits, a rate is replicated", {
  d <- is_divisible(column = c("cap.up", "demand", "eff", "cost"))
  expect_equal(d$action[d$column == "cap.up"], "split")
  expect_equal(d$action[d$column == "demand"], "split")
  expect_true(all(d$action[d$column %in% c("eff", "cost")] == "replicate"))
  expect_true(d$divisible[d$column == "cap.up"])
  expect_false(d$divisible[d$column == "eff"])
})

# @covers is_divisible depth=U
test_that("a fixed or floor quantity is undecidable, and says why", {
  d <- is_divisible(column = c("cap.fx", "cap.lo", "cap.up", "ava.fx"))
  dec <- d$column[d$action == "decide"]
  expect_setequal(dec, c("cap.fx", "cap.lo", "ava.fx"))
  expect_true(all(is.na(d$divisible[d$action == "decide"])))
  expect_match(d$reason[d$column == "cap.fx"], "fixed or floor")
  # a ceiling is not lumpy: the parts' ceilings sum to the parent's
  expect_equal(d$action[d$column == "cap.up"], "split")
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
  expect_setequal(unique(d$action), c("split", "replicate", "decide"))
  # the levies are rates per unit and keep the common value
  lev <- d[d$class %in% c("tax", "subsidy"), ]
  expect_true(all(lev$action == "replicate"))
})

# @covers is_divisible depth=U
test_that("an object reports what it carries, not the whole contract", {
  sup <- newSupply("SUP_X", commodity = "COA",
                   supply = data.frame(region = "R1", year = 2030,
                                       ava.up = 10, cost = 2))
  d <- is_divisible(sup)
  expect_setequal(d$column[d$slot == "supply"], c("ava.up", "cost"))
  expect_equal(d$action[d$column == "ava.up"], "split")
  expect_equal(d$action[d$column == "cost"], "replicate")
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
  expect_equal(d$action[d$name == "SUP_COA" & d$column == "ava.up"], "split")
  expect_equal(d$action[d$name == "SUP_COA" & d$column == "cost"],
               "replicate")
  # the corridor costs are rates, so a split zone does not divide them
  expect_true(all(d$action[d$slot == "invcost"] == "replicate"))
  # every technology is region-free or province-level: none appears
  expect_false(any(c("ECOA", "EWIN", "ENUC") %in% d$name))
})
