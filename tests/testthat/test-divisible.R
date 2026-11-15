# @covers is_divisible depth=U
test_that("a rate is carried as declared; a quantity asks", {
  d <- is_divisible(column = c("cap.up", "demand", "eff", "cost"))
  expect_true(all(d$action[d$column %in% c("cap.up", "demand")] == "decide"))
  expect_true(all(d$needs_rule[d$column %in% c("cap.up", "demand")]))
  expect_true(all(d$action[d$column %in% c("eff", "cost")] == "replicate"))
  expect_false(any(d$needs_rule[d$column %in% c("eff", "cost")]))
  expect_false(attr(d, "sufficient"))
})

# @covers is_divisible depth=U
test_that("every quantity asks, bound or not, and says why", {
  d <- is_divisible(column = c("cap.fx", "cap.lo", "cap.up", "stock",
                               "ava.fx"))
  expect_true(all(d$action == "decide"))
  expect_match(d$reason[d$column == "stock"], "pooled, indivisible or drop")
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
  expect_setequal(unique(d$action), c("decide", "replicate"))
  # a quantity is a quantity whether or not its column is a bound
  expect_equal(sum(d$action == "decide"), sum(d$rule == "sum"))
  # the levies are rates per unit and keep the common value
  lev <- d[d$class %in% c("tax", "subsidy"), ]
  expect_true(all(lev$action == "replicate"))
})

# @covers is_divisible depth=U
test_that("a ruling settles a column, and the most specific key wins", {
  d <- is_divisible(column = c("ava.up", "cost"),
                    rules = c("supply@ava.up" = "pooled"))
  expect_equal(d$action[d$column == "ava.up"], "decide")  # no class to key on
  d2 <- is_divisible(column = "ava.up", rules = c(ava.up = "pooled"))
  expect_equal(d2$action, "decide")                       # keys are who@column
  sup <- newSupply("SUP_X", commodity = "COA",
                   supply = data.frame(region = "R1", year = 2030,
                                       ava.up = 10, cost = 2))
  r1 <- is_divisible(sup, rules = c(supply = "pooled"))
  expect_equal(r1$action[r1$column == "ava.up"], "split")
  expect_equal(r1$rule_given[r1$column == "ava.up"], "pooled")
  expect_true(attr(r1, "sufficient"))
  r2 <- is_divisible(sup, rules = c(supply = "pooled",
                                    "supply@ava.up" = "indivisible"))
  expect_equal(r2$action[r2$column == "ava.up"], "keep")
  r3 <- is_divisible(sup, rules = c(supply = "drop", NOPE = "pooled"))
  expect_equal(r3$action[r3$column == "ava.up"], "drop")
  expect_equal(attr(r3, "unmatched_rules"), "NOPE")
})

# @covers is_divisible depth=U
test_that("a ruling must be named and from the vocabulary", {
  expect_error(is_divisible(column = "cap.up", rules = "pooled"), "must be named")
  expect_error(is_divisible(column = "cap.up", rules = c(technology = "share")),
               "not a subset rule")
})

# @covers is_divisible depth=U
test_that("an object reports what it carries, not the whole contract", {
  sup <- newSupply("SUP_X", commodity = "COA",
                   supply = data.frame(region = "R1", year = 2030,
                                       ava.up = 10, cost = 2))
  d <- is_divisible(sup)
  expect_setequal(d$column[d$slot == "supply"], c("ava.up", "cost"))
  expect_equal(d$action[d$column == "ava.up"], "decide")
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
  # the whole model asks about exactly two columns: the two zone resources
  ask <- d[d$needs_rule, c("name", "column")]
  expect_setequal(ask$name, c("SUP_COA", "SUP_GAS"))
  expect_true(all(ask$column == "ava.up"))
  expect_false(attr(d, "sufficient"))
  expect_true(attr(is_divisible(mod, rules = c(supply = "pooled")),
                   "sufficient"))
  # the corridor costs are rates, so a split zone does not divide them
  expect_true(all(d$action[d$slot == "invcost"] == "replicate"))
  # every technology is region-free or province-level: none appears
  expect_false(any(c("ECOA", "EWIN", "ENUC") %in% d$name))
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
