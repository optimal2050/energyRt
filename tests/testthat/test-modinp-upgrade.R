# The 2026-08 modInp slot cleanup: `set` retired, `gams.equation` /
# `costs.equation` renamed to `user_constraints` / `user_costs`. Objects
# serialized before it carry the old slots as plain attributes;
# .upgrade_modInp() (called by load_scenario() and the variant swap) moves
# them onto the new slots.

test_that(".upgrade_modInp migrates pre-cleanup slot attributes", {
  mi <- new("modInp")
  attr(mi, "set") <- list(region = c("R1", "R2"))
  attr(mi, "gams.equation") <- list(CNS1 = list(equation = "x =l= 1;"))
  attr(mi, "costs.equation") <- "pUserCost1"
  up <- energyRt:::.upgrade_modInp(mi)
  expect_identical(up@sets$region, c("R1", "R2"))
  expect_identical(names(up@user_constraints), "CNS1")
  expect_identical(up@user_costs, "pUserCost1")
  # exact = TRUE: plain attr() would partially match the live `sets` slot
  expect_null(attr(up, "set", exact = TRUE))
  expect_null(attr(up, "gams.equation", exact = TRUE))
  expect_null(attr(up, "costs.equation", exact = TRUE))
})

test_that(".upgrade_modInp never clobbers populated new slots", {
  mi <- new("modInp")
  mi@sets <- list(region = "NEW")
  mi@user_constraints <- list(KEEP = list(equation = "y =g= 0;"))
  mi@user_costs <- "pKeep"
  attr(mi, "set") <- list(region = "OLD")
  attr(mi, "gams.equation") <- list(STALE = 1)
  attr(mi, "costs.equation") <- "pStale"
  up <- energyRt:::.upgrade_modInp(mi)
  expect_identical(up@sets$region, "NEW")
  expect_identical(names(up@user_constraints), "KEEP")
  expect_identical(up@user_costs, "pKeep")
  expect_null(attr(up, "gams.equation"))
})

test_that(".upgrade_modInp is a no-op on current objects and non-modInp", {
  mi <- new("modInp")
  expect_identical(energyRt:::.upgrade_modInp(mi), mi)
  expect_identical(energyRt:::.upgrade_modInp(list(a = 1)), list(a = 1))
})
