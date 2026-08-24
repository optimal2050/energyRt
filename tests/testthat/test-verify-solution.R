# =========================================================================== #
# verify_solution(): the generic identity checker must pass on every valid
# solved fixture, catch seeded corruptions, and skip (never silently pass)
# when inputs are missing. See R/verify_solution.R.
# =========================================================================== #

# solved_tier() (helper-mapping.R) caches one GLPK solve per tier
.vs_solved <- function(tier) solved_tier(tier)

# bump one value of a solution variable, returning a corrupted copy
.vs_corrupt <- function(scen, variable, delta = 1, row = 1L) {
  v <- scen@modOut@variables[[variable]]
  d <- as.data.frame(energyRt:::get_data_slot(v))
  d$value[row] <- d$value[row] + delta
  v@data <- d
  scen@modOut@variables[[variable]] <- v
  scen
}

# @covers vBalance vOutTot vInpTot vTotalCost vObjective depth=S backends=glpk
# @covers eqBal eqOutTot eqInpTot eqCost eqObjective depth=S backends=glpk
test_that("all identities hold on solved tier models", {
  skip_if_no_fixtures()
  skip_if_no_solver()
  for (tier in c("tm_core", "tm_weather")) {
    vs <- verify_solution(.vs_solved(tier))
    expect_true(vs$ok, label = paste0(tier, " invariants"))
    statuses <- vapply(vs$checks, function(x) x$status, "")
    expect_true(all(statuses %in% c("ok", "skipped")),
                label = paste0(tier, " statuses"))
    # the core checks must actually RUN on these fixtures, not skip
    expect_equal(unname(statuses[c("balance", "balance_sign", "out_tot",
                                   "inp_tot", "cost", "objective")]),
                 rep("ok", 6), label = paste0(tier, " all checks ran"))
  }
})

test_that("a corrupted total is caught by balance and out_tot", {
  skip_if_no_fixtures()
  skip_if_no_solver()
  scen <- .vs_corrupt(.vs_solved("tm_core"), "vOutTot", delta = 2.5)
  vs <- verify_solution(scen)
  expect_false(vs$ok)
  expect_equal(vs$checks$balance$status, "violated")
  expect_equal(vs$checks$out_tot$status, "violated")
  expect_equal(vs$checks$cost$status, "ok")
  expect_gte(nrow(vs$checks$balance$violations), 1)
})

test_that("a corrupted cost component is caught by the cost decomposition", {
  skip_if_no_fixtures()
  skip_if_no_solver()
  scen0 <- .vs_solved("tm_core")
  # pick any populated cost-role component (not the totals themselves)
  comp <- NULL
  for (nm in names(scen0@modOut@variables)) {
    v <- scen0@modOut@variables[[nm]]
    if (!identical(v@role, "cost")) next
    if (nm %in% c("vObjective", "vTotalCost", "vUserCosts",
                  "vTechInv", "vStorageInv", "vTradeInv")) next
    d <- energyRt:::get_data_slot(v, optional = TRUE)
    if (!is.null(d) && nrow(d) > 0) { comp <- nm; break }
  }
  skip_if(is.null(comp), "no populated cost component in tm_core solution")
  scen <- .vs_corrupt(scen0, comp, delta = 100)
  vs <- verify_solution(scen)
  expect_false(vs$ok)
  expect_equal(vs$checks$cost$status, "violated")
})

test_that("a corrupted objective is caught", {
  skip_if_no_fixtures()
  skip_if_no_solver()
  scen <- .vs_corrupt(.vs_solved("tm_core"), "vObjective", delta = 1)
  vs <- verify_solution(scen, checks = "objective")
  expect_false(vs$ok)
  expect_equal(vs$checks$objective$status, "violated")
})

test_that("an unsolved scenario skips every check rather than passing", {
  skip_if_no_fixtures()
  env <- .mapping_fixture_env()
  scen <- suppressMessages(suppressWarnings(
    interpolate_model(env$tm_core(), name = "vs_unsolved", ondisk = FALSE)))
  vs <- verify_solution(scen)
  expect_true(all(vapply(vs$checks, function(x) x$status, "") == "skipped"))
  # skipped is not a pass certificate, but it is not a violation either
  expect_true(vs$ok)
})

test_that("tolerances separate solver noise from real violations", {
  skip_if_no_fixtures()
  skip_if_no_solver()
  # noise below tol_abs must pass
  scen <- .vs_corrupt(.vs_solved("tm_core"), "vOutTot", delta = 1e-9)
  expect_true(verify_solution(scen)$ok)
  # the same corruption above tolerance must fail
  scen <- .vs_corrupt(.vs_solved("tm_core"), "vOutTot", delta = 1e-3)
  expect_false(verify_solution(scen)$ok)
})

test_that("print method summarises statuses", {
  skip_if_no_fixtures()
  skip_if_no_solver()
  out <- capture.output(print(verify_solution(.vs_solved("tm_core"))))
  expect_match(out[1], "verify_solution: scenario 'st_tm_core' -- OK")
  expect_true(any(grepl("balance\\s+ok", out)))
})
