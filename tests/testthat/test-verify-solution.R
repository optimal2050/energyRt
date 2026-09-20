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
  # Skipped is not a pass certificate -- and `ok` now says so. Counting only
  # "violated" made a scenario with NO solution verify clean, which is how an
  # all-zero solve (every variable empty, every check skipped for want of rows)
  # sailed through `expect_true(vs$ok)` in the golden suites for a whole day.
  expect_false(vs$ok)
  expect_equal(vs$n_ran, 0L)
  expect_equal(vs$n_skipped, length(vs$checks))
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

test_that("join keys of differing type are aligned, not fatal", {
  # a parameter can carry `year` as character where its gating map has integer
  # (pDiscountFactor vs mvTotalCost); data.table refuses such a join, which
  # downgraded the whole objective check to "skipped"
  d <- data.table::data.table(region = "R1", year = c("2030", "2040"),
                              value = c(0.5, 0.25))
  kk <- data.table::data.table(region = "R1", year = c(2030L, 2040L))
  on_cols <- c("region", "year")
  expect_error(d[kk, on = on_cols], "Incompatible join types")

  a <- energyRt:::.vs_align_keys(d, data.table::copy(kk), on_cols)
  expect_identical(class(a$d$year)[1], class(a$kk$year)[1])
  m <- a$d[a$kk, on = on_cols]
  expect_equal(m$value, c(0.5, 0.25))

  # matching types are left untouched
  kk2 <- data.table::data.table(region = "R1", year = c("2030", "2040"))
  b <- energyRt:::.vs_align_keys(d, kk2, on_cols)
  expect_identical(b$d$year, d$year)
})

# The divergence table: how far each identity came from closing, reported even
# when the check PASSED. Without it, tolerances can only be guessed -- the
# numbers that justify `tol_abs`/`tol_rel` were computed and thrown away.
test_that("divergence reports every check, worst first", {
  skip_if_no_fixtures()
  skip_if_no_solver()
  vs <- verify_solution(.vs_solved("tm_core"))
  d <- vs$divergence
  expect_s3_class(d, "data.frame")
  expect_setequal(d$check, names(vs$checks))
  expect_named(d, c("check", "status", "n_compared", "n_violated",
                    "max_abs", "median_abs", "mean_abs", "max_rel"))
  # worst first, skipped (NA) last
  fin <- d$max_abs[!is.na(d$max_abs)]
  expect_false(is.unsorted(rev(fin)))
  expect_true(all(is.na(d$max_abs[is.na(d$max_abs)])))
  # a passing check still reports its distance -- that is the whole point
  expect_true(all(d$n_violated[d$status == "ok"] == 0L))
  expect_true(all(d$n_compared[d$status == "ok"] > 0L))
  expect_true(all(d$median_abs[d$status == "ok"] <= d$max_abs[d$status == "ok"]))
})

test_that("divergence records the size of a seeded corruption", {
  skip_if_no_fixtures()
  skip_if_no_solver()
  scen <- .vs_corrupt(.vs_solved("tm_core"), "vOutTot", delta = 2.5)
  d <- verify_solution(scen)$divergence
  bal <- d[d$check == "balance", ]
  expect_equal(bal$status, "violated")
  expect_gte(bal$max_abs, 2.5 - 1e-6)   # the corruption is visible in the stats
  expect_gte(bal$n_violated, 1L)
  # and it sorts to the top
  expect_equal(d$check[1], "balance")
})

test_that("a skipped check contributes an NA row, not a missing one", {
  skip_if_no_fixtures()
  env <- .mapping_fixture_env()
  scen <- suppressMessages(suppressWarnings(
    interpolate_model(env$tm_core(), name = "vs_div_skip", ondisk = FALSE)))
  d <- verify_solution(scen)$divergence
  expect_equal(nrow(d), length(verify_solution(scen)$checks))
  expect_true(all(d$status == "skipped"))
  expect_true(all(is.na(d$max_abs)))
  expect_true(all(d$n_compared == 0L))
})
