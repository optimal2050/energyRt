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
                    "max_abs", "median_abs", "mean_abs", "max_rel",
                    "max_scaled"))
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

# =========================================================================== #
# Thread 3 stage 1 -- scaffolding: registry, sentinels, verbose, print(detail)
# =========================================================================== #

test_that("verify_checks() describes the vocabulary, incl. the data path", {
  vc <- verify_checks()
  expect_true(all(c("check", "group", "path", "tier") %in% names(vc)))
  expect_true(nrow(vc) >= 9L)
  expect_true(all(vc$tier %in% c("default", "optin")))
  expect_true(all(vc$path %in% c("modInp", "objects", "registry")))
  # the distinction the whole design rests on: only an objects-path check can
  # catch a map that was never built
  expect_true(any(vc$path == "objects"))
  expect_true(any(vc$path == "modInp"))
})

test_that("the 'default' and 'all' sentinels expand, explicit names still work", {
  skip_if_no_fixtures()
  skip_if_no_solver()
  scen <- .vs_solved("tm_core")
  vc <- verify_checks()
  dflt <- verify_solution(scen)
  expect_setequal(names(dflt$checks), vc$check[vc$tier == "default"])
  all_ <- verify_solution(scen, checks = "all")
  expect_setequal(names(all_$checks), vc$check)
  # back-compat: the pre-stage-1 call form is unchanged
  old <- verify_solution(scen, checks = c("balance", "objective"))
  expect_equal(names(old$checks), c("balance", "objective"))
})

test_that("print(detail = 'issues') narrows the view but not the computation", {
  skip_if_no_fixtures()
  skip_if_no_solver()
  vs <- verify_solution(.vs_solved("tm_core"))
  full <- utils::capture.output(print(vs, detail = "full"))
  iss <- utils::capture.output(print(vs, detail = "issues"))
  expect_true(length(full) > length(iss))
  # every requested check was computed regardless of the view
  expect_setequal(names(vs$checks), verify_checks()$check[
    verify_checks()$tier == "default"])
})

test_that("verbose reports each check as it runs", {
  skip_if_no_fixtures()
  skip_if_no_solver()
  msg <- utils::capture.output(
    verify_solution(.vs_solved("tm_core"), checks = "balance", verbose = TRUE),
    type = "message")
  expect_true(any(grepl("balance", msg)))
})

# =========================================================================== #
# Thread 3 stage 2 -- positivity (B9), inputs_present (A1), units (D1)
# =========================================================================== #

test_that("positivity catches a negative value in a variable declared positive", {
  skip_if_no_fixtures()
  skip_if_no_solver()
  scen <- .vs_solved("tm_core")
  clean <- verify_solution(scen, checks = "positivity")
  expect_equal(clean$checks$positivity$status, "ok")
  expect_gt(clean$checks$positivity$n, 0L)          # it must actually run

  # seed one negative into vTechCap, which the registry declares positive
  bad <- .vs_corrupt(scen, "vTechCap", delta = -1e6)
  vs <- verify_solution(bad, checks = "positivity")
  expect_equal(vs$checks$positivity$status, "violated")
  expect_false(vs$ok)
  expect_true("vTechCap" %in% vs$checks$positivity$violations$variable)
})

test_that("inputs_present catches an object that never reached the sets", {
  skip_if_no_fixtures()
  skip_if_no_solver()
  scen <- .vs_solved("tm_core")
  clean <- verify_solution(scen, checks = "inputs_present")
  expect_equal(clean$checks$inputs_present$status, "ok")
  expect_gt(clean$checks$inputs_present$n, 0L)

  # drop a declared technology from the `tech` set: the object still exists in
  # the model, so the OBJECTS path must notice it no longer reaches the model
  # inputs. This is the shape of the "silently dropped object" family of bugs.
  bad <- scen
  tset <- as.character(bad@modInp@sets$tech)
  bad@modInp@sets$tech <- setdiff(tset, "ECOA")
  vs <- verify_solution(bad, checks = "inputs_present")
  expect_equal(vs$checks$inputs_present$status, "violated")
  expect_false(vs$ok)
  expect_true("ECOA" %in% vs$checks$inputs_present$violations$object)
  expect_false(vs$checks$inputs_present$violations[
    object == "ECOA"]$in_set)
})

test_that("units reports unresolved units and never fails", {
  skip_if_no_fixtures()
  skip_if_no_solver()
  vs <- verify_solution(.vs_solved("tm_core"), checks = "units")
  u <- vs$checks$units
  expect_equal(u$status, "ok")        # report-only: never "violated"
  expect_gt(u$n, 0L)
  # the tm fixtures declare no units, so the report must be non-empty -- that
  # is the "how much is actually declared" signal stage 5 is sequenced behind
  expect_true(NROW(u$report) > 0L)
  expect_true(all(c("object", "parameter", "unit") %in% names(u$report)))
  expect_true(vs$ok)                  # a missing unit never blocks
})

test_that("object-path checks skip on an unsolved scenario, like the rest", {
  skip_if_no_fixtures()
  env <- .mapping_fixture_env()
  scen <- suppressMessages(suppressWarnings(
    interpolate_model(env$tm_core(), name = "vs_unsolved2", ondisk = FALSE)))
  vs <- verify_solution(scen, checks = c("inputs_present", "units"))
  # these read scen@model@data and COULD run without a solution -- gating them
  # keeps `verify_solution(unsolved)$ok` false, which is the invariant the
  # n_ran accounting exists to protect
  expect_equal(unname(vapply(vs$checks, function(x) x$status, "")),
               c("skipped", "skipped"))
  expect_false(vs$ok)
})

# =========================================================================== #
# Thread 3 stage 3 -- the objects path: inputs_values (A3), inputs_bounds (A2)
# =========================================================================== #

# corrupt one value of a modInp PARAMETER (not a solution variable)
.vs_corrupt_par <- function(scen, param, delta = 1, row = 1L) {
  p <- scen@modInp@parameters[[param]]
  d <- as.data.frame(energyRt:::get_data_slot(p))
  d$value[row] <- d$value[row] + delta
  p@data <- d
  scen@modInp@parameters[[param]] <- p
  scen
}

.vs_drop_par_row <- function(scen, param, row = 1L) {
  p <- scen@modInp@parameters[[param]]
  d <- as.data.frame(energyRt:::get_data_slot(p))
  p@data <- d[-row, , drop = FALSE]
  scen@modInp@parameters[[param]] <- p
  scen
}

test_that("inputs_values passes on every tier fixture without false positives", {
  skip_if_no_fixtures()
  skip_if_no_solver()
  for (tier in c("tm_core", "tm_flows", "tm_io", "tm_policy", "tm_weather")) {
    vs <- verify_solution(.vs_solved(tier), checks = "inputs_values")
    ck <- vs$checks$inputs_values
    expect_equal(ck$status, "ok", label = paste0(tier, " inputs_values"))
    expect_gt(ck$n, 0L)          # and it must have compared something
  }
})

test_that("inputs_values catches a value changed after interpolation", {
  skip_if_no_fixtures()
  skip_if_no_solver()
  # pTechInvcost is fed straight from technology@invcost$invcost
  bad <- .vs_corrupt_par(.vs_solved("tm_core"), "pTechInvcost", delta = 77)
  vs <- verify_solution(bad, checks = "inputs_values")
  expect_equal(vs$checks$inputs_values$status, "violated")
  expect_false(vs$ok)
  v <- as.data.frame(vs$checks$inputs_values$violations)
  expect_true("pTechInvcost" %in% v$parameter)
  expect_true(any(grepl("changed", v$issue)))
})

test_that("inputs_values catches a declared value that never reached modInp", {
  skip_if_no_fixtures()
  skip_if_no_solver()
  bad <- .vs_drop_par_row(.vs_solved("tm_core"), "pTechInvcost", row = 1L)
  vs <- verify_solution(bad, checks = "inputs_values")
  expect_equal(vs$checks$inputs_values$status, "violated")
  v <- as.data.frame(vs$checks$inputs_values$violations)
  expect_true(any(grepl("absent", v$issue)))
})

test_that("inputs_bounds passes on the tiers that declare bounds", {
  skip_if_no_fixtures()
  skip_if_no_solver()
  for (tier in c("tm_io", "tm_policy", "tm_weather")) {
    ck <- verify_solution(.vs_solved(tier),
                          checks = "inputs_bounds")$checks$inputs_bounds
    expect_equal(ck$status, "ok", label = paste0(tier, " inputs_bounds"))
    expect_gt(ck$n, 0L)
  }
  # a model with no bounds must SKIP with a reason, never silently pass
  ck <- verify_solution(.vs_solved("tm_core"),
                        checks = "inputs_bounds")$checks$inputs_bounds
  expect_equal(ck$status, "skipped")
  expect_match(ck$reason, "no bounds")
})

test_that("inputs_bounds catches a solution that breaks a declared bound", {
  skip_if_no_fixtures()
  skip_if_no_solver()
  scen <- .vs_solved("tm_io")
  ck0 <- verify_solution(scen, checks = "inputs_bounds")$checks$inputs_bounds
  expect_equal(ck0$status, "ok")
  # EXP_ELC declares exp.up, which bounds vExportRow -- push it past the ceiling
  bad <- .vs_corrupt(scen, "vExportRow", delta = 1e6)
  vs <- verify_solution(bad, checks = "inputs_bounds")
  expect_equal(vs$checks$inputs_bounds$status, "violated")
  expect_false(vs$ok)
  v <- as.data.frame(vs$checks$inputs_bounds$violations)
  expect_true("vExportRow" %in% v$variable)
  expect_true(all(v$direction[v$variable == "vExportRow"] == "up"))
})

test_that("a bound whose parameter has no solution variable is skipped, not guessed", {
  skip_if_no_fixtures()
  skip_if_no_solver()
  # supply ava.up maps to pSupAva, and there is no vSupAva to test it against;
  # the check must leave it alone rather than invent a comparison
  expect_false("vSupAva" %in% names(energyRt:::.variables))
  ck <- verify_solution(.vs_solved("tm_io"),
                        checks = "inputs_bounds")$checks$inputs_bounds
  expect_equal(ck$status, "ok")
})

test_that("every check runs on every tier without erroring into a skip", {
  skip_if_no_fixtures()
  skip_if_no_solver()
  # A check that errors is reported as "skipped (check errored: ...)", which
  # looks like an honest skip in the summary. `inputs_present` did exactly
  # that on tm_weather -- `[[` on a named vector raises "subscript out of
  # bounds" for a class with no set, instead of returning NULL -- so the whole
  # check silently stopped covering anything.
  for (tier in c("tm_core", "tm_flows", "tm_io", "tm_policy", "tm_weather")) {
    vs <- verify_solution(.vs_solved(tier), checks = "all")
    errored <- vapply(vs$checks, function(ck) {
      identical(ck$status, "skipped") &&
        grepl("check errored", ck$reason %||% "")
    }, logical(1))
    expect_false(any(errored),
                 label = paste0(tier, " checks errored: ",
                                paste(names(vs$checks)[errored], collapse = ", ")))
    expect_true(vs$ok, label = paste0(tier, " all checks"))
  }
})

# =========================================================================== #
# Thread 3 stage 4 -- storage_dynamics (B7)
# =========================================================================== #

test_that("storage_dynamics holds where the store is actually used", {
  skip_if_no_fixtures()
  skip_if_no_solver()
  ck <- verify_solution(.vs_solved("tm_core"),
                        checks = "storage_dynamics")$checks$storage_dynamics
  expect_equal(ck$status, "ok")
  expect_gt(ck$n, 0L)
})

test_that("an unused store SKIPS rather than passing on an empty table", {
  skip_if_no_fixtures()
  skip_if_no_solver()
  # A solver exports non-zeros only, so a store that is never charged leaves
  # vStorageLevel with no rows at all. Every term then defaults to zero and
  # `0 = 0` holds for every row -- the check would report "ok" over 16
  # comparisons having verified precisely nothing. It must skip instead.
  for (tier in c("tm_flows", "tm_io", "tm_policy", "tm_weather")) {
    scen <- .vs_solved(tier)
    lv <- energyRt:::.vs_var(scen, "vStorageLevel")
    skip_if(!is.null(lv) && nrow(lv) > 0,
            paste(tier, "does use its store"))
    ck <- verify_solution(scen,
                          checks = "storage_dynamics")$checks$storage_dynamics
    expect_equal(ck$status, "skipped", label = paste0(tier, " unused store"))
    expect_match(ck$reason, "absent from the solution")
  }
})

test_that("storage_dynamics tests the fullYear closure, not just the interior", {
  skip_if_no_fixtures()
  skip_if_no_solver()
  scen <- .vs_solved("tm_core")
  k <- as.data.frame(energyRt:::.vs_par(scen, "meqStorageLevel"))
  # the chronology must CLOSE: every slice appears once as `timeslice` and
  # once as `timeslicep`, so the wrap row (last -> first) is checked by the
  # same identity as the rest. The fullYear bug lived exactly in that row and
  # was invisible to the goldens.
  g <- k[k$stg == k$stg[1] & k$region == k$region[1] & k$year == k$year[1], ]
  expect_setequal(g$timeslice, g$timeslicep)
  expect_equal(nrow(g), length(unique(g$timeslice)))
})

test_that("storage_dynamics catches a corrupted level", {
  skip_if_no_fixtures()
  skip_if_no_solver()
  bad <- .vs_corrupt(.vs_solved("tm_core"), "vStorageLevel", delta = 5)
  vs <- verify_solution(bad, checks = "storage_dynamics")
  expect_equal(vs$checks$storage_dynamics$status, "violated")
  expect_false(vs$ok)
})

test_that("the storage tolerance follows the series scale, not the row", {
  skip_if_no_fixtures()
  skip_if_no_solver()
  ck <- verify_solution(.vs_solved("tm_core"),
                        checks = "storage_dynamics")$checks$storage_dynamics
  # `max_rel` is degenerate for a level that empties -- |diff|/max(|lhs|,|rhs|)
  # is 1 whenever both sides are ~0 -- so `max_scaled`, which divides by the
  # magnitude of the storage SERIES, is the figure that says whether the check
  # is drifting. Measured across resolutions it stays ~1e-10 while max_abs
  # grows with the levels.
  expect_true("max_scaled" %in% names(verify_solution(
    .vs_solved("tm_core"), checks = "storage_dynamics")$divergence))
  if (!is.na(ck$stats$max_scaled)) {
    expect_lt(ck$stats$max_scaled, 1e-6)
  }
})

# =========================================================================== #
# Thread 3 stage 4 -- capacity_accumulation (B2), eac (B3), flow_chain (B5)
# =========================================================================== #

test_that("capacity_accumulation, eac and flow_chain hold on every tier", {
  skip_if_no_fixtures()
  skip_if_no_solver()
  for (tier in c("tm_core", "tm_flows", "tm_io", "tm_policy", "tm_weather")) {
    vs <- verify_solution(.vs_solved(tier),
      checks = c("capacity_accumulation", "eac", "flow_chain"))
    for (nm in names(vs$checks)) {
      expect_equal(vs$checks[[nm]]$status, "ok",
                   label = paste0(tier, " ", nm))
      expect_gt(vs$checks[[nm]]$n, 0L)      # and it compared something
    }
  }
})

test_that("capacity_accumulation catches a corrupted capacity", {
  skip_if_no_fixtures()
  skip_if_no_solver()
  bad <- .vs_corrupt(.vs_solved("tm_core"), "vTechCap", delta = 3)
  ck <- verify_solution(bad,
    checks = "capacity_accumulation")$checks$capacity_accumulation
  expect_equal(ck$status, "violated")
})

test_that("capacity_accumulation is sensitive to the periodLen factor", {
  skip_if_no_fixtures()
  skip_if_no_solver()
  # vTechNewCap is a RATE, so it enters multiplied by pPeriodLen. The defect
  # that dropped that factor under-charged capital by exactly periodLen; this
  # asserts the identity actually depends on it.
  scen <- .vs_solved("tm_core")
  expect_equal(verify_solution(scen,
    checks = "capacity_accumulation")$checks$capacity_accumulation$status, "ok")
  p <- scen@modInp@parameters[["pPeriodLen"]]
  d <- as.data.frame(energyRt:::get_data_slot(p))
  d$value <- d$value * 2
  p@data <- d
  scen@modInp@parameters[["pPeriodLen"]] <- p
  ck <- verify_solution(scen,
    checks = "capacity_accumulation")$checks$capacity_accumulation
  expect_equal(ck$status, "violated")
})

test_that("eac catches a charge that does not match its capacity", {
  skip_if_no_fixtures()
  skip_if_no_solver()
  bad <- .vs_corrupt(.vs_solved("tm_core"), "vTechEac", delta = 10)
  ck <- verify_solution(bad, checks = "eac")$checks$eac
  expect_equal(ck$status, "violated")
})

test_that("flow_chain catches output produced without the matching input", {
  skip_if_no_fixtures()
  skip_if_no_solver()
  bad <- .vs_corrupt(.vs_solved("tm_core"), "vTechOut", delta = 4)
  ck <- verify_solution(bad, checks = "flow_chain")$checks$flow_chain
  expect_equal(ck$status, "violated")
  expect_true("meqTechSng2Sng" %in% ck$violations$form)
})

test_that("no tm tier reaches the grouped-input branch (covered separately)", {
  skip_if_no_fixtures()
  skip_if_no_solver()
  # No tm tier declares an input group, so meqTechGrp2Sng / Sng2Grp / Grp2Grp
  # are empty on all of them. The co-firing path is covered instead by the
  # dedicated fixture below (`.vs_cofire_model()`), which is what exposed the
  # key collision in `.vs_group_expand()`. This test records WHY that separate
  # fixture exists, so it is not mistaken for redundant.
  scen <- .vs_solved("tm_flows")
  for (m in c("meqTechGrp2Sng", "meqTechSng2Grp", "meqTechGrp2Grp")) {
    d <- energyRt:::.vs_par(scen, m)
    expect_true(is.null(d) || nrow(d) == 0L,
                label = paste0(m, " unexpectedly populated"))
  }
})

test_that("the units report says which classes are missing units", {
  skip_if_no_fixtures()
  skip_if_no_solver()
  ck <- verify_solution(.vs_solved("tm_weather"),
                        checks = "units")$checks$units
  expect_equal(ck$status, "ok")            # report-only, never a violation
  cv <- ck$coverage
  expect_s3_class(cv, "data.frame")
  expect_true(all(c("class", "objects", "params", "declared", "unresolved",
                    "pct_open") %in% names(cv)))
  expect_equal(sum(cv$params), ck$n)
  expect_equal(sum(cv$unresolved), NROW(ck$report))
  expect_true(all(cv$declared + cv$unresolved == cv$params))
  # worst first, so the summary leads with where the gap is
  expect_false(is.unsorted(rev(cv$unresolved)))
})

test_that("no shipped fixture declares a unit, so D2-D4 have nothing to check", {
  skip_if_no_fixtures()
  # Recorded as a fact, not an assumption: unit CHAIN coherence (D2), the
  # cross-dimension property backing (D3) and result units (D4) are parked
  # because nothing declares a unit to check them against, and because
  # `.substitute_units()` performs string substitution rather than composition.
  # If a fixture starts declaring units this test flips and the stage can run.
  env <- .mapping_fixture_env()
  n <- 0L
  for (tier in c("tm_core", "tm_weather")) {
    for (o in getObjects(env[[tier]]())) {
      u <- tryCatch(methods::slot(o, "unit"), error = function(e) NULL)
      if (length(u) && any(!is.na(u) & nzchar(u))) n <- n + 1L
    }
  }
  expect_equal(n, 0L)
})

# =========================================================================== #
# Thread 3 -- purpose-built fixtures for the two branches the tm tiers leave
# uncovered: grouped inputs (co-firing) and a storage that actually cycles.
# =========================================================================== #

# A co-firing technology: two fuels in ONE input group, so the flow chain runs
# through eqTechGrp2Sng rather than eqTechSng2Sng. This is the shape in which a
# technology once produced output with zero fuel, because the group maps were
# never built.
.vs_cofire_model <- function(ginp2use = 2.5) {
  newModel("vs_cofire", repo = newRepository("vs_cf",
    newCommodity("ELC", timeframe = "SEASON"),
    newCommodity("COAL", timeframe = "ANNUAL"),
    newCommodity("BIOM", timeframe = "ANNUAL"),
    newSupply("SCOAL", commodity = "COAL",
              supply = data.frame(ava.up = 1e4, cost = 2)),
    newSupply("SBIOM", commodity = "BIOM",
              supply = data.frame(ava.up = 1e4, cost = 5)),
    newTechnology("PP",
      input  = data.frame(comm = c("COAL", "BIOM"), group = "FUEL",
                          stringsAsFactors = FALSE),
      output = list(comm = "ELC"),
      group  = data.frame(group = "FUEL", desc = "fuel",
                          stringsAsFactors = FALSE),
      geff   = data.frame(group = "FUEL", ginp2use = ginp2use,
                          stringsAsFactors = FALSE),
      invcost = list(invcost = 100), olife = list(olife = 30), cap2act = 1),
    newDemand("DEM", commodity = "ELC",
              demand = data.frame(demand = 10))),
    calendar = calendars$s4, region = "R1",
    horizon = newHorizon(2025), discount = 0)
}

.vs_solve_model <- function(mod, nm) {
  scen <- suppressMessages(suppressWarnings(
    interpolate_model(mod, name = nm, ondisk = FALSE, overwrite = TRUE)))
  .fork_solve(scen, solver_options$glpk)
}

test_that("flow_chain runs through the GROUPED-input branch on a co-firing tech", {
  skip_if_no_solver()
  sol <- .vs_solve_model(.vs_cofire_model(), "vs_cf")
  skip_if(is.na(.fork_objective(sol)), "co-firing fixture did not solve")

  # the branch the tm tiers cannot reach
  g <- energyRt:::.vs_par(sol, "meqTechGrp2Sng")
  expect_true(!is.null(g) && nrow(g) > 0L)

  ck <- verify_solution(sol, checks = "flow_chain")$checks$flow_chain
  expect_equal(ck$status, "ok")
  expect_gt(ck$n, 0L)
})

test_that("flow_chain catches grouped output produced without its fuel", {
  skip_if_no_solver()
  sol <- .vs_solve_model(.vs_cofire_model(), "vs_cf2")
  skip_if(is.na(.fork_objective(sol)), "co-firing fixture did not solve")
  expect_equal(verify_solution(sol,
    checks = "flow_chain")$checks$flow_chain$status, "ok")

  # remove fuel from one group member: the group sum no longer supports the
  # output, which is exactly the co-firing failure mode
  bad <- .vs_corrupt(sol, "vTechInp", delta = -5)
  ck <- verify_solution(bad, checks = "flow_chain")$checks$flow_chain
  expect_equal(ck$status, "violated")
  expect_true("meqTechGrp2Sng" %in% ck$violations$form)
})

test_that("flow_chain is sensitive to the group efficiency itself", {
  skip_if_no_solver()
  sol <- .vs_solve_model(.vs_cofire_model(), "vs_cf3")
  skip_if(is.na(.fork_objective(sol)), "co-firing fixture did not solve")
  # pTechGinp2use multiplies the whole group sum; if the check ignored it the
  # identity would still close, so perturb it and require a violation
  p <- sol@modInp@parameters[["pTechGinp2use"]]
  skip_if(is.null(p), "pTechGinp2use absent")
  d <- as.data.frame(energyRt:::get_data_slot(p, optional = TRUE))
  skip_if(is.null(d) || nrow(d) == 0L, "pTechGinp2use carries no rows")
  d$value <- d$value * 1.5
  p@data <- d
  sol@modInp@parameters[["pTechGinp2use"]] <- p
  ck <- verify_solution(sol, checks = "flow_chain")$checks$flow_chain
  expect_equal(ck$status, "violated")
})

# --------------------------------------------------------------------------- #
# a storage that actually cycles, so storage_dynamics is exercised on
# non-trivial levels rather than skipping or closing 0 = 0
# --------------------------------------------------------------------------- #
.vs_storage_model <- function(stgeff = 0.98) {
  newModel("vs_stg", repo = newRepository("vs_st",
    newCommodity("ELC", timeframe = "SEASON"),
    newCommodity("GAS", timeframe = "ANNUAL"),
    newSupply("SGAS", commodity = "GAS", supply = data.frame(cost = 6)),
    newTechnology("EGAS",
      input = list(comm = "GAS"), output = list(comm = "ELC"),
      ceff = data.frame(comm = "GAS", cinp2use = 0.5),
      invcost = list(invcost = 900), fixom = 25, cap2act = 31.536,
      olife = 30L),
    newStorage("STG_ELC", commodity = "ELC", olife = 30L,
      invcost = data.frame(stg.invcost = 20, out.invcost = 50,
                           inp.invcost = 50),
      seff = data.frame(stgeff = stgeff, inpeff = 0.95, outeff = 0.95)),
    newDemand("DEM_ELC", commodity = "ELC",
              demand = data.frame(demand = 100))),
    calendar = calendars$s4, region = "R1", discount = 0.05,
    horizon = newHorizon(period = 2020:2025, intervals = 1))
}

test_that("storage_dynamics runs on a store that actually holds energy", {
  skip_if_no_solver()
  sol <- .vs_solve_model(.vs_storage_model(), "vs_st")
  skip_if(is.na(.fork_objective(sol)), "storage fixture did not solve")
  lv <- energyRt:::.vs_var(sol, "vStorageLevel")
  expect_true(!is.null(lv) && nrow(lv) > 0L)
  expect_gt(max(abs(as.numeric(lv$value))), 0)   # not a 0 = 0 identity

  ck <- verify_solution(sol,
    checks = "storage_dynamics")$checks$storage_dynamics
  expect_equal(ck$status, "ok")
  expect_gt(ck$n, 0L)
  # the residual must be small RELATIVE TO THE SERIES, the criterion the
  # tolerance actually uses
  expect_lt(ck$stats$max_scaled, 1e-6)
})

test_that("storage_dynamics catches a broken carry-over efficiency", {
  skip_if_no_solver()
  sol <- .vs_solve_model(.vs_storage_model(), "vs_st2")
  skip_if(is.na(.fork_objective(sol)), "storage fixture did not solve")
  expect_equal(verify_solution(sol,
    checks = "storage_dynamics")$checks$storage_dynamics$status, "ok")

  # stgeff enters as stgeff^share on the PREVIOUS level; perturbing it must
  # break the identity, otherwise the check is not reading the carry term
  p <- sol@modInp@parameters[["pStorageStgEff"]]
  skip_if(is.null(p), "pStorageStgEff absent")
  d <- as.data.frame(energyRt:::get_data_slot(p, optional = TRUE))
  skip_if(is.null(d) || nrow(d) == 0L, "pStorageStgEff is all default")
  d$value <- d$value * 0.5
  p@data <- d
  sol@modInp@parameters[["pStorageStgEff"]] <- p
  ck <- verify_solution(sol,
    checks = "storage_dynamics")$checks$storage_dynamics
  expect_equal(ck$status, "violated")
})

test_that("storage_dynamics closes the cycle, not just the interior", {
  skip_if_no_solver()
  sol <- .vs_solve_model(.vs_storage_model(), "vs_st3")
  skip_if(is.na(.fork_objective(sol)), "storage fixture did not solve")
  k <- as.data.frame(energyRt:::.vs_par(sol, "meqStorageLevel"))
  g <- k[k$stg == k$stg[1] & k$region == k$region[1] & k$year == k$year[1], ]
  # every slice appears once on each side: the chronology is a closed cycle,
  # so the wrap row is checked by the same identity as the interior
  expect_setequal(g$timeslice, g$timeslicep)

  # corrupting the level in the LAST slice must break the wrap row too
  lv <- energyRt:::.vs_var(sol, "vStorageLevel")
  last <- utils::tail(sort(unique(as.character(lv$timeslice))), 1)
  v <- sol@modOut@variables[["vStorageLevel"]]
  d2 <- as.data.frame(energyRt:::get_data_slot(v))
  i <- which(as.character(d2$timeslice) == last)[1]
  skip_if(is.na(i), "no level row in the last slice")
  d2$value[i] <- d2$value[i] + 7
  v@data <- d2
  sol@modOut@variables[["vStorageLevel"]] <- v
  ck <- verify_solution(sol,
    checks = "storage_dynamics")$checks$storage_dynamics
  expect_equal(ck$status, "violated")
  # two rows break: the one where that slice IS the level, and the one where
  # it is the PREVIOUS level -- the second is the cycle closing
  expect_gte(nrow(ck$violations), 2L)
})
