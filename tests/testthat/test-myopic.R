# Sequential (myopic) solving: windows, the carry ledger, and the driver
# (R/carry.R + R/solve_myopic.R). Fixtures from helper-myopic.R.

test_that("horizon_windows slices intervals exactly, with look-ahead", {
  h <- newHorizon(2020:2025, intervals = rep(1L, 6))
  w <- horizon_windows(h, step = 2L)
  expect_length(w, 3L)
  expect_identical(w[[1]]$decided, c(2020L, 2021L))
  expect_identical(w[[3]]$decided, c(2024L, 2025L))
  expect_equal(as.integer(w[[2]]$horizon@intervals$mid), c(2022L, 2023L))

  w2 <- horizon_windows(h, step = 2L, overlap = 1L)
  expect_identical(w2[[1]]$decided, c(2020L, 2021L))
  expect_identical(w2[[1]]$lookahead, 2022L)
  expect_equal(as.integer(w2[[1]]$horizon@intervals$mid), 2020:2022)
  expect_identical(w2[[3]]$lookahead, integer(0)) # nothing beyond the end

  # multi-year intervals keep their exact rows
  h2 <- newHorizon(2020:2030, intervals = c(1L, 5L, 5L))
  w3 <- horizon_windows(h2, step = 1L)
  expect_length(w3, 3L)
  expect_identical(w3[[2]]$horizon@intervals$start,
                   h2@intervals$start[2])
  expect_identical(w3[[2]]$horizon@intervals$end, h2@intervals$end[2])
})

test_that("myopic reproduces the foresight capacity path (no intertemporal coupling)", {
  skip_if_no_solver()
  yrs <- 2020:2022
  old_sp <- set_scenarios_path(my_scen_path("fe"))
  on.exit(set_scenarios_path(old_sp), add = TRUE)

  mod <- my_mod(years = yrs, olife = 50L, demand = 1, name = "fe")
  fore <- solve_scenario(interpolate_model(mod, name = "fe-fs"), echo = FALSE)
  cap_fore <- my_by_year(fore, "vTechCap", yrs)

  res <- solve_myopic(mod, name = "fe", verbose = FALSE)
  expect_s3_class(res, "myopic")
  expect_identical(nrow(res$steps), 3L)
  expect_true(all(res$steps$status == "solved"))
  cap_myo <- my_by_year(res, "vTechCap", yrs)
  expect_equal(cap_myo, cap_fore)

  # capacity built in step 1 arrives in later steps as carried stock
  stock_myo <- my_by_year(res, "vTechStockCap", yrs)
  expect_equal(unname(stock_myo[1]), 0)
  expect_equal(unname(stock_myo[2]), unname(cap_fore[1]))
  new_myo <- my_by_year(res, "vTechNewCap", yrs)
  expect_equal(unname(new_myo[2]), 0) # nothing re-built: the carry covers it
})

test_that("carried capacity ages out at olife and is replaced", {
  skip_if_no_solver()
  yrs <- 2020:2023
  old_sp <- set_scenarios_path(my_scen_path("ol"))
  on.exit(set_scenarios_path(old_sp), add = TRUE)

  res <- solve_myopic(my_mod(years = yrs, olife = 2L, name = "ol"),
                      name = "ol", verbose = FALSE)
  expect_true(all(res$steps$status == "solved"))
  stock <- my_by_year(res, "vTechStockCap", yrs)
  new <- my_by_year(res, "vTechNewCap", yrs)
  # 2020 build survives as stock through 2021 (2021-2020 < 2) and is gone in
  # 2022 (2022-2020 >= 2), forcing a replacement build there
  expect_gt(new[["2020"]], 0)
  expect_equal(unname(stock[2]), unname(new[["2020"]]))
  expect_gt(new[["2022"]], 0)
  # 2021 needs nothing new (2020's build still alive), 2023 is covered by
  # 2022's replacement
  expect_equal(unname(new[c("2021", "2023")]), c(0, 0))
})

test_that("a cumulative supply reserve is debited across steps", {
  skip_if_no_solver()
  yrs <- 2020:2022
  old_sp <- set_scenarios_path(my_scen_path("bd"))
  on.exit(set_scenarios_path(old_sp), add = TRUE)

  # CHEAP supply covers demand 4/yr but holds only 8 in total: without the
  # debit each myopic step would take 4 forever; with it the reserve runs
  # out after two step-years and TECH (or BACK) must take over
  mod <- my_mod(years = yrs, olife = 50L, demand = 1, name = "bd",
                sup_res = 8)
  res <- solve_myopic(mod, name = "bd", verbose = FALSE)
  expect_true(all(res$steps$status == "solved"))
  sup <- my_by_year(res, "vSupOut", yrs) # CHEAP + BACK, but BACK is costly
  d <- getData(res, "vSupOut", merge = TRUE)
  cheap <- d[d$sup == "CHEAP", , drop = FALSE]
  cheap_by_year <- stats::setNames(rep(0, 3), as.character(yrs))
  agg <- tapply(cheap$value, as.character(cheap$year), sum)
  cheap_by_year[names(agg)] <- as.numeric(agg)
  expect_equal(unname(cheap_by_year[c("2020", "2021")]), c(4, 4))
  expect_equal(unname(cheap_by_year[["2022"]]), 0) # reserve exhausted
  # total taken over the sequence never exceeds the reserve
  expect_lte(sum(cheap_by_year), 8 + 1e-6)
})

test_that("store = 'variants': one scenario, one variant per step", {
  skip_if_no_solver()
  yrs <- 2020:2022
  old_sp <- set_scenarios_path(my_scen_path("vs"))
  on.exit(set_scenarios_path(old_sp), add = TRUE)

  res <- solve_myopic(my_mod(years = yrs, name = "vs"),
                      name = "vs", verbose = FALSE)
  p <- res$path
  expect_true(dir.exists(p))
  runs <- scenario_runs(res$scenarios[[3]])
  expect_identical(sort(unique(runs$variant)),
                   c("s01-2020", "s02-2021", "s03-2022"))
  expect_true(all(runs$modinp == "own"))
  # variant.yml carries the driver metadata
  vy <- yaml::read_yaml(file.path(p, "runs", "s02-2021", "variant.yml"))
  expect_identical(vy$type, "myopic_step")
  expect_identical(vy$sequence, "vs")
  expect_identical(vy$step, 2L)
  expect_identical(vy$decided, 2021L)
  # each step's problem is switchable after loading
  back <- suppressMessages(load_scenario(p, env = NULL, verbose = FALSE))
  runs3 <- scenario_runs(back)
  expect_identical(nrow(runs3[runs3$modinp == "own", ]), 3L)
})

test_that("stitching: each year exactly once, incl. with overlap", {
  skip_if_no_solver()
  yrs <- 2020:2022
  old_sp <- set_scenarios_path(my_scen_path("st"))
  on.exit(set_scenarios_path(old_sp), add = TRUE)

  res <- solve_myopic(my_mod(years = yrs, name = "st"),
                      name = "st", overlap = 1L, verbose = FALSE)
  d <- getData(res, "vTechCap", merge = TRUE)
  expect_identical(sort(unique(d$year)), yrs)
  expect_false(any(duplicated(d[, c("tech", "region", "year")])))
  # a decided year comes from the step that decided it, not from look-ahead
  expect_identical(unique(d$step[d$year == 2021]), 2L)
  # vObjective is excluded from stitching with a pointer to myopic_objective
  expect_message(getData(res, "vObjective", merge = TRUE), "myopic_objective")
  # the recomputed global objective is a finite scalar
  expect_true(is.finite(myopic_objective(res)))
})

test_that("an infeasible step surfaces cleanly; on_error='return' keeps the partial", {
  skip_if_no_solver()
  yrs <- 2020:2022
  old_sp <- set_scenarios_path(my_scen_path("inf"))
  on.exit(set_scenarios_path(old_sp), add = TRUE)

  # demand jumps but capacity is capped below it AND the backstop is the
  # rescue -- to force infeasibility instead, cap TECH below demand and
  # remove the backstop by making demand > cap with no alternative:
  # simplest: cap.up = 0.1 per slice-hour < demand rate 1, and drop BACK by
  # pricing it... BACK keeps it feasible, so instead make step 2 infeasible
  # via an impossible capacity bound below the carried stock with
  # retirement not optimized
  mod <- my_mod(years = yrs, olife = 50L, demand = 1, name = "inf",
                cap_up = c(10, 0.001, 10))
  # step 1 builds ~1 unit; step 2's model then has carried stock ~1 but a
  # cap.up of 0.001 -> eqTechCapUp is violated by the exogenous stock
  expect_error(
    suppressWarnings(solve_myopic(mod, name = "inf", verbose = FALSE)),
    "did not solve")
  res <- suppressWarnings(
    solve_myopic(mod, name = "inf2", verbose = FALSE, on_error = "return"))
  expect_s3_class(res, "myopic")
  expect_true(res$failed)
  expect_identical(res$steps$status, c("solved", "failed"))
})

test_that("intertemporal user constraints trigger the one loud warning", {
  yrs <- 2020:2022
  mod <- my_mod(years = yrs, name = "wc")
  # a cumulative cap: no `year` in for.each, so the year dim is summed —
  # exactly what a windowed solve cannot honor horizon-wide
  CUM <- newConstraint(
    name = "CUMNEW", eq = "<=",
    term1 = list(variable = "vTechNewCap"),
    defVal = 100)
  mod <- add(mod, CUM, overwrite = TRUE)
  expect_warning(horizon_windows(mod@config@horizon) |> invisible(),
                 NA) # windows alone don't warn
  expect_warning(
    energyRt:::.myopic_warn_intertemporal(mod),
    "WITHIN its own")
})

test_that("storage capacity carries across steps (three parts, no re-build)", {
  skip_if_no_solver()
  yrs <- 2020:2022
  old_sp <- set_scenarios_path(my_scen_path("sg"))
  on.exit(set_scenarios_path(old_sp), add = TRUE)

  mod <- my_stg_mod(years = yrs, name = "sg")
  fore <- solve_scenario(interpolate_model(mod, name = "sg-fs"), echo = FALSE)
  out_fore <- my_by_year(fore, "vStorageOutCap", yrs)
  expect_gt(out_fore[["2020"]], 0) # the fixture really builds storage

  res <- solve_myopic(mod, name = "sg", verbose = FALSE)
  expect_true(all(res$steps$status == "solved"))
  expect_equal(my_by_year(res, "vStorageOutCap", yrs), out_fore)
  expect_equal(my_by_year(res, "vStorageStgCap", yrs),
               my_by_year(fore, "vStorageStgCap", yrs))
  # step 2 sees the step-1 build as exogenous stock and re-builds nothing
  stock2 <- my_by_year(res, "vStorageOutStockCap", yrs)
  expect_equal(unname(stock2[2]), unname(out_fore[1]))
  new_myo <- my_by_year(res, "vStorageOutNewCap", yrs)
  expect_equal(unname(new_myo[2:3]), c(0, 0))
})

test_that("trade capacity carries across steps (region-free family)", {
  skip_if_no_solver()
  yrs <- 2020:2022
  old_sp <- set_scenarios_path(my_scen_path("tr"))
  on.exit(set_scenarios_path(old_sp), add = TRUE)

  mod <- my_trd_mod(years = yrs, name = "tr")
  fore <- solve_scenario(interpolate_model(mod, name = "tr-fs"), echo = FALSE)
  cap_fore <- my_by_year(fore, "vTradeCap", yrs)
  expect_gt(cap_fore[["2020"]], 0) # the fixture really builds the link

  res <- solve_myopic(mod, name = "tr", verbose = FALSE)
  expect_true(all(res$steps$status == "solved"))
  expect_equal(my_by_year(res, "vTradeCap", yrs), cap_fore)
  stock2 <- my_by_year(res, "vTradeStockCap", yrs)
  expect_equal(unname(stock2[2]), unname(cap_fore[1]))
  expect_equal(unname(my_by_year(res, "vTradeNewCap", yrs)[2:3]), c(0, 0))
})

test_that("a vintaged build is carried onto the RIGHT variant (provenance)", {
  skip_if_no_solver()
  yrs <- 2020:2022
  old_sp <- set_scenarios_path(my_scen_path("vn"))
  on.exit(set_scenarios_path(old_sp), add = TRUE)

  mod <- my_vin_mod(years = yrs, name = "vn")
  res <- solve_myopic(mod, name = "vn", verbose = FALSE)
  expect_true(all(res$steps$status == "solved"))
  # capacity path continuous: the early build persists as stock
  cap <- my_by_year(res, "vTechCap", yrs)
  expect_true(all(cap > 0))
  expect_equal(unname(my_by_year(res, "vTechNewCap", yrs)[2:3]), c(0, 0))

  # the ledger routed the carry through provenance: applying it to the
  # pristine model writes the stock rows WITH the vintage selector
  w <- horizon_windows(mod@config@horizon)[[2]]
  mod2 <- apply_ledger(mod, res$ledger, w$horizon)
  tech2 <- energyRt:::.model_find_object(mod2, "TECH", "technology")$obj
  cap_df <- tech2@capacity
  expect_true("vintage" %in% names(cap_df))
  st_rows <- cap_df[!is.na(cap_df$stock) & cap_df$stock > 0, , drop = FALSE]
  expect_true(nrow(st_rows) > 0)
  expect_true(all(st_rows$vintage == "early"))
})
