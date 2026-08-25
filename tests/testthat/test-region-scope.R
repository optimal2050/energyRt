# A region-restricted process must stay in its region -- on every back-end.
#
# `eqSupOutTot` summed `vSupOut` over supplies gated by COMMODITY alone:
#
#     sum{s1 in sup:((s1,c) in mSupComm)}(vSupOut[s1,c,r,y,s])
#
# `vSupOut` lives on `mSupAva`, which carries the region, so the sum reached a
# region-restricted supply outside its own domain. The two back-end families
# failed in opposite ways, and the noisy one was the lucky one:
#
#   * GLPK and GAMS declare variables over the full index cube, so
#     `vSupOut[SUP, ELC, R2, ...]` was a legal variable with no availability
#     limit and no cost equation -- free, unlimited supply in every region the
#     process was excluded from. A SILENT wrong answer.
#   * Julia and Pyomo declare variables over their maps, so the same reference
#     was a hard KeyError and the model would not build at all.
#
# The fix gates the sum on `mSupAva` as well. These tests pin the behaviour that
# matters -- confinement -- rather than merely that the model builds.

rs_model <- function(sup_region = "R1") {
  cal <- newCalendar(timetable = make_timetable(struct = list(ANNUAL = "ANNUAL")),
                     name = "rs_cal")
  SUP <- if (is.null(sup_region)) {
    newSupply("SUP", commodity = "ELC", supply = data.frame(cost = 1))
  } else {
    newSupply("SUP", commodity = "ELC", region = sup_region,
              supply = data.frame(cost = 1))
  }
  newModel(
    name = "rs", region = c("R1", "R2"),
    horizon = newHorizon(2020:2021, intervals = c(1, 1)),
    discount = 0, calendar = cal,
    repo = newRepository("rs_repo",
      newCommodity("ELC", timeframe = "ANNUAL"),
      SUP,
      # a costly fallback, so exclusion is expensive rather than infeasible
      newSupply("BACK", commodity = "ELC", supply = data.frame(cost = 1000)),
      newDemand("DEM", commodity = "ELC", demand = data.frame(demand = 1))))
}

test_that("a region-restricted supply does not serve the region it excludes", {
  skip_if_no_solver()
  sol <- solve_scen(interpolate_model(rs_model("R1"), name = "rs_glpk"))

  out <- as.data.frame(getData(sol, "vSupOut", merge = TRUE))
  out <- out[out$value != 0, ]
  # SUP appears only in R1; R2's demand is met by the expensive fallback.
  expect_setequal(unique(out$region[out$sup == "SUP"]), "R1")
  expect_setequal(unique(out$region[out$sup == "BACK"]), "R2")

  # 2 years x (1 unit at 1 in R1 + 1 unit at 1000 in R2).
  expect_equal(getData(sol, "vObjective", merge = TRUE)$value[1], 2002)
})

test_that("lifting the restriction is what makes the cheap supply available", {
  skip_if_no_solver()
  # The companion case: same model, no `region =`. If the restricted variant
  # above ever drops below this, the supply is leaking again -- which is exactly
  # how the defect read (it solved to 2, cheaper than either correct answer).
  sol <- solve_scen(interpolate_model(rs_model(NULL), name = "rs_open"))
  expect_equal(getData(sol, "vObjective", merge = TRUE)$value[1], 4)
})

test_that("every back-end agrees on a region-restricted supply", {
  skip_if_no_solver()
  # Julia and Pyomo could not build this model at all before the fix.
  sc  <- interpolate_model(rs_model("R1"), name = "rs_multi")
  ref <- getData(solve_scen(sc), "vObjective", merge = TRUE)$value[1]
  have <- function(f) { p <- tryCatch(f(), error = function(e) NULL)
                        !is.null(p) && nzchar(p) }
  engines <- c(if (have(get_julia_path))  "julia_highs",
               if (have(get_python_path)) "pyomo_glpk")
  for (eng in engines) {
    sol <- solve_scen(sc, solver = solver_options[[eng]])
    expect_equal(getData(sol, "vObjective", merge = TRUE)$value[1], ref,
                 tolerance = 1e-6, info = eng)
  }
})

test_that("the region gate is present in every back-end template", {
  # Structural guard, checked as text so it needs no solver. Both fixes are a
  # membership test added to an inner sum; a template edit that drops one would
  # not produce a wrong number on GLPK, it would produce a silently cheaper one.
  code <- getFromNamespace(".modelCode", "energyRt")
  engines <- c("GLPK", "GAMS", "JuMP", "PYOMOConcrete")
  expect_true(all(engines %in% names(code)))

  for (eng in engines) {
    body <- paste(code[[eng]], collapse = "
")

    # 1. eqSupOutTot must gate on mSupAva, which carries the region.
    expect_true(grepl("mSupAva", body, fixed = TRUE),
                info = paste(eng, "does not mention mSupAva"))

    # 2. Every vTradeIr sum in the trade-aux equations must gate on mvTradeIr.
    #    Count them: 4 inner sums (2 per equation, AInp and AOut).
    expect_gte(lengths(regmatches(body, gregexpr("mvTradeIr", body, fixed = TRUE))),
               4, label = paste(eng, "mvTradeIr references"))
  }
})

# ---------------------------------------------------------------------------- #
# A region-scoped DEMAND with a wildcard (NA-region) data row.
#
# `ob2mi("demand")` filtered its rows with a bare `region %in% dem_regions`;
# `NA %in% "R2"` is FALSE, so the wildcard row was DELETED and
# `update_parameter()`'s empty-frame early exit made the loss silent — the
# model built and solved with no demand at all. Demand was the only class with
# this filter: supply/technology resolve the wildcard through their span maps,
# export/import expand it eagerly with `.expand_na_region()`. The fix expands
# demand's wildcard onto the object's scope the same eager way (a surviving NA
# must NOT reach unfold: pDemand has no `dem` membership key, so it would
# broadcast through the COMMODITY map to every region — a leak, not a fix).

rs_dem_rows <- function(dem_obj) {
  cal <- newCalendar(timetable = make_timetable(struct = list(ANNUAL = "ANNUAL")),
                     name = "rsd_cal")
  mod <- newModel(
    name = "rsd", region = c("R1", "R2"),
    horizon = newHorizon(2020:2021, intervals = c(1, 1)),
    discount = 0, calendar = cal,
    repo = newRepository("rsd_repo",
      newCommodity("ELC", timeframe = "ANNUAL"),
      newSupply("SUP", commodity = "ELC",
                supply = data.frame(region = c("R1", "R2"), cost = c(1, 2))),
      dem_obj))
  sc <- suppressWarnings(suppressMessages(
    interpolate_model(mod, name = "rsd")))
  d <- get_data_slot(sc@modInp@parameters[["pDemand"]])
  as.data.frame(d)
}

test_that("a scoped demand's wildcard row expands to the scope, not to nothing", {
  # THE bug: scope + NA-region row used to interpolate to ZERO pDemand rows
  d <- rs_dem_rows(newDemand("DEM", commodity = "ELC", region = "R2",
                             demand = data.frame(demand = 1)))
  expect_gt(nrow(d), 0)
  expect_setequal(unique(as.character(d$region)), "R2")
  expect_true(all(d$value == 1))

  # controls: explicit column, and scope + column, give the identical rows
  d_col <- rs_dem_rows(newDemand("DEM", commodity = "ELC",
                                 demand = data.frame(region = "R2", demand = 1)))
  d_both <- rs_dem_rows(newDemand("DEM", commodity = "ELC", region = "R2",
                                  demand = data.frame(region = "R2", demand = 1)))
  key <- c("region", "year", "value")
  expect_equal(d[key], d_col[key])
  expect_equal(d[key], d_both[key])
})

test_that("an unscoped wildcard demand row still broadcasts to all model regions", {
  d <- rs_dem_rows(newDemand("DEM", commodity = "ELC",
                             demand = data.frame(demand = 1)))
  expect_setequal(unique(as.character(d$region)), c("R1", "R2"))
})

test_that("a multi-region scope expands the wildcard to every scope region", {
  d <- rs_dem_rows(newDemand("DEM", commodity = "ELC", region = c("R1", "R2"),
                             demand = data.frame(demand = 1)))
  expect_setequal(unique(as.character(d$region)), c("R1", "R2"))
})

test_that("an explicit row OUTSIDE the scope errors loudly upstream", {
  # `get_process_region()` (the region-scope validation) rejects a slot row
  # naming a region outside `@region` before ob2mi ever runs — a loud error,
  # which is the better contract than the old silent filter.
  expect_error(
    rs_dem_rows(newDemand("DEM", commodity = "ELC", region = "R2",
                          demand = data.frame(region = c("R1", "R2"),
                                              demand = c(7, 1)))),
    "not in its @region scope")
})
