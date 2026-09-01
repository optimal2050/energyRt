# The boundary trade WINDOW and the region-by-region driver
# (R/sample_region.R additions + R/solve_by_region.R).
#
# The properties that matter and could break silently:
#   * the window's quantity is sized from demand BEFORE interpolation, so the
#     estimate must equal what the solver ends up seeing;
#   * a curve stub must fall for exports / rise for imports and its steps must
#     sum back to the flat bound;
#   * with no trade, per-region objectives ADD UP to the full model's -- and
#     with a window they do not, which the result must say.
#
# NOTE on the fixtures: they use ONE supply object carrying per-region rows,
# never one supply object per region. Two or more region-scoped supply objects
# in a multi-region model make `vSupCost` come back EMPTY and the objective
# zero -- a live defect unrelated to this feature, recorded separately.

rw_path <- function(name) {
  gsub("[\\/]+", "/", file.path(tempdir(), "region-window", name))
}

rw_local <- function(name, env = parent.frame()) {
  unlink(rw_path(name), recursive = TRUE)
  old <- set_scenarios_path(rw_path(name))
  do.call(on.exit, list(bquote(set_scenarios_path(.(old))), add = TRUE),
          envir = env)
  invisible(NULL)
}

rw_cal <- function() calendars$s4_h24
rw_leaf <- function() {
  cal <- rw_cal()
  cal@timeframes[[length(cal@timeframes)]]
}

# three regions, self-sufficient, ascending supply cost; optionally one route
rw_mod <- function(regs = c("R1", "R2", "R3"), route = FALSE, demand = 2,
                   name = "rw") {
  leaf <- rw_leaf()
  ELC <- newCommodity("ELC", timeframe = "HOUR")
  objs <- list(ELC, newSupply("SUP", commodity = "ELC",
    supply = data.frame(region = regs,
                        cost = 10 + 5 * seq_along(regs))))
  for (r in regs) {
    objs <- c(objs, list(newDemand(paste0("DEM_", r), commodity = "ELC",
      demand = data.frame(region = r, year = 2020L, timeslice = leaf,
                          demand = demand))))
  }
  if (route) {
    objs <- c(objs, list(newTrade("TR23", commodity = "ELC",
      routes = data.frame(src = "R2", dst = "R3"),
      trade = data.frame(src = "R2", dst = "R3", teff = 1, ava.up = 4))))
  }
  newModel(name, region = regs, discount = 0, calendar = rw_cal(),
           horizon = newHorizon(2020),
           repo = do.call(newRepository, c(list("rwr"), objs)))
}

# --- the share rule ---------------------------------------------------------- #

test_that("the pre-interpolation demand estimate is what the solver sees", {
  # The window sizes its bound from demand BEFORE interpolation. If that
  # shortcut diverged from the interpolated quantity the whole window would be
  # sized wrong, silently.
  rw_local("dem")
  mod <- rw_mod(demand = 2)
  est <- .region_annual_demand(mod)
  expect_setequal(est$region, c("R1", "R2", "R3"))
  expect_equal(sort(est$demand), rep(2 * 96, 3))   # 96 leaf slices

  scen <- suppressMessages(suppressWarnings(
    interpolate_model(mod, name = "rw_dem", overwrite = TRUE)))
  pd <- as.data.frame(getData(scen, "pDemand", merge = TRUE))
  tw <- as.data.frame(getData(scen, "pTimesliceWeight", merge = TRUE))
  m <- merge(pd, tw[, c("year", "timeslice", "value")],
             by = c("year", "timeslice"), suffixes = c("", ".w"))
  act <- stats::aggregate(list(demand = m$value * m$value.w),
                          by = list(region = as.character(m$region),
                                    year = as.integer(m$year)), FUN = sum)
  cmp <- merge(est, act, by = c("region", "year"))
  expect_equal(cmp$demand.x, cmp$demand.y)
})

test_that(".region_demand_profile broadcasts an NA region across the sample", {
  leaf <- rw_leaf()
  ELC <- newCommodity("ELC", timeframe = "HOUR")
  D <- newDemand("D", commodity = "ELC",
                 demand = data.frame(year = 2020L, timeslice = leaf,
                                     demand = 1))   # no region column
  mod <- newModel("wc", region = c("R1", "R2"), discount = 0,
                  calendar = rw_cal(), horizon = newHorizon(2020),
                  repo = newRepository("wcr", ELC, D))
  prof <- .region_demand_profile(mod)
  expect_setequal(unique(prof$region), c("R1", "R2"))
  expect_identical(nrow(prof), 192L)              # 96 slices x 2 regions
})

# --- the window -------------------------------------------------------------- #

test_that("a demand with no year or timeslice is not dropped", {
  # REGRESSION: the profile used stats::aggregate(), which silently drops every
  # row whose grouping variable is NA -- and `year`/`timeslice` are legitimately
  # NA on a demand declared for the whole horizon and every slice, which is the
  # most common shape. The window then sized itself from nothing and produced
  # no rows at all.
  ELC <- newCommodity("ELC", timeframe = "ANNUAL")
  D <- newDemand("D", commodity = "ELC",
                 demand = data.frame(region = c("R1", "R2"),
                                     demand = c(10, 20)))
  TR <- newTrade("TR", commodity = "ELC",
                 routes = data.frame(src = "R2", dst = "R3"),
                 trade = data.frame(src = "R2", dst = "R3", ava.up = 4))
  mod <- newModel("na", region = c("R1", "R2", "R3"), discount = 0,
                  horizon = newHorizon(2020),
                  repo = newRepository("nar", ELC, D, TR))

  prof <- .region_demand_profile(mod, c("R1", "R2"))
  expect_identical(nrow(prof), 2L)
  expect_true(all(is.na(prof$year)), label = "NA year survives")
  expect_true(all(is.na(prof$timeslice)), label = "NA timeslice survives")

  bw <- boundary_window(mod, regions = c("R1", "R2"), share = 0.1,
                        nsteps = 3, price = 40)
  expect_identical(nrow(bw), 1L)
  expect_equal(bw$cap.up, 0.1 * 20)     # sized from R2's own demand
})

test_that("boundary_window sizes the bound at `share` of demand", {
  mod <- rw_mod(route = TRUE, demand = 2)
  bw <- boundary_window(mod, regions = c("R1", "R2"), share = 0.1,
                        nsteps = 3, price = 40)
  expect_true(all(c("src", "dst", "price", "cap.up", "nsteps",
                    "price_lo", "price_hi") %in% names(bw)))
  expect_equal(unique(bw$cap.up), 0.2)            # 10% of 2, per timeslice
  expect_identical(unique(bw$nsteps), 3L)
  # R2 is the kept endpoint of R2->R3, so the window is an EXPORT: falling
  expect_lt(unique(bw$price_lo), unique(bw$price_hi))

  # summing the per-timeslice bound over the year is `share` of annual demand
  ann <- .region_annual_demand(mod, "R2")$demand
  expect_equal(sum(bw$cap.up), 0.1 * ann)
})

test_that("boundary_window refuses to invent a price", {
  mod <- rw_mod(route = TRUE)
  expect_error(boundary_window(mod, c("R1", "R2")), "cannot invent a price")
  expect_error(boundary_window(mod, c("R1", "R2"), price = 40, share = 0),
               "must be one number")
})

test_that("a windowed stub is a stepped curve; a flat one is unchanged", {
  mod <- rw_mod(route = TRUE)
  bw <- boundary_window(mod, regions = c("R1", "R2"), share = 0.1,
                        nsteps = 3, price = 40, spread = 0.5)
  m <- subset_model_regions(mod, c("R1", "R2"), boundary_prices = bw,
                            verbose = FALSE)
  st <- m@data[["boundary_stubs"]]@data[[1]]
  expect_s4_class(st, "export")
  cl <- as.data.frame(st@cluster)
  expect_identical(nrow(cl), 3L)
  ex <- as.data.frame(st@export)
  # export revenue is a negative cost, so the curve must FALL
  pr <- unique(ex$price)
  expect_identical(length(pr), 3L)
  # the steps split the flat bound, they do not multiply it
  per_step <- unique(ex[, c("cluster", "exp.up")])
  expect_equal(sum(per_step$exp.up), unique(bw$cap.up))

  # no curve columns -> exactly today's flat stub
  flat <- data.frame(src = "R2", dst = "R3", price = 40, cap.up = 4)
  mf <- subset_model_regions(mod, c("R1", "R2"), boundary_prices = flat,
                             verbose = FALSE)
  sf <- mf@data[["boundary_stubs"]]@data[[1]]
  expect_identical(nrow(as.data.frame(sf@cluster)), 0L)
  expect_equal(unique(as.data.frame(sf@export)$price), 40)
})

# --- the driver -------------------------------------------------------------- #

test_that("with no trade the per-region objectives add up to the full model", {
  skip_if_no_solver()
  rw_local("add")
  mod <- rw_mod()
  full <- solve_scenario(interpolate_model(mod, name = "rw_full"),
                         echo = FALSE)
  o_full <- as.numeric(getData(full, "vObjective", merge = TRUE)$value[1])

  br <- solve_by_region(mod, name = "rw_none", verbose = FALSE)
  expect_s3_class(br, "by_region")
  expect_true(br$additive)
  expect_false(br$failed)
  expect_identical(nrow(br$runs), 3L)
  expect_true(all(br$runs$status == "solved"))
  expect_equal(sum(br$runs$objective), o_full, tolerance = 1e-6)

  # and the same holds for groups
  bg <- solve_by_region(mod, name = "rw_grp",
                        group = list(c("R1", "R2"), "R3"), verbose = FALSE)
  expect_identical(nrow(bg$runs), 2L)
  expect_equal(sum(bg$runs$objective), o_full, tolerance = 1e-6)
})

test_that("a trade window binds and additivity is flagged as gone", {
  skip_if_no_solver()
  rw_local("win")
  mod <- rw_mod(route = TRUE)

  none <- solve_by_region(mod, name = "rw_w0", regions = "R2",
                          verbose = FALSE)
  auto <- solve_by_region(mod, name = "rw_w1", regions = "R2",
                          trade = "auto", price = 40, nsteps = 3,
                          verbose = FALSE)
  expect_true(none$additive)
  expect_false(auto$additive)
  # R2 gains an export outlet, so its net cost falls
  expect_lt(auto$runs$objective[1], none$runs$objective[1])
  # a 3-step curve is three boundary objects after variant expansion
  expect_identical(auto$runs$boundary[1], 3L)
})

test_that("runs are stored as variants and tagged", {
  skip_if_no_solver()
  rw_local("store")
  mod <- rw_mod(regs = c("R1", "R2"))
  br <- solve_by_region(mod, name = "rw_store", verbose = FALSE)

  rr <- scenario_runs(br$scenarios[[1]])
  expect_true(all(c("R1", "R2") %in% rr$variant))
  vy <- yaml::read_yaml(file.path(br$scenarios[[1]]@path, "runs", "R1",
                                  "variant.yml"))
  expect_identical(vy$type, "region_sample")
  expect_identical(vy$sequence, "rw_store")
  expect_identical(vy$trade, "none")

  # getData stacks the regions
  d <- getData(br, "vSupOut", merge = TRUE)
  expect_setequal(unique(as.character(d$region)), c("R1", "R2"))
})

test_that("the driver validates its arguments", {
  mod <- rw_mod()
  expect_error(solve_by_region(mod, regions = "R9"), "not declared")
  expect_error(solve_by_region(mod, trade = "user"), "needs `boundary_prices`")
  expect_warning(
    solve_by_region(mod, regions = character(0), trade = "none",
                    boundary_prices = data.frame(src = "a", dst = "b",
                                                 price = 1), verbose = FALSE),
    "ignored")
})
