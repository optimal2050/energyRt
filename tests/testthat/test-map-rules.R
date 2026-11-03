# Map parameters defined by a rule (R/map-rules.R): the rule enumerates the
# built table, the tables are dropped after interpolation and come back on
# read, the scenario saves without them, and the multimod import carries the
# rule to the matrix route. Fixtures: helper-param-refs.R.

mr_maps <- c("mTechTimeslice", "mCommTimeslice", "mWeatherTimeslice",
             "mvTechAct", "mvTechOut", "meqTechActSng")

as_tuples <- function(d) {
  d <- as.data.frame(d)
  for (j in seq_along(d)) d[[j]] <- as.character(d[[j]])
  d <- d[do.call(order, d), , drop = FALSE]
  rownames(d) <- NULL
  d
}

mr_interpolate <- function(name, rules, ...) {
  withr::with_options(list(energyRt.map_rules = rules, energyRt.map_rules_verify = TRUE),
    suppressMessages(interpolate_model(pr_model(), name = name,
                                       path = pr_root("scenarios", name), ...)))
}

test_that("a rule enumerates the join of its tables", {
  pf <- data.frame(tech = c("A", "B"), timeframe = c("ANNUAL", "SL"))
  fs <- data.frame(timeframe = c("ANNUAL", "SL", "SL"), timeslice = c("y", "s1", "s2"))
  r <- .map_rule(of = c("proc_frame", "frame_slice"),
                 tables = list(proc_frame = pf, frame_slice = fs), cols = c("tech", "timeslice"))
  expect_equal(as_tuples(.rule_tuples(r)),
               as_tuples(data.frame(tech = c("A", "B", "B"), timeslice = c("y", "s1", "s2"))))
  # nothing shared: a cross product
  r2 <- .map_rule(of = c("w", "l"), cols = c("weather", "timeslice"),
                  tables = list(w = data.frame(weather = c("W1", "W2")), l = data.frame(timeslice = c("s1", "s2"))))
  expect_equal(nrow(.rule_tuples(r2)), 4L)
  expect_false(identical(r$hash, r2$hash))
  expect_identical(r$hash, .map_rule(of = c("proc_frame", "frame_slice"),
                                     tables = list(proc_frame = pf, frame_slice = fs),
                                     cols = c("tech", "timeslice"))$hash)
  bad <- .map_rule(of = "w", tables = list(w = data.frame(weather = "W1")), cols = c("weather", "region"))
  expect_error(.rule_tuples(bad), "in none of its tables")
  # gates keep the matching rows, subtractions drop them; year comes back integer
  span <- data.frame(tech = c("A", "A", "B"), region = c("R1", "R2", "R1"), year = c(2025L, 2025L, 2030L))
  cr <- data.frame(comm = "ELC", region = "R1")
  gaps <- data.frame(tech = "B", region = "R1")
  g <- .map_rule(of = c("span", "pf", "fs"), gate = "cr", minus = "gaps", cols = c("tech", "region", "year", "timeslice"),
                 tables = list(span = span, pf = pf, fs = fs, cr = cr, gaps = gaps))
  got <- .rule_tuples(g)
  expect_true(is.integer(got$year))
  expect_equal(as_tuples(got), as_tuples(data.frame(tech = "A", region = "R1", year = 2025L, timeslice = "y")))
})

test_that("a rule's tuples are counted without enumerating it", {
  pf <- data.frame(tech = c("A", "B"), timeframe = c("ANNUAL", "SL"))
  fs <- data.frame(timeframe = c("ANNUAL", "SL", "SL"), timeslice = c("y", "s1", "s2"))
  span <- data.frame(tech = c("A", "A", "B"), region = c("R1", "R2", "R1"),
                     year = c(2025L, 2025L, 2030L))
  cr <- data.frame(tech = c("A", "B"), region = "R1")
  gaps <- data.frame(tech = "B", region = "R1")
  rules <- list(
    .map_rule(of = c("pf", "fs"), cols = c("tech", "timeslice"),
              tables = list(pf = pf, fs = fs)),
    .map_rule(of = c("span", "pf", "fs"), gate = "cr", cols = c("tech", "region", "year", "timeslice"),
              tables = list(span = span, pf = pf, fs = fs, cr = cr)),
    .map_rule(of = c("span", "pf", "fs"), minus = "gaps", cols = c("tech", "region", "year", "timeslice"),
              tables = list(span = span, pf = pf, fs = fs, gaps = gaps)),
    # nothing shared: a cross product, enumerated
    .map_rule(of = c("w", "l"), cols = c("weather", "timeslice"),
              tables = list(w = data.frame(weather = c("W1", "W2")),
                            l = data.frame(timeslice = c("s1", "s2", "s3")))),
    # a technology on two timeframes whose slices overlap: enumerated
    .map_rule(of = c("pf", "fs"), cols = c("tech", "timeslice"),
              tables = list(pf = data.frame(tech = "A", timeframe = c("F1", "F2")),
                            fs = data.frame(timeframe = c("F1", "F1", "F2"),
                                            timeslice = c("s1", "s2", "s2")))))
  for (r in rules) expect_identical(.rule_count(r), nrow(.rule_tuples(r)))
})

test_that("the activity, output and single-output domains carry exact rules on the tier fixtures", {
  skip_if_no_fixtures()
  pr_local_model()
  unlink(pr_root(), recursive = TRUE)
  env <- .mapping_fixture_env()
  for (nm in c("tm_core", "tm_flows", "tm_io", "tm_weather")) {
    build <- env[[nm]]
    scen <- withr::with_options(list(energyRt.map_rules = TRUE, energyRt.map_rules_verify = TRUE),
      suppressMessages(suppressWarnings(interpolate_model(build(), name = paste0("mr_", nm),
                                                          path = pr_root("scenarios", paste0("mr_", nm))))))
    rp <- .rule_params(scen)
    expect_true(all(c("mTechTimeslice", "mvTechAct") %in% rp), info = nm)
    if (.no_coarse_processes(scen)) expect_true("mvTechOut" %in% rp, info = nm)
  }
})

test_that("the TOPIA model interpolates with every rule verified against its table", {
  pr_local_model()
  unlink(pr_root(), recursive = TRUE)
  scen <- withr::with_options(list(energyRt.map_rules = TRUE, energyRt.map_rules_verify = TRUE),
    suppressMessages(suppressWarnings(interpolate_model(ut_build("R1", "s4_h24"), name = "mr_topia",
                                                        path = pr_root("scenarios", "mr_topia")))))
  expect_true(all(mr_maps %in% .rule_params(scen)))
  for (nm in mr_maps) expect_equal(nrow(scen@modInp@parameters[[nm]]@data), 0L, info = nm)
  # the availability-factor maps: one bound per cell in this model
  af <- intersect(c("meqTechAfLo", "meqTechAfUp", "meqTechAfFx"),
                  names(Filter(function(p) nrow(get_data_slot(p)) > 0, scen@modInp@parameters)))
  expect_true(length(af) > 0)
  expect_true(all(af %in% .rule_params(scen)))
})

test_that("an availability-factor map with per-timeslice bounds keeps its table", {
  pr_local_model()
  unlink(pr_root(), recursive = TRUE)
  # a technology whose af.up is open on some timeslices: the Up map covers
  # part of the cell, not a whole one
  repo <- newRepository(
    "mrr",
    newCommodity("ELC", timeframe = "SL"),
    newTechnology("GAS", output = list(comm = "ELC"),
                  af = data.frame(timeslice = pr_slices, af.up = c(0.9, Inf, 0.7, Inf)),
                  invcost = list(invcost = 1000), olife = list(olife = 30), cap2act = 1),
    newDemand("DEM", commodity = "ELC", demand = data.frame(timeslice = pr_slices, demand = 10)))
  cal <- newCalendar(timetable = make_timetable(struct = list(ANNUAL = "ANNUAL", SL = pr_slices)), name = "prc")
  mod <- newModel("mrm", region = "R1", discount = 0, calendar = cal, horizon = newHorizon(2025), repo = repo)
  scen <- withr::with_options(list(energyRt.map_rules = TRUE, energyRt.map_rules_verify = TRUE),
    suppressMessages(suppressWarnings(interpolate_model(mod, name = "mr_af", path = pr_root("scenarios", "mr_af")))))
  expect_false("meqTechAfUp" %in% .rule_params(scen))
  expect_gt(nrow(scen@modInp@parameters[["meqTechAfUp"]]@data), 0)
  expect_true("mvTechAct" %in% .rule_params(scen))
})

test_that("the availability-factor maps decided per cell equal the hourly build", {
  pr_local_model()
  unlink(pr_root(), recursive = TRUE)
  af <- c("meqTechAfLo", "meqTechAfUp", "meqTechAfFx")
  tuples <- function(scen, nm) {
    p <- scen@modInp@parameters[[nm]]
    d <- if (is.null(p)) NULL else get_data_slot(p)
    if (is.null(d) || !nrow(d)) return(NULL)
    as_tuples(as.data.frame(d)[, c("tech", "region", "year", "timeslice")])
  }
  # whole cells (TOPIA), and a technology open on some timeslices (partial cells)
  repo <- newRepository(
    "mrc",
    newCommodity("ELC", timeframe = "SL"),
    newTechnology("GAS", output = list(comm = "ELC"),
                  af = data.frame(timeslice = pr_slices, af.up = c(0.9, Inf, 0.7, Inf)),
                  invcost = list(invcost = 1000), olife = list(olife = 30), cap2act = 1),
    newTechnology("NUC", output = list(comm = "ELC"), af = data.frame(af.lo = 0.8, af.up = 0.8),
                  invcost = list(invcost = 3000), olife = list(olife = 40), cap2act = 1),
    newDemand("DEM", commodity = "ELC", demand = data.frame(timeslice = pr_slices, demand = 10)))
  cal <- newCalendar(timetable = make_timetable(struct = list(ANNUAL = "ANNUAL", SL = pr_slices)), name = "prc")
  models <- list(topia = ut_build("R1", "s4_h24"),
                 part = newModel("mrc", region = "R1", discount = 0, calendar = cal, horizon = newHorizon(2025), repo = repo))
  for (nm in names(models)) {
    cells <- withr::with_options(list(energyRt.map_rules = TRUE, energyRt.map_rules_verify = FALSE),
      suppressMessages(suppressWarnings(interpolate_model(models[[nm]], name = paste0("mrc_", nm, "_c"),
                                                          path = pr_root("scenarios", paste0("mrc_", nm, "_c"))))))
    hourly <- withr::with_options(list(energyRt.map_rules = FALSE),
      suppressMessages(suppressWarnings(interpolate_model(models[[nm]], name = paste0("mrc_", nm, "_h"),
                                                          path = pr_root("scenarios", paste0("mrc_", nm, "_h"))))))
    for (m in af) expect_identical(tuples(cells, m), tuples(hourly, m), info = paste(nm, m))
  }
  # the partial-cell model: the Up map keeps a table, the Fx map of NUC is a rule
  expect_false("meqTechAfUp" %in% .rule_params(cells))
  expect_true("meqTechAfFx" %in% .rule_params(cells))
})

test_that("af rows with a year and no timeslice are decided per cell, not hour by hour", {
  pr_local_model()
  unlink(pr_root(), recursive = TRUE)
  af <- c("meqTechAfLo", "meqTechAfUp", "meqTechAfFx")
  tuples <- function(scen, nm) {
    p <- scen@modInp@parameters[[nm]]
    d <- if (is.null(p)) NULL else get_data_slot(p)
    if (is.null(d) || !nrow(d)) return(NULL)
    as_tuples(as.data.frame(d)[, c("tech", "region", "year", "timeslice")])
  }
  cal <- newCalendar(timetable = make_timetable(struct = list(ANNUAL = "ANNUAL", SL = pr_slices)), name = "prc")
  W <- newWeather("W1", timeframe = "SL", region = "R1",
                  weather = data.frame(region = "R1", timeslice = pr_slices, wval = c(0.2, 0.9, 0.5, 0.1)))
  # a factor for the first year only: af.fx by year, no timeslice
  yr_fx <- data.frame(year = c(2025L, 2030L), af.fx = c(0.9, 1))
  wind <- newTechnology("WIND", output = list(comm = "ELC"), cap2act = 1, af = yr_fx,
                        weather = data.frame(weather = "W1", waf.fx = 1),
                        capacity = data.frame(region = "R1", stock = 10))
  # the same rows beside per-timeslice ones in the same cells
  sol <- newTechnology("SOL", output = list(comm = "ELC"), cap2act = 1,
                       af = rbind(transform(yr_fx, timeslice = NA_character_, af.up = NA_real_),
                                  data.frame(year = 2030L, timeslice = pr_slices[1:2], af.fx = NA, af.up = 0.5)),
                       weather = data.frame(weather = "W1", waf.fx = 1),
                       capacity = data.frame(region = "R1", stock = 10))
  gas <- newTechnology("GAS", output = list(comm = "ELC"), cap2act = 1,
                       capacity = data.frame(region = "R1", stock = 20), varom = list(varom = 50))
  mk <- function(nm, ...) newModel(nm, region = "R1", discount = 0, calendar = cal, horizon = newHorizon(c(2025, 2030)),
    repo = newRepository(paste0("r_", nm), newCommodity("ELC", timeframe = "SL"), W, ...,
                         newDemand("DEM", commodity = "ELC", demand = data.frame(timeslice = pr_slices, demand = 5))))
  models <- list(wild = mk("mrw", wind, gas), mixed = mk("mrx", wind, sol, gas))
  for (nm in names(models)) {
    cells <- withr::with_options(list(energyRt.map_rules = TRUE, energyRt.map_rules_verify = TRUE),
      suppressMessages(suppressWarnings(interpolate_model(models[[nm]], name = paste0("mry_", nm, "_c"),
                                                          path = pr_root("scenarios", paste0("mry_", nm, "_c"))))))
    hourly <- withr::with_options(list(energyRt.map_rules = FALSE),
      suppressMessages(suppressWarnings(interpolate_model(models[[nm]], name = paste0("mry_", nm, "_h"),
                                                          path = pr_root("scenarios", paste0("mry_", nm, "_h"))))))
    for (m in af) expect_identical(tuples(cells, m), tuples(hourly, m), info = paste(nm, m))
    # the wildcard-only model: WIND's fixed cells are whole, kept as a rule
    if (nm == "wild") expect_true("meqTechAfFx" %in% .rule_params(cells))
  }
})

test_that("the timeslice maps equal their rules and keep only the rule after interpolation", {
  pr_local_model()
  unlink(pr_root(), recursive = TRUE)
  tables <- mr_interpolate("mrt", rules = FALSE)
  scen <- mr_interpolate("mrr", rules = TRUE)
  for (nm in mr_maps) {
    pt <- tables@modInp@parameters[[nm]]
    pr <- scen@modInp@parameters[[nm]]
    expect_false(is.null(pt@misc$rule), info = nm)      # recorded either way
    expect_gt(nrow(pt@data), 0)
    expect_equal(nrow(pr@data), 0L, info = nm)
    expect_identical(pr@misc$rule$hash, pt@misc$rule$hash, info = nm)
    expect_equal(as_tuples(.rule_tuples(pt@misc$rule)), as_tuples(pt@data), info = nm)
    expect_equal(as_tuples(get_data_slot(pr)), as_tuples(get_data_slot(pt)), info = nm)
  }
  # the readers see the tuples
  expect_equal(as_tuples(.gds(scen, "mTechTimeslice")), as_tuples(.gds(tables, "mTechTimeslice")))
  expect_equal(as_tuples(.read_map(scen, "mCommTimeslice")), as_tuples(.read_map(tables, "mCommTimeslice")))
  # the option keeps the tables
  expect_gt(nrow(tables@modInp@parameters[["mTechTimeslice"]]@data), 0)

  # saved without the tables; the rule survives the round trip
  saved <- suppressMessages(save_scenario(scen, verbose = FALSE))
  for (nm in mr_maps) {
    expect_false(dir.exists(fp(saved@path, "modInp", "parameters", nm, "data")), info = nm)
  }
  mf <- yaml::read_yaml(fp(saved@path, "scenario.yml"))
  refs <- vapply(Filter(function(e) identical(e$via, "parameter"), mf$datasets), function(e) e$object, "")
  expect_false(any(mr_maps %in% refs))
  ld <- suppressMessages(load_scenario(saved@path, env = NULL, verbose = FALSE))
  for (nm in mr_maps) {
    expect_identical(ld@modInp@parameters[[nm]]@misc$rule$hash, scen@modInp@parameters[[nm]]@misc$rule$hash)
    expect_equal(as_tuples(get_data_slot(ld@modInp@parameters[[nm]])),
                 as_tuples(get_data_slot(tables@modInp@parameters[[nm]])), info = nm)
  }
})

test_that("an on-disk interpolation drops the rule maps' directories", {
  pr_local_model()
  unlink(pr_root(), recursive = TRUE)
  scen <- mr_interpolate("mro", rules = TRUE, ondisk = TRUE)
  for (nm in mr_maps) {
    p <- scen@modInp@parameters[[nm]]
    expect_false(isOnDisk(p), info = nm)
    expect_equal(nrow(p@data), 0L, info = nm)
    pth <- getObjPath(p)
    if (!is.null(pth)) expect_false(dir.exists(fp(pth, "data")), info = nm)
    expect_gt(nrow(get_data_slot(p)), 0)
  }
})

test_that("the multimod import carries the rule and builds the same LP", {
  skip_if_not_installed("multimod")
  skip_if_not_installed("highs")
  pr_local_model()
  unlink(pr_root(), recursive = TRUE)
  withr::local_options(energyRt.mm_keep_wildcards = TRUE)
  tables <- mr_interpolate("mmt", rules = FALSE)
  scen <- mr_interpolate("mmr", rules = TRUE)
  mt <- suppressMessages(suppressWarnings(multimod_from_energyRt(tables, inMemory = TRUE, verbose = FALSE)))
  mr <- suppressMessages(suppressWarnings(multimod_from_energyRt(scen, inMemory = FALSE, verbose = FALSE)))
  for (nm in mr_maps) {
    m <- mr$mappings[[nm]]
    expect_s3_class(m$rule, "mm_rule")
    expect_equal(NROW(m$data), 0L)
    expect_equal(as_tuples(multimod::get_data(mr, nm, type = "mapping")),
                 as_tuples(multimod::get_data(mt, nm, type = "mapping")), info = nm)
  }
  # the sets got their members from the rule's tables
  expect_setequal(multimod::get_data(mr, "timeslice", type = "set"),
                  multimod::get_data(mt, "timeslice", type = "set"))
  lt <- multimod::model_to_lp(mt)
  lr <- multimod::model_to_lp(mr)
  expect_identical(dim(lr$A), dim(lt$A))
  expect_equal(sum(abs(lr$A@x)), sum(abs(lt$A@x)))
  st <- multimod::solve_highs(mt, attach = FALSE)
  sr <- multimod::solve_highs(mr, attach = FALSE)
  expect_equal(sr$objective, st$objective, tolerance = 1e-9)
  # and the streamed route enumerates the rule in codes
  dir <- pr_root("mps"); dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  f <- fp(dir, "m.mps")
  withr::with_options(list(multimod.engine = "native"), multimod::write_mps(mr, f, stream = TRUE))
  r <- multimod::solve_mps(f)
  expect_equal(r$objective, st$objective, tolerance = 1e-9)
})

test_that("the text writers render a rule map as its tuples", {
  pr_local_model()
  unlink(pr_root(), recursive = TRUE)
  tables <- mr_interpolate("wrt", rules = FALSE)
  scen <- mr_interpolate("wrr", rules = TRUE)
  for (nm in mr_maps) {
    a <- .sm_to_glpk(tables@modInp@parameters[[nm]])
    b <- .sm_to_glpk(scen@modInp@parameters[[nm]])
    expect_gt(length(b), 3L, label = nm)
    expect_setequal(b, a)
  }
})

test_that("the GLPK route solves a rule scenario to the table scenario's objective", {
  skip_if_not(has_glpsol(), "glpsol not available")
  pr_local_model()
  unlink(pr_root(), recursive = TRUE)
  solve_glpk <- function(scen) suppressMessages(suppressWarnings(
    solve_scenario(scen, solver = solver_options$glpk, wait = TRUE, tmp.del = TRUE, force = TRUE)))
  objective <- function(sol) get_data_slot(sol@modOut@variables[["vObjective"]], optional = TRUE)$value[1]
  st <- solve_glpk(mr_interpolate("glt", rules = FALSE))
  sr <- solve_glpk(mr_interpolate("glr", rules = TRUE))
  expect_false(is.na(objective(st)))
  expect_equal(objective(sr), objective(st), tolerance = 1e-9)
})
