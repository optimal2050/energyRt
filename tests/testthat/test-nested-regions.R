# =========================================================================== #
# Mixed-resolution commodities: `commodity@geoframe`.
#
# A commodity may be balanced at a COARSER geoscale level than the model's own
# regions -- steel nationally while electricity stays per-state. The spatial
# twin of `commodity@timeframe`.
#
# The design reuses the balance layer the agg-rewrite already built for time:
# `mvOutTot`/`mvInpTot` gain the coarse cells, `eqOutTot`/`eqInpTot` sum the
# finer ones into them via `mRegionFamily` (adjacent levels only, plain sum --
# regional quantities are extensive, so there is no `pRegionAgg`), and
# `mvBalance` alone is pinned to the commodity's own level by `mCommRegion`.
#
# The load-bearing assertion is `nr_equivalence`: balancing a commodity at a
# coarse level must give exactly the objective of the same system modelled the
# old way, with per-region commodities plus a free, unlimited trade route. That
# is what "free transport within the level" means, and it is the property the
# whole feature rests on.
# =========================================================================== #

skip_if_no_geoscales <- function() {
  testthat::skip_if_not_installed("geoscales")
}

nr_geoscale <- function(regions = c("R1", "R2")) {
  geoscales::geoscale_from_leaftable(
    data.frame(nation = "NAT", region = regions),
    geoframes = c("nation", "region"), key = "region", name = "nr")
}

# Two regions, coal supply and a plant in each, a steel mill in R1 only.
# `steel_level` = "nation" puts STEEL on the coarse balance; character() leaves
# it at the finest level. `extra` receives the free trade route for the
# equivalence variant.
nr_model <- function(steel_level, steel_dem_region, extra = list(),
                     name = "nr", regions = c("R1", "R2"),
                     mill_region = "R1") {
  cal <- newCalendar(timetable = make_timetable(
    struct = list(ANNUAL = "ANNUAL", SEASON = c("WIN", "SUM"))),
    name = paste0("cal_", name))
  objs <- c(list(
    newCommodity("COA", timeframe = "ANNUAL"),
    newCommodity("ELC", timeframe = "SEASON"),
    newCommodity("STEEL", timeframe = "ANNUAL", geoframe = steel_level),
    newSupply("SUP_COA", commodity = "COA",
              supply = data.frame(region = regions, cost = 1)),
    newTechnology("ECOA", input = list(comm = "COA"),
                  output = list(comm = "ELC"),
                  invcost = data.frame(invcost = 900),
                  vintage = data.frame(olife = 30L), cap2act = 1),
    newTechnology("MILL", input = list(comm = "ELC"),
                  output = list(comm = "STEEL"), region = mill_region,
                  ceff = data.frame(comm = "ELC", cinp2use = 0.5),
                  invcost = data.frame(invcost = 50),
                  vintage = data.frame(olife = 30L), cap2act = 1),
    newDemand("DEM_ELC", commodity = "ELC",
              demand = data.frame(region = rep(regions, each = 2),
                               timeslice = c("WIN", "SUM"), demand = 30)),
    newDemand("DEM_STL", commodity = "STEEL", region = steel_dem_region,
              demand = data.frame(region = steel_dem_region,
                               timeslice = "ANNUAL", demand = 10))
  ), extra)
  newModel(name = name, desc = "", calendar = cal, region = regions,
           horizon = newHorizon(2020:2030, intervals = 10), discount = 0.05,
           repo = do.call(newRepository, c(list(paste0("repo_", name)), objs)))
}

nr_gd <- function(scen, nm) {
  p <- scen@modInp@parameters[[nm]]
  if (is.null(p)) return(NULL)
  d <- get_data_slot(p)
  if (is.null(d) || nrow(d) == 0) return(NULL)
  as.data.frame(d)
}

# --------------------------------------------------------------------------- #

test_that("commodity@geoframe round-trips and defaults to empty", {
  expect_equal(newCommodity("STEEL", geoframe = "nation")@geoframe, "nation")
  expect_length(newCommodity("ELC")@geoframe, 0)
})

test_that("the hierarchy is pruned to the model's own regions", {
  skip_if_no_geoscales()
  gs <- geoscales::geoscale_from_leaftable(
    data.frame(nation = "NAT", zone = c("W", "W", "E"),
               region = c("R1", "R2", "R3")),
    geoframes = c("nation", "zone", "region"), key = "region", name = "p")
  h <- energyRt:::.geo_hierarchy(gs, c("R1", "R2"))
  # E has no declared child, so it must not enter the region set: a geoscale
  # covering more ground than the model is normal and must stay free.
  expect_setequal(h$region, c("R1", "R2", "NAT", "W"))
  expect_false("E" %in% h$region)
  expect_null(energyRt:::.geo_hierarchy(NULL, c("R1")))
})

test_that("attaching a geoscale widens sets$region but expands no parameter", {
  skip_if_no_geoscales()
  skip_if_no_solver()
  flat <- vt_interp(vt_model(name = "nr0"), "nr0")
  nest <- vt_interp(setGeoscale(vt_model(name = "nr1"), nr_geoscale()), "nr1")

  expect_setequal(flat@modInp@sets$region, c("R1", "R2"))
  expect_setequal(nest@modInp@sets$region, c("R1", "R2", "NAT"))
  # Declared regions keep their order and stay at the head of the set.
  expect_equal(nest@modInp@sets$region[1:2], flat@modInp@sets$region)

  # The extra members are INERT: no parameter may broadcast a value onto a
  # coarse region. This is the guard that caught pSupCost / mSupSpan /
  # mvTotalCost silently widening onto the nation.
  leaked <- character()
  for (nm in names(nest@modInp@parameters)) {
    if (nm %in% c("region", "mRegionFamily", "mCommRegion")) next
    d <- get_data_slot(nest@modInp@parameters[[nm]])
    if (is.null(d) || nrow(d) == 0) next
    d <- as.data.frame(d)
    for (col in intersect(c("region", "regionp", "src", "dst"), names(d))) {
      if (any(d[[col]] == "NAT", na.rm = TRUE)) leaked <- c(leaked, nm)
    }
  }
  expect_equal(unique(leaked), character())

  # Only the region set itself and the two new maps may differ structurally.
  # Row counts of the RATE parameters do change: `ANYREGION` would now also
  # cover NAT, so folding correctly declines and pWacc/pSdr/pDiscountFactor stay
  # explicit. Same values, same LP -- just unfolded.
  rows <- function(s) vapply(s@modInp@parameters,
                             function(p) nrow(get_data_slot(p)), integer(1))
  r0 <- rows(flat)
  r1 <- rows(nest)[names(r0)]
  changed <- names(r0)[r1 != r0]
  expect_setequal(changed,
                  c("region", "mRegionFamily", "mCommRegion",
                    "pWacc", "pSdr", "pDiscountFactor"))
})

test_that("mCommRegion / mRegionFamily / mvBalance have the right shape", {
  skip_if_no_geoscales()
  skip_if_no_solver()
  scen <- vt_interp(setGeoscale(nr_model("nation", "NAT", name = "nr2"),
                                nr_geoscale()), "nr2")

  cr <- nr_gd(scen, "mCommRegion")
  expect_equal(sort(cr$region[cr$comm == "STEEL"]), "NAT")
  expect_setequal(cr$region[cr$comm == "ELC"], c("R1", "R2"))

  fam <- nr_gd(scen, "mRegionFamily")
  expect_setequal(paste(fam$region, fam$regionp), c("NAT R1", "NAT R2"))

  # The balance is enforced ONLY at the commodity's own level ...
  bal <- nr_gd(scen, "mvBalance")
  expect_setequal(bal$region[bal$comm == "STEEL"], "NAT")
  expect_setequal(bal$region[bal$comm == "ELC"], c("R1", "R2"))

  # ... while the TOTALS keep their finer cells, or eqOutTot would have nothing
  # to aggregate upward.
  ot <- nr_gd(scen, "mvOutTot")
  expect_true(all(c("NAT", "R1") %in% ot$region[ot$comm == "STEEL"]))
})

test_that("nr_equivalence: a coarse balance equals free transport", {
  skip_if_no_geoscales()
  skip_if_no_solver()
  obj <- function(mod, tag) {
    scen <- suppressMessages(suppressWarnings(
      interpolate_model(mod, name = tag, fold = TRUE)))
    sol <- vt_solve(scen)
    sum(suppressMessages(getData(sol, "vObjective", merge = TRUE))$value)
  }
  coarse <- obj(setGeoscale(nr_model("nation", "NAT", name = "nrA"),
                            nr_geoscale()), "nrA")
  # The same system the old way: STEEL per region plus a free, unlimited route.
  flat <- obj(nr_model(character(), "R2", name = "nrB", extra = list(newTrade(
    "TR_STL", commodity = "STEEL",
    routes = data.frame(src = "R1", dst = "R2"),
    trade = data.frame(timeslice = "ANNUAL", teff = 1)))), "nrB")

  expect_equal(coarse, flat, tolerance = 1e-9)
  # A missed mCommRegion guard would show up here as steel taxed or counted
  # twice, so this doubles as the no-double-counting assertion.
  expect_gt(coarse, 0)

  # The same property on the other back-ends. Julia and Pyomo REFUSED a coarse
  # balance until the up-aggregation term over `mRegionFamily` was ported to
  # their eqOutTot / eqInpTot, so this is what says the port is real rather than
  # merely no longer erroring. GAMS is left out: it needs a dense scenario and a
  # NEOS submission.
  have <- function(f) { p <- tryCatch(f(), error = function(e) NULL)
                        !is.null(p) && nzchar(p) }
  engines <- c(if (have(get_julia_path))  "julia_highs",
               if (have(get_python_path)) "pyomo_glpk")
  for (eng in engines) {
    o <- function(mod, tag) {
      scen <- suppressMessages(suppressWarnings(
        interpolate_model(mod, name = tag, fold = TRUE, sparse = FALSE)))
      sum(suppressMessages(getData(
        solve_scenario(scen, solver = solver_options[[eng]]),
        "vObjective", merge = TRUE))$value)
    }
    expect_equal(
      o(setGeoscale(nr_model("nation", "NAT", name = paste0("nrA_", eng)),
                    nr_geoscale()), paste0("nrA_", eng)),
      coarse, tolerance = 1e-6, info = eng)
  }
})

test_that("three levels chain through the intermediate one", {
  skip_if_no_geoscales()
  skip_if_no_solver()
  # mRegionFamily is ADJACENT-only, so reaching the nation from R1 must go
  # R1 -> W -> NAT, and the intermediate cell has to exist in mvOutTot for the
  # second step to have anything to read. This is the assertion that would fail
  # if the chain were built as a transitive closure instead.
  regs <- c("R1", "R2", "R3", "R4")
  gs3 <- geoscales::geoscale_from_leaftable(
    data.frame(nation = "NAT", zone = c("W", "W", "E", "E"), region = regs),
    geoframes = c("nation", "zone", "region"), key = "region", name = "t3")

  obj <- function(mod, tag) {
    scen <- suppressMessages(suppressWarnings(
      interpolate_model(mod, name = tag, fold = TRUE)))
    list(scen = scen,
         v = sum(suppressMessages(
           getData(vt_solve(scen), "vObjective", merge = TRUE))$value))
  }
  a <- obj(setGeoscale(nr_model("nation", "NAT", name = "t3a",
                                regions = regs), gs3), "t3a")

  ot <- nr_gd(a$scen, "mvOutTot")
  expect_setequal(ot$region[ot$comm == "STEEL"], c("R1", "W", "NAT"))
  bal <- nr_gd(a$scen, "mvBalance")
  expect_setequal(bal$region[bal$comm == "STEEL"], "NAT")
  fam <- nr_gd(a$scen, "mRegionFamily")
  expect_setequal(paste(fam$region, fam$regionp),
                  c("NAT W", "NAT E", "W R1", "W R2", "E R3", "E R4"))

  b <- obj(nr_model(character(), "R4", name = "t3b", regions = regs,
                    extra = list(newTrade(
                      "TR_STL", commodity = "STEEL",
                      routes = data.frame(src = "R1",
                                          dst = c("R2", "R3", "R4")),
                      trade = data.frame(timeslice = "ANNUAL", teff = 1)))), "t3b")
  expect_equal(a$v, b$v, tolerance = 1e-9)
})

# --------------------------------------------------------------------------- #
# STRICT-level processes AT the commodity's coarse level. check_levels demands
# the exact-level shape for supply/storage; the span maps must then honour it
# -- the historic bug was map_mSupSpan intersecting with the atoms and silently
# DELETING a nation-level supply, leaving the balance a free costless vOutTot
# cell (energy from nowhere, objective 0, feasible).

# One-nation model: GAS balanced at NAT, demanded at NAT. `sup` is the supply
# object under test.
nrs_model <- function(name, sup, demand = 10) {
  cal <- newCalendar(timetable = make_timetable(
    struct = list(ANNUAL = "ANNUAL", SEASON = c("WIN", "SUM"))),
    name = paste0("cal_", name))
  newModel(name = name, calendar = cal, region = c("R1", "R2"),
    horizon = newHorizon(2025), discount = 0,
    repo = newRepository(paste0("repo_", name),
      newCommodity("GAS", timeframe = "ANNUAL", geoframe = "nation"),
      sup,
      newDemand("DEM_GAS", commodity = "GAS", region = "NAT",
                demand = data.frame(region = "NAT", timeslice = "ANNUAL",
                                    demand = demand))))
}

nrs_solve <- function(mod, tag) {
  scen <- suppressMessages(suppressWarnings(interpolate_model(
    setGeoscale(mod, nr_geoscale()), name = tag, overwrite = TRUE)))
  vt_solve(scen)
}

# @covers pSupCost pSupAva mSupOutTot vSupCost vSupOut depth=S backends=glpk forks=geoframe
test_that("a supply at the commodity's coarse level is honoured and priced", {
  skip_if_no_geoscales()
  skip_if_no_solver()
  sol_val <- function(scen, nm) {
    d <- get_data_slot(scen@modOut@variables[[nm]], optional = TRUE)
    if (is.null(d) || nrow(d) == 0) return(NULL)
    as.data.frame(d)
  }
  # (a) explicit region = NAT rows in the supply frame
  a <- nrs_solve(nrs_model("nrsA", newSupply("SUP_GAS", commodity = "GAS",
    region = "NAT", supply = data.frame(region = "NAT", cost = 2))), "nrsA")
  # hand: 10 GAS x cost 2, discount 0, one year -> objective exactly 20
  expect_equal(sum(sol_val(a, "vObjective")$value), 20, tolerance = 1e-9)
  sc <- sol_val(a, "vSupCost")
  expect_equal(as.character(sc$region), "NAT")
  expect_equal(sc$value, 20)
  expect_equal(unique(as.character(sol_val(a, "vSupOutTot")$region)), "NAT")

  # (b) the same supply through a region-less frame (wildcard row): exercises
  # the unfold path that used to DELETE pSupCost against the empty span, and
  # the cost_agg scan that used to run before the wildcard was materialised
  b <- nrs_solve(nrs_model("nrsB", newSupply("SUP_GAS", commodity = "GAS",
    region = "NAT", supply = data.frame(cost = 2))), "nrsB")
  expect_equal(sum(sol_val(b, "vObjective")$value), 20, tolerance = 1e-9)
  expect_equal(sol_val(b, "vSupCost")$value, 20)

  # equivalence: the flat twin (finest-level GAS, supply and demand per atom)
  # costs exactly the same
  cal <- newCalendar(timetable = make_timetable(
    struct = list(ANNUAL = "ANNUAL", SEASON = c("WIN", "SUM"))),
    name = "cal_nrsF")
  flat <- newModel(name = "nrsF", calendar = cal, region = c("R1", "R2"),
    horizon = newHorizon(2025), discount = 0,
    repo = newRepository("repo_nrsF",
      newCommodity("GAS", timeframe = "ANNUAL"),
      newSupply("SUP_GAS", commodity = "GAS",
                supply = data.frame(cost = 2)),
      newDemand("DEM_GAS", commodity = "GAS",
                demand = data.frame(region = c("R1", "R2"),
                                    timeslice = "ANNUAL", demand = 5))))
  f <- vt_solve(suppressMessages(suppressWarnings(
    interpolate_model(flat, name = "nrsF", overwrite = TRUE))))
  expect_equal(sum(sol_val(f, "vObjective")$value),
               sum(sol_val(a, "vObjective")$value), tolerance = 1e-9)
})

# @covers vStorageOut vStorageLevel depth=S backends=glpk forks=geoframe
test_that("a storage at the commodity's coarse level operates and is priced", {
  skip_if_no_geoscales()
  skip_if_no_solver()
  cal <- newCalendar(timetable = make_timetable(
    struct = list(ANNUAL = "ANNUAL", SEASON = c("S1", "S2"))), name = "cal_nrsS")
  mod <- newModel(name = "nrsS", calendar = cal, region = c("R1", "R2"),
    horizon = newHorizon(2025), discount = 0,
    repo = newRepository("repo_nrsS",
      newCommodity("GAS", timeframe = "SEASON", geoframe = "nation"),
      newSupply("SUP_GAS", commodity = "GAS", region = "NAT",
                supply = data.frame(region = "NAT", timeslice = c("S1", "S2"),
                                    cost = c(1, 100))),
      newStorage("STG_GAS", commodity = "GAS", region = "NAT", olife = 1L,
                 cap2act = 1, invcost = list(out.invcost = 1),
                 seff = data.frame(inpeff = 1, outeff = 1)),
      newDemand("DEM_GAS", commodity = "GAS", region = "NAT",
                demand = data.frame(region = "NAT",
                                    timeslice = c("S1", "S2"), demand = 5))))
  scen <- vt_solve(suppressMessages(suppressWarnings(interpolate_model(
    setGeoscale(mod, nr_geoscale()), name = "nrsS", overwrite = TRUE))))
  d <- as.data.frame(get_data_slot(scen@modOut@variables[["vObjective"]]))
  # hand: buy 10 in S1 at 1 (5 direct + 5 stored) + storage out cap 5 x 1 = 15
  expect_equal(sum(d$value), 15, tolerance = 1e-9)
  so <- as.data.frame(get_data_slot(scen@modOut@variables[["vStorageOut"]]))
  expect_equal(unique(as.character(so$region)), "NAT")
})

test_that("RoW import/export at a coarse level are refused loudly", {
  skip_if_no_geoscales()
  skip_if_no_solver()
  # import/export carry no @region slot; their region resolution defaults to
  # the atoms, so the strict-level check refuses the shape instead of running
  # a silently different model. Supporting them at a coarse level is an open
  # capability -- this pins the refusal until it is built.
  mod <- nrs_model("nrsI", newSupply("SUP_GAS", commodity = "GAS",
    region = "NAT", supply = data.frame(region = "NAT", cost = 2)))
  mod <- add(mod, newImport("IMP_GAS", commodity = "GAS",
                            import = data.frame(region = "NAT", price = 3)))
  expect_error(
    suppressMessages(suppressWarnings(interpolate_model(
      setGeoscale(mod, nr_geoscale()), name = "nrsI", overwrite = TRUE))),
    "resolution does not match")
})

test_that("@geoframe without a geoscale is refused", {
  skip_if_no_solver()
  expect_error(
    suppressMessages(suppressWarnings(interpolate_model(
      nr_model("nation", "R2", name = "nrC"), name = "nrC", fold = TRUE))),
    "geoframe"
  )
})

test_that("an unknown @geoframe names the levels that do exist", {
  skip_if_no_geoscales()
  skip_if_no_solver()
  expect_error(
    suppressMessages(suppressWarnings(interpolate_model(
      setGeoscale(nr_model("continent", "NAT", name = "nrD"), nr_geoscale()),
      name = "nrD", fold = TRUE))),
    "continent"
  )
})

test_that("a process coarser than its commodity is refused", {
  skip_if_no_geoscales()
  skip_if_no_solver()
  # The mill sits at NAT but ELC is balanced per region, so its input could
  # never be drawn from any balance -- it would silently vanish.
  mod <- setGeoscale(
    nr_model("nation", "NAT", name = "nrE", mill_region = "NAT"),
    nr_geoscale())
  expect_error(
    suppressMessages(suppressWarnings(
      interpolate_model(mod, name = "nrE", fold = TRUE))),
    "COARSER"
  )
})

test_that("no engine refuses a coarse commodity any more", {
  skip_if_no_geoscales()
  skip_if_no_solver()
  # Julia and Pyomo used to have no `mRegionFamily` term in eqOutTot/eqInpTot, so
  # they would have pinned the balance to NAT and found nothing feeding it. They
  # were made to refuse rather than solve a different problem
  # (`.assert_geoframe_supported()`, now retired to drafts/). The term is ported,
  # so the guard is gone and every writer must accept the model.
  mod <- setGeoscale(nr_model("nation", "NAT", name = "nrF"), nr_geoscale())
  scen <- vt_interp(mod, "nrF")
  expect_false(exists(".assert_geoframe_supported",
                      envir = asNamespace("energyRt"), inherits = FALSE))

  # The equations themselves must carry the term, in every template.
  code <- getFromNamespace(".modelCode", "energyRt")
  for (eng in c("GLPK", "GAMS", "JuMP", "PYOMOConcrete")) {
    body <- paste(code[[eng]], collapse = "
")
    expect_true(grepl("mRegionFamily", body, fixed = TRUE), info = eng)
  }
})

