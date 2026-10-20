# =========================================================================== #
# `@geoframe` on the region-carrying classes: the geoscale level an object's
# regions are expressed at.
#
# Optional by design. Unset, the level is DERIVED from the region codes, which
# is what every model written before the slot relies on. Declared, it
#
#   * names the level, and makes that level's regions the object's scope when
#     it names no regions of its own -- the ONE default the slot sets;
#   * requires every region code to be a member of that level.
#
# It adds no new behaviour beyond that default: the rule that an object may be
# coarser than what it CONSUMES but never than what it produces or stores is
# enforced where it always was, by `.assert_process_geoframe()` and
# `.check_process_levels()`. These tests pin that the declaration routes into
# those gates rather than duplicating them.
#
# @covers depth=S backends=glpk forks=geoframe
# =========================================================================== #

gd_gs <- function() {
  geoscales::geoscale_from_leaftable(
    data.frame(nation = "NAT", zone = c("W", "W", "E"),
               region = c("R1", "R2", "R3")),
    geoframes = c("nation", "zone", "region"), key = "region", name = "gd")
}

gd_cal <- function(nm) {
  newCalendar(timetable = make_timetable(
    struct = list(ANNUAL = "ANNUAL", SEASON = c("WIN", "SUM"))),
    name = paste0("gd_", nm))
}

# ELC per region, STEEL wherever `steel_gf` says, a MILL that consumes ELC and
# produces STEEL, and a steel demand at the same level as STEEL.
gd_model <- function(nm, mill_gf = character(0), mill_region = character(0),
                     steel_gf = character(0)) {
  objs <- list(
    newCommodity("ELC", timeframe = "SEASON"),
    newCommodity("COA", timeframe = "ANNUAL"),
    newCommodity("STEEL", timeframe = "ANNUAL", geoframe = steel_gf),
    newSupply("S", commodity = "COA", supply = data.frame(cost = 0)),
    newTechnology("ECOA", input = list(comm = "COA"),
                  output = list(comm = "ELC"), cap2act = 1,
                  invcost = data.frame(invcost = 100),
                  vintage = data.frame(olife = 30L)),
    newTechnology("MILL", input = list(comm = "ELC"),
                  output = list(comm = "STEEL"),
                  geoframe = mill_gf, region = mill_region, cap2act = 1,
                  ceff = data.frame(comm = "ELC", cinp2use = 0.5),
                  invcost = data.frame(invcost = 50),
                  vintage = data.frame(olife = 30L)),
    newDemand("D", commodity = "ELC",
              demand = data.frame(region = rep(c("R1", "R2", "R3"), each = 2),
                                  timeslice = c("WIN", "SUM"), demand = 5)),
    newDemand("DS", commodity = "STEEL", geoframe = steel_gf,
              demand = data.frame(timeslice = "ANNUAL", demand = 1)))
  setGeoscale(newModel(
    name = nm, calendar = gd_cal(nm), region = c("R1", "R2", "R3"),
    horizon = newHorizon(2025), discount = 0,
    repo = do.call(newRepository, c(list(paste0("gdr_", nm)), objs))), gd_gs())
}

gd_weather_model <- function(nm, wgf = character(0), wreg = character(0)) {
  objs <- list(
    newCommodity("ELC", timeframe = "SEASON"),
    newWeather("WSUN", unit = "1", timeframe = "SEASON",
               geoframe = wgf, region = wreg,
               weather = data.frame(timeslice = c("WIN", "SUM"),
                                    wval = c(0.2, 0.8))),
    newTechnology("ESOL", output = list(comm = "ELC"), cap2act = 1,
                  invcost = data.frame(invcost = 100),
                  vintage = data.frame(olife = 30L),
                  weather = data.frame(weather = "WSUN", waf.up = 1)),
    newDemand("D", commodity = "ELC",
              demand = data.frame(region = rep(c("R1", "R2", "R3"), each = 2),
                                  timeslice = c("WIN", "SUM"), demand = 1)))
  setGeoscale(newModel(
    name = nm, calendar = gd_cal(nm), region = c("R1", "R2", "R3"),
    horizon = newHorizon(2025), discount = 0,
    repo = do.call(newRepository, c(list(paste0("gdw_", nm)), objs))), gd_gs())
}

gd_interp <- function(mod, nm) {
  suppressMessages(suppressWarnings(
    interpolate_model(mod, name = nm, overwrite = TRUE)))
}

gd_pw_regions <- function(scen) {
  d <- as.data.frame(get_data_slot(scen@modInp@parameters[["pWeather"]]))
  sort(unique(as.character(d$region)))
}

# --------------------------------------------------------------------------- #

test_that("the slot is optional and empty by default on every class", {
  cls <- c("supply", "demand", "technology", "storage", "trade", "import",
           "export", "weather", "tax", "subsidy")
  for (k in cls) {
    expect_true("geoframe" %in% slotNames(k), label = k)
    expect_length(methods::slot(methods::new(k), "geoframe"), 0)
  }
})

test_that("each constructor takes and keeps a declared level", {
  expect_equal(newTechnology("T", geoframe = "nation")@geoframe, "nation")
  expect_equal(newWeather("W", geoframe = "zone")@geoframe, "zone")
  expect_equal(newTrade("TR", commodity = "ELC", geoframe = "zone")@geoframe,
               "zone")
  expect_equal(newSupply("S", geoframe = "nation")@geoframe, "nation")
  expect_equal(newDemand("D", geoframe = "nation")@geoframe, "nation")
  expect_equal(newTax("TX", geoframe = "nation")@geoframe, "nation")
})

test_that("a declared level requires its own regions", {
  skip_if_not_installed("geoscales")
  # `geoframe = "nation"` while naming leaf regions says two things that cannot
  # both be true
  expect_error(
    gd_interp(gd_model("gd_bad", mill_gf = "nation",
                       mill_region = c("R1", "R2")), "gd_bad"),
    "geoframe.*nation.*names region\\(s\\).*belong to.*region")
  # the message names the object and the class
  expect_error(
    gd_interp(gd_model("gd_bad2", mill_gf = "nation",
                       mill_region = "R1"), "gd_bad2"),
    "technology \"MILL\"")
  # consistent regions pass the same check
  expect_error(
    gd_interp(gd_model("gd_ok", mill_gf = "region",
                       mill_region = c("R1", "R2")), "gd_ok"),
    NA)
})

test_that("a level the geoscale does not have is refused by name", {
  skip_if_not_installed("geoscales")
  expect_error(
    gd_interp(gd_weather_model("gd_planet", wgf = "planet"), "gd_planet"),
    "not a level of the model's geoscale \\(it has: nation, zone, region\\)")
})

test_that("a declared level scopes an object that names no regions", {
  skip_if_not_installed("geoscales")
  # the one default the slot sets, and the reason it matters for weather: a
  # series whose data names no region is stored once per level member
  expect_setequal(gd_pw_regions(gd_interp(gd_weather_model("gd_w0"), "gd_w0")),
                  c("R1", "R2", "R3", "NAT", "E", "W"))
  expect_equal(gd_pw_regions(
    gd_interp(gd_weather_model("gd_wn", wgf = "nation"), "gd_wn")), "NAT")
  expect_setequal(gd_pw_regions(
    gd_interp(gd_weather_model("gd_wz", wgf = "zone"), "gd_wz")), c("E", "W"))
  # `@region` narrows a declared level rather than competing with it
  expect_equal(gd_pw_regions(gd_interp(
    gd_weather_model("gd_wb", wgf = "nation", wreg = "NAT"), "gd_wb")), "NAT")
})

test_that("a declared level places a process, and the coarse gates still rule", {
  skip_if_not_installed("geoscales")
  # undeclared: every region, as before
  base <- gd_interp(gd_model("gd_p0"), "gd_p0")
  expect_setequal(get_process_region(base, process = "MILL")[["MILL"]],
                  c("R1", "R2", "R3"))

  # declared national, but STEEL is balanced per region: refused where it
  # always was -- only INPUTS may be finer than the process
  expect_error(
    gd_interp(gd_model("gd_p1", mill_gf = "nation"), "gd_p1"),
    "only INPUTS may be finer than the process")

  # declared national with STEEL balanced nationally: the mill sits at the
  # nation and consumes a finer commodity, which is the allowed direction
  nat <- gd_interp(gd_model("gd_p2", mill_gf = "nation", steel_gf = "nation"),
                   "gd_p2")
  expect_equal(get_process_region(nat, process = "MILL")[["MILL"]], "NAT")
})

test_that("declaring the level gives the same answer as writing the regions", {
  skip_if_not_installed("geoscales")
  skip_if_no_solver()
  obj <- function(mod, nm) {
    sol <- suppressMessages(suppressWarnings(solve_scenario(
      gd_interp(mod, nm), solver = solver_options$glpk, wait = TRUE)))
    sum(as.data.frame(get_data_slot(
      sol@modOut@variables[["vObjective"]]))$value)
  }
  declared <- obj(gd_model("gd_sd", mill_gf = "region"), "gd_sd")
  spelt <- obj(gd_model("gd_ss", mill_region = c("R1", "R2", "R3")), "gd_ss")
  expect_gt(declared, 0)
  expect_equal(declared, spelt, tolerance = 1e-9)
})
