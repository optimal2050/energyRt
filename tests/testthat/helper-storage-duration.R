# Fixture for test-storage-duration.R: 21 days at hourly resolution with two
# DIFFERENT storage duties layered on one store.
#
#   * "solar" generation runs only between 09:00 and 16:00, so the store must
#     cycle every day -- a short-duration duty;
#   * days 8 to 10 are overcast, so it must ALSO carry energy across several
#     days -- a long-duration duty.
#
# A decomposition that works puts those in different bands. One that quietly
# averages them (a moving average, say) would not.
#
# Solved once and cached: the LP is ~500 rows and four tests need it.
.sd_scen_cache <- new.env(parent = emptyenv())

sd_solved_scenario <- function() {
  if (!is.null(.sd_scen_cache$scen)) return(.sd_scen_cache$scen)

  ND <- 21L
  days <- sprintf("d%03d", seq_len(ND))
  hrs <- sprintf("h%02d", 0:23)
  cal <- newCalendar(
    timetable = make_timetable(struct = list(
      ANNUAL = "ANNUAL", DAY = days, HOUR = hrs)),
    name = "sd_cal")

  tsl <- as.vector(t(outer(days, hrs, paste, sep = "_")))
  hour_of <- as.integer(sub(".*_h", "", tsl))
  day_of <- as.integer(sub("^d0*([0-9]+)_.*", "\\1", tsl))
  solar <- ifelse(hour_of >= 9 & hour_of <= 16, 1, 0)
  solar[day_of %in% 8:10] <- 0
  V <- data.frame(olife = 30L)

  mod <- newModel(
    name = "sd_fixture", region = "R1", horizon = newHorizon(2020),
    discount = 0, calendar = cal,
    repo = newRepository("sd_repo",
      newCommodity("SUN", timeframe = "HOUR"),
      newCommodity("ELC", timeframe = "HOUR"),
      # The backstop needs its OWN always-available fuel. On solar it would be
      # dark exactly when it is needed and the model is infeasible.
      newCommodity("GAS", timeframe = "ANNUAL"),
      newSupply("SUP_SUN", commodity = "SUN",
                supply = data.frame(timeslice = tsl, ava.up = solar, cost = 0)),
      newSupply("SUP_GAS", commodity = "GAS", supply = data.frame(cost = 50)),
      newTechnology("PV", input = list(comm = "SUN"), output = list(comm = "ELC"),
        invcost = data.frame(invcost = 30), vintage = V, cap2act = 1),
      newTechnology("BACKSTOP", input = list(comm = "GAS"), output = list(comm = "ELC"),
        invcost = data.frame(invcost = 9000), vintage = V, cap2act = 1),
      newStorage("STG", commodity = "ELC", vintage = V,
        invcost = list(invcost = 4), storage = list(invcost = 1),
        duration = data.frame(duration.lo = 1, duration.up = 200),
        af = data.frame(cinp.up = 1, cout.up = 1)),
      newDemand("DEM", commodity = "ELC",
                demand = data.frame(timeslice = tsl, demand = 10))))

  scen <- suppressMessages(suppressWarnings(
    solve_model(mod, name = "sd_fixture")))
  .sd_scen_cache$scen <- scen
  scen
}
