# Simulated TOPIA profiles: the demonstration dataset for the small calendars.
#
# The generator itself is `topia_simulate()` in R/topia-simulate.R, so this is
# one cached run of an exported function rather than a second authoring site.
# Levels are calibrated against the measured sample, which is read from the
# rds directly: at this point in the build the shipped dataset is what is
# being written.
#
# The roster is the one the article's weather objects declare, so the
# simulated set carries the same grades as the measured one.

REG_SIM <- sprintf("R%02d", 1:12)

.sim_measured <- function(rds = "data-raw/topia_profiles_sample.rds") {
  if (!file.exists(rds)) {
    stop("missing ", rds, ": the simulated profiles are calibrated against ",
         "the sample, so run data-raw/topia_profiles_sample.R with ",
         "ENERGYRT_TOPIA_SAMPLE=refresh first.", call. = FALSE)
  }
  smp <- readRDS(rds)
  list(weather = as.data.frame(smp$weather),
       demand = as.data.frame(smp$demand))
}

.sim_roster <- function(measured) {
  w <- measured$weather
  u <- unique(data.frame(resource = as.character(w$resource),
                         cluster = as.character(w$cluster),
                         region = as.character(w$region),
                         stringsAsFactors = FALSE))
  u[order(u$resource, u$cluster, u$region), , drop = FALSE]
}

local({
  measured <- .sim_measured()
  p <- topia_simulate(regions = REG_SIM, calendar = "s4_h24",
                      clusters = .sim_roster(measured), measured = measured)
  topia_sim <<- list(
    weather = data.frame(calendar = "s4_h24", p$weather,
                         stringsAsFactors = FALSE),
    demand = data.frame(calendar = "s4_h24", p$demand,
                        stringsAsFactors = FALSE))
})

stopifnot(nrow(topia_sim$weather) > 0, nrow(topia_sim$demand) > 0,
          max(topia_sim$weather$wval) <= 1, min(topia_sim$weather$wval) >= 0)
