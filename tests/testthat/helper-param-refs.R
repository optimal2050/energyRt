# Fixtures shared by test-param-refs.R and test-map-rules.R: a scratch project
# (datasets, registry, models, scenarios, repositories under tempdir()), small
# parameter tables, and a one-technology model with four timeslices.

pr_root <- function(...) {
  gsub("[\\/]+", "/", file.path(tempdir(), "param-refs-suite", ...))
}

pr_local <- function(env = parent.frame()) {
  old_dp <- set_datasets_path(pr_root("datasets"))
  old_rf <- set_registry_file(pr_root("reg.csv"))
  expr <- bquote({
    set_datasets_path(.(old_dp))
    set_registry_file(.(old_rf))
  })
  do.call(on.exit, list(expr, add = TRUE), envir = env)
  invisible(NULL)
}

pr_param <- function(name, data) {
  p <- new("modInp")@parameters[[name]]
  stopifnot(!is.null(p))
  p@data <- data.table::as.data.table(data)
  p
}

pr_weather <- function(scale = 1) {
  data.frame(weather = "WSOL", region = rep(c("R1", "R2"), each = 4L),
             year = 2025L, timeslice = rep(sprintf("t%02d", 1:4), 2L),
             value = scale * c(0.1, 0.5, 0.8, 0.3, 0.2, 0.6, 0.7, 0.2),
             stringsAsFactors = FALSE)
}

pr_af <- function(scale = 1) {
  data.frame(tech = "TPV", region = rep(c("R1", "R2"), each = 4L), year = 2025L,
             timeslice = rep(sprintf("t%02d", 1:4), 2L), type = "up",
             value = scale * c(0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2),
             stringsAsFactors = FALSE)
}

pr_scen <- function(...) {
  mi <- new("modInp")
  mi@parameters <- list(...)
  mi <- mark_inMemory(mi)
  scen <- new("scenario", name = "prtest")
  scen@modInp <- mi
  scen@status$sparse <- TRUE
  scen
}

pr_slices <- sprintf("s%02d", 1:4)

pr_repo <- function(wvals = c(1, 0.5, 0.25, 0.2)) {
  newRepository(
    "prr",
    newCommodity("ELC", timeframe = "SL"),
    newWeather("WCF", timeframe = "SL",
               weather = data.frame(timeslice = pr_slices, wval = wvals)),
    newTechnology("WIND", output = list(comm = "ELC"),
                  weather = data.frame(weather = "WCF", waf.up = 1),
                  invcost = list(invcost = 1000), olife = list(olife = 30),
                  cap2act = 1),
    newDemand("DEM", commodity = "ELC",
              demand = data.frame(timeslice = pr_slices, demand = 10)))
}

pr_model <- function(name = "prm") {
  cal <- newCalendar(timetable = make_timetable(
    struct = list(ANNUAL = "ANNUAL", SL = pr_slices)), name = "prc")
  newModel(name, region = "R1", discount = 0, calendar = cal,
           horizon = newHorizon(2025), repo = pr_repo())
}

pr_local_model <- function(env = parent.frame()) {
  pr_local(env = env)
  old_mp <- set_models_path(pr_root("models"))
  old_sp <- set_scenarios_path(pr_root("scenarios"))
  old_rp <- set_repositories_path(pr_root("repositories"))
  expr <- bquote({
    set_models_path(.(old_mp))
    set_scenarios_path(.(old_sp))
    set_repositories_path(.(old_rp))
  })
  do.call(on.exit, list(expr, add = TRUE), envir = env)
  invisible(NULL)
}
