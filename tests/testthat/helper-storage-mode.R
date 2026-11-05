# =========================================================================== #
# Fixtures for the storage-mode invariance suite
# (test-storage-mode-invariance.R).
#
# A scenario's tables live either in memory or in its own on-disk store:
# `save_scenario()` moves every table above `energyRt.ondisk_min_rows` out of
# `@data` and leaves the slot empty. Both are normal states, and a READER
# must not be able to tell them apart.
#
# The core invariant: for one and the same scenario, every query returns the
# same answer in both storage modes. A reader that reaches into `@data`
# instead of the package's accessors breaks it -- and breaks it SILENTLY,
# reading an empty table as "nothing to report" rather than failing, which is
# why this is a suite rather than a single test.
#
# The fixture pair is deliberately built from ONE interpolation, so a
# difference can only come from the storage mode and never from the build.
# =========================================================================== #

# A small model that still carries the structures a reader can trip over: a
# bounded supply, a demand, an aux commodity drawn per unit of STANDING
# capacity (the coupling `check_feasibility()` reads), vintages (so variant
# columns exist) and a user constraint whose `for.each` is scoped against the
# milestone maps (the reads in `class-constraint.R`).
sm_model <- function(name = "SM", demand = 5, land = 1000, per_gw = 2,
                     optimizeRetirement = FALSE, ret_up = NA_real_) {
  cal <- newCalendar(
    timetable = make_timetable(struct = list(ANNUAL = "ANNUAL",
                                             SL = sprintf("s%02d", 1:4))),
    name = paste0("cal_", name))
  newModel(
    name = name, desc = "storage-mode fixture", calendar = cal,
    region = "R1", horizon = newHorizon(c(2025, 2030, 2035)), discount = 0,
    # the GLOBAL gate; a process's own slot is checked on top of it
    optimizeRetirement = optimizeRetirement,
    repo = newRepository(
      paste0("repo_", name),
      newCommodity("GAS", timeframe = "ANNUAL",
                   emis = data.frame(comm = "CO2", emis = 1)),
      newCommodity("CO2", timeframe = "ANNUAL"),
      newCommodity("ELC", timeframe = "ANNUAL"),
      newCommodity("LAND", timeframe = "ANNUAL"),
      newSupply("SUP_GAS", commodity = "GAS", unit = "PJ",
                supply = data.frame(region = "R1", cost = 1, ava.up = 1e4)),
      newSupply("RES_LAND", commodity = "LAND", unit = "km2",
                supply = data.frame(region = "R1", cost = 0, ava.up = land)),
      newDemand("DEM_ELC", commodity = "ELC", unit = "GWh",
                demand = data.frame(region = "R1", demand = demand)),
      newTechnology(
        "EGAS",
        input = data.frame(comm = "GAS", unit = "PJ"),
        output = data.frame(comm = "ELC", unit = "GWh"),
        aux = data.frame(acomm = "LAND", unit = "km2"),
        aeff = data.frame(acomm = "LAND", cap2ainp = per_gw),
        cap2act = 1,
        # a vintage LABEL is character; `start`/`olife` are the integers
        vintage = data.frame(vintage = c("2025", "2030"),
                             start = c(2025L, 2030L), olife = 30L),
        optimizeRetirement = optimizeRetirement,
        capacity = if (is.na(ret_up)) {
          data.frame(region = "R1", year = 2025, stock = 1)
        } else {
          data.frame(region = "R1", year = 2025, stock = 1, ret.up = ret_up)
        },
        ceff = data.frame(comm = "GAS", cinp2use = 1))))
}

# A cap on CO2 over the milestones, so the constraint builder's milestone
# reads are exercised -- those are the reads that go through the modInp's
# parameters and can meet an on-disk table under `interpolate_model(base =)`.
sm_cap <- function(rhs = 1e6) {
  newConstraint(
    name = "SM_CAP", desc = "CO2 cap", eq = "<=",
    for.each = data.frame(year = c(2025L, 2030L, 2035L), comm = "CO2"),
    term1 = list(variable = "vEmsFuelTot"),
    rhs = data.frame(year = c(2025L, 2035L), rhs = c(rhs, rhs)),
    defVal = 1e10, replace_zerros = NULL, interpolation = "inter.forth")
}

# The pair under test. ONE interpolation, then the same scenario saved with
# every table pushed to the store and read back, so `mem` and `disk` differ
# in storage mode alone.
#
# `env` receives the path cleanup, as the other suites' helpers do.
sm_pair <- function(..., env = parent.frame(), name = "SM") {
  pr_local(env = env)
  old_sp <- set_scenarios_path(pr_root("scenarios"))
  do.call(on.exit, list(bquote(set_scenarios_path(.(old_sp))), add = TRUE),
          envir = env)
  mem <- suppressMessages(interpolate_model(sm_model(name = name), ...,
                                            name = name, verbose = FALSE))
  disk <- withr::with_options(
    list(energyRt.ondisk_min_rows = 1L, energyRt.param_ref_min_rows = 1e12),
    {
      s <- suppressMessages(save_scenario(mem, verbose = FALSE))
      suppressMessages(load_scenario(s@path, env = NULL, verbose = FALSE))
    })
  list(mem = mem, disk = disk)
}

# Parameters with a table worth comparing, in the order the modInp holds them.
sm_params <- function(scen, min_rows = 1L) {
  nm <- names(scen@modInp@parameters)
  keep <- vapply(nm, function(k) {
    d <- tryCatch(get_data_slot(scen@modInp@parameters[[k]], optional = TRUE),
                  error = function(e) NULL)
    !is.null(d) && NROW(d) >= min_rows
  }, logical(1))
  nm[keep]
}

# How many of a scenario's parameters hold their table in the SLOT.
sm_n_in_slot <- function(scen, params = sm_params(scen)) {
  sum(vapply(params, function(k) {
    d <- tryCatch(scen@modInp@parameters[[k]]@data, error = function(e) NULL)
    !is.null(d) && NROW(d) > 0
  }, logical(1)))
}

# A table, normalised for comparison: the parquet round-trip does not promise
# row order, and `getData()` labels its rows.
sm_norm <- function(d) {
  if (is.null(d)) return(NULL)
  d <- as.data.frame(d)
  d <- d[, setdiff(names(d), c("scenario", "name")), drop = FALSE]
  if (!nrow(d)) return(d)
  d <- d[do.call(order, unname(as.list(d))), , drop = FALSE]
  rownames(d) <- NULL
  d
}
