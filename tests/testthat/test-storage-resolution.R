# =========================================================================== #
# A storage capacity is a RATE, not a per-timeslice quantity.
#
# The flow bounds used to read
#
#     vStorageInp <= cinp.up * vStorageInpCap
#
# with no duration factor, so `vStorageInpCap` meant "commodity per TIMESLICE".
# The same storage written `cap.fx = 7` then allowed 7 per hour on a 24 x 1-hour
# calendar and 1.75 per hour on a 6 x 4-hour one: refine an hourly model to
# 4-hourly and every store silently became a quarter as powerful. The technology
# form never had this problem -- `eqTechAfUp` carries
# `pTechCap2act * pTimesliceShare` -- and the storage block was inconsistent even
# with ITSELF, since the standing-loss term is already
# `(pStorageStgEff)^(pTimesliceShare)`, i.e. share was already being read as
# elapsed time for decay while the capacities ignored it.
#
# The bounds now carry `cap2act * pTimesliceShare[s]`, and `cap2act` defaults to
# 8760, so a capacity reads as commodity per HOUR on any calendar.
#
# The ENERGY capacity deliberately gets no such factor -- an hour is an hour and
# MWh are MWh, which is also exactly what PyPSA does (`soc <= e_nom` carries no
# elapsed-hours weighting; only the accumulation equation does).
# =========================================================================== #

HOURS_PER_YEAR <- 8760

# One storage spec, one number, several calendar resolutions. `cinp.fx` FORCES
# the flow onto its bound, so this measures the constraint itself rather than
# whatever the optimiser happens to find useful.
sr_rate_per_hour <- function(nslice, nm) {
  sl <- sprintf("t%04d", seq_len(nslice))
  cal <- newCalendar(
    timetable = make_timetable(struct = list(ANNUAL = "ANNUAL", HOUR = sl)),
    name = nm)
  mod <- newModel(
    name = nm, region = "R1", horizon = newHorizon(2020), discount = 0,
    calendar = cal,
    repo = newRepository(paste0("r", nm),
      newCommodity("A", timeframe = "HOUR"),
      newCommodity("B", timeframe = "HOUR"),
      newSupply("SA", commodity = "A", supply = data.frame(cost = 1)),
      newStorage("S",
        input    = list(comm = "A"),
        storage  = list(comm = "B"),
        output   = list(comm = "B"),
        af       = data.frame(cinp.fx = 1),
        capacity = list(inp.cap.fx = 7, stg.cap.up = 1e9, out.cap.up = 1e9),
        duration = data.frame(duration.lo = 0, duration.up = 1e9),
        vintage  = data.frame(olife = 10L)),
      # B has free disposal (limtype LO), so the forced throughput needs no sink
      newDemand("D", commodity = "B", demand = data.frame(demand = 0))))
  scen <- suppressMessages(suppressWarnings(solve_model(mod, name = nm)))
  inp <- as.data.frame(getData(scen, "vStorageInp", merge = TRUE, drop.zeros = TRUE))
  share <- as.data.frame(getCalendar(scen)@timeslice_share)
  hours <- share$share[match(sl[1], share$timeslice)] * HOURS_PER_YEAR
  max(inp$value) / hours
}

test_that("a storage capacity means the same power at every resolution", {
  skip_if_no_solver()
  # 8760 x 1h, 2190 x 4h, 365 x 24h -- the same `cap.fx = 7` throughout
  got <- c(hourly = sr_rate_per_hour(8760L, "sr_h"),
           four   = sr_rate_per_hour(2190L, "sr_4h"),
           daily  = sr_rate_per_hour(365L,  "sr_d"))
  expect_equal(unname(got), rep(7, 3), tolerance = 1e-8)
  # the property, stated directly: refining the calendar must not move the number
  expect_equal(max(got) / min(got), 1, tolerance = 1e-8)
})

test_that("the ENERGY capacity is NOT rescaled by resolution", {
  skip_if_no_solver()
  # `capacity = list(stg.cap.fx = )` is MWh and must mean the same MWh on any
  # calendar. If it were given the share factor too, a coarse calendar would
  # inflate the store instead of leaving it alone.
  lvl <- function(nslice, nm) {
    sl <- sprintf("t%04d", seq_len(nslice))
    cal <- newCalendar(timetable = make_timetable(struct = list(
      ANNUAL = "ANNUAL", HOUR = sl)), name = nm)
    mod <- newModel(
      name = nm, region = "R1", horizon = newHorizon(2020), discount = 0,
      calendar = cal,
      repo = newRepository(paste0("r", nm),
        newCommodity("A", timeframe = "HOUR"),
        newCommodity("B", timeframe = "HOUR"),
        newSupply("SA", commodity = "A", supply = data.frame(cost = 1)),
        newStorage("S",
          input    = list(comm = "A"),
          storage  = list(comm = "B"),
          output   = list(comm = "B"),
          capacity = list(stg.cap.fx = 60, out.cap.up = 1e9),
          duration = data.frame(duration.lo = 0, duration.up = 1e9),
          vintage  = data.frame(olife = 10L)),
        newDemand("D", commodity = "B", demand = data.frame(demand = 0))))
    scen <- suppressMessages(suppressWarnings(solve_model(mod, name = nm)))
    d <- as.data.frame(getData(scen, "vStorageStgCap", merge = TRUE,
                               drop.zeros = FALSE))
    max(d$value)
  }
  expect_equal(lvl(365L, "sre_d"), 60, tolerance = 1e-8)
  expect_equal(lvl(24L,  "sre_24"), 60, tolerance = 1e-8)
})

test_that("`duration` now reads as HOURS", {
  skip_if_no_solver()
  # With cap2act at its 8760 default the discharger is a per-hour rate and the
  # reservoir is energy, so their ratio is an hour count -- which is what
  # `duration` has always claimed to be and, before this change, was not.
  sl <- sprintf("t%04d", seq_len(365L))
  cal <- newCalendar(timetable = make_timetable(struct = list(
    ANNUAL = "ANNUAL", HOUR = sl)), name = "sr_dur")
  mod <- newModel(
    name = "sr_dur", region = "R1", horizon = newHorizon(2020), discount = 0,
    calendar = cal,
    repo = newRepository("rsr_dur",
      newCommodity("A", timeframe = "HOUR"),
      newCommodity("B", timeframe = "HOUR"),
      newSupply("SA", commodity = "A", supply = data.frame(cost = 1)),
      newStorage("S",
        input    = list(comm = "A"),
        storage  = list(comm = "B"),
        output   = list(comm = "B"),
        capacity = list(stg.cap.up = 1e9,
                        out.cap.fx = 10),  # 10 per hour of discharge
        duration = 6,                      # a SIX-HOUR store
        vintage  = data.frame(olife = 10L)),
      newDemand("D", commodity = "B", demand = data.frame(demand = 0))))
  scen <- suppressMessages(suppressWarnings(solve_model(mod, name = "sr_dur")))
  stg <- as.data.frame(getData(scen, "vStorageStgCap", merge = TRUE,
                               drop.zeros = FALSE))
  out <- as.data.frame(getData(scen, "vStorageOutCap", merge = TRUE,
                               drop.zeros = FALSE))
  expect_equal(max(out$value), 10, tolerance = 1e-8)
  expect_equal(max(stg$value), 60, tolerance = 1e-8)   # 6 h x 10 per hour
})
