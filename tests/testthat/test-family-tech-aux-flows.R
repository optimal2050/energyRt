# =========================================================================== #
# Feature family: tech-aux-flows (aeff -- auxiliary commodity flows driven by
# activity, capacity, new capacity, and main-commodity flows).
#
# Mini-model: ECOA COA->ELC (invcost 1000, olife 30, cap2act 1), flat ELC
# demand 10 x 4 slices (act 40, cap 40), COA at cost 1, WAT aux at cost 2,
# HET aux output (free disposal). One year, discount 0. Base obj 1373.333.
#
# Conventions (the aux flow lands at the AUX COMMODITY's timeslices -- here WAT
# and HET are ANNUAL, so one row per year):
#   act2ainp  r: aux = r * sum_s act_s        (0.02 -> 0.8,  +1.6 cost)
#   cap2ainp  r: aux = r * cap, per YEAR      (0.10 -> 4,    +8   cost)
#   ncap2ainp r: aux = r * newcap, per YEAR   (0.05 -> 2,    +4   cost)
#   cinp2ainp r: aux = r * sum_s input_s      (0.50 -> 20,   +40  cost)
#   act2aout / cout2aout: symmetric on the output side (free disposal here)
# Capacity-type terms are booked as the aux slice's share of the year
# (pTimesliceShare, 1 at ANNUAL); flow-type terms sum the process's slices.
# =========================================================================== #

ax_slices <- paste0("s", 1:4)

ax_build <- function(aeff = data.frame()) {
  newModel("ax",
    repo = newRepository("ax_repo",
      newCommodity("COA", timeframe = "ANNUAL"),
      newCommodity("ELC", timeframe = "SL"),
      newCommodity("WAT", timeframe = "ANNUAL"),
      newCommodity("HET", timeframe = "ANNUAL"),
      newSupply("SCOA", commodity = "COA", supply = data.frame(cost = 1)),
      newSupply("SWAT", commodity = "WAT", supply = data.frame(cost = 2)),
      newTechnology("ECOA", input = list(comm = "COA"),
                    output = list(comm = "ELC"),
                    aux = data.frame(acomm = c("WAT", "HET")),
                    aeff = aeff,
                    invcost = list(invcost = 1000), olife = list(olife = 30),
                    cap2act = 1),
      newDemand("DEM", commodity = "ELC",
                demand = data.frame(timeslice = ax_slices, demand = 10))),
    calendar = newCalendar(
      timetable = make_timetable(struct = list(ANNUAL = "ANNUAL", SL = ax_slices)),
      name = "ax_cal"),
    region = "R1", horizon = newHorizon(2025), discount = 0)
}

ax_base_obj <- 40 * 1000 / 30 + 40

ax_case <- function(aeff, tag, param, aux_annual, extra_cost,
                    aux_var = "vTechAInp") {
  scen <- suppressMessages(suppressWarnings(interpolate_model(
    ax_build(aeff), name = tag, overwrite = TRUE)))
  p <- ff_param(scen, param)
  expect_false(is.null(p) || nrow(p) == 0, label = paste0(tag, " ", param, " rows"))
  scen <- .fork_solve(scen, solver_options$glpk)
  expect_true(verify_solution(scen)$ok, label = paste0(tag, " invariants"))
  av <- as.data.frame(ff_solution_sum(scen, aux_var, by = "timeslice"))
  expect_equal(as.character(av$timeslice), "ANNUAL",
               label = paste0(tag, " aux at the aux commodity's slice"))
  expect_equal(av$value, aux_annual, tolerance = 1e-6,
               label = paste0(tag, " aux per year"))
  expect_equal(.fork_objective(scen), ax_base_obj + extra_cost, tolerance = 1e-6,
               label = paste0(tag, " objective"))
}

# @covers pTechAct2AInp vTechAInp eqTechAInp depth=S backends=glpk
test_that("family tech-aux-flows: activity-driven aux input", {
  skip_if_no_solver()
  ax_case(data.frame(acomm = "WAT", act2ainp = 0.02), "ax_act",
          "pTechAct2AInp", 0.8, 1.6)
})

# @covers pTechCap2AInp depth=S backends=glpk
test_that("family tech-aux-flows: capacity-driven aux input", {
  skip_if_no_solver()
  ax_case(data.frame(acomm = "WAT", cap2ainp = 0.10), "ax_cap",
          "pTechCap2AInp", 4, 8)
})

# @covers pTechNCap2AInp depth=S backends=glpk
test_that("family tech-aux-flows: new-capacity-driven aux input", {
  skip_if_no_solver()
  ax_case(data.frame(acomm = "WAT", ncap2ainp = 0.05), "ax_ncap",
          "pTechNCap2AInp", 2, 4)
})

# @covers pTechCinp2AInp depth=S backends=glpk
test_that("family tech-aux-flows: input-commodity-driven aux input", {
  skip_if_no_solver()
  ax_case(data.frame(acomm = "WAT", comm = "COA", cinp2ainp = 0.5), "ax_cinp",
          "pTechCinp2AInp", 20, 40)
})

# @covers pTechAct2AOut vTechAOut eqTechAOut depth=S backends=glpk
test_that("family tech-aux-flows: activity-driven aux output (free disposal)", {
  skip_if_no_solver()
  ax_case(data.frame(acomm = "HET", act2aout = 0.25), "ax_actout",
          "pTechAct2AOut", 10, 0, aux_var = "vTechAOut")
})

# @covers pTechCout2AOut depth=S backends=glpk
test_that("family tech-aux-flows: output-commodity-driven aux output", {
  skip_if_no_solver()
  ax_case(data.frame(acomm = "HET", comm = "ELC", cout2aout = 0.3), "ax_cout",
          "pTechCout2AOut", 12, 0, aux_var = "vTechAOut")
})

# @covers pTechCout2AInp vTechAInp eqTechAInp depth=S backends=glpk
test_that("family tech-aux-flows: output-commodity-driven aux INPUT", {
  skip_if_no_solver()
  # the aux INPUT side driven by the main OUTPUT: 0.3 x 40 = 12 units of WAT
  ax_case(data.frame(acomm = "WAT", comm = "ELC", cout2ainp = 0.3),
          "ax_coutin", "pTechCout2AInp", 12, 24)
})

# @covers pTechCinp2AOut vTechAOut eqTechAOut depth=S backends=glpk
test_that("family tech-aux-flows: input-commodity-driven aux OUTPUT", {
  skip_if_no_solver()
  # the aux OUTPUT side driven by the main INPUT: 0.3 x 40 = 12 units of HET,
  # free disposal so the objective does not move
  ax_case(data.frame(acomm = "HET", comm = "COA", cinp2aout = 0.3),
          "ax_cinpout", "pTechCinp2AOut", 12, 0, aux_var = "vTechAOut")
})

# @covers pTechCap2AOut pTechNCap2AOut pTechCout2AInp pTechCinp2AOut depth=I backends=glpk
test_that("family tech-aux-flows: remaining direction ratios land in modInp", {
  scen <- suppressMessages(suppressWarnings(interpolate_model(
    ax_build(rbind(
      data.frame(acomm = "HET", comm = NA, cap2aout = 0.01, ncap2aout = NA,
                 cout2ainp = NA, cinp2aout = NA),
      data.frame(acomm = "HET", comm = NA, cap2aout = NA, ncap2aout = 0.02,
                 cout2ainp = NA, cinp2aout = NA),
      data.frame(acomm = "WAT", comm = "ELC", cap2aout = NA, ncap2aout = NA,
                 cout2ainp = 0.03, cinp2aout = NA),
      data.frame(acomm = "HET", comm = "COA", cap2aout = NA, ncap2aout = NA,
                 cout2ainp = NA, cinp2aout = 0.04))),
    name = "ax_rest", overwrite = TRUE)))
  expect_equal(unique(ff_param(scen, "pTechCap2AOut")$value), 0.01)
  expect_equal(unique(ff_param(scen, "pTechNCap2AOut")$value), 0.02)
  expect_equal(unique(ff_param(scen, "pTechCout2AInp")$value), 0.03)
  expect_equal(unique(ff_param(scen, "pTechCinp2AOut")$value), 0.04)
})

# ---- cap2ainp is a per-year coefficient, independent of cap2act ------------ #
# On a technology with cap2act = 8760 and an ANNUAL aux commodity the solved
# aux input must satisfy
#   vTechAInp(ANNUAL) = act2ainp * sum_s vTechAct_s + cap2ainp * vTechCap
# -- cap2act does not enter. The written GLPK row carries no `/`.
ax_build_c2a <- function(cap2act = 8760) {
  newModel("axc",
    repo = newRepository("axc_repo",
      newCommodity("COA", timeframe = "ANNUAL"),
      newCommodity("ELC", timeframe = "SL"),
      newCommodity("WAT", timeframe = "ANNUAL"),
      newSupply("SCOA", commodity = "COA", supply = data.frame(cost = 1)),
      newSupply("SWAT", commodity = "WAT", supply = data.frame(cost = 2)),
      newTechnology("ECOA", input = list(comm = "COA"),
                    output = list(comm = "ELC"),
                    aux = data.frame(acomm = "WAT"),
                    aeff = data.frame(acomm = "WAT", act2ainp = 0.02,
                                      cap2ainp = 876),
                    invcost = list(invcost = 1000), olife = list(olife = 30),
                    cap2act = cap2act),
      newDemand("DEM", commodity = "ELC",
                demand = data.frame(timeslice = ax_slices, demand = 10))),
    calendar = newCalendar(
      timetable = make_timetable(struct = list(ANNUAL = "ANNUAL", SL = ax_slices)),
      name = "axc_cal"),
    region = "R1", horizon = newHorizon(2025), discount = 0)
}

test_that("the written aux-flow rows divide by nothing", {
  scen <- suppressMessages(suppressWarnings(
    interpolate_model(ax_build_c2a(), name = "axc_w", overwrite = TRUE)))
  d <- withr::local_tempdir()
  write_script(scen, solver.dir = d, solver = solver_options$glpk)
  src <- readLines(file.path(d, "energyRt.mod"), warn = FALSE)
  rows <- grep("^s[.]t[.]  eq(TechAInp|TechAOut)", src, value = TRUE)
  expect_length(rows, 2L)
  expect_false(any(grepl(") / (", rows, fixed = TRUE)))
  # the parameter itself is untouched: cap2ainp as declared
  p <- as.data.frame(get_data_slot(scen@modInp@parameters$pTechCap2AInp))
  expect_equal(unique(p$value), 876)
})

.axc_backends <- list(
  glpk = list(solver = quote(solver_options$glpk), skip = quote(skip_if_no_solver())),
  julia_highs = list(solver = quote(solver_options$julia_highs),
                     skip = quote(skip_if_no_julia_highs())),
  pyomo_cbc = list(solver = quote(solver_options$pyomo_cbc),
                   skip = quote(skip_if_no_pyomo())))

for (bk in names(.axc_backends)) {
  local({
    spec <- .axc_backends[[bk]]; tag <- bk
    # @covers pTechCap2AInp pTechCap2act eqTechAInp depth=X backends=glpk,julia_highs,pyomo_cbc
    test_that(paste0("family tech-aux-flows: per-year cap2ainp identity on ", tag), {
      eval(spec$skip)
      scen <- suppressMessages(suppressWarnings(
        interpolate_model(ax_build_c2a(), name = paste0("axc_", tag),
                          overwrite = TRUE)))
      scen <- suppressMessages(suppressWarnings(
        solve_scenario(scen, solver = eval(spec$solver), force = TRUE,
                       solver.dir = withr::local_tempdir())))
      expect_true(verify_solution(scen)$ok)
      act <- as.data.frame(getData(scen, "vTechAct", merge = TRUE))
      cap <- as.data.frame(getData(scen, "vTechCap", merge = TRUE))
      ain <- as.data.frame(getData(scen, "vTechAInp", merge = TRUE))
      expect_gt(sum(cap$value), 0)
      expect_equal(unique(as.character(ain$timeslice)), "ANNUAL")
      expect_equal(sum(ain$value),
                   0.02 * sum(act$value) + 876 * sum(cap$value),
                   tolerance = 1e-6, label = paste(tag, "aux per year"))
    })
  })
}
