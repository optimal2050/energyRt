# =========================================================================== #
# Feature family: tech-aux-flows (aeff -- auxiliary commodity flows driven by
# activity, capacity, new capacity, and main-commodity flows).
#
# Mini-model: ECOA COA->ELC (invcost 1000, olife 30, cap2act 1), flat ELC
# demand 10 x 4 slices (act 40, cap 40), COA at cost 1, WAT aux at cost 2,
# HET aux output (free disposal). One year, discount 0. Base obj 1373.333.
#
# Verified conventions (aux lands at the TECH's own timeslices; an ANNUAL aux
# commodity aggregates through eqTechInpTot's Agg branch):
#   act2ainp  r: aux_s = r * act_s            (0.02 -> 0.2/slice, +1.6  cost)
#   cap2ainp  r: aux_s = r * cap / cap2act    (0.10 -> 4/slice,   +32   cost)
#   ncap2ainp r: aux_s = r * newcap           (0.05 -> 2/slice,   +16   cost)
#   cinp2ainp r: aux_s = r * main input_s     (0.50 -> 5/slice,   +40   cost)
#   act2aout / cout2aout: symmetric on the output side (free disposal here)
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

ax_case <- function(aeff, tag, param, aux_per_slice, extra_cost,
                    aux_var = "vTechAInp") {
  scen <- suppressMessages(suppressWarnings(interpolate_model(
    ax_build(aeff), name = tag, overwrite = TRUE)))
  p <- ff_param(scen, param)
  expect_false(is.null(p) || nrow(p) == 0, label = paste0(tag, " ", param, " rows"))
  scen <- .fork_solve(scen, solver_options$glpk)
  expect_true(verify_solution(scen)$ok, label = paste0(tag, " invariants"))
  av <- ff_solution_sum(scen, aux_var, by = "timeslice")
  expect_equal(av$value, rep(aux_per_slice, 4), tolerance = 1e-6,
               label = paste0(tag, " aux per slice"))
  expect_equal(.fork_objective(scen), ax_base_obj + extra_cost, tolerance = 1e-6,
               label = paste0(tag, " objective"))
}

# @covers pTechAct2AInp vTechAInp eqTechAInp depth=S backends=glpk
test_that("family tech-aux-flows: activity-driven aux input", {
  skip_if_no_solver()
  ax_case(data.frame(acomm = "WAT", act2ainp = 0.02), "ax_act",
          "pTechAct2AInp", 0.2, 1.6)
})

# @covers pTechCap2AInp depth=S backends=glpk
test_that("family tech-aux-flows: capacity-driven aux input", {
  skip_if_no_solver()
  ax_case(data.frame(acomm = "WAT", cap2ainp = 0.10), "ax_cap",
          "pTechCap2AInp", 4, 32)
})

# @covers pTechNCap2AInp depth=S backends=glpk
test_that("family tech-aux-flows: new-capacity-driven aux input", {
  skip_if_no_solver()
  ax_case(data.frame(acomm = "WAT", ncap2ainp = 0.05), "ax_ncap",
          "pTechNCap2AInp", 2, 16)
})

# @covers pTechCinp2AInp depth=S backends=glpk
test_that("family tech-aux-flows: input-commodity-driven aux input", {
  skip_if_no_solver()
  ax_case(data.frame(acomm = "WAT", comm = "COA", cinp2ainp = 0.5), "ax_cinp",
          "pTechCinp2AInp", 5, 40)
})

# @covers pTechAct2AOut vTechAOut eqTechAOut depth=S backends=glpk
test_that("family tech-aux-flows: activity-driven aux output (free disposal)", {
  skip_if_no_solver()
  ax_case(data.frame(acomm = "HET", act2aout = 0.25), "ax_actout",
          "pTechAct2AOut", 2.5, 0, aux_var = "vTechAOut")
})

# @covers pTechCout2AOut depth=S backends=glpk
test_that("family tech-aux-flows: output-commodity-driven aux output", {
  skip_if_no_solver()
  ax_case(data.frame(acomm = "HET", comm = "ELC", cout2aout = 0.3), "ax_cout",
          "pTechCout2AOut", 3, 0, aux_var = "vTechAOut")
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

# ---- cap2act scaling of the capacity-driven aux flow ----------------------- #
# eqTechAInp / eqTechAOut are written multiplied through by pTechCap2act (no
# division in any coefficient), so the solved aux input must still satisfy
#   vTechAInp = act2ainp * vTechAct + cap2ainp / cap2act * vTechCap
# on a technology with cap2act = 8760. The written GLPK row carries no `/`.
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
    test_that(paste0("family tech-aux-flows: cap2ainp / cap2act identity on ", tag), {
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
      for (s in unique(ain$timeslice)) {
        a <- sum(act$value[act$timeslice == s])
        expect_equal(sum(ain$value[ain$timeslice == s]),
                     0.02 * a + 876 / 8760 * sum(cap$value),
                     tolerance = 1e-6, label = paste(tag, "aux in", s))
      }
    })
  })
}
