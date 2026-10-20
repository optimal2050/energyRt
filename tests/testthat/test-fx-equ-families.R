# =========================================================================== #
# fx_equ across the bound families.
#
# A cell whose lower and upper bound coincide (`.fx`, or lo == up) is written
# as one equality row, eq<Fam>Fx, instead of eq<Fam>Lo + eq<Fam>Up. For each
# small model below: the Fx maps are filled, the Lo/Up maps lose those cells,
# and the solution is the same with `fx_equ` on and off.
# =========================================================================== #

fe_cal <- function(name) {
  newCalendar(timetable = make_timetable(struct = list(ANNUAL = "ANNUAL", HOUR = c("h1", "h2"))),
              name = paste0("cal_", name))
}

fe_tech <- function(name) {
  newModel(name = name, region = "R1", horizon = newHorizon(2025), discount = 0, calendar = fe_cal(name),
    repo = newRepository(paste0("r_", name),
      newCommodity("ELC", timeframe = "HOUR"),
      newTechnology("GT", output = list(comm = "ELC"), cap2act = 1,
                    capacity = data.frame(cap.fx = 10),                         # TechCap
                    afs = data.frame(timeslice = "ANNUAL", afs.fx = 0.5),       # TechAfs
                    ceff = data.frame(comm = "ELC", afc.fx = 0.5)),             # TechAfcOut
      newImport("IMP", commodity = "ELC", import = data.frame(price = 100, imp.up = 1000)),
      newDemand("DEM", commodity = "ELC", demand = data.frame(timeslice = c("h1", "h2"), demand = 3))))
}

fe_sup <- function(name) {
  newModel(name = name, region = "R1", horizon = newHorizon(2025), discount = 0, calendar = fe_cal(name),
    repo = newRepository(paste0("r_", name),
      newCommodity("ELC", timeframe = "HOUR"),
      newSupply("SUP", commodity = "ELC", supply = data.frame(cost = 1, ava.fx = 2)),   # SupAva
      newImport("IMP", commodity = "ELC", import = data.frame(price = 100, imp.up = 1000)),
      newDemand("DEM", commodity = "ELC", demand = data.frame(timeslice = c("h1", "h2"), demand = 3))))
}

fe_row <- function(name) {
  newModel(name = name, region = "R1", horizon = newHorizon(2025), discount = 0,
    calendar = newCalendar(timetable = make_timetable(struct = list(ANNUAL = "ANNUAL")),
                           name = paste0("cal_", name)),
    repo = newRepository(paste0("r_", name),
      newCommodity("ELC", timeframe = "ANNUAL"),
      newCommodity("GAS", timeframe = "ANNUAL"),
      newDemand("DEM", commodity = "ELC", demand = data.frame(demand = 100)),
      newImport("IMP", commodity = "ELC", import = data.frame(price = 3, imp.fx = 100)),  # ImportRow
      newSupply("SUP", commodity = "GAS", supply = data.frame(cost = 2)),
      newExport("EXP", commodity = "GAS", export = data.frame(price = 5, exp.fx = 30))))  # ExportRow
}

fe_interp <- function(build, name, fx_equ) {
  suppressMessages(suppressWarnings(
    interpolate_model(build(name), name = name, fx_equ = fx_equ, overwrite = TRUE)))
}

fe_rows <- function(scen, map) {
  m <- .gds(scen, map)
  if (is.null(m)) 0L else nrow(m)
}

fe_cases <- list(
  tech = list(build = fe_tech, fx = c("mTechCapFx", "meqTechAfsFx", "meqTechAfcOutFx"),
              lo = c("mTechCapLo", "meqTechAfsLo", "meqTechAfcOutLo")),
  sup = list(build = fe_sup, fx = "meqSupAvaFx", lo = "meqSupAvaLo"),
  row = list(build = fe_row, fx = c("meqImportRowFx", "meqExportRowFx"),
             lo = c("meqImportRowLo", "meqExportRowLo")),
  trade = list(build = function(name) tr_model(data.frame(src = "R1", dst = "R2"), cap = 100, name = name),
               fx = "mTradeCapFx", lo = "mTradeCapLo")
)

# @covers mTechCapFx meqTechAfsFx meqTechAfcOutFx meqSupAvaFx meqImportRowFx meqExportRowFx mTradeCapFx depth=I
test_that("fx_equ moves equal-sided cells to the Fx maps, family by family", {
  for (k in names(fe_cases)) {
    cs <- fe_cases[[k]]
    on <- fe_interp(cs$build, paste0("fe_", k, "_on"), TRUE)
    off <- fe_interp(cs$build, paste0("fe_", k, "_off"), FALSE)
    for (i in seq_along(cs$fx)) {
      n <- fe_rows(off, cs$lo[i])
      expect_gt(n, 0, label = paste(k, cs$lo[i], "off"))
      expect_equal(fe_rows(on, cs$fx[i]), n, label = paste(k, cs$fx[i], "on"))
      expect_equal(fe_rows(on, cs$lo[i]), 0L, label = paste(k, cs$lo[i], "on"))
      expect_equal(fe_rows(off, cs$fx[i]), 0L, label = paste(k, cs$fx[i], "off"))
    }
  }
})

# @covers mTechCapFx meqTechAfsFx meqTechAfcOutFx meqSupAvaFx meqImportRowFx meqExportRowFx mTradeCapFx depth=S backends=glpk
test_that("fx_equ does not change the solution (GLPK)", {
  skip_if_no_solver()
  for (k in names(fe_cases)) {
    cs <- fe_cases[[k]]
    on <- .fork_solve(fe_interp(cs$build, paste0("fes_", k, "_on"), TRUE), solver_options$glpk)
    off <- .fork_solve(fe_interp(cs$build, paste0("fes_", k, "_off"), FALSE), solver_options$glpk)
    expect_false(is.na(.fork_objective(off)), label = paste(k, "solved"))
    expect_equal(.fork_objective(on), .fork_objective(off), tolerance = 1e-8, label = k)
  }
})

# @covers mTechCapFx meqSupAvaFx meqImportRowFx mTradeCapFx depth=S backends=julia,pyomo,mm
test_that("fx_equ gives the GLPK objective on JuMP, Pyomo and multimod", {
  skip_if_no_solver()
  for (k in names(fe_cases)) {
    cs <- fe_cases[[k]]
    ref <- .fork_objective(.fork_solve(fe_interp(cs$build, paste0("fer_", k), FALSE), solver_options$glpk))
    for (b in list(list("julia", skip_if_no_julia_highs, solver_options$julia_highs),
                   list("pyomo", skip_if_no_pyomo, solver_options$pyomo_highs),
                   list("mm", skip_if_no_multimod, solver_options$mm_highs))) {
      ok <- tryCatch({ b[[2]](); TRUE }, skip = function(e) FALSE)
      if (!ok) next
      got <- .fork_objective(.fork_solve(fe_interp(cs$build, paste0("fe_", k, "_", b[[1]]), TRUE), b[[3]]))
      expect_equal(got, ref, tolerance = 1e-6, label = paste(k, b[[1]]))
    }
  }
})

# A folded bound can carry an NA wildcard on some rows only (here the battery's
# region): it must still match, or the cell reads the defaults (1 = 1 for
# inp2out) and an open ratio (0 / Inf) is wrongly fixed -- vStorageInpCap == 0.
# @covers mStorageInp2outFx depth=I
test_that("row-wise NA wildcards in a bound are matched, most specific first", {
  src <- data.frame(stg = c("HYD", "HYD", "BAT", "BAT", "PHS"),
                    region = c("BE", "BE", NA, NA, "BE"), year = 2025,
                    type = c("lo", "up", "lo", "up", "lo"), value = c(0, Inf, 0, Inf, 2))
  cells <- data.frame(stg = c("BAT", "PHS", "NEW"), region = "BE", year = 2025)
  expect_equal(.bound_at(src, cells, "lo", 1), c(0, 2, 1))
  expect_equal(.bound_at(src, cells, "up", 1), c(Inf, 1, 1))
  # a specific row beats a wildcard row for the same entity
  src2 <- rbind(src, data.frame(stg = "BAT", region = "BE", year = 2025, type = "lo", value = 5))
  expect_equal(.bound_at(src2, cells[1, ], "lo", 1), 5)
})
