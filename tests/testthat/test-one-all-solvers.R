# Config/testthat/edition: 3
# Config/testthat/parallel: true
# usethis::edit_r_environ()
# TESTTHAT_CPUS=4
# Solver paths come from the energyRt config file (`~/.energyRt/config.yml`),
# applied automatically at load. Inspect with `en_config_show()`; write with
# `en_config_write()`. This used to `source("~/.energyRt/settings.R")`, a path
# the package itself never read.
try(get_python_path(), silent = TRUE)

h10 <- make_timetable(
  struct = list(
    ANNUAL = "ANNUAL",
    HOUR = paste0("h", 0:9))
)

calendar_h10 <- newCalendar(timetable = h10, name = "h10")

INP <- newCommodity("INP", timeframe = "HOUR")
OUT <- newCommodity("OUT", timeframe = "HOUR")

SUP_INP <- newSupply(
  name = "SUP_INP",
  commodity = "INP",
  supply = data.frame(
    # region = NA,
    # year = NA,
    # timeslice = NA,
    cost = 1
  )
)

TECH <- newTechnology(
  name = "TECH",
  input = list(comm = "INP"),
  output = list(comm = "OUT"),
  invcost = list(invcost = 100),
  # fixom = list(fixom = 1),
  # varom = list(varom = .1),
  olife = list(olife = 10)
)

# draw(TECH)
DEM <- newDemand(
  name = "DEM",
  commodity = "OUT",
  demand = data.frame(
    # region = NA,
    # year = NA,
    # timeslice = NA,
    demand = 1
  )
)

repo <- newRepository("repo", INP, OUT, SUP_INP, TECH, DEM)

mod_unit <- newModel(
  name = "UNIT",
  desc = "Unit test model",
  repo = repo,
  calendar = calendar_h10,
  region = "REG",
  horizon = newHorizon(2010:2050),
  discount = 0
)

# mod_unit@config@calendar
# scen_unit <- solve(mod_unit, solver = solver_options$glpk)
# vObjective <- getData(scen_unit, "vObjective", merge = TRUE)$value
vObjective <- 4510

# @covers vObjective vTechAct vTechNewCap depth=S backends=glpk
test_that("ONE_glpk", {
  skip_if_no_solver()
  invisible({
    scen <- solve(mod_unit, solver = solver_options$glpk)
  })
  vObj <- getData(scen, "vObjective", merge = TRUE)$value
  expect_equal(vObjective, vObj)
})

# test_that("ONE_pyomo_highs", {
# not working (Windows)
#   invisible({
#     # scen <- solve(mod_unit, solver = solver_options$pyomo_cbc)
#     scen_ph <- interpolate(mod_unit)
#     pyomo_highs <- solver_options$pyomo_cbc
#     pyomo_highs$solver <- "highspy" # not working
#     # pyomo_highs$name <- "pyomo_highspy" # not working
#     scen_ph <- write_sc(scen_ph, solver = pyomo_highs)
#     scen_ph@misc$tmp.dir
#     solve_scenario(scen_ph, force = F, wait = F, read = T)
#     scen_ph_i <- read(scen_ph)
#   })
#   # scen <- solve(mod_unit, solver = solver_options$pyomo_glpk)
#   vObj <- getData(scen, "vObjective", merge = TRUE)$value
#   expect_equal(vObjective, vObj)
# })

# @covers vObjective depth=X backends=pyomo
test_that("ONE_pyomo_cbc", {
  skip_if_no_pyomo()
  invisible({
    scen <- solve(mod_unit, solver = solver_options$pyomo_cbc)
  })
  # scen <- solve(mod_unit, solver = solver_options$pyomo_glpk)
  vObj <- getData(scen, "vObjective", merge = TRUE)$value
  expect_equal(vObjective, vObj)
})

# @covers vObjective depth=X backends=julia_highs
test_that("ONE_julia_highs", {
  skip_if_no_julia_highs()
  invisible({
    scen <- solve(mod_unit, solver = solver_options$julia_highs)
  })
  # scen <- solve(mod_unit, solver = solver_options$pyomo_glpk)
  vObj <- getData(scen, "vObjective", merge = TRUE)$value
  expect_equal(vObjective, vObj)
})

# @covers vObjective depth=X backends=gams
test_that("ONE_gams_gdx_cplex", {
  skip_if_no_gams()
  invisible({
    # GAMS export requires a dense (sparse = FALSE) scenario
    scen <- solve(mod_unit, solver = solver_options$gams_gdx_cplex, sparse = FALSE)
  })
  vObj <- getData(scen, "vObjective", merge = TRUE)$value
  expect_equal(vObjective, vObj)
})

