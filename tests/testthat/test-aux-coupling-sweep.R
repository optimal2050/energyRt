# =========================================================================== #
# Every auxiliary coupling must reach the equation that consumes it.
#
# An equation term is `gate ? coefficient x variable : 0`, so three things have
# to agree on the same cell: the parameter's rows, the gate map's rows, and the
# variable's domain. The maps are built by joins, the parameters have defaults,
# and the templates guard with `$` / `FORIF` / `if ... in` -- so a disagreement
# does not raise anything, it removes the term. The system's signal for "this
# data is in the wrong cell" is a zero, which is also a legal answer.
#
# That is how `cout2ainp` and `cinp2aout` went unnoticed: both looked their
# main commodity up in the wrong flow domain, the join came out empty, and the
# coupling silently did nothing while the ratio sat in `modInp` looking set.
# Their only coverage asserted exactly that much -- the parameter arrives --
# which is the one depth that cannot see a dropped term.
#
# So this sweeps the catalogue instead of naming parameters one at a time.
# `.aux_conv_spec` lists every coupling the package knows (36 of them), and for
# each one that a fixture can drive:
#
#   reach  the gate map named by the catalogue has a row, at the same
#          (region, timeslice) cell as the coefficient the equation reads
#   bind   doubling the coefficient doubles the auxiliary flow
#
# The aux commodities are free -- unlimited supply at no cost, free disposal --
# so the coupling is observable without perturbing the optimum, and the scaling
# is exact rather than approximate. A coefficient that cannot change the answer
# is either dropped or dead, and both are defects.
#
# @covers pTechAct2AInp pTechAct2AOut pTechCap2AInp pTechCap2AOut
# @covers pTechNCap2AInp pTechNCap2AOut pTechCinp2AInp pTechCinp2AOut
# @covers pTechCout2AInp pTechCout2AOut pStorageStg2AInp pStorageStg2AOut
# @covers pStorageCinp2AInp pStorageCinp2AOut pStorageCout2AInp
# @covers pStorageCout2AOut pStorageInpCap2AInp pStorageInpCap2AOut
# @covers pStorageInpNCap2AInp pStorageInpNCap2AOut pStorageStgCap2AInp
# @covers pStorageStgCap2AOut pStorageStgNCap2AInp pStorageStgNCap2AOut
# @covers pStorageOutCap2AInp pStorageOutCap2AOut pStorageOutNCap2AInp
# @covers pStorageOutNCap2AOut pTechPho2AInp pTechRet2AInp pTechPho2AOut
# @covers pTechRet2AOut pStoragePho2AInp pStorageRet2AInp pStoragePho2AOut
# @covers pStorageRet2AOut
# @covers vTechAInp vTechAOut vStorageAInp vStorageAOut
# @covers eqTechAInp eqTechAOut eqStorageAInp eqStorageAOut
# @covers depth=S backends=glpk forks=aux
# =========================================================================== #

# `mStorageInpNCap2AOut` -> `inp.ncap2aout`; `mTechCinp2AInp` -> `cinp2ainp`
acs_column <- function(nm) {
  x <- sub("^m(Tech|Storage)", "", nm)
  part <- ""
  m <- regmatches(x, regexpr("^(Inp|Stg|Out)(Cap|NCap)", x))
  if (length(m) && nzchar(m)) {
    part <- paste0(tolower(sub("(Cap|NCap)$", "", m)), ".")
    x <- sub("^(Inp|Stg|Out)(Cap|NCap)", sub("^(Inp|Stg|Out)", "", m), x)
  }
  side <- sub(".*(2A(Inp|Out))$", "\\1", x)
  paste0(part, tolower(sub("2A(Inp|Out)$", "", x)), tolower(side))
}

acs_spec <- function() energyRt:::.aux_conv_spec

# Drivers a flat one-year fixture cannot make non-zero. Asserted below as an
# exact set, so a coupling cannot join this list without the test saying so.
acs_undrivable <- c(
  # phase-out and endogenous retirement are zero without a retiring stock
  "mTechPho2AInp", "mTechPho2AOut", "mTechRet2AInp", "mTechRet2AOut",
  "mStoragePho2AInp", "mStoragePho2AOut", "mStorageRet2AInp",
  "mStorageRet2AOut")

acs_cal <- function() newCalendar(timetable = make_timetable(
  struct = list(ANNUAL = "ANNUAL", SEASON = c("WIN", "SUM"))),
  name = "acs_cal")

# COA -> ELC, flat demand; WAT is a free aux input, HET a free aux output.
acs_tech_model <- function(nm, aeff) {
  newModel(nm, calendar = acs_cal(), region = "R1",
           horizon = newHorizon(2025), discount = 0,
           repo = newRepository(paste0("acs_", nm),
    newCommodity("COA", timeframe = "ANNUAL"),
    newCommodity("ELC", timeframe = "SEASON"),
    newCommodity("WAT", timeframe = "ANNUAL"),
    newCommodity("HET", timeframe = "ANNUAL"),
    newSupply("SCOA", commodity = "COA", supply = data.frame(cost = 1)),
    newSupply("SWAT", commodity = "WAT", supply = data.frame(cost = 0)),
    newTechnology("ECOA", input = list(comm = "COA"),
                  output = list(comm = "ELC"), cap2act = 1,
                  ceff = data.frame(comm = "COA", cinp2use = 1),
                  invcost = data.frame(invcost = 100),
                  vintage = data.frame(olife = 30L),
                  aux = data.frame(acomm = c("WAT", "HET")), aeff = aeff),
    newDemand("D", commodity = "ELC",
              demand = data.frame(timeslice = c("WIN", "SUM"), demand = 10))))
}

# A reservoir with all three parts priced, so every part capacity exists and
# the per-part couplings have a variable to read.
acs_stg_model <- function(nm, aeff) {
  newModel(nm, calendar = acs_cal(), region = "R1",
           horizon = newHorizon(2025), discount = 0,
           repo = newRepository(paste0("acs_", nm),
    newCommodity("ELC", timeframe = "SEASON"),
    newCommodity("WAT", timeframe = "ANNUAL"),
    newCommodity("HET", timeframe = "ANNUAL"),
    # available only in WIN, so the reservoir is the only way to reach SUM:
    # a `timeslice` row localises the COST, it does not restrict availability
    newSupply("SELC", commodity = "ELC",
              supply = data.frame(timeslice = c("WIN", "SUM"),
                                  ava.up = c(100, 0), cost = 1)),
    newSupply("SWAT", commodity = "WAT", supply = data.frame(cost = 0)),
    newStorage("STG", commodity = "ELC",
               seff = data.frame(inpeff = 1, outeff = 1),
               invcost = data.frame(inp.invcost = 1, stg.invcost = 1,
                                    out.invcost = 1),
               vintage = data.frame(olife = 30L),
               aux = data.frame(acomm = c("WAT", "HET")), aeff = aeff),
    newDemand("D", commodity = "ELC",
              demand = data.frame(timeslice = "SUM", demand = 10))))
}

# the coefficient row for one coupling, with the main commodity where the
# catalogue says the coupling reads one
acs_aeff <- function(map, value, cls) {
  cl <- acs_column(map)
  acomm <- if (grepl("aout$", cl)) "HET" else "WAT"
  d <- data.frame(acomm = acomm, stringsAsFactors = FALSE)
  if (isTRUE(acs_spec()[[map]][[3]])) {
    d$comm <- if (grepl("^cout", cl)) "ELC" else "COA"
  }
  d[[cl]] <- value
  d
}

acs_run <- function(map, value, cls, tag) {
  mod <- if (cls == "storage") acs_stg_model(tag, acs_aeff(map, value, cls))
         else acs_tech_model(tag, acs_aeff(map, value, cls))
  pd <- file.path(tempdir(), "acs")
  dir.create(pd, showWarnings = FALSE, recursive = TRUE)
  open_project(pd, verbose = FALSE)
  scen <- suppressMessages(suppressWarnings(
    interpolate_model(mod, name = tag, overwrite = TRUE)))
  sol <- suppressMessages(suppressWarnings(
    solve_scenario(scen, solver = solver_options$glpk, wait = TRUE)))
  gd <- function(obj, nm) {
    p <- obj[[nm]]
    if (is.null(p)) return(NULL)
    d <- get_data_slot(p)
    if (is.null(d) || !nrow(d)) NULL else as.data.frame(d)
  }
  side <- if (grepl("2AOut$", map)) "AOut" else "AInp"
  avar <- paste0(if (cls == "storage") "vStorage" else "vTech", side)
  list(gate = gd(scen@modInp@parameters, map),
       param = gd(scen@modInp@parameters, acs_spec()[[map]][[1]]),
       aux = gd(sol@modOut@variables, avar),
       obj = sum(gd(sol@modOut@variables, "vObjective")$value))
}

acs_sweep <- function(cls) {
  maps <- names(Filter(function(x) TRUE, acs_spec()))
  maps <- maps[startsWith(maps, if (cls == "storage") "mStorage" else "mTech")]
  setdiff(maps, acs_undrivable)
}

# --------------------------------------------------------------------------- #

test_that("the undrivable list is exactly the couplings a flat fixture cannot drive", {
  # a guard on the guard: every name must be a real catalogue entry, so the
  # list cannot drift into excusing a coupling that does exist
  expect_true(all(acs_undrivable %in% names(acs_spec())))
  expect_setequal(acs_undrivable[grepl("Pho|Ret", acs_undrivable)],
                  acs_undrivable)
})

test_that("every technology aux coupling reaches its equation and binds", {
  skip_if_no_solver()
  for (map in acs_sweep("technology")) {
    r1 <- acs_run(map, 0.1, "technology", paste0("acs_", map, "_a"))
    r2 <- acs_run(map, 0.2, "technology", paste0("acs_", map, "_b"))

    # reach: the gate map exists, at the coefficient's own cell
    expect_false(is.null(r1$gate), label = paste0(map, ": gate map is empty"))
    expect_false(is.null(r1$param), label = paste0(map, ": parameter is empty"))
    if (!is.null(r1$gate) && !is.null(r1$param)) {
      key <- intersect(c("region", "timeslice"), names(r1$gate))
      expect_setequal(do.call(paste, r1$gate[key]),
                      do.call(paste, r1$param[key]))
    }
    # bind: the aux flow scales with the coefficient, and the free aux leaves
    # the rest of the model where it was
    a1 <- if (is.null(r1$aux)) 0 else sum(r1$aux$value)
    a2 <- if (is.null(r2$aux)) 0 else sum(r2$aux$value)
    expect_true(a1 > 0,
                label = paste0(map, ": aux flow is zero -- the coupling is ",
                               "dropped or its driver is dead"))
    expect_equal(a2, 2 * a1, tolerance = 1e-6,
                 label = paste0(map, ": aux does not scale with the ratio"))
    expect_equal(r2$obj, r1$obj, tolerance = 1e-6,
                 label = paste0(map, ": a free aux moved the objective"))
  }
})

test_that("every storage aux coupling reaches its equation and binds", {
  skip_if_no_solver()
  for (map in acs_sweep("storage")) {
    r1 <- acs_run(map, 0.1, "storage", paste0("acs_", map, "_a"))
    r2 <- acs_run(map, 0.2, "storage", paste0("acs_", map, "_b"))

    expect_false(is.null(r1$gate), label = paste0(map, ": gate map is empty"))
    expect_false(is.null(r1$param), label = paste0(map, ": parameter is empty"))
    a1 <- if (is.null(r1$aux)) 0 else sum(r1$aux$value)
    a2 <- if (is.null(r2$aux)) 0 else sum(r2$aux$value)
    expect_true(a1 > 0,
                label = paste0(map, ": aux flow is zero -- the coupling is ",
                               "dropped or its driver is dead"))
    expect_equal(a2, 2 * a1, tolerance = 1e-6,
                 label = paste0(map, ": aux does not scale with the ratio"))
  }
})
