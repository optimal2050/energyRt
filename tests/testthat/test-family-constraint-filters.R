# =========================================================================== #
# Feature family: constraint-filters -- process targeting on a constraint
# summand, and index validation for every dimension.
#
# Fixture: 1 region, two 10-year periods, discount 0. TA is VINTAGED, so its
# realized names are TA_VIN2025 / TA_VIN2035; TB is plain. Demand 40 of ELC,
# COA supplied at cost 1, TA cheap (invcost 300) and TB dear (3000).
#
# What the filters mean (same resolver as the levy classes, R/levy.R):
#   process_  regex over process names, matched against the DECLARED (base)
#             name as well as the realized one
#   vintage / cluster (+ `_` regex forms)
#             joined from `modInp@sets$variant`, never parsed out of the
#             mangled variant name -- the separator is user-configurable
#   classes   restricts to a process class ("technology", "storage", ...)
# and in `for.sum`:
#   <dim>_    a regex over that dimension
#   a base name expands to its realized variants, the way a coarse region
#   expands to its children
#
# Every one of these is an ERROR when it resolves to nothing. Before this, only
# `region` and `timeslice` were checked, so `for.sum = list(tech = "ECOA")`
# against a vintaged ECOA_VIN2030 dropped the term and still solved.
# =========================================================================== #

cnf_build <- function(...) {
  objs <- list(
    newCommodity("COA", timeframe = "ANNUAL"),
    newCommodity("ELC", timeframe = "ANNUAL"),
    newSupply("SUP", commodity = "COA", supply = data.frame(cost = 1)),
    newTechnology("TA", input = list(comm = "COA"), output = list(comm = "ELC"),
                  invcost = list(invcost = 300), cap2act = 1,
                  vintage = data.frame(vintage = c("2025", "2035"),
                                       olife = 30L)),
    newTechnology("TB", input = list(comm = "COA"), output = list(comm = "ELC"),
                  invcost = list(invcost = 3000), cap2act = 1,
                  vintage = data.frame(olife = 30L)),
    newDemand("DEM", commodity = "ELC", demand = data.frame(demand = 40)), ...)
  newModel("cnf",
    repo = do.call(newRepository, c(list("cnf_repo"), objs)),
    calendar = newCalendar(
      timetable = make_timetable(struct = list(ANNUAL = "ANNUAL")),
      name = "cnf_cal"),
    region = "R1", horizon = newHorizon(2025:2044, intervals = c(10, 10)),
    discount = 0)
}

cnf_interp <- function(cns, tag) {
  suppressMessages(suppressWarnings(
    interpolate_model(cnf_build(cns), name = tag, overwrite = TRUE)))
}

# A capacity constraint with whatever targeting the case under test needs.
cnf_cns <- function(...) {
  newConstraint("CNF", eq = "<=", for.each = data.frame(year = 2025),
                cap = c(list(variable = "vTechCap"), list(...)),
                rhs = 1000, defVal = 0)
}

# Which techs the compiled constraint actually selected. An unrestricted term
# emits no per-tech map at all, which is how "all of them" looks.
cnf_techs <- function(scen) {
  out <- character()
  for (n in grep("Cns", names(scen@modInp@parameters), value = TRUE)) {
    d <- tryCatch(get_data_slot(scen@modInp@parameters[[n]]),
                  error = function(e) NULL)
    if (!is.null(d) && "tech" %in% names(d)) {
      out <- c(out, as.character(d$tech))
    }
  }
  sort(unique(out))
}

test_that("family constraint-filters: the fixture realizes the vintages", {
  scen <- cnf_interp(NULL, "cnf_base")
  expect_setequal(scen@modInp@sets$tech, c("TA_VIN2025", "TA_VIN2035", "TB"))
  # the provenance table is what vintage/cluster filters join
  expect_true(!is.null(scen@modInp@sets$variant))
})

# @covers vTechCap depth=I backends=glpk
test_that("family constraint-filters: process_ regex selects every variant", {
  scen <- cnf_interp(cnf_cns(process_ = "^TA", mult = 1), "cnf_rx")
  expect_equal(cnf_techs(scen), c("TA_VIN2025", "TA_VIN2035"))
})

# @covers vTechCap depth=I backends=glpk
test_that("family constraint-filters: vintage selects one vintage", {
  scen <- cnf_interp(cnf_cns(vintage = "2025", mult = 1), "cnf_vin")
  expect_equal(cnf_techs(scen), "TA_VIN2025")
})

# @covers vTechCap depth=I backends=glpk
test_that("family constraint-filters: a for.sum base name expands to variants", {
  # the headline fix: "TA" is not an index of vTechCap at all, but it is the
  # declared name behind both variants
  scen <- cnf_interp(cnf_cns(for.sum = list(tech = "TA"), mult = 1), "cnf_base_nm")
  expect_equal(cnf_techs(scen), c("TA_VIN2025", "TA_VIN2035"))
})

# @covers vTechCap depth=I backends=glpk
test_that("family constraint-filters: a <dim>_ for.sum key is a regex", {
  scen <- cnf_interp(cnf_cns(for.sum = list(tech_ = "^TA"), mult = 1), "cnf_key")
  expect_equal(cnf_techs(scen), c("TA_VIN2025", "TA_VIN2035"))
  # and it composes with the slot form to the same selection
  scen2 <- cnf_interp(cnf_cns(for.sum = list(tech_ = "^TA"),
                              process_ = "^TA", mult = 1), "cnf_key2")
  expect_equal(cnf_techs(scen2), c("TA_VIN2025", "TA_VIN2035"))
})

# @covers vTechCap depth=I backends=glpk
test_that("family constraint-filters: classes restricts to a process class", {
  # every process here is a technology, so this selects all of them and emits
  # no restriction -- the point is that it resolves rather than erroring
  scen <- cnf_interp(cnf_cns(classes = "technology", mult = 1), "cnf_cls")
  expect_true(length(cnf_techs(scen)) %in% c(0L, 3L))
})

test_that("family constraint-filters: a filter resolving to nothing errors", {
  expect_error(cnf_interp(cnf_cns(process_ = "^ZZ", mult = 1), "cnf_e1"),
               "`process_`.*selects no process")
  expect_error(cnf_interp(cnf_cns(vintage = "2099", mult = 1), "cnf_e2"),
               "`vintage`.*selects no process")
  expect_error(cnf_interp(cnf_cns(cluster = "nope", mult = 1), "cnf_e3"),
               "`cluster`.*selects no process")
  expect_error(cnf_interp(cnf_cns(classes = "nope", mult = 1), "cnf_e4"),
               "unknown class")
})

test_that("family constraint-filters: an unmatched for.sum index errors", {
  # this is the silent zero this family exists to close
  expect_error(cnf_interp(cnf_cns(for.sum = list(tech = "NOPE"), mult = 1),
                          "cnf_e5"),
               "is not an index of variable")
  # a regex key over a dimension the variable does not have
  expect_error(cnf_interp(cnf_cns(for.sum = list(zzz_ = "^T"), mult = 1),
                          "cnf_e6"),
               "regular expression over")
  # a regex key that matches nothing
  expect_error(cnf_interp(cnf_cns(for.sum = list(tech_ = "^ZZ"), mult = 1),
                          "cnf_e7"),
               "selects no process")
})

test_that("family constraint-filters: targeting needs a process dimension", {
  # vBalance is (comm, region, year, timeslice) -- nothing to target
  cns <- newConstraint("CNB", eq = "<=", for.each = data.frame(year = 2025),
                       bal = list(variable = "vBalance", process_ = "^TA"),
                       rhs = 1000, defVal = 0)
  expect_error(cnf_interp(cns, "cnf_e8"), "no process dimension")
})

test_that("family constraint-filters: a summand saved without the slots still works", {
  # `.upgrade_summand()` must add the targeting slots, exactly as it already
  # does for timeframe/geoframe -- otherwise a model serialized before them
  # errors with "no slot of name ..." at compile time
  cns <- cnf_cns(for.sum = list(tech = "TA"), mult = 1)
  sm <- cns@lhs[[1]]
  for (sl in c("process_", "classes", "vintage", "vintage_",
               "cluster", "cluster_")) {
    attr(sm, sl) <- NULL
  }
  expect_false(methods::.hasSlot(sm, "process_"))
  up <- energyRt:::.upgrade_summand(sm)
  expect_true(methods::.hasSlot(up, "process_"))
  expect_length(energyRt:::.summand_slot(up, "process_"), 0L)
})

test_that("family constraint-filters: newConstraintS is retired", {
  expect_false(exists("newConstraintS", where = asNamespace("energyRt"),
                      inherits = FALSE))
})

test_that("family constraint-filters: a term that could only be zero errors", {
  # `COA` is a real commodity, so it passes set membership -- but no technology
  # here OUTPUTS it, so `vTechOut` has no COA rows and the term would be zero on
  # every row. A declared-but-absent member is tolerated among others (TOPIA's
  # RES_SHARE names EHYD, which has no resource in that layout); a term made of
  # nothing but absent members is not.
  cns <- newConstraint("CNZ", eq = "<=", for.each = data.frame(year = 2025),
                       out = list(variable = "vTechOut",
                                  for.sum = list(comm = "COA")),
                       rhs = 1000, defVal = 0)
  expect_error(cnf_interp(cns, "cnf_e9"),
               "names no member that variable")

  # the tolerated case: one absent member alongside a present one
  cns2 <- newConstraint("CNZ2", eq = "<=", for.each = data.frame(year = 2025),
                        out = list(variable = "vTechOut",
                                   for.sum = list(comm = c("ELC", "COA"))),
                        rhs = 1000, defVal = 0)
  expect_s4_class(cnf_interp(cns2, "cnf_ok9"), "scenario")
})
