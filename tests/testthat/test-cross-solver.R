# =========================================================================== #
# Cross-solver parity suite (tier "cross"): the same model solved by every
# available backend must reproduce the GLPK reference -- objective at
# TOL_XSOLVER_OBJ and the tracked aggregates (annual x region / annual x
# total; never raw per-timeslice values) at TOL_XSOLVER_AGG -- and pass
# verify_solution() on every cell.
#
# Models: the tm_core / tm_weather tier fixtures (storage + weather stress all
# writers) and the TOPIA R1 teaching layout. GAMS cells skip on a missing or
# unlicensed install; the NEOS fallback is future work (skip_if_no_neos).
#
# Backends are also a DATA-EXCHANGE dimension: `julia_highs` / `pyomo_cbc` run
# the default Arrow exchange, `julia_highs_rdata` / `pyomo_cbc_sqlite` the
# legacy containers. Both must reach the same objective as GLPK, so a drift in
# what a writer hands the solver cannot hide inside one format.
#
# What is compared: DEGENERACY constrains it hard. tm_core has equal-cost
# alternative optima where even annual, entity-collapsed production totals
# differ between solvers (GLPK 65 vs HiGHS 58 at the identical objective:
# trade and storage paths substitute freely). Optima-invariant quantities are
# the objective, the accounting identities (verify_solution), and quantities
# pinned by constraints -- demand served (vDemInp). Anything else is a
# property of the solver's chosen vertex, not of the model.
# =========================================================================== #

# A technology declared COARSER than an input it uses -- the case whose `Agg`
# branches are written separately in every template, so parity here is the only
# thing that catches one backend reading the aggregation differently. A real
# instance: the GAMS branch read `pTechCinp2ginp` at the FINE cell, where it is
# undefined; GLPK/Julia/Pyomo fell back to their default of 1 and agreed by
# accident, GAMS read 0 and went infeasible. Hence the NON-default 2 / 3 below:
# with 1 / 1 the discrepancy is invisible.
.xs_coarse_time <- function(mill_tf = "ANNUAL", group = FALSE) {
  ceff <- if (group) {
    data.frame(comm = c("ELC", "GAS"), cinp2ginp = c(2, 3),
               grp.share.up = c(NA, 0.1))
  } else NULL
  mill <- if (group) {
    newTechnology("MILL",
      input = list(comm = c("ELC", "GAS"), group = c("FUEL", "FUEL")),
      output = list(comm = "STEEL"),
      geff = data.frame(group = "FUEL", ginp2use = 1), ceff = ceff,
      timeframe = mill_tf, invcost = data.frame(invcost = 50),
      vintage = data.frame(olife = 30L), cap2act = 1)
  } else {
    newTechnology("MILL", input = list(comm = "ELC"),
      output = list(comm = "STEEL"), timeframe = mill_tf,
      invcost = data.frame(invcost = 50),
      vintage = data.frame(olife = 30L), cap2act = 1)
  }
  objs <- list(
    newCommodity("COA", timeframe = "ANNUAL"),
    newCommodity("ELC", timeframe = "SEASON"),
    newCommodity("STEEL", timeframe = "ANNUAL"),
    newSupply("SUP", commodity = "COA", supply = data.frame(cost = 1)),
    newTechnology("ECOA", input = list(comm = "COA"),
                  output = list(comm = "ELC"),
                  invcost = data.frame(invcost = 900),
                  vintage = data.frame(olife = 30L), cap2act = 1),
    mill,
    newDemand("DS", commodity = "STEEL", demand = data.frame(demand = 4)),
    newDemand("DE", commodity = "ELC",
              demand = data.frame(timeslice = c("WIN", "SUM"),
                                  demand = if (group) c(2, 18) else c(20, 20))))
  if (group) {
    objs <- c(objs, list(
      newCommodity("GAS", timeframe = "SEASON"),
      newTechnology("EGAS", input = list(comm = "COA"),
                    output = list(comm = "GAS"),
                    invcost = data.frame(invcost = 800),
                    vintage = data.frame(olife = 30L), cap2act = 1),
      newDemand("DG", commodity = "GAS",
                demand = data.frame(timeslice = c("WIN", "SUM"),
                                    demand = c(18, 2)))))
  }
  newModel("xsc", repo = do.call(newRepository, c(list("r"), objs)),
    calendar = newCalendar(timetable = make_timetable(
      struct = list(ANNUAL = "ANNUAL", SEASON = c("WIN", "SUM"))),
      name = "xscc"),
    region = "R1", horizon = newHorizon(2020:2030, intervals = 10),
    discount = 0)
}

.xs_models <- function() {
  env <- .mapping_fixture_env()
  list(
    tm_core    = function() env$tm_core(),
    tm_weather = function() env$tm_weather(),
    topia_R1  = function() ut_build("R1", "s4_h24"),
    # coarse-process parity, with and without groups/shares
    coarse_time  = function() .xs_coarse_time("ANNUAL", group = FALSE),
    coarse_group = function() .xs_coarse_time("ANNUAL", group = TRUE)
  )
}

.xs_backends <- list(
  julia_highs = list(solver = quote(solver_options$julia_highs),
                     skip = quote(skip_if_no_julia_highs())),
  pyomo_cbc = list(solver = quote(solver_options$pyomo_cbc),
                   skip = quote(skip_if_no_pyomo())),
  # The same backends through their LEGACY exchange containers. Arrow is the
  # default now, so without these a format-dependent drift in the data written
  # to a solver would pass unnoticed: each entry solves the identical model and
  # is compared against the same GLPK reference as its arrow twin.
  julia_highs_rdata = list(solver = quote(solver_options$julia_highs_rdata),
                           skip = quote(skip_if_no_julia_highs())),
  pyomo_cbc_sqlite = list(solver = quote(solver_options$pyomo_cbc_sqlite),
                          skip = quote(skip_if_no_pyomo())),
  gams = list(solver = quote(solver_options$gams_gdx_cplex),
              skip = quote(skip_if_no_gams())),
  # The matrix route: no model script and no solver process -- multimod builds
  # the LP from the AST and HiGHS solves it in process. It reaches the solver
  # by a different path from every other entry here, so parity against the
  # GLPK reference is the only thing that catches a drift in the matrix.
  mm_highs = list(solver = quote(solver_options$mm_highs),
                  skip = quote(skip_if_no_multimod())),
  # the same route with a forwarded HiGHS option: presolve off must reach the
  # same optimum
  mm_highs_nopresolve = list(solver = quote(within(solver_options$mm_highs,
                                                   highs_options <- list(presolve = "off"))),
                             skip = quote(skip_if_no_multimod())),
  # the same route on a FOLDED scenario. multimod takes the folded parameters
  # as they are and resolves the wildcards on import, so `.fold_code_block()`
  # returns NA for this lang and the artificial-member substitution is skipped.
  # When it was not skipped the data carried `ANY*` / year `0` labels that the
  # template equations never gate on: 864 of 5994 coefficients were dropped on
  # topia_R1 and HiGHS reported Optimal at objective 0.
  mm_highs_folded = list(solver = quote(solver_options$mm_highs),
                         skip = quote(skip_if_no_multimod()),
                         fold = TRUE)
)

.xs_solve <- function(build, tag, solver, dense = FALSE, fold = FALSE) {
  args <- c(if (dense) list(sparse = FALSE) else list(),
            if (fold) list(fold = TRUE) else list())
  scen <- suppressMessages(suppressWarnings(do.call(interpolate_model,
    c(list(build(), name = tag, overwrite = TRUE), args))))
  suppressMessages(suppressWarnings(
    solve_scenario(scen, solver = solver, force = TRUE, wait = TRUE)))
}

# compare an optima-invariant tracked variable (constraint-pinned) between
# the two captures at the annual x region level
.xs_expect_pinned_parity <- function(ref, alt, variable, label) {
  a <- ref$values[[variable]][["annual_region"]]
  b <- alt$values[[variable]][["annual_region"]]
  if (is.null(a) || is.null(b)) {
    testthat::skip(paste0(label, ": ", variable, " not tracked"))
  }
  keys <- setdiff(colnames(a), "value")
  m <- merge(a, b, by = keys, all = TRUE, suffixes = c(".ref", ".alt"))
  m$value.ref[is.na(m$value.ref)] <- 0
  m$value.alt[is.na(m$value.alt)] <- 0
  bad <- abs(m$value.alt - m$value.ref) >
    TOL_ABS_FLOOR + TOL_XSOLVER_AGG * pmax(abs(m$value.alt), abs(m$value.ref))
  expect_false(any(bad),
               label = paste0(label, ": ", variable, " (", sum(bad), "/",
                              nrow(m), " differ)"))
}

for (model_nm in c("tm_core", "tm_weather", "topia_R1",
                   "coarse_time", "coarse_group")) {
  for (backend in names(.xs_backends)) {
    local({
      mn <- model_nm; bk <- backend
      spec <- .xs_backends[[bk]]
      test_that(paste0("cross-solver: ", mn, " on ", bk, " matches GLPK"), {
        eval(spec$skip)
        skip_if_no_solver()
        if (startsWith(mn, "tm_")) skip_if_no_fixtures()
        build <- .xs_models()[[mn]]
        ref_scen <- .xs_solve(build, paste0("xs_", mn, "_ref"),
                              solver_options$glpk)
        expect_true(verify_solution(ref_scen)$ok,
                    label = paste0(mn, " glpk invariants"))
        ref <- capture_tracked_values(ref_scen)
        alt_scen <- .xs_solve(build, paste0("xs_", mn, "_", bk),
                              eval(spec$solver), dense = identical(bk, "gams"),
                              fold = isTRUE(spec$fold))
        expect_true(verify_solution(alt_scen)$ok,
                    label = paste0(mn, " ", bk, " invariants"))
        alt <- capture_tracked_values(alt_scen)
        expect_equal(alt$objective, ref$objective,
                     tolerance = TOL_XSOLVER_OBJ,
                     label = paste0(mn, " ", bk, " objective"))
        .xs_expect_pinned_parity(ref, alt, "vDemInp", paste0(mn, ":", bk))
      })
    })
  }
}
