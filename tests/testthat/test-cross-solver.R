# =========================================================================== #
# Cross-solver parity suite (tier "cross"): the same model solved by every
# available backend must reproduce the GLPK reference -- objective at
# TOL_XSOLVER_OBJ and the tracked aggregates (annual x region / annual x
# total; never raw per-timeslice values) at TOL_XSOLVER_AGG -- and pass
# verify_solution() on every cell.
#
# Models: the tm_core / tm_weather tier fixtures (storage + weather stress all
# writers) and the UTOPIA R1 teaching layout. GAMS cells skip on a missing or
# unlicensed install; the NEOS fallback is future work (skip_if_no_neos).
#
# What is compared: DEGENERACY constrains it hard. tm_core has equal-cost
# alternative optima where even annual, entity-collapsed production totals
# differ between solvers (GLPK 65 vs HiGHS 58 at the identical objective:
# trade and storage paths substitute freely). Optima-invariant quantities are
# the objective, the accounting identities (verify_solution), and quantities
# pinned by constraints -- demand served (vDemInp). Anything else is a
# property of the solver's chosen vertex, not of the model.
# =========================================================================== #

.xs_models <- function() {
  env <- .mapping_fixture_env()
  list(
    tm_core    = function() env$tm_core(),
    tm_weather = function() env$tm_weather(),
    utopia_R1  = function() ut_build("R1", "s4_h24")
  )
}

.xs_backends <- list(
  julia_highs = list(solver = quote(solver_options$julia_highs),
                     skip = quote(skip_if_no_julia_highs())),
  pyomo_cbc = list(solver = quote(solver_options$pyomo_cbc),
                   skip = quote(skip_if_no_pyomo())),
  gams = list(solver = quote(solver_options$gams_gdx_cplex),
              skip = quote(skip_if_no_gams()))
)

.xs_solve <- function(build, tag, solver, dense = FALSE) {
  args <- if (dense) list(sparse = FALSE) else list()
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

for (model_nm in c("tm_core", "tm_weather", "utopia_R1")) {
  for (backend in names(.xs_backends)) {
    local({
      mn <- model_nm; bk <- backend
      spec <- .xs_backends[[bk]]
      test_that(paste0("cross-solver: ", mn, " on ", bk, " matches GLPK"), {
        eval(spec$skip)
        skip_if_no_solver()
        if (mn != "utopia_R1") skip_if_no_fixtures()
        build <- .xs_models()[[mn]]
        ref_scen <- .xs_solve(build, paste0("xs_", mn, "_ref"),
                              solver_options$glpk)
        expect_true(verify_solution(ref_scen)$ok,
                    label = paste0(mn, " glpk invariants"))
        ref <- capture_tracked_values(ref_scen)
        alt_scen <- .xs_solve(build, paste0("xs_", mn, "_", bk),
                              eval(spec$solver), dense = identical(bk, "gams"))
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
