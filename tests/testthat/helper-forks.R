# =========================================================================== #
# Fork harness: run one feature family's mini-model across the fork grid --
# interpolation modes x solver backends -- with the invariant checker wired
# into every solved cell.
#
# A family file supplies:
#   build          function() -> unsolved model (parameter under test set to a
#                  value with an ANALYTICALLY predictable effect)
#   check_params   function(scen, case_name) -- depth-I assertions on modInp
#                  content (runs for every interp case, solver-free)
#   check_solution function(scen, backend, case_name) -- depth-S/X assertions
#                  (objective / aggregated flows vs the analytic expectation)
#
# The harness adds for free:
#   - interp-mode invariance: every interp case must give the SAME GLPK
#     objective (TOL_SAME_SOLVER)
#   - cross-backend parity: every backend must reproduce the GLPK objective
#     (TOL_XSOLVER_OBJ)
#   - verify_solution() on every solved cell (physics check)
#
# Tier mapping: check_params -> tier check; GLPK solves -> tier fast;
# other backends -> tier cross. GAMS constraints are honoured automatically
# (dense scenario required; folded scenarios unsupported -> cell skipped).
# =========================================================================== #

# canonical interp cases; a family can pass a subset or extras
fork_interp_cases <- function() {
  list(
    default = list(),
    dense = list(sparse = FALSE),
    folded = list(fold = TRUE),
    ondisk = list(ondisk = TRUE)
  )
}

.fork_backends <- list(
  glpk = list(solver = quote(solver_options$glpk), skip = quote(skip_if_no_solver())),
  julia_highs = list(solver = quote(solver_options$julia_highs),
                     skip = quote(skip_if_no_julia_highs())),
  pyomo_cbc = list(solver = quote(solver_options$pyomo_cbc),
                   skip = quote(skip_if_no_pyomo())),
  gams = list(solver = quote(solver_options$gams_gdx_cplex),
              skip = quote(skip_if_no_gams()))
)

.fork_interp <- function(build, family, case_name, args) {
  mod <- build()
  # harness scratch scenarios are disposable: always overwrite leftovers of a
  # previous run (matters for the ondisk case, which claims a directory)
  args$overwrite <- TRUE
  do.call(interpolate_model,
          c(list(mod, name = paste0("ff_", family, "_", case_name)), args))
}

.fork_solve <- function(scen, solver) {
  suppressMessages(suppressWarnings(
    solve_scenario(scen, solver = solver, tmp.del = TRUE, force = TRUE,
                   wait = TRUE)
  ))
}

.fork_objective <- function(scen) {
  d <- get_data_slot(scen@modOut@variables[["vObjective"]], optional = TRUE)
  if (is.null(d) || nrow(d) == 0) NA_real_ else d$value[1]
}

run_family <- function(family, build,
                       check_params = NULL,
                       check_solution = NULL,
                       interp_cases = fork_interp_cases(),
                       backends = c("glpk", "julia_highs", "pyomo_cbc", "gams"),
                       solve_cases = names(interp_cases)) {

  # ---- depth I: every interp case, solver-free ---------------------------- #
  test_that(paste0("family ", family, ": interp cases build and expose parameters"), {
    skip_if_tier_below("check")
    for (case_name in names(interp_cases)) {
      scen <- suppressMessages(suppressWarnings(
        .fork_interp(build, family, case_name, interp_cases[[case_name]])))
      expect_s4_class(scen, "scenario")
      if (!is.null(check_params)) check_params(scen, case_name)
    }
  })

  # ---- depth S: GLPK over the interp cases, invariance built in ----------- #
  objectives <- new.env(parent = emptyenv())
  test_that(paste0("family ", family, ": GLPK solves every interp case identically"), {
    skip_if_no_solver()
    objs <- c()
    for (case_name in intersect(solve_cases, names(interp_cases))) {
      scen <- suppressMessages(suppressWarnings(
        .fork_interp(build, family, case_name, interp_cases[[case_name]])))
      scen <- .fork_solve(scen, solver_options$glpk)
      vs <- verify_solution(scen)
      expect_true(vs$ok, label = paste0(family, ":", case_name, " invariants"))
      obj <- .fork_objective(scen)
      expect_false(is.na(obj), label = paste0(family, ":", case_name, " solved"))
      objs[case_name] <- obj
      if (!is.null(check_solution)) check_solution(scen, "glpk", case_name)
    }
    if (length(objs) > 1) {
      expect_equal(unname(objs), rep(unname(objs[1]), length(objs)),
                   tolerance = TOL_SAME_SOLVER,
                   label = paste0(family, ": objective across interp cases"))
    }
    assign("glpk", unname(objs[1]), envir = objectives)
  })

  # ---- depth X: other backends on one case, parity with GLPK -------------- #
  for (backend in setdiff(backends, "glpk")) {
    spec <- .fork_backends[[backend]]
    test_that(paste0("family ", family, ": ", backend, " reproduces the objective"), {
      eval(spec$skip)
      skip_if_no_solver()  # the GLPK reference objective is required
      # GAMS needs a dense scenario; folded scenarios are unsupported there
      args <- if (backend == "gams") list(sparse = FALSE) else list()
      scen <- suppressMessages(suppressWarnings(
        .fork_interp(build, family, paste0("x_", backend), args)))
      scen <- .fork_solve(scen, eval(spec$solver))
      vs <- verify_solution(scen)
      expect_true(vs$ok, label = paste0(family, ":", backend, " invariants"))
      obj <- .fork_objective(scen)
      ref <- if (exists("glpk", envir = objectives)) get("glpk", envir = objectives) else NA_real_
      if (is.na(ref)) {
        # cross tier may run without the fast-tier block having executed
        mod_ref <- .fork_interp(build, family, "x_ref", list())
        ref <- .fork_objective(.fork_solve(mod_ref, solver_options$glpk))
      }
      expect_equal(obj, ref, tolerance = TOL_XSOLVER_OBJ,
                   label = paste0(family, ":", backend, " objective parity"))
      if (!is.null(check_solution)) check_solution(scen, backend, "default")
    })
  }
}

# convenience: value of one modInp numeric parameter as a data.table
ff_param <- function(scen, name) {
  p <- scen@modInp@parameters[[name]]
  if (is.null(p)) return(NULL)
  d <- get_data_slot(p, optional = TRUE)
  if (is.null(d)) return(NULL)
  d <- data.table::as.data.table(d)
  for (j in colnames(d)) {
    if (is.factor(d[[j]])) data.table::set(d, j = j, value = as.character(d[[j]]))
  }
  d
}

# convenience: one side of a bounds parameter (stored unexpanded under the
# base name with a `type` column: "lo" / "up" / "fx")
ff_bound <- function(scen, name, side = c("up", "lo", "fx")) {
  side <- match.arg(side)
  d <- ff_param(scen, name)
  if (is.null(d) || !"type" %in% colnames(d)) return(NULL)
  d[type == side]
}

# convenience: aggregated solution value (sum over all dims but `by`)
ff_solution_sum <- function(scen, variable, by = NULL) {
  v <- scen@modOut@variables[[variable]]
  if (is.null(v)) return(NULL)
  d <- get_data_slot(v, optional = TRUE)
  if (is.null(d) || nrow(d) == 0) return(0)
  d <- data.table::as.data.table(d)
  if (is.null(by)) return(d[, sum(value)])
  for (j in colnames(d)) {
    if (is.factor(d[[j]])) data.table::set(d, j = j, value = as.character(d[[j]]))
  }
  d[, .(value = sum(value)), by = by]
}
