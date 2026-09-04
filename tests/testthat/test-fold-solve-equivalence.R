# Folding must be solution-invariant: on every tier fixture, the fold fixture
# and UTOPIA R1, a fully folded build solves to the objective of the unfolded
# build, passes verify_solution(), and serves the same demand (vDemInp is
# constraint-pinned, so it is optimum-invariant even under degeneracy).
# GLPK anchors the fast tier; the other backends run at tier cross.

.fe_models <- function() {
  env <- .mapping_fixture_env()
  m <- list(fold_fixture = function() fold_model())
  if (!is.null(env)) {
    for (nm in c("tm_core", "tm_flows", "tm_io", "tm_policy", "tm_weather")) {
      m[[nm]] <- local({ f <- env[[nm]]; function() f() })
    }
  }
  m$utopia_R1 <- function() ut_build("R1", "s4_h24")
  m
}

.fe_backends <- list(
  glpk = list(solver = quote(solver_options$glpk), skip = quote(skip_if_no_solver()),
              dense = FALSE),
  julia_highs = list(solver = quote(solver_options$julia_highs),
                     skip = quote(skip_if_no_julia_highs()), dense = FALSE),
  pyomo_cbc = list(solver = quote(solver_options$pyomo_cbc),
                   skip = quote(skip_if_no_pyomo()), dense = FALSE),
  gams = list(solver = quote(solver_options$gams_gdx_cplex),
              skip = quote(skip_if_no_gams()), dense = TRUE)
)

.fe_solve <- function(build, tag, fold, solver, dense) {
  scen <- suppressMessages(suppressWarnings(
    interpolate_model(build(), name = tag, ondisk = FALSE, overwrite = TRUE,
                      verbose = FALSE, fold = fold, sparse = !dense)))
  suppressMessages(suppressWarnings(
    solve_scenario(scen, solver = solver, force = TRUE, wait = TRUE,
                   solver.dir = withr::local_tempdir(.local_envir = parent.frame()))))
}

.fe_demand <- function(scen) {
  d <- as.data.frame(getData(scen, "vDemInp", merge = TRUE))
  fold_norm(stats::aggregate(value ~ region + year, d, sum))
}

for (model_nm in names(.fe_models())) {
  for (backend in names(.fe_backends)) {
    local({
      mn <- model_nm; bk <- backend; spec <- .fe_backends[[bk]]
      test_that(paste0("fold equivalence: ", mn, " on ", bk), {
        eval(spec$skip)
        if (grepl("^tm_", mn)) skip_if_no_fixtures()
        build <- .fe_models()[[mn]]
        tag <- gsub("[^A-Za-z0-9_]", "_", paste0("fe_", mn, "_", bk))
        uf <- .fe_solve(build, paste0(tag, "_uf"), FALSE, eval(spec$solver), spec$dense)
        fo <- .fe_solve(build, paste0(tag, "_fo"), FOLD_ALL, eval(spec$solver), spec$dense)
        expect_true(verify_solution(fo)$ok, label = paste(mn, bk, "invariants"))
        expect_equal(fold_objective(fo), fold_objective(uf), tolerance = 1e-6,
                     label = paste(mn, bk, "objective"))
        expect_equal(.fe_demand(fo), .fe_demand(uf), tolerance = 1e-6,
                     label = paste(mn, bk, "demand served"), ignore_attr = TRUE)
      })
    })
  }
}
