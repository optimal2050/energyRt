# =========================================================================== #
# The unit kits (utopia$modules$unit): every input is 1 on the symmetric
# unit_s4 calendar (4 equal seasons), single-year horizon, discount 0 -- so
# the objective of every variant is a small integer computed BY HAND below.
# These are the quickest sanity anchors in the suite: if one moves, the
# arithmetic of capacity, activity, trade, storage, or supply-curve costs
# moved with it. Goldens: tools/test/make_goldens.R --suite=unit.
#
# Hand arithmetic (unit_s4: 4 slices, share 1/4, weight 4; cap2act = 1,
# invcost = 1, olife = 1, wacc = 0 -> EAC = 1 per unit of capacity):
#   base_U1  demand 1/slice -> E1 activity 4, fuel 4x1 = 4; peak output 1
#            over share 1/4 -> capacity 4, cost 4.               objective 8
#   trade_U3 PRM endowment in R1 only: E1 activity 12 (fuel 12) needs
#            capacity 12; corridor R1->R2 carries 2/slice -> trade cap 8,
#            R2->R3 carries 1/slice -> cap 4.                    objective 36
#   curve_U1 SUP_CURVE replaces the flat supply: 2 units @1 + 2 units @2
#            -> fuel 6, + capacity 4.                            objective 10
#   solar_U1 kit$SOLAR standalone: ESUN capacity 8 (2 per ON slice over
#            share 1/4, ON = half the year via utopia_profile step levels=2)
#            + storage out capacity 2 (the part capacity rates ANNUAL
#            throughput at cap2act = 1 -- total discharge 2; no sub-annual
#            af declared, so no per-slice power bound).          objective 10
#   basesolar_U1 SOLAR layered on the base: solar idle (10 > 8). objective 8
# =========================================================================== #

# @covers vObjective vTechNewCap vSupOut vStorageOut depth=S backends=glpk
test_that("unit kits solve to their hand-computed integer objectives", {
  skip_if_no_solver()
  g <- golden_read("unit")   # optional: also diff against the frozen golden
  ## TEMP DIAGNOSTIC -- remove after the order-dependency hunt
  cat("\n[DIAG] wd            =", getwd(),
      "\n[DIAG] scenarios_path =", get_scenarios_path(),
      "\n[DIAG] glpsol        =", energyRt:::.find_glpsol(),
      "\n[DIAG] glpk lang/name=", solver_options$glpk$lang,
      "/", solver_options$glpk$name,
      "\n[DIAG] tempdir exists=", dir.exists(tempdir()),
      "\n[DIAG] golden read   =", !is.null(g), "\n")
  for (nm in names(un_entries())) {
    mod <- do.call(un_build, un_entries()[[nm]])
    scen <- suppressMessages(suppressWarnings(
      solve_model(mod, name = paste0("unit_", nm),
                  solver = solver_options$glpk, tmp.del = TRUE, wait = TRUE)))
    ## TEMP DIAGNOSTIC
    cat("[DIAG]", nm, "optimal=", isTRUE(scen@status$optimal),
        "solved=", isTRUE(scen@status$solved),
        "script=", isTRUE(scen@status$script),
        "nvars=", length(scen@modOut@variables),
        "solver.dir=", scen@misc[["solver.dir"]] %||% "NULL", "\n")
    vs <- verify_solution(scen)
    expect_true(vs$ok, label = paste0(nm, " invariants"))
    obj <- capture_tracked_values(scen)$objective
    expect_equal(obj, unname(un_hand_objectives[nm]), tolerance = 1e-9,
                 label = paste0(nm, " hand-computed objective"))
    if (!is.null(g) && !is.null(g[[nm]])) {
      expect_matches_golden(scen, "unit", nm, kind = "same_solver")
    }
  }
})

test_that("unit add-on modules replace, not duplicate, the base objects", {
  skip_if_no_fixtures()
  kit <- utopia$modules$unit$U1
  mod <- add(un_build("U1"), kit$SUP_CURVE, overwrite = TRUE)
  scen <- suppressMessages(suppressWarnings(interpolate_model(
    mod, "unit_curve_I", ondisk = FALSE, overwrite = TRUE)))
  sup <- as.character(scen@modInp@sets$sup)
  # the flat SUP_PRM must be GONE, replaced by the two curve steps
  expect_identical(sort(sup), c("SUP_PRM_CLS1", "SUP_PRM_CLS2"))
})

# @covers mTechNew vTechNewCap depth=I backends=glpk forks=cluster,vintage
test_that("UTOPIA add-on modules land as clusters/vintages (R3)", {
  skip_if_no_fixtures()
  k3 <- utopia$modules$electricity$R3
  mod <- newModel("R3mods", data = k3$repo,
                  calendar = utopia$modules$calendars$s4_h24,
                  region = k3$regions,
                  horizon = utopia$modules$horizons$base, discount = 0.05)
  mod <- add(mod, k3$GAS_CURVE, overwrite = TRUE)
  mod <- add(mod, k3$EWIN_SITES, overwrite = TRUE)
  mod <- add(mod, k3$ENUC_VINT, overwrite = TRUE)
  scen <- suppressMessages(suppressWarnings(interpolate_model(
    mod, "R3mods_I", ondisk = FALSE, overwrite = TRUE)))
  s <- scen@modInp@sets
  expect_identical(sort(grep("SUP_GAS", as.character(s$sup), value = TRUE)),
                   paste0("SUP_GAS_CLS", 1:3))
  expect_identical(sort(grep("EWIN", as.character(s$tech), value = TRUE)),
                   c("EWIN_CLGOOD", "EWIN_CLPOOR"))
  expect_identical(sort(grep("ENUC", as.character(s$tech), value = TRUE)),
                   c("ENUC_VINv2025", "ENUC_VINv2040"))
})
