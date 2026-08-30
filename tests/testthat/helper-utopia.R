# =========================================================================== #
# UTOPIA golden-suite builders, shared by test-utopia-golden.R,
# test-cross-solver.R, and tools/test/make_goldens.R (--suite=utopia).
#
# Assembles a model from the shipped `utopia_modules` teaching kits (deterministic
# inputs) per the documented pattern (?utopia_modules): a region layout's `repo`
# + a calendar + the base horizon; scenario levers (constraints / a carbon tax)
# are added on top. Closes the gap that the UTOPIA vignettes are `eval = FALSE`
# ("validated out-of-band"): these entries pin them to goldens.
# =========================================================================== #

ut_build <- function(layout = "R3", calendar = "utopia_seasons", lever = NULL) {
  um <- utopia_modules$electricity[[layout]]
  stopifnot(!is.null(um))
  mod <- newModel(paste0("UT_", layout),
    data = um$repo,
    calendar = utopia_modules$calendars[[calendar]],
    region = um$regions,
    horizon = utopia_modules$horizons$base,
    discount = 0.05)
  if (!is.null(lever)) {
    lv <- um[[lever]]
    stopifnot(!is.null(lv))
    mod <- add(mod, lv)
  }
  mod
}

# The golden-suite entries: name -> builder args. Every layout kit targets
# `s4_h24` (ELC lives at the HOUR level; coarser calendars lack it), so
# all entries run on that calendar. R3 base ~seconds; the five levers run on
# the single-region R1 layout to keep the fast tier quick. The larger R7/R11
# layouts live in the `utopia_nightly` suite below (test-nightly-deep.R).
ut_entries <- function() list(
  base_R1      = list(layout = "R1", calendar = "s4_h24"),
  base_R3      = list(layout = "R3", calendar = "s4_h24"),
  co2cap_R1    = list(layout = "R1", calendar = "s4_h24", lever = "CO2_CAP"),
  ctco2_R1     = list(layout = "R1", calendar = "s4_h24", lever = "CT_CO2"),
  resshare_R1  = list(layout = "R1", calendar = "s4_h24", lever = "RES_SHARE"),
  nonewnuc_R1  = list(layout = "R1", calendar = "s4_h24", lever = "NO_NEW_NUC"),
  earlyret_R1  = list(layout = "R1", calendar = "s4_h24", lever = "EARLY_RET")
)

# Nightly-only entries: the two larger region layouts on the same calendar.
# Kept out of `ut_entries()` so the fast/cross tiers stay quick; goldens are
# frozen with make_goldens.R --suite=utopia_nightly.
ut_nightly_entries <- function() list(
  base_R7  = list(layout = "R7", calendar = "s4_h24"),
  base_R11 = list(layout = "R11", calendar = "s4_h24")
)

# -- unit kits (utopia_modules$unit): all-unit inputs, integer objectives ---- #
# `module = "SOLAR_only"` builds the self-contained solar+storage model from
# kit$SOLAR; any other module name is `add()`ed onto the base repo with
# overwrite = TRUE (SUP_CURVE replaces the flat supply, SOLAR layers on idle).
un_build <- function(layout = "U1", module = NULL) {
  kit <- utopia_modules$unit[[layout]]
  stopifnot(!is.null(kit))
  mod <- newModel(paste0("UN_", layout),
    data = if (identical(module, "SOLAR_only")) kit$SOLAR else kit$repo,
    calendar = utopia_modules$calendars[[kit$calendar]],
    region = kit$regions,
    horizon = utopia_modules$horizons$unit,
    discount = 0)
  if (!is.null(module) && !identical(module, "SOLAR_only")) {
    mod <- add(mod, kit[[module]], overwrite = TRUE)
  }
  mod
}

# name -> builder args + the HAND-COMPUTED objective each model must hit
# exactly (the kit's whole point); arithmetic in data-raw/utopia_modules.R.
un_entries <- function() list(
  base_U1      = list(layout = "U1"),                          # 8
  trade_U3     = list(layout = "U3"),                          # 36
  curve_U1     = list(layout = "U1", module = "SUP_CURVE"),    # 10
  solar_U1     = list(layout = "U1", module = "SOLAR_only"),   # 10
  basesolar_U1 = list(layout = "U1", module = "SOLAR")         # 8 (solar idle)
)

un_hand_objectives <- c(base_U1 = 8, trade_U3 = 36, curve_U1 = 10,
                        solar_U1 = 10, basesolar_U1 = 8)

# Solve a UTOPIA entry with a chosen backend. The backend-choice convention
# (dev/TESTING.md): glpsol for small models, julia/HiGHS for mid-size and
# sampled ones -- so R1/R3 suites reference glpk, R7/R11 julia_highs
# (matching SUITE_SOLVERS in tools/test/make_goldens.R).
ut_solve <- function(args, tag, solver = "glpk") {
  mod <- do.call(ut_build, args)
  scen <- suppressMessages(suppressWarnings(interpolate_model(
    mod, name = tag, overwrite = TRUE)))
  suppressMessages(suppressWarnings(
    solve_scenario(scen, solver = solver_options[[solver]], force = TRUE,
                   wait = TRUE)))
}

ut_solve_glpk <- function(args, tag) ut_solve(args, tag, solver = "glpk")
