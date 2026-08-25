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
# `utopia_s4h24` (ELC lives at the HOUR level; coarser calendars lack it), so
# all entries run on that calendar. R3 base ~seconds; the five levers run on
# the single-region R1 layout to keep the fast tier quick. R7/R11 and
# utopia_m12h24 are nightly-tier candidates (not in this suite).
ut_entries <- function() list(
  base_R1      = list(layout = "R1", calendar = "utopia_s4h24"),
  base_R3      = list(layout = "R3", calendar = "utopia_s4h24"),
  co2cap_R1    = list(layout = "R1", calendar = "utopia_s4h24", lever = "CO2_CAP"),
  ctco2_R1     = list(layout = "R1", calendar = "utopia_s4h24", lever = "CT_CO2"),
  resshare_R1  = list(layout = "R1", calendar = "utopia_s4h24", lever = "RES_SHARE"),
  nonewnuc_R1  = list(layout = "R1", calendar = "utopia_s4h24", lever = "NO_NEW_NUC"),
  earlyret_R1  = list(layout = "R1", calendar = "utopia_s4h24", lever = "EARLY_RET")
)

ut_solve_glpk <- function(args, tag) {
  mod <- do.call(ut_build, args)
  scen <- suppressMessages(suppressWarnings(interpolate_model(
    mod, name = tag, overwrite = TRUE)))
  suppressMessages(suppressWarnings(
    solve_scenario(scen, solver = solver_options$glpk, force = TRUE,
                   wait = TRUE)))
}
