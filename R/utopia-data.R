# Dataset documentation for the UTOPIA teaching model.

#' UTOPIA reference maps
#'
#' Geographic maps for the imaginary country "Utopia" used by the UTOPIA
#' vignette to lay out regions, neighbours and trade routes.
#'
#' @format A named list. Element `map` is a named list of `sf` polygon layers
#'   (`honeycomb`, `continent`, `island`, `squares`, ...); the vignette uses
#'   `utopia$map$honeycomb` and keeps the first few regions.
#' @seealso the `utopia` vignette, [utopia_weather], [utopia_demand],
#'   [utopia_stock], [utopia_profiles()]
#' @examples
#' names(utopia)
#' names(utopia$map)
"utopia"

#' UTOPIA representative capacity-factor profiles
#'
#' Deterministic solar / wind / hydro capacity factors for the UTOPIA model,
#' provided for the three teaching calendars (`s4_h24`, 96 timeslices,
#' the default base case; `m12_h24`, 288; and `utopia_seasons`, 12).
#' Region-agnostic; expand to regions with [utopia_profiles()]. Built by
#' `data-raw/utopia_data.R` from IDEEA reanalysis profiles (with a curated
#' fallback when IDEEA is absent); see `attr(.,"source")`.
#'
#' @format A data.frame with columns `calendar`
#'   (`s4_h24`/`m12_h24`/`utopia_seasons`), `resource`
#'   (`WSOL`/`WWIN`/`WHYD`), `timeslice` (e.g. `SUM_h12`, `m06_h12` or
#'   `SUM_DAY`) and `wval` (capacity factor, 0-1). Attribute `source`.
#' @seealso [utopia_profiles()], [utopia_demand], [utopia_stock], [calendars]
#' @examples
#' head(utopia_weather)
#' attr(utopia_weather, "source")
"utopia_weather"

#' UTOPIA electricity load shape
#'
#' A deterministic relative electricity-load shape by timeslice, for the three
#' teaching calendars (replaces the vignette's former random load curve).
#' [utopia_profiles()] / the vignette scale it by a region's annual demand and
#' the timeslice shares to get energy per timeslice.
#'
#' @format A data.frame with columns `calendar`
#'   (`s4_h24`/`m12_h24`/`utopia_seasons`), `timeslice` and `load`
#'   (relative, mean ~1).
#' @seealso [utopia_profiles()], [utopia_weather], [utopia_stock]
#' @examples
#' utopia_demand
"utopia_demand"

#' UTOPIA base-year capacity stock
#'
#' Deterministic base-year installed capacity per technology (GW), per region
#' (replaces the vignette's former `runif` stocks). Expand across regions with
#' [utopia_profiles()].
#'
#' @format A data.frame with columns `tech` and `gw` (base-year capacity, GW).
#' @seealso [utopia_profiles()], [utopia_weather], [utopia_demand]
#' @examples
#' utopia_stock
"utopia_stock"

#' UTOPIA model modules
#'
#' A kit of ready energyRt building blocks and scenario levers for the UTOPIA
#' teaching model, mirroring the structure of `IDEEA::ideea_modules`. Assemble a
#' model from a chosen region layout, solve it, and layer the levers to run
#' scenarios (see the `utopia-use` vignette). Built by
#' `data-raw/utopia_modules.R`, which follows the same explicit steps as the
#' *UTOPIA I: building the model* vignette.
#'
#' @format A named list:
#' \describe{
#'   \item{info}{a description string.}
#'   \item{maps}{`utopia$map` -- the reference `sf` maps.}
#'   \item{calendars}{the UTOPIA calendars (`annual`/`utopia_seasons`/
#'     `s4_h24`/`m12_h24`) plus the symmetric unit calendars
#'     (`unit_s4`, 4 equal seasons; `unit_s4h4`, 4x4). Note `annual`
#'     cannot drive the shipped kits (`ELC` is balanced at the `HOUR` level
#'     and [utopia_profiles()] has no annual shapes); it ships as the
#'     coarsest example calendar.}
#'   \item{horizons}{planning horizons (`base` = 2020/2030/2040/2050;
#'     `unit` = the single year 2025 for the unit kits).}
#'   \item{electricity}{per region layout (`R1`, `R3`, `R7`, `R11` -- an
#'     n-region layout is keyed `R<n>`, matching the `R1`..`R11` region names
#'     used by the maps), each a "kit":
#'     `repo` (the base repository), the individual blocks (`repo_comm`,
#'     `repo_supply`, `DEM_ELC`, `WSOL`/`WWIN`/`WHYD`, the technologies,
#'     `STG_ELC`), the scenario levers `CO2_CAP`, `CT_CO2`, `RES_SHARE`,
#'     `NO_NEW_NUC`, `EARLY_RET`, and the add-on modules -- re-declared
#'     objects that REPLACE their base counterpart when added with
#'     `add(mod, ., overwrite = TRUE)`: `GAS_CURVE` (domestic gas as a
#'     3-step [asSupplyCurve()], absent in `R1` which has no gas region),
#'     `EWIN_SITES` (wind in two site-grade clusters, finite GOOD +
#'     down-rated POOR), `ENUC_VINT` (nuclear in two build vintages).}
#'   \item{unit}{the "unit model" kits (`U1`, `U3`): every input is 1 on the
#'     symmetric `unit_s4` calendar, single-year `unit` horizon,
#'     `discount = 0`, so each variant's objective is a small hand-checkable
#'     integer -- base `U1` solves to exactly 8, `U3` (trade chain from the
#'     `R1` endowment) to 36, `SUP_CURVE` (2-step unit supply curve
#'     replacing the flat supply) to 10, the self-contained `SOLAR`
#'     repository (on/off resource from [utopia_profile()] + storage
#'     bridging) to 10. The arithmetic is written out in
#'     `data-raw/utopia_modules.R` and pinned by `test-unit-model.R`.}
#' }
#' @seealso [calendars], [horizons], [utopia_profiles()], [utopia_profile()],
#'   [asSupplyCurve()], the UTOPIA vignettes
#' @examples
#' \dontrun{
#' um <- utopia_modules$electricity$R3
#' mod <- newModel("UTOPIA", data = um$repo,
#'                 calendar = utopia_modules$calendars$s4_h24,
#'                 region = um$regions, horizon = utopia_modules$horizons$base,
#'                 discount = 0.05)
#' scen <- solve_scenario(interpolate_model(mod, "BASE"),
#'                        solver = solver_options$glpk)
#'
#' # the unit model: hand-checkable integer objective (8)
#' uk <- utopia_modules$unit$U1
#' umod <- newModel("UNIT", data = uk$repo,
#'                  calendar = utopia_modules$calendars$unit_s4,
#'                  region = uk$regions,
#'                  horizon = utopia_modules$horizons$unit, discount = 0)
#' }
"utopia_modules"
