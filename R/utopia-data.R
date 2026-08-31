# Dataset documentation for the UTOPIA teaching model.

#' The UTOPIA reference dataset
#'
#' Everything the UTOPIA teaching model needs, in one list: the maps of the
#' imaginary country "Utopia", the deterministic weather / demand / stock
#' profiles, and the kit of ready energyRt building blocks and scenario levers.
#' Built by `data-raw/utopia_maps.R`, `utopia_data.R` and `utopia_modules.R`,
#' folded together by `data-raw/utopia_assemble.R`.
#'
#' The profile tables are region-agnostic; expand them to a model's regions
#' with [utopia_profiles()]. For synthetic shapes on any calendar (rather than
#' UTOPIA's saved curves) use [utopia_profile()].
#'
#' @format A named list of six elements:
#' \describe{
#'   \item{map}{named list of `sf` polygon layers (`squares`, `honeycomb`,
#'     `island`, `continent`) used to lay out regions, neighbours and trade
#'     routes.}
#'   \item{geo}{a data.frame with columns `nation`, `zone`, `region`, `name` --
#'     the region hierarchy behind the maps.}
#'   \item{weather}{deterministic solar / wind / hydro capacity factors for the
#'     three teaching calendars. Columns `calendar`
#'     (`s4_h24`/`m12_h24`/`utopia_seasons`), `resource`
#'     (`WSOL`/`WWIN`/`WHYD`), `timeslice` (e.g. `SUM_h12`, `m06_h12` or
#'     `SUM_DAY`) and `wval` (capacity factor, 0-1). Attribute `source` records
#'     whether it came from IDEEA reanalysis or the curated fallback.}
#'   \item{demand}{a relative electricity-load shape by timeslice, for the same
#'     three calendars. Columns `calendar`, `timeslice` and `load` (relative,
#'     mean ~1); scale it by a region's annual demand and the timeslice shares
#'     to get energy per timeslice.}
#'   \item{stock}{deterministic base-year installed capacity per technology.
#'     Columns `tech` and `gw`.}
#'   \item{modules}{the kit -- see below.}
#' }
#'
#' @section The `modules` kit:
#' Mirrors the structure of `IDEEA::ideea_modules`. Assemble a model from a
#' chosen region layout, solve it, and layer the levers to run scenarios (see
#' the `utopia-use` vignette).
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
#'
#' @seealso [utopia_profiles()], [utopia_profile()], [utopia_geoscale()],
#'   [calendars], [horizons], [asSupplyCurve()], the UTOPIA vignettes
#' @family utopia
#' @examples
#' names(utopia)
#' names(utopia$map)
#' head(utopia$weather)
#' utopia$stock
#'
#' \dontrun{
#' um <- utopia$modules$electricity$R3
#' mod <- newModel("UTOPIA", data = um$repo,
#'                 calendar = utopia$modules$calendars$s4_h24,
#'                 region = um$regions,
#'                 horizon = utopia$modules$horizons$base,
#'                 discount = 0.05)
#' scen <- solve_scenario(interpolate_model(mod, "BASE"),
#'                        solver = solver_options$glpk)
#'
#' # the unit model: hand-checkable integer objective (8)
#' uk <- utopia$modules$unit$U1
#' umod <- newModel("UNIT", data = uk$repo,
#'                  calendar = utopia$modules$calendars$unit_s4,
#'                  region = uk$regions,
#'                  horizon = utopia$modules$horizons$unit, discount = 0)
#' }
"utopia"

#' Deprecated UTOPIA datasets
#'
#' These four datasets are now elements of the single [utopia] list --
#' `utopia$weather`, `utopia$demand`, `utopia$stock` and `utopia$modules`.
#' They are still shipped so existing code keeps working, and will be removed
#' in energyRt v0.90.
#'
#' @format See [utopia].
#' @name utopia-deprecated-data
#' @keywords internal
#' @seealso [utopia]
"utopia_weather"

#' @rdname utopia-deprecated-data
"utopia_demand"

#' @rdname utopia-deprecated-data
"utopia_stock"

#' @rdname utopia-deprecated-data
"utopia_modules"
