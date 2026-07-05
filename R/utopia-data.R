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
#' provided for both teaching calendars (`utopia_m12h24`, 288 slices, the
#' default; and `utopia_seasons`, 12). Region-agnostic; expand to regions with
#' [utopia_profiles()]. Built by `data-raw/utopia_data.R` from IDEEA reanalysis
#' profiles (with a curated fallback when IDEEA is absent); see `attr(.,"source")`.
#'
#' @format A data.frame with columns `calendar`
#'   (`utopia_m12h24`/`utopia_seasons`), `resource` (`WSOL`/`WWIN`/`WHYD`),
#'   `slice` (e.g. `m06_h12` or `SUM_DAY`) and `wval` (capacity factor, 0-1).
#'   Attribute `source`.
#' @seealso [utopia_profiles()], [utopia_demand], [utopia_stock], [calendars]
#' @examples
#' head(utopia_weather)
#' attr(utopia_weather, "source")
"utopia_weather"

#' UTOPIA electricity load shape
#'
#' A deterministic relative electricity-load shape by slice, for both teaching
#' calendars (replaces the vignette's former random load curve).
#' [utopia_profiles()] / the vignette scale it by a region's annual demand and
#' the slice shares to get energy per slice.
#'
#' @format A data.frame with columns `calendar`
#'   (`utopia_m12h24`/`utopia_seasons`), `slice` and `load` (relative, mean ~1).
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
#'   \item{calendars}{the UTOPIA calendars (`utopia_annual`/`utopia_seasons`/
#'     `utopia_s4h24`/`utopia_m12h24`).}
#'   \item{horizons}{planning horizons (`base` = 2020/2030/2040/2050).}
#'   \item{electricity}{per region layout (`reg1`, `reg3`, `reg7`), each a "kit":
#'     `repo` (the base repository), the individual blocks (`repo_comm`,
#'     `repo_supply`, `DEM_ELC`, `WSOL`/`WWIN`/`WHYD`, the technologies,
#'     `STG_BTR`), and the scenario levers `CO2_CAP`, `CT_CO2`, `RES_SHARE`,
#'     `NO_NEW_NUC`.}
#' }
#' @seealso [calendars], [horizons], [utopia_profiles()], the UTOPIA vignettes
#' @examples
#' \dontrun{
#' um <- utopia_modules$electricity$reg3
#' mod <- newModel("UTOPIA", data = um$repo,
#'                 calendar = utopia_modules$calendars$utopia_s4h24,
#'                 region = um$regions, horizon = utopia_modules$horizons$base,
#'                 discount = 0.05)
#' scen <- solve_scenario(interpolate_model(mod, "BASE"),
#'                        solver = solver_options$glpk)
#' }
"utopia_modules"
