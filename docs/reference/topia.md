# The TOPIA reference dataset

Everything the TOPIA teaching model needs, in one list: the maps of the
imaginary country "Topia", the deterministic weather / demand / stock
profiles, and the kit of ready energyRt building blocks and scenario
levers. Built by `data-raw/topia_maps.R`, `topia_data.R` and
`topia_modules.R`, folded together by `data-raw/topia_assemble.R`.

## Usage

``` r
topia
```

## Format

A named list of five elements:

- geoscales:

  named list of four
  [geoscales::GeoScale](https://optimal2050.github.io/geoscales/r/reference/GeoScale.html)
  objects (`squares`, `honeycomb`, `island`, `continent`). Each carries
  the same hierarchy – `nation TOPIA -> zone Z1..Z5 -> province` over
  `R01`..`R12` – and that layout's polygons, plus a planar `area`
  weight. This is the dataset's ONLY copy of the geometry: the plain
  `topia$map` and the `topia$geo` hierarchy table were removed in 0.91.
  Get either back with `geoscales::geoscale_geometry(gs, "province")`
  and `geoscales::geoscale_leaftable(gs)`. Every layout draws the same
  twelve provinces in different places, and each assigns the codes so
  that a zone is one contiguous block on that picture. The finest
  geoframe is `province`, not `region`: energyRt uses `region` for a
  model's own spatial dimension, so a geoframe of that name would make
  `level = "region"` ambiguous. The leaftable still carries a `region`
  column – the atom key, a copy of `province` – so anything joining on
  `region` is unaffected.

- weather:

  solar / wind / hydro capacity factors for the two calendars TOPIA runs
  on. Columns `dataset`, `calendar` (`s4_h24`/`d365_h24`), `resource`
  (`WSOL`/`WWIN`/`WHYD`), `region`, `cluster` (the resource grade, `NA`
  where ungraded), `timeslice` and `wval` (capacity factor, 0-1). The
  `dataset` column is the split that matters: `measured` carries the
  graded hourly series on `d365_h24` – wind in `HI`/`MD`/`LO`, solar in
  `FL`/`TH` – and `simulated` the small stand-in on `s4_h24`, whose
  `cluster` is `NA` throughout. A clustered technology therefore has no
  weather rows on `s4_h24`.

- demand:

  relative electricity-load shapes by province and timeslice, for the
  same two calendars. Columns `dataset`, `calendar`, `region`,
  `timeslice` and `load` (relative, mean ~1); scale by a province's
  annual demand and the timeslice shares to get energy per timeslice.

- stock:

  deterministic base-year installed capacity per technology. Columns
  `tech` and `gw`.

- grades:

  the resource grades the measured weather is split into: `resource`,
  `region`, `cluster`, `ann_cf` (annual capacity factor), `avail` (the
  ceiling, GW) and `cells` (how many grid cells it pools). 36 rows – a
  province appears only in the grades its own ground reaches.

- modules:

  the kit – see below.

## Details

The profile tables are region-agnostic; expand them to a model's regions
with
[`topia_profiles()`](https://energyRt.org/reference/topia_profiles.md).
For synthetic shapes on any calendar (rather than TOPIA's saved curves)
use
[`topia_profile()`](https://energyRt.org/reference/topia_profile.md).

## The `modules` kit

Mirrors the structure of
[`IDEEA::ideea_modules`](https://ideea-model.github.io/IDEEA/reference/ideea_modules.html).
Assemble a model from a chosen region layout, solve it, and layer the
levers to run scenarios (see the `topia-use` vignette).

- info:

  a description string.

- calendars:

  the two calendars TOPIA runs on: `d365_h24` (365 days x 24 hours,
  9,126 timeslices), which is the only one carrying the measured graded
  weather, and `s4_h24` (4 seasons x 24 hours, 96), the small simulated
  stand-in. Samples of the hourly calendar – `d365_h24_1dpm` (301) and
  `d365_h24_1dps` (101) – live in
  [calendars](https://energyRt.org/reference/calendars.md) and keep
  `d365_h24`'s timeslice names, so the measured grades survive being
  filtered onto them. The symmetric unit calendars (`unit_s4`, 4 equal
  seasons; `unit_s4h4`, 4x4) ship beside the unit kits under
  `$unit$calendars`, not here.

- horizons:

  planning horizons, named for their MILESTONES rather than the period
  end, with `BY` marking a one-year base year at the front.
  `BY2025_50_by10` = milestones 2025/2030/2040/2050, a base year then
  10-year intervals (its periods run to 2055). Each milestone is the
  MID-point of its period, so that is where its costs are discounted
  from. `BY2025` = the base year alone, for the unit kits.

- electricity:

  THE model, over all twelve provinces `R01`..`R12`: `repo` (the
  repository), `regions`, `calendar` (the name of the calendar it was
  built for), `geoscale` (its `nation -> zone -> province` hierarchy,
  without geometry – a model MUST carry it, see the examples), and the
  scenario levers `CO2_CAP`, `CT_CO2`, `EARLY_RET` and `GREEN_H2`, which
  are NOT in `repo` and are added to a scenario on top of it. Built by
  `data-raw/topia_model.R`, which tangles `vignettes/topia-build.Rmd` –
  the vignette is the source, so what it documents and what ships cannot
  drift apart. Smaller cases come from
  [`subset_model_regions()`](https://energyRt.org/reference/subset_model_regions.md),
  not from a saved layout.

  The resources are CONCENTRATED, which is what drives trade: `COA` and
  `GAS` are balanced per ZONE (`commodity@geoframe`) and traded between
  zones on `TBD_COA_*` / `TBD_GAS_*` corridors, with finite `ava.up`
  endowments – coal cheap and plentiful in `WEST`, gas in `EAST` – above
  which `IMP_COA` / `IMP_GAS` are the unbounded backstop. Electricity is
  balanced and traded at the region level (`TBD_ELC_*`). Reservoir hydro
  sits in `C4` (12 GW), `E1` (6) and `C1` (3), and the single nuclear
  station in `C1`. Costs are USD (MUSD/GW, MUSD/PJ).

- unit:

  the "unit model" kits (`U1`, `U3`): every input is 1 on the symmetric
  `unit_s4` calendar, single-year `unit` horizon, `discount = 0`, so
  each variant's objective is a small hand-checkable integer – base `U1`
  solves to exactly 8, `U3` (trade chain from the `W1` endowment) to 36,
  `SUP_CURVE` (2-step unit supply curve replacing the flat supply) to
  10, the self-contained `SOLAR` repository (on/off resource from
  [`topia_profile()`](https://energyRt.org/reference/topia_profile.md) +
  storage bridging) to 10. The arithmetic is written out in
  `data-raw/topia_modules.R` and pinned by `test-unit-model.R`.

## See also

[`topia_profiles()`](https://energyRt.org/reference/topia_profiles.md),
[`topia_profile()`](https://energyRt.org/reference/topia_profile.md),
[calendars](https://energyRt.org/reference/calendars.md),
[horizons](https://energyRt.org/reference/horizons.md),
[`asSupplyCurve()`](https://energyRt.org/reference/supply-curve.md), the
TOPIA vignettes

Other topia:
[`topia_profile()`](https://energyRt.org/reference/topia_profile.md),
[`topia_profiles()`](https://energyRt.org/reference/topia_profiles.md),
[`topia_simulate()`](https://energyRt.org/reference/topia_simulate.md)

## Examples

``` r
names(topia)
#> [1] "geoscales" "links"     "weather"   "demand"    "stock"     "grades"   
#> [7] "modules"  
names(topia$geoscales)
#> [1] "squares"   "honeycomb" "island"    "continent"
head(topia$weather)
#>     dataset calendar resource region cluster timeslice  wval
#> 1 simulated   s4_h24     WHYD    R01    <NA>   WIN_h00 0.535
#> 2 simulated   s4_h24     WHYD    R01    <NA>   WIN_h01 0.535
#> 3 simulated   s4_h24     WHYD    R01    <NA>   WIN_h02 0.535
#> 4 simulated   s4_h24     WHYD    R01    <NA>   WIN_h03 0.535
#> 5 simulated   s4_h24     WHYD    R01    <NA>   WIN_h04 0.535
#> 6 simulated   s4_h24     WHYD    R01    <NA>   WIN_h05 0.535
topia$stock
#>   tech gw
#> 1 ECOA  6
#> 2 EGAS  3
#> 3 ENUC  2
#> 4 EHYD  5
#> 5 ESOL  1
#> 6 EWIN  1

if (FALSE) { # \dontrun{
gs <- topia$geoscales$honeycomb          # hierarchy + that layout's polygons
geoscales::geoscale_leaftable(gs)        # the province table
plot_geoscale(gs, type = "map")

um <- topia$modules$electricity
mod <- newModel("TOPIA", data = um$repo,
                calendar = topia$modules$calendars$s4_h24,
                region = um$regions,
                horizon = topia$modules$horizons$BY2025_50_by10,
                discount = 0.05)
mod <- setGeoscale(mod, um$geoscale)
scen <- solve_scenario(interpolate_model(mod, "BASE"),
                       solver = solver_options$glpk)

# the unit model: hand-checkable integer objective (8)
uk <- topia$modules$unit$U1
umod <- newModel("UNIT", data = uk$repo,
                 calendar = topia$modules$unit$calendars$unit_s4,
                 region = uk$regions,
                 horizon = topia$modules$horizons$BY2025, discount = 0)
} # }
```
