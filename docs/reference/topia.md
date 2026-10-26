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
  the same hierarchy –
  `nation TOPIA -> zone WEST/CENTRAL/EAST -> region` over `W1`, `W2`,
  `C1`..`C6`, `E1`..`E3` – and that layout's polygons, plus a planar
  `area` weight. This is the dataset's ONLY copy of the geometry: the
  plain `topia$map` and the `topia$geo` hierarchy table were removed in
  0.91. Get either back with
  `geoscales::geoscale_geometry(gs, "region")` and
  `geoscales::geoscale_leaftable(gs)`. Every layout draws the same
  eleven regions in different places, and each assigns the codes so that
  a zone is one contiguous block on that picture.

- weather:

  deterministic solar / wind / hydro capacity factors for the three
  teaching calendars. Columns `calendar`
  (`s4_h24`/`m12_h24`/`topia_seasons`), `resource`
  (`WSOL`/`WWIN`/`WHYD`), `timeslice` (e.g. `SUM_h12`, `m06_h12` or
  `SUM_DAY`) and `wval` (capacity factor, 0-1). Attribute `source`
  records whether it came from IDEEA reanalysis or the curated fallback.

- demand:

  a relative electricity-load shape by timeslice, for the same three
  calendars. Columns `calendar`, `timeslice` and `load` (relative, mean
  ~1); scale it by a region's annual demand and the timeslice shares to
  get energy per timeslice.

- stock:

  deterministic base-year installed capacity per technology. Columns
  `tech` and `gw`.

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

  the TOPIA calendars (`annual`/`topia_seasons`/ `s4_h24`/`m12_h24`)
  plus the symmetric unit calendars (`unit_s4`, 4 equal seasons;
  `unit_s4h4`, 4x4). Note `annual` cannot drive the shipped kits (`ELC`
  is balanced at the `HOUR` level and
  [`topia_profiles()`](https://energyRt.org/reference/topia_profiles.md)
  has no annual shapes); it ships as the coarsest example calendar.

- horizons:

  planning horizons (`base` = 2025/2030/2040/2050, each milestone the
  MID-point of its period, so that is where its costs are discounted
  from; `unit` = the single year 2025 for the unit kits).

- electricity:

  per region layout (`R1`, `R3`, `R7`, `R11` – an n-region layout is
  keyed `R<n>` and takes the first n regions of the canonical
  west-to-east order `W1`, `W2`, `C1`..`C6`, `E1`..`E3`), each a "kit":
  `repo` (the base repository), `geoscale` (the layout's slice of the
  `nation -> zone -> region` hierarchy, without geometry – a kit model
  MUST carry it, see the examples), the individual blocks (`repo_comm`,
  `repo_supply`, `DEM_ELC`, `WSOL`/`WWIN`/`WHYD`, the technologies,
  `STG_ELC`), the scenario levers `CO2_CAP`, `CT_CO2`, `RES_SHARE`,
  `NO_NEW_NUC`, `EARLY_RET`, and the add-on modules – re-declared
  objects that REPLACE their base counterpart when added with
  `add(mod, ., overwrite = TRUE)`: `GAS_CURVE` (domestic gas as a 3-step
  [`asSupplyCurve()`](https://energyRt.org/reference/supply-curve.md)),
  `EWIN_SITES` (wind in two site-grade clusters, finite GOOD +
  down-rated POOR), `ENUC_VINT` (nuclear in two build vintages, absent
  from the `R1` kit, which reaches no nuclear region).

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
[`topia_profiles()`](https://energyRt.org/reference/topia_profiles.md)

## Examples

``` r
names(topia)
#> [1] "geoscales" "weather"   "demand"    "stock"     "modules"  
names(topia$geoscales)
#> [1] "squares"   "honeycomb" "island"    "continent"
head(topia$weather)
#>   calendar resource timeslice       wval
#> 1   s4_h24     WSOL   FAL_h00 0.00000000
#> 2   s4_h24     WSOL   FAL_h01 0.00000000
#> 3   s4_h24     WSOL   FAL_h02 0.00000000
#> 4   s4_h24     WSOL   FAL_h03 0.00000000
#> 5   s4_h24     WSOL   FAL_h04 0.00000000
#> 6   s4_h24     WSOL   FAL_h05 0.00132967
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
geoscales::geoscale_leaftable(gs)        # the region table
plot_geoscale(gs, type = "map")

um <- topia$modules$electricity$R3
mod <- newModel("TOPIA", data = um$repo,
                calendar = topia$modules$calendars$s4_h24,
                region = um$regions,
                horizon = topia$modules$horizons$base,
                discount = 0.05)
mod <- setGeoscale(mod, um$geoscale)
scen <- solve_scenario(interpolate_model(mod, "BASE"),
                       solver = solver_options$glpk)

# the unit model: hand-checkable integer objective (8)
uk <- topia$modules$unit$U1
umod <- newModel("UNIT", data = uk$repo,
                 calendar = topia$modules$unit$calendars$unit_s4,
                 region = uk$regions,
                 horizon = topia$modules$horizons$unit, discount = 0)
} # }
```
