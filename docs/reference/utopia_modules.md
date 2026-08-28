# UTOPIA model modules

A kit of ready energyRt building blocks and scenario levers for the
UTOPIA teaching model, mirroring the structure of
`IDEEA::ideea_modules`. Assemble a model from a chosen region layout,
solve it, and layer the levers to run scenarios (see the `utopia-use`
vignette). Built by `data-raw/utopia_modules.R`, which follows the same
explicit steps as the *UTOPIA I: building the model* vignette.

## Usage

``` r
utopia_modules
```

## Format

A named list:

- info:

  a description string.

- maps:

  `utopia$map` – the reference `sf` maps.

- calendars:

  the UTOPIA calendars (`annual`/`utopia_seasons`/ `s4_h24`/`m12_h24`)
  plus the symmetric unit calendars (`unit_s4`, 4 equal seasons;
  `unit_s4h4`, 4x4). Note `annual` cannot drive the shipped kits (`ELC`
  is balanced at the `HOUR` level and
  [`utopia_profiles()`](https://energyRt.org/reference/utopia_profiles.md)
  has no annual shapes); it ships as the coarsest example calendar.

- horizons:

  planning horizons (`base` = 2020/2030/2040/2050; `unit` = the single
  year 2025 for the unit kits).

- electricity:

  per region layout (`R1`, `R3`, `R7`, `R11` – an n-region layout is
  keyed `R<n>`, matching the `R1`..`R11` region names used by the maps),
  each a "kit": `repo` (the base repository), the individual blocks
  (`repo_comm`, `repo_supply`, `DEM_ELC`, `WSOL`/`WWIN`/`WHYD`, the
  technologies, `STG_ELC`), the scenario levers `CO2_CAP`, `CT_CO2`,
  `RES_SHARE`, `NO_NEW_NUC`, `EARLY_RET`, and the add-on modules –
  re-declared objects that REPLACE their base counterpart when added
  with `add(mod, ., overwrite = TRUE)`: `GAS_CURVE` (domestic gas as a
  3-step
  [`asSupplyCurve()`](https://energyRt.org/reference/supply-curve.md),
  absent in `R1` which has no gas region), `EWIN_SITES` (wind in two
  site-grade clusters, finite GOOD + down-rated POOR), `ENUC_VINT`
  (nuclear in two build vintages).

- unit:

  the "unit model" kits (`U1`, `U3`): every input is 1 on the symmetric
  `unit_s4` calendar, single-year `unit` horizon, `discount = 0`, so
  each variant's objective is a small hand-checkable integer – base `U1`
  solves to exactly 8, `U3` (trade chain from the `R1` endowment) to 36,
  `SUP_CURVE` (2-step unit supply curve replacing the flat supply) to
  10, the self-contained `SOLAR` repository (on/off resource from
  [`utopia_profile()`](https://energyRt.org/reference/utopia_profile.md) +
  storage bridging) to 10. The arithmetic is written out in
  `data-raw/utopia_modules.R` and pinned by `test-unit-model.R`.

## See also

[calendars](https://energyRt.org/reference/calendars.md),
[horizons](https://energyRt.org/reference/horizons.md),
[`utopia_profiles()`](https://energyRt.org/reference/utopia_profiles.md),
[`utopia_profile()`](https://energyRt.org/reference/utopia_profile.md),
[`asSupplyCurve()`](https://energyRt.org/reference/supply-curve.md), the
UTOPIA vignettes

## Examples

``` r
if (FALSE) { # \dontrun{
um <- utopia_modules$electricity$R3
mod <- newModel("UTOPIA", data = um$repo,
                calendar = utopia_modules$calendars$s4_h24,
                region = um$regions, horizon = utopia_modules$horizons$base,
                discount = 0.05)
scen <- solve_scenario(interpolate_model(mod, "BASE"),
                       solver = solver_options$glpk)

# the unit model: hand-checkable integer objective (8)
uk <- utopia_modules$unit$U1
umod <- newModel("UNIT", data = uk$repo,
                 calendar = utopia_modules$calendars$unit_s4,
                 region = uk$regions,
                 horizon = utopia_modules$horizons$unit, discount = 0)
} # }
```
