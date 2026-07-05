# UTOPIA model modules

A kit of ready energyRt building blocks and scenario levers for the
UTOPIA teaching model, mirroring the structure of
[`IDEEA::ideea_modules`](https://ideea-model.github.io/IDEEA/reference/ideea_modules.html).
Assemble a model from a chosen region layout, solve it, and layer the
levers to run scenarios (see the `utopia-use` vignette). Built by
`data-raw/utopia_modules.R`, which follows the same explicit steps as
the *UTOPIA I: building the model* vignette.

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

  the UTOPIA calendars (`utopia_annual`/`utopia_seasons`/
  `utopia_s4h24`/`utopia_m12h24`).

- horizons:

  planning horizons (`base` = 2020/2030/2040/2050).

- electricity:

  per region layout (`reg1`, `reg3`, `reg7`), each a "kit": `repo` (the
  base repository), the individual blocks (`repo_comm`, `repo_supply`,
  `DEM_ELC`, `WSOL`/`WWIN`/`WHYD`, the technologies, `STG_BTR`), and the
  scenario levers `CO2_CAP`, `CT_CO2`, `RES_SHARE`, `NO_NEW_NUC`.

## See also

[calendars](https://energyRt.org/reference/calendars.md),
[horizons](https://energyRt.org/reference/horizons.md),
[`utopia_profiles()`](https://energyRt.org/reference/utopia_profiles.md),
the UTOPIA vignettes

## Examples

``` r
if (FALSE) { # \dontrun{
um <- utopia_modules$electricity$reg3
mod <- newModel("UTOPIA", data = um$repo,
                calendar = utopia_modules$calendars$utopia_s4h24,
                region = um$regions, horizon = utopia_modules$horizons$base,
                discount = 0.05)
scen <- solve_scenario(interpolate_model(mod, "BASE"),
                       solver = solver_options$glpk)
} # }
```
