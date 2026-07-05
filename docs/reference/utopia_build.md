# Build the UTOPIA model building blocks

Assembles a complete UTOPIA electricity model as a named list of
reusable energyRt building blocks – a commodity repository, a supply
repository, a demand, weather objects, technologies, storage, a
ready-to-solve base repository (`$repo`), and pre-built scenario levers
(CO2 cap, carbon tax, renewable-share target, no-new-nuclear).
Region-dependent inputs (weather, demand, base-year stock) come from
[`utopia_profiles()`](https://energyRt.org/reference/utopia_profiles.md).

## Usage

``` r
utopia_build(
  regions = c("reg1", "reg2", "reg3"),
  calendar = "utopia_s4h24",
  annual_demand = 100,
  years = c(2020, 2030, 2040, 2050),
  demand_growth = c(1, 1.2, 1.4, 1.6)
)
```

## Arguments

- regions:

  character vector of region names.

- calendar:

  UTOPIA calendar name: `"utopia_s4h24"` (default), `"utopia_m12h24"` or
  `"utopia_seasons"`.

- annual_demand:

  base-year electricity demand per region, PJ (default 100).

- years:

  milestone years for demand/levers (default 2020, 2030, 2040, 2050).

- demand_growth:

  demand multipliers per milestone (default 1/1.2/1.4/1.6).

## Value

a named list (a "module kit"): `regions`, `calendar`, `repo` (the base
repository), `repo_comm`, `repo_supply`, `DEM_ELC`, `WSOL`/`WWIN`/
`WHYD`, the technologies (`ECOA` ... `ECOABIO`), `STG_BTR`, and the
levers `CO2_CAP`, `CT_CO2`, `RES_SHARE`, `NO_NEW_NUC`.

## See also

utopia_modules,
[`utopia_profiles()`](https://energyRt.org/reference/utopia_profiles.md),
[calendars](https://energyRt.org/reference/calendars.md)
