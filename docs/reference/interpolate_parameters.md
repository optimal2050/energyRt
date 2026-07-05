# Interpolate all year-indexed numeric/bounds parameters of a scenario

Interpolate all year-indexed numeric/bounds parameters of a scenario

## Usage

``` r
interpolate_parameters(scen, drop_default = FALSE)
```

## Arguments

- scen:

  scenario object whose `modInp@parameters` already hold the raw
  (sparse) values collected by `ob2mi`.

- drop_default:

  logical; when `TRUE` rows whose interpolated value equals the
  parameter default are dropped (the solver substitutes the declared
  `default`), yielding a smaller data slot. When `FALSE` (the default,
  and the legacy behaviour) default-valued rows are kept and written
  explicitly. Dropping defaults requires the solver/writer to honour
  declared parameter defaults (GLPK / JuMP / Pyomo do; GAMS support is
  unverified).

## Value

the scenario with interpolated parameter data.
