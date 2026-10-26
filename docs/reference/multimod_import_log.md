# Get data import log from model

Get data import log from model

## Usage

``` r
multimod_import_log(model)
```

## Arguments

- model:

  Multimod model with import log

## Value

Import log list or NULL if not available

## Examples

``` r
if (FALSE) { # \dontrun{
ek   <- topia$modules$electricity$R3
mod  <- newModel("TOPIA", data = ek$repo,
                 calendar = topia$modules$calendars$s4_h24,
                 region = ek$regions, horizon = topia$modules$horizons$base)
mod <- setGeoscale(mod, ek$geoscale)
scen <- interpolate_model(mod, name = "BASE")

# a model read from its algebra, with empty sets and parameters
m <- multimod::read_gams("my_model.gms")
m <- multimod_import_data(m, scen)

# one row per symbol: what it was filled from, and how many rows
multimod_import_log(m)
} # }
```
