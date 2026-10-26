# Show unmatched elements

Show unmatched elements

## Usage

``` r
multimod_unmatched(model)
```

## Arguments

- model:

  Multimod model with import log

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

# the symbols the scenario had no data for -- the list to work through
# before the imported model will solve
multimod_unmatched(m)
} # }
```
