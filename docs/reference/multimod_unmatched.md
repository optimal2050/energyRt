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
topia_elc <- topia$modules$electricity
mod  <- newModel("TOPIA", data = topia_elc$repo,
                 calendar = topia$modules$calendars$s4_h24,
                 region = topia_elc$regions,
                 horizon = topia$modules$horizons$BY2025_50_by10)
mod <- setGeoscale(mod, topia_elc$geoscale)
scen <- interpolate_model(mod, name = "BASE")

# a model read from its algebra, with empty sets and parameters
m <- multimod::read_gams("my_model.gms")
m <- multimod_import_data(m, scen)

# the symbols the scenario had no data for -- the list to work through
# before the imported model will solve
multimod_unmatched(m)
} # }
```
