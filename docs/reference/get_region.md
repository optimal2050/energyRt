# Collect the regions an object operates in

Generic, reflective accessor that walks every slot of an S4 model object
(except `misc`) and gathers the regions it refers to. Regions are read
from:

- any atomic slot named `region`, `src`, or `dst`, and

- the `region`, `src`, and `dst` columns of any `data.frame` slot.

## Usage

``` r
get_region(object)
```

## Arguments

- object:

  a model object (S4) such as `technology`, `storage`, `trade`,
  `import`, or `export`.

## Value

a character vector of region labels (possibly empty).

## Details

The result is the set of unique, non-missing, non-empty region labels.
The function is intentionally schema-agnostic so it keeps working as
region information is added to classes that do not yet carry an explicit
`@region` slot (e.g. `import` / `export`).

## See also

Other model:
[`find_in_model()`](https://energyRt.org/reference/find_in_model.md),
[`get_weather()`](https://energyRt.org/reference/get_weather.md)

## Examples

``` r
topia_elc <- topia$modules$electricity
mod <- newModel("TOPIA", data = topia_elc$repo,
                calendar = topia$modules$calendars$s4_h24,
                region = topia_elc$regions,
                horizon = topia$modules$horizons$BY2025_50_by10,
                discount = 0.05)
mod <- setGeoscale(mod, topia_elc$geoscale)
get_region(mod)
#>  [1] "W1" "W2" "C1" "C2" "C3" "C4" "C5" "C6" "E1" "E2" "E3"
```
