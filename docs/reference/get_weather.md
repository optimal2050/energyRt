# Collect the weather objects an object refers to

Reflective accessor that walks every slot of an S4 model object (except
`misc`) and gathers the names in any `weather` column. A `technology`,
`storage` or `supply` links to a weather profile by name; the profile
itself is a separate `weather` object in the repository.

## Usage

``` r
get_weather(object)
```

## Arguments

- object:

  a model object (S4) such as `technology`, `storage` or `supply`.

## Value

a character vector of weather object names (possibly empty).

## See also

Other model:
[`find_in_model()`](https://energyRt.org/reference/find_in_model.md),
[`get_region()`](https://energyRt.org/reference/get_region.md)

## Examples

``` r
# the weather series a process is linked to, read from its own slots
ek <- topia$modules$electricity$R3
get_weather(ek$EWIN)
#> [1] "WWIN"

# a process with no weather link reports none
get_weather(ek$ECOA)
#> character(0)
```
