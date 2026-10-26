# Materialize one weather transform

Applies a named transform to a weather object and returns the DERIVED
weather object – the same clone interpolation creates when a link
selects that transform. For inspection and reporting; the input object
is not modified.

## Usage

``` r
materialize_weather(weather, transform, ...)
```

## Arguments

- weather:

  a `weather` object.

- transform:

  single character, the transform name; resolved
  `weather@misc$transform` -\> session registry -\> built-ins.

- ...:

  arguments forwarded to the transform function (what a link row's
  parameter columns would carry).

## Value

a `weather` object named `<WEATHER>_<transform>` whose `wval` (and
`defVal`) carry the transformed series; `misc$transform_source` records
the provenance.

## See also

[`register_weather_transform()`](https://energyRt.org/reference/register_weather_transform.md)

## Examples

``` r
w <- newWeather("W_SPEED", weather = data.frame(
  timeslice = c("s1", "s2"), wval = c(4, 12)))
w@misc$transform <- list(AF = function(weather, ...) pmin(weather / 10, 1))
materialize_weather(w, "AF")@weather
#>   cluster region year timeslice wval
#> 1    <NA>   <NA>   NA        s1  0.4
#> 2    <NA>   <NA>   NA        s2  1.0
```
