# Register a weather transform for the session

Adds a named transform to the session registry, the middle tier of the
lookup chain `weather@misc$transform` -\> registry -\> built-ins.
Transforms stored in a weather object's `misc$transform` list travel
with the model and take precedence; the registry serves interactive work
and shared helpers. Built-ins `identity` and `scale`
(`weather * wscale`) are always available.

## Usage

``` r
register_weather_transform(name, fun)
```

## Arguments

- name:

  single character, a valid object-name suffix (letters, digits,
  underscore, starting with a letter). The derived weather object is
  named `<WEATHER>_<name>`.

- fun:

  `function(weather, ...) -> numeric`: `weather` receives the stream's
  `wval` series; `...` receives the non-structural columns of the
  selecting link row as named scalars. Must return a numeric vector of
  the input's length.

## Value

`fun`, invisibly.

## See also

[`materialize_weather()`](https://energyRt.org/reference/materialize_weather.md)

## Examples

``` r
register_weather_transform("power_v112",
  function(weather, wscale = 1, ...) {
    v <- data.frame(speed = c(0, 3, 7, 12, 25, 25.01),
                    af = c(0, 0, 0.4, 1, 1, 0))
    approx(v$speed, v$af, pmax(wscale * weather, 0),
           yleft = 0, yright = 0)$y
  })
```
