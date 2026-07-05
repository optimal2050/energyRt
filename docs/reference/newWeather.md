# Create new weather object

`weather` is a data-carrying class with exogenous shocks used to
influence operation of processes in the model.

## Usage

``` r
newWeather(
  name = "",
  desc = "",
  unit = as.character(NA),
  region = character(),
  timeframe = character(),
  defVal = 0,
  weather = data.frame(),
  misc = list(),
  ...
)
```

## Arguments

- name:

  character. Name of the weather factor, used in sets.

- desc:

  character. Description of the weather factor.

- unit:

  character. Unit of the weather factor.

- region:

  character. Region where the weather factor is applied.

- timeframe:

  character. Timeframe of the weather factor.

- defVal:

  numeric. Default value of the weather factor, 0 by default.

- weather:

  data.frame. Weather factor values.

  region

  :   character. Region name to apply the parameter, NA for every
      region.

  year

  :   integer. Year to apply the parameter, NA for every year.

  slice

  :   character. Time slice to apply the parameter, NA for every slice.

  wval

  :   numeric. Weather factor value.

## Value

weather object with given specifications.

## Details

Weather factors are separated from the model parameters and can be added
or replaced for different scenarios. !!!Additional details...

## Examples

``` r
if (FALSE) { # \dontrun{

# use/make time resolution of the model: timetalbe
ttbl <- make_timetable(tsl_levels$d365_h24)
ttbl

WSOL <- newWeather(
  name = "WSOL",
  desc = "Horiontal solar PV capacity factor",
  timeframe = "HOUR",
  defVal = 0.,
  weather = data.frame(
    region = "R1",
    year = 2015, # 
    slice = ttbl$slice,
    wval = runif(length(ttbl$slice), 0., 1) # use your data
  )
)
} # }
```
