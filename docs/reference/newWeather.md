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

  character. Region(s) the weather factor is declared at. May be a
  COARSER region of the model's geoscale than the processes that use it:
  a process reads the series at its own region when the object serves
  it, otherwise at the NEAREST ancestor that does. One profile declared
  at a parent therefore feeds every child region from a single stored
  series instead of being copied per child. Unset means every region. A
  process whose region the object neither serves nor has an ancestor for
  is an error – the factor is multiplicative and defaults to 0, so an
  unresolved link would shut the process down silently.

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

  timeslice

  :   character. Time timeslice to apply the parameter, NA for every
      timeslice.

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
    timeslice = ttbl$timeslice,
    wval = runif(length(ttbl$timeslice), 0., 1) # use your data
  )
)
} # }
```
