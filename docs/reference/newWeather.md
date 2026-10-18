# Create new weather object

`weather` is a data-carrying class with exogenous shocks used to
influence operation of processes in the model.

## Usage

``` r
newWeather(
  name = "",
  desc = "",
  unit = as.character(NA),
  geoframe = character(),
  region = character(),
  timeframe = character(),
  defVal = 0,
  cluster = data.frame(),
  weather = data.frame(),
  misc = list(),
  ...
)

# S4 method for class 'weather'
update(object, ...)
```

## Arguments

- name:

  character. Name of the weather factor, used in sets.

- desc:

  character. Description of the weather factor.

- unit:

  character. Unit of the weather factor.

- geoframe:

  character. Optional. The geoscale level this object's regions are
  expressed at; unset (the default) derives it from the region codes.
  See `technology@geoframe` for the full rule, including the consistency
  checks it turns on. A weather factor is multiplicative and balances
  nothing, so no commodity rule applies. This is the class where
  declaring the level matters most: left unset, a series whose data
  names no region spans every region of every level, where naming the
  level scopes it to that level alone.

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

- cluster:

  data.frame. Declaration of the object's parallel series. For weather a
  cluster is a RESOURCE TIER – one profile per grade of the same
  resource, so a single object covers a whole tier set instead of one
  hand-named object per tier. This slot declares WHAT the tiers are; the
  per-tier values live in the `cluster` column of `weather`. It is
  REQUIRED whenever that column carries a label, and authoritative:
  labels are never harvested from the series, so a misspelled tier is an
  error rather than a series nobody asked for. At interpolation each
  tier referenced by a process is cloned into an ordinary weather object
  named with the model's cluster prefix (`WWIN` tier `T3` -\>
  `WWIN_CLT3`), so the backends only ever see plain series. A consumer
  finds its tier BY CLUSTER NAME: a `weather` link row of a technology,
  storage or supply carrying `cluster = "T3"` reads this object's `T3`
  rows. A link row with no cluster reads an unclustered object whole, as
  before, and is an ERROR against a clustered one – the factor is
  multiplicative, so "every tier at once" has no reading. LIMITATION –
  there are deliberately no share columns. A share ties parallel parts'
  capacity or activity to fixed proportions, and a capacity-factor
  series has neither. Size the tiers on the processes that read them.

  cluster

  : character. Tier label, used in the expanded object name.

  desc

  : character. Description of the tier.

  region

  : character. Region(s) the tier's series is declared for, NA for
    everywhere. Intersected with the object's own `region` slot. A
    process whose region no tier series covers (directly or through the
    nearest ancestor) is an error, not a silent zero.

  order

  : integer. Reporting order, 1 = the best tier. Without it labels sort
    alphabetically and "T10" would precede "T2".

- weather:

  data.frame. Weather factor values.

  cluster

  : character. Tier this row belongs to, NA for every tier. See the
    `cluster` slot.

  region

  : character. Region name to apply the parameter, NA for every region.

  year

  : integer. Year to apply the parameter, NA for every year.

  timeslice

  : character. Time timeslice to apply the parameter, NA for every
    timeslice.

  wval

  : numeric. Weather factor value.

- misc:

  list. Additional information.

- ...:

  slot-names with data to update (see `newWeather`)

- object:

  object of class weather

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
