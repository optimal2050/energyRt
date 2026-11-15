# Synthetic TOPIA profiles

Generate TOPIA weather and demand profiles from a closed-form model of
the map, at any resolution and for any cluster roster. The shipped
`"simulated"` dataset ([topia](https://energyRt.org/reference/topia.md))
is one cached run of this function, so `topia_simulate()` at its
defaults reproduces it and every other parameter set is an alternative.

The output is shaped like
[`topia_profiles()`](https://energyRt.org/reference/topia_profiles.md)'s,
so either can be passed to `build_topia(weather = , demand = )`.

## Usage

``` r
topia_simulate(
  regions = NULL,
  calendar = "s4_h24",
  clusters = NULL,
  solar_season = .SIM_M_SOL,
  wind_season = .SIM_M_WIN,
  hydro_season_calm = .SIM_M_HYD_A,
  hydro_season_windy = .SIM_M_HYD_B,
  wind_day_calm = .SIM_H_WIN_CALM,
  wind_day_windy = .SIM_H_WIN_WINDY,
  load_day_res = .SIM_H_RES,
  load_day_ind = .SIM_H_IND,
  load_season_res = .SIM_M_RES,
  load_season_ind = .SIM_M_IND,
  residential = .SIM_W_RES,
  wind_grade = .SIM_WIN_GRADE,
  solar_halflen = .SIM_SOL_HALFLEN,
  hydro_cf = .SIM_HYD_CF,
  jitter = 0,
  seed = NULL,
  layout = "honeycomb",
  measured = NULL
)
```

## Arguments

- regions:

  provinces to generate, defaulting to all of TOPIA's.

- calendar:

  a calendar name, a calendar object, or a timetable `data.frame`. It
  must carry an `HOUR` frame and either `YDAY` or `SEASON` above it.

- clusters:

  the roster to generate: a `data.frame` of `resource`, `cluster` and
  `region`, or `NULL` to take the rosters the shipped weather objects
  declare. `FALSE` generates one ungraded series per resource.

- solar_season, wind_season, hydro_season_calm, hydro_season_windy:

  twelve monthly multipliers each, normalised to mean 1 internally.

- wind_day_calm, wind_day_windy, load_day_res, load_day_ind:

  twenty-four hourly values each: the endpoints the shapes are blended
  between.

- load_season_res, load_season_ind:

  twelve monthly multipliers each.

- residential:

  residential share of load by zone, named.

- wind_grade, solar_halflen:

  the cluster shape parameters, named by cluster.

- hydro_cf:

  annual mean inflow factor, used where the measured set has no hydro
  series for a province.

- jitter:

  relative amplitude of a deterministic perturbation applied to each
  field, `0` for none. The mean is preserved and no values are made
  equal, so the energy is unbiased and the LP stays non-degenerate.

- seed:

  integer seed for `jitter`.

- layout:

  the geoscale whose zones the load mix is keyed on.

- measured:

  the measured frames the levels are calibrated against, as
  `list(weather =, demand =)`. `NULL` reads the shipped set, which is
  what a user wants; the dataset build passes its own frames, since at
  that point the shipped set is what is being written.

## Value

a list of two tidy `data.frame`s: `weather` (`resource`, `cluster`,
`region`, `timeslice`, `wval`) and `demand` (`region`, `timeslice`,
`load`), both carrying a `dataset` attribute naming the generator and
the parameters that differ from the defaults.

## Details

Levels are calibrated against the shipped measured set: each
`(resource, cluster, province)` field is scaled to the mean capacity
factor measured there, and the load shape to the measured peak-to-mean.
What the arguments change is the SHAPE.

Clusters differ through one parameter each – `wind_grade` offsets a
grade along the calm-to-windy axis, `solar_halflen` sets the half-length
of the solar day per mounting – so a grade is a different resource
rather than another ceiling on one series. A roster with no clusters
yields one series per resource with `cluster` `NA`, which no graded
technology can use; see `build_topia(clusters = )`.

## See also

[`topia_profiles()`](https://energyRt.org/reference/topia_profiles.md)
for the shipped sets,
[`build_topia()`](https://energyRt.org/reference/build_topia.md),
[`topia_profile()`](https://energyRt.org/reference/topia_profile.md) for
geometric shapes

Other topia: [`topia`](https://energyRt.org/reference/topia.md),
[`topia_profile()`](https://energyRt.org/reference/topia_profile.md),
[`topia_profiles()`](https://energyRt.org/reference/topia_profiles.md)

## Examples

``` r
p <- topia_simulate(regions = c("R01", "R10"), calendar = "s4_h24")
head(p$weather, 3)
#>   resource cluster region timeslice wval
#> 1     WSOL      FL    R01   WIN_h00    0
#> 2     WSOL      FL    R01   WIN_h01    0
#> 3     WSOL      FL    R01   WIN_h02    0

# an alternative: no seasonality in the wind
p2 <- topia_simulate(regions = c("R01", "R10"), wind_season = rep(1, 12))
```
