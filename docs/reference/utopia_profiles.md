# UTOPIA input profiles (deterministic)

Expand UTOPIA's saved, region-agnostic profiles (`utopia$weather`,
`utopia$demand`, `utopia$stock`) to a set of regions for a chosen
calendar, returning a list of three tidy data.frames. The weather
capacity factors can be re-sourced at run time from IDEEA
(`source = "ideea"`); `"saved"` (default) uses the packaged data and
never needs an external dataset.

## Usage

``` r
utopia_profiles(
  regions,
  calendar = c("s4_h24", "m12_h24", "utopia_seasons"),
  source = c("saved", "ideea"),
  resources = c(WSOL = "WSOL", WWIN = "WWIN", WHYD = "WHYD"),
  cluster = 1L,
  diversify = TRUE
)
```

## Arguments

- regions:

  character vector of region names.

- calendar:

  target resolution: `"s4_h24"` (4 seasons x 24 hours, 96 timeslices,
  the default base case), `"m12_h24"` (12 months x 24 hours, 288) or
  `"utopia_seasons"` (4 seasons x 3 dayparts, 12).

- source:

  `"saved"` (packaged data, default) or `"ideea"` (re-aggregate from
  `IDEEA::ideea_modules` if installed).

- resources:

  named character vector mapping resource keys (`WSOL`, `WWIN`, `WHYD`)
  to IDEEA element names, used when `source = "ideea"`.

- cluster:

  integer, which IDEEA resource cluster to use (`source = "ideea"`).

- diversify:

  logical (default `TRUE`): scale the solar and wind capacity factors by
  deterministic per-region factors (defined for the UTOPIA map regions
  `R1`–`R11`; other names get factor 1), so regions have different
  renewable endowments – sunnier south, windier coast. `FALSE`
  replicates identical profiles to every region.

## Value

a list of tidy data.frames, each replicated across `regions`: `weather`
(`resource`, `region`, `timeslice`, `wval`), `demand` (`region`,
`timeslice`, `load` – a relative load shape) and `stock` (`region`,
`tech`, `gw` – base-year capacity).

## Details

Not to be confused with the near-namesake
[`utopia_profile()`](https://energyRt.org/reference/utopia_profile.md),
which takes no UTOPIA data at all: it generates a synthetic step / sine
/ cosine / hex shape over any calendar and returns a single data.frame.

## See also

[utopia](https://energyRt.org/reference/utopia.md),
[`utopia_profile()`](https://energyRt.org/reference/utopia_profile.md),
[calendars](https://energyRt.org/reference/calendars.md)

Other utopia: [`utopia`](https://energyRt.org/reference/utopia.md),
[`utopia_geoscale()`](https://energyRt.org/reference/utopia_geoscale.md),
[`utopia_profile()`](https://energyRt.org/reference/utopia_profile.md)
