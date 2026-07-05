# UTOPIA input profiles (deterministic)

Expand the saved, region-agnostic UTOPIA profiles
([utopia_weather](https://energyRt.org/reference/utopia_weather.md),
[utopia_demand](https://energyRt.org/reference/utopia_demand.md),
[utopia_stock](https://energyRt.org/reference/utopia_stock.md)) to a set
of regions for a chosen calendar. Replaces the vignette's former random
generators. The weather capacity factors can be re-sourced at run time
from IDEEA (`source = "ideea"`); `"saved"` (default) uses the packaged
data and never needs an external dataset.

## Usage

``` r
utopia_profiles(
  regions,
  calendar = c("utopia_s4h24", "utopia_m12h24", "utopia_seasons"),
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

  target resolution: `"utopia_s4h24"` (4 seasons x 24 hours, 96 slices,
  the default base case), `"utopia_m12h24"` (12 months x 24 hours, 288)
  or `"utopia_seasons"` (4 seasons x 3 dayparts, 12).

- source:

  `"saved"` (packaged data, default) or `"ideea"` (re-aggregate from
  [`IDEEA::ideea_modules`](https://ideea-model.github.io/IDEEA/reference/ideea_modules.html)
  if installed).

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
(`resource`, `region`, `slice`, `wval`), `demand` (`region`, `slice`,
`load` – a relative load shape) and `stock` (`region`, `tech`, `gw` –
base-year capacity).

## See also

[utopia_weather](https://energyRt.org/reference/utopia_weather.md),
[utopia_demand](https://energyRt.org/reference/utopia_demand.md),
[utopia_stock](https://energyRt.org/reference/utopia_stock.md),
[calendars](https://energyRt.org/reference/calendars.md)
