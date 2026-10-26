# TOPIA input profiles (deterministic)

Expand TOPIA's saved, region-agnostic profiles (`topia$weather`,
`topia$demand`, `topia$stock`) to a set of regions for a chosen
calendar, returning a list of three tidy data.frames. The weather
capacity factors can be re-sourced at run time from IDEEA
(`source = "ideea"`); `"saved"` (default) uses the packaged data and
never needs an external dataset.

## Usage

``` r
topia_profiles(
  regions,
  calendar = c("s4_h24", "m12_h24", "topia_seasons"),
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
  `"topia_seasons"` (4 seasons x 3 dayparts, 12).

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
  deterministic per-region factors (defined for the TOPIA map regions
  `W1`, `W2`, `C1`–`C6`, `E1`–`E3`; other names get factor 1), so
  regions have different renewable endowments – sunnier south, windier
  coast. `FALSE` replicates identical profiles to every region.

## Value

a list of tidy data.frames, each replicated across `regions`: `weather`
(`resource`, `region`, `timeslice`, `wval`), `demand` (`region`,
`timeslice`, `load` – a relative load shape) and `stock` (`region`,
`tech`, `gw` – base-year capacity).

## Details

Not to be confused with the near-namesake
[`topia_profile()`](https://energyRt.org/reference/topia_profile.md),
which takes no TOPIA data at all: it generates a synthetic step / sine /
cosine / hex shape over any calendar and returns a single data.frame.

## See also

[topia](https://energyRt.org/reference/topia.md),
[`topia_profile()`](https://energyRt.org/reference/topia_profile.md),
[calendars](https://energyRt.org/reference/calendars.md)

Other topia: [`topia`](https://energyRt.org/reference/topia.md),
[`topia_profile()`](https://energyRt.org/reference/topia_profile.md)

## Examples

``` r
# weather, demand and stock expanded to the regions of a model
prof <- topia_profiles(c("W1", "W2"), calendar = "s4_h24")
lapply(prof, head, 3)
#> $weather
#>   resource region timeslice wval
#> 1     WSOL     W1   FAL_h00    0
#> 2     WSOL     W1   FAL_h01    0
#> 3     WSOL     W1   FAL_h02    0
#> 
#> $demand
#>   region timeslice load
#> 1     W1   WIN_h00 0.84
#> 2     W1   SPR_h00 0.63
#> 3     W1   SUM_h00 0.77
#> 
#> $stock
#>   region tech gw
#> 1     W1 ECOA  6
#> 2     W1 EGAS  3
#> 3     W1 ENUC  2
#> 
```
