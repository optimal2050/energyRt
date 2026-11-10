# TOPIA input profiles

Expand TOPIA's per-region profiles (`topia$weather`, `topia$demand`,
`topia$stock`) to a set of regions for a chosen calendar, returning a
list of three tidy data.frames.

## Usage

``` r
topia_profiles(
  regions,
  calendar = c("s4_h24", "m12_h24", "topia_seasons", "d365_h24"),
  dataset = NULL,
  source = c("saved", "ideea"),
  resources = c(WSOL = "WSOL", WWIN = "WWIN", WHYD = "WHYD"),
  cluster = 1L,
  diversify = NULL
)
```

## Arguments

- regions:

  character vector of region codes: a region of the TOPIA map (`W1`,
  `W2`, `C1`–`C6`, `E1`–`E3`), a zone (`WEST`, `CENTRAL`, `EAST`) or the
  nation (`TOPIA`). An unknown code is an error that lists what is
  known.

- calendar:

  target resolution: `"s4_h24"` (4 seasons x 24 hours, 96 timeslices,
  the default base case), `"m12_h24"` (12 months x 24 hours, 288),
  `"topia_seasons"` (4 seasons x 3 dayparts, 12) or `"d365_h24"` (8760).

- dataset:

  `"simulated"` or `"sample"`. `NULL` (the default) picks the set built
  for `calendar` – the sample for `"d365_h24"`, the simulated set
  otherwise. Naming a pair that was not built is an error saying what
  each set covers.

- source:

  `"saved"` (packaged data, default) or `"ideea"` (re-aggregate from
  [`IDEEA::ideea_modules`](https://ideea-model.github.io/IDEEA/reference/ideea_modules.html)
  if installed). The IDEEA route yields one region-agnostic shape per
  resource and replicates it to every region.

- resources:

  named character vector mapping resource keys (`WSOL`, `WWIN`, `WHYD`)
  to IDEEA element names, used when `source = "ideea"`.

- cluster:

  integer, which IDEEA resource cluster to use (`source = "ideea"`).

- diversify:

  defunct. TOPIA's profiles are per region and are not scaled copies of
  one shape, so there is nothing to diversify; passing it warns and
  changes nothing.

## Value

a list of tidy data.frames: `weather` (`resource`, `region`,
`timeslice`, `wval`), `demand` (`region`, `timeslice`, `load` – a
relative load shape) and `stock` (`region`, `tech`, `gw` – base-year
capacity, replicated across `regions`).

## Details

Two input datasets ship and the caller chooses between them:

- `"simulated"`:

  closed-form shapes on the small calendars (`s4_h24`, `m12_h24`,
  `topia_seasons`). Demonstration data: it carries a spatial gradient
  and a diurnal structure, and is not a resource estimate for anywhere.

- `"sample"`:

  a measured hourly set on `d365_h24`. Multi-day calm spells survive
  only at that resolution, so it is where flexibility has something to
  earn – reducing the hourly series to a handful of typical days keeps
  about a quarter of the wind's variance and none of its calm spells,
  which is why the small calendars are served by the simulated set
  instead.

A request for a coarser geoscale code – a zone, or the nation – averages
its member regions, which is what a copperplate zone sees.

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
# weather, demand and stock for two regions of the TOPIA map
prof <- topia_profiles(c("W1", "W2"), calendar = "s4_h24")
#> Error: no TOPIA profile for region 'W1'. Known codes: R01, R02, R03, R04, R05, R06, R07, R08, R09, R10, R11, R12, Z1, Z2, Z3, Z4, Z5, TOPIA
head(prof$weather, 3)
#> Error: object 'prof' not found
```
