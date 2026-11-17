# Update a model's elements and the frames they are declared at

Writes `@geoframe` / `@timeframe` on the named elements and on every
object pinned to them, and the slots given in `data`. It changes no
value: a declaration that does not already sit at the requested frame is
refused, naming the object and the column.

## Usage

``` r
# S4 method for class 'model'
update(
  object,
  geoframe = NULL,
  timeframe = NULL,
  data = NULL,
  force = FALSE,
  dryrun = FALSE,
  verbose = isVerbose(),
  ...
)
```

## Arguments

- object:

  a `model`.

- geoframe:

  named frame requests, `list(ELC = "zone")` or `c(ELC = "zone")`; the
  element is a `commodity`, `technology` or `storage`.

- timeframe:

  the same on the time axis, for a `commodity`, `technology` or
  `weather`.

- data:

  named list of per-object slot updates,
  `list(DEM_ELC = list(demand = dem_zone))`, each handed to that
  object's own [`update()`](https://energyRt.org/reference/update.md).

- force:

  write the frames although data is not at them, with a warning naming
  what is left inconsistent – the sequential route. The model will not
  interpolate until the data follows.

- dryrun:

  return the report instead of the model, changing nothing.

- verbose:

  report what moved.

- ...:

  unused.

## Value

The updated `model`, or the report when `dryrun = TRUE`.

## Details

A commodity's frame is where it balances, and the strict classes
carrying it – `demand`, `supply`, `import`, `export`, `trade`, `storage`
– are pinned to it, so they move with it or the call refuses. Those
flows have no aggregation path, which is why they cannot sit at a frame
of their own.

`data` is applied BEFORE the frames move, so a single call carrying both
the re-keyed data and the new frame has nothing left to refuse. Moving
data is otherwise a separate operation, and upward only – see
[`aggregate_model_regions()`](https://energyRt.org/reference/aggregate_model_regions.md)
– because the balance layer only aggregates.

## See also

Other model:
[`find_in_model()`](https://energyRt.org/reference/find_in_model.md),
[`get_region()`](https://energyRt.org/reference/get_region.md),
[`get_weather()`](https://energyRt.org/reference/get_weather.md)

## Examples

``` r
# \donttest{
kit <- topia$modules$electricity
mod <- newModel("TOPIA", data = kit$repo,
                calendar = topia$modules$calendars$d365_h24,
                region = kit$regions,
                horizon = topia$modules$horizons$BY2025_50_by10,
                discount = 0.05)
mod <- setGeoscale(mod, kit$geoscale)

# what moving ELC to the zone would ask for
update(mod, geoframe = list(ELC = "zone"), dryrun = TRUE)
#> ELC: geoframe province -> zone (17 objects)
#> 
#> Data is not at the requested frame:
#>   STG_ELC          storage    capacity$region                R11, R12
#>   TBD_ELC_R02_R03  trade      invcost$region, routes$dst, routes$src, ... (7) R02, R03
#>   TBD_ELC_R02_R05  trade      invcost$region, routes$dst, routes$src, ... (7) R02, R05
#>   TBD_ELC_R04_R05  trade      invcost$region, routes$dst, routes$src, ... (7) R04, R05
#>   TBD_ELC_R04_R06  trade      invcost$region, routes$dst, routes$src, ... (7) R04, R06
#>   TBD_ELC_R05_R06  trade      invcost$region, routes$dst, routes$src, ... (7) R05, R06
#>   TBD_ELC_R02_R07  trade      invcost$region, routes$dst, routes$src, ... (7) R02, R07
#>   TBD_ELC_R03_R07  trade      invcost$region, routes$dst, routes$src, ... (7) R03, R07
#>   ... and 8 more object(s): DEM_ELC, TBD_ELC_R05_R07, TBD_ELC_R06_R10, TBD_ELC_R07_R08, TBD_ELC_R08_R09, TBD_ELC_R09_R10, ... (8) 
#> 
#> Endpoints (src/dst) are a network decision, not a recast: one route per child or one shared route.
#> 
#> Move the data first -- aggregate_model_regions(), or declare it at the frame -- then update the frame. force = TRUE writes the frame now and leaves the data to a later call.
# }
```
