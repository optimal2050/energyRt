# Declared processes that cannot operate

Lists the processes of an interpolated scenario that cannot operate in a
region and year, and why. Only what follows directly from the model data
is reported: an input commodity with no source in the region, no
capacity in a year (no stock left and no investment window), or a bound
or one-term user constraint that fixes the capacity, availability or
activity at zero. Limits that act through the solution, such as an
emission cap or a tax, are not covered.

## Usage

``` r
process_operability(object)
```

## Arguments

- object:

  an interpolated `scenario` (solved or not).

## Value

a data frame with columns `process`, `class`, `region`, `years`
(comma-separated milestone years, or `"all"`), `kind` (`"structural"` or
`"bound"`), `reason` and `detail`; zero rows when every process can
operate.

## Examples

``` r
cal <- newCalendar(
  timetable = make_timetable(struct = list(ANNUAL = "ANNUAL")),
  name = "annual")
mod <- newModel(
  name = "op", region = "R1", horizon = newHorizon(2025), discount = 0,
  calendar = cal,
  repo = newRepository("r",
    newCommodity("ELC"),
    newSupply("SUP", commodity = "ELC", supply = data.frame(cost = 1)),
    newTechnology("T", input = list(comm = "ELC"),
                  output = list(comm = "ELC"), cap2act = 1,
                  capacity = data.frame(stock = 1, cap.up = 0)),
    newDemand("D", commodity = "ELC", demand = data.frame(demand = 1))))
scen <- interpolate_model(mod, name = "op_example")
#> 
#> ── interpolate_model: op_example ───────────────────────────────────────────────
#> storage: in-memory
#> sparse: TRUE
#> prune: TRUE
#> fold: none
#> validate: TRUE
#> ℹ checking declarations
#> ✔ checking declarations (0.0s)
#> ℹ expanding vintage/cluster variants
#> ✔ expanding vintage/cluster variants (0.0s)
#> ℹ materializing commodity clusters
#> ✔ materializing commodity clusters (0.0s)
#> ℹ materializing weather clusters
#> ✔ materializing weather clusters (0.3s)
#> ℹ materializing weather transforms
#> ✔ materializing weather transforms (0.3s)
#> ℹ collecting sets: object names
#> ✔ collecting sets: object names (0.0s)
#> ℹ collecting sets: process classes
#> ✔ collecting sets: process classes (0.0s)
#> ℹ collecting sets: timeframes
#> ✔ collecting sets: timeframes (0.0s)
#> ℹ collecting sets: process inputs/outputs
#> ✔ collecting sets: process inputs/outputs (0.0s)
#> ℹ collecting sets: membership comm sets
#> ✔ collecting sets: membership comm sets (0.0s)
#> ℹ collecting sets: process regions
#> ✔ collecting sets: process regions (0.0s)
#> ℹ building maps: membership + closure
#> Warning: non-unique value when setting 'row.names': 'ELC'
#> Error in `.rowNamesDF<-`(x, value = value): duplicate 'row.names' are not allowed
process_operability(scen)
#> Error: object 'scen' not found
```
