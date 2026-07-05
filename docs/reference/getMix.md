# Extract a tidy mix (generation, capacity, fuel) from a solved scenario

Collects the building blocks of a "mix" chart from a solved scenario
into one tidy data.frame: technology output, storage charge/discharge,
inter-regional and rest-of-world trade, and demand. The same extractor
feeds `autoplot(scenario, ...)`, the scenario report, and any custom
analysis (including sonification – see the useR! training course).

## Usage

``` r
getMix(
  scen,
  type = c("generation", "capacity", "new_capacity", "fuel"),
  comm = "ELC",
  region = NULL,
  year = NULL,
  slice = NULL,
  drop_small = 0
)
```

## Arguments

- scen:

  a solved `scenario` object, or a **named list** of scenarios (results
  are row-bound with the list names in the `scenario` column).

- type:

  character: `"generation"` (default – output of `comm` by process, with
  storage-in and exports negative), `"capacity"` (`vTechCap` +
  `vStorageCap`), `"new_capacity"` (`vTechNewCap` + `vStorageNewCap`),
  or `"fuel"` (technology fuel consumption `vTechInp` by input
  commodity).

- comm:

  character, the balanced commodity for `"generation"` (default
  `"ELC"`). Ignored for the other types.

- region:

  character vector or `NULL` (all regions). Inter-regional trade flows
  are only meaningful for a region subset and are skipped when
  `region = NULL` (they cancel out in an all-region sum).

- year:

  integer vector or `NULL` (all milestone years).

- slice:

  `NULL` for annual sums, or a regular expression selecting a slice
  sample (e.g. `"^SUM_"` for the summer day on the `utopia_s4h24`
  calendar). When the matched slices carry an hour tag
  (`"_h00"..."_h23"`), an integer `hour` column is added.

- drop_small:

  numeric in `[0, 1)`: drop processes whose total absolute value is
  below this share of the largest process (default `0`, keep all).

## Value

A tidy data.frame with columns `scenario`, `type`, `process`, `flow`
(`generation`, `storage-in/out`, `import/export`, `demand`, `fuel`,
`capacity`, `new_capacity`), `comm`, `region`, `year`, `value`, and –
when `slice` is given – `slice` (+ `hour` when parsable). Missing
variables are skipped silently (e.g. a model without storage or trade).

## Examples

``` r
if (FALSE) { # \dontrun{
gen <- getMix(scen, "generation")                      # annual, all regions
day <- getMix(scen, "generation", slice = "^SUM_")     # summer-day dispatch
cmp <- getMix(list(BASE = s1, CO2CAP = s2), "capacity")
} # }
```
