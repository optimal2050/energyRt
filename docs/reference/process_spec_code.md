# The constructor call equivalent to a process spec, as text

Used by the designer's export tab.

## Usage

``` r
process_spec_code(spec)
```

## Arguments

- spec:

  Path or list (see
  [`read_process_spec()`](https://energyRt.org/reference/read_process_spec.md)).

## Value

A single character string of R code.

## See also

Other process spec:
[`process_from_spec()`](https://energyRt.org/reference/process_from_spec.md),
[`process_spec_issues()`](https://energyRt.org/reference/process_spec_issues.md),
[`process_to_spec()`](https://energyRt.org/reference/process_to_spec.md),
[`read_process_spec()`](https://energyRt.org/reference/read_process_spec.md)

## Examples

``` r
spec <- process_to_spec(getObject(topia$modules$electricity$repo, name = "ECOA", drop = TRUE))

# the constructor call that would rebuild the object, as text
cat(process_spec_code(spec))
#> newTechnology(
#>   name = "ECOA",
#>   desc = "Coal-fired power plant, subcritical",
#>   units = list(capacity = "GW", activity = "GWh", currency = "MUSD"),
#>   cap2act = 8760,
#>   optimizeRetirement = TRUE,
#>   input = data.frame(
#>     comm = c("COA"),
#>     unit = c("PJ"),
#>     combustion = c(1),
#>     stringsAsFactors = FALSE),
#>   output = data.frame(
#>     comm = c("ELC"),
#>     unit = c("GWh"),
#>     stringsAsFactors = FALSE),
#>   ceff = data.frame(
#>     comm = c("COA", "ELC"),
#>     cinp2use = c(0.4, NA),
#>     use2cact = c(NA, 277.7778),
#>     stringsAsFactors = FALSE),
#>   invcost = data.frame(
#>     invcost = c(2000),
#>     retcost = c(25),
#>     stringsAsFactors = FALSE),
#>   fixom = data.frame(
#>     fixom = c(55),
#>     stringsAsFactors = FALSE),
#>   vintage = data.frame(
#>     start = c(2010),
#>     end = c(2030),
#>     olife = c(30),
#>     stringsAsFactors = FALSE),
#>   capacity = data.frame(
#>     region = c("R01", "R02", "R03", "R04", "R05", "R06", "R07", "R08", "R09", "R10", "R11", "R12", "R01", "R02", "R03", "R04", "R05", "R06", "R07", "R08", "R09", "R10", "R11", "R12"),
#>     year = c(2025, 2025, 2025, 2025, 2025, 2025, 2025, 2025, 2025, 2025, 2025, 2025, 2050, 2050, 2050, 2050, 2050, 2050, 2050, 2050, 2050, 2050, 2050, 2050),
#>     stock = c(1.0, 1.0, 1.0, 1.0, 1.0, 3.0, 1.0, 8.0, 4.0, 1.0, 1.0, 1.0, 0.3, 0.3, 0.3, 0.3, 0.3, 0.9, 0.3, 2.4, 1.2, 0.3, 0.3, 0.3),
#>     stringsAsFactors = FALSE)
#> )
```
