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
spec <- process_to_spec(topia$modules$electricity$R3$ECOA)

# the constructor call that would rebuild the object, as text
cat(process_spec_code(spec))
#> newTechnology(
#>   name = "ECOA",
#>   desc = "Coal-fired power plant, subcritical",
#>   cap2act = 31.536,
#>   optimizeRetirement = TRUE,
#>   input = data.frame(
#>     comm = c("COA"),
#>     combustion = c(1),
#>     stringsAsFactors = FALSE),
#>   output = data.frame(
#>     comm = c("ELC"),
#>     stringsAsFactors = FALSE),
#>   ceff = data.frame(
#>     comm = c("COA"),
#>     cinp2use = c(0.4),
#>     stringsAsFactors = FALSE),
#>   invcost = data.frame(
#>     invcost = c(2000),
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
#>     region = c("W1", "W2", "C1", "W1", "W2", "C1"),
#>     year = c(2025, 2025, 2025, 2050, 2050, 2050),
#>     stock = c(8.0, 4.0, 1.0, 2.4, 1.2, 0.3),
#>     stringsAsFactors = FALSE)
#> )
```
