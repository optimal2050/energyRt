# Display labels of the model's milestone years

The milestone key in a model is the integer year
(`horizon@intervals$mid`). `year_label()` returns the display name of
each milestone: the year itself for an ordinary January-start calendar,
or the fiscal form (`FY2025-26`) when the calendar carries a non-January
`year_start`. Labels are presentation only – they never reach the
solver.

Set a label explicitly with the `label` column of the `intervals` table
in [`newHorizon()`](https://energyRt.org/reference/newHorizon.md); an
explicit label always wins over the derived one.

## Usage

``` r
year_label(object)
```

## Arguments

- object:

  a `horizon`, `calendar`, `config`, `settings`, `model` or `scenario`
  object. A bare `horizon` has no calendar, so it never yields fiscal
  labels.

## Value

Named character vector: labels, named by the integer milestone year.

## Examples

``` r
year_label(newHorizon(2020:2030, c(1, 2, 5, 10)))
#>   2020   2021   2025   2028 
#> "2020" "2021" "2025" "2028" 
```
