# Interpolate numerical parameter for missing years

Interpolate numerical parameter for missing years

## Usage

``` r
interpolate_numpar(
  data,
  value_col,
  set_cols = NULL,
  int_rule = "inter",
  def_val = NULL
)
```

## Arguments

- data:

  data frame with columns of sets and the parameter to interpolate

- value_col:

  name of the column with the parameter to interpolate

- set_cols:

  optional character vector of set columns to group by, if NULL, all
  columns except `value_col` are used

- int_rule:

  interpolation rule, default is "inter" (linear interpolation between
  years). Other options are "forw" (forward fill) and "back" (backward
  fill) of the last or first value respectively.

- def_val:

  default value for the parameter, if not provided, NA values will be
  returned for missing years not covered by the interpolation rule.

## Value

data frame with interpolated values for the parameter
