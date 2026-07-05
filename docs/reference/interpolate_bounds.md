# Interpolate lower and upper bounds

Interpolate lower and upper bounds

## Usage

``` r
interpolate_bounds(
  data,
  value_col,
  set_cols,
  int_rule = "mid",
  def_val = NULL,
  value_sfx = c(".lo", ".up", ".fx")
)
```

## Arguments

- data:

  data frame with columns of sets and the parameter to interpolate

- value_col:

  name of the column with the parameter to interpolate

- set_cols:

  optional character vector of set columns to group by, if NULL,

- int_rule:

  interpolation rule, default is "inter" (linear interpolation

- def_val:

  default value for the parameter, if not provided, NA values will be
  returned for missing years not covered by the interpolation rule.

- value_sfx:

  suffix for the value column, default is c(".lo", ".up", ".fx")

## Value

data frame with interpolated values for the parameter
