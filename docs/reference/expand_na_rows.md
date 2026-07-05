# Expand rows with NA values in set columns

Replaces `NA` values in a data frame column with all possible values for
each unique combination of other columns.

## Usage

``` r
expand_na_rows(data, column, all_values, group_cols = NULL)

expand_na_regions(data, all_regions, group_cols = NULL)

expand_na_years(data, all_years, group_cols = NULL)

expand_sets(
  data,
  full_sets,
  name_col = NULL,
  value_col = NULL,
  skip_na_dims = FALSE,
  add_missing_dims = !skip_na_dims,
  unmatched_action = c("warning", "drop")
)
```

## Arguments

- data:

  data frame with columns of sets and parameters

- column:

  name of the column to expand, can be a symbol or string

- all_values:

  vector of all possible values for the column

- all_regions:

  vector of all possible regions

- all_years:

  vector of all possible years

- full_sets:

  data frame with all possible combinations of process years, and other
  sets for the process (e.g. region, vintage, etc.). The data frame is
  considered as a full set of elements for the process.

- skip_na_dims:

  logical, if TRUE, do not expand dimensions with all NA values.

- add_missing_dims:

  logical, if TRUE, add missing dimensions to the data from the
  full_sets data frame.

- unmatched_action:

  action to take if no matching process years are found in the data
  frame. Possible values are "warning", "drop", "error", and "ignore".
  Default is a combination of "warning" and "drop".

## Value

data frame with expanded rows
