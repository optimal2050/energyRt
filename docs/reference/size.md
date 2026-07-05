# Size of an object

Size of an object

## Usage

``` r
size(
  x,
  level1 = FALSE,
  units = "auto",
  sort = TRUE,
  decreasing = FALSE,
  byteTol = 0,
  asNumeric = FALSE
)
```

## Arguments

- x:

  any R object

- level1:

  logical, if TRUE, the function will return the size of the object and
  its slots (if any)

- units:

  character, units to display the size, default is "auto"

- sort:

  logical, if TRUE, the function will sort the slots by size

- decreasing:

  logical, if TRUE, the function will sort the slots in decreasing order

- byteTol:

  numeric, threshold in bytes to filter the slots

- asNumeric:

  logical, if TRUE, the function will return the size of the object and
  its slots in bytes

## Value

character value or vector, size of the object or its slots

## Examples

``` r
size(1)
#> [1] "56 bytes"
size(rep(1, 1e3))
#> [1] "7.9 Kb"
size(rep(1L, 1e3))
#> [1] "4 Kb"
```
