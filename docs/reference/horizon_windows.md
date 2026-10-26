# Split a horizon into sequential solve windows

Slices a horizon's milestone intervals into consecutive windows for
sequential solving. Each window's horizon is built from the ORIGINAL
interval rows (start/mid/end preserved exactly); `step` intervals are
DECIDED per window and `overlap` further intervals are appended as
look-ahead (solved for guidance, decisions discarded).

## Usage

``` r
horizon_windows(horizon, step = 1L, overlap = 0L)
```

## Arguments

- horizon:

  a horizon object.

- step:

  integer, milestone intervals decided per window (a scalar, or a vector
  of window sizes summing to at most the interval count).

- overlap:

  integer, look-ahead intervals appended to each window.

## Value

a list of window specs: `horizon` (the window's horizon), `decided` (its
decided milestone years), `lookahead` (the look-ahead milestone years,
possibly empty).

## See also

[`solve_myopic()`](https://energyRt.org/reference/solve_myopic.md)

## Examples

``` r
h <- topia$modules$horizons$base

# one window per milestone year
vapply(horizon_windows(h), function(x) x$horizon@name, character(1))
#> [1] "w01_2025" "w02_2030" "w03_2040" "w04_2050"

# two years per window, overlapping by one
vapply(horizon_windows(h, step = 2L, overlap = 1L),
       function(x) x$horizon@name, character(1))
#> [1] "w01_2025" "w02_2040"
```
