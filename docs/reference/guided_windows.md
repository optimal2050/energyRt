# Two-milestone windows for a guided solve

Builds the `{y_k, last}` windows the `pairs` stage of
[`solve_guided()`](https://energyRt.org/reference/solve_guided.md)
solves: each interior milestone paired with the horizon's last
milestone, so every subproblem optimises one milestone's expansion while
still seeing the end state. The guided counterpart of
[`horizon_windows()`](https://energyRt.org/reference/horizon_windows.md),
which slices a horizon into CONSECUTIVE windows instead.

The focus interval is widened to cover the years up to the endpoint, so
the window's period lengths span the calendar (`pPeriodLen` is
row-local, so a bare row slice would leave the intervening years
unweighted).

## Usage

``` r
guided_windows(
  horizon,
  direction = c("backward", "forward"),
  include_base = FALSE
)
```

## Arguments

- horizon:

  a horizon object with at least two milestone intervals.

- direction:

  `"backward"` (default) walks the last interior milestone down to the
  first, so each step's future is already decided; `"forward"` walks up
  from the base year.

- include_base:

  logical, include the base-year milestone as a focus window. `FALSE`
  (default) leaves the base year to the `base` stage, which pins it as
  `stock`.

## Value

a list of window specs: `horizon`, `focus` (the milestone being decided)
and `endpoint`.

## See also

[`solve_guided()`](https://energyRt.org/reference/solve_guided.md),
[`horizon_windows()`](https://energyRt.org/reference/horizon_windows.md)

## Examples

``` r
h <- topia$modules$horizons$base

# backward guidance: the endpoint first, then each neighbouring pair
w <- guided_windows(h)
vapply(w, function(x) x$horizon@name, character(1))
#> [1] "g_2040_2050" "g_2030_2050"

# forward instead, including the base period
vapply(guided_windows(h, direction = "forward", include_base = TRUE),
       function(x) x$horizon@name, character(1))
#> [1] "g_2025_2050" "g_2030_2050" "g_2040_2050"
```
