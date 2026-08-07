# Geometry of the faded "ghost" vintages behind a drawn technology

Collects the ghost-stack settings of
[`draw()`](https://energyRt.org/reference/draw.md) into one object, so
the drawing methods keep a short signature. Pass either this constructor
or a plain named list: `draw(x, ghost = list(drift = 0.2))` is merged
onto the defaults. Unknown names are an error in both forms –
misspelling a flat argument used to be swallowed silently by `...` and
forwarded downstream.

## Usage

``` r
ghost_options(
  alpha = 0.45,
  alpha_min = 0.12,
  scale = 0.72,
  drift = 0.17,
  rise = 0,
  stamp = TRUE
)
```

## Arguments

- alpha:

  opacity of the nearest ghost; further ones fade towards `alpha_min`.

- alpha_min:

  opacity of the furthest ghost.

- scale, drift, rise:

  geometry of the ghost stack, all describing the FURTHEST ghost rather
  than a per-step increment: its size relative to the selected figure,
  and how far it is offset horizontally and vertically (in npc).
  Intermediate ghosts interpolate. Defining the total extent this way
  keeps a technology with thirty annual vintages inside the viewport – a
  per-step offset would put it off-canvas. `rise` defaults to 0: with
  ghost arrows suppressed there is nothing for a vertical offset to
  clear, so the stack reads better as a flat fan.

- stamp:

  label each ghost with its vintage. The first, last and selected
  vintages are always stamped; intermediate ones only when the spacing
  leaves room, so a dense stack does not turn into overlapping text.

## Value

a list of class `ghost_options`.

## See also

Other draw: [`plot()`](https://energyRt.org/reference/draw.md),
[`theme_energyRt()`](https://energyRt.org/reference/theme_energyRt.md)

## Examples

``` r
ghost_options(drift = 0.25, stamp = FALSE)
#> $alpha
#> [1] 0.45
#> 
#> $alpha_min
#> [1] 0.12
#> 
#> $scale
#> [1] 0.72
#> 
#> $drift
#> [1] 0.25
#> 
#> $rise
#> [1] 0
#> 
#> $stamp
#> [1] FALSE
#> 
#> attr(,"class")
#> [1] "ghost_options"
```
