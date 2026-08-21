# energyRt's default ggplot theme

A thin wrapper around `theme_energyRt()` giving every energyRt figure
one base size and look. Package plots call it instead of
[`theme_bw()`](https://ggplot2.tidyverse.org/reference/ggtheme.html)
directly, so restyling them is a one-line change here rather than an
edit to every plotting function.

## Usage

``` r
theme_energyRt(base_size = 11, ...)
```

## Arguments

- base_size:

  base font size in points.

- ...:

  passed on to `theme_energyRt()`.

## Value

A ggplot2 theme object.

## See also

Other draw:
[`ghost_options()`](https://energyRt.org/reference/ghost_options.md),
[`plot()`](https://energyRt.org/reference/draw.md)

## Examples

``` r
if (FALSE) { # \dontrun{
ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
  ggplot2::geom_point() + theme_energyRt()
} # }
```
