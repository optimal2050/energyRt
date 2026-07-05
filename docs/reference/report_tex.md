# Generate a LaTeX report for an energyRt object

Thin wrapper around [`report`](https://energyRt.org/reference/report.md)
that fixes `format = "tex"`. Produces a standalone `.tex` file without
compiling to PDF.

## Usage

``` r
report_tex(object, ...)

# S4 method for class 'technology'
report_tex(object, ...)
```

## Arguments

- object:

  An energyRt S4 object: `technology`, `repository`, `model`, or solved
  `scenario`.

- ...:

  Arguments forwarded to
  [`levcost`](https://energyRt.org/reference/levcost.md) (when
  `levcost = NULL` and levcost parameters are provided) and/or to
  [`rmarkdown::render()`](https://pkgs.rstudio.com/rmarkdown/reference/render.html).
  Known `levcost` parameter names are intercepted automatically;
  everything else is passed to the renderer.

## Value

Path to the generated `.tex` file (invisibly).
