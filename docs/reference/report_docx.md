# Generate a Word report for an energyRt object

Thin wrapper around [`report`](https://energyRt.org/reference/report.md)
that fixes `format = "docx"`.

## Usage

``` r
report_docx(object, ...)

# S4 method for class 'technology'
report_docx(object, ...)
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

Path to the generated `.docx` file (invisibly).
