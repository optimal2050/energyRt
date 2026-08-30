# Generate a PDF report for an energyRt object

Thin wrapper around [`report`](https://energyRt.org/reference/report.md)
that fixes `format = "pdf"`.

## Usage

``` r
report_pdf(object, ...)

# S4 method for class 'ANY'
report_pdf(object, ...)
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

  The `scenario` method additionally accepts `run` (character or
  `NULL`): the run id to report, as listed by
  [`scenario_runs`](https://energyRt.org/reference/scenario_runs.md)
  (`"<solve>"` or `"<variant>/<solve>"`). The default `NULL` reports the
  active run; a different run is read into a local copy, so the caller's
  scenario keeps its active run. It also accepts `verify` (logical,
  default `TRUE`): include the
  [`verify_solution`](https://energyRt.org/reference/verify_solution.md)
  checks section. For a whole-scenario report, `levcost = TRUE` adds an
  ex-post levelized-cost table via `levcost(scenario)`.

  The model, repository and scenario methods accept branding arguments:
  `logos` (character vector of image paths rendered as a banner row;
  default from `misc$logos`, then the scalar `misc$logo` / `misc$image`
  – a scenario shows its model's logos first, then its own), the model
  method `figure` (an optional half-page hero figure, default
  `misc$figure`), and the scenario method `badges` (small
  property-indicator images, default `misc$badges`). Branding content
  enters the render key, so a changed logo re-renders an otherwise
  up-to-date report.

## Value

Path to the generated `.pdf` file (invisibly).
