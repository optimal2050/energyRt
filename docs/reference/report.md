# Generate a summary report for an energyRt object

Creates a PDF, HTML, or LaTeX document summarising key parameters of the
object. The layout and content are controlled by the `template`
argument.

For a `technology`, the datasheet describes that technology. For a
`repository`, `model`, or solved `scenario`, give the technology via
`name = `: the same datasheet is produced, but the embedded levelized
cost is computed for that container / solution (the `scenario` method
uses the *ex-post* cost from the solved model).

**Without** `name`, the container methods report the whole object:

- `model` / `repository`:

  a full model report – the configuration (regions, horizon, calendar,
  discount), an inventory of commodities / supplies / demands / trade /
  constraints, the process availability windows chart, and every
  technology and storage described one-by-one (diagram + key
  parameters).

- `scenario`:

  a results overview of the solved scenario – solve status and
  objective, generation / capacity / new-capacity mixes (via
  [`getMix`](https://energyRt.org/reference/getMix.md) and `autoplot`),
  a sub-annual dispatch profile when the calendar has one, emissions and
  cost tables.

## Usage

``` r
report(
  object,
  template = NULL,
  image_file = NULL,
  file = NULL,
  format = c("html", "pdf", "tex"),
  levcost = NULL,
  cost_unit = NULL,
  open = interactive(),
  ...
)

# S4 method for class 'technology'
report(
  object,
  template = NULL,
  image_file = NULL,
  file = NULL,
  format = c("html", "pdf", "tex"),
  levcost = NULL,
  cost_unit = NULL,
  open = interactive(),
  ...
)

# S4 method for class 'repository'
report(
  object,
  template = NULL,
  image_file = NULL,
  file = NULL,
  format = c("html", "pdf", "tex"),
  levcost = NULL,
  cost_unit = NULL,
  open = interactive(),
  ...
)

# S4 method for class 'model'
report(
  object,
  template = NULL,
  image_file = NULL,
  file = NULL,
  format = c("html", "pdf", "tex"),
  levcost = NULL,
  cost_unit = NULL,
  open = interactive(),
  ...
)

# S4 method for class 'scenario'
report(
  object,
  template = NULL,
  image_file = NULL,
  file = NULL,
  format = c("html", "pdf", "tex"),
  levcost = NULL,
  cost_unit = NULL,
  open = interactive(),
  ...
)
```

## Arguments

- object:

  An energyRt S4 object: `technology`, `repository`, `model`, or solved
  `scenario`.

- template:

  Character. Template name that defines which parameters to display.
  Currently `"generic"` is built into the package. Pass the absolute
  path to a custom `.Rmd` file to use your own template. Default `NULL`
  selects `"generic"` automatically.

- image_file:

  Character. Optional path to a PNG/JPG image displayed in the
  upper-right corner of the page. `NULL` skips the image.

- file:

  Character. Destination file path. Defaults to `report_<name>` in the
  current working directory, with the extension appropriate for
  `format`.

- format:

  Character. Output format: `"html"` (default), `"pdf"`, or `"tex"`
  (standalone LaTeX source). Multiple values are accepted; one file is
  produced per format. `"pdf"`/`"tex"` require a LaTeX installation
  (e.g.
  [`tinytex::install_tinytex()`](https://rdrr.io/pkg/tinytex/man/install_tinytex.html))
  and are skipped with a warning when none is found.

- levcost:

  A `levcost` (or `levcost_list`) object returned by
  [`levcost`](https://energyRt.org/reference/levcost.md), or `NULL`
  (default). When `NULL` and any `levcost` keyword arguments are passed
  via `...` (e.g. `group`, `repo`, `discount`),
  [`levcost()`](https://energyRt.org/reference/levcost.md) is called
  automatically on `object` with those arguments.

- cost_unit:

  Character or `NULL`. Cost unit label used on LCOE axis (e.g.
  `"USD/GJ"`). `NULL` derives the label from `object@units`.

- open:

  Logical. Open the rendered report in the system browser/viewer when
  done. Defaults to
  [`interactive()`](https://rdrr.io/r/base/interactive.html) (opens in
  an interactive session, stays quiet in scripts and knits).

- ...:

  Arguments forwarded to
  [`levcost`](https://energyRt.org/reference/levcost.md) (when
  `levcost = NULL` and levcost parameters are provided) and/or to
  [`rmarkdown::render()`](https://pkgs.rstudio.com/rmarkdown/reference/render.html).
  Known `levcost` parameter names are intercepted automatically;
  everything else is passed to the renderer.

- name:

  Character (container/scenario methods). Name of the technology /
  process to report.

## Value

The path(s) to the generated output file(s) (invisibly). A single string
when one format is requested; a character vector when multiple formats
are requested.

## See also

[`levcost`](https://energyRt.org/reference/levcost.md),
[`report_pdf`](https://energyRt.org/reference/report_pdf.md),
[`report_html`](https://energyRt.org/reference/report_html.md),
[`report_tex`](https://energyRt.org/reference/report_tex.md)
