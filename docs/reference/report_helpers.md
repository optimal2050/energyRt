# Report template helpers

Exported building blocks for
[`report()`](https://energyRt.org/reference/report.md) templates. The
shipped templates in `inst/templates/` are built on these, and custom
user templates should call them (namespace-qualified, e.g.
`energyRt::report_tbl(...)`) instead of re-implementing formatting: they
render the same content correctly for the three supported outputs –
HTML, Word (`docx`), and LaTeX/PDF.

## Usage

``` r
report_output(default = c("html", "word", "latex"))

report_setup(default = c("html", "word", "latex"))

report_layout()

report_esc(x, output = report_output())

report_fmt_val(x, digits = 4L)

report_img(path, frac = 1, output = report_output(), caption = NULL)

report_img_row(paths, frac_total = 1, gap = 0.02, output = report_output())

report_pagebreak(output = report_output())

report_fig_height(p, base = 3.2, per_row = 1.5, max = 9)

report_plot_png(p, w_in, h_in = NULL, dpi = 150)

report_fig(
  p,
  w_in = 7,
  h_in = NULL,
  caption = NULL,
  frac = 1,
  dpi = 150,
  output = report_output()
)

report_sec(title, note = NULL, output = report_output())

report_tbl(
  df,
  caption = NULL,
  digits = 4L,
  output = report_output(),
  max_rows = NULL,
  scroll = TRUE
)

report_css(output = report_output())

report_header(
  name,
  desc = "",
  image_file = NULL,
  frac = report_layout()$hdr_img,
  output = report_output()
)
```

## Arguments

- default:

  character, the output assumed when no knitr/pandoc context is
  available (e.g. interactive use or tests).

- x:

  character or numeric value to escape / format.

- output:

  character, one of `"html"`, `"word"`, `"latex"`. Defaults to
  auto-detection via `report_output()`; pass explicitly in tests.

- digits:

  integer, significant digits for numeric formatting.

- path:

  character, path to an image file.

- frac:

  numeric, width as a fraction of the text/line width.

- paths:

  character vector of image paths (missing files dropped).

- frac_total:

  numeric, total line-width fraction a row of images spans.

- gap:

  numeric, fraction of the line width between row images.

- p:

  a ggplot object.

- base:

  numeric, figure height in inches for a single panel row.

- per_row:

  numeric, inches added for every further panel row.

- max:

  numeric, cap on the returned height in inches.

- w_in, h_in:

  numeric, plot size in inches.

- dpi:

  integer, raster resolution.

- title, note, caption:

  character, section / table headings.

- df:

  data.frame to render as a table.

- max_rows:

  integer or `NULL`: cap the printed rows with a "... K more rows"
  footer. `NULL` prints all rows in HTML and caps PDF/Word at a 200-row
  safety limit.

- scroll:

  logical (HTML only): render tables over 15 rows inside a scrollable
  box with a sticky header.

- name, desc:

  character, object name and description for the page header.

- image_file:

  character or `NULL`, optional header image (logo).

## Value

`report_output()` returns the output id invisibly-testable as a
character scalar; `report_layout()` returns a named list of sizing
constants; `report_plot_png()` returns the path to the rendered png; the
[`cat()`](https://rdrr.io/r/base/cat.html)-based emitters (`report_img`,
`report_sec`, `report_tbl`, `report_css`, `report_header`) return
`invisible(NULL)` and are meant for `results='asis'` chunks.

## Details

Word must be told apart from LaTeX explicitly:
[`knitr::is_latex_output()`](https://rdrr.io/pkg/knitr/man/output_type.html)
is `FALSE` for docx, but so is `is_html_output()`, and raw TeX emitted
in an "else" branch is silently discarded by pandoc's docx writer.
Emitters therefore branch on the three-value `output` returned by
`report_output()`.

## Functions

- `report_output()`: Detect the current render target: `"html"`,
  `"word"`, or `"latex"`. Outside a knitr/pandoc context the `default`
  is returned.

- `report_setup()`: Setup-chunk boilerplate: chunk options
  (`echo`/`warning`/`message` off) and the per-output `knitr.kable.NA`
  placeholder. Returns the detected output invisibly, so templates can
  open with `fmt <- energyRt::report_setup()`.

- `report_layout()`: The one sizing system: named fractions of the text
  width shared by all outputs, plus the raster `dpi`.

- `report_esc()`: Escape a string for the current output (TeX escaping
  for `"latex"`, HTML escaping otherwise – Word content goes through
  pandoc markdown, where HTML entities are safe).

- `report_fmt_val()`: Fixed-notation value formatting – never scientific
  (no `"1.2e+03"`); `"--"` for missing.

- `report_img()`: Emit an image at a fraction of the line width; the
  ONLY way images enter a report page. With a `caption` the image is a
  pandoc markdown figure (`![caption](path)` alone in a paragraph):
  `<figure>`/`<figcaption>` in HTML, a numbered `figure` environment in
  LaTeX, a captioned figure in Word.

- `report_img_row()`: Emit a ROW of images (logos, banners, badges)
  spanning `frac_total` of the line width, split evenly with `gap`
  between them. Missing files are dropped silently; a single image is
  capped at half of `frac_total` so a lone logo does not span the page.

- `report_pagebreak()`: Emit a page break: an always-break `<div>` for
  HTML (honoured when printing), `\newpage` for LaTeX, and a raw
  `{=openxml}` block for Word. Call from a `results='asis'` chunk.

- `report_fig_height()`: Figure height (inches) from a ggplot's facet
  layout: `base` for one panel row, `per_row` more for every further
  row, capped at `max`. Non-ggplot input returns `base`.

- `report_plot_png()`: Rasterise a ggplot at `dpi` and return the png
  path (uniform pipeline for all three formats; avoids knitr fig
  options, which docx partly rejects). `h_in = NULL` sizes by the facet
  layout via `report_fig_height()`.

- `report_fig()`: Emit a ggplot as a report figure: in-plot
  `title`/`subtitle` are dropped (the document names the figure), the
  height follows the facet layout unless `h_in` is given, and `caption`
  becomes a pandoc figure caption. `NULL` plots emit nothing.

- `report_sec()`: Section divider + bold title (+ gray note); Word gets
  a markdown heading.

- `report_tbl()`: One table pipeline: prune empty columns, round
  numerics, `kable` per format. kable does the escaping (single-escaped,
  never double). Large tables stay readable: in HTML a table with more
  than 15 rows renders inside a scrollable box with a sticky header
  (`scroll = FALSE` disables); `max_rows` caps the printed rows with a
  "... K more rows (of N)" footer – PDF/Word additionally enforce a
  200-row safety cap when `max_rows` is not given.

- `report_css()`: Emit the shared report stylesheet (HTML only; a no-op
  for Word and LaTeX).

- `report_header()`: Page header: name + description on the left,
  optional image (logo) on the right.

## Examples

``` r
report_esc("a & b", output = "html")
#> [1] "a &amp; b"
report_fmt_val(1234.5678)
#> [1] " 1235"
report_layout()$dpi
#> [1] 150
```
