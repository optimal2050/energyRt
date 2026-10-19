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
  format = "html",
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
  format = "html",
  levcost = NULL,
  cost_unit = NULL,
  levcost_peers = NULL,
  open = interactive(),
  geoscale = NULL,
  model = NULL,
  scenario = NULL,
  reports_path = NULL,
  force = FALSE,
  ...
)

# S4 method for class 'storage'
report(
  object,
  template = NULL,
  image_file = NULL,
  file = NULL,
  format = "html",
  levcost = NULL,
  cost_unit = NULL,
  open = interactive(),
  geoscale = NULL,
  model = NULL,
  scenario = NULL,
  reports_path = NULL,
  force = FALSE,
  ...
)

# S4 method for class 'repository'
report(
  object,
  template = NULL,
  image_file = NULL,
  file = NULL,
  format = "html",
  levcost = NULL,
  cost_unit = NULL,
  open = interactive(),
  reports_path = NULL,
  force = FALSE,
  logos = NULL,
  figure = NULL,
  groups = NULL,
  ...
)

# S4 method for class 'model'
report(
  object,
  template = NULL,
  image_file = NULL,
  file = NULL,
  format = "html",
  levcost = NULL,
  cost_unit = NULL,
  open = interactive(),
  reports_path = NULL,
  force = FALSE,
  logos = NULL,
  figure = NULL,
  groups = NULL,
  interpolate = FALSE,
  scenario = NULL,
  ...
)

# S4 method for class 'scenario'
report(
  object,
  template = NULL,
  image_file = NULL,
  file = NULL,
  format = "html",
  levcost = NULL,
  cost_unit = NULL,
  open = interactive(),
  reports_path = NULL,
  force = FALSE,
  run = NULL,
  verify = TRUE,
  logos = NULL,
  badges = NULL,
  groups = NULL,
  ...
)

# S4 method for class 'list'
report(
  object,
  template = NULL,
  image_file = NULL,
  file = NULL,
  format = "html",
  levcost = NULL,
  cost_unit = NULL,
  open = interactive(),
  reports_path = NULL,
  force = FALSE,
  ...
)

# S4 method for class 'scenarios_cmp'
report(
  object,
  template = NULL,
  image_file = NULL,
  file = NULL,
  format = "html",
  levcost = NULL,
  cost_unit = NULL,
  open = interactive(),
  reports_path = NULL,
  force = FALSE,
  ...
)

# S4 method for class 'commodity'
report(
  object,
  template = NULL,
  image_file = NULL,
  file = NULL,
  format = "html",
  levcost = NULL,
  cost_unit = NULL,
  open = interactive(),
  reports_path = NULL,
  force = FALSE,
  ...
)

# S4 method for class 'supply'
report(
  object,
  template = NULL,
  image_file = NULL,
  file = NULL,
  format = "html",
  levcost = NULL,
  cost_unit = NULL,
  open = interactive(),
  reports_path = NULL,
  force = FALSE,
  ...
)

# S4 method for class 'demand'
report(
  object,
  template = NULL,
  image_file = NULL,
  file = NULL,
  format = "html",
  levcost = NULL,
  cost_unit = NULL,
  open = interactive(),
  reports_path = NULL,
  force = FALSE,
  ...
)

# S4 method for class 'trade'
report(
  object,
  template = NULL,
  image_file = NULL,
  file = NULL,
  format = "html",
  levcost = NULL,
  cost_unit = NULL,
  open = interactive(),
  reports_path = NULL,
  force = FALSE,
  ...
)

# S4 method for class 'import'
report(
  object,
  template = NULL,
  image_file = NULL,
  file = NULL,
  format = "html",
  levcost = NULL,
  cost_unit = NULL,
  open = interactive(),
  reports_path = NULL,
  force = FALSE,
  ...
)

# S4 method for class 'export'
report(
  object,
  template = NULL,
  image_file = NULL,
  file = NULL,
  format = "html",
  levcost = NULL,
  cost_unit = NULL,
  open = interactive(),
  reports_path = NULL,
  force = FALSE,
  ...
)

# S4 method for class 'weather'
report(
  object,
  template = NULL,
  image_file = NULL,
  file = NULL,
  format = "html",
  levcost = NULL,
  cost_unit = NULL,
  open = interactive(),
  reports_path = NULL,
  force = FALSE,
  ...
)

# S4 method for class 'tax'
report(
  object,
  template = NULL,
  image_file = NULL,
  file = NULL,
  format = "html",
  levcost = NULL,
  cost_unit = NULL,
  open = interactive(),
  reports_path = NULL,
  force = FALSE,
  ...
)

# S4 method for class 'subsidy'
report(
  object,
  template = NULL,
  image_file = NULL,
  file = NULL,
  format = "html",
  levcost = NULL,
  cost_unit = NULL,
  open = interactive(),
  reports_path = NULL,
  force = FALSE,
  ...
)

# S4 method for class 'constraint'
report(
  object,
  template = NULL,
  image_file = NULL,
  file = NULL,
  format = "html",
  levcost = NULL,
  cost_unit = NULL,
  open = interactive(),
  reports_path = NULL,
  force = FALSE,
  ...
)

# S4 method for class 'calendar'
report(
  object,
  template = NULL,
  image_file = NULL,
  file = NULL,
  format = "html",
  levcost = NULL,
  cost_unit = NULL,
  open = interactive(),
  reports_path = NULL,
  force = FALSE,
  ...
)

# S4 method for class 'horizon'
report(
  object,
  template = NULL,
  image_file = NULL,
  file = NULL,
  format = "html",
  levcost = NULL,
  cost_unit = NULL,
  open = interactive(),
  reports_path = NULL,
  force = FALSE,
  ...
)

# S4 method for class 'config'
report(
  object,
  template = NULL,
  image_file = NULL,
  file = NULL,
  format = "html",
  levcost = NULL,
  cost_unit = NULL,
  open = interactive(),
  reports_path = NULL,
  force = FALSE,
  ...
)

# S4 method for class 'costs'
report(
  object,
  template = NULL,
  image_file = NULL,
  file = NULL,
  format = "html",
  levcost = NULL,
  cost_unit = NULL,
  open = interactive(),
  reports_path = NULL,
  force = FALSE,
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
  upper-right corner of the page. Defaults to the object's own
  `misc$image` when that names an existing local file (see
  [`object_image`](https://energyRt.org/reference/object_image.md));
  `NULL` with no `misc$image` skips the image.

- file:

  Character. Destination file path (used verbatim). Without it the
  report lands inside the owning folder's `reports/` when the object is
  saved (a scenario folder, a model/repository store entry), and
  otherwise in the temporary project-level
  [`get_reports_path()`](https://energyRt.org/reference/reports_path.md)
  folder.

- format:

  Character. Output format: `"html"` (default), `"pdf"`, `"tex"`
  (standalone LaTeX source), or `"docx"` (Word; the templates' LaTeX
  styling degrades to Word's own styles). Multiple values are accepted;
  one file is produced per format. `"pdf"`/`"tex"` require a LaTeX
  installation (e.g.
  [`tinytex::install_tinytex()`](https://rdrr.io/pkg/tinytex/man/install_tinytex.html))
  and are skipped with a warning when none is found.

- levcost:

  A `levcost` (or `levcost_list`) object returned by
  [`levcost`](https://energyRt.org/reference/levcost.md), or `NULL`
  (default). When `NULL` and any `levcost` keyword arguments are passed
  via `...` (e.g. `group`, `repo`, `discount`),
  [`levcost()`](https://energyRt.org/reference/levcost.md) is called
  automatically on `object` with those arguments. On a whole-model or
  scenario report, `levcost = TRUE` adds the levelized-cost comparison
  section (overall and per-topology-group charts + table).

  For a process declaring vintages or clusters the report carries the
  per-variant levelised costs (a table plus a comparison chart)
  alongside the detail figures for one display instance. Pass
  `by_variant = FALSE` in `...` to report that single instance only.
  `by_variant` is consumed by `report()` and deliberately not forwarded
  to [`levcost()`](https://energyRt.org/reference/levcost.md), whose
  `by_variant = TRUE` returns an extracted data frame rather than the
  object the report is built from.

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
  everything else is passed to the renderer. For a whole-scenario
  report, `levcost = TRUE` adds an ex-post levelized-cost table via
  `levcost(scenario)`.

- levcost_peers:

  A `levcost_list` of the process's topology-group peers, rendered as a
  comparison chart. Filled automatically when the datasheet is requested
  from a container (`report(mod, name = )`); rarely passed by hand.

- geoscale:

  A
  [`geoscales::GeoScale`](https://optimal2050.github.io/geoscales/r/reference/GeoScale.html)
  (`technology` / `storage` methods), giving the report geometry so it
  can map WHERE the process is declared. Nothing about the costs
  changes.

- model:

  The `model` the process sits in (`technology` / `storage` methods).
  The levelized cost is then computed in that model's world – its
  calendar, horizon, discount and regions – and the model's geoscale
  supplies the map.

- scenario:

  A solved `scenario` (`technology` / `storage` methods): the report
  adds what the solve REALIZED – the capacity built per region, and the
  ex-post levelized cost beside the ex-ante one, per variant. `scenario`
  implies its model, which implies its geoscale; a `model` that is not
  the scenario's is refused. For the `model` method, an interpolated
  scenario (solved or not) of the model: its diagnostics – the processes
  that cannot operate
  ([`process_operability()`](https://energyRt.org/reference/process_operability.md))
  – are added to the report.

- reports_path:

  Character. Redirects the default output directory for this call
  (`file =` still wins).

- force:

  Logical. Re-render even when the existing files are up to date.
  Reports are skip-if-current: a sidecar (`*.report.yml`) records a
  content key over the object, template, arguments, image and levcost
  identity, and an unchanged report is not rebuilt.

- logos:

  Character vector of image paths rendered as a banner row (`model` /
  `repository` / `scenario` methods). Given explicitly it REPLACES the
  `misc`-derived set entirely and every file must exist; `NULL`
  (default) takes `misc$logos`, then the scalar `misc$logo` /
  `misc$image` – a scenario shows its model's logos first, then its own.

- figure:

  An optional half-page hero figure (`model` / `repository` methods);
  `NULL` (default) takes `misc$figure`.

- groups:

  Process groups for model and scenario reports: a named list whose
  elements are character vectors of process names or regular expressions
  over process names
  (`list(Coal = "_coal_", Wind = c("WIN_ON", "^R._win"))`). Defaults to
  `object@misc$report_groups` (a scenario falls back to its model's).
  `FALSE` disables groups, `"topology"` groups automatically by
  structure. Without groups the report shows a representative sample of
  up to 12 processes. Processes in no group form a final "Ungrouped"
  sample.

- interpolate:

  `model` method: `FALSE` (default) adds no diagnostics; `TRUE`
  interpolates the model in memory (nothing is saved) and adds the
  processes that cannot operate and the interpolation's warnings; a list
  passes further arguments to
  [`interpolate_model()`](https://energyRt.org/reference/interpolate_model.md),
  e.g. `list(calendars$d365_h24_1dpm)`. An interpolation that stops is
  reported in the section instead of failing the report.

- run:

  Character or `NULL` (`scenario` method). The run id to report, as
  listed by
  [`scenario_runs`](https://energyRt.org/reference/scenario_runs.md)
  (`"<solve>"` or `"<variant>/<solve>"`). The default `NULL` reports the
  active run; a different run is read into a local copy, so the caller's
  scenario keeps its active run.

- verify:

  Logical (`scenario` method), default `TRUE`: include the
  [`verify_solution`](https://energyRt.org/reference/verify_solution.md)
  checks section.

- badges:

  Small property-indicator images (`scenario` method); `NULL` (default)
  takes `misc$badges`.

  Branding content enters the render key, so a changed logo or figure
  re-renders an otherwise up-to-date report.

## Value

The path(s) to the generated output file(s) (invisibly). A single string
when one format is requested; a character vector when multiple formats
are requested.

## Functions

- `report(commodity)`: Element datasheets. Every element class –
  commodity, supply, demand, trade, import, export, weather, tax,
  subsidy, constraint, calendar, horizon, config, costs – has a
  `report()` method rendering a one-object datasheet from its shipped
  `report_<class>.Rmd` template. The default template is the class name;
  an object-level default can be stored in `misc$report` (the techspec
  `report:` key), and an explicit `template =` always wins. From a
  container, `report(mod, name = "X")` finds the element of any class
  (pass `class = ` to disambiguate same-named objects); the levelized
  cost section applies to technology, storage and trade only.

## See also

[`levcost`](https://energyRt.org/reference/levcost.md),
[`report_pdf`](https://energyRt.org/reference/report_pdf.md),
[`report_html`](https://energyRt.org/reference/report_html.md),
[`report_tex`](https://energyRt.org/reference/report_tex.md)

## Examples

``` r
if (FALSE) { # \dontrun{
open_project(tempdir())

ek  <- topia$modules$electricity$R3
tech <- ek$ECOA

# an object on its own: its assumptions, and an ex-ante estimate of what it
# could do
report(tech)

# the same object as the model sees it -- prices and calendar from the
# model it belongs to
mod <- newModel("TOPIA", data = ek$repo,
                calendar = topia$modules$calendars$s4_h24,
                region = ek$regions, horizon = topia$modules$horizons$base)
mod <- setGeoscale(mod, ek$geoscale)
report(tech, model = mod)

# and as a solved scenario realized it, against that ex-ante estimate
scen <- solve_model(mod, name = "BASE", solver = solver_options$glpk)
report(tech, scenario = scen)

# with a map of where it is sited
report(tech, model = mod, geoscale = topia$geoscales$squares)

# a container reports what the container is about: a model its
# assumptions, a scenario its results
report(mod)
report(scen)

# other formats, and several at once
report(tech, format = "pdf")
report(tech, format = c("html", "docx"))
} # }
```
