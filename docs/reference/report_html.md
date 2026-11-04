# Generate an HTML report for an energyRt object

Thin wrapper around [`report`](https://energyRt.org/reference/report.md)
that fixes `format = "html"`. Produces a portable single-file `.html`
document with embedded assets.

## Usage

``` r
report_html(object, ...)

# S4 method for class 'ANY'
report_html(object, ...)
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
  everything else is passed to the renderer. For a whole-scenario
  report, `levcost = TRUE` adds an ex-post levelized-cost table via
  `levcost(scenario)`.

## Value

Path to the generated `.html` file (invisibly).

## Examples

``` r
if (FALSE) { # \dontrun{
open_project(tempdir())
tech <- getObject(topia$modules$electricity$repo, name = "ECOA", drop = TRUE)

# a portable single-file page under the project's reports/
report_html(tech)

# a scenario's report covers results, where an object's covers assumptions
topia_elc <- topia$modules$electricity
mod  <- newModel("TOPIA", data = topia_elc$repo,
                 calendar = topia$modules$calendars$s4_h24,
                 region = topia_elc$regions,
                 horizon = topia$modules$horizons$BY2025_50_by10)
mod <- setGeoscale(mod, topia_elc$geoscale)
scen <- solve_model(mod, name = "BASE", solver = solver_options$glpk)
report_html(scen)
} # }
```
