# Launch the process designer

An interactive editor for
[techspec](https://energyRt.org/reference/tech_from_spec.md) YAML
process specifications: ports and parameter blocks as forms, a live
[`draw()`](https://energyRt.org/reference/draw.md) schematic, a
validation report enforcing the techspec policy (nothing auto-invented;
partial vintage coverage is reported), and YAML / R-code export.
Currently edits `technology` processes; the other process classes
(storage, supply, demand, trade, import, export) are reserved. Requires
the suggested packages `shiny` and `DT`.

## Usage

``` r
process_designer(spec = NULL, dir = NULL, launch.browser = NULL, ...)

tech_designer(spec = NULL, dir = NULL, launch.browser = NULL, ...)
```

## Arguments

- spec:

  Optional techspec to open — a path to a `.yml` file or a spec list
  (see
  [`read_techspec()`](https://energyRt.org/reference/read_techspec.md)).

- dir:

  Optional directory of techspec `.yml` files (e.g. specs exported from
  a workbook import) added to the app's template gallery alongside the
  built-in examples.

- launch.browser:

  Where the app opens. `NULL` (default) uses the standard shiny
  behaviour for your session — in RStudio that is the Shiny app window /
  viewer. `TRUE` opens the system web browser instead (no RStudio window
  chrome, e.g. its "Publish" button). `FALSE` starts the server without
  opening anything (the URL is printed to the console).

- ...:

  Passed to
  [`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html) (e.g.
  `port`).

## Value

The value of
[`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html)
(invisibly; blocks while the app runs).

## Details

`tech_designer()` is the deprecated former name.

## Examples

``` r
if (FALSE) { # \dontrun{
process_designer()
process_designer(system.file("techspec", "examples", "coal_power.yml",
                             package = "energyRt"))
process_designer(launch.browser = TRUE)  # system browser, no IDE chrome
} # }
```
