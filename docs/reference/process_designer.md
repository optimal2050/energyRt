# Launch the process designer

An interactive editor for [process
spec](https://energyRt.org/reference/process_from_spec.md) YAML
specifications: ports and parameter blocks as forms, a live
[`draw()`](https://energyRt.org/reference/draw.md) schematic, a
validation report enforcing the spec policy (nothing auto-invented;
partial vintage coverage is reported), and YAML / R-code export.

## Usage

``` r
process_designer(spec = NULL, dir = NULL, launch.browser = NULL, ...)
```

## Arguments

- spec:

  Optional process spec to open — a path to a `.yml` file or a spec list
  (see
  [`read_process_spec()`](https://energyRt.org/reference/read_process_spec.md)).

- dir:

  Optional directory of process-spec `.yml` files (e.g. specs exported
  from a workbook import) added to the app's template gallery alongside
  the built-in examples.

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

The underlying spec layer reads, validates, draws and exports
`technology`, `storage` and `trade` specs alike (see
[`process_from_spec()`](https://energyRt.org/reference/process_from_spec.md)).
The editing **forms** are technology-shaped for now: opening a storage
or trade spec works, and every tab except the parameter forms is fully
functional, but its blocks are edited as YAML. The remaining process
classes (supply, demand, import, export) are reserved. Requires the
suggested packages `shiny` and `DT`.

[`tech_designer()`](https://energyRt.org/reference/energyRt-deprecated.md)
is the deprecated former name.

## Examples

``` r
if (FALSE) { # \dontrun{
process_designer()
process_designer(system.file("techspec", "examples", "battery.yml",
                             package = "energyRt"))
process_designer(launch.browser = TRUE)  # system browser, no IDE chrome
} # }
```
