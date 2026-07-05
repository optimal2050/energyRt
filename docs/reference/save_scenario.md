# Save scenario object on disk in parquet format using `arrow` package.

Save scenario object on disk in parquet format using `arrow` package.

## Usage

``` r
save_scenario(
  scen,
  path = scen@path,
  format = get_arrow_format(),
  overwrite = TRUE,
  clean_start = FALSE,
  write_log = TRUE,
  verbose = TRUE
)
```

## Arguments

- scen:

  scenario object.

- path:

  character. Path to scenario directory.

- format:

  file format (currently `parquet` only, arrow or feather will be
  implemented in further releases).

- overwrite:

  logical. Overwrite existing scenario directory.

- clean_start:

  logical. Clean scenario directory before saving.

- write_log:

  logical. Write (update) logfile.

- verbose:

  logical. Print messages.

## Value

scenario object with most of the slots saved on disk.

## Examples

``` r
if (FALSE) { # \dontrun{
scen_BASE@path # check the scenarion directory
scen_BASE <- save_scenario(scen_BASE) # saving in the default directory
} # }
```
