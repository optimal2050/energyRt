# Save scenario object on disk in parquet format using `arrow` package.

Save scenario object on disk in parquet format using `arrow` package.

## Usage

``` r
save_scenario(
  scen,
  path = scen@path,
  embed_model = NULL,
  embed_datasets = NULL,
  format = get_storage_format(),
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

- embed_model:

  `NULL` (default), `TRUE`, or `FALSE`. Controls whether the model is
  embedded in the saved scenario or referenced from the model store (see
  [`save_model()`](https://energyRt.org/reference/model_store.md)).
  `NULL`: reference when the identical model content is already in the
  store, embed otherwise. `TRUE`: always embed (self-contained folder).
  `FALSE`: require a store hit, error otherwise. A referenced save
  stores only `{name, hash, path}`;
  [`load_scenario()`](https://energyRt.org/reference/load_scenario.md)
  resolves it back via the registry / model store.

- embed_datasets:

  the same choice for the big data tables and maps — the geoscale on
  `@settings` and, when the model is embedded, its geoscale and its
  repositories' weather/demand tables (see
  [`save_dataset()`](https://energyRt.org/reference/dataset_store.md)).
  `NULL` (default) references whatever identical content is already in
  the dataset store; `TRUE` always embeds; `FALSE` requires a store hit.

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
