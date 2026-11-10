# Extract data from energyRt objects

Generic accessor. Dispatches on the object class:

- `scenario` (or a list of scenarios): interpolated/solved data from
  `modInp`/`modOut` – see the `scenario` method below.

- model objects (`technology`, `commodity`, `storage`, `supply`,
  `demand`, `trade`, ...): the object's own *raw input* slot data
  (pre-interpolation).

- `model` / `repository`: raw input slots stacked across all contained
  objects.

Model-object methods (`technology`, `commodity`, `storage`, `supply`,
`demand`, `trade`) return the object's raw input slot data.
`model`/`repository` stack that across all contained objects. Use `name`
to select slot(s) (e.g. `"invcost"`), `...` to filter (`region=`,
`year=`, `comm=`, or `col_=`regex), `merge=TRUE` for one long tidy frame
(`param`/`value`), and `interpolate=TRUE` to expand year-bearing slots
over `years` (or the observed year range) with each parameter's default
interpolation rule.

## Usage

``` r
getData(object, ...)

# S3 method for class 'scenario'
getData(
  object,
  name = NULL,
  ...,
  merge = FALSE,
  timeframe = c("highest", "lowest", "all"),
  geoframe = c("finest", "coarsest", "all"),
  process = FALSE,
  parameters = TRUE,
  variables = TRUE,
  sets = FALSE,
  maps = FALSE,
  ignore.case = TRUE,
  newNames = NULL,
  newValues = NULL,
  na.rm = FALSE,
  digits = NULL,
  drop.zeros = FALSE,
  add_weights = "auto",
  add_period_length = "auto",
  apply_weights = FALSE,
  apply_period_length = FALSE,
  asTibble = TRUE,
  as_data_table = FALSE,
  stringsAsFactors = FALSE,
  yearsAsFactors = FALSE,
  drop_duplicated_scenarios = TRUE,
  scenNameInList = as.logical(length(object) - 1),
  unfold = TRUE,
  variants = TRUE,
  run = NULL,
  base_year = FALSE,
  verbose = FALSE
)

# S3 method for class 'list'
getData(
  object,
  name = NULL,
  ...,
  merge = FALSE,
  timeframe = c("highest", "lowest", "all"),
  geoframe = c("finest", "coarsest", "all"),
  process = FALSE,
  parameters = TRUE,
  variables = TRUE,
  sets = FALSE,
  maps = FALSE,
  ignore.case = TRUE,
  newNames = NULL,
  newValues = NULL,
  na.rm = FALSE,
  digits = NULL,
  drop.zeros = FALSE,
  add_weights = "auto",
  add_period_length = "auto",
  apply_weights = FALSE,
  apply_period_length = FALSE,
  asTibble = TRUE,
  as_data_table = FALSE,
  stringsAsFactors = FALSE,
  yearsAsFactors = FALSE,
  drop_duplicated_scenarios = TRUE,
  scenNameInList = as.logical(length(object) - 1),
  unfold = TRUE,
  variants = TRUE,
  run = NULL,
  base_year = FALSE,
  verbose = FALSE
)

# S3 method for class 'character'
getData(object, ...)

# S3 method for class 'environment'
getData(object, ...)

# Default S3 method
getData(object, ...)

# S3 method for class 'technology'
getData(
  obj,
  name = NULL,
  ...,
  merge = FALSE,
  interpolate = FALSE,
  years = NULL,
  process = FALSE,
  asTibble = TRUE,
  ignore.case = TRUE,
  verbose = FALSE
)

# S3 method for class 'commodity'
getData(
  obj,
  name = NULL,
  ...,
  merge = FALSE,
  interpolate = FALSE,
  years = NULL,
  process = FALSE,
  asTibble = TRUE,
  ignore.case = TRUE,
  verbose = FALSE
)

# S3 method for class 'storage'
getData(
  obj,
  name = NULL,
  ...,
  merge = FALSE,
  interpolate = FALSE,
  years = NULL,
  process = FALSE,
  asTibble = TRUE,
  ignore.case = TRUE,
  verbose = FALSE
)

# S3 method for class 'supply'
getData(
  obj,
  name = NULL,
  ...,
  merge = FALSE,
  interpolate = FALSE,
  years = NULL,
  process = FALSE,
  asTibble = TRUE,
  ignore.case = TRUE,
  verbose = FALSE
)

# S3 method for class 'demand'
getData(
  obj,
  name = NULL,
  ...,
  merge = FALSE,
  interpolate = FALSE,
  years = NULL,
  process = FALSE,
  asTibble = TRUE,
  ignore.case = TRUE,
  verbose = FALSE
)

# S3 method for class 'trade'
getData(
  obj,
  name = NULL,
  ...,
  merge = FALSE,
  interpolate = FALSE,
  years = NULL,
  process = FALSE,
  asTibble = TRUE,
  ignore.case = TRUE,
  verbose = FALSE
)

# S3 method for class 'import'
getData(
  obj,
  name = NULL,
  ...,
  merge = FALSE,
  interpolate = FALSE,
  years = NULL,
  process = FALSE,
  asTibble = TRUE,
  ignore.case = TRUE,
  verbose = FALSE
)

# S3 method for class 'export'
getData(
  obj,
  name = NULL,
  ...,
  merge = FALSE,
  interpolate = FALSE,
  years = NULL,
  process = FALSE,
  asTibble = TRUE,
  ignore.case = TRUE,
  verbose = FALSE
)

# S3 method for class 'weather'
getData(
  obj,
  name = NULL,
  ...,
  merge = FALSE,
  interpolate = FALSE,
  years = NULL,
  process = FALSE,
  asTibble = TRUE,
  ignore.case = TRUE,
  verbose = FALSE
)

# S3 method for class 'model'
getData(
  obj,
  name = NULL,
  ...,
  merge = FALSE,
  interpolate = FALSE,
  years = NULL,
  process = FALSE,
  asTibble = TRUE,
  ignore.case = TRUE,
  verbose = FALSE
)

# S3 method for class 'repository'
getData(
  obj,
  name = NULL,
  ...,
  merge = FALSE,
  interpolate = FALSE,
  years = NULL,
  process = FALSE,
  asTibble = TRUE,
  ignore.case = TRUE,
  verbose = FALSE
)
```

## Arguments

- object:

  the object whose data is returned, for the per-class methods
  (`technology`, `commodity`, `storage`, ...).

- ...:

  filters for various sets (setname = c(val1, val2) or setname\_ =
  "matching pattern"), see details.

- name:

  character vector with names of parameters and/or variables.

- merge:

  if TRUE, the search results will be merged in one dataframe; the named
  list will be returned if FALSE. When TRUE, a data.frame (empty if
  nothing matched) is always returned, never NULL.

- timeframe:

  controls sub-annual time aggregation of results that carry a
  `timeslice` column. One of `"highest"` (default, native/finest
  resolution, as stored), `"lowest"` (aggregate/sum flows up to the
  coarsest level, normally `ANNUAL`), `"all"` (return every timeframe
  level stacked), or an explicit calendar level name (e.g. `"SEASON"`,
  `"YDAY"`) to aggregate to that level. Non-timeslice data is returned
  unchanged. How a variable combines is declared per variable (`recast`
  in `data-raw/variables.yml`): a quantity is summed at the child/target
  weight ratio, while a state/level variable (e.g. `vStorageLevel`)
  takes its CLOSING value, with the swing inside each target slice
  recorded in `attr(x, "absorbed")`.

  The default was `"lowest"` up to v0.80 and is now `"highest"`,
  matching the spatial twin `geoframe`, whose default `"finest"` has
  always meant "as stored". Returning data at native resolution unless
  asked otherwise is the safer default: the old one silently summed an
  8760-slice hourly series into a single annual number, which reads as a
  plausible value rather than as an error, and is indistinguishable from
  a genuine annual result. Aggregation is easy to ask for and hard to
  notice when it is not wanted. Code that relied on annual totals must
  now pass `timeframe = "lowest"` explicitly.

- geoframe:

  controls spatial aggregation of results that carry a `region` column,
  the spatial twin of `timeframe`. One of `"finest"` (default, native
  resolution as stored), `"coarsest"` (aggregate up to the top geoscale
  level), `"all"` (return every level stacked), or an explicit geoscale
  level name (e.g. `"zone"`, `"nation"`). Requires a geoscale on the
  model; without one this is inert. How a variable combines is declared
  per variable (`recast` in `data-raw/variables.yml`): a quantity is
  summed, while an inter-regional flow (e.g. `vTradeIr`) is summed only
  where it CROSSES the new border. Rows whose endpoints land in the same
  target region are no longer a transfer, so they leave the total and
  are recorded as in-region flow in `attr(x, "absorbed")` rather than
  double-counted.

- process:

  `TRUE` renames the process-class id dimension ("tech", "stg", "trade",
  "sup", "dem", "imp", "impp", "expp") to "process", so results from
  different classes stack into one frame. A character value instead
  SELECTS the named process(es) and renames – `process = "COAL"`. Use
  `process_` for a regex, e.g. `process_ = "^E"`. A character value is
  always a selection, so "T"/"TRUE" are read as process names, not
  logicals.

- parameters:

  if TRUE, parameters will be included in the search and returned if
  found.

- variables:

  if TRUE, variables will be included in the search and returned if
  found.

- sets:

  logical, include set-type parameters in the result (by default only
  value-bearing parameters are returned).

- maps:

  if TRUE, map-type parameters (membership mappings, no `value` column)
  are also returned.

- ignore.case:

  grepl parameter if regular expressions are used in '...' or 'name\_'.

- newNames:

  renaming sets, named character vector or list with new names as
  values, and old names as names - the input parameter to renameSets
  function. The operation is performed before merging the data (merge
  parameter).

- newValues:

  revalue sets, named character vector or list with new values as
  values, and old values as names - the input parameter to revalueSets
  function. The operation is performed after merging the data (merge
  parameter).

- na.rm:

  if TRUE, NA values will be dropped.

- digits:

  if integer, indicates the number of decimal places for rounding, if
  NULL - no actions.

- drop.zeros:

  logical, should rows containing zero values be filtered out.

- add_weights, add_period_length, apply_weights, apply_period_length,
  as_data_table:

  accepted and currently ignored.

- asTibble:

  logical, if the data.frames should be converted into tibbles.

- stringsAsFactors:

  logical, should the sets values be converted to factors?

- yearsAsFactors:

  logical, should `year` be converted to factors? Set 'year' is integer
  by default.

- drop_duplicated_scenarios:

  logical, when several scenarios carry the same name, keep only the
  first.

- scenNameInList:

  logical, should the name of the scenarios be used if not provided in
  the list with several scenarios?

- unfold:

  logical, expand a stored parameter back over its full domain before
  returning it.

- variants:

  logical, default `TRUE`: attach the technology-variant provenance
  columns `base`, `vintage` and `cluster` (from
  `scenario@modInp@sets$variant`) to the returned data, so results of a
  vintaged / clustered technology can be grouped or rolled up by those
  dimensions without parsing the variant names. These are TECHNOLOGY
  variants — unrelated to the own-problem run variants under `runs/`,
  which are selected by switching the scenario with
  [`read_solution()`](https://energyRt.org/reference/read.md). Has no
  effect when the model has no variants, so the returned shape is
  unchanged for such models. Set `FALSE` to suppress the columns.

- run:

  character, one or more runs to read (`"<solve>"` or
  `"<variant>/<solve>"`), or `"all"` for every run of each scenario. The
  returned rows gain a `run` column naming their source. `NULL`
  (default) reads each object's ACTIVE run and adds no column, so
  existing calls are unchanged. Each run is read on a COPY of the
  scenario, so the object you passed keeps its active run; a variant run
  brings its own problem with it, parameters included. Spanning runs of
  different problems warns — their results are not like-for-like. Not to
  be confused with `variants`, below, which is about technologies.

- base_year:

  logical, default `FALSE`. When `TRUE`, a scenario whose horizon marks
  a base year also returns that year, read from the separate run the
  scenario recorded (see
  [`solve_base_year()`](https://energyRt.org/reference/solve_base_year.md)),
  and every row gains a `source` column — `"solved"` for the scenario's
  own LP, `"base"` for the base year. `vObjective` is not unioned: the
  base year and the projection are separate optimisations, so only the
  projection's is returned. A scenario that records no base year, or
  whose recorded source cannot be read, warns and returns its own rows
  alone. Note that post-processing applies the FIRST object's horizon
  and calendar labels to every row.

- verbose:

  logical, print progress and diagnostic messages.

- interpolate:

  if TRUE, expand year-bearing object slots over the target years using
  each parameter's default interpolation rule (for quick demonstration
  plots). Default FALSE (raw data as stored). Only meaningful for
  object/model/repository methods.

- years:

  integer vector of target milestone years for `interpolate` (default:
  the yearly range observed in the data).

## See also

the per-class methods for the full argument list.

## Examples

``` r
if (FALSE) { # \dontrun{
data("topia_scen_BAU.RData")
getData(scen, name = "pDemand", year = 2015, merge = TRUE)
getData(scen, name = "vTechOut", comm = "ELC", merge = TRUE, year = 2015)
elc2050 <- getData(scen, parameters = FALSE, comm = "ELC", year = 2050)
names(elc2050)
elc2050$vBalance
} # }
```
