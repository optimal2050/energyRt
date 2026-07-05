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
getData(scen, ...)

# S3 method for class 'scenario'
getData(
  scen,
  name = NULL,
  ...,
  merge = FALSE,
  timeframe = c("lowest", "highest", "all"),
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
  scenNameInList = as.logical(length(scen) - 1),
  unfold = TRUE,
  verbose = FALSE
)

# S3 method for class 'list'
getData(
  scen,
  name = NULL,
  ...,
  merge = FALSE,
  timeframe = c("lowest", "highest", "all"),
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
  scenNameInList = as.logical(length(scen) - 1),
  unfold = TRUE,
  verbose = FALSE
)

# Default S3 method
getData(scen, ...)

get_data(scen, ...)

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

- scen:

  Object scenario or list of scenarios.

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

  controls sub-annual time aggregation of results that carry a `slice`
  column. One of `"lowest"` (default, aggregate/sum flows up to the
  coarsest level, normally `ANNUAL`), `"highest"` (native/finest, as
  stored), `"all"` (return every timeframe level stacked), or an
  explicit calendar level name (e.g. `"SEASON"`, `"YDAY"`) to aggregate
  to that level. Non-slice data, and state/level variables (e.g.
  `vStorageStore`) for which summing over slices is meaningless, are
  returned unchanged.

- process:

  if TRUE, dimensions "tech", "stg", "trade", "imp", "expp", "dem", and
  "sup" will be renamed with "process".

- parameters:

  if TRUE, parameters will be included in the search and returned if
  found.

- variables:

  if TRUE, variables will be included in the search and returned if
  found.

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

- asTibble:

  logical, if the data.frames should be converted into tibbles.

- stringsAsFactors:

  logical, should the sets values be converted to factors?

- yearsAsFactors:

  logical, should `year` be converted to factors? Set 'year' is integer
  by default.

- scenNameInList:

  logical, should the name of the scenarios be used if not provided in
  the list with several scenarios?

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
data("utopia_scen_BAU.RData")
getData(scen, name = "pDemand", year = 2015, merge = TRUE)
getData(scen, name = "vTechOut", comm = "ELC", merge = TRUE, year = 2015)
elc2050 <- getData(scen, parameters = FALSE, comm = "ELC", year = 2050)
names(elc2050)
elc2050$vBalance
} # }
```
