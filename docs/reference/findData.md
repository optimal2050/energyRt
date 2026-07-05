# Performs search for available data in *scenario* object.

Performs search for available data in *scenario* object.

## Usage

``` r
findData(
  scen,
  dataType = c("parameters", "variables"),
  setsNames_ = NULL,
  valueColumn = TRUE,
  allSets = TRUE,
  ignore.case = FALSE,
  add_weights = "auto",
  dropEmpty = TRUE,
  dfDim = TRUE,
  dfNames = TRUE,
  asMatrix = FALSE
)
```

## Arguments

- scen:

  object *scenario* with model solution.

- dataType:

  type of data to lok for (currently only "parameters" and "variables").

- setsNames\_:

  regular expression pattern for names of sets which will be included in
  search.

- valueColumn:

  logical, if TRUE will return variables and parameters with 'value'
  column (to filter sets and mappings).

- allSets:

  logical, if TRUE *and* operator should be used in search the sets,
  *or* will be used if FALSE.

- ignore.case:

  grepl parameter for matching names.

- dropEmpty:

  logical, if TRUE drops parameters and variables with zero length.

- dfDim:

  logical, if TRUE returns dimension *dim*.

- dfNames:

  logical, when TRUE returns names of the data frame column.

- asMatrix:

  return results as a matrix (not implemented).

## Value

list with variables and parameters name, each includes *dim* and *names*
character vectors.
