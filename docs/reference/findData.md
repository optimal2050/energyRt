# Performs search for available data in *scenario* object.

Performs search for available data in *scenario* object.

## Usage

``` r
findData(
  object,
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

- object:

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

- add_weights:

  accepted and currently ignored.

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

## Examples

``` r
if (FALSE) { # \dontrun{
open_project(tempdir())

ek   <- topia$modules$electricity$R3
mod  <- newModel("TOPIA", data = ek$repo,
                 calendar = topia$modules$calendars$s4_h24,
                 region = ek$regions,
                 horizon = topia$modules$horizons$base,
                 discount = 0.05)
scen <- solve_model(mod, name = "BASE", solver = solver_options$glpk)

# which parameters carry a `tech` set, and what their dimensions are
findData(scen, setsNames_ = "tech")

# the solved variables only
findData(scen, dataType = "variables", setsNames_ = "tech")
} # }
```
