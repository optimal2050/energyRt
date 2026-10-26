# Import energyRt scenario data into a multimod model

Combines set population, parameter linking, and special handling for
bounds-style parameters (stored as single objects in energyRt but split
into \*Lo/\*Up entries inside multimod models). Creates a detailed
import log stored in model\$misc\$data_import_log.

## Usage

``` r
multimod_import_data(
  model,
  scenario,
  inMemory = scenario@inMemory,
  log_file = NULL
)
```

## Arguments

- model:

  Multimod model

- scenario:

  energyRt scenario object

- inMemory:

  Logical. Load parameter data into memory?

- log_file:

  Optional path to export import log as CSV

## Value

Modified model with scenario data linked and import log

## Examples

``` r
if (FALSE) { # \dontrun{
ek   <- topia$modules$electricity$R3
mod  <- newModel("TOPIA", data = ek$repo,
                 calendar = topia$modules$calendars$s4_h24,
                 region = ek$regions, horizon = topia$modules$horizons$base)
mod <- setGeoscale(mod, ek$geoscale)
scen <- interpolate_model(mod, name = "BASE")

# a model read from its algebra, with empty sets and parameters
m <- multimod::read_gams("my_model.gms")

# fill them from the scenario
m <- multimod_import_data(m, scen)

# what matched, and what did not
multimod_import_log(m)
multimod_unmatched(m)
} # }
```
