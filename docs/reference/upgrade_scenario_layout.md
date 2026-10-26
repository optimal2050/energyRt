# Upgrade a scenario folder to the current on-disk layout

Migrates a scenario directory in place to layout 3 with the current run
structure. Idempotent — running it on an up-to-date folder changes
nothing. It handles, in one pass:

- layout 2: each `script/<solver>/` directory (recognized by its
  `solver` csv) becomes `runs/<solver>/solver/` with a backfilled
  `run.yml` (`status: "legacy"`, solver identity from the csv,
  timestamps from file modification times); the top-level `modOut/`
  store moves under the active run. Transient timestamp directories are
  left where they are.

- interim layout 3 (early builds): the `runs/default/<solve>/` wrapper
  is flattened to `runs/<solve>/`, and each run's `script/` working dir
  is renamed to `solver/`.

The scenario object is re-saved with rebased paths, `scenario.yml` (with
its `default:` run) and the `layout` marker are written, and the project
registry is updated.

## Usage

``` r
upgrade_scenario_layout(path, verbose = TRUE)
```

## Arguments

- path:

  character, the scenario directory.

- verbose:

  logical.

## Value

the upgraded scenario object, invisibly.

## Examples

``` r
if (FALSE) { # \dontrun{
# migrate a scenario folder written by an older energyRt to the current
# runs/<variant>/<solve>/ layout
upgrade_scenario_layout("scenarios/BASE-TOPIA-s4_h24-base")
} # }
```
