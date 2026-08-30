# Show the effective energyRt configuration

Prints every registered option with its current value and where that
value came from – an R option, an environment variable, a config file,
or the package default. This is the first thing to run when a solver is
not found.

## Usage

``` r
en_config_show()
```

## Value

a data frame with columns `option`, `value`, `source` and `origin`,
invisibly. `origin` names the environment variable or config file the
value came from, where applicable.

## See also

[`en_config_write()`](https://energyRt.org/reference/en_config.md) to
persist the current settings.

Other options:
[`en_config_path()`](https://energyRt.org/reference/en_config.md),
[`get_arrow_format()`](https://energyRt.org/reference/arrow_format.md),
[`get_default_solver()`](https://energyRt.org/reference/default_solver.md),
[`isVerbose()`](https://energyRt.org/reference/isVerbose.md),
[`set_log_file()`](https://energyRt.org/reference/log.md),
[`set_option()`](https://energyRt.org/reference/en_option.md),
[`set_registry_file()`](https://energyRt.org/reference/registry_file.md),
[`set_reports_path()`](https://energyRt.org/reference/reports_path.md),
[`set_scenarios_path()`](https://energyRt.org/reference/scenarios_path.md),
[`set_solver_path()`](https://energyRt.org/reference/solver_path.md)

## Examples

``` r
en_config_show()
#> 
#> ── energyRt configuration ──────────────────────────────────────────────────────
#>   arrow_compression        "zstd"                      [default]
#>   arrow_compression_level  15                          [default]
#>   arrow_format             "feather"                   [default]
#>   datasets_path            "datasets/"                 [default]
#>   debug                    0                           [default]
#>   gams_path                "C:/GAMS/32/"               [config] C:\Users\admin\AppData\Roaming/R/config/R/energyRt/config.yml
#>   gdxlib_path              "C:/GAMS/35/"               [config] C:\Users\admin\AppData\Roaming/R/config/R/energyRt/config.yml
#>   glpk_path                NULL                        [default]
#>   julia_path               NULL                        [default]
#>   levcost_cache_path       "levcosts/"                 [default]
#>   log_file                 ""                          [default]
#>   models_path              "models/"                   [default]
#>   neos_email               NULL                        [default]
#>   neos_endpoint            "https://neos-server.org:3333"  [default]
#>   progress_bar             TRUE                        [default]
#>   python_path              "C:\Users\admin\AppData\Local\r-miniconda\env...  [config] C:\Users\admin\AppData\Roaming/R/config/R/energyRt/config.yml
#>   registry_file            "energyRt_registry.csv"     [default]
#>   reports_path             "reports/"                  [default]
#>   repositories_path        "repositories/"             [default]
#>   scenarios_path           "scenarios/"                [default]
#>   solver                   list(name="glpk", lang="GLPK")  [config] C:\Users\admin\AppData\Roaming/R/config/R/energyRt/config.yml
#>   store_versioning         "none"                      [default]
#>   verbose                  0                           [default]
#> 
#> ℹ Priority: R option > env var > ./.energyRt.yml > C:\Users\admin\AppData\Roaming/R/config/R/energyRt/config.yml > default
```
