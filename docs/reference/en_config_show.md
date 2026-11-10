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
[`get_default_solver()`](https://energyRt.org/reference/default_solver.md),
[`get_exchange_format()`](https://energyRt.org/reference/exchange_format.md),
`get_project_root()`,
[`get_storage_format()`](https://energyRt.org/reference/storage_format.md),
[`isVerbose()`](https://energyRt.org/reference/isVerbose.md),
[`set_log_file()`](https://energyRt.org/reference/log.md),
[`set_option()`](https://energyRt.org/reference/en_option.md),
[`set_path_builder()`](https://energyRt.org/reference/path_builders.md),
[`set_registry_file()`](https://energyRt.org/reference/registry_file.md),
[`set_reports_path()`](https://energyRt.org/reference/reports_path.md),
[`set_scenarios_path()`](https://energyRt.org/reference/scenarios_path.md),
[`set_solver_path()`](https://energyRt.org/reference/solver_path.md)

## Examples

``` r
en_config_show()
#> 
#> ── energyRt configuration ──────────────────────────────────────────────────────
#>   base_maps                   FALSE                       [default]
#>   base_maps_verify            FALSE                       [default]
#>   datasets_path               "datasets/"                 [default]
#>   debug                       0                           [default]
#>   exchange_compression        "lz4"                       [default]
#>   exchange_compression_level  15                          [default]
#>   exchange_format             "feather"                   [default]
#>   fx_equ                      TRUE                        [option]
#>   gams_path                   "C:/GAMS/32/"               [config] C:\Users\admin\AppData\Roaming/R/config/R/energyRt/config.yml
#>   gdxlib_path                 "C:/GAMS/35/"               [config] C:\Users\admin\AppData\Roaming/R/config/R/energyRt/config.yml
#>   glpk_path                   NULL                        [default]
#>   julia_path                  NULL                        [default]
#>   lazy_hash                   TRUE                        [default]
#>   levcost_cache_path          "levcosts/"                 [default]
#>   log_file                    ""                          [default]
#>   map_rules                   TRUE                        [default]
#>   map_rules_verify            FALSE                       [default]
#>   mem_gc_full                 FALSE                       [default]
#>   mm_keep_wildcards           FALSE                       [default]
#>   model_checks_stop           FALSE                       [default]
#>   models_path                 "models/"                   [default]
#>   neos_email                  "admin@optimalsolution.dev"  [config] C:\Users\admin\AppData\Roaming/R/config/R/energyRt/config.yml
#>   neos_endpoint               "https://neos-server.org:3333"  [default]
#>   on_missing_input            "stop"                      [option]
#>   ondisk_min_rows             1e+05                       [default]
#>   param_ref_min_rows          1e+06                       [default]
#>   parent_levels               FALSE                       [option]
#>   path_builders               list()                      [default]
#>   profile_dir                 ""                          [default]
#>   progress_bar                TRUE                        [default]
#>   project_root                file.path(tempdir(), "energyRt")  [default]
#>   python_path                 "C:/Users/admin/.conda/envs/energyrt-solve/"  [config] C:\Users\admin\AppData\Roaming/R/config/R/energyRt/config.yml
#>   registry_file               "energyRt_registry.csv"     [default]
#>   reports_path                "reports/"                  [default]
#>   repositories_path           "repositories/"             [default]
#>   save_mode                   "ref"                       [default]
#>   scen_file_mb_limit          256                         [default]
#>   scenarios_path              "scenarios/"                [default]
#>   solver                      list(name="glpk", lang="GLPK")  [config] C:\Users\admin\AppData\Roaming/R/config/R/energyRt/config.yml
#>   storage_compression         "zstd"                      [default]
#>   storage_compression_level   15                          [default]
#>   storage_format              "feather"                   [default]
#>   threads                     0                           [default]
#>   verbose                     1                           [default]
#> 
#> ℹ Priority: R option > env var > ./.energyRt.yml > C:\Users\admin\AppData\Roaming/R/config/R/energyRt/config.yml > default
```
