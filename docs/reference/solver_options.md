# Solver presets

Ready-made solver configurations to pass to
[`solve_scenario()`](https://energyRt.org/reference/solve_model.md) as
`solver = solver_options[["<name>"]]`. The name encodes the route and
the algorithm: a bare backend (`glpk`), `pyomo_*` for the Python route,
`mm_*` for the multimod matrix route, and a `_barrier` suffix for the
interior-point method rather than simplex. The matrix presets name the
matrix writer, the solver and the place: `mm_cuopt_wsl` (cuOpt on a
local GPU through WSL), `mm_cuopt_cloud` (cuOpt on a rented GPU),
`pyomo_cuopt_cloud` (a Pyomo-written matrix on a rented GPU).

## Usage

``` r
solver_options
```

## Format

A named list of solver settings objects.

## Source

`data-raw/solver_options.R`

## Details

These entries are data only – selecting one does not check that the
solver is installed. The former names `mmcloud`, `mmcloud_crossover`,
`mmcloud_pyomo` and `mmcloud_pyomo_crossover` remain as entries that
stop a solve and name their replacement.

## See also

[`solve_scenario()`](https://energyRt.org/reference/solve_model.md)

## Examples

``` r
names(solver_options)
#>  [1] "glpk"                        "pyomo_cbc"                  
#>  [3] "pyomo_cbc_sqlite"            "pyomo_cplex"                
#>  [5] "pyomo_cplex_barrier"         "pyomo_glpk"                 
#>  [7] "pyomo_highs"                 "pyomo_highs_barrier"        
#>  [9] "pyomo_mps"                   "mm_highs"                   
#> [11] "mm_highs_barrier"            "mm_highs_barrier_bs3"       
#> [13] "mm_julia_highs_barrier"      "mm_julia_highs_barrier_bs3" 
#> [15] "mm_cuopt_wsl"                "mm_cuopt_wsl_crossover"     
#> [17] "mm_cuopt_cloud"              "mm_cuopt_cloud_crossover"   
#> [19] "pyomo_cuopt_cloud"           "pyomo_cuopt_cloud_crossover"
#> [21] "julia_mps"                   "julia_cuopt_cloud"          
#> [23] "mmcloud"                     "mmcloud_crossover"          
#> [25] "mmcloud_pyomo"               "mmcloud_pyomo_crossover"    
#> [27] "neos_pyomo_cplex"            "neos_pyomo_cplex_barrier"   
#> [29] "neos_pyomo_cbc"              "julia_cbc"                  
#> [31] "julia_cplex"                 "julia_cplex_barrier"        
#> [33] "julia_highs"                 "julia_highs_rdata"          
#> [35] "julia_highs_barrier"         "julia_highs_barrier_bs3"    
#> [37] "julia_glpk"                  "julia_highs_simplex"        
#> [39] "julia_highs_parallel"        "julia_highs_pdlp"           
#> [41] "julia_highs_hipdlp"          "julia_cuopt"                
#> [43] "julia_cuopt_concurrent"      "julia_cuopt_pdlp"           
#> [45] "julia_cuopt_crossover"       "gams_csv_cplex"             
#> [47] "gams_gdx_cplex"              "gams_gdx_cplex_barrier"     
#> [49] "gams_gdx_cplex_parallel"     "gams_csv_cbc"               
#> [51] "gams_gdx_cbc"                "neos_gams_cplex"            
#> [53] "neos_gams_cplex_barrier"     "neos_gams_cbc"              
solver_options[["glpk"]]
#> $name
#> [1] "glpk"
#> 
#> $lang
#> [1] "GLPK"
#> 
```
