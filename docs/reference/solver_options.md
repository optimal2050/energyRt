# Solver presets

Ready-made solver configurations to pass to
[`solve_scenario()`](https://energyRt.org/reference/solve_model.md) as
`solver = solver_options[["<name>"]]`. The name encodes the route and
the algorithm: a bare backend (`glpk`), `pyomo_*` for the Python route,
`mm_*` for the multimod matrix route, and a `_barrier` suffix for the
interior-point method rather than simplex. The matrix presets name the
matrix writer, the solver and the place: `mm_cuopt_wsl` (cuOpt on a
local GPU through WSL), `mm_cuopt_wsl_py` (the same through cuOpt's
Python interface: a solution at any status, PDLP warm starts through
`gpu_save_warm_start` and `gpu_warm_start`), `mm_cuopt_cloud` (cuOpt on
a rented GPU), `pyomo_cuopt_cloud` (a Pyomo-written matrix on a rented
GPU).

## Usage

``` r
solver_options
```

## Format

A named list of solver settings objects.

## Source

`data-raw/solver_options.R`

## Details

The `julia_*` presets call Julia directly, building a JuMP model in a
Julia subprocess, and so reach the solvers Julia's HiGHS build carries
and the R and Python builds do not: `julia_highs_hipo` (HiGHS's own
parallel interior point method, a threaded CPU factorisation, named
explicitly so it is not restarted with IPX), with `_xover` for a vertex
solution and `_bs3` for scaled bounds, `julia_highs_ipx` for the older
interior point method, and `julia_highs_pdlp` / `julia_highs_hipdlp` for
the first-order methods. The R `highs` build is compiled without amd,
blas, metis and rcm and silently reverts to `choose` when asked for
`hipo`.

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
#> [17] "mm_cuopt_wsl_py"             "mm_cuopt_cloud"             
#> [19] "mm_cuopt_cloud_crossover"    "pyomo_cuopt_cloud"          
#> [21] "pyomo_cuopt_cloud_crossover" "julia_mps"                  
#> [23] "julia_cuopt_cloud"           "mmcloud"                    
#> [25] "mmcloud_crossover"           "mmcloud_pyomo"              
#> [27] "mmcloud_pyomo_crossover"     "neos_pyomo_cplex"           
#> [29] "neos_pyomo_cplex_barrier"    "neos_pyomo_cbc"             
#> [31] "julia_cbc"                   "julia_cplex"                
#> [33] "julia_cplex_barrier"         "julia_highs"                
#> [35] "julia_highs_rdata"           "julia_highs_barrier"        
#> [37] "julia_highs_barrier_bs3"     "julia_highs_hipo"           
#> [39] "julia_highs_hipo_xover"      "julia_highs_hipo_bs3"       
#> [41] "julia_highs_ipx"             "julia_glpk"                 
#> [43] "julia_highs_simplex"         "julia_highs_parallel"       
#> [45] "julia_highs_pdlp"            "julia_highs_hipdlp"         
#> [47] "julia_cuopt"                 "julia_cuopt_concurrent"     
#> [49] "julia_cuopt_pdlp"            "julia_cuopt_crossover"      
#> [51] "gams_csv_cplex"              "gams_gdx_cplex"             
#> [53] "gams_gdx_cplex_barrier"      "gams_gdx_cplex_parallel"    
#> [55] "gams_csv_cbc"                "gams_gdx_cbc"               
#> [57] "neos_gams_cplex"             "neos_gams_cplex_barrier"    
#> [59] "neos_gams_cbc"              
solver_options[["glpk"]]
#> $name
#> [1] "glpk"
#> 
#> $lang
#> [1] "GLPK"
#> 
```
