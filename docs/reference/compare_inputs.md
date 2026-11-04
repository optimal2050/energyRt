# Compare two scenarios' model inputs

Mechanically diffs the interpolated model input (`modInp`) of two
scenarios: parameter presence, per-parameter metadata (`dimSets` –
distinguishing order-only changes – `type`, `defVal`, `interpolation`,
`desc`), parameter data (key-based, row-order insensitive, with numeric
tolerance), model sets, the compiled user constraints / user cost terms,
and `@misc`. The classic use is explaining WHY two solved scenarios
differ, or guarding a refactor of the interpolation pipeline.

## Usage

``` r
compare_inputs(a, b, tol = 1e-08, unfold = FALSE, names = c("A", "B"))

# S3 method for class 'inputs_cmp'
print(x, max_diffs = 5L, ...)
```

## Arguments

- a, b:

  `scenario` or `modInp` objects.

- tol:

  numeric, absolute tolerance for value comparisons.

- unfold:

  logical, unfold folded parameters before comparing (scenario inputs
  only).

- names:

  character(2), display names of the two sides.

- x:

  an `inputs_cmp` object.

- max_diffs:

  integer, number of differing parameters to render with a detailed
  waldo diff (when waldo is installed).

- ...:

  ignored.

## Value

An `inputs_cmp` object (a list) with components `names`, `summary` (one
row per common parameter:
`parameter, status, n_A, n_B, n_diff, max_abs_diff, meta`; status is one
of `identical`, `meta-diff`, `value-diff`, `rows-mismatch`,
`rows-only-A`, `rows-only-B`, `structural`, `diff`), `params` (`only_A`
/ `only_B` / `common` with per-parameter detail), `sets`, `equations`,
`misc`, and `tol`. [`print()`](https://energyRt.org/reference/print.md)
gives a compact report; with waldo installed it renders textual diffs
for the first differing parameters.

## Details

Data is compared as stored by default; `unfold = TRUE` collapses
fold-vs-dense representation drift first (requires scenario inputs).

## See also

[`compare_scenarios()`](https://energyRt.org/reference/compare_scenarios.md)
for solved-results comparison,
[`compare_models()`](https://energyRt.org/reference/compare_models.md)
for declaration-level model diffs.

Other comparison:
[`compare_models()`](https://energyRt.org/reference/compare_models.md),
[`compare_scenarios()`](https://energyRt.org/reference/compare_scenarios.md)

## Examples

``` r
# \donttest{
topia_elc <- topia$modules$electricity
mod <- newModel("TOPIA", data = topia_elc$repo,
                calendar = topia$modules$calendars$s4_h24,
                region = topia_elc$regions,
                horizon = topia$modules$horizons$BY2025_50_by10,
                discount = 0.05)
mod <- setGeoscale(mod, topia_elc$geoscale)

# the same model with every step of the gas supply curve priced 25% higher
sup_gas <- getObject(topia_elc$repo, name = "SUP_GAS", drop = TRUE)
sup_gas@supply$cost <- sup_gas@supply$cost * 1.25
mod2 <- add(mod, sup_gas, overwrite = TRUE)
a <- interpolate_model(mod,  name = "BASE")
#> Cost declared at a coarse geoscale level:
#>   TBD_COA_WEST_CENTRAL@invcost: CENTRAL, WEST
#>   TBD_COA_CENTRAL_EAST@invcost: CENTRAL, EAST
#>   TBD_GAS_EAST_CENTRAL@invcost: CENTRAL, EAST
#> The cost is charged ONCE at that cell rather than at each child region, and is discounted at the children's rate. That is usually what a national cost datum means -- said here because it is a different number from naming the children individually.
b <- interpolate_model(mod2, name = "DEAR_GAS")
#> Cost declared at a coarse geoscale level:
#>   TBD_COA_WEST_CENTRAL@invcost: CENTRAL, WEST
#>   TBD_COA_CENTRAL_EAST@invcost: CENTRAL, EAST
#>   TBD_GAS_EAST_CENTRAL@invcost: CENTRAL, EAST
#> The cost is charged ONCE at that cell rather than at each child region, and is discounted at the children's rate. That is usually what a national cost datum means -- said here because it is a different number from naming the children individually.

cmp <- compare_inputs(a, b, names = c("base", "dear gas"))
cmp
#> ===========================================================================
#> compare_inputs:  A = base   vs   B = dear gas   (tol=1e-08, unfold=FALSE)
#> ===========================================================================
#> Parameters: 574 common  | identical: 573  differing: 1  | only-base: 0  only-dear gas: 0
#> 
#> --- Differing parameters ---
#>  parameter     status n_A n_B n_diff max_abs_diff meta
#>   pSupCost value-diff 160 160     12         1.82     
#> 
#> Sets: identical.
#> User constraints / costs: identical.
#> misc: identical.
#> 
#> --- Data diffs (waldo, first 1 of 1) ---
#> ------------------------------------------------------------
#> pSupCost  [value-diff]
#> base vs dear gas
#>                    value
#>   base[142, ]      2.288
#>   base[143, ]      2.470
#>   base[144, ]      2.600
#> - base[145, ]      3.200
#> + dear gas[145, ]  4.000
#> - base[146, ]      3.360
#> + dear gas[146, ]  4.200
#> - base[147, ]      3.680
#> + dear gas[147, ]  4.600
#> - base[148, ]      4.160
#> + dear gas[148, ]  5.200
#> - base[149, ]      4.000
#> + dear gas[149, ]  5.000
#> - base[150, ]      4.200
#> + dear gas[150, ]  5.250
#> - base[151, ]      4.600
#> + dear gas[151, ]  5.750
#> and 8 more ...
#> 
#>       base$value | dear gas$value               
#> [142] 2.29       | 2.29           [142]         
#> [143] 2.47       | 2.47           [143]         
#> [144] 2.60       | 2.60           [144]         
#> [145] 3.20       - 4.00           [145]         
#> [146] 3.36       - 4.20           [146]         
#> [147] 3.68       - 4.60           [147]         
#> [148] 4.16       - 5.20           [148]         
#> [149] 4.00       - 5.00           [149]         
#> [150] 4.20       - 5.25           [150]         
#> [151] 4.60       - 5.75           [151]         
#>   ... ...          ...            and 8 more ...
#> 
#> ===========================================================================
print(cmp, max_diffs = 3)
#> ===========================================================================
#> compare_inputs:  A = base   vs   B = dear gas   (tol=1e-08, unfold=FALSE)
#> ===========================================================================
#> Parameters: 574 common  | identical: 573  differing: 1  | only-base: 0  only-dear gas: 0
#> 
#> --- Differing parameters ---
#>  parameter     status n_A n_B n_diff max_abs_diff meta
#>   pSupCost value-diff 160 160     12         1.82     
#> 
#> Sets: identical.
#> User constraints / costs: identical.
#> misc: identical.
#> 
#> --- Data diffs (waldo, first 1 of 1) ---
#> ------------------------------------------------------------
#> pSupCost  [value-diff]
#> base vs dear gas
#>                    value
#>   base[142, ]      2.288
#>   base[143, ]      2.470
#>   base[144, ]      2.600
#> - base[145, ]      3.200
#> + dear gas[145, ]  4.000
#> - base[146, ]      3.360
#> + dear gas[146, ]  4.200
#> - base[147, ]      3.680
#> + dear gas[147, ]  4.600
#> - base[148, ]      4.160
#> + dear gas[148, ]  5.200
#> - base[149, ]      4.000
#> + dear gas[149, ]  5.000
#> - base[150, ]      4.200
#> + dear gas[150, ]  5.250
#> - base[151, ]      4.600
#> + dear gas[151, ]  5.750
#> and 8 more ...
#> 
#>       base$value | dear gas$value               
#> [142] 2.29       | 2.29           [142]         
#> [143] 2.47       | 2.47           [143]         
#> [144] 2.60       | 2.60           [144]         
#> [145] 3.20       - 4.00           [145]         
#> [146] 3.36       - 4.20           [146]         
#> [147] 3.68       - 4.60           [147]         
#> [148] 4.16       - 5.20           [148]         
#> [149] 4.00       - 5.00           [149]         
#> [150] 4.20       - 5.25           [150]         
#> [151] 4.60       - 5.75           [151]         
#>   ... ...          ...            and 8 more ...
#> 
#> ===========================================================================
# }
```
