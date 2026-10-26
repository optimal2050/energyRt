# Compare two models' contents

Diffs two models (or repositories) at the declaration level: every
stored object (technologies, commodities, supply, demand, ...) is
matched by name and compared by content hash
([`object_hash()`](https://energyRt.org/reference/model_hash.md), which
ignores `@misc` bookkeeping), yielding an added / removed / changed /
same inventory. For models, the configuration (regions, horizon,
calendar, discount rates, `optimizeRetirement`) is compared as well.
With `details = TRUE` every changed object also gets a recursive
slot-by-slot drill-down, printable per object via
`print(x, name = "...")`.

## Usage

``` r
compare_models(a, b, names = c("A", "B"), tol = 1e-08, details = TRUE)

# S3 method for class 'models_cmp'
print(x, name = NULL, ...)
```

## Arguments

- a, b:

  `model` or `repository` objects.

- names:

  character(2), display names of the two sides.

- tol:

  numeric tolerance for the drill-down's data comparisons.

- details:

  logical, compute the per-object slot drill-down for changed objects.

- x:

  a `models_cmp` object.

- name:

  character, a changed object's name: print its slot-by-slot drill-down
  instead of the summary.

- ...:

  ignored.

## Value

A `models_cmp` object (a list): `inventory`
(`name, class, hash_a, hash_b, status`), `config`
(`item, A, B, differs`; `NULL` for repositories), `details` (named list
of slot diffs for changed objects), `counts` (status table), `names`.

## See also

[`compare_scenarios()`](https://energyRt.org/reference/compare_scenarios.md),
[`compare_inputs()`](https://energyRt.org/reference/compare_inputs.md),
[`object_hash()`](https://energyRt.org/reference/model_hash.md).

Other comparison:
[`compare_inputs()`](https://energyRt.org/reference/compare_inputs.md),
[`compare_scenarios()`](https://energyRt.org/reference/compare_scenarios.md)

## Examples

``` r
ek  <- topia$modules$electricity$R3
mod <- newModel("TOPIA", data = ek$repo,
                calendar = topia$modules$calendars$s4_h24,
                region = ek$regions,
                horizon = topia$modules$horizons$base,
                discount = 0.05)
mod <- setGeoscale(mod, ek$geoscale)

# swap domestic gas for a three-step supply curve and see what moved
mod2 <- add(mod, ek$GAS_CURVE, overwrite = TRUE)
cmp <- compare_models(mod, mod2, names = c("flat", "curve"))
cmp
#> model comparison: flat vs curve
#> changed: 1  same: 34 
#> 
#> --- Objects ---
#>     name  class   hash_a   hash_b  status
#>  SUP_GAS supply e74383cf 757453a4 changed
#> Configuration: identical.
#> 
#> Drill down with print(x, name = "SUP_GAS")

# drill into the one object that changed
print(cmp, name = "SUP_GAS")
#> slot differences for 'SUP_GAS' (flat vs curve):
#> $desc
#> $desc$a
#> [1] "Domestic gas, pooled within a zone"
#> 
#> $desc$b
#> [1] "Domestic gas, stepped marginal cost"
#> 
#> 
#> $supply
#> `attr(old, 'row.names')`: 1    
#> `attr(new, 'row.names')`: 1 2 3
#> 
#> old vs new
#>            ava.fx ava.lo   ava.up cluster cost region timeslice year
#> - old[1, ]     NA     NA 50.00000      NA    6   WEST        NA   NA
#> + new[1, ]     NA     NA 16.66667      S1    4   WEST        NA   NA
#> + new[2, ]     NA     NA 16.66667      S2    8   WEST        NA   NA
#> + new[3, ]     NA     NA 16.66667      S3   12   WEST        NA   NA
#> 
#> `old$ava.fx`: NA      
#> `new$ava.fx`: NA NA NA
#> 
#> `old$ava.lo`: NA      
#> `new$ava.lo`: NA NA NA
#> 
#> `old$ava.up`:               50                                  
#> `new$ava.up`: 16.6666666666667 16.6666666666667 16.6666666666667
#> 
#> `old$cluster`: NA            
#> `new$cluster`: "S1" "S2" "S3"
#> 
#> `old$cost`: 6     
#> `new$cost`: 4 8 12
#> 
#> `old$region`: "WEST"              
#> `new$region`: "WEST" "WEST" "WEST"
#> 
#> `old$timeslice`: NA      
#> `new$timeslice`: NA NA NA
#> 
#> `old$year`: NA      
#> `new$year`: NA NA NA
#> 
#> $cluster
#> `old` is NULL
#> `new` is an S3 object of class <data.frame>, a list
#> 
```
