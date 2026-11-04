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
topia_elc <- topia$modules$electricity
mod <- newModel("TOPIA", data = topia_elc$repo,
                calendar = topia$modules$calendars$s4_h24,
                region = topia_elc$regions,
                horizon = topia$modules$horizons$BY2025_50_by10,
                discount = 0.05)
mod <- setGeoscale(mod, topia_elc$geoscale)

# reprice the gas supply curve and see what moved
sup_gas <- getObject(topia_elc$repo, name = "SUP_GAS", drop = TRUE)
sup_gas@supply$cost <- sup_gas@supply$cost * 1.25
mod2 <- add(mod, sup_gas, overwrite = TRUE)
cmp <- compare_models(mod, mod2, names = c("base", "dear gas"))
cmp
#> model comparison: base vs dear gas
#> changed: 1  same: 47 
#> 
#> --- Objects ---
#>     name  class   hash_a   hash_b  status
#>  SUP_GAS supply 4d4340e5 c74858d7 changed
#> Configuration: identical.
#> 
#> Drill down with print(x, name = "SUP_GAS")

# drill into the one object that changed
print(cmp, name = "SUP_GAS")
#> slot differences for 'SUP_GAS' (base vs dear gas):
#> $supply
#> old vs new
#>             cost
#> - old[1, ]  3.20
#> + new[1, ]  4.00
#> - old[2, ]  4.00
#> + new[2, ]  5.00
#> - old[3, ]  5.60
#> + new[3, ]  7.00
#> - old[4, ]  3.36
#> + new[4, ]  4.20
#> - old[5, ]  4.20
#> + new[5, ]  5.25
#> - old[6, ]  5.88
#> + new[6, ]  7.35
#> - old[7, ]  3.68
#> + new[7, ]  4.60
#> - old[8, ]  4.60
#> + new[8, ]  5.75
#> - old[9, ]  6.44
#> + new[9, ]  8.05
#> - old[10, ] 4.16
#> + new[10, ] 5.20
#> and 2 more ...
#> 
#> `old$cost`: 3.20 4.00 5.60 3.36 4.20 5.88 3.68 4.60 6.44 4.16 and 2 more...
#> `new$cost`: 4.00 5.00 7.00 4.20 5.25 7.35 4.60 5.75 8.05 5.20           ...
#> 
```
