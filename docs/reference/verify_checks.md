# Checks available to `verify_solution()`

Checks available to
[`verify_solution()`](https://energyRt.org/reference/verify_solution.md)

## Usage

``` r
verify_checks()
```

## Value

a data.frame with one row per check: `check`, `group`, `path`
(`"modInp"`, `"objects"` or `"registry"` – which data the expectation is
derived from) and `tier` (`"default"` or `"optin"`).

## Examples

``` r
# the catalogue of checks `verify_solution()` can run, with their tiers
checks <- verify_checks()
head(checks, 4)
#>          check     group   path    tier                                   desc
#> 1      balance equations modInp default           vBalance = vOutTot - vInpTot
#> 2 balance_sign equations modInp default sign of vBalance per commodity limtype
#> 3      out_tot equations modInp default    vOutTot = sum of source-side totals
#> 4      inp_tot equations modInp default      vInpTot = sum of sink-side totals
table(checks$tier)
#> 
#> default   optin 
#>      10       6 
```
