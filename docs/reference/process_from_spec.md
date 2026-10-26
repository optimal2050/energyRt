# Build a process object from a spec

Reads, validates, and evaluates the constructor call described by the
spec —
[`newTechnology()`](https://energyRt.org/reference/technology.md),
[`newStorage()`](https://energyRt.org/reference/storage.md) or
[`newTrade()`](https://energyRt.org/reference/newTrade.md) according to
the spec's `class`. Errors in the issue report stop the build (warnings
are shown).

## Usage

``` r
process_from_spec(spec, strict = FALSE)
```

## Arguments

- spec:

  Path or list (see
  [`read_process_spec()`](https://energyRt.org/reference/read_process_spec.md)).

- strict:

  Stop on warnings too? Default `FALSE`.

## Value

A `technology`, `storage` or `trade` object.

## See also

Other process spec:
[`process_spec_code()`](https://energyRt.org/reference/process_spec_code.md),
[`process_spec_issues()`](https://energyRt.org/reference/process_spec_issues.md),
[`process_to_spec()`](https://energyRt.org/reference/process_to_spec.md),
[`read_process_spec()`](https://energyRt.org/reference/read_process_spec.md)

## Examples

``` r
f <- system.file("techspec", "examples", "coal_power.yml",
                 package = "energyRt")
if (nzchar(f)) process_from_spec(f)
#> Name:  ECOA 
#> desc:  Coal condensing power plant 
#> cap2act:  31.536 
#> region: ""
#> 
#>  input 
#>   comm unit group combustion
#> 1  COA   PJ  <NA>         NA
#> 
#> 
#>  output 
#>   comm unit group
#> 1  ELC   PJ  <NA>
#> 
#> 
#>  aux 
#>   acomm unit
#> 1   CO2   kt
#> 
#> 
#>  units 
#>   capacity  use activity costs
#> 1       GW <NA>       PJ  MUSD
#> 
#> 
#>  ceff 
#>   vintage cluster region year timeslice comm cinp2use use2cact cact2cout
#> 1    <NA>    <NA>   <NA>   NA      <NA>  COA        1       NA        NA
#> 2    <NA>    <NA>   <NA>   NA      <NA>  ELC       NA     0.35         1
#>   cinp2ginp grp.share.lo grp.share.up grp.share.fx afc.lo afc.up afc.fx
#> 1        NA           NA           NA           NA     NA     NA     NA
#> 2        NA           NA           NA           NA     NA     NA     NA
#> 
#> 
#>  aeff 
#>   vintage cluster acomm comm region year timeslice cinp2ainp cinp2aout
#> 1    <NA>    <NA>   CO2  COA   <NA>   NA      <NA>        NA        96
#>   cout2ainp cout2aout act2ainp act2aout cap2ainp cap2aout ncap2ainp ncap2aout
#> 1        NA        NA       NA       NA       NA       NA        NA        NA
#>   pho2ainp pho2aout ret2ainp ret2aout
#> 1       NA       NA       NA       NA
#> 
#> 
#>  af 
#>   vintage cluster region year timeslice af.lo af.up af.fx rampup rampdown
#> 1    <NA>    <NA>   <NA>   NA      <NA>    NA   0.9    NA     NA       NA
#> 
#> 
#>  fixom 
#>   vintage cluster region year fixom
#> 1    <NA>    <NA>   <NA>   NA    30
#> 
#> 
#>  invcost 
#>   vintage cluster region year invcost wacc payback eac retcost
#> 1    <NA>    <NA>   <NA>   NA    1500   NA      NA  NA      NA
#> 
#> 
#>  vintage 
#>   vintage region cluster start end olife
#> 1    <NA>   <NA>    <NA>    NA  NA    40
#> 
```
