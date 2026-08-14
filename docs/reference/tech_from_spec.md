# Build a technology object from a techspec

Reads, validates, and constructs the
[`newTechnology()`](https://energyRt.org/reference/technology.md) call
described by the spec. Errors in the issue report stop the build
(warnings are shown).

## Usage

``` r
tech_from_spec(spec, strict = FALSE)
```

## Arguments

- spec:

  Path or list (see
  [`read_techspec()`](https://energyRt.org/reference/read_techspec.md)).

- strict:

  Stop on warnings too? Default `FALSE`.

## Value

A `technology` object.

## Examples

``` r
f <- system.file("techspec", "examples", "coal_power.yml",
                 package = "energyRt")
if (nzchar(f)) tech_from_spec(f)
#> An object of class "technology"
#> Slot "name":
#> [1] "ECOA"
#> 
#> Slot "desc":
#> [1] "Coal condensing power plant"
#> 
#> Slot "input":
#>   comm unit group combustion
#> 1  COA   PJ  <NA>         NA
#> 
#> Slot "output":
#>   comm unit group
#> 1  ELC   PJ  <NA>
#> 
#> Slot "aux":
#>   acomm unit
#> 1   CO2   kt
#> 
#> Slot "units":
#>   capacity  use activity costs
#> 1       GW <NA>       PJ  MUSD
#> 
#> Slot "group":
#> [1] group desc  unit 
#> <0 rows> (or 0-length row.names)
#> 
#> Slot "cap2act":
#> [1] 31.536
#> 
#> Slot "geff":
#> [1] vintage   cluster   region    year      timeslice group     ginp2use 
#> <0 rows> (or 0-length row.names)
#> 
#> Slot "ceff":
#>   vintage cluster region year timeslice comm cinp2use use2cact cact2cout
#> 1    <NA>    <NA>   <NA>   NA      <NA>  COA        1       NA        NA
#> 2    <NA>    <NA>   <NA>   NA      <NA>  ELC       NA     0.35         1
#>   cinp2ginp share.lo share.up share.fx afc.lo afc.up afc.fx
#> 1        NA       NA       NA       NA     NA     NA     NA
#> 2        NA       NA       NA       NA     NA     NA     NA
#> 
#> Slot "aeff":
#>   vintage cluster acomm comm region year timeslice cinp2ainp cinp2aout
#> 1    <NA>    <NA>   CO2  COA   <NA>   NA      <NA>        NA        96
#>   cout2ainp cout2aout act2ainp act2aout cap2ainp cap2aout ncap2ainp ncap2aout
#> 1        NA        NA       NA       NA       NA       NA        NA        NA
#>   stg2ainp sinp2ainp sout2ainp stg2aout sinp2aout sout2aout
#> 1       NA        NA        NA       NA        NA        NA
#> 
#> Slot "af":
#>   vintage cluster region year timeslice af.lo af.up af.fx rampup rampdown
#> 1    <NA>    <NA>   <NA>   NA      <NA>    NA   0.9    NA     NA       NA
#> 
#> Slot "afs":
#> [1] vintage   cluster   region    year      timeslice afs.lo    afs.up   
#> [8] afs.fx   
#> <0 rows> (or 0-length row.names)
#> 
#> Slot "weather":
#>  [1] vintage cluster weather comm    wafc.lo wafc.up wafc.fx waf.lo  waf.up 
#> [10] waf.fx  wafs.lo wafs.up wafs.fx
#> <0 rows> (or 0-length row.names)
#> 
#> Slot "fixom":
#>   vintage cluster region year fixom
#> 1    <NA>    <NA>   <NA>   NA    30
#> 
#> Slot "varom":
#>  [1] vintage   cluster   region    year      timeslice comm      acomm    
#>  [8] varom     cvarom    avarom   
#> <0 rows> (or 0-length row.names)
#> 
#> Slot "invcost":
#>   vintage cluster region year invcost wacc payback eac retcost
#> 1    <NA>    <NA>   <NA>   NA    1500   NA      NA  NA      NA
#> 
#> Slot "cluster":
#> [1] cluster desc    region  order  
#> <0 rows> (or 0-length row.names)
#> 
#> Slot "vintage":
#>   vintage region cluster start end olife
#> 1    <NA>   <NA>    <NA>    NA  NA    40
#> 
#> Slot "capacity":
#>  [1] vintage cluster region  year    stock   cap.lo  cap.up  cap.fx  ncap.lo
#> [10] ncap.up ncap.fx ret.lo  ret.up  ret.fx 
#> <0 rows> (or 0-length row.names)
#> 
#> Slot "optimizeRetirement":
#> [1] FALSE
#> 
#> Slot "fullYear":
#> [1] TRUE
#> 
#> Slot "timeframe":
#> character(0)
#> 
#> Slot "region":
#> character(0)
#> 
#> Slot "misc":
#> $notes
#> [1] "Teaching example. Numbers are illustrative, not calibrated."
#> 
#> 
```
