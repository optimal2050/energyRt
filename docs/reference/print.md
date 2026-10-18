# Print methods for the energyRt classes

Print methods for the energyRt classes

## Usage

``` r
# S4 method for class 'modelData'
print(x, ...)

print(x, ...)

# S3 method for class 'demand'
print(x, ...)

# S3 method for class 'model'
print(x, ...)

# S3 method for class 'scenario'
print(x, ...)

# S3 method for class 'supply'
print(x, ...)

# S3 method for class 'technology'
print(x, ...)

# S3 method for class 'config'
print(x, ...)
```

## Arguments

- x:

  the object to print.

- ...:

  passed to the dispatched print method.

## Examples

``` r
# every class prints a summary of what it carries, not its slots
tech <- newTechnology(
  "ECOAL",
  desc    = "Coal power plant",
  input   = data.frame(comm = "COA", unit = "MMBtu"),
  output  = data.frame(comm = "ELC", unit = "MWh"),
  ceff    = data.frame(comm = "COA", cinp2use = 0.4),
  invcost = data.frame(invcost = 2000),
  olife   = data.frame(olife = 30),
  cap2act = 8760
)
print(tech)
#> Name:  ECOAL 
#> desc:  Coal power plant 
#> cap2act:  8760 
#> region: ""
#> 
#>  input 
#>   comm  unit group combustion
#> 1  COA MMBtu  <NA>         NA
#> 
#> 
#>  output 
#>   comm unit group
#> 1  ELC  MWh  <NA>
#> 
#> 
#>  ceff 
#>   vintage cluster region year timeslice comm cinp2use use2cact cact2cout
#> 1    <NA>    <NA>   <NA>   NA      <NA>  COA      0.4       NA        NA
#>   cinp2ginp grp.share.lo grp.share.up grp.share.fx afc.lo afc.up afc.fx
#> 1        NA           NA           NA           NA     NA     NA     NA
#> 
#> 
#>  invcost 
#>   vintage cluster region year invcost wacc payback eac retcost
#> 1    <NA>    <NA>   <NA>   NA    2000   NA      NA  NA      NA
#> 
#> 
#>  vintage 
#>   vintage region cluster start end olife
#> 1    <NA>   <NA>    <NA>    NA  NA    30
#> 

# a model lists its data, calendar, regions and horizon
ek  <- topia$modules$electricity$R3
mod <- newModel("TOPIA", data = ek$repo,
                calendar = topia$modules$calendars$s4_h24,
                region = ek$regions,
                horizon = topia$modules$horizons$base)
print(mod)
#> Name:  TOPIA 

# the configuration object behind it
print(mod@config)
#> 
#>  defVal 
#>   act2ainp act2aout af.lo af.up afc.lo afc.up afs.lo afs.up agg ava.lo ava.up
#> 1        0        0     0     1      0    Inf      0    Inf   0      0    Inf
#>   avarom cact2cout cap2act cap2ainp cap2aout cap2cat duration card cdst2ainp
#> 1      0         1       1        0        0       1        1    1         0
#>   cdst2aout charge inp.af.lo inp.af.up cinp2ainp cinp2aout cinp2ginp cinp2use
#> 1         0      0         0       Inf         0         0         1        1
#>   combustion cost out.af.lo out.af.up cout2ainp cout2aout csrc2ainp csrc2aout
#> 1          1    0         0       Inf         0         0         0         0
#>   cvarom dem discount dummyExport dummyImport emis exp.lo exp.up fixom ginp2use
#> 1      0   0        0         Inf         Inf    0      0    Inf     0        1
#>   imp.lo imp.up inpcost inpeff invcost markup ncap2ainp ncap2aout ncap2stg
#> 1      0    Inf       0      1       0      0         0         0        0
#>   olife ord outcost outeff price rampdown rampup res.lo res.up reserve rhs
#> 1     1   1       0      1     0      Inf    Inf      0    Inf     Inf   0
#>   grp.share.lo grp.share.up stg2ainp stg2aout stgcost stgeff stock subs tax
#> 1            0            1        0        0       0      1     0    0   0
#>   teff use2cact value varom waf.lo waf.up wafc.lo wafc.up wafs.lo wafs.up
#> 1    1        1     0     0      0      0       0       0       0       0
#>   wava.lo wava.up inp.waf.lo inp.waf.up out.waf.lo out.waf.up wval cap.lo
#> 1       0       0          0          0          0          0    0      0
#>   cap.up ncap.lo ncap.up ret.lo ret.up retcost
#> 1    Inf       0     Inf      0    Inf       0
#> 
#> 
#>  interpolation 
#>           act2ainp         act2aout            af.lo            af.up
#> 1 back.inter.forth back.inter.forth back.inter.forth back.inter.forth
#>             afc.lo           afc.up           afs.lo           afs.up
#> 1 back.inter.forth back.inter.forth back.inter.forth back.inter.forth
#>                agg           ava.lo           ava.up           avarom
#> 1 back.inter.forth back.inter.forth back.inter.forth back.inter.forth
#>          cact2cout          cap2act         cap2ainp         cap2aout
#> 1 back.inter.forth back.inter.forth back.inter.forth back.inter.forth
#>            cap2cat         duration             card        cdst2ainp
#> 1 back.inter.forth back.inter.forth back.inter.forth back.inter.forth
#>          cdst2aout charge        inp.af.lo        inp.af.up        cinp2ainp
#> 1 back.inter.forth        back.inter.forth back.inter.forth back.inter.forth
#>          cinp2aout        cinp2ginp         cinp2use       combustion
#> 1 back.inter.forth back.inter.forth back.inter.forth back.inter.forth
#>               cost        out.af.lo        out.af.up        cout2ainp
#> 1 back.inter.forth back.inter.forth back.inter.forth back.inter.forth
#>          cout2aout        csrc2ainp        csrc2aout           cvarom
#> 1 back.inter.forth back.inter.forth back.inter.forth back.inter.forth
#>                dem         discount      dummyExport      dummyImport
#> 1 back.inter.forth back.inter.forth back.inter.forth back.inter.forth
#>               emis           exp.lo           exp.up            fixom
#> 1 back.inter.forth back.inter.forth back.inter.forth back.inter.forth
#>           fraction         ginp2use           imp.lo           imp.up
#> 1 back.inter.forth back.inter.forth back.inter.forth back.inter.forth
#>            inpcost           inpeff          invcost          retcost
#> 1 back.inter.forth back.inter.forth back.inter.forth back.inter.forth
#>             markup        ncap2ainp        ncap2aout ncap2stg            olife
#> 1 back.inter.forth back.inter.forth back.inter.forth          back.inter.forth
#>                ord          outcost           outeff            price
#> 1 back.inter.forth back.inter.forth back.inter.forth back.inter.forth
#>           rampdown           rampup           res.lo           res.up
#> 1 back.inter.forth back.inter.forth back.inter.forth back.inter.forth
#>            reserve   rhs     grp.share.lo     grp.share.up         stg2ainp
#> 1 back.inter.forth inter back.inter.forth back.inter.forth back.inter.forth
#>           stg2aout          stgcost           stgeff            stock
#> 1 back.inter.forth back.inter.forth back.inter.forth back.inter.forth
#>        cap.lo      cap.up     ncap.lo     ncap.up      ret.lo      ret.up
#> 1 inter.forth inter.forth inter.forth inter.forth inter.forth inter.forth
#>          subs         tax             teff         use2cact            value
#> 1 inter.forth inter.forth back.inter.forth back.inter.forth back.inter.forth
#>              varom           waf.lo           waf.up          wafc.lo
#> 1 back.inter.forth back.inter.forth back.inter.forth back.inter.forth
#>            wafc.up          wafs.lo          wafs.up          wava.lo
#> 1 back.inter.forth back.inter.forth back.inter.forth back.inter.forth
#>            wava.up       inp.waf.lo       inp.waf.up       out.waf.lo
#> 1 back.inter.forth back.inter.forth back.inter.forth back.inter.forth
#>         out.waf.up             wval
#> 1 back.inter.forth back.inter.forth
#> 
```
