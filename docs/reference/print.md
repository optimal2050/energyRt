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
#> 
#>  defVal 
#> data frame with 0 columns and 1 row
#> 
#> 
#>  interpolation 
#> data frame with 0 columns and 1 row
#> 

# a model lists its data, calendar, regions and horizon
topia_elc <- topia$modules$electricity
mod <- newModel("TOPIA", data = topia_elc$repo,
                calendar = topia$modules$calendars$s4_h24,
                region = topia_elc$regions,
                horizon = topia$modules$horizons$BY2025_50_by10)
mod <- setGeoscale(mod, topia_elc$geoscale)
print(mod)
#> Name:  TOPIA 

# the configuration object behind it
print(mod@config)
#> 
#>  defVal 
#> data frame with 0 columns and 1 row
#> 
#> 
#>  interpolation 
#> data frame with 0 columns and 1 row
#> 
```
