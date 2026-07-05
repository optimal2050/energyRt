# Draw a schematic representation of a storage process

Draw a schematic representation of a storage process

## Usage

``` r
# S3 method for class 'storage'
draw(obj, ...)
```

## Arguments

- obj:

  A storage object

- ...:

  Additional arguments to be passed to draw_process

## Value

A figure with a schematic representation of the storage process.

## Examples

``` r
STG_ELC <- newStorage(
 name = "STG_ELC", # used in sets
 desc = "Electricity storage (battery)", # for own reference
 commodity = "ELECTRICITY", # must match the commodity name in the model
 aux = data.frame(
   acomm = "LITHIUM", # auxiliary commodity for battery production
   unit = "ton" # unit of the auxiliary commodity
 ),
 start = data.frame(
   start = 2020 # the first year of the process is available for installation
 ),
 end = data.frame(
   end = 2030 # last year of the process is available for installation
 ),
 olife = data.frame(
   olife = 20 # operational life of the storage in years
 ),
 seff = data.frame(
   stgeff = 0.999, # storage efficiency
   inpeff = 0.9, # charging efficiency
   outeff = 0.9 # discharging efficiency
 ),
 aeff = data.frame(
   acomm = "LITHIUM", # track lithium use for battery production
   ncap2ainp = convert(4 * 250, "Wh/kg", "GWh/kt") # lithium per energy capacity
 ),
 af = data.frame(
   # af.lo = 0., # lower bound for the capacity factor
   af.up = 1. # upper bound for the capacity factor
 ),
 fixom = data.frame(
   #region = "R1",
   #year = 2020,
   fixom = 0.9 # fixed operation and maintenance cost
 ),
 cap2stg = 4, # four-hours of storage
 invcost = data.frame(
   region = c("R1", NA), # region R1 and all other regions
   invcost = c(1e3, 1.1e3) # investment cost in MUSD/GWh of 4-hour storage
 ),
 fullYear = TRUE, # full year storage cycle
 weather = data.frame(
   weather = "AMBIENT_TEMP", # weather factor for capacity factor
   waf.up = 1 # affects upper boundary of capacity factor
   # waf.lo = 0.9 # affects lower boundary of capacity factor
 )
 # region = c("R1", "R2", "R3"),
)
draw(STG_ELC)
#> Error: unable to find an inherited method for function ‘draw’ for signature ‘obj = "storage"’
```
