# Plot a schematic representation of an export process

Plot a schematic representation of an export process

## Usage

``` r
# S3 method for class 'export'
plot(obj, ...)
```

## Arguments

- ...:

## Examples

``` r
EXPOIL <- newExport(
  name = "EXPOIL", # used in sets
  desc = "Oil export from the model to RoW", # for own reference
  commodity = "OIL", # must match the commodity name in the model
  unit = "Mtoe", # for own reference
  exp = data.frame(
    region = rep(c("R1", "R2"), each = 2), # export region(s)
    year = rep(c(2020, 2050)), # export years
    price = 500, # export price in MUSD/Mtoe (USD/t),
    exp.up = rep(c(1e3, 1e4), each = 2), # upper bound for export in each year
    exp.lo = rep(c(5e2, 0), each = 2) # lower bound for export in each year
  )
)
plot(EXPOIL)
#> Error in as.double(y): cannot coerce type 'S4' to vector of type 'double'
```
