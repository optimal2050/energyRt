# Draw a schematic representation of an export process

Draw a schematic representation of an export process

## Usage

``` r
# S3 method for class 'export'
draw(obj, ...)
```

## Arguments

- obj:

  An export object

- ...:

  Additional arguments to be passed to draw_process

## Value

A figure with a schematic representation of the export process.

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
draw(EXPOIL)
#> Error: unable to find an inherited method for function ‘draw’ for signature ‘obj = "export"’
```
