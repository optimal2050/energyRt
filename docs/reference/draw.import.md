# Draw a schematic representation of an import process

Draw a schematic representation of an import process

## Usage

``` r
draw.import(obj, ...)
```

## Arguments

- obj:

  An import object

- ...:

  Additional arguments to be passed to draw_process

## Value

A figure with a schematic representation of the import process.

## Examples

``` r
IMPOIL <- newImport(
  name = "IMPOIL", # used in sets
  desc = "Oil import to the model to RoW", # for own reference
  commodity = "OIL", # must match the commodity name in the model
  unit = "Mtoe", # for own reference
  imp = data.frame(
    region = rep(c("R1", "R2"), each = 2), # import region(s)
    year = rep(c(2020, 2050)), # import years
    price = 600, # import price in MUSD/Mtoe (USD/t),
    imp.up = rep(c(1e4, 1e6), each = 2), # upper bound for import in each year
    imp.lo = rep(c(1e4, 1e5), each = 2) # lower bound for import in each year
  )
)
draw(IMPOIL)
#> Error: unable to find an inherited method for function ‘draw’ for signature ‘obj = "import"’
```
