# Draw a schematic representation of a supply process

Draw a schematic representation of a supply process

## Usage

``` r
# S3 method for class 'supply'
draw(obj, ...)
```

## Arguments

- obj:

  A supply object

- ...:

  Additional arguments to be passed to draw_process

## Value

A figure with a schematic representation of the supply process.

## Examples

``` r
SUP_COA <- newSupply(
   name = "SUP_COA",
   desc = "Coal supply",
   commodity = "COA",
   unit = "PJ",
   reserve = data.frame(
      region = c("R1", "R2", "R3"),
      res.up = c(2e5, 1e4, 3e6) # total reserves/deposits
   ),
   availability = data.frame(
      region = c("R1", "R2", "R3"),
      year = NA_integer_,
      slice = "ANNUAL",
      ava.up = c(1e3, 1e2, 2e2), # annual availability
      cost = c(10, 20, 30) # cost of the resource (currency per unit)
   ),
   region = c("R1", "R2", "R3")
 )
class(SUP_COA)
#> [1] "supply"
#> attr(,"package")
#> [1] "energyRt"
draw(SUP_COA)
#> Error: unable to find an inherited method for function ‘draw’ for signature ‘obj = "supply"’
```
