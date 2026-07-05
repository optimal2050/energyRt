# Draw a schematic representation of a demand process

Draw a schematic representation of a demand process

## Usage

``` r
# S3 method for class 'demand'
draw(obj, ...)
```

## Arguments

- obj:

  A demand object

- ...:

  Additional arguments to be passed to draw_process

## Value

A figure with a schematic representation of the demand process.

## Examples

``` r
DSTEEL <- newDemand(
 name = "DSTEEL",
 desc = "Steel demand",
 commodity = "STEEL",
 unit = "Mt",
 dem = data.frame(
    region = "UTOPIA", # NA for every region
    year = c(2020, 2030, 2050),
    slice = "ANNUAL",
    dem = c(100, 200, 300)
 ),
 region = "UTOPIA", # optional, to narrow the specification of the demand
 )
 class(DSTEEL)
#> [1] "demand"
#> attr(,"package")
#> [1] "energyRt"
 draw(DSTEEL)
#> Error: unable to find an inherited method for function ‘draw’ for signature ‘obj = "demand"’
```
