# Generate an R script that recreates an energyRt object

`design()` takes an energyRt object and returns (and optionally writes
to a file) an R script that recreates it using the appropriate
constructor function (e.g.
[`newTechnology()`](https://energyRt.org/reference/technology.md),
[`newCommodity()`](https://energyRt.org/reference/newCommodity.md),
etc.).

## Usage

``` r
design(x, ...)

# S4 method for class 'technology'
design(x, file = NULL, var = NULL, ...)
```

## Arguments

- x:

  An energyRt object (e.g. `technology`, `commodity`, `supply`, …).

- ...:

  Reserved for future use / class-specific arguments.

- file:

  Optional character string. Path to a file to write the script to. If
  `NULL` (default), the script is printed to the console.

- var:

  Optional character string. Name of the R variable to assign the result
  to in the generated script. Defaults to `x@name`.

## Value

Invisibly returns the generated code as a character string.

## Examples

``` r
ECOAL <- newTechnology(
  name    = "ECOAL",
  desc    = "Coal power plant",
  input   = data.frame(comm = "COAL", unit = "MMBtu", combustion = 1),
  output  = data.frame(comm = "ELC",  unit = "MWh"),
  cap2act = 8760,
  ceff    = data.frame(comm = "COAL", cinp2use = 1/10),
  olife   = data.frame(olife = 30L),
  region  = c("R1", "R2")
)
design(ECOAL)
#> ECOAL <- newTechnology(
#>   name = "ECOAL",
#>   desc = "Coal power plant",
#>   input = data.frame(
#>     comm = "COAL",
#>     unit = "MMBtu",
#>     combustion = 1
#>   ),
#>   output = data.frame(
#>     comm = "ELC",
#>     unit = "MWh"
#>   ),
#>   ceff = data.frame(
#>     comm = "COAL",
#>     cinp2use = 0.1
#>   ),
#>   olife = data.frame(
#>     olife = 30L
#>   ),
#>   cap2act = 8760,
#>   region = c("R1", "R2")
#> )
```
