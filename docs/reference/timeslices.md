# Sets of the common formats with structure

For each shipped timeslice format, the labels of every level it is built
from, outermost first. `d365_h24` for example holds the 365 day labels
(`d001`..`d365`) and the 24 hour labels (`h00`..`h23`); the timeslice
names themselves are their cross-product.

## Usage

``` r
tsl_sets

tsl_levels
```

## Format

An object of class `list` of length 1.

A named list of 7 formats – `d364`, `d365`, `d366`, `d364_h24`,
`d365_h24`, `d366_h24`, `m12_h24` – each a list of character vectors,
one per level.

## Examples

``` r
names(tsl_levels)
#> [1] "d364"     "d365"     "d366"     "d364_h24" "d365_h24" "d366_h24" "m12_h24" 
lengths(tsl_levels$d365_h24)
#> YDAY HOUR 
#>  365   24 
```
