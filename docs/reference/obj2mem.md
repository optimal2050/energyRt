# Loads objects from disk to memory

Loads objects from disk to memory

## Usage

``` r
obj2mem(obj, verbose = TRUE)
```

## Arguments

- obj:

  Object of S4 class, saved on disk (scenario, model, etc.)

- verbose:

  If TRUE, prints messages

## Value

Object of the same S4 class as input object, with all of the slots
loaded in memory.

## Examples

``` r
if (FALSE) { # \dontrun{
obj2mem(scen_ondisk)
} # }
```
