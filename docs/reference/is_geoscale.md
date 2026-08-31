# Is this a geoscales::Geoscale?

Type test that does not require `geoscales` to be installed – an S7
object carries its class attribute either way. Accepts both the
installed class name and the unqualified one used when the package is
sourced rather than installed.

## Usage

``` r
is_geoscale(x)
```

## Arguments

- x:

  Any object.

## Value

`TRUE` or `FALSE`.

## Examples

``` r
energyRt:::is_geoscale(NULL)
#> [1] FALSE
```
