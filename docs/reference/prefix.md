# Unit prefix multipliers

The multiplier each unit prefix stands for, used by
[`convert()`](https://energyRt.org/reference/convert.md) to strip a
prefix off a unit string before looking the unit itself up. `prefix` is
keyed by prefix SYMBOL and `prefix_longname` by the written-out name;
the two carry identical values and differ only in their names.

## Usage

``` r
prefix

prefix_longname
```

## Format

A named numeric array of 11 multipliers, from `p`/`pico` (1e-12) to
`Z`/`zetta` (1e+21).

An object of class `array` of length 11.

## Details

Note `m` is the symbol for *micro* (1e-06) here, not milli.

## Examples

``` r
prefix[["k"]]
#> [1] 1000
prefix_longname[["kilo"]]
#> [1] 1000
```
