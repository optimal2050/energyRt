# Is object stored on disk?

The complement of
[`isInMemory()`](https://energyRt.org/reference/isInMemory.md). An
on-disk object holds its data in an Arrow dataset rather than in its
slots, so reading it is a scan rather than a subset – which is what
makes predicate pushdown possible for it and pointless for an in-memory
one.

## Usage

``` r
isOnDisk(object)
```

## Arguments

- object:

  Object, checks

## Value

Logical value, TRUE if object is stored on disk, FALSE if in memory.

## See also

[`isInMemory()`](https://energyRt.org/reference/isInMemory.md)

## Examples

``` r
if (FALSE) { # \dontrun{
isOnDisk(scen_BASE)
} # }
```
