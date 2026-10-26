# Clear cached levcost results

Deletes the levcost cache entries belonging to an object (its owning
folder's `levcost/` for saved objects, the project-level
[`get_levcost_cache_path()`](https://energyRt.org/reference/reports_path.md)
folder otherwise), or a directory given directly.

## Usage

``` r
clear_levcost_cache(x = NULL)
```

## Arguments

- x:

  an energyRt object whose cache should be cleared, or a character path
  to a levcost cache directory.

## Value

the deleted directory, invisibly.

## Examples

``` r
# drop every memoized levelised cost (they are keyed on the inputs that
# produced them, so this is only needed after editing a scenario in place)
clear_levcost_cache()
```
