# Image or icon attached to a model object

Resolve the `misc$image` (or `misc$icon`) convention on a model object
and report whether the reference can be used. Local paths are checked
for existence; URLs are reported as such without a network round-trip.

## Usage

``` r
object_image(object, which = c("image", "icon"))
```

## Arguments

- object:

  a model object, e.g. a `commodity` or `technology`.

- which:

  character, `"image"` (the default) or `"icon"`.

## Value

A list with `path` (character, or `NULL` when nothing is set) and
`status`, one of `"none"`, `"url"`, `"ok"` or `"missing"`.

## See also

Other commodity:
[`commodity_properties()`](https://energyRt.org/reference/commodity_properties.md),
[`commodity_property()`](https://energyRt.org/reference/commodity_property.md),
[`newCommodity()`](https://energyRt.org/reference/newCommodity.md)

## Examples

``` r
COA <- newCommodity("COA", image = "https://example.org/coal.png")
object_image(COA)
#> $path
#> [1] "https://example.org/coal.png"
#> 
#> $status
#> [1] "url"
#> 
```
