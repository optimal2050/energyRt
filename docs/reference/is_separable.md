# Can these regions be separated from the model?

A quick check, before any cutting, of whether
[`subset_model_regions()`](https://energyRt.org/reference/subset_model_regions.md)
can honour a region set as it stands. A cut that keeps a whole region
group needs nothing; one that splits a group has to be told what to do
with the values declared at that group, and
`subset_model_regions(dryrun = TRUE)` is the same check in full – what
splits, which values ask, which are carried, and the ways on.

## Usage

``` r
is_separable(
  object,
  region = NULL,
  geoframe = NULL,
  split_weight = NULL,
  keep = NULL,
  drop = NULL
)
```

## Arguments

- object:

  a `model`.

- region:

  the regions to keep, at any geoframe (a coarse code expands to its
  members).

- geoframe:

  instead of `region`: sweep a geoframe, asking the question once per
  code at that frame. Returns a named logical.

- split_weight, keep, drop:

  as in
  [`subset_model_regions()`](https://energyRt.org/reference/subset_model_regions.md):
  the weight each quantity would be divided by, or the columns held
  whole / left out.

## Value

A logical. With `region`, one value, carrying the report in
`attr(x, "report")`. With `geoframe`, one value per code at that frame.

## See also

[`subset_model_regions()`](https://energyRt.org/reference/subset_model_regions.md)
