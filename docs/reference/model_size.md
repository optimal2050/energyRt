# Size estimate of an interpolated scenario and the result of folding

Size estimate of an interpolated scenario and the result of folding

## Usage

``` r
model_size(scen, top_n = 15L)
```

## Arguments

- scen:

  a scenario built by
  [`interpolate_model()`](https://energyRt.org/reference/interpolate_model.md).

- top_n:

  number of largest parameters to list.

## Value

an S3 `model_size` object (see `print.model_size()`).
