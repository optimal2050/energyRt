# Stepped supply, export and import curves

Turn a single-price `supply`, `export` or `import` object into a stepped
price curve: parallel steps, each with its own share of the quantity and
its own price. **\[experimental\]**

## Usage

``` r
asSupplyCurve(
  object,
  range = NULL,
  nsteps = NULL,
  shares = NULL,
  price = NULL,
  labels = NULL,
  desc = NULL
)

asImportCurve(
  object,
  range = NULL,
  nsteps = NULL,
  shares = NULL,
  price = NULL,
  labels = NULL,
  desc = NULL
)

asExportCurve(
  object,
  range = NULL,
  nsteps = NULL,
  shares = NULL,
  price = NULL,
  labels = NULL,
  desc = NULL
)
```

## Arguments

- object:

  a `supply`, `export` or `import` object.

- range:

  numeric of length 2, the first and last price of a linear curve.

- nsteps:

  integer, the number of steps. Ignored when `shares` is given.

- shares:

  numeric, each step's fraction of the quantity. Must be positive and
  sum to 1. Default equal.

- price:

  numeric, explicit per-step prices, as an alternative to `range`.

- labels:

  character, step names. Default `S1`, `S2`, ...

- desc:

  character, per-step descriptions.

## Value

An object of the same class, carrying a `cluster` axis.

## Details

`range` describes a continuous linear marginal-price curve and each step
is priced at the MIDPOINT of its band, which reproduces the area under
that curve exactly – the quantity-weighted mean of the step prices is
`mean(range)`. Pass an explicit `price` vector instead for a curve that
is not linear.

The steps become `cluster` variants, so one object expands into one
ordinary object per step before interpolation and they all sum into the
same commodity balance. No new variable, equation or solver-template
change is involved.

**Direction matters and is enforced.** `asSupplyCurve()` and
`asImportCurve()` require a non-decreasing curve, `asExportCurve()` a
non-increasing one. This is not style: the solver fills the cheapest
step first (or, for export, the best-paid first, since export revenue is
a negative cost), and a curve running the other way is not convex.

Every finite quantity bound the object carries is split across the steps
– `ava.*` / `exp.*` / `imp.*` and the cumulative `@reserve`. An object
with no finite bound is refused, because an unbounded curve is
meaningless.

## See also

[`newSupply()`](https://energyRt.org/reference/newSupply.md),
[`newExport()`](https://energyRt.org/reference/newExport.md),
[`newImport()`](https://energyRt.org/reference/newImport.md),
[`lossTranches()`](https://energyRt.org/reference/lossTranches.md)

## Examples

``` r
SUP <- newSupply("COA", commodity = "COA",
                 supply = data.frame(ava.up = 100))
SUP <- asSupplyCurve(SUP, range = c(10, 50), nsteps = 5)
SUP@cluster
#>   cluster                                   desc order
#> 1      S1   step 1 of 5: 0-0.2 of quantity at 14     1
#> 2      S2 step 2 of 5: 0.2-0.4 of quantity at 22     2
#> 3      S3 step 3 of 5: 0.4-0.6 of quantity at 30     3
#> 4      S4 step 4 of 5: 0.6-0.8 of quantity at 38     4
#> 5      S5   step 5 of 5: 0.8-1 of quantity at 46     5
```
