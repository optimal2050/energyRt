# Derive the exogenous stock path parameters

Runs after interpolation, next to `compute_eac_parameters()`, and turns
each class's `stock` level into a commissioning stream and a per-period
survival share. See the file header for why the split is necessary.

## Usage

``` r
compute_stock_parameters(scen)
```

## Arguments

- scen:

  scenario object.

## Value

the scenario with `pXStockNew` / `pXStockSurv` filled.
