# Convert a model's costs to one currency

A model has one currency whether it says so or not: the objective adds
every cost together with coefficient 1. Where objects were priced in
another one, this converts them to a single currency-year and reports
every factor it applied.

## Usage

``` r
normalize_model_currency(
  mod,
  target = NULL,
  basis = "gdp",
  currency_route = "deflate_then_fx",
  verbose = TRUE
)
```

## Arguments

- mod:

  a `model`.

- target:

  the currency-year to convert to, e.g. `"MUSD2020"`. Defaults to the
  model's own `config@units$currency`.

- basis:

  deflator basis, `"gdp"` (default) or `"cpi"`.

- currency_route:

  `"deflate_then_fx"` (default) or `"fx_then_deflate"`; see
  [`convert()`](https://energyRt.org/reference/convert.md).

- verbose:

  print the per-group factors.

## Value

The model, with `misc$currency` carrying the report. Unchanged when it
declares no currency, or when every factor is 1.

## Details

Called for its own sake it returns a model whose costs have moved.
[`interpolate_model()`](https://energyRt.org/reference/interpolate_model.md)
calls it on the copy it builds from, so a model's objects keep the
values they were written with while the scenario carries the converted
ones; the report is then in `scen@misc$currency`.

An object that declares no currency is in the **model's** currency,
which is what a model-level declaration means. Its factor is therefore 1
when `target` is the model's own currency – the interpolation case,
where such objects are reported and left alone – and a real conversion
when another `target` is asked for.

## See also

[`convert()`](https://energyRt.org/reference/convert.md),
[currency_fx](https://energyRt.org/reference/currency_fx.md),
[currency_deflator](https://energyRt.org/reference/currency_deflator.md)

## Examples

``` r
PV <- newTechnology("PV", units = data.frame(capacity = "GW",
                                             currency = "MEUR2015"),
                    invcost = data.frame(invcost = 700))
GAS_SUP <- newSupply("GAS_SUP", commodity = "GAS", unit = "PJ",
                     units = data.frame(currency = "MUSD2015"),
                     supply = data.frame(cost = 8))
mod <- newModel("demo", currency = "MUSD2020",
                data = add(newRepository("r"), newCommodity("GAS"), PV,
                           GAS_SUP))

mod <- normalize_model_currency(mod)
#> Warning: EUR2015 -> USD2020: the two routes differ by 3.3% (a factor of 1.033). Using deflate_then_fx.
#> The gap IS the change in the real exchange rate over the interval, so neither route is a rounding of the other.
#> Pass currency_route = "both" to see each, or raise options(energyRt.currency_route_tol=) to quieten this.
#> Currency normalisation -> MUSD2020
#>     PV        MEUR2015   × 1.241     (deflate_then_fx)
#>     GAS_SUP   MUSD2015   × 1.0828    (USD gdp deflator)
mod@misc$currency$objects          # one row per object, with its factor
#>          object      class     from declared   factor             note
#> PV           PV technology MEUR2015     TRUE 1.241012  deflate_then_fx
#> GAS_SUP GAS_SUP     supply MUSD2015     TRUE 1.082839 USD gdp deflator
#>         converted
#> PV           TRUE
#> GAS_SUP      TRUE
mod@misc$currency$fx               # the rows the factors were built from
#>   code year lcu_per_usd      source
#> 1  EUR 2020   0.8755064 PA.NUS.FCRF
#> 2  EUR 2025   0.8849690 PA.NUS.FCRF
#> 3  USD 2020   1.0000000   numeraire
#> 4  USD 2025   1.0000000   numeraire

# the costs as authored, and as normalised
getObject(mod, name = "PV")[[1]]@invcost$invcost
#> [1] 868.7081
700 * convert(1, "MEUR2015", "MUSD2020")
#> Warning: EUR2015 -> USD2020: the two routes differ by 3.3% (a factor of 1.033). Using deflate_then_fx.
#> The gap IS the change in the real exchange rate over the interval, so neither route is a rounding of the other.
#> Pass currency_route = "both" to see each, or raise options(energyRt.currency_route_tol=) to quieten this.
#> [1] 868.7081
```
