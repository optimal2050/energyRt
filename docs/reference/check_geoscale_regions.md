# Check a geoscale against the model's declared regions

Warns when the two disagree. This is deliberately a warning, not an
error: `config@region` remains authoritative, and a geoscale covering
more ground than the model uses (a world map for a two-country model) is
normal and useful.

## Usage

``` r
check_geoscale_regions(geoscale, region, level = NULL)
```

## Arguments

- geoscale:

  A
  [`geoscales::Geoscale`](https://optimal2050.github.io/geoscales/r/reference/Geoscale.html).

- region:

  Character vector of declared model regions.

- level:

  Level of the geoscale to match against. Defaults to the finest.

## Value

Invisibly, the regions that are declared but absent from `geoscale`.

## See also

Other geoscale:
[`plot_map()`](https://energyRt.org/reference/plot_map.md),
[`setGeoscale,config-method`](https://energyRt.org/reference/setGeoscale.md),
[`utopia_geoscale()`](https://energyRt.org/reference/utopia_geoscale.md)
