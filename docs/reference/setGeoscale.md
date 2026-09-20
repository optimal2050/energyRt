# Attach or read a model's geoscale

A geoscale describes the model's regions – their nesting into coarser
levels, their weights, and optionally their geometry. It is built with
the [geoscales](https://github.com/optimal2050/geoscales) package.

## Usage

``` r
# S4 method for class 'config'
setGeoscale(obj, geoscale, ...)

# S4 method for class 'config'
getGeoscale(obj, ...)

# S4 method for class 'model'
setGeoscale(obj, geoscale, ...)

# S4 method for class 'model'
getGeoscale(obj, ...)

# S4 method for class 'scenario'
setGeoscale(obj, geoscale, ...)

# S4 method for class 'scenario'
getGeoscale(obj, ...)

# S4 method for class 'config'
getCalendar(obj)

# S4 method for class 'model'
getCalendar(obj)

# S4 method for class 'scenario'
getCalendar(obj)
```

## Arguments

- obj:

  A `config`, `settings`, `model` or `scenario`.

- geoscale:

  A
  [`geoscales::Geoscale`](https://optimal2050.github.io/geoscales/r/reference/Geoscale.html),
  or `NULL` to clear it.

- ...:

  Unused.

## Value

`setGeoscale()` returns the modified object; `getGeoscale()` returns a
[`geoscales::Geoscale`](https://optimal2050.github.io/geoscales/r/reference/Geoscale.html)
or `NULL`.

## Details

Attaching one is inert by itself: it widens the model's region set with
the coarser codes and enables level-aware plotting, reporting and
subsetting (`getData(geoframe = )`), but the extra members stay unused
and the model solves exactly as before. It is also the prerequisite for
`newCommodity(geoframe = )`, which balances a commodity at a coarser
level and does change the optimisation model – see
`vignette("space-resolution")`. The geoframes must NEST: adjacent levels
have to refine one another, or a region would be aggregated into two
parents.

`setGeoscale()` stores it on a `config` (and therefore on a `settings`,
which inherits from `config`) or on a `model`. `getGeoscale()` reads it
back; for a `scenario` it reads `@settings` first and falls back to the
model's `@config`, matching how `@calendar` is resolved.

## See also

Other geoscale:
[`check_geoscale_regions()`](https://energyRt.org/reference/check_geoscale_regions.md),
[`plot_geoscale()`](https://energyRt.org/reference/plot_geoscale.md),
[`plot_map()`](https://energyRt.org/reference/plot_map.md),
[`utopia_geoscale()`](https://energyRt.org/reference/utopia_geoscale.md)

## Examples

``` r
if (FALSE) { # \dontrun{
gs <- geoscales::geoscale_from_leaftable(
  data.frame(zone = c("N", "N", "S"), region = c("R1", "R2", "R3")),
  geoframes = c("zone", "region")
)
mod <- newModel("demo", region = c("R1", "R2", "R3"), geoscale = gs)
getGeoscale(mod)
} # }
```
