# Hourly wind and solar capacity factors, one site, one year

A full year of hourly capacity factors for one solar and one wind
resource at a single site, for examples that need a realistic sub-annual
profile rather than a synthetic one – storage sizing, curtailment and
[`storage_duration()`](https://energyRt.org/reference/storage_duration.md)
in particular.

## Usage

``` r
vre_cf
```

## Format

A data.frame of 8760 rows and three columns:

- slice:

  timeslice, `"d001_h00"` to `"d365_h23"`.

- solar:

  solar capacity factor, 0-1.

- wind:

  wind capacity factor, 0-1.

## Source

Derived from the NASA MERRA-2 reanalysis with the `merra2ools` package.
See `data-raw/vre_cf.R` for the generating script.

## Details

The site is deliberately unlabelled: the data carries no coordinates,
region code or name. It is one plausible pair of contrasting profiles,
not a claim about a particular place. Solar is a fixed-tilt profile,
wind a 100 m hub height; each comes from a single resource cluster.

`slice` is already in the `d365_h24` vocabulary, so the table joins
directly onto `calendars$d365_h24` with no conversion.

## See also

[calendars](https://energyRt.org/reference/calendars.md),
[`storage_duration()`](https://energyRt.org/reference/storage_duration.md)

## Examples

``` r
str(vre_cf)
#> 'data.frame':    8760 obs. of  3 variables:
#>  $ slice: chr  "d001_h00" "d001_h01" "d001_h02" "d001_h03" ...
#>  $ solar: num  0 0 0 0 0 0 0 0 0.002 0.013 ...
#>  $ wind : num  0.995 0.966 0.89 0.84 0.81 ...
# annual mean capacity factor of each resource
colMeans(vre_cf[, c("solar", "wind")])
#>     solar      wind 
#> 0.1256643 0.2488839 
```
