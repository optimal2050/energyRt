# Process variants of a solved (or interpolated) scenario

Lists the vintage/cluster variants a scenario was built from, i.e. the
provenance table `scenario@modInp@sets$variant`: which base object each
variant came from, of which class, and which `(vintage, cluster)` cell
it represents.

Variant names are an implementation detail of the solver-facing sets;
use this table (or `getData(..., variants = TRUE)`) to analyse results
by vintage or cluster rather than parsing the names.

A `model` is accepted as well, which answers the same question *before*
interpolating: the names a constraint, levy or report will have to refer
to. The expansion runs on a throwaway copy, so the model passed in is
unchanged.

## Usage

``` r
getVariants(object, class = NULL)
```

## Arguments

- object:

  a `scenario` object, or a `model` to expand on the fly.

- class:

  optional character, keep only variants of these process classes (e.g.
  `"technology"`, `"storage"`).

## Value

A data.frame with columns `name`, `class`, `base`, `vintage`, `cluster`,
or `NULL` when the model has no variants.

## See also

Other technology process:
[`newTechnology()`](https://energyRt.org/reference/technology.md),
[`read_technology_xlsx()`](https://energyRt.org/reference/read_technology_xlsx.md),
[`variantSummary()`](https://energyRt.org/reference/variantSummary.md),
[`write_technology_xlsx()`](https://energyRt.org/reference/write_technology_xlsx.md)

## Examples

``` r
topia_elc <- topia$modules$electricity
mod <- newModel("TOPIA", data = topia_elc$repo,
                calendar = topia$modules$calendars$s4_h24,
                region = topia_elc$regions,
                horizon = topia$modules$horizons$BY2025_50_by10,
                discount = 0.05)
mod <- setGeoscale(mod, topia_elc$geoscale)

# the shipped model is already varied: wind is split into site grades and
# several processes carry vintages. This is the roster of names the
# solver-facing sets will carry.
getVariants(mod)
#>                 name      class      base vintage cluster
#> 1       SUP_COA_CLS1     supply   SUP_COA    <NA>      S1
#> 2       SUP_COA_CLS2     supply   SUP_COA    <NA>      S2
#> 3       SUP_COA_CLS3     supply   SUP_COA    <NA>      S3
#> 4       SUP_GAS_CLS1     supply   SUP_GAS    <NA>      S1
#> 5       SUP_GAS_CLS2     supply   SUP_GAS    <NA>      S2
#> 6       SUP_GAS_CLS3     supply   SUP_GAS    <NA>      S3
#> 7       SUP_BIO_CLS1     supply   SUP_BIO    <NA>      S1
#> 8       SUP_BIO_CLS2     supply   SUP_BIO    <NA>      S2
#> 9       SUP_BIO_CLS3     supply   SUP_BIO    <NA>      S3
#> 10    RES_LANDW_CLHI     supply RES_LANDW    <NA>      HI
#> 11    RES_LANDW_CLMD     supply RES_LANDW    <NA>      MD
#> 12    RES_LANDW_CLLO     supply RES_LANDW    <NA>      LO
#> 13 ESOL_VIN2015_CLFL technology      ESOL    2015      FL
#> 14 ESOL_VIN2030_CLFL technology      ESOL    2030      FL
#> 15 ESOL_VIN2045_CLFL technology      ESOL    2045      FL
#> 16 ESOL_VIN2015_CLTH technology      ESOL    2015      TH
#> 17 ESOL_VIN2030_CLTH technology      ESOL    2030      TH
#> 18 ESOL_VIN2045_CLTH technology      ESOL    2045      TH
#> 19 EWIN_VIN2015_CLHI technology      EWIN    2015      HI
#> 20 EWIN_VIN2030_CLHI technology      EWIN    2030      HI
#> 21 EWIN_VIN2045_CLHI technology      EWIN    2045      HI
#> 22 EWIN_VIN2015_CLMD technology      EWIN    2015      MD
#> 23 EWIN_VIN2030_CLMD technology      EWIN    2030      MD
#> 24 EWIN_VIN2045_CLMD technology      EWIN    2045      MD
#> 25 EWIN_VIN2015_CLLO technology      EWIN    2015      LO
#> 26 EWIN_VIN2030_CLLO technology      EWIN    2030      LO
#> 27 EWIN_VIN2045_CLLO technology      EWIN    2045      LO
#> 28   STG_ELC_VIN2025    storage   STG_ELC    2025    <NA>
#> 29   STG_ELC_VIN2035    storage   STG_ELC    2035    <NA>
#> 30   STG_ELC_VIN2045    storage   STG_ELC    2045    <NA>
#> 31     H2ELC_VIN2030 technology     H2ELC    2030    <NA>
#> 32     H2ELC_VIN2040 technology     H2ELC    2040    <NA>
#> 33     H2ELC_VIN2050 technology     H2ELC    2050    <NA>
getVariants(mod, class = "technology")
#>                 name      class  base vintage cluster
#> 1  ESOL_VIN2015_CLFL technology  ESOL    2015      FL
#> 2  ESOL_VIN2030_CLFL technology  ESOL    2030      FL
#> 3  ESOL_VIN2045_CLFL technology  ESOL    2045      FL
#> 4  ESOL_VIN2015_CLTH technology  ESOL    2015      TH
#> 5  ESOL_VIN2030_CLTH technology  ESOL    2030      TH
#> 6  ESOL_VIN2045_CLTH technology  ESOL    2045      TH
#> 7  EWIN_VIN2015_CLHI technology  EWIN    2015      HI
#> 8  EWIN_VIN2030_CLHI technology  EWIN    2030      HI
#> 9  EWIN_VIN2045_CLHI technology  EWIN    2045      HI
#> 10 EWIN_VIN2015_CLMD technology  EWIN    2015      MD
#> 11 EWIN_VIN2030_CLMD technology  EWIN    2030      MD
#> 12 EWIN_VIN2045_CLMD technology  EWIN    2045      MD
#> 13 EWIN_VIN2015_CLLO technology  EWIN    2015      LO
#> 14 EWIN_VIN2030_CLLO technology  EWIN    2030      LO
#> 15 EWIN_VIN2045_CLLO technology  EWIN    2045      LO
#> 16     H2ELC_VIN2030 technology H2ELC    2030    <NA>
#> 17     H2ELC_VIN2040 technology H2ELC    2040    <NA>
#> 18     H2ELC_VIN2050 technology H2ELC    2050    <NA>

# a process gains variants from its own @cluster / @vintage tables
getObject(topia_elc$repo, name = "EWIN", drop = TRUE)@cluster
#>    cluster                    desc region cap.share.fx act.share.lo
#> 1       HI exposed coast and ridge    R01           NA           NA
#> 2       HI exposed coast and ridge    R09           NA           NA
#> 3       HI exposed coast and ridge    R10           NA           NA
#> 4       HI exposed coast and ridge    R11           NA           NA
#> 5       HI exposed coast and ridge    R12           NA           NA
#> 6       MD            open country    R01           NA           NA
#> 7       MD            open country    R02           NA           NA
#> 8       MD            open country    R11           NA           NA
#> 9       MD            open country    R12           NA           NA
#> 10      LO          inland terrain    R02           NA           NA
#> 11      LO          inland terrain    R03           NA           NA
#> 12      LO          inland terrain    R04           NA           NA
#> 13      LO          inland terrain    R05           NA           NA
#> 14      LO          inland terrain    R06           NA           NA
#> 15      LO          inland terrain    R07           NA           NA
#> 16      LO          inland terrain    R08           NA           NA
#> 17      LO          inland terrain    R09           NA           NA
#>    act.share.up act.share.fx order
#> 1            NA           NA     1
#> 2            NA           NA     1
#> 3            NA           NA     1
#> 4            NA           NA     1
#> 5            NA           NA     1
#> 6            NA           NA     2
#> 7            NA           NA     2
#> 8            NA           NA     2
#> 9            NA           NA     2
#> 10           NA           NA     3
#> 11           NA           NA     3
#> 12           NA           NA     3
#> 13           NA           NA     3
#> 14           NA           NA     3
#> 15           NA           NA     3
#> 16           NA           NA     3
#> 17           NA           NA     3
```
