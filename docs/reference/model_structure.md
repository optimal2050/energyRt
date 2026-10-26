# Catalogue of the model vocabulary

Every set, parameter and variable the package can emit, with its
dimensions and the mapping that gates it. This is the reference the
interpolation and the writers are built against, and what
`tests/coverage/parameter-matrix.csv` is checked against.

## Usage

``` r
model_structure
```

## Format

A data.frame of 613 rows and 5 columns:

- name:

  character. The set, parameter or variable name.

- description:

  character. One-line description.

- type:

  character. One of `set` (279), `parameter` (241) or `variable` (93).

- dim:

  character. Comma-separated dimensions, `""` for a plain set.

- map:

  character. The mapping that restricts the domain, `""` if the full
  cross-product applies.

## Source

Built by `data-raw/maps.R`; the `variable` rows' `dim` and `map` are
re-attached by name in `data-raw/DATASET.R`.

## Examples

``` r
table(model_structure$type)
#> 
#> parameter       set  variable 
#>       269       314        93 
head(model_structure[model_structure$type == "variable", ])
#>                      name                                     description
#> vTechInv         vTechInv                      Overnight investment costs
#> vTechEac         vTechEac                     Annualized investment costs
#> vTechRetCost vTechRetCost                          Early retirement costs
#> vTechFixom     vTechFixom                                 Fixed O&M costs
#> vTechVarom     vTechVarom Variable O&M costs (AVarom + CVarom + ActVarom)
#> vSupCost         vSupCost                         Supply costs (weighted)
#>                  type                dim                              map
#> vTechInv     variable tech, region, year     mTechInv(tech, region, year)
#> vTechEac     variable tech, region, year     mTechEac(tech, region, year)
#> vTechRetCost variable tech, region, year mTechRetCost(tech, region, year)
#> vTechFixom   variable tech, region, year   mTechFixom(tech, region, year)
#> vTechVarom   variable tech, region, year   mTechVarom(tech, region, year)
#> vSupCost     variable  sup, region, year     mvSupCost(sup, region, year)
```
