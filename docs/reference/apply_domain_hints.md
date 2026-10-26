# Apply domain hints to a model_structure object

Takes domain hints extracted from GAMS comments and applies them to
variables in a model_structure object.

## Usage

``` r
apply_domain_hints(model, domain_hints, verbose = FALSE)
```

## Arguments

- model:

  A model_structure object from read_gams()

- domain_hints:

  Named list from extract_gams_domain_hints()

- verbose:

  Print progress messages

## Value

Modified model_structure with domain field populated
