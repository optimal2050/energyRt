# Build mapping parameters for a scenario, in recipe (dependency) order

Build mapping parameters for a scenario, in recipe (dependency) order

## Usage

``` r
build_mappings(
  scen,
  fmp = NULL,
  spec = load_mapping_spec(),
  recipes = .mapping_recipe_order,
  maps = NULL
)
```

## Arguments

- scen:

  scenario object with sets already populated from model objects.

- fmp:

  function mapping a parameter name to its on-disk path. When `NULL`,
  parameters are kept in memory.

- spec:

  mapping specification (defaults to
  [`load_mapping_spec()`](https://energyRt.org/reference/load_mapping_spec.md)).

- recipes:

  character vector of recipes to run (defaults to all, in
  `.mapping_recipe_order`).

- maps:

  optional character vector: build only these maps of the recipes.

## Value

updated scenario object.
