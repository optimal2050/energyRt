# Group mapping specs by recipe, in evaluation order

Group mapping specs by recipe, in evaluation order

## Usage

``` r
mappings_by_recipe(spec = load_mapping_spec())
```

## Arguments

- spec:

  mapping specification (defaults to
  [`load_mapping_spec()`](https://energyRt.org/reference/load_mapping_spec.md)).

## Value

named list keyed by recipe, each a character vector of mapping names,
ordered per `.mapping_recipe_order`.
