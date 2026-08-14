# Export a technology object as a techspec list

The reverse of
[`tech_from_spec()`](https://energyRt.org/reference/tech_from_spec.md):
slot tables become row lists (all-`NA` columns and empty tables are
omitted; `NA` dimension values are omitted from rows, i.e. read back as
"applies to ALL").

## Usage

``` r
tech_to_spec(tech, file = NULL)
```

## Arguments

- tech:

  A `technology` object.

- file:

  Optional path; when given, the spec is written as YAML, or as JSON
  when the path ends in `.json` (same structure, an alternative
  container).

## Value

The spec list (invisibly when `file` is given).
