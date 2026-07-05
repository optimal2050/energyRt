# Read `technology` object(s) from an Excel workbook

Inverse of
[`write_technology_xlsx()`](https://energyRt.org/reference/write_technology_xlsx.md).
Reads the block-structured worksheets produced by that function and
rebuilds technology objects, coercing each mini-table's columns back to
the types declared by the class.

## Usage

``` r
read_technology_xlsx(path, sheet = NULL, as_repository = FALSE)
```

## Arguments

- path:

  Path to the `.xlsx` file.

- sheet:

  Optional sheet name or index. If a single sheet is given, a single
  `technology` object is returned; otherwise all sheets are read.

- as_repository:

  Logical; if `TRUE`, wrap the result in a `repository`.

## Value

A `technology` object (single sheet), a named list of `technology`
objects, or a `repository` (when `as_repository = TRUE`).

## See also

[`write_technology_xlsx()`](https://energyRt.org/reference/write_technology_xlsx.md)

Other technology process:
[`newTechnology()`](https://energyRt.org/reference/technology.md),
[`write_technology_xlsx()`](https://energyRt.org/reference/write_technology_xlsx.md)
