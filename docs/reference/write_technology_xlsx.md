# Write `technology` object(s) to an Excel workbook

Serialises one or more technology objects to a `.xlsx` file, one
technology per worksheet. Each slot of the class is written as a
labelled mini-table whose header row matches the columns of the
corresponding data.frame slot (`names(slot(tech, s))`). Scalar slots
(`name`, `desc`, `cap2act`, `region`, `timeframe`, `fullYear`,
`optimizeRetirement`, `misc`) are written into a leading `meta` block.
The workbook can be read back with
[`read_technology_xlsx()`](https://energyRt.org/reference/read_technology_xlsx.md).

## Usage

``` r
write_technology_xlsx(
  x,
  path,
  include_empty = TRUE,
  drop_empty_cols = FALSE,
  header_fill = "#DDEBF7",
  info = TRUE,
  overwrite = TRUE
)
```

## Arguments

- x:

  A `technology` object, a (named) list of `technology` objects, or a
  `repository` (its `technology` members are written).

- path:

  Path to the output `.xlsx` file.

- include_empty:

  Logical; if `TRUE` (default) every data.frame slot is written,
  including empty ones (header only) so the full class structure is
  visible and the file can serve as an editable template. If `FALSE`,
  only slots with at least one row are written.

- drop_empty_cols:

  Logical; if `TRUE`, columns that are entirely empty (all `NA`) within
  a populated slot's mini-table are dropped, so only columns that carry
  data are written. Empty slots (no rows) keep their full header.
  Default `FALSE`. Round-trips safely: missing columns are restored from
  the class prototype on read.

- header_fill:

  Header-row background colour (hex string, e.g. `"#DDEBF7"`). Set to
  `NA` or `NULL` to disable the fill (headers stay bold). Default
  `"#DDEBF7"` (light blue).

- info:

  Logical; if `TRUE` (default) a leading `Info` worksheet is added
  documenting every slot and column of the `technology` class (names,
  types and descriptions, taken from the class documentation).
  [`read_technology_xlsx()`](https://energyRt.org/reference/read_technology_xlsx.md)
  ignores this sheet.

- overwrite:

  Logical; overwrite `path` if it exists (default `TRUE`).

## Value

(Invisibly) `path`.

## See also

[`read_technology_xlsx()`](https://energyRt.org/reference/read_technology_xlsx.md)

Other technology process:
[`newTechnology()`](https://energyRt.org/reference/technology.md),
[`read_technology_xlsx()`](https://energyRt.org/reference/read_technology_xlsx.md)
