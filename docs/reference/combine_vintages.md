# Combine per-vintage technology objects into one vintaged technology

Merges a list of `technology` objects, one per build-year vintage (e.g.
`FDAC_S_VIN2030`, `FDAC_S_VIN2040`, ...), into a single technology with
a [`@vintage`](https://energyRt.org/reference/technology.md) table and
vintage-keyed parameter rows — the "one object, many vintages" form
introduced with the vintage machinery.

## Usage

``` r
combine_vintages(
  techs,
  vintages = NULL,
  name = NULL,
  desc = NULL,
  close_windows = TRUE,
  collapse_common = TRUE
)
```

## Arguments

- techs:

  A named list of `technology` objects, one per vintage. Objects
  declaring clusters are not supported.

- vintages:

  Optional character vector of vintage labels (same length as `techs`).
  Default: parsed from `names(techs)` (`"..._VIN<label>"`), falling back
  to each object's `@vintage$start`.

- name, desc:

  Name/description of the combined technology. Defaults: the common
  `"..._VIN"` prefix of the input names, and the first object's
  description.

- close_windows:

  Close open-ended investment windows at the next vintage's start
  (default `TRUE`).

- collapse_common:

  Collapse rows identical across all vintages to a single all-vintages
  row (default `TRUE`).

## Value

A `technology` object with one `@vintage` row per input.

## Details

- Ports (`input`/`output`/`aux`/`group`) are the union across vintages
  (a commodity present only in some vintages is fine: its `ceff` rows
  carry the vintage key).

- Structural attributes (`units`, `cap2act`, `timeframe`, `region`,
  `fullYear`) are taken from the first object; differences warn.

- Every parameter row is tagged with its object's vintage; rows that are
  identical across ALL vintages collapse to one all-vintages row
  (disable with `collapse_common = FALSE`).

- Investment windows: each vintage's `start` is kept (defaulting to the
  vintage year); with `close_windows = TRUE` a missing `end` is closed
  at the next vintage's start minus one (the last stays open).

## Examples

``` r
if (FALSE) { # \dontrun{
dac <- make_dea_dac_techs()   # FDAC_S_VIN2020, FDAC_S_VIN2030, ...
fam <- split(dac, sub("_VIN[0-9]+$", "", names(dac)))
dac1 <- lapply(fam, combine_vintages)
draw(dac1$FDAC_S)             # vintage fan
} # }
```
