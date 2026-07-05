# Find where value(s) are stored across a model's objects

Reflectively walks every slot of every object in a `model` / `scenario`
/ `repository` (or a single S4 model object) and reports where the given
value(s) appear: the object, its class, the slot, and – for `data.frame`
slots – the column. Handy for tracking down a stray label, e.g. an
undeclared region (`"ES_off"`) or a mistyped commodity.

## Usage

``` r
find_in_model(x, pattern, fixed = TRUE, slots = NULL, classes = NULL)
```

## Arguments

- x:

  a `model`, `scenario`, `repository`, or a single model object (S4).

- pattern:

  character vector of value(s) to look for.

- fixed:

  if `TRUE` (default) match exactly; if `FALSE`, treat `pattern` as a
  regular expression (matched with
  [`grepl()`](https://rdrr.io/r/base/grep.html), alternated over the
  vector).

- slots:

  optional character vector restricting which slot names to search.

- classes:

  optional character vector restricting which object classes to search
  (e.g. `"technology"`).

## Value

a `data.frame` with columns `object`, `class`, `slot`, `column`,
`value`, `n` – one row per (object, slot, column, matched value);
`column` is `NA` for atomic slots and `n` counts the matching
elements/rows. Empty (0-row) data.frame when nothing matches.

## See also

Other model:
[`get_region()`](https://energyRt.org/reference/get_region.md)

## Examples

``` r
if (FALSE) { # \dontrun{
find_in_model(mod, c("ES_off", "PT_off"))   # locate stray regions
find_in_model(scen, "BIO", fixed = FALSE)    # regex over every object
find_in_model(mod, "ES_off", classes = "technology")
} # }
```
