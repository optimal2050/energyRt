# Get parameter units for energyRt class objects

Returns a data.frame describing each parameter in the object along with
its inferred unit. Units are derived from the unit slots stored on the
object (`@unit` for commodity, `@units` for technology) and the
dimensional semantics of each parameter (e.g. investment cost is
`{costs}/{capacity}`).

Unresolved base units are shown as placeholder tokens (`{capacity}`,
`{activity}`, `{costs}`) when the corresponding `@units` slot is not
set.

## Usage

``` r
# S4 method for class 'commodity'
getUnits(object, slots = NULL, ...)

# S4 method for class 'technology'
getUnits(object, slots = NULL, ...)

get_units(object, ...)
```

## Arguments

- object:

  A `commodity` or `technology` S4 object.

- slots:

  Character vector of slot names to include. `NULL` (default) returns
  all populated slots.

- ...:

  Reserved for future use.

## Value

A data.frame with columns `slot`, `parameter`, `comm`, `description`,
and `unit`.

## Examples

``` r
if (FALSE) { # \dontrun{
coal <- newCommodity("COAL", unit = "PJ")
getUnits(coal)

getUnits(PTL_RWGS_FT_SYN2030)
getUnits(PTL_RWGS_FT_SYN2030, slots = "ceff")
} # }
```
