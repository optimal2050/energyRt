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
getUnits(object, slots = NULL, complete = FALSE, ...)

# S4 method for class 'supply'
getUnits(object, slots = NULL, complete = FALSE, units = NULL, ...)

# S4 method for class 'demand'
getUnits(object, slots = NULL, complete = FALSE, units = NULL, ...)

# S4 method for class 'import'
getUnits(object, slots = NULL, complete = FALSE, units = NULL, ...)

# S4 method for class 'export'
getUnits(object, slots = NULL, complete = FALSE, units = NULL, ...)

# S4 method for class 'storage'
getUnits(object, slots = NULL, complete = FALSE, units = NULL, ...)
```

## Arguments

- object:

  A `commodity` or `technology` S4 object.

- slots:

  Character vector of slot names to include. `NULL` (default) returns
  all populated slots.

- ...:

  Reserved for future use.

- complete:

  (technology only) If `TRUE`, parameters with a known unit formula are
  reported even when they carry no data yet; commodity-linked units are
  then resolved from the declared ports when unambiguous and left as
  placeholder tokens otherwise. Default `FALSE` (populated parameters
  only).

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
