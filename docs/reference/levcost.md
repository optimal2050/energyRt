# Levelized cost of commodity production

Computes the levelized cost of energy (LCOE) for a `technology`,
`repository`, `model`, or `scenario` object.

## Usage

``` r
levcost(object, comm, name, ...)

# S4 method for class 'repository'
levcost(object, comm, name, ...)

# S4 method for class 'model'
levcost(object, comm, name, ...)

# S4 method for class 'scenario'
levcost(object, comm, name, ...)
```

## Arguments

- object:

  A `technology` (or list thereof), `repository`, `model`, or solved
  `scenario` object.

- comm:

  Character vector or `NULL`. Output commodity(ies) to use for LCOE
  normalisation. `NULL` uses all commodities in the resolved output
  group.

- name:

  Character. For `technology`, a name tag; for
  `repository`/`model`/`scenario`, the name of the technology / process
  to price.

- ...:

  Additional arguments passed to the underlying implementation. For
  `technology` objects the most useful are:

  `group`

  :   Character or `NULL`. Output group name.

  `repo`

  :   A `repository` or list of energyRt objects (supplies, commodities,
      weather) to supplement the mini-model.

  `fuel_costs`

  :   Named numeric vector `[commodity -> cost]` for input commodities
      not found in `repo`.

  `autocomplete`

  :   Logical, default `FALSE` (`repository`/`model` methods). When
      `TRUE`, input commodities without a supply in the container are
      auto-supplied (zero-cost, or priced via `fuel_costs`) instead of
      returning `NULL`.

  `discount`

  :   Numeric (0–1), default `0.05`.

  `base_year`

  :   Integer or `NULL`.

  `horizon`

  :   A `horizon` object, numeric year vector, or `NULL` (derives from
      `@olife`).

  `calendar`

  :   A `calendar` object or `NULL`.

  `timeframe`

  :   `"ANNUAL"` (default) or `"native"`. `"ANNUAL"` prices the
      technology on a single annual time-slice: any weather profile is
      collapsed to an annual capacity factor (applied as the
      technology's annual availability), so capacity is sized to serve
      unit annual demand at that factor (textbook LCOE). `"native"`
      keeps the supplied (sub-annual) calendar and normalises by total
      generation – useful when the technology is analysed together with
      storage or transmission, where sub-annual dispatch matters.

  `backstop`

  :   Logical, default `TRUE`. Enables a very expensive dummy-import
      slack on the output commodity balance so the mini-model always
      solves even when the technology cannot serve a slice on its own;
      the slack cost is excluded from the LCOE.

  `region`

  :   Character or `NULL`.

  `weather`

  :   A `weather` object, list of weather objects, or `NULL`.

  `frontier`

  :   Logical, default `FALSE`. When `TRUE` additional solves are
      performed to map the production frontier for technologies with
      multi-commodity grouped output and share constraints.

  `solver`

  :   Solver spec list, default `solver_options$glpk`.

  `as_scenario`

  :   Logical, default `FALSE`. When `TRUE` the full solved `scenario`
      is returned with LCOE tables attached to `scenario@misc`.

  `verbose`

  :   Logical, default `TRUE`.

## Value

For `technology` input: a list of class `"levcost"` with fields:

- `$levcost`:

  data.frame – total levelized cost by year.

- `$levcost_npv`:

  Named numeric – NPV-weighted average LCOE.

- `$cost_breakdown`:

  data.frame – tidy cost components by year. Components: `eac`, `fixom`,
  `varom`, `supply`, `import`, `export` (negative, a credit).

- `$cost_breakdown_npv`:

  data.frame – NPV-weighted component breakdown.

- `$cost_yearly`:

  data.frame – wide undiscounted cost table with activity, capacity,
  per-commodity outputs and inputs.

- `$levcost_per_act`:

  data.frame or `NULL`.

- `$frontier`:

  data.frame or `NULL` (requires `frontier = TRUE` and multi-commodity
  grouped output).

- `$scenario`:

  The solved `scenario` object.

For a list of technology objects: a named list of class
`"levcost_list"`.

## Details

- `technology`:

  a minimal single-technology energyRt model is built around the
  technology, solved, and the LCOE derived from the resulting cost and
  production variables.

- `repository` / `model`:

  give the technology `name`; the method selects the related commodity
  and supply objects from the container and prices the named technology
  as above. If an input commodity has no supply in the container, it
  returns `NULL` with a message unless `autocomplete = TRUE` (which adds
  a zero-cost / `fuel_costs` supply). The `model` method also takes the
  calendar, region, horizon and discount rate from the model's
  configuration (each overridable).

- `scenario`:

  an *ex-post* cost of the named process in the *solved* scenario: the
  discounted sum of its own costs (annualised investment `vTechEac`,
  `vTechFixom`, `vTechVarom`, plus attributed fuel cost) divided by its
  discounted output. Fuel is attributed from `vTechInp`; a technology
  with a *grouped* input (whose per-commodity consumption is not a
  solution variable) therefore reports no fuel component.

## Examples

``` r
if (FALSE) { # \dontrun{
lc <- levcost(my_tech, discount = 0.07, base_year = 2025)
lc$levcost_npv
lc$cost_breakdown
autoplot(lc)
autoplot(lc, type = "npv")

# List of technologies (each solved independently):
lc_list <- levcost(list(tech1, tech2), discount = 0.07)
autoplot(lc_list, type = "npv")
} # }
```
