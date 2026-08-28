# Levelized cost of commodity production

Computes the levelized cost of energy (LCOE) for a `technology`,
`repository`, `model`, or `scenario` object.

## Usage

``` r
levcost(object, comm, name, ...)

# S4 method for class 'storage'
levcost(object, comm, name, ...)

# S4 method for class 'trade'
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
      not found in `repo`. Auxiliary commodities consumed by the process
      (aux inputs) can be priced the same way; unnamed ones are supplied
      at zero cost.

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
      technology on a single annual time-timeslice: any weather profile
      is collapsed to an annual capacity factor (applied as the
      technology's annual availability), so capacity is sized to serve
      unit annual demand at that factor (textbook LCOE). `"native"`
      keeps the supplied (sub-annual) calendar and normalises by total
      generation – useful when the technology is analysed together with
      storage or transmission, where sub-annual dispatch matters.

  `backstop`

  :   Logical, default `TRUE`. Enables a very expensive dummy-import
      slack on the output commodity balance so the mini-model always
      solves even when the technology cannot serve a timeslice on its
      own; the slack cost is excluded from the LCOE.

  `region`

  :   Character or `NULL`.

  `weather`

  :   A `weather` object, list of weather objects, or `NULL`.

  `frontier`

  :   Logical, default `TRUE`. When `TRUE` additional solves are
      performed to map the production frontier for technologies with
      multi-commodity grouped output and share constraints (one solve
      per output / input corner solution). Has no effect for
      single-output or unconstrained technologies (the frontier is not
      meaningful, so only the base solve runs). Set `FALSE` to skip the
      extra solves.

  `solver`

  :   Solver spec list, default `solver_options$glpk`.

  `method`

  :   `"auto"` (default), `"analytic"`, or `"solve"`. The unit-demand
      annual mini-model has a closed-form optimum for most technologies;
      `"auto"` computes it analytically (no solver needed) whenever the
      technology qualifies and falls back to the solver otherwise, with
      a message naming the reason. `"analytic"` refuses non-qualifying
      technologies instead of falling back; `"solve"` always builds and
      solves the mini-model. The analytic result carries the same fields
      (plus `$method = "analytic"` and `$frontier_vertices`, which
      reports EVERY corner of the input/output share polytopes with its
      per-activity NPV cost breakdown and an `optimal` flag);
      `$scenario` is `NULL` on this path. Not analytically representable
      (solver required): technology chains, `timeframe = "native"`,
      `afc.*` bounds, availability lower bounds, `optimizeRetirement`,
      year-varying `invcost`, reserve- or availability-constrained
      supplies, and group substitution combined with year-varying prices
      or efficiencies.

  `run`

  :   `"single"` (default) or `"sequential"` – only meaningful for a
      technology declaring vintages or clusters. `"single"` prices every
      variant in one model (each in its own region, so they cannot
      compete); if that model does not solve it is retried with the
      dummy-import slack enabled and then, failing that, falls back to
      one model per variant. `"sequential"` goes straight to one model
      per variant, so a single non-converging variant cannot take the
      rest down.

  `max_failures`

  :   Integer, default `10`. Abort the sequential fallback after this
      many consecutive failed variant solves, to bound the time spent on
      a technology whose variants all fail.

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

For a technology declaring **vintages** and/or **clusters**: a named
list of class `"levcost_variants"` (which inherits `"levcost_list"`),
holding one full `"levcost"` result per variant, keyed by variant name
(`"PWR_VIN2030"`). Each variant is priced in its own region so the
variants cannot serve each other's demand; without that isolation they
compete for one unit of demand and the result silently sums across them.
The stacked tables are reachable with
[`levcost_by_variant()`](https://energyRt.org/reference/levcost_by_variant.md),
and `autoplot()` compares the variants. `$frontier` is `NULL` on this
path – the frontier corners are a per-commodity sweep, orthogonal to
variants, and are not fanned out. A technology with no vintages or
clusters is unaffected and still returns a single `"levcost"` object.

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
  discounted sum of its own costs (annualised investment, fixed and
  variable O&M, plus attributed fuel cost) divided by its discounted
  output. The process may be a `technology`, `storage` or `trade`; each
  reads its own solved variables (`vTechOut`/`vStorageOut`/`vTradeIr`
  and the matching `*Eac`/`*Fixom`/`*Varom`). For a `trade` the
  denominator is what ARRIVES, i.e. `vTradeIr` times the route
  efficiency.

On the `scenario` path, fuel is attributed from the process's own input
variable and priced from the `supply` objects in the model. A commodity
with no `supply` – anything the model produces itself, which is the
usual case for a store's charging electricity – is charged at ZERO here,
because what it is worth is the commodity balance's dual and not a slot.
The scenario path therefore reports a store's OWN cost per unit
discharged, not its delivered cost; use `levcost()` on the `storage`
object with `price =` for the latter. Likewise a technology with a
*grouped* input (whose per-commodity consumption is not a solution
variable) reports no fuel component.

## See also

[`levcost_by_variant()`](https://energyRt.org/reference/levcost_by_variant.md)

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
