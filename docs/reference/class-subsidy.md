# An S4 class to represent a commodity subsidy

Subsidies are used to represent the financial support provided to
production, consumption, or balance of a commodity.

## Slots

- `name`:

  character. Name of the subsidy object. It is the instrument's own
  identity and is reported against, so two subsidy objects never merge.

- `desc`:

  character. Description of the subsidy object.

- `comm`:

  character. Commodity filter: the commodity or commodities the levy
  applies to. Empty means every commodity the chosen base reaches.

- `geoframe`:

  character. Optional. The geoscale level this object's regions are
  expressed at; unset (the default) derives it from the region codes.
  See `technology@geoframe` for the full rule, including the consistency
  checks it turns on. A levy neither produces nor consumes a commodity,
  so the declaration is a region-membership check only.

- `region`:

  character. Region filter. Empty means every region; otherwise the levy
  is restricted to the named regions.

- `process`:

  character. Process filter, by name. Matched against both the declared
  (base) name and the realized variant name, so naming a vintaged or
  clustered process selects all of its variants. A name that matches
  nothing is an error, never a silent zero.

- `process_`:

  character. Process filter, as a regular expression over process names
  (the `_` suffix convention of
  [`getData()`](https://energyRt.org/reference/getData.md)). Matched
  against the base name as well as the realized one. A pattern that
  matches nothing is an error.

- `classes`:

  character. Process class filter, e.g. "technology" or "storage".
  Restricts the levy to processes of these classes. Spelled `classes` to
  match the `get_process_*()` family; `class` is a reserved S4 slot
  name.

- `vintage`:

  character. Vintage filter for vintaged processes. Resolved by joining
  the variant provenance table, NOT by parsing the mangled variant name
  – the name separator is configurable, so a pattern over names is not
  stable.

- `vintage_`:

  character. Vintage filter as a regular expression.

- `cluster`:

  character. Cluster filter for clustered processes, resolved from the
  variant provenance table in the same way as `vintage`.

- `cluster_`:

  character. Cluster filter as a regular expression.

- `variable`:

  character. Escape hatch: the name of a model variable to levy
  directly, for a target no base shorthand reaches. Mutually exclusive
  with the base rate columns; the rate then goes in the data slot's
  `rate` column.

- `defVal`:

  numeric. Default value of the subsidy for not specified sets, 0 if not
  specified.

- `subsidy`:

  data.frame. Rate table. The key columns filter, the rate columns say
  what is levied; a row carries one or more rates.

  region

  : character. Region name to apply the parameter, NA for every region.

  year

  : integer. Year to apply the parameter, NA for every year.

  timeslice

  : character. Time timeslice to apply the parameter, NA for every
    timeslice.

  process

  : character. Process name to apply the parameter to, NA for every
    selected process.

  comm

  : character. Commodity to apply the parameter to, NA for every
    selected commodity.

  inp

  : numeric. Per unit of commodity consumed. Without a process filter
    this is the commodity total over every process; with one it is that
    process's own input.

  out

  : numeric. Per unit of commodity produced. Without a process filter
    this is the commodity total over every process; with one it is that
    process's own output.

  bal

  : numeric. Per unit of commodity balance (production - consumption).
    Commodity level only: the balance belongs to no single process, so
    `bal` cannot be combined with a process filter.

  act

  : numeric. Per unit of process activity.

  cap

  : numeric. Per unit of installed capacity, charged in every year the
    capacity stands.

  ncap

  : numeric. Per unit of capacity added in a period, charged once at
    commissioning rather than annualised.

  rate

  : numeric. Per unit of the variable named in `variable`. Used only
    with the escape hatch.

- `misc`:

  list. Any additional information or data to store in the subsidy
  object.

## See also

Other class constraint policy:
[`class-constraint`](https://energyRt.org/reference/class-constraint.md),
[`class-costs`](https://energyRt.org/reference/class-costs.md),
[`newConstraint()`](https://energyRt.org/reference/newConstraint.md),
[`newCosts()`](https://energyRt.org/reference/newCosts.md),
[`newSubsidy()`](https://energyRt.org/reference/newSubsidy.md),
[`newTax()`](https://energyRt.org/reference/newTax.md),
[`tax-class`](https://energyRt.org/reference/class-tax.md)
