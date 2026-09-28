# Constructor for supply object.

Creates an instance of the `supply` class and initializes it with the
given data and parameters.

## Usage

``` r
newSupply(
  name = "",
  desc = "",
  commodity = character(),
  unit = character(),
  weather = data.frame(),
  reserve = data.frame(),
  supply = data.frame(),
  cluster = data.frame(),
  region = character(),
  misc = list(),
  ...
)
```

## Arguments

- name:

  character. Name of the supply object, used in sets.

- desc:

  character. Description of the supply object.

- commodity:

  character. The supplied commodity short name.

- unit:

  character. The main unit of the commodity used in the model.

- weather:

  data.frame. Weather factors to apply to the supply.

  cluster

  :   character. Price step this row applies to, NA for every step. See
      the `cluster` slot.

  weather

  :   character. Name of the weather factor to apply. Must match the
      weather factor names in a `weather` class in the model.

  wava.lo

  :   numeric. Coefficient that links the weather factor with the lower
      bound of the availability factor `ava.lo`.

  wava.up

  :   numeric. Coefficient that links the weather factor with the upper
      bound of the availability factor `ava.up`.

  wava.fx

  :   numeric. Coefficient that links the weather factor with the fixed
      value of the availability factor `ava.fx`. This parameter
      overrides `wava.lo` and `wava.up`.

- reserve:

  data.frame. Total available resource. Applicable to exhaustible
  resources. Set for each region. If not set, the resource is considered
  infinite.

  cluster

  :   character. Price step this row applies to, NA for every step. See
      the `cluster` slot.

  region

  :   character. Region name to apply the parameter. Use NA to apply to
      all regions.

  res.lo

  :   numeric. Lower bound of the total available resource.

  res.up

  :   numeric. Upper bound of the total available resource.

  res.fx

  :   numeric. Fixed value of the total available resource. This
      parameter overrides `res.lo` and `res.up`.

- supply:

  data.frame. Availability of the resource in physical units. Unlike the
  `af`/`afs` availability *factors* of technologies, `ava.*` is an
  absolute bound on the supplied quantity per timeslice: supply
  processes have no capacity variable, so nothing multiplies it (except
  weather factors, see `weather`). Rows here also define where and when
  the supply exists in the model.

  cluster

  :   character. Price step this row applies to, NA for every step. See
      the `cluster` slot.

  region

  :   character. Region name to apply the parameter. Use NA to apply to
      all regions.

  year

  :   integer. Year to apply the parameter. Use NA to apply to all
      years.

  timeslice

  :   character. Time timeslice to apply the parameter. Use NA to apply
      to all timeslices.

  ava.lo

  :   numeric. Lower bound on the supplied quantity, in physical
      commodity units per timeslice.

  ava.up

  :   numeric. Upper bound on the supplied quantity, in physical
      commodity units per timeslice.

  ava.fx

  :   numeric. Fixed value of the supplied quantity, in physical
      commodity units per timeslice. This parameter overrides `ava.lo`
      and `ava.up`.

  cost

  :   numeric. Cost of the resource extraction, if not set, the resource
      is considered free.

- cluster:

  data.frame. Declaration of the object's parallel sub-supplies. For
  supply a cluster is a PRICE STEP – a resource grade: its own share of
  the availability and reserve, at its own cost. A single `cost` cannot
  express a supply curve, where cheap grades are exhausted first and the
  next ones cost more. This slot declares WHAT the grades are; the
  per-grade values live in the `cluster` column of `supply`, `reserve`
  and `weather`. Build both with
  [`asSupplyCurve()`](https://energyRt.org/reference/supply-curve.md),
  which also computes the costs and checks that the curve rises.
  Optional. When empty, labels are harvested from the `cluster` columns
  of the other slots. When populated it is AUTHORITATIVE. LIMITATION –
  there is deliberately no `cap.share.fx` column. A capacity share ties
  the variants' capacities to fixed proportions, and this class has no
  capacity variable for the tie to act on. The column is absent rather
  than present-and-ignored, so the mistake is rejected at construction
  instead of silently doing nothing. Bound the split with
  `act.share.lo/up/fx`, which acts on the quantity this class does have,
  or fix it directly in the per-step bounds. The share columns are
  different levers, not substitutes. `cap.share.fx` fixes the ratio of
  the variants' CAPACITIES in every year: loss tranches need it, because
  each tranche efficiency is calibrated to its place in the capacity
  stack and the flow must stay free to fill the cheapest first.
  `act.share.lo/up/fx` bounds a variant share of the family THROUGHPUT
  instead, which is what stops a clustered family collapsing to its
  cheapest member once regional borders are aggregated away. Both are
  optional; a variant with neither sizes and runs freely.

  cluster

  :   character. Grade label, used in the expanded object name.

  desc

  :   character. Description of the grade.

  region

  :   character. Region(s) the grade exists in, NA for everywhere.
      Intersected with the object's own `region` slot.

  share

  :   numeric. The grade's fraction of the object's quantity.
      DESCRIPTIVE, not enforced: supply has no capacity variable to tie,
      so
      [`asSupplyCurve()`](https://energyRt.org/reference/supply-curve.md)
      writes the split straight into `ava.*` / `res.*` and records here
      what it did.

  order

  :   integer. Fill order, 1 = the cheapest grade. Without it labels
      sort alphabetically and "S10" would precede "S2".

- region:

  character. Regions where the supply process exists. Must include all
  regions used in other slots. `availability` and `reserve` slots also
  limit possible regions.

- misc:

  list. List of additional parameters that are not used in the model but
  can be used for reference or user-defined functions. For example,
  links to the source of the supply data, or other metadata.

## Value

supply object with given specifications.

## Details

The `supply` class is used to add a domestic source of a commodity to
the model, with given reserves, availability, and costs.

## Examples

``` r
SUP_COA <- newSupply(
   name = "SUP_COA",
   desc = "Coal supply",
   commodity = "COA",
   unit = "PJ",
   reserve = data.frame(
      region = c("R1", "R2", "R3"),
      res.up = c(2e5, 1e4, 3e6) # total reserves/deposits
   ),
   supply = data.frame(
      region = c("R1", "R2", "R3"),
      year = NA_integer_,
      timeslice = "ANNUAL",
      ava.up = c(1e3, 1e2, 2e2), # annual availability
      cost = c(10, 20, 30) # cost of the resource (currency per unit)
   ),
   region = c("R1", "R2", "R3")
 )
class(SUP_COA)
#> [1] "supply"
#> attr(,"package")
#> [1] "energyRt"
# draw(SUP_COA)
```
