# Create new demand object

Create new demand object

Update data in a demand object

## Usage

``` r
newDemand(
  name = "",
  desc = character(),
  commodity = character(),
  unit = character(),
  demand = data.frame(),
  cluster = data.frame(),
  region = character(),
  misc = list(),
  ...
)

# S4 method for class 'demand'
update(object, ...)
```

## Arguments

- name:

  character. Name of the demand.

- desc:

  character. Optional description of the demand for reference.

- commodity:

  character. Name of the commodity for which the demand will be
  specified.

- unit:

  character. Optional unit of the commodity.

- demand:

  data.frame. Specification of the demand.

  region

  :   character. Name of region for the demand value. NA for every
      region.

  year

  :   integer. Year of the demand. NA for every year.

  timeslice

  :   character. Name of the timeslice for the demand value. NA for
      every timeslice.

  demand

  :   numeric. Value of the demand.

- cluster:

  data.frame. Declaration of the demand's parallel parts. For demand a
  cluster is a SUB-REGIONAL SHARE: the object carries one total,
  declared at a coarse region, and each cluster takes a fixed fraction
  of it in one finer region. Variant expansion mints one `dem` set
  member per cluster and `pDemand` is indexed by `dem`, so `eqDemInp`
  sums the parts back – the coarse figure is the AGGREGATE of its
  children and stays the number you edit. LIMITATION – only
  `dem.share.fx`, deliberately. A `lo`/`up` split would need the LP to
  CHOOSE where load sits inside the region, and nothing indexed by `dem`
  is a variable for such a bound to act on: `pDemand` sits on the
  right-hand side of an equality. There is no `cap.share.fx` either (a
  demand has no capacity) and no `act.share.*` (same reason – no
  variable per demand object). Shares are fractions of THIS object's
  total and must sum to 1, so one demand object is one coarse region.
  Several zones are several demand objects, the same way one
  [`lossTranches()`](https://energyRt.org/reference/lossTranches.md)
  object is one line. Note the commodity's own `@geoframe` is a
  different question: a commodity balanced at the coarse level cannot
  read region-level demand at all, and declaring both is refused. Split
  the demand when the commodity stays at the finer level.

  cluster

  :   character. Cluster label, referenced by the `cluster` column of
      `@demand`.

  desc

  :   character. Optional description of the share.

  region

  :   character. The finer region this share lands in. Required – it is
      where the share of the demand goes.

  dem.share.fx

  :   numeric. This cluster's fixed fraction of the object's total
      demand. Must be positive, and the shares of one object must sum to
      1.

  order

  :   integer. Optional ordering for reporting. Without it labels sort
      alphabetically and "R10" would precede "R2".

- region:

  character. Optional name of region to narrow the specification of the
  demand in the case of used NAs. Error will be returned if specified
  regions in `@demand` are not mensioned in the `@region` slot (if the
  slot is not empty).

- misc:

  list. Optional list of additional information.

- object:

  demand object

## Value

demand object with given specifications.

## Examples

``` r
DSTEEL <- newDemand(
 name = "DSTEEL",
 desc = "Steel demand",
 commodity = "STEEL",
 unit = "Mt",
 demand = data.frame(
    region = "TOPIA", # NA for every region
    year = c(2020, 2030, 2050),
    timeslice = "ANNUAL",
    demand = c(100, 200, 300)
 ),
 region = "TOPIA", # optional, to narrow the specification of the demand
 )
 class(DSTEEL)
#> [1] "demand"
#> attr(,"package")
#> [1] "energyRt"
 draw(DSTEEL)

```
