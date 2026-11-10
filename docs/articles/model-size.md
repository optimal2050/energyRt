# Reducing a model's size

Most models worth building are larger than the solver available to them.
The usual response is to make the model smaller, and energyRt offers
eight ways to do it. They are not interchangeable: each removes
something different, and each pays for the saving in a different
currency.

Three questions separate them.

*What is removed* — cells of the calendar, cells of the geoscale,
milestone years, or merely the freedom to decide. *Whether any value is
recomputed* — a reduction that only drops rows leaves every remaining
number exactly as declared, while one that merges cells must combine
values, and combining them is where meaning can be lost. *Whether the
answer is still the full model’s* — two of the eight reductions are
exact, five approximate, and three of the approximations report their
own error.

This article treats each reduction in the same six terms. A companion
article, `vignette("resolution")`, maps the same ground by axis rather
than by cost, and `vignette("time-resolution")` and
`vignette("space-resolution")` cover one axis each in depth.

``` r

library(energyRt)
open_project(tempdir())   # every driver below writes scenarios to a project

topia_elc <- topia$modules$electricity   # twelve regions, five zones
mod <- newModel("TOPIA", data = topia_elc$repo,
                calendar = calendars[["d365_h24_1dpm"]],
                region   = topia_elc$regions,
                horizon  = topia$modules$horizons$BY2025_50_by10,
                discount = 0.05)
mod <- setGeoscale(mod, topia_elc$geoscale)
```

## Time

### Calendar sampling

- **Reduces** the number of timeslices, by drawing whole members of one
  level (days, weeks, seasons) and keeping every finer slice beneath
  them. A year of 8760 hours solved on twelve sampled days carries 288.
- **Recomputes nothing.** A sample is a label-subset of the full
  calendar: timeslice names, levels and shares are the originals. What
  changes is `pTimesliceWeight`, which carries `1 / year_fraction`, so
  twelve days weigh 30.4167 each and the sample annualises exactly.
  Parameter rows outside the sample are dropped, not averaged.
- **Size** falls in proportion to the slices kept, in every
  timeslice-indexed row and column: dispatch, storage levels, flows,
  availability factors.
- **The answer estimates the full year.** Each sample is a replicate,
  not a shard, so sample objectives form a distribution and must never
  be summed — the opposite of solving by region. Sampling smooths
  extremes, so peaking capacity and storage are the quantities that
  suffer first. A sample must retain at least two members at every level
  below the year: dropping a level renames the leaf timeslices, the
  model’s data stops matching, and the result solves to zero without
  complaint.
- **Used for** screening, parameter sweeps, and questions where the
  spread of answers is more informative than one answer.
- **Driven by**
  [`calendar_samples()`](https://energyRt.org/reference/calendar_samples.md),
  which writes the draw as an editable table, and
  [`solve_by_sample()`](https://energyRt.org/reference/solve_by_sample.md),
  which solves each sample as an own-problem variant of one scenario.
  [`sample_summary()`](https://energyRt.org/reference/sample_summary.md)
  reads the distribution back.

``` r

cal <- calendars[["d365_h24"]]

# consecutive blocks tiling the year: the deterministic, full-coverage crawl
sam <- calendar_samples(cal, n = 2, seed = 1)

# or two random draws of two seasons each, without replacement: the draws are
# disjoint, so n x sample_size cannot exceed the level's membership
calendar_samples(cal, level = "SEASON", sample_size = 2, n = 2,
                 method = "random", seed = 42)

smp <- solve_by_sample(mod, name = "BY_SAMPLE", samples = sam,
                       solver = solver_options$glpk)

# the spread of installed capacity across the replicates
sample_summary(smp, "vTechCap", by = "tech")
```

Three traversals are offered. `"sequential"` tiles the year in
consecutive blocks and covers it completely; `"random"` draws disjoint
samples without replacement, so coverage may be partial; `"bootstrap"`
draws with replacement, and a member drawn twice becomes a scaled share
rather than a repeated row, because a calendar holds each timeslice
once.

### Calendar coarsening

- **Reduces** the number of timeslices by moving the model onto a
  coarser calendar — hours into days, days into seasons — rather than by
  selecting from the fine one.
- **Recomputes every timeslice-indexed value.** Each column moves by its
  declared rule: extensive quantities add, intensive ones take a
  weighted mean, and a column with no merged form is reported rather
  than invented. Rules can be overridden per column.
- **Size** falls with the coarse calendar’s cardinality, and unlike
  sampling it falls for the whole year at once rather than for a drawn
  part of it.
- **The answer loses the variation inside each coarse slice**, and no
  reweighting recovers it. One deterministic result rather than a
  distribution: there is no sampling error, because the error *is* the
  averaging. A recast model is its own model, and comparing its
  objective against the fine model’s compares two models rather than
  measuring an error.
- **Used for** a model that must stay cheap permanently, and as the
  cheap intermediate stage of a guided solve.
- **Driven by**
  [`recast_model_calendar()`](https://energyRt.org/reference/recast_model_calendar.md),
  over
  [`timescales::recast_calendar()`](https://optimal2050.github.io/timescales/r/reference/recast_calendar.html).

``` r

annual <- recast_model_calendar(mod, topia$modules$calendars$annual)

# monthly hours, with one column moved by a rule other than its declared one
monthly <- recast_model_calendar(mod, topia$modules$calendars$m12_h24,
                                 rules = c(demand = "sum"))
```

### A given base year

- **Reduces** the year axis. The milestones before the base year are
  history and enter no linear programme at all; the base year is solved
  on its own; the projection optimises what remains.
- **Recomputes nothing.** What the base year decided crosses into the
  projection as `stock` — exogenous, sunk, paying no annualised capital
  charge — leaving the model’s declared `@stock` untouched. Every
  departed year still anchors interpolation, because the interpolation
  grid already includes anchor years outside the solved set.
- **Size** falls by the removed years’ full share of the problem, which
  on a long horizon with history is the largest single reduction
  available.
- **The answer is exact for the projection**, conditional on accepting
  the base-year result. Nothing about the remaining years is
  approximated, which makes this a reduction rather than an
  approximation.
- **Used for** calibrated runs, where the base year is a measurement
  rather than a decision.
- **Driven by** `newHorizon(base_year = )`,
  [`solve_base_year()`](https://energyRt.org/reference/solve_base_year.md)
  and `interpolate_model(base_year_scenario = )`.

``` r

# the horizon `mod` already carries: 2025 given, then 2030/2040/2050 decided
newHorizon(period = 2025:2055, intervals = c(1, 10, 10, 10), base_year = 2025)

by  <- solve_base_year(mod, solver = solver_options$glpk)
prj <- interpolate_model(mod, name = "PROJECTION", base_year_scenario = by)
```

The split is made by slicing the interval table rather than by flagging
a year inside it. Every derived set — the first and next milestone, the
investment and stock windows, the stock path — reads `intervals$mid` or
`sets$year`, so a slice moves them together, while a flag would leave a
set naming a year the problem does not contain.

### Horizon windows

- **Reduces** the number of years in any one solve. The horizon is
  sliced into consecutive windows, each deciding `step` milestones, with
  `overlap` further milestones appended as look-ahead whose decisions
  are discarded.
- **Recomputes nothing.** Each window re-interpolates the pristine model
  with the accumulated ledger applied, so no window inherits another’s
  rounding.
- **Size** falls to roughly one window’s share, and the sequence is many
  small problems in place of one large one.
- **The answer loses perfect foresight** and is sub-optimal by
  construction. Its objective is not comparable with a full-horizon one:
  capacity carried as stock pays no annualised charge, so comparing the
  two numbers measures an accounting difference as much as a foresight
  penalty.
- **Used for** horizons that do not fit whole, and for representing
  agents who genuinely cannot see ahead.
- **Driven by**
  [`horizon_windows()`](https://energyRt.org/reference/horizon_windows.md)
  and
  [`solve_recursive()`](https://energyRt.org/reference/solve_recursive.md).

``` r

# what the sequence will decide, before committing to it
vapply(horizon_windows(getHorizon(mod), step = 2L, overlap = 1L),
       function(w) w$horizon@name, character(1))

seq_res <- solve_recursive(mod, step = 2L, overlap = 1L,
                           solver = solver_options$glpk)
```

## Space

### Region subsetting

- **Reduces** the region set to a sub-territory, keeping the model’s own
  resolution in what remains.
- **Recomputes nothing.** Rows outside the kept regions are dropped. A
  trade route with one endpoint outside the set becomes a boundary stub
  — an import or an export at the price the severed neighbour would have
  traded at — or, with no price, is dropped, which makes the sub-model
  an island and generally understates its options.
- **Size** falls with the share of regions kept, in every region-indexed
  row and column.
- **The answer is exact when nothing crosses the boundary.** Regional
  quantities are extensive, so sub-problems solved without trade have
  objectives that sum to the full model’s. Once a boundary price is
  supplied, each piece faces an exterior the others cannot see and the
  objectives are no longer additive; the result records which case
  applied. A cost declared at a coarser level does not scale with a
  subset, and is warned about before the surgery rather than after.
- **Used for** single-country studies inside a continental dataset, and
  as the shard of a spatial decomposition.
- **Driven by**
  [`subset_model_regions()`](https://energyRt.org/reference/subset_model_regions.md),
  by
  [`interpolate_model()`](https://energyRt.org/reference/interpolate_model.md)
  when handed a filtered geoscale, and by
  [`solve_by_region()`](https://energyRt.org/reference/solve_by_region.md)
  with its three boundary modes.
  [`boundary_window()`](https://energyRt.org/reference/boundary_window.md)
  sizes the stub from the region’s own demand.

``` r

west <- subset_model_regions(mod, region = c("R02", "R03"))   # zone Z2

# the same cut, but facing an exterior rather than an edge: a stepped curve
# around a supplied price, sized from the severed routes' own demand
bw   <- boundary_window(mod, regions = c("R02", "R03"), price = 60, nsteps = 3)
west <- subset_model_regions(mod, region = c("R02", "R03"), boundary_prices = bw)

# or let the driver cut, solve and store each region in turn
by_reg <- solve_by_region(mod, trade = "none", solver = solver_options$glpk)
```

[`boundary_window()`](https://energyRt.org/reference/boundary_window.md)
derives the *quantity* a stub may carry and will not invent its price: a
price must be supplied, because the number that belongs there is the
neighbour’s marginal value of the commodity, which only a solved model
knows.

### Region aggregation

- **Reduces** the region set by rolling the model up to a coarser
  geoframe — sites into zones, zones into countries.
- **Recomputes every region-indexed value.** Extensive quantities add,
  intensive ones take a mean weighted by the size of each contributing
  region, and corridors merge only when they agree on commodity and
  cluster structure. A value with no merged form — a lifetime, a levy
  rate — stops the call and names the object, the column and the regions
  that disagree, unless `resolve_conflicts` is set to take their mean or
  to leave the value unset.
- **Size** falls with the coarse region count, and with it the
  intra-zone network: routes inside a merged region cease to exist.
- **The answer bounds the fine one in neither direction.** With uniform
  efficiency across the merged regions the coarse objective is lower,
  because merging removed transport limits; with heterogeneous
  efficiency it is higher, because averaging destroyed the ability to
  dispatch the best unit first. What is lost is the difference between a
  zone’s cheap sites and its dear ones, and a model that cannot see that
  difference builds the average everywhere.
- **Used for** continental models in which only aggregate behaviour is
  asked of most regions.
- **Driven by**
  [`aggregate_model_regions()`](https://energyRt.org/reference/aggregate_model_regions.md),
  which returns a model declared over the coarser regions and carrying
  the pruned geoscale.

``` r

repo <- newRepository(
  "mini",
  newCommodity("COA", unit = "MMBtu"),
  newCommodity("ELC", unit = "MWh", timeframe = "ANNUAL"),
  newSupply("SUP_COA", commodity = "COA", supply = data.frame(cost = 1)),
  newTechnology("ECOA", input = list(comm = "COA"), output = list(comm = "ELC"),
                cap2act = 1, invcost = list(invcost = 100),
                olife = list(olife = 30)),
  newDemand("DEM", commodity = "ELC",
            demand = data.frame(region = c("R02", "R03"), year = 2020,
                                demand = 10))
)
mini <- newModel("MINI", data = repo, calendar = calendars$annual,
                 region = c("R02", "R03"), horizon = horizons$Y2020_2060_by_5)

# the two regions roll up into the zone they share
zones <- aggregate_model_regions(mini, geoscale = topia$geoscales$squares,
                                 level = "zone")
get_region(zones)
```

### Clusters within an aggregation

- **Reduces** less, deliberately. The fine regions inside each coarse
  region are grouped into `k` connected clusters, carried as parallel
  sub-processes with their own capacity, availability and costs.
- **Recomputes within each cluster** instead of within each coarse
  region.
- **Size** sits between the two extremes and is tunable: `k` runs from
  one cluster per coarse region — exactly plain aggregation — up to one
  per fine region, which averages nothing.
- **The answer recovers the part of the spread that matters.** At the
  upper end of `k`, a model whose fine regions already pool inside each
  coarse region reproduces the fine objective exactly. Settings are per
  technology family, because one `k` for a whole model is meaningless: a
  coal fleet groups on cost and efficiency, a wind fleet on resource
  quality and profile shape. Clusters never straddle a coarse region,
  and a family that already has clusters keeps them — the two groupings
  compose.
- **Used for** keeping resource heterogeneity in an otherwise coarse
  model, which is the usual reason an aggregated renewables model
  misbehaves.
- **Driven by** `aggregate_model_regions(clusters = )`, with
  [`get_process_groups()`](https://energyRt.org/reference/get_process_groups.md)
  to see what can be merged and
  [`process_cluster_sweep()`](https://energyRt.org/reference/process_cluster_sweep.md)
  to see where the structure is. The grouping is a scale-construction
  method supplied by `nestedscales`, and the number of clusters is
  reported rather than chosen: `k = "auto"` picks the best average
  silhouette, which is a rule and not an answer.

``` r

get_process_groups(mod)        # what could be merged, and over which regions

# per family, never one k for the model:
# aggregate_model_regions(mod, level = "zone",
#                         clusters = list(ECOA = list(k = 3,
#                                                     features = c("invcost",
#                                                                  "eff")),
#                                         EWIN = 8))
```

A full worked example, with objectives at every `k` from plain
aggregation to the fine model, is in `vignette("region-aggregation")`.

## Fixing decisions rather than removing cells

The reductions above make a model smaller. The two below leave its
dimensions untouched and remove freedom instead, and they are what turn
the reductions into *methods*: a cheap solve answers a question, and its
answer constrains the expensive one.

### Carried stock

- **Reduces** the free investment decisions. Capacity decided by an
  earlier solve re-enters a later one as exogenous `stock` at its true
  vintage.
- **Recomputes nothing**; the model’s own declared `@stock` is left
  alone and the carried fleet is added at the vintage it was built.
- **Size**: the same rows, materially fewer free columns, and a much
  easier problem.
- **The answer treats the carried fleet as sunk** — it pays no
  annualised capital charge. That is what a past decision should cost,
  and it is also why objectives along such a sequence are not comparable
  with a single full-horizon objective.
- **Used by** sequential solving, where each window inherits what its
  predecessors decided.
- **Driven by**
  [`solution_ledger()`](https://energyRt.org/reference/solution_ledger.md),
  which reads a solved scenario into a ledger, and internally by
  [`solve_recursive()`](https://energyRt.org/reference/solve_recursive.md)
  and
  [`solve_guided()`](https://energyRt.org/reference/solve_guided.md),
  which apply it. Carrying stock is a property of those drivers rather
  than a step to be performed by hand.

### Capacity targets

- **Reduces** the search space, by turning a cheap solve’s answer into
  bounds on an expensive one: `cap.lo`, or fixed, with optional slack.
- **Recomputes nothing.** Targets are written into the model’s own bound
  columns and re-interpolated; no solver variable is fixed behind the
  model’s back.
- **Size**: unchanged in rows and columns, far smaller in search.
- **The answer still pays for everything it builds**, so the guided
  solve remains an ordinary one and its objective is comparable with a
  true perfect-foresight objective. The comparison has a known sign —
  guided is never better than perfect foresight — and the gap is the
  price of the guidance.
- **Used for** full-horizon problems that are intractable head-on but
  tractable once told roughly where to arrive.
- **Driven by**
  [`solution_targets()`](https://energyRt.org/reference/solution_targets.md)
  and
  [`apply_targets()`](https://energyRt.org/reference/apply_targets.md),
  and by
  [`solve_guided()`](https://energyRt.org/reference/solve_guided.md),
  which sequences the cheap solves: an optional base year, an end state,
  an optional cheap transition on a reduced calendar, each interior
  milestone refined against the end state, and finally one full-horizon
  solve carrying the accumulated bounds.
  [`guided_gap()`](https://energyRt.org/reference/guided_gap.md) reports
  the price.

``` r

gdd <- solve_guided(mod, stages = c("endpoint", "pairs", "final"),
                    slack = 0.2, mode = "lo",
                    calendar_guide = topia$modules$calendars$annual,
                    solver = solver_options$glpk)

# What the guidance cost. Only measurable where the full problem is solvable
# both ways, which is how the method is validated before it is relied on.
scen_pf <- mod |>
  interpolate_model(name = "PF") |>
  solve_scenario(solver = solver_options$glpk)

guided_gap(gdd, pf = scen_pf)
```

What distinguishes this from sequential solving is that every subproblem
still sees where it has to arrive. Targets build and pay; carried stock
is sunk and does not — the distinction is why the two are separate
primitives.

## Reductions that change nothing

`interpolate_model(fold = , sparse = , prune = )`, the rule-defined
mapping sets and
[`trim_parameters()`](https://energyRt.org/reference/trim_parameters.md)
make a model cheaper to build, store and solve without removing a single
modelling cell. The answer is identical by construction;
`options(energyRt.map_rules_verify = TRUE)` checks each rule against the
table it replaces. These are representation choices, not resolution
choices.

``` r

scen <- interpolate_model(mod, name = "COMPACT", fold = TRUE, sparse = TRUE)
model_size(scen)
```

[`model_size()`](https://energyRt.org/reference/model_size.md) is also
the way to find out what any of the earlier reductions actually
achieved, rather than what it was expected to achieve.

## Choosing

| The question being asked | The reduction that fits |
|----|----|
| Is the base year measured rather than decided? | a given base year — exact, and usually the largest single win |
| Does the question concern a few regions? | region subsetting — exact when nothing crosses the boundary |
| Does the answer hinge on intra-annual detail? | coarsen the calendar, or sample it and report the spread |
| Does it hinge on resource quality? | aggregate with clusters, never plainly |
| Is the full horizon simply too large? | guided solving, whose gap is measurable; sequential only where foresight is genuinely absent |
| Is the model merely slow to build or store? | `fold`, `sparse`, `prune`, [`trim_parameters()`](https://energyRt.org/reference/trim_parameters.md) |

Two reductions are exact: a given base year, and a region subset whose
boundary is severed. The rest approximate, and three of them say by how
much:
[`sample_summary()`](https://energyRt.org/reference/sample_summary.md)
for the sampling spread,
[`guided_gap()`](https://energyRt.org/reference/guided_gap.md) for the
guidance penalty, and each object’s `@misc$aggregation` record for what
was merged, forced or dropped on the way up.

The approximations also compose, which is how large models are usually
made solvable: a sampled calendar on an aggregated geoscale, solved
region by region, is four of these features at once. Composing them
multiplies the saving and compounds the error, and nothing in the
framework warns about the combination — only about each part.

## What is not yet possible

Three things a reader may reasonably expect are not implemented, and are
named here so the boundary is explicit rather than implied.

**Uneven resolution across space.** Resolution is a property of the
model, not of the cell: `aggregate_model_regions(level = )` moves the
whole model to one level. A model fine in two regions and coarse in the
remaining forty-eight cannot yet be declared, although the refusals that
stand in the way are all stated and none is fundamental.

**Uneven resolution across time.** One calendar serves every milestone
year. A pathway with 8760 hours in 2030, 2040 and 2050 and a twelve-day
sample in 2035 and 2045 cannot yet be declared.

**A boundary price taken from a solved model.**
[`boundary_window()`](https://energyRt.org/reference/boundary_window.md)
sizes a stub’s quantity but refuses to invent its price, and nothing
derives that price from a neighbour’s solution, because the right number
is a dual of the commodity balance.

And one thing that is refused by design rather than unbuilt: **no
reduction runs backwards**. The balance layer aggregates and never
disaggregates, so no operation takes a solved coarse result and
distributes it over finer regions or slices. Disaggregation means
building a finer model and solving it.

## See also

- `vignette("resolution")` — the same ground mapped by axis, with the
  four operations named and distinguished.
- `vignette("time-resolution")`, `vignette("space-resolution")` — one
  axis each, in depth.
- `vignette("region-aggregation")` — a full spatial example, aggregated
  and clustered, with objectives at every step.
- `vignette("scenario-management")` — where the variants these drivers
  write are stored and how they are read back.
