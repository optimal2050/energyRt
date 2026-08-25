# energyRt (development version)

## Breaking changes

* A storage's availability columns are renamed onto the part prefixes the cost
  and capacity slots already use: `@af$cinp.{lo,up,fx}` -> `inp.af.*`,
  `cout.*` -> `out.af.*`, and in `@weather` `wcinp.*` -> `inp.waf.*`,
  `wcout.*` -> `out.waf.*`. The old names are an error naming their
  replacement. Solver parameters and all four back-end templates moved with
  them; results are unchanged.
* `commodity@geolevel` and `summand@geolevel` are now `@geoframe`, as are the
  `geolevel =` arguments of `newCommodity()`, `newSummand()` and `getData()`;
  `map_comm_geolevel()` is now `map_comm_geoframe()`. Supported on all four
  back-ends.
* The variable-catalogue role `flow` is now `interregional`.

## New features

* `solve_myopic()` solves a horizon window by window instead of all at once.
  The primitives are exported and composable: `horizon_windows()`,
  `solution_ledger()`, `model_apply_ledger()`.
* `levcost()` prices `storage` (LCOS, per unit discharged, with a `cycles`
  argument) and `trade` (LCOT, per unit arriving, summed over both endpoints)
  as well as `technology` — closed-form and solver engines, held to each other
  by a parity test. Containers price all three classes; `classes =` narrows.
* `asSupplyCurve()`, `asImportCurve()` and `asExportCurve()` turn a single
  price per `(region, year, timeslice)` into a stepped curve, so cheap resource
  grades are exhausted before dearer ones.
* Transmission losses can be quadratic, approximated by capacity tranches,
  instead of the single fixed `teff` fraction.
* `newACLine()` and `newDCLink()`, with an opt-in Kirchhoff voltage law — an AC
  line is no longer modelled only as a controllable transport route.
* `@trade$af` rates a route's flow *relative to its capacity*, alongside the
  existing absolute `ava.lo/up/fx`.
* A filtered geoscale passed to `interpolate_model()` produces a sub-territory
  model — the spatial mirror of calendar sampling.
* `"TOTAL"` in a `vintage` or `cluster` column now works on flow bounds and
  availability factors, not only on `@capacity`.
* `vTradeIr` and the `*RetiredNewCap` variables can be used in custom
  constraints.
* Scenario storage, in stages: a persisted per-project registry replaces the
  in-memory one; solves get one folder each under `runs/<variant>/<solve>/`
  with provenance; `save_model()` writes a content-addressed model store that
  scenarios reference instead of embedding; `save_repository()` stores a shared
  repository once; and a scenario can hold several own-problem variants side by
  side via `solve_scen(variant = )`.

## Deprecations

* The mosox back-end experiment moved to `drafts/mosox/`; it was not
  functional.

## Bug fixes

* `technology@af$rampup` / `$rampdown` were accepted by the constructor and
  never reached the solver — the parameter catalogue named slots that do not
  exist, so the ramp parameters stayed empty.
* A storage flow into a commodity with a COARSER timeframe never reached the
  balance: `eqStorageInpTot` / `eqStorageOutTot` summed only at identical
  timeslices, so a slice-level storage's aux into an ANNUAL commodity was
  computed but silently FREE (a priced aux cost nothing). The storage totals
  now use the same SameTimeslice/Agg classification the technology totals have
  had all along (12 new `mStorage{Inp,AInp,Out,AOut}Comm*` maps, 4-branch
  equations on all four backends), and the totals domain is reduced to the
  commodity's native timeslice and mCommReg-cut like `.filter_proc_tot`.
  Covered by `test-family-storage-aux-flows.R` incl. Julia/Pyomo parity.
* A one-sided `inp2out` (or `duration`) range no longer resurrects the binding
  default on its open side: `.norm_ratio()` completes one-sided ranges with an
  OPEN `lo = 0` / `up = Inf`, but writers drop `Inf` rows from solver data, so
  the structural equation fell back to the default of 1 —
  `inp2out = data.frame(inp2out.lo = 2)` alone made the model INFEASIBLE
  (charger ≥ 2× discharger against a resurrected charger ≤ 1× discharger).
  The structural ratio maps (`mStorageInp2out*`, `mStorageDuration*`) now skip
  entries whose explicit value is non-binding, so an open side produces no
  equation on any backend. Declared-open semantics unchanged and now tested
  (`test-family-storage-inp2out.R`): a lone `.up` still leaves the charger
  floor open — a priced charger may size to zero.
* A supply restricted to one region leaked into every other region:
  `eqSupOutTot` gated the sum on commodity alone.
* Multi-level regions were inert — the geoscale hierarchy was read under the
  old `parent_level` / `child_level` column names.
* A cost declared at a coarser geoscale level (e.g. a national `invcost`) never
  reached the objective.
* `add()` on a model worked exactly once; the second call failed.
* A scalar `mult` on a custom constraint summand was silently discarded.
* Exogenous stock could phase out and retire at the same time; `capacity$stock`
  is now read as the fleet still standing, not as the original endowment.
* Trade phase-out is now reported rather than only computed, and GAMS declares
  retirement non-negative.
* A scenario folder that was moved now loads: stored paths are rebased onto the
  folder actually being read, and one tool upgrades older layouts.

## Documentation

* One `trade` object is one **shared throughput budget**: `eqTradeCapFlow` sums
  every route on the object against its single capacity, so a network in one
  object is not a set of independently rated lines.
* Trade costs are a **rate per endpoint region** — capacity is region-free, but
  `invcost`, `fixom`, `retcost` and `eac` are region-indexed and each named
  region pays its own rate on the whole capacity.

# energyRt 0.80 (development) — the time dimension is now `timeslice`

## Pyomo-Abstract retired

`pyomo/energyRtAbstract.py` and its output writer have moved to `drafts/`. Asking
for it now raises an error naming a Concrete option instead.

It had fallen behind on three separate refactors and was documented as such in
three places: the **agg-rewrite** (it still declared `mBalanceRY`, retired
everywhere else as dead reporting — which is what made it crash on
construction), the **eac-fix** (flat `pXEac * vXCap` rather than the vintaged
new-capacity form, `NEWS` below), and **vintaging/payback**, which `R/eac.R`
records as unreachable "without a bigger change". Retirement equations would
have been the fourth.

It rotted because nothing ran it: **no shipped solver option ever selected it** —
the branch needs `lang` to contain `"abstract"`, and none of the eight Pyomo
options does. That also made it a sixth template every equation change had to
touch, with no test to notice when the change was wrong.

Abstract earns its keep when one model structure is instantiated over many
datasets in a single process. energyRt writes a fresh model per scenario with
the data already resolved and then exits, so that separation already exists at
the R level. If an AbstractModel is wanted later it should be **generated** from
the Concrete template, the way the mosox template is generated from GLPK.

## `storage`: role slots declare, parameter slots parameterise

**Breaking, with no deprecation path.** A `storage`'s three role slots —
`@input`, `@output`, `@storage` — used to carry capacity and economics as well
as the commodity they name. That mixed two different kinds of thing in one
table: `comm`, `unit` and `cap2act` are invariant, while capacity varies by
vintage, region and year. It also gave the same quantity two homes, because the
output side's capacity already lived in `@capacity`/`@invcost`/`@fixom` and was
folded there at construction.

Now every parameter has exactly one home. The role slots keep `comm`, `unit` and
`cap2act` (`@storage` keeps `comm` and `unit` — the level is energy on both
sides), and every capacity and cost lives in `@capacity`, `@invcost` or `@fixom`
under a prefix naming its part:

| prefix | part | measured in |
|---|---|---|
| `out.` | discharger | power |
| `inp.` | charger | power |
| `stg.` | reservoir | energy |

```r
# before                                  # now
newStorage("BTR",                         newStorage("BTR",
  output  = list(comm = "ELC",              output  = list(comm = "ELC"),
                 invcost = 12144),          storage = list(comm = "ELC"),
  storage = list(comm = "ELC",              invcost = list(out.invcost = 12144,
                 invcost = 8081),                          stg.invcost = 8081),
  capacity = list(cap.up = 100))            capacity = list(out.cap.up = 100))
```

The old spellings are **errors**, not warnings, and the message names the column
to write instead. `setValidity("storage")` enforces the same rule, so a model
serialised before this change fails loudly on `validObject()` rather than
solving with a store that no mapping reads.

Alongside it, the parameters were renamed to agree with the variables, which
already used `Out`: `pStorageCap` → `pStorageOutCap`, `pStorageInvcost` →
`pStorageOutInvcost`, and so on for all ten output-side families.

Group-aggregate (`"TOTAL"`) bounds needed teaching about the prefix. The scan in
`R/variants.R` matched `@capacity` columns by bare name, so on a storage it
matched nothing — which would have silently dropped cluster group bounds *and*
the refusal that tells you a group `ret.*` bound needs `newConstraint()`. The
class definition now carries `bound_prefix`, and a guard that had stopped
guarding is a test (`test-storage-variants.R`) rather than a discovery.

The charger and the reservoir also gained the parameters only the discharger
had — `ret.*`, `wacc`, `payback`, `retcost` — so all three parts are described
the same way. **These are declared and inert**: storage retirement has no
equation in any back-end, for any part, and that was already true before this
change. Giving them meaning is separate work.


## Plot fixes for converted continental models

Both surfaced on a PyPSA-Eur conversion, where one `technology` groups a carrier
across dozens of countries and a `weather` factor carries every region.

-   **`draw()` printed one label per region.** A technology's `@ceff` holds a row
    per region, so `E_CCGT` on a 41-node European model drew **36** `cinp2use`
    labels stacked on one arrow — unreadable, and never the intent. Values now
    collapse through a new internal `format_value_range()`: identical ones show
    as a single number, differing ones as a `min-max` range (`0.50-0.71`).
    Two paths were affected, grouped and non-grouped commodities; the latter also
    grouped by `region`/`year`/`timeslice`, which is what multiplied the rows.
    Fixed alongside it: `paste0(parameter, ":")` received the whole 36-element
    vector rather than the group's single value.

-   **`autoplot.weather()` / `plot_weather()` gained `region`.** Faceting 41
    regions onto one heatmap leaves nothing legible but the strip labels. Pass a
    count (`region = 2`, the first two) or names (`region = "BE0_0"`); `NULL`,
    the default, keeps every region as before. An unknown name warns and is
    dropped rather than failing.

Known, unfixed, and older than these changes: `draw()` errors with
`Can't combine ..1$region <logical> and ..2$region <character>` on a technology
that carries `cap.lo` but no `stock` or `cap.up` — reproduced on `E_CCGT` and
`E_NUCLEAR` in a converted single-region model.

## New data: a year of hourly wind and solar

-   **`vre_cf`** — 8760 hourly capacity factors for one wind and one
    solar resource, derived from the MERRA-2 reanalysis via
    `merra2ools`. The site is deliberately unlabelled: no coordinates,
    region code or name, since the example needs a realistic pair of
    contrasting profiles rather than a claim about a place. Its `slice`
    column is already in the `d365_h24` vocabulary, so it joins straight
    onto `calendars$d365_h24`.
-   **`vre_storage_duration`** — the `storage_duration()` decomposition
    of a battery in a full-year, hourly wind-solar model built on
    `vre_cf`. Duration bands only mean something when the model can hold
    energy for weeks, which needs all 8760 timeslices and a solve of
    minutes on julia/HiGHS, so the result is precomputed
    (`data-raw/vre_storage_duration.R`) and the **Storage** article plots
    it beside the model that produced it. It shows what the coarse
    example cannot: a thin band cycling within the day against most of
    the energy committed for more than thirty days.

## `storage_duration()` returned nothing on month-based calendars

-   `storage_duration()` returned **zero rows, with no message**, for any
    month-based calendar (`m12_h24`, e.g. `calendars$utopia_m12h24`) —
    an empty chart and nothing to explain it. It called `tsl2dtm()`
    without `mday`, which for that format returns `NULL`, so every group
    was silently dropped. It now passes `mday`, and month calendars
    decompose correctly.
-   When the level genuinely cannot be dated, the function now **stops
    with the format it read** instead of returning an empty frame, so an
    unsuitable calendar says so.
-   `tsl2dtm()` died with `object 'dtm' not found` for any format it has
    no branch for — a season x hour calendar (`h24`,
    `calendars$utopia_s4h24`) hit this. It returns `NULL` for those now,
    the same "cannot date this" signal its other exits use.

## Storage: `unit` on the three parts, and a `draw()` fix

-   `storage@input`, `@output` and `@storage` now take a **`unit`**
    column, as `technology@input`/`@output` already did. It is the unit
    of the commodity on that side, carried for reporting and
    `convert()`; it never reaches the solver, so adding one leaves the
    model unchanged. Previously `newStorage(input = list(comm = "ELC",
    unit = "MWh"))` was rejected outright, and a storage had nowhere to
    declare its units.
-   `draw()` on a storage drew **no arrows at all** unless `@seff` was
    populated. The commodity frame was built by cross-joining the three
    roles with `@seff`, whose prototype has zero rows, so an optional
    slot decided whether the figure had edges. The arrows now come from
    the roles and `@seff` only labels them. The same change fixes a
    store whose roles differ — a hydrogen cavern exchanging electricity
    drew both commodities on both sides, because the sides were split by
    parameter name rather than by role.
-   The **Storage** article is restyled to the modelling convention
    (`STG_*` names, vertical calls, `desc` and `unit` declared, objects
    defined before the repository) and extended to cover the slots it
    previously skipped: `@af`, `@capacity`, `@optimizeRetirement`,
    `@invcost`/`@fixom`/`@varom`, `@aux`/`@aeff`, `@weather`, `@region`,
    `@cluster`/`@vintage`, and a slot map of the whole object.

## New article: Units

-   New pkgdown article **Units** (`vignettes/articles/units.Rmd`),
    covering where each unit is declared (`commodity@unit`, a process's
    `@units`, and the model target), how `convert()` moves between
    dimensions, how `commodity@property` lets a conversion cross them,
    and why money needs a year attached. Sections describing work still
    in progress are feature-gated, so they appear as the features land.
-   `print()` on a `getUnits()` result now works. It was dead code:
    `@export` alone does not register an S3 method on an S4 generic, so
    dispatch fell through to the data.frame default and printed the wide
    `description` column that this method exists to drop. Same defect as
    `print.commodity`; `getUnits(EGAS)` now prints a compact
    slot/parameter/comm/unit table.
-   **Model bricks** corrected: `cap2act = 8.76` was documented as
    giving "8.76 GWh per GW per year (8760 h)" — wrong by a factor of
    1000, and contradicted by the `CHP` chunk in the same article, which
    correctly uses `8760`. The text now states the unit basis explicitly
    (`8760` with activity in GWh, `31.536` with activity in PJ). The
    "mixed units live in the coefficients" note no longer says a
    conversion *must* ride on a chain coefficient, and cross-links to
    the new article.

## Commodities carry physical properties

-   New slot **`commodity@property`**: a tidy table of physical
    properties — heating values, density, molar mass, composition — with
    columns `property`, `value`, `min`, `max`, `sd`, `dist`, `unit` and
    `comment`. It is reference data: `ob2mi()` never writes it to
    `modInp` and no solver template sees it.
-   Its purpose is to record the physics that links different measures of
    the same commodity. A property whose unit is a ratio of two
    dimensions is a conversion factor between them — `lhv = 25.8 GJ/t`
    relates Energy and Mass, `density = 0.85 t/m3` relates Mass and
    Volume. Today those numbers have to be worked out by hand and baked
    into a `ceff` coefficient, with the heating value surviving only as a
    comment. A forthcoming release teaches `convert()` to traverse them.
-   `value` is always the deterministic point estimate; `min`, `max`,
    `sd` and `dist` describe uncertainty around it for sensitivity
    analysis and are optional — a table supplying only
    `property`/`value`/`unit` is the normal case.
-   **`@property` describes the commodity itself.** Anything whose
    numerator is a *different* commodity — a CO₂ emission factor, say —
    belongs in `@emis`, which the model actually reads; a property name
    that looks like an emission factor now warns and points there. The
    two remain related: `frac_C` × 44.009/12.011 ÷ `lhv` reproduces the
    per-energy emission factor, which makes `@emis` checkable rather than
    duplicated.
-   New `commodity_properties()` lists the recognised property names and
    the dimension pair each relates; `commodity_property()` reads a
    single value, unit or range out of an object. Unrecognised names are
    kept with a warning, so properties the package has not anticipated
    can still be recorded.
-   Property tables are validated on construction — unknown names,
    negative or non-finite values, missing units, duplicate keys,
    composition fractions summing above 1, unknown distributions, values
    outside `[min, max]`, and distributions missing their parameters all
    warn. Validation never blocks model building.

## Images and icons on commodities, and `report()` picks them up

-   `newCommodity()` gains `image =` and `icon =` arguments, stored as
    `misc$image` / `misc$icon` following the convention `technology`
    already uses for techspec files. New `object_image()` resolves either
    and reports whether it is a URL, an existing file or missing.
-   **`report()` now defaults `image_file` from the object's own
    `misc$image`** when that names an existing local file. Previously
    only the process designer wired the two together, so `report(tech)`
    on a technology that *had* an image showed none.
-   `print()` on a commodity now works. It was dead code: `@export`
    alone does not register an S3 method on an S4 generic, so dispatch
    fell through to the default and dumped raw slots. It now shows
    `@unit`, the image and the data-frame slots, hiding all-empty
    columns.
-   `getUnits()` on a commodity reports the `property` rows.

## Object autoplots actually plot: points + interpolation, defaults on demand

-   Fixed the defect that made `autoplot()` on supply / import / export /
    technology / storage objects report **"No year-indexed data to plot"**
    for virtually every object: the plot passed `year = NULL` into
    `getData()`'s `...`, where a NULL selector matched *nothing* and
    dropped every row. NULL/empty filters are now ignored (a fix to
    `getData()` for objects generally), and the plots' `year` argument is
    routed as the interpolation grid (`getData(years = )`), so
    `autoplot(x, year = 2020:2050)` extends the lines beyond the given
    years.
-   The intended display — **points for given data, lines for the
    interpolated series** — now actually renders; new argument
    `interpolate = TRUE` (set `FALSE` for given data only). Also on the
    tax/subsidy/constraint and demand plots.
-   New `show_defaults = FALSE` argument (process classes): when `TRUE`,
    parameters mapped to the model but not set in the object are drawn as
    dotted lines at their **default values** (e.g. `ava.lo = 0`);
    non-finite defaults (`ava.up = Inf`) are listed in the caption.
-   Fixed the silently-empty parameter registry behind
    `getData(interpolate = TRUE)`: it read `.modInp@parameters`, but the
    baked-in `.modInp` is the plain YAML list — every lookup fell back to
    generic interpolation. The registry is now built from the list (with
    bounds params expanded to `.lo/.up/.fx` columns and stale YAML slot
    names aliased: supply/import/export `availability`, demand `dem`), so
    object interpolation uses each parameter's own rule and default.
    Interpolation expands only *given* columns — defaults never materialise
    uninvited.
-   Also fixed: `getData(object, interpolate = TRUE)` crashed on any empty
    data-frame slot; demand plots dropped region-NA rows on aggregation.
-   New test suite `test-autoplot-objects.R`; the autoplot vignette's
    `years =` example corrected to `year =` (it was silently ignored).

## Technology reports: vintages, consistent layout, working PDF and docx

`report()` and the tech templates were overhauled around the vehicle
datasheet layout:

-   **Per-vintage levelized costs.** A `levcost_variants` result is no
    longer silently reduced to its first variant: the generic report
    gains a "Levelized Cost by Vintage" section (component-stacked
    comparison chart + per-vintage NPV table), a **Vintages** table, and
    per-vintage cost tables (a per-vintage `invcost` used to display
    only its first row). The detail figures (components, frontier) show
    ONE instance — newest vintage, first cluster, same convention as the
    designer — re-priced with the same arguments (analytic, instant) and
    labeled in the section header. Key-parameter scalars
    (`olife`/`start`/`end`) go blank when several vintages make them
    ambiguous.
-   **`report_generic.Rmd` rebuilt** on the vehicle two-column layout
    with ONE sizing system: fractions of the text width shared by HTML
    and LaTeX (no more px/`\textwidth`/inch mix), every ggplot
    rasterised at 150 dpi through one helper, LCOE labels in
    `cost/activity` units.
-   **docx works**: a Word-safe branch (markdown headings, pipe tables,
    embedded figures) replaces the raw-TeX output pandoc used to
    discard.
-   **Empty columns are dropped everywhere** (`.report_drop_empty_cols`
    in `report()` + template-side pruning); NA cells render as a dash
    (`na.string=` was never a real `kable` argument).
-   PDF soundness fixes shared with the vehicle/summary templates:
    correct LaTeX escaping (the old `fixed = TRUE` patterns never
    matched `$ ^ { }`; brace escaping now ordered around backslash),
    forward-slashed image paths in HTML output, single (not double)
    kable escaping, the vehicle share-panel px/pt bug, aligned HTML/LaTeX
    column fractions, and the input/output `rbind` column mismatch.
-   `report()` now passes only the params a template declares, so
    older or user-supplied custom templates keep rendering as `report()`
    grows new params; the container levcost whitelist was synced with
    the technology one (`repo`, `method`, `full_output`).
-   New test suite `test-report.R` (31 assertions; pandoc/LaTeX-gated).

## `levcost()` computes analytically — no solver required

The unit-demand annual mini-model that `levcost()` builds has a
closed-form optimum for most technologies, and `levcost()` now computes
it directly. New argument `method = c("auto", "analytic", "solve")`:

-   `"auto"` (the new default) prices the technology **analytically —
    no GLPK or any other solver needed** — whenever it qualifies, and
    falls back to the solver otherwise with a message naming the
    reason. `"analytic"` refuses non-qualifying technologies instead of
    falling back; `"solve"` forces the previous behaviour.
-   The analytic engine (`R/levcost_analytic.R`) mirrors the model
    equations one for one — activity/output/input chains (`cact2cout`,
    `use2cact`, `cinp2use`, `ginp2use`, `cinp2ginp`), annual
    availability (`af`/`afs`, weather collapsed to a CF), the greedy
    build–retire–rebuild capacity schedule, EAC annuities
    (`wacc`/`payback`/`olife`), fixom/varom/cvarom/avarom, auxiliary
    flows, and supply pricing — and reproduces the solver's numbers to
    the GLPK output precision (parity-tested in
    `test-levcost-analytic.R`).
-   **Group shares report every corner solution.** The optimum over a
    share polytope is a vertex; the analytic result evaluates *all*
    vertices of the input and output share polytopes and returns them in
    `$frontier_vertices` (share vector, per-activity NPV cost breakdown,
    `optimal` flag), with the cost-minimal corner as the headline
    levcost. The classic `$frontier` / `$levcost_by_*` corner tables are
    produced as before.
-   A vintaged/clustered technology is priced per cell directly — no
    artificial-region isolation, no per-cell LP — which makes
    many-variant technologies essentially instant.
-   The analytic result has the same fields (plus `$method =
    "analytic"`); `$scenario` is `NULL`. Not representable
    analytically (solver still used): technology chains, `timeframe =
    "native"`, `afc.*` bounds, availability lower bounds,
    `optimizeRetirement`, year-varying `invcost`, constrained supplies,
    and group substitution combined with year-varying prices.
-   The process designer and `report(levcost = TRUE)` use
    `method = "auto"`, so the levcost tab and report sections work on
    machines with no solver installed.
-   Fixed in passing: on the solve path the annual capacity factor
    collapsed from `@weather` never reached the solver (the mini-model
    was built from the pre-collapse objects), so weather-driven
    technologies were priced at full availability. Both paths now apply
    the CF.

## techspec containers: YAML and JSON

The techspec format (`read_techspec()`, `tech_from_spec()`,
`tech_to_spec()`, the process designer) now reads and writes **JSON**
(`.json`) as an alternative container for the same validated structure —
`tech_to_spec(tech, file = "x.json")` writes it, `read_techspec()` and
the designer's spec upload/gallery accept it, and the designer gained a
"Save JSON" button. Full numeric precision is preserved.
(A fuller NEWS section for the process designer itself is pending.)

Stack-wide rename `slice` -> `timeslice` (paired with the `timescales`
package; matches the TIMES/OSeMOSYS vocabulary and reads unambiguously
next to the spatial `region` dimension):

-   The set/index family in all four model backends (GAMS,
    GLPK/MathProg, Pyomo, JuMP): `slice`/`slicep`/`slicepp`/`slice2` ->
    `timeslice`/`timeslicep`/..., every `mSlice*`/`pSlice*` symbol ->
    `mTimeslice*`/`pTimeslice*` (e.g. `mTimesliceParentChild`,
    `pTimesliceShare`), and the `ANYSLICE` wildcard -> `ANYTIMESLICE`.
    Equation-level short aliases (`s`, `sp`, ...) are unchanged.
-   Solution output CSVs now carry a `timeslice` column.
-   S4 `calendar` slots: `slice_share`/`slice_family`/`slice_ancestry`/
    `slices_in_frame` -> `timeslice_*`/`timeslices_in_frame`.
-   Input-data columns are `timeslice`; a **compatibility shim** accepts
    the pre-rename `slice` name in user data (`new*` constructors and
    `update()`, `newCalendar()` timetables, `newConstraint()`
    `for.each`/`for.sum`, `fold_/unfold_scenario_parameters()` dims) --
    renamed with a once-per-session warning. External datasets (IDEEA)
    keep working through the shim.
-   Bundled data (`calendars`, `utopia_*`, `model_structure`) and
    `sysdata` regenerated with the new vocabulary.
-   multimod's matching update is a recorded follow-up; until then it
    pairs with pre-v0.80 generated models.

## A storage capacity is a RATE: `@input$cap2act` / `@output$cap2act`

-   The flow bounds carried no duration factor, so `vStorageInpCap` and
    `vStorageOutCap` meant "commodity per **timeslice**", not power. The same
    storage written `cap.fx = 7` allowed **7 per hour** on a 24 x 1-hour calendar
    and **1.75 per hour** on a 6 x 4-hour one: refine an hourly model to
    4-hourly and every store silently became a quarter as powerful.
-   `@input` and `@output` now carry a **`cap2act`**, and the four flow bounds
    (plus `eqStorageThroughputUp`) read
    `cinp.up * cap2act * cap * pTimesliceShare[s]` -- the same shape
    `eqTechAfUp` has always had.
-   **`cap2act` defaults to 8760**, the hours in a year, so a capacity reads as
    commodity per HOUR on any calendar. On `d365_h24` -- and on representative-day
    subsets, whose leaf share is still 1/8760 -- the factor is exactly 1, so
    those models do not move at all. It assumes commodity unit = capacity unit x
    hour (GW with GWh); GW with TWh wants `8.76`. Unlike `pTechCap2act`
    (default 1) because a storage has an intrinsic clock -- the level -- that a
    technology activity does not.
-   The **storing** side deliberately gets no `cap2act`. Energy is energy at any
    resolution, which is also exactly what PyPSA does: its `soc <= e_nom` and
    `soc <= p_nom * max_hours` carry no elapsed-hours weighting, and only the
    accumulation equation does.
-   The storage block was already inconsistent with **itself**: the standing-loss
    term has always been `(pStorageStgEff)^(pTimesliceShare)`, i.e. share was
    already read as elapsed time for decay while the capacities ignored it. That
    is now uniform.
-   A welcome side effect: **`duration` finally reads as hours.** With the
    discharger a per-hour rate and the reservoir an amount, `duration = 6` on a
    10 MW discharger gives exactly 60 MWh -- pinned by a test.

### It did not move the goldens

The flow bounds are constraint maps built from `pStorageCinp`/`pStorageCout`
DATA: with no `cinp`/`cout` supplied no bound equation is emitted at all, so the
change is a no-op for those storages. No tier model in `data-raw/testing-models.R`
sets them, and `model-regression` (396 assertions) did not move. The PyPSA-Eur
converter does set `cinp.up = cout.up = 1`, but on the hourly calendar where the
factor is exactly 1; it now states `cap2act = 8760` explicitly rather than
leaning on the default.

### What aggregation still costs, and what this does NOT fix

Making the rating resolution-independent does not make a coarse calendar
adequate. On slices longer than the discharge duration the power bound goes slack
and the store is governed by its energy capacity alone -- and the **cycle count**
collapses: `N` timeslices allow at most `floor(N/2)` cycles, so a day/night
calendar permits a 6-hour battery exactly ONE cycle a year (60 MWh moved against
roughly 21,900 MWh for a store that really cycles daily), and a single annual
timeslice has no dynamic at all. A store whose cycle is shorter than one
timeslice is **understated, not approximated**. The Storage article says so
plainly; a cycle/throughput bound would be a separate feature.

Still outstanding: aux flows driven by capacity (`pStorageCap2AInp *
vStorageCap`) are now rate-proportional too and want the same factor. Left with
the deferred `@aeff` pass.

### Each `@seff` coefficient now reaches its own role's commodity

-   `@seff` feeds three parameters -- `pStorageInpEff`, `pStorageOutEff` and
    `pStorageStgEff` -- and the slot carries no `comm` column, so `ob2mi()` fills
    one per parameter from the matching role. It assigned back into the shared
    frame, so whichever parameter was processed first stamped `comm` and the
    other two inherited it.
-   Invisible while all three roles held the same commodity, and wrong the moment
    they differ. An electric vehicle storing kWh and selling kilometres had its
    six-kilometres-per-kWh `outeff` filed under **electricity**, where nothing
    reads it, so the motor ran at the default efficiency of 1 and the car drew
    six times too much from the grid -- with an OPTIMAL solve and no warning.
-   Pinned by `test-storage-variants.R`, which checks both the parameter keys and
    the resulting energy chain.

## `storage_duration()`: how long the energy actually sits in the store

-   A storage level answers "how full is it", not the question people ask of
    it: how much of that energy is doing SHORT cycling -- an evening peak -- and
    how much is a seasonal reserve sitting there for weeks. Those have entirely
    different economics and are served by different technologies, yet both
    appear as one `vStorageLevel` number.
-   New exported **`storage_duration(scen, width = )`** splits the level into
    duration bands and returns a tidy table ready to stack:

    ``` r
    d <- storage_duration(scen)                       # 12h / 1d / 1w / 30d
    ggplot(d, aes(datetime, value, fill = duration)) + geom_area()
    ```

-   The split is by nested FLOORS, not by smoothing. For a window of `w` hours,
    `floor_w` is the energy that never leaves the store across some `w`-hour
    stretch -- energy committed for at least that long. Floors are nested by
    construction, so successive differences partition the level with no overlap
    and **no remainder**: the bands sum back to `vStorageLevel` exactly.
-   Adapted from the IDEEA figure script (`fMA.R` +
    `storage_duration_figure.R`). Despite the name, none of it was a moving
    average -- an average would blur the bands together and would not sum back.
    The original sampled a rolling minimum at every phase offset, filled down
    and took a row-wise maximum, a loop over `w`; that is exactly a
    right-aligned rolling MAXIMUM of the rolling minimum, so the port computes
    the same number in one pass. The equality is what the test pins, against a
    brute-force definition rather than a remembered output.
-   Fixed while porting: a hard-coded `"Asia/Kolkata"` that silently moved every
    model's clock (now a `tmz` argument defaulting to UTC), a `browser()`, the
    pre-v0.80 `slice` vocabulary, and `vStorageStore` -> `vStorageLevel`. End
    padding follows each storage's `@fullYear`: a store that cycles over the
    year genuinely continues into itself and is wrapped, one that does not
    repeats its edge values. `cyclic = TRUE`/`FALSE` overrides.
-   Verified on a 21-day hourly model with two duties layered on one store --
    solar-only generation forcing a daily cycle, plus a three-day overcast
    stretch forcing a multi-day carry. They land in different bands (`>1w` mean
    9.46 against `6h-1d` mean 1.73), and the bands reproduce the level to
    4.4e-16.

## A storage sizes its charger separately: `vStorageInpCap` and `@inp2out`

-   The charging and discharging sides shared one capacity variable, so an
    electric vehicle drawing 7 kW from the grid while delivering 100 kW to its
    motor had nowhere to put the charger. `@input` now carries the CHARGER's own
    stock, bounds and costs, and **`@inp2out`** links the two ratings the way
    `@duration` links energy to power:

    ``` r
    newStorage("EV", commodity = "ELC",
               output  = list(invcost = 12144),   # the motor / discharger
               input   = list(invcost = 500),     # the on-board charger
               inp2out = data.frame(inp2out.lo = 0, inp2out.up = 1))
    ```

-   Same bound semantics throughout: `.fx` ties the two, `.lo`/`.up` let the
    model choose, a bare number is the fixed shorthand, and a one-sided range
    opens the other side. The default `[1, 1]` keeps the two sides symmetric,
    which is what a storage without a separate charger has always been.
-   `@input$invcost` annuitises through the storage's own `@vintage`, like the
    other two parts.

## All three parts are written the same way

-   `@output` now accepts the same capacity and cost columns as `@input` and
    `@storage`, and **folds** them at construction into `@capacity`, `@invcost`
    and `@fixom` — the slots where the output side has always lived. So
    `output = list(invcost = 12144)` and `invcost = 12144` are the same
    statement, and supplying both is an error rather than a silent winner. No
    parallel `pStorageOut*` parameter family was created to say what the existing
    one already says.

    ``` r
    newStorage("BAT", commodity = "ELC",
               output  = list(invcost = 12144),   # EUR/MW
               storage = list(invcost = 8081),    # EUR/MWh
               input   = list(invcost = 500),     # EUR/MW
               duration = 6, inp2out = 1)
    ```

## Removed: `@sharedThroughput`

-   Added and then withdrawn in the same development cycle, before any release.
    It asserted that charging and discharging share one device, so their flows in
    a timeslice had to fit within the output capacity.
-   Measurement showed it is **redundant where it is meaningful and meaningless
    where it would matter**:
    -   For a single-commodity storage the constraint never binds. On a 24-hour
        arbitrage model the maximum simultaneous charge-and-discharge in any
        timeslice was **0.000000** with and without the flag, at an identical
        objective -- and that held even for a **lossless** store. The LP has no
        reason to push energy into and out of the same store at once.
    -   For a multi-commodity storage simultaneity does occur -- an EV charging
        while driving showed an overlap of 3.703704 -- but the flag did not stop
        it, and could not: the constraint added `vStorageInp` to `vStorageOut`
        without conversion, i.e. **kWh to km**, and compared the sum against the
        motor rating.
-   Its right-hand side also omitted `pStorageCoutUp` while `eqStorageOutUp`
    includes it, so with `cout.up > 1` it was simply a tighter per-flow bound.
-   Archived with the measurements and a sketch of the correct form -- both flows
    in STORED-commodity units, `inpeff*inp + out/outeff`, which is the only basis
    on which they are commensurable -- in
    `drafts/storage-shared-throughput.R`. Nothing in PyPSA imposes an equivalent
    constraint either.

## Verified

-   A model exercising **all** of 2c at once — energy priced per MWh with an
    optimised duration, a charger priced and rated separately, and a shared
    device — gives the identical answer on **GLPK, Julia/HiGHS, Pyomo/GLPK,
    Pyomo/CBC and GAMS/CPLEX (NEOS)**: objective 83.0000, output capacity 10,
    energy 40, charger 10, duration 4.000 (strictly inside `[2, 8]`), `inp2out`
    1.000.
-   **Structure follows data** holds for the charging side too: omitting `@input`
    data produces no `vStorageInpCap` at all and the input bounds inline
    `inp2out * vStorageOutCap`, which at the default of 1 is the previous model.
    `model-regression` (396 assertions) does not move.
-   A bare bound binds with no cost attached: `input = list(cap.lo = 25)` gives a
    charger of exactly 25 where only 10 is needed, and `cap.up = 8` gives exactly
    8 — starving the store and forcing the expensive backstop.
-   **GAMS/CPLEX via NEOS** returns the same answer as the other four, so the
    GAMS port is verified at runtime and not only by inspection. No GAMS licence
    is installed locally; `set_neos_email()` plus the existing `gdxlib_path`
    (`C:/GAMS/35/`) is what makes the remote route work.

## Also

-   `draw()` showed no duration label once `@duration` became a bound: it read
    the bare `duration` column, which the constructor now normalises into
    `duration.fx`. It reads the bound columns and renders a range as `lo-up`.

## A storage's energy capacity is its own variable: `vStorageStgCap`

-   `@duration` used to be the *only* way to say how much energy a store held,
    and it was a fixed coefficient on the right-hand side of two availability
    constraints: `pStorageDuration` was indexed by **storage alone** — one number
    for every region and every year — and appeared only as
    `af * duration * vStorageCap`. Energy capacity could therefore not be
    bounded, priced, reported or chosen by the model, and a 6-hour battery's
    €/MWh component had to be hand-multiplied into its €/MW number.
-   There is now a real **`vStorageStgCap`** (with `vStorageStgNewCap`), measured
    in the commodity's own unit rather than in power, with its own stock, bounds
    and capital and fixed costs on `@storage`:

    ``` r
    newStorage("BAT", commodity = "ELC",
               invcost = list(invcost = 12144),   # EUR/MW  -- the inverter
               storage = list(invcost = 8081),    # EUR/MWh -- the cells
               duration = data.frame(duration.lo = 2, duration.up = 8))
    ```

-   **`@duration` is a bound** on `(storage, region, year)`: `duration.fx` ties
    energy to power, `duration.lo`/`.up` let the model choose the ratio. The
    scalar shorthand `duration = 6` normalises to `duration.fx = 6`. A one-sided
    range opens the other side, so `duration.up = 8` means "up to 8 hours" and
    does not silently inherit a lower bound of 1.
-   `@storage$invcost` annuitises through the same `@vintage` as the power side —
    one object, one lifetime and one wacc, two capital costs on different bases.
    Per-part lifetimes are deliberately not in this change.

### Structure follows data, so nothing moves for existing models

-   A part carrying only a commodity name gets **no capacity variable**: naming a
    commodity is metadata, not data. `vStorageStgCap` exists only where
    `@storage` supplies a stock, a bound or a price (`mStorageStgCap`), and
    everywhere else the availability bounds inline `duration * vStorageOutCap`
    exactly as before. A storage written the old way therefore keeps one power
    variable and its previous LP — `model-regression` (396 assertions) does not
    move.
-   That single rule is what the plan called "structure follows data" and "inline
    a fixed link"; they turned out to be the same mechanism. It also avoids
    over-determining a part pinned both by its own accounting equation and by an
    `.fx` link.

### Verified

-   **Split-cost equivalence.** `invcost = 12 + 6*8` with no storing part gives
    the same objective (80.000000) as `invcost = 12` + `storage = list(invcost =
    8)` + `duration = 6`; the second reports `vStorageStgCap / vStorageOutCap` of
    exactly 6, the first has no such variable at all.
-   **The ratio is genuinely optimised.** With the output capacity made binding
    and the range opened to `[2, 8]`, the model lands on **4.000** — strictly
    interior — at an objective of 81.33, against 85.33 at a fixed 2 h and 92.00 at
    a fixed 8 h (where energy is forced to 80 MWh though only 40 is needed).
-   Identical on **GLPK, Julia/HiGHS, Pyomo/GLPK, Pyomo/CBC and GAMS/CPLEX via
    NEOS**.

### Renamed: `vStorageCap` -> `vStorageOutCap`

-   `vStorageCap` means **power** and `vStorageStgCap` means **energy**, one
    syllable apart. Rather than leave a name whose meaning now has to be guessed,
    `vStorageCap` and `vStorageNewCap` become **`vStorageOutCap`** and
    **`vStorageOutNewCap`** — the old names resolve to nothing instead of quietly
    returning the wrong quantity. `getData(scen, "vStorageCap")` returns no rows;
    use `"vStorageOutCap"`.

### Three constructor defects fixed on the way

-   **`newStorage(commodity = "ELC", storage = list(invcost = 8081))` silently
    dropped the `invcost`.** The commodity shorthand *replaced* a supplied part
    frame instead of adding the commodity to it — and that is exactly the shape
    the documented examples use. It did not bite before only because `@storage`
    had no cost column to lose.
-   **`cap2stg` could never work.** `newStorage()`'s formal default was a bare
    `duration = 1`, so "was duration supplied?" was always true and the
    deprecation path always raised "supply either `duration` or the deprecated
    `cap2stg`, not both". The default is now `NULL`; `pStorageDuration`'s `[1, 1]`
    default supplies the tie instead.
-   `!is.na(NULL)` is `logical(0)` and `TRUE & logical(0)` is `logical(0)`, so
    testing the bound columns inline collapsed the normaliser's whole vector and
    moved nothing whenever `.lo`/`.up` were absent — the common case.

### Known, not fixed here

-   **`pyomo/energyRtAbstract.py` still carries the pre-`[eac-fix]` flat EAC**
    (`pStorageEac` on TOTAL capacity) while GLPK, GAMS, Julia and Pyomo-Concrete
    use the vintaged new-capacity form. The storing-side term added here matches
    the shape actually present so the energy cost is charged, but the underlying
    divergence predates this change and is not addressed by it.

## A storage names a commodity per role: `@input`, `@storage`, `@output`

-   A storage used to have a single `@commodity`, so whatever went in came
    back out. Modelling anything else meant composing three objects and an
    **artificial commodity** to join them -- a hydrogen store became an
    electrolyser + a store + a fuel cell, plus an `H2` balance whose only
    job was to link them; an electric car needed an onboard `ELC_CAR`
    commodity to stop the motor drawing from the grid while driving.
-   The commodity is now named **per role**: `@input$comm` fills the store,
    `@storage$comm` is what it HOLDS (what `vStorageLevel` measures), and
    `@output$comm` is what it releases. One shape covers a battery
    (ELC/ELC/ELC), a hydrogen store (ELC/**H2**/ELC) and a reservoir
    (HYD/HYD/**ELC**).
-   `@seff$inpeff` / `$outeff` become **cross-commodity conversion factors**
    (input -> stored, stored -> output), the role `cinp2use` plays for a
    technology. When the three commodities coincide they are the round-trip
    efficiencies they always were.
-   **There is no `@commodity` slot.** `newStorage(commodity = )` still
    works and is still the one-liner for the common case -- it is folded
    into all three roles at construction, so an object never carries two
    answers to what it consumes, and `update()` stays unambiguous. Every
    storage written the old way yields three identical roles and an
    unchanged LP: `model-regression` (396 assertions) does not move.
-   Measured on a two-day test model, the fused hydrogen storage reproduces
    the objective of the storage + 2 technologies decomposition exactly
    (193.142857 on GLPK, Julia/HiGHS and Pyomo/GLPK alike) in **149
    variables / 151 constraints instead of 251 / 257**.

### Three defects this exposed, each of which silently disabled storage

-   **`mStorageInpTot` / `mStorageOutTot` followed the LEVEL's commodity.**
    Both totals were built from `mvStorageLevel` (plus `mvStorageAInp` for
    *both* sides), which was invisible while a storage had one commodity
    and fatal once it could hold something else: a hydrogen store registered
    only in the `H2` balance, so the `ELC` balance never saw it charge or
    discharge and the store sat unused at a valid-looking optimum. Each
    total now follows its own flow, and the aux side is split so `AInp`
    feeds the input total and `AOut` the output.
-   **The stored commodity never reached `mCommReg`.** `map_mCommReg()`
    derives availability from process *outputs*, and a store's held
    commodity is in neither its inputs nor its outputs, so it was absent
    from the commodity-region closure, `.filt_cr()` emptied `mvStorageLevel`
    and the storage lost its level variable altogether -- built, solved and
    reported, storing nothing. A storage now makes its stored commodity
    available wherever it operates.
-   **`vStorageInp` / `vStorageOut` were declared and reported over
    `mvStorageLevel`.** With the roles split, the flows exist on their own
    domains; the previous domain silently reported zero flow for any
    storage whose level commodity differed from what it exchanged.
-   `pStorageInpEff` / `pStorageOutEff` are likewise densified over
    `mvStorageInp` / `mvStorageOut` rather than the level's domain, where
    they would have been materialised against the wrong commodity and read
    as 0 -- zeroing the flow they scale.

-   Ported to **all five backends**: GLPK, GAMS, JuMP/Julia, Pyomo-Concrete
    and Pyomo-Abstract, plus the mosox template (regenerated from its
    generator). Verified end-to-end on GLPK, Julia/HiGHS, Pyomo/CBC, Pyomo/GLPK
    and **GAMS/CPLEX via NEOS** -- the fused hydrogen storage and its
    storage+2-technologies decomposition both return 193.142857 on GAMS too,
    with the same 114.286 in and 40.000 out.

-   New `tests/testthat/test-namespace-integrity.R`: every `export()` in
    NAMESPACE must name a real object. Roxygen has twice attached a block
    (and its `@export`) to a helper inserted above the documented function
    and then swallowed the following prose as tags, producing `export(a)`,
    `export(and)` and `export("(hydrogen:")` while the real function lost
    its export -- invisible under `load_all()`, which exports everything.
    The earlier guard only checked a fixed list of constructors and missed
    the second occurrence.

## Renamed: `storage@cap2stg` -> `@duration`, `eqStorageClear` -> `eqStorageOutLevel`

-   **`@cap2stg` is now `@duration`** (`pStorageCap2stg` -> `pStorageDuration`),
    the rename `data-raw/maps.R` had already flagged ("to be renamed to
    duration"). Same number, plainer meaning: how long the store runs at its
    rated output. `cap2stg = ` is still accepted, renamed with a warning once
    per session; supplying both spellings is an error. The `duration = 6` scalar
    shorthand works exactly as `cap2stg = 6` did.
-   Note `ncap2stg` (in `@aeff`) is a **different** parameter and is unchanged.
-   **`eqStorageClear` is now `eqStorageOutLevel`.** "Clear" said nothing; the
    equation bounds `vStorageOut` by the level available. It takes no `Lo`/`Up`
    suffix because that marks a *paired* bound (a `.lo` and `.up` from one
    parameter) and this one has no partner — the same convention as
    `eqTradeCapFlow` and `eqTechRetiredStockCum`. Both backends now record why
    it is stricter than the level's own non-negativity: it forbids discharging
    energy charged in the *same* timeslice, and carries no decay factor. The
    dead `eqStorageClean` placeholder beside it is removed.
-   Neither rename changes a number: the model-regression goldens (396
    assertions, objectives included) are unmoved.
-   `data/utopia_modules.rda` was regenerated. Bundled datasets serialise S4
    objects with the class definition of their time, so a storage saved before
    the rename has no `@duration` slot and `draw()` fails on it. There is no
    upgrade path for such objects — they must be rebuilt from `data-raw/`, the
    same rule the `@vintage` merge established.
-   New guard in `test-storage-inflow.R`: the public constructors must stay
    exported. Inserting a helper *between* a roxygen block and the function it
    documents silently reassigns the block — and its `@export` — to the helper.
    `devtools::load_all()` exposes everything, so the suite stays green while an
    installed package loses the function. That happened to `newStorage()` while
    this shim was being added.

## `storage@charge` -> `@startLevel`: an endowment once per cycle, not per timeslice

-   The slot is now **`@startLevel`** (`pStorageCharge` -> `pStorageStartLevel`),
    and the semantics changed deliberately. `charge = ` and the short-lived
    `inflow = ` are both still accepted, renamed with a warning once per
    session; supplying two spellings is an error.
-   **The value is ANNUAL.** When the cycle is shorter than a year, each cycle
    receives its share of it — 365 daily cycles get 1/365 each — so the annual
    endowment is the same however the cycle closes, rather than being multiplied
    by the number of cycles. A partial-year calendar endows that fraction.
-   **`@startLevel` has no `timeslice` column.** The slice is derived from the
    calendar and `@fullYear`: the energy is added ONCE PER CYCLE, at the first
    timeslice of the cycle — once a year with `fullYear = TRUE`, once per parent
    timeframe with `FALSE`. A `timeslice` column on a deprecated spelling is
    dropped with a warning rather than silently ignored.
-   **That is the bug fix.** `@charge` was documented as a "pre-charged level at
    the beginning of the operational cycle" but was implemented as an additive
    term at *every* timeslice it was given a row for. Since the storage balance
    is cyclic there is no "beginning", so a row without a `timeslice` was
    broadcast by the usual NA wildcard to every slice — 8760 unpriced injections
    on an hourly calendar where one was meant, and exactly the form the shipped
    `newStorage()` example used. Removing the column removes the trap: there is
    no longer anything to leave unset.
-   It is **free to the model**, by design and unavoidably: a store that ends a
    cycle below where it started has consumed an endowment nobody paid for.
    PyPSA's `state_of_charge_initial` has the same property. Being additive, the
    level at the first timeslice is `startLevel` *plus* any carry-over — at
    least `startLevel`, not exactly it; the model may end the cycle empty to
    make it exact.
-   **Hydro inflow does not belong here.** Use a weather-driven `supply` (with
    `ava.up`, so spilling is free) feeding the storage: the water then stays
    visible in the commodity balance, where it can be bounded, reported and
    constrained.
-   Implemented without touching any solver template: the balance term already
    existed and defaults to zero, so a new interpolation step
    (`place_start_level()`, alongside `compute_eac_parameters()`) rewrites the
    wildcard row into one explicit row per cycle. It derives the cycle from the
    same successor map the balance uses, so the two cannot disagree.
-   New `tests/testthat/test-storage-startlevel.R` covers the placement under
    both `fullYear` settings, that a zero writes nothing, both deprecated
    spellings, the dropped `timeslice` column, and that the public constructors
    stay exported.

## Renamed: `vStorageStore` -> `vStorageLevel`

-   The storage state-of-charge variable is now **`vStorageLevel`**, with
    `eqStorageStore` -> `eqStorageLevel` and the maps `mvStorageStore` ->
    `mvStorageLevel`, `meqStorageStore` -> `meqStorageLevel` following. The
    rename had been flagged in the GAMS source itself (`*eqStorageStore -
    rename to eqStorageLevel`) before it was made.
-   Why: the level is a **state**, not a flow. Summing it over timeslices is
    meaningless -- which is what `role: stock` already encoded -- while the old
    name read as a verb and paired confusingly with `vStorageInp`/`vStorageOut`.
    It also frees the word "charge" for the charger, ahead of the storage-class
    redesign.
-   **This is a breaking rename for result-reading code**:
    `getData(scen, "vStorageStore")` returns nothing; use `"vStorageLevel"`.
    Numbers are unchanged -- the model-regression goldens (396 assertions,
    objectives included) are unmoved.
-   Two dead placeholders were removed from the GAMS source at the same time:
    `eqStorageStorePS` / `eqStorageStoreFY`, intended for splitting the
    parent-timeslice and full-year storage cycles into separate equations. That
    split never happened and is no longer needed -- the choice lives entirely in
    `meqStorageLevel`, which picks `mTimesliceNext` or `mTimesliceFYearNext` per
    storage from `@fullYear`.

## `payback` now works on GAMS, Julia and Pyomo-Concrete, not just GLPK

-   **`@invcost$payback` was implemented in the GLPK model only.** Every other
    engine refused a model that set it (`.assert_payback_supported()`), because
    their `eqXEac` still keyed the charging window on `pXOlife` — annuitising
    over the payback period while charging for the full operational life would
    have over-recovered the investment. The GAMS source even declared
    `pTechPayback`/`pStoragePayback`/`pTradePayback` "for parity with the GLPK
    model" and then did not use them.
-   The cost-recovery disjunction is now ported into `eqTechEac`,
    `eqStorageEac` and `eqTradeEac` of **GAMS, JuMP/Julia and Pyomo-Concrete**,
    mirroring GLPK: `pXPayback` where the user set one (> 0), otherwise
    `pXOlife`. `eqXCap` deliberately keeps `pXOlife` in every backend — the
    technical life still governs when capacity *operates*; only the cost window
    moves.
-   **`Pyomo-Abstract` still refuses it**, and the guard now says so precisely.
    Its `eqXEac` is the pre-vintaging `pXEac * vXCap` form, which charges the
    annuity on total capacity including pre-existing stock, so a cost-recovery
    window would have nothing to narrow. That form is a separate known defect;
    it is unreachable from the shipped presets, which all resolve to Concrete.
-   Verified by solving the same payback model on GLPK and Julia/HiGHS: the
    objective agrees to 1.000000000000 and `sum(vTechEac)` to 1e-11, while
    `payback = 10` moves the objective 5.6% against the same model without it —
    so the agreement is between two genuinely different models, not two copies
    of one. GAMS and Pyomo were not runnable in the development environment, so
    they are covered structurally rather than numerically.
-   New structural test in `test-payback.R`: the `eqXEac` of every backend that
    claims support must reference `pXPayback` in `.modelCode`, and
    `PYOMOAbstract` must not. It fails both on a template edit that drops the
    disjunction and on a `sysdata` that was not rebuilt after one.

## `prod()` over several weather factors: verified, guard retired

-   `write_jump()` carried a commented-out guard refusing any model with more
    than one weather factor on the same process, left from an early JuMP
    translation that could not express the product. The emitted Julia does
    express it, with the explicit `; init = 1` identity the empty case needs.
-   Confirmed by solving a two-factor technology on GLPK and Julia/HiGHS: the
    objectives agree to 1.0000000000. Dropping a factor would roughly double
    `af.up` and halve capacity, so the objective is a sharp test. The dead guard
    is replaced by a note recording the check.

## Breaking: `getData(timeframe =)` now defaults to `"highest"`, not `"lowest"`

-   **`getData()` returned sub-annual results aggregated to `ANNUAL` by
    default.** An 8760-slice hourly series came back as a single number,
    summed over every timeslice. The result looks like a perfectly
    ordinary annual figure, so the mistake is invisible: nothing errors,
    nothing warns, and the value is indistinguishable from a genuine
    annual total until it is compared against something external.
-   The default is now **`"highest"`** — native resolution, exactly as
    stored. This matches the spatial twin `geolevel`, whose default
    `"finest"` has always meant "as stored". Aggregation is easy to ask
    for and hard to notice when it was not wanted, so the safer default
    is the one that does nothing.
-   **Migration:** code that relied on annual totals must pass
    `timeframe = "lowest"` explicitly. Within the package, every
    internal caller that wanted annual sums has been pinned:
    `levcost()` (seven call sites) and `.mix_fetch()` in
    `R/plot_scenario.R`. `.mix_fetch()` now passes `timeframe` on both
    branches rather than letting the `native = FALSE` branch inherit it.

## Bug fix: `storage@fullYear` had no effect — every storage cycled within its parent timeframe

-   **`storage@fullYear` was silently ignored.**
    `.build_meqStorageStore()` joined `mTimesliceNext` unconditionally
    when pairing each storing timeslice with its predecessor, so the
    state-of-charge cycle always closed inside the parent timeframe. The
    slot defaults to `TRUE`, so **every** storage got the `FALSE`
    behaviour, whether or not the user asked for it. `mStorageFullYear`
    was built and declared in all four backends but referenced by no
    equation; the archived GAMS template
    (`gams/.archive/energyRt - 202308.gms`) did honour it, so this was a
    regression introduced when the balance map moved into the mapping
    engine.
-   **What it meant.** On a calendar of hours nested under days, a
    battery could not carry energy from one day into the next: each day
    was an independent loop. Multi-day and seasonal storage — a
    168-hour hydrogen store, a pumped-hydro reservoir spanning
    seasons — were not representable at all, and a battery was sized on
    within-day peak power rather than on arbitrage across the year.
-   **Now** the successor map is chosen per storage:
    `mTimesliceFYearNext` for `fullYear = TRUE`, `mTimesliceNext` for
    `FALSE` — the same treatment technology ramping already received in
    `.build_ramp_maps()`. Fixed entirely in the mapping engine; no
    template, `sysdata` or `.dat` format change.
-   **Results change** for any model with a calendar **three or more
    levels deep** (e.g. `ANNUAL/DAY/HOUR`) that contains a storage. A
    two-level calendar (`ANNUAL/SEASON`) is unaffected, because there
    the parent-timeframe wrap and the year wrap are the same thing —
    which is also why the bundled regression models
    (`data-raw/testing-models.R`, `ANNUAL/SEASON`) did not catch it and
    why their goldens are unmoved. If your model has a deeper calendar,
    expect storage to be used more and total cost to fall; set
    `fullYear = FALSE` explicitly to keep the old behaviour.
-   If the calendar supplies no year-wide successor map, storages
    requesting `fullYear = TRUE` now fall back to the parent-timeframe
    cycle **with a warning** rather than dropping out of the balance
    entirely. A storage missing from `meqStorageStore` has no balance
    equation at all, which leaves its level unconstrained by history —
    free energy, reported `OPTIMAL`.
-   Slot documentation for `storage@fullYear` rewritten: the `TRUE` and
    `FALSE` branches were described with the same sentence.
    `technology@fullYear`, documented as "currently ignored for
    technologies", in fact governs ramping — corrected.
-   New `tests/testthat/test-storage-fullyear.R` pins the balance map
    itself on a two-day/four-hour calendar (solver-free) and then the
    behaviour: with the cheap plant available only on day 1 and demand
    only on day 2, storage bridges the boundary when `fullYear = TRUE`
    and cannot when it is `FALSE`.

# energyRt 0.74.0.9000-dev

**The user config moved out of the home directory**

`en_config_write()` now writes to `tools::R_user_dir("energyRt", "config")`
rather than `~/.energyRt/config.yml`. CRAN policy does not permit a package to
write in the user's home filespace, and the R >= 4.0 user directories are the
sanctioned alternative. The old location is still read, so existing setups keep
working; when you next call `en_config_write()` the legacy files
(`~/.energyRt/config.yml` and the deprecated `~/.energyRt.R`) are renamed to
`*.bak`, since the latter is sourced at attach and would otherwise keep
overriding the new config. Pass `backup = FALSE` to leave them alone.

**An unservable demand no longer aborts interpolation**

`interpolate_model()` warns instead of stopping when a demand commodity has no
supply, production, trade or import, so an incomplete model can be inspected and
handed to the solver. Restore the previous behaviour with
`options(en.model_checks_stop = TRUE)`.

**Settings — one registry, one prefix, one config file**

Package options were spread over four unrelated mechanisms: the
`options` registry, bare `getOption()` calls, load-time side effects,
and an R script sourced from the home directory. They are now a single
documented layer.

*Option names are prefixed `en.`* (**breaking**). Every option is
declared with the same naming scheme instead of one hand-written name
per declaration, so `options(verbose = )` becomes
`options(en.verbose = )`, `options(solver = )` becomes
`options(en.solver = )`, and so on. The old names were un-prefixed and
collided with base R — `verbose` in particular. The documented API is
the `get_*()` / `set_*()` functions, which are unchanged.

*Environment variables are prefixed `ENERGYRT_`* — `ENERGYRT_GAMS_PATH`,
`ENERGYRT_JULIA_PATH`, and so on. This also fixes `glpk_path`, which was
the only lower-case one. The old un-prefixed variables (`GAMS_PATH`,
`JULIA_PATH`, ...) still work for one release and warn once.
`NEOS_EMAIL` deliberately keeps its bare name: `set_neos_email()`
exports it so the Pyomo subprocess inherits it.

*New: a persisted configuration file.* `en_config_write()` saves the
current settings to `~/.energyRt/config.yml` (or `./.energyRt.yml` for
one project), `en_config_read()` reads it back, and energyRt applies it
when the package loads — without overriding anything already set through
an R option or an environment variable. Sourcing `~/.energyRt.R` still
works but is deprecated.

*New: `en_config_show()`* prints every option with its current value and
where that value came from — option, environment variable, config file,
or package default. This is the first thing to run when a solver is not
being found.

*New: `set_solver_path()` / `get_solver_path()`* — the generic form of
`set_gams_path()`, `set_julia_path()` and the other four, which are now
thin wrappers over it rather than five copies of the same body. A path
that does not exist is still rejected at the point of the mistake.

*New: `?energyRt-options`* documents all seventeen options with their
defaults, option names and environment variables. It is generated from
the declarations, so it cannot drift.

*Verbosity is one setting, not two.* `en.verbose` is a level (`0`, `1`,
`2`, ...; `TRUE`/`FALSE` are read as `1`/`0`), tested with
`isVerbose(level)`. The separate `energyRt.verbose` option is deprecated
and honoured for one release. `en.debug` gained a matching `isDebug()`
and now actually does something: it gates internal consistency warnings
that used to be silent — or, in one case, used to call `browser()`.

*Other option changes.* `en.neos_endpoint` is now a declared option
rather than a bare `getOption()`. `en.progress_bar` is wired to
`set_progress_bar()` / `show_progress_bar()` instead of being declared
and never read. The internal "GDX library already loaded" flag is no
longer a user-visible option. `data.table::setNumericRounding(2)` moved
from source time into `.onLoad()`.

**Dead model code removed — LEC and the trade-cost aggregators**

An audit of all 237 mapping parameters (`dev/audit-dead-maps.R`, measuring
"populated in any of six models" against "referenced by any live solver
template") found four that nothing could ever fill. They are gone, along
with the equations they gated:

- **LEC** (`eqLECActivity`, `meqLECActivity`, `mLECRegion`, `pLECLoACT`).
  The `model@LECdata` slot had already been commented out, so there was no
  way to supply data; the map builders were identity no-ops and the
  constraint iterated an empty set in all four backends. Removed from
  GLPK, GAMS, Julia and both Pyomo templates.
- **`mvTradeCost` / `mvTradeRowCost`**, which gated `eqCostTrade` and
  `eqCostRowTrade` — equations already inside a `$ontext` block in the
  GAMS template. Trade costs reach the objective through `vTradeEac`,
  `vTradeFixom`, `vImportIrCost` and `vExportIrCost` instead. (There is no
  `vTradeVarom`: the activity-side cost arrives via
  `eqImport/ExportIrCost` from `pTradeIrCost` + markup.) These follow
  `mvTradeIrCost`, retired the same way earlier.

Every written `.dat` loses five now-meaningless lines
(`set mvTradeCost := ;` and friends). **Objectives are unchanged** — the
removed equations were vacuous, verified by solving single- and
multi-region UTOPIA before and after and comparing to the last decimal.

Not removed, despite looking similar: `mTechAfUp` / `mTechAfcUp` are also
forced empty, but `af.up` still binds through `meqTechAfUp` + `pTechAf`.
They are redundant domain maps, not the binding mechanism.

**Fixes**

- `get_scenarios_path()` was defined twice, in `R/options.R` and
  `R/utils.R`; the second silently shadowed the first. Both it and
  `set_scenarios_path()` now live in `R/options.R`.
- `.call_solver()` restored the working directory only from its error
  handlers, so a non-error early return left the session inside the
  solver run folder. It now restores on every exit path.
- `interpolate_model(ondisk = TRUE)` referenced `mi_path` before it was
  assigned — it was only initialised on the in-memory branch.
- The default solver reported `lang = "glpk"` while
  `solver_options$glpk` reported `lang = "GLPK"`, so the two compared
  unequal; only a case-insensitive dispatch hid it.
- Two test scripts changed the default solver without restoring it,
  leaking into every test that ran after them.

# energyRt 0.70.5.9000-dev

**Renames — main data slots, the `sub` class, and the plotting vocabulary**

All of the below are **breaking**. Nothing in this group changes the
generated solver models: every GAMS/GLPK file written before and after
the renames is byte-identical, because the modInp set and parameter
names were deliberately left alone.

*Main data slot of class `X` is now named `X`.* The four
commodity/flow classes carried an abbreviation instead of the class
name, so an object printed as `SUP_COA@availability` rather than the
predictable `@supply`:

| class | slot | constructor argument |
|---------------|------------------------|--------------------------|
| `demand` | `@dem` → `@demand` | `dem =` → `demand =` |
| `supply` | `@availability` → `@supply` | `availability =` → `supply =` |
| `import` | `@imp` → `@import` | `imp =` → `import =` |
| `export` | `@exp` → `@export` | `exp =` → `export =` |

-   `demand`'s value **column** is renamed too (`dem` → `demand`), since
    it shares the slot's name. The other three keep their short column
    prefixes (`ava.lo/up/fx`, `imp.*`, `exp.*`), and `price`/`cost` are
    unchanged — they encode a real distinction (market price for
    import/export vs. internal extraction cost for supply).

-   The break is **not symmetric**, which is worth knowing before you
    rely on it. `dem =`, `imp =` and `exp =` still work silently: each is
    a unique prefix of its new name and sits before `...`, so R
    partial-matches it and the data lands in the right slot.
    `availability =` is not a prefix of `supply` and fails loudly with
    `Unidentified slot(s): "availability"`. A `dem` **column** inside the
    data frame does fail, with `Unknown column "dem" in the slot
    "demand"`.

*The `sub` class is now `subsidy`.* It was the only class whose name was
an abbreviation, it collided with `base::sub()`, and its own parameter
metadata already called it `subsidy`:

-   S4 class `sub` → **`subsidy`**; its main data slot `@sub` →
    `@subsidy` (columns `inp`/`out`/`bal` unchanged).
-   `newSubsidy()` is the constructor; `newSub()` remains as an alias, and
    `sub =` still partial-matches `subsidy =`.
-   The modInp set dimension `"sub"` and the parameter names
    `pSubCostInp` / `pSubCostOut` / `pSubCostBal` are **unchanged**.

*One vocabulary across the plotting layer.*

-   `geo_map()` → **`plot_map()`**. `geo_*` is `geoscales`' prefix;
    energyRt's map entry points are now `plot_map()` and
    `plot_trade_map()`.
-   The first argument of every `plot_*()` function and of the `draw()`
    generic is now **`object`** (was a mix of `object`, `x`, `obj` and
    `scen`). `plot()` keeps `x, y` — base R fixes those.
-   **`years` → `year`**, matching `getData()`, `getMix()` and the `year`
    set.
-   **`type` now always means the quantity** shown
    (`getMix()`, `plot_map()`: generation/capacity/new_capacity/fuel).
    The chart *shape* is now **`style`**: `autoplot(dem, style = "area")`,
    `autoplot(wthr, style = "heatmap")`.
-   `plot_demand()`, `plot_weather()` and `plot_process_windows()` are no
    longer exported — all three are reachable through `autoplot()`, and
    two public names for one chart is what this pass removed. Still
    exported are the plotters no generic reaches: `plot_heatmap()`
    (accepts a data frame or a named vector), `plot_map()`,
    `plot_trade_map()` and `plot_share_frontier()`.

*What replaces the un-exported functions.* `plot()` now delegates to
`autoplot()` for every class that has one — 17 S4 classes plus `levcost`
and `levcost_list` — so whichever verb you reach for works and
`autoplot()` stays the single implementation. `draw()` keeps the
schematic-diagram role and still needs no ggplot2. New
`theme_energyRt()` is the one place the package's ggplot look is set.

-   `autoplot()`, `autoplot(model)` and `autoplot(repository)` now fail
    with energyRt's own message when ggplot2 (a **Suggests**) is absent,
    instead of R's bare `there is no package called 'ggplot2'`.

**`levcost()` prices vintages and clusters separately**

-   `levcost()` on a technology declaring `@vintage` or `@cluster` was
    **silently wrong**. Its mini-model prices one process against a unit
    demand, but `interpolate_model()` expands a vintaged technology into
    one process per cell, so the cells competed for that single unit and
    the extraction summed across them. No error — just one number where
    there should have been one per vintage.

-   Each cell is now priced in **its own region** (`IND_VIN2030`), so the
    variants cannot serve each other's demand, and results are extracted
    by filtering to that region. Region is the only axis that works:
    `vTotalCost` has no `tech` dimension but is on a region × year grid,
    so per-cell cost can only be attributed regionally.

-   Such a technology now returns a **`levcost_variants`** object — a
    named list of ordinary `levcost` results, one per variant, which
    `autoplot()` compares directly. `levcost_by_variant(x, what =)`
    stacks the per-variant tables with `vintage`/`cluster` keys. A
    technology with no vintages or clusters is **unaffected** and still
    returns a single `levcost` object.

-   New `run = c("single", "sequential")` argument. `"single"` (default)
    prices every variant in one model; if that does not solve it retries
    with the dummy-import slack and then falls back to one model per
    variant, so one bad cell cannot take the rest down. `"sequential"`
    goes straight to per-variant models. `max_failures` (default 10)
    bounds the fallback.

-   `$frontier` is `NULL` on the per-variant path: the frontier corners
    are a per-commodity sweep, orthogonal to variants, and are not
    fanned out per cell.

**Fixes**

-   `tech_from_spec()` read `@input$combustion` back as character, so any
    technology that sets it wrote a perfectly valid techspec that then
    failed to load with *"Unexpected data format (character) ... expecting
    numeric"*. `combustion` is a numeric share, not a dimension label, and
    had been listed among the character columns in `.techspec_rows_df()`.
    Round-tripping such a technology through `tech_to_spec()` /
    `tech_from_spec()` now works.

-   `size()` regained its `@export`: a helper inserted between its
    roxygen block and the function had silently taken over the block, so
    the next `document()` would have dropped `size` from `NAMESPACE` and
    exported `.instance_slots` instead.

**Mixed-resolution commodities (`commodity@geolevel`)**

-   A commodity can now be **balanced at a coarser geoscale level than
    the model's own regions** — steel nationally while electricity stays
    per-state, in one model. `newCommodity("STEEL", geolevel = "nation")`.
    This is the spatial twin of `commodity@timeframe`, and it removes the
    old workaround of modelling the commodity per region plus trade
    routes between every pair purely to let it move.

-   Commodities remain **region-invariant objects**: there is still no
    region slot. `@geolevel` declares only *at what spatial resolution
    the balance is written*. It defaults to the finest level, which is
    exactly today's behaviour.

-   Balancing a commodity at a coarse level asserts **free, unlimited
    transport of it within that level**. That suits a good with a
    genuinely integrated market; it is never right for a
    network-constrained carrier such as electricity. The test suite pins
    this: a coarse balance must give the same objective as the same
    system modelled with per-region commodities plus a zero-cost,
    unlimited trade route.

-   Implementation reuses the balance layer the aggregation rewrite
    already built for time. Two new maps — `mRegionFamily` (immediate
    parent→child regions, the spatial `mSliceFamily`) and `mCommRegion`
    (the level a commodity is balanced at) — plus one extra term in
    `eqOutTot`/`eqInpTot`. There is deliberately **no `pRegionAgg`**:
    `pSliceAgg` exists only because slice values are intensive rates
    needing renormalisation, whereas regional quantities are extensive
    and simply add up.

-   With a geoscale attached, `sets$region` holds **every level at once**
    — the regions, the zones and the nation together — mirroring how
    `sets$slice` already holds the slices of every timeframe. The coarse
    members are inert until a commodity names one. A geoscale covering
    more ground than the model (a world map for a two-region model) is
    pruned, so only ancestors of declared regions enter the set.

-   A process may sit at a coarse region (national demand for a
    nationally-balanced commodity), but **never coarser than a commodity
    it uses** — its flows could not reach that commodity's balance and
    would silently vanish. This is now an error, mirroring the
    finest-timeframe rule for processes.

-   **GLPK and GAMS only.** Julia and Pyomo do not carry the aggregation
    term, so writing a model with a non-default `@geolevel` for them
    raises rather than silently solving a different problem — the same
    treatment `payback` already gets.

-   *Note:* attaching a geoscale makes `pWacc`, `pSdr` and
    `pDiscountFactor` slightly larger on disk. Their `ANYREGION`
    wildcard would now also cover the coarse regions, so folding
    correctly declines and the values stay explicit. Same values, same
    LP.

**Geographic information for model regions**

-   A model can now carry a **geoscale** — a
    [`geoscales::Geoscale`](https://github.com/optimal2050/geoscales)
    describing how its regions nest into coarser levels, what they
    weigh, and where they are on a map. Attach one with
    `newModel(..., geoscale = gs)` or `setGeoscale()`, and read it back
    with `getGeoscale()`.

-   `config@region` stays **authoritative** and is always the finest
    level. A region the geoscale does not cover is a warning at
    interpolation time, not an error. (When this feature first landed a
    geoscale was presentation-only; `commodity@geolevel` above now also
    gives it a role in the optimisation model.)

-   `geoscales` is an optional (**Suggests**) dependency. Storing,
    printing, saving, interpolating and solving a model that carries a
    geoscale all work without it installed; only maps and region-level
    aggregation need the package, and they say so.

-   **`geo_map()`** draws a choropleth of results — `"generation"`,
    `"capacity"`, `"new_capacity"` or `"fuel"` — over the model's
    geography, optionally aggregated to a coarser level.

-   **`plot_trade_map()`** now accepts a `Geoscale` as its `map`, falls
    back to the model's own geoscale when none is given, and can draw at
    a coarser `level` (routes internal to an aggregate region drop out).

-   *Bug fix:* `plot_trade_map()` drew routes with `geom_segment()` on
    raw `x`/`y` alongside a `geom_sf()` layer. That works only when the
    map has no CRS, which is true of the reference `utopia$map` layouts
    but not of real data — with a CRS, `geom_sf()` installs a
    `coord_sf()` that reprojects the polygons and leaves the routes
    behind. Routes, centroids and labels are now drawn as `sf` layers
    whenever the map is projected.

-   Aggregation across regions is delegated to `geoscales::geo_recast()`,
    with the rules read off the **variable catalogue** rather than
    hardcoded — the same approach `.is_state_var()` takes for the
    temporal roll-up. `vTradeIr` is identified by its declared
    `role: flow` and is **netted**, not summed: a flow between two
    regions that end up in the same aggregate is internal to it and
    cancels. Note `role: stock` is a *temporal* exclusion only — a
    storage level must not be summed over slices, but summing it across
    regions is meaningful and is done.

-   `utopia_geoscale()` builds a geoscale for the UTOPIA reference model
    (`nation -> zone -> region` over `R1`…`R11`), with geometry from any
    of the four `utopia$map` layouts. The hierarchy itself ships as the
    plain table `utopia$geo`, so `data/` carries no class from an
    optional package.

-   `getCalendar()` was declared as a generic but had no methods, so
    `getCalendar(mod)` failed. Methods added for `config`, `model` and
    `scenario`, alongside the new `getGeoscale()`.

# energyRt 0.70.4.9000-dev

**A `variable` class, mirroring `parameter`**

-   Model variables are now S4 objects. `scenario@modOut@variables` is a
    named list of `variable` objects, exactly as
    `scenario@modInp@parameters` is a list of `parameter` objects, and
    both now extend a shared virtual class `modelData` that carries the
    in-memory / on-disk storage contract.

-   A `variable` knows what it is: its model dimensions, the column names
    it is written out with, its gating map, whether it is declared
    positive, its `role` (`source`, `sink`, `flow`, `activity`,
    `balance`, `stock`, `capacity`, `cost`), a `unit` kind, and whether
    it comes from the solver or is computed in R. The specification is
    composed at build time from the GAMS source, the GLPK template and
    the new `data-raw/variables.yml` overlay, which is validated for
    completeness so it cannot fall behind the model.

-   `modOut` pre-populates every declared variable, so a variable the
    solver skipped (no non-zero values) still reports its column names
    instead of being absent entirely.

-   Facts that used to be hard-coded now come from the specification: the
    "never sum this over slices" rule (was
    `.timeframe_state_vars <- c("vStorageStore")`) and the sign of each
    series in a generation mix.

-   `getData()` is unchanged for callers.

**Breaking changes**

-   `scenario@modOut@variables$vTechCap` returns a `variable`, not a
    data.frame. `$`, `[[`, `dim()`, `names()` and `as.data.frame()` work
    on it, so reading `...$vObjective$value` still works; `merge()`,
    `rbind()` and `colnames<-` do not. Use `getData()` as before, or
    `get_variable(scen, "vTechCap")`.

-   The on-disk scenario layout changed: a variable's data moved from
    `variables/<name>/` to `variables/<name>/data/`. Scenario directories
    now carry a `layout` file, and `load_scenario()` refuses an older
    one rather than silently reading it as empty. Re-save affected
    scenarios.

-   `vTechRetiredNewCap` read from a GDX now has its second year column
    named `yearp`, matching every other engine. The `src`/`dst` renaming
    for `vTradeIr` is likewise applied to all engines rather than to GDX
    alone.

-   `vDummyImportCost` / `vDummyExportCost` are the solver's own values.
    An R recomputation used to overwrite them after every solve with an
    unweighted, slice-resolved version whose shape did not match the
    declaration; it has been removed.

-   `parameter@misc$nValues` (a cached row count) is gone. It was used to
    **truncate** a parameter's data in the GAMS, Pyomo and Julia writers
    while GLPK never truncated, so a stale count made those engines emit
    less data than GLPK for the same scenario.

**Bug fixes**

-   `summary()` on a solved scenario never reported dummy import/export
    costs, because it read a variable name that has never existed. Dummy
    flows mask infeasibility, so this was invisible to every user.

-   `model_structure` attached each variable's dimensions and gating map
    to the *wrong* variable: it took names from one generated table and
    dims from another, and the two carry the same names in a different
    order.

-   `model_size()` counted a gating map's rows once however many
    variables it gated, understating models whose maps gate several
    variables (`mvStorageStore` gates three).

-   `.get_data_slot()` read a parameter's data slot directly, so all of
    its call sites saw zero rows for an on-disk parameter. It is now the
    same on-disk-aware reader as `get_data_slot()`.

-   `update_parameter()` built an on-disk path missing its `parameters`
    segment, pointing at a directory that never exists.

# energyRt 0.70.3.9000-dev

**Two discount rates: WACC and SDR**

-   A model now has **two** rates rather than one. `wacc` (weighted
    average cost of capital) annuitises investment into the equivalent
    annual cost; `sdr` (social discount rate) discounts the stream of
    system costs in the objective. They are independent, with **no
    fallback from one to the other**.

-   `discount` survives as an **argument only**, the shorthand for the
    simple case where one rate plays both roles:
    `newModel(discount = 0.05)` is exactly
    `data.frame(wacc = 0.05, sdr = 0.05)` and every existing model is
    unaffected. Per row you supply either `discount`, or both `wacc` and
    `sdr`; a partial pair or a mix of the two forms is an error.

-   Technologies, storages and trades can carry their own
    `@invcost$wacc`, which overrides the model-wide rate when annuitising
    that process. There is deliberately no per-process `sdr` --- a social
    discount rate is a property of the model, not of a technology.

-   `@invcost$eac` is now honoured: supply the annuity directly and it is
    used verbatim, in place of annuitising `invcost`. Rows left empty are
    computed as before.

-   New `@invcost$payback`, the **cost-recovery period**. Where given it
    replaces the operational life both in the annuity and in the years
    over which the annuity is charged, so the investment is repaid over
    `payback` years while the capacity keeps operating for its full
    `@vintage$olife`. Must be positive and no longer than `olife`.
    Implemented for **GLPK only**; the GAMS, Pyomo and Julia writers
    refuse a model that sets it. A payback that does not land on a
    milestone boundary is reported with the span actually charged.

**Breaking changes**

-   `config@discount` no longer has a `discount` column; the table holds
    `region`, `year`, `wacc` and `sdr`. Code that read the column should
    read `wacc` or `sdr` according to which job it means.

-   The `pDiscount` parameter is replaced by `pWacc` and `pSdr`.
    `pDiscountFactor` is unchanged in name and meaning but is now built
    from `sdr`.

-   `pTechEac`, `pStorageEac` and `pTradeEac` now read `@invcost$eac`
    instead of `@invcost$invcost`. This also resolves a latent collision
    in which `pTechInvcost` and `pTechEac` registered the same
    class/slot/column key.

-   `levcost()` annuitises at the `wacc` rather than at whichever of
    `sdr`/`wacc`/`discount` happened to be set first; `report()` prints
    both rates under their own names.

**Capacity vintage and cluster**

-   Process classes (`technology`, `storage`, `trade`) can now describe a
    *group* of similar processes whose characteristics evolve over time
    (vintage) or across space/type (cluster). Add a `vintage` and/or
    `cluster` column to the data slots (`capacity`, `invcost`, `fixom`,
    `varom`, `af`, `ceff`, `geff`, `aeff`, ...) and the object is
    replicated into one ordinary process per cell before the model is
    built. Equations and set dimensions are unchanged, so vintaging
    costs nothing in model structure.

-   New `@vintage` slot on all three classes, with columns
    `(vintage, region, cluster, start, end, olife)`. It **replaces
    `@start`, `@end` and `@olife`**, which are removed as slots. The
    constructors and `update()` still accept `start=`, `end=` and
    `olife=` arguments and fold them into `@vintage`, so existing code
    keeps working; saved objects from earlier versions must be rebuilt.
    All three of `start`, `end` and `olife` now carry a `region`
    dimension.

-   New `@cluster` slot on `technology` and `storage`, declaring cluster
    names with an optional description, region and ordering. Clusters
    used only in the data slots are still picked up, so the slot is
    optional. `trade` has no cluster dimension -- `@routes` already
    provides the multiplicity.

-   Variant names are formed by suffixing the base name, e.g.
    `ECOA_VIN2030_CLnorth`. The prefixes are controlled by
    `@config@variant_prefix` (default `c(vintage = "_VIN", cluster =
    "_CL")`), inherited by `@settings`, with a collision check across
    all process names and a consistency check if both copies are set.

-   `getData()` gains `variants = TRUE` (the default), attaching
    `base`, `class`, `vintage` and `cluster` columns to results keyed on
    a process. New `getVariants(scen, class = )` and `variantSummary()`
    report the expansion; the provenance table lives in
    `modInp@sets$variant`.

-   A `vintage = "TOTAL"` (or `cluster = "TOTAL"`) row in `@capacity`
    bounds the **sum** over the variants of that process rather than
    each one, generating a single group constraint.

-   `storage@cap2stg` is promoted from a scalar to a data.frame
    `(vintage, cluster, region, year, cap2stg)`.

**Breaking changes**

-   `trade@capacityVariable` has been **removed**: a trade's capacity is
    always a decision variable, as for technology and storage. In this
    version the flag gated nothing --- the capacity variable, its
    equations, and the investment and fixed O&M charges were built
    either way; its only remaining effect widened the flow domain. For a
    fixed transfer limit with no investment, drop `@capacity`/`@invcost`
    and set `trade = data.frame(src =, dst =, ava.up = <limit>)`.
    Passing `capacityVariable` to `newTrade()` or `update()` now errors
    with that remedy. The `mTradeCapacityVariable` mapping is gone.

-   `@start`, `@end` and `@olife` are no longer slots (see `@vintage`
    above); read sites must use `@vintage` or the `.proc_lifespan()`
    helper.

-   Passing an unrecognised slot name to a constructor now errors
    instead of being silently dropped.

**Bug fixes**

-   A per-region group capacity bound was applied to each region's
    variants separately instead of to their sum, letting a region build
    far past its declared cap.

-   `getData()` returned no rows for parameters selected with a wildcard
    `slice = NA`.

-   `check_name()` had inverted guards, so some invalid names passed and
    some valid ones were rejected.

-   `pTradeIrEff` declared `slot: teff`, but `teff` is a *column* of
    `@trade`, so the parameter was never populated.

# energyRt 0.50.9-dev

**New features & critical changes in the model code:**

-   Weighting of time-slices in a subset has been revised and rewritten.
    Since this version, all variables with 'slice' dimension are not
    weighed for consistency of slice-level operation across sampled and
    non-sampled model runs.

-   Slice-weights can vary across model years.

-   All variables with 'year' dimension are not weighted to the interval
    lengths. \
    Exception:

    -   cumulative variables (`vSupReserveCum`, etc.) which have to
        account the interval length for every milestone year. 

    -   capacity variables (`vTechCap`, etc.) represent the state of the
        variable by the end of the period, including any accumulation or
        retirement of capacity over the interval of the milestone year.

-   New capacity variables (`vTechNewCap`, etc.) are given for a year.
    Period length (`pPeriodLen`) must be applied to the annual capacity
    additions (such as `vTechNewCap`) to get total new capacity of a
    process by the end of each period.

-   System costs have been regrouped by type (capital, fixed O&M,
    variable O&M, supply, taxes, subsidies) and by process type
    (technology, trade, storage, etc.) to facilitate the analysis of the
    cost structure. Total Costs equation has been rewritten to reflect
    the new cost structure.\
    \
    **Bug fixes**

-   Early retirement option (`optimizeRetirment = TRUE`) is corrected to
    exclude retirement of "new" technologies at the same time as their
    installation.

-   `draw()` method for 'trade' is fixed to exclude repeated arrows in
    the plot.

-   `newCosts()` is debugged, an example is added to the Utopia
    tutorial.

-   `tsl2hour()` fixed to be able identify n-digits hours (previously
    worked for 2 only).\
    \
    **Miscellaneous**

-   A new version (3) of the logo design idea (by DALL-E).

-   The code clean-up, testing, and documentation are in progress to
    comply with CRAN requirements.

-   Functions/method in progress: `levcost()` and `report()`,

-   `add_weights` and `add_intervals` arguments will be added to
    `getData()` function to add time-slice weights and interval lengths
    to the requested data if applicable.

# energyRt 0.50.7-dev

-   Fixed a few stability issues in the `draw()` method.
-   Added "Hello World" example to the tutorial.
-   A new version of the logo design idea (by DALL-E).
-   The code clean-up and documentation are in progress to comply with
    CRAN requirements.
-   The very first draft of the package CRAN-like
    [manual](https://github.com/optimal2050/energyRt/blob/master/man/figures/energyRt-manual.pdf)
    is added.
-   The version might be unstable due to ongoing changes.

# energyRt 0.50.6-dev

-   draw() is drafted for all processes: 'technology', 'export',
    'import', 'supply', 'demand', 'trade', 'storage
-   docs completed for main classes with examples.
-   code clean-up and documentation in progress.

# energyRt 0.50.5-dev

-   draw() is rewritten based on 'grid' package, and is now a generic
    method.
-   added draw() methods for 'technology', 'export', and 'import'
    classes.
-   fixed several interface-level bugs introduced in 0.50.4-dev during
    clean-up and documentation.

# energyRt 0.50.4-dev

-   Documentation of classes is in progress (\~70% docs completed).
-   Logo-search has started! "logo" page added.
-   Website is reshaped, added new, not populated yet "articles".
-   !!! Not Tested!!! Due to the ongoing changes in both documentation
    and functions/methods clean-up, the version may have "surprises" -
    tests are in progress.

# energyRt 0.50.3-dev

-   Development version in the preparation for CRAN submission.
-   Added a `NEWS.md` file to track changes to the package.
-   Added functions to document classes from yaml file 'classes.yaml'.
-   `technology-class` and `newTechnology` function documented.
