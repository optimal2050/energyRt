---
editor_options: 
  markdown: 
    wrap: 72
---

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
