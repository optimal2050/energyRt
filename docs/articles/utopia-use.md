# UTOPIA II: running scenarios

## Overview

This vignette uses the packaged \[`utopia`\]`$modules` kit (built in
*UTOPIA I: building the model modules*) to assemble an energy-system
model and run scenarios with **GLPK** (bundled, no external solver
needed).

``` r

library(energyRt)
library(dplyr)
library(ggplot2)
have_sf <- requireNamespace("sf", quietly = TRUE)   # region maps need the sf package
set_scenarios_path(file.path(tempdir(), "utopia"))
set_default_solver(solver_options$glpk)             # set once, used by every solve
```

Two calls run every scenario in this vignette:
[`interpolate_model()`](https://energyRt.org/reference/interpolate_model.md)
expands the model over regions, years and timeslices;
[`solve_scenario()`](https://energyRt.org/reference/solve_model.md)
writes the solver files, runs the default solver (set once above) and
reads the solution. `echo = FALSE` keeps the solver log out of the page
— drop it to watch GLPK work.

## Selecting a UTOPIA structure

`utopia$modules$electricity` offers ready region layouts (`R1`, `R3`,
`R7`, `R11`); each is a kit with a base repository `$repo` and scenario
levers. Calendars, horizons and maps sit alongside. (For a custom layout
or calendar, build the blocks yourself following *UTOPIA I: building the
model*.)

``` r

names(utopia$modules$electricity)          # available layouts
um  <- utopia$modules$electricity$R3     # the 3-region base case
cal <- utopia$modules$calendars$s4_h24
hor <- utopia$modules$horizons$base        # milestones 2020/2030/2040/2050
```

## Assembling and solving the base model

``` r

mod <- newModel("UTOPIA", data = um$repo, calendar = cal,
                region = um$regions, horizon = hor, discount = 0.05)
scen_BASE <- interpolate_model(mod, name = "BASE") |>
  solve_scenario(echo = FALSE)
getData(scen_BASE, "vObjective", merge = TRUE)$value    # total system cost, MEUR
```

The `R3` kit wires its regions with bi-directional transmission links
(`TBD_ELC_*`).
[`plot_trade_map()`](https://energyRt.org/reference/plot_trade_map.md)
draws that network over a map layout — pass any of `utopia$modules$maps`
(here `honeycomb`); the 7-region kit shows six links:

``` r

plot_trade_map(um$repo, map = utopia$modules$maps$honeycomb)
```

Electricity generation by technology:

``` r

getData(scen_BASE, "vTechOut", comm = "ELC", merge = TRUE) |>
  group_by(year, tech) |> summarise(PJ = sum(value), .groups = "drop") |>
  ggplot(aes(factor(year), PJ, fill = tech)) + geom_col() +
  labs(x = "year", title = "Generation by technology (BASE)") + theme_bw()
```

The dispatch over a representative summer day – generation stacked by
technology, storage charging below zero, demand as the line
(`autoplot(scenario)` reads the solution via
[`getMix()`](https://energyRt.org/reference/getMix.md); note that
[`getData()`](https://energyRt.org/reference/getData.md) aggregates
sub-annual values to ANNUAL by default – pass `timeframe = "highest"`
for timeslice-level data):

``` r

autoplot(scen_BASE, "generation", timeslice = "^SUM_")
```

## Scenarios: layering the levers

Each scenario adds a pre-built lever to the base model. The pattern is
always `interpolate_model(mod, name, LEVER)` then
[`solve_scenario()`](https://energyRt.org/reference/solve_model.md):

``` r

scen_CO2CAP <- interpolate_model(mod, "CO2CAP", um$CO2_CAP) |>       # emissions cap
  solve_scenario(echo = FALSE)
scen_CTAX <- interpolate_model(mod, "CARBONTAX", um$CT_CO2) |>       # carbon tax
  solve_scenario(echo = FALSE)
scen_RES <- interpolate_model(mod, "RES_SHARE", um$RES_SHARE) |>     # renewable target
  solve_scenario(echo = FALSE)
scen_NUC <- interpolate_model(mod, "NO_NEW_NUC", um$NO_NEW_NUC) |>   # no new nuclear
  solve_scenario(echo = FALSE)
scen_ERET <- interpolate_model(mod, "EARLY_RET", um$EARLY_RET) |>    # early fossil retirement
  solve_scenario(echo = FALSE)
```

The fifth lever, `EARLY_RET`, puts a declining per-region ceiling on
installed coal and gas capacity – the plants carry
`optimizeRetirement = TRUE`, so the model chooses *which* units to close
first while the constraint sets the pace. The ceiling bites hardest on
gas (the flexible backstop for wind and solar), pushing investment into
nuclear, biomass and storage instead.

Compare the scenarios with
[`compare_scenarios()`](https://energyRt.org/reference/compare_scenarios.md):
the overview carries every run’s objective (with differences against the
baseline), the value tables flag which solved variables moved, and
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
draws the comparison charts. The full comparative document is one
`report(cmp)` call away.

``` r

scl <- list(BASE = scen_BASE, CO2CAP = scen_CO2CAP,
            CARBONTAX = scen_CTAX, RES_SHARE = scen_RES,
            EARLY_RET = scen_ERET)
cmp <- compare_scenarios(scl)
cmp
autoplot(cmp, "emissions")
```

Total system cost (objective) by scenario, straight from the overview:

``` r

with(cmp$overview, setNames(round(objective), scenario))
```

(The same charts can always be hand-rolled from the tidy layer –
`getData(scl, ...)` and `getMix(scl, ...)` accept named scenario lists
and add a `scenario` column.)

A carbon **tax** and a CO2 **cap** both cut emissions but through
different mechanisms (price vs quantity); a **renewable target** raises
cost and can even raise CO2 relative to a carbon price – a useful
contrast for policy discussion.

## Levelized cost of a process

[`levcost()`](https://energyRt.org/reference/levcost.md) on a **solved
scenario** returns the *ex-post* levelized cost of a process: the
discounted sum of its own costs (annualised investment `vTechEac`, fixed
and variable O&M, and attributed fuel) divided by its output in that
solution. Because it reads the actual results – not a mini-model – it is
exact at the model’s time resolution, with no calendar caveat.

``` r

lc <- levcost(scen_BASE, name = "ENUC")      # the existing nuclear fleet
lc$levcost_npv                               # NPV levelized cost, MEUR/PJ
autoplot(lc, type = "components")            # eac / fixom / varom / supply, by year
```

`ENUC` runs on base-year capacity, so its **capital is already sunk** –
the annualised-investment term `eac` is ~0 and the realized cost is
**fixed O&M plus fuel**; a *newly built* plant would add its `eac` here.
The cost per PJ also depends on how hard a plant is run: more output
spreads the fixed costs, lowering the levelized cost. Priced across
scenarios, `ENUC`’s LCOE tracks how much the existing fleet is
dispatched as the policy mix changes:

``` r

sapply(scl, function(s) {
  lc <- suppressMessages(levcost(s, name = "ENUC"))
  if (inherits(lc, "levcost")) round(as.numeric(lc$levcost_npv), 2) else NA_real_
})
```

(`levcost(scen, name)` attributes fuel from `vTechInp`, so it works for
the single-fuel technologies here; a plant with a *grouped* fuel input
would need its group activity instead.) A full technology datasheet with
the ex-post cost embedded is one call away
([`report()`](https://energyRt.org/reference/report.md) renders
HTML/PDF; needs pandoc):

``` r

report(scen_BASE, name = "ENUC", format = "html")

# the whole-scenario results report: run provenance (scenario_runs()),
# solution checks, generation/capacity charts, and a role-driven cost
# breakdown; `run =` reports any recorded run, not only the active one
report(scen_BASE)
```

## Other structures

The same code runs on a different layout – e.g. the single-region `R1`
or the larger `R7` / `R11` (heavier). Just swap the kit:

``` r

um7 <- utopia$modules$electricity$R7
mod7 <- newModel("UTOPIA7", data = um7$repo, calendar = cal,
                 region = um7$regions, horizon = hor, discount = 0.05)
scen7 <- interpolate_model(mod7, "BASE7") |>
  solve_scenario(echo = FALSE)
```

## Next steps

Extend UTOPIA with inter-regional trade (using the
`utopia$modules$maps`), technologies introduced via scenarios (CCS,
hydrogen, CHP), and the honeycomb multi-region layout. See the workshop
exercises for a step-by-step build.
