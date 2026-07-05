# UTOPIA II: running scenarios

## Overview

This vignette uses the packaged \[`utopia_modules`\] kit (built in
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
expands the model over regions, years and slices;
[`solve_scenario()`](https://energyRt.org/reference/solve_model.md)
writes the solver files, runs the default solver (set once above) and
reads the solution. `echo = FALSE` keeps the solver log out of the page
— drop it to watch GLPK work.

## Selecting a UTOPIA structure

`utopia_modules$electricity` offers ready region layouts (`reg1`,
`reg3`, `reg7`); each is a kit with a base repository `$repo` and
scenario levers. Calendars, horizons and maps sit alongside. (For a
custom layout or calendar, build the blocks yourself following *UTOPIA
I: building the model*.)

``` r

names(utopia_modules$electricity)          # available layouts
um  <- utopia_modules$electricity$reg3     # the 3-region base case
cal <- utopia_modules$calendars$utopia_s4h24
hor <- utopia_modules$horizons$base        # milestones 2020/2030/2040/2050
```

## Assembling and solving the base model

``` r

mod <- newModel("UTOPIA", data = um$repo, calendar = cal,
                region = um$regions, horizon = hor, discount = 0.05)
scen_BASE <- interpolate_model(mod, name = "BASE") |>
  solve_scenario(echo = FALSE)
getData(scen_BASE, "vObjective", merge = TRUE)$value    # total system cost, MEUR
```

The `reg3` kit wires its regions with bi-directional transmission links
(`TBD_ELC_*`).
[`plot_trade_map()`](https://energyRt.org/reference/plot_trade_map.md)
draws that network over a map layout — pass any of `utopia_modules$maps`
(here `honeycomb`); the 7-region kit shows six links:

``` r

plot_trade_map(um$repo, map = utopia_modules$maps$honeycomb)
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
for slice-level data):

``` r

autoplot(scen_BASE, "generation", slice = "^SUM_")
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

Compare CO2 emissions across scenarios:

``` r

scl <- list(BASE = scen_BASE, CO2CAP = scen_CO2CAP,
            CARBONTAX = scen_CTAX, RES_SHARE = scen_RES,
            EARLY_RET = scen_ERET)
lapply(scl, function(s) getData(s, "vEmsFuelTot", comm = "CO2", merge = TRUE)) |>
  bind_rows() |>
  group_by(scenario, year) |> summarise(ktCO2 = sum(value), .groups = "drop") |>
  ggplot(aes(factor(year), ktCO2, fill = scenario)) +
  geom_col(position = "dodge") +
  labs(x = "year", title = "CO2 emissions by scenario") + theme_bw()
```

Total system cost (objective) by scenario:

``` r

sapply(scl, function(s) round(getData(s, "vObjective", merge = TRUE)$value[1]))
```

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
```

## Other structures

The same code runs on a different layout – e.g. the single-region `reg1`
or the 7-region `reg7` (heavier). Just swap the kit:

``` r

um7 <- utopia_modules$electricity$reg7
mod7 <- newModel("UTOPIA7", data = um7$repo, calendar = cal,
                 region = um7$regions, horizon = hor, discount = 0.05)
scen7 <- interpolate_model(mod7, "BASE7") |>
  solve_scenario(echo = FALSE)
```

## Next steps

Extend UTOPIA with inter-regional trade (using the
`utopia_modules$maps`), technologies introduced via scenarios (CCS,
hydrogen, CHP), and the honeycomb multi-region layout. See the workshop
exercises for a step-by-step build.
