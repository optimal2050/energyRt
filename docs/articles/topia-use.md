# TOPIA II: running scenarios

## Overview

This vignette takes the packaged \[`topia`\]`$modules` model – the one
built object by object in *TOPIA I: building the model* – assembles it
and runs scenarios on it.

``` r

library(energyRt)
library(dplyr)
library(ggplot2)
have_sf  <- requireNamespace("sf", quietly = TRUE)        # maps need sf
have_pal <- requireNamespace("energypal", quietly = TRUE) # palette + order
set_scenarios_path(file.path(tempdir(), "topia"))
set_default_solver(solver_options$glpk)   # set once, used by every solve
set_default_solver(solver_options$julia_highs)             
# get_default_solver()
```

Two calls run every scenario in this vignette:
[`interpolate_model()`](https://energyRt.org/reference/interpolate_model.md)
expands the model over regions, years and timeslices;
[`solve_scenario()`](https://energyRt.org/reference/solve_model.md)
writes the solver files, runs the default solver (set once above) and
reads the solution. `verbose = FALSE` keeps the solver log out of the
page — drop it to watch the solver work.

The horizon marks 2025 as its base year, so each scenario is **two**
solves and not one: the base year on its own, then the projection that
starts from it.
[`solve_base_year()`](https://energyRt.org/reference/solve_base_year.md)
does the first, and `base_year_scenario =` hands its result to the
second.

## The TOPIA model

`topia$modules$electricity` carries the model: a base repository
`$repo`, the regions it covers, its geoscale, and the scenario levers
used below. Calendars, horizons and map layouts sit alongside it. (For a
different layout or calendar, build the blocks yourself following *TOPIA
I: building the model*.)

``` r

topia_elc <- topia$modules$electricity           # the model
# A SAMPLED calendar: one day per month, every hour of it, drawn from the
# model's own `d365_h24`. The kit's weather is measured on that calendar, so a
# sample of its labels resolves against the same series while the LP shrinks
# about thirty-fold -- which is what makes this vignette runnable. `s4_h24`
# would not: its labels (`WIN_h00`) are not `d365_h24`'s (`d001_h00`), so
# every weather row would be dropped and the renewables would run unbounded.
calendar_sample <- calendars[["d365_h24_1dpm"]]
horizon_BY2025_50_by10 <- topia$modules$horizons$BY2025_50_by10
topia_elc$regions
```

## Assembling and solving the base model

``` r

set_progress_bar()
mod <- newModel("TOPIA", data = topia_elc$repo,
                calendar = calendar_sample,
                region = topia_elc$regions,
                horizon = horizon_BY2025_50_by10, discount = 0.05)
mod <- setGeoscale(mod, topia_elc$geoscale)
```

### The base year

2025 is given, not chosen, so it is solved by itself.
[`solve_base_year()`](https://energyRt.org/reference/solve_base_year.md)
slices the base year out of the horizon, switches endogenous retirement
off – an observed retirement is data – and returns the solved scenario
together with the ledger of what it decided.

A well-stated base year builds nothing: every generator standing in 2025
is already in the fleet. New capacity here means the input data
understates the system, and capacity left idle means it overstates it.

``` r

by <- solve_base_year(mod, name = "BY2025", verbose = FALSE)
getData(by, "vObjective", merge = TRUE)   # the base year's own cost, MUSD

# nothing should appear here
new_by <- getData(by, "vTechNewCap", merge = TRUE)
new_by[new_by$value > 1e-6, ]
```

### The projection

`base_year_scenario =` carries the base year forward: what it decided
becomes exogenous stock, and the solved years are 2030, 2040 and 2050.

``` r

scen_BASE <- interpolate_model(mod, name = "BASE", base_year_scenario = by,
                               verbose = TRUE, check = TRUE)
scen_BASE@modInp@sets$year                # 2030, 2040, 2050

scen_BASE <- solve_scenario(scen_BASE, verbose = TRUE)
getData(scen_BASE, "vObjective", merge = TRUE) # projection cost, MUSD
```

The regions are wired west to east with bi-directional transmission
links (`TBD_ELC_*`), and the fuels move zone to zone.
[`gg_trade_map()`](https://energyRt.org/reference/gg_trade_map.md) draws
that network over a map layout — pass any of `topia$geoscales`, here
`honeycomb`:

``` r

gg_trade_map(topia_elc$repo, map = topia$geoscales$honeycomb)
```

## Reading the solution

### The electricity balance

Generation is only one side of the account. The bands below are drawn
from the `carriers` palette in **energypal**, which supplies the colours
*and* the order: `merit_order` is a sequence the palette declares, so
neither is invented here. TOPIA’s own codes are mapped onto that
vocabulary explicitly – a fuzzy lookup mis-reads them, matching `NUC` to
nothing and `EGAS` to enhanced geothermal.

``` r

pal <- energypal::energypal("carriers", order = "merit_order",
                            include_groups = FALSE, include_subtypes = FALSE)

carrier_of <- c(ESOL = "Solar", EWIN = "Wind", EHYD = "Hydro",
                ENUC = "Nuclear", EBIO = "Bioenergy",
                ECOA = "FossilCoal", EGAS = "FossilGas")

stem <- function(x) sub("_(VIN|CL).*$", "", x)   # drop vintage/cluster suffixes
```

Sources stack above zero and consumers below it. Electricity leaves the
system through three doors: final demand (`vDemInp`), the electrolyser
drawing power to make hydrogen (`vTechInp`), and storage charging
(`vStorageInp`).

``` r

gen <- getData(scen_BASE, "vTechOut", comm = "ELC", merge = TRUE)
elc_use <- getData(scen_BASE, "vTechInp", comm = "ELC", merge = TRUE)
elc_dem <- getData(scen_BASE, "vDemInp", comm = "ELC", merge = TRUE)
elc_sto <- getData(scen_BASE, "vStorageInp", comm = "ELC", merge = TRUE)

sources <- gen |>
  mutate(band = unname(carrier_of[stem(tech)])) |>
  group_by(year, band) |>
  summarise(GWh = sum(value), .groups = "drop")

consumers <- bind_rows(
  elc_dem |> mutate(band = "Electricity"),
  elc_use |> mutate(band = "Hydrogen"),
  elc_sto |> mutate(band = "Storage")) |>
  group_by(year, band) |>
  summarise(GWh = -sum(value), .groups = "drop")

balance <- bind_rows(sources, consumers)
balance$band <- factor(balance$band, levels = names(pal))

ggplot(balance, aes(factor(year), GWh, fill = band)) +
  geom_col() +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  scale_fill_manual(values = pal, breaks = names(pal), drop = TRUE) +
  labs(x = "year", y = "GWh", fill = "carrier",
       title = "The electricity balance (BASE)",
       subtitle = "sources above zero, consumers below") +
  theme_bw()
```

Generation runs well above final demand, and the chart says why: the
electrolyser is a load, and a large one. Three further readings of the
same figure are worth making explicit.

- The **storage band is empty** – the battery is never built. At a
  sampled calendar the multi-day calm spells that make storage valuable
  are not present to be found, and the model has cheaper ways to meet a
  peak.
- **Trade does not appear as a consumer.** `vTradeIr` moves electricity
  between regions rather than out of the system, and no electricity is
  exported to the rest of the world, so at system level trade shows up
  only as the small residual of line losses. Per region it is a genuine
  source and sink.
- The gap between the two sides is exactly that residual: 3% is lost at
  each transmission hop.

``` r

balance |>
  mutate(GWh = round(GWh)) |>
  arrange(band) |>
  tidyr::pivot_wider(id_cols = band, names_from = year, values_from = GWh,
                     values_fill = 0) |>
  knitr::kable(caption = "The electricity balance, GWh (consumers negative)")
```

### The colour of the hydrogen

Hydrogen is conventionally described by the electricity that made it:
green from renewables, pink from nuclear, grey from unabated fossil
fuel. The model itself supports no such label. Electricity is a pooled
commodity, so the solution records how much each plant generated in a
region-hour and how much the electrolyser drew in the same region-hour,
and nothing about which flowed into which. A colour is therefore a
**convention laid over the solution**, and the one used here is the
hourly average mix: the draw in each hour is divided among the plants
generating in that hour, in proportion to their output.

The hourly resolution carries the argument. An electrolyser exists
partly to absorb surplus, so an annual average would credit it with the
year’s mix and conceal whether it actually ran when the wind blew.
`timeframe = "highest"` keeps the timeslice detail that
[`getData()`](https://energyRt.org/reference/getData.md) otherwise
aggregates away.

``` r

gen_h <- getData(scen_BASE, "vTechOut", comm = "ELC", merge = TRUE,
                 timeframe = "highest")
use_h <- getData(scen_BASE, "vTechInp", comm = "ELC", merge = TRUE,
                 timeframe = "highest")

colour_of <- c(Solar = "GreenHydrogen", Wind = "GreenHydrogen",
               Hydro = "GreenHydrogen", Nuclear = "PinkHydrogen",
               Bioenergy = "OtherHydrogen", FossilCoal = "GreyHydrogen",
               FossilGas = "GreyHydrogen")

draw <- use_h |>
  filter(stem(tech) == "H2ELC") |>
  group_by(region, year, timeslice) |>
  summarise(drawn = sum(value), .groups = "drop")

h2_colour <- gen_h |>
  group_by(region, year, timeslice) |>
  mutate(share = value / sum(value)) |>
  ungroup() |>
  inner_join(draw, by = c("region", "year", "timeslice")) |>
  mutate(colour = unname(colour_of[carrier_of[stem(tech)]]),
         GWh = share * drawn) |>
  group_by(year, colour) |>
  summarise(GWh = sum(GWh), .groups = "drop")

ggplot(h2_colour, aes(factor(year), GWh, fill = colour)) +
  geom_col(position = "fill") +
  scale_fill_manual(values = energypal::energypal_colors(
    unique(h2_colour$colour), palette = "carriers")) +
  labs(x = "year", y = "share of the electricity consumed", fill = "colour",
       title = "The colour of the hydrogen (BASE)",
       subtitle = paste("the electrolyser's draw attributed to the",
                        "hourly generation mix")) +
  theme_bw()
```

Most of the hydrogen is green, a third of it grey, and a small pink
share comes from the nuclear fleet. The attributed total matches the
electrolyser’s recorded draw, which is the check that the attribution
neither invents nor loses energy:

``` r

c(attributed = round(sum(h2_colour$GWh)),
  drawn = round(sum(elc_use$value)))
```

The national average hides the more instructive result. Attribution by
region shows the colour following whatever that region happens to
generate: hydrogen made beside the hydro reservoirs is almost entirely
green, hydrogen made in the coal regions is predominantly grey, and the
region holding the nuclear fleet produces hydrogen that is substantially
pink.

``` r

h2_region <- gen_h |>
  group_by(region, year, timeslice) |>
  mutate(share = value / sum(value)) |>
  ungroup() |>
  inner_join(draw, by = c("region", "year", "timeslice")) |>
  mutate(colour = unname(colour_of[carrier_of[stem(tech)]]),
         GWh = share * drawn) |>
  filter(year == max(year)) |>
  group_by(region, colour) |>
  summarise(GWh = sum(GWh), .groups = "drop_last") |>
  mutate(share = round(100 * GWh / sum(GWh), 1)) |>
  ungroup()

h2_region |>
  tidyr::pivot_wider(id_cols = region, names_from = colour,
                     values_from = share, values_fill = 0) |>
  knitr::kable(caption = "The colour of the hydrogen by region, final year (%)")
```

The convention is one of several defensible ones, and the choice changes
the answer. An average attribution of this kind credits the electrolyser
with the mix it ran alongside. A *marginal* attribution would instead
ask which plant changed its output because the electrolyser ran, which
requires either the duals or a counterfactual solve. A stricter
hourly-matching rule would count only renewable generation in the same
hour and leave the remainder uncoloured. None is more correct than the
others; what matters is that the rule is stated alongside the number.

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
always `interpolate_model(mod, name, LEVER, base_year_scenario = by)`
then
[`solve_scenario()`](https://energyRt.org/reference/solve_model.md).
Every scenario starts from the same base year: 2025 is an observation,
so a lever cannot change it.

``` r

# emissions cap
scen_CO2CAP <- interpolate_model(mod, "CO2CAP", topia_elc$CO2_CAP,
                                 base_year_scenario = by) |>
  solve_scenario(verbose = FALSE)
# carbon tax
scen_CTAX <- interpolate_model(mod, "CARBONTAX", topia_elc$CT_CO2,
                               base_year_scenario = by) |>
  solve_scenario(verbose = FALSE)
# early fossil retirement
scen_ERET <- interpolate_model(mod, "EARLY_RET", topia_elc$EARLY_RET,
                               base_year_scenario = by) |>
  solve_scenario(verbose = FALSE)
# renewable-backed hydrogen
scen_GRN <- interpolate_model(mod, "GREEN_H2", topia_elc$GREEN_H2,
                              base_year_scenario = by) |>
  solve_scenario(verbose = FALSE)
```

The third lever, `EARLY_RET`, puts a declining per-region ceiling on
installed coal and gas capacity – the plants carry
`optimizeRetirement = TRUE`, so the model chooses *which* units to close
first while the constraint sets the pace. The ceiling bites hardest on
gas; the generation chart above shows what fills the gap.

Whether the battery is built at all is worth reading as a result rather
than assumed. The model carries several ways to meet a peak – a legacy
gas fleet alive to 2050, unlimited LNG above the domestic cap,
transmission to a neighbour, and an electrolyser that can soak up
surplus and sell the hydrogen – so shifting electricity in time has to
beat all of them before it earns its capital. Check `vStorageOutCap`
across the scenarios before concluding either way, and note that on a
sampled calendar the multi-day calm spells that make storage valuable
are not present to be found: the sample takes one day per month, so
nothing in it lasts longer than a night.

Compare the scenarios with
[`compare_scenarios()`](https://energyRt.org/reference/compare_scenarios.md):
the overview carries every run’s objective (with differences against the
baseline), the value tables flag which solved variables moved, and
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
draws the comparison charts. The full comparative document is one
`report(cmp)` call away.

``` r

scl <- list(BASE = scen_BASE, CO2CAP = scen_CO2CAP,
            CARBONTAX = scen_CTAX, EARLY_RET = scen_ERET,
            GREEN_H2 = scen_GRN)
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
different mechanisms – a price the model responds to, against a quantity
it must meet – and the two reach a given level of abatement at different
cost. That contrast is the point of running them side by side.

### Requiring green hydrogen, and what that does not buy

The `GREEN_H2` lever makes the matching rule of the previous section
binding rather than descriptive: in every region and timeslice, the
electrolyser’s draw may not exceed renewable generation at that moment.
It is the test used to certify renewable hydrogen, and it is costly –
hydrogen output falls and total system cost rises, both visible in the
comparison above.

What it does not do is turn the attribution wholly green.

``` r

colour_share <- function(scen) {
  g <- getData(scen, "vTechOut", comm = "ELC", merge = TRUE,
               timeframe = "highest")
  u <- getData(scen, "vTechInp", comm = "ELC", merge = TRUE,
               timeframe = "highest")
  d <- u |>
    filter(stem(tech) == "H2ELC") |>
    group_by(region, year, timeslice) |>
    summarise(drawn = sum(value), .groups = "drop")
  g |>
    group_by(region, year, timeslice) |>
    mutate(share = value / sum(value)) |>
    ungroup() |>
    inner_join(d, by = c("region", "year", "timeslice")) |>
    mutate(colour = unname(colour_of[carrier_of[stem(tech)]]),
           GWh = share * drawn) |>
    group_by(colour) |>
    summarise(GWh = sum(GWh), .groups = "drop") |>
    mutate(pct = round(100 * GWh / sum(GWh), 1))
}

base_mix <- colour_share(scen_BASE)
green_mix <- colour_share(scen_GRN)
base_mix |>
  select(colour, BASE = pct) |>
  full_join(select(green_mix, colour, GREEN_H2 = pct), by = "colour") |>
  knitr::kable(caption = "Attributed colour, whole horizon (%)")
```

The share attributed to renewables rises but does not reach 100%, and
the reason is that the constraint and the attribution answer different
questions. The constraint compares two quantities in the same hour:
electrolysis against renewable output. The attribution divides the
electrolyser’s draw among *everything* generating in that hour, and the
coal and gas plants serving the rest of the load are still running. A
single pooled commodity cannot say which generator supplied which
consumer, so one solution supports both statements at once – the
hydrogen is fully backed by renewable generation, and roughly a quarter
of the electricity mix it drew from was fossil.

Reconciling the two would require either a stricter rule – no fossil
generation anywhere in the region while the electrolyser runs, which
needs integer variables – or a physically separate green commodity that
only renewable capacity feeds. Both are heavier than the teaching point
they would buy, and neither is more honest than stating the convention.

## Levelized cost of a process

[`levcost()`](https://energyRt.org/reference/levcost.md) on a **solved
scenario** returns the *ex-post* levelized cost of a process: the
discounted sum of its own costs (annualised investment `vTechEac`, fixed
and variable O&M, and attributed fuel) divided by its output in that
solution. Because it reads the actual results – not a mini-model – it is
exact at the model’s time resolution, with no calendar caveat.

``` r

lc <- levcost(scen_BASE, name = "ENUC")      # the existing nuclear fleet
lc$levcost_npv                               # NPV levelized cost, MUSD/GWh
autoplot(lc, type = "components")            # eac / fixom / varom / supply, by year
```

`ENUC` runs on base-year capacity, so its **capital is already sunk** –
the annualised-investment term `eac` is ~0 and the realized cost is
**fixed O&M plus fuel**; a *newly built* plant would add its `eac` here.
The cost per GWh also depends on how hard a plant is run: more output
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

## Next steps

Extend TOPIA with technologies introduced via scenarios (CCS, CHP,
direct air capture), a finer calendar where storage and the hydrogen
chain have more to earn, and the other `topia$geoscales` layouts. See
the workshop exercises for a step-by-step build.
