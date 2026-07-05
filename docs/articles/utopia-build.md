# UTOPIA I: building the model

## Overview

UTOPIA is a teaching model for energy-system optimization – a small
**Reference Energy System (RES)** mapping primary resources through
conversion technologies to electricity demand. This vignette builds
**every block** explicitly, one constructor call per object, so you can
see exactly how a model is assembled – and ends by packing the blocks
into a repository and a solvable model. The companion vignette *UTOPIA
II: running scenarios* loads a ready-made kit (\[`utopia_modules`\],
built by these same steps) and solves it.

All inputs are **deterministic** and shipped with the package.

``` r

library(energyRt)
library(dplyr)
library(ggplot2)
have_sf <- requireNamespace("sf", quietly = TRUE)   # region maps need the sf package
set_default_solver(solver_options$glpk)   # used by levcost() screening below
```

**Units.** Capacity in **GW**, energy/activity in **PJ**, costs in
**MEUR**, emissions in **kt**; `cap2act = 31.536` (1 GW x 8760 h =
31.536 PJ). Cost figures are rounded, catalogue-range values (Danish
Energy Agency / NREL ATB): capex in EUR/kW (= MEUR/GW via `convert()`),
fixed O&M in EUR/kW/yr, fuel in EUR/GJ (= MEUR/PJ).

## Conventions

A few naming rules keep a model portable and readable. They matter
because the same model is exported to several backends with **different
case-sensitivity**: GAMS is case-*insensitive*, while GLPK/MathProg,
Python/Pyomo and Julia/JuMP are case-*sensitive*. A model that relies on
case to distinguish names would solve on one backend and break (or
silently merge names) on another.

- **Set elements are UPPER-CASE.** Every name that appears as a set
  element – regions, commodities, technologies, groups, slices you
  define – is written in capitals (`R1`, `COA`, `ECOA`, `FUEL`). This is
  the rule that guarantees identical behaviour across backends.

- **Process names carry a type prefix**, so the role is obvious at a
  glance and names never collide across process families:

  | Prefix | Process family                                                   |
  |--------|------------------------------------------------------------------|
  | `SUP_` | supply of a fuel / primary energy (`SUP_COA`, `SUP_GAS`)         |
  | `RES_` | supply of a renewable resource (`RES_SOL`, `RES_WIN`, `RES_HYD`) |
  | `STG_` | storage (`STG_ELC`)                                              |
  | `TRD_` | inter-regional trade (uni-directional)                           |
  | `TBD_` | inter-regional trade (bi-directional, `TBD_ELC_R1_R2`)           |
  | `IMP_` | import from the rest of the world                                |
  | `EXP_` | export to the rest of the world                                  |

  Technologies are the conversion processes and take no fixed prefix –
  just an upper-case mnemonic (`ECOA` = electricity from coal, `ESOL` =
  electricity from solar).

We build every object below to these conventions – including
inter-regional transmission (`TBD_`) and rest-of-world fuel imports /
electricity exports (`IMP_`/`EXP_`).

## Regions and the map

UTOPIA is a small multi-region country. The package ships four 11-region
map layouts in `utopia$map` (`squares`, `honeycomb`, `island`,
`continent`) — all share the region names `R1`…`R11`, so a model built
for one layout draws over any of them. Pick whichever suits the story:

``` r

library(sf)
op <- par(mfrow = c(2, 2), mar = c(0.5, 0.5, 2, 0.5))
for (nm in names(utopia$map)) {
  m <- utopia$map[[nm]]
  plot(st_geometry(m), col = hcl.colors(nrow(m), "Set 3"), border = "white", main = nm)
  text(m$x, m$y, m$region, cex = 0.75)
}
par(op)
```

This vignette builds a **three-region** model on the first three regions
(`R1`, `R2`, `R3`) — we draw the transmission network over the
`honeycomb` layout below.

## Time resolution and inputs

We pick a ready calendar – `utopia_s4h24`, four seasons x 24 hours (96
slices); calendar construction is covered in the [time-resolution
article](https://energyrt.org/articles/time-resolution.html). Region
names are upper-case (`R1`, `R2`, `R3`), matching the region names in
the UTOPIA maps. Deterministic capacity-factor and load profiles come
from
\[[`utopia_profiles()`](https://energyRt.org/reference/utopia_profiles.md)\].

``` r

regions <- c("R1", "R2", "R3")
cal  <- calendars$utopia_s4h24
prof <- utopia_profiles(regions, calendar = "utopia_s4h24")
str(prof, max.level = 1)
```

## Commodities

Energy carriers, the three renewable resource carriers
(`SOL`/`WIN`/`HYD`) and the `CO2` emission commodity. Fossil fuels carry
an emission factor (`emis`); biomass is carbon-neutral. All names are
upper-case set elements.

``` r

COA <- newCommodity(
  name = "COA",
  desc = "Steam coal",
  timeframe = "ANNUAL",
  emis = data.frame(comm = "CO2", unit = "kt/PJ", emis = 95)
)
GAS <- newCommodity(
  name = "GAS",
  desc = "Natural gas",
  timeframe = "ANNUAL",
  emis = data.frame(comm = "CO2", unit = "kt/PJ", emis = 56)
)
BIO <- newCommodity(
  name = "BIO",
  desc = "Solid biomass (carbon-neutral)",
  timeframe = "ANNUAL"
)
NUC <- newCommodity(
  name = "NUC",
  desc = "Nuclear fuel",
  timeframe = "ANNUAL"
)
SOL <- newCommodity(
  name = "SOL",
  desc = "Solar irradiation",
  timeframe = "HOUR"
)
WIN <- newCommodity(
  name = "WIN",
  desc = "Wind resource",
  timeframe = "HOUR"
)
HYD <- newCommodity(
  name = "HYD",
  desc = "Hydro inflow",
  timeframe = "HOUR"
)
ELC <- newCommodity(
  name = "ELC",
  desc = "Electricity",
  timeframe = "HOUR"
)
CO2 <- newCommodity(
  name = "CO2",
  desc = "Carbon dioxide emissions",
  timeframe = "ANNUAL"
)
```

The emission intensities we just declared, compared:

``` r

autoplot(COA, GAS)
```

## Supply and resources

Fuel **supplies** (`SUP_*`) are priced in EUR/GJ (= MEUR/PJ); renewable
**resources** (`RES_*`) are free and weather-limited.

**Endowments differ by region** – that is what makes a multi-region
model interesting: `R1` mines coal, `R2` produces gas, `R3` has the
hydro. Regions without a domestic fuel must import it (next section) or
buy electricity over the grid – regional diversity *stimulates trade*.
The `region` column of `availability` is where an endowment lives. The
first two supplies in full:

``` r

SUP_COA <- newSupply(
  name = "SUP_COA",
  desc = "Coal supply (mined in R1)",
  commodity = "COA",
  unit = "PJ",
  availability = data.frame(
    region = "R1",                        # coal exists only in R1
    cost = 2.5
  )
)
SUP_GAS <- newSupply(
  name = "SUP_GAS",
  desc = "Natural gas supply (produced in R2)",
  commodity = "GAS",
  unit = "PJ",
  availability = data.frame(
    region = "R2",                        # gas exists only in R2
    cost = 6.0
  )
)
```

The remaining five differ only in name, commodity, price and region set.
When a *family* of similar objects is needed, wrapping the constructor
in a small helper is the idiomatic advanced pattern (the packaged kit is
built exactly this way):

``` r

sup <- function(nm, comm, cost, reg = regions) newSupply(nm, commodity = comm,
  availability = data.frame(region = reg, cost = cost))

SUP_BIO <- sup("SUP_BIO", "BIO", 8.0)              # biomass: everywhere
SUP_NUC <- sup("SUP_NUC", "NUC", 0.9)              # nuclear fuel: world market
RES_SOL <- sup("RES_SOL", "SOL", 0)                # sun shines everywhere...
RES_WIN <- sup("RES_WIN", "WIN", 0)                # ...wind blows everywhere
RES_HYD <- sup("RES_HYD", "HYD", 0, reg = "R3")    # hydro inflow: only R3
```

## Rest-of-world trade: imports and exports

Any region can buy fuel from the rest of the world – at a premium over
the domestic source. `IMP_*` objects add an external supply at a price;
`EXP_*` objects add an external market that *buys* at a price (capped,
so exports stay a side business, not the objective):

``` r

IMP_COA <- newImport(
  name = "IMP_COA",
  desc = "Coal import from the rest of the world",
  commodity = "COA",
  unit = "PJ",
  imp = data.frame(
    region = regions,                     # available to every region
    price = 3.5                           # vs 2.5 domestic in R1
  )
)
IMP_GAS <- update(IMP_COA,
  name = "IMP_GAS",
  desc = "LNG import from the rest of the world",
  commodity = "GAS",
  imp = data.frame(
    region = regions,
    price = 9.0                           # vs 6.0 domestic in R2
  )
)

EXP_ELC <- newExport(
  name = "EXP_ELC",
  desc = "Electricity export to the rest of the world",
  commodity = "ELC",
  unit = "PJ",
  exp = merge(                            # ~10 PJ/yr per region, paced by slice
    data.frame(region = regions, price = 5.0),
    data.frame(slice  = as.data.frame(cal@slice_share)$slice,
               exp.up = 10 * as.data.frame(cal@slice_share)$share)
  ),
  reserve = 300                           # plus a cumulative cap, PJ
)
```

The export price sits *below* every technology’s levelized cost, so the
model never builds capacity just to export – it only sells genuine
surplus. Two caps work together: `exp.up` **paces** exports (the annual
10 PJ is spread over the slices via their year-shares – a bare
`exp.up = 10` would be read as 10 *per slice*), and `reserve` bounds the
horizon total. Without the pacing, the model front-loads the whole
reserve into the base year, where the sunk fleet makes surplus cheapest.

## Weather

`newWeather` objects hold a capacity factor per region and slice for
solar, wind and hydro – the physical limits on the renewable
technologies below. Solar in full, the siblings via a helper:

``` r

WSOL <- newWeather(
  name = "WSOL",
  desc = "Solar capacity factor",
  timeframe = "HOUR",
  weather = prof$weather[prof$weather$resource == "WSOL",
                         c("region", "slice", "wval")]
)

wobj <- function(res) newWeather(res, timeframe = "HOUR",
  weather = prof$weather[prof$weather$resource == res,
                         c("region", "slice", "wval")])
WWIN <- wobj("WWIN")
WHYD <- wobj("WHYD")
```

[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
shows the profile structure at a glance – the solar day/season shape.
Custom season names (`WIN`, `SPR`, …) are defined by the calendar, so
pass it to resolve the slice layout. Note the **regional endowments**
([`utopia_profiles()`](https://energyRt.org/reference/utopia_profiles.md)
scales the profiles deterministically): `R1` is the sunniest region,
`R2` the windiest – one more reason for the regions to trade:

``` r

autoplot(WSOL, type = "line", calendar = cal)
```

(UTOPIA’s profiles derive from the
[IDEEA](https://ideea-model.github.io/IDEEA/) model of India – note the
*monsoon* signature: summer is the solar minimum, spring the maximum.
Weather is data, not an assumption.)

## Demand

Electricity demand follows the deterministic load shape, weighted by
each slice’s share of the year and grown over the milestone years.

``` r

years  <- c(2020, 2030, 2040, 2050)
growth <- c(1, 1.2, 1.4, 1.6)
annual_demand <- 100                                   # base-year PJ per region

share <- as.data.frame(cal@slice_share)[, c("slice", "share")]
d0 <- merge(prof$demand, share, by = "slice")
d0$w <- d0$load * d0$share
dem_rows <- do.call(rbind, lapply(seq_along(years), function(i) {
  do.call(rbind, lapply(regions, function(r) {
    dr <- d0[d0$region == r, ]
    data.frame(region = r, year = years[i], slice = dr$slice,
               dem = annual_demand * growth[i] * dr$w / sum(dr$w))
  }))
}))

DEM_ELC <- newDemand(
  name = "DEM_ELC",
  desc = "Final electricity demand",
  commodity = "ELC",
  unit = "PJ",
  dem = dem_rows
)
```

Two views of the demand object: aggregated annual totals by region
(`type = "area"`, the default), and the within-year profiles by region
and year (`type = "line"`) – note the morning ramp and the evening peak:

``` r

autoplot(DEM_ELC, years = 2020:2050)
```

``` r

autoplot(DEM_ELC, type = "line", years = c(2020, 2050))
```

## Technologies

Thermal plants combust a fuel (`combustion = 1`) and convert it to
electricity at an efficiency; renewables convert a free resource capped
by weather.

Two modeling choices run through all of them:

- **Existing stock retires on a schedule.** `capacity$stock` is declared
  not just for the base year but as a *declining path*: energyRt
  interpolates between the anchors, so the base-year fleet fades out
  naturally – faster for solar and wind (mid-life already), ~30 years
  for thermal plants, longer for nuclear, longest for hydro.
  `optimizeRetirement = TRUE` additionally lets the model retire
  capacity *earlier* when that saves money.
- **Investment windows.** `start` and `end` bound the years when *new*
  capacity can be built – coal is phase-limited, nuclear needs a lead
  time, hydro sites are nearly exhausted. We chart all the windows after
  assembling the model.

### Coal power plant

``` r

ECOA <- newTechnology(
  name = "ECOA",
  desc = "Coal power plant",
  input = list(
    comm = "COA",
    unit = "PJ",
    combustion = 1
  ),
  output = list(
    comm = "ELC",
    unit = "PJ"
  ),
  cap2act = 31.536,
  ceff = data.frame(
    comm = "COA",
    cinp2use = 0.40                       # 40% efficiency
  ),
  invcost = list(
    invcost = convert("EUR/kW", "MEUR/GW", 2000)
  ),
  fixom = list(
    fixom = 55
  ),
  capacity = data.frame(                  # fleet clusters near the mines (R1)
    region = c("R1", "R2", "R3", "R1", "R2", "R3"),
    year   = c(2020, 2020, 2020, 2050, 2050, 2050),
    stock  = c(   8,    3,    3,    0,    0,    0)
  ),
  start = list(start = 2010),
  end   = list(end   = 2030),             # no new coal after 2030
  olife = list(olife = 30),
  optimizeRetirement = TRUE
)
draw(ECOA)
```

### Gas power plant

The gas plant has the same structure – we spell it out once more (the
last time; further siblings will use
[`update()`](https://energyRt.org/reference/newDemand.html)):

``` r

EGAS <- newTechnology(
  name = "EGAS",
  desc = "Gas power plant (CCGT)",
  input = list(
    comm = "GAS",
    unit = "PJ",
    combustion = 1
  ),
  output = list(
    comm = "ELC",
    unit = "PJ"
  ),
  cap2act = 31.536,
  ceff = data.frame(
    comm = "GAS",
    cinp2use = 0.58
  ),
  invcost = list(
    invcost = convert("EUR/kW", "MEUR/GW", 900)
  ),
  fixom = list(
    fixom = 25
  ),
  capacity = data.frame(                  # gas plants sit on the gas (R2)
    region = c("R1", "R2", "R3", "R1", "R2", "R3"),
    year   = c(2020, 2020, 2020, 2050, 2050, 2050),
    stock  = c(   1,    6,    1,    0,    0,    0)
  ),
  start = list(start = 2010),
  olife = list(olife = 30),
  optimizeRetirement = TRUE
)
```

### Nuclear power plant

Nuclear is a must-run baseload (`af.lo = 0.7`), with a long-lived
existing fleet (retiring by 2060) and a build window that opens only in
2025 (licensing and construction lead times):

``` r

ENUC <- newTechnology(
  name = "ENUC",
  desc = "Nuclear power plant",
  input = list(
    comm = "NUC",
    unit = "PJ"
  ),
  output = list(
    comm = "ELC",
    unit = "PJ"
  ),
  cap2act = 31.536,
  ceff = data.frame(
    comm = "NUC",
    cinp2use = 0.35
  ),
  af = data.frame(
    af.lo = 0.7                           # must-run baseload
  ),
  invcost = list(
    invcost = convert("EUR/kW", "MEUR/GW", 8000)   # recent Western builds
  ),
  fixom = list(
    fixom = 120
  ),
  capacity = data.frame(
    region = c("R1", "R2", "R3", "R1", "R2", "R3"),
    year   = c(2020, 2020, 2020, 2060, 2060, 2060),
    stock  = c(   2,    2,    2,    0,    0,    0)
  ),
  start = list(start = 2025),
  olife = list(olife = 50)
)
```

### Renewables

VRE technologies draw a free resource and are bounded by the weather
capacity factor (`waf.up = 1` means output can reach the profile, never
exceed it). Solar, in full:

``` r

ESOL <- newTechnology(
  name = "ESOL",
  desc = "Utility-scale solar PV",
  input = list(
    comm = "SOL",
    unit = "PJ"
  ),
  output = list(
    comm = "ELC",
    unit = "PJ"
  ),
  cap2act = 31.536,
  ceff = data.frame(
    comm = "SOL",
    cinp2use = 1
  ),
  weather = list(
    weather = "WSOL",
    comm = "SOL",
    waf.up = 1
  ),
  invcost = list(
    invcost = convert("EUR/kW", "MEUR/GW", 650)
  ),
  fixom = list(
    fixom = 12
  ),
  capacity = data.frame(                  # young but small fleet, out by 2040
    region = c("R1", "R2", "R3", "R1", "R2", "R3"),
    year   = c(2020, 2020, 2020, 2040, 2040, 2040),
    stock  = c(   1,    1,    1,    0,    0,    0)
  ),
  start = list(start = 2015),
  olife = list(olife = 25)
)
```

Wind and hydro share solar’s structure, so instead of retyping we
**derive** them with
[`update()`](https://energyRt.org/reference/newDemand.html) – it takes
an existing object and replaces only the slots you name. This is the
package’s intended way to build families of similar processes:

``` r

EWIN <- update(ESOL,
  name = "EWIN",
  desc = "Onshore wind",
  input = list(comm = "WIN", unit = "PJ"),
  ceff = data.frame(comm = "WIN", cinp2use = 1),
  weather = list(weather = "WWIN", comm = "WIN", waf.up = 1),
  invcost = list(invcost = convert("EUR/kW", "MEUR/GW", 1300)),
  fixom = list(fixom = 35)
)

EHYD <- update(EWIN,
  name = "EHYD",
  desc = "Hydro power (only in R3, no new sites)",
  input = list(comm = "HYD", unit = "PJ"),
  ceff = data.frame(comm = "HYD", cinp2use = 1),
  weather = list(weather = "WHYD", comm = "HYD", waf.up = 1),
  invcost = list(invcost = convert("EUR/kW", "MEUR/GW", 3000)),
  fixom = list(fixom = 45),
  capacity = data.frame(                  # the hydro endowment sits in R3
    region = c("R3", "R3"),
    year   = c(2020, 2070),
    stock  = c(  12,    0)
  ),
  start = list(start = 2010),
  end   = list(end   = 2010),             # window closed: NO new hydro at all
  olife = list(olife = 60)
)
draw(EHYD)
```

Note how `EWIN` inherited solar’s capacity path and window untouched,
while `EHYD` overrides them: the hydro fleet exists only in `R3` (12 GW,
operating into the 2070s) and its **investment window is closed** – the
model can run the legacy dams but never build new ones. And since
`RES_HYD` flows only in `R3`, the other regions can tap this cheap
resource **only over the transmission lines**.

### The biomass plant

`EBIO` burns biomass to make electricity. Biomass is **carbon-neutral**
(the `BIO` commodity carries no emission factor), so `EBIO` is a
dispatchable zero-carbon generator – more expensive to fuel than coal,
but emitting nothing:

``` r

EBIO <- newTechnology(
  name = "EBIO",
  desc = "Biomass power plant (carbon-neutral)",
  input = list(
    comm = "BIO",
    unit = "PJ",
    combustion = 1
  ),
  output = list(
    comm = "ELC",
    unit = "PJ"
  ),
  ceff = data.frame(
    comm = "BIO",
    cinp2use = 0.35                       # 35% efficiency
  ),
  cap2act = 31.536,
  invcost = list(
    invcost = convert("EUR/kW", "MEUR/GW", 2200)
  ),
  fixom = list(
    fixom = 60
  ),
  start = list(start = 2025),             # a new option, available from 2025
  olife = list(olife = 30),
  optimizeRetirement = TRUE
)
draw(EBIO)
```

Its **levelized cost**
([`levcost()`](https://energyRt.org/reference/levcost.md) builds and
solves a tiny single-technology model; annual commodities make the LCOE
read in MEUR/PJ):

``` r

ELCa <- newCommodity("ELCa", timeframe = "ANNUAL")
EBIO_lc <- update(EBIO, output = list(comm = "ELCa"))
lc <- levcost(EBIO_lc, discount = 0.05, verbose = FALSE,
  repo = newRepository("r", BIO, CO2, ELCa),
  fuel_costs = c(BIO = 8.0))
lc$levcost_npv
autoplot(lc, type = "components")
```

Commodity **groups** (a fuel blend on the input side, or a product split
on the output side) are covered structurally in the *Model bricks*
article. They are left out of this solvable UTOPIA model pending a fix
to how grouped technologies enforce their input-output balance.

## Storage

A 4-hour battery (`STG_*`) shifts electricity between slices:

``` r

STG_ELC <- newStorage(
  name = "STG_ELC",
  desc = "Grid battery, 4-hour",
  commodity = "ELC",
  invcost = list(
    invcost = convert("EUR/kWh", "MEUR/PJ", 200)
  ),
  cap2stg = 4,                            # 4 hours of storage per GW
  seff = data.frame(
    inpeff = 0.95,
    outeff = 0.95                         # ~90% round trip
  ),
  olife = list(olife = 20)
)
```

## Interregional trade

Trade objects open **routes** between regions for a commodity. UTOPIA’s
three regions form a line, so two **bi-directional** transmission links
(`TBD_`) connect them. Each `newTrade` lists its routes (both
directions), the transfer efficiency (`teff` – losses), and an
endogenous capacity with investment cost (`capacityVariable = TRUE` lets
the model expand the line):

``` r

TBD_ELC_R1_R2 <- newTrade(
  name = "TBD_ELC_R1_R2",
  desc = "Bidirectional transmission line R1-R2",
  commodity = "ELC",
  routes = data.frame(
    src = c("R1", "R2"),
    dst = c("R2", "R1")
  ),
  trade = data.frame(
    src = c("R1", "R2"),
    dst = c("R2", "R1"),
    teff = 0.97                           # 3% losses per transfer
  ),
  capacity = data.frame(
    stock = 1                             # 1 GW existing interconnector
  ),
  capacityVariable = TRUE,                # the model may build more
  invcost = data.frame(
    region = c("R1", "R2"),
    invcost = 350                         # MEUR/GW (per line end)
  ),
  olife = list(olife = 50)
)

TBD_ELC_R2_R3 <- update(TBD_ELC_R1_R2,
  name = "TBD_ELC_R2_R3",
  desc = "Bidirectional transmission line R2-R3",
  routes = data.frame(
    src = c("R2", "R3"),
    dst = c("R3", "R2")
  ),
  trade = data.frame(
    src = c("R2", "R3"),
    dst = c("R3", "R2"),
    teff = 0.97
  ),
  invcost = data.frame(
    region = c("R2", "R3"),
    invcost = 350
  )
)
draw(TBD_ELC_R1_R2)
```

[`draw()`](https://energyRt.org/reference/draw.md) shows one link’s
schematic;
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
(or
[`plot_trade_map()`](https://energyRt.org/reference/plot_trade_map.md))
puts the whole network **on the map** — pass the layout to draw over.
Bidirectional links render as double-headed arrows:

``` r

plot_trade_map(list(TBD_ELC_R1_R2, TBD_ELC_R2_R3), map = utopia$map$honeycomb)
```

## Scenario levers

Policies and pathways are **pre-built objects** layered onto the base
model when running scenarios (mirroring IDEEA’s `CO2_CAP` etc.):

- `CO2_CAP` – a declining cap on CO2 emissions (`newConstraint` on
  `vEmsFuelTot`; starts near BASE emissions (~5,000 kt/region in 2020)
  and tightens to a deep cut by 2050);
- `CT_CO2` – a rising carbon tax (`newTax`; costs are MEUR and emissions
  kt, so **20-80 EUR/t reads as 0.02-0.08 MEUR/kt** – watch the units);
- `RES_SHARE` – a growing renewable-generation floor;
- `NO_NEW_NUC` – no new nuclear capacity;
- `EARLY_RET` – forced early retirement of the coal and gas fleets (a
  declining per-region ceiling on installed coal and gas capacity;
  `optimizeRetirement = TRUE` on the plants lets the model choose
  *which* units to close).

``` r

nreg <- length(regions)
CO2_CAP <- newConstraint(
  name = "CO2_CAP",
  eq = "<=",
  for.each = data.frame(year = years, comm = "CO2"),
  term1 = list(variable = "vEmsFuelTot"),
  rhs = data.frame(year = range(years), rhs = c(5000, 2500) * nreg),
  defVal = Inf
)

CT_CO2 <- newTax(
  name = "CT_CO2",
  comm = "CO2",
  tax = data.frame(year = range(years), bal = c(0.02, 0.08))  # 20-80 EUR/t
)

ren <- c("ESOL", "EWIN", "EHYD")
sh  <- seq(0.15, 0.50, length.out = length(years))
RES_SHARE <- newConstraint(
  name = "RES_SHARE",
  eq = ">=",
  for.each = data.frame(year = years, comm = "ELC"),
  term1 = list(variable = "vTechOut", for.sum = list(tech = ren)),
  rhs = data.frame(year = years, rhs = sh * annual_demand * growth * nreg),
  defVal = 0
)

NO_NEW_NUC <- newConstraint(
  name = "NO_NEW_NUC",
  eq = "<=",
  for.each = data.frame(year = years, tech = "ENUC"),
  term1 = list(variable = "vTechNewCap"),
  rhs = 0,
  defVal = 0
)

# vTechCap is indexed by region, so this ceiling applies region by region.
# for.each is the full year x tech grid (expand.grid, not a recycled
# data.frame); rhs sets a declining per-region ceiling. The binding effect is
# on GAS -- without it the model builds gas out as the flexible VRE backstop;
# coal already phases out on economics. A full fossil exit is infeasible here
# (must-run nuclear + limited transmission need firm capacity for peak), so the
# schedule keeps a backup floor. defVal = Inf leaves 2020 unbound.
EARLY_RET <- newConstraint(
  name = "EARLY_RET",
  eq = "<=",
  # scope to 2030+ only: adding the 2020 milestone (even unbound) makes the LP
  # infeasible through rhs interpolation.
  for.each = expand.grid(year = c(2030, 2040, 2050), tech = c("ECOA", "EGAS"),
                         stringsAsFactors = FALSE),
  term1 = list(variable = "vTechCap"),
  rhs = data.frame(
    tech = rep(c("ECOA", "EGAS"), each = 3),
    year = rep(c(2030, 2040, 2050), 2),
    rhs  = c(12, 10, 8,   7, 6, 5)     # coal & gas ceilings (GW/region), falling
  ),
  defVal = Inf
)
```

## Assembling the model

Collect every block into a base **repository**, then bind it to a region
set, a calendar and a horizon to get a solvable **model**:

``` r

repo <- newRepository("utopia",
  COA, GAS, BIO, NUC, SOL, WIN, HYD, ELC, CO2,          # commodities
  SUP_COA, SUP_GAS, SUP_BIO, SUP_NUC,                   # fuel supplies
  RES_SOL, RES_WIN, RES_HYD,                            # renewable resources
  IMP_COA, IMP_GAS, EXP_ELC,                            # rest-of-world trade
  WSOL, WWIN, WHYD,                                     # weather
  ECOA, EGAS, ENUC, ESOL, EWIN, EHYD, EBIO,             # technologies
  STG_ELC,                                              # storage
  TBD_ELC_R1_R2, TBD_ELC_R2_R3,                         # interregional trade
  DEM_ELC)                                              # demand
length(repo); names(repo)
```

``` r

hor <- newHorizon(period = 2020:2050, intervals = c(1, 10, 10, 10),
                  mid_is_end = TRUE, name = "base")
mod <- newModel("UTOPIA", data = repo, calendar = cal,
                region = regions, horizon = hor, discount = 0.05)
mod
```

### Investment windows

The `start`/`end` choices above define when each process can be
**built**; the translucent tails show how long the last vintage then
operates (`end + olife`). One call charts the whole fleet:

``` r

autoplot(mod, type = "windows")
```

That is a complete UTOPIA model, ready for
[`interpolate_model()`](https://energyRt.org/reference/interpolate_model.md) +
[`solve_scenario()`](https://energyRt.org/reference/solve_model.md) –
the subject of *UTOPIA II*.

### Screening technology costs

With the model assembled we can compare technologies **before** solving.
`levcost(mod, name = ...)` prices one technology by building and solving
a tiny unit-demand model that inherits the model’s regions, calendar,
discount rate and fuel supplies. By default it reports the **annual**
levelized cost: a weather-driven technology’s profile is collapsed to
its annual capacity factor, so capacity is sized to serve demand at that
factor (textbook LCOE).

``` r

techs <- c("ECOA", "EGAS", "ENUC", "ESOL", "EWIN", "EHYD")
lcoe  <- sapply(techs, function(tn)
  as.numeric(levcost(mod, name = tn, verbose = FALSE)$levcost_npv))
round(sort(lcoe), 1)                       # MEUR/PJ  (x3.6 ~ EUR/MWh)
```

``` r

data.frame(tech = names(lcoe), lcoe = lcoe) |>
  ggplot(aes(reorder(tech, lcoe), lcoe, fill = tech)) +
  geom_col(show.legend = FALSE) + coord_flip() +
  labs(x = NULL, y = "levelized cost, MEUR/PJ",
       title = "A-priori technology cost (annual LCOE)") + theme_bw()
```

This *a-priori* screening ranks technologies by their stand-alone cost.
It is not the same as their **realized** cost in a solved system – a
plant that runs only a few hours spreads its fixed costs over little
output, and a variable renewable’s value depends on *when* it produces.
For that (`timeframe = "native"`, or the ex-post cost from a solved
scenario) see *UTOPIA II*.

### Reports

Any technology, the whole model, or (after solving) a scenario can be
rendered as a shareable HTML/PDF document – parameters, diagrams, cost
breakdowns included. Not run here (reports open in the browser); try
them in your session:

``` r

# one technology: datasheet with diagram, share frontier and levelized cost
report(EBIO, discount = 0.05,
       repo = newRepository("r", COA, BIO, CO2, ELCa),
       fuel_costs = c(COA = 2.5, BIO = 8.0))

# a single process, priced inside the assembled model
report(mod, name = "ENUC")

# the WHOLE model: configuration, inventory, every process one-by-one
report(mod)

# a solved scenario: results overview (see UTOPIA II)
# report(scen_BASE)
```

## The packaged kit

The steps above are exactly what `data-raw/utopia_modules.R` runs for a
few region layouts (1, 3 and 7 regions) to produce the shipped
\[`utopia_modules`\] dataset – a ready base repository (`$repo`), the
individual blocks, and the levers, alongside the calendars, horizons and
maps:

``` r

names(utopia_modules)
names(utopia_modules$electricity)                 # kits by number of regions
um <- utopia_modules$electricity$reg3
identical(names(repo), names(um$repo))            # same blocks we just built
```

Continue with *UTOPIA II: running scenarios*, which loads this kit,
assembles a model and solves the base case and policy scenarios.
