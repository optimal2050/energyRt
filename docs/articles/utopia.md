# UTOPIA: a teaching energy-system model with energyRt

## Introduction

UTOPIA is the classic teaching model for energy-system optimization: a
small **Reference Energy System (RES)** that maps primary resources →
conversion technologies → an energy carrier → final demand, and finds
the least-cost way to meet demand over a planning horizon. This vignette
rebuilds UTOPIA with `energyRt` end to end – commodities, supply,
demand, technologies (including renewables with weather, storage, and a
blended-fuel plant), a multi-region layout, then solves it with **GLPK**
(bundled, no external solver needed) and analyses the results.

All inputs are **deterministic** and shipped with the package: the
sub-annual calendars (`calendars`), representative capacity-factor /
load profiles (`utopia_weather`, `utopia_demand`, `utopia_stock`),
expanded to regions by
\[[`utopia_profiles()`](https://energyRt.org/reference/utopia_profiles.md)\].
Time-resolution construction is covered in a companion article; here we
simply pick a ready calendar.

``` r

library(energyRt)
library(dplyr)
library(ggplot2)
# keep all scenario files in a temporary folder
set_scenarios_path(file.path(tempdir(), "utopia"))
# capital-cost helper: EUR/kW -> MEUR/GW (numerically 1:1)
meur_gw <- function(eur_per_kw) convert("EUR/kW", "MEUR/GW", eur_per_kw)
```

**Units.** Capacity is in **GW**, energy/activity in **PJ**, costs in
**MEUR**, emissions in **kt**. The capacity-to-activity factor
`cap2act = 31.536` converts 1 GW running a full year to 31.536 PJ (1 GW
x 8760 h). Cost figures below are illustrative, rounded values in the
range of published technology catalogues (e.g. Danish Energy Agency /
NREL ATB): overnight capital cost in EUR/kW (= MEUR/GW), fixed O&M in
EUR/kW/yr, and fuel prices in EUR/GJ (= MEUR/PJ).

## Time resolution and horizon

We use `utopia_s4h24` – four seasons x 24 hours (96 slices). The full
diurnal detail lets storage cycle within a day, while 96 slices keep the
model fast. (`utopia_m12h24`, 288 slices, is the higher-resolution
option.)

``` r

cal <- calendars$utopia_s4h24
cal                      # 4 seasons x 24 hours = 96 slices
```

The planning **horizon** uses four milestone years (2020, 2030, 2040,
2050); the base year is a single year, then 10-year steps.

``` r

hor <- newHorizon(period = 2020:2050, intervals = c(1, 10, 10, 10),
                  mid_is_end = TRUE)
hor@intervals
```

## Regions

UTOPIA is an imaginary country. For the base case we use three regions;
the package ships example maps in `utopia$map` (honeycomb, continent,
island, …) for larger multi-region layouts and trade networks.

``` r

regs <- c("reg1", "reg2", "reg3")
prof <- utopia_profiles(regs, calendar = "utopia_s4h24")   # weather/demand/stock
```

## Commodities

Energy carriers and the CO2 emission commodity. Fossil fuels carry an
emission factor (`emis`, kt CO2 per PJ); biomass is treated as
carbon-neutral.

``` r

COA <- newCommodity("COA", timeframe = "ANNUAL",
  emis = data.frame(comm = "CO2", unit = "kt/PJ", emis = 95))
GAS <- newCommodity("GAS", timeframe = "ANNUAL",
  emis = data.frame(comm = "CO2", unit = "kt/PJ", emis = 56))
BIO <- newCommodity("BIO", timeframe = "ANNUAL")   # carbon-neutral
NUC <- newCommodity("NUC", timeframe = "ANNUAL")
SOL <- newCommodity("SOL", timeframe = "HOUR")
WIN <- newCommodity("WIN", timeframe = "HOUR")
HYD <- newCommodity("HYD", timeframe = "HOUR")
ELC <- newCommodity("ELC", timeframe = "HOUR")     # electricity (hourly)
CO2 <- newCommodity("CO2", timeframe = "ANNUAL")   # emissions accounting
```

## Supply

Primary resources, priced in MEUR per PJ. Solar/wind/hydro “resource”
commodities are free; their availability is limited by weather (below).

``` r

# fuel prices in EUR/GJ (= MEUR/PJ)
SCOA <- newSupply("SCOA", commodity = "COA", availability = data.frame(region = regs, cost = 2.5))
SGAS <- newSupply("SGAS", commodity = "GAS", availability = data.frame(region = regs, cost = 6.0))
SBIO <- newSupply("SBIO", commodity = "BIO", availability = data.frame(region = regs, cost = 8.0))
SNUC <- newSupply("SNUC", commodity = "NUC", availability = data.frame(region = regs, cost = 0.9))
SSOL <- newSupply("SSOL", commodity = "SOL", availability = data.frame(region = regs, cost = 0))
SWIN <- newSupply("SWIN", commodity = "WIN", availability = data.frame(region = regs, cost = 0))
SHYD <- newSupply("SHYD", commodity = "HYD", availability = data.frame(region = regs, cost = 0))
```

## Final demand

Electricity demand follows the deterministic load shape in
`utopia_demand` (a relative shape by slice), scaled to an annual level
per region and grown over the horizon.
[`utopia_profiles()`](https://energyRt.org/reference/utopia_profiles.md)
already expanded the shape to our regions.

``` r

share <- as.data.frame(cal@slice_share)[, c("slice", "share")]
d0 <- merge(prof$demand, share, by = "slice")
d0$w <- d0$load * d0$share                       # energy weight per slice
years <- c(2020, 2030, 2040, 2050); growth <- c(1, 1.2, 1.4, 1.6)
dem_rows <- do.call(rbind, lapply(seq_along(years), function(i) {
  do.call(rbind, lapply(regs, function(r) {
    dr <- d0[d0$region == r, ]
    data.frame(region = r, year = years[i], slice = dr$slice,
               dem = 100 * growth[i] * dr$w / sum(dr$w))   # ~100 PJ/yr in 2020
  }))
}))
DEM <- newDemand("DEM_ELC", commodity = "ELC", dem = dem_rows)
```

## Renewable resources: weather

Solar, wind and hydro availability come from representative
capacity-factor profiles (`utopia_weather`, sourced from reanalysis
data). A `weather` object holds a capacity factor `wval` per region and
slice; a technology references it to cap its output.

``` r

wobj <- function(res) newWeather(res, timeframe = "HOUR",
  weather = prof$weather[prof$weather$resource == res, c("region", "slice", "wval")])
WSOL <- wobj("WSOL"); WWIN <- wobj("WWIN"); WHYD <- wobj("WHYD")

# a quick look at the solar day-shape (summer vs winter)
prof$weather |>
  dplyr::filter(resource == "WSOL", region == "reg1",
                grepl("^(WIN|SUM)_", slice)) |>
  dplyr::mutate(season = sub("_.*", "", slice),
                hour = as.integer(sub(".*_h", "", slice))) |>
  ggplot(aes(hour, wval, colour = season)) + geom_line() +
  labs(title = "Solar capacity factor by hour", y = "capacity factor") +
  theme_bw()
```

## Technologies

### Thermal and nuclear

Coal and gas plants combust a fuel (`combustion = 1`, so their CO2 is
counted) and convert it to electricity at a given efficiency
(`ceff$cinp2use`). Existing base-year capacity comes from
`utopia_stock`.

``` r

stk <- function(tech) {
  g <- prof$stock[prof$stock$tech == tech, ]
  data.frame(region = g$region, year = 2020, stock = g$gw)
}
thermal <- function(nm, comm, eff, inv, fixom) newTechnology(nm,
  input = list(comm = comm, combustion = 1), output = list(comm = "ELC"),
  ceff = data.frame(comm = comm, cinp2use = eff), cap2act = 31.536, fixom = fixom,
  invcost = data.frame(region = regs, invcost = inv),
  olife = 30L, start = 2010L, capacity = stk(nm), optimizeRetirement = TRUE)

ECOA <- thermal("ECOA", "COA", 0.40, meur_gw(2000), 55)   # coal
EGAS <- thermal("EGAS", "GAS", 0.58, meur_gw(900), 25)    # CCGT
ENUC <- newTechnology("ENUC", input = list(comm = "NUC"), output = list(comm = "ELC"),
  ceff = data.frame(comm = "NUC", cinp2use = 0.35), af = data.frame(af.lo = 0.7),
  cap2act = 31.536, fixom = 120, invcost = data.frame(region = regs, invcost = meur_gw(6500)),
  olife = 50L, start = 2010L, capacity = stk("ENUC"))
```

### Renewables (weather-limited)

Solar, wind and hydro convert a free resource to electricity, with
output capped by the linked `weather` capacity factor.

``` r

vre <- function(nm, comm, wname, inv, fixom) newTechnology(nm,
  input = list(comm = comm), output = list(comm = "ELC"),
  ceff = data.frame(comm = comm, cinp2use = 1),
  weather = list(weather = wname, comm = comm, waf.up = 1),
  cap2act = 31.536, fixom = fixom, invcost = data.frame(region = regs, invcost = inv),
  olife = 25L, start = 2015L, capacity = stk(nm))
ESOL <- vre("ESOL", "SOL", "WSOL", meur_gw(650), 12)     # utility solar PV
EWIN <- vre("EWIN", "WIN", "WWIN", meur_gw(1300), 35)    # onshore wind
EHYD <- vre("EHYD", "HYD", "WHYD", meur_gw(3000), 45)    # hydro
```

### A blended-fuel plant (commodity groups)

`ECOABIO` is a coal-biomass **co-firing** plant: it burns a *blend* of
two fuels. Both fuels sit in one input **group** `FUEL`; the group
converts to electricity (`geff$ginp2use`), each fuel contributes to the
group total (`ceff$cinp2ginp`), and the biomass **share** is bounded
(`ceff$share.up` – at most 30% here). This is the commodity-group /
share machinery in a nutshell.

``` r

ECOABIO <- newTechnology("ECOABIO", desc = "Coal-biomass co-firing (blended fuel)",
  input  = data.frame(comm = c("COA", "BIO"), group = "FUEL", combustion = c(1, 0)),
  output = list(comm = "ELC"),
  group  = data.frame(group = "FUEL", desc = "Blended solid fuel", unit = "PJ"),
  geff   = data.frame(group = "FUEL", ginp2use = 0.40),
  ceff   = data.frame(comm = c("COA", "BIO"), cinp2ginp = c(1, 1),
                      share.up = c(1.0, 0.30)),
  cap2act = 31.536, fixom = 60, invcost = data.frame(region = regs, invcost = meur_gw(2200)),
  olife = 30L, start = 2010L, optimizeRetirement = TRUE)
```

The feasible blend band requires no solve –
[`tech_share_frontier()`](https://energyRt.org/reference/tech_share_frontier.md)
intersects each fuel’s share bounds:

``` r

tech_share_frontier(ECOABIO)          # feasible coal/biomass share band
```

For the **levelized cost** we look at the blend plant in isolation.
[`levcost()`](https://energyRt.org/reference/levcost.md) builds and
solves a tiny single-technology model; using annual commodities here
makes the LCOE read directly in MEUR per PJ.

``` r

ELCa <- newCommodity("ELCa", timeframe = "ANNUAL")
ECOABIO_lc <- newTechnology("ECOABIO", desc = "Coal-biomass co-firing",
  input  = data.frame(comm = c("COA", "BIO"), group = "FUEL", combustion = c(1, 0)),
  output = list(comm = "ELCa"),
  group  = data.frame(group = "FUEL", desc = "Blended solid fuel", unit = "PJ"),
  geff   = data.frame(group = "FUEL", ginp2use = 0.40),
  ceff   = data.frame(comm = c("COA", "BIO"), cinp2ginp = c(1, 1), share.up = c(1.0, 0.30)),
  cap2act = 31.536, fixom = 60, invcost = list(invcost = meur_gw(2200)), olife = 30L)
lc <- levcost(ECOABIO_lc, discount = 0.05, verbose = FALSE,
  repo = newRepository("r", COA, BIO, CO2, ELCa), fuel_costs = c(COA = 2.5, BIO = 8.0))
lc$levcost_npv                        # NPV levelized cost, MEUR/PJ
autoplot(lc, type = "components")     # cost breakdown by year
```

``` r

# a full technology datasheet (schematic, tables, share frontier, LCOE):
report(ECOABIO_lc, format = "html", discount = 0.05,
       repo = newRepository("r", COA, BIO, CO2, ELCa))
```

### Storage

A battery that shifts electricity between slices – essential once we
resolve the 24-hour day, so solar can serve evening demand.

``` r

# 4-hour battery; energy-capacity cost ~200 EUR/kWh
STG <- newStorage("STGELC", commodity = "ELC", olife = 20L,
  invcost = data.frame(region = regs, invcost = convert("EUR/kWh", "MEUR/PJ", 200)),
  cap2stg = 4, seff = data.frame(inpeff = 0.95, outeff = 0.95))
```

## Assemble and solve the base scenario

``` r

repo <- newRepository("utopia",
  COA, GAS, BIO, NUC, SOL, WIN, HYD, ELC, CO2,
  SCOA, SGAS, SBIO, SNUC, SSOL, SWIN, SHYD, WSOL, WWIN, WHYD,
  ECOA, EGAS, ENUC, ESOL, EWIN, EHYD, ECOABIO, STG, DEM)
mod <- newModel("UTOPIA", data = repo, calendar = cal, region = regs,
                horizon = hor, discount = 0.05)
```

Interpolate the model to milestone years, then solve the scenario with
GLPK. Building the scenario in two steps –
[`interpolate_model()`](https://energyRt.org/reference/interpolate_model.md)
then [`solve_scenario()`](https://energyRt.org/reference/solve_model.md)
– is the pattern we reuse for every scenario below.

``` r

scen_BASE <- solve_scenario(
  interpolate_model(mod, name = "BASE"), solver = solver_options$glpk)
getData(scen_BASE, "vObjective", merge = TRUE)$value   # total system cost, MEUR
```

## Results

Electricity generation by technology and year:

``` r

gen <- getData(scen_BASE, "vTechOut", comm = "ELC", merge = TRUE)
gen |>
  group_by(year, tech) |> summarise(PJ = sum(value), .groups = "drop") |>
  ggplot(aes(factor(year), PJ, fill = tech)) +
  geom_col() + labs(x = "year", title = "Electricity generation by technology") +
  theme_bw()
```

Storage cycling within a representative day (it charges midday,
discharges in the evening):

``` r

getData(scen_BASE, "vStorageInp", merge = TRUE) |>
  filter(year == 2050, grepl("^SUM_", slice)) |>
  mutate(hour = as.integer(sub(".*_h", "", slice))) |>
  group_by(hour) |> summarise(charge = sum(value), .groups = "drop") |>
  ggplot(aes(hour, charge)) + geom_col() +
  labs(title = "Battery charging, summer day (2050)", y = "PJ") + theme_bw()
```

CO2 emissions over the horizon:

``` r

getData(scen_BASE, "vEmsFuelTot", comm = "CO2", merge = TRUE) |>
  group_by(year) |> summarise(ktCO2 = sum(value), .groups = "drop")
```

## Scenarios

Scenarios reuse the base model and add or modify objects. We contrast a
**CO2 cap** (a quantity limit) with a **carbon tax** (a price).

### CO2 cap

A declining cap on total CO2 emissions, tightening to 40% of the base
level:

``` r

base_emis <- getData(scen_BASE, "vEmsFuelTot", comm = "CO2", merge = TRUE) |>
  group_by(year) |> summarise(v = sum(value), .groups = "drop")
cap0 <- base_emis$v[base_emis$year == 2020]
CO2CAP <- newConstraint(name = "CO2CAP", eq = "<=",
  for.each = data.frame(year = years, comm = "CO2"),
  term1 = list(variable = "vEmsFuelTot"),
  rhs = data.frame(year = c(2020, 2050), rhs = round(cap0) * c(1.0, 0.4)),
  defVal = Inf)
scen_CO2CAP <- solve_scenario(
  interpolate_model(mod, name = "CO2CAP", CO2CAP), solver = solver_options$glpk)
```

### Carbon tax

A rising tax on CO2 emissions:

``` r

CTAX <- newTax(name = "CTAX", comm = "CO2",
  tax = data.frame(year = c(2020, 2050), bal = c(20, 80)))
scen_CTAX <- solve_scenario(
  interpolate_model(mod, name = "CARBONTAX", CTAX), solver = solver_options$glpk)
```

Compare emissions across scenarios:

``` r

lapply(list(BASE = scen_BASE, CO2CAP = scen_CO2CAP, CTAX = scen_CTAX),
  function(s) getData(s, "vEmsFuelTot", comm = "CO2", merge = TRUE)) |>
  bind_rows() |>
  group_by(scenario, year) |> summarise(ktCO2 = sum(value), .groups = "drop") |>
  ggplot(aes(factor(year), ktCO2, fill = scenario)) +
  geom_col(position = "dodge") + labs(x = "year", title = "CO2 emissions by scenario") +
  theme_bw()
```

## Next steps

This base UTOPIA can be extended with **inter-regional trade**,
additional technologies introduced via scenarios (CCS, hydrogen, CHP),
renewable-share targets, and a nuclear phase-out – each a small addition
to the repository. See the companion articles for time-resolution
construction and the workshop exercises for a step-by-step build.
