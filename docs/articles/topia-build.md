# TOPIA I: building the model

## Overview

TOPIA is a teaching model for energy-system optimization. Its
**Reference Energy System (RES)** links primary resources, conversion
technologies, storage, trade, and electricity demand across up to eleven
regions.

The complete model is packaged as `topia$modules$electricity`. Smaller
cases are cut from it with
[`subset_model_regions()`](https://energyRt.org/reference/subset_model_regions.md)
rather than kept as separate saved layouts.

## Conventions

TOPIA uses uppercase set elements and typed process names.

- **Set elements are uppercase:** regions, commodities, technologies,
  groups, and user-defined timeslices use names such as `W1`, `COA`, and
  `ECOA`.

- **Process names carry a type prefix:**

  | Prefix | Process family                                           |
  |--------|----------------------------------------------------------|
  | `SUP_` | supply of a fuel / primary energy (`SUP_COA`, `SUP_GAS`) |
  | `RES_` | supply of a renewable resource (`RES_HYD`)               |
  | `STG_` | storage (`STG_ELC`)                                      |
  | `TRD_` | inter-regional trade (uni-directional)                   |
  | `TBD_` | inter-regional trade (bi-directional, `TBD_ELC_W1_W2`)   |
  | `IMP_` | import from the rest of the world                        |
  | `EXP_` | export to the rest of the world                          |

  Electricity generating technologies use an uppercase mnemonic with `E`
  fixed prefix, such as `ECOA` and `ESOL`.

### Units

TOPIA uses the following units:

| quantity | unit | where it is declared |
|----|----|----|
| electricity, and the renewable resources feeding it | **GWh** | `commodity@unit` |
| coal, gas, biomass, nuclear fuel | **PJ** | `commodity@unit` |
| emissions | kt | `commodity@unit` |
| capacity (plants, storage power, corridors) | GW | `cap2act` fixes the link |
| investment cost | MUSD/GW | `invcost` |
| storage reservoir | GWh, priced MUSD/GWh | `duration`, `stg.invcost` |
| fuel cost, import price | MUSD/PJ (= USD/GJ) | `supply@cost`, `import@price` |

Electricity and renewable-resource flows use GWh; fuels use PJ. `@unit`
records the unit but does not convert values. Explicit conversions
appear in:

- `cap2act` – GW of capacity to GWh of annual activity.
- `ceff$use2cact` – the PJ-to-GWh bridge on a fuel-burning plant.
- `trade@cap2act` – GW of corridor capacity to GWh of annual flow.

Conversion efficiency is dimensionless in `ceff$cinp2use`.

``` r

convert("PJ", "GWh", 1) # 277.7778 -- the fuel/electricity bridge
#> [1] 277.7778
convert("GW*year", "GWh", 1) # 8760     -- cap2act for electricity
#> [1] 8760
```

## Regions and the map

TOPIA has eleven regions ordered from west to east: `W1`, `W2`,
`C1`–`C6`, and `E1`–`E3`. The `squares`, `honeycomb`, `island`, and
`continent` layouts in `topia$geoscales` share these region names.

``` r

gs <- topia$geoscales$honeycomb
regions <- geoscale_regions(gs, geoframe = "region")
regions
#>  [1] "W1" "W2" "C1" "C2" "C3" "C4" "C5" "C6" "E1" "E2" "E3"
```

``` r

plot_geoscale(gs, type = "map")
```

![The honeycomb layout.](topia-build_files/figure-html/maps-1.png)

The honeycomb layout.

### Zones: the region hierarchy

The regions nest into three zones and one nation.
[`geoscale_leaftable()`](https://optimal2050.github.io/geoscales/r/reference/geoscale_leaftable.html)
returns this hierarchy.

``` r

geo <- as.data.frame(geoscale_leaftable(gs))

knitr::kable(geo, caption = "Regional hierarchy")
```

| nation | zone    | region | name     |     area |
|:-------|:--------|:-------|:---------|---------:|
| TOPIA  | WEST    | W1     | Oswestia | 5.655895 |
| TOPIA  | WEST    | W2     | Antidia  | 5.655895 |
| TOPIA  | CENTRAL | C1     | Kopalia  | 5.655895 |
| TOPIA  | CENTRAL | C2     | Kalgia   | 5.655895 |
| TOPIA  | CENTRAL | C3     | Robatia  | 5.655895 |
| TOPIA  | CENTRAL | C4     | Ramia    | 5.655895 |
| TOPIA  | CENTRAL | C5     | Nishi    | 5.655895 |
| TOPIA  | CENTRAL | C6     | Masland  | 5.655895 |
| TOPIA  | EAST    | E1     | Sind     | 5.655895 |
| TOPIA  | EAST    | E2     | Kumalia  | 5.655895 |
| TOPIA  | EAST    | E3     | Neutrals | 5.655895 |

Regional hierarchy {.table}

Fuel balances and regional archetypes are defined by zone:

| zone | regions | resource | load |
|----|----|----|----|
| `WEST` | `W1`, `W2` | sunny, little wind | moderate, sharp evening peak |
| `CENTRAL` | `C1`…`C6` | poor on both counts | large, flat, industrial |
| `EAST` | `E1`, `E2`, `E3` | windy at night, in winter | small |

``` r

geoscale_autoplot(gs,
  type = "stack", view = "pyramid", direction = "up",
  outline = c("zone"), outline_colour = "black",
  frame = TRUE, frame_fill = alpha("grey60", 0.10)
)
```

![The spatial hierarchy in a 3D
stack.](topia-build_files/figure-html/geoscale-stack-1.png)

The spatial hierarchy in a 3D stack.

## Time resolution

The base calendar has four seasons and 24 hourly timeslices per season.

``` r

calendar <- topia$modules$calendars$s4_h24
autoplot(calendar)
```

![](topia-build_files/figure-html/calendar-1.png)

## Horizon

The horizon contains four milestone years: 2025, 2030, 2040, and 2050.

``` r

horizon <- topia$modules$horizons$BY2025_50_by10
horizon@intervals
#>    start   mid   end  label
#>    <num> <num> <num> <char>
#> 1:  2025  2025  2025   2025
#> 2:  2026  2030  2035   2030
#> 3:  2036  2040  2045   2040
#> 4:  2046  2050  2055   2050
autoplot(horizon)
```

![](topia-build_files/figure-html/horizon-1.png)

## Input profiles

[`topia_profiles()`](https://energyRt.org/reference/topia_profiles.md)
returns weather, demand, and base-year stock profiles for a selected
region set and calendar.

``` r

prof <- topia_profiles(regions = regions, calendar = "s4_h24")
head(prof$weather, 3)
#>   resource region timeslice   wval
#> 1     WHYD     W1   FAL_h00 0.2265
#> 2     WHYD     W1   FAL_h01 0.2265
#> 3     WHYD     W1   FAL_h02 0.2265
head(prof$demand, 3)
#>   region timeslice   load
#> 1     W1   FAL_h00 0.7961
#> 2     W1   FAL_h01 0.7836
#> 3     W1   FAL_h02 0.7764
# head(prof$stock, 3) # !!! should not be here, stock is not a profile
```

## Commodities

The model has ten commodities. `COA` and `GAS` are annual zone-level
commodities; `BIO`, `NUC`, `LANDS`, `LANDW`, and `CO2` are annual; and
`HYD`, `H2`, and `ELC` are hourly. Electricity is balanced by region.
Solar and wind carry no fuel: their technologies are driven by a weather
profile and consume nothing but land. The two land commodities are
separate pools, so solar siting and wind siting do not compete for the
same hectares.

``` r

COA <- newCommodity(
  name = "COA",
  desc = "Steam coal",
  unit = "PJ",
  timeframe = "ANNUAL", # no sub-annual balances
  geoframe = "zone", # balancing on zone-level makes it moving freely
  # between all regions in each zone
  emis = data.frame(
    comm = "CO2",
    unit = "kt/PJ",
    emis = 95
  )
)

GAS <- newCommodity(
  name = "GAS",
  desc = "Natural gas",
  unit = "PJ",
  timeframe = "ANNUAL",
  geoframe = "zone",
  emis = data.frame(
    comm = "CO2",
    unit = "kt/PJ",
    emis = 56
  )
)

BIO <- newCommodity(
  name = "BIO",
  desc = "Biomass residues, carbon-neutral",
  unit = "PJ",
  timeframe = "ANNUAL",
  geoframe = "region" # the lower level in Topia is named "region"
)

NUC <- newCommodity(
  name = "NUC",
  desc = "Nuclear fuel assemblies",
  unit = "PJ",
  timeframe = "ANNUAL"
)

HYD <- newCommodity(
  name = "HYD",
  desc = "River inflow",
  unit = "GWh",
  timeframe = "HOUR"
)

LANDS <- newCommodity(
  name = "LANDS",
  desc = "Land suitable for solar siting",
  unit = "km2",
  timeframe = "ANNUAL",
  geoframe = "region"
)

LANDW <- newCommodity(
  name = "LANDW",
  desc = "Land suitable for wind siting",
  unit = "km2",
  timeframe = "ANNUAL",
  geoframe = "region",
  # tiers, not labels: ridgeline and inland terrain must not pool, so the
  # commodity becomes one balance per tier and a clustered wind farm draws
  # on the tier its own cluster names
  cluster = data.frame(
    cluster = c("HI", "LO"),
    desc = c("exposed ridges and coast", "open inland terrain"),
    order = 1:2
  )
)

H2 <- newCommodity(
  name = "H2",
  desc = "Hydrogen, lower heating value",
  unit = "GWh",
  timeframe = "HOUR",
  geoframe = "region"
)

ELC <- newCommodity(
  name = "ELC",
  desc = "Electricity",
  unit = "GWh",
  timeframe = "HOUR"
)

CO2 <- newCommodity(
  name = "CO2",
  desc = "Carbon dioxide",
  unit = "kt",
  timeframe = "ANNUAL"
)

repo_commodities <- newRepository(
  name = "topia_commodities",
  COA, GAS, BIO, NUC, LANDS, LANDW, HYD, H2, ELC, CO2
)
```

## Supply and resources

Each fossil fuel comes from one zone and one only: coal from `WEST`, gas
from `EAST`. `CENTRAL` has neither, so everything it burns arrives over
a corridor or from the rest of the world – which is what gives the map a
direction and the corridors something to carry. Biomass is available in
each region, and nuclear fuel is supplied in `C1`. For zone-pooled
fuels, `ava.up` applies to the entire zone.

None of the three is a single price. A resource gets dearer as it is
used up – the accessible seam first, then the deep one – and that is a
**supply curve**: the quantity split into steps, each dearer than the
last, declared as `cluster`s of one supply object. The model buys the
cheap step first because it is cheaper, not because anything orders it
to.

``` r

# A curve that does not move is a snapshot. Both fossil curves travel: the
# quantity on every step grows as more of the resource becomes reachable,
# while the price goes the other way for coal (mechanization outruns
# depletion) and the same way for gas (the easy fields go first).
fuel_steps <- data.frame(
  cluster = c("S1", "S2", "S3"),
  order = 1:3
)

fuel_path <- tibble::tribble(
  ~fuel,  ~year, ~price_x, ~volume_x,
  "COA",   2025,     1.00,      1.00,
  "COA",   2030,     0.95,      1.10,
  "COA",   2040,     0.88,      1.25,
  "COA",   2050,     0.80,      1.40,
  "GAS",   2025,     1.00,      1.00,
  "GAS",   2030,     1.05,      1.15,
  "GAS",   2040,     1.15,      1.35,
  "GAS",   2050,     1.30,      1.50
)

# The 2025 curve, which the path above moves. `asSupplyCurve()` is the
# shortcut when a curve stands still -- its `price` is absolute, so it cannot
# carry a trajectory, and the steps are written out instead.
fuel_base <- tibble::tribble(
  ~fuel, ~region,   ~cluster, ~price0, ~ava0,
  "COA",  "WEST",    "S1",       1.4,    100,
  "COA",  "WEST",    "S2",       1.8,    100,
  "COA",  "WEST",    "S3",       2.6,    100,
  "GAS",  "EAST",    "S1",       3.2,    120,
  "GAS",  "EAST",    "S2",       4.0,    120,
  "GAS",  "EAST",    "S3",       5.6,    120
)

# every step meets every year, which is the expansion wanted here -- said
# out loud, because dplyr warns about a many-to-many join it has to infer
fuel_supply <- fuel_base |>
  left_join(fuel_path, by = "fuel", relationship = "many-to-many") |>
  mutate(cost = price0 * price_x, ava.up = ava0 * volume_x) |>
  select(fuel, region, cluster, year, ava.up, cost) |>
  arrange(fuel, cluster, year)

coa_supply <- fuel_supply |> filter(fuel == "COA") |> select(-fuel)
gas_supply <- fuel_supply |> filter(fuel == "GAS") |> select(-fuel)

knitr::kable(
  coa_supply,
  digits = 2,
  caption = "Coal: three steps, each cheaper and larger over the horizon"
)
```

| region | cluster | year | ava.up | cost |
|:-------|:--------|-----:|-------:|-----:|
| WEST   | S1      | 2025 |    100 | 1.40 |
| WEST   | S1      | 2030 |    110 | 1.33 |
| WEST   | S1      | 2040 |    125 | 1.23 |
| WEST   | S1      | 2050 |    140 | 1.12 |
| WEST   | S2      | 2025 |    100 | 1.80 |
| WEST   | S2      | 2030 |    110 | 1.71 |
| WEST   | S2      | 2040 |    125 | 1.58 |
| WEST   | S2      | 2050 |    140 | 1.44 |
| WEST   | S3      | 2025 |    100 | 2.60 |
| WEST   | S3      | 2030 |    110 | 2.47 |
| WEST   | S3      | 2040 |    125 | 2.29 |
| WEST   | S3      | 2050 |    140 | 2.08 |

Coal: three steps, each cheaper and larger over the horizon {.table}

``` r


SUP_COA <- newSupply(
  name = "SUP_COA",
  desc = "Domestic coal, pooled within a zone",
  units = data.frame(
    activity = "PJ",
    costs = "MUSD"
  ),
  commodity = "COA",
  unit = "PJ",
  region = "WEST",
  cluster = fuel_steps |>
    mutate(desc = c("open cast", "shallow seam", "deep seam")),
  supply = coa_supply
)

SUP_GAS <- newSupply(
  name = "SUP_GAS",
  desc = "Domestic gas, pooled within a zone",
  units = data.frame(
    activity = "PJ",
    costs = "MUSD"
  ),
  commodity = "GAS",
  unit = "PJ",
  region = "EAST",
  cluster = fuel_steps |>
    mutate(desc = c("associated gas", "conventional", "tight")),
  supply = gas_supply
)
autoplot(SUP_GAS, style = "map", map = gs)
```

![](topia-build_files/figure-html/supply-fuels-1.png)

``` r



# Biomass is an agricultural residue, so it follows the land rather than the
# geology: plentiful and cheap across the central belt, thin and dear at
# either end. Three steps per region -- the easy quarter, the bulk, and the
# tail that has to be collected from far away.
bio <- tibble::tribble(
  ~region, ~bio_PJ, ~bio_cost,
  "W1",       25,       7.5,
  "W2",       30,       7.0,
  "C1",       55,       5.5,
  "C2",       60,       5.0,
  "C3",       55,       5.5,
  "C4",       45,       6.0,
  "C5",       40,       6.5,
  "C6",       35,       7.0,
  "E1",       20,       8.0,
  "E2",       18,       8.5,
  "E3",       15,       9.0
)

bio_steps <- data.frame(
  cluster = c("S1", "S2", "S3"),
  desc = c("residues at the mill", "field collection", "long haul"),
  order = 1:3
)

# `asSupplyCurve()` prices steps in ABSOLUTE terms, which would flatten the
# regional spread above, so the steps are written out: each region keeps its
# own price level and the three steps scale around it.
bio_supply <- bio |>
  tidyr::crossing(bio_steps["cluster"]) |>
  mutate(
    share = c(S1 = 0.25, S2 = 0.45, S3 = 0.30)[cluster],
    mult  = c(S1 = 0.75, S2 = 1.00, S3 = 1.50)[cluster],
    ava.up = bio_PJ * share,
    cost = bio_cost * mult
  ) |>
  select(region, cluster, ava.up, cost) |>
  arrange(match(region, regions), cluster)

knitr::kable(
  head(bio_supply, 6),
  digits = 2,
  caption = "Biomass supply steps, the first two regions"
)
```

| region | cluster | ava.up |  cost |
|:-------|:--------|-------:|------:|
| W1     | S1      |   6.25 |  5.62 |
| W1     | S2      |  11.25 |  7.50 |
| W1     | S3      |   7.50 | 11.25 |
| W2     | S1      |   7.50 |  5.25 |
| W2     | S2      |  13.50 |  7.00 |
| W2     | S3      |   9.00 | 10.50 |

Biomass supply steps, the first two regions {.table}

``` r


SUP_BIO <- newSupply(
  name = "SUP_BIO",
  desc = "Biomass residues, collected locally",
  units = data.frame(
    activity = "PJ",
    costs = "MUSD"
  ),
  commodity = "BIO",
  unit = "PJ",
  region = regions,
  cluster = bio_steps,
  supply = bio_supply
)
autoplot(SUP_BIO, style = "map", map = gs, param = "cost")
```

![](topia-build_files/figure-html/supply-fuels-2.png)

``` r

autoplot(SUP_BIO)
```

![](topia-build_files/figure-html/supply-fuels-3.png)

``` r



SUP_NUC <- newSupply(
  name = "SUP_NUC",
  desc = "Nuclear fuel assemblies",
  units = data.frame(
    activity = "PJ",
    costs = "MUSD"
  ),
  commodity = "NUC",
  unit = "PJ",
  region = "C1",
  supply = data.frame(region = "C1", cost = 0.9)
)
autoplot(SUP_NUC, style = "map", map = gs)
```

![](topia-build_files/figure-html/supply-fuels-4.png)

Hydro inflow is available in `C1`, `C4`, and `E1` at zero cost. Solar
and wind draw no fuel, but both occupy land, and a land pool replaces
their former build limits. The pools are separate because the terrain
is: `RES_LANDS` is the open ground a photovoltaic field can take,
`RES_LANDW` the exposed terrain a wind farm needs. Each region’s budget
is the land its former cap would have occupied, at 20 km2 per GW of
solar and 100 per GW of wind.

``` r

siting <- tibble::tribble(
  ~region, ~sol_GW, ~win_GW,
  "W1",        6.0,     1.2,
  "W2",        6.0,     1.2,
  "C1",        2.0,     2.5,
  "C2",        2.0,     2.5,
  "C3",        2.0,     2.5,
  "C4",        2.0,     2.5,
  "C5",        2.0,     2.5,
  "C6",        2.0,     2.5,
  "E1",        1.5,    12.0,
  "E2",        1.5,    12.0,
  "E3",        1.5,    12.0
)

knitr::kable(
  siting,
  caption = "Solar and wind capacity the land pool is sized for, GW"
)
```

| region | sol_GW | win_GW |
|:-------|-------:|-------:|
| W1     |    6.0 |    1.2 |
| W2     |    6.0 |    1.2 |
| C1     |    2.0 |    2.5 |
| C2     |    2.0 |    2.5 |
| C3     |    2.0 |    2.5 |
| C4     |    2.0 |    2.5 |
| C5     |    2.0 |    2.5 |
| C6     |    2.0 |    2.5 |
| E1     |    1.5 |   12.0 |
| E2     |    1.5 |   12.0 |
| E3     |    1.5 |   12.0 |

Solar and wind capacity the land pool is sized for, GW {.table}

``` r

RES_LANDS <- newSupply(
  name = "RES_LANDS",
  desc = "Land suitable for solar siting",
  units = data.frame(
    activity = "km2",
    costs = "MUSD"
  ),
  commodity = "LANDS",
  unit = "km2",
  region = regions,
  supply = siting |> transmute(region, ava.up = sol_GW * 20)
)
autoplot(RES_LANDS, style = "map", map = gs)
```

![](topia-build_files/figure-html/supply-renewables-1.png)

``` r



RES_LANDW <- newSupply(
  name = "RES_LANDW",
  desc = "Land suitable for wind siting",
  units = data.frame(
    activity = "km2",
    costs = "MUSD"
  ),
  commodity = "LANDW",
  unit = "km2",
  region = regions,
  cluster = data.frame(
    cluster = c("HI", "LO"),
    desc = c("exposed ridges and coast", "open inland terrain"),
    order = 1:2
  ),
  supply = bind_rows(
    siting |> transmute(region, cluster = "HI", ava.up = win_GW * 30),
    siting |> transmute(region, cluster = "LO", ava.up = win_GW * 100)
  )
)
autoplot(RES_LANDW, style = "map", map = gs)
```

![](topia-build_files/figure-html/supply-renewables-2.png)

``` r


RES_HYD <- newSupply(
  name = "RES_HYD",
  desc = "River inflow to the reservoirs",
  units = data.frame(
    activity = "GWh",
    costs = "MUSD"
  ),
  commodity = "HYD",
  unit = "GWh",
  region = c("C1", "C4", "E1"),
  supply = data.frame(
    region = c("C1", "C4", "E1"),
    cost = 0
  )
)
autoplot(RES_HYD, style = "map", map = gs)
```

![](topia-build_files/figure-html/supply-renewables-3.png)

## Rest-of-world trade

Coal and gas can be imported without a quantity limit in each zone at
3.5 and 9.0 MUSD/PJ, respectively. Hydrogen can be sold outward from any
region at 0.09 MUSD/GWh, capped at 38 GWh per region in each timeslice:
the demand that makes electrolysis worth building.

``` r

IMP_COA <- newImport(
  name = "IMP_COA",
  desc = "Coal import from the rest of the world",
  commodity = "COA",
  unit = "PJ",
  region = c("WEST", "CENTRAL", "EAST"),
  import = data.frame(
    region = c("WEST", "CENTRAL", "EAST"),
    price = 3.5
  )
)
autoplot(IMP_COA, style = "map", map = gs)
```

![](topia-build_files/figure-html/imports-1.png)

``` r



IMP_GAS <- newImport(
  name = "IMP_GAS",
  desc = "LNG import from the rest of the world",
  commodity = "GAS",
  unit = "PJ",
  region = c("WEST", "CENTRAL", "EAST"),
  import = data.frame(
    region = c("WEST", "CENTRAL", "EAST"),
    price = 9.0
  )
)
autoplot(IMP_GAS, style = "map", map = gs)
```

![](topia-build_files/figure-html/imports-2.png)

``` r



EXP_H2 <- newExport(
  name = "EXP_H2",
  desc = "Hydrogen export to the rest of the world",
  commodity = "H2",
  unit = "GWh",
  region = regions,
  export = data.frame(
    region = regions,
    price = 0.09,
    exp.up = 38
  )
)
autoplot(EXP_H2, style = "map", map = gs)
```

![](topia-build_files/figure-html/imports-3.png)

``` r



repo_supply <- newRepository(
  name = "topia_supply",
  SUP_COA, SUP_GAS, SUP_BIO, SUP_NUC,
  RES_LANDS, RES_LANDW, RES_HYD,
  IMP_COA, IMP_GAS
)
```

## Weather

`WSOL`, `WWIN`, and `WHYD` contain hourly capacity factors by region and
timeslice for solar, wind, and hydro.

``` r

wsol <- prof$weather |>
  filter(resource == "WSOL") |>
  select(region, timeslice, wval)

wwin_hi <- prof$weather |>
  filter(resource == "WWIN") |>
  select(region, timeslice, wval) |>
  mutate(cluster = "HI")

# Inland sites are both weaker and steadier: three quarters of the level, and
# the hour-to-hour swing pulled to 60% of the exposed grade's.
wwin_lo <- wwin_hi |>
  mutate(wval = 0.75 * (mean(wval) + 0.6 * (wval - mean(wval))),
         cluster = "LO",
         .by = region)

wwin <- bind_rows(wwin_hi, wwin_lo)

whyd <- prof$weather |>
  filter(resource == "WHYD") |>
  select(region, timeslice, wval)

WSOL <- newWeather(
  name = "WSOL",
  desc = "Solar capacity factor by timeslice and region",
  timeframe = "HOUR",
  weather = wsol
)

# autoplot(WSOL, region = "C1")

WWIN <- newWeather(
  name = "WWIN",
  desc = "Wind capacity factor by timeslice, region and site grade",
  timeframe = "HOUR",
  cluster = data.frame(
    cluster = c("HI", "LO"),
    desc = c("exposed ridges and coast", "open inland terrain"),
    order = 1:2
  ),
  weather = wwin
)

WHYD <- newWeather(
  name = "WHYD",
  desc = "Hydro inflow factor by timeslice and region",
  timeframe = "HOUR",
  weather = whyd
)

repo_weather <- newRepository(
  name = "topia_weather",
  WSOL, WWIN, WHYD
)
```

Solar capacity factors decline from west to east. Wind profiles
interpolate from weaker daytime wind in the west to stronger nocturnal,
winter-peaking wind in the east.

``` r

cf_summary <- prof$weather |>
  filter(resource %in% c("WSOL", "WWIN")) |>
  summarise(cf = mean(wval), .by = c(resource, region)) |>
  tidyr::pivot_wider(names_from = resource, values_from = cf) |>
  arrange(match(region, regions))

knitr::kable(cf_summary, digits = 3, caption = "Annual mean capacity factor by region")
```

| region |  WSOL |  WWIN |
|:-------|------:|------:|
| W1     | 0.171 | 0.140 |
| W2     | 0.166 | 0.183 |
| C1     | 0.161 | 0.226 |
| C2     | 0.156 | 0.269 |
| C3     | 0.151 | 0.312 |
| C4     | 0.146 | 0.355 |
| C5     | 0.141 | 0.398 |
| C6     | 0.136 | 0.441 |
| E1     | 0.131 | 0.484 |
| E2     | 0.126 | 0.527 |
| E3     | 0.121 | 0.570 |

Annual mean capacity factor by region {.table}

``` r

autoplot(WSOL, calendar = calendar, region = c("C1", "W2"))
```

![](topia-build_files/figure-html/weather-plot-1.png)

## Demand

`DEM_ELC` defines hourly electricity demand for eleven regions and four
milestone years. Western profiles have an evening peak; central profiles
are flatter and more industrial. The central regions are the industrial
load centre, the eastern ones the lightest.

``` r

levels <- tibble::tribble(
  ~region, ~annual_GWh,
  "W1", 22000,
  "W2", 22000,
  "C1", 39000,
  "C2", 39000,
  "C3", 39000,
  "C4", 39000,
  "C5", 39000,
  "C6", 39000,
  "E1", 17000,
  "E2", 17000,
  "E3", 17000
)

knitr::kable(
  levels,
  caption = "Annual electricity demand in 2025, GWh"
)
```

| region | annual_GWh |
|:-------|-----------:|
| W1     |      22000 |
| W2     |      22000 |
| C1     |      39000 |
| C2     |      39000 |
| C3     |      39000 |
| C4     |      39000 |
| C5     |      39000 |
| C6     |      39000 |
| E1     |      17000 |
| E2     |      17000 |
| E3     |      17000 |

Annual electricity demand in 2025, GWh {.table}

Demand grows from a factor of 1.0 in 2025 to 1.5 in 2050. Each regional
profile is normalized and scaled to its annual demand level.

``` r

growth <- tibble::tribble(
  ~year, ~factor,
  2025, 1.0,
  2030, 1.1,
  2040, 1.3,
  2050, 1.5
)

tsl_share <- calendar@timeslice_share |>
  as.data.frame() |>
  select(timeslice, share)

shape <- prof$demand |>
  left_join(tsl_share, by = "timeslice") |>
  mutate(w = load * share) |>
  mutate(w = w / sum(w), .by = region)

dem_elc <- shape |>
  left_join(levels, by = "region") |>
  tidyr::crossing(growth) |>
  mutate(demand = annual_GWh * factor * w) |>
  select(region, year, timeslice, demand)

DEM_ELC <- newDemand(
  name = "DEM_ELC",
  desc = "Electricity demand",
  commodity = "ELC",
  unit = "GWh",
  demand = dem_elc
)

repo_demand <- newRepository(
  name = "topia_demand",
  DEM_ELC
)
```

The projected demand data, interpolated across years can be displayed as
`"area"` plot of the annual totals stacked by region over every year
between the first and last milestone. The bands are interpolated, the
points are the four milestone years as given, drawn on the edge of their
own band.

``` r

autoplot(DEM_ELC, style = "area")
```

![](topia-build_files/figure-html/demand-annual-1.png)

Sub-annual profiles can be displayed by changing the style to `"line"`
or `"heatmap"`, filtered by region and year.

``` r

autoplot(DEM_ELC, calendar = calendar, style = "line", 
         region = c("C1", "E2"), year = 2025)
```

![](topia-build_files/figure-html/demand-plot-1.png)

``` r


autoplot(DEM_ELC, calendar = calendar, style = "heatmap", 
         region = c("C1"), year = 2025)
```

![](topia-build_files/figure-html/demand-plot-2.png)

## Technologies

The model includes seven electricity-generation technologies. `cap2act`
converts GW of capacity to annual GWh; fuel-based technologies also
convert PJ of input to GWh of output.

### Coal

`ECOA` is a 35%-efficient subcritical coal plant. Its existing regional
fleet declines to 30% of 2025 capacity by 2050, may retire early, and
cannot be built after 2030.

``` r

# Capacity fleat: (arbitrary) assignment by region
coal_fleet <- tibble::tribble(
  ~region, ~GW,
  "W1",      8,
  "W2",      4,
  "C1",      1,
  "C2",      1,
  "C3",      1,
  "C4",      1,
  "C5",      3,
  "C6",      1,
  "E1",      1,
  "E2",      1,
  "E3",      1
)

# Explicit retirement of existing capacity is set by defining the stock by year
coal_cap <- bind_rows(
  coal_fleet |> mutate(year = 2025, stock = GW),
  coal_fleet |> mutate(year = 2050, stock = GW * 0.3)
) |>
  select(region, year, stock)

# Technological process is defined for each type of technology with unique name
ECOA <- newTechnology(
  name = "ECOA",
  desc = "Coal-fired power plant, subcritical",
  units = data.frame(
    capacity = "GW",
    activity = "GWh",
    costs = "MUSD"
  ),
  input = list(
    comm = "COA",
    unit = "PJ",
    combustion = 1
  ),
  output = list(
    comm = "ELC",
    unit = "GWh"
  ),
  ceff = data.frame(
    comm     = c("COA", "ELC"),
    cinp2use = c(0.40, NA), # 40% conversion efficiency
    use2cact = c(NA, convert("PJ", "GWh", 1)) # the PJ -> GWh bridge
  ),
  cap2act = convert("GW*year", "GWh", 1),
  invcost = data.frame(
    invcost = convert("USD/kW", "MUSD/GW", 2000),
    retcost = convert("USD/kW", "MUSD/GW", 25) # paid on early retirement only
  ),
  fixom = 55,
  vintage = data.frame(
    start = 2010L,
    end = 2030L, # no new coal after 2030
    olife = 30L
  ),
  optimizeRetirement = TRUE,
  capacity = coal_cap
)

# A schematic view of the process with key parameters
draw(ECOA)
```

![](topia-build_files/figure-html/fleet-coal-1.png)

### Gas

`EGAS` is a 58%-efficient combined-cycle gas plant. Its existing fleet
declines to 60% of 2025 capacity by 2050, may retire early, and remains
available for new construction throughout the horizon.

``` r

gas_fleet <- tibble::tribble(
  ~region, ~GW,
  "W1",      1,
  "W2",      2,
  "C1",      1,
  "C2",      1,
  "C3",      1,
  "C4",      1,
  "C5",      1,
  "C6",      2,
  "E1",      2,
  "E2",      6,
  "E3",      4
)

gas_cap <- bind_rows(
  gas_fleet |> mutate(year = 2025, stock = GW),
  gas_fleet |> mutate(year = 2050, stock = GW * 0.6)
) |>
  select(region, year, stock)

EGAS <- newTechnology(
  name = "EGAS",
  desc = "Gas-fired combined-cycle plant",
  units = data.frame(
    capacity = "GW",
    activity = "GWh",
    costs = "MUSD"
  ),
  input = list(
    comm = "GAS", 
    unit = "PJ", 
    combustion = 1),
  output = list(
    comm = "ELC", 
    unit = "GWh"),
  ceff = data.frame(
    comm     = c("GAS", "ELC"),
    cinp2use = c(0.58, NA),
    use2cact = c(NA, convert("PJ", "GWh", 1))
  ),
  cap2act = convert("GW*year", "GWh", 1),
  invcost = list(
    invcost = convert("USD/kW", "MUSD/GW", 900)
    ),
  fixom = 25,
  vintage = data.frame(
    start = 2010L,
    olife = 30L
  ),
  optimizeRetirement = TRUE,
  capacity = gas_cap
)
```

### Nuclear

`ENUC` is a 4 GW station in `C1` with 35% conversion efficiency, a 70%
minimum availability factor, a 50-year life, and new builds allowed from
2030.

``` r

ENUC <- newTechnology(
  name = "ENUC",
  desc = "Nuclear power plant",
  units = data.frame(
    capacity = "GW",
    activity = "GWh",
    costs = "MUSD"
  ),
  region = "C1",
  input = list(comm = "NUC", unit = "PJ"),
  output = list(comm = "ELC", unit = "GWh"),
  ceff = data.frame(
    comm     = c("NUC", "ELC"),
    cinp2use = c(0.35, NA),
    use2cact = c(NA, convert("PJ", "GWh", 1))
  ),
  af = data.frame(
    af.lo = 0.7 # lower bound on hourly generation (must work)
  ),
  cap2act = convert("GW*year", "GWh", 1),
  invcost = data.frame(
    invcost = convert("USD/kW", "MUSD/GW", 8000)
  ),
  fixom = 120,
  vintage = data.frame(
    start = 2030L,
    olife = 50L
  ),
  capacity = data.frame(
    region = "C1",
    year = c(2025, 2050),
    stock = c(4, 4)
  )
)

draw(ENUC)
```

![](topia-build_files/figure-html/tech-nuclear-1.png)

### Solar and wind

`ESOL` and `EWIN` have no fuel: the hourly weather profile bounds their
output directly, and their land pool bounds how much may be built. Each
region starts with 1 GW of legacy solar and wind capacity, declining to
zero by 2050.

`EWIN` carries two site grades as clusters, and the grades reach through
every object the wind chain touches. `WWIN` holds a profile per grade:
`HI` is the exposed ridge and coast as measured, `LO` the open inland
terrain, weaker and steadier. `LANDW` is declared in the same two tiers,
so the commodity becomes one balance per grade and ridgeline cannot be
spent on an inland farm; `RES_LANDW` gives each tier its own budget, the
exposed land being the scarce one. `EWIN` then carries its own cost and
land intensity per grade, and a reference to `WWIN` or `LANDW` resolves
to the matching tier by cluster name.

The grades are not simply better and worse. `HI` yields more energy per
GW and occupies more land per GW, and its land is the scarce kind; `LO`
is weaker but plentiful. Which one a region builds is a question about
its terrain.

``` r

sol_cap <- data.frame(
  vintage = "2015",
  region = rep(regions, times = 2),
  year = rep(c(2025, 2050), each = length(regions)),
  stock = rep(c(1, 0), each = length(regions))
)

ESOL <- newTechnology(
  name = "ESOL",
  desc = "Solar photovoltaic plant",
  units = data.frame(
    capacity = "GW",
    activity = "GWh",
    costs = "MUSD"
  ),
  output = list(
    comm = "ELC", 
    unit = "GWh"
  ),
  weather = list(
    weather = "WSOL", 
    waf.up = 1
  ),
  cap2act = convert("GW*year", "GWh", 1),
  vintage = data.frame(
    vintage = c("2015", "2030", "2045"),
    start = c(2015L, 2030L, 2045L),
    end = c(2025L, 2040L, 2050L),
    olife = c(25L, 30L, 30L)
  ),
  invcost = data.frame(
    vintage = c("2015", "2030", "2045"),
    invcost = convert("USD/kW", "MUSD/GW", c(650, 450, 350))
  ),
  fixom = data.frame(
    vintage = c("2015", "2030", "2045"),
    fixom = c(12, 10, 9)
  ),
  aux = data.frame(
    acomm = "LANDS",
    unit = "km2"
  ),
  aeff = data.frame(
    vintage = c("2015", "2030", "2045"),
    acomm = "LANDS",
    cap2ainp = c(20, 16, 14) # km2 of land per GW standing
  ),
  capacity = sol_cap
)

draw(ESOL)
```

![](topia-build_files/figure-html/tech-solar-1.png)

``` r


# The legacy fleet goes on `LO`. A clustered technology cannot broadcast one
# stock across its clusters, so the grade has to be named -- and 1 GW of
# inherited turbines is not all on the best ridges. It also has to FIT: `HI`
# land is 30 km2 per GW of former cap, and an `HI` farm takes 120 km2 per GW,
# so putting the legacy fleet there would demand land that does not exist and
# the model would be infeasible in its first year. `check_feasibility()` says
# so without a solver.
win_cap <- data.frame(
  vintage = "2015",
  cluster = "LO",
  region = rep(regions, times = 2),
  year = rep(c(2025, 2050), each = length(regions)),
  stock = rep(c(1, 0), each = length(regions))
)

EWIN <- newTechnology(
  name = "EWIN",
  desc = "Onshore wind farm",
  units = data.frame(
    capacity = "GW",
    activity = "GWh",
    costs = "MUSD"
  ),
  output = list(comm = "ELC", unit = "GWh"),
  weather = list(weather = "WWIN", waf.up = 1),
  cap2act = convert("GW*year", "GWh", 1),
  vintage = data.frame(
    vintage = c("2015", "2030", "2045"),
    start = c(2015L, 2030L, 2045L),
    end = c(2025L, 2040L, 2050L),
    olife = c(25L, 30L, 30L)
  ),
  invcost = data.frame(
    vintage = c("2015", "2030", "2045"),
    invcost = convert("USD/kW", "MUSD/GW", c(1300, 1100, 950))
  ),
  fixom = data.frame(
    vintage = c("2015", "2030", "2045"),
    fixom = c(35, 30, 28)
  ),
  cluster = data.frame(
    cluster = c("HI", "LO"),
    desc = c("exposed ridges and coast", "open inland terrain"),
    order = 1:2
  ),
  aux = data.frame(
    acomm = "LANDW",
    unit = "km2"
  ),
  aeff = data.frame(
    cluster = rep(c("HI", "LO"), each = 3),
    vintage = rep(c("2015", "2030", "2045"), times = 2),
    acomm = "LANDW",
    # the windier grade needs wider spacing, so it buys its yield with land
    cap2ainp = c(120, 114, 108, 70, 66, 63)
  ),
  capacity = win_cap
)
draw(EWIN)
```

![](topia-build_files/figure-html/tech-solar-2.png)

### Hydro

`EHYD` represents fixed reservoir hydropower capacity of 3 GW in `C1`,
12 GW in `C4`, and 6 GW in `E1`. The inherited plants have a 60-year
life and cannot be expanded.

``` r

EHYD <- newTechnology(
  name = "EHYD",
  desc = "Reservoir hydropower plant",
  units = data.frame(
    capacity = "GW",
    activity = "GWh",
    costs = "MUSD"
  ),
  region = c("C1", "C4", "E1"),
  input = list(
    comm = "HYD", 
    unit = "GWh"),
  output = list(
    comm = "ELC", 
    unit = "GWh"),
  ceff = data.frame(
    comm = "HYD", 
    cinp2use = 1),
  weather = list(
    weather = "WHYD", 
    comm = "HYD", 
    waf.up = 1),
  cap2act = convert("GW*year", "GWh", 1),
  invcost = list(
    invcost = convert("USD/kW", "MUSD/GW", 3000)
    ),
  fixom = 45,
  vintage = data.frame(
    start = 2010L,
    end = 2010L,
    olife = 60L
  ),
  capacity = data.frame(
    region = rep(c("C1", "C4", "E1"), times = 2),
    year = rep(c(2025, 2050), each = 3),
    stock = rep(c(3, 12, 6), times = 2)
  )
)
autoplot(EHYD, map = gs, style = "map")
```

![](topia-build_files/figure-html/tech-hydro-1.png)

### Biomass

`EBIO` is a dispatchable, carbon-neutral biomass plant available for new
builds from 2030.

``` r

EBIO <- newTechnology(
  name = "EBIO",
  desc = "Biomass power plant, carbon-neutral",
  units = data.frame(
    capacity = "GW",
    activity = "GWh",
    costs = "MUSD"
  ),
  input = list(
    comm = "BIO", 
    unit = "PJ", 
    combustion = 1
  ),
  output = list(
    comm = "ELC", 
    unit = "GWh"),
  ceff = data.frame(
    comm     = c("BIO", "ELC"),
    cinp2use = c(0.35, NA),
    use2cact = c(NA, convert("PJ", "GWh", 1))
  ),
  cap2act = convert("GW*year", "GWh", 1),
  invcost = list(invcost = convert("USD/kW", "MUSD/GW", 1600)),
  fixom = 60,
  vintage = data.frame(
    start = 2030L,
    olife = 30L
  ),
  optimizeRetirement = TRUE
)
draw(EBIO)
```

![](topia-build_files/figure-html/tech-biomass-1.png)

``` r


repo_generators <- newRepository(
  name = "topia_generators",
  ECOA, EGAS, ENUC, ESOL, EWIN, EHYD, EBIO
)
summary(repo_generators)
#> technology 
#>          7
```

### Hydrogen

`H2ELC` is an electrolyser: it consumes electricity and produces
hydrogen. Three vintages become available from 2030, each cheaper and
more efficient than the last. Having an hourly electricity input and an
hourly hydrogen output, it can absorb surplus wind and solar in the
hours when they are abundant, which is what makes `STG_H2` and `EXP_H2`
below worth building.

``` r

H2ELC <- newTechnology(
  name = "H2ELC",
  desc = "Water electrolyser, grid-connected",
  units = data.frame(
    capacity = "GW",
    activity = "GWh",
    costs = "MUSD"
  ),
  input = list(
    comm = "ELC",
    unit = "GWh"
  ),
  output = list(
    comm = "H2",
    unit = "GWh"
  ),
  ceff = data.frame(
    vintage = c("2030", "2040", "2050"),
    comm = "ELC",
    cinp2use = c(0.65, 0.70, 0.74) # GWh of H2 per GWh of electricity
  ),
  cap2act = convert("GW*year", "GWh", 1),
  vintage = data.frame(
    vintage = c("2030", "2040", "2050"),
    start = c(2030L, 2040L, 2050L),
    end = 2050L,
    olife = 25L
  ),
  invcost = data.frame(
    vintage = c("2030", "2040", "2050"),
    invcost = convert("USD/kW", "MUSD/GW", c(1400, 1000, 800))
  ),
  fixom = data.frame(
    vintage = c("2030", "2040", "2050"),
    fixom = c(40, 34, 30)
  )
)
draw(H2ELC)
```

![](topia-build_files/figure-html/tech-h2-1.png)

## Storage

`STG_ELC` is a four-hour lithium-ion battery with 95% input and output
efficiencies. `stg.invcost` prices energy capacity in MUSD/GWh;
`out.invcost` prices discharge power in MUSD/GW. Three vintages become
available over the horizon, each cheaper than the last.

``` r

STG_ELC <- newStorage(
  name = "STG_ELC",
  desc = "Lithium-ion battery, four-hour reservoir",
  units = data.frame(
    capacity = "GW",
    activity = "GWh",
    costs = "MUSD"
  ),
  commodity = "ELC",
  cap2act = convert("GW*year", "GWh", 1),
  duration = 4, # GWh per GW = hours
  vintage = data.frame(
    vintage = c("2025", "2035", "2045"),
    start = c(2025L, 2035L, 2045L),
    end = c(2030L, 2040L, 2050L),
    olife = c(20L, 20L, 25L)
  ),
  invcost = data.frame(
    vintage = c("2025", "2035", "2045"),
    stg.invcost = convert("USD/kWh", "MUSD/GWh", c(200, 110, 70)),
    out.invcost = convert("USD/kW", "MUSD/GW", c(150, 120, 95))
  ),
  seff = data.frame(inpeff = 0.95, outeff = 0.95)
)
draw(STG_ELC)
```

![](topia-build_files/figure-html/storage-1.png)

``` r


STG_H2 <- newStorage(
  name = "STG_H2",
  desc = "Hydrogen store, two hundred hours",
  units = data.frame(
    capacity = "GW",
    activity = "GWh",
    costs = "MUSD"
  ),
  commodity = "H2",
  cap2act = convert("GW*year", "GWh", 1),
  duration = 200, # GWh per GW = hours
  vintage = data.frame(olife = 30L),
  invcost = data.frame(
    stg.invcost = convert("USD/kWh", "MUSD/GWh", 2),
    out.invcost = convert("USD/kW", "MUSD/GW", 400)
  ),
  seff = data.frame(inpeff = 0.98, outeff = 0.98)
)
draw(STG_H2)
```

![](topia-build_files/figure-html/storage-2.png)

``` r


repo_storage <- newRepository(
  name = "topia_storage",
  STG_ELC
)

# the hydrogen chain is declared across three sections: the electrolyser with
# the technologies, the store here, the export with the rest-of-world trade
repo_hydrogen <- newRepository(
  name = "topia_hydrogen",
  H2ELC, STG_H2, EXP_H2
)
```

## Inter-regional trade

`topia$links` is the transmission network, a symmetric region-by-region
matrix read off the four map layouts in `topia$geoscales`: two regions
are linked when they share a border on most of them, so a link never
crosses another on the map it was taken from. `W1` is the one exception
– it is a detached island on two layouts and is joined to `W2` so that
no region is stranded.

``` r

links <- topia$links[regions, regions]

knitr::kable(
  ifelse(links, "x", ""),
  caption = "Transmission network: a cross marks a linked pair"
)
```

|     | W1  | W2  | C1  | C2  | C3  | C4  | C5  | C6  | E1  | E2  | E3  |
|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|
| W1  |     | x   |     |     |     |     |     |     |     |     |     |
| W2  | x   |     | x   |     | x   |     |     |     |     |     |     |
| C1  |     | x   |     | x   |     |     |     |     |     |     |     |
| C2  |     |     | x   |     | x   |     |     |     |     |     |     |
| C3  |     | x   |     | x   |     | x   |     |     |     |     |     |
| C4  |     |     |     |     | x   |     | x   |     |     |     |     |
| C5  |     |     |     |     |     | x   |     | x   | x   |     |     |
| C6  |     |     |     |     |     |     | x   |     |     |     |     |
| E1  |     |     |     |     |     |     | x   |     |     | x   |     |
| E2  |     |     |     |     |     |     |     |     | x   |     | x   |
| E3  |     |     |     |     |     |     |     |     |     | x   |     |

Transmission network: a cross marks a linked pair {.table}

Each linked pair becomes one bidirectional corridor carrying `ELC`, with
3 GW of existing capacity, 97% transfer efficiency, 350 MUSD/GW
investment cost and a 2 USD/MWh wheeling cost. `cap2act` converts
corridor capacity from GW to GWh. The upper triangle is enough: a pair
is one object, declared in both directions inside it.

``` r

pairs <- which(links & upper.tri(links), arr.ind = TRUE)

repo_transmission <- newRepository("topia_transmission")

for (i in seq_len(nrow(pairs))) {
  a <- regions[pairs[i, 1]]
  b <- regions[pairs[i, 2]]
  trd <- newTrade(
    name = paste0("TBD_ELC_", a, "_", b),
    desc = paste0("Bidirectional transmission line ", a, " - ", b),
    units = data.frame(
      capacity = "GW",
      activity = "GWh",
      costs = "MUSD"
    ),
    commodity = "ELC",
    routes = data.frame(
      src = c(a, b),
      dst = c(b, a)
    ),
    trade = data.frame(
      src = c(a, b),
      dst = c(b, a),
      teff = 0.97
    ),
    cap2act = convert("GW*year", "GWh", 1),
    capacity = data.frame(stock = 3),
    invcost = data.frame(
      region = c(a, b),
      invcost = 350
    ),
    varom = data.frame(
      src = c(a, b),
      dst = c(b, a),
      varom = convert("USD/MWh", "MUSD/GWh", 2)
    ),
    vintage = data.frame(olife = 50L)
  )
  repo_transmission <- add(repo_transmission, trd)
}

names(repo_transmission)
#>  [1] "TBD_ELC_W1_W2" "TBD_ELC_W2_C1" "TBD_ELC_C1_C2" "TBD_ELC_W2_C3"
#>  [5] "TBD_ELC_C2_C3" "TBD_ELC_C3_C4" "TBD_ELC_C4_C5" "TBD_ELC_C5_C6"
#>  [9] "TBD_ELC_C5_E1" "TBD_ELC_E1_E2" "TBD_ELC_E2_E3"
# repo_transmission@data$TBD_ELC_C4_C6 <- NULL
```

### Fuel corridors

Coal rail corridors and gas pipelines connect adjacent zones. They are
written out rather than looped because they live at a different
geoframe: `COA` and `GAS` are balanced per zone, so their corridors run
zone to zone and are not in the region matrix.

The two fuels move differently, which is the point of separating them.
Coal runs on **two** corridors and in **both** directions on each, so a
surplus can travel either way along `WEST - CENTRAL - EAST`. Gas has
**one** pipe and it runs **one way**, `EAST` to `CENTRAL`: a pipeline is
built to deliver from a field to a market, and declaring a single
`routes` row is how that is said – the model then cannot send gas back,
whatever the prices do. Coal corridors carry 60 PJ/year at 99%
efficiency, the gas pipe 40 PJ/year at 98%. Their capacity unit is
PJ/year, so `cap2act = 1`.

``` r

TBD_COA_WEST_CENTRAL <- newTrade(
  name = "TBD_COA_WEST_CENTRAL",
  desc = "Coal rail corridor WEST - CENTRAL",
  units = data.frame(
    capacity = "PJ/year",
    activity = "PJ",
    costs = "MUSD"
  ),
  commodity = "COA",
  routes = data.frame(
    src = c("WEST", "CENTRAL"),
    dst = c("CENTRAL", "WEST")
  ),
  trade = data.frame(
    src = c("WEST", "CENTRAL"),
    dst = c("CENTRAL", "WEST"),
    teff = 0.99
  ),
  cap2act = 1, # capacity in PJ/yr
  capacity = data.frame(stock = 60),
  invcost = data.frame(
    region = c("WEST", "CENTRAL"),
    invcost = 20
  ),
  varom = data.frame(
    src = c("WEST", "CENTRAL"),
    dst = c("CENTRAL", "WEST"),
    varom = 0.3
  ),
  vintage = data.frame(olife = 40L)
)

TBD_COA_CENTRAL_EAST <- newTrade(
  name = "TBD_COA_CENTRAL_EAST",
  desc = "Coal rail corridor CENTRAL - EAST",
  units = data.frame(
    capacity = "PJ/year",
    activity = "PJ",
    costs = "MUSD"
  ),
  commodity = "COA",
  routes = data.frame(
    src = c("CENTRAL", "EAST"),
    dst = c("EAST", "CENTRAL")
  ),
  trade = data.frame(
    src = c("CENTRAL", "EAST"),
    dst = c("EAST", "CENTRAL"),
    teff = 0.99
  ),
  cap2act = 1,
  capacity = data.frame(stock = 60),
  invcost = data.frame(
    region = c("CENTRAL", "EAST"),
    invcost = 20
  ),
  varom = data.frame(
    src = c("CENTRAL", "EAST"),
    dst = c("EAST", "CENTRAL"),
    varom = 0.3
  ),
  vintage = data.frame(olife = 40L)
)

TBD_GAS_EAST_CENTRAL <- newTrade(
  name = "TBD_GAS_EAST_CENTRAL",
  desc = "Gas pipeline EAST - CENTRAL, one way",
  units = data.frame(
    capacity = "PJ/year",
    activity = "PJ",
    costs = "MUSD"
  ),
  commodity = "GAS",
  # one row, so the pipe runs one way only: gas leaves the producing zone
  routes = data.frame(
    src = "EAST",
    dst = "CENTRAL"
  ),
  trade = data.frame(
    src = "EAST",
    dst = "CENTRAL",
    teff = 0.98
  ),
  cap2act = 1,
  capacity = data.frame(stock = 40),
  invcost = data.frame(
    region = c("EAST", "CENTRAL"),
    invcost = 60
  ),
  varom = data.frame(
    src = "EAST",
    dst = "CENTRAL",
    varom = 0.2
  ),
  vintage = data.frame(olife = 40L)
)

repo_fuel_trade <- newRepository(
  name = "topia_fuel_trade",
  TBD_COA_WEST_CENTRAL, TBD_COA_CENTRAL_EAST,
  TBD_GAS_EAST_CENTRAL
)

names(repo_fuel_trade)
#> [1] "TBD_COA_WEST_CENTRAL" "TBD_COA_CENTRAL_EAST" "TBD_GAS_EAST_CENTRAL"
```

### Drawing the network

Four ready-made views of the same repository.
[`plot_trade_map()`](https://energyRt.org/reference/plot_trade_map.md)
draws straight arrows between region centroids;
[`plot_trade_network()`](https://energyRt.org/reference/plot_trade_network.md)
curves the edges and scales them by capacity, which separates the links
that are not borders on this particular layout.

``` r

plot_trade_map(repo_transmission, map = gs)
```

![](topia-build_files/figure-html/trade-plot-map-1.png)

``` r

plot_trade_map(repo_fuel_trade, map = gs, comm = "GAS", level = "zone")
```

![](topia-build_files/figure-html/trade-plot-map-2.png)

``` r

plot_trade_map(repo_fuel_trade, map = gs, comm = "COA", level = "zone")
```

![](topia-build_files/figure-html/trade-plot-map-3.png)

``` r

plot_trade_network(repo_transmission, map = gs)
```

![](topia-build_files/figure-html/trade-plot-network-1.png)

The fuel corridors run zone to zone, so they are absent from a
region-level drawing and need `level =` to appear at all:

``` r

plot_trade_network(repo_transmission, map = gs, level = "zone")
```

![](topia-build_files/figure-html/trade-plot-zone-1.png)

A single corridor draws on its own:

``` r

autoplot(getObject(repo_transmission, name = "TBD_ELC_C3_C4", drop = TRUE),
         map = gs)
```

![](topia-build_files/figure-html/trade-plot-one-1.png)

## The repository

Each group of objects was collected into its own repository as it was
built: `repo_commodities`, `repo_supply`, `repo_weather`,
`repo_generators`, `repo_storage`, `repo_hydrogen`, `repo_transmission`,
`repo_fuel_trade` and `repo_demand`. A group can be inspected, drawn or
swapped on its own – `repo_generators` is the fleet, `repo_hydrogen` the
whole hydrogen chain declared across three sections, and
`repo_transmission` the electricity grid kept apart from
`repo_fuel_trade` because the two sit at different geoframes and carry
different units. Combining them gives the model’s repository:

``` r

repo <- newRepository(
  name = "topia",
  repo_commodities,
  repo_supply,
  repo_weather,
  repo_generators,
  repo_storage,
  repo_hydrogen,
  repo_transmission,
  repo_fuel_trade,
  repo_demand
)
repo
#> repository 'topia': 48 objects.
```

## The model

`TOPIA` combines the repository with the region set, calendar, horizon,
5% discount rate, and spatial hierarchy.

``` r

TOPIA <- newModel(
  name = "TOPIA",
  currency = "MUSD",
  region = regions,
  calendar = calendar,
  horizon = horizon,
  discount = 0.05,
  data = repo
) |>
  setGeoscale(gs)

TOPIA
#> Name:  TOPIA
```

## Scenario levers

Three optional scenario levers are defined but not added to the base
repository: an emissions cap, a carbon tax, and early-retirement limits.

``` r

CO2_CAP <- newConstraint(
  name = "CO2_CAP",
  eq = "<=",
  for.each = data.frame(
    year = c(2025, 2030, 2040, 2050),
    comm = "CO2"
  ),
  term1 = list(variable = "vEmsFuelTot"),
  rhs = data.frame(
    year = c(2025, 2050),
    rhs = c(5000, 2500) * length(regions)
  ),
  defVal = Inf
)

CT_CO2 <- newTax(
  name = "CT_CO2",
  comm = "CO2",
  tax = data.frame(
    year = c(2025, 2050),
    bal = c(0.02, 0.08)
  )
)
```

`CT_CO2` raises the carbon price from 20 USD/t in 2025 to 80 USD/t in
2050.

``` r

EARLY_RET <- newConstraint(
  name = "EARLY_RET",
  eq = "<=",
  for.each = expand.grid(
    year = c(2030, 2040, 2050),
    tech = c("ECOA", "EGAS"),
    stringsAsFactors = FALSE
  ),
  term1 = list(variable = "vTechCap"),
  rhs = data.frame(
    tech = rep(c("ECOA", "EGAS"), each = 3),
    year = rep(c(2030, 2040, 2050), times = 2),
    rhs = c(
      12, 10, 8, # coal ceiling, GW per region
      7, 6, 5
    ) # gas ceiling, GW per region
  ),
  defVal = Inf
)
```

`EARLY_RET` sets regional coal and gas capacity ceilings for 2030, 2040,
and 2050.

``` r

GREEN_H2 <- newConstraint(
  name = "GREEN_H2",
  desc = "Electrolysis capped by renewable generation in the same hour",
  eq = "<=",
  for.each = expand.grid(
    region = regions,
    year = c(2030, 2040, 2050),
    timeslice = as.character(calendar@timetable$timeslice),
    stringsAsFactors = FALSE
  ),
  term1 = list(
    variable = "vTechInp",
    for.sum = list(tech = "H2ELC", comm = "ELC")
  ),
  term2 = list(
    variable = "vTechOut",
    for.sum = list(tech = c("ESOL", "EWIN", "EHYD"), comm = "ELC"),
    mult = -1
  ),
  rhs = 0,
  defVal = 0
)
```

`GREEN_H2` requires the electrolyser’s electricity draw to stay within
renewable generation in the same region and the same timeslice. It is an
*hourly matching* rule, of the kind used to certify renewable hydrogen,
rather than a physical separation: the same renewable MWh may back both
grid demand and electrolysis, because the model balances one pooled
electricity commodity and cannot trace individual flows. Hydro counts as
renewable here; biomass does not, matching the attribution used in
*TOPIA II*.

Two consequences are worth anticipating. The rule is binding, so
hydrogen output falls and system cost rises. And the hydrogen it
produces is *not* reported as wholly green by an average attribution:
that convention divides the draw among everything generating in the
hour, and fossil plants are still serving other loads at the time. The
constraint and the attribution answer two different questions, which
*TOPIA II* takes up with the solved results.

## Next steps

**TOPIA II: running scenarios** builds and solves scenarios from this
model.
