# Units

``` r

library(energyRt)
```

A model is arithmetic on numbers whose meaning lives outside the
numbers. A coal plant costing `2000` is meaningless until you know
whether that is EUR per kW or MEUR per GW, and whether the coal it burns
is counted in PJ or in tonnes. This article covers where each unit is
declared, how `convert()` moves between them, and what the model checks
for you.

## Where units live

Units are declared in three places, and they answer three different
questions.

| Where | Slot | Question it answers |
|----|----|----|
| Commodity | `@unit` | in what measure is this commodity balanced? |
| Process | `@units` | what are this process’s capacity, activity and cost units? |
| Model | `config@units` | what should everything be expressed in? |

A commodity’s `@unit` is its **native measure** — the one the model
balances it in. Electricity in GWh, coal in PJ, water in Mm3.

``` r

ELC <- newCommodity("ELC", desc = "Electricity", unit = "GWh")
COA <- newCommodity("COA", desc = "Steam coal",  unit = "PJ")
```

A process declares its own units in `@units`, which is where a cost
currency is stated:

``` r

EGAS <- newTechnology(
  name = "EGAS", desc = "Gas turbine",
  input  = data.frame(comm = "GAS", unit = "PJ"),
  output = data.frame(comm = "ELC", unit = "GWh"),
  units  = data.frame(capacity = "GW", activity = "GWh", costs = "MEUR"),
  invcost = data.frame(invcost = 0.9),         # MEUR per GW
  varom   = data.frame(varom = 0.003),         # MEUR per GWh
  cap2act = 8760)                              # 1 GW × 8760 h = 8760 GWh
```

[`getUnits()`](https://energyRt.org/reference/getUnits.md) shows what an
object declares and what each of its parameters is therefore measured
in. It composes the answer from the slots above and the dimensional
meaning of each parameter — investment cost is *costs per capacity*,
variable cost is *costs per activity*, and so on:

``` r

getUnits(EGAS)
#> Units for technology 'EGAS':
#> 
#>     slot parameter comm     unit
#>  cap2act   —          — GWh/GW  
#>  varom     varom      — MEUR/GWh
#>  invcost   invcost    — MEUR/GW
```

Where a process leaves a unit undeclared,
[`getUnits()`](https://energyRt.org/reference/getUnits.md) shows the
placeholder token (`{costs}`, `{capacity}`, `{activity}`) rather than
guessing.

## Dimensions and conversion

Every unit belongs to a **dimension** — energy, mass, volume, time,
currency and so on. Two units are interconvertible when they share one,
and `convert()` is the function that does it:

``` r

convert("PJ", "GWh")        # 1 PJ is how many GWh?
#> [1] 277.7778
convert("kWh", "MJ")
#> [1] 3.6
convert("toe", "GJ")
#> [1] 41.868
```

The third argument is the value to convert, so a whole column can go
through at once:

``` r

convert("EUR/kW", "MEUR/GW", 2000)     # capex catalogue value -> model units
#> [1] 2000
convert("MMBtu", "MWh", c(1, 10, 100))
#> [1]  0.2930711  2.9307111 29.3071111
```

Compound units are built from `/`, `*` and `^`, and SI prefixes attach
to the symbols that accept them. Note that `EUR/kW` and `MEUR/GW` are
the *same* unit — the mega on top cancels the giga underneath — which is
why the call above returns its input unchanged and is worth writing
anyway: it documents the catalogue’s units in the code.

### What deliberately will not convert

Some things are refused rather than guessed at, and the refusals are
deliberate.

**Different dimensions do not convert.** `convert("t", "PJ")` has no
answer without knowing *what substance* is being weighed — that is the
subject of the next section.

**`MBtu` is not accepted.** In oil and gas `M` is the Roman thousand, so
`MBtu` means 1000 Btu; under SI it would mean 1e6 Btu. The two readings
differ by a factor of 1000, so `Btu` does not take SI prefixes at all
and `MMBtu` must be written out. Write `MMBtu`, or `kJ`/`GJ` if you want
prefixes that behave.

**Spellings are case-sensitive.** `mW` is a milliwatt and `MW` is a
megawatt, a factor of 1e9 apart, so there is no case-insensitive
fallback anywhere in the unit system. `BTU` is not `Btu`.

## Physical properties: crossing dimensions

A tonne of coal and a petajoule of coal are the same coal. Moving
between them needs a physical fact — a heating value — and that fact
belongs to the commodity. `commodity@property` is where it lives:

``` r

COA <- newCommodity(
  "COA", desc = "Steam coal", unit = "PJ",
  property = data.frame(
    property = c("lhv", "hhv", "density", "frac_C"),
    value    = c(25.8,  27.1,  0.85,      0.665),
    unit     = c("GJ/t", "GJ/t", "t/m3",  "kg/kg"),
    comment  = c("IEA average, other bituminous", "",
                 "bulk, as stockpiled", "ultimate analysis")))

commodity_property(COA, "lhv")
#> [1] 25.8
```

The recognised property names, and the dimension pair each one relates,
are in
[`commodity_properties()`](https://energyRt.org/reference/commodity_properties.md):

``` r

commodity_properties()[, c("property", "quantity", "default_unit")]
#>        property                  quantity default_unit
#> 1           lhv               Energy/Mass         GJ/t
#> 2           hhv               Energy/Mass         GJ/t
#> 3       density               Mass/Volume         t/m3
#> 4    molar_mass               Mass/Amount        g/mol
#> 5        frac_*             dimensionless        kg/kg
#> 6     density_*               Mass/Volume         t/m3
#> 7 heat_capacity Energy/(Mass*Temperature)      kJ/kg/K
```

The key idea is that **a property whose unit is a ratio of two
dimensions is a conversion factor between them**. `lhv = 25.8 GJ/t` is
an energy-to-mass edge; `density = 0.85 t/m3` is a mass-to-volume edge.
Chain them and a volume of coal becomes an energy. Given the properties,
`convert()` can find that chain itself:

> **Not in this build.** Commodity-aware conversion is not available in
> energyRt 0.89.1.9000. The calls below are shown for reference; their
> output will appear here once the feature lands.

``` r

convert(1, "t",  "GJ", commodity = COA)   # 25.8   — via lhv
convert(1, "m3", "GJ", commodity = COA)   # 21.93  — via density, then lhv
convert(1, COA, "Mt")                     # 0.03876 — COA's native PJ into Mt
convert_path("m3", "GJ", commodity = COA) # which properties it used
```

Because a commodity can stand wherever a unit string can, two
commodities can be compared directly — but only once you say **what is
being held constant**:

``` r

convert(1, COA, ELC, "energy")   # 1 PJ of coal as GWh of electricity
```

That fourth argument is not decoration. Coal and electricity are not
physically interconvertible; the only thing that can carry across is a
shared dimension, and which one is a modelling choice rather than a
fact. Where the choice is unambiguous it is inferred, and where it is
not `convert()` reports the candidates and the number each would give
rather than picking one.

### When a property is ambiguous

Some commodities have more than one value for what looks like one
property. Hydrogen has three densities, spanning nearly three orders of
magnitude:

``` r

H2 <- newCommodity(
  "H2", desc = "Hydrogen", unit = "PJ",
  property = data.frame(
    property = c("lhv", "hhv", "density", "density_liquid", "density_700bar",
                 "molar_mass"),
    value    = c(120.0, 141.8, 8.988e-5, 0.07085, 0.0420, 2.016),
    unit     = c("GJ/t", "GJ/t", "t/m3", "t/m3", "t/m3", "g/mol"),
    comment  = c("119.96 MJ/kg", "141.88 MJ/kg", "gas, 0 C, 1 atm",
                 "liquid, -253 C", "compressed, 700 bar", "")))
```

Only the bare name joins the default set of conversion edges, so
`density` — the one the model works in — is used unless you name a
variant. The same holds for `lhv` against `hhv`:

``` r

convert(1, "m3", "MJ", commodity = H2)                          # 10.79, NTP gas
convert(1, "m3", "MJ", commodity = H2, via = "density_liquid")  # 8502
convert(1, "t",  "GJ", commodity = H2, via = "hhv")             # 141.8
```

The first two differ by a factor of 788, which is exactly why the state
has to be named rather than assumed.

**A property describes the commodity itself.** Anything whose numerator
is a *different* commodity — a CO2 emission factor, say — belongs in
`@emis`, which is what the model actually reads. The two stay
consistent: coal’s carbon fraction times 44.009/12.011, divided by its
heating value, reproduces the per-energy emission factor, which makes
`@emis` checkable rather than duplicated.

## Money is a unit with a year

Currency is a dimension like any other, so `convert("MEUR", "MUSD")`
works. But money has a property no physical unit has: **it changes value
over time**. A cost of 1000 EUR is not one number, it is one number and
a year.

Write the year into the unit:

> **Not in this build.** Year-tagged currency is not available in
> energyRt 0.89.1.9000. The calls below are shown for reference; their
> output will appear here once the feature lands.

``` r

convert(1, "MEUR2020", "MUSD2020")   # an exchange-rate question
convert(1, "MUSD2015", "MUSD2020")   # an inflation question
```

Those are two different problems and they need two different kinds of
data: **exchange rates** move between currencies at one point in time,
**deflators** move between points in time within one currency.

|         | 2015       | →                      | 2020       |
|---------|------------|------------------------|------------|
| **EUR** | `MEUR2015` | *deflator (euro area)* | `MEUR2020` |
| ↓       | *FX 2015*  |                        | *FX 2020*  |
| **USD** | `MUSD2015` | *deflator (US)*        | `MUSD2020` |

Read that table as a square. To get from the top-left to the
bottom-right you can go along and then down, or down and then along —
and **the two routes do not give the same answer**. That is not a defect
in the arithmetic: the gap between them *is* the change in the real
exchange rate over the interval, and it is zero only if purchasing power
parity held. For EUR to USD between 2015 and 2020 the two agree to
better than 0.1%; for JPY 2012 to USD 2023 they differ by a factor of
2.2.

So both are always computed. The default follows the World Bank and IPCC
convention — deflate in the source currency, then convert at the target
year — and you are warned when the routes disagree materially:

``` r

convert(1, "MJPY2012", "MUSD2023")                              # default route
convert(1, "MJPY2012", "MUSD2023", currency_route = "both")     # see both
```

**Untagged money is left alone.** A conversion where neither side
carries a year never consults an exchange-rate table, never warns, and
returns exactly what it always did — so
`convert("EUR/kW", "MEUR/GW", 2000)` is unaffected by any of this.

## What the model enforces

A model has exactly one currency whether it says so or not: the
objective adds every cost together with coefficient 1, across regions as
well as processes. So declare it, and let interpolation do the
arithmetic:

> **Not in this build.** A model-level unit target is not available in
> energyRt 0.89.1.9000. The calls below are shown for reference; their
> output will appear here once the feature lands.

``` r

mod <- newModel("demo", units = data.frame(costs = "MUSD2020"))
```

Processes that declare a different currency in their own `@units$costs`
are converted to the model target during interpolation, and the
conversion is reported rather than applied silently:

    i Currency normalisation -> MUSD2020
        WIND_OFF, WIND_ON, PV      MEUR2015   × 1.163   (deflate_then_fx)
        COAL_PP                    MUSD2015   × 1.080   (US GDP deflator)
        17 objects untagged, assumed MUSD2020

Every factor applied is recorded in `scen@misc$currency`, with the
exchange-rate and deflator rows it came from, so a result can be traced
back to its inputs. Objects themselves are left as you wrote them:
[`getData()`](https://energyRt.org/reference/getData.md) on a process
returns the values you authored, while
[`getData()`](https://energyRt.org/reference/getData.md) on the scenario
returns the normalised ones.

Two things this does **not** do yet, stated plainly so you do not assume
otherwise. Physical units are not normalised — a technology declaring
its coal input in Mt while the commodity is PJ is not corrected, only
conversion helpers know how to relate them. And normalising to a
constant currency-year commits `wacc` and `sdr` to being **real** rates;
nothing checks that the rate you supplied is real rather than nominal.

## See also

- **Model bricks** — where `@units`, `cap2act` and the chain
  coefficients sit in a process.
- **UTOPIA I** — a full model built in one consistent unit convention
  (GW, PJ, MEUR, kt).
- `?convert`, [`?getUnits`](https://energyRt.org/reference/getUnits.md),
  [`?commodity_properties`](https://energyRt.org/reference/commodity_properties.md),
  [`?commodity_property`](https://energyRt.org/reference/commodity_property.md),
  [`?newCommodity`](https://energyRt.org/reference/newCommodity.md).
