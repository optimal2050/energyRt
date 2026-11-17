# Units, conversions, and currencies

**Key functions**

- Declare and inspect units with constructor arguments such as `unit =`
  and `units =`, then use
  [`getUnit()`](https://energyRt.org/reference/getUnit.md) and
  [`getUnits()`](https://energyRt.org/reference/getUnits.md).
- Convert compatible measures with
  [`convert()`](https://energyRt.org/reference/convert.md); use
  [`commodity_property()`](https://energyRt.org/reference/commodity_property.md)
  and
  [`commodity_properties()`](https://energyRt.org/reference/commodity_properties.md)
  when a physical property must bridge dimensions.
- Normalize currencies and currency-years with
  [`normalize_model_currency()`](https://energyRt.org/reference/normalize_model_currency.md)
  when that feature is available in the installed build.

A model is arithmetic on numbers whose meaning lives outside the
numbers. A coal plant costing `2000` is meaningless until you know
whether that is EUR per kW or MEUR per GW, and whether the coal it burns
is counted in PJ or in tonnes. This article covers where each unit is
declared, how [`convert()`](https://energyRt.org/reference/convert.md)
moves between them, and what the model checks for you.

## Where units live

One slot carries units, and its columns name what is being measured.

| Where | Column | Question it answers |
|----|----|----|
| Commodity, or a flow | `@units$comm` | in what measure is this commodity counted? |
| Process | `@units$capacity`, `$activity`, `$use` | what are this process’s capacity and activity units? |
| Anything priced | `@units$currency` | what money are its costs in? |
| Weather | `@units$wval` | what does the series measure, if anything? |
| Model | `config@units` | what should everything be expressed in? |

A commodity’s `@units$comm` is its **native measure** — the one the
model balances it in. Electricity in GWh, coal in PJ, water in Mm3. The
`unit =` argument of a constructor writes it, and
[`getUnit()`](https://energyRt.org/reference/getUnit.md) reads it back:

``` r

ELC <- newCommodity("ELC", desc = "Electricity", unit = "GWh")
COA <- newCommodity("COA", desc = "Steam coal",  unit = "PJ")
getUnit(COA)
#> [1] "PJ"
```

A process declares its own units in `@units`, which is where a cost
currency is stated:

``` r

EGAS <- newTechnology(
  name = "EGAS", desc = "Gas turbine",
  input  = data.frame(comm = "GAS", unit = "PJ"),
  output = data.frame(comm = "ELC", unit = "GWh"),
  units  = data.frame(capacity = "GW", activity = "GWh", currency = "MEUR"),
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

Up to version 0.91 a commodity’s and a flow’s own measure was a slot of
its own, `@unit`, beside the `@units` table a process carried. The two
are one slot now, so
[`getUnit()`](https://energyRt.org/reference/getUnit.md) and
[`getUnits()`](https://energyRt.org/reference/getUnits.md) read the same
place. `unit =` is still the constructor argument, which is why the code
above is unchanged, and a saved object’s `@unit` is migrated to
`@units$comm` when it is read.

## Dimensions and conversion

Every unit belongs to a **dimension** — energy, mass, volume, time,
currency and so on. Two units are interconvertible when they share one,
and [`convert()`](https://energyRt.org/reference/convert.md) is the
function that does it:

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
[`convert()`](https://energyRt.org/reference/convert.md) can find that
chain itself:

> **Not in this build.** Commodity-aware conversion is not available in
> energyRt 0.91.5.9001. The calls below are shown for reference; their
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
not [`convert()`](https://energyRt.org/reference/convert.md) reports the
candidates and the number each would give rather than picking one.

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

``` r

convert(1, "MEUR2020", "MUSD2020")   # an exchange-rate question
#> [1] 1.142196
convert(1, "MUSD2015", "MUSD2020")   # an inflation question
#> [1] 1.082839
```

Those are two different problems and they need two different kinds of
data: **exchange rates** move between currencies at one point in time,
**deflators** move between points in time within one currency.

|         | 2015       | →                 | 2020       |
|---------|------------|-------------------|------------|
| **EUR** | `MEUR2015` | *deflator (euro)* | `MEUR2020` |
| ↓       | *FX 2015*  |                   | *FX 2020*  |
| **USD** | `MUSD2015` | *deflator (US)*   | `MUSD2020` |

An exchange rate belongs to a currency, but a deflator belongs to an
economy, and a currency need not be one: there is no euro-area deflator
in these series, so the euro deflates on its largest member. Which
economy stood in is recorded rather than implied — that is what
`currency_deflator$area` is for.

Read that table as a square. To get from the top-left to the
bottom-right you can go along and then down, or down and then along —
and **the two routes do not give the same answer**. That is not a defect
in the arithmetic: the gap between them *is* the change in the real
exchange rate over the interval, and it is zero only if purchasing power
parity held.

How far apart they are is a question for the data. Between 2015 and 2020
the euro and the dollar part company by 3.3%; between 2012 and 2023 the
yen and the dollar differ by a factor of 2.07 — the yen lost half its
dollar value over an interval in which Japanese prices barely moved,
which is a large change in the real exchange rate and so a large gap
between the routes.

So both are always computed. The default follows the World Bank and IPCC
convention — deflate in the source currency, then convert at the target
year — and you are warned when the routes disagree materially:

``` r

convert(1, "MJPY2012", "MUSD2023")                              # default route
#> [1] 0.007956359
convert(1, "MJPY2012", "MUSD2023", currency_route = "both")     # see both
#> deflate_then_fx fx_then_deflate 
#>     0.007956359     0.016460766
```

Both tables are annual, cover every currency the series carry, and are
built by `data-raw/currency.R` from a cached pull that ships with the
sources, so a number in a result can be traced to the row it came from
and the whole table can be rebuilt. They stop at the year they were
built from; a rate newer than the release, or one of your own, goes in
for the session:

``` r

add_currency_fx("EUR", 2026, 0.92, source = "my own estimate")
convert(1, "MEUR2026", "MUSD2026")
#> [1] 1.086957
```

**Untagged money consults no deflator.** A conversion where neither side
carries a year is a pure exchange rate, read at the one year
`attr(currency_fx, "base_year")` names, and it never warns. What did
change in this release is where that rate comes from: the coefficients
used to be typed in by hand and undated — they carried a euro rate from
one year beside a yen rate from another — and they are now generated
from the annual series at a single stated year. So untagged
cross-currency answers moved once, here, and a conversion within one
currency (`convert("EUR/kW", "MEUR/GW")`, a prefix ratio) did not move
at all.

``` r

convert("EUR/kW", "MEUR/GW")        # a prefix ratio: no rate is consulted
#> [1] 1
attr(currency_fx, "base_year")      # the year untagged money is read at
#> [1] 2025
currency_base_year(2020)            # or pin it somewhere else
convert("MEUR", "MUSD")
#> [1] 1.142196
currency_base_year(attr(currency_fx, "base_year"))
```

## What the model enforces

A model has exactly one currency whether it says so or not: the
objective adds every cost together with coefficient 1, across regions as
well as processes. So declare it, in the same slot and under the same
column name an object uses:

``` r

mod <- newModel("demo", units = data.frame(currency = "MUSD"))
mod@config@units$currency
#> [1] "MUSD"
```

`currency =` is the shorter way to write the same thing, and what every
example before this release used: `newModel("demo", currency = "MUSD")`.
It was the `config@currency` slot up to version 0.91.

What this enforces is that the pair can be **reconciled**. An object
that declares a currency must declare one the model’s can be reached
from; one that declares nothing is in the model’s own currency, which is
what a model-level declaration means. The check runs at
[`newModel()`](https://energyRt.org/reference/newModel.md), so a
declaration that leads nowhere is refused where it was written rather
than surfacing in a model much later:

``` r

COAL <- newSupply("COAL", commodity = "COA", unit = "PJ",
                  units = data.frame(currency = "bnEUR"))
newModel("demo", currency = "MUSD", data = add(newRepository("r"), COAL))
#> Error:
#> ! the model counts in 'MUSD', but 1 object(s) are priced in a currency it cannot be reached from:
#>   COAL: bnEUR
#>   bnEUR -> MUSD: Unknown unit bnEUR
#> Restate the costs, or clear `@units$currency` to claim nothing.
```

A mismatch that *can* be converted is not an error, it is work. Where an
object was priced in another currency-year, its costs are converted to
the model’s and every factor applied is reported — never applied
silently:

``` r

PV <- newTechnology("PV", units = data.frame(capacity = "GW",
                                             currency = "MEUR2015"),
                    invcost = data.frame(invcost = 700))
GAS_SUP <- newSupply("GAS_SUP", commodity = "GAS", unit = "PJ",
                     units = data.frame(currency = "MUSD2015"),
                     supply = data.frame(cost = 8))
mod <- newModel("demo", currency = "MUSD2020",
                data = add(newRepository("r"), newCommodity("GAS"), PV,
                           GAS_SUP))
mod <- normalize_model_currency(mod)
#> Currency normalisation -> MUSD2020
#>     PV        MEUR2015   × 1.241     (deflate_then_fx)
#>     GAS_SUP   MUSD2015   × 1.0828    (USD gdp deflator)
```

The factors are the article’s two questions, one each: `PV` crosses a
currency and a year, `GAS_SUP` crosses only a year and so moves on the
US deflator alone, where the route cannot matter. Every one is recorded,
with the exchange-rate and deflator rows behind it, so a result can be
traced back to its inputs:

``` r

mod@misc$currency$objects
#>          object      class     from declared   factor             note
#> PV           PV technology MEUR2015     TRUE 1.241012  deflate_then_fx
#> GAS_SUP GAS_SUP     supply MUSD2015     TRUE 1.082839 USD gdp deflator
#>         converted
#> PV           TRUE
#> GAS_SUP      TRUE
mod@misc$currency$deflator
#>   code area year basis     index         source
#> 1  EUR  DEU 2015   gdp  92.03751 NY.GDP.DEFL.ZS
#> 2  EUR  DEU 2020   gdp 100.00000 NY.GDP.DEFL.ZS
#> 3  USD  USA 2015   gdp  92.34986 NY.GDP.DEFL.ZS
#> 4  USD  USA 2020   gdp 100.00000 NY.GDP.DEFL.ZS
```

[`interpolate_model()`](https://energyRt.org/reference/interpolate_model.md)
does this on the copy it builds from, where the report lands in
`scen@misc$currency`. Objects are therefore left as you wrote them:
[`getData()`](https://energyRt.org/reference/getData.md) on a process
returns the values you authored, while
[`getData()`](https://energyRt.org/reference/getData.md) on the scenario
returns the normalised ones.

Two things this does **not** do, stated plainly so you do not assume
otherwise. Physical units are not normalised — a technology declaring
its coal input in Mt while the commodity is PJ is not corrected, only
conversion helpers know how to relate them. And normalising to a
constant currency-year commits `wacc` and `sdr` to being **real** rates;
nothing checks that the rate you supplied is real rather than nominal.

## See also

- **Model bricks** — where `@units`, `cap2act` and the chain
  coefficients sit in a process.
- **TOPIA I** — a full model built in one consistent unit convention
  (GW, PJ, MEUR, kt).
- [`?convert`](https://energyRt.org/reference/convert.md),
  [`?getUnits`](https://energyRt.org/reference/getUnits.md),
  [`?commodity_properties`](https://energyRt.org/reference/commodity_properties.md),
  [`?commodity_property`](https://energyRt.org/reference/commodity_property.md),
  [`?newCommodity`](https://energyRt.org/reference/newCommodity.md).
