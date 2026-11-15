# TOPIA instances, cut from the shipped model

The dataset ships ONE model: twelve provinces, hourly, 51 objects. This
cuts smaller cases from it – one province, one zone, a sample across
zones – in a single call, resolving as it goes the things that are
tedious and error-prone to do by hand.

## Usage

``` r
build_topia(
  regions = NULL,
  preset = NULL,
  layout = "honeycomb",
  level = "province",
  calendar = "d365_h24",
  horizon = "BY2025_50_by10",
  techs = NULL,
  modules = NULL,
  clusters = TRUE,
  demand = NULL,
  weather = NULL,
  levers = TRUE,
  as = c("kit", "model", "repository"),
  boundary_prices = NULL,
  quiet = FALSE
)
```

## Arguments

- regions:

  province codes, zone codes, the nation code, or a count for the first
  *n* in canonical order. `NULL` keeps all twelve.

- preset:

  one of `"full"`, `"island"` (`R01`, no wire and no domestic fuel) or
  `"zone"` (`Z3`, carrying the nuclear station and the reservoir). A
  preset expands to the arguments below and reports what it chose.

- layout:

  `"squares"`, `"honeycomb"` (the default, and the one the shipped model
  carries), `"island"` or `"continent"`.

- level:

  the geoframe the model's own regions are. Only `"province"` is built
  so far.

- calendar:

  a calendar name or object. It must be a **sample** of the model's own
  – its timeslices a subset of `d365_h24`'s – which
  `d365_h24_1dpm` (301) and `d365_h24_1dps` (101) are. Anything else is
  refused, naming the slices that do not match.

- horizon:

  `"BY2025_50_by10"`, `"BY2025"`, or a `horizon` object.

- techs:

  which technologies to keep, or to drop with a leading `-`
  (`c("-EBIO", "-H2ELC")`). Two keywords are derived from the objects
  rather than listed: `"vre"` is a technology declaring a weather
  series, `"thermal"` one that burns its input. Keeps and drops cannot
  be mixed.

- modules:

  which of the article's nine sub-repositories to keep, or to drop with
  a leading `-`: `commodities`, `supply`, `weather`, `generators`,
  `storage`, `hydrogen`, `transmission`, `fuel_trade`, `demand`.
  `modules = "-hydrogen"` strips in one word the chain the article
  declares across three sections.

- clusters:

  not built yet.

- weather, demand:

  the name of a shipped dataset (`"sample"`, `"simulated"`) or a
  `data.frame` shaped like the matching element of
  [`topia_profiles()`](https://energyRt.org/reference/topia_profiles.md)
  – so the output of one is the input of the other. The frames are
  written into the shipped objects, which keeps each series' name, units
  and timeframe. A weather frame must cover every `(cluster, region)`
  pair the object's roster declares; a demand frame carrying `load` is a
  relative shape and is scaled to the annual energy the shipped object
  holds, one carrying `demand` is taken as given.

- levers:

  `TRUE` for all four, `FALSE` for none, or names.

- as:

  `"kit"` (the named list the dataset itself uses, so the two are
  interchangeable), `"model"` (a ready
  [`newModel()`](https://energyRt.org/reference/newModel.md) with the
  geoscale set), or `"repository"`.

- boundary_prices:

  passed to
  [`subset_model_regions()`](https://energyRt.org/reference/subset_model_regions.md)
  to price the corridors a subset cuts.

- quiet:

  suppress the one-line report of what was built.

## Value

A kit (named list), a `model`, or a `repository`.

## Details

`build_topia()` authors nothing. Every object it returns was declared in
*TOPIA I: building the model*, which generates the shipped data files;
this selects from them. A technology TOPIA does not declare cannot be
asked for, and new processes are added to the article rather than here.

With every argument at its default the result is
`topia$modules$electricity` **unchanged**, which is what
`tests/testthat/test-topia-build.R` pins.

`regions` takes codes at any geoframe. A zone code expands to the
provinces beneath it, so a request never cuts a zone in half by accident
– which matters because `COA` and `GAS` balance at the zone, and a
subset that splits one is refused by
[`subset_model_regions()`](https://energyRt.org/reference/subset_model_regions.md)
rather than averaged.

Selecting processes pulls a **dependency closure** behind it: drop every
coal burner and `COA`, `SUP_COA`, `IMP_COA` and both coal corridors go
too, because nothing is left to use them. A storage and a corridor are
not counted as consumers or producers while deciding this – one
time-shifts and the other moves, so counting them lets a commodity keep
itself alive after its last real user is gone. `CO2` is reachable only
as an emission declared on `COA` and `GAS`, and is kept on that basis. A
lever naming a process that is gone goes with it, since it would
otherwise reference what the model does not declare.

A cheaper `calendar` re-keys the series and the demand curve onto it,
and the **annual energy is held fixed**: a sample is a cheaper view of
the same year, not a smaller year, and two cases are only comparable if
the demand they must meet is the same. Only a sample of the model
calendar is accepted. The measured wind and solar grades exist on
`d365_h24` alone, so a calendar with timeslice names of its own –
`s4_h24`, whose series are a simulated stand-in with no grades at all –
would leave every clustered technology with no weather row and running
unbounded.

The two levers that carry a spatial shape are narrowed with the model:
`GREEN_H2`, whose `for.each` names every province, and `CO2_CAP`, whose
cap is a per-province allowance times the province count and so is
rescaled by the count kept. `CT_CO2` and `EARLY_RET` have no province
dimension.

`layout` chooses which of the four pictures the model is drawn on. The
corridor network is the SAME on all four – fourteen corridors frozen in
`data-raw/topia_links.R`, not derived from any one layout – so a layout
changes the map and nothing the model solves.

## See also

[topia](https://energyRt.org/reference/topia.md) for the dataset,
[`subset_model_regions()`](https://energyRt.org/reference/subset_model_regions.md)
and
[`aggregate_model_regions()`](https://energyRt.org/reference/aggregate_model_regions.md)
for the reduction verbs this sits beside.

## Examples

``` r
if (FALSE) { # \dontrun{
# the shipped model, unchanged
identical(build_topia(), topia$modules$electricity)

# one zone, ready to interpolate
mod <- build_topia("Z3", as = "model")

# the island, by name
kit <- build_topia(preset = "island")
} # }
```
