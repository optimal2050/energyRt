# Trim numeric/bounds parameters to the domain of the maps that index them

Drops rows of a value parameter that lie outside the union of the
equation- domain maps referencing it (lifespan window x membership). The
maps are the authority on the minimal domain, so any parameter row no
map indexes is dead data: it bloats the written model and (for unfolded
scenarios) may carry a stale wildcard NA that is out-of-domain in the
solver.

## Usage

``` r
trim_parameters(object, verbose = isVerbose(2))
```

## Arguments

- object:

  scenario.

- verbose:

  logical; report per-parameter trim counts.

## Value

scenario with trimmed parameter slots.

## Details

Intended for the unfolded (`fold = FALSE`) pipeline, where wildcard (NA)
dimensions have already been materialised. A parameter is left untouched
when none of its registered maps is available, to avoid clearing data
whose map was not built.

## Examples

``` r
# \donttest{
topia_elc <- topia$modules$electricity
mod  <- newModel("TOPIA", data = topia_elc$repo,
                 calendar = topia$modules$calendars$s4_h24,
                 region = topia_elc$regions,
                 horizon = topia$modules$horizons$BY2025_50_by10,
                 discount = 0.05)
mod <- setGeoscale(mod, topia_elc$geoscale)
scen <- interpolate_model(mod, name = "EX")
#> 
#> ── interpolate_model: EX ───────────────────────────────────────────────────────
#> storage: in-memory
#> sparse: TRUE
#> prune: TRUE
#> fold: none
#> validate: TRUE
#> base year 2025: not optimised, and no base-year solution was given -- the projection starts from the model's own @stock (base_year_scenario = carries one forward)
#> ℹ checking declarations
#> Warning: 8760 timeslice(s) named by 4 object(s) are outside the sampled calendar "s4_h24"; their rows are dropped.
#> Cost declared at a coarse geoscale level:
#>   TBD_COA_Z4_Z3@invcost: Z3, Z4
#>   TBD_COA_Z4_Z5@invcost: Z4, Z5
#>   TBD_GAS_Z2_Z3@invcost: Z2, Z3
#> The cost is charged ONCE at that cell rather than at each child region, and is discounted at the children's rate. That is usually what a national cost datum means -- said here because it is a different number from naming the children individually.
#> ✔ checking declarations (0.2s)
#> ℹ expanding vintage/cluster variants
#> Expanded 8 process objects into 33 variants (vintage/cluster).
#> ✔ expanding vintage/cluster variants (0.1s)
#> ℹ materializing commodity clusters
#> Materialized 3 commodity cluster(s): LANDW_CLHI, LANDW_CLLO, LANDW_CLMD
#> ✔ materializing commodity clusters (0.0s)
#> ℹ materializing weather clusters
#> Materialized 5 weather cluster(s): WSOL_CLFL, WSOL_CLTH, WWIN_CLHI, WWIN_CLMD, WWIN_CLLO
#> ✔ materializing weather clusters (0.1s)
#> ℹ materializing weather transforms
#> ✔ materializing weather transforms (0.4s)
#> ℹ collecting sets: object names
#> ✔ collecting sets: object names (0.1s)
#> ℹ collecting sets: process classes
#> ✔ collecting sets: process classes (0.0s)
#> ℹ collecting sets: timeframes
#> Processes with no input commodity (output is bounded, not fuelled): ESOL_VIN2015_CLFL, ESOL_VIN2015_CLTH, ESOL_VIN2030_CLFL, ESOL_VIN2030_CLTH, ESOL_VIN2045_CLFL, ESOL_VIN2045_CLTH, EWIN_VIN2015_CLHI, EWIN_VIN2015_CLLO, EWIN_VIN2015_CLMD, EWIN_VIN2030_CLHI, EWIN_VIN2030_CLLO, EWIN_VIN2030_CLMD, EWIN_VIN2045_CLHI, EWIN_VIN2045_CLLO, EWIN_VIN2045_CLMD
#> ✔ collecting sets: timeframes (0.1s)
#> ℹ collecting sets: process inputs/outputs
#> Processes with no input commodity (output is bounded, not fuelled): ESOL_VIN2015_CLFL, ESOL_VIN2015_CLTH, ESOL_VIN2030_CLFL, ESOL_VIN2030_CLTH, ESOL_VIN2045_CLFL, ESOL_VIN2045_CLTH, EWIN_VIN2015_CLHI, EWIN_VIN2015_CLLO, EWIN_VIN2015_CLMD, EWIN_VIN2030_CLHI, EWIN_VIN2030_CLLO, EWIN_VIN2030_CLMD, EWIN_VIN2045_CLHI, EWIN_VIN2045_CLLO, EWIN_VIN2045_CLMD
#> Processes with no input commodity (output is bounded, not fuelled): ESOL_VIN2015_CLFL, ESOL_VIN2015_CLTH, ESOL_VIN2030_CLFL, ESOL_VIN2030_CLTH, ESOL_VIN2045_CLFL, ESOL_VIN2045_CLTH, EWIN_VIN2015_CLHI, EWIN_VIN2015_CLLO, EWIN_VIN2015_CLMD, EWIN_VIN2030_CLHI, EWIN_VIN2030_CLLO, EWIN_VIN2030_CLMD, EWIN_VIN2045_CLHI, EWIN_VIN2045_CLLO, EWIN_VIN2045_CLMD
#> ✔ collecting sets: process inputs/outputs (0.0s)
#> ℹ collecting sets: membership comm sets
#> Processes with no input commodity (output is bounded, not fuelled): ESOL_VIN2015_CLFL, ESOL_VIN2015_CLTH, ESOL_VIN2030_CLFL, ESOL_VIN2030_CLTH, ESOL_VIN2045_CLFL, ESOL_VIN2045_CLTH, EWIN_VIN2015_CLHI, EWIN_VIN2015_CLLO, EWIN_VIN2015_CLMD, EWIN_VIN2030_CLHI, EWIN_VIN2030_CLLO, EWIN_VIN2030_CLMD, EWIN_VIN2045_CLHI, EWIN_VIN2045_CLLO, EWIN_VIN2045_CLMD
#> ✔ collecting sets: membership comm sets (0.0s)
#> ℹ collecting sets: process regions
#> ✔ collecting sets: process regions (0.0s)
#> ℹ building maps: membership + closure
#> Processes with no input commodity (output is bounded, not fuelled): ESOL_VIN2015_CLFL, ESOL_VIN2015_CLTH, ESOL_VIN2030_CLFL, ESOL_VIN2030_CLTH, ESOL_VIN2045_CLFL, ESOL_VIN2045_CLTH, EWIN_VIN2015_CLHI, EWIN_VIN2015_CLLO, EWIN_VIN2015_CLMD, EWIN_VIN2030_CLHI, EWIN_VIN2030_CLLO, EWIN_VIN2030_CLMD, EWIN_VIN2045_CLHI, EWIN_VIN2045_CLLO, EWIN_VIN2045_CLMD
#> Processes with no input commodity (output is bounded, not fuelled): ESOL_VIN2015_CLFL, ESOL_VIN2015_CLTH, ESOL_VIN2030_CLFL, ESOL_VIN2030_CLTH, ESOL_VIN2045_CLFL, ESOL_VIN2045_CLTH, EWIN_VIN2015_CLHI, EWIN_VIN2015_CLLO, EWIN_VIN2015_CLMD, EWIN_VIN2030_CLHI, EWIN_VIN2030_CLLO, EWIN_VIN2030_CLMD, EWIN_VIN2045_CLHI, EWIN_VIN2045_CLLO, EWIN_VIN2045_CLMD
#> ✔ building maps: membership + closure (0.3s)
#> ℹ collecting sets: invest/stock windows
#> ✔ collecting sets: invest/stock windows (0.2s)
#> ℹ settings parameters (discount, period length, timeslice share)
#> ✔ settings parameters (discount, period length, timeslice share) (0.2s)
#> ℹ building maps: calendar + lifespan
#> Processes with no input commodity (output is bounded, not fuelled): ESOL_VIN2015_CLFL, ESOL_VIN2015_CLTH, ESOL_VIN2030_CLFL, ESOL_VIN2030_CLTH, ESOL_VIN2045_CLFL, ESOL_VIN2045_CLTH, EWIN_VIN2015_CLHI, EWIN_VIN2015_CLLO, EWIN_VIN2015_CLMD, EWIN_VIN2030_CLHI, EWIN_VIN2030_CLLO, EWIN_VIN2030_CLMD, EWIN_VIN2045_CLHI, EWIN_VIN2045_CLLO, EWIN_VIN2045_CLMD
#> Processes with no input commodity (output is bounded, not fuelled): ESOL_VIN2015_CLFL, ESOL_VIN2015_CLTH, ESOL_VIN2030_CLFL, ESOL_VIN2030_CLTH, ESOL_VIN2045_CLFL, ESOL_VIN2045_CLTH, EWIN_VIN2015_CLHI, EWIN_VIN2015_CLLO, EWIN_VIN2015_CLMD, EWIN_VIN2030_CLHI, EWIN_VIN2030_CLLO, EWIN_VIN2030_CLMD, EWIN_VIN2045_CLHI, EWIN_VIN2045_CLLO, EWIN_VIN2045_CLMD
#> ✔ building maps: calendar + lifespan (0.3s)
#> ℹ ob2mi: interpolating 81 model objects
#> ✔ ob2mi: interpolating 81 model objects (1.2s)
#> ℹ interpolating parameters over milestone years
#> ✔ interpolating parameters over milestone years (3.5s)
#> ℹ calendar-filter: restricting parameters to declared timeslices
#> ✔ calendar-filter: restricting parameters to declared timeslices (0.1s)
#> ℹ computing equivalent annual costs (EAC)
#> ✔ computing equivalent annual costs (EAC) (0.1s)
#> ℹ building maps: value
#> ✔ building maps: value (0.2s)
#> ℹ building maps: filter
#> Processes with no input commodity (output is bounded, not fuelled): ESOL_VIN2015_CLFL, ESOL_VIN2015_CLTH, ESOL_VIN2030_CLFL, ESOL_VIN2030_CLTH, ESOL_VIN2045_CLFL, ESOL_VIN2045_CLTH, EWIN_VIN2015_CLHI, EWIN_VIN2015_CLLO, EWIN_VIN2015_CLMD, EWIN_VIN2030_CLHI, EWIN_VIN2030_CLLO, EWIN_VIN2030_CLMD, EWIN_VIN2045_CLHI, EWIN_VIN2045_CLLO, EWIN_VIN2045_CLMD
#> Processes with no input commodity (output is bounded, not fuelled): ESOL_VIN2015_CLFL, ESOL_VIN2015_CLTH, ESOL_VIN2030_CLFL, ESOL_VIN2030_CLTH, ESOL_VIN2045_CLFL, ESOL_VIN2045_CLTH, EWIN_VIN2015_CLHI, EWIN_VIN2015_CLLO, EWIN_VIN2015_CLMD, EWIN_VIN2030_CLHI, EWIN_VIN2030_CLLO, EWIN_VIN2030_CLMD, EWIN_VIN2045_CLHI, EWIN_VIN2045_CLLO, EWIN_VIN2045_CLMD
#> Processes with no input commodity (output is bounded, not fuelled): ESOL_VIN2015_CLFL, ESOL_VIN2015_CLTH, ESOL_VIN2030_CLFL, ESOL_VIN2030_CLTH, ESOL_VIN2045_CLFL, ESOL_VIN2045_CLTH, EWIN_VIN2015_CLHI, EWIN_VIN2015_CLLO, EWIN_VIN2015_CLMD, EWIN_VIN2030_CLHI, EWIN_VIN2030_CLLO, EWIN_VIN2030_CLMD, EWIN_VIN2045_CLHI, EWIN_VIN2045_CLLO, EWIN_VIN2045_CLMD
#> ✔ building maps: filter (1.6s)
#> ℹ building maps: constraint
#> Error: waf: the weather series has no value at the timeslices of the bound it multiplies (ESOL_VIN2015_CLFL/WSOL_CLFL, ESOL_VIN2030_CLFL/WSOL_CLFL, ESOL_VIN2045_CLFL/WSOL_CLFL, ESOL_VIN2015_CLTH/WSOL_CLTH, ESOL_VIN2030_CLTH/WSOL_CLTH). Give the weather the technology's timeframe.

# drop the approximation context the scenario no longer needs; the
# parameters themselves are untouched
model_size(scen)
#> Error: object 'scen' not found
model_size(trim_parameters(scen))
#> Error: object 'scen' not found
# }
```
