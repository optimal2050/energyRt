# Validating and debugging a model

A model can fail in two ways. It can fail loudly: the solver returns
INFEASIBLE, and nothing in the solver log names the row that cannot be
met. Or it can fail quietly: the solve returns OPTIMAL, but a missing
value was replaced by a default, a process could never run, or the
matrix carries coefficients the solver had to scale away. energyRt has a
check for each stage of the model life cycle, and most of them need no
solver at all.

| Stage | Check | What it finds |
|----|----|----|
| Declared model | [`check_feasibility()`](https://energyRt.org/reference/check_feasibility.md), `interpolate_model(check = TRUE)` | forced requirements above ceilings that cannot be raised |
| Interpolated scenario | [`validate_scenario_parameters()`](https://energyRt.org/reference/validate_scenario_parameters.md) | missing, duplicated or out-of-domain parameter rows; storages and corridors without their governing equations |
|  | [`process_operability()`](https://energyRt.org/reference/process_operability.md) | processes that cannot operate in a region and year, and why |
|  | [`audit_coefficients()`](https://energyRt.org/reference/audit_coefficients.md) | the coefficient range of each equation family |
| Infeasible solve | dummy import / export on commodity balances | which balance cannot close, where and when |
|  | [`multimod::lp_bounds()`](https://optimal2050.github.io/multimod/r/reference/lp_bounds.html) | matrix rows whose activity range misses their bounds |
| Solved scenario | [`verify_solution()`](https://energyRt.org/reference/verify_solution.md) | accounting identities recomputed from the solution |
|  | [`compare_solve_settings()`](https://energyRt.org/reference/compare_solve_settings.md) | disagreement between backends or storage settings |

This article walks through each check on a small model with one planted
defect per stage. Every block runs on the bundled GLPK in a few seconds.

``` r

library(energyRt)

debug_project <- file.path(tempdir(), "debugging")
set_scenarios_path(path = debug_project)
set_models_path(path = file.path(debug_project, "models"))
set_repositories_path(path = file.path(debug_project, "repositories"))
set_registry_file(path = file.path(debug_project, "energyRt_registry.csv"))

annual_timetable <- make_timetable(struct = list(ANNUAL = "ANNUAL"))

annual <- newCalendar(
  name = "annual",
  timetable = annual_timetable
)

horizon_2030 <- newHorizon(period = 2030)
```

Energy is measured in PJ and costs in MEUR. With `cap2act = 1`, one unit
of capacity produces 1 PJ per year.

## Before interpolation: contradictions in the declarations

Some infeasibilities are visible in the data before any decision is
made: a demand larger than everything that can ever supply its
commodity, or a stock of capacity that consumes an auxiliary commodity
the model cannot provide.
[`check_feasibility()`](https://energyRt.org/reference/check_feasibility.md)
compares such forced requirements with ceilings that no decision can
raise, per commodity, region and year. In the model below the only
source of electricity is a supply capped at 6, against a demand of 10.

``` r

ELC <- newCommodity(
  name = "ELC",
  desc = "Electricity",
  timeframe = "ANNUAL",
  unit = "PJ"
)

SUPELC <- newSupply(
  name = "SUPELC",
  desc = "Electricity supply with a fixed ceiling",
  commodity = "ELC",
  unit = "PJ",
  supply = data.frame(
    cost = 1,
    ava.up = 6
  )
)

DEMELC <- newDemand(
  name = "DEMELC",
  desc = "Electricity demand",
  commodity = "ELC",
  unit = "PJ",
  demand = data.frame(demand = 10)
)

short_repo <- newRepository(
  name = "short_repo",
  ELC,
  SUPELC,
  DEMELC
)

short <- newModel(
  name = "short",
  desc = "Supply ceiling below demand",
  region = "R1",
  horizon = horizon_2030,
  calendar = annual,
  discount = 0.05,
  repo = short_repo
)

check_feasibility(object = short)
#> check_feasibility() [declared]: 1 contradiction(s) under the model's own setting (retirement = off); 0 under the other.
#>   A forced requirement exceeds a ceiling that cannot be raised, with no import
#>   and no route in. No solver setting will make this feasible.
#>   Economic retirement is OFF here, so standing capacity is forced to stand
#>   and there is one setting to report.
#>             check commodity region year required available shortfall forced_by
#>  demand-vs-supply       ELC     R1   NA       10         6         4    DEMELC
#>  retirement
#>         off
```

The finding lists the requirement, the ceiling, the shortfall and the
object that forces it. A declaration without a region applies to every
region of the model, and one without a year to every year, which the
table shows as `NA`. The same check runs inside
[`interpolate_model()`](https://energyRt.org/reference/interpolate_model.md)
with `check = TRUE`, which then stops instead of building a scenario
that would only come back INFEASIBLE:

``` r

SHORT <- interpolate_model(
  object = short,
  name = "SHORT",
  check = TRUE,
  verbose = FALSE
)
#> check_feasibility() [interpolated]: 1 contradiction(s) under the model's own setting (retirement = off); 0 under the other.
#>   A forced requirement exceeds a ceiling that cannot be raised, with no import
#>   and no route in. No solver setting will make this feasible.
#>   Economic retirement is OFF here, so standing capacity is forced to stand
#>   and there is one setting to report.
#>             check commodity region year required available shortfall forced_by
#>  demand-vs-supply       ELC     R1 2030       10         6         4    DEMELC
#>  retirement
#>         off
#> Error:
#> ! interpolate_model(): the model contradicts itself in 1 place(s), listed above (retirement = off) -- a forced requirement exceeds a ceiling that cannot be raised. Solving it would return INFEASIBLE without saying why. Fix the declarations, or call again with `check = FALSE` to build the scenario anyway.
```

A clean result does not mean the model is feasible. Everything that
depends on a decision – whether capacity can serve the load, whether a
bound binds, whether dispatch closes in every timeslice – is left to the
solver.

## After interpolation: parameters, operability and coefficients

The running example from here on converts gas to electricity. The
combined cycle plant has 5 units of standing capacity and is capped at
5; an open-cycle plant is declared but capped at zero; demand is 10.

``` r

GAS <- newCommodity(
  name = "GAS",
  desc = "Natural gas",
  timeframe = "ANNUAL",
  unit = "PJ"
)

SUPGAS <- newSupply(
  name = "SUPGAS",
  desc = "Natural gas supply",
  commodity = "GAS",
  unit = "PJ",
  supply = data.frame(cost = 1)
)

CCGT <- newTechnology(
  name = "CCGT",
  desc = "Combined-cycle gas power plant",
  input = list(comm = "GAS"),
  output = list(comm = "ELC"),
  cap2act = 1,
  ceff = data.frame(
    comm = "GAS",
    cinp2use = 2
  ),
  capacity = data.frame(
    stock = 5,
    cap.up = 5
  ),
  invcost = data.frame(invcost = 100)
)

OCGT <- newTechnology(
  name = "OCGT",
  desc = "Open-cycle gas power plant",
  input = list(comm = "GAS"),
  output = list(comm = "ELC"),
  cap2act = 1,
  ceff = data.frame(
    comm = "GAS",
    cinp2use = 3
  ),
  capacity = data.frame(cap.up = 0),
  invcost = data.frame(invcost = 50)
)

toy_repo <- newRepository(
  name = "toy_repo",
  GAS,
  ELC,
  SUPGAS,
  CCGT,
  OCGT,
  DEMELC
)

toy <- newModel(
  name = "toy",
  desc = "Gas-to-power model with a capacity cap",
  region = "R1",
  horizon = horizon_2030,
  calendar = annual,
  discount = 0.05,
  repo = toy_repo
)

BASE <- interpolate_model(
  object = toy,
  name = "BASE",
  verbose = FALSE
)
```

### Parameter consistency

[`interpolate_model()`](https://energyRt.org/reference/interpolate_model.md)
validates the interpolated parameters by default (`validate = TRUE`) and
warns about advisory findings. The same checks can be run again on any
scenario to collect the findings as a table: index columns without
values, duplicated keys, parameter rows outside their maps, maps not
covered by their parameters (a value the solver would silently replace
by its default), and objects whose flows lack their governing equations
– a storage without its balance chain or a corridor without its capacity
link, either of which would supply a balance without limit and still
solve to OPTIMAL.

``` r

base_findings <- validate_scenario_parameters(
  object = BASE,
  action = "silent"
)

base_findings
#> [1] parameter check     detail    severity 
#> <0 rows> (or 0-length row.names)
```

The toy scenario has no findings of this kind.

### Processes that cannot operate

A process can be declared and still be unable to run: its input has no
source in the region, it has no capacity and no investment window, or a
bound fixes its capacity, availability or activity at zero.
[`process_operability()`](https://energyRt.org/reference/process_operability.md)
lists such processes and the reason.

``` r

process_operability(object = BASE)
#>   process      class region years  kind                                 reason
#> 1    OCGT technology     R1   all bound capacity bound `cap.up` / `cap.fx` = 0
#>   detail
#> 1
```

Limits that act through the solution, such as an emission cap or a tax,
are outside this check.

### Coefficient ranges

Wide coefficient ranges come from unit choices – a capacity in MW
against an hourly activity share, a ramp divided by a timeslice share –
and make a solver work harder or stop with a numerical failure.
[`audit_coefficients()`](https://energyRt.org/reference/audit_coefficients.md)
writes the scenario, reads the matrix back and summarises the non-zero
coefficients per equation family, with the variable families at each end
of the widest row.

``` r

audit_coefficients(x = BASE)
#> Solver directory:  C:/Users/admin/AppData/Local/Temp/RtmpgJ2f2G/audit_37d036e84ff3 
#> Writing files: 0.29s
#> coefficient audit: 32 rows, 30 columns, 64 non-zeros
#>   |a_ij| in [1, 100] (global ratio 1e+02); 0 row(s) wider than 1e+06
#>   per decade: 1e0:62  1e1:1  1e2:1 
#>   equation families by widest row (top 12):
#>     eqTechInv                rows 2      ratio 100        vTechInv = 1  ..  vTechNewCap = 100
#>     eqTechEac                rows 2      ratio 5          vTechEac = 1  ..  vTechNewCap = 5
#>     eqTechSng2Sng            rows 2      ratio 3          vTechOut = 1  ..  vTechInp = 3
#>     eqBal                    rows 2      ratio 1          vOutTot = 1  ..  vOutTot = 1
#>     eqBalLo                  rows 2      ratio 1          vBalance = 1  ..  vBalance = 1
#>     eqCost                   rows 1      ratio 1          vTotalCost = 1  ..  vTotalCost = 1
#>     eqDemInp                 rows 1      ratio 1          vDemInp = 1  ..  vDemInp = 1
#>     eqInpTot                 rows 2      ratio 1          vDemInp = 1  ..  vDemInp = 1
#>     eqObjective              rows 1      ratio 1          vObjective = 1  ..  vObjective = 1
#>     eqOutTot                 rows 2      ratio 1          vTechOutTot = 1  ..  vTechOutTot = 1
#>     eqSupCost                rows 1      ratio 1          vSupCost = 1  ..  vSupCost = 1
#>     eqSupOutTot              rows 1      ratio 1          vSupOut = 1  ..  vSupOut = 1
```

## An infeasible solve

None of the checks above finds the planted defect: the plant that can
serve the load is capped at half of it, and the cap only bites once the
model has to dispatch. The solve stops with a warning and the scenario
status records it.

``` r

BASE <- solve_scenario(
  object = BASE,
  solver = solver_options$glpk,
  verbose = FALSE
)
#> Warning: Scenario 'BASE': the model is INFEASIBLE (glpsol: PROBLEM HAS NO
#> PRIMAL FEASIBLE SOLUTION). No solution was written to
#> C:/Users/admin/AppData/Local/Temp/RtmpgJ2f2G/debugging/BASE-toy-annual/runs/glpk

BASE@status$optimal
#> [1] FALSE
```

### Dummy import and export on commodity balances

The model has a built-in slack on every commodity balance: a dummy
import that adds the commodity and a dummy export that removes it, each
at a price per unit. Both are off by default. They are switched on
through the `debug` argument of
[`newModel()`](https://energyRt.org/reference/newModel.md), a table with
one row per commodity, region, year and timeslice; `NA` in any of these
columns stands for all of them, and an infinite price keeps that side
off.

``` r

dummy_prices <- data.frame(
  comm = NA_character_,
  region = NA_character_,
  year = NA_integer_,
  timeslice = NA_character_,
  dummyImport = 1e3,
  dummyExport = 1e3
)

toy_slack <- newModel(
  name = "toy_slack",
  desc = "Gas-to-power model with dummy import and export",
  region = "R1",
  horizon = horizon_2030,
  calendar = annual,
  discount = 0.05,
  debug = dummy_prices,
  repo = toy_repo
)

SLACK <- interpolate_model(
  object = toy_slack,
  name = "SLACK",
  verbose = FALSE
)

SLACK <- solve_scenario(
  object = SLACK,
  solver = solver_options$glpk,
  verbose = FALSE
)

SLACK@status$optimal
#> [1] TRUE
```

With the slack the model solves, and the slack variables show which
balance could not close and by how much:

``` r

dummy_import <- getData(
  object = SLACK,
  name = "vDummyImport",
  merge = TRUE
)

knitr::kable(
  x = dummy_import,
  caption = "Dummy import, PJ"
)
```

| scenario | name         | comm | region | year | timeslice | value |
|:---------|:-------------|:-----|:-------|-----:|:----------|------:|
| SLACK    | vDummyImport | ELC  | R1     | 2030 | ANNUAL    |     5 |

Dummy import, PJ {.table}

Electricity in 2030 is short by 5: the gap between the demand and the
capped plant. No dummy export is used.
[`summary()`](https://rdrr.io/r/base/summary.html) reports the total
cost of the slack, so a non-zero value is visible without querying the
variables:

``` r

summary(object = SLACK)
#> Scenario: SLACK 
#> desc:  
#> Model: toy_slack 
#> Interpolated: TRUE 
#> path: C:/Users/admin/AppData/Local/Temp/RtmpgJ2f2G/debugging/SLACK-toy_slack-annual 
#> Solution status: optimal
#> vObjective:  5002.5 
#> Dummy import/export costs:  5000 
#> Size: 5.4 Mb
```

The price should be well above every real cost in the model, so that the
slack is used only where nothing else can close the balance. The slack
does not cover user constraints
([`newConstraint()`](https://energyRt.org/reference/newConstraint.md))
or a bound that contradicts another bound; a model that stays infeasible
with the slack on every commodity is infeasible for one of those
reasons.

### The matrix: implied bounds

The direct-matrix route of the
[multimod](https://github.com/optimal2050/multimod) package builds the
same linear programme without a solver.
[`multimod::lp_bounds()`](https://optimal2050.github.io/multimod/r/reference/lp_bounds.html)
propagates the row and column bounds to a fixed point and lists the rows
whose possible activity cannot reach their bounds.

``` r

toy_mm <- multimod_from_energyRt(
  object = BASE,
  verbose = FALSE
)

toy_lp <- multimod::model_to_lp(model = toy_mm)

toy_bounds <- multimod::lp_bounds(lp = toy_lp)
```

``` r

infeasible_rows <- toy_bounds$bounds$infeasible

toy_lp$row_index[infeasible_rows, ]
#>        i       symbol     i1     i2     i3     i4     i5     i6  sense   rhs
#>    <int>       <char> <char> <char> <char> <char> <char> <char> <char> <num>
#> 1:    23     eqOutTot    ELC     R1   2030 ANNUAL   <NA>   <NA>     ==     0
#> 2:    29 eqTechOutTot    ELC     R1   2030 ANNUAL   <NA>   <NA>     ==     0
```

The flagged rows are the electricity output totals of region `R1` in
2030: the output that the capped plant can deliver does not reach what
the balance requires. Propagation is row by row, so it finds conflicts
that a single equation decides; a contradiction that only arises from
several equations together passes it and is left to the dummy slack or
to the solver.

## After a solve: accounting identities

A solve that returns OPTIMAL can still carry a defect in the solver, the
model template, the writer or the reader.
[`verify_solution()`](https://energyRt.org/reference/verify_solution.md)
recomputes the core identities – the commodity balance and its sign, the
source and sink totals, the cost decomposition and the discounted
objective – from the solution tables and reports any row that does not
close.

``` r

slack_verified <- verify_solution(object = SLACK)

slack_verified$ok
#> [1] TRUE

print(
  x = slack_verified,
  detail = "issues"
)
#> verify_solution: scenario 'SLACK' -- OK
#>   9 check(s) ran, 1 skipped
#>   user_costs       skipped   (vTotalUserCosts not in solution) 
#>   units            ok        rows=143 reported=81 
#>       technology 80/130  supply 1/10
```

[`verify_checks()`](https://energyRt.org/reference/verify_checks.md)
lists the available checks; `checks = "all"` adds the costlier ones.

## Cross-checks between backends

The objective of a model does not depend on the backend or on how the
scenario is stored. Solving the same scenario with two backends, or with
different storage settings, separates a modelling problem from a backend
problem.
[`compare_solve_settings()`](https://energyRt.org/reference/compare_solve_settings.md)
tabulates objective, time and status for every combination and flags
disagreement per solver.

``` r

slack_compared <- compare_solve_settings(
  object = toy_slack,
  settings = list(default = list()),
  solvers = list(glpk = solver_options$glpk)
)
#> Solver directory:  C:/Users/admin/AppData/Local/Temp/RtmpgJ2f2G/debugging/cmp_default-toy_slack-annual/runs/glpk 
#> Writing files: 0.3s
#> Starting  GLPK 
#> 0.04s
#> Reading solution: 0.21s

slack_compared$summary
#>   setting solver   ok seconds objective error
#> 1 default   glpk TRUE    0.64    5002.5  <NA>
```

Further solvers are added to the `solvers` list, for example
`solver_options$mm_highs` or `solver_options$julia_highs` where
installed.

## A sequence for a failing model

1.  [`check_feasibility()`](https://energyRt.org/reference/check_feasibility.md)
    on the model, or `interpolate_model(check = TRUE)`.
2.  The findings of
    [`validate_scenario_parameters()`](https://energyRt.org/reference/validate_scenario_parameters.md)
    and
    [`process_operability()`](https://energyRt.org/reference/process_operability.md)
    on the interpolated scenario.
3.  For an INFEASIBLE solve: the dummy import and export on all
    commodities. A non-zero slack names the commodity, region, year and
    timeslice; a model that stays infeasible points to a user constraint
    or to conflicting bounds.
4.  [`multimod::lp_bounds()`](https://optimal2050.github.io/multimod/r/reference/lp_bounds.html)
    on the matrix for the rows that cannot be met.
5.  For an OPTIMAL but suspect solution:
    [`verify_solution()`](https://energyRt.org/reference/verify_solution.md),
    then a second backend.
6.  For numerical trouble (slow convergence, a solver that stops with a
    numerical status):
    [`audit_coefficients()`](https://energyRt.org/reference/audit_coefficients.md).
