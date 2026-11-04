# Check a model for contradictions that need no solver

Compares requirements that are FORCED by the declarations against
ceilings that CANNOT be raised, per commodity, region and year. A
finding is a contradiction in the data: no solver setting, starting
point or tolerance will make such a model feasible, and an infeasible
solve will not say which constraint was to blame.

Two checks run:

- `aux-from-stock`:

  exogenous capacity (`@capacity$stock`) consumes an auxiliary commodity
  through `@aeff$cap2ainp`. The stock is given, so the consumption is
  unavoidable; if the commodity's supply is bounded and cannot be
  imported or traded in, a shortfall is a contradiction.

- `demand-vs-supply`:

  a declared `demand` exceeds the bounded supply of its commodity in the
  same region and year, again with no import and no inbound route.

## Usage

``` r
check_feasibility(object, years = NULL, verbose = TRUE)
```

## Arguments

- object:

  a `model`, `scenario` or `repository`.

- years:

  milestone years to check. `NULL` (default) checks every year present
  in the declarations.

- verbose:

  print the report. The findings are returned either way.

## Value

A data.frame of findings – `check`, `commodity`, `region`, `year`,
`required`, `available`, `shortfall`, `forced_by` – with zero rows when
nothing is contradicted. Returned invisibly when `verbose`.

## What a clean result does NOT mean

It does not mean the model is feasible. Everything that depends on a
DECISION is the solver's question and is not asked here: whether
capacity can serve the load, whether a bound binds once the model
optimises, whether the dispatch closes in every timeslice, whether a
corridor is big enough. This function proves the absence of one kind of
contradiction, not the presence of a solution.

## Examples

``` r
if (FALSE) { # \dontrun{
check_feasibility(mod)
# or, before a solve that would otherwise fail without saying why:
interpolate_model(mod, name = "BASE", check = TRUE)
} # }
```
