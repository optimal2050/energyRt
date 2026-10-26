# Rebuild a solve sequence from the variants on disk

Reassembles the object a driver returned —
[`solve_myopic()`](https://energyRt.org/reference/solve_myopic.md),
[`solve_by_sample()`](https://energyRt.org/reference/solve_by_sample.md),
[`solve_by_region()`](https://energyRt.org/reference/solve_by_region.md)
or [`solve_guided()`](https://energyRt.org/reference/solve_guided.md) —
from a scenario's variants, so
[`getData()`](https://energyRt.org/reference/getData.md),
[`sample_summary()`](https://energyRt.org/reference/sample_summary.md),
[`myopic_objective()`](https://energyRt.org/reference/myopic_objective.md)
and [`guided_gap()`](https://energyRt.org/reference/guided_gap.md) work
on a scenario reloaded from disk rather than only on the live result.

## Usage

``` r
read_sequence(object, sequence = NULL, type = NULL, verbose = TRUE)
```

## Arguments

- object:

  a scenario object, or the name of a stored scenario.

- sequence:

  character, the sequence name to rebuild (as recorded in
  `variant.yml`). Needed only when the scenario holds more than one.

- type:

  character, restrict to one driver: `"myopic_step"`,
  `"calendar_sample"`, `"region_sample"` or `"guided_stage"`.

- verbose:

  logical.

## Value

an object of the driver's class (`myopic`, `by_sample`, `by_region` or
`guided`), with `$reconstructed = TRUE`.

## Details

Each driver stores one variant per unit of work, tagged in
`runs/<variant>/variant.yml` with the method (`type:`), the sequence it
belongs to and how the driver was configured (`params:`). This groups
those variants back into a sequence and reads each one's solution.

A scenario can hold more than one sequence. With several present, name
the one you want with `sequence`, or narrow by `type`.

## What cannot be rebuilt

A driver's working state is never written to disk: the carry ledger
(myopic, guided), the capacity targets (guided), and the timeslice
membership of a sampling spec (by_sample). Those fields are `NULL` in
the result and named in `$missing`, and `$reconstructed` is `TRUE`. The
functions that read `$scenarios` —
[`getData()`](https://energyRt.org/reference/getData.md) and the
summaries — do not use them;
[`apply_ledger()`](https://energyRt.org/reference/apply_ledger.md) and
anything else that continues a sequence does, so a rebuilt sequence is
for reading, not for resuming.

A `by_region` sequence also loses its `boundary` stub counts, which are
derived after variant expansion and never written to disk. Its model is
recovered:
[`solve_by_region()`](https://energyRt.org/reference/solve_by_region.md)
stores the full model on the scenario and each unit's region set in that
unit's cartridge, so a variant rebuilds its own cut.

## See also

[`scenario_runs()`](https://energyRt.org/reference/scenario_runs.md),
[`scenario_solutions()`](https://energyRt.org/reference/scenario_solutions.md)

## Examples

``` r
if (FALSE) { # \dontrun{
scen <- getScenario("ELC_myopic")
seq <- read_sequence(scen)
myopic_objective(seq)
} # }
```
