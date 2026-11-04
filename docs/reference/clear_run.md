# Release a run that is marked running

A run whose process died without closing its own record stays marked
`running`, and that mark refuses reads, re-solves, drops and output
seals. `clear_run()` is the override: it records the run as `abandoned`
so those operations proceed again.

## Usage

``` r
clear_run(object, run, verbose = TRUE)
```

## Arguments

- object:

  a scenario.

- run:

  the run id, as
  [`scenario_runs()`](https://energyRt.org/reference/scenario_runs.md)
  lists it.

- verbose:

  logical.

## Value

The run record, invisibly.

## Details

Nothing is deleted and no solver output is touched — only the record's
`status` changes.
[`get_status()`](https://energyRt.org/reference/get_status.md) reports a
run whose process is demonstrably gone as `crashed` without any help, so
this is needed for the cases that cannot be demonstrated: a run recorded
on another machine, or one written before process ids were kept.

To remove the run and its files instead, use
[`drop_scenario_run()`](https://energyRt.org/reference/scenario_runs.md).

## See also

[`scenario_runs()`](https://energyRt.org/reference/scenario_runs.md),
[`get_status()`](https://energyRt.org/reference/get_status.md),
[`drop_scenario_run()`](https://energyRt.org/reference/scenario_runs.md)
