# Verify accounting identities of a solved scenario

Recomputes core model identities (commodity balance and its sign per
commodity `limtype`, the source/sink total layers, cost decomposition,
and the discounted objective) from the solution tables and the
scenario's own maps, and reports any violations. A valid optimal
solution from any backend must pass every check; a failure indicates a
solver, template, writer, or reader defect – or a corrupted solution.

## Usage

``` r
verify_solution(
  object,
  checks = "default",
  tol_rel = 1e-06,
  tol_abs = 1e-06,
  verbose = FALSE
)
```

## Arguments

- object:

  a solved `scenario` (must carry `modOut` variables).

- checks:

  character. Individual check names, or the sentinels `"default"` (the
  cheap set, run everywhere) and `"all"`. See
  [`verify_checks()`](https://energyRt.org/reference/verify_checks.md)
  for the vocabulary, each check's group, and which data path it derives
  its expectation from. Defaults to `"default"`.

- tol_rel, tol_abs:

  numeric tolerances; a row violates its identity when
  `|lhs - rhs| > tol_abs + tol_rel * max(|lhs|, |rhs|)`.

- verbose:

  logical, report each check and its elapsed time as it runs. The opt-in
  checks join the whole solution table on a large model, where silence
  for minutes is not useful.

## Value

an object of class `solution_verification`: a list with `ok` (logical),
`scenario` (name), `checks` – per check a list with `status` (`"ok"`,
`"violated"`, `"skipped"`), `n` (rows checked), `violations` (data.table
of offending rows with `lhs`, `rhs`, `diff`), `stats`, `reason` when
skipped, and `report` for report-only checks – and `divergence`.

[`print()`](https://energyRt.org/reference/print.md) takes
`detail = c("full", "issues")`; `"issues"` shows only violated, skipped
and reporting checks. The full set requested is always computed either
way – an issues-only *compute* mode could hide a check that silently did
not run.

`divergence` is a data.frame, one row per check, worst first: `status`,
`n_compared`, `n_violated`, `max_abs`, `median_abs`, `mean_abs` and
`max_rel`. It summarises how far each identity came from closing **even
when the check passed**, so tolerances can be set from the noise the
backends actually produce rather than guessed. `max_abs` is what a
`tol_abs` must clear; `median_abs` beside it separates one bad cell from
a uniformly loose check. Aggregate identities (`objective`, `cost`) sum
over the whole model, so their absolute divergence grows with model size
while `max_rel` stays flat – which is why the relative term carries the
tolerance on large models.

## Examples

``` r
if (FALSE) { # \dontrun{
scen <- solve_model(mod, solver = solver_options$glpk)
vs <- verify_solution(scen)
vs$ok
print(vs)
} # }
```
