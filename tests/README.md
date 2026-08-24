# energyRt test suite — orientation

This suite is organized around a **parameter-level coverage matrix**: every
numeric parameter, map, equation, and variable of the model is a row in
`tests/coverage/parameter-matrix.csv`, regenerated from package metadata by
`tools/coverage/build_matrix.R`. Test files declare what they cover with
structured tags; the generator joins tags with the metadata and reports gaps.

Full contributor recipes (adding tests for new parameters/variables/constraints,
regenerating benchmarks): see `dev/TESTING.md`.

## The `@covers` tag

Place one or more tag comments in a test file, ideally directly above the
`test_that()` block that earns them:

```r
# @covers pTechRampUp pTechRampDown depth=S backends=glpk forks=fx,fold
test_that("ramp limits constrain dispatch", { ... })
```

- Names: any row name from the matrix — a `.modInp` entry (`pSupAva`), an
  expanded bound name (`pSupAvaLo`), a map (`mTechSpan`), an equation
  (`eqTechAf`), or a variable (`vTechAct`). Space-separated, several per tag.
- `depth=` one of `C` (constructed only), `I` (interpolated, content asserted in
  modInp), `S` (solved, result asserted), `X` (solved on 2+ backends).
  Default `S`.
- `backends=` comma list of `glpk,julia_highs,pyomo,gams,neos,mosox`.
  Default `glpk`.
- `forks=` comma list of exercised forks:
  `fx,default,fold,ondisk,sparse,prune,vintage,cluster,sampled,geoframe,code`.
  Optional.

Tags are validated by `test-coverage-matrix.R` and by
`Rscript tools/coverage/build_matrix.R --check` — a typo in a name fails the
suite. Rows without tags fall back to keyword-inferred coverage
(`status=inferred` in the CSV); a tag always overrides inference.

## Regenerating the matrix

```powershell
Rscript tools/coverage/build_matrix.R          # rewrite CSV + matrix-summary.md
Rscript tools/coverage/build_matrix.R --check  # validate tags only (no writes)
```

Regenerate after adding tests or tags and commit the refreshed CSV/summary with
them — the summary diff is the review artifact. Never edit the CSV by hand.

## Test tiers (execution frame)

Selected via the `ENERGYRT_TEST_TIER` environment variable
(`check < fast < cross < nightly`); see `dev/TESTING.md` for the cheat sheet.

| Tier | Needs | Typical use |
|---|---|---|
| check | R only | `R CMD check` / CRAN — no solver, fixtures ship in `tests/` |
| fast | + glpsol | default for `devtools::test()`; run before commit |
| cross | + julia / pyomo / gams (any) | cross-backend parity |
| nightly | + external model repos | deep regression, full fork grid |

Solver tests always skip cleanly when a toolchain is missing; they must never
error on absence.
