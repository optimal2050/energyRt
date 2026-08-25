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
(`check < fast < cross < nightly`); see `dev/TESTING.md` for the full guide.

| Tier | Needs | Typical use |
|---|---|---|
| check | R only | `R CMD check` / CRAN — no solver, fixtures ship in `tests/` |
| fast | + glpsol | default for `devtools::test()`; `Rscript tools/test/run_fast.R` before commit |
| cross | + julia / pyomo / gams (any) | `Rscript tools/test/run_cross.R` — cross-backend parity |
| nightly | + external model repos | `Rscript tools/test/run_nightly.R` — deep regression, interp goldens, external models |

Solver tests always skip cleanly when a toolchain is missing (the GAMS guard
also skips an unlicensed install); they must never error on absence.

Key shared machinery: `helper-forks.R` (`run_family()` fork harness),
`helper-goldens.R` + `tools/test/make_goldens.R` (tracked-value benchmarks,
capture gated by `verify_solution()`), `helper-utopia.R` (UTOPIA reference
suite), `fixtures/testing-models.R` (the tm_* tier models),
`helper-mapping.R::solved_tier()` (cached GLPK solves shared across files),
`tools/test/interp_guard.R` (tracked interpolation baselines in
`goldens/interp/`, checked nightly by `test-interp-golden.R`).

## The nightly run

`Rscript tools/test/run_nightly.R` runs the interp-goldens check plus the FULL
suite at tier `nightly` (a superset of fast and cross) and writes a markdown
report to `tmp/test-reports/nightly-YYYYMMDD-HHMM.md`. Opt-in extras via the
environment:

- `ENERGYRT_EXT_MODELS=<dir>` — directory with external real-world models
  (the useR2026 belgium models: `belgium_model.rds`,
  `belgium_storage_duration.rds`, `belgium_copperplate.rds`); each is loaded
  through the serialization upgrade path, solved on julia/HiGHS, and checked
  with `verify_solution()`. Unset ⇒ clean skips.
- `ENERGYRT_TEST_HEAVY=true` — adds the full-year (8760 h) copperplate model.
- `ENERGYRT_TEST_NEOS=true` — adds the NEOS remote-solver tests.

To schedule on Windows (daily 02:30, from the package root):

```powershell
schtasks /Create /TN energyRt-nightly /SC DAILY /ST 02:30 /TR "pwsh -NoProfile -Command `"Set-Location 'C:\Users\admin\Documents\R\energyRt'; `$env:ENERGYRT_EXT_MODELS='C:\Users\admin\Documents\R\useR2026_ensys\data'; Rscript tools/test/run_nightly.R`""
```

## GitHub Actions (sketch — not set up)

CI is deliberately not configured yet; the tier env var means workflows need
zero test-code changes whenever it is. The intended shape, all on
`ubuntu-latest` with `r-lib/actions/setup-r` + dependency caching:

1. **check** — `R CMD check` via `r-lib/actions/check-r-package`. No solver
   installed; the tier auto-resolves to `check`, so only solver-free tests run.
2. **fast** — `apt-get install glpk-utils`, then
   `Rscript tools/test/run_fast.R`.
3. **cross** — additionally `julia-actions/setup-julia` (+ `JuMP`/`HiGHS`, with
   `julia-actions/cache` for the depot) and `actions/setup-python` (+ `pyomo`
   and CBC via `apt-get install coinor-cbc`), then
   `Rscript tools/test/run_cross.R`. GAMS is omitted (licensing on runners);
   NEOS parity can be enabled by setting `ENERGYRT_TEST_NEOS=true` plus an
   email in a repo secret, ideally on a schedule rather than per-push.

Missing toolchains skip rather than fail by design, so partial jobs degrade
gracefully. The nightly tier stays local (external model repos + wall time).
