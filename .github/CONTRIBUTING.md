# Contributing to energyRt

> Stack-wide conventions (naming, style, git policy, API
> principles, and the terms on which AI assistance is used) are unified in
> [optimal2050 CONVENTIONS.md](https://github.com/optimal2050/.github/blob/main/CONVENTIONS.md);
> this file covers the workflow of this repo.

Thanks for your interest! `energyRt` is the model hub of the optimal2050
modeling stack — it builds energy system optimization models and writes them
out to several solver backends.

> The repository is public and contributions are welcome — issues, ideas, and
> pull requests alike. Open an issue first for larger changes; the package is
> pre-1.0 and APIs still move.

**Before writing code, read [`.claude/CLAUDE.md`](../.claude/CLAUDE.md).** It
records the failure modes of this codebase, most of which are *silent* — a run
that looks green while having tested nothing, a parameter accepted by a
constructor that never reaches the solver. It is the highest-value page here.

## Development workflow

```r
# from the repo root
devtools::load_all()
devtools::document()      # regenerates NAMESPACE and all of man/
devtools::test()
devtools::check()
```

Two things about the test suite that catch everyone once:

- **Use `devtools::test()`, not a bare `Rscript`.** `skip_if_no_solver()` calls
  `skip_on_cran()`, and outside `devtools` there is no `NOT_CRAN`, so every
  solver test skips. The symptom is a suspiciously green, suspiciously small
  run. Set `NOT_CRAN=true` if you invoke `testthat::test_file()` directly.
- A run that **aborted early is not a passing run**. Check the totals.

## Solver backends

Models are rendered to GLPK/MathProg, JuMP (Julia), Pyomo and GAMS, and the
templates in `glpk/`, `julia/`, `pyomo/` and `gams/` are baked into
`R/sysdata.rda` by `data-raw/DATASET.R`. **Editing a template does nothing
until that script is re-run.** A change to one backend almost always has to be
made in all four; the golden benchmarks under `tests/testthat/goldens/` are
what catch a backend left behind, because they store objectives and aggregated
values rather than names.

GAMS needs a license; NEOS is the practical way to exercise that backend
(`set_neos_email()`, then a `neos_gams_*` solver option).

## Repository layout

```
R/                 the package
glpk/ julia/ pyomo/ gams/   solver templates (baked into sysdata)
data-raw/          DATASET.R rebuilds sysdata from the templates + YAML specs
tests/testthat/    unit tests, goldens/, and _snaps/
drafts/            archived code — superseded work moves here, it is not deleted
dev/ dev-scripts/  design notes and the mapping-pipeline docs
```

## Commit & PR conventions

- Open PRs against **`dev`** — this repo's default branch is not `main`.
- Conventional Commits style is encouraged but not enforced
  (`feat:`, `fix:`, `docs:`, `refactor:`, `test:`, `chore:`).
- CI must pass.
- User-facing changes get a bullet in `NEWS.md` under the development version,
  filed beneath one of `## Breaking changes / New features / Deprecations /
  Bug fixes / Documentation`. Keep it to the final state in one or two lines —
  rationale belongs in the commit message or `dev/`, not in NEWS.
- Reference the sibling package's PR when a change spans both.

## License

energyRt is **AGPL-3**. Note that other packages in the stack are Apache-2.0 or
MIT — do not copy code or data across that boundary without an explicit
maintainer decision. By contributing you agree that your contributions are
licensed under the AGPL-3. See [LICENSE](../LICENSE).

## Code of Conduct

Please note that this project is released with a Contributor Code of Conduct.
By participating in it you agree to abide by its terms.
