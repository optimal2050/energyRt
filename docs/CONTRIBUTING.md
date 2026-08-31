# Contributing to energyRt

> Stack-wide conventions (naming, style, git policy, API principles, and
> the terms on which AI assistance is used) are unified in [optimal2050
> CONVENTIONS.md](https://github.com/optimal2050/.github/blob/main/CONVENTIONS.md);
> this file covers the workflow of this repo.

Thanks for your interest! `energyRt` is the model hub of the optimal2050
modeling stack — it builds energy system optimization models and writes
them out to several solver backends.

> The repository is public and contributions are welcome — issues,
> ideas, and pull requests alike. Open an issue first for larger
> changes; the package is pre-1.0 and APIs still move.

**Before writing code, read
[`.claude/CLAUDE.md`](https://energyRt.org/.claude/CLAUDE.md).** It
records the failure modes of this codebase, most of which are *silent* —
a run that looks green while having tested nothing, a parameter accepted
by a constructor that never reaches the solver. It is the highest-value
page here.

## Development workflow

``` r

# from the repo root
devtools::load_all()
devtools::document()      # regenerates NAMESPACE and all of man/
devtools::test()
devtools::check()
```

Two things about the test suite that catch everyone once:

- **Tiers, not `skip_on_cran()`.** Solver tests are gated by
  `ENERGYRT_TEST_TIER` (`check < fast < cross < nightly`; unset resolves
  to `fast` locally and `check` under `R CMD check`), so a plain run
  solves whenever `glpsol` is present. For a standalone
  [`testthat::test_file()`](https://testthat.r-lib.org/reference/test_file.html),
  `pkgload::load_all(".")` first. See `tests/README.md` and
  `dev/TESTING.md`.
- A run that **aborted early is not a passing run**. Check the totals.

## Naming (digest — the full rule lives in the stack doc)

- **`verb_class()`** for operations and transforms:
  [`save_model()`](https://energyRt.org/reference/model_store.md),
  [`load_registry()`](https://energyRt.org/reference/registry.md),
  [`drop_scenario_run()`](https://energyRt.org/reference/scenario_runs.md),
  [`apply_ledger()`](https://energyRt.org/reference/apply_ledger.md).
- **Class-prefixed nouns** for properties and queries:
  [`model_hash()`](https://energyRt.org/reference/model_hash.md),
  [`scenario_runs()`](https://energyRt.org/reference/scenario_runs.md),
  [`solution_ledger()`](https://energyRt.org/reference/solution_ledger.md).
- Constructors
  ([`newScenario()`](https://energyRt.org/reference/newScenario.md)),
  `get_/set_` option accessors, and foreign generics keep their
  conventional shapes. See [CONVENTIONS.md §
  Naming](https://github.com/optimal2050/.github/blob/main/CONVENTIONS.md).
- **`en_` is the package’s namespace prefix** wherever a name must be
  claimed in a space energyRt does not own: low-level utilities
  (`en_config()`, `en_option()`, `en_open_dataset()`, `en_install_*()`),
  S3 class tags on foreign structures (`en_registry` on the registry
  tibble), CSS classes and LaTeX colour names in the report templates
  (`.en-rule`, `en_blue`), and, as **`en-`**, the knitr chunk labels of
  every shipped template (`en-setup`, `en-css`, …). Template chunk
  labels MUST carry the prefix:
  [`report()`](https://energyRt.org/reference/report.md) can run inside
  a user’s own knit, where an unprefixed label like `setup` collides
  with the outer document’s chunks and aborts the render (“Duplicate
  chunk label”). **`en` is the only permitted prefix** — never package
  initials or project names (`ert`, `ideea`). This is now checked:
  `tests/testthat/test-naming.R` fails on a foreign prefix, so the rule
  cannot rot back in.

## Solver backends

Models are rendered to GLPK/MathProg, JuMP (Julia), Pyomo and GAMS, and
the templates in `glpk/`, `julia/`, `pyomo/` and `gams/` are baked into
`R/sysdata.rda` by `data-raw/DATASET.R`. **Editing a template does
nothing until that script is re-run.** A change to one backend almost
always has to be made in all four; the golden benchmarks under
`tests/testthat/goldens/` are what catch a backend left behind, because
they store objectives and aggregated values rather than names.

GAMS needs a license; NEOS is the practical way to exercise that backend
([`set_neos_email()`](https://energyRt.org/reference/neos_email.md),
then a `neos_gams_*` solver option).

**Backend choice by model size.** The default backend for *small* models
is **GLPK** (`solver_options$glpk`): zero startup cost, no external
toolchain, and faster end-to-end whenever the solve itself is under ~30
seconds — the tm/unit test fixtures and the UTOPIA `R1`/`R3` layouts all
fall here. For **mid-size and sampled models** — the UTOPIA `R7`/`R11`
layouts, hourly or full-year calendars, spatially or temporally sampled
runs — use **julia/HiGHS** (`solver_options$julia_highs`): its presolve
and dual simplex dominate once the LP is large or degenerate (a
full-year 8760-slice model: ~3 minutes in glpsol vs ~2 seconds of HiGHS
runtime after the one-off Julia warmup). The golden suites encode the
same rule per suite (`SUITE_SOLVERS` in `tools/test/make_goldens.R`), so
regenerating the mid-size goldens requires Julia with JuMP/HiGHS
installed.

## Repository layout

    R/                 the package
    glpk/ julia/ pyomo/ gams/   solver templates (baked into sysdata)
    data-raw/          DATASET.R rebuilds sysdata from the templates + YAML specs
    tests/testthat/    unit tests, goldens/, and _snaps/
    drafts/            archived code — superseded work moves here, it is not deleted
    dev/ dev-scripts/  design notes and the mapping-pipeline docs

## Commit & PR conventions

- Open PRs against **`dev`** — this repo’s default branch is not `main`.
- Conventional Commits style is encouraged but not enforced (`feat:`,
  `fix:`, `docs:`, `refactor:`, `test:`, `chore:`).
- CI must pass.
- User-facing changes get a bullet in `NEWS.md` under the development
  version, filed beneath one of
  `## Breaking changes / New features / Deprecations / Bug fixes / Documentation`.
  Keep it to the final state in one or two lines — rationale belongs in
  the commit message or `dev/`, not in NEWS.
- Reference the sibling package’s PR when a change spans both.

## License

energyRt is **AGPL-3**. Note that other packages in the stack are
Apache-2.0 or MIT — do not copy code or data across that boundary
without an explicit maintainer decision. By contributing you agree that
your contributions are licensed under the AGPL-3. See
[LICENSE](https://energyRt.org/LICENSE).

## Code of Conduct

Please note that this project is released with a Contributor Code of
Conduct. By participating in it you agree to abide by its terms.
