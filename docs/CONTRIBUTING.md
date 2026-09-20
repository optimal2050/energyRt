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

## Code style

The stack rules are in [CONVENTIONS.md § Code
style](https://github.com/optimal2050/.github/blob/main/CONVENTIONS.md).
The one that bites hardest here:

**Never nest the pipeline calls.** Use `|>`, or assign each step. A
reader should follow a run top to bottom without unwrapping parentheses,
and every intermediate should be inspectable — which is the whole point
of a scenario object that carries its own state.

``` r

# no
scen <- solve_scenario(interpolate_model(mod, name = "BASE"),
                       solver = solver_options$glpk)
```

energyRt gives you two shapes instead, and both are first-class API.

**One call** — interpolates and solves. Use it in READMEs, `@examples`
and anywhere the intermediate scenario is not the point:

``` r

scen <- solve_model(mod, name = "BASE", solver = solver_options$glpk)
scen <- solve(mod, name = "BASE", solver = solver_options$glpk)   # identical
```

**Step by step** — each verb takes a scenario and returns one, so a
stage can be inspected or re-run with different arguments:

``` r

scen <- interpolate(mod, name = "BASE")   # or interpolate_model()
scen <- solve(scen)                       # or solve_scenario()

# same thing as a pipeline
scen <- mod |> interpolate(name = "BASE") |> solve()
```

**[`solve()`](https://rdrr.io/r/base/solve.html) does the whole solver
stage**: it writes the script, runs the solver, waits (`wait = TRUE`)
and reads the solution back (`read.solution = TRUE`). Do not append
`read()` to a solve — the scenario it returns is already populated. The
other two verbs exist for the cases where that is not what you want:

- [`write_script()`](https://energyRt.org/reference/write.md) /
  [`write_sc()`](https://energyRt.org/reference/write.md) — write the
  solver files without running them, to hand off or inspect.
- `read()` / `read_solution(scen, run = )` — read a **different recorded
  run** back into the scenario (`"<solve>"` or `"<variant>/<solve>"`,
  see
  [`scenario_runs()`](https://energyRt.org/reference/scenario_runs.md)),
  or pick up a solve that ran with `wait = FALSE`.

[`solve()`](https://rdrr.io/r/base/solve.html), `interpolate()` and
`read()` are the S4 generics;
[`solve_model()`](https://energyRt.org/reference/solve_model.md),
[`solve_scenario()`](https://energyRt.org/reference/solve_model.md),
[`interpolate_model()`](https://energyRt.org/reference/interpolate_model.md)
and [`read_solution()`](https://energyRt.org/reference/read.md) are the
canonical names they dispatch onto. Both spellings are supported and
neither is deprecated — prefer the generic in prose and examples, the
explicit name where the class being acted on is not obvious from
context.

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
- User-facing changes get a bullet in `NEWS.md` (rules below).
- Reference the sibling package’s PR when a change spans both.

## NEWS, comments and where narrative goes

Three places carry three different kinds of text. Keep them apart.

**`NEWS.md`** follows the r-pkgs.org convention:

- Every bullet sits under one of
  `## Breaking changes / New features / Deprecations / Bug fixes / Documentation`
  in the development-version section — never between the version heading
  and the first `##`.
- One change per bullet, the **final state** in one to three lines: what
  the user sees now, and for a bug fix what was wrong in a clause. No
  blank lines between bullets.
- No discovery story, no timings or measurements, no design
  alternatives, no internal (dot-prefixed or unexported) function names.
  Exported functions, arguments, slots, options and file names are the
  vocabulary.
- If a bullet needs more than three lines to be understood, the extra
  belongs in `dev/` (below), not in NEWS.

**Code comments** state mechanism and constraints the code cannot show —
“stats::aggregate drops NA by-groups”, “must enter the render key”, “the
fence must sit at column 0”. They never say how a finding was made, who
asked for it, what it replaced, or what it measured. Roxygen is user
documentation, not commentary: no design essays there either.

**Narrative** — rationale, measurements, traps, rejected alternatives,
lifted essays — goes to a topic file under `dev/`
(`dev/notes-<topic>.md`, e.g. `notes-reports.md`,
`notes-news-rationale.md`, `notes-dispatch-surrogate.md`) or into the
commit message. Prose meant for users becomes a vignette or article.
Before committing, check the diff for comments that answer “why did we
discover this” instead of “what must hold here”, and for NEWS bullets
longer than three lines.

## License

energyRt is **Apache-2.0** from v0.90. By contributing you agree that
your contributions are licensed under the same terms (inbound =
outbound). See [LICENSE](https://energyRt.org/LICENSE).

## Code of Conduct

Please note that this project is released with a Contributor Code of
Conduct. By participating in it you agree to abide by its terms.
