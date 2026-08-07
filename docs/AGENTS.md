# Working on energyRt

Onboarding notes for contributors and coding agents. Everything here is
something you would otherwise discover the hard way — most of these
failures are **silent**.

This is the canonical file; `CLAUDE.md` and
`.github/copilot-instructions.md` just point here.

## Layout

| Path | What it is |
|----|----|
| `R/` | package source (~44k lines). `class-*.R` define the S4 classes |
| `glpk/ gams/ julia/ pyomo/` | solver model templates — **not shipped as files**, see below |
| `data-raw/` | scripts that build `R/sysdata.rda` and `data/*.rda` |
| `dev/`, `dev-scripts/` | design notes and maintenance utilities |
| `depreciated/`, `drafts/` | archived code — **do not edit** |
| `docs/` | pkgdown build output — **never edit by hand** |

## Build and test

``` r

devtools::load_all()
devtools::document()      # regenerates NAMESPACE and all of man/
devtools::test()
```

Two traps:

- **[`devtools::test()`](https://devtools.r-lib.org/reference/test.html)
  aborts the whole run** with `unexpected input` at `1:1` — the file
  `tests/testthat/test-mapping-engine.R` starts with a UTF-8 BOM
  (`EF BB BF`). Use `devtools::test(filter = "...")` to work around it,
  and note that a “clean” run that stopped early is not a passing run.
- **Running a test file directly silently skips solver tests.**
  `skip_if_no_solver()` begins with `skip_on_cran()`, and a bare
  `Rscript` has no `NOT_CRAN`.
  [`devtools::test()`](https://devtools.r-lib.org/reference/test.html)
  sets it; if you call
  [`testthat::test_file()`](https://testthat.r-lib.org/reference/test_file.html)
  yourself, set `NOT_CRAN=true`. Symptom is a suspiciously green run
  (e.g. 5 pass / 7 skip instead of 28 pass).

## Traps that fail silently

**`new()` does not construct objects.** `new("technology", name = "X")`
returns an *empty* technology — no error. Sixteen no-op `initialize`
methods (`function(.Object, ...) .Object`) deliberately disable S4’s
argument filling. The real constructor is `R/data2slots.R`, reached via
[`newTechnology()`](https://energyRt.org/reference/technology.md),
[`newCommodity()`](https://energyRt.org/reference/newCommodity.md),
[`newSupply()`](https://energyRt.org/reference/newSupply.md), … Use
those. [`update()`](https://rdrr.io/r/stats/update.html) on an existing
object goes through the same path.

**S3 methods on S4 classes need `@exportS3Method`.** `print` is an S4
generic whose default body is `UseMethod("print")` (`R/print.R:11`). A
`#' @export` tag on `print.<class>` produces a plain `export()` in
NAMESPACE, which does *not* register the method — dispatch falls through
to the S4 default and dumps raw slots. Use
`#' @exportS3Method print <class>`. Currently dead for this reason:
`print.commodity`, `print.demand`, `print.supply`.

**Editing a solver template has no effect until you rebuild `sysdata`.**
`glpk/`, `gams/`, `julia/` and `pyomo/` are in `.Rbuildignore`; the
templates ship *only* as character vectors baked into `R/sysdata.rda` by
`data-raw/DATASET.R`. After changing any template — or `data-raw/maps.R`
or `data-raw/modInp.yml` — re-run `data-raw/DATASET.R` **from the
package root** (it sources `maps.R` first and all its paths are
relative) and commit the regenerated `R/sysdata.rda`.

**The templates are spliced at grep-located markers.** Writers in
`R/write_glpk.R`, `R/write_gams.R`, `R/write_jump.R` and
`R/write_pyomo.R` locate split points by regex, and some hard-stop
unless the match is unique. When editing a template, do not introduce a
line matching `^minimize` or `^end;` (GLPK), `$include data.gms` (GAMS),
`^##### decl par #####` or `^model.obj` (Pyomo), or `^@objective`
(Julia). `glpk/energyRt.mod` also contains a UUID marker on line ~529
that must stay unique.

**[`devtools::document()`](https://devtools.r-lib.org/reference/document.html)
touches far more than your change.** It rewrites every `.Rd` and can
flush a backlog of stale docs from unrelated in-flight work. Always
check `git diff NAMESPACE` and `git status man/` before committing.

**`Collate:` in DESCRIPTION is load-bearing.** S4 requires parent
classes to exist at `setClass` time. New class files need an `@include`
tag so roxygen orders them correctly.

**Changing a class slot breaks saved scenarios.**
[`save_scenario()`](https://energyRt.org/reference/save_scenario.md)/[`load_scenario()`](https://energyRt.org/reference/load_scenario.md)
use plain
[`save()`](https://rdrr.io/r/base/save.html)/[`load()`](https://rdrr.io/r/base/load.html)
with no `updateObject()` step. There is no test fixture guarding this.
Several one-off upgrade shims exist
([`.upgrade_summand()`](https://energyRt.org/reference/class-summand.md)
in `R/class-constraint.R`, `.hasSlot()` probes in `R/class-settings.R`
and `R/class-config.R`) — follow that pattern if you must add a slot.

## Conventions

- **Don’t delete code — move it to `drafts/`.**
- Prefer dplyr/dtplyr and the native pipe (`|>`) for data-frame work.
- Follow the tidyverse style guide; don’t restyle code unrelated to your
  change.
- User-facing changes get a bullet at the top of `NEWS.md`.
- Class and slot documentation is generated from `data-raw/classes.yml`
  via [`get_slot_doc()`](https://energyRt.org/reference/get_slot_doc.md)
  — keep it in step with the `setClass` prototypes when adding or
  renaming a slot.

## Further reading

- `dev-scripts/EXTENDING.md`, `dev-scripts/PIPELINE.md` — the
  data-driven mapping pipeline
- `dev/known_bugs.qmd` — open defects
- `dev/s7-migration-decision.md` — why the package stays on S4
- `.github/CONTRIBUTING.md` — PR process, and the policy on AI-assisted
  contributions

------------------------------------------------------------------------

*Last verified against 0.71.0.9000 (2026-08-01). Drafted with AI
assistance; content is the maintainers’ responsibility. Please re-check
the commands and file references at each release — a stale instruction
is worse than none.*
