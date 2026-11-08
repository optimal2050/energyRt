# `data-raw/` — the inputs baked into the package

Everything in this directory is a build-time input. None of it ships: the whole
directory is excluded by `.Rbuildignore`. The package sees only what
`data-raw/DATASET.R` generates from it.

**The rule that governs all of it: editing any file here has no effect until
`data-raw/DATASET.R` is re-run from the package root, and the regenerated
artefacts are committed.** The script sources `maps.R` first and all its paths
are relative, so the working directory matters. The artefacts are
`R/sysdata.rda` and `R/recast-rules.R`.

The three model-symbol catalogues — `sets.yml`, `parameters.yml` and
`mappings.yml` — are concatenated in that order, and that order is `.modInp`'s
key order, which the matrix writers follow: it is not cosmetic.

`parameters.yml` has two sections. The 200 top-level keys are the parameters,
each declaring everything about itself that nothing upstream can know: its
`dimSets`, `defVal`, `interpolation` rule, `recast` rule, `unit` formula, and
the class / slot / column it is written from. The reserved `columns:` key at
the end lists the value columns that belong to *no* parameter, each with its
own `recast`, `unit` and a one-line reason — a stated set rather than whatever
a scan happens to miss. None of `recast:`, `unit:` or `columns:` reaches
`.modInp`: `DATASET.R` strips them, and `data-raw/col_param.R` and
`data-raw/recast_rules.R` read them instead.

A unit is a formula over the tokens `{capacity}`, `{activity}`, `{use}`,
`{currency}`, `{comm}`, `{acomm}` and `{group}`, which `getUnits()` resolves
against the object's own `@units`, or a literal. One per parameter: measured
across the 41 parameters that own more than one column, none disagreed.

The literals are deliberate and not interchangeable. `fraction` is
dimensionless and bounded by 1 — an efficiency, a share. `factor` is
dimensionless and **may exceed 1**, which is why a weather coefficient is one:
low temperature raising conductivity puts output above nameplate. `1/year` is
a rate (`wacc`, `sdr`), `years` a lifetime, `year` a calendar year.

## What becomes what

| input | object | lands in | maintained |
|---|---|---|---|
| `sets.yml` | `.modInp` (15 sets) | `R/sysdata.rda` | by hand |
| `parameters.yml` | `.modInp` (200 parameters), `.col_param`, `.defRecast` | `R/sysdata.rda`, `R/recast-rules.R` | by hand |
| `mappings.yml` | `.modInp` (359 mappings) | `R/sysdata.rda` | by hand |
| `classes-docs.yml` | `.classes` | `R/sysdata.rda` | by hand |
| `variables.yml` | `.variables` | `R/sysdata.rda` | by hand |
| `mapping_spec.yml` | `.mapping_spec` | `R/sysdata.rda` | generated + annotated |
| `maps.R` | `.variable_mapping`, map definitions | `R/sysdata.rda` | gams2x output |

`data-raw/col_param.R` builds `.col_param`, one row per value column of every
slot: the parameter that owns it, the rule, default and unit for that column,
and — where nothing owns it — the reason. A parameter that names a slot but no
column, such as the scalar `technology@cap2act`, gets a row with `column = NA`. It is the only place that resolves the two
irregularities in the relation: `settings` reads config's slots by inheritance,
and `pDemand` is catalogued under `colName: dem` while the column of
`demand@demand` is `demand` (declared as `colAlias`). `.param_interp_map()` and
the recast contract both index it rather than re-deriving it, and
`tests/testthat/test-col-param-map.R` holds it against the S4 classes.

Two details are easy to get wrong. `.defRecast` is **not** in `sysdata.rda`:
it is written out as R source, so a stale `R/recast-rules.R` is as much a
problem as a stale `sysdata.rda`. And `mapping_spec.yml` is half generated: `make_mapping_spec.R`
refreshes `name`, `dims`, `gates_var` and `gates_eq` from `maps.R` and
`mappings.yml`, while `recipe`, `source`, `predicate`, `join_keys`, `filter_by`
and `depends_on` come from `mapping_annotations.R` and survive regeneration.

## Which file is authoritative

`parameters.yml` is the parameter catalogue and the authority for a parameter's
`dimSets`, `defVal` and `interpolation`, and for the class / slot / column it is
written from. Nothing else declares a default or a rule.
`config@defVal` and `config@interpolation` can override either one per model,
but they declare nothing: they are empty until a user sets a key, and the key
is the PARAMETER name — `pDemand`, or `pTechAf.lo` / `pTechAf.up` for a bound.
A key naming no parameter is an error. Before 0.92 those slots were pre-filled
from two `config_default_*.yml` files keyed by column name, which was a second
copy of every default and drifted from this one.

`tests/testthat/test-family-interp-rules.R` pins what each rule does, per
parameter and under an override, and that every parameter is overridable;
`tests/testthat/test-col-param-map.R` holds the column-to-parameter relation
against the S4 classes.

`variables.yml` must stay total over `.variable_mapping`; `DATASET.R` errors
otherwise. Its own header explains why, and names the dead overlay that
motivated the check.
