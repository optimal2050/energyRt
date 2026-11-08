# `data-raw/` — the inputs baked into the package

Everything in this directory is a build-time input. None of it ships: the whole
directory is excluded by `.Rbuildignore`. The package sees only what
`data-raw/DATASET.R` generates from it.

**The rule that governs all of it: editing any file here has no effect until
`data-raw/DATASET.R` is re-run from the package root, and the regenerated
artefacts are committed.** The script sources `maps.R` first and all its paths
are relative, so the working directory matters. The artefacts are
`R/sysdata.rda`, `R/defaults.R` and `R/recast-rules.R`.

The three model-symbol catalogues — `sets.yml`, `parameters.yml` and
`mappings.yml` — are concatenated in that order, and that order is `.modInp`'s
key order, which the matrix writers follow: it is not cosmetic.

`parameters.yml` has two sections. The 200 top-level keys are the parameters.
The reserved `columns:` key at the end lists the value columns that belong to
*no* parameter, each with its recast rule and a one-line reason — a stated set
rather than whatever a scan happens to miss. Neither `recast:` nor `columns:`
reaches `.modInp`: `DATASET.R` strips them, and they are read by
`data-raw/col_param.R` and `data-raw/recast_rules.R` instead.

## What becomes what

| input | object | lands in | maintained |
|---|---|---|---|
| `sets.yml` | `.modInp` (15 sets) | `R/sysdata.rda` | by hand |
| `parameters.yml` | `.modInp` (200 parameters), `.col_param`, `.defRecast` | `R/sysdata.rda`, `R/recast-rules.R` | by hand |
| `mappings.yml` | `.modInp` (359 mappings) | `R/sysdata.rda` | by hand |
| `classes-docs.yml` | `.classes` | `R/sysdata.rda` | by hand |
| `variables.yml` | `.variables` | `R/sysdata.rda` | by hand |
| `mapping_spec.yml` | `.mapping_spec` | `R/sysdata.rda` | generated + annotated |
| `config_default_values.yml` | `.defVal` | `R/defaults.R` | by hand |
| `config_default_interpolation.yml` | `.defInt` | `R/defaults.R` | by hand |
| `maps.R` | `.variable_mapping`, map definitions | `R/sysdata.rda` | gams2x output |

`data-raw/col_param.R` builds `.col_param`, one row per value column of every
slot: the parameter that owns it, the rule and default for that column, and —
where nothing owns it — the reason. It is the only place that resolves the two
irregularities in the relation: `settings` reads config's slots by inheritance,
and `pDemand` is catalogued under `colName: dem` while the column of
`demand@demand` is `demand` (declared as `colAlias`). `.param_interp_map()` and
the recast contract both index it rather than re-deriving it, and
`tests/testthat/test-col-param-map.R` holds it against the S4 classes.

Two details are easy to get wrong. `.defVal`, `.defInt` and `.defRecast` are
**not** in `sysdata.rda`: they are written out as R source, so a stale
`R/defaults.R` or `R/recast-rules.R` is as much a problem as a stale
`sysdata.rda`. And `mapping_spec.yml` is half generated: `make_mapping_spec.R`
refreshes `name`, `dims`, `gates_var` and `gates_eq` from `maps.R` and
`mappings.yml`, while `recipe`, `source`, `predicate`, `join_keys`, `filter_by`
and `depends_on` come from `mapping_annotations.R` and survive regeneration.

## Which file is authoritative

`parameters.yml` is the parameter catalogue and the authority for a parameter's
`dimSets`, `defVal` and `interpolation`, and for the class / slot / column it is
written from. The two `config_default_*.yml` tables are **override baselines**
keyed by `colName`, not the defaults themselves:
`.apply_settings_param_overrides()` compares a scenario's settings against them,
so a `colName` with no key there cannot be overridden from settings at all.
Their keys follow the parameter's type — a `bounds` parameter with
`colName: x` is reached as `x.lo` / `x.up`, a `numpar` as bare `x` — and a bare
key on a bounds parameter is unreachable.

Agreement between the catalogue and the interpolation baseline, together with
the set of colNames the baseline cannot reach, is pinned by
`tests/testthat/test-family-interp-rules.R`. There is no equivalent guard for
`config_default_values.yml` yet.

`variables.yml` must stay total over `.variable_mapping`; `DATASET.R` errors
otherwise. Its own header explains why, and names the dead overlay that
motivated the check.
