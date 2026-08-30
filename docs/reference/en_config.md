# The energyRt configuration file

Solver paths and other options can be persisted to a YAML file so they
are restored in every session: the user-wide file under
[`tools::R_user_dir()`](https://rdrr.io/r/tools/userdir.html)`("energyRt", "config")`,
or `./.energyRt.yml` for a single project. energyRt applies them when
the package loads, without overriding anything the user has already set
through an R option or an environment variable.

Use
[`en_config_show()`](https://energyRt.org/reference/en_config_show.md)
to see which options are in effect and where each value came from.

`en_config_read()` reads the project config (`./.energyRt.yml`) merged
over the user-wide one. It reports what is *on disk*; use
[`en_config_show()`](https://energyRt.org/reference/en_config_show.md)
for what is actually in effect.

`en_config_write()` persists options so they are restored in later
sessions. Called with no arguments it writes every option whose current
value differs from the package default – so the usual workflow is to set
paths interactively, check them with
[`en_config_show()`](https://energyRt.org/reference/en_config_show.md),
then call `en_config_write()` once.

## Usage

``` r
en_config_path(global = TRUE)

en_config_read(global = NULL)

en_config_write(config = NULL, global = TRUE, backup = TRUE)
```

## Arguments

- global:

  logical. `TRUE` writes the user-wide config (see `en_config_path()`),
  `FALSE` writes `./.energyRt.yml`.

- config:

  named list of option values, using names *without* the `en.` prefix
  (e.g. `list(gams_path = "C:/GAMS/48")`). `NULL` (default) collects the
  currently non-default options.

- backup:

  logical. When writing the user-wide config, move any pre-0.74 files in
  the home directory (`~/.energyRt/config.yml` and the deprecated
  `~/.energyRt.R`) aside to `*.bak`. They would otherwise keep
  overriding the new config. Set `FALSE` to leave them in place.

## Value

character, the path. The file need not exist.

a named list of option values, possibly empty.

the written config, invisibly.

## Location

Up to energyRt 0.74 the user-wide file was `~/.energyRt/config.yml`.
CRAN policy does not allow a package to write in the user's home
filespace, so it now lives in the standard R user configuration
directory instead. The old file is still *read* (nothing breaks), and
`en_config_write()` moves it aside once the new one exists.

## See also

Other options:
[`en_config_show()`](https://energyRt.org/reference/en_config_show.md),
[`get_arrow_format()`](https://energyRt.org/reference/arrow_format.md),
[`get_default_solver()`](https://energyRt.org/reference/default_solver.md),
[`isVerbose()`](https://energyRt.org/reference/isVerbose.md),
[`set_log_file()`](https://energyRt.org/reference/log.md),
[`set_option()`](https://energyRt.org/reference/en_option.md),
[`set_registry_file()`](https://energyRt.org/reference/registry_file.md),
[`set_reports_path()`](https://energyRt.org/reference/reports_path.md),
[`set_scenarios_path()`](https://energyRt.org/reference/scenarios_path.md),
[`set_solver_path()`](https://energyRt.org/reference/solver_path.md)

## Examples

``` r
en_config_path()
#> [1] "C:\\Users\\admin\\AppData\\Roaming/R/config/R/energyRt/config.yml"
if (FALSE) { # \dontrun{
set_solver_path("gams", "C:/GAMS/win64/48.1")
en_config_write()
} # }
```
