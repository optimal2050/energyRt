# Delete rendered reports

Removes the reports
[`report()`](https://energyRt.org/reference/report.md) rendered for an
object — the files and their `.report.yml` sidecars. **Dry-run by
default.** Reports are derived artifacts: the sidecar keys them on the
object content, the template and the render arguments, so anything
removed here re-renders on demand.

## Usage

``` r
clear_report_cache(x = NULL, dry_run = TRUE, verbose = TRUE)
```

## Arguments

- x:

  an energyRt object whose reports should be removed, or a character
  path to a reports directory. `NULL` (default) targets the
  project-level tier.

- dry_run:

  logical. `TRUE` (default) lists what would go without removing it.

- verbose:

  logical.

## Value

a tibble of report groups (`report`, `formats`, `size_mb`, `created`,
`action`, `path`), visible on a dry run and invisible otherwise. Files
no sidecar claims are listed with `report = NA` and are removed only
when a whole directory is cleared.

## Details

Where it looks, matching where
[`report()`](https://energyRt.org/reference/report.md) writes: an
explicit directory path; otherwise the owning folder's `reports/` for a
saved object (`<scenario>/reports/`, `models/<name>/reports/`);
otherwise the project-level
[`get_reports_path()`](https://energyRt.org/reference/reports_path.md)
tier, which is where reports of in-memory objects land.

A sealed owner refuses — a sealed entry is an archive, and its reports
are part of what was archived.

## See also

[`clear_levcost_cache()`](https://energyRt.org/reference/clear_levcost_cache.md),
[`scenario_artifacts()`](https://energyRt.org/reference/scenario_artifacts.md)

## Examples

``` r
if (FALSE) { # \dontrun{
clear_report_cache(scen)                    # what would go
clear_report_cache(scen, dry_run = FALSE)   # remove them
} # }
```
