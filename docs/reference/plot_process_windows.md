# Plot process investment / availability windows

A Gantt-style chart of when each technology (and storage) can be
**built** (solid bar, from `@start` to `@end`; defaults to the horizon
when the slots are empty) and how long the last-built vintage can
**operate** (translucent tail, `end + olife`). Faceted by region when
the windows differ regionally.

`autoplot(model, type = "windows")` and
`autoplot(repository, type = "windows")` dispatch here.

## Usage

``` r
plot_process_windows(object, region = NULL, horizon = NULL)

# S3 method for class 'model'
autoplot(object, type = c("windows"), ...)

# S3 method for class 'repository'
autoplot(object, type = c("windows"), ...)
```

## Arguments

- object:

  a `model` / `repository` object (autoplot methods).

- region:

  character vector to filter regions, or `NULL` (all).

- horizon:

  a `horizon` object used for defaults; taken from the model's config
  automatically (optional for a repository).

- type:

  only `"windows"` currently.

- ...:

  passed on to `plot_process_windows()`.

## Value

A `ggplot` object.

## Examples

``` r
if (FALSE) { # \dontrun{
autoplot(mod, type = "windows")
autoplot(repo, horizon = newHorizon(2020:2050))
} # }
```
