# Validate a techspec and report issues

Applies the techspec validation policy: unknown sections/columns are
errors; per-vintage values covering only a subset of the declared
vintages are reported (never silently filled); nothing is auto-invented.

## Usage

``` r
tech_spec_issues(spec)
```

## Arguments

- spec:

  Path or list (see
  [`read_techspec()`](https://energyRt.org/reference/read_techspec.md)).

## Value

A `data.frame` with columns `severity` (`"error"`/`"warning"`),
`section`, and `message`. Zero rows means a clean spec.

## Examples

``` r
f <- system.file("techspec", "examples", "coal_power.yml",
                 package = "energyRt")
if (nzchar(f)) tech_spec_issues(f)
#> [1] severity section  message 
#> <0 rows> (or 0-length row.names)
```
