# Validate a process spec and report issues

Applies the spec validation policy: unknown sections/columns are errors;
per-vintage values covering only a subset of the declared vintages are
reported (never silently filled); nothing is auto-invented.

## Usage

``` r
process_spec_issues(spec)
```

## Arguments

- spec:

  Path or list (see
  [`read_process_spec()`](https://energyRt.org/reference/read_process_spec.md)).

## Value

A `data.frame` with columns `severity` (`"error"`/`"warning"`),
`section`, and `message`. Zero rows means a clean spec.

## See also

Other process spec:
[`process_from_spec()`](https://energyRt.org/reference/process_from_spec.md),
[`process_spec_code()`](https://energyRt.org/reference/process_spec_code.md),
[`process_to_spec()`](https://energyRt.org/reference/process_to_spec.md),
[`read_process_spec()`](https://energyRt.org/reference/read_process_spec.md)

## Examples

``` r
f <- system.file("techspec", "examples", "coal_power.yml",
                 package = "energyRt")
if (nzchar(f)) process_spec_issues(f)
#> [1] severity section  message 
#> <0 rows> (or 0-length row.names)
```
