# Read and validate a process spec

Parses a spec file (or an already-parsed list) into the normalised list
form used by
[`process_from_spec()`](https://energyRt.org/reference/process_from_spec.md)
and the process designer. YAML (`.yml`/`.yaml`) and JSON (`.json`) are
alternative containers for the same structure.

## Usage

``` r
read_process_spec(spec)
```

## Arguments

- spec:

  Path to a `.yml`/`.yaml`/`.json` file, or a list as returned by
  [`yaml::read_yaml()`](https://yaml.r-lib.org/reference/read_yaml.html).

## Value

The normalised spec list (invisibly validated; see
[`process_spec_issues()`](https://energyRt.org/reference/process_spec_issues.md)
for the report).

## See also

Other process spec:
[`process_from_spec()`](https://energyRt.org/reference/process_from_spec.md),
[`process_spec_code()`](https://energyRt.org/reference/process_spec_code.md),
[`process_spec_issues()`](https://energyRt.org/reference/process_spec_issues.md),
[`process_to_spec()`](https://energyRt.org/reference/process_to_spec.md)

## Examples

``` r
spec <- process_to_spec(topia$modules$electricity$R3$ECOA)

# a spec already in memory is returned as-is, so the same call accepts
# either a list or the path to a YAML file written by `process_to_spec()`
f <- tempfile(fileext = ".yml")
process_to_spec(topia$modules$electricity$R3$ECOA, file = f)
identical(read_process_spec(f)$name, spec$name)
#> [1] TRUE
```
