# Read and validate a techspec

Parses a techspec file (or an already-parsed list) into the normalised
list form used by
[`tech_from_spec()`](https://energyRt.org/reference/tech_from_spec.md)
and the process designer. YAML (`.yml`/`.yaml`) and JSON (`.json`) are
alternative containers for the same structure.

## Usage

``` r
read_techspec(spec)
```

## Arguments

- spec:

  Path to a `.yml`/`.yaml`/`.json` file, or a list as returned by
  [`yaml::read_yaml()`](https://yaml.r-lib.org/reference/read_yaml.html).

## Value

The normalised spec list (invisibly validated; see
[`tech_spec_issues()`](https://energyRt.org/reference/tech_spec_issues.md)
for the report).
