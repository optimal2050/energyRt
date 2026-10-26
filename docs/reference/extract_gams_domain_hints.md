# Extract domain mapping hints from GAMS file comments

Reads a GAMS file and extracts \*@ domain mapping comments that appear
before variable declarations. Does NOT modify the model structure.

## Usage

``` r
extract_gams_domain_hints(file_or_text)
```

## Arguments

- file_or_text:

  Path to GAMS file or text content

## Value

Named list mapping variable names to domain mapping comments
