# Export a process object as a spec list

The reverse of
[`process_from_spec()`](https://energyRt.org/reference/process_from_spec.md):
slot tables become row lists (all-`NA` columns and empty tables are
omitted; `NA` dimension values are omitted from rows, i.e. read back as
"applies to ALL").

## Usage

``` r
process_to_spec(object, file = NULL)
```

## Arguments

- object:

  A `technology`, `storage` or `trade` object.

- file:

  Optional path; when given, the spec is written as YAML, or as JSON
  when the path ends in `.json` (same structure, an alternative
  container).

## Value

The spec list (invisibly when `file` is given).

## See also

Other process spec:
[`process_from_spec()`](https://energyRt.org/reference/process_from_spec.md),
[`process_spec_code()`](https://energyRt.org/reference/process_spec_code.md),
[`process_spec_issues()`](https://energyRt.org/reference/process_spec_issues.md),
[`read_process_spec()`](https://energyRt.org/reference/read_process_spec.md)

## Examples

``` r
tech <- getObject(topia$modules$electricity$repo, name = "ECOA", drop = TRUE)

# the object as a plain nested list: diffable, editable, writable as YAML
spec <- process_to_spec(tech)
names(spec)
#>  [1] "techspec"           "class"              "name"              
#>  [4] "desc"               "units"              "cap2act"           
#>  [7] "optimizeRetirement" "inputs"             "outputs"           
#> [10] "efficiency"         "invcost"            "fixom"             
#> [13] "vintages"           "capacity"          
spec$efficiency
#> [[1]]
#> [[1]]$comm
#> [1] "COA"
#> 
#> [[1]]$cinp2use
#> [1] 0.4
#> 
#> 
#> [[2]]
#> [[2]]$comm
#> [1] "ELC"
#> 
#> [[2]]$use2cact
#> [1] 277.7778
#> 
#> 

# `process_from_spec()` is the way back
identical(process_from_spec(spec)@name, tech@name)
#> [1] TRUE
```
