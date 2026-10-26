# Check if an element of a set is "ANY\*" or NA

Check if an element of a set is "ANY\*" or NA

## Usage

``` r
.udf(x)
```

## Arguments

- x:

  character, vector of a set elements

- na:

  logical, if TRUE, NA values are included

- any_mask:

  character, regular expression to match "ANY\*" elements

## Value

logical vector, TRUE if an element of the set is "ANY\*"

## Examples

``` r
energyRt:::is_any(c("ANY", "ANYREGION", "ANYTIMESLICE", "ANYYEAR", "A", "B"))
#> [1]  TRUE  TRUE  TRUE  TRUE FALSE FALSE
```
