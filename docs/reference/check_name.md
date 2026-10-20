# Check validity of object's names used in sets

Check validity of object's names used in sets

## Usage

``` r
check_name(x, dot = FALSE)
```

## Arguments

- x:

  character, name of an object of `energyRt`

## Value

logical, TRUE if the name is valid.

## Examples

``` r
energyRt:::check_name("name")
#> [1] TRUE
energyRt:::check_name("1name")
#> [1] FALSE
energyRt:::check_name("name1")
#> [1] TRUE
energyRt:::check_name("name_1")
#> [1] TRUE
energyRt:::check_name("name_1!")
#> [1] FALSE
energyRt:::check_name(c("a", "b")) # FALSE, not a single name
#> [1] FALSE
energyRt:::check_name(1) # FALSE, not character
#> [1] FALSE
```
