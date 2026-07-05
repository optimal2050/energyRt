# Check validity of object's names used in sets

Check validity of object's names used in sets

## Usage

``` r
check_name(x)
```

## Arguments

- x:

  character, name of an object of `energyRt`

## Value

logical, TRUE if the name is valid.

## Examples

``` r
check_name("name")
#> [1] TRUE
check_name("1name")
#> [1] FALSE
check_name("name1")
#> [1] TRUE
check_name("name_1")
#> [1] TRUE
check_name("name_1!")
#> [1] FALSE
```
