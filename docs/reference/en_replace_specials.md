# Find and replace special characters

Find and replace special characters

## Usage

``` r
en_replace_specials(x, pattern = "[^[:alnum:]]", repl = "_")

en_has_specials(x, pattern = "[^[:alnum:]]")

en_find_specials(x, pattern = "[^[:alnum:]]")
```

## Arguments

- x:

  character vector

- pattern:

  regular expression pattern to match special characters

- repl:

  replacement character

## Value

character vector with special characters replaced

## Functions

- `en_has_specials()`: Return `TRUE` if any element contains a special
  character.

- `en_find_specials()`: Return the match positions of special characters
  (via [`gregexpr()`](https://rdrr.io/r/base/grep.html)).

## Examples

``` r
en_replace_specials(c("valid", "invalid!", "in-valid", "valid_1", "invalid.2"))
#> [1] "valid"     "invalid_"  "in_valid"  "valid_1"   "invalid_2"
en_replace_specials(c("valid", "invalid!"), "[\\.\\^\\$\\*\\+\\?\\!]", "_fixed")
#> [1] "valid"         "invalid_fixed"
```
