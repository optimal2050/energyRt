# Rename data.frame columns of list of data.frames.

Rename data.frame columns of list of data.frames.

## Usage

``` r
renameSets(x, newNames = NULL)
```

## Arguments

- x:

  a data.frame or a list with data frames.

- newNames:

  named character vector or list with new names as values, and old names
  as names.

## Value

depending on input, the renamed data.frame or the list with renamed
data.frames.

## Examples

``` r
if (FALSE) { # \dontrun{
x <- data.frame(a = letters, n = 1:length(letters))
x
renameSets(x[1:3, ], c(a = "A", n = "N"))
renameSets(x[1:3, ], list(a = "B", n = "M"))
} # }
```
