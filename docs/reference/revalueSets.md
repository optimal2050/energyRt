# Replace specified values with new values in factor or character columns of a data.frame.

Replace specified values with new values in factor or character columns
of a data.frame.

## Usage

``` r
revalueSets(x, newValues = NULL)
```

## Arguments

- x:

  vector

- newValues:

  a names list with named vectors. The names of the list should be equal
  to the names of the data.frame columns in wich values will be
  replaced. The named vector should have new names as values and old
  values as names.

## Value

the x data.frame with revalued variables.

## Examples

``` r
if (FALSE) { # \dontrun{
x <- data.frame(a = letters, n = 1:length(letters))
nw1 <- LETTERS[1:10]
names(nw1) <- letters[1:10]
nw2 <- formatC(1:9, width = 3, flag = "0")
names(nw2) <- 1:9
newValues <- list(a = nw1, n = nw2)
newValues
revalueSets(x, newValues)
} # }
```
