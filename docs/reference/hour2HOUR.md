# Convert hours (integer) values to HOUR set 'hNN'

Convert hours (integer) values to HOUR set 'hNN'

## Usage

``` r
hour2HOUR(x, width = 2, prefix = "h", flag = "0")
```

## Arguments

- x:

  integer vector, hours (for example, 0-23 for daily data, 0-167 for
  weekly data, etc.)

- width:

  integer, width of the output string

- prefix:

  character, prefix to add to the name, default is 'h'

- flag:

  character, flag to add to the name, default is '0'

## Value

character vector of the same length as `x` with formatted hours to be
used in the HOUR set.

## Examples

``` r
hour2HOUR(0:23)
#>  [1] "h00" "h01" "h02" "h03" "h04" "h05" "h06" "h07" "h08" "h09" "h10" "h11"
#> [13] "h12" "h13" "h14" "h15" "h16" "h17" "h18" "h19" "h20" "h21" "h22" "h23"
```
