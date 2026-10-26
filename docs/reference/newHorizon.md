# Create a new object of class 'horizon'

The function creates a new object of class 'horizon' that represents the
planning horizon of the model/scenario.

## Usage

``` r
newHorizon(
  period = NULL,
  intervals = NULL,
  mid_is_end = FALSE,
  mid_is_start = FALSE,
  force_BY_interval_to_1_year = TRUE,
  desc = NULL,
  name = NULL
)

# S4 method for class 'horizon'
update(object, ..., warn_nodata = TRUE)

# S4 method for class 'config'
setHorizon(obj, period, ...)

# S4 method for class 'config'
update(object, ..., warn_nodata = TRUE)
```

## Arguments

- period:

  (optional) integer vector with a range or a sequence of years to
  define the full period of the model/scenario. If not provided, the
  range of 'intervals' will be used.

- intervals:

  (optional) either data.frame or integer vector. The data.frame must
  have `start`, `mid`, and `end` columns with modeled interval. An
  optional `label` column gives each milestone a display name; it is
  filled from `mid` when absent, must be unique, and is presentation
  only – the model key for a period stays the integer `mid`. See
  [`year_label()`](https://energyRt.org/reference/year_label.md). The
  vector will be considered as lengths of each modeled interval in
  period.

- mid_is_end:

  logical, if TRUE, the mid-year will be set to the end of the interval.

- mid_is_start:

  logical, if TRUE, the mid-year will be set to the start of the
  interval.

- force_BY_interval_to_1_year:

  logical, if TRUE (default), the base-year (first) interval will be
  forced to one year.

- desc:

  character. Description of the horizon object, for own references.

- name:

  character. Name of the horizon object. Used to distinguish between
  different horizons in the model or scenario, including the automatic
  creation of the folder name for the model/scenario scripts.

- object:

  the `horizon` or `config` object to update.

- ...:

  named slots with new values.

- warn_nodata:

  logical; warn when the update is called with no data.

- obj:

  the `config` object, for the `setHorizon` method.

- horizon:

  a new horizon object to be set.

## Value

An object of class 'horizon'

## Examples

``` r
newHorizon(2020:2050)
#> An object of class "horizon"
#> Slot "name":
#> character(0)
#> 
#> Slot "desc":
#> character(0)
#> 
#> Slot "period":
#>  [1] 2020 2021 2022 2023 2024 2025 2026 2027 2028 2029 2030 2031 2032 2033 2034
#> [16] 2035 2036 2037 2038 2039 2040 2041 2042 2043 2044 2045 2046 2047 2048 2049
#> [31] 2050
#> 
#> Slot "intervals":
#>     start   mid   end  label
#>     <int> <int> <int> <char>
#>  1:  2020  2020  2020   2020
#>  2:  2021  2021  2021   2021
#>  3:  2022  2022  2022   2022
#>  4:  2023  2023  2023   2023
#>  5:  2024  2024  2024   2024
#>  6:  2025  2025  2025   2025
#>  7:  2026  2026  2026   2026
#>  8:  2027  2027  2027   2027
#>  9:  2028  2028  2028   2028
#> 10:  2029  2029  2029   2029
#> 11:  2030  2030  2030   2030
#> 12:  2031  2031  2031   2031
#> 13:  2032  2032  2032   2032
#> 14:  2033  2033  2033   2033
#> 15:  2034  2034  2034   2034
#> 16:  2035  2035  2035   2035
#> 17:  2036  2036  2036   2036
#> 18:  2037  2037  2037   2037
#> 19:  2038  2038  2038   2038
#> 20:  2039  2039  2039   2039
#> 21:  2040  2040  2040   2040
#> 22:  2041  2041  2041   2041
#> 23:  2042  2042  2042   2042
#> 24:  2043  2043  2043   2043
#> 25:  2044  2044  2044   2044
#> 26:  2045  2045  2045   2045
#> 27:  2046  2046  2046   2046
#> 28:  2047  2047  2047   2047
#> 29:  2048  2048  2048   2048
#> 30:  2049  2049  2049   2049
#> 31:  2050  2050  2050   2050
#>     start   mid   end  label
#>     <int> <int> <int> <char>
#> 
newHorizon(2020:2030, desc = "One-year intervals")
#> An object of class "horizon"
#> Slot "name":
#> character(0)
#> 
#> Slot "desc":
#> [1] "One-year intervals"
#> 
#> Slot "period":
#>  [1] 2020 2021 2022 2023 2024 2025 2026 2027 2028 2029 2030
#> 
#> Slot "intervals":
#>     start   mid   end  label
#>     <int> <int> <int> <char>
#>  1:  2020  2020  2020   2020
#>  2:  2021  2021  2021   2021
#>  3:  2022  2022  2022   2022
#>  4:  2023  2023  2023   2023
#>  5:  2024  2024  2024   2024
#>  6:  2025  2025  2025   2025
#>  7:  2026  2026  2026   2026
#>  8:  2027  2027  2027   2027
#>  9:  2028  2028  2028   2028
#> 10:  2029  2029  2029   2029
#> 11:  2030  2030  2030   2030
#> 
newHorizon(2020:2030, c(1, 2, 5, 10), desc = "Different length intervals")
#> An object of class "horizon"
#> Slot "name":
#> character(0)
#> 
#> Slot "desc":
#> [1] "Different length intervals"
#> 
#> Slot "period":
#>  [1] 2020 2021 2022 2023 2024 2025 2026 2027 2028 2029 2030
#> 
#> Slot "intervals":
#>    start   mid   end  label
#>    <num> <num> <num> <char>
#> 1:  2020  2020  2020   2020
#> 2:  2021  2021  2022   2021
#> 3:  2023  2025  2027   2025
#> 4:  2028  2028  2030   2028
#> 
newHorizon(2020:2035, c(1, 2, 5, 5, 5))
#> An object of class "horizon"
#> Slot "name":
#> character(0)
#> 
#> Slot "desc":
#> character(0)
#> 
#> Slot "period":
#>  [1] 2020 2021 2022 2023 2024 2025 2026 2027 2028 2029 2030 2031 2032 2033 2034
#> [16] 2035
#> 
#> Slot "intervals":
#>    start   mid   end  label
#>    <num> <num> <num> <char>
#> 1:  2020  2020  2020   2020
#> 2:  2021  2021  2022   2021
#> 3:  2023  2025  2027   2025
#> 4:  2028  2030  2032   2030
#> 5:  2033  2033  2035   2033
#> 
newHorizon(2020:2050, c(1, 2, 5, 7, 1))
#> An object of class "horizon"
#> Slot "name":
#> character(0)
#> 
#> Slot "desc":
#> character(0)
#> 
#> Slot "period":
#>  [1] 2020 2021 2022 2023 2024 2025 2026 2027 2028 2029 2030 2031 2032 2033 2034
#> [16] 2035
#> 
#> Slot "intervals":
#>    start   mid   end  label
#>    <num> <num> <num> <char>
#> 1:  2020  2020  2020   2020
#> 2:  2021  2021  2022   2021
#> 3:  2023  2025  2027   2025
#> 4:  2028  2031  2034   2031
#> 5:  2035  2035  2035   2035
#> 

newHorizon(intervals = data.frame(
  start = c(2030, 2031, 2034),
  mid =   c(2030, 2032, 2037),
  end =   c(2030, 2033, 2040)),
  desc = "Explicit assignment of intervals via data.frame"
  )
#> An object of class "horizon"
#> Slot "name":
#> character(0)
#> 
#> Slot "desc":
#> [1] "Explicit assignment of intervals via data.frame"
#> 
#> Slot "period":
#>  [1] 2030 2031 2032 2033 2034 2035 2036 2037 2038 2039 2040
#> 
#> Slot "intervals":
#>    start   mid   end  label
#>    <num> <num> <num> <char>
#> 1:  2030  2030  2030   2030
#> 2:  2031  2032  2033   2032
#> 3:  2034  2037  2040   2037
#> 

newHorizon(period = 2020:2050,
           intervals = data.frame(
             start = c(2030, 2031, 2034),
             mid =   c(2030, 2032, 2037),
             end =   c(2030, 2033, 2040)),
             desc = "The period will be trimmed to the scope of intervals")
#> An object of class "horizon"
#> Slot "name":
#> character(0)
#> 
#> Slot "desc":
#> [1] "The period will be trimmed to the scope of intervals"
#> 
#> Slot "period":
#>  [1] 2030 2031 2032 2033 2034 2035 2036 2037 2038 2039 2040
#> 
#> Slot "intervals":
#>    start   mid   end  label
#>    <num> <num> <num> <char>
#> 1:  2030  2030  2030   2030
#> 2:  2031  2032  2033   2032
#> 3:  2034  2037  2040   2037
#> 

newHorizon(2020:2050, c(3, 2, 5, 10),
           desc = "Pay attention to the length of the first interval")
#> An object of class "horizon"
#> Slot "name":
#> character(0)
#> 
#> Slot "desc":
#> [1] "Pay attention to the length of the first interval"
#> 
#> Slot "period":
#>  [1] 2020 2021 2022 2023 2024 2025 2026 2027 2028 2029 2030 2031 2032 2033 2034
#> [16] 2035 2036 2037 2038 2039
#> 
#> Slot "intervals":
#>    start   mid   end  label
#>    <num> <num> <num> <char>
#> 1:  2020  2020  2020   2020
#> 2:  2021  2021  2022   2021
#> 3:  2023  2023  2024   2023
#> 4:  2025  2027  2029   2027
#> 5:  2030  2034  2039   2034
#> 

# Explicit display labels; `mid` remains the model key
newHorizon(intervals = data.frame(
  start = c(2025, 2030),
  mid   = c(2025, 2030),
  end   = c(2029, 2034),
  label = c("FY2025-26", "FY2030-31")))
#> An object of class "horizon"
#> Slot "name":
#> character(0)
#> 
#> Slot "desc":
#> character(0)
#> 
#> Slot "period":
#>  [1] 2025 2026 2027 2028 2029 2030 2031 2032 2033 2034
#> 
#> Slot "intervals":
#>    start   mid   end     label
#>    <int> <int> <int>    <char>
#> 1:  2025  2025  2025 FY2025-26
#> 2:  2026  2026  2029      2026
#> 3:  2030  2030  2034 FY2030-31
#> 

newHorizon(period = 2020:2040,
           intervals = data.frame(
             start = c(2030, 2032, 2035),
             mid =   c(2031, 2033, 2037),
             end =   c(2032, 2034, 2040)))
#> An object of class "horizon"
#> Slot "name":
#> character(0)
#> 
#> Slot "desc":
#> character(0)
#> 
#> Slot "period":
#>  [1] 2030 2031 2032 2033 2034 2035 2036 2037 2038 2039 2040
#> 
#> Slot "intervals":
#>    start   mid   end  label
#>    <int> <int> <int> <char>
#> 1:  2030  2030  2030   2030
#> 2:  2031  2031  2032   2031
#> 3:  2032  2033  2034   2033
#> 4:  2035  2037  2040   2037
#> 
```
