# Create timetable of time-slices from given structure as a list

Create timetable of time-slices from given structure as a list

## Usage

``` r
make_timetable(
  struct = list(ANNUAL = "ANNUAL"),
  year_fraction = 1,
  warn = FALSE
)
```

## Arguments

- struct:

  named list of timeframes with sets of timeslices and optional shares
  of every slice or frame in the nest

- warn:

  logical, if TRUE, warning will be issued if `ANNUAL` level does not
  exists in the given structure. The level will be auto-created to
  complete the time-structure.

## Value

an data.frame with the specified structure.

## Examples

``` r
make_timetable()
#>    ANNUAL  slice share weight
#>    <char> <char> <num>  <num>
#> 1: ANNUAL ANNUAL     1      1
make_timetable(list("SEASON" = c("WINTER", "SUMMER")))
#>    ANNUAL SEASON  slice share weight
#>    <char> <char> <char> <num>  <num>
#> 1: ANNUAL SUMMER SUMMER   0.5      1
#> 2: ANNUAL WINTER WINTER   0.5      1
make_timetable(list("SEASON" = c("WINTER" = .6, "SUMMER" = .4)))
#>    ANNUAL SEASON  slice share weight
#>    <char> <char> <char> <num>  <num>
#> 1: ANNUAL SUMMER SUMMER   0.4      1
#> 2: ANNUAL WINTER WINTER   0.6      1
make_timetable(list(
  "SEASON" = list(
    "WINTER" = list(.3, DAY = c("MORNING", "EVENING")),
    "SUMMER" = list(.7, DAY = c("MORNING", "EVENING"))
  )
))
#>    ANNUAL SEASON     DAY          slice share weight
#>    <char> <char>  <char>         <char> <num>  <num>
#> 1: ANNUAL SUMMER EVENING SUMMER_EVENING  0.35      1
#> 2: ANNUAL SUMMER MORNING SUMMER_MORNING  0.35      1
#> 3: ANNUAL WINTER EVENING WINTER_EVENING  0.15      1
#> 4: ANNUAL WINTER MORNING WINTER_MORNING  0.15      1

make_timetable(list(
  "SEASON" = list("WINTER" = .3, "SUMMER" = .7),
  "DAY" = c("MORNING", "EVENING")
))
#>    ANNUAL SEASON     DAY          slice share weight
#>    <char> <char>  <char>         <char> <num>  <num>
#> 1: ANNUAL SUMMER EVENING SUMMER_EVENING  0.35      1
#> 2: ANNUAL SUMMER MORNING SUMMER_MORNING  0.35      1
#> 3: ANNUAL WINTER EVENING WINTER_EVENING  0.15      1
#> 4: ANNUAL WINTER MORNING WINTER_MORNING  0.15      1
```
