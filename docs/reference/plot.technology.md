# Plot a schematic representation of a technology

Plot a schematic representation of a technology

## Usage

``` r
# S3 method for class 'technology'
plot(obj, ...)
```

## Arguments

- obj:

  A technology object

- ...:

  Additional arguments, currently not used

## Value

A plot of the technology

## Examples

``` r
TECH01 <- newTechnology(
  "TECH01",
  desc = "Technology Description",
  input = data.frame(
    comm = c("COM1", "COM2", "COM5", "COM7", "COM8", "COM9"),
    group = c("1", "1", NA, "2", "2", "2"),
    unit = c("unit1", "unit2", "unit5", "unit7", "unit8", "unit9")
  ),
  output = data.frame(
    comm = c("COM3", "COM4", "COM6"),
    group = c("3", NA, "3"),
    unit = c("unit3", "unit4", "unit6")
  ),
  group = data.frame(
    group = c("1", "2", "3"),
    desc = c("Group1", "Group2", "Group3"),
    unit = "unit"
  ),
  aux = data.frame(
    acomm = c("AUX1", "AUX2", "AUX3", "AUX4"),
    unit = c("unit1", "unit2", "unit3", "unit4")
  ),
  region = c("R1", "R2", "R3"),
  geff = data.frame(
    group = c("1", "2"),
    ginp2use = c(0.12, 0.789)
  ),
  ceff = data.frame(
    comm = c("COM1", "COM2", "COM5", "COM7", "COM8", "COM9", "COM3", "COM4", "COM6"),
    cinp2ginp = c(.1, .2, NA, .7, .8, .9, rep(NA, 3)),
    cinp2use = c(NA, NA, .5, NA, NA, NA, rep(NA, 3)),
    use2cact = c(rep(NA, 6), .36, .4, .36),
    cact2cout = c(rep(NA, 6), .3, NA, .6),
    share.lo = c(.01, .02, NA, .07, .08, .0, .03, NA, .06),
    share.up = c(.91, .92, NA, .97, .98, 1, .83, NA, .96)
  ),
  aeff = data.frame(
    acomm = c("AUX1", "AUX2", "AUX3", "AUX4"),
    comm = c(NA, "COM1", NA, "COM3"),
    act2ainp = c(1, NA, NA, NA),
    cinp2aout = c(NA, 2, NA, NA),
    cap2aout = c(NA, NA, 3, NA),
    cout2aout = c(NA, NA, NA, 4)
  ),
  weather = data.frame(
    weather = "WEATHER_CF1",
    waf.up = .99
  )
)
plot(TECH01)
```
