# Create an AC transmission line

A `trade` object for a passive AC branch: a line whose flow is set by
impedance rather than chosen. **\[experimental\]**

## Usage

``` r
newACLine(
  name,
  commodity,
  from,
  to,
  reactance,
  resistance = NA_real_,
  trade = data.frame(),
  ...
)
```

## Arguments

- name:

  character, name of the object, used in sets.

- commodity:

  character, the transmitted commodity (normally electricity).

- from, to:

  character, the two endpoint regions of the line.

- reactance:

  numeric, series reactance `x`, strictly positive.

- resistance:

  numeric, series resistance `r`. Recorded but not used for losses –
  losses are the fixed fraction `teff`.

- trade:

  data.frame of further per-route parameters (`ava.*`, `af.*`, `teff`),
  applied to both directions unless it names `src` / `dst`.

- ...:

  further arguments passed to
  [`newTrade()`](https://energyRt.org/reference/newTrade.md).

## Value

Object of class `trade`.

## Details

The constructor differs from
[`newTrade()`](https://energyRt.org/reference/newTrade.md) in three
ways, all of which follow from an AC line being a physical two-way
branch rather than an arbitrary set of routes:

- it takes the two endpoints as `from` / `to` and builds BOTH directed
  rows, because Kirchhoff's voltage law constrains the net flow;

- `reactance` is required and is written to both rows, so it is
  symmetric by construction;

- every other per-route parameter passed through `trade` is applied to
  both directions unless it already names `src` / `dst`.

A finite `reactance` is what marks the route as subject to KVL. The
object is an ordinary `trade` in every other respect, and behaves
identically to one unless the model is interpolated with `kvl = TRUE` –
so adding reactances to a working model changes nothing until you ask
for the physics.

Where several physical circuits have been merged into one modelled line,
pass the EQUIVALENT reactance `1/x_eq = sum(1/x_i)`, not any one
circuit's.

## See also

[`newDCLink()`](https://energyRt.org/reference/newDCLink.md),
[`newTrade()`](https://energyRt.org/reference/newTrade.md)

Other class trade:
[`lossTranches()`](https://energyRt.org/reference/lossTranches.md),
[`newDCLink()`](https://energyRt.org/reference/newDCLink.md)

## Examples

``` r
ac <- newACLine("L_W1_W2", commodity = "ELC", from = "W1", to = "W2",
                reactance = 0.02, resistance = 0.001)

# an AC line is bidirectional: both directions are declared as routes
ac@routes
#>   src dst
#> 1  W1  W2
#> 2  W2  W1

# the electrical parameters travel in @misc, where the KVL constraints read them
ac@misc[c("reactance", "resistance")]
#> [[1]]
#> NULL
#> 
#> [[2]]
#> NULL
#> 
```
