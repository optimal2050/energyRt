# Create a DC transmission link

A `trade` object for a controllable DC link – the behaviour energyRt's
`trade` already had, named. **\[experimental\]**

## Usage

``` r
newDCLink(
  name,
  commodity,
  from,
  to,
  resistance = NA_real_,
  trade = data.frame(),
  bidirectional = TRUE,
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

- resistance:

  numeric, series resistance `r`. Recorded but not used for losses –
  losses are the fixed fraction `teff`.

- trade:

  data.frame of further per-route parameters (`ava.*`, `af.*`, `teff`),
  applied to both directions unless it names `src` / `dst`.

- bidirectional:

  logical, `TRUE` (default) declares both directions and `FALSE` only
  `from` -\> `to`. Note both directions still share ONE capacity.

- ...:

  further arguments passed to
  [`newTrade()`](https://energyRt.org/reference/newTrade.md).

## Value

Object of class `trade`.

## Details

A DC link's flow is a control decision, which is what a transport model
represents natively, so this constructor adds no physics: it exists so
that a network reads as what it is, with AC lines built by
[`newACLine()`](https://energyRt.org/reference/newACLine.md) and DC
links by this.

A DC link carries NO reactance and is therefore never part of the KVL
cycle basis, however the model is interpolated. `resistance` is recorded
for round-tripping only; losses are the fixed fraction `teff`.

## See also

[`newACLine()`](https://energyRt.org/reference/newACLine.md),
[`newTrade()`](https://energyRt.org/reference/newTrade.md)

Other class trade:
[`lossTranches()`](https://energyRt.org/reference/lossTranches.md),
[`newACLine()`](https://energyRt.org/reference/newACLine.md)

## Examples

``` r
dc <- newDCLink("K_W1_C1", commodity = "ELC", from = "W1", to = "C1",
                resistance = 0.001)
dc@routes
#>   src dst
#> 1  W1  C1
#> 2  C1  W1

# a one-way link declares a single route
newDCLink("K_ONE_WAY", commodity = "ELC", from = "W1", to = "C1",
          bidirectional = FALSE)@routes
#>   src dst
#> 1  W1  C1
```
