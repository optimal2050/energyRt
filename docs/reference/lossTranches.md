# Loss tranches for a transmission line

Derive a piecewise-linear approximation of a line's quadratic losses, as
a set of parallel capacity tranches with rising loss rates.
**\[experimental\]**

## Usage

``` r
lossTranches(
  shares,
  loss = NULL,
  resistance = NULL,
  capacity = NULL,
  teff = 1,
  labels = NULL,
  fix = NULL,
  desc = NULL
)
```

## Arguments

- shares:

  numeric, the fraction of the line's capacity each tranche occupies.
  Must be positive and sum to 1. A single share reproduces the ordinary
  flat-`teff` line exactly.

- loss:

  numeric, the loss fraction at rated load (dimensionless).

- resistance, capacity:

  numeric, an alternative to `loss`: their product is the loss fraction
  at rated load. Both or neither.

- teff:

  numeric, a base efficiency multiplied through every tranche, for
  losses that are not resistive (default 1).

- labels:

  character, tranche names. Default `T1`, `T2`, ...

- fix:

  numeric, a fixed line rating. When given, the result carries a
  `capacity` frame with `cap.fx = shares * fix`.

- desc:

  character, per-tranche descriptions.

## Value

An object of class `loss_tranches`: a list with `cluster`, `trade` and
(optionally) `capacity` data frames, ready to pass to
[`newACLine()`](https://energyRt.org/reference/newACLine.md),
[`newDCLink()`](https://energyRt.org/reference/newDCLink.md) or
[`newTrade()`](https://energyRt.org/reference/newTrade.md).

## Details

Returns the two (or three) data frames a `trade` object needs to
describe the tranches: a `cluster` declaration carrying the shares, a
`trade` frame carrying each tranche's `teff`, and – when `fix` is given
– a `capacity` frame splitting a fixed rating between them.

The tranches are declared as **fractions** of the line's capacity rather
than absolute quantities. That is what makes them invariant under
parallel-circuit expansion, so they need no recomputation when capacity
is optimised; see the loss-tranche note at the top of this file.

`loss` is the documented input: the dimensionless fraction of the sent
flow lost when the line runs at its rating. It is the quantity the
arithmetic actually needs, and the one that stays put under expansion. A
`resistance` may be given instead, but only together with the `capacity`
it was measured against, because `loss = resistance * capacity` – a
resistance alone does not determine a loss fraction. The two must be in
units whose product is dimensionless; for a per-unit resistance on base
`S_base`, pass `capacity = F / S_base`.

## See also

[`newACLine()`](https://energyRt.org/reference/newACLine.md),
[`newDCLink()`](https://energyRt.org/reference/newDCLink.md),
[`newTrade()`](https://energyRt.org/reference/newTrade.md)

Other class trade:
[`newACLine()`](https://energyRt.org/reference/newACLine.md),
[`newDCLink()`](https://energyRt.org/reference/newDCLink.md)

## Examples

``` r
# A line losing 3% at full load, in three tranches
lossTranches(shares = c(0.4, 0.35, 0.25), loss = 0.03)
#> Loss tranches: 3 segment(s), 3% lost at rated load
#>  cluster share from   to   loss   teff
#>       T1  0.40 0.00 0.40 0.0120 0.9880
#>       T2  0.35 0.40 0.75 0.0345 0.9655
#>       T3  0.25 0.75 1.00 0.0525 0.9475
#> check: sum(share x loss) = 0.03  (= loss at rated load, 0.03)
```
