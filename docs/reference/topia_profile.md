# Synthetic input shapes on a calendar (deterministic)

Generate a simple geometric profile – a step-wise staircase, a
sine/cosine wave, or a hexagonal trapezoid – over the timeslices of a
calendar, ready to use as weather (`wval`), availability multipliers, or
demand shapes. The synthetic counterpart of the realistic
[`topia_profiles()`](https://energyRt.org/reference/topia_profiles.md):
where those answer "what does a year look like", these answer "what is
the simplest input that isolates one mechanism" (storage bridging an
on/off resource, techs following a ramp, trade smoothing opposite
phases).

## Usage

``` r
topia_profile(
  shape = c("step", "sine", "cosine", "hex"),
  levels = 4L,
  calendar = "s4_h24",
  regions = NULL,
  vary = c("none", "phase", "amplitude"),
  min = 0,
  max = 1,
  period = c("year", "frame"),
  phase = 0
)
```

## Arguments

- shape:

  `"step"` (ascending staircase with `levels` plateaus: 0, 1/(levels-1),
  ..., 1), `"sine"`, `"cosine"`, or `"hex"` (trapezoid ramp/plateau/ramp
  in equal thirds).

- levels:

  integer \>= 2, number of plateaus for `shape = "step"` (ignored
  otherwise). `levels = 2` is the on/off case.

- calendar:

  a
  [calendar](https://optimal2050.github.io/timescales/r/reference/calendar_build.html)
  object, the name of a shipped calendar (see
  [calendars](https://energyRt.org/reference/calendars.md)), or a
  timetable `data.frame`.

- regions:

  optional character vector. `NULL` returns the bare shape (`timeslice`,
  `step`, `value`); otherwise the shape is replicated per region with a
  leading `region` column.

- vary:

  per-region variation (needs `regions`): `"none"` replicates
  identically; `"phase"` shifts region i by `(i-1)/n` of the period (a
  gradient across the map – neighbouring regions peak in sequence);
  `"amplitude"` scales region i's amplitude by `1 - (i-1)/(2(n-1))`
  (from 1 down to 1/2 – good sites to poor sites).

- min, max:

  numeric range the unit shape is scaled to
  (`value = min + shape * (max - min)`).

- period:

  `"year"` (one cycle over the calendar) or `"frame"` (one cycle inside
  each parent timeframe of the finest level).

- phase:

  numeric, fraction of the period the shape is shifted by (applied to
  all regions, on top of `vary = "phase"` offsets).

## Value

A tidy `data.frame`, chronologically ordered: `region` (only when
`regions` is given), `timeslice` (the calendar's finest level), `step`
(integer plateau index `0..levels-1` for `"step"`, `NA` otherwise – the
pretty-number handle for hand-checkable inputs), and `value` in
`[min, max]`.

## Details

Despite the shared prefix, this function reads no TOPIA data and is not
limited to TOPIA: it works on any calendar and returns a single
data.frame.
[`topia_profiles()`](https://energyRt.org/reference/topia_profiles.md)
(plural) is the other one – it expands TOPIA's saved weather / demand /
stock to regions and returns a list of three.

The shape spans `period` once: `"year"` stretches it over the whole
calendar; `"frame"` repeats it inside each parent timeframe (e.g. each
season's hours get the same diurnal shape). Slice positions are the
midpoints of each timeslice's `share` along the calendar's own
chronological chain, so unequal slices land where they belong; the
cycle's anchor is the alphabetically first timeslice (rotate with
`phase`).

## See also

[`topia_profiles()`](https://energyRt.org/reference/topia_profiles.md)
for the realistic TOPIA curves,
[topia](https://energyRt.org/reference/topia.md) for the reference
dataset and the teaching kits.

Other topia: [`topia`](https://energyRt.org/reference/topia.md),
[`topia_profiles()`](https://energyRt.org/reference/topia_profiles.md)

## Examples

``` r
# on/off resource on the 4-slice test calendar: free half the year
topia_profile("step", levels = 2, calendar = "s4")
#>   timeslice step value
#> 1       FAL    0     0
#> 2       WIN    0     0
#> 3       SPR    1     1
#> 4       SUM    1     1

# a diurnal sine for every season, phased across three regions
head(topia_profile("sine", calendar = "s4_h24",
                    regions = c("W1", "W2", "C1"), vary = "phase",
                    period = "frame"))
#>   region timeslice step     value
#> 1     W1   FAL_h00   NA 0.5652631
#> 2     W1   FAL_h01   NA 0.6913417
#> 3     W1   FAL_h02   NA 0.8043807
#> 4     W1   FAL_h03   NA 0.8966767
#> 5     W1   FAL_h04   NA 0.9619398
#> 6     W1   FAL_h05   NA 0.9957224
```
