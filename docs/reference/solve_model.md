# Solve a model or scenario (legacy names; new pipeline)

`solve_model()` is the "do everything" entry point: it interpolates a
model via
[`interpolate_model()`](https://energyRt.org/reference/interpolate_model.md)
and solves it (or, given an un-interpolated scenario, interpolates it
first), then routes to
[`solve_mod()`](https://energyRt.org/reference/solve_mod.md) /
[`solve_scen()`](https://energyRt.org/reference/solve_mod.md).
`solve_scenario()` **expects an already-interpolated scenario** and only
solves it via
[`solve_scen()`](https://energyRt.org/reference/solve_mod.md); it does
**not** re-interpolate (an un-interpolated scenario is an error pointing
to `solve_model()` /
[`interpolate_model()`](https://energyRt.org/reference/interpolate_model.md)).

## Usage

``` r
solve_model(obj, ...)

solve_scenario(obj, ...)
```

## Arguments

- obj:

  a `model` or `scenario`.

- ...:

  passed to [`solve_mod()`](https://energyRt.org/reference/solve_mod.md)
  / [`solve_scen()`](https://energyRt.org/reference/solve_mod.md).

## See also

[`solve_mod()`](https://energyRt.org/reference/solve_mod.md),
[`solve_scen()`](https://energyRt.org/reference/solve_mod.md)
