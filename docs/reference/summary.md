# Summarize a model or scenario

Prints a concise summary of a
[model](https://energyRt.org/reference/class-model.md) or
[scenario](https://energyRt.org/reference/class-scenario.md) object:
name, description, contents and — for a solved scenario — the solution
status and size.

## Usage

``` r
# S4 method for class 'model'
summary(object, ...)

# S4 method for class 'scenario'
summary(object, ...)
```

## Arguments

- object:

  a `model` or `scenario` object.

- ...:

  additional arguments (currently unused).

## Value

The `object`, invisibly; called for its printed summary.
