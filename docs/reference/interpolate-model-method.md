# Interpolate model

Interpolate model

## Usage

``` r
# S4 method for class 'model'
interpolate(object, ...)
```

## Arguments

- object:

  model or scenario type of object.

## Value

scenario object with enclosed model (slot `@model`) and interpolated
parameters (slot `@modInp`).

## Examples

``` r
if (FALSE) { # \dontrun{
scen <- interpolate(mod)
} # }
```
