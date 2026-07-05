# Create new model object

Create new model object

## Usage

``` r
newModel(name = "", desc = "", ...)

# S4 method for class 'model'
setHorizon(obj, ...)

# S4 method for class 'model'
getHorizon(obj)
```

## Arguments

- name:

  name of the model

- ...:

  configuration parameters (see class config) and model elements
  (classes commodity, technology, etc.)

## Value

model object containing model elements (`@data`) and configuration
(`@config`)

## Examples

``` r
if (FALSE) { # \dontrun{
mod <- newModel(
  name = "MyModel",
  desc = "My first model",
  data = model_repository,
  discount = 0.05,
  horizon = newHorizon(period = 2020:2050,
                       intervals = rep(5, 10)),
  calendar = calendars$d365h24
  )
} # }
```
