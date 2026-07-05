# Add an object to the model's repository

Add an object to the model's repository

## Usage

``` r
# S4 method for class 'repository'
add(obj, ..., overwrite = FALSE)

# S4 method for class 'model'
add(obj, ..., overwrite = FALSE, repo_name = NULL)
```

## Arguments

- obj:

  model object

- ...:

  model elements, allowed classes: ...

- overwrite:

  logical, if TRUE, objects with the same name will be overwritten,
  error will be reported if FALSE

- repo_name:

  character, optional name of a (sub-)repository to add the object.

## Value

model object with added elements to the repository
