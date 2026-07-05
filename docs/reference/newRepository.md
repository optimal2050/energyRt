# A constructor for the repository class

Repository class is used to store the model 'bricks' such as commodity,
technology, supply, demand, trade, import, export, trade, storage, etc.
Calendars, settings, and configurations cannot be stored in the
repository, they have separate slots in model or scenario objects.

## Usage

``` r
newRepository(
  name = "base_repository",
  ...,
  desc = NA_character_,
  misc = list()
)
```

## Arguments

- name:

  character. Name of the repository.

- ...:

  list. Model objects ("bricks"), e.g., technologies, constraints,
  costs, etc., stored in with their names as keys, or gropped in named
  lists.

- desc:

  character. Description of the repository.

- misc:

  list. Any additional data or information to store in the object.

## See also

Other repository model data:
[`class-repository`](https://energyRt.org/reference/class-repository.md)
