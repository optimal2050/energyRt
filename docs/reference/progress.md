# Switch on/off and select/customize progress bar

Switch on/off and select/customize progress bar

## Usage

``` r
set_progress_bar(type = "bw", show = TRUE, clear = FALSE)

show_progress_bar(show = TRUE)
```

## Arguments

- type:

  character, type of the progress bar to display. Existing options:
  "bw", "default", "cli", "progress".

- show:

  logical, the progress bar is visible if `TRUE`.

- clear:

  logical, sets `progressr.clear` global option. If `TRUE`, all outout
  from the progress bar will be cleared.

## Value

sets the progress bar and returns `NULL`

## Examples

``` r
if (FALSE) { # \dontrun{
set_progress_bar("bw")
set_progress_bar("default")
set_progress_bar("cli")
set_progress_bar("progress")
set_progress_bar("pbcol")
} # }
if (FALSE) { # \dontrun{
show_progress_bar()
show_progress_bar(FALSE)
} # }
```
