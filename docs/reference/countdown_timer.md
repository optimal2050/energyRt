# Countdown timer to use in R scripts

Countdown timer to use in R scripts

## Usage

``` r
countdown_timer(
  seconds,
  warn_message = NULL,
  start_message = "Press Esc (Ctrl+C in terminal) to interrupt the execution.\n",
  count_message = "\rTime remaining: %2d seconds",
  final_message = "-> resuming...\n"
)
```

## Arguments

- seconds:

  numeric, time in seconds to count down

- warn_message:

  character, warning message to display before the countdown

- start_message:

  character, message to display at the beginning of the countdown

- count_message:

  character, message to display during the countdown

- final_message:

  character, message to display at the end of the countdown

## Examples

``` r
if (FALSE) { # \dontrun{
countdown_timer(10)
countdown_timer(10, warn_message = "Something important is going to happen in 10 seconds.")
} # }
```
