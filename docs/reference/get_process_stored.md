# Stored commodities of each process

The commodity a process HOLDS, as distinct from what it consumes or
produces. Only `storage` has one. It is `@storage$comm` where given,
otherwise `@commodity` – so a storage that names no roles keeps today's
behaviour.

## Usage

``` r
get_process_stored(scen, process = NULL, classes = NULL)
```

## Arguments

- scen:

  scenario object

- process:

  optional character vector to restrict to

- classes:

  classes to search; defaults to `"storage"`

## Value

a named list mapping each process to its stored commodity

## Details

Needed because a storage may hold something that is neither its input
nor its output (hydrogen: ELC in, H2 stored, ELC out). Without this the
stored commodity never reaches
[`get_process_comm()`](https://energyRt.org/reference/get_process_comm.md),
and the storage's timeframe would be derived from its ELC flows alone –
putting the level at the wrong resolution.
