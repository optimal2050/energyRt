# Set GAMS and GDX library directory

This (optional) function sets path to GAMS directory to R-options. It
might be useful if for the cases when several different version (and
licenses) of GAMS installed, to easily switch between them. It is also
possible to set different path for GAMS and GAMS Data Exchange (GDX)
libraries. If GDX path is not set, the GAMS path will be used. If GAMS
path is not set, the default system GAMS-path (OS environment variables)
instead.

## Usage

``` r
set_gams_path(path = NULL)

get_gams_path()

set_gdxlib_path(path = NULL)

get_gdxlib_path()
```

## Arguments

- path:

  character, path to installed GAMS distribution to use to solve models
  and/or with GDX library to use in reading and writing gdx-files.

## Value

Sets path to GAMS library in R-options

The current path to GAMS library, set in R-options

Sets path to GDX library in R-options

The current path to GDX library, set in R-options

## Examples

``` r
# set_gams_path("C:/GAMS/win64/32.2/")

# get_gams_path()
# set_gdxlib("C:/GAMS/35")
# get_gdxlib()
```
