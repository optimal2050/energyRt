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

set_glpk_path(path = NULL)

get_glpk_path()

set_julia_path(path = NULL)

get_julia_path()

set_python_path(path = NULL)

get_python_path()
```

## Arguments

- path:

  character path to the python installation. If NULL, the global
  operation path is used.

## Value

Sets path to GAMS library in R-options

The current path to GAMS library, set in R-options

Sets path to GDX library in R-options

The current path to GDX library, set in R-options

sets the path to the GLPK library in R options and returns NULL.

returns the path to the GLPK library.

Sets the path to Julia installation in the energyRt environment options
and returns NULL.

character. Path to Julia installation.

Writes or reads the path to python installation or environment to/from
`energyRt` options.

## Details

By default energyRt auto-detects the `glpsol` executable on the session
`PATH` (via
[`base::Sys.which()`](https://rdrr.io/r/base/Sys.which.html)). On
Windows this picks up the copy **bundled with Rtools** (Rtools ships the
GLPK dev kit), so an Rtools user needs no separate GLPK install. Use
`set_glpk_path()` only to point at a standalone GLPK installation, which
then takes precedence over auto-detection.

## Examples

``` r
# set_gams_path("C:/GAMS/win64/32.2/")

# get_gams_path()
# set_gdxlib("C:/GAMS/35")
# get_gdxlib()
if (FALSE) { # \dontrun{
set_glpk_path("/usr/local/bin/glpk") # Linux & Mac
set_glpk_path("C:/Program Files/glpk/bin") # Windows
get_glpk_path()
} # }
if (FALSE) { # \dontrun{
set_julia_path("C:/Program Files/Julia-1.10.1/bin/")
get_julia_path()
} # }
if (FALSE) { # \dontrun{
set_python_path("C:/Python3")
set_python_path()
get_python_path()
} # }
```
