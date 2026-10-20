# Installation and Settings

energyRt formulates an optimization model once and hands it to one of
several mathematical-programming **backends**. One backend is sufficient
to start; others can be added later and results cross-checked between
them.

| Backend | Software to install | License | Notes |
|----|----|----|----|
| **GLPK** | `glpsol` executable | open-source | Easiest to install; slow on very large models. |
| **Julia / JuMP** | Julia + `JuMP`, `HiGHS` | open-source | Fast (HiGHS barrier); recommended for large models. |
| **Python / Pyomo** | Python + `pyomo` + `highspy` | open-source | Fast (HiGHS barrier), low start-up overhead. Needs a **current** HiGHS — see below. |
| **GAMS** | GAMS distribution | proprietary | Needs a license; also enables GDX I/O. |

The package ships helpers to **detect** installed runtimes and
**auto-install the library layer** (R/Julia/Python packages). The
*system software* itself (Julia, Python, conda, GAMS) is installed
manually, with guidance below.

## 1. Install R and energyRt

Install R (≥ 4.3) — platform installers:
[Windows](https://cran.r-project.org/bin/windows/base/),
[macOS](https://cran.r-project.org/bin/macosx/),
[Ubuntu/Debian](https://cran.r-project.org/bin/linux/ubuntu/).
[RStudio](https://posit.co/download/rstudio-desktop/) is the recommended
IDE.

The quickest install is one line — it sets up `pak`, reports any Linux
system libraries to install, installs all dependencies, then energyRt
itself:

``` r

source("https://raw.githubusercontent.com/optimal2050/energyRt/master/inst/install.R")
install_energyRt()
```

To track the development version instead, pass a branch —
`install_energyRt()` otherwise follows the repository’s default branch:

``` r

install_energyRt(branch = "dev")
```

The setup is then verified in a fresh session with
[`library(energyRt); en_setup()`](https://energyRt.org).

For a direct install, energyRt’s CRAN dependencies are pulled in
automatically by `pak` (no local sibling packages are required):

``` r

install.packages("pak")
pak::pkg_install("optimal2050/energyRt")

# or, with remotes:
# install.packages("remotes")
# remotes::install_github("optimal2050/energyRt")
```

Installing energyRt does not install a solver: at least one backend
(below) is required. The GAMS GDX bridge (`gdxtools`) is pulled later by
[`en_install_deps()`](https://energyRt.org/reference/en_install_deps.md)
when the GAMS backend is used.

**Install failure on Linux.** If the install stops with
`dependencies '...' are not available for package 'energyRt'`, one CRAN
import (often one that needs a system library) failed and aborted the
rest. Install the imports one by one so the failure is isolated and
reported:

``` r

install.packages("pak")

# energyRt's direct CRAN imports (base packages are omitted):
deps <- c(
  "generics", "data.table", "DBI", "RSQLite", "tibble", "tidyr", "dplyr",
  "rlang", "stringr", "lubridate", "purrr", "arrow", "progressr", "tictoc",
  "cli", "zoo", "registry", "options", "glue", "plyr",
  # suggested — plots and reports (optional but recommended, see section 7):
  "ggplot2", "patchwork", "knitr", "rmarkdown", "tinytex", "sf"
)

failed <- character(0)
for (pkg in deps) {
  ok <- tryCatch(
    {
      pak::pkg_install(pkg, ask = FALSE)
      message("  [ok]   ", pkg)
      TRUE
    },
    error = function(e) {
      message("  [FAIL] ", pkg, ": ", conditionMessage(e))
      FALSE
    }
  )
  if (!ok) failed <- c(failed, pkg)
}

if (length(failed) == 0) {
  message("All dependencies installed — installing energyRt itself:")
  pak::pkg_install("optimal2050/energyRt")
} else {
  message("Failed packages: ", paste(failed, collapse = ", "))
}
```

On Linux, a package that fails to compile is missing a system library.
`pak` lists the apt packages required; once they are installed, the loop
is re-run:

``` r

pak::pkg_sysreqs("optimal2050/energyRt")   # lists the apt packages
# e.g. sudo apt install libcurl4-openssl-dev libssl-dev libxml2-dev
```

## 2. Check the installed runtimes

``` r

library(energyRt)
en_check_dependencies()   # solver backends: GLPK / Julia / Python / GAMS / GDX
en_check_packages()       # R packages, optional extras, LaTeX engine, MuseScore
```

[`en_check_dependencies()`](https://energyRt.org/reference/en_check.md)
prints a status table — for each backend: installed?, version, path, and
a hint for the next step — and states whether at least one solver is
ready. Each backend also has its own detector, e.g.
[`en_check_julia()`](https://energyRt.org/reference/en_check.md),
[`en_check_python()`](https://energyRt.org/reference/en_check.md),
[`en_check_pyomo()`](https://energyRt.org/reference/en_check.md),
[`en_check_glpk()`](https://energyRt.org/reference/en_check.md),
[`en_check_gams()`](https://energyRt.org/reference/en_check.md).
[`en_check_packages()`](https://energyRt.org/reference/en_check_packages.md)
complements it by checking energyRt’s R dependencies, optional packages
(plots/reports), and external tools (a LaTeX engine, MuseScore).
[`en_setup()`](https://energyRt.org/reference/en_setup.md) runs both.

## 3. Install the system software (one or more backends)

At least one is required. These are the only manual steps; commands are
grouped by operating system.

- **GLPK** — the `glpsol` executable. The simplest route on every
  platform is the [glpsol](https://cran.r-project.org/package=glpsol) R
  package, which bundles the GLPK binary: `install.packages("glpsol")`.
  energyRt uses it first whenever it is installed, so a first model
  needs no system solver. A system GLPK also works:
  - *Windows*: `glpsol.exe` ships with **Rtools 4.5** at
    `C:/rtools45/x86_64-w64-mingw32.static.posix/bin`.
  - *macOS*: `brew install glpk`.
  - *Ubuntu/Debian*: `sudo apt install glpk-utils`.
  - If a system `glpsol` is not detected, set its path —
    `set_glpk_path("C:/rtools45/x86_64-w64-mingw32.static.posix/bin")`
    (macOS/Ubuntu package managers put it on the `PATH` automatically).
- **Julia** — install via
  [juliaup](https://github.com/JuliaLang/juliaup).
  - *Windows*: `winget install julia -s msstore` (or the official
    installer).
  - *macOS*: `brew install juliaup` (or
    `curl -fsSL https://install.julialang.org | sh`).
  - *Ubuntu*: `curl -fsSL https://install.julialang.org | sh`.
  - Then `set_julia_path("~/.juliaup/bin/")` if Julia is not already on
    the `PATH`.
- **Python** — install
  [Miniforge](https://github.com/conda-forge/miniforge) (recommended;
  provides `conda`).
  - *Windows*: run the `Miniforge3` installer; avoid the Microsoft Store
    “App execution alias” python —
    [`en_check_python()`](https://energyRt.org/reference/en_check.md)
    flags it as non-functional.
  - *macOS*: `brew install miniforge` (or the `arm64`/`x86_64`
    installer).
  - *Ubuntu*: `bash Miniforge3-Linux-x86_64.sh` from the
    [releases](https://github.com/conda-forge/miniforge/releases/latest).
- **GAMS** — install from <https://www.gams.com/download/> (license
  required), then point energyRt at the GAMS system directory:
  - *Windows*: `set_gams_path("C:/GAMS/<version>/")`.
  - *macOS*:
    `set_gams_path("/Library/Frameworks/GAMS.framework/Versions/Current/Resources")`.
  - *Ubuntu*:
    `set_gams_path("/opt/gams/gams<version>_linux_x64_64_sfx")`.

## 4. Auto-install the library layer

Once the runtimes are present, install the packages each backend needs
in one call:

``` r

en_install_deps()   # detects runtimes, then installs Julia + Python + R libs
```

[`en_install_deps()`](https://energyRt.org/reference/en_install_deps.md)
runs a dependency check, installs only the safe library layer for the
runtimes it finds (skipping — with a warning — any runtime that is
missing), and re-checks at the end. Each backend can also be installed
separately:

``` r

en_install_julia_pkgs()      # JuMP, HiGHS, Cbc, Clp, ... into the Julia env
en_install_python_deps()     # conda env "energyRt" with pyomo + cbc (or pip)
en_install_python_deps(use_conda = FALSE)  # pip into the configured python
```

### Manual alternative

If a helper fails, the same steps can be run by hand.

R layer — the GAMS GDX bridge (not on CRAN) plus optional I/O helpers:

``` r

pak::pkg_install("lolow/gdxtools")   # or remotes::install_github("lolow/gdxtools")
install.packages(c("jsonlite", "readxl", "openxlsx"))

# geoscale features -- topia$geoscales, plot_geoscale(), multi-level regions.
# A sibling optimal2050 package, not on CRAN:
pak::pkg_install("optimal2050/geoscales")
```

(`timescales`, the other sibling, is a *build-time* dependency only: the
shipped `calendars` are generated from its catalog by
`data-raw/calendars.R`. Nothing at run time needs it, so there is no
reason to install it to use energyRt.)

Julia layer — from the Julia REPL (or `julia -e "..."`):

``` julia
using Pkg
Pkg.add(["JuMP", "HiGHS", "Cbc", "Clp", "RData", "RCall",
         "CodecBzip2", "Gadfly", "DataFrames", "CSV", "SQLite", "Dates"])
```

Python/Pyomo layer — conda (recommended) or pip:

``` bash
conda create -y -n energyRt python
conda install -y -n energyRt -c conda-forge pyomo pandas pyarrow highspy coincbc glpk
```

`glpk` and `coincbc` are what `solver_options$pyomo_glpk` and
`$pyomo_cbc` reach for: Pyomo finds them as **executables**, so leaving
either out makes that preset report itself unavailable even though Pyomo
itself is installed. `highspy` is different — it ships HiGHS as a
library (see below).

`pip` works just as well for this backend, because `highspy` ships HiGHS
itself as a wheel (unlike CBC, which pip cannot supply):

``` bash
conda create -y -n energyRt python=3.12 pip
conda run -n energyRt python -m pip install --upgrade pyomo highspy pandas pyarrow
```

### HiGHS version matters

`highspy` is the **only** way Pyomo reaches HiGHS:
`SolverFactory("highs")` resolves to a shell solver that looks for a
`highs` executable and reports itself unavailable. A current `highspy`
is required, and the installed version should be verified:

``` bash
python -c "import highspy; h=highspy.Highs(); print(h.versionMajor(), h.versionMinor(), h.versionPatch())"
```

On **HiGHS 1.7.2** the interior-point option — `solver = "ipm"`, which
`solver_options$pyomo_highs_barrier` sets — **hangs before the solver
starts**: no HiGHS banner, no CPU use, and a `time_limit` cannot
interrupt it, because HiGHS is never reached. The same models solve
normally on **HiGHS 1.13+**. Where an old `highspy` cannot be replaced,
`solver_options$pyomo_highs` (simplex defaults) works on 1.7.2, at
several times the barrier solve time on a large LP.

## 5. Point energyRt at the installations

If a runtime is not on the `PATH`, its path is set explicitly. For
Python/Pyomo, the path is the conda environment created above:

``` r

set_julia_path("~/.juliaup/bin/")             # same on all platforms
set_python_path("~/.conda/envs/energyRt")     # or the environment created above
set_glpk_path("C:/rtools45/x86_64-w64-mingw32.static.posix/bin")  # Windows (Rtools 4.5); unneeded on macOS/Ubuntu
set_gams_path("C:/GAMS/<version>/")           # see step 3 for macOS/Ubuntu paths
```

These are equivalent to `set_solver_path("julia", ...)`,
`set_solver_path("gams", ...)` and so on; either form is accepted.

### Persisting the settings

The calls above last for the current session. Saved once, they apply to
every later session:

``` r

en_config_write()        # -> ~/.energyRt/config.yml
```

[`en_config_write()`](https://energyRt.org/reference/en_config.md) with
no arguments writes every option that differs from the package default.
`en_config_write(global = FALSE)` writes `.energyRt.yml` in the current
project instead, for a project that needs a different GAMS version from
the rest.

To show what is in effect — and, when a solver is not found, where the
wrong value came from:

``` r

en_config_show()
```

Each option reports its source: an R option
(`options(en.gams_path = )`), an environment variable
(`ENERGYRT_GAMS_PATH`), a config file, or the package default — checked
in that order. `?energyRt-options` lists them all.

## 6. Verify end-to-end

A re-check (every installed backend should now report ready), a default
solver, and a small model solve complete the setup:

``` r

en_check_dependencies(solver_pkgs = TRUE)   # also verifies JuMP/HiGHS in Julia
set_default_solver(solver_options$julia_highs_barrier)  # or solver_options$glpk

# a minimal model solve confirms the full toolchain works
# (see the "Model bricks" article for assembling a model)
```

If two or more backends are installed, solving the same scenario with
each and comparing objectives is the strongest check of a correct setup.

## 7. Optional extras

Everything below is optional — energyRt solves models without any of it.

``` r

# plotting (all autoplot()/draw() charts) + levcost side-by-side panels
install.packages(c("ggplot2", "patchwork"))

# report(): HTML datasheets & model/scenario reports need pandoc.
# RStudio ships pandoc; outside RStudio install it from pandoc.org
# (or reuse Quarto's: Sys.setenv(RSTUDIO_PANDOC = "<quarto>/bin/tools")).
install.packages("rmarkdown")

# report(format = "pdf" / "tex") additionally needs a LaTeX toolchain:
install.packages("tinytex")
tinytex::install_tinytex()

# maps for plot_trade_map() / the topia$geoscales layouts
install.packages("sf")

# GAMS gdx exchange
# install.packages("gdxtools", repos = "...")   # see the gdxtools README
```
