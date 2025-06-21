## energyRt <a href="https://energyrt.org/articles/logo.html"><img src="man/figures/logo.png" align="right" height="120" alt="Logo-search" /></a>

**energyRt** (*energy* system modeling *R-t*oolbox /ˈɛnərdʒi ɑrt/) is a
set of classes, methods, and functions that define a macro-language for
energy system modeling within the R environment. This package offers a
high-level, user-friendly interface that simplifies the development and
analysis of complex energy models. By abstracting much of the underlying
complexity, **energyRt** allows users to concentrate on strategic and
analytical aspects rather than the technical details of coding.

**Key Features:**

-   **User-Friendly Interface: energyRt** enables users to define energy
    systems, input data, and configure scenarios using intuitive,
    domain-specific commands. It is designed to be accessible for both
    experienced modelers and those new to the field.
-   **Seamless R Integration:** The package integrates seamlessly with
    R’s extensive ecosystem of packages, allowing users to utilize
    powerful data handling and visualization tools within their energy
    modeling projects.
-   The **energyRt** optimization
    [model](https://energyrt.github.io/book/model.html) is implemented
    in four widely-used mathematical programming languages, both
    proprietary and open-source: [GAMS](http://www.gams.com/),
    [GLPK/Mathprog](https://www.gnu.org/software/glpk/),
    [Python/Pyomo](http://www.pyomo.org/),
    [Julia/JuMP](http://www.juliaopt.org/JuMP.jl/stable/). The package
    is designed to work seamlessly with any of these versions, allowing
    users to solve models using their preferred software while ensuring
    consistent and equivalent results across all platforms.
-   **Modular Model Construction: energyRt** supports the construction
    of models in a modular fashion, enabling incremental development,
    individual component testing, and code reuse across different
    projects. This modularity, combined with R’s interactive
    environment, promotes an iterative approach to modeling where
    assumptions can be tested, and results explored in real-time.
-   **Applications: energyRt** is designed to facilitate the creation of
    sophisticated energy system models, offering both flexibility and
    depth for detailed analysis. It is an essential tool for
    researchers, policymakers, and industry professionals engaged in
    long-term energy system planning, energy transition, and
    decarbonization efforts.

The package website: <https://energyrt.org>\
Documentation in progress: <https://energyrt.github.io/book/>

### Development status

**energyRt** is currently in preparation for its first release and
publication on [CRAN](https://cran.r-project.org/). The major milestone
for the package is the version **v0.50** (*"half-way-there"*), a proof
of concept with a full-featured and efficient model written in four
math-prog languages, with R-interface for the model design, processing
results, and producing reports. This version will have frozen model
code, classes and methods. Any updates will address only potential fixes
and new features with minimal impact on already existing modeling
projects.

Further development, versions starting from **v0.9** towards the
**v1.0** will have fully reviewed model and classes with the goal to
further increase efficiency, reduce memory footprint and computational
burden for both the model and its R interface, and significantly extend
features.

## Installation

Assuming that R is already installed (if not, please download and
install from <https://www.r-project.org/>), we also recommend RStudio
(<https://www.rstudio.com/>), a powerful IDE (Integrated Development
Environment) for R. The installation of the package is done via the
`pak` or `remotes` packages:

`pak::pkg_install("optimal2050/energyRt@v0.50")`\
or\
`remotes::install_github("optimal2050/energyRt", ref = "v0.50")`

The next step would be to install at least one of the solvers: GAMS,
GLPK, Python/Pyomo, Julia/JuMP. Please refer to the respective websites
for installation instructions. More details available on the
[IDEEA](https://ideea-model.github.io/IDEEA/articles/install.html) model
website, a project based on the **energyRt** package.
