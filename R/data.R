# =========================================================================== #
# Documentation for the shipped datasets that have no single owning module.
# The datasets themselves are built in data-raw/ (see each @source).
# =========================================================================== #

#' Catalogue of the model vocabulary
#'
#' Every set, parameter and variable the package can emit, with its dimensions
#' and the mapping that gates it. This is the reference the interpolation and
#' the writers are built against, and what
#' `tests/coverage/parameter-matrix.csv` is checked against.
#'
#' @format A data.frame of 613 rows and 5 columns:
#' \describe{
#'   \item{name}{character. The set, parameter or variable name.}
#'   \item{description}{character. One-line description.}
#'   \item{type}{character. One of `set` (279), `parameter` (241) or
#'     `variable` (93).}
#'   \item{dim}{character. Comma-separated dimensions, `""` for a plain set.}
#'   \item{map}{character. The mapping that restricts the domain, `""` if the
#'     full cross-product applies.}
#' }
#' @source Built by `data-raw/maps.R`; the `variable` rows' `dim` and `map` are
#'   re-attached by name in `data-raw/DATASET.R`.
#' @examples
#' table(model_structure$type)
#' head(model_structure[model_structure$type == "variable", ])
"model_structure"

#' Solver presets
#'
#' Ready-made solver configurations to pass to [solve_scenario()] as
#' `solver = solver_options[["<name>"]]`. The name encodes the route and the
#' algorithm: a bare backend (`glpk`), `pyomo_*` for the Python route, `mm_*`
#' for the multimod matrix route, and a `_barrier` suffix for the interior-point
#' method rather than simplex. The matrix presets name the matrix writer, the
#' solver and the place: `mm_cuopt_wsl` (cuOpt on a local GPU through WSL),
#' `mm_cuopt_cloud` (cuOpt on a rented GPU), `pyomo_cuopt_cloud` (a Pyomo-written
#' matrix on a rented GPU).
#'
#' These entries are data only -- selecting one does not check that the solver
#' is installed. The former names `mmcloud`, `mmcloud_crossover`,
#' `mmcloud_pyomo` and `mmcloud_pyomo_crossover` remain as entries that stop a
#' solve and name their replacement.
#'
#' @format A named list of solver settings objects.
#' @source `data-raw/solver_options.R`
#' @seealso [solve_scenario()]
#' @examples
#' names(solver_options)
#' solver_options[["glpk"]]
"solver_options"
