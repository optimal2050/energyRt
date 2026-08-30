#' @include zzz.R docs.R
#
# `options::as_roxygen_docs()` hardcodes `@name options` / `@rdname options`,
# which would give energyRt a help topic aliased `options` -- colliding with
# `?options` from base R. Rewrite those two tags to `energyRt-options`; the rest
# of the generated page (one entry per registered option, with its default,
# `en.` option name and `ENERGYRT_` environment variable) is used as-is.
#' @eval sub("^@(name|rdname) options$", "@\\1 energyRt-options", options::as_roxygen_docs())
NULL

# Naming scheme ###############################################################
# Set once here, BEFORE any `define_option()` call, rather than repeating
# `option_name=` / `envvar_name=` on every declaration. `option_spec()` resolves
# both names at definition time from these functions, so ordering matters --
# `options.R` is loaded early in `Collate:` and nothing before it declares an
# option.
#
#   R option  ->  en.<name>            e.g. options(en.gams_path = "C:/GAMS/48")
#   env var   ->  ENERGYRT_<NAME>      e.g. ENERGYRT_GAMS_PATH
#
# `en.` matches the package's `en_*` function family and follows R's convention
# of dot-separated package option names. The env-var prefix is spelled out in
# full because environment variables are global to the OS, where `EN_` would be
# far too collision-prone.
options::set_option_name_fn(function(package, option) paste0("en.", option))

options::set_envvar_name_fn(function(package, option) {
  paste0("ENERGYRT_", toupper(gsub("[^A-Za-z0-9]", "_", option)))
})

# Toolchain paths #############################################################
# Locations of the external programs the solver backends shell out to. All
# default to `NULL`, meaning "not configured" -- each backend then falls back to
# its own discovery (see `.find_glpsol()`) or to the session
# `PATH`. Set them with `set_solver_path()` or the per-backend wrappers.

options::define_option(
  "gams_path",
  desc = paste(
    "Path to the GAMS installation directory. Prefixed to the `gams` command",
    "when solving with a GAMS backend. If unset, `gams` is resolved on the",
    "session PATH."
  ),
  default = NULL
)

options::define_option(
  "gdxlib_path",
  desc = paste(
    "Path to the GAMS Data Exchange (GDX) library, used to read and write",
    "`*.gdx` files. If unset, `gams_path` is used instead."
  ),
  default = NULL
)

options::define_option(
  "python_path",
  desc = paste(
    "Path to the Python installation or environment used by the Pyomo backend.",
    "Prefixed to the `python` command. If unset, `python` is resolved on the",
    "session PATH."
  ),
  default = NULL
)

options::define_option(
  "julia_path",
  desc = paste(
    "Path to the Julia installation used by the JuMP backend. Prefixed to the",
    "`julia` command. If unset, `julia` is resolved on the session PATH."
  ),
  default = NULL
)

options::define_option(
  "glpk_path",
  desc = paste(
    "Path to a standalone GLPK installation containing the `glpsol`",
    "executable. If unset, `glpsol` is auto-detected on the session PATH --",
    "which on Windows finds the copy bundled with Rtools."
  ),
  default = NULL
)

# (the `mosox_path` option moved to drafts/mosox/ with the mosox experiment)

# NEOS ########################################################################
# `neos_email` is the one option that keeps an explicit `envvar_name`: NEOS
# requires an email for job submission, `set_neos_email()` exports it with
# `Sys.setenv()`, and the Pyomo NEOS shim reads `NEOS_EMAIL` from inside the
# python subprocess (see `data-raw/solver_options.R`). Renaming it to
# `ENERGYRT_NEOS_EMAIL` would break that hand-off.
options::define_option(
  "neos_email",
  desc = paste(
    "Email address for NEOS Server job submission (required by NEOS). Read",
    "from the `NEOS_EMAIL` environment variable if the option is unset."
  ),
  default = NULL,
  envvar_name = "NEOS_EMAIL"
)

options::define_option(
  "neos_endpoint",
  desc = "XML-RPC endpoint of the NEOS Server.",
  default = "https://neos-server.org:3333"
)

# Defaults ####################################################################

options::define_option(
  "solver",
  desc = paste(
    "Default solver specification, used when a scenario carries none. A named",
    "list, normally one of the `solver_options` presets."
  ),
  default = list(
    name = "glpk",
    lang = "GLPK"
  )
)

options::define_option(
  "scenarios_path",
  desc = paste(
    "Root directory for scenario folders. Scenario paths and solver working",
    "directories are built underneath it."
  ),
  default = "scenarios/"
)

options::define_option(
  "registry_file",
  desc = paste(
    "Path to the project registry CSV indexing saved models, scenarios and",
    "runs. Kept at the project root, a sibling of the scenarios/ and models/",
    "stores, so one registry spans both."
  ),
  default = "energyRt_registry.csv"
)

options::define_option(
  "models_path",
  desc = paste(
    "Root directory for the model store. save_model() writes",
    "content-addressed model folders (<name>@<hash8>) underneath it."
  ),
  default = "models/"
)

options::define_option(
  "repositories_path",
  desc = paste(
    "Root directory for the repository store. save_repository() writes",
    "content-addressed repository folders (<name>@<hash8>) underneath it."
  ),
  default = "repositories/"
)

options::define_option(
  "datasets_path",
  desc = paste(
    "Root directory for the dataset store. save_dataset() writes",
    "content-addressed dataset folders (<name>@<hash8>) underneath it."
  ),
  default = "datasets/"
)

options::define_option(
  "log_file",
  desc = paste(
    "Optional operation-log CSV. Empty (default) = logging off; a path",
    "makes interpolate_model()/solve_scenario()/solve_myopic() append one",
    "line per operation. Read it back with read_log()."
  ),
  default = ""
)

options::define_option(
  "reports_path",
  desc = paste(
    "Default directory for rendered reports of IN-MEMORY objects (treated as",
    "temporary output). Reports of saved objects land inside the owning",
    "folder (<scenario>/reports/, <model store entry>/reports/) instead."
  ),
  default = "reports/"
)

options::define_option(
  "levcost_cache_path",
  desc = paste(
    "Default directory for cached levcost results of IN-MEMORY objects",
    "(treated as temporary). Results for saved objects are cached inside the",
    "owning folder (<owner>/levcost/) instead."
  ),
  default = "levcosts/"
)

options::define_option(
  "path_builders",
  desc = paste(
    "Named list of user functions overriding the built-in folder-name",
    "derivations (session-local; set with set_path_builder()). Hookable",
    "kinds: scenario_dir, store_entry, run_label, slug — see",
    "?path_builders for the contracts."
  ),
  default = list()
)

# Storage / exchange format ###################################################
# Format used to exchange model data / solution with the JuMP / Pyomo solvers
# (written into the solver run-folder), and the default on-disk storage codec.

options::define_option(
  "arrow_format",
  desc = paste(
    "Default Arrow exchange format for the JuMP/Pyomo solvers:",
    "'feather' (IPC), 'parquet', or 'csv'."
  ),
  default = "feather"
)

options::define_option(
  "arrow_compression",
  desc = "Arrow compression codec: 'zstd', 'lz4', or 'uncompressed'.",
  default = "zstd"
)

options::define_option(
  "arrow_compression_level",
  desc = "Arrow compression level (codec-dependent; ZSTD supports 1-22).",
  default = 15L
)

# Reporting ###################################################################

options::define_option(
  "verbose",
  desc = paste(
    "Verbosity level: `0` silent, `1` progress messages, `2` and above for",
    "increasingly detailed reporting. `TRUE`/`FALSE` are accepted and read as",
    "`1`/`0`. Test with `isVerbose(level)`."
  ),
  default = 0
)

options::define_option(
  "debug",
  desc = paste(
    "Debug level, in the same 0/1/2 form as `verbose`. Enables internal",
    "consistency warnings that are silent in normal use. Test with",
    "`isDebug(level)`."
  ),
  default = 0
)

options::define_option(
  "progress_bar",
  desc = paste(
    "Whether long-running operations display a progress bar. Set through",
    "`show_progress_bar()` / `set_progress_bar()`."
  ),
  default = TRUE
)

# Generic accessors ###########################################################

#' Get or set an energyRt option
#'
#' @description
#' Generic accessors for any option in the energyRt registry. See
#' `?energyRt-options` for the full list, or [en_config_show()] to print every
#' option together with the source its current value came from.
#'
#' Each option can be set three ways, in decreasing priority: the R option
#' (`options(en.<name> = )`), the environment variable
#' (`ENERGYRT_<NAME>`), or the configuration file (see [en_config_write()]).
#'
#' @param name character, the option name *without* the `en.` prefix,
#'   e.g. `"gams_path"`.
#' @param value the new value.
#' @param default value returned if the option is not defined.
#'
#' @return `get_option()` the option value; `set_option()` the previous value,
#'   invisibly.
#'
#' @family options
#' @rdname en_option
#' @export
#' @examples
#' get_option("arrow_format")
set_option <- function(name, value) {
  options::opt_set(name, value, env = "energyRt")
}

#' @family options
#' @rdname en_option
#' @export
get_option <- function(name, default = NULL) {
  options::opt(name, default = default, env = "energyRt")
}

# Toolchain path accessors ####################################################

# option name for each backend, keyed by the short backend name used in the
# public `set_solver_path()` / `get_solver_path()` API.
.solver_path_options <- c(
  gams   = "gams_path",
  gdxlib = "gdxlib_path",
  glpk   = "glpk_path",
  python = "python_path",
  julia  = "julia_path"
)

# Environment variables these options used before the `ENERGYRT_` prefix was
# introduced. Honoured as a deprecated fallback so existing setups keep working;
# drop after one release.
.solver_path_legacy_envvars <- c(
  gams   = "GAMS_PATH",
  gdxlib = "GDXLIB_PATH",
  glpk   = "glpk_path",
  python = "PYTHON_PATH",
  julia  = "JULIA_PATH"
)

# Shared body of every path setter: reject a non-existent directory at the point
# of the mistake rather than at solve time, and guarantee a trailing "/" so the
# value can be pasted straight onto an executable name.
.normalize_solver_path <- function(path) {
  if (is.null(path) || !nzchar(path)) {
    return(path)
  }
  if (!dir.exists(path)) {
    stop(paste0('The path "', path, '" does not exist.'), call. = FALSE)
  }
  if (!grepl("/$", path)) {
    path <- paste0(path, "/")
  }
  path
}

#' Set or get the path to a solver toolchain
#'
#' @description
#' `set_solver_path()` and `get_solver_path()` are the generic form of the
#' per-backend accessors ([set_gams_path()], [set_julia_path()], ...), which are
#' thin wrappers around them.
#'
#' Setting a path is optional. When it is unset, each backend falls back to its
#' own discovery: `glpk` searches the session `PATH` and well-known
#' install locations (on Windows `glpsol` is found in Rtools, which bundles
#' GLPK), while `gams`, `python` and `julia` simply invoke the bare command and
#' let the OS resolve it on `PATH`. Set a path only to override that, for
#' instance to pick between several installed GAMS versions.
#'
#' @param backend character, one of `"gams"`, `"gdxlib"`, `"glpk"`,
#'   `"python"`, `"julia"`.
#' @param path character, path to the directory containing the executable or
#'   library, or `NULL` to clear it. The directory must exist.
#'
#' @return `get_solver_path()` the configured path with a trailing `/`, or
#'   `NULL` if unset; `set_solver_path()` the path, invisibly.
#'
#' @family options
#' @family solver
#' @rdname solver_path
#' @export
#' @examples
#' \dontrun{
#' set_solver_path("gams", "C:/GAMS/win64/48.1")
#' get_solver_path("gams")
#' }
set_solver_path <- function(backend, path = NULL) {
  backend <- match.arg(backend, names(.solver_path_options))
  options::opt_set(.solver_path_options[[backend]],
                   .normalize_solver_path(path))
  invisible(path)
}

#' @family options
#' @family solver
#' @rdname solver_path
#' @export
get_solver_path <- function(backend) {
  backend <- match.arg(backend, names(.solver_path_options))
  name <- .solver_path_options[[backend]]
  res <- tryCatch(options::opt(name), error = function(e) NULL)

  # Deprecated: the un-prefixed environment variables used before ENERGYRT_*.
  if ((is.null(res) || !nzchar(res)) &&
      identical(.en_opt_source(name), "default")) {
    legacy <- Sys.getenv(.solver_path_legacy_envvars[[backend]], unset = "")
    if (nzchar(legacy)) {
      .en_deprecate_once(
        paste0("envvar_", backend),
        paste0("Environment variable `", .solver_path_legacy_envvars[[backend]],
               "` is deprecated. Use `",
               toupper(paste0("ENERGYRT_", .solver_path_options[[backend]])),
               "` instead.")
      )
      res <- legacy
    }
  }

  if (is.null(res) || !nzchar(res)) NULL else res
}

# Reporting accessors #########################################################

# Coerce whatever the user put in `en.verbose` / `en.debug` into a level.
.as_report_level <- function(x) {
  if (is.null(x) || length(x) != 1L || is.na(x)) {
    return(0)
  }
  if (is.logical(x)) {
    return(as.numeric(x))
  }
  suppressWarnings(res <- as.numeric(x))
  if (is.na(res)) 0 else res
}

#' Verbosity and debug level
#'
#' @description
#' Test the current reporting level. `isVerbose()` gates user-facing progress
#' messages, `isDebug()` gates internal consistency warnings that are silent in
#' normal use. Set the levels with `options(en.verbose = )` /
#' `options(en.debug = )`, or `set_option("verbose", )`.
#'
#' Both accept a level (`0`, `1`, `2`, ...) or a logical, which is read as
#' `1`/`0`.
#'
#' @param level numeric, the level to test against.
#'
#' @return `TRUE` if the configured level is at or above `level`.
#'
#' @family options
#' @rdname isVerbose
#' @export
#' @examples
#' isVerbose()
#' isDebug(2)
isVerbose <- function(level = 1) {
  # Deprecated: `energyRt.verbose` predates the registry and is documented in
  # ?interpolate_model, so honour it while the user has not set `en.verbose`.
  if (identical(.en_opt_source("verbose"), "default")) {
    legacy <- getOption("energyRt.verbose", NULL)
    if (!is.null(legacy)) {
      .en_deprecate_once(
        "energyRt.verbose",
        'Option "energyRt.verbose" is deprecated. Use options(en.verbose = 1).'
      )
      return(.as_report_level(legacy) >= level)
    }
  }
  .as_report_level(options::opt("verbose", env = "energyRt")) >= level
}

#' @family options
#' @rdname isVerbose
#' @export
isDebug <- function(level = 1) {
  .as_report_level(options::opt("debug", env = "energyRt")) >= level
}

# Default solver ##############################################################

#' Default solver
#'
#' @description
#' Get or set the solver specification used when a scenario does not carry one
#' of its own. Normally one of the `solver_options` presets.
#'
#' @param solver a named list describing the solver, e.g. `solver_options$glpk`.
#'
#' @return `get_default_solver()` the solver list; `set_default_solver()` the
#'   previous value, invisibly.
#'
#' @family options
#' @family solver
#' @rdname default_solver
#' @export
#' @examples
#' get_default_solver()
get_default_solver <- function() {
  options::opt("solver")
}

#' @family options
#' @family solver
#' @rdname default_solver
#' @export
set_default_solver <- function(solver) {
  options::opt_set("solver", solver)
}

# Scenarios directory #########################################################

#' Set or get the directory with scenarios
#'
#' @param path character, path to the directory holding scenario folders.
#'
#' @return `get_scenarios_path()` the path; `set_scenarios_path()` the previous
#'   value, invisibly.
#'
#' @family options
#' @rdname scenarios_path
#' @export
#' @examples
#' get_scenarios_path()
set_scenarios_path <- function(path = NULL) {
  options::opt_set("scenarios_path", path)
}

#' @family options
#' @rdname scenarios_path
#' @export
get_scenarios_path <- function() {
  options::opt("scenarios_path")
}

# Registry ####################################################################

#' Registry file and model store locations
#'
#' @description
#' `get_registry_file()` / `set_registry_file()` locate the project registry
#' CSV (see [load_registry()]). `get_models_path()` / `set_models_path()`
#' locate the model store root used by `save_model()`. Both default to the
#' project root / `models/`, siblings of [get_scenarios_path()].
#'
#' @param path character, new location.
#'
#' @return getters return the path; setters return the previous value,
#'   invisibly.
#'
#' @family options
#' @rdname registry_file
#' @export
#' @examples
#' get_registry_file()
#' get_models_path()
set_registry_file <- function(path = NULL) {
  options::opt_set("registry_file", path)
}

#' @family options
#' @rdname registry_file
#' @export
get_registry_file <- function() {
  options::opt("registry_file")
}

#' @family options
#' @rdname registry_file
#' @export
set_models_path <- function(path = NULL) {
  options::opt_set("models_path", path)
}

#' @family options
#' @rdname registry_file
#' @export
get_models_path <- function() {
  options::opt("models_path")
}

#' @family options
#' @rdname registry_file
#' @export
set_repositories_path <- function(path = NULL) {
  options::opt_set("repositories_path", path)
}

#' @family options
#' @rdname registry_file
#' @export
get_repositories_path <- function() {
  options::opt("repositories_path")
}

#' @family options
#' @rdname registry_file
#' @export
set_datasets_path <- function(path = NULL) {
  options::opt_set("datasets_path", path)
}

#' @family options
#' @rdname registry_file
#' @export
get_datasets_path <- function() {
  options::opt("datasets_path")
}


# Derived-artifact locations ##################################################

#' Report and levcost output locations
#'
#' @description
#' Derived artifacts live with the object they describe when that object is
#' saved: a scenario's reports render into `<scenario>/reports/` and its
#' levcost results cache into `<scenario>/levcost/`; models and repositories
#' use their store entries the same way. For IN-MEMORY objects the output is
#' considered temporary and lands in these project-level folders instead:
#' `get_reports_path()` (default `reports/`) and `get_levcost_cache_path()`
#' (default `levcosts/`).
#'
#' @param path character, new location.
#' @return getters return the path; setters return the previous value,
#'   invisibly.
#'
#' @family options
#' @rdname reports_path
#' @export
#' @examples
#' get_reports_path()
#' get_levcost_cache_path()
set_reports_path <- function(path = NULL) {
  options::opt_set("reports_path", path)
}

#' @family options
#' @rdname reports_path
#' @export
get_reports_path <- function() {
  options::opt("reports_path")
}

#' @family options
#' @rdname reports_path
#' @export
set_levcost_cache_path <- function(path = NULL) {
  options::opt_set("levcost_cache_path", path)
}

#' @family options
#' @rdname reports_path
#' @export
get_levcost_cache_path <- function() {
  options::opt("levcost_cache_path")
}

# Path builders ###############################################################

#' Override how folder names are derived
#'
#' @description
#' The storage layer derives every folder name through a small set of
#' seams, each of which a user function can replace for the session:
#'
#' | kind | signature | names |
#' |---|---|---|
#' | `scenario_dir` | `function(name, model, calendar, horizon)` | the scenario folder basename under [get_scenarios_path()] |
#' | `store_entry` | `function(type, name)` — type `"model"`, `"repository"` or `"dataset"` | a store entry's basename under its root |
#' | `run_label` | `function(solver)` — the solver spec list | the default solve label when `run =` is not given |
#' | `slug` | `function(parts)` — character vector | the sanitize-and-join primitive used everywhere else |
#'
#' A hook must return a single non-empty, filesystem-safe string (no path
#' separators; letters, digits, `.`/`_`/`-`); an invalid return is an
#' ERROR naming the contract — never a silent fallback. Custom scenario
#' and store folder names are fully safe: resolution goes through the
#' registry and the folder manifests, never through parsing names. The
#' run TREE (`runs/<solve>` vs `runs/<variant>/<solve>`) is structural and
#' not hookable. A custom `slug` should keep dashes out of the individual
#' parts, or folder labels become ambiguous (harmless — nothing parses
#' them — but stated). Hooks are session-local options, not stored in
#' projects.
#'
#' @param scenario_dir,store_entry,run_label,slug a function per the table
#'   (merged into the active set), or `FALSE` to remove that hook; `NULL`
#'   (default) leaves it unchanged.
#' @param kind character, one hook name to fetch (or `NULL` for the whole
#'   list).
#' @return `set_path_builder()` the previous list, invisibly;
#'   `get_path_builder()` the requested function (or `NULL`) / the list.
#'
#' @examples
#' \dontrun{
#' set_path_builder(scenario_dir = function(name, model, calendar, horizon)
#'   paste(name, calendar, sep = "."))
#' set_path_builder(scenario_dir = FALSE)   # back to the default
#' }
#' @family options
#' @rdname path_builders
#' @export
set_path_builder <- function(scenario_dir = NULL, store_entry = NULL,
                             run_label = NULL, slug = NULL) {
  hooks <- list(scenario_dir = scenario_dir, store_entry = store_entry,
                run_label = run_label, slug = slug)
  cur <- options::opt("path_builders") %||% list()
  old <- cur
  for (k in names(hooks)) {
    v <- hooks[[k]]
    if (is.null(v)) next
    if (isFALSE(v)) {
      cur[[k]] <- NULL
    } else if (is.function(v)) {
      cur[[k]] <- v
    } else {
      stop("`", k, "` must be a function (or FALSE to remove the hook).")
    }
  }
  options::opt_set("path_builders", cur)
  invisible(old)
}

#' @rdname path_builders
#' @export
get_path_builder <- function(kind = NULL) {
  cur <- options::opt("path_builders") %||% list()
  if (is.null(kind)) cur else cur[[kind]]
}

#' @family options
#' @rdname log
#' @export
set_log_file <- function(path = NULL) {
  options::opt_set("log_file", path)
}

#' @family options
#' @rdname log
#' @export
get_log_file <- function() {
  options::opt("log_file")
}

# Arrow exchange format #######################################################

#' Arrow exchange-format options
#'
#' Getters / setters for the Arrow format used to exchange data with the
#' JuMP / Pyomo solvers (and the default on-disk storage codec).
#'
#' @param format one of `"feather"`, `"parquet"`, `"csv"`.
#' @param codec compression codec, e.g. `"zstd"`, `"lz4"`, `"uncompressed"`.
#' @param level integer compression level (ZSTD: 1-22).
#'
#' @return the option value (getters) or the previous value, invisibly (setters).
#'
#' @family options
#' @rdname arrow_format
#' @export
#' @examples
#' get_arrow_format()
get_arrow_format <- function() options::opt("arrow_format")

#' @family options
#' @rdname arrow_format
#' @export
set_arrow_format <- function(format = c("feather", "parquet", "csv")) {
  format <- match.arg(format)
  options::opt_set("arrow_format", format)
}

#' @family options
#' @rdname arrow_format
#' @export
get_arrow_compression <- function() options::opt("arrow_compression")

#' @family options
#' @rdname arrow_format
#' @export
set_arrow_compression <- function(codec = c("zstd", "lz4", "uncompressed")) {
  codec <- match.arg(codec)
  options::opt_set("arrow_compression", codec)
}

#' @family options
#' @rdname arrow_format
#' @export
get_arrow_compression_level <- function() options::opt("arrow_compression_level")

#' @family options
#' @rdname arrow_format
#' @export
set_arrow_compression_level <- function(level = 15L) {
  options::opt_set("arrow_compression_level", as.integer(level))
}
