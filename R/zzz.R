# zzz.R #######################################################################
# Package-private state and load-time side effects.
#
# Everything here used to be scattered: `data.table::setNumericRounding(2)` ran
# at source time in `R/options.R`, `options(progressr.clear = )` was set in
# `.onAttach`, and the GDX load-once latch lived in a user-visible base option
# (`en_gdxlib_loaded`). None of those are settings -- they are internal state
# and one-off initialisation, so they belong here rather than in the options
# registry.

# Package-private mutable state. NOT user configuration -- see `R/options.R`
# for that. Fields:
#   gdxlib_loaded   logical, TRUE once `gdxtools::igdx()` has succeeded
#   deprecated      character, keys already warned about (see below)
#   config_applied  named list, options taken from the configuration file at
#                   load time, and which file each came from
.en_state <- new.env(parent = emptyenv())
.en_state$gdxlib_loaded <- FALSE
.en_state$deprecated <- character()
.en_state$config_applied <- list()

# Where the option value came from: "option", "envvar" or "default".
#
# `options::opt()` and `opt_set()` run their `env=` argument through `as_env()`,
# so they accept the package name or any calling frame. `opt_source()` does
# neither -- it needs the environment that literally holds the registry, and
# given anything else it warns "option ... is not defined in environment" and
# returns NA. `topenv()` resolves to the energyRt namespace from anywhere inside
# the package, including under `devtools::load_all()`.
.en_opt_source <- function(name) {
  options::opt_source(name, env = topenv(environment()))
}

# Emit a deprecation warning at most once per session, keyed by `key`.
# Deprecated paths are hit on every read (e.g. an option consulted inside a
# loop), so an unconditional warning would flood the console.
.en_deprecate_once <- function(key, message) {
  if (key %in% .en_state$deprecated) {
    return(invisible(FALSE))
  }
  .en_state$deprecated <- c(.en_state$deprecated, key)
  cli::cli_warn(message)
  invisible(TRUE)
}

# Reset the once-only warning ledger. Used by tests.
.en_deprecate_reset <- function() {
  .en_state$deprecated <- character()
  invisible(NULL)
}

.onLoad <- function(libname, pkgname) {
  # Default rounding used by data.table's `unique()` / `duplicated()`. energyRt
  # relies on this when de-duplicating parameter rows on doubles.
  data.table::setNumericRounding(2)

  # Apply `~/.energyRt/config.yml` (and any project-level `.energyRt.yml`) to
  # options the user has not already set via an R option or environment
  # variable. Never fatal: a malformed config must not stop the package loading.
  try(.en_apply_config(), silent = TRUE)

  invisible()
}
