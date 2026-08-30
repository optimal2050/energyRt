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

#' Check validity of object's names used in sets
#'
#' @param x character, name of an object of `energyRt`
#'
#' @return logical, TRUE if the name is valid.
#' @export
#'
#' @examples
#' check_name("name")
#' check_name("1name")
#' check_name("name1")
#' check_name("name_1")
#' check_name("name_1!")
#' check_name(c("a", "b")) # FALSE, not a single name
#' check_name(1) # FALSE, not character
check_name <- function(x, dot = FALSE) {
  # NB the guards used to be OR'd into the "valid" expression, so a non-scalar
  # or non-character input was reported as VALID. They must negate it instead.
  # `dot = TRUE` additionally allows ONE leading dot — the internal scratch
  # convention (levcost mini-models, `.LCS_RUN`) — never dots elsewhere.
  pat <- if (dot) "^\\.?[[:alpha:]][[:alnum:]_]*$" else
    "^[[:alpha:]][[:alnum:]_]*$"
  length(x) == 1 && is.character(x) && sub(pat, "", x) == ""
}

# Enforce the object-name rule at construction: letters, digits, underscore,
# starting with a letter (optionally one leading dot for internal scratch
# objects). Names travel into GAMS/GLPK/JuMP/Pyomo identifiers, where
# dashes, dots and spaces are illegal — and "-" is the path-slug separator.
# Run/variant LABELS are not object names and are not checked here.
.assert_object_name <- function(name, what = "object") {
  if (!length(name) || !nzchar(name %||% "")) return(invisible(TRUE))
  if (!check_name(name, dot = TRUE)) {
    bad <- gsub("[.A-Za-z0-9_]", "", name)
    stop("Invalid ", what, " name '", name, "'",
         if (nzchar(bad)) paste0(" (offending: '", bad, "')"),
         ": names use letters, digits and underscore, and start with a ",
         "letter — they become solver identifiers on all four backends.",
         call. = FALSE)
  }
  invisible(TRUE)
}
