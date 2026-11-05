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

  # as_multimod.scenario() is registered by NAMESPACE, via
  # `@exportS3Method multimod::as_multimod` -> `S3method(multimod::as_multimod,
  # scenario)`. That is R's own delayed registration for a generic owned by a
  # SUGGESTED package: dispatch works when multimod is installed, and nothing
  # breaks when it is not. A hand-rolled setHook() lived here until
  # 2026-09-24 and is no longer needed.

  invisible()
}


#' Check validity of object's names used in sets
#'
#' @param x character, name of an object of `energyRt`
#'
#' @return logical, TRUE if the name is valid.
#'
#' @examples
#' energyRt:::check_name("name")
#' energyRt:::check_name("1name")
#' energyRt:::check_name("name1")
#' energyRt:::check_name("name_1")
#' energyRt:::check_name("name_1!")
#' energyRt:::check_name(c("a", "b")) # FALSE, not a single name
#' energyRt:::check_name(1) # FALSE, not character
#' @keywords internal
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
# `new("<class>", slot = value)`: refuse rather than discard
#
# Most classes define `initialize()` as a passthrough returning `.Object`, so
# anything handed to `new()` used to vanish -- `new("technology", name = "X")`
# returned a technology named "". The public constructors are the supported
# path because they are the only route through `.data2slots()`: column checks,
# type coercion, renamed-column diagnostics and the conflicting-row check all
# live there, so a `new()` that accepted slot values would mint objects that
# skipped every one of them.
#
# Bare `new("<class>")` stays supported: prototypes are built that way in ~29
# internal places.
#' @noRd
.refuse_initialize_dots <- function(cls, ctor, ...) {
  n <- ...length()
  if (n == 0L) return(invisible(NULL))
  nms <- names(list(...))
  if (is.null(nms)) nms <- rep("", n)
  shown <- ifelse(nzchar(nms), nms, paste0("<", seq_len(n), ">"))
  # `ctor = NULL` for a generated class with no public constructor
  advice <- if (is.null(ctor)) {
    paste0("  It is built by the solve pipeline (`solve_scenario()`, ",
           "`read_solution()`); assign a slot after construction if you ",
           "really need to (`obj@<slot> <- value`).")
  } else {
    paste0("  Use ", ctor, "() instead -- the constructors are the only path ",
           "through the construction checks (column names, types, renamed ",
           "columns, conflicting rows).")
  }
  stop("new(\"", cls, "\", ...) does not accept slot values, and would ",
       "silently discard ", if (n == 1L) "" else paste0(n, " of them: "),
       paste0("`", shown, "`", collapse = ", "),
       ".\n", advice, "\n  `new(\"", cls,
       "\")` with no arguments is still supported for an empty prototype.",
       call. = FALSE)
}

# Classes a repository stores, i.e. the ones addressed BY NAME: `repo[["X"]]`,
# `add(mod, obj, overwrite = TRUE)`, the solver symbol and the scenario path
# all go through the name. Mirrors `newRepository()@permit`.
#' @noRd
.NAMED_OBJECT_CLASSES <- c(
  "commodity", "demand", "supply",
  "technology", "storage",
  "trade", "export", "import", "weather",
  "tax", "subsidy", "constraint", "costs"
)

.assert_object_name <- function(name, what = "object") {
  if (!length(name) || !nzchar(name %||% "")) {
    # Deprecated 2026-11-05: an unnamed brick cannot be addressed in a
    # repository, and the name becomes a solver identifier. Still accepted for
    # one cycle; containers and internal prototypes keep empty names for good.
    if (length(what) == 1L && what %in% .NAMED_OBJECT_CLASSES) {
      warning("A ", what, " should have a non-empty name. An unnamed object ",
              "cannot be addressed in a repository (`repo[[\"<name>\"]]`, ",
              "`add(..., overwrite = TRUE)`), and the name becomes a solver ",
              "identifier. This will be an error in a future release.",
              call. = FALSE)
    }
    return(invisible(TRUE))
  }
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
