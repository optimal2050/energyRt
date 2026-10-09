# =========================================================================== #
# Parent levels: giving a variable rows at the time / space levels COARSER than
# the one it occupies, each defined by a roll-up of its finer rows.
#
# Why. `summand@timeframe` / `@geoframe` let a constraint take a variable at a
# named level. When the variable has a row AT that level the term pins to it;
# when it has none, the term sums the finer rows. Both are correct, but a model
# whose objects sit at DIFFERENT levels can make the request ambiguous for some
# members and not others, which `.resolve_level_indices()` refuses rather than
# answering wrongly. Giving the variable its parent rows removes the ambiguity:
# every member then has a row at the level, so the term simply pins.
#
# What this costs. Parents only -- never finer rows. Measured on `s4_h24`
# (ANNUAL 1 < SEASON 4 < HOUR 96): 54,398 -> 57,429 domain rows, 1.06x overall
# and 1.12x on the worst variable. Adding CHILD levels instead would be 1.7x
# with 101x outliers on annual-only variables, which is why only ancestors are
# ever added. Under simplex the added rows are definitional and presolve removes
# them (81% of rows on TOPIA R3); on a PDLP/GPU route without crossover they are
# not free, which is what `parent_levels = FALSE` is for.
#
# `vOutTot` / `vInpTot` are NOT governed here. They carry parent rows
# unconditionally because `eqOutTot` / `eqInpTot` are written against them (see
# the `[timeframe-contract]` / `[nested-regions]` notes in R/mapping-filter.R),
# so switching this off must not take theirs away -- that is what makes OFF
# reproduce the pre-feature baselines exactly.
# =========================================================================== #

# Variables that have a roll-up equation in the templates, and may therefore be
# given parent rows. A parent row with no equation to define it is a FREE
# variable: the model would still solve, and silently wrong. So the supported
# set is the equations that exist, and naming anything else is refused.
#
# Grows one variable at a time as the roll-up equations land; empty means the
# switch is a guaranteed no-op.
.parent_level_vars <- character()

#' Resolve the `parent_levels` argument to a character vector of variables
#'
#' @param x `TRUE` (every supported variable), `FALSE`/`NULL` (none), or a
#'   character vector selecting among the supported ones.
#' @return character vector, possibly empty.
#' @keywords internal
#' @noRd
.parent_levels_resolve <- function(x) {
  if (is.null(x)) return(character())
  if (is.logical(x)) {
    if (length(x) != 1 || is.na(x)) {
      stop("`parent_levels` must be TRUE, FALSE, or a character vector of ",
           "variable names.", call. = FALSE)
    }
    return(if (x) .parent_level_vars else character())
  }
  if (!is.character(x)) {
    stop("`parent_levels` must be TRUE, FALSE, or a character vector of ",
         "variable names; got ", paste(class(x), collapse = "/"), ".",
         call. = FALSE)
  }
  x <- unique(x[!is.na(x) & nzchar(x)])
  if (length(x) == 0) return(character())
  unknown <- setdiff(x, names(.variable_mapping))
  if (length(unknown) > 0) {
    stop("`parent_levels` names unknown variable(s) ",
         paste0('"', unknown, '"', collapse = ", "), ".", call. = FALSE)
  }
  unsupported <- setdiff(x, .parent_level_vars)
  if (length(unsupported) > 0) {
    stop("`parent_levels` names ",
         paste0('"', unsupported, '"', collapse = ", "),
         ", which has no roll-up equation yet, so its parent rows would be ",
         "free variables.",
         if (length(.parent_level_vars) == 0) {
           " No variable supports parent levels yet."
         } else {
           paste0(" Supported: ",
                  paste0('"', .parent_level_vars, '"', collapse = ", "), ".")
         }, call. = FALSE)
  }
  x
}

# The resolved set for the interpolation in progress, as published by
# `interpolate_model()` for the duration of the call. Read by the mapping layer,
# which is dispatched as `f(scen, fmp)` and has no argument channel.
#' @noRd
.parent_levels_active <- function() {
  .parent_levels_resolve(getOption("energyRt.parent_levels", FALSE))
}

#' Does this variable get parent rows in the interpolation in progress?
#' @keywords internal
#' @noRd
.has_parent_levels <- function(variable) {
  variable %in% .parent_levels_active()
}
