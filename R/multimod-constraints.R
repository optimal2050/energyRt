# =============================================================================
# energyRt user constraints and user costs
# =============================================================================
#
# `newConstraint()` / `newCosts()` objects never reach a backend as objects.
# `interp_mod()` compiles each one into a solver-agnostic intermediate form and
# stores it on `modInp@user_constraints` (a list of three strings per
# constraint) and `modInp@user_costs` (a single string). That IR *is* GAMS:
#
#   eqCnsC_LWIN_AF20_CL1(region, year)$mCnsForEachC_LWIN_AF20_CL1(region, year)..
#     sum(tech$(mCnsC_LWIN_AF20_CL1_1(tech) and mTechSpan(tech, region, year)),
#         1 * pCnsMultC_LWIN_AF20_CL1_1(tech) * vTechCap(tech, region, year))
#     =l= pCnsRhsC_LWIN_AF20_CL1(region, year);
#
# so multimod reads it with the parser it already has, rather than
# reimplementing `constraint` / `summand` semantics. Everything expensive -
# `for.each` expansion, horizon filtering, timeframe/geoframe hierarchy
# expansion, `<set>p` aliasing, lag/lead exponentiation, year interpolation -
# was already done by interpolation and materialised into ordinary parameters:
#
#   mCnsForEach<NAME>   the row set (the equation's domain)
#   mCns<NAME>_<k>      one membership map per distinct for.sum restriction
#   pCnsRhs<NAME>       the interpolated right-hand side
#   pCnsMult<NAME>_<i>  one coefficient table per summand
#   mCosts<NAME> / pCosts<NAME>   the same for `newCosts()`
#
# Those symbols are absent from `energyRt.gms` (they are per-scenario), so they
# have to be declared on the model before the usual linking step can attach
# their data - otherwise the importer logs them "unmatched_in_multimod" and
# drops them silently.
#
# Note on `vTotalUserCosts`: the core .gms declares the variable and its map
# and adds it into the cost equation, but the equation that *defines* it is
# generated, never written to the .gms. Without the `user_costs` string parsed
# in, it is an undefined column that minimisation pushes to its lower bound -
# accidentally right while `user_costs` is "=e= 0", silently too cheap as soon
# as a `newCosts()` object exists.
# =============================================================================

# Symbols interpolation creates for user constraints / costs. Anchored so a
# model symbol that merely starts with the same letters is not swept in.
.cns_symbol_regex <- "^(mCnsForEach|mCns|pCnsRhs|pCnsMult|pCns|mCosts|pCosts)"

#' Declare a scenario's user-constraint support symbols on a model
#'
#' Adds an empty `mapping` / `parameter` for every `mCns*` / `pCns*` /
#' `mCosts*` / `pCosts*` entry of the scenario, so that
#' `link_scenario_data_with_log()` can attach their data. Existing symbols are
#' left untouched.
#'
#' @param model A multimod model.
#' @param scenario An energyRt scenario.
#' @param verbose Logical; report what was declared.
#'
#' @return The model, with the new declarations.
#' @keywords internal
#' @noRd
declare_user_constraint_symbols <- function(model, scenario, verbose = FALSE) {
  stopifnot(inherits(model, "multimod") || inherits(model, "model"))
  params <- scenario@modInp@parameters
  nms <- grep(.cns_symbol_regex, names(params), value = TRUE)
  n_map <- 0L
  n_par <- 0L

  for (nm in nms) {
    p <- params[[nm]]
    if (!isS4(p) || !methods::is(p, "parameter")) next
    tp <- as.character(p@type)
    dims <- as.character(p@dimSets)
    if (!length(dims)) next
    desc <- if (length(p@desc) && nzchar(p@desc)) {
      p@desc
    } else {
      paste("user constraint symbol", nm)
    }

    if (identical(tp, "map")) {
      if (nm %in% names(model$mappings)) next
      model$mappings[[nm]] <- multimod::new_mapping(
        name = nm, desc = desc, dims = dims, active_dims = dims,
        data = data.frame()
      )
      n_map <- n_map + 1L
    } else if (identical(tp, "numpar")) {
      if (nm %in% names(model$parameters)) next
      model$parameters[[nm]] <- multimod::new_parameter(
        name = nm, desc = desc, dims = dims, active_dims = dims,
        data = data.frame(),
        defVal = if (length(p@defVal)) p@defVal else NULL
      )
      n_par <- n_par + 1L
    }
  }

  if (verbose || n_map + n_par > 0L) {
    cat("  User-constraint symbols declared:", n_map, "mappings,",
        n_par, "parameters\n")
  }
  model
}

#' Split one GAMS statement into its header and body
#'
#' The IR is a single string holding `<header>.. <body>;`. The separator is the
#' first `..`; a decimal point cannot produce one.
#' @keywords internal
#' @noRd
.split_gams_statement <- function(txt) {
  s <- trimws(gsub("[\r\n]+", " ", txt))
  s <- sub(";\\s*$", "", s)
  at <- regexpr("\\.\\.", s, perl = TRUE)
  if (at < 0L) {
    stop("Not a GAMS equation - no '..' separating header from body:\n  ", s)
  }
  list(header = trimws(substr(s, 1L, at - 1L)),
       body   = trimws(substr(s, at + 2L, nchar(s))))
}

#' Parse one user-constraint IR string into a multimod equation
#'
#' @param txt The `equation` string of a `modInp@user_constraints` entry, or
#'   the whole `modInp@user_costs` string.
#' @param symbols A symbols list as built by `build_symbols_list()`.
#' @param desc Description to attach.
#' @keywords internal
#' @noRd
.parse_cns_equation <- function(txt, symbols, desc = "") {
  parts <- .split_gams_statement(txt)
  hdr <- multimod::parse_equation_header(paste0(parts$header, ".."))
  if (is.null(hdr)) {
    stop("Could not read the equation header:\n  ", parts$header)
  }
  multimod::parse_gams_equation(
    list(name = hdr$name, dims = hdr$dims, desc = desc,
         domain = hdr$condition, gams = parts$body),
    symbols
  )
}

#' Add a scenario's user constraints and user costs to a multimod model
#'
#' Declares the supporting `mCns*` / `pCns*` symbols, then parses each
#' constraint's GAMS intermediate form into an equation and appends it to
#' `model$equations`. The single `modInp@user_costs` string, which defines
#' `eqTotalUserCosts`, is parsed the same way.
#'
#' Called by [multimod_import_data()]; exported so a model can be extended
#' without a full re-import.
#'
#' @param model A multimod model.
#' @param scenario An energyRt scenario.
#' @param verbose Logical; report progress.
#'
#' @return The model, with the new equations and symbols.
#' @keywords internal
#' @noRd
add_user_constraints <- function(model, scenario, verbose = FALSE) {
  stopifnot(inherits(model, "multimod") || inherits(model, "model"))
  ucns <- scenario@modInp@user_constraints
  ucost <- scenario@modInp@user_costs
  has_cns <- length(ucns) > 0L
  has_cost <- length(ucost) == 1L && nzchar(ucost)
  if (!has_cns && !has_cost) {
    if (verbose) cat("  No user constraints or user costs in this scenario.\n")
    return(model)
  }

  model <- declare_user_constraint_symbols(model, scenario, verbose = verbose)
  symbols <- multimod::build_symbols_list(model)

  n_ok <- 0L
  failed <- character()
  for (nm in names(ucns)) {
    ir <- ucns[[nm]]
    if (is.null(ir$equation) || !nzchar(ir$equation)) {
      failed <- c(failed, paste0(nm, " (no equation string)"))
      next
    }
    eq <- tryCatch(
      .parse_cns_equation(ir$equation, symbols,
                          desc = paste("user constraint", nm)),
      error = function(e) {
        failed <<- c(failed, sprintf("%s (%s)", nm, conditionMessage(e)))
        NULL
      }
    )
    if (is.null(eq)) next
    model$equations[[eq$name]] <- eq
    n_ok <- n_ok + 1L
  }

  n_cost <- 0L
  if (has_cost) {
    eq <- tryCatch(
      .parse_cns_equation(ucost, symbols, desc = "total user-defined costs"),
      error = function(e) {
        failed <<- c(failed, sprintf("user_costs (%s)", conditionMessage(e)))
        NULL
      }
    )
    if (!is.null(eq)) {
      model$equations[[eq$name]] <- eq
      n_cost <- 1L
    }
  }

  cat("  User constraints added:", n_ok, "of", length(ucns),
      if (n_cost) "(+ user costs)" else "", "\n")
  if (length(failed)) {
    # A dropped constraint is a silently *relaxed* model, so this must not be
    # a quiet log line.
    stop("add_user_constraints(): ", length(failed),
         " user constraint(s) could not be parsed, which would silently relax",
         " the model:\n  ", paste(failed, collapse = "\n  "))
  }
  model
}
