# ============================================================================ #
# The objective weight
#
# `eqObjective` is one weighted sum over the `(region, year)` cells of
# `mvTotalCost`:
#
#   vObjective = sum  vTotalCost[r,y] * pPeriodLen[y] * pDiscountFactor[r,y]
#                                     * pObjWeight[r,y]
#
# so a per-year QUALITY weight and a per-region PRIORITY weight are one more
# factor on the cell the objective already sums over, not two features. The two
# halves are declared separately and MULTIPLY:
#
#   `horizon@intervals$weight`  per milestone  (the year half)
#   `config@objweight`          per (region, year)  (the region half)
#
# One table could not express both: "2035 at 0.5 and R2 at 0.6" would force the
# user to spell out 0.3 for the cell. Two tables multiply, and either may be
# left out.
#
# WHOLE-CELL, nothing carved out. The weight multiplies the entire
# `vTotalCost[r,y]` -- capital, fuel, variable cost, levies and the debug
# penalties alike. The argument is within-cell neutrality: when every cost term
# in a cell scales by one factor, no trade-off inside that cell changes, so
# conditional on existing capacity the year's dispatch is exactly what it would
# have been unweighted, and the only thing reweighted is how much that cell
# counts in the cross-year investment decision -- which is the purpose.
# Weighting operating costs alone was considered and rejected: it halves a gas
# plant's fuel bill while barely touching a wind plant's, so it shifts the
# relative ranking of technologies INSIDE the down-weighted cell toward
# fuel-burning, and that fleet distortion persists through the capacity built.
# Exempting the shortage penalties fails for the same reason in the other
# direction: load-shedding would become relatively dearer in a down-weighted
# year.
#
# Carried deliberately: a down-weighted cell's capital is discounted along with
# its dispatch, so capital standing there is mildly cheaper and investment is
# nudged earlier. No scheme keeps the good information and drops only the bad;
# whole-cell buys neutrality at that price.
#
# NOT folded into `pDiscountFactor`, and not a column of `config@discount`:
# `cfg@discount` feeds the objective's discount factor AND, independently,
# `levcost()`'s own discounting (R/levcost.R), so a weight placed there would
# silently corrupt every levelised cost in the package while the objective
# still looked right.
#
# NOT interpolated. A declared weight applies to the cells it names; every cell
# it does not name reads the default 1. Interpolating between two declared
# years would invent a weight nobody declared -- the same reason
# `.extend_discount_to_coarse()` refuses to average a discount factor.
# ============================================================================ #

# The region half, read defensively: a config serialised before the slot
# existed does not carry it. Normalised to `(region, year, weight)` with the
# column types the rest of the file assumes.
#' @noRd
.cfg_objweight <- function(x) {
  empty <- data.frame(region = character(), year = integer(),
                      weight = numeric(), stringsAsFactors = FALSE)
  if (is.null(x)) return(empty)
  d <- if (methods::is(x, "config") || methods::is(x, "settings")) {
    if (!methods::.hasSlot(x, "objweight")) return(empty)
    tryCatch(methods::slot(x, "objweight"), error = function(e) NULL)
  } else x
  if (is.null(d) || !NROW(d)) return(empty)
  d <- as.data.frame(d)
  if (is.null(d[["weight"]])) {
    stop("`objweight` must have a `weight` column; got: ",
         paste(colnames(d), collapse = ", "), call. = FALSE)
  }
  out <- data.frame(
    region = if (is.null(d[["region"]])) NA_character_ else as.character(d$region),
    year = if (is.null(d[["year"]])) NA_integer_ else
      suppressWarnings(as.integer(d$year)),
    weight = suppressWarnings(as.numeric(d$weight)),
    stringsAsFactors = FALSE)
  .objweight_check(out$weight, "config@objweight$weight")
  out
}

# One rule for both halves, so the horizon column and the config table cannot
# disagree about what a legal weight is.
#' @noRd
.objweight_check <- function(w, what) {
  bad <- !is.na(w) & (!is.finite(w) | w <= 0)
  if (any(bad)) {
    stop("`", what, "` must be finite and > 0; got ",
         paste(unique(w[bad]), collapse = ", "),
         ".\n  Use NA (or 1) for a cell that counts in full. A weight of 0 ",
         "would leave those costs out of the objective entirely, so the ",
         "dispatch there would be cost-free and the solution arbitrary among ",
         "many; a negative weight would reward cost.", call. = FALSE)
  }
  invisible(TRUE)
}

# The year half: one weight per milestone, from `horizon@intervals$weight`.
#' @noRd
.hor_objweight <- function(h) {
  iv <- tryCatch(as.data.frame(h@intervals), error = function(e) NULL)
  if (is.null(iv) || !NROW(iv) || is.null(iv[["weight"]])) return(NULL)
  w <- suppressWarnings(as.numeric(iv$weight))
  .objweight_check(w, "horizon@intervals$weight")
  w[is.na(w)] <- 1
  if (all(w == 1)) return(NULL)
  stats::setNames(w, as.integer(iv$mid))
}

# `pObjWeight`, dense over the model's `(region, year)` grid, or NULL when
# nothing is declared.
#
# Dense rather than sparse once ANY weight is declared, for two reasons: GAMS
# reads an undeclared parameter cell as 0 rather than as the declared default,
# and `.extend_objweight_to_coarse()` judges a coarse cell by its children's
# rows, which must exist to be compared. Nothing declared returns NULL, no rows
# are written, and every backend reads the `defVal` of 1 -- so an unweighted
# model is bit-identical to one built before the weight existed.
#' @noRd
.obj_weight_cells <- function(app, regions, years) {
  regions <- unique(as.character(regions))
  years <- unique(as.integer(years))
  if (!length(regions) || !length(years)) return(NULL)
  yw <- .hor_objweight(app@horizon)
  rw <- .cfg_objweight(app)
  if (is.null(yw) && !NROW(rw)) return(NULL)

  grid <- expand.grid(region = regions, year = years,
                      KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  grid$value <- 1

  # the year half
  if (!is.null(yw)) {
    v <- yw[match(grid$year, as.integer(names(yw)))]
    v[is.na(v)] <- 1
    grid$value <- grid$value * as.numeric(v)
  }

  # the region half. MORE SPECIFIC WINS: a row naming both region and year
  # beats one naming a region, which beats one naming a year, which beats the
  # all-wildcard row. Two rows of the same specificity on one cell are
  # contradictory and refused rather than silently ordered.
  if (NROW(rw)) {
    rw$weight[is.na(rw$weight)] <- 1
    rw$spec <- (!is.na(rw$region)) * 2L + (!is.na(rw$year)) * 1L
    key <- paste(ifelse(is.na(rw$region), "*", rw$region),
                 ifelse(is.na(rw$year), "*", rw$year))
    if (anyDuplicated(key)) {
      dup <- unique(key[duplicated(key)])
      stop("`config@objweight` declares the same cell twice: ",
           paste(dup, collapse = "; "),
           ".\n  One weight per (region, year) cell; `*` is the wildcard.",
           call. = FALSE)
    }
    pick <- rep(NA_real_, nrow(grid))
    best <- rep(-1L, nrow(grid))
    for (i in seq_len(nrow(rw))) {
      hit <- (is.na(rw$region[i]) | grid$region == rw$region[i]) &
        (is.na(rw$year[i]) | grid$year == rw$year[i])
      take <- hit & rw$spec[i] > best
      pick[take] <- rw$weight[i]
      best[take] <- rw$spec[i]
    }
    # a region or year the model does not have is a declaration that reaches
    # nothing: reported, not silently dropped
    reach <- vapply(seq_len(nrow(rw)), function(i) {
      any((is.na(rw$region[i]) | grid$region == rw$region[i]) &
            (is.na(rw$year[i]) | grid$year == rw$year[i]))
    }, logical(1))
    if (any(!reach)) {
      warning("`config@objweight` has ", sum(!reach), " row(s) matching no ",
              "(region, year) cell of the model: ",
              paste(utils::head(key[!reach], 5), collapse = "; "),
              call. = FALSE)
    }
    pick[is.na(pick)] <- 1
    grid$value <- grid$value * pick
  }

  grid$year <- as.integer(grid$year)
  grid[, c("region", "year", "value")]
}

# The objective weight of a coarse cost cell.
#
# The twin of `.extend_discount_to_coarse()`, and for the same reason: a cost
# declared at a coarse region is one more `mvTotalCost` cell, and `eqObjective`
# weights it like any other. A coarse cell has no weight of its own, so it
# inherits its children's -- but only when they agree. Where they differ there
# is no defensible answer and averaging one would silently pick a number nobody
# declared, so name the regions and stop. A child with no row reads the default
# 1, which is what makes the common case (nothing declared anywhere below)
# silent rather than a refusal.
#' @noRd
.extend_objweight_to_coarse <- function(scen, skip = NULL) {
  p <- scen@modInp@parameters[["pObjWeight"]]
  if (is.null(p)) return(scen)
  dv <- suppressWarnings(as.numeric(p@defVal[[1]]))
  if (!length(dv) || !is.finite(dv)) dv <- 1
  df <- as.data.frame(get_data_slot(p))
  if (is.null(df)) df <- data.frame(region = character(), year = integer(),
                                    value = numeric())

  cells <- .coarse_cost_cells(scen)
  if (NROW(skip)) {
    cells <- cells[!paste(cells$region, cells$year) %in%
                     paste(skip$region, skip$year), , drop = FALSE]
  }
  if (NROW(cells) == 0L) return(scen)

  h <- .scen_geo_hierarchy(scen)
  if (is.null(h) || is.null(h$family) || NROW(h$family) == 0L) return(scen)
  fam <- as.data.frame(h$family)   # region = parent, regionp = child

  add <- list()
  bad <- character()
  for (r in unique(cells$region)) {
    if (any(as.character(df$region) == r)) next   # declares its own weight
    kids <- fam$regionp[as.character(fam$region) == r]
    if (!length(kids)) next
    for (y in unique(cells$year[cells$region == r])) {
      # every child, not just the ones with rows: an absent row reads the
      # default, and a 0.5 among absent children is a disagreement, not a
      # consensus of 0.5
      v <- df$value[match(paste(kids, y), paste(df$region, df$year))]
      v[is.na(v)] <- dv
      u <- unique(round(v, 12))
      if (length(u) > 1L) {
        bad <- c(bad, paste0(r, " ", y, " (",
                             paste(sort(unique(as.character(kids))),
                                   collapse = ", "), ")"))
        next
      }
      if (u[1] == dv) next   # nothing to write: the default already says it
      add[[length(add) + 1L]] <- data.frame(region = r, year = as.integer(y),
                                            value = u[1])
    }
  }
  if (length(bad)) {
    stop("A cost is declared at a coarse region whose child regions do not ",
         "share one objective weight: ", paste(bad, collapse = "; "),
         ".\n  A coarse cell has no weight of its own and energyRt will not ",
         "average one. Declare the weight at the coarse region too, declare ",
         "the cost at the child regions instead, or give the children a ",
         "common weight.", call. = FALSE)
  }
  if (!length(add)) return(scen)
  new_rows <- unique(do.call(rbind, add))
  key <- paste(df$region, df$year)
  new_rows <- new_rows[!paste(new_rows$region, new_rows$year) %in% key, ,
                       drop = FALSE]
  if (!NROW(new_rows)) return(scen)
  scen@modInp@parameters[["pObjWeight"]] <- .dat2par(p, new_rows)
  scen
}


# ---------------------------------------------------------------------------- #
# The setter
# ---------------------------------------------------------------------------- #

#' Weight the objective by milestone year and by region
#'
#' @description
#' Sets how much each `(region, year)` cell of total system cost counts in the
#' objective. Two halves, declared separately and multiplied: `years` writes
#' `horizon@intervals$weight`, `regions` writes `config@objweight`, and
#' interpolation combines them into `pObjWeight[region, year]`.
#'
#' @details
#' The usual reason to weight a year is uneven time resolution. A milestone
#' solved on a SAMPLE of the model calendar still carries full-year cost at
#' full period weight -- `sample_calendar()` annualises every sample -- so a
#' cheaply resolved interim year counts in the objective exactly as much as a
#' fully resolved one, and several of them can dominate the sum. A weight below
#' 1 says the estimate deserves less trust.
#'
#' The usual reason to weight a region is that the study does not value every
#' region's costs equally: differences in income level or purchasing power, a
#' region modelled only as context, or a priority stated deliberately.
#'
#' The weight scales the WHOLE cost cell -- capital, fuel, variable cost,
#' levies, penalties. That is what makes it safe: when every cost term in a
#' cell scales by one factor, no trade-off inside the cell changes, so the
#' cell's dispatch is what it would have been unweighted and only its share of
#' the cross-cell investment decision moves. Weighting operating cost alone
#' would shift the merit order inside the down-weighted cell toward
#' fuel-burning plant. What the weight does change is capital: a down-weighted
#' year's capital is charged less, which nudges investment earlier.
#'
#' Weights are not interpolated. A cell no declaration names counts in full.
#'
#' After a solve, [objective_weights()] reports the weighted objective beside
#' the same sum recomputed without the weights, so the two can be compared
#' without solving twice.
#'
#' @param obj a `model`, `config` or `scenario`.
#' @param years the year half: a named numeric over milestone years
#'   (`c("2035" = 0.5, "2045" = 0.5)`), an unnamed numeric of one weight per
#'   milestone, or `NULL` to leave it alone. `NA` or 1 means the milestone
#'   counts in full.
#' @param regions the region half: a named numeric over regions
#'   (`c(R1 = 1, R2 = 0.6)`), a `data.frame(region, year, weight)` for weights
#'   that differ by year as well, or `NULL` to leave it alone.
#' @param reset logical; `TRUE` clears the half (or halves) named by a `NULL`
#'   argument instead of leaving it alone. `setObjectiveWeights(mod, reset =
#'   TRUE)` removes every weight.
#' @param ... unused, for method consistency.
#'
#' @return the object, with the weights written.
#' @seealso [objective_weights()] to read them back after a solve,
#'   [sample_calendar()] and [setCalendar()] for the uneven-resolution case the
#'   year half exists for.
#' @export
#' @examples
#' mod <- newModel("w", horizon = newHorizon(2030:2049, intervals = 5),
#'                 region = c("R1", "R2"))
#' mid <- getHorizon(mod)@intervals$mid
#' # the interim milestones are solved on a sample, so trust them half
#' w <- stats::setNames(rep(0.5, length(mid) - 2L), mid[-c(1, length(mid))])
#' mod <- setObjectiveWeights(mod, years = w)
#' getHorizon(mod)@intervals[, c("mid", "weight")]
#' # and R2's costs count for 60%
#' mod <- setObjectiveWeights(mod, regions = c(R2 = 0.6))
#' mod@config@objweight
setGeneric("setObjectiveWeights",
           function(obj, ...) standardGeneric("setObjectiveWeights"))

#' @rdname setObjectiveWeights
#' @export
setMethod("setObjectiveWeights", signature(obj = "config"),
          function(obj, years = NULL, regions = NULL, reset = FALSE, ...) {
  .set_objweight_on(obj, years, regions, reset)
})

#' @rdname setObjectiveWeights
#' @export
setMethod("setObjectiveWeights", signature(obj = "model"),
          function(obj, years = NULL, regions = NULL, reset = FALSE, ...) {
  obj@config <- .set_objweight_on(obj@config, years, regions, reset)
  obj
})

#' @rdname setObjectiveWeights
#' @export
setMethod("setObjectiveWeights", signature(obj = "scenario"),
          function(obj, years = NULL, regions = NULL, reset = FALSE, ...) {
  # A scenario's settings are what the solve reads; `@model` keeps the model as
  # given, so both are written or the two disagree about the weights.
  obj@settings <- .set_objweight_on(obj@settings, years, regions, reset)
  if (methods::is(tryCatch(obj@model, error = function(e) NULL), "model")) {
    obj@model <- setObjectiveWeights(obj@model, years = years,
                                     regions = regions, reset = reset)
  }
  obj
})

# The one implementation behind the three methods.
#' @noRd
.set_objweight_on <- function(cfg, years, regions, reset = FALSE) {
  # --- the year half -> horizon@intervals$weight
  if (!is.null(years) || isTRUE(reset)) {
    h <- cfg@horizon
    iv <- as.data.frame(h@intervals)
    mid <- as.integer(iv$mid)
    if (is.null(years)) {
      iv$weight <- NULL
    } else {
      w <- suppressWarnings(as.numeric(years))
      if (length(w) != length(years) || any(is.na(w) & !is.na(years))) {
        stop("`years` must be numeric", call. = FALSE)
      }
      .objweight_check(w, "years")
      if (!is.null(names(years)) && any(nzchar(names(years)))) {
        y <- suppressWarnings(as.integer(names(years)))
        if (any(is.na(y))) {
          stop("`years` must be named by milestone year, e.g. ",
               "c(\"2035\" = 0.5)", call. = FALSE)
        }
        miss <- setdiff(y, mid)
        if (length(miss)) {
          stop("`years` names ", paste(miss, collapse = ", "),
               ", which the horizon has no milestone for. Milestones: ",
               paste(mid, collapse = ", "), call. = FALSE)
        }
        cur <- if (is.null(iv[["weight"]])) rep(1, nrow(iv)) else
          as.numeric(iv$weight)
        cur[is.na(cur)] <- 1
        cur[match(y, mid)] <- w
        iv$weight <- cur
      } else {
        if (length(w) == 1L) w <- rep(w, nrow(iv))
        if (length(w) != nrow(iv)) {
          stop("`years` is unnamed, so it must give one weight per milestone (",
               nrow(iv), ") or a single weight for all; got ", length(w),
               ".\n  Name it by year instead to weight some milestones only.",
               call. = FALSE)
        }
        iv$weight <- w
      }
    }
    .check_intervals(iv)
    h@intervals <- data.table::as.data.table(iv)
    cfg@horizon <- h
  }

  # --- the region half -> config@objweight
  if (!is.null(regions) || isTRUE(reset)) {
    if (is.null(regions)) {
      cfg@objweight <- .cfg_objweight(NULL)
    } else {
      d <- if (is.data.frame(regions)) {
        .cfg_objweight(regions)
      } else {
        w <- suppressWarnings(as.numeric(regions))
        nm <- names(regions)
        if (is.null(nm) || !all(nzchar(nm))) {
          stop("`regions` must be named by region, e.g. c(R1 = 1, R2 = 0.6), ",
               "or a data.frame(region, year, weight)", call. = FALSE)
        }
        .cfg_objweight(data.frame(region = nm, year = NA_integer_,
                                  weight = w, stringsAsFactors = FALSE))
      }
      known <- as.character(cfg@region)
      if (length(known)) {
        miss <- setdiff(stats::na.omit(d$region), known)
        if (length(miss)) {
          stop("`regions` names ", paste(miss, collapse = ", "),
               ", which the model does not declare. Regions: ",
               paste(known, collapse = ", "), call. = FALSE)
        }
      }
      cfg@objweight <- d
    }
  }
  cfg
}


# ---------------------------------------------------------------------------- #
# The readout
# ---------------------------------------------------------------------------- #

#' The objective, weighted and unweighted
#'
#' @description
#' Recomputes `eqObjective` from a solved scenario, with and without
#' `pObjWeight`, so a weighted run can be read beside the net present value it
#' would have reported unweighted. No second solve: both sums are decoded from
#' `vTotalCost`, which the solution carries per `(region, year)` cell.
#'
#' @details
#' `weighted` is the identity the solver optimised,
#' `sum(vTotalCost * pPeriodLen * pDiscountFactor * pObjWeight)`, and should
#' equal the reported `vObjective` -- a mismatch is a defect, and
#' [verify_solution()] checks it as one. `unweighted` is the same sum with the
#' weight omitted: the net present value of the system as costed, which is the
#' number to report when the weights were a device for uneven resolution rather
#' than a statement about what the costs are worth.
#'
#' The two differ only in how cells are added up. Every cost in the solution is
#' in real money either way -- the weight lives in the summation, not in any
#' variable -- so nothing else needs reinterpreting. energyRt exports no duals,
#' so there are no weighted prices to divide out.
#'
#' @param scen a solved `scenario`.
#'
#' @return an object of class `objective_weights` (a list with a `print`
#'   method): `objective` as the solver reported it, the recomputed `weighted`
#'   and `unweighted` sums, their `ratio`, and `cells`, one row per
#'   `(region, year)` with its cost, period length, discount factor and weight.
#' @seealso [setObjectiveWeights()] to declare the weights,
#'   [verify_solution()] for the full set of post-solve identities.
#' @export
objective_weights <- function(scen) {
  if (!methods::is(scen, "scenario")) {
    stop("`scen` must be a scenario", call. = FALSE)
  }
  dom <- .vs_par(scen, "mvTotalCost")
  if (is.null(dom) || nrow(dom) == 0) {
    stop("`mvTotalCost` is absent or empty: the scenario is not interpolated.",
         call. = FALSE)
  }
  dom <- unique(dom[, c("region", "year"), drop = FALSE])
  tc <- .vs_value_at(scen, "vTotalCost", dom)
  plen <- .vs_par_at(scen, "pPeriodLen", dom)
  dfac <- .vs_par_at(scen, "pDiscountFactor", dom)
  w <- .vs_par_at(scen, "pObjWeight", dom)
  obj <- .vs_var(scen, "vObjective")
  cells <- as.data.frame(dom)
  cells$cost <- tc
  cells$period_len <- plen
  cells$discount <- dfac
  cells$weight <- w
  cells$weighted <- tc * plen * dfac * w
  cells$unweighted <- tc * plen * dfac
  out <- list(
    objective = if (is.null(obj) || !nrow(obj)) NA_real_ else
      as.numeric(obj$value[1]),
    weighted = sum(cells$weighted),
    unweighted = sum(cells$unweighted),
    weights_declared = any(w != 1),
    cells = cells[order(cells$region, cells$year), , drop = FALSE])
  out$ratio <- if (out$unweighted == 0) NA_real_ else
    out$weighted / out$unweighted
  structure(out, class = "objective_weights")
}

#' @method print objective_weights
#' @export
print.objective_weights <- function(x, ...) {
  cat("Objective, weighted and unweighted\n")
  cat("  reported vObjective :", format(x$objective), "\n")
  cat("  recomputed weighted :", format(x$weighted), "\n")
  cat("  recomputed unweight.:", format(x$unweighted), "\n")
  if (isTRUE(x$weights_declared)) {
    cat("  weighted / unweighted:", format(x$ratio), "\n")
    w <- x$cells[x$cells$weight != 1, c("region", "year", "weight"),
                 drop = FALSE]
    cat("  cells weighted      :", nrow(w), "of", nrow(x$cells), "\n")
    if (nrow(w)) {
      print(utils::head(w, 10), row.names = FALSE)
      if (nrow(w) > 10) cat("  ... and", nrow(w) - 10, "more\n")
    }
  } else {
    cat("  no weight declared: pObjWeight is 1 everywhere\n")
  }
  invisible(x)
}
