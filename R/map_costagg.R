# =========================================================================== #
# map_costagg.R  —  top-level cost-aggregation mapping builders (family "cost_agg")
#
# One `map_<Name>(scen, fmp) -> scen` per cost-aggregation map. Registered in
# `.cost_agg_builders`. Reuses `.set_map` / get_data_slot from mapping_engine.R.
# =========================================================================== #

# Every map whose variable `eqCost` sums into `vTotalCost`. Read off the GLPK
# equation, so a new cost stream has to be added here too -- which is the point:
# the coarse-cell rule below must see every place a cost can land.
.COST_DOMAIN_MAPS <- c(
  "mDummyExportCost", "mDummyImportCost", "mExportIrCost", "mExportRowCost",
  "mImportIrCost", "mImportRowCost", "mStorageEac", "mStorageFixom",
  "mStorageRetCost", "mStorageVarom", "mSubCost", "mTaxCost", "mTechEac",
  "mTechFixom", "mTechRetCost", "mTechVarom", "mTradeEac", "mTradeFixom",
  "mTradeRetCost", "mvSupCost"
)

# Coarse (non-atomic) regions that a cost actually lands in, with the years they
# land in. Empty in a flat model, and empty in a model whose geoscale nobody uses
# for a cost -- so both stay bit-for-bit unchanged.
.coarse_cost_cells <- function(scen) {
  atomic <- .model_regions(scen)
  known  <- .known_regions(scen)
  coarse <- setdiff(known, atomic)
  empty  <- data.frame(region = character(), year = integer())
  if (length(coarse) == 0L) return(empty)

  out <- list()
  for (nm in intersect(.COST_DOMAIN_MAPS, names(scen@modInp@parameters))) {
    d <- get_data_slot(scen@modInp@parameters[[nm]])
    if (is.null(d) || NROW(d) == 0L) next
    d <- as.data.frame(d)
    if (!all(c("region", "year") %in% names(d))) next
    d <- d[as.character(d$region) %in% coarse, c("region", "year"), drop = FALSE]
    if (NROW(d)) out[[length(out) + 1L]] <- d
  }
  if (!length(out)) return(empty)
  d <- unique(do.call(rbind, out))
  d$region <- as.character(d$region)
  d$year <- as.integer(d$year)
  d[order(d$region, d$year), , drop = FALSE]
}

# Region x year grid: the domain of the system-cost variable.
#
# The atomic regions always -- costs accrue where processes are. PLUS any coarse
# geoscale cell a cost was actually declared at: `sets$region` carries those
# levels, so `eqTradeFixom` and friends will happily compute a cost there, and
# without a matching `mvTotalCost` row `eqCost` never visits it and the cost is
# silently dropped. A cost lands in exactly one cell, coarse or atomic, so adding
# the coarse ones cannot double-count.
.cost_region_year <- function(scen) {
  grid <- tidyr::expand_grid(
    region = .model_regions(scen),
    year   = as.integer(scen@modInp@sets[["year"]])
  ) |> as.data.frame()
  coarse <- .coarse_cost_cells(scen)
  if (NROW(coarse) == 0L) return(grid)
  unique(rbind(grid, coarse))
}

# mvTotalCost: total system cost domain = full region x year grid.
map_mvTotalCost <- function(scen, fmp) {
  .set_map(scen, "mvTotalCost", .cost_region_year(scen), fmp)
}

# mvTotalUserCosts: domain of user-defined cost constraints. For each user cost
# map (`mCosts*`), take its region/year footprint; collapse to the full grid when
# any cost spans it, otherwise union the per-cost footprints. Stays empty when the
# model declares no user costs (matching legacy).
map_mvTotalUserCosts <- function(scen, fmp) {
  dregionyear <- .cost_region_year(scen)
  cost_nms <- grep("^mCosts", names(scen@modInp@parameters), value = TRUE)
  footprints <- lapply(cost_nms, function(x) {
    xx <- get_data_slot(scen@modInp@parameters[[x]])
    if (is.null(xx) || nrow(xx) == 0) return(NULL)
    xx <- as.data.frame(xx) |>
      dplyr::select(dplyr::any_of(c("region", "year"))) |>
      dplyr::distinct()
    if (nrow(xx) == nrow(dregionyear) || ncol(xx) == 0) return(dregionyear)
    if (is.null(xx$region)) {
      return(dplyr::filter(dregionyear, .data$year %in% unique(xx$year)))
    } else if (is.null(xx$year)) {
      return(dplyr::filter(dregionyear, .data$region %in% unique(xx$region)))
    }
    xx
  })
  footprints <- Filter(Negate(is.null), footprints)
  df <- NULL
  if (any(vapply(footprints, nrow, integer(1)) == nrow(dregionyear))) {
    df <- dregionyear
  } else if (length(footprints) > 0) {
    df <- dplyr::distinct(dplyr::bind_rows(footprints))
  } else if (length(scen@modInp@user_costs) > 0) {
    # a cost term with no `subset` builds no mCosts* map but still spans the
    # whole grid -- without this the eqTotalUserCosts domain stayed empty and
    # the term silently never reached the objective
    df <- dregionyear
  }
  if (!is.null(df) && nrow(df) > 0) {
    scen <- .set_map(scen, "mvTotalUserCosts", df, fmp)
  }
  scen
}

# -- registry for the cost_agg family -------------------------------------- #
.cost_agg_builders <- list(
  mvTotalCost      = map_mvTotalCost,
  mvTotalUserCosts = map_mvTotalUserCosts
)


# Discount factor for a coarse cost cell.
#
# `eqObjective` weights every `mvTotalCost` row by `pDiscountFactor[r,y]`, which
# is built per region from the social discount rate (`R/obj2modInp.R`). A coarse
# cell has no rate of its own, so it inherits its children's -- but only when
# they agree. Where they differ there is no defensible answer, and averaging one
# would silently pick a number nobody declared, so name the regions and stop.
#
# Runs after the cost_agg recipe, once `mvTotalCost` knows which coarse cells
# exist. A no-op when there are none.
.extend_discount_to_coarse <- function(scen) {
  p <- scen@modInp@parameters[["pDiscountFactor"]]
  if (is.null(p)) return(scen)
  df <- as.data.frame(get_data_slot(p))
  if (is.null(df) || NROW(df) == 0L) return(scen)

  cells <- .coarse_cost_cells(scen)
  if (NROW(cells) == 0L) return(scen)

  h <- .scen_geo_hierarchy(scen)
  if (is.null(h) || is.null(h$family) || NROW(h$family) == 0L) return(scen)
  fam <- as.data.frame(h$family)   # region = parent, regionp = child

  add <- list()
  for (r in unique(cells$region)) {
    kids <- fam$regionp[as.character(fam$region) == r]
    if (!length(kids)) next
    yrs <- unique(cells$year[cells$region == r])
    sub <- df[as.character(df$region) %in% kids & df$year %in% yrs, , drop = FALSE]
    if (!NROW(sub)) next
    bad <- character()
    keep <- lapply(split(sub, sub$year), function(d) {
      v <- unique(round(d$value, 12))
      if (length(v) > 1L) {
        bad <<- c(bad, paste0(d$year[1], " (",
                              paste(sort(unique(as.character(d$region))),
                                    collapse = ", "), ")"))
        return(NULL)
      }
      data.frame(region = r, year = d$year[1], value = v[1])
    })
    if (length(bad)) {
      stop("A cost is declared at the coarse region '", r, "', but its child ",
           "regions do not share one discount factor in: ",
           paste(bad, collapse = "; "), ".
",
           "  A coarse cell has no discount rate of its own and energyRt will ",
           "not average one. Declare the cost at the child regions instead, or ",
           "give the children a common `sdr`.", call. = FALSE)
    }
    keep <- keep[!vapply(keep, is.null, logical(1))]
    if (length(keep)) add[[length(add) + 1L]] <- do.call(rbind, keep)
  }
  if (!length(add)) return(scen)
  # `.dat2par()` APPENDS to the parameter's data (`rbindlist(obj@data, data)`),
  # so only the NEW rows go in. Passing the existing frame back duplicates every
  # row, which GLPK rejects outright ("pDiscountFactor[R1,2020] already defined")
  # while the sparse path quietly folds it away.
  new_rows <- unique(do.call(rbind, add))
  key <- paste(df$region, df$year)
  new_rows <- new_rows[!paste(new_rows$region, new_rows$year) %in% key, , drop = FALSE]
  if (!NROW(new_rows)) return(scen)
  scen@modInp@parameters[["pDiscountFactor"]] <- .dat2par(p, new_rows)
  scen
}
