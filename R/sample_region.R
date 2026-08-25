# =========================================================================== #
# Spatial sampling: the region mirror of calendar sampling.
#
# A SAMPLED GEOSCALE supplied at interpolation (a `geoscales::filter_geoscale()`
# subset of the model's regions) turns the scenario into a SUB-TERRITORY model:
# only the sampled regions are declared, parameters interpolated for other
# regions are filtered out (exactly as `.filter_params_by_declared_timeslices()`
# does for a sampled calendar), and trade routes crossing the sample boundary
# are dropped -- optionally replaced by priced import/export stubs at the kept
# endpoint. Unlike calendar sampling there is NO reweighting: regional
# quantities are extensive (see map_region.R header), so the objective is the
# sample's own, not an estimate of the full model's.
#
# A PRUNED geoscale (coarser atom layer) requests a full-territory solve at the
# parent level; that requires aggregating object data fine->parent and is a
# separate stage (`aggregate_model_regions()`, not yet implemented).
#
# Validation note, mirroring `.declaration_calendar()`: declarations are judged
# against the MODEL's own regions and geoscale, never against the sample --
# otherwise every sampled model would flag its out-of-sample declarations.
# =========================================================================== #

#' @include geoscale.R map_region.R
NULL

# Classify the geoscale on `settings` relative to the model's own regions.
# "full"      -- its atoms are exactly the model's regions (presentation +
#                multi-level hierarchy only; today's behaviour)
# "filtered"  -- its atoms are a strict subset (spatial sample)
# "pruned"    -- its atoms are a coarser level of the model's own geoscale
#' @noRd
.spatial_sample_mode <- function(gs, config) {
  check_package("geoscales")
  finest <- geoscales::geoscale_geoframes(gs, finest = TRUE)
  atoms  <- as.character(geoscales::geoscale_regions(gs, finest))
  r_m <- as.character(config@region)
  r_m <- r_m[!is.na(r_m) & nzchar(r_m)]
  if (length(r_m) == 0L || setequal(atoms, r_m)) return("full")
  if (all(atoms %in% r_m)) return("filtered")
  gm <- tryCatch(config@geoscale, error = function(e) NULL)
  if (!is.null(gm) && is_geoscale(gm)) {
    gmf <- geoscales::geoscale_geoframes(gm)
    if (finest %in% gmf[-length(gmf)] &&
        all(atoms %in% as.character(
          geoscales::geoscale_regions(gm, finest)))) {
      return("pruned")
    }
  }
  stop("The geoscale passed to interpolate_model() matches neither the ",
       "model's regions (", paste(utils::head(r_m, 4), collapse = ", "),
       if (length(r_m) > 4) ", ..." else "",
       ") nor a coarser level of the model's own geoscale. Its atoms: ",
       paste(utils::head(atoms, 4), collapse = ", "),
       if (length(atoms) > 4) ", ..." else "",
       ". A spatial sample must be built with geoscales::filter_geoscale() ",
       "or prune_geoscale() from the model's geoscale.", call. = FALSE)
}

# The regions the DECLARATIONS were written against: the model's own region
# set, widened by the model's own geoscale hierarchy. The twin of
# `.declaration_calendar()` -- a sampled geoscale on `settings` must never be
# the validation reference, or every sampled model would flag itself.
#' @noRd
.declaration_regions <- function(scen) {
  r <- as.character(tryCatch(scen@model@config@region,
                             error = function(e) NULL))
  r <- r[!is.na(r) & nzchar(r)]
  if (length(r) == 0L) {
    r <- as.character(scen@settings@region)
    r <- r[!is.na(r) & nzchar(r)]
  }
  h <- .declaration_geo_hierarchy(scen)
  if (is.null(h)) r else unique(c(r, h$region))
}

# The geo hierarchy the DECLARATIONS were written against (model's geoscale
# first, settings' as fallback) -- the geo twin of `.declaration_calendar()`.
#' @noRd
.declaration_geo_hierarchy <- function(scen) {
  gs <- tryCatch(scen@model@config@geoscale, error = function(e) NULL)
  if (is.null(gs) || !is_geoscale(gs)) {
    gs <- tryCatch(getGeoscale(scen@settings), error = function(e) NULL)
  }
  r <- as.character(tryCatch(scen@model@config@region,
                             error = function(e) NULL))
  r <- r[!is.na(r) & nzchar(r)]
  if (length(r) == 0L) r <- .model_regions(scen)
  .geo_hierarchy(gs, r)
}

#' Restrict a model to a region subset
#'
#' The spatial mirror of interpolating on a sampled calendar, usable up front:
#' declares only `region`, drops trade routes that cross the subset boundary
#' (with a message), and -- when `boundary_prices` supplies a price for a
#' dropped route -- replaces it with an import/export stub at the kept
#' endpoint. When the model carries a geoscale, it is filtered in step
#' (recording coverage; see `geoscales::geoscale_coverage()`).
#'
#' The same behaviour is available without this verb by passing a
#' `geoscales::filter_geoscale()` subset to [interpolate_model()].
#'
#' @param mod a [model].
#' @param region character vector of regions to keep (must be a subset of the
#'   model's declared regions).
#' @param boundary_prices optional `data.frame` pricing the boundary stubs of
#'   dropped routes, with columns `src`, `dst`, `price`, and optionally
#'   `trade` (object name; `NA` = any), `year`, `timeslice` (both `NA` = all),
#'   and `cap.up` (explicit stub bound overriding the derivation from the
#'   route's `ava.*` rows). Routes without a price row are dropped without a
#'   stub.
#' @param verbose print per-object messages.
#'
#' @return the restricted [model].
#' @family interpolation
#' @export
subset_model_regions <- function(mod, region, boundary_prices = NULL,
                                 verbose = isVerbose()) {
  stopifnot(inherits(mod, "model"))
  r_m <- as.character(mod@config@region)
  unknown <- setdiff(region, r_m)
  if (length(unknown) > 0L) {
    stop("region(s) not declared in the model: ",
         paste(unknown, collapse = ", "), call. = FALSE)
  }
  mod <- .subset_model_regions(mod, keep = region,
                               boundary_prices = boundary_prices,
                               verbose = verbose)
  mod@config@region <- r_m[r_m %in% region]
  gs <- tryCatch(mod@config@geoscale, error = function(e) NULL)
  if (!is.null(gs) && is_geoscale(gs)) {
    check_package("geoscales")
    finest <- geoscales::geoscale_geoframes(gs, finest = TRUE)
    atoms <- as.character(geoscales::geoscale_regions(gs, finest))
    keep_atoms <- intersect(atoms, region)
    if (length(keep_atoms) > 0L && length(keep_atoms) < length(atoms)) {
      mod@config@geoscale <- geoscales::filter_geoscale(
        gs, finest, keep_atoms, drop_empty_geoframes = TRUE)
    }
  }
  mod
}

# Trade surgery on the (build copy of the) model: drop routes crossing the
# boundary, filter route-level and endpoint-level rows, mint priced stubs.
#' @noRd
.subset_model_regions <- function(mod, keep, boundary_prices = NULL,
                                  verbose = isVerbose()) {
  keep <- as.character(keep)
  stubs <- list()

  prune_obj <- function(el, nm) {
    if (!methods::is(el, "trade")) {
      # A non-trade object declares its scope in `@region`. Narrowing the
      # scenario's region set without narrowing the DECLARATIONS leaves an
      # object claiming regions the sample no longer has, and validation
      # rejects it ("region(s) ... are not declared in the scenario region
      # set") -- so the sample has to do both.
      #
      # An empty `@region` is the wildcard "everywhere" and is left alone: it
      # broadcasts onto whatever the sample declares, which is already right.
      if (.hasSlot(el, "region")) {
        r <- methods::slot(el, "region")
        if (is.character(r) && length(r)) {
          kept <- r[r %in% keep]
          if (!length(kept)) return(NULL)      # nothing of it is in the sample
          if (length(kept) < length(r)) methods::slot(el, "region") <- kept
        }
      }
      # Narrowing the scope is only half of it: the DATA has to follow. A row
      # for a dropped region left in `@capacity` or `@af` is outside the
      # object's own scope, and validation says so ("appear in a slot but are
      # not in its @region scope"). NA stays -- it is the wildcard.
      for (sl in methods::slotNames(el)) {
        v <- methods::slot(el, sl)
        if (is.data.frame(v) && nrow(v) && "region" %in% names(v)) {
          methods::slot(el, sl) <-
            v[is.na(v$region) | v$region %in% keep, , drop = FALSE]
        }
      }
      return(el)
    }
    rt <- el@routes
    if (nrow(rt) == 0L) return(el)
    in_keep <- rt$src %in% keep & rt$dst %in% keep
    if (all(in_keep)) return(el)

    dropped <- rt[!in_keep, , drop = FALSE]
    for (i in seq_len(nrow(dropped))) {
      s <- as.character(dropped$src[i]); d <- as.character(dropped$dst[i])
      stub <- .boundary_stub(el, s, d, keep, boundary_prices)
      if (!is.null(stub)) stubs[[length(stubs) + 1L]] <<- stub
      message("spatial sample: dropping trade route ", el@name, ": ",
              s, " -> ", d,
              if (!is.null(stub)) paste0(" (", stub@name, " stub added)")
              else " (no boundary price; no stub)")
    }
    if (!any(in_keep)) return(NULL)          # whole object leaves the sample

    # keep only surviving routes in every route-indexed slot; endpoint-level
    # rows (invcost/fixom/vintage) keep NA wildcards -- per-endpoint rates
    # broadcast onto the sample and carry exactly its own cost share
    keep_route_rows <- function(df) {
      if (is.null(df) || nrow(df) == 0L ||
          !all(c("src", "dst") %in% names(df))) return(df)
      df[
        (is.na(df$src) | df$src %in% keep) &
          (is.na(df$dst) | df$dst %in% keep), , drop = FALSE]
    }
    keep_region_rows <- function(df) {
      if (is.null(df) || nrow(df) == 0L || !"region" %in% names(df)) return(df)
      df[is.na(df$region) | df$region %in% keep, , drop = FALSE]
    }
    el@routes <- rt[in_keep, , drop = FALSE]
    for (sl in c("trade", "aeff", "varom", "aux")) {
      if (.hasSlot(el, sl)) methods::slot(el, sl) <-
          keep_route_rows(methods::slot(el, sl))
    }
    for (sl in c("invcost", "fixom", "vintage")) {
      if (.hasSlot(el, sl)) methods::slot(el, sl) <-
          keep_region_rows(methods::slot(el, sl))
    }
    if (isTRUE(verbose) && nrow(el@capacity %||% data.frame()) > 0L) {
      message("spatial sample: trade ", el@name, " keeps its OBJECT-level ",
              "capacity bound while losing route(s); review @capacity")
    }
    el
  }

  walk <- function(r) {
    if (!methods::is(r, "repository")) return(r)
    for (nm in names(r@data)) {
      el <- r@data[[nm]]
      if (methods::is(el, "repository")) {
        r@data[[nm]] <- walk(el)
      } else {
        res <- prune_obj(el, nm)
        if (is.null(res)) {
          message("spatial sample: removing ", class(el)[1], " ", nm,
                  if (methods::is(el, "trade")) " (no route inside the sample)"
                  else " (declared in no sampled region)")
          r@data[[nm]] <- NULL
        } else {
          r@data[[nm]] <- res
        }
      }
    }
    r
  }
  for (i in seq_along(mod@data)) mod@data[[i]] <- walk(mod@data[[i]])

  if (length(stubs) > 0L) {
    repo <- newRepository("boundary_stubs")
    for (st in stubs) repo <- add(repo, st)
    mod <- add(mod, repo)
  }
  mod
}

# One stub (import at a kept dst / export at a kept src) for a dropped route,
# or NULL when no boundary price is given for it.
#' @noRd
.boundary_stub <- function(trd, src, dst, keep, boundary_prices) {
  if (is.null(boundary_prices) || nrow(boundary_prices) == 0L) return(NULL)
  bp <- boundary_prices
  hit <- bp$src == src & bp$dst == dst &
    (if ("trade" %in% names(bp)) is.na(bp$trade) | bp$trade == trd@name
     else TRUE)
  bp <- bp[hit & !is.na(bp$price), , drop = FALSE]
  if (nrow(bp) == 0L) return(NULL)

  kept_end <- if (dst %in% keep) "dst" else if (src %in% keep) "src" else
    return(NULL)                                 # neither endpoint kept

  # bound ladder: explicit cap.up > the route's ava rows > Inf (loud)
  route_rows <- trd@trade[
    !is.na(trd@trade$src) & trd@trade$src == src &
      !is.na(trd@trade$dst) & trd@trade$dst == dst, , drop = FALSE]
  mk_rows <- function(kind) {
    reg <- if (kind == "imp") dst else src
    up_col <- paste0(kind, ".up")
    rows <- data.frame(region = reg,
                       year = as.integer(bp$year %||% NA_integer_),
                       timeslice = as.character(bp$timeslice %||%
                                                  NA_character_),
                       price = bp$price,
                       stringsAsFactors = FALSE)
    if ("cap.up" %in% names(bp) && any(!is.na(bp$cap.up))) {
      rows[[up_col]] <- bp$cap.up
    } else if (nrow(route_rows) > 0L &&
               any(is.finite(route_rows$ava.up %||% NA)) ) {
      bound <- max(route_rows$ava.up, na.rm = TRUE)
      if (kind == "imp" && "teff" %in% names(route_rows) &&
          any(is.finite(route_rows$teff))) {
        bound <- bound * max(route_rows$teff, na.rm = TRUE)
        message("boundary stub ", src, "->", dst,
                ": import bound scaled by route teff")
      }
      rows[[up_col]] <- bound
    } else {
      message("boundary stub ", src, "->", dst, ": no cap.up and no route ",
              "ava.up -- the stub is priced but UNBOUNDED")
    }
    rows
  }

  if (kept_end == "dst") {
    newImport(name = paste0("IMP_", trd@name, "_", src, "2", dst),
              desc = paste0("boundary import stub for dropped route ",
                            src, " -> ", dst, " of ", trd@name),
              commodity = trd@commodity,
              import = mk_rows("imp"))
  } else {
    newExport(name = paste0("EXP_", trd@name, "_", src, "2", dst),
              desc = paste0("boundary export stub for dropped route ",
                            src, " -> ", dst, " of ", trd@name),
              commodity = trd@commodity,
              export = mk_rows("exp"))
  }
}

# Filter interpolated VALUE parameters to the scenario's DECLARED regions --
# the exact spatial twin of `.filter_params_by_declared_timeslices()` (see the
# rationale there). NA in a spatial column is a wildcard and always kept.
#' @noRd
.filter_params_by_declared_regions <- function(scen, verbose = FALSE) {
  rg <- scen@modInp@sets$region
  if (is.null(rg) || length(rg) == 0L) return(scen)
  rg <- as.character(rg)
  for (pn in names(scen@modInp@parameters)) {
    param <- scen@modInp@parameters[[pn]]
    if (is.null(param) || !(param@type %in% c("numpar", "bounds"))) next
    pdata <- get_data_slot(param)
    if (is.null(pdata) || nrow(pdata) == 0L) next
    scols <- intersect(c("region", "regionp", "src", "dst"), colnames(pdata))
    if (length(scols) == 0L) next
    keep <- Reduce(`&`, lapply(scols, function(cc) {
      v <- as.character(pdata[[cc]]); is.na(v) | v %in% rg
    }))
    if (all(keep)) next
    new_data <- as.data.frame(pdata)[keep, , drop = FALSE]
    if (isTRUE(verbose)) {
      message("region-filter '", pn, "': ", nrow(pdata), " -> ",
              nrow(new_data), " rows")
    }
    scen <- .interp_write_param(scen, pn, new_data)
  }
  scen
}
