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
  # Dropping model content must never be silent: count here, report once at the end.
  n_routes_dropped <- 0L
  n_objs_dropped <- 0L

  # The model's full declaration universe: its own regions plus every cell of
  # its geoscale (declarations may legitimately name a coarser level). The
  # pruning below drops only rows for regions INSIDE this universe but outside
  # the sample; a region the model never knew stays put, so the undeclared-
  # region guard (`.check_declared_regions()`, which runs after the sample) can
  # still name it instead of the sample silently swallowing the typo.
  declared <- as.character(mod@config@region)
  gs <- tryCatch(mod@config@geoscale, error = function(e) NULL)
  if (!is.null(gs) && is_geoscale(gs)) {
    declared <- unique(c(declared, unlist(lapply(
      geoscales::geoscale_geoframes(gs),
      function(f) as.character(geoscales::geoscale_regions(gs, f))))))
  }

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
          kept <- r[r %in% keep | !(r %in% declared)]
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
            v[is.na(v$region) | v$region %in% keep |
                !(v$region %in% declared), , drop = FALSE]
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
      n_routes_dropped <<- n_routes_dropped + 1L
      if (isTRUE(verbose)) message(
        "spatial sample: dropping trade route ", el@name, ": ",
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
          n_objs_dropped <<- n_objs_dropped + 1L
          if (isTRUE(verbose)) {
            message("spatial sample: removing ", class(el)[1], " ", nm,
                    if (methods::is(el, "trade")) " (no route inside the sample)"
                    else " (declared in no sampled region)")
          }
          r@data[[nm]] <- NULL
        } else {
          r@data[[nm]] <- res
        }
      }
    }
    r
  }
  for (i in seq_along(mod@data)) mod@data[[i]] <- walk(mod@data[[i]])

  # One line, always: what left the model. `verbose = TRUE` names each item.
  if (n_objs_dropped > 0L || n_routes_dropped > 0L) {
    message("spatial sample: kept ", length(keep), " region(s); dropped ",
            n_objs_dropped, " object(s) and ", n_routes_dropped,
            " trade route(s) outside the sample",
            if (!isTRUE(verbose)) " (verbose = TRUE lists them)" else "")
  }

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
    lo_col <- paste0(kind, ".lo")
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
    # the lower side of the window: a minimum contracted flow
    lo_src <- if (lo_col %in% names(bp)) bp[[lo_col]] else
      if ("cap.lo" %in% names(bp)) bp$cap.lo else NULL
    if (!is.null(lo_src) && any(!is.na(lo_src))) rows[[lo_col]] <- lo_src
    rows
  }

  stub <- if (kept_end == "dst") {
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

  # A stepped price curve instead of a flat price, when the window asks for one.
  # The exterior then has a rising cost of imports / falling revenue on exports
  # rather than being an infinitely elastic price taker. `asImportCurve()`
  # refuses an unbounded object and enforces the direction, so both are checked
  # for us -- but the range must be handed over in the right order: ASCENDING
  # for an import, DESCENDING for an export (export revenue is a negative cost,
  # so a rising curve would not be convex).
  .n <- suppressWarnings(as.integer((bp$nsteps %||% NA)[1]))
  .plo <- suppressWarnings(as.numeric((bp$price_lo %||% NA)[1]))
  .phi <- suppressWarnings(as.numeric((bp$price_hi %||% NA)[1]))
  if (!is.na(.n) && .n > 1L && !is.na(.plo) && !is.na(.phi)) {
    rng <- if (kept_end == "dst") c(.plo, .phi) else c(.phi, .plo)
    stub <- tryCatch({
      if (kept_end == "dst") {
        asImportCurve(stub, range = rng, nsteps = .n)
      } else {
        asExportCurve(stub, range = rng, nsteps = .n)
      }
    }, error = function(e) {
      warning("boundary stub ", src, "->", dst, ": could not build a ",
              .n, "-step price curve (", conditionMessage(e),
              "); falling back to the flat price.", call. = FALSE)
      stub
    })
  }
  stub
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

# =========================================================================== #
# The boundary trade WINDOW.
#
# A dropped route can be replaced by a priced import/export stub at the kept
# endpoint (see .boundary_stub()). A flat price gives the exterior infinite
# elasticity; the window below builds a stepped curve instead -- a rising cost
# of imports, a falling revenue on exports -- sized from the region's demand.
#
# The bound is per timeslice, not an annual budget. An annual per-region cap
# cannot be expressed: `@reserve` is an all-region horizon total, and
# `newConstraint()` collapses the region dimension when interpolating its RHS.
# A per-timeslice bound lands on `pImportRowUp`, and applied to each slice's
# own demand it sums over the year to `share` x annual demand.
# =========================================================================== #

# The demand profile of a model, summed over every demand object, at the
# resolution it was DECLARED. `NA` region is a wildcard broadcast across
# `regions`; `NA` timeslice is left as-is (it means "every slice" and is
# resolved by interpolation, not here).
#' @noRd
.region_demand_profile <- function(mod, regions = NULL) {
  stopifnot(is(mod, "model"))
  if (is.null(regions)) regions <- as.character(mod@config@region)
  regions <- as.character(regions)
  out <- list()
  for (rp in names(mod@data)) {
    repo <- mod@data[[rp]]
    if (!isS4(repo)) next
    for (nm in names(repo@data)) {
      o <- repo@data[[nm]]
      if (!is(o, "demand")) next
      d <- as.data.frame(o@demand)
      if (!nrow(d) || !"demand" %in% names(d)) next
      d <- d[!is.na(d$demand), , drop = FALSE]
      if (!nrow(d)) next
      if (!"region" %in% names(d)) d$region <- NA_character_
      if (!"year" %in% names(d)) d$year <- NA_integer_
      if (!"timeslice" %in% names(d)) d$timeslice <- NA_character_
      # a region-less row applies to every region the object is declared for,
      # falling back to the sample
      wild <- is.na(d$region)
      if (any(wild)) {
        own <- as.character(o@region)
        tgt <- if (length(own)) intersect(own, regions) else regions
        if (length(tgt)) {
          rep_rows <- d[rep(which(wild), each = length(tgt)), , drop = FALSE]
          rep_rows$region <- rep(tgt, times = sum(wild))
          d <- rbind(d[!wild, , drop = FALSE], rep_rows)
        } else {
          d <- d[!wild, , drop = FALSE]
        }
      }
      d <- d[d$region %in% regions, , drop = FALSE]
      if (!nrow(d)) next
      out[[length(out) + 1L]] <- data.frame(
        region = as.character(d$region), year = as.integer(d$year),
        timeslice = as.character(d$timeslice),
        demand = as.numeric(d$demand), stringsAsFactors = FALSE)
    }
  }
  if (!length(out)) {
    return(data.frame(region = character(0), year = integer(0),
                      timeslice = character(0), demand = numeric(0),
                      stringsAsFactors = FALSE))
  }
  res <- do.call(rbind, out)
  # NOT stats::aggregate(): it drops every row whose grouping variable is NA,
  # and `year`/`timeslice` are legitimately NA on a demand that applies to the
  # whole horizon and every slice -- the most common declaration shape.
  as.data.frame(dplyr::summarise(
    res, demand = sum(.data$demand, na.rm = TRUE),
    .by = c("region", "year", "timeslice")))
}

# Annualised demand per (region, year): each declared row weighted by its own
# timeslice weight, which is 1 on a full calendar and 1/year_fraction on a
# sampled one. Verified against `sum(pDemand * pTimesliceWeight)` of the
# interpolated scenario -- see test-region-window.R.
#' @noRd
.region_annual_demand <- function(mod, regions = NULL, calendar = NULL) {
  prof <- .region_demand_profile(mod, regions)
  if (!nrow(prof)) {
    return(data.frame(region = character(0), year = integer(0),
                      demand = numeric(0), stringsAsFactors = FALSE))
  }
  cal <- calendar %||% tryCatch(mod@config@calendar, error = function(e) NULL)
  w <- 1
  if (!is.null(cal) && is(cal, "calendar")) {
    ts <- as.data.frame(cal@timeslice_share)
    wv <- stats::setNames(as.numeric(ts$weight), as.character(ts$timeslice))
    w <- unname(wv[as.character(prof$timeslice)])
    w[is.na(w)] <- 1                 # NA timeslice, or a slice not in the map
  }
  prof$demand <- prof$demand * w
  as.data.frame(dplyr::summarise(
    prof, demand = sum(.data$demand, na.rm = TRUE),
    .by = c("region", "year")))
}

#' A generic boundary trade window
#'
#' @description
#' Builds a `boundary_prices` table for [subset_model_regions()] /
#' [solve_by_region()] sized from the model's own demand: each severed route is
#' replaced by a stepped import or export curve whose total quantity is
#' `share` of the region's demand in that timeslice.
#'
#' The quantity is sized from the model's own demand; the price is taken from
#' `price` and is not estimated from the model. Inspect and edit the returned
#' data.frame before use, or supply your own.
#'
#' @details
#' The bound is applied **per timeslice** (`share * demand[r, y, s]`), which is
#' where `pImportRowUp` lives. Summed over the year that is `share` of annual
#' demand, so the familiar "trade is at most 10% of consumption" reading holds
#' without needing an annual budget — which could not be expressed correctly
#' anyway (a per-region reserve does not exist, and a constraint RHS averages
#' across regions).
#'
#' @param mod a model object.
#' @param regions character, the regions being solved (the sample). Routes are
#'   priced at whichever endpoint is inside this set.
#' @param share numeric in `(0, 1]`, the fraction of a region's demand the
#'   boundary may supply or absorb. `0.1` by default.
#' @param nsteps integer, steps in the price curve; `1` gives a flat price.
#' @param price numeric, the reference price. With `nsteps > 1` the curve spans
#'   `price * (1 +/- spread)`, rising for imports and falling for exports.
#' @param spread numeric, the half-width of the curve as a fraction of `price`.
#' @param routes optional `data.frame(src, dst)` limiting which routes get a
#'   window; by default every route leaving the sample.
#'
#' @return a `data.frame` with the `boundary_prices` columns — `src`, `dst`,
#'   `price`, `cap.up`, and the curve columns `nsteps`, `price_lo`, `price_hi`.
#' @seealso [subset_model_regions()], [solve_by_region()], [asImportCurve()]
#' @export
boundary_window <- function(mod, regions, share = 0.1, nsteps = 3,
                            price = NULL, spread = 0.5, routes = NULL) {
  stopifnot(is(mod, "model"))
  if (!is.numeric(share) || length(share) != 1L || is.na(share) ||
      share <= 0 || share > 1) {
    stop("`share` must be one number in (0, 1].", call. = FALSE)
  }
  if (is.null(price) || !is.numeric(price) || is.na(price[1])) {
    stop("`boundary_window()` needs a reference `price`: the generic window ",
         "sizes the QUANTITY from demand but cannot invent a price. Pass ",
         "`price =`, or use trade = \"none\" for an autarky run.",
         call. = FALSE)
  }
  regions <- as.character(regions)
  price <- as.numeric(price)[1]
  nsteps <- max(1L, as.integer(nsteps))

  # every route with exactly one endpoint in the sample
  rr <- routes
  if (is.null(rr)) {
    rows <- list()
    for (rp in names(mod@data)) {
      repo <- mod@data[[rp]]
      if (!isS4(repo)) next
      for (nm in names(repo@data)) {
        o <- repo@data[[nm]]
        if (!is(o, "trade")) next
        rt <- as.data.frame(o@routes)
        if (!nrow(rt)) next
        cross <- xor(rt$src %in% regions, rt$dst %in% regions)
        if (any(cross)) {
          rows[[length(rows) + 1L]] <- data.frame(
            src = as.character(rt$src[cross]),
            dst = as.character(rt$dst[cross]),
            stringsAsFactors = FALSE)
        }
      }
    }
    rr <- if (length(rows)) unique(do.call(rbind, rows)) else NULL
  }
  if (is.null(rr) || !nrow(rr)) {
    return(data.frame(src = character(0), dst = character(0),
                      price = numeric(0), cap.up = numeric(0),
                      nsteps = integer(0), price_lo = numeric(0),
                      price_hi = numeric(0), stringsAsFactors = FALSE))
  }

  # the bound: `share` of the kept endpoint's demand, per timeslice
  prof <- .region_demand_profile(mod, regions)
  out <- list()
  for (i in seq_len(nrow(rr))) {
    src <- as.character(rr$src[i])
    dst <- as.character(rr$dst[i])
    kept <- if (dst %in% regions) dst else src
    imp <- identical(kept, dst)
    dd <- prof[prof$region == kept, , drop = FALSE]
    if (!nrow(dd)) {
      # no declared demand to size against: skip rather than invent a bound
      next
    }
    # one row per (year, timeslice) the region declares demand for
    lo <- if (imp) price else price * (1 - spread)
    hi <- if (imp) price * (1 + spread) else price
    out[[length(out) + 1L]] <- data.frame(
      src = src, dst = dst,
      year = dd$year, timeslice = dd$timeslice,
      price = price, cap.up = share * dd$demand,
      nsteps = nsteps, price_lo = lo, price_hi = hi,
      stringsAsFactors = FALSE)
  }
  if (!length(out)) {
    return(data.frame(src = character(0), dst = character(0),
                      price = numeric(0), cap.up = numeric(0),
                      nsteps = integer(0), price_lo = numeric(0),
                      price_hi = numeric(0), stringsAsFactors = FALSE))
  }
  do.call(rbind, out)
}
