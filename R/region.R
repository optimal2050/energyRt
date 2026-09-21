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
# parent level; that requires aggregating object data fine->parent, which is a
# separate stage -- `aggregate_model_regions()` in aggregate_region.R.
#
# Validation note, mirroring `.declaration_calendar()`: declarations are judged
# against the MODEL's own regions and geoscale, never against the sample --
# otherwise every sampled model would flag its out-of-sample declarations.
# =========================================================================== #

#' @include geoscale.R mapping-builders.R
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
# Weather names surviving in a model, and the pruning of references to those
# that did not.
#
# Dropping a region-scoped weather object leaves its NAME behind in every
# process that referenced it (a `weather` column in one of the process's
# data.frame slots). `.check_declared_objects()` then refuses the model before
# it is ever interpolated: "references weather object(s) that are not in the
# repository". For resource-cluster models this is the normal case rather than
# an edge one -- narrowing to one region drops every other region's clusters
# while the cluster technologies go on naming them -- so pruning the references
# is the other half of dropping the objects.
#
# NA is the wildcard and stays. Returns the model and counts what it cut.
.prune_weather_refs <- function(mod, verbose = FALSE) {
  declared <- character(0)
  collect <- function(r) {
    if (!methods::is(r, "repository")) {
      if (methods::is(r, "weather")) declared <<- c(declared, r@name)
      return(invisible(NULL))
    }
    for (el in r@data) collect(el)
  }
  for (el in mod@data) collect(el)

  n_refs <- 0L
  dropped_names <- character(0)
  prune <- function(el) {
    if (methods::is(el, "repository")) {
      for (nm in names(el@data)) el@data[[nm]] <- prune(el@data[[nm]])
      return(el)
    }
    if (!isS4(el) || methods::is(el, "weather")) return(el)
    for (sn in .instance_slots(el)) {
      if (identical(sn, "misc")) next
      v <- methods::slot(el, sn)
      if (!is.data.frame(v) || !nrow(v) || !("weather" %in% colnames(v))) next
      w <- as.character(v[["weather"]])
      bad <- !is.na(w) & nzchar(w) & !(w %in% declared)
      if (any(bad)) {
        n_refs <<- n_refs + sum(bad)
        dropped_names <<- c(dropped_names, unique(w[bad]))
        methods::slot(el, sn) <- v[!bad, , drop = FALSE]
      }
    }
    el
  }
  for (i in seq_along(mod@data)) mod@data[[i]] <- prune(mod@data[[i]])

  if (n_refs > 0L) {
    message("spatial sample: dropped ", n_refs, " weather reference(s) to ",
            length(unique(dropped_names)), " object(s) outside the sample",
            if (isTRUE(verbose))
              paste0(": ", paste(unique(dropped_names), collapse = ", "))
            else " (verbose = TRUE lists them)")
  }
  mod
}

.subset_model_regions <- function(mod, keep, boundary_prices = NULL,
                                  verbose = isVerbose()) {
  # legacy models: storages serialized before the `inp2stg` slot would error
  # in the slot walk below ("no slot of name"); same shim as interp_mod()
  mod <- .upgrade_model_storages(mod)
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

  # A `weather` object may legitimately be declared at a COARSER region than
  # the sample: one adm1 profile feeding its adm2 children (mWeatherRegionAt).
  # Judging it by `keep` alone drops the parent, `.prune_weather_refs()` then
  # strips the surviving child's link, and the process silently loses its
  # availability limit altogether. Weather is therefore scoped against the
  # sample PLUS its ancestors.
  keep_wx <- keep
  if (!is.null(gs) && is_geoscale(gs)) {
    h <- tryCatch(.geo_hierarchy(gs, keep), error = function(e) NULL)
    if (!is.null(h)) keep_wx <- unique(c(keep, as.character(h$region)))
  }

  prune_obj <- function(el, nm) {
    if (!methods::is(el, "trade")) {
      keep <- if (methods::is(el, "weather")) keep_wx else keep
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

  # The objects are gone; now the references to them.
  mod <- .prune_weather_refs(mod, verbose = verbose)

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
# timeslice weight -- 1 on a full calendar and at the top (ANNUAL) slice,
# 1/year_fraction on the sub-annual slices of a sampled one. Verified against
# `sum(pDemand * pTimesliceWeight)` of the interpolated scenario -- see
# test-region-window.R.
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


# ---------------------------------------------------------------------------
# (was R/aggregate_region.R, merged 2026-09-20)
# ---------------------------------------------------------------------------

# =========================================================================== #
# Region aggregation: a coarser model built from a finer one.
#
# The stage `interpolate_model()` names when it is handed a pruned geoscale.
# Every region-keyed slot is recast from the model's own regions onto a coarser
# geoframe: extensive quantities add up, intensive ones take a weighted mean,
# and paired-region slots (trade) drop the routes that become internal.
#
# Objects that span many regions -- a technology, a supply, a demand -- keep
# their identity and only their data moves. Trade objects are one per region
# pair, so those MERGE: several fine corridors become one coarse corridor.
# =========================================================================== #

#' @include geoscale.R
NULL

# Columns that identify a row rather than measure it. `year`, `vintage` and
# `timeslice` are grouping keys, not quantities to be averaged.
.AGG_ID_COLS <- c("region", "src", "dst", "year", "vintage", "cluster",
                  "timeslice", "comm", "acomm", "group", "desc")

# Extensive: the quantity of a parent is the total of its children. Anchored on
# both sides so `ncap2ainp` and `cap2ainp` -- emission coefficients -- do not
# match the capacity family.
.AGG_EXTENSIVE_RE <- paste0(
  "(^|\\.)(stock|cap|ncap|ret)(\\.|$)",
  "|(^|\\.)ava(\\.|$)",
  "|^res\\.",
  "|^demand$",
  "|^(imp|exp)\\."
)

# Structural: the same value for every region of the group, and a conflict is
# an error rather than something to average.
.AGG_COPY_COLS <- c("start", "end", "olife", "order")

#' Aggregation rule for each value column
#' @noRd
.agg_rules <- function(cols) {
  r <- ifelse(cols %in% .AGG_COPY_COLS, "copy",
              ifelse(grepl(.AGG_EXTENSIVE_RE, cols), "sum", "weighted_mean"))
  stats::setNames(r, cols)
}

#' Value columns of a slot: numeric, and not an identifier
#' @noRd
.agg_values <- function(df) {
  num <- names(df)[vapply(df, is.numeric, logical(1))]
  setdiff(num, .AGG_ID_COLS)
}

# One weight per region per object -- a single size, not a time series.
# A year- or vintage-varying weight would make an efficiency's weighting
# depend on which year it is read for; a single size per region does not.
#' @noRd
.agg_weight <- function(obj) {
  pick <- function(slot, cols) {
    if (!.hasSlot(obj, slot)) return(NULL)
    df <- methods::slot(obj, slot)
    if (!is.data.frame(df) || !nrow(df) || !"region" %in% names(df)) return(NULL)
    cols <- intersect(cols, names(df))
    if (!length(cols)) return(NULL)
    v <- rowSums(as.matrix(df[, cols, drop = FALSE]), na.rm = TRUE)
    keep <- !is.na(df$region)
    if (!any(keep)) return(NULL)
    agg <- stats::aggregate(list(.agg_w = v[keep]),
                            by = list(region = df$region[keep]),
                            FUN = function(z) sum(z, na.rm = TRUE))
    if (all(!is.finite(agg$.agg_w) | agg$.agg_w == 0)) return(NULL)
    agg
  }
  cl <- class(obj)[1]
  switch(
    cl,
    technology = pick("capacity", c("stock", "cap.up", "cap.fx")),
    storage = pick("capacity", c("out.stock", "out.cap.up", "out.cap.fx")),
    trade = pick("trade", c("ava.up", "ava.fx")),
    supply = pick("supply", c("ava.up", "ava.fx")),
    demand = pick("demand", "demand"),
    import = pick("import", c("imp.up", "imp.fx")),
    export = pick("export", c("exp.up", "exp.fx")),
    NULL
  )
}

# An NA region is the wildcard "every region". For an intensive quantity that
# survives aggregation unchanged -- the weighted mean of one value is that
# value -- so the wildcard is left alone. An extensive one does not: a parent's
# total is the sum over its children, which differs by how many it has, so
# those rows are written out per region first.
#' @noRd
.agg_expand_wildcards <- function(df, regions, ext_cols) {
  if (!"region" %in% names(df) || !length(ext_cols)) return(df)
  wc <- which(is.na(df$region))
  if (!length(wc)) return(df)
  has_ext <- vapply(wc, function(i)
    any(!is.na(unlist(df[i, ext_cols, drop = FALSE]))), logical(1))
  wc <- wc[has_ext]
  if (!length(wc)) return(df)
  ex <- df[rep(wc, each = length(regions)), , drop = FALSE]
  ex$region <- rep(regions, times = length(wc))
  rbind(df[-wc, , drop = FALSE], ex)
}

#' Recast one region-keyed slot onto the coarser geoframe
#' @noRd
.agg_slot <- function(df, gs, level, w, regions) {
  if (!is.data.frame(df) || !nrow(df)) return(df)
  vals <- .agg_values(df)
  rules <- .agg_rules(vals)

  # A slot with nothing to measure -- @routes -- is a set of keys: remap and
  # take the distinct rows.
  if (!length(vals)) {
    df$region <- .agg_map_codes(df$region, gs, level)
    return(unique(df[!is.na(df$region), , drop = FALSE]))
  }

  df <- .agg_expand_wildcards(df, regions, vals[rules == "sum"])
  wild <- df[is.na(df$region), , drop = FALSE]
  df <- df[!is.na(df$region), , drop = FALSE]
  if (!nrow(df)) return(wild)

  df$.agg_w <- if (is.null(w)) 1 else
    w$.agg_w[match(df$region, w$region)]
  df$.agg_w[is.na(df$.agg_w)] <- 0

  out <- geoscales::recast_from_geoatoms(
    df, gs, to = level, key = "region", values = vals, rule = rules,
    weight = ".agg_w", na_rm = TRUE, na_action = "drop")
  out <- as.data.frame(out)
  names(out)[names(out) == level] <- "region"
  out <- out[, intersect(names(df), names(out)), drop = FALSE]

  if (nrow(wild)) {
    wild$.agg_w <- NULL
    out <- rbind(out[, names(wild), drop = FALSE], wild)
  }
  rownames(out) <- NULL
  out
}

#' Map fine region codes to their code at `level`
#' @noRd
.agg_map_codes <- function(x, gs, level) {
  lt <- as.data.frame(geoscales::geoscale_leaftable(gs))
  fine <- geoscales::geoscale_geoframes(gs, finest = TRUE)
  as.character(lt[[level]])[match(as.character(x), as.character(lt[[fine]]))]
}

# -- trade ------------------------------------------------------------------- #
# A trade object is one region pair, so aggregation merges objects rather than
# moving rows inside one. Objects merge only when they agree on what they carry
# (commodity) and on how they are split (cluster names): corridors with
# different loss-tranche structures cannot share one set of tranche shares.

#' The object name with its `_<src>__<dst>` tail removed
#' @noRd
.agg_trade_prefix <- function(obj) {
  nm <- obj@name
  rt <- obj@routes
  if (is.data.frame(rt) && nrow(rt)) {
    tl <- paste0("_", rt$src[1], "__", rt$dst[1])
    if (endsWith(nm, tl)) return(substr(nm, 1L, nchar(nm) - nchar(tl)))
  }
  "TRD"
}

#' Coarse endpoints of a trade object, or NULL when it becomes internal
#' @noRd
.agg_trade_pair <- function(obj, gs, level) {
  rt <- obj@routes
  if (!is.data.frame(rt) || !nrow(rt)) return(NULL)
  s <- .agg_map_codes(rt$src, gs, level)
  d <- .agg_map_codes(rt$dst, gs, level)
  keep <- !is.na(s) & !is.na(d) & s != d
  if (!any(keep)) return(NULL)
  data.frame(src = pmin(s[keep], d[keep]), dst = pmax(s[keep], d[keep]),
             stringsAsFactors = FALSE) |> unique()
}

#' Group key: objects sharing one merge into a single coarse corridor
#' @noRd
.agg_trade_key <- function(obj, pair) {
  paste(c(.agg_trade_prefix(obj),
          paste(sort(as.character(obj@commodity)), collapse = ","),
          paste(sort(as.character(obj@cluster$cluster)), collapse = ","),
          sort(paste(pair$src, pair$dst, sep = "|"))),
        collapse = "::")
}

#' Recast one src/dst-keyed slot of the merged corridors
#' @noRd
.agg_pair_slot <- function(df, gs, level, w) {
  if (!is.data.frame(df) || !nrow(df)) return(df)
  vals <- .agg_values(df)
  if (!length(vals)) {
    df$src <- .agg_map_codes(df$src, gs, level)
    df$dst <- .agg_map_codes(df$dst, gs, level)
    df <- df[!is.na(df$src) & !is.na(df$dst) & df$src != df$dst, , drop = FALSE]
    return(unique(df))
  }
  df$.agg_w <- if (is.null(w)) 1 else
    w$.agg_w[match(paste(df$src, df$dst), paste(w$src, w$dst))]
  df$.agg_w[is.na(df$.agg_w)] <- 0
  out <- suppressWarnings(geoscales::recast_pairs(
    df, gs, to = level, src = "src", dst = "dst", values = vals,
    rule = .agg_rules(vals), weight = ".agg_w", na_rm = TRUE))
  out <- as.data.frame(out)
  out[, intersect(names(df), names(out)), drop = FALSE]
}

#' Merge a group of trade objects into one coarse corridor
#' @noRd
.agg_trade_merge <- function(objs, gs, level, pair) {
  base <- objs[[1L]]
  bind <- function(slot) {
    parts <- lapply(objs, function(o) {
      v <- methods::slot(o, slot)
      if (is.data.frame(v) && nrow(v)) v else NULL
    })
    parts <- parts[!vapply(parts, is.null, logical(1))]
    if (!length(parts)) return(methods::slot(base, slot))
    do.call(rbind, parts)
  }

  # Corridor capacity weights the intensive quantities, the way a cluster's
  # `p_nom` weights an aggregated line.
  tr <- bind("trade")
  w <- NULL
  if (is.data.frame(tr) && nrow(tr) && "ava.up" %in% names(tr)) {
    v <- tr$ava.up
    if (!"ava.up" %in% names(tr) || all(is.na(v))) v <- rep(NA_real_, nrow(tr))
    ok <- !is.na(tr$src) & !is.na(tr$dst)
    if (any(ok)) {
      w <- stats::aggregate(list(.agg_w = v[ok]),
                            by = list(src = tr$src[ok], dst = tr$dst[ok]),
                            FUN = function(z) sum(z, na.rm = TRUE))
    }
  }

  out <- base
  out@name <- paste0(.agg_trade_prefix(base), "_", pair$src[1], "__",
                     pair$dst[1])
  out@routes <- rbind(pair, stats::setNames(pair[, c("dst", "src")],
                                            c("src", "dst")))
  rownames(out@routes) <- NULL
  for (sl in c("trade", "aeff", "varom")) {
    methods::slot(out, sl) <- .agg_pair_slot(bind(sl), gs, level, w)
  }
  rw <- NULL
  for (sl in c("invcost", "fixom", "vintage")) {
    methods::slot(out, sl) <- .agg_slot(bind(sl), gs, level, rw,
                                        character(0))
  }
  if (length(objs) > 1L) {
    out@desc <- paste0(base@desc, " (merged from ", length(objs),
                       " corridors)")
  }
  out
}

# -- the driver -------------------------------------------------------------- #

#' Aggregate a model to a coarser set of regions
#'
#' Build a smaller model from a finer one by collapsing its regions onto a
#' coarser geoframe of a [geoscales::Geoscale]. The result is a model in its
#' own right: it can be interpolated, solved and reported like any other.
#'
#' Every region-keyed slot is recast. Extensive quantities -- capacities,
#' demand, supply availability, import and export limits -- are summed over the
#' regions of each parent. Intensive ones -- efficiencies, availability
#' factors, costs, weather values -- take a mean weighted by the object's own
#' size in each region, so an aggregated value stays within the range of the
#' values it came from. When the weights of a group are all zero the mean is
#' unweighted, which keeps a group of empty regions at their common value
#' rather than undefined.
#'
#' Trade is different: a trade object is one region pair, so corridors that
#' land inside a single parent region become internal and are dropped, and the
#' rest merge -- their capacities add, and their loss factors take the
#' capacity-weighted mean. Corridors merge only when they agree on commodity
#' and on cluster structure, so tranched and untranched routes stay apart.
#'
#' @param mod A `model`.
#' @param geoscale A [geoscales::Geoscale] whose finest geoframe contains the
#'   model's regions. `NULL` (default) uses the model's own.
#' @param level Name of the target geoframe, coarser than the model's regions.
#' @param name Name for the aggregated model. `NULL` (default) appends
#'   `level` to the source model's name.
#' @param verbose Report what was aggregated, merged and dropped.
#'
#' @return A `model` declared over the coarser regions, carrying the pruned
#'   geoscale.
#'
#' @seealso [subset_model_regions()], which takes a sub-territory at the
#'   model's own resolution instead of coarsening the whole of it.
#' @export
aggregate_model_regions <- function(mod, geoscale = NULL, level,
                                    name = NULL, verbose = isVerbose()) {
  stopifnot(inherits(mod, "model"))
  check_package("geoscales")
  # Weather values are mean-aggregated below, and a mean does not commute
  # with a nonlinear transform: transform(mean(stream)) != mean(transform).
  # Refuse rather than silently distort; materialize first if aggregation of
  # the derived series is what is wanted.
  .assert_no_weather_transforms(mod)
  gs <- geoscale
  if (is.null(gs)) gs <- tryCatch(mod@config@geoscale, error = function(e) NULL)
  if (is.null(gs) || !is_geoscale(gs)) {
    stop("no geoscale: pass `geoscale=`, or attach one to the model with ",
         "setGeoscale().", call. = FALSE)
  }
  frames <- geoscales::geoscale_geoframes(gs)
  if (!isTRUE(level %in% frames)) {
    stop("`level` = \"", level, "\" is not a geoframe of the geoscale; ",
         "one of: ", paste(frames, collapse = ", "), call. = FALSE)
  }
  fine <- geoscales::geoscale_geoframes(gs, finest = TRUE)
  if (identical(level, fine)) {
    stop("`level` = \"", level, "\" is the geoscale's finest geoframe; ",
         "there is nothing to aggregate.", call. = FALSE)
  }

  regions <- as.character(mod@config@region)
  regions <- regions[!is.na(regions) & nzchar(regions)]
  atoms <- as.character(geoscales::geoscale_regions(gs, fine))
  unknown <- setdiff(regions, atoms)
  if (length(unknown) > 0L) {
    stop(length(unknown), " of the model's ", length(regions), " region(s) ",
         "are not atoms of the geoscale, so their data would be dropped ",
         "silently: ", paste(utils::head(unknown, 6), collapse = ", "),
         if (length(unknown) > 6) ", ..." else "",
         ". The geoscale must be keyed the way the model is.", call. = FALSE)
  }

  coarse <- unique(stats::na.omit(.agg_map_codes(regions, gs, level)))
  if (isTRUE(verbose)) {
    message("Aggregating ", length(regions), " region(s) to ", length(coarse),
            " at `", level, "`")
  }

  objs <- mod@data[[1]]@data
  cls <- vapply(objs, function(o) class(o)[1], "")
  out <- list()

  # A `weather` object may sit ABOVE the atom layer: one adm1 profile shared by
  # its adm2 children (mWeatherRegionAt). The recast below keys on the FINEST
  # leaftable column, so such an object matches nothing and the aggregation
  # dies with "no rows of `x` matched the Geoscale's atoms". Three cases:
  #   * at the atoms          -> mean-aggregate, as before
  #   * already AT `level`    -> nothing to aggregate; pass it through untouched
  #   * anywhere else (an intermediate frame, or coarser than `level`)
  #                           -> refuse; recasting between two non-atom frames
  #                              needs a level-aware crosswalk we do not have
  target_codes <- as.character(geoscales::geoscale_regions(gs, level))
  .wx_level_case <- function(o) {
    if (!methods::is(o, "weather")) return("agg")
    r <- unique(c(as.character(methods::slot(o, "region")),
                  if (.hasSlot(o, "weather") &&
                      is.data.frame(o@weather) &&
                      "region" %in% names(o@weather))
                    as.character(o@weather$region)))
    r <- r[!is.na(r) & nzchar(r)]
    if (length(r) == 0) return("agg")          # wildcard: broadcasts either way
    if (all(r %in% atoms)) return("agg")
    if (all(r %in% target_codes)) return("keep")
    "refuse"
  }

  # -- objects that span regions: the object stays, its data moves ------------
  for (i in which(cls != "trade")) {
    o <- objs[[i]]
    case <- .wx_level_case(o)
    if (identical(case, "refuse")) {
      r <- unique(c(as.character(methods::slot(o, "region")),
                    as.character(o@weather$region)))
      r <- r[!is.na(r) & nzchar(r)]
      stop("weather '", o@name, "' is declared at region(s) that are neither ",
           "atoms of the geoscale nor members of `level` = \"", level, "\": ",
           paste(utils::head(sort(unique(r)), 5), collapse = ", "),
           ".
Aggregating between two non-atom levels needs a level-aware ",
           "crosswalk that `aggregate_model_regions()` does not have. Declare ",
           "the profile at the model's finest regions, or aggregate to the ",
           "level it already sits at.", call. = FALSE)
    }
    if (identical(case, "keep")) {
      # already at the target level -- nothing to aggregate
      out[[o@name]] <- o
      next
    }
    w <- .agg_weight(o)
    for (sl in methods::slotNames(o)) {
      v <- methods::slot(o, sl)
      if (is.data.frame(v) && nrow(v) && "region" %in% names(v)) {
        methods::slot(o, sl) <- .agg_slot(v, gs, level, w, regions)
      }
    }
    if (.hasSlot(o, "region")) {
      r <- methods::slot(o, "region")
      if (is.character(r) && length(r)) {
        methods::slot(o, "region") <-
          unique(stats::na.omit(.agg_map_codes(r, gs, level)))
      }
    }
    out[[o@name]] <- o
  }

  # -- trade: merge the corridors that survive -------------------------------
  tr <- objs[cls == "trade"]
  pairs <- lapply(tr, .agg_trade_pair, gs = gs, level = level)
  internal <- vapply(pairs, is.null, logical(1))
  keys <- rep(NA_character_, length(tr))
  for (i in which(!internal)) keys[i] <- .agg_trade_key(tr[[i]], pairs[[i]])

  n_merged <- 0L
  for (k in unique(stats::na.omit(keys))) {
    idx <- which(keys == k)
    merged <- .agg_trade_merge(tr[idx], gs, level, pairs[[idx[1L]]])
    if (length(idx) > 1L) n_merged <- n_merged + length(idx)
    out[[merged@name]] <- merged
  }
  if (isTRUE(verbose)) {
    message("Trade: ", length(tr), " corridor(s) -> ",
            sum(!is.na(unique(keys))), " (", sum(internal),
            " became internal, ", n_merged, " merged)")
  }

  res <- mod
  res@name <- name %||% paste0(mod@name, "_", level)
  res@data[[1]]@data <- out
  res@config@region <- coarse
  res@config@geoscale <- geoscales::prune_geoscale(gs, level)
  res
}


# ---------------------------------------------------------------------------
# (was R/get_region.R, merged 2026-09-20)
# ---------------------------------------------------------------------------

#' Collect the regions an object operates in
#'
#' Generic, reflective accessor that walks every slot of an S4 model object
#' (except `misc`) and gathers the regions it refers to. Regions are read from:
#'   * any atomic slot named `region`, `src`, or `dst`, and
#'   * the `region`, `src`, and `dst` columns of any `data.frame` slot.
#'
#' The result is the set of unique, non-missing, non-empty region labels. The
#' function is intentionally schema-agnostic so it keeps working as region
#' information is added to classes that do not yet carry an explicit `@region`
#' slot (e.g. `import` / `export`).
#'
#' @param obj a model object (S4) such as `technology`, `storage`, `trade`,
#'   `import`, or `export`.
#'
#' @returns a character vector of region labels (possibly empty).
#'
#' @family model
#' @export
get_region <- function(obj) {
  if (!isS4(obj)) {
    return(character(0))
  }
  # A model declares its regions on `@config`, not on a slot of its own, so the
  # reflective walk below finds nothing there. Its declared regions are the
  # answer a caller wants, and reading them should not require `@config@region`.
  if (methods::.hasSlot(obj, "config")) {
    cfg <- methods::slot(obj, "config")
    if (isS4(cfg) && methods::.hasSlot(cfg, "region")) {
      out <- as.character(methods::slot(cfg, "region"))
      return(unique(out[!is.na(out) & nzchar(out)]))
    }
  }
  keys <- c("region", "src", "dst")
  out <- character(0)
  for (sn in .instance_slots(obj)) {
    if (identical(sn, "misc")) next
    v <- methods::slot(obj, sn)
    if (is.data.frame(v)) {
      for (cc in intersect(keys, colnames(v))) {
        out <- c(out, as.character(v[[cc]]))
      }
    } else if (is.atomic(v) && sn %in% keys) {
      out <- c(out, as.character(v))
    }
  }
  out <- out[!is.na(out) & nzchar(out)]
  unique(out)
}

# Guard: error if any region referenced in the model's objects is not declared.
# `model@data` is a list of repositories (or bare objects); regions are gathered
# reflectively via `get_region()` (covers region / src / dst, atomic or
# data.frame). NA / "" entries are wildcards ("all declared regions") and are
# ignored. Run early in `interp_mod()` so a stray region fails fast with a clear
# message rather than as an out-of-domain error in a solver writer.
.check_declared_regions <- function(model, declared) {
  declared <- as.character(declared)
  declared <- declared[!is.na(declared) & nzchar(declared)]
  used <- character(0)
  for (rp in model@data) {
    objs <- if (methods::is(rp, "repository")) rp@data else list(rp)
    for (o in objs) used <- c(used, get_region(o))
  }
  undeclared <- setdiff(unique(used), declared)
  if (length(undeclared) > 0) {
    stop(
      "The model references undeclared region(s): ",
      paste(sort(undeclared), collapse = ", "),
      ".\nDeclare them in the model's regions (config/settings) or remove them ",
      "from the affected objects' data.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' Collect the weather objects an object refers to
#'
#' Reflective accessor that walks every slot of an S4 model object (except
#' `misc`) and gathers the names in any `weather` column. A `technology`,
#' `storage` or `supply` links to a weather profile by name; the profile itself
#' is a separate `weather` object in the repository.
#'
#' @param obj a model object (S4) such as `technology`, `storage` or `supply`.
#'
#' @returns a character vector of weather object names (possibly empty).
#'
#' @family model
#' @export
get_weather <- function(obj) {
  if (!isS4(obj)) {
    return(character(0))
  }
  out <- character(0)
  for (sn in .instance_slots(obj)) {
    if (identical(sn, "misc")) next
    v <- methods::slot(obj, sn)
    if (is.data.frame(v) && "weather" %in% colnames(v)) {
      out <- c(out, as.character(v[["weather"]]))
    }
  }
  out <- out[!is.na(out) & nzchar(out)]
  unique(out)
}

# Guard: error if a weather profile referenced by name has no object behind it.
#
# `collect_set_elements()` builds the `weather` set from the REFERENCES, not
# from the declared objects, so a dangling name enters the set, contributes no
# `pWeather` rows, and the availability limit it carries is dropped without
# error -- the process then runs unconstrained.
.check_declared_objects <- function(model) {
  declared <- character(0)
  used <- list()
  for (rp in model@data) {
    objs <- if (methods::is(rp, "repository")) rp@data else list(rp)
    for (o in objs) {
      if (methods::is(o, "weather")) declared <- c(declared, o@name)
      w <- get_weather(o)
      if (length(w) > 0) {
        used[[length(used) + 1L]] <- data.frame(
          object = .object_label(o), weather = w, stringsAsFactors = FALSE)
      }
    }
  }
  if (length(used) == 0L) {
    return(invisible(TRUE))
  }
  used <- do.call(rbind, used)
  miss <- used[!used$weather %in% declared, , drop = FALSE]
  if (nrow(miss) > 0L) {
    by_w <- tapply(miss$object, miss$weather, function(x)
      paste(unique(x), collapse = ", "))
    stop(
      "The model references weather object(s) that are not in the repository: ",
      paste(sprintf("%s (from %s)", names(by_w), by_w), collapse = "; "),
      ".\nAdd them with `add()`, or remove the reference from the affected ",
      "objects' `weather` slot. A dangling reference does not fail on its own: ",
      "the availability limit it carries is silently dropped.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

# Guard: error if a technology's `geff` names an input group that no commodity
# belongs to.
#
# `@group` itself is optional -- groups are collected from the `group` column of
# `@input` / `@output`, and a model with the same groups declared and undeclared
# gives an identical answer -- so membership, not declaration, is what can be
# checked. A `geff` row for an empty group builds a group-efficiency constraint
# over no commodities, making the problem infeasible; the solver reports only
# "PROBLEM HAS NO PRIMAL FEASIBLE SOLUTION", which does not name the group.
.check_group_members <- function(model) {
  bad <- list()
  for (rp in model@data) {
    objs <- if (methods::is(rp, "repository")) rp@data else list(rp)
    for (o in objs) {
      if (!methods::is(o, "technology")) next
      ge <- methods::slot(o, "geff")
      if (!is.data.frame(ge) || !"group" %in% colnames(ge)) next
      used <- .nonblank(ge[["group"]])
      if (length(used) == 0L) next
      members <- unique(c(.slot_col(o, "input", "group"),
                          .slot_col(o, "output", "group")))
      orphan <- setdiff(used, members)
      if (length(orphan) > 0L) {
        bad[[length(bad) + 1L]] <- sprintf("%s: %s", .object_label(o),
                                           paste(sort(orphan), collapse = ", "))
      }
    }
  }
  if (length(bad) > 0L) {
    stop(
      "The `geff` of the following technology/ies names input group(s) that no ",
      "commodity belongs to: ", paste(unlist(bad), collapse = "; "),
      ".\nPut a commodity in the group via the `group` column of `@input` or ",
      "`@output`, or drop the `geff` row. A group with no members yields an ",
      "infeasible problem, not a wrong one.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.slot_col <- function(o, slot, col) {
  v <- tryCatch(methods::slot(o, slot), error = function(e) NULL)
  if (!is.data.frame(v) || !col %in% colnames(v)) return(character(0))
  .nonblank(v[[col]])
}

.nonblank <- function(x) {
  x <- as.character(x)
  unique(x[!is.na(x) & nzchar(x)])
}

# A name for the error message. Every model object has `@name`, but an unnamed
# one would make the message useless, so fall back to the class.
.object_label <- function(o) {
  nm <- tryCatch(as.character(methods::slot(o, "name")), error = function(e) "")
  if (length(nm) != 1L || is.na(nm) || !nzchar(nm)) class(o)[1] else nm
}

# =========================================================================== #
# Cost-slot region guards.
#
# Trade capacity is region-free -- one number per corridor -- but its costs are
# region-indexed, and `eqTradeInv`/`eqTradeEac`/`eqTradeFixom`/`eqTradeRetCost`
# apply the FULL capacity in every region the cost names, with no share factor.
# That is the intended convention (`invcost` is what each endpoint bears), but it
# has two edges a user cannot see:
#
#   * an UNREGIONED row broadcasts to every endpoint, so a two-endpoint
#     corridor is charged twice;
#   * a COARSER geoscale level is charged ONCE at that cell -- not at each
#     child -- which is usually what a national cost datum means, but is a
#     different number from naming the children individually.
#
# Both are reported, not corrected.
#
# The unregioned case is a MESSAGE. It looked like an ambiguity worth refusing
# until the arithmetic was read properly: `invcost` is a RATE PER ENDPOINT, so
# an unregioned row is not a doubled total, it is the rate applied at each end.
# That is also what makes it right -- a corridor over A-B-C-D at 25 each costs
# 100 whole and 50 in a B-C subset, so a partial-region study bears its own
# share instead of the whole corridor. Refusing it would force allocation that
# buys nothing and produces the same numbers.
#
# The coarse-level case was a WARNING while such a cost never reached the
# objective at all. It does now (`mvTotalCost` carries the cells a cost was
# actually declared at), so it is a MESSAGE: legal, meaningful, and worth
# naming only because the arithmetic differs from the per-child form.
#
# Scope is the COST slots only. Trade `@aeff` also has `src`/`dst`, but there
# they are ROUTE SELECTORS -- "NA for every source region on the route" -- not
# a cost attribution, so an NA is meaningful there and stays allowed.
# =========================================================================== #

# Region column of a cost slot, or character(0) when the slot is absent/empty.
.cost_slot_regions <- function(obj, slot) {
  if (!.hasSlot(obj, slot)) return(character(0))
  d <- methods::slot(obj, slot)
  if (!is.data.frame(d) || !nrow(d) || !("region" %in% names(d))) return(character(0))
  as.character(d$region)
}

# `model_regions` = the regions costs can actually accrue in (`.model_regions()`
# in the scenario); `known_regions` additionally carries the geoscale's coarser
# levels. A cost region in the second but not the first is silently free.
.check_cost_regions <- function(model, model_regions, known_regions,
                                slots = c("invcost", "fixom")) {
  model_regions <- as.character(model_regions)
  coarse_hits <- list()
  broadcast <- character(0)

  for (rp in model@data) {
    objs <- if (methods::is(rp, "repository")) rp@data else list(rp)
    for (o in objs) {
      nm <- tryCatch(o@name, error = function(e) NA_character_)
      for (sl in slots) {
        rg <- .cost_slot_regions(o, sl)
        if (!length(rg)) next
        # coarse: named, declared, but not a region costs accrue in
        cs <- setdiff(unique(rg[!is.na(rg)]), model_regions)
        cs <- intersect(cs, as.character(known_regions))
        if (length(cs)) coarse_hits[[length(coarse_hits) + 1L]] <-
          paste0(nm, "@", sl, ": ", paste(sort(cs), collapse = ", "))
        # broadcast: an NA region on a multi-endpoint object (only trade has one)
        if (any(is.na(rg)) && methods::is(o, "trade")) {
          ends <- unique(c(as.character(o@routes$src), as.character(o@routes$dst)))
          ends <- ends[!is.na(ends) & nzchar(ends)]
          if (length(ends) > 1L)
            broadcast <- c(broadcast,
                           paste0(nm, "@", sl, " (", length(ends), " endpoints: ",
                                  paste(sort(ends), collapse = ", "), ")"))
        }
      }
    }
  }

  if (length(coarse_hits)) {
    message(
      "Cost declared at a coarse geoscale level:\n  ",
      paste(unlist(coarse_hits), collapse = "\n  "),
      "\nThe cost is charged ONCE at that cell rather than at each ",
      "child region, and is discounted at the children's rate. That is ",
      "usually what a national cost datum means -- said here because it is ",
      "a different number from naming the children individually.")
  }
  if (length(broadcast)) {
    message(
      "Trade cost with no `region` on a multi-endpoint route:\n  ",
      paste(sort(unique(broadcast)), collapse = "\n  "),
      "\nTrade costs are a rate borne by EACH endpoint, so an unregioned ",
      "row applies at every endpoint of the route: a two-region corridor at ",
      "100 costs 200 in total. Name the regions to vary the rate between ",
      "them, e.g.\n",
      "    invcost = data.frame(region = c(\"A\", \"B\"), invcost = c(60, 40))\n",
      "This is reported, not corrected: the rate is the same one a region ",
      "subset would see, so it is what makes a partial-region study bear its ",
      "own share rather than the whole corridor.")
  }
  invisible(NULL)
}


# ---------------------------------------------------------------------------
# (was R/region_gaps.R, merged 2026-09-20)
# ---------------------------------------------------------------------------

# =========================================================================== #
# region_gaps.R -- processes that cannot operate in a region they are declared
# in, because a commodity they require is not available there.
#
# `mCommReg` is the commodity-region closure: where each commodity can exist.
# The activity domains (`mvTechAct`, and the storage analogue) are built from
# the process's SPAN and carry no commodity, so they are not narrowed by it --
# only the per-commodity flow domains are (`.filt_cr`). A process declared where
# its input is unavailable therefore keeps its activity and output rows while
# losing its input rows: the input constraint is not violated, it does not
# exist, and the process produces from nothing at zero cost. The solution is
# feasible, optimal, and passes every `verify_solution()` invariant, because the
# balance it would violate was never generated.
#
# The gaps are computed once here, next to the closure that decides
# availability, and the activity domains subtract them.
# =========================================================================== #

#' @include mapping-builders.R
NULL

# Input requirements of one technology, split into commodities that are each
# required on their own and groups of which ANY ONE member suffices.
#
# A grouped input is a substitution set (co-firing, dual-fuel): the technology
# runs on whichever member is available, so requiring all of them would refuse
# models that work.
.tech_input_requirements <- function(scen) {
  one <- .gds(scen, "mTechOneComm")      # (tech, comm) -- ungrouped, any role
  inp <- .gds(scen, "mTechInpComm")      # (tech, comm) -- every input
  grp <- .gds(scen, "mTechGroupComm")    # (tech, group, comm)
  igr <- .gds(scen, "mTechInpGroup")     # (tech, group) -- groups typed input
  if (is.null(inp)) return(list(single = NULL, group = NULL))
  inp <- as.data.frame(inp)

  single <- if (is.null(one)) inp else
    dplyr::semi_join(inp, as.data.frame(one), by = c("tech", "comm"))

  group <- NULL
  if (!is.null(grp) && !is.null(igr)) {
    group <- dplyr::semi_join(as.data.frame(grp), as.data.frame(igr),
                              by = c("tech", "group"))
    if (nrow(group) == 0) group <- NULL
  }
  list(single = single, group = group)
}

# (process, class, region, missing) for every declared cell a process cannot
# operate in. `missing` names the commodities that are unavailable there.
.process_region_gaps <- function(scen) {
  cr <- .gds(scen, "mCommReg")
  if (is.null(cr) || nrow(cr) == 0) return(NULL)
  cr <- dplyr::distinct(as.data.frame(cr)[, c("comm", "region")])

  # `have`: the cells that DO work. A cell is missing when its span row has no
  # match here.
  gaps <- list()

  add_gap <- function(span, req, key, cls) {
    if (is.null(span) || is.null(req) || nrow(span) == 0 || nrow(req) == 0) {
      return(NULL)
    }
    span <- dplyr::distinct(as.data.frame(span)[, c(key, "region")])
    need <- dplyr::inner_join(span, as.data.frame(req), by = key,
                              relationship = "many-to-many")
    if (nrow(need) == 0) return(NULL)
    short <- dplyr::anti_join(need, cr, by = c("comm", "region"))
    if (nrow(short) == 0) return(NULL)
    short |>
      dplyr::summarise(missing = paste(sort(unique(.data$comm)), collapse = ", "),
                       .by = dplyr::all_of(c(key, "region"))) |>
      dplyr::transmute(process = .data[[key]], class = cls,
                       region = .data$region, missing = .data$missing)
  }

  # -- technology: every ungrouped input, plus every input group that has no
  #    available member at all.
  tspan <- .gds(scen, "mTechSpan")
  treq  <- .tech_input_requirements(scen)
  gaps[[length(gaps) + 1L]] <- add_gap(tspan, treq$single, "tech", "technology")

  if (!is.null(tspan) && !is.null(treq$group)) {
    span <- dplyr::distinct(as.data.frame(tspan)[, c("tech", "region")])
    g <- dplyr::inner_join(span, treq$group, by = "tech",
                           relationship = "many-to-many") |>
      dplyr::left_join(dplyr::mutate(cr, .ok = TRUE), by = c("comm", "region")) |>
      dplyr::summarise(any_ok = any(!is.na(.data$.ok)),
                       missing = paste(sort(unique(.data$comm)), collapse = " | "),
                       .by = dplyr::all_of(c("tech", "region", "group"))) |>
      dplyr::filter(!.data$any_ok)
    if (nrow(g) > 0) {
      gaps[[length(gaps) + 1L]] <- g |>
        dplyr::summarise(missing = paste0("group(", paste(.data$missing,
                                                          collapse = "); group("),
                                          ")"),
                         .by = dplyr::all_of(c("tech", "region"))) |>
        dplyr::transmute(process = .data$tech, class = "technology",
                         region = .data$region, missing = .data$missing)
    }
  }

  # -- storage: the input role only. A storage with no input commodity
  #    available cannot be charged, so it cannot operate.
  gaps[[length(gaps) + 1L]] <-
    add_gap(.gds(scen, "mStorageSpan"), .gds(scen, "mStorageInpComm"),
            "stg", "storage")

  out <- dplyr::bind_rows(gaps)
  if (is.null(out) || nrow(out) == 0) return(NULL)
  out <- out[order(out$class, out$process, out$region), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# Record the gaps on the scenario and report them once. Called from the closure
# recipe, after `mCommReg` exists; `map_mvTechAct()` / `.filter_storage_act()`
# subtract what it records.
.record_region_gaps <- function(scen) {
  g <- .process_region_gaps(scen)
  scen@misc$region_gaps <- g
  if (is.null(g)) return(scen)
  n <- nrow(g)
  head_n <- utils::head(g, 10)
  warning(
    "Dropped ", n, " process-region cell", if (n > 1) "s" else "",
    ": a required commodity is not available there.\n",
    "Without this the process would keep its activity and output while losing\n",
    "its input, and produce from nothing.\n   ",
    paste(utils::capture.output(print(head_n, row.names = FALSE)),
          collapse = "\n   "),
    if (n > nrow(head_n)) paste0("\n   ... and ", n - nrow(head_n), " more"),
    "\n\nSee `scenario@misc$region_gaps` for the full table. Declare the ",
    "missing supply/import/trade, or narrow the process `@region`.",
    call. = FALSE
  )
  scen
}

# The (key, region) pairs to subtract from an activity domain.
.region_gap_pairs <- function(scen, key, cls) {
  g <- scen@misc$region_gaps
  if (is.null(g) || nrow(g) == 0) return(NULL)
  g <- g[g$class == cls, , drop = FALSE]
  if (nrow(g) == 0) return(NULL)
  stats::setNames(data.frame(g$process, g$region, stringsAsFactors = FALSE),
                  c(key, "region"))
}

# Drop them from an activity-domain frame.
.drop_region_gaps <- function(scen, df, key, cls) {
  pairs <- .region_gap_pairs(scen, key, cls)
  if (is.null(pairs) || is.null(df) || nrow(df) == 0) return(df)
  as.data.frame(dplyr::anti_join(as.data.frame(df), pairs,
                                 by = c(key, "region")))
}

# --------------------------------------------------------------------------- #
# Declaration-level check (no interpolation)
# --------------------------------------------------------------------------- #

# Every element object of a model, flattened out of its repositories.
.model_elements <- function(mod) {
  out <- list()
  for (rep in mod@data) {
    d <- tryCatch(rep@data, error = function(e) NULL)
    if (is.null(d)) next
    out <- c(out, d)
  }
  out
}

# The regions an object is declared in, per R1: `@region` when populated,
# every model region otherwise. Trade scopes on its route endpoints instead.
.declared_regions_of <- function(obj, regions) {
  if (is(obj, "trade")) {
    r <- tryCatch(unique(c(as.character(obj@routes$src),
                           as.character(obj@routes$dst))),
                  error = function(e) character())
    r <- r[!is.na(r) & nzchar(r)]
    return(if (length(r) == 0) regions else intersect(r, regions))
  }
  r <- if (.hasSlot(obj, "region")) as.character(obj@region) else character()
  r <- r[!is.na(r) & nzchar(r)]
  if (length(r) == 0) regions else intersect(r, regions)
}

# Input / output commodities of one process, with input groups kept apart:
# a group is a substitution set, so any one available member satisfies it.
.declared_io <- function(obj) {
  single <- character(); groups <- list(); outs <- character()
  if (is(obj, "technology")) {
    ct <- tryCatch(checkInpOut(obj)$comm, error = function(e) NULL)
    gt <- tryCatch(checkInpOut(obj)$group, error = function(e) NULL)
    if (is.null(ct) || nrow(ct) == 0) return(list(single = single, groups = groups,
                                                  outs = outs))
    cm <- rownames(ct)
    is_in  <- !is.na(ct$type) & ct$type == "input"
    is_out <- !is.na(ct$type) & ct$type == "output"
    outs <- cm[is_out]
    single <- cm[is_in & is.na(ct$group)]
    gnames <- unique(ct$group[is_in & !is.na(ct$group)])
    for (g in gnames) groups[[g]] <- cm[is_in & !is.na(ct$group) & ct$group == g]
  } else if (is(obj, "storage")) {
    gi <- function(sl) {
      d <- tryCatch(methods::slot(obj, sl), error = function(e) NULL)
      if (is.data.frame(d) && "comm" %in% names(d)) as.character(d$comm) else
        as.character(obj@commodity)
    }
    single <- unique(gi("input")); outs <- unique(gi("output"))
  } else if (is(obj, "supply") || is(obj, "import")) {
    outs <- as.character(obj@commodity)
  } else if (is(obj, "trade")) {
    outs <- as.character(obj@commodity); single <- as.character(obj@commodity)
  }
  list(single = unique(single[nzchar(single)]), groups = groups,
       outs = unique(outs[nzchar(outs)]))
}

#' Processes declared where a commodity they need is unavailable
#'
#' @description
#' Reports `(process, region)` cells a model declares but cannot support,
#' because a commodity the process consumes is produced nowhere in that region
#' and cannot be traded in. Works on the DECLARATIONS alone -- no interpolation
#' and no solver -- so it can be run on a model while it is being built.
#'
#' @details
#' Availability is grown to a fixed point: a commodity is available in a region
#' if a supply or import declares it there, if a process whose own inputs are
#' available there produces it, or if a trade route reaches that region from one
#' where it is available. A process is then short in a region when an ungrouped
#' input is unavailable, or when no member of one of its input groups is.
#'
#' `interpolate_model()` performs the same check on the interpolated model and
#' drops the cells it finds, recording them in `scenario@misc$region_gaps`.
#' This function is the declaration-time counterpart: it answers the same
#' question earlier and without the pipeline.
#'
#' @param mod a model object.
#'
#' @return a data.frame with columns `process`, `class`, `region` and `missing`,
#'   or `NULL` when every declared cell is supportable.
#'
#' Internal until a report template consumes it: templates call the package
#' through `energyRt::`, so the export goes in with the wiring, and the return
#' columns are still free to change until then.
#' @noRd
model_region_gaps <- function(mod) {
  stopifnot(is(mod, "model"))
  regions <- as.character(get_region(mod))
  regions <- regions[!is.na(regions) & nzchar(regions)]
  objs <- .model_elements(mod)
  if (length(regions) == 0 || length(objs) == 0) return(NULL)

  info <- list()
  for (o in objs) {
    if (!.hasSlot(o, "name") || is(o, "commodity")) next
    io <- .declared_io(o)
    if (length(io$single) == 0 && length(io$groups) == 0 &&
        length(io$outs) == 0) next
    info[[length(info) + 1L]] <- list(
      name = o@name, cls = class(o)[1],
      regions = .declared_regions_of(o, regions), io = io,
      routes = if (is(o, "trade")) tryCatch(o@routes, error = function(e) NULL)
    )
  }
  if (length(info) == 0) return(NULL)

  key <- function(cm, rg) paste(cm, rg, sep = "\r")
  avail <- character(0)
  for (i in info) {
    if (i$cls %in% c("supply", "import")) {
      avail <- c(avail, key(rep(i$io$outs, each = length(i$regions)),
                            rep(i$regions, times = length(i$io$outs))))
    }
  }
  avail <- unique(avail)

  can_run <- function(i, rg) {
    if (length(i$io$single) > 0 &&
        !all(key(i$io$single, rg) %in% avail)) return(FALSE)
    for (g in i$io$groups) if (!any(key(g, rg) %in% avail)) return(FALSE)
    TRUE
  }

  repeat {
    n0 <- length(avail)
    for (i in info) {
      if (i$cls == "trade") {
        rt <- i$routes
        if (is.null(rt) || nrow(rt) == 0) next
        for (k in seq_len(nrow(rt))) {
          if (key(i$io$outs, rt$src[k]) %in% avail) {
            avail <- unique(c(avail, key(i$io$outs, rt$dst[k])))
          }
        }
        next
      }
      if (length(i$io$outs) == 0) next
      for (rg in i$regions) {
        if (can_run(i, rg)) avail <- unique(c(avail, key(i$io$outs, rg)))
      }
    }
    if (length(avail) == n0) break
  }

  rows <- list()
  for (i in info) {
    if (i$cls %in% c("supply", "import", "export", "demand", "trade")) next
    for (rg in i$regions) {
      if (can_run(i, rg)) next
      miss <- i$io$single[!key(i$io$single, rg) %in% avail]
      for (g in names(i$io$groups)) {
        gm <- i$io$groups[[g]]
        if (!any(key(gm, rg) %in% avail)) {
          miss <- c(miss, paste0("group(", paste(gm, collapse = " | "), ")"))
        }
      }
      rows[[length(rows) + 1L]] <- data.frame(
        process = i$name, class = i$cls, region = rg,
        missing = paste(miss, collapse = ", "), stringsAsFactors = FALSE)
    }
  }
  if (length(rows) == 0) return(NULL)
  out <- do.call(rbind, rows)
  out <- out[order(out$class, out$process, out$region), , drop = FALSE]
  rownames(out) <- NULL
  out
}
