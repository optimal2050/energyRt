# geoscale ####################################################################
# Attaching spatial information to a model's regions.
#
# `config@region` stays authoritative: it is the set of region labels the model
# is built on, and it is always the FINEST level. A geoscale adds structure
# above those labels -- how they nest into coarser levels, what they weigh, and
# where they are on a map.
#
# Two things use it, and they are independent:
#
#  * PRESENTATION (plot_map.R, plot.R, reporting, subsetting). Needs nothing
#    from the optimisation model and never did.
#
#  * MODEL STRUCTURE. `sets$region` becomes the union of every level, so a
#    coarse region such as a nation is a member alongside the states, and a
#    commodity may declare `@geolevel` to be balanced there instead
#    (`mCommRegion`, `mRegionFamily` -- see map_region.R). Attaching a geoscale
#    alone changes nothing observable: the extra members are inert until some
#    commodity names a coarser level, which is what Verification 2 pins.
#
# The hierarchy is pruned to the model: a geoscale covering more ground than
# the model uses (a world map for a two-region model) is normal and supported,
# and only the ancestors of declared regions enter `sets$region`.
#
# `geoscales` is an optional (Suggests) dependency, so every entry point that
# needs the package guards with `check_package("geoscales")`, and the type test
# `is_geoscale()` works whether or not it is installed.

#' @include class-model.R class-scenario.R class-settings.R
NULL

#' Attach or read a model's geoscale
#'
#' A geoscale describes the model's regions -- their nesting into coarser
#' levels, their weights, and optionally their geometry. It is built with the
#' [geoscales](https://github.com/optimal2050/geoscales) package and is used
#' for plotting, reporting and subsetting only: attaching one never changes the
#' optimisation model.
#'
#' `setGeoscale()` stores it on a `config` (and therefore on a `settings`, which
#' inherits from `config`) or on a `model`. `getGeoscale()` reads it back;
#' for a `scenario` it reads `@settings` first and falls back to the model's
#' `@config`, matching how `@calendar` is resolved.
#'
#' @param obj A `config`, `settings`, `model` or `scenario`.
#' @param geoscale A `geoscales::Geoscale`, or `NULL` to clear it.
#' @param ... Unused.
#'
#' @return `setGeoscale()` returns the modified object; `getGeoscale()` returns
#'   a `geoscales::Geoscale` or `NULL`.
#'
#' @examples
#' \dontrun{
#' gs <- geoscales::geoscale_from_leaftable(
#'   data.frame(zone = c("N", "N", "S"), region = c("R1", "R2", "R3")),
#'   geoframes = c("zone", "region")
#' )
#' mod <- newModel("demo", region = c("R1", "R2", "R3"), geoscale = gs)
#' getGeoscale(mod)
#' }
#'
#' @family geoscale
#' @name setGeoscale
#' @aliases setGeoscale getGeoscale getCalendar
NULL

#' @rdname setGeoscale
#' @export
setMethod("setGeoscale", signature(obj = "model"), function(obj, geoscale,
                                                            ...) {
  obj@config <- setGeoscale(obj@config, geoscale)
  obj
})

#' @rdname setGeoscale
#' @export
setMethod("getGeoscale", signature(obj = "model"), function(obj, ...) {
  getGeoscale(obj@config)
})

#' @rdname setGeoscale
#' @export
setMethod("setGeoscale", signature(obj = "scenario"), function(obj, geoscale,
                                                               ...) {
  obj@settings <- setGeoscale(obj@settings, geoscale)
  obj
})

#' @rdname setGeoscale
#' @export
setMethod("getGeoscale", signature(obj = "scenario"), function(obj, ...) {
  # Settings is the read path for everything model-wide (see `@calendar`), but
  # a scenario built before the slot existed, or one never interpolated, may
  # only carry it on the model.
  gs <- getGeoscale(obj@settings)
  if (is.null(gs)) gs <- getGeoscale(obj@model)
  gs
})

# `getCalendar` was declared as a generic in generics.R but never given a
# method, so `getCalendar(mod)` failed. Added here alongside its spatial twin.

#' @rdname setGeoscale
#' @export
setMethod("getCalendar", signature(obj = "config"), function(obj) obj@calendar)

#' @rdname setGeoscale
#' @export
setMethod("getCalendar", signature(obj = "model"), function(obj) {
  obj@config@calendar
})

#' @rdname setGeoscale
#' @export
setMethod("getCalendar", signature(obj = "scenario"), function(obj) {
  obj@settings@calendar
})

# Diagnostics -----------------------------------------------------------------

#' Check a geoscale against the model's declared regions
#'
#' Warns when the two disagree. This is deliberately a warning, not an error:
#' `config@region` remains authoritative, and a geoscale covering more ground
#' than the model uses (a world map for a two-country model) is normal and
#' useful.
#'
#' @param geoscale A `geoscales::Geoscale`.
#' @param region Character vector of declared model regions.
#' @param level Level of the geoscale to match against. Defaults to the finest.
#'
#' @return Invisibly, the regions that are declared but absent from `geoscale`.
#'
#' @family geoscale
#' @export
check_geoscale_regions <- function(geoscale, region, level = NULL) {
  if (is.null(geoscale)) return(invisible(character()))
  check_package("geoscales")
  level <- level %||% .geo_default_level(geoscale)
  known <- geoscales::geoscale_regions(geoscale, level)

  region <- as.character(region)
  region <- region[!is.na(region) & nzchar(region)]
  missing <- setdiff(region, known)
  if (length(missing) > 0) {
    warning(
      "The geoscale has no code at level '", level, "' for ",
      length(missing), " declared region(s): ",
      paste(utils::head(sort(missing), 5), collapse = ", "),
      if (length(missing) > 5) ", ..." else "",
      ". Maps and region-level reporting will omit them.",
      call. = FALSE
    )
  }
  invisible(missing)
}

# The level whose codes are the model's regions: the finest, i.e. the atom
# layer. Everything coarser is a grouping of it.
#
# Read through the geoscales API rather than `S7::prop()`: reaching into the
# object's properties would put S7 in energyRt's Imports to read one field, and
# leak geoscales' class system across the package boundary. Everything crosses
# as plain data or through exported accessors.
#' @noRd
.geo_default_level <- function(geoscale) {
  check_package("geoscales")
  geoscales::geoscale_geoframes(geoscale, finest = TRUE)
}

# Region hierarchy -------------------------------------------------------------

# The part of a geoscale that sits above the model's own regions.
#
# Declared regions are the atoms (the finest level). Walking up level by level
# and keeping only parents that still have a kept child prunes a geoscale that
# covers more ground than the model -- a world map on a two-region model must
# not put 200 countries into `sets$region`.
#
# Returns NULL when there is no geoscale or no structure above the atoms, which
# is the signal to leave everything exactly as it was.
#
#' @noRd
.geo_hierarchy <- function(geoscale, region) {
  if (is.null(geoscale)) return(NULL)
  check_package("geoscales")

  levels <- geoscales::geoscale_geoframes(geoscale)
  if (length(levels) < 2L) return(NULL)
  finest <- levels[length(levels)]

  declared <- unique(as.character(region))
  declared <- declared[!is.na(declared) & nzchar(declared)]
  if (length(declared) == 0L) return(NULL)

  fam <- as.data.frame(geoscales::geoscale_family(geoscale))

  # level -> the codes of that level that survive pruning.
  keep <- list()
  keep[[finest]] <- intersect(geoscales::geoscale_regions(geoscale, finest),
                              declared)
  pairs <- list()
  for (i in rev(seq_len(length(levels) - 1L))) {
    pl <- levels[i]
    cl <- levels[i + 1L]
    f <- fam[fam$parent_level == pl & fam$child_level == cl &
               fam$child %in% keep[[cl]], , drop = FALSE]
    keep[[pl]] <- unique(f$parent)
    if (nrow(f) > 0L) {
      pairs[[length(pairs) + 1L]] <-
        data.frame(region = as.character(f$parent),
                   regionp = as.character(f$child),
                   stringsAsFactors = FALSE)
    }
  }

  coarser <- levels[-length(levels)]
  extra <- unlist(keep[coarser], use.names = FALSE)
  extra <- setdiff(unique(extra), declared)
  if (length(extra) == 0L) return(NULL)

  # Declared regions keep their original order and stay at the head of the set,
  # so adding a hierarchy appends members rather than reshuffling existing ones.
  family <- if (length(pairs)) do.call(rbind, pairs) else
    data.frame(region = character(), regionp = character(),
               stringsAsFactors = FALSE)

  list(
    levels  = levels,
    finest  = finest,
    region  = c(declared, extra),
    family  = unique(family),
    members = keep
  )
}

# The regions a process, supply or cost can actually occupy: the model's
# DECLARED regions, i.e. the finest level.
#
# `modInp@sets$region` is the union of every level once a geoscale is attached,
# so any site that means "somewhere a thing can be" must ask for this instead --
# otherwise supplies, spans and cost grids silently broadcast onto nations and
# zones. `settings@region` is authoritative and is never widened.
#' @noRd
.model_regions <- function(scen) {
  r <- as.character(scen@settings@region)
  r <- r[!is.na(r) & nzchar(r)]
  if (length(r) > 0L) r else as.character(scen@modInp@sets[["region"]])
}

# Every region name the model recognises: the declared ones plus the geoscale's
# coarser levels.
#
# The counterpart of `.model_regions()`, and the distinction matters. Use THIS
# for VALIDATION ("is this a region at all?"), because a national demand for a
# nationally-balanced commodity legitimately names the nation. Use
# `.model_regions()` for BROADCAST ("where does an unqualified thing exist?"),
# which is always the finest level.
#' @noRd
.known_regions <- function(scen) {
  r <- .model_regions(scen)
  h <- .geo_hierarchy(tryCatch(getGeoscale(scen@settings),
                               error = function(e) NULL), r)
  if (is.null(h)) r else h$region
}

# Regions at a named level, pruned to the model. `level = NULL` (a commodity
# with no `@geolevel`) means the finest level, i.e. today's flat behaviour.
#' @noRd
.geo_level_regions <- function(hier, level = NULL) {
  if (is.null(hier)) return(NULL)
  if (is.null(level) || length(level) == 0L || is.na(level[1]) ||
      !nzchar(level[1])) {
    level <- hier$finest
  }
  level <- level[1]
  if (!level %in% hier$levels) {
    stop("Unknown geo-level '", level, "'. The model's geoscale has: ",
         paste(hier$levels, collapse = ", "), ".", call. = FALSE)
  }
  hier$members[[level]]
}

# Resolve a geoscale from an explicit argument, else from the object.
#' @noRd
.resolve_geoscale <- function(x, geoscale = NULL, required = TRUE) {
  gs <- geoscale
  if (is.null(gs) && !is.null(x)) {
    gs <- tryCatch(getGeoscale(x), error = function(e) NULL)
  }
  if (is.null(gs) && required) {
    stop("No geoscale found. Attach one with `setGeoscale()`, or pass ",
         "`geoscale = `.", call. = FALSE)
  }
  if (!is.null(gs)) {
    if (!is_geoscale(gs)) {
      stop("`geoscale` must be a `geoscales::Geoscale` object.", call. = FALSE)
    }
    check_package("geoscales")
  }
  gs
}
