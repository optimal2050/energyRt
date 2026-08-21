# Physical properties of commodities #########################################
#
# `commodity@property` is a tidy table of physical properties -- heating values,
# density, molar mass, composition. It is REFERENCE data: `ob2mi()` never writes
# it to `modInp`, and no solver template sees it.
#
# Its purpose is to give `convert()` the physics needed to move between measures
# of the same commodity. A property whose unit is a ratio of two dimensions is a
# conversion edge between them: `lhv = 25.8 GJ/t` is an Energy<->Mass edge,
# `density = 0.85 t/m3` a Mass<->Volume edge. The registry below declares which
# pair each recognised property bridges; the traversal itself lands in a later
# stage together with the dimension-aware unit parser.
#
# The `@property` / `@emis` line: a property row describes the SUBSTANCE ITSELF.
# Anything whose numerator is a *different* commodity -- a CO2 emission factor,
# say -- belongs in `@emis`, which is real model data feeding `pEmissionFactor`.
# Keeping them apart avoids two homes for one number, and avoids a genuine
# hazard: an emission factor in "t/GJ" is dimensionally Mass/Energy, identical
# to `1/lhv`, so it would let `convert("GJ", "t")` return tonnes of CO2 where
# tonnes of fuel were meant. Composition rows (`frac_C` and friends) are safe
# because "kg/kg" is dimensionless and can never act as an edge.
# ############################################################################

# The recognised properties. `quantity` is the contract with `convert()`: it
# declares which dimension pair the property bridges. `bridges` says whether the
# property joins the default conversion graph:
#   "yes"     -- traversed automatically
#   "on_ask"  -- state variants (liquid, compressed, true vs bulk); reachable
#                only through an explicit `via =`, because e.g. hydrogen's three
#                densities span three orders of magnitude and picking one
#                silently would be the easiest way to get a wrong answer
#   "no"      -- reference only; dimensionless, or needs a quantity the model
#                does not carry (`heat_capacity` needs a temperature delta)
# Names ending in "*" are prefix rules, matched by `.property_def()`.
.COMMODITY_PROPERTIES <- data.frame(
  property = c(
    "lhv", "hhv", "density", "molar_mass",
    "frac_*", "density_*", "heat_capacity"
  ),
  quantity = c(
    "Energy/Mass", "Energy/Mass", "Mass/Volume", "Mass/Amount",
    "dimensionless", "Mass/Volume", "Energy/(Mass*Temperature)"
  ),
  default_unit = c(
    "GJ/t", "GJ/t", "t/m3", "g/mol",
    "kg/kg", "t/m3", "kJ/kg/K"
  ),
  bridges = c(
    "yes", "yes", "yes", "yes",
    "no", "on_ask", "no"
  ),
  description = c(
    "Lower (net) heating value",
    "Higher (gross) heating value",
    "Density at the commodity's reference state",
    "Molar mass",
    "Element mass fraction from ultimate analysis ('mol/mol' for mole basis)",
    "Density variant -- liquid, compressed, true vs bulk",
    "Specific heat capacity; reference only, cannot bridge without a temperature delta"
  ),
  stringsAsFactors = FALSE
)

# Distributions understood by the `dist` column. `value` is always the
# deterministic point estimate; the rest describe uncertainty around it and are
# never read by `convert()`.
#   point/NA            value
#   uniform             min, max
#   triangular, pert    min, value (the mode), max
#   normal, lognormal   value (the mean), sd
.PROPERTY_DISTS <- c(
  "point", "uniform", "triangular", "pert", "normal", "lognormal"
)

# Property names that almost certainly belong in `@emis` rather than `@property`
# (see the header note). Matched case-insensitively against the whole name.
.PROPERTY_EMIS_LIKE <- paste0(
  "^(co2|ch4|n2o|so2|sox|nox|pm|ghg|",
  "carbon_content|carbon_intensity|emis|emission|emissions)"
)

#' Recognised commodity properties
#'
#' @description
#' The registry of physical properties understood by `commodity@property` and,
#' in turn, by [convert()]. `quantity` declares which dimension pair a property
#' relates, which is what lets a conversion cross between measures of the same
#' commodity -- `lhv` in "GJ/t" links Energy and Mass, `density` in "t/m3" links
#' Mass and Volume.
#'
#' Property names are not restricted to this list: an unrecognised name is kept,
#' with a warning, so that data the package has not anticipated can still be
#' recorded. Names ending in `*` are prefix rules -- `frac_C`, `frac_H` and
#' `frac_ash` all match `frac_*`.
#'
#' @return A data.frame with columns `property`, `quantity`, `default_unit`,
#'   `bridges` and `description`.
#'
#' @seealso [newCommodity()], [commodity_property()]
#' @family commodity
#' @export
#'
#' @examples
#' commodity_properties()
commodity_properties <- function() {
  .COMMODITY_PROPERTIES
}

# Registry row for a property name: exact match first, then prefix rules.
# Returns NULL when the name is not recognised.
#' @noRd
.property_def <- function(property) {
  reg <- .COMMODITY_PROPERTIES
  i <- match(property, reg$property)
  if (!is.na(i)) return(reg[i, , drop = FALSE])
  rules <- reg[endsWith(reg$property, "*"), , drop = FALSE]
  if (nrow(rules) == 0) return(NULL)
  stem <- sub("\\*$", "", rules$property)
  # longest stem wins, so "density_*" is preferred over a hypothetical "d*"
  hit <- which(startsWith(property, stem) & nchar(property) > nchar(stem))
  if (length(hit) == 0) return(NULL)
  rules[hit[which.max(nchar(stem[hit]))], , drop = FALSE]
}

# Read `@property` safely from objects serialised before the slot existed.
# `save_scenario()`/`load_scenario()` use plain save()/load() with no
# updateObject() step, so every read must go through this.
#' @noRd
.comm_property <- function(x) {
  if (!methods::.hasSlot(x, "property")) {
    return(new("commodity")@property)
  }
  x@property
}

#' Physical property of a commodity
#'
#' @description
#' Read a single property out of `commodity@property`, or the whole table.
#' Safe on commodity objects created before the slot existed.
#'
#' @param x a `commodity` object.
#' @param property character, name of the property, e.g. `"lhv"`. If `NULL`
#'   (the default), the whole property table is returned.
#' @param stat character, which part of the row to return. `"value"` (the
#'   default) gives the deterministic point estimate; `"range"` gives
#'   `c(min, max)`; `"row"` gives the whole row as a one-row data.frame.
#'
#' @return For `stat = "value"`, `"min"`, `"max"` or `"sd"` a numeric of length
#'   one; for `"range"` a numeric of length two; for `"unit"`, `"dist"` or
#'   `"comment"` a character of length one; for `"row"` a one-row data.frame.
#'   Missing properties give `NA` (or a zero-row data.frame for `"row"`).
#'
#' @seealso [commodity_properties()], [newCommodity()]
#' @family commodity
#' @export
#'
#' @examples
#' COA <- newCommodity("COA",
#'   unit = "PJ",
#'   property = data.frame(
#'     property = c("lhv", "density"),
#'     value = c(25.8, 0.85),
#'     min = c(24.1, 0.75),
#'     max = c(27.2, 0.95),
#'     dist = c("uniform", "triangular"),
#'     unit = c("GJ/t", "t/m3")
#'   )
#' )
#' commodity_property(COA, "lhv")
#' commodity_property(COA, "lhv", "range")
#' commodity_property(COA)
commodity_property <- function(x, property = NULL,
                               stat = c("value", "range", "min", "max", "sd",
                                        "unit", "dist", "comment", "row")) {
  if (!methods::is(x, "commodity")) {
    stop("`x` must be a commodity object, not ", class(x)[1], ".")
  }
  df <- .comm_property(x)
  if (is.null(property)) return(df)
  stat <- match.arg(stat)
  if (length(property) != 1L || is.na(property)) {
    stop("`property` must be a single, non-missing name.")
  }
  row <- df[!is.na(df$property) & df$property == property, , drop = FALSE]
  if (stat == "row") return(row)
  if (nrow(row) == 0) {
    return(switch(stat,
      range = c(NA_real_, NA_real_),
      unit = NA_character_, dist = NA_character_, comment = NA_character_,
      NA_real_
    ))
  }
  # a duplicate key is a validation warning, not an error -- take the first
  if (stat == "range") return(c(row$min[1], row$max[1]))
  row[[stat]][1]
}

# Warn-only validation of a `@property` table. Warnings rather than errors,
# because the table is reference data: a questionable value should be visible
# but must never block model building. Dimensional validation of `unit` against
# the registry's `quantity` needs the unit parser and lands with it.
#' @noRd
.check_commodity_property <- function(df, name = "") {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(invisible(df))
  if (!"property" %in% names(df)) return(invisible(df))
  where <- if (nzchar(name[1])) paste0(" in commodity '", name[1], "'") else ""
  warn <- function(...) {
    warning("@property", where, ": ", ..., call. = FALSE)
  }
  # `.data2slots()` has not run yet, so columns the user omitted are absent --
  # treat them as all-NA rather than indexing into nothing.
  col <- function(nm, default = NA) {
    if (nm %in% names(df)) df[[nm]] else rep(default, nrow(df))
  }
  prop <- as.character(col("property", NA_character_))
  value <- suppressWarnings(as.numeric(col("value", NA_real_)))
  vmin <- suppressWarnings(as.numeric(col("min", NA_real_)))
  vmax <- suppressWarnings(as.numeric(col("max", NA_real_)))
  vsd <- suppressWarnings(as.numeric(col("sd", NA_real_)))
  dist <- as.character(col("dist", NA_character_))
  unit <- as.character(col("unit", NA_character_))

  # -- names ----------------------------------------------------------------
  dup <- unique(prop[duplicated(prop) & !is.na(prop)])
  if (length(dup)) {
    warn("duplicated property name(s): ", paste(dup, collapse = ", "),
         ". Only the first row of each is used; use distinct names such as ",
         "'density' and 'density_liquid' for state variants.")
  }
  is_emis_like <- !is.na(prop) & grepl(.PROPERTY_EMIS_LIKE, prop,
                                       ignore.case = TRUE)
  # emission-like names are also unrecognised, but the pointer to `@emis` is the
  # useful message -- don't say it twice.
  unknown <- prop[!is.na(prop) & !is_emis_like &
                    vapply(prop, \(p) is.null(.property_def(p)), logical(1))]
  if (length(unknown)) {
    warn("unrecognised property name(s): ", paste(unique(unknown), collapse = ", "),
         ". They are kept as reference data but `convert()` will ignore them. ",
         "See `commodity_properties()` for the recognised names.")
  }
  emis_like <- prop[is_emis_like]
  if (length(emis_like)) {
    warn("property name(s) ", paste(unique(emis_like), collapse = ", "),
         " look like emission factors. `@property` describes the commodity ",
         "itself; a factor whose numerator is a different commodity belongs ",
         "in `@emis`, which the model actually reads.")
  }

  # -- values ---------------------------------------------------------------
  bad <- !is.na(prop) & (is.na(value) | !is.finite(value))
  if (any(bad)) {
    warn("missing or non-finite value(s) for: ",
         paste(prop[bad], collapse = ", "), ".")
  }
  neg <- !is.na(value) & is.finite(value) & value < 0
  if (any(neg)) {
    warn("negative value(s) for: ", paste(prop[neg], collapse = ", "),
         ". Physical properties are expected to be non-negative.")
  }
  no_unit <- !is.na(prop) & (is.na(unit) | !nzchar(unit))
  if (any(no_unit)) {
    warn("missing unit for: ", paste(prop[no_unit], collapse = ", "),
         ". A property without a unit cannot be used for conversion.")
  }
  # composition fractions must not over-account for the substance
  frac <- !is.na(prop) & startsWith(prop, "frac_") & !is.na(value)
  if (any(frac) && sum(value[frac]) > 1 + 1e-8) {
    warn("composition fractions (frac_*) sum to ",
         format(sum(value[frac]), digits = 4), ", above 1.")
  }

  # -- uncertainty ----------------------------------------------------------
  bad_dist <- !is.na(dist) & nzchar(dist) & !(dist %in% .PROPERTY_DISTS)
  if (any(bad_dist)) {
    warn("unknown distribution(s): ", paste(unique(dist[bad_dist]), collapse = ", "),
         ". Expected one of: ", paste(.PROPERTY_DISTS, collapse = ", "), ".")
  }
  lo <- !is.na(vmin) & !is.na(value) & vmin > value
  hi <- !is.na(vmax) & !is.na(value) & value > vmax
  if (any(lo | hi)) {
    warn("value outside [min, max] for: ",
         paste(prop[lo | hi], collapse = ", "), ".")
  }
  bad_sd <- !is.na(vsd) & vsd <= 0
  if (any(bad_sd)) {
    warn("non-positive sd for: ", paste(prop[bad_sd], collapse = ", "), ".")
  }
  bad_ln <- !is.na(dist) & dist == "lognormal" & !is.na(value) & value <= 0
  if (any(bad_ln)) {
    warn("lognormal distribution with a non-positive value for: ",
         paste(prop[bad_ln], collapse = ", "), ".")
  }
  # parameters required by the declared distribution
  needs_range <- !is.na(dist) & dist %in% c("uniform", "triangular", "pert") &
    (is.na(vmin) | is.na(vmax))
  if (any(needs_range)) {
    warn("distribution needs both `min` and `max` for: ",
         paste(prop[needs_range], collapse = ", "), ".")
  }
  needs_sd <- !is.na(dist) & dist %in% c("normal", "lognormal") & is.na(vsd)
  if (any(needs_sd)) {
    warn("distribution needs `sd` for: ",
         paste(prop[needs_sd], collapse = ", "), ".")
  }
  # uncertainty supplied with nothing to interpret it
  orphan <- (is.na(dist) | !nzchar(dist)) &
    (!is.na(vmin) | !is.na(vmax) | !is.na(vsd))
  if (any(orphan)) {
    warn("uncertainty given without `dist` for: ",
         paste(prop[orphan], collapse = ", "),
         ". Set `dist` so the range can be interpreted.")
  }
  invisible(df)
}

# Images and icons ###########################################################
#
# Images are a convention on `@misc`, not slots: `misc$image` and `misc$icon`
# hold a path or a URL. This mirrors `technology`, where the techspec `image`
# scalar lands in `misc$image` (see `R/techspec.R`).
# ############################################################################

# Fold `image =` / `icon =` constructor arguments into a `misc` list.
#' @noRd
.add_misc_image <- function(misc = list(), image = NULL, icon = NULL) {
  if (is.null(misc)) misc <- list()
  if (!is.list(misc)) stop("`misc` must be a list.")
  if (!is.null(image)) misc$image <- as.character(image)
  if (!is.null(icon)) misc$icon <- as.character(icon)
  misc
}

#' Image or icon attached to a model object
#'
#' @description
#' Resolve the `misc$image` (or `misc$icon`) convention on a model object and
#' report whether the reference can be used. Local paths are checked for
#' existence; URLs are reported as such without a network round-trip.
#'
#' @param object a model object, e.g. a `commodity` or `technology`.
#' @param which character, `"image"` (the default) or `"icon"`.
#'
#' @return A list with `path` (character, or `NULL` when nothing is set) and
#'   `status`, one of `"none"`, `"url"`, `"ok"` or `"missing"`.
#'
#' @family commodity
#' @export
#'
#' @examples
#' COA <- newCommodity("COA", image = "https://example.org/coal.png")
#' object_image(COA)
object_image <- function(object, which = c("image", "icon")) {
  which <- match.arg(which)
  misc <- if (methods::.hasSlot(object, "misc")) object@misc else list()
  path <- misc[[which]]
  if (is.null(path) || length(path) == 0 || !nzchar(path[1]) || is.na(path[1])) {
    return(list(path = NULL, status = "none"))
  }
  path <- as.character(path[1])
  status <- if (grepl("^https?://", path)) {
    "url"
  } else if (file.exists(path)) {
    "ok"
  } else {
    "missing"
  }
  list(path = path, status = status)
}

# Local file path of an object's image, or NULL. Used to default `report()`'s
# `image_file`: report templates take local files only, so URLs yield NULL.
#' @noRd
.object_image_file <- function(object, which = "image") {
  img <- object_image(object, which)
  if (identical(img$status, "ok")) img$path else NULL
}
