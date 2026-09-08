# ========================================================================== #
# Weather transforms: one data stream -> many derived series.
#
# A weather object may carry named functions in `@misc$transform`; a consumer
# link (technology/storage/supply `@weather` row) selects one by name in its
# `transform` column. At interpolation, each referenced (weather, transform)
# pair is materialized ONCE into an ordinary derived weather object named
# `<WEATHER>_<TRANSFORM>`, and the link is repointed to it -- the backends
# only ever see plain `pWeather` series, so all four templates are untouched.
#
# The transform contract is `function(weather, ...) -> numeric`: the first
# argument is the stream's `wval` series; `...` receives every non-structural
# column of the SAME link row as a named scalar (NA cells are not forwarded).
# Parameter columns are user-invented -- the weather link slots accept
# undeclared columns (see `.open_slots` in R/data2slots.R).
#
# One name = one series: rows that share a (weather, transform) pair must
# forward identical arguments; differing parameterizations need their own
# names. Functions resolve misc -> session registry -> built-ins, so a model
# whose transforms all live in `@misc` is self-contained when serialized.
# ========================================================================== #

.weather_transform_registry <- new.env(parent = emptyenv())

# Fallbacks when a name is absent from the object's misc and the registry.
# Deliberately assumption-free: anything resource-specific (a power curve, a
# COP curve) belongs to the modeller, in `@misc$transform`.
.builtin_weather_transforms <- list(
  identity = function(weather, ...) weather,
  scale = function(weather, wscale = 1, ...) weather * wscale
)

# Link-table columns that are NOT transform arguments: identifiers and the
# declared multiplier bounds of each class's weather slot.
.weather_link_structural <- list(
  technology = c("vintage", "cluster", "weather", "transform", "comm",
                 "wafc.lo", "wafc.up", "wafc.fx",
                 "waf.lo", "waf.up", "waf.fx",
                 "wafs.lo", "wafs.up", "wafs.fx"),
  storage = c("vintage", "cluster", "weather", "transform",
              "waf.lo", "waf.up", "waf.fx",
              "inp.waf.lo", "inp.waf.up", "inp.waf.fx",
              "out.waf.lo", "out.waf.up", "out.waf.fx"),
  supply = c("cluster", "weather", "transform",
             "wava.lo", "wava.up", "wava.fx")
)

#' Register a weather transform for the session
#'
#' Adds a named transform to the session registry, the middle tier of the
#' lookup chain `weather@misc$transform` -> registry -> built-ins. Transforms
#' stored in a weather object's `misc$transform` list travel with the model
#' and take precedence; the registry serves interactive work and shared
#' helpers. Built-ins `identity` and `scale` (`weather * wscale`) are always
#' available.
#'
#' @param name single character, a valid object-name suffix (letters, digits,
#'   underscore, starting with a letter). The derived weather object is named
#'   `<WEATHER>_<name>`.
#' @param fun `function(weather, ...) -> numeric`: `weather` receives the
#'   stream's `wval` series; `...` receives the non-structural columns of the
#'   selecting link row as named scalars. Must return a numeric vector of the
#'   input's length.
#'
#' @return `fun`, invisibly.
#' @seealso [materialize_weather()]
#' @export
#'
#' @examples
#' register_weather_transform("power_v112",
#'   function(weather, wscale = 1, ...) {
#'     v <- data.frame(speed = c(0, 3, 7, 12, 25, 25.01),
#'                     af = c(0, 0, 0.4, 1, 1, 0))
#'     approx(v$speed, v$af, pmax(wscale * weather, 0),
#'            yleft = 0, yright = 0)$y
#'   })
register_weather_transform <- function(name, fun) {
  if (!check_name(name)) {
    stop("Transform name must be a valid object-name suffix ",
         "(letters, digits, underscore, starting with a letter): ", name)
  }
  if (!is.function(fun)) stop("`fun` must be a function")
  assign(name, fun, envir = .weather_transform_registry)
  invisible(fun)
}

# misc -> registry -> built-ins; loud miss.
.resolve_weather_transform <- function(wobj, tname) {
  tf <- wobj@misc$transform
  if (is.list(tf) && is.function(tf[[tname]])) return(tf[[tname]])
  if (exists(tname, envir = .weather_transform_registry, inherits = FALSE)) {
    return(get(tname, envir = .weather_transform_registry))
  }
  fun <- .builtin_weather_transforms[[tname]]
  if (is.function(fun)) return(fun)
  stop("Unknown weather transform \"", tname, "\" on weather object \"",
       wobj@name, "\". Store it in the object's misc$transform list, ",
       "register_weather_transform(\"", tname, "\", ...), or use a ",
       "built-in (", paste(names(.builtin_weather_transforms),
                           collapse = ", "), ").")
}

#' Materialize one weather transform
#'
#' Applies a named transform to a weather object and returns the DERIVED
#' weather object -- the same clone interpolation creates when a link selects
#' that transform. For inspection and reporting; the input object is not
#' modified.
#'
#' @param weather a `weather` object.
#' @param transform single character, the transform name; resolved
#'   `weather@misc$transform` -> session registry -> built-ins.
#' @param ... arguments forwarded to the transform function (what a link row's
#'   parameter columns would carry).
#'
#' @return a `weather` object named `<WEATHER>_<transform>` whose `wval` (and
#'   `defVal`) carry the transformed series; `misc$transform_source` records
#'   the provenance.
#' @seealso [register_weather_transform()]
#' @export
#'
#' @examples
#' w <- newWeather("W_SPEED", weather = data.frame(
#'   timeslice = c("s1", "s2"), wval = c(4, 12)))
#' w@misc$transform <- list(AF = function(weather, ...) pmin(weather / 10, 1))
#' materialize_weather(w, "AF")@weather
materialize_weather <- function(weather, transform, ...) {
  stopifnot(methods::is(weather, "weather"))
  .materialize_weather_clone(weather, transform, args = list(...))
}

.materialize_weather_clone <- function(wobj, tname, args) {
  if (!check_name(tname)) {
    stop("Weather transform name \"", tname, "\" on \"", wobj@name,
         "\" is not a valid object-name suffix (letters, digits, ",
         "underscore, starting with a letter).")
  }
  fun <- .resolve_weather_transform(wobj, tname)
  vals <- do.call(fun, c(list(wobj@weather$wval), args))
  if (!is.numeric(vals) || length(vals) != nrow(wobj@weather)) {
    stop("Weather transform \"", tname, "\" on \"", wobj@name, "\" must ",
         "return a numeric vector of the series length (",
         nrow(wobj@weather), "), got ", class(vals)[1], " of length ",
         length(vals), ".")
  }
  cl <- wobj
  # single-underscore join, the same convention as the _VIN / _CL variant
  # suffixes; collisions with declared names are checked by the caller
  cl@name <- paste0(wobj@name, "_", tname)
  cl@weather$wval <- as.numeric(vals)
  # the default fills absent slices; it must pass through the same lens
  cl@defVal <- as.numeric(do.call(fun, c(list(wobj@defVal), args)))[1]
  cl@misc$transform <- NULL
  cl@misc$transform_source <- list(weather = wobj@name, transform = tname,
                                   args = args)
  cl
}

# Harvest one process object's weather links: validate, register the pending
# (weather, transform, args) tuples in `state`, repoint the rows. Returns the
# modified object, or NULL when it declares no transform.
.harvest_weather_links <- function(el, state) {
  wt <- el@weather
  if (is.null(wt) || !nrow(wt) || !"transform" %in% names(wt)) return(NULL)
  act <- which(!is.na(wt$transform) & wt$transform != "")
  if (!length(act)) return(NULL)
  structural <- .weather_link_structural[[class(el)[1]]]
  for (i in act) {
    wname <- wt$weather[i]
    tname <- wt$transform[i]
    wobj <- state$wobjs[[wname]]
    if (is.null(wobj)) {
      stop(class(el)[1], " \"", el@name, "\" declares transform \"", tname,
           "\" on unknown weather object \"", wname, "\".")
    }
    args <- as.list(wt[i, setdiff(names(wt), structural), drop = FALSE])
    args <- args[!vapply(args, function(v) is.na(v), logical(1))]
    key <- paste0(wname, "_", tname)
    prior <- state$pending[[key]]
    if (is.null(prior)) {
      if (key %in% state$taken) {
        stop("Materialized weather name \"", key, "\" collides with a ",
             "declared object. Rename the transform or the object.")
      }
      state$pending[[key]] <- list(weather = wname, transform = tname,
                                   args = args)
    } else if (!identical(prior$args, args)) {
      stop("Weather transform \"", tname, "\" on \"", wname, "\" is used ",
           "with differing arguments across links. One name = one series: ",
           "give each parameterization its own transform name.")
    }
    wt$weather[i] <- key
    wt$transform[i] <- NA_character_
  }
  el@weather <- wt
  el
}

# The interpolation stage. Runs on the internal build copy of the model,
# after `expand_variants()` (each variant owns its link rows) and before
# `sets$weather` is collected, so the derived objects join the set like any
# declared weather. Non-destructive: the caller's model is untouched.
materialize_weather_transforms <- function(mod, verbose = FALSE) {
  state <- new.env(parent = emptyenv())
  state$wobjs <- list()
  state$pending <- list()

  state$taken <- character()
  collect <- function(r) {
    for (nm in names(r@data)) {
      el <- r@data[[nm]]
      if (methods::is(el, "repository")) {
        collect(el)
      } else {
        if (.hasSlot(el, "name")) state$taken <- c(state$taken, el@name)
        if (methods::is(el, "weather")) state$wobjs[[el@name]] <- el
      }
    }
  }
  for (r in mod@data) if (methods::is(r, "repository")) collect(r)

  rewrite <- function(r) {
    for (nm in names(r@data)) {
      el <- r@data[[nm]]
      if (methods::is(el, "repository")) {
        r@data[[nm]] <- rewrite(el)
      } else if (class(el)[1] %in% names(.weather_link_structural)) {
        res <- .harvest_weather_links(el, state)
        if (!is.null(res)) r@data[[nm]] <- res
      }
    }
    r
  }
  for (i in seq_along(mod@data)) {
    if (methods::is(mod@data[[i]], "repository")) {
      mod@data[[i]] <- rewrite(mod@data[[i]])
    }
  }

  if (!length(state$pending)) return(mod)
  clones <- .weather_transform_build_clones(state, verbose)
  do.call(add, c(list(mod), unname(clones)))
}

.weather_transform_build_clones <- function(state, verbose) {
  clones <- lapply(state$pending, function(p) {
    .materialize_weather_clone(state$wobjs[[p$weather]], p$transform, p$args)
  })
  if (verbose) {
    message("Materialized ", length(clones), " weather transform(s): ",
            paste(vapply(clones, slot, "", "name"), collapse = ", "))
  }
  clones
}

# Guard used by aggregate_model_regions(): any process link naming a transform
# means the model carries streams meant for a nonlinear lens.
.assert_no_weather_transforms <- function(mod) {
  offender <- NULL
  scan <- function(r) {
    for (nm in names(r@data)) {
      el <- r@data[[nm]]
      if (methods::is(el, "repository")) {
        scan(el)
      } else if (class(el)[1] %in% names(.weather_link_structural)) {
        wt <- el@weather
        if (!is.null(wt) && nrow(wt) && "transform" %in% names(wt) &&
            any(!is.na(wt$transform) & wt$transform != "")) {
          offender <<- c(offender, el@name)
        }
      }
    }
  }
  for (r in mod@data) if (methods::is(r, "repository")) scan(r)
  if (!is.null(offender)) {
    stop("The model declares weather transforms (",
         paste(unique(offender), collapse = ", "), "): a regional mean of ",
         "the stream does not commute with a nonlinear transform. ",
         "Interpolate/materialize the transforms first, or aggregate the ",
         "derived model.", call. = FALSE)
  }
  invisible(TRUE)
}
