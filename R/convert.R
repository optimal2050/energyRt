# convert <- function(x, ...) UseMethod("convert")
# @export convert
convert.character <- function(from, to, x = 1, database = "base",
                              currency_route = "deflate_then_fx",
                              basis = "gdp", from_year = NULL,
                              to_year = NULL, ...) {
  h_from <- .find_unit(from, database = database)
  h_to <- .find_unit(to, database = database)
  if (h_from$type != h_to$type) {
    stop("Different unit type")
  }
  cf <- .currency_factor(h_from, h_to, currency_route = currency_route,
                         basis = basis, from_year = from_year,
                         to_year = to_year)
  if (length(cf) > 1L && length(x) > 1L) {
    stop("`currency_route = \"both\"` returns one value per route, ",
         "so `x` must be one number", call. = FALSE)
  }
  conval <- x * (h_from$coef / h_to$coef) * cf
  names(conval) <- if (length(cf) > 1L) names(cf) else NULL
  conval
}

#' Convert units
#'
#' A currency may carry a four-digit year -- `MEUR2015`, `MUSD2020` -- because
#' money is the one unit whose value moves in time. Converting between two
#' currency-years needs an exchange rate and a price index, and the two can be
#' applied in either order: deflating in the source currency and converting at
#' the target year is not the same as converting at the source year and
#' deflating in the target currency. The gap between them IS the change in the
#' real exchange rate over the interval, and `convert()` warns when it is
#' material.
#'
#' Money with no year on either side is read at [currency_base_year()] and
#' behaves exactly as it always did.
#'
#' @param from character of length one with unit name
#' @param to character of length one with unit name
#' @param x numeric vector with data to convert
#' @param database units database
#' @param currency_route how to combine the exchange rate with the deflators:
#'   `"deflate_then_fx"` (the default) deflates in the source currency and
#'   converts at the target year, which is the World Bank and IPCC convention;
#'   `"fx_then_deflate"` converts at the source year and deflates in the
#'   target currency; `"both"` returns one value per route, named.
#' @param basis which price index deflates, `"gdp"` (the default) or `"cpi"`.
#'   Technology cost data is often published on a CPI basis.
#' @param from_year,to_year the currency year, as an alternative to writing it
#'   into the unit. `convert(1, "MEUR", "MUSD", from_year = 2015,
#'   to_year = 2020)` is `convert(1, "MEUR2015", "MUSD2020")`.
#' @param ... currently ignored
#'
#' @return numeric vector with converted values, or one value per route when
#'   `currency_route = "both"`.
#' @rdname convert
#'
#' @include utils.R
#' @method convert character
#' @export
#'
#' @examples
#' convert("MWh", "kWh")
#' convert("kWh", "MJ")
#' convert("kWh/kg", "MJ/t", 1e-3)
#' convert("cents/kWh", "USD/MWh")
#'
#' # money with a year
#' convert(1, "MUSD2015", "MUSD2020")    # an inflation question
#' convert(1, "MEUR2020", "MUSD2020")    # an exchange-rate question
#' convert(1, "MEUR2015", "MUSD2020", currency_route = "both")
setMethod("convert", c("character"), convert.character)

convert.numeric <- function(x = 1, from, to, database = "base",
                            currency_route = "deflate_then_fx",
                            basis = "gdp", from_year = NULL,
                            to_year = NULL, ...) {
  h_from <- .find_unit(from, database = database)
  h_to <- .find_unit(to, database = database)
  if (h_from$type != h_to$type) {
    stop("Different unit type")
  }
  cf <- .currency_factor(h_from, h_to, currency_route = currency_route,
                         basis = basis, from_year = from_year,
                         to_year = to_year)
  if (length(cf) > 1L && length(x) > 1L) {
    stop("`currency_route = \"both\"` returns one value per route, ",
         "so `x` must be one number", call. = FALSE)
  }
  conval <- x * (h_from$coef / h_to$coef) * cf
  names(conval) <- if (length(cf) > 1L) names(cf) else NULL
  conval
}

#' @rdname convert
#' @method convert numeric
#' @export
#'
#' @examples
#' convert(1000, "kWh", "MWh")
#' convert("kWh", "MJ")
#' convert(1, "kWh/kg", "MJ/t")
#' convert(5, "cents/kWh", "USD/MWh")
setMethod("convert", c("numeric"), convert.numeric)

#' Basic units conversion database for `convert` methods
"convert_data"

#' Unit prefix multipliers
#'
#' The multiplier each unit prefix stands for, used by `convert()` to strip a
#' prefix off a unit string before looking the unit itself up. `prefix` is keyed
#' by prefix SYMBOL and `prefix_longname` by the written-out name; the two carry
#' identical values and differ only in their names.
#'
#' Note `m` is the symbol for *micro* (1e-06) here, not milli.
#'
#' @format A named numeric array of 11 multipliers, from `p`/`pico` (1e-12) to
#'   `Z`/`zetta` (1e+21).
#' @name prefix
#' @rdname prefix
#' @examples
#' prefix[["k"]]
#' prefix_longname[["kilo"]]
"prefix"

#' @rdname prefix
"prefix_longname"

setGeneric("add_to_convert", function(type, unit, coef, ...) {
                    # alias,
                    # SI_prefixes, database, update,
                    # ...) {
  standardGeneric("add_to_convert")
})

#' Add units to convert function
#'
#' @param type character, type of the unit (one of "Energy", "Power", "Mass", "Time", "Length", "Area", "Pressure", "Density", "Volume", "Flow Rates", "Currency").
#' @param unit character, the name of the new unit to add to the `database`.
#' @param coef numeric, convert factor to the base unit of this type (see the first column of `convert_data[[database]][[type]]`).
#' @param SI_prefixes logical, can be used with `SI` prefixes, FALSE by default.
#' @param alias character vector, alternative name(s) for the same unit.
#' @param database character name of a database with units (`base` by default, other options are not implemented yet).
#' @param update logical; when the unit already exists with different values,
#'   `TRUE` (default) replaces them and `FALSE` raises an error instead.
#'
#' @return updated `convert_data` in the `.GlobalEnv`, the values will not update the package data.
#'
#' @aliases convert add_to_convert
#' @rdname convert
#' @export
#'
#' @examples
#' ## Not run:
#' convert_data$base$Currency
#' add_to_convert("Currency", unit = "JPY", coef = 140)
#' convert_data$base$Currency
#' ## End(Not run)
setMethod(
  "add_to_convert",
  signature(
    type = "character", unit = "character", coef = "numeric"
    # alias = "character", SI_prefixes = "logical", database = "character",
    # update = "logical"
  ),
  function(type, unit, coef, alias = "",
           SI_prefixes = FALSE, database = "base", update = TRUE) {
    SI <- SI_prefixes # rename
    convert_data <- get("convert_data", globalenv())
    alias <- unique(alias)
    if (!exists("convert_data", where = .GlobalEnv)) data("convert_data")
    if (all(names(convert_data[[database]]) != type)) {
      if (!SI_prefixes) {
        stop("Adding alternative databases is not implemented yet")
        # stop("First unit must be standart")
      }
      if (alias[1] != "") {
        # one column per name in one array: the loop this replaces rebuilt the
        # element on every pass, so only the last alias survived
        nms <- c(unit, alias)
        convert_data[[database]][[type]] <- array(
          rep(c(coef, SI_prefixes), length(nms)),
          dim = c(2, length(nms)),
          dimnames = list(c("coef", "SI_prefixes"), nms))
      } else {
        convert_data[[database]][[type]] <- array(c(
          coef,
          SI_prefixes
        ), dim = c(2, 1), dimnames = list(c("coef", "SI_prefixes"), unit))
      }
    } else if (any(unit == dimnames(convert_data[[database]][[type]])[[2]])) {
      if (any(convert_data[[database]][[type]][, unit] !=
              c(coef, SI_prefixes)) && !update) {
        stop("Unit '", unit, "' already exists (use `update = TRUE`?)") # ???
      }
      if (dimnames(convert_data[[database]][[type]])[[2]][1] == unit &&
          convert_data[[database]][[type]][SI_prefixes, unit] != coef) {
        stop("Base unit '(", unit, " = 1)' cannot be redefined") # ???
      }
      convert_data[[database]][[type]]["SI_prefixes", unit] <- SI_prefixes
      convert_data[[database]][[type]]["coef", unit] <- coef
      if (alias[1] != "") {
        convert_data[[database]][[type]]["SI_prefixes", alias] <- SI_prefixes
        convert_data[[database]][[type]]["coef", alias] <- coef
      }
    } else if (alias[1] != "") {
      # `unit` once plus every alias: the loop this replaces appended `unit`
      # again on each pass, which is what the de-duplication below cleaned up
      cur <- convert_data[[database]][[type]]
      nms <- c(unit, alias)
      convert_data[[database]][[type]] <- array(
        c(cur, rep(c(coef, SI_prefixes), length(nms))),
        dim = c(2, dim(cur)[2] + length(nms)),
        dimnames = list(c("coef", "SI_prefixes"), c(colnames(cur), nms))
      )
    } else {
      convert_data[[database]][[type]] <- array(
        c(
          convert_data[[database]][[type]],
          coef, SI_prefixes
        ),
        dim = c(2, 1 + dim(convert_data[[database]][[type]])[2]),
        dimnames = list(c("coef", "SI_prefixes"), c(
          dimnames(convert_data[[database]][[type]])[[2]],
          unit
        ))
      )
    }
    assign("convert_data", convert_data, globalenv())
  }
)

.split_to_unit <- function(st, database = "base") {
  ss <- unique(strsplit(st, "[-+*/^]"))[[1]]
  ss <- ss[suppressWarnings(is.na(as.numeric(ss)))]
  h <- lapply(ss, function(x) .prefix_find_unit(x, database))
  g <- array(sapply(h, function(x) x$coef),
    dim = length(ss),
    dimnames = list(ss)
  )
  g2 <- array(sapply(h, function(x)
    dimnames(convert_data[[database]][[x$type]])[[2]][1]),
    dim = length(ss), dimnames = list(ss)
  )
  s2 <- st
  for (k in ss[sort(nchar(ss), index.return = TRUE, decreasing = TRUE)$ix]) {
    s2 <- gsub(
      k,
      g2[k], s2
    )
  }
  for (k in ss[sort(nchar(ss), index.return = TRUE, decreasing = TRUE)$ix]) {
    st <- gsub(
      k,
      g[k], st
    )
  }
  out <- list(type = s2, coef = eval(parse(text = st)))
  # Carry the money through a compound. `MEUR2015/GW` is still euros of 2015,
  # and the pairwise factor has to know that after the string has been reduced
  # to its base-unit signature, which keeps no trace of either.
  cur <- which(vapply(h, function(x) identical(x$type, "Currency"), TRUE))
  if (length(cur)) {
    out$currency <- h[[cur[1]]]$currency
    out$year <- h[[cur[1]]]$year
  }
  out
}

.find_unit <- function(unit, database = "base") {
  g <- .prefix_find_unit(unit, database = database, FALSE)
  if (!is.null(g)) {
    return(g)
  } else {
    g <- .split_to_unit(unit, database = database)
    g2 <- .prefix_find_unit(g$type, database = database, FALSE)
    if (!is.null(g2)) {
      g2$coef <- g2$coef * g$coef
      g2$currency <- g2$currency %||% g$currency
      g2$year <- g2$year %||% g$year
      return(g2)
    } else {
      return(g)
    }
  }
}

# The atom resolver proper. Scans types then columns, first match wins, and
# reports WHICH column matched so the wrapper can map a currency spelling to
# its ISO code.
#' @noRd
.prefix_find_unit_raw <- function(unit, database = "base",
                                  stop_on_error = TRUE) {
  if (!exists("convert_data", where = .GlobalEnv)) data("convert_data")
  i <- 1
  fl <- FALSE
  while (i <= length(convert_data[[database]])) {
    u <- convert_data[[database]][[i]]
    ss <- dimnames(u)[[2]]
    for (k in 1:dim(u)[[2]]) {
      if (ss[k] == unit) {
        return(list(
          type = names(convert_data[[database]])[i],
          unit_name = ss[k],
          coef = u["coef", k]
        ))
      } else {
        if (any(paste(dimnames(prefix)[[1]], ss[k], sep = "") ==
          unit)) {
          if (ss[k] == unit || (u["SI_prefixes", k] == 1 &&
            any(paste(dimnames(prefix)[[1]], ss[k], sep = "") == unit))) {
            return(list(
              type = names(convert_data[[database]])[i],
              unit_name = ss[k],
              coef = prefix[paste(dimnames(prefix)[[1]],
                ss[k],
                sep = ""
              ) == unit] * u[
                "coef",
                k
              ]
            ))
          }
        }
      }
    }
    i <- i + 1
  }
  if (stop_on_error) {
    stop("Unknown unit ", unit)
  } else {
    return(NULL)
  }
}

# setGeneric("convert", function(x, ...) standardGeneric("convert"))

# @method convert character character numeric


# setGeneric("convert", function(value, from, to, ...) standardGeneric("convert"))

# A four-digit suffix on a currency is a YEAR -- and nothing else is allowed to
# claim one. The stem must resolve to `Currency` on its own, so `m3`, `cm3` and
# the rest are untouched, and no spelling in the database ends in four digits.
#
# Resolution is prefix, then code, then year: `MUSD2020` is mega + USD + 2020,
# three layers where the database knows two. The year is stripped first because
# the prefix scan is first-match-wins over column names and would never see
# `USD` inside `USD2020`.
#' @noRd
.prefix_find_unit <- function(unit, database = "base", stop_on_error = TRUE) {
  g <- .prefix_find_unit_raw(unit, database = database, stop_on_error = FALSE)
  if (!is.null(g)) return(.tag_currency(g, year = NULL))
  if (grepl("[0-9]{4}$", unit)) {
    stem <- sub("[0-9]{4}$", "", unit)
    if (nzchar(stem)) {
      g <- .prefix_find_unit_raw(stem, database = database,
                                 stop_on_error = FALSE)
      if (!is.null(g) && identical(g$type, "Currency")) {
        y <- as.integer(substring(unit, nchar(unit) - 3L))
        return(.tag_currency(g, year = y))
      }
    }
  }
  if (stop_on_error) {
    stop("Unknown unit ", unit)
  }
  NULL
}

# Name the currency a resolved atom is in, so the pairwise factor can look up
# its rate. A currency with no year is tagged all the same: that is what lets
# an untagged side be read at the base year.
#' @noRd
.tag_currency <- function(g, year = NULL) {
  if (!identical(g$type, "Currency")) return(g)
  cd <- .currency_data("codes")
  code <- cd$code[match(g$unit_name, cd$spelling)]
  if (length(code) == 1L && !is.na(code)) g$currency <- code
  if (!is.null(year)) g$year <- year
  g
}
