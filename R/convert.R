# convert <- function(x, ...) UseMethod("convert")
# @export convert
convert.character <- function(from, to, x = 1, database = "base", ...) {
  h_from <- .find_unit(from, database = database)
  h_to <- .find_unit(to, database = database)
  if (h_from$type != h_to$type) {
    stop("Different unit type")
  }
  conval <- x * (h_from$coef / h_to$coef)
  names(conval) <- NULL
  conval
}

#' Convert units
#'
#' @param from character of length one with unit name
#' @param to character of length one with unit name
#' @param x numeric vector with data to convert
#' @param database units database
#' @param ... currently ignored
#'
#' @return numeric vector with converted values
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
setMethod("convert", c("character"), convert.character)

convert.numeric <- function(x = 1, from, to, database = "base", ...) {
  h_from <- .find_unit(from, database = database)
  h_to <- .find_unit(to, database = database)
  if (h_from$type != h_to$type) {
    stop("Different unit type")
  }
  conval <- x * (h_from$coef / h_to$coef)
  names(conval) <- NULL
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
#'
#' @return updated `convert_data` in the `.GlobalEnv`, the values will not update the package data.
#'
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
        for (i in 1:length(alias)) { #ToDo: rewrite
          convert_data[[database]][[type]] <- array(
            c(coef, SI_prefixes, coef, SI_prefixes),
            dim = c(2,2),
            dimnames = list(c("coef", "SI_prefixes"), c(unit, alias[i])))
        }
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
      for (i in 1:length(alias)) {
        convert_data[[database]][[type]] <- array(
          c(convert_data[[database]][[type]], coef, SI_prefixes,
            coef, SI_prefixes),
          dim = c(2, 2 + dim(convert_data[[database]][[type]])[2]),
          dimnames = list(
            c("coef", "SI_prefixes"),
            c(dimnames(convert_data[[database]][[type]])[[2]], unit, alias[i]))
        )
      }
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
    jj <- duplicated(colnames(convert_data[[database]][[type]])) #!!! quick fix
    convert_data[[database]][[type]] <- convert_data[[database]][[type]][,!jj]
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
  list(type = s2, coef = eval(parse(text = st)))
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
      return(g2)
    } else {
      return(g)
    }
  }
}

.prefix_find_unit <- function(unit, database = "base", stop_on_error = TRUE) {
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
          coef = u["coef", k]
        ))
      } else {
        if (any(paste(dimnames(prefix)[[1]], ss[k], sep = "") ==
          unit)) {
          if (ss[k] == unit || (u["SI_prefixes", k] == 1 &&
            any(paste(dimnames(prefix)[[1]], ss[k], sep = "") == unit))) {
            return(list(
              type = names(convert_data[[database]])[i],
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


