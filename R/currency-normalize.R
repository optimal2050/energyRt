## Bringing a model's money to one currency-year.
##
## `.check_model_currency()` asks whether the objects agree with the model.
## This answers the harder question: where they disagree but a conversion
## exists, what is each object's factor, and what was it built from.
##
## The target set is DERIVED, not listed. Every parameter's unit is declared
## once as `unit:` in data-raw/parameters.yml and baked into `.col_param`, so
## the columns denominated in money are exactly those whose formula carries the
## `{currency}` token -- 39 of them across nine classes. A cost column added to
## a class later is normalised the day its unit is declared, with nothing here
## to update; one renamed cannot leave a stale entry behind.
##
## Only the currency moves. `{currency}/{capacity}` converts by the currency
## factor alone: a technology pricing capacity in GW while the model counts in
## MW is a separate error that nothing here corrects, which is the limitation
## `vignettes/articles/units.Rmd` states plainly.
##
## An object that declares nothing is in the MODEL's currency -- that is what a
## model-level declaration means -- so its factor is 1 when the target is the
## model's own currency and a real conversion when a target is asked for
## instead. Both cases are reported; neither is assumed quietly.
##
## Objects are left as authored. `interpolate_model()` normalises its BUILD
## copy, so `getData()` on a process returns the values written down and
## `getData()` on the scenario returns the values summed, with every factor
## applied recorded in `scen@misc$currency`.

#' @include currency-convert.R units.R
NULL

# The "slot.column" keys of one class that are denominated in money, from the
# parameter catalogue. `.unit_formulas_for()` is the same reader `getUnits()`
# uses, so a class alias (`config` -> `settings`) resolves identically.
#' @noRd
.currency_money_keys <- function(cls) {
  f <- .unit_formulas_for(cls)
  if (is.null(f) || !length(f)) return(character(0))
  u <- unlist(f, use.names = TRUE)
  u <- u[!is.na(u) & grepl("{currency}", u, fixed = TRUE)]
  if (!length(u)) return(character(0))
  # Currency has only ever appeared as a numerator, and a reciprocal would
  # need the inverse factor. Refuse rather than silently scale it the wrong
  # way, so the day a `{comm}/{currency}` formula is declared this stops.
  rec <- grepl("/[[:space:]]*[{]currency[}]", u)
  if (any(rec)) {
    stop("currency appears in the DENOMINATOR of ", sum(rec), " ", cls,
         " parameter(s): ", paste(names(u)[rec], collapse = ", "),
         "\nNormalisation would have to invert the factor for these.",
         call. = FALSE)
  }
  names(u)
}

# Multiply every money column of one object by `f`, and restate what it is
# priced in. The slot may be a data.frame (almost all of them) or a scalar.
#' @noRd
.currency_scale_object <- function(obj, f, target) {
  if (!isTRUE(is.finite(f))) return(obj)
  for (k in .currency_money_keys(class(obj)[1])) {
    sl <- sub("[.].*$", "", k)
    col <- if (identical(sl, k)) NA_character_ else sub("^[^.]+[.]", "", k)
    v <- tryCatch(methods::slot(obj, sl), error = function(e) NULL)
    if (is.null(v)) next
    if (is.data.frame(v)) {
      if (is.na(col) || !(col %in% names(v)) || !nrow(v)) next
      if (!is.numeric(v[[col]])) next
      methods::slot(obj, sl) <- .currency_mul_col(v, col, f)
    } else if (is.na(col) && is.numeric(v) && length(v)) {
      methods::slot(obj, sl) <- v * f
    }
  }
  .currency_declare(obj, target)
}

# `x[[col]] * f`, NAs left as NAs.
#' @noRd
.currency_mul_col <- function(x, col, f) {
  i <- !is.na(x[[col]])
  if (any(i)) x[[col]][i] <- x[[col]][i] * f
  x
}

# Restate an object's own currency. Written to whichever column it already
# carries: `costs` is the pre-0.92 spelling and an object serialized then
# still has it, which no renamer reaches inside a data.frame.
#' @noRd
.currency_declare <- function(obj, target) {
  u <- tryCatch(methods::slot(obj, "units"), error = function(e) NULL)
  if (!is.data.frame(u)) return(obj)
  cc <- if ("currency" %in% names(u)) "currency" else
    if ("costs" %in% names(u)) "costs" else return(obj)
  if (!nrow(u)) u[1, ] <- NA
  u[[cc]][1] <- target
  methods::slot(obj, "units") <- u
  obj
}

# The same, for the model itself: once the costs are in the target, the model
# counts in it.
#' @noRd
.currency_declare_model <- function(mod, target) {
  cfg <- tryCatch(mod@config, error = function(e) NULL)
  if (is.null(cfg)) return(mod)
  mod@config <- .currency_declare(cfg, target)
  mod
}

# Replace objects throughout a model's repositories, keyed by class and name.
# The walk is recursive: a repository may hold another.
#' @noRd
.currency_put_objects <- function(x, repl) {
  put <- function(lst) {
    for (j in seq_along(lst)) {
      o <- lst[[j]]
      if (methods::is(o, "repository")) {
        o@data <- put(o@data)
        lst[[j]] <- o
        next
      }
      if (!isS4(o)) next
      k <- tryCatch(paste(class(o)[1], o@name), error = function(e) NA)
      if (!is.na(k) && !is.null(repl[[k]])) lst[[j]] <- repl[[k]]
    }
    lst
  }
  x@data <- put(x@data)
  x
}

# What a group's factor should be CALLED. A conversion within one currency is a
# deflator and the route cannot matter; across two it is the route's name, the
# distinction the warning in `.currency_factor()` is about.
#' @noRd
.currency_note <- function(a, b, ya, yb, basis, currency_route) {
  if (identical(a, b)) {
    if (identical(ya, yb)) return("unchanged")
    return(paste(a, basis, "deflator"))
  }
  if (identical(ya, yb)) return(paste("exchange rate", ya))
  currency_route
}

# The currency and year a declaration resolves to. `bnEUR` is not a unit and
# `euros` is not a spelling, so either lands here as an error rather than as a
# factor of 1.
#' @noRd
.currency_parse <- function(u, what) {
  g <- tryCatch(.prefix_find_unit(u, stop_on_error = FALSE),
                error = function(e) NULL)
  if (is.null(g) || !identical(g$type, "Currency")) {
    stop(what, " declares '", u, "', which is not a currency unit.",
         "\nUse a spelling convert() knows (see currency_codes$spelling), ",
         "optionally with a four-digit year.", call. = FALSE)
  }
  list(code = g$currency, year = as.integer(g$year %||% currency_base_year()))
}

#' Convert a model's costs to one currency
#'
#' A model has one currency whether it says so or not: the objective adds every
#' cost together with coefficient 1. Where objects were priced in another one,
#' this converts them to a single currency-year and reports every factor it
#' applied.
#'
#' Called for its own sake it returns a model whose costs have moved.
#' [interpolate_model()] calls it on the copy it builds from, so a model's
#' objects keep the values they were written with while the scenario carries
#' the converted ones; the report is then in `scen@misc$currency`.
#'
#' An object that declares no currency is in the **model's** currency, which is
#' what a model-level declaration means. Its factor is therefore 1 when
#' `target` is the model's own currency -- the interpolation case, where such
#' objects are reported and left alone -- and a real conversion when another
#' `target` is asked for.
#'
#' @param mod a `model`.
#' @param target the currency-year to convert to, e.g. `"MUSD2020"`. Defaults
#'   to the model's own `config@units$currency`.
#' @param basis deflator basis, `"gdp"` (default) or `"cpi"`.
#' @param currency_route `"deflate_then_fx"` (default) or `"fx_then_deflate"`;
#'   see [convert()].
#' @param verbose print the per-group factors.
#' @return The model, with `misc$currency` carrying the report. Unchanged when
#'   it declares no currency, or when every factor is 1.
#' @seealso [convert()], [currency_fx], [currency_deflator]
#' @export
#' @examples
#' PV <- newTechnology("PV", units = data.frame(capacity = "GW",
#'                                              currency = "MEUR2015"),
#'                     invcost = data.frame(invcost = 700))
#' GAS_SUP <- newSupply("GAS_SUP", commodity = "GAS", unit = "PJ",
#'                      units = data.frame(currency = "MUSD2015"),
#'                      supply = data.frame(cost = 8))
#' mod <- newModel("demo", currency = "MUSD2020",
#'                 data = add(newRepository("r"), newCommodity("GAS"), PV,
#'                            GAS_SUP))
#'
#' mod <- normalize_model_currency(mod)
#' mod@misc$currency$objects          # one row per object, with its factor
#' mod@misc$currency$fx               # the rows the factors were built from
#'
#' # the costs as authored, and as normalised
#' getObject(mod, name = "PV")[[1]]@invcost$invcost
#' 700 * convert(1, "MEUR2015", "MUSD2020")
normalize_model_currency <- function(mod, target = NULL, basis = "gdp",
                                     currency_route = "deflate_then_fx",
                                     verbose = TRUE) {
  r <- .normalize_currency(mod, target = target, basis = basis,
                           currency_route = currency_route, verbose = verbose)
  if (is.null(r)) return(mod)
  r$model@misc$currency <- r$report
  r$model
}

# The worker. NULL when there is nothing to do -- no currency declared
# anywhere, or every factor exactly 1 -- so a caller can skip without copying
# a model, and so an untagged model interpolates bit-identically.
#' @noRd
.normalize_currency <- function(mod, target = NULL, basis = "gdp",
                                currency_route = "deflate_then_fx",
                                verbose = TRUE) {
  cur <- .model_currency(mod)
  tgt_s <- target %||% cur
  if (is.null(tgt_s)) return(NULL)
  objs <- tryCatch(getObject(mod), error = function(e) NULL)
  if (!length(objs)) return(NULL)
  # A class with no money column cannot be converted, and a commodity naming a
  # currency is not declaring anything about costs it does not have.
  money <- vapply(objs, function(o)
    length(.currency_money_keys(class(o)[1])) > 0L, logical(1))
  if (!any(money)) return(NULL)
  own <- vapply(objs, function(o) .object_units(o, "currency") %||%
                  NA_character_, character(1))
  # an untagged object is in the model's currency; with none declared it
  # claims nothing and is left out entirely
  from <- ifelse(is.na(own), cur %||% NA_character_, own)
  keep <- money & !is.na(from)
  if (!any(keep)) return(NULL)

  tgt <- .currency_parse(tgt_s, "the model")
  # One factor per DECLARATION, not per object: a hundred technologies priced
  # in MEUR2015 ask the same question once.
  decl <- unique(from[keep])
  fac <- stats::setNames(rep(NA_real_, length(decl)), decl)
  note <- stats::setNames(character(length(decl)), decl)
  fx <- list()
  dfl <- list()
  for (d in decl) {
    who <- if (d %in% own) paste0("object '", names(own)[!is.na(own) &
                                                           own == d][1], "'")
      else "the model"
    src <- .currency_parse(d, who)
    f <- convert(1, d, tgt_s, basis = basis, currency_route = currency_route)
    # The rows behind the factor APPLIED. `convert()` cannot be traced for
    # this: `.currency_factor()` evaluates both routes to measure their
    # divergence, so a trace around it also collects the rows of the route
    # that was not taken, which is not what the number came from. Re-reading
    # the chosen route is the same single implementation, called with the
    # codes and years the unit resolver already gave.
    .currency_trace_begin()
    tr <- tryCatch({
      .currency_one_route(src$code, tgt$code, src$year, tgt$year,
                          currency_route, basis, .currency_generated_at())
      .currency_trace_end()
    }, error = function(e) { .currency_trace_end(); NULL })
    fac[[d]] <- f
    note[[d]] <- .currency_note(src$code, tgt$code, src$year, tgt$year, basis,
                                currency_route)
    if (!is.null(tr$fx)) fx[[length(fx) + 1L]] <- tr$fx
    if (!is.null(tr$deflator)) dfl[[length(dfl) + 1L]] <- tr$deflator
  }

  # Nothing to rewrite: every object is already in the target, which is the
  # common case and must cost nothing.
  moved <- decl[!vapply(decl, function(d) isTRUE(all.equal(fac[[d]], 1)),
                        logical(1))]
  if (!length(moved) && identical(tgt_s, cur)) return(NULL)

  repl <- list()
  for (i in which(keep)) {
    if (!(from[i] %in% moved)) next
    o <- objs[[i]]
    repl[[paste(class(o)[1], o@name)]] <-
      .currency_scale_object(o, fac[[from[i]]], tgt_s)
  }
  if (length(repl)) mod <- .currency_put_objects(mod, repl)
  if (!identical(tgt_s, cur)) mod <- .currency_declare_model(mod, tgt_s)

  i <- which(keep)
  df <- data.frame(
    object = names(objs)[i],
    class = vapply(objs[i], function(o) class(o)[1], character(1)),
    from = unname(from[i]),
    declared = !is.na(unname(own[i])),
    factor = unname(fac[from[i]]),
    note = unname(note[from[i]]),
    converted = unname(from[i]) %in% moved,
    stringsAsFactors = FALSE)
  rep <- list(target = tgt_s, basis = basis, route = currency_route,
              base_year = currency_base_year(), objects = df,
              fx = .currency_bind(fx), deflator = .currency_bind(dfl))
  if (isTRUE(verbose)) .currency_report_print(rep)
  list(model = mod, report = rep)
}

# Re-apply a recorded normalisation to a model rebuilt from `scen@model`.
#
# `scen@model` is the model as AUTHORED, while the scenario's tables hold the
# converted numbers, so a copy rebuilt from it has to be converted again or it
# disagrees with the scenario it came from -- `levcost()` reads such a copy.
# The same reason `scen@misc$spatial` exists: what interpolation did to the
# build copy has to be reproducible from the scenario.
#
# The base year recorded in the report is restored while it runs, because an
# untagged declaration is read at whatever `currency_base_year()` says now,
# which need not be what it said then.
#' @noRd
.currency_reapply <- function(mod, cu) {
  if (is.null(cu) || is.null(cu$target)) return(mod)
  now <- currency_base_year()
  then <- as.integer(cu$base_year %||% now)
  if (!identical(now, then)) {
    currency_base_year(then)
    on.exit(currency_base_year(now), add = TRUE)
  }
  r <- suppressWarnings(.normalize_currency(
    mod, target = cu$target, basis = cu$basis %||% "gdp",
    currency_route = cu$route %||% "deflate_then_fx", verbose = FALSE))
  if (is.null(r)) mod else r$model
}

#' @noRd
.currency_bind <- function(x) {
  x <- x[!vapply(x, is.null, logical(1))]
  if (!length(x)) return(NULL)
  out <- unique(do.call(rbind, x))
  row.names(out) <- NULL
  out
}

# The report. Grouped by declaration, because that is what a factor belongs
# to, and the object names are what a reader recognises. Objects that moved
# first; the ones left alone are a count, since there is nothing to check.
#' @noRd
.currency_report_print <- function(rep) {
  d <- rep$objects
  lines <- character(0)
  grp <- unique(d$from[d$converted])
  # one name list per group, then one column width for all of them: a report
  # of two short names should not be indented as if it carried twenty
  shown <- vapply(grp, function(d1) {
    nm <- d$object[d$from == d1 & d$converted]
    if (length(nm) > 4L)
      paste0(paste(nm[1:4], collapse = ", "), " +", length(nm) - 4L, " more")
    else paste(nm, collapse = ", ")
  }, character(1))
  w <- max(c(8L, nchar(shown)))
  for (i in seq_along(grp)) {
    k <- which(d$from == grp[i] & d$converted)[1]
    lines <- c(lines, sprintf("    %-*s  %-10s × %-9.5g (%s)",
                              w, shown[i], grp[i], d$factor[k], d$note[k]))
  }
  n <- sum(!d$converted)
  if (n > 0L) {
    u <- sum(!d$converted & !d$declared)
    lines <- c(lines, sprintf(
      "    %d object(s) already in %s%s", n, rep$target,
      if (u > 0L) sprintf(" (%d declaring no currency of their own)", u)
      else ""))
  }
  message(paste(c(paste0("Currency normalisation -> ", rep$target), lines),
                collapse = "\n"))
  invisible(NULL)
}
