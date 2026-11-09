## Per-object overrides of a parameter's default and interpolation rule.
##
## Three layers decide what the engine uses, in this order: the catalogue
## (`data-raw/parameters.yml`, baked into `param@defVal` / `@interpolation`),
## the model (`config`/`settings`, applied by
## `.apply_settings_param_overrides()`), and an individual object's own
## `@defVal` / `@interpolation`, applied here. Last writer wins.
##
## The two levels are keyed differently, because the information available
## differs. On an OBJECT the class is known, so the short COLUMN name is the
## key -- `wval`, `af.up` -- which is the name the user already typed in the
## slot. In `config` a bare column would be ambiguous: 46 of 190 column names
## are shared across classes, `af.up` alone meaning `pTechAf`, `pStorageAf` or
## `pTradeIrAf`, so there the key is the parameter name.
##
## An override equal to what the parameter already carries records NOTHING, so
## the common case leaves `interpolate_parameters()` on its single whole-table
## path and the interpolation goldens unmoved.

#' @include class-parameter.R
NULL

# Every accepted key for one class, as a lookup from the written spelling to
# the canonical column key(s). Built from `.col_param`, memoised per class.
#
# Canonical is `column`, or `slot.column` where a bare column is ambiguous
# within the class (measured: one case, `storage` `cap2act`, which is both
# `input$cap2act` -> `pStorageInpCap2act` and `output$cap2act` ->
# `pStorageOutCap2act`). Accepted aliases are the parameter name with the
# `.lo`/`.up` suffix, and the bare parameter name, which expands to every
# column that parameter owns -- the only way to write "both bounds", since
# there is no bare `af` column.
.override_key_cache <- new.env(parent = emptyenv())

#' @noRd
.override_keys_for <- function(cls) {
  if (length(cls) != 1L || is.na(cls)) return(NULL)
  hit <- .override_key_cache[[cls]]
  if (!is.null(hit)) return(hit)
  cp <- tryCatch(.col_param, error = function(e) NULL)
  if (!is.data.frame(cp)) return(NULL)
  j <- which(cp$class == cls & !is.na(cp$parameter) & !is.na(cp$column))
  if (!length(j)) {
    out <- list(canon = character(0), alias = list())
    .override_key_cache[[cls]] <- out
    return(out)
  }
  d <- cp[j, , drop = FALSE]

  # canonical: the bare column, unless that column name occurs in more than
  # one slot of this class
  n_by_col <- table(d$column)
  canon <- ifelse(n_by_col[d$column] > 1L,
                  paste(d$slot, d$column, sep = "."), d$column)

  alias <- stats::setNames(as.list(canon), canon)
  # the bare column, where it is unambiguous, already IS canonical; where it is
  # not, it is recorded as an error with the candidates named
  amb <- unique(d$column[n_by_col[d$column] > 1L])
  for (a in amb) {
    alias[[a]] <- structure(canon[d$column == a], ambiguous = TRUE)
  }
  # the parameter spellings
  for (i in seq_len(nrow(d))) {
    p <- d$parameter[i]
    b <- d$bound[i]
    if (!is.na(b) && nzchar(b)) {
      k <- paste0(p, ".", b)
      if (is.null(alias[[k]])) alias[[k]] <- canon[i]
    }
  }
  for (p in unique(d$parameter)) {
    # the bare parameter name covers every column it owns
    if (is.null(alias[[p]])) alias[[p]] <- unname(canon[d$parameter == p])
  }
  out <- list(canon = unname(canon), alias = alias,
              param = stats::setNames(d$parameter, canon),
              slot = stats::setNames(d$slot, canon),
              column = stats::setNames(d$column, canon))
  .override_key_cache[[cls]] <- out
  out
}

# Resolve one written key to its canonical column key(s) for `cls`. Errors --
# never guesses -- on an unknown or ambiguous key, naming what would work.
#' @noRd
.resolve_override_key <- function(key, cls, what = "interpolation") {
  k <- .override_keys_for(cls)
  if (is.null(k) || !length(k$canon)) {
    stop("`", cls, "@", what, "` cannot be set: the class owns no ",
         "catalogued parameter column.", call. = FALSE)
  }
  hit <- k$alias[[key]]
  if (is.null(hit)) {
    stop("unknown ", what, " override key \"", key, "\" for class \"", cls,
         "\".\nAccepted keys are this class's columns: ",
         paste(sort(k$canon), collapse = ", "),
         ".\nA parameter name also works (",
         paste(utils::head(sort(unique(k$param)), 3), collapse = ", "),
         ", ...).", call. = FALSE)
  }
  if (isTRUE(attr(hit, "ambiguous"))) {
    stop("ambiguous override key \"", key, "\" for class \"", cls,
         "\": it names ", length(hit), " columns (",
         paste(hit, collapse = ", "), ").\nWrite one of those, or the ",
         "parameter name.", call. = FALSE)
  }
  as.character(hit)
}

# The whole override table of one object, canonicalised: a data.frame of
# `key`, `defVal`, `interpolation`, one row per canonical column key. Empty
# when the object declares nothing, which is the overwhelmingly common case.
#' @noRd
.object_overrides <- function(obj) {
  cls <- class(obj)[1]
  empty <- data.frame(key = character(0), defVal = numeric(0),
                      interpolation = character(0), stringsAsFactors = FALSE)
  if (!isS4(obj)) return(empty)
  out <- empty
  for (what in c("defVal", "interpolation")) {
    tab <- tryCatch(methods::slot(obj, what), error = function(e) NULL)
    if (!is.data.frame(tab) || !ncol(tab)) next
    for (nm in names(tab)) {
      v <- tab[[nm]][1]
      if (length(v) != 1 || is.na(v)) next
      if (identical(what, "interpolation")) {
        if (!nzchar(as.character(v))) next
        # errors naming the accepted set on an unknown token; stored as
        # written, canonical only where the engine reads it, exactly as
        # `config@interpolation` behaves
        v <- .normalise_interp_rule(as.character(v))
      } else {
        v <- as.numeric(v)
        if (!is.finite(v)) {
          stop("`", cls, "@defVal$", nm, "` must be one finite number.",
               call. = FALSE)
        }
      }
      for (k in .resolve_override_key(nm, cls, what = what)) {
        i <- match(k, out$key)
        if (is.na(i)) {
          out <- rbind(out, data.frame(key = k, defVal = NA_real_,
                                       interpolation = NA_character_,
                                       stringsAsFactors = FALSE))
          i <- nrow(out)
        }
        out[[what]][i] <- v
      }
    }
  }
  out
}

# Object validity: every key resolves, every value is well formed. Called from
# each class's `validObject()`, so a typo is an ERROR where in `@misc` it would
# have been a silent no-op.
#' @noRd
# `weather@defVal`, `tax@defVal` and `subsidy@defVal` were a NUMBER until 0.92
# and are the per-object override table since. An object serialized earlier
# still holds a number, which the class no longer accepts, so it is converted
# here -- to the column(s) the class owns, and only when it differs from the
# catalogue's own default, exactly as `.fold_defval_arg()` does for the
# argument.
#' @noRd
.upgrade_override_slots <- function(x) {
  if (!isS4(x)) return(x)
  cls <- class(x)[1]
  if (!(cls %in% names(.override_dim))) return(x)
  v <- tryCatch(attr(x, "defVal", exact = TRUE), error = function(e) NULL)
  if (is.null(v) || is.data.frame(v) || !is.numeric(v) || length(v) != 1) {
    return(x)
  }
  k <- .override_keys_for(cls)
  if (is.null(k) || !length(k$canon)) return(x)
  mi <- tryCatch(.modInp, error = function(e) NULL)
  tab <- list()
  if (!is.na(v)) {
    for (cc in k$canon) {
      dv <- if (!is.null(mi)) mi[[k$param[[cc]]]]$defVal else NULL
      if (!is.null(dv) && length(dv) >= 1 &&
            isTRUE(all.equal(as.numeric(dv[1]), as.numeric(v)))) next
      tab[[cc]] <- as.numeric(v)
    }
  }
  attr(x, "defVal") <- if (length(tab))
    as.data.frame(tab, stringsAsFactors = FALSE) else
    data.frame(row.names = 1L)
  x
}

.check_param_overrides <- function(obj) {
  invisible(.object_overrides(obj))
}

# The dim column that names an object of each class, i.e. the column of a
# parameter's data that an override filters on. Same map `.check_process_names()`
# uses, plus `weather` and the shared `levy` set of `tax`/`subsidy`.
#' @noRd
.override_dim <- c(technology = "tech", storage = "stg", trade = "trade",
                   supply = "sup", demand = "dem", import = "imp",
                   export = "expp", weather = "weather", tax = "levy",
                   subsidy = "levy")

# Walk the model's objects once and record, on each affected parameter, which
# of its members override its rule or default. Runs beside
# `.apply_settings_param_overrides()`, so the layers apply in order: catalogue,
# then `config`, then this.
#
# A value equal to what the parameter already carries records NOTHING, which is
# what keeps `interpolate_parameters()` on its single whole-table path for
# every model that sets no override.
#' @noRd
.collect_param_overrides <- function(scen) {
  objs <- tryCatch(getObject(scen), error = function(e) NULL)
  if (!length(objs)) return(scen)
  pars <- scen@modInp@parameters
  acc <- list()

  for (o in objs) {
    cls <- class(o)[1]
    # a commodity, a calendar, a constraint: no override slots, so skip
    if (!(cls %in% names(.override_dim))) next
    dim <- .override_dim[[cls]]
    ov <- .object_overrides(o)
    if (!nrow(ov)) next
    k <- .override_keys_for(cls)
    for (i in seq_len(nrow(ov))) {
      pn <- k$param[[ov$key[i]]]
      p <- pars[[pn]]
      if (is.null(p)) next
      col <- k$column[[ov$key[i]]]
      bnd <- .override_bound(p, col)
      rule <- ov$interpolation[i]
      dv <- ov$defVal[i]
      # the catalogue (and `config` before us) already say this
      if (!is.na(rule) &&
            identical(rule, .interp_rule_at(p@interpolation, bnd))) {
        rule <- NA_character_
      }
      if (!is.na(dv) && isTRUE(all.equal(dv, .defval_at(p@defVal, bnd)))) {
        dv <- NA_real_
      }
      if (is.na(rule) && is.na(dv)) next
      acc[[pn]] <- rbind(
        acc[[pn]],
        data.frame(dim = dim, member = o@name, bound = bnd %||% NA_character_,
                   rule = rule, defVal = dv, stringsAsFactors = FALSE))
    }
  }
  if (!length(acc)) return(scen)
  for (pn in names(acc)) {
    scen@modInp@parameters[[pn]]@misc$override <- unique(acc[[pn]])
  }
  scen
}

# `lo`/`up`/`fx` from a column name, or NULL for a `numpar` column.
#' @noRd
.override_bound <- function(p, col) {
  if (!identical(as.character(p@type), "bounds")) return(NULL)
  m <- regmatches(col, regexpr("[.](lo|up|fx)$", col))
  if (!length(m)) return(NULL)
  sub("^[.]", "", m)
}

#' @noRd
.interp_rule_at <- function(rule, bound) {
  if (!length(rule)) return(NA_character_)
  i <- if (is.null(bound) || identical(bound, "lo")) 1L else
    min(2L, length(rule))
  as.character(rule[i])
}

#' @noRd
.defval_at <- function(dv, bound) {
  if (!length(dv)) return(NA_real_)
  i <- if (is.null(bound) || identical(bound, "lo")) 1L else min(2L, length(dv))
  as.numeric(dv[i])
}
