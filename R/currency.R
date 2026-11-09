## The model's currency, and the check that objects agree with it.
##
## The unit SYSTEM lives in `units.R` (`getUnits()`, the per-parameter
## formulas, the `{costs}`/`{capacity}`/`{activity}` tokens). This file adds
## only the model-level declaration those tokens can now be resolved against,
## and the check that an object priced elsewhere is not silently absorbed.
##
## A cost in energyRt is a bare number: `supply@supply$cost`,
## `technology@invcost$invcost`, `trade@varom$varom`. Nothing in the arithmetic
## says what currency it is in, so two models that count in different money
## produce objectives that look alike and are not comparable, and an object
## moved from one to the other is wrong in a way no output reveals.
##
## Two declarations close that. `config@units$currency` states what the model
## counts in -- one per model, the unit of the objective. `@units$currency` on an object
## states what IT was priced in, and travels with it. Where both are declared
## and disagree, that is an error: there is no exchange rate in the model, so
## there is nothing to convert with.
##
## Both are optional and unset claims nothing, so a model written before the
## slots existed behaves exactly as it did.

#' @include class-config.R class-supply.R class-storage.R class-trade.R
NULL

# The model's currency, or NULL when it declares none. It lives in
# `config@units$currency` since 0.92, the same column name an object uses, so
# `.check_model_currency()` compares like with like. A config serialized
# earlier carries the removed `currency` slot as a plain attribute, which is
# read as a fallback -- see `.upgrade_unit_slot()`.
#' @noRd
.model_currency <- function(x) {
  cfg <- if (methods::is(x, "config")) x else
    tryCatch(x@config, error = function(e) NULL)
  if (is.null(cfg)) return(NULL)
  v <- .object_units(cfg, "currency")
  if (!is.null(v)) return(v)
  v <- as.character(attr(cfg, "currency", exact = TRUE))
  v <- v[!is.na(v) & nzchar(v)]
  if (length(v)) v[1] else NULL
}

# One object's declared units, or NULL. Classes that predate the slot, and
# classes that never had one (tax, constraint, ...), return NULL rather than
# erroring, so callers can ask any object.
#' @noRd
.object_units <- function(obj, what = "currency") {
  if (!isS4(obj) || !("units" %in% names(attributes(obj)))) return(NULL)
  u <- methods::slot(obj, "units")
  if (!is.data.frame(u) || !nrow(u)) return(NULL)
  # `costs` is the pre-0.92 spelling, accepted on read (see units.R)
  if (!(what %in% names(u))) {
    if (!identical(what, "currency") || !("costs" %in% names(u))) return(NULL)
    what <- "costs"
  }
  v <- as.character(u[[what]])
  v <- v[!is.na(v) & nzchar(v)]
  if (length(v)) v[1] else NULL
}

# Every object that names a currency must name the model's. Silence when
# either side declares nothing: the check is opt-in, and a model that has
# never heard of currencies is not suddenly wrong.
#' @noRd
.check_model_currency <- function(mod) {
  cur <- .model_currency(mod)
  if (is.null(cur)) return(invisible(NULL))
  objs <- tryCatch(getObject(mod), error = function(e) NULL)
  if (!length(objs)) return(invisible(NULL))
  own <- vapply(objs, function(o) .object_units(o, "currency") %||% NA_character_,
                character(1))
  bad <- !is.na(own) & own != cur
  if (!any(bad)) return(invisible(NULL))
  nm <- names(objs)[bad]
  stop("the model counts in '", cur, "', but ", sum(bad), " object(s) are ",
       "priced in another currency:\n  ",
       paste(sprintf("%s: %s", nm, own[bad]), collapse = "\n  "),
       "\nThere is no exchange rate in a model, so this is not converted. ",
       "Restate the costs, or clear `@units$currency` to claim nothing.",
       call. = FALSE)
}

## The weather side of the same problem. A weather factor is DIMENSIONLESS by
## default and that is the common case; a series carrying a real measure
## declares it in `weather@units$wval`, and a process says what it expects in
## the `unit` column of its own `@weather` row -- per row, because one process
## may read a temperature from one series and a wind speed from another. Where
## both are declared and differ, that is an error: nothing in the model
## converts degC to degK, so a coefficient fitted to one and applied to the
## other is wrong in a way no output reveals. Where either side says nothing,
## the check is silent.

# Every process that states what it expects from a weather series must state
# what the series declares. Both sides are optional; silence when either is
# empty.
#' @noRd
.check_weather_units <- function(mod) {
  objs <- tryCatch(getObject(mod), error = function(e) NULL)
  if (!length(objs)) return(invisible(NULL))
  declared <- character(0)
  for (o in objs) {
    if (!methods::is(o, "weather")) next
    v <- .object_units(o, "wval")
    if (!is.null(v)) declared[o@name] <- v
  }
  if (!length(declared)) return(invisible(NULL))
  bad <- list()
  for (o in objs) {
    w <- tryCatch(methods::slot(o, "weather"), error = function(e) NULL)
    if (!is.data.frame(w) || !nrow(w) ||
          !all(c("weather", "unit") %in% names(w))) next
    for (i in seq_len(nrow(w))) {
      want <- w$unit[i]
      nm <- w$weather[i]
      if (is.na(want) || !nzchar(want) || is.na(nm) || !nzchar(nm)) next
      if (!(nm %in% names(declared))) next
      if (identical(as.character(want), declared[[nm]])) next
      bad[[length(bad) + 1L]] <- sprintf(
        "%s %s expects %s from weather '%s', which declares %s",
        class(o)[1], o@name, want, nm, declared[[nm]])
    }
  }
  if (!length(bad)) return(invisible(NULL))
  stop(length(bad), " weather unit mismatch(es):\n  ",
       paste(unique(unlist(bad)), collapse = "\n  "),
       "\nThere is no conversion between weather units in a model, so this is ",
       "not reconciled. Restate one side, or clear the `unit` column to claim ",
       "nothing.", call. = FALSE)
}

# An object serialized before a slot existed has no attribute for it, and
# `validObject()` rejects it ("slots in class definition but not in object").
# Every saved model predates `@units`, so rather than one upgrade per slot --
# the device `.upgrade_storage_inp2stg()` and `.upgrade_summand()` each needed
# -- fill ANY slot the class defines and the instance lacks, from the
# prototype. Only absent slots are touched: a slot that is present, even
# empty, is the object's own.
#' @noRd
.upgrade_missing_slots <- function(x) {
  if (!isS4(x)) return(x)
  cls <- class(x)[1]
  if (methods::isVirtualClass(cls)) return(x)
  defined <- tryCatch(names(methods::getSlots(cls)), error = function(e) NULL)
  if (!length(defined)) return(x)
  absent <- setdiff(defined, names(attributes(x)))
  if (!length(absent)) return(.upgrade_unit_slot(.upgrade_override_slots(x)))
  proto <- tryCatch(methods::new(cls), error = function(e) NULL)
  if (is.null(proto)) return(x)
  for (sl in absent) attr(x, sl) <- methods::slot(proto, sl)
  .upgrade_unit_slot(.upgrade_override_slots(x))
}
# The scalar `@unit` slot was removed in 0.92: a commodity's or a flow's own
# measure is `@units$comm`, a weather series' is `@units$wval`. A slot removed
# from a class definition is NOT dropped from an object serialized with it --
# it survives as a plain attribute, which `validObject()` tolerates -- so the
# value is migrated here rather than lost. The orphan attribute is left in
# place; nothing reads it once `@units` carries the measure.
#' @noRd
.upgrade_unit_slot <- function(x) {
  if (!isS4(x)) return(x)
  cls <- class(x)[1]
  defined <- tryCatch(names(methods::getSlots(cls)), error = function(e) NULL)
  if (!("units" %in% defined)) return(x)
  folds <- .unit_arg_folds(cls)
  nm <- names(folds)[!(names(folds) %in% defined)]
  if (!length(nm)) return(x)
  nm <- nm[1]
  v <- attr(x, nm, exact = TRUE)
  if (length(v) != 1 || is.na(v) || !nzchar(as.character(v))) return(x)
  cc <- folds[[nm]]
  u <- methods::slot(x, "units")
  if (!is.data.frame(u) || !(cc %in% names(u))) return(x)
  if (nrow(u) == 0) u[1, ] <- NA
  if (is.na(u[[cc]][1]) || !nzchar(as.character(u[[cc]][1]))) {
    u[[cc]][1] <- as.character(v)
    methods::slot(x, "units") <- u
  }
  x
}

