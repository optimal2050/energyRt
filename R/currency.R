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
## Two declarations close that. `config@currency` states what the model counts
## in -- one per model, the unit of the objective. `@units$costs` on an object
## states what IT was priced in, and travels with it. Where both are declared
## and disagree, that is an error: there is no exchange rate in the model, so
## there is nothing to convert with.
##
## Both are optional and unset claims nothing, so a model written before the
## slots existed behaves exactly as it did.

#' @include class-config.R class-supply.R class-storage.R class-trade.R
NULL

# The model's currency, or NULL when it declares none. Guarded because configs
# serialized before the slot existed have no `currency` attribute at all and
# `@currency` on them errors -- same device as `.upgrade_storage_inp2stg()`.
#' @noRd
.model_currency <- function(x) {
  cfg <- if (methods::is(x, "config")) x else
    tryCatch(x@config, error = function(e) NULL)
  if (is.null(cfg)) return(NULL)
  if (!("currency" %in% names(attributes(cfg)))) return(NULL)
  v <- as.character(methods::slot(cfg, "currency"))
  v <- v[!is.na(v) & nzchar(v)]
  if (length(v)) v[1] else NULL
}

# One object's declared units, or NULL. Classes that predate the slot, and
# classes that never had one (tax, constraint, ...), return NULL rather than
# erroring, so callers can ask any object.
#' @noRd
.object_units <- function(obj, what = "costs") {
  if (!isS4(obj) || !("units" %in% names(attributes(obj)))) return(NULL)
  u <- methods::slot(obj, "units")
  if (!is.data.frame(u) || !nrow(u) || !(what %in% names(u))) return(NULL)
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
  own <- vapply(objs, function(o) .object_units(o, "costs") %||% NA_character_,
                character(1))
  bad <- !is.na(own) & own != cur
  if (!any(bad)) return(invisible(NULL))
  nm <- names(objs)[bad]
  stop("the model counts in '", cur, "', but ", sum(bad), " object(s) are ",
       "priced in another currency:\n  ",
       paste(sprintf("%s: %s", nm, own[bad]), collapse = "\n  "),
       "\nThere is no exchange rate in a model, so this is not converted. ",
       "Restate the costs, or clear `@units$costs` to claim nothing.",
       call. = FALSE)
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
  if (!length(absent)) return(x)
  proto <- tryCatch(methods::new(cls), error = function(e) NULL)
  if (is.null(proto)) return(x)
  for (sl in absent) attr(x, sl) <- methods::slot(proto, sl)
  x
}
