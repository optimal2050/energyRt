#' Object construction and validity in energyRt
#'
#' @description
#' The package states one rule for how its S4 objects come into being and what
#' they guarantee once they exist. The rule is the contract the rest of the
#' package is written against, and it has three parts.
#'
#' @section Constructors are the supported path:
#' Every class has a public constructor -- [newCommodity()], [newTechnology()],
#' [newStorage()], [newModel()] and so on. The constructors are the only route
#' through the construction checks: column names are validated against the
#' class, types are coerced, renamed columns are diagnosed, and conflicting
#' rows are detected. An object returned by a constructor is a complete,
#' documented object of its class, and `methods::validObject()` accepts it.
#'
#' @section Direct `new()` refuses slot values:
#' `new("<class>", slot = value)` raises an error naming the constructor to use
#' instead. The passthrough `initialize()` methods these classes define would
#' otherwise discard the value in silence -- a technology asked for by name
#' came back named `""` -- and assigning the slot directly would skip every
#' check listed above. `new("<class>")` with no arguments remains supported and
#' returns the class prototype.
#'
#' Two generated classes are deliberate exceptions and still assign named
#' arguments to slots, because `newParameter()` and the internal model-data
#' machinery are built on that behaviour: `parameter` and `variable`.
#'
#' @section Prototypes are drafts, not public objects:
#' A prototype obtained from a bare `new("<class>")` is an internal starting
#' point. It is not required to satisfy `validObject()`, and one does not:
#' a prototype `scenario` carries empty `model`, `settings`, `modInp` and
#' `modOut` slots, because building valid empties for them costs time that
#' would be paid on every package load for an object no user asks for.
#' [newScenario()] returns a valid scenario; `new("scenario")` is a draft that
#' the interpolation and solve pipelines fill in.
#'
#' Everything a user is handed -- by a constructor, by [interpolate()], by a
#' solve -- is a valid object of its class. Validity of a bare prototype is not
#' promised, and code that needs the guarantee should construct through the
#' constructor rather than validate a prototype.
#'
#' @seealso [newModel()], [newScenario()], [newTechnology()]
#' @family class
#' @name energyRt-validity
#' @rdname energyRt-validity
NULL
