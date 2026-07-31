#' A virtual S4 class for the model's data-carrying objects
#'
#' @description
#' `modelData` is the common parent of [class-parameter] (the model's input
#' data) and [class-variable] (its solved output). It declares no slots and
#' cannot be instantiated: it exists so the storage contract those two share --
#' a `data` slot that may be materialised in memory or read lazily from an
#' Arrow dataset, plus a `misc` list holding `path` / `inMemory` / `onDisk` --
#' is implemented once rather than duplicated per class.
#'
#' Methods dispatching on it: `print`, `.reset_data`. The rest of the contract
#' (`get_data_slot`, `isOnDisk`, `getObjPath`, `setObjPath`, `mark_ondisk`,
#' `mark_inMemory`) is already written generically over "any S4 object with a
#' `misc` list" and needs no dispatch at all.
#'
#' Deliberately slot-free. The two classes' similar-looking slots do not mean
#' the same thing -- `parameter@dimSets` is validated against `.dimSets` and
#' drives its `data` skeleton positionally, while `variable@dimSets` carries
#' duplicate dimensions that a positional skeleton cannot express -- so hoisting
#' them would buy nothing and prevent the two from diverging. It also keeps the
#' change backward compatible: an S4 object serialises as an attribute list
#' keyed by slot NAME, so a parent that adds no slot names loads cleanly from a
#' scenario saved before this class existed, which `load_scenario()` relies on
#' (it uses a plain `load()`, with no `updateObject()` step).
#'
#' @name class-modelData
#' @keywords internal
#' @export
setClass("modelData", representation("VIRTUAL"))

# Empty an object's data slot, keeping its schema.
.reset_data <- function(x) {
  x@data <- x@data[0, , drop = FALSE]
  x
}
