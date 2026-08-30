#' An S4 class to represent a commodity
#' 
#' @name class-commodity
#'
#' @description
#' A commodity is a good or service that is produced and consumed in the model.
#' The commodity class is used to store information about the commodity.
#' All processes in the model operate on commodities, i.e. they either generate,
#' produce, consume, transform, store, or transport commodities.
#' The creation of a commodity object is done with the `newCommodity` function.
#'
#' @md
#' @slot name `r get_slot_doc("commodity", "name")`
#' @slot desc `r get_slot_doc("commodity", "desc")`
#' @slot limtype `r get_slot_doc("commodity", "limtype")`
#' @slot timeframe `r get_slot_doc("commodity", "timeframe")`
#' @slot geoframe `r get_slot_doc("commodity", "geoframe")`
#' @slot unit `r get_slot_doc("commodity", "unit")`
#' @slot emis `r get_slot_doc("commodity", "emis")`
#' @slot agg `r get_slot_doc("commodity", "agg")`
#' @slot property `r get_slot_doc("commodity", "property")`
#' @slot misc `r get_slot_doc("commodity", "misc")`
#'
#' @rdname class-commodity
#' @family class, commodity
#' @include class-calendar.R
#'
setClass("commodity",
  representation(
    name = "character", # Short name
    desc = "character", # Details
    limtype = "factor",
    timeframe = "character",
    geoframe = "character",
    unit = "character",
    emis = "data.frame", # Emission factors
    agg = "data.frame", # Aggregation parameter
    property = "data.frame", # Physical properties
    misc = "list"
  ),
  prototype(
    name = character(),
    desc = character(),
    limtype = factor("LO", levels = c("FX", "UP", "LO")),
    timeframe = character(),
    geoframe = character(),
    unit = character(),
    agg = data.frame(
      comm = character(),
      unit = character(),
      agg = numeric(),
      stringsAsFactors = FALSE
    ),
    emis = data.frame(
      comm = character(),
      unit = character(),
      emis = numeric(),
      stringsAsFactors = FALSE
    ),
    property = data.frame(
      property = character(),
      value = numeric(),
      min = numeric(),
      max = numeric(),
      sd = numeric(),
      dist = character(),
      unit = character(),
      comment = character(),
      stringsAsFactors = FALSE
    ),
    misc = list()
  ),
  S3methods = FALSE
)

setMethod("initialize", "commodity", function(.Object, ...) {
  .Object
})

#' Create new commodity object
#'
#' @md
#' @param name `r get_slot_doc("commodity", "name")`
#' @param desc `r get_slot_doc("commodity", "desc")`
#' @param limtype `r get_slot_doc("commodity", "limtype")`
#' @param timeframe `r get_slot_doc("commodity", "timeframe")`
#' @param geoframe `r get_slot_doc("commodity", "geoframe")`
#' @param unit `r get_slot_doc("commodity", "unit")`
#' @param agg `r get_slot_doc("commodity", "agg")`
#' @param emis `r get_slot_doc("commodity", "emis")`
#' @param property `r get_slot_doc("commodity", "property")`
#' @param misc `r get_slot_doc("commodity", "misc")`
#' @param image character, optional path or URL to an illustration of the
#'   commodity, used in reports. Stored in `misc$image`.
#' @param icon character, optional path or URL to a small icon for the
#'   commodity. Stored in `misc$icon`.
#'
#' @return commodity object
#' @export
#'
#' @rdname newCommodity
#'
#' @family commodity
#'
#' @examples
#' newCommodity(name = "ELC", desc = "Electricity", unit = "GWh")
#'
#' # physical properties give `convert()` the physics to move between measures
#' newCommodity(
#'   name = "COA", desc = "Steam coal", unit = "PJ",
#'   property = data.frame(
#'     property = c("lhv", "density"),
#'     value = c(25.8, 0.85),
#'     unit = c("GJ/t", "t/m3"),
#'     comment = c("IEA average, other bituminous", "bulk, as stockpiled")
#'   ),
#'   emis = data.frame(comm = "CO2", unit = "Mt", emis = 0.0946)
#' )
newCommodity <- function(
    name = "",
    desc = "",
    limtype = "LO",
    timeframe = character(),
    geoframe = character(),
    unit = character(),
    agg = data.frame(),
    emis = data.frame(),
    property = data.frame(),
    misc = list(),
    image = NULL,
    icon = NULL) {
  # `image`/`icon` are conventions on `misc`, not slots of their own, so they are
  # folded in here -- `.data2slots()` would reject them as unknown slot names.
  misc <- .add_misc_image(misc, image = image, icon = icon)
  obj <- .data2slots(
    "commodity",
    name,
    desc = desc,
    limtype = limtype,
    timeframe = timeframe,
    geoframe = geoframe,
    unit = unit,
    agg = agg,
    emis = emis,
    property = property,
    misc = misc
  )
  # after `.data2slots()`, so that a malformed table fails on its column names
  # first and the checks below see a normalised, fully-columned table
  .check_commodity_property(obj@property, name = obj@name)
  obj
}


#' @method update commodity
#' @family commodity update
#' @export
setMethod("update", signature(object = "commodity"), function(object, ...) {
  # update.supply <- function(obj, ...) {
  arg <- list(...)
  # `image`/`icon` are `misc` conventions, not slots -- fold them in, merging
  # with whatever `misc` the object already carries unless a new one is given.
  if (any(c("image", "icon") %in% names(arg))) {
    base_misc <- if ("misc" %in% names(arg)) arg$misc else object@misc
    arg$misc <- .add_misc_image(base_misc,
                                image = arg$image, icon = arg$icon)
    arg$image <- NULL
    arg$icon <- NULL
  }
  obj <- do.call(.data2slots, c(list("commodity", object), arg))
  if ("property" %in% names(arg)) {
    .check_commodity_property(obj@property, name = obj@name)
  }
  obj
})
