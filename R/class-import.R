#' An S4 class to represent commodity import from the rest of the world.
#' 
#' Use `newImport` to create a new `import` object.
#'
#' @name class-import 
#' 
#' @inherit newImport description
#' @inherit newImport details
#'
#' @md
#' @slot name `r get_slot_doc("import", "name")`
#' @slot desc `r get_slot_doc("import", "desc")`
#' @slot commodity `r get_slot_doc("import", "commodity")`
#' @slot unit `r get_slot_doc("import", "unit")`
#' @slot reserve `r get_slot_doc("import", "reserve")`
#' @slot imp `r get_slot_doc("import", "imp")`
#' @slot misc `r get_slot_doc("import", "misc")`
#'
#' @include class-trade.R
#' @family class import
#' @rdname class-import
#'
#' @export
#'
setClass("import",
  representation(
    name = "character",
    desc = "character",
    commodity = "character",
    unit = "character",
    reserve = "numeric",
    # !!! add region to export
    # !!! make reserve a data.frame with region, year, upper, lower, fixed
    imp = "data.frame",
    # timeframe = "character", # set to commodity@timeframe
    misc = "list"
  ),
  prototype(
    name = "",
    desc = "",
    commodity = "",
    unit = "",
    reserve = Inf,
    imp = data.frame(
      region = character(),
      year = integer(),
      slice = character(),
      imp.lo = numeric(),
      imp.up = numeric(),
      imp.fx = numeric(),
      price = numeric(),
      stringsAsFactors = FALSE
    ),
    # timeframe = character(),
    misc = list()
  ),
  S3methods = FALSE
)
setMethod("initialize", "import", function(.Object, ...) {
  .Object
})

#' Create new export object
#'
#' Constructor for import object.
#'
#' @name newImport
#'
#' @description
#' Import object to represent commodity import from the Rest of the World (RoW).
#'
#' @details
#' Import object adds an "external" source of commodity to the model.
#' The RoW is not modeled explicitly as a region, `export` and `import` objects
#' define and control the exchange with the RoW. The operation is similar to
#' the `demand` object, but the two ideas distinguishes between internal
#' and external final consumption.
#' This exchange can be exogenously defined (`imp.fx`) or optimized by the model
#' within the given limits (`imp.lo`, `imp.up`). The `price` column is used to
#' define the price of the imported commodity.
#' "Reserve" sets the total amount that can be imported over the model horizon.
#'
#' @param name `r get_slot_doc("import", "name")`
#' @param desc `r get_slot_doc("import", "desc")`
#' @param commodity `r get_slot_doc("import", "commodity")`
#' @param unit `r get_slot_doc("import", "unit")`
#' @param reserve `r get_slot_doc("import", "reserve")`
#' @param imp `r get_slot_doc("import", "imp")`
#' @param misc `r get_slot_doc("import", "misc")`
#'
#' @return import object with given specifications.
#' @rdname newImport
#' @export
#' @examples
#' IMPOIL <- newImport(
#'   name = "IMPOIL", # used in sets
#'   desc = "Oil import to the model to the RoW", # for own reference
#'   commodity = "OIL", # must match the commodity name in the model
#'   unit = "Mtoe", # for own reference
#'   imp = data.frame(
#'     region = rep(c("R1", "R2"), each = 2), # import region(s)
#'     year = rep(c(2020, 2050)), # import years
#'     price = 600, # import price in MUSD/Mtoe (USD/t),
#'     imp.up = rep(c(1e4, 1e6), each = 2), # upper bound for import in each year
#'     imp.lo = rep(c(1e4, 1e5), each = 2) # lower bound for import in each year
#'   )
#' )
#' draw(IMPOIL)
#'
newImport <- function(
    name,
    desc = "",
    commodity = "",
    unit = NULL,
    reserve = Inf,
    imp = data.frame(),
    misc = list(),
    ...
    ) {
  .data2slots(
    "import", name,
    desc = desc,
    commodity = commodity,
    unit = unit,
    reserve = reserve,
    imp = imp,
    misc = misc,
    ...
  )
}

#' @param object an S4 class object to be updated.
#'
#' @param ... slot-names with data to update the S4 object
#'
#' @rdname update
#' @family update import
#' @method update import
#' @export
setMethod("update", "import", function(object, ...) {
  .data2slots("import", object, ...)
})
