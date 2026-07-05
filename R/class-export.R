#' An S4 class to represent commodity export to the rest of the world.
#' 
#' @name class-export
#' 
#' @inherit newExport description
#' @inherit newExport details
#'
#' @md
#' @slot name `r get_slot_doc("export", "name")`
#' @slot desc `r get_slot_doc("export", "desc")`
#' @slot commodity `r get_slot_doc("export", "commodity")`
#' @slot unit `r get_slot_doc("export", "unit")`
#' @slot reserve `r get_slot_doc("export", "reserve")`
#' @slot exp `r get_slot_doc("export", "exp")`
#' @slot misc `r get_slot_doc("export", "misc")`
#'
#' @include class-import.R
#' @family class export process
#' @rdname class-export
#'
#' @export
#'
setClass("export",
  representation(
    name = "character",
    desc = "character",
    commodity = "character",
    unit = "character",
    # !!! add @region
    # !!! make reserve a data.frame with region, year, upper, lower, fixed
    reserve = "numeric",
    exp = "data.frame",
    # timeframe = "character", # depreciated (equal to commodity@timeframe)
    misc = "list"
  ),
  prototype(
    name = "", # ...
    desc = "",
    commodity = "",
    unit = "",
    reserve = Inf,
    exp = data.frame(
      region = character(),
      year = integer(),
      slice = character(),
      exp.lo = numeric(),
      exp.up = numeric(),
      exp.fx = numeric(),
      price = numeric(),
      stringsAsFactors = FALSE
    ),
    # GIS           = NULL,
    # timeframe = character(),
    # ! Misc
    misc = list()
  ),
  S3methods = FALSE
)

setMethod("initialize", "export", function(.Object, ...) {
  .Object
})

#' Create new export object
#'
#' @description
#' Export object represent commodity export to the Rest of the World (RoW).
#'
#' @details
#' `export` is a type of process that adds an "external" source to a commodity
#' to the model. The Rest of the World (RoW) is not modeled explicitly,
#' `export` and `import` objects define and control the exchange with the RoW.
#' The operation of the export object is similar to the `demand` objects,
#' the two different classes are used to distinguish domestic and external
#' sources of final consumption.
#' The export is controlled by the `exp` data frame, which specifies
#' bounds and fixed values for the export of the export flow.
#' The `exp.fx` column is used to specify fixed values of the export flow,
#' making the export flow exogenous. The `exp.lo` and `exp.up` columns are used
#' to specify lower and upper bounds of the export flow, making the export flow
#' endogenous. The `price` column is used to specify the exogenous price
#' for the export commodity.
#' The `reserve` slot is used to set limits on the total export over the
#' model horizon.
#'
#' @md
#' @param name `r get_slot_doc("export", "name")`
#' @param desc  `r get_slot_doc("export", "desc")`
#' @param commodity `r get_slot_doc("export", "commodity")`
#' @param unit `r get_slot_doc("export", "unit")`
#' @param reserve `r get_slot_doc("export", "reserve")`
#' @param exp `r get_slot_doc("export", "exp")`
#' @param misc `r get_slot_doc("export", "misc")`
#'
#' @return export object with given specifications.
#' @rdname newExport
#' @order 1
#' @export
#' @family create export
#' @examples
#'EXPOIL <- newExport(
#'   name = "EXPOIL", # used in sets
#'   desc = "Oil export from the model to RoW", # for own reference
#'   commodity = "OIL", # must match the commodity name in the model
#'   unit = "Mtoe", # for own reference
#'   exp = data.frame(
#'     region = rep(c("R1", "R2"), each = 2), # export region(s)
#'     year = rep(c(2020, 2050)), # export years
#'     price = 500, # export price in MUSD/Mtoe (USD/t),
#'     exp.up = rep(c(1e3, 1e4), each = 2), # upper bound for export in each year
#'     exp.lo = rep(c(5e2, 0), each = 2) # lower bound for export in each year
#'   )
#' )
#' draw(EXPOIL)
newExport <- function(
    name,
    desc = "",
    commodity = "",
    unit = NULL,
    reserve = Inf,
    exp = data.frame(),
    misc = list(),
    ...
    ) {
  .data2slots("export",
    name,
    desc = desc,
    commodity = commodity,
    unit = unit,
    reserve = reserve,
    exp = exp,
    misc = misc,
    ...
    )
}

#' Update export object
#'
#' @description
#' The method replaces slots of the export object with new values.
#'
#' @param object object of class export
#'
#' @param ... arguments-slots (see `newExport`) with updated values to replace.
#'
#' @rdname newTechnology
#' @family update export
#' @method update export
#' @export
setMethod("update", "export", function(object, ...) {
  .data2slots("export", object, ...)
})

