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
#' @slot export `r get_slot_doc("export", "export")`
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
    reserve = "data.frame",
    export = "data.frame",
    cluster = "data.frame",
    # timeframe = "character", # depreciated (equal to commodity@timeframe)
    misc = "list"
  ),
  prototype(
    name = "", # ...
    desc = "",
    commodity = "",
    unit = "",
    # Cumulative limit over the whole horizon, summed across ALL regions, years
    # and timeslices -- see `eqExportRowCum` / `eqImportRowCum`, which sum over
    # `r in region`. A data.frame rather than a scalar so it can carry a
    # `cluster` column and be split across price steps; without that every step
    # of a stepped curve would inherit the FULL limit and the model would quietly
    # hold `nsteps` times the resource. There is deliberately NO `region` column:
    # adding one would turn this into a per-region cap and LOSE the all-region
    # total, which is what it means today.
    reserve = data.frame(
      cluster = character(),
      res.lo = numeric(),
      res.up = numeric(),
      res.fx = numeric(),
      stringsAsFactors = FALSE
    ),
    export = data.frame(
      cluster = character(),
      region = character(),
      year = integer(),
      timeslice = character(),
      exp.lo = numeric(),
      exp.up = numeric(),
      exp.fx = numeric(),
      price = numeric(),
      stringsAsFactors = FALSE
    ),
    # GIS           = NULL,
    # timeframe = character(),
    # Cluster declaration. A cluster is a parallel sub-object of the same
    # process -- for these classes, a PRICE STEP of a stepped supply / export /
    # import curve, with its own share of the quantity and its own price. A
    # single price cannot express a curve: real resource grades get dearer as
    # you exhaust them, and a real export market pays less as you push volume
    # into it.
    #
    # This slot declares WHAT the steps are; the per-step values live in the
    # `cluster` column of the other slots. Build both with `asSupplyCurve()` /
    # `asExportCurve()` / `asImportCurve()` rather than by hand.
    #
    # `share` here is DESCRIPTIVE, not enforced: unlike `technology` or `trade`,
    # these classes have no capacity variable to tie, so the helper writes the
    # split straight into the quantity bounds and records what it did here.
    # `order` fixes the fill order (1 = first); without it labels sort
    # alphabetically and "S10" would precede "S2".
    cluster = data.frame(
      cluster = character(),
      desc = character(),
      share = numeric(),
      order = integer(),
      stringsAsFactors = FALSE
    ),
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
#' @param export `r get_slot_doc("export", "export")`
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
#'   export = data.frame(
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
    reserve = data.frame(),
    export = data.frame(),
    cluster = data.frame(),
    misc = list(),
    ...
    ) {
  # `reserve` was a bare number before 0.85. Accept one and read it as the
  # cumulative upper limit it always was, so existing models keep working.
  if (is.numeric(reserve)) {
    reserve <- if (length(reserve) == 1L && is.finite(reserve))
      data.frame(res.up = as.numeric(reserve), stringsAsFactors = FALSE) else
      data.frame()
  }
  .data2slots("export",
    name,
    desc = desc,
    commodity = commodity,
    unit = unit,
    reserve = reserve,
    export = export,
    cluster = cluster,
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

