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
#' @slot import `r get_slot_doc("import", "import")`
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
    reserve = "data.frame",
    # !!! add region to export
    import = "data.frame",
    cluster = "data.frame",
    # timeframe = "character", # set to commodity@timeframe
    misc = "list"
  ),
  prototype(
    name = "",
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
    import = data.frame(
      cluster = character(),
      region = character(),
      year = integer(),
      timeslice = character(),
      imp.lo = numeric(),
      imp.up = numeric(),
      imp.fx = numeric(),
      price = numeric(),
      stringsAsFactors = FALSE
    ),
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
#' @param import `r get_slot_doc("import", "import")`
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
#'   import = data.frame(
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
    reserve = data.frame(),
    import = data.frame(),
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
  .data2slots(
    "import", name,
    desc = desc,
    commodity = commodity,
    unit = unit,
    reserve = reserve,
    import = import,
    cluster = cluster,
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
