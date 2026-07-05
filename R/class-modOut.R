#' An S4 class to store results of a solved scenario
#'
#' @name class-modOut
#'
#' @description
#' The class is a part of the scenario object and stores the
#' results of a solved scenario. It is not intended to be used
#' as a standalone object.
#'
#' @slot sets `r get_slot_doc("modOut", "sets")`
#' @slot variables `r get_slot_doc("modOut", "variables")`
#' @slot stage `r get_slot_doc("modOut", "stage")`
#' @slot solutionLogs `r get_slot_doc("modOut", "solutionLogs")`
#' @slot misc `r get_slot_doc("modOut", "misc")`
#'
#' @include class-modInp.R
#' @rdname class-modOut
#' @export
setClass("modOut",
  representation(
    sets = "list",
    # data = "list", # Should be removed
    variables = "list",
    stage = "character",
    solutionLogs = "data.frame",
    misc = "list"
  ),
  prototype(
    sets = list(),
    # data = list(),
    variables = list(),
    stage = character(),
    solutionLogs = data.frame(
      parameter = character(),
      value = character(),
      time = character()
    ),
    misc = list()
  ),
  S3methods = FALSE
)

setMethod("initialize", "modOut", function(.Object, ...) {
  .Object
})
