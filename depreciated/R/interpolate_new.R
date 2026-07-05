# the function takes a model object,
# attaches additional objects from `...` to the model repository,
# creates scenario object
# reads the model configuration (from `model@config`)
# looks for settings parameters in `...` and updates config if available,
# reads parameters from the repository objects,
# interpolates using parameters from `settings`,
# and populates scenario@modInp with the model data
# returns scenario object with interpolated, ready to use sets and parameters

.interpolate_mod <- function(obj,
                             name = NULL,
                             desc = NULL,
                             ...,
                             overwrite = FALSE) {
  # obj - model
  scen <- new("scenario")
  scen@model <- obj
  # if (!is.null(name)) scen@name <- name
  # if (!is.null(desc)) scen@desc <- desc
  .interpolate_scen(scen, name, desc, ..., overwrite = FALSE)

}

.interpolate_scen <- function(obj,
                              name = NULL,
                              desc = NULL,
                              ...,
                              overwrite = FALSE
                              ) {
  arg <- list(...)
  if (!is.null(arg$year)) scen@model@sysInfo@year <- arg$year #!!! horizon
  if (!is.null(arg$repository)) {
    scen@model <- .add_repository(scen@model, arg$repository, ...,
                                  overwrite = overwrite)
  }
  if (!is.null(arg$region)) scen@model@sysInfo@region <- arg$region
  if (!is.null(arg$discount)) scen@model@sysInfo@discount <- arg$discount
  if (is.null(arg$verbose)) arg$verbose <- 0
  # add "update" option

  ### Interpolation
  scen@modInp <- new("modInp")

  ## horizon

}

# withinHorizon <- function(obj, settings) {
#   # return(T)
#   # browser()
#   # if (inherits(obj, "trade")) browser()
#   # yrs <- range()
#   yrs <- settings@horizon@period
#   ret <- NULL # return NULL if not applicable to the object
#   # check stock
#   sn <- slotNames(obj)
#   if (any(sn == "stock")) {
#     stock <- obj@stock # !!! add check for interpolation rule or interpolate first
#     if (nrow(stock) > 0 && any(stock$year > min(yrs)) && any(stock$stock > 0))  {
#       return(TRUE) # capacity exists within the period
#     } else {
#       ret <- FALSE
#     }
#   }
#   if (any(sn == "end")) {
#     if (is.data.frame(obj@end)) {
#       end <- obj@end$end
#     } else {
#       end <- obj@end
#     }
#
#     if (is.null(end) || is_empty(end)) {
#       end <- TRUE
#     } else if (any(is.na(end))) { # at least in one region
#       end <- TRUE
#     } else if (!all(end < min(yrs))) {
#       end <- TRUE
#     } else {
#       end <- FALSE
#       return(FALSE) # not available for investment
#     }
#
#     # if (end == TRUE) { supposed to be true
#     if (is.data.frame(obj@start)) {
#       start <- obj@start$start
#     } else {
#       start <- obj@start
#     }
#     if (is.null(start) || is_empty(start)) {
#       start <- TRUE
#     } else if (any(is.na(start))) { # at least in one region
#       start <- TRUE
#     } else if (!all(start > max(yrs))) {
#       start <- TRUE
#     } else {
#       start <- FALSE
#       return(FALSE) # not available for investment
#     }
#
#     if (end & start) return(TRUE)
#     ret <- FALSE
#     # }
#   }
#   return(ret)
# }

if (F) {
  obj <- ideea_modules$techs$ECOASUB$ECOASUB_2020
  settings <- scen@settings
  settings@horizon@period
  withinHorizon(obj, settings)
  sapply(ideea_modules$techs$ECOASUB@data, withinHorizon, settings = settings)
  sapply(ideea_modules$techs$ECOAULT@data, withinHorizon, settings = settings)

  withinHorizon("obj", settings)

}
