# class repository ####
#' An S4 class to store the model objects.
#'
#' @name class-repository
#'
#' @description
#' Use `newRepository` to create a new repository object.
#'
#' @inherit newRepository description
#'
#' @md
#' @slot name `r get_slot_doc("repository", "name")`
#' @slot desc `r get_slot_doc("repository", "desc")`
#' @slot data `r get_slot_doc("repository", "data")`
#' @slot permit `r get_slot_doc("repository", "permit")`
#' @slot misc `r get_slot_doc("repository", "misc")`
#'
#' @export
#' @family repository model data
#' @include generics.R
#' @rdname class-repository
setClass("repository",
  representation(
    name = "character",
    desc = "character", # Details
    data = "list",
    permit = "character",
    misc = "list"
  ),
  prototype(
    name = "",
    desc = "", # Details
    data = list(),
    permit = character(),
    misc = list()
  ),
  S3methods = FALSE
)

# initialize ####
setMethod("initialize", "repository", function(.Object, ...) {
  .Object@permit <- c(
    "commodity", "demand", "supply",
    "technology", "storage",
    "trade", "export", "import", "weather",
    "tax", "sub", "constraint", "costs"
  )
  .Object
})

# newRepository ####
#' A constructor for the repository class
#' @name newRepository
#'
#' @description
#' Repository class is used to store the model 'bricks' such as commodity,
#' technology, supply, demand, trade, import, export, trade, storage, etc.
#' Calendars, settings, and configurations cannot be stored in the repository, they
#' have separate slots in model or scenario objects.
#'
#' @md
#' @param name `r get_slot_doc("repository", "name")`
#' @param ... `r get_slot_doc("repository", "data")`
#' @param desc `r get_slot_doc("repository", "desc")`
#' @param misc `r get_slot_doc("repository", "misc")`
#'
#' @export
#' @family repository model data
newRepository <- function(
    name = "base_repository",
    ...,
    desc = NA_character_,
    misc = list()
    ) {
  # browser()
  obj <- new("repository")
  obj@name <- name
  arg <- list(...)
  if (!is.na(desc) && !is_empty(desc)) arg <- c(arg, desc = desc)
  if (!is_empty(misc)) arg <- c(arg, misc = misc)
  if (is_empty(arg)) return(obj)
  slots <- slotNames(obj); slots <- slots[slots != ".S3Class"]
  for (s in slots) {
    if (!is.null(arg[[s]])) {slot(obj, s) <- arg[[s]]; arg[[s]] <- NULL}
  }
  if (is_empty(arg)) return(obj)
  # obj <- do.call("add", c(obj = obj, unlist(add)))
  obj <- add(obj, arg)
  return(obj)
}

if (F) {
  newRepository() #|> print()
  newRepository("repo", ELC, GAS) #|> print()
  newRepository("repo", ELC, GAS, ECOA) |> print()
  newRepository("repo", ELC, GAS, ECOA, TRBD_ELC) |> print()

}
  # nn <- rep(FALSE, length(arg)) # imported args
  # for (i in seq_along(arg)) {
  #   if (class(arg[[i]])[1] %in% obj@permit) {
  #     obj <- add(obj, arg[[i]]); nn[[i]] <- TRUE
  #   }
  # }
  # arg <- arg[!nn]
  # N <- length(arg)
  # if (!is_empty(arg)) warning("Ignored ", N, "objects: ",
  #                             paste(head(names(arg), 100), sep = ", "),
  #                             ifelse(N > 100, "...", "."))
  # obj <- do.call(add, list(obj = obj, unlist(arg)))
  # return(obj)
  # old script
  # in_rep <- c("commodity", "technology", "supply", "demand", "trade",
  #             "import", "export", "trade", "storage")
  # rps <- .data2slots("repository", name, ignore_classes = in_rep, ...)
  # arg <- list(...)
  # arg <- arg[sapply(arg, class) %in% in_rep]
  # if (length(arg) > 0) rps <- add(rps, arg)
  # rps
# }


# Methods ####
## [[ ####
#' @export
setMethod("[[", c("repository", "ANY"),
  function(x, name) x@data[[name]]
)

#' @export
setMethod("[[", c("repository", "character"),
          function(x, i) x@data[[i]]
)

#' @export
setMethod("[[", c("repository", "numeric"),
          function(x, i) x@data[[i]]
)

## $ ####
#' @export
setMethod("$", "repository", function(x, name) x@data[[name]])

setReplaceMethod("$", c("repository", "ANY"),
  function(x, name, value) {
    nm <- names(x@data)
    ii <- which(nm == value@name)
    if (length(ii) > 0) {
      # replace name
      nm[ii] <- value@name
      x@data[[name]] <- value
      names(x@data) <- nm
    } else {
      x@data[[name]] <- value
    }
    x
  }
)

## names ####
#' @export
#' @family repository
setMethod("names", "repository", function(x) names(x@data))

## print ####
#' @export
#' @family repository
setMethod("print", "repository", function(x) {
  cat("repository '", x@name, "': ", length(x@data), " objects.",
      if_else(is_empty(x@desc) || x@desc == "", "\n",
             paste("\n", x@desc, "\n")), sep = "")
  # print(
    data.table::data.table(
      name = sapply(x@data, function(x) x@name),
      class = sapply(x@data, class)
    )
  # )
})

## show ####
#' @method show repository
#' @export
#' @family repository
setMethod("show", "repository", function(object) print(object))

## length ####
#' @method length repository
#' @export
#' @family repository
setMethod("length", "repository", function(x) length(x@data))

## summary ####
#' @export
#' @method summary repository
#' @family repository
setMethod("summary", signature(object = "repository"), function(object, ...) {
  x <- sapply(object@data, class)
  x <- as.factor(x)
  return(summary(x))
})

## add ####
#' @method add repository
#' @rdname add
#' @family repository
#' @export
setMethod("add", signature("repository"), function(obj, ..., overwrite = FALSE) {
  # browser()
  arg = list(...) |> unlist()
  if (is_empty(arg)) return(obj)
  arg <- sapply(arg, function(x) {
    if (is(x, "repository")) return(x@data)
    x
  }) |> list_flatten()
  ii <- sapply(arg, function(x) class(x)[1] %in% obj@permit)
  for (ob in arg[ii]) {
    if (!is.null(obj@data[[ob@name]]) && !overwrite) {
      stop("Object ", ob@name, " already exists in the repository.\n",
           "Use overwrite = TRUE to replace.")
    }
    obj@data[[ob@name]] <- ob
  }
  arg[ii] <- NULL
  N <- length(arg)
  if (N > 0) {
    warning("Ignored ", N, "objects: ",
            paste(head(names(arg), 10), sep = ", "),
            ifelse(N > 10, "...", "."))
  }
  return(obj)
})

#
# setReplaceMethod("[[", c("repository", "ANY", "ANY"),
#                  function(x, name, value) {
#                    x@data[[name]] = value
#                    x
#                  }
# )

# setMethod("add", "repository", add.repository)
# setGeneric("newRepository", function(name, ...) standardGeneric("newRepository"))
#
# setMethod("newRepository", signature(name = "character"), function(name, ...) {
#   in_rep <- c("commodity", "technology", "supply", "demand", "trade", "import", "export", "trade", "storage")
#   rps <- .data2slots("repository", name, ignore_classes = in_rep, ...)
#   arg <- list(...)
#   arg <- arg[sapply(arg, class) %in% in_rep]
#   if (length(arg) > 0) rps <- add(rps, arg)
#   rps
# })

# setMethod("add", signature("repository", "commodity"), function())

#------------------------------------------------------------------------------#
# ! add_to_repository <- function(x, add) : Add to repository
#------------------------------------------------------------------------------#
# if (!isClassUnion('repository')) setClassUnion("repository")
# Add to repository

# add <- function(...) UseMethod("add")

# @rdname add
#
# @export
# @family repository
# add.repository <- function(obj, app, ..., overwrite = FALSE) {
#   if (length(list(...)) != 0) {
#     obj <- add(obj, app, overwrite = overwrite)
#     arg <- list(...)
#     for (i in seq(along = arg)) {
#       obj <- add(obj, arg[[i]], overwrite = overwrite)
#     }
#   } else if (class(app) == "repository") {
#     for (i in seq(along = app@data)) {
#       obj <- add(obj, app@data[[i]], overwrite = overwrite)
#     }
#   } else if (class(app) == "list") {
#     for (i in seq(along = app)) {
#       obj <- add(obj, app[[i]], overwrite = overwrite)
#     }
#   } else {
#     # if (class(add) != tolower(sub('^.', '', class(x)))) stop('Error type to repository')
#     if (all(class(app) != c(
#       # "region", "commodity", "stock", "reserve",
#       "commodity", "demand", "supply", "technology", "storage",
#       "trade", "export", "import", "weather",
#       "tax", "sub", "constraint", "costs"
#     ))) {
#       stop("Error type to repository ", class(app))
#     }
#     if (app@name == "" ||
#       any(sapply(obj@data, function(z) app@name == z@name & class(app) == class(z)))) {
#       if (app@name == "" || !overwrite) stop("Check name of the object")
#       obj@data <-
#         obj@data[!sapply(obj@data, function(z) app@name == z@name & class(app) == class(z))]
#     }
#     if (sub("[[:alpha:]][[:alnum:]_]*", "", app@name) != "") {
#       stop("Check name of the object")
#     }
#     # mx <- c(names(x@data), add@name)
#     obj@data[[app@name]] <- app
#   }
#   obj
# }
