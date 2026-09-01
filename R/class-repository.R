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
    "tax", "subsidy", "constraint", "costs"
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
  obj <- new("repository")
  .assert_object_name(name, what = "repository")
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
  arg = list(...) |> unlist()
  if (is_empty(arg)) return(obj)
  arg <- lapply(arg, function(x) {
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


# setMethod("add", signature("repository", "commodity"), function())

#------------------------------------------------------------------------------#
# ! add_to_repository <- function(x, add) : Add to repository
#------------------------------------------------------------------------------#
# if (!isClassUnion('repository')) setClassUnion("repository")
# Add to repository

# add <- function(...) UseMethod("add")

