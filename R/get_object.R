# get_object.R -- retrieve model *objects* (commodities, technologies, ...) from
# a repository / model / scenario, filtered by class, name, description and
# region. The object-level counterpart of getData() (which returns their data).
# Reuses the internal getObjects()/.getNames() engine (class/name/slot filters,
# R/get_data.R) and adds a class-agnostic region filter via get_region().

#' Retrieve model objects from a repository, model or scenario
#'
#' `getObject()` returns the model building-block objects held in a container --
#' commodities, technologies, supplies, storages, and so on -- selected by
#' **class**, **name**, **description**, **region** and/or any object **slot**.
#' It is the object-level counterpart of [getData()], which returns those
#' objects' parameter data.
#'
#' Class, name, description and any other slot are matched by the same engine
#' that powers the internal object accessors; the **region** filter is applied
#' with [get_region()], which reads regions uniformly from `@region` slots and
#' from the `region`/`src`/`dst` columns of data.frame slots, so it works for
#' every class (including `import`/`export`/`trade`, which store region
#' structurally). A scenario is unwrapped to its model automatically.
#'
#' @param x a `repository`, `model` or `scenario`.
#' @param class character vector of object classes to keep (e.g. `"technology"`,
#'   `c("supply", "import")`); `NULL` (default) keeps all classes.
#' @param name character, object name(s) to match against `@name`.
#' @param desc character, description pattern(s) to match against `@desc`.
#' @param region character, keep objects belonging to any of these regions.
#'   Region-agnostic objects (e.g. commodities, which carry no region) are kept
#'   for every region unless `region_agnostic = FALSE`.
#' @param ... additional per-slot filters forwarded to the matching engine, e.g.
#'   `timeframe = "HOUR"`. Character filters are exact by default; a data.frame
#'   slot is filtered by named columns. Filtering by a slot implicitly restricts
#'   results to classes that have it.
#' @param regex logical; treat `name`, `desc` and character `...` filters as
#'   regular expressions instead of exact matches (default `FALSE`).
#' @param ignore.case logical; case-insensitive matching for `regex = TRUE`
#'   (default `TRUE`).
#' @param region_agnostic logical; whether objects with no region information
#'   satisfy a `region` filter (default `TRUE`).
#' @param drop logical; if `TRUE` and exactly one object matches, return that
#'   object itself instead of a one-element list (default `FALSE`).
#'
#' @return A named list of model objects keyed by `@name` (empty list if none
#'   match); or a single object when `drop = TRUE` and exactly one matches.
#' @seealso [getData()], [get_region()], [find_in_model()]
#' @examples
#' \dontrun{
#' repo <- utopia_modules$electricity$reg3$repo
#' getObject(repo, class = "technology")                 # all technologies
#' getObject(repo, class = c("supply", "commodity"))     # two classes
#' getObject(repo, region = "R1")                         # everything in R1
#' getObject(repo, name = "ECOA", drop = TRUE)            # the ECOA object
#' getObject(repo, desc = "coal", regex = TRUE)           # by description
#' getObject(repo, class = "technology", timeframe = "HOUR")  # slot filter
#' }
#' @export
getObject <- function(x, ...) UseMethod("getObject")

#' @rdname getObject
#' @export
getObject.default <- function(x, ...) {
  stop("getObject() is defined for 'repository', 'model' and 'scenario' ",
       "objects; got '", class(x)[1], "'.", call. = FALSE)
}

.getObject_container <- function(x, class = NULL, name = NULL, desc = NULL,
                                 region = NULL, ..., regex = FALSE,
                                 ignore.case = TRUE, region_agnostic = TRUE,
                                 drop = FALSE) {
  flt <- list(...)
  if (!is.null(name)) flt[["name"]] <- as.character(name)
  if (!is.null(desc)) flt[["desc"]] <- as.character(desc)
  rgx <- if (isTRUE(regex)) TRUE else NULL

  pull <- function(cl) do.call(getObjects, c(
    list(obj = x, class = cl, regex = rgx, ignore.case = ignore.case), flt))

  # getObjects()/.getNames() matches a single class at a time; loop for many.
  if (is.null(class) || length(class) == 0L) {
    objs <- pull(c())
  } else {
    objs <- list()
    for (cl in class) objs <- c(objs, pull(cl))
  }

  # class-agnostic region filter via get_region()
  if (!is.null(region) && length(objs)) {
    region <- as.character(region)
    keep <- vapply(objs, function(o) {
      r <- get_region(o)
      if (length(r) == 0L) isTRUE(region_agnostic) else
        length(intersect(region, r)) > 0L
    }, logical(1))
    objs <- objs[keep]
  }

  if (isTRUE(drop) && length(objs) == 1L) return(objs[[1L]])
  objs
}

#' @rdname getObject
#' @export
getObject.repository <- .getObject_container

#' @rdname getObject
#' @export
getObject.model <- .getObject_container

#' @rdname getObject
#' @export
getObject.scenario <- .getObject_container
