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
#' repo <- utopia$modules$electricity$R3$repo
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

# -- registry ------------------------------------------------------------------
# A registry indexes SAVED objects, so getObject() on one loads what the rows
# point at. Names are unique only within a type (a model and a scenario may
# both be "UTOPIA", and run labels repeat across scenarios), so the returned
# list is keyed `type/name` rather than by bare name -- otherwise same-named
# objects of different types would overwrite each other.

#' @rdname getObject
#' @param type character, registry row type(s) to load: "model", "repository",
#'   "scenario", "dataset". `NULL` (default) loads every loadable type.
#' @param hash character, content hash (prefix) selecting a stored version.
#' @param parent,variant character, extra registry filters (run bookkeeping).
#' @param run which run to activate in loaded scenarios: `"main"` (default)
#'   keeps the scenario's own run, a character selects that run, and `NULL`,
#'   `NA` or `"ALL"` returns one entry per recorded run, keyed
#'   `scenario/<name>:<variant>/<label>`.
#' @export
getObject.en_registry <- function(x, type = NULL, name = NULL, hash = NULL,
                                  parent = NULL, variant = NULL,
                                  run = "main", drop = FALSE,
                                  verbose = FALSE, ...) {
  rows <- find_in_registry(x, type = type, name = name, hash = hash,
                           parent = parent, variant = variant)
  # run rows are bookkeeping inside a scenario, not standalone objects; they
  # are reachable through `run =` below, or scenario_runs()
  rows <- rows[rows$type != "run", , drop = FALSE]
  if (!nrow(rows)) {
    if (isTRUE(drop)) {
      stop("No registry entry matches; nothing to return with drop = TRUE.",
           call. = FALSE)
    }
    return(list())
  }

  all_runs <- .run_is_all(run)
  out <- list()
  for (i in seq_len(nrow(rows))) {
    r <- rows[i, , drop = FALSE]
    obj <- .registry_row_object(r, verbose = verbose)
    key <- paste0(r$type, "/", r$name)
    if (identical(r$type, "scenario") && !identical(run, "main")) {
      if (all_runs) {
        rr <- tryCatch(scenario_runs(obj), error = function(e) NULL)
        if (is.null(rr) || !nrow(rr)) { out[[key]] <- obj; next }
        for (j in seq_len(nrow(rr))) {
          sc <- tryCatch(read_solution(obj, run = rr$run[j], echo = FALSE),
                         error = function(e) NULL)
          if (is.null(sc)) next
          out[[paste0(key, ":", rr$run[j])]] <- sc
        }
        next
      }
      obj <- read_solution(obj, run = run, echo = FALSE)
      key <- paste0(key, ":", run)
    }
    out[[key]] <- obj
  }

  if (isTRUE(drop)) {
    if (length(out) != 1L) {
      stop("drop = TRUE needs exactly one match; got ", length(out),
           if (length(out)) paste0(": ", paste(names(out), collapse = ", "))
           else "", ".\n  Narrow with type=/name=/hash=, or use drop = FALSE.",
           call. = FALSE)
    }
    return(out[[1]])
  }
  out
}
