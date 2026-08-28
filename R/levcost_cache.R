# =============================================================================#
# levcost_cache.R — on-disk cache for levcost() results.
#
# A levcost result is a pure function of (object content, assumptions), so it
# is cached under a key that contains everything affecting the output and
# nothing else. Cached results live WITH the object they price when that
# object is saved (`<owner>/levcost/`); results for in-memory objects are
# temporary and land in the project-level `get_levcost_cache_path()` folder
# (default `levcosts/`). Artifacts: `<name-slug>@<key8>.rds` (the levcost list
# with `$scenario` stripped) plus a `.yml` sidecar carrying the FULL key — the
# 8-character filename is an address, the sidecar is the authority.
# =============================================================================#

# Arguments that select cache behavior; stripped from the compute call.
.levcost_cache_args <- c("cache", "force", "cache_dir")

# Arguments that never affect the result and are excluded from the key.
.levcost_key_drop <- c(.levcost_cache_args, "verbose", "max_failures",
                       "as_scenario", "full_output")

# The folder an object's derived artifacts belong to, or NULL when the object
# is in-memory / unsaved (reports and levcost caches are then temporary).
.object_store_dir <- function(x) {
  if (is(x, "scenario")) {
    p <- x@path
    if (length(p) == 1L && nzchar(p) && dir.exists(p)) return(p)
    return(NULL)
  }
  if (is(x, "model") || is(x, "repository")) {
    p <- x@misc[["path"]]
    if (!is.null(p) && dir.exists(p)) return(p)
    cand <- tryCatch(
      if (is(x, "model")) .model_store_resolve(x@name, NULL) else
        .repo_store_resolve(x@name, NULL),
      error = function(e) NULL)
    if (!is.null(cand) && dir.exists(cand)) return(cand)
  }
  NULL
}

# Resolution order: explicit cache_dir > owning folder (saved objects only) >
# the project-level temporary tier.
.levcost_cache_dir <- function(owner = NULL, cache_dir = NULL) {
  if (!is.null(cache_dir)) return(cache_dir)
  own <- if (!is.null(owner)) .object_store_dir(owner) else NULL
  if (!is.null(own)) return(fp(own, "levcost"))
  get_levcost_cache_path()
}

# The cache key: engine + object content + normalized assumptions. S4 values
# in the assumptions (repo, horizon, calendar, weather objects) enter as
# content hashes; a solver option list enters as its name/lang only (never
# paths); everything else is keyed as passed, sorted by name.
.levcost_cache_key <- function(class, object, args) {
  a <- args[setdiff(names(args), .levcost_key_drop)]
  a <- if (length(a)) a[order(names(a))] else list()
  norm_one <- function(v) {
    if (isS4(v)) return(object_hash(v))
    if (is.list(v) && !is.data.frame(v)) {
      if (!is.null(v[["lang"]])) {
        return(list(name = v[["name"]], lang = v[["lang"]]))
      }
      return(lapply(v, norm_one))   # e.g. repo = list of S4 objects
    }
    v
  }
  rlang::hash(list(engine = paste0("levcost/", class), ver = 1L,
                   object = object_hash(object), args = lapply(a, norm_one)))
}

.levcost_cache_base <- function(dir, name, key) {
  fp(dir, paste0(.path_slug(name), "@", substr(key, 1, 8)))
}

# Recursively drop the solved mini-scenario(s): they are working state, large,
# and reproducible; the cached artifact is the result tables only.
.levcost_strip_scenario <- function(result) {
  if (is.list(result) && !is.data.frame(result)) {
    at <- attributes(result)
    result$scenario <- NULL
    out <- lapply(result, function(el) {
      if (is.list(el) && !is.data.frame(el)) .levcost_strip_scenario(el) else el
    })
    at$names <- names(out)   # lapply drops class and co.; restore everything
    attributes(out) <- at
    return(out)
  }
  result
}

.levcost_cache_read <- function(dir, name, key, verbose = TRUE) {
  base <- .levcost_cache_base(dir, name, key)
  rds <- paste0(base, ".rds")
  yml <- paste0(base, ".yml")
  if (!file.exists(rds) || !file.exists(yml)) return(NULL)
  side <- tryCatch(yaml::read_yaml(yml), error = function(e) NULL)
  if (is.null(side) || !identical(side$key, key)) return(NULL)
  res <- tryCatch(readRDS(rds), error = function(e) NULL)
  if (is.null(res)) return(NULL)
  if (isTRUE(verbose)) {
    message("levcost: cached result is current: ", rds,
            " (force = TRUE to recompute)")
  }
  res
}

.levcost_cache_write <- function(dir, name, key, result, class, method = NULL) {
  ok <- tryCatch({
    if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
    base <- .levcost_cache_base(dir, name, key)
    saveRDS(.levcost_strip_scenario(result), paste0(base, ".rds"))
    yaml::write_yaml(list(
      kind = "levcost", version = 1L, key = key, class = class,
      process = name, method = method,
      created = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      energyRt_version = as.character(utils::packageVersion("energyRt"))
    ), paste0(base, ".yml"))
    TRUE
  }, error = function(e) {
    warning("levcost: could not write the cache entry: ",
            conditionMessage(e), call. = FALSE)
    FALSE
  })
  invisible(ok)
}

# The single integration point, called by the levcost() methods: split the
# cache arguments out of the dots, consult the cache, run the driver on a
# miss (`fun(object, <dots minus cache args>)`), and record the fresh result.
# `as_scenario` requests bypass the cache read (the cached artifact has no
# $scenario) but the computed result is still recorded.
.levcost_with_cache <- function(class, object, dots, fun) {
  cache <- dots[["cache"]] %||% TRUE
  force <- dots[["force"]] %||% FALSE
  verbose <- dots[["verbose"]] %||% TRUE
  name <- if (isS4(object) && .hasSlot(object, "name")) object@name else "levcost"
  dir <- .levcost_cache_dir(object, dots[["cache_dir"]])
  key <- .levcost_cache_key(class, object, dots)
  bypass <- isTRUE(dots[["as_scenario"]]) || isTRUE(dots[["full_output"]])
  if (isTRUE(cache) && !isTRUE(force) && !bypass) {
    hit <- .levcost_cache_read(dir, name, key, verbose = verbose)
    if (!is.null(hit)) return(hit)
  }
  res <- do.call(fun, c(list(object),
                        dots[setdiff(names(dots), .levcost_cache_args)]))
  if (isTRUE(cache) && !bypass && !is.null(res)) {
    .levcost_cache_write(dir, name, key, res, class = class,
                         method = res[["method"]] %||% dots[["method"]])
  }
  res
}

#' Clear cached levcost results
#'
#' Deletes the levcost cache entries belonging to an object (its owning
#' folder's `levcost/` for saved objects, the project-level
#' [get_levcost_cache_path()] folder otherwise), or a directory given
#' directly.
#'
#' @param x an energyRt object whose cache should be cleared, or a character
#'   path to a levcost cache directory.
#' @return the deleted directory, invisibly.
#' @export
clear_levcost_cache <- function(x = NULL) {
  dir <- if (is.character(x)) x else .levcost_cache_dir(x)
  if (!is.null(dir) && dir.exists(dir)) unlink(dir, recursive = TRUE)
  invisible(dir)
}
