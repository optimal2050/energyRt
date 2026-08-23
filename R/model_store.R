# =============================================================================#
# model_store.R — content-addressed model store (storage layout 3, stage S3).
#
# `models/<name>@<hash8>/` holds one saved model version: a `model.yml`
# manifest, the thinned `mod.RData`, and parquet stores for the big data
# slots (weather/demand), written with the same obj2disk machinery as
# scenarios. The full content hash identifies a model version; scenarios can
# reference a stored model by name+hash instead of embedding it
# (`save_scenario(embed_model = ...)`), which removes the model copy from
# every scenario's scen.RData.
# =============================================================================#

# Canonical form for hashing: strip volatile bookkeeping (@misc everywhere,
# the inMemory flag, and data.table's session-specific attributes — the
# `.internal.selfref` external pointer and any `index`) so the same modeled
# content always hashes the same, regardless of on-disk state or session
# history.
.strip_volatile <- function(x) {
  if (isS4(x)) {
    if (.hasSlot(x, "misc")) x@misc <- list()
    if (.hasSlot(x, "inMemory")) x@inMemory <- TRUE
    for (s in slotNames(x)) {
      slot(x, s) <- .strip_volatile(slot(x, s))
    }
    return(x)
  }
  if (is.data.frame(x)) {
    attr(x, ".internal.selfref") <- NULL
    attr(x, "index") <- NULL
    return(x)
  }
  if (is.list(x) && length(x)) {
    return(lapply(x, .strip_volatile))
  }
  x
}

#' Content hash of a model
#'
#' @description
#' A stable content hash (`rlang::hash`, xxHash) of the model with volatile
#' bookkeeping stripped: every `@misc` list (paths, on-disk markers,
#' timestamps) and the `inMemory` flag are removed, recursively, before
#' hashing. Two models built identically hash identically; renaming a folder,
#' saving, or loading does not change the hash — changing any modeled content
#' does.
#'
#' Note: a model whose data slots were thinned to disk (e.g. inside an
#' on-disk scenario) hashes differently from its in-memory original, because
#' the data is genuinely absent from the object. Hash models before/without
#' thinning — `save_model()` does.
#'
#' @param mod a model object.
#' @return character, the full hash. The store uses its first 8 characters
#'   in directory names (`<name>@<hash8>`).
#' @export
#' @examples
#' \dontrun{
#' model_hash(mod)
#' }
model_hash <- function(mod) {
  stopifnot(is(mod, "model"))
  rlang::hash(.strip_volatile(mod))
}

#' Save a model to / load a model from the model store
#'
#' @description
#' `save_model()` writes a model into the content-addressed store
#' `<models_path>/<name>@<hash8>/` (see [get_models_path()]): a `model.yml`
#' manifest, the thinned `mod.RData`, and parquet stores for the large data
#' slots. Saving the identical content again is a no-op. The model is
#' registered in the project registry (see [registry_load()]) unless
#' `registry = FALSE`.
#'
#' `load_model()` resolves a stored model by `name` (optionally
#' `"name@hash8"`, or with `hash =`) via the registry first, then by scanning
#' the store directory; `path` bypasses resolution and loads that directory.
#'
#' @param mod a model object.
#' @param name character, model name, `"name@hash8"`, or a store directory
#'   path.
#' @param hash character, full or short content hash to pin a version.
#' @param path character, explicit store directory (bypasses the registry).
#' @param registry logical, add/refresh the registry row on save.
#' @param format storage format for the data slots (as in [save_scenario()]).
#' @param overwrite logical, rewrite the store entry even when the hash
#'   matches.
#' @param env environment to assign the model into by name, or `NULL`
#'   (default) to return it.
#' @param verbose logical.
#'
#' @return `save_model()` the model, invisibly (with `misc$hash` /
#'   `misc$path` recording the store entry); `load_model()` the model object
#'   (or `invisible(TRUE)` when assigned into `env`).
#'
#' @rdname model_store
#' @export
save_model <- function(
    mod,
    path = NULL,
    registry = TRUE,
    format = get_arrow_format(),
    overwrite = FALSE,
    verbose = TRUE) {
  stopifnot(is(mod, "model"))
  if (!nzchar(mod@name)) stop("The model must have a non-empty @name")
  format <- if (tolower(format) %in% c("feather", "arrow", "ipc")) {
    "parquet"
  } else {
    tolower(format)
  }
  h <- model_hash(mod)
  h8 <- substr(h, 1, 8)
  if (is.null(path)) {
    path <- fp(get_models_path(), paste0(.path_slug(mod@name), "@", h8))
  }
  path <- gsub("[\\/]+", "/", path)

  mf_path <- fp(path, "model.yml")
  if (file.exists(mf_path) && !overwrite) {
    prev <- tryCatch(yaml::read_yaml(mf_path), error = function(e) NULL)
    if (!is.null(prev) && identical(prev$hash, h)) {
      if (verbose) {
        message("Model '", mod@name, "' (", h8, ") is already in the store: ",
                path)
      }
      mod@misc$hash <- h
      mod@misc$path <- path
      return(invisible(mod))
    }
  }

  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  # thin the big data slots into parquet next to the manifest
  mod <- obj2disk(mod, path = path, format = format, verbose = verbose)
  mod@misc$hash <- h
  save(mod, file = fp(path, "mod.RData"))
  write("model", fp(path, "class"), append = FALSE)
  write(as.character(.SCENARIO_LAYOUT), fp(path, "layout"), append = FALSE)
  yaml::write_yaml(list(
    layout = .SCENARIO_LAYOUT,
    class = "model",
    name = mod@name,
    hash = h,
    created = .registry_now(),
    energyRt_version = as.character(utils::packageVersion("energyRt")),
    format = format
  ), mf_path)
  if (verbose) {
    cat("Model '", mod@name, "' (", h8, ") saved in '", path, "'\n", sep = "")
  }

  if (isTRUE(registry)) {
    tryCatch({
      reg <- registry_load()
      reg <- registry_add(reg, "model", mod@name,
                          path = .registry_rel_path(path), hash = h)
      registry_save(reg)
    }, error = function(e) {
      warning("Could not update the project registry (",
              conditionMessage(e), ")", call. = FALSE)
    })
  }
  invisible(mod)
}

# Resolve a stored model directory from name/hash: registry first, then a
# scan of the store. Returns the directory or NULL.
.model_store_resolve <- function(name, hash = NULL) {
  if (grepl("@", name, fixed = TRUE)) {
    parts <- strsplit(name, "@", fixed = TRUE)[[1]]
    name <- parts[1]
    if (is.null(hash)) hash <- parts[2]
  }
  # 1. registry
  hit <- tryCatch(
    registry_find(registry_load(), type = "model", name = name, hash = hash),
    error = function(e) NULL)
  if (!is.null(hit) && nrow(hit)) {
    d <- fp(dirname(get_registry_file()), hit$path[1])
    if (file.exists(fp(d, "model.yml"))) return(gsub("[\\/]+", "/", d))
  }
  # 2. store scan
  root <- get_models_path()
  if (!dir.exists(root)) return(NULL)
  dd <- list.dirs(root, recursive = FALSE)
  slug <- .path_slug(name)
  dd <- dd[startsWith(basename(dd), paste0(slug, "@"))]
  if (!is.null(hash) && nzchar(hash)) {
    keep <- vapply(dd, function(d) {
      mf <- tryCatch(yaml::read_yaml(fp(d, "model.yml")),
                     error = function(e) NULL)
      !is.null(mf) && startsWith(mf$hash %||% "", hash)
    }, logical(1))
    dd <- dd[keep]
  }
  if (length(dd) == 1L) return(gsub("[\\/]+", "/", dd))
  if (length(dd) > 1L) {
    stop("Model '", name, "' has ", length(dd), " versions in '", root,
         "': ", paste(basename(dd), collapse = ", "),
         ". Pin one with hash= or \"name@hash8\".")
  }
  NULL
}

# Rebase the on-disk paths of a stored model to the directory it was actually
# loaded from (the store entry may have been moved or created elsewhere).
.model_rebase <- function(mod, root) {
  root <- gsub("[\\/]+", "/", root)
  if (length(get_ondisk_slots(mod))) mod@misc$path <- root
  for (rp in names(mod@data)) {
    repo <- mod@data[[rp]]
    if (!isS4(repo)) next
    if (length(get_ondisk_slots(repo))) {
      repo@misc$path <- fp(root, "data", rp)
    }
    for (ob in names(repo@data)) {
      o <- repo@data[[ob]]
      if (isS4(o) && .hasSlot(o, "misc") && length(get_ondisk_slots(o))) {
        o@misc$path <- fp(root, "data", rp, "data", ob)
        repo@data[[ob]] <- o
      }
    }
    mod@data[[rp]] <- repo
  }
  mod
}

#' @rdname model_store
#' @export
load_model <- function(name, hash = NULL, path = NULL, env = NULL,
                       verbose = TRUE) {
  if (is.null(path)) {
    if (dir.exists(name) && file.exists(fp(name, "model.yml"))) {
      path <- name
    } else {
      path <- .model_store_resolve(name, hash)
    }
  }
  if (is.null(path) || !file.exists(fp(path, "mod.RData"))) {
    stop("Model '", if (is.null(hash)) name else paste0(name, "@", hash),
         "' was not found in the registry or the model store ('",
         get_models_path(), "').\n",
         "  Run registry_refresh() to rescan, save_model() to store it, ",
         "or pass path= to a model directory.")
  }
  e <- new.env(parent = emptyenv())
  nm <- load(fp(path, "mod.RData"), envir = e)
  if (length(nm) != 1L || !is(get(nm, envir = e), "model")) {
    stop("'", fp(path, "mod.RData"), "' must contain exactly one model object")
  }
  mod <- .model_rebase(get(nm, envir = e), path)
  if (verbose) {
    cat("Model '", mod@name, "' loaded from '", path, "'\n", sep = "")
  }
  if (is.null(env)) return(mod)
  assign(mod@name, mod, envir = env)
  invisible(TRUE)
}
