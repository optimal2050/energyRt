# =============================================================================#
# repo_store.R — content-addressed repository store (layout 3, stage S4c).
#
# `repositories/<name>@<hash8>/` holds one saved repository version:
# `repository.yml`, the thinned `repo.RData`, and parquet stores for the big
# data slots — the model store's pattern (R/model_store.R) one level down.
# Repositories are where real sharing happens: many models differing only in
# config carry identical repos, and `save_model(embed_repos=)` can reference
# a stored repository instead of embedding its data into every model entry.
# =============================================================================#

#' Content hash of a repository
#'
#' @description
#' The repository analogue of [model_hash()]: a stable content hash of the
#' repository with volatile bookkeeping stripped (every `@misc`, the
#' `inMemory` flag, data.table's session-specific attributes). Identical
#' content hashes identically; on-disk state and session history do not
#' matter. As with models, a repository whose data slots were thinned to disk
#' hashes differently from its in-memory original — hash before thinning
#' (`save_repository()` does).
#'
#' @param repo a repository object.
#' @return character, the full hash (the store uses its first 8 characters).
#' @export
repository_hash <- function(repo) {
  stopifnot(is(repo, "repository"))
  rlang::hash(.strip_volatile(repo))
}

#' Save a repository to / load a repository from the repository store
#'
#' @description
#' `save_repository()` writes a repository into the content-addressed store
#' `<repositories_path>/<name>@<hash8>/` (see [get_repositories_path()]):
#' a `repository.yml` manifest, the thinned `repo.RData`, and parquet stores
#' for the large data slots. Saving identical content again is a no-op. The
#' repository is registered in the project registry unless `registry =
#' FALSE`. A stored repository can then be REFERENCED by models instead of
#' embedded — see [save_model()]'s `embed_repos`.
#'
#' `load_repository()` resolves a stored repository by `name` (optionally
#' `"name@hash8"`, or with `hash =`) via the registry first, then by scanning
#' the store; `path` bypasses resolution.
#'
#' @param repo a repository object.
#' @param name character, repository name, `"name@hash8"`, or a store
#'   directory path.
#' @param hash character, full or short content hash to pin a version.
#' @param path character, explicit store directory (bypasses the registry).
#' @param registry logical, add/refresh the registry row on save.
#' @param format storage format for the data slots (as in [save_scenario()]).
#' @param overwrite logical, rewrite the store entry even when the hash
#'   matches.
#' @param env environment to assign the repository into by name, or `NULL`
#'   (default) to return it.
#' @param verbose logical.
#'
#' @return `save_repository()` the repository, invisibly (with `misc$hash` /
#'   `misc$path` recording the store entry); `load_repository()` the
#'   repository object (or `invisible(TRUE)` when assigned into `env`).
#'
#' @rdname repo_store
#' @export
save_repository <- function(
    repo,
    path = NULL,
    registry = TRUE,
    format = get_arrow_format(),
    overwrite = FALSE,
    verbose = TRUE) {
  stopifnot(is(repo, "repository"))
  if (!nzchar(repo@name)) stop("The repository must have a non-empty @name")
  format <- if (tolower(format) %in% c("feather", "arrow", "ipc")) {
    "parquet"
  } else {
    tolower(format)
  }
  h <- repository_hash(repo)
  h8 <- substr(h, 1, 8)
  if (is.null(path)) {
    path <- fp(get_repositories_path(),
               paste0(.path_slug(repo@name), "@", h8))
  }
  path <- gsub("[\\/]+", "/", path)

  mf_path <- fp(path, "repository.yml")
  if (file.exists(mf_path) && !overwrite) {
    prev <- tryCatch(yaml::read_yaml(mf_path), error = function(e) NULL)
    if (!is.null(prev) && identical(prev$hash, h)) {
      if (verbose) {
        message("Repository '", repo@name, "' (", h8,
                ") is already in the store: ", path)
      }
      repo@misc$hash <- h
      repo@misc$path <- path
      return(invisible(repo))
    }
  }

  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  repo <- obj2disk(repo, path = path, format = format, verbose = verbose)
  repo@misc$hash <- h
  save(repo, file = fp(path, "repo.RData"))
  write("repository", fp(path, "class"), append = FALSE)
  write(as.character(.SCENARIO_LAYOUT), fp(path, "layout"), append = FALSE)
  yaml::write_yaml(list(
    layout = .SCENARIO_LAYOUT,
    class = "repository",
    name = repo@name,
    hash = h,
    created = .registry_now(),
    energyRt_version = as.character(utils::packageVersion("energyRt")),
    format = format
  ), mf_path)
  if (verbose) {
    cat("Repository '", repo@name, "' (", h8, ") saved in '", path, "'\n",
        sep = "")
  }

  if (isTRUE(registry)) {
    tryCatch({
      reg <- registry_load()
      reg <- registry_add(reg, "repository", repo@name,
                          path = .registry_rel_path(path), hash = h)
      registry_save(reg)
    }, error = function(e) {
      warning("Could not update the project registry (",
              conditionMessage(e), ")", call. = FALSE)
    })
  }
  invisible(repo)
}

.repo_store_resolve <- function(name, hash = NULL) {
  .store_resolve(name, hash, reg_type = "repository",
                 root = get_repositories_path(),
                 manifest = "repository.yml")
}

# Rebase the on-disk paths of a stored repository to the directory it was
# loaded from (obj2disk writes each object's data at data/<object>/<slot>/).
.repo_rebase <- function(repo, root) {
  root <- gsub("[\\/]+", "/", root)
  if (length(get_ondisk_slots(repo))) repo@misc$path <- root
  for (ob in names(repo@data)) {
    o <- repo@data[[ob]]
    if (isS4(o) && .hasSlot(o, "misc") && length(get_ondisk_slots(o))) {
      o@misc$path <- fp(root, "data", ob)
      repo@data[[ob]] <- o
    }
  }
  repo
}

#' @rdname repo_store
#' @export
load_repository <- function(name, hash = NULL, path = NULL, env = NULL,
                            verbose = TRUE) {
  if (is.null(path)) {
    if (dir.exists(name) && file.exists(fp(name, "repository.yml"))) {
      path <- name
    } else {
      path <- .repo_store_resolve(name, hash)
    }
  }
  if (is.null(path) || !file.exists(fp(path, "repo.RData"))) {
    stop("Repository '",
         if (is.null(hash)) name else paste0(name, "@", hash),
         "' was not found in the registry or the repository store ('",
         get_repositories_path(), "').\n",
         "  Run registry_refresh() to rescan, save_repository() to store ",
         "it, or pass path= to a repository directory.")
  }
  e <- new.env(parent = emptyenv())
  nm <- load(fp(path, "repo.RData"), envir = e)
  if (length(nm) != 1L || !is(get(nm, envir = e), "repository")) {
    stop("'", fp(path, "repo.RData"),
         "' must contain exactly one repository object")
  }
  repo <- .repo_rebase(get(nm, envir = e), path)
  if (verbose) {
    cat("Repository '", repo@name, "' loaded from '", path, "'\n", sep = "")
  }
  if (is.null(env)) return(repo)
  assign(repo@name, repo, envir = env)
  invisible(TRUE)
}
