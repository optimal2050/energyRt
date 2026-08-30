# =============================================================================#
# repo_store.R — content-addressed repository store (layout 3, stage S4c).
#
# `repositories/<name>/` holds one saved repository (hash in the manifest):
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
#' `<repositories_path>/<name>/` (see [get_repositories_path()]):
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
#' @param embed_datasets `NULL` (default), `TRUE`, or `FALSE` — whether the
#'   big data tables (weather/demand) are embedded in this entry or stored
#'   as references into the dataset store (see [save_dataset()]). `NULL`
#'   references whatever identical content is already stored and embeds the
#'   rest; `TRUE` always embeds; `FALSE` requires a store hit and errors
#'   otherwise. Mirrors `embed_repos` / `embed_model` one level down.
#' @param name character, repository name, `"name@hash8"`, or a store
#'   directory path.
#' @param hash character, full or short content hash to pin a version.
#' @param path character, explicit store directory (bypasses the registry).
#' @param registry logical, add/refresh the registry row on save.
#' @param format storage format for the data slots (as in [save_scenario()]).
#' @param overwrite logical, rewrite the store entry even when the hash
#'   matches.
#' @param rehash logical; `FALSE` keeps the DESTINATION entry's recorded
#'   hash while writing the changed content — for changes the user declares
#'   insignificant (a description, a memo). The manifest marks the
#'   exception (`hash_kept: true`); references then verify against the
#'   retained hash. With `rehash = FALSE` the hash is a user-managed
#'   version tag, not a content proof.
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
    embed_datasets = NULL,
    registry = TRUE,
    format = get_arrow_format(),
    overwrite = FALSE,
    rehash = TRUE,
    verbose = TRUE) {
  stopifnot(is(repo, "repository"))
  if (!nzchar(repo@name)) stop("The repository must have a non-empty @name")
  format <- if (tolower(format) %in% c("feather", "arrow", "ipc")) {
    "parquet"
  } else {
    tolower(format)
  }
  # rehydrate a thinned repository: hash the true content, never re-save
  # an empty shell (see save_model)
  if (!isInMemory(repo)) repo <- obj2mem(repo, verbose = FALSE)
  h <- repository_hash(repo)
  h8 <- substr(h, 1, 8)
  if (is.null(path)) {
    path <- .store_entry_dir(get_repositories_path(), repo@name, h,
                             type = "repository")
  }
  path <- gsub("[\\/]+", "/", path)

  mf_path <- fp(path, "repository.yml")
  prev <- NULL
  hash_kept <- FALSE
  if (file.exists(mf_path)) {
    prev <- tryCatch(yaml::read_yaml(mf_path), error = function(e) NULL)
    if (!overwrite && !is.null(prev) && identical(prev$hash, h)) {
      if (verbose) {
        message("Repository '", repo@name, "' (", h8,
                ") is already in the store: ", path)
      }
      repo@misc$hash <- h
      repo@misc$path <- path
      return(invisible(repo))
    }
    .seal_guard(prev, "repository", repo@name, "unseal_repository")
    if (!isTRUE(rehash) && nzchar(prev$hash %||% "")) {
      h <- prev$hash
      h8 <- substr(h, 1, 8)
      hash_kept <- TRUE
    }
    .store_entry_wipe(path)
  }

  # Large data slots already in the dataset store become {name, hash} refs:
  # a stubbed (0-row) slot makes obj2disk skip the duplicate parquet, and the
  # ref travels in repo.RData on the owning object's @misc. NOTE: `h` was
  # computed on the FULL repository above — content identity is independent
  # of how the datasets are stored.
  ds_entries <- list()
  ds_full <- list()
  if (!isTRUE(embed_datasets)) {
    for (ob in names(repo@data)) {
      th <- .thin_dataset_slot(repo@data[[ob]], embed_datasets, id = ob,
                               verbose = verbose)
      if (!is.null(th$entry)) {
        repo@data[[ob]] <- th$obj
        ds_entries[[length(ds_entries) + 1L]] <- th$entry
        ds_full[[ob]] <- th$full
      }
    }
  }

  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  repo <- obj2disk(repo, path = path, format = format, verbose = verbose)
  repo@misc$hash <- h
  # obj2disk records the path only when it wrote data slots; with every big
  # table referenced there is nothing to write, so record it explicitly
  repo@misc$path <- gsub("[\\/]+", "/", path)
  save(repo, file = fp(path, "repo.RData"))
  write("repository", fp(path, "class"), append = FALSE)
  write(as.character(.SCENARIO_LAYOUT), fp(path, "layout"), append = FALSE)
  yaml::write_yaml(c(list(
    layout = .SCENARIO_LAYOUT,
    class = "repository",
    name = repo@name,
    hash = h,
    created = prev$created %||% .registry_now(),
    updated = .registry_now(),
    energyRt_version = as.character(utils::packageVersion("energyRt")),
    format = format
  ), if (length(ds_entries)) list(datasets = ds_entries),
     if (hash_kept) list(hash_kept = TRUE),
     .lifecycle_carry(prev)), mf_path)

  # the caller keeps a fully-loaded object: restore the referenced payloads
  # (the stub lives only in the store entry)
  for (ob in names(ds_full)) {
    o <- repo@data[[ob]]
    slot_name <- .dataset_slot_of(o)
    methods::slot(o, slot_name) <- ds_full[[ob]]
    o@misc[["dataset_ref"]][[slot_name]] <- NULL
    if (!length(o@misc[["dataset_ref"]])) o@misc[["dataset_ref"]] <- NULL
    repo@data[[ob]] <- o
  }
  if (verbose) {
    cat("Repository '", repo@name, "' (", h8, ") saved in '", path, "'\n",
        sep = "")
  }

  if (isTRUE(registry)) {
    tryCatch({
      reg <- .registry_open()
      reg <- add_to_registry(reg, "repository", repo@name,
                          path = .registry_rel_path(path), hash = h)
      save_registry(reg)
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
         "  Run refresh_registry() to rescan, save_repository() to store ",
         "it, or pass path= to a repository directory.")
  }
  .mark_notice("repository", path, name)
  e <- new.env(parent = emptyenv())
  nm <- load(fp(path, "repo.RData"), envir = e)
  if (length(nm) != 1L || !is(get(nm, envir = e), "repository")) {
    stop("'", fp(path, "repo.RData"),
         "' must contain exactly one repository object")
  }
  repo <- .repo_rebase(get(nm, envir = e), path)
  # dataset refs resolve eagerly: a live object never holds a stub
  for (ob in names(repo@data)) {
    o <- repo@data[[ob]]
    if (isS4(o) && .hasSlot(o, "misc")) {
      repo@data[[ob]] <- .resolve_dataset_refs(o, verbose = verbose)
    }
  }
  if (verbose) {
    cat("Repository '", repo@name, "' loaded from '", path, "'\n", sep = "")
  }
  if (is.null(env)) return(repo)
  assign(repo@name, repo, envir = env)
  invisible(TRUE)
}
