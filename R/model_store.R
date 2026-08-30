# =============================================================================#
# model_store.R — content-addressed model store (storage layout 3, stage S3).
#
# `models/<name>/` holds one saved model — the folder is the NAME, the
# content hash lives in the manifest (never in the path): a `model.yml`
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
      v <- .strip_volatile(slot(x, s))
      # a slot whose prototype is NULL (e.g. scenario@desc) reads back as
      # NULL, which cannot be re-assigned into a typed slot -- leave it be
      if (!is.null(v)) slot(x, s) <- v
    }
    return(x)
  }
  if (is.data.frame(x)) {
    attr(x, ".internal.selfref") <- NULL
    attr(x, "index") <- NULL
    # canonical representation: serialization is sensitive to attribute
    # ORDER and row-name form, which differ between a constructed table and
    # one rehydrated from parquet (arrow orders names,class,row.names) —
    # normalize so content, not representation, decides the hash. Natively
    # built tables already have this shape, so their hashes are unchanged.
    at <- attributes(x)
    ord <- intersect(c("names", "row.names", "class"), names(at))
    attributes(x) <- at[c(ord, setdiff(names(at), ord))]
    attr(x, "row.names") <- .set_row_names(nrow(x))
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

#' Content hash of any energyRt object
#'
#' @description
#' The generalization of [model_hash()] to any S4 object of the package
#' (technology, storage, trade, scenario, calendar, horizon, weather, ...):
#' `rlang::hash` of the object with volatile bookkeeping stripped, so two
#' identically-built objects hash identically regardless of `@misc` contents,
#' paths, or the `inMemory` flag. Models and repositories delegate to their
#' dedicated hashes. Used as the object-identity half of the report and
#' levcost cache keys.
#'
#' The thinning caveat of [model_hash()] applies: an object whose data slots
#' live on disk hashes differently from its in-memory original.
#'
#' @param x an S4 object (model and repository delegate to [model_hash()] /
#'   [repository_hash()]).
#' @return character, the full hash.
#' @rdname model_hash
#' @export
object_hash <- function(x) {
  if (is(x, "model")) return(model_hash(x))
  if (is(x, "repository")) return(repository_hash(x))
  stopifnot(isS4(x))
  rlang::hash(.strip_volatile(x))
}

#' Save a model to / load a model from the model store
#'
#' @description
#' `save_model()` writes a model into the content-addressed store
#' `<models_path>/<name>/` (see [get_models_path()]): a `model.yml`
#' manifest, the thinned `mod.RData`, and parquet stores for the large data
#' slots. Saving the identical content again is a no-op. The model is
#' registered in the project registry (see [load_registry()]) unless
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
#' @param embed_repos `NULL` (default), `TRUE`, or `FALSE`. Controls whether
#'   the model's repositories are embedded in the store entry or referenced
#'   from the repository store (see [save_repository()]). `NULL`: reference a
#'   repository when its identical content is already stored, embed
#'   otherwise. `TRUE`: always embed. `FALSE`: require a store hit for every
#'   repository. The model hash is always taken over the FULL model, so it is
#'   unaffected by this choice; `load_model()` resolves references back.
#' @param embed_datasets the same choice one level further down, for the big
#'   data tables (weather/demand) of embedded repositories and the geoscale
#'   map on `@config` — see [save_dataset()]. `NULL` (default) references
#'   whatever identical content is already in the dataset store; `TRUE`
#'   always embeds; `FALSE` requires a store hit.
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
    embed_repos = NULL,
    embed_datasets = NULL,
    registry = TRUE,
    format = get_arrow_format(),
    overwrite = FALSE,
    rehash = TRUE,
    verbose = TRUE) {
  stopifnot(is(mod, "model"))
  if (!nzchar(mod@name)) stop("The model must have a non-empty @name")
  format <- if (tolower(format) %in% c("feather", "arrow", "ipc")) {
    "parquet"
  } else {
    tolower(format)
  }
  # A thinned (on-disk) model hashes by its EMPTY slots — rehydrate first,
  # so re-saving a just-saved/just-loaded model is a genuine no-op and an
  # edited one a clean update (never a spurious new entry, never an entry
  # overwritten with empty data).
  if (!isInMemory(mod)) mod <- obj2mem(mod, verbose = FALSE)
  # CRITICAL ORDER: the model hash is computed on the FULL model, before any
  # repository is replaced by a store reference below — the recorded hash
  # identifies the complete content regardless of how the repositories are
  # persisted (embedded vs referenced).
  h <- model_hash(mod)
  h8 <- substr(h, 1, 8)
  if (is.null(path)) {
    path <- .store_entry_dir(get_models_path(), mod@name, h, type = "model")
  }
  path <- gsub("[\\/]+", "/", path)

  mf_path <- fp(path, "model.yml")
  prev <- NULL
  hash_kept <- FALSE
  if (file.exists(mf_path)) {
    prev <- tryCatch(yaml::read_yaml(mf_path), error = function(e) NULL)
    if (!overwrite && !is.null(prev) && identical(prev$hash, h)) {
      if (verbose) {
        message("Model '", mod@name, "' (", h8, ") is already in the store: ",
                path)
      }
      mod@misc$hash <- h
      mod@misc$path <- path
      return(invisible(mod))
    }
    .seal_guard(prev, "model", mod@name, "unseal_model")
    # rehash = FALSE: the user declares the change insignificant — write the
    # new content but KEEP the recorded hash (a version tag, not a content
    # proof; the manifest marks the exception with `hash_kept`)
    if (!isTRUE(rehash) && nzchar(prev$hash %||% "")) {
      h <- prev$hash
      h8 <- substr(h, 1, 8)
      hash_kept <- TRUE
    }
    # changed content: the entry updates IN PLACE — clear the previous
    # payload, keep the derived-artifact caches beside it
    .store_entry_wipe(path)
  }

  # Repository references: with embed_repos = NULL (auto), each repository
  # whose exact content is already in the repository store (save_repository())
  # is replaced — in the SAVED copy only — by a stub carrying
  # `misc$repo_ref`; the manifest records every repository either way.
  # TRUE always embeds; FALSE requires a store hit for every repository.
  repo_entries <- list()
  full_repos <- NULL
  for (rp in names(mod@data)) {
    repo <- mod@data[[rp]]
    if (!is(repo, "repository")) next
    rh <- tryCatch(repository_hash(repo), error = function(e) "")
    store_dir <- NULL
    if (!isTRUE(embed_repos) && nzchar(rh)) {
      store_dir <- tryCatch(.repo_store_resolve(repo@name, rh),
                            error = function(e) NULL)
    }
    if (!is.null(store_dir)) {
      repo_entries[[rp]] <- list(name = repo@name, hash = rh,
                                 source = "ref",
                                 path = .registry_rel_path(store_dir))
    } else if (isFALSE(embed_repos)) {
      stop("embed_repos = FALSE, but repository '", repo@name, "' (",
           substr(rh, 1, 8), ") is not in the repository store ('",
           get_repositories_path(), "'). save_repository() it first, or ",
           "use embed_repos = NULL/TRUE.")
    } else {
      repo_entries[[rp]] <- list(name = repo@name, hash = rh,
                                 source = "embedded")
    }
  }
  ref_slots <- names(repo_entries)[vapply(repo_entries,
                                          \(e) e$source == "ref", logical(1))]
  if (length(ref_slots)) {
    full_repos <- mod@data
    for (rp in ref_slots) {
      stub <- new("repository")
      stub@name <- mod@data[[rp]]@name
      stub@misc$repo_ref <- repo_entries[[rp]]
      mod@data[[rp]] <- stub
    }
    if (verbose) {
      cat("Repositories referenced from the store (not embedded): ",
          paste(vapply(repo_entries[ref_slots], `[[`, "", "name"),
                collapse = ", "), "\n", sep = "")
    }
  }

  # Dataset references (one level below repositories): the geoscale map on
  # @config and the big tables of EMBEDDED repositories' weather/demand
  # objects become {name, hash} refs when the identical content is in the
  # dataset store (a REFERENCED repository's datasets are that store
  # entry's business). Same hash-before-stub contract: `h` is already fixed.
  ds_entries <- list()
  full_config <- NULL
  if (!isTRUE(embed_datasets)) {
    th <- .thin_dataset_slot(mod@config, embed_datasets, id = "config",
                             verbose = verbose)
    if (!is.null(th$entry)) {
      mod@config <- th$obj
      ds_entries[[length(ds_entries) + 1L]] <- th$entry
      full_config <- th$full
    }
    for (rp in setdiff(names(mod@data), ref_slots)) {
      repo <- mod@data[[rp]]
      if (!is(repo, "repository")) next
      touched <- FALSE
      for (ob in names(repo@data)) {
        th <- .thin_dataset_slot(repo@data[[ob]], embed_datasets, id = ob,
                                 verbose = verbose)
        if (!is.null(th$entry)) {
          repo@data[[ob]] <- th$obj
          ds_entries[[length(ds_entries) + 1L]] <- th$entry
          touched <- TRUE
        }
      }
      if (touched) {
        if (is.null(full_repos)) full_repos <- mod@data
        mod@data[[rp]] <- repo
      }
    }
  }

  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  # thin the big data slots into parquet next to the manifest
  mod <- obj2disk(mod, path = path, format = format, verbose = verbose)
  mod@misc$hash <- h
  # obj2disk records the path only when it wrote data slots; with everything
  # referenced there is nothing to write, so record it explicitly
  mod@misc$path <- path
  save(mod, file = fp(path, "mod.RData"))
  write("model", fp(path, "class"), append = FALSE)
  write(as.character(.SCENARIO_LAYOUT), fp(path, "layout"), append = FALSE)
  yaml::write_yaml(c(list(
    layout = .SCENARIO_LAYOUT,
    class = "model",
    name = mod@name,
    hash = h,
    created = prev$created %||% .registry_now(),
    updated = .registry_now(),
    energyRt_version = as.character(utils::packageVersion("energyRt")),
    format = format,
    repositories = unname(repo_entries)
  ), if (length(ds_entries)) list(datasets = ds_entries),
     if (hash_kept) list(hash_kept = TRUE),
     .lifecycle_carry(prev)), mf_path)
  # the returned in-memory object keeps its full repositories and geoscale
  if (!is.null(full_repos)) mod@data <- full_repos
  if (!is.null(full_config)) {
    mod@config@geoscale <- full_config
    mod@config@misc[["dataset_ref"]] <- NULL
  }
  if (verbose) {
    cat("Model '", mod@name, "' (", h8, ") saved in '", path, "'\n", sep = "")
  }

  if (isTRUE(registry)) {
    tryCatch({
      reg <- .registry_open()
      reg <- add_to_registry(reg, "model", mod@name,
                          path = .registry_rel_path(path), hash = h)
      save_registry(reg)
    }, error = function(e) {
      warning("Could not update the project registry (",
              conditionMessage(e), ")", call. = FALSE)
    })
  }
  invisible(mod)
}

# Resolve a content-addressed store directory from name/hash: registry first,
# then a scan of the store root. Shared by the model and repository stores.
# Returns the directory or NULL; errors when several versions match and no
# hash pins one.
# The folder a store entry lives in: one human-readable folder per NAME,
# updated in place. The content hash stays in the manifest/misc — for no-op
# detection and reference verification — and never in the path. (Pre-rework
# `<name>@<hash8>` folders still resolve; they just cannot be newly made.)
# Basename overridable via the `store_entry` path builder (receives type +
# name); resolution stays manifest-based, so custom folder names load fine.
.store_entry_dir <- function(root, name, hash, type = "") {
  base <- .path_hook("store_entry", function(type, name) .path_slug(name),
                     type, name)
  gsub("[\\/]+", "/", fp(root, base))
}

# In-place update of an entry dir: clear the previous payload but KEEP the
# mutable derived-artifact caches living beside it.
.store_entry_wipe <- function(path, keep = c("reports", "levcost")) {
  if (!dir.exists(path)) return(invisible(FALSE))
  ff <- list.files(path, all.files = TRUE, no.. = TRUE)
  unlink(fp(path, setdiff(ff, keep)), recursive = TRUE, force = TRUE)
  invisible(TRUE)
}

.store_resolve <- function(name, hash, reg_type, root, manifest) {
  if (grepl("@", name, fixed = TRUE)) {
    parts <- strsplit(name, "@", fixed = TRUE)[[1]]
    name <- parts[1]
    if (is.null(hash)) hash <- parts[2]
  }
  # 1. registry
  hit <- tryCatch(
    find_in_registry(.registry_open(), type = reg_type, name = name, hash = hash),
    error = function(e) NULL)
  if (!is.null(hit) && nrow(hit)) {
    d <- fp(dirname(get_registry_file()), hit$path[1])
    if (file.exists(fp(d, manifest))) return(gsub("[\\/]+", "/", d))
  }
  # 2. store scan: a plain `<slug>/` entry (default layout) and/or
  # `<slug>@<hash8>` versions (versioned layout, or pre-rework saves)
  if (!dir.exists(root)) return(NULL)
  dd <- list.dirs(root, recursive = FALSE)
  slug <- .path_slug(name)
  cand <- dd[basename(dd) == slug |
               startsWith(basename(dd), paste0(slug, "@"))]
  cand <- cand[file.exists(fp(cand, manifest))]
  if (!length(cand)) {
    # slug miss (custom `store_entry` folder names): match manifests by name
    dd <- dd[file.exists(fp(dd, manifest))]
    mf_name <- vapply(dd, function(d) {
      mf <- tryCatch(yaml::read_yaml(fp(d, manifest)),
                     error = function(e) NULL)
      as.character(mf$name %||% "")[1]
    }, character(1))
    cand <- dd[mf_name == name]
  }
  dd <- cand
  if (!is.null(hash) && nzchar(hash)) {
    mf_hash <- vapply(dd, function(d) {
      mf <- tryCatch(yaml::read_yaml(fp(d, manifest)),
                     error = function(e) NULL)
      mf$hash %||% ""
    }, character(1))
    keep <- startsWith(mf_hash, hash)
    if (length(dd) && !any(keep)) {
      stop(reg_type, " '", name, "' version @", substr(hash, 1, 8),
           " is not in the store ('", root, "'): it now holds @",
           paste(substr(mf_hash, 1, 8), collapse = ", @"),
           ". The entry was updated in place; load without hash= for the ",
           "current version (entries update in place; the hash in the ",
           "manifest is the version record).")
    }
    dd <- dd[keep]
  }
  if (length(dd) == 1L) return(gsub("[\\/]+", "/", dd))
  if (length(dd) > 1L) {
    stop(reg_type, " '", name, "' has ", length(dd), " versions in '", root,
         "': ", paste(basename(dd), collapse = ", "),
         ". Pin one with hash= or \"name@hash8\".")
  }
  NULL
}

.model_store_resolve <- function(name, hash = NULL) {
  .store_resolve(name, hash, reg_type = "model", root = get_models_path(),
                 manifest = "model.yml")
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
         "  Run refresh_registry() to rescan, save_model() to store it, ",
         "or pass path= to a model directory.")
  }
  .mark_notice("model", path, name)
  e <- new.env(parent = emptyenv())
  nm <- load(fp(path, "mod.RData"), envir = e)
  if (length(nm) != 1L || !is(get(nm, envir = e), "model")) {
    stop("'", fp(path, "mod.RData"), "' must contain exactly one model object")
  }
  mod <- .model_rebase(get(nm, envir = e), path)

  # resolve repository references (repos saved to the repository store rather
  # than embedded — see save_model(embed_repos=))
  for (rp in names(mod@data)) {
    repo <- mod@data[[rp]]
    ref <- if (isS4(repo) && .hasSlot(repo, "misc")) {
      repo@misc$repo_ref
    } else {
      NULL
    }
    if (is.null(ref)) next
    r <- tryCatch(
      load_repository(ref$name, hash = ref$hash, verbose = FALSE),
      error = function(e) NULL)
    if (is.null(r) && !is.null(ref$path)) {
      cand <- fp(dirname(get_registry_file()), ref$path)
      cand_h <- tryCatch(yaml::read_yaml(fp(cand, "repository.yml"))$hash,
                         error = function(e) "")
      # accept the recorded path only when it still holds the recorded
      # VERSION — an in-place-updated entry falls through to the warning
      if (file.exists(fp(cand, "repo.RData")) &&
          startsWith(cand_h %||% "", ref$hash %||% "")) {
        r <- tryCatch(load_repository(ref$name, path = cand, verbose = FALSE),
                      error = function(e) NULL)
      }
    }
    if (is.null(r)) {
      # the exact version is gone (entry updated in place): resolve by NAME
      # and warn — the hash is verification, not an address
      cur <- tryCatch(.repo_store_resolve(ref$name, NULL),
                      error = function(e) NULL)
      if (!is.null(cur)) {
        cur_h <- tryCatch(yaml::read_yaml(fp(cur, "repository.yml"))$hash,
                          error = function(e) "")
        r <- tryCatch(load_repository(ref$name, path = cur, verbose = FALSE),
                      error = function(e) NULL)
        if (!is.null(r)) {
          warning("Model '", mod@name, "' references repository '", ref$name,
                  "' @", substr(ref$hash %||% "", 1, 8),
                  "; the store now holds @", substr(cur_h %||% "", 1, 8),
                  " — loading the current version. Results may not ",
                  "reproduce; re-save the model to adopt it, or seal_repository() the ",
                  "inputs of finished work.", call. = FALSE)
        }
      }
    }
    if (is.null(r)) {
      stop("Model '", mod@name, "' references repository '", ref$name, "@",
           substr(ref$hash %||% "", 1, 8), "' which is not in the registry ",
           "or the repository store ('", get_repositories_path(), "').\n",
           "  Run refresh_registry() to rescan, or re-save the model with ",
           "embed_repos = TRUE from a session that has the repository.")
    }
    mod@data[[rp]] <- r
  }

  # resolve dataset references (the geoscale map on @config, the big tables
  # of embedded repositories' objects) — a live object never holds a stub
  mod@config <- .resolve_dataset_refs(mod@config, verbose = verbose)
  for (rp in names(mod@data)) {
    repo <- mod@data[[rp]]
    if (!is(repo, "repository")) next
    for (ob in names(repo@data)) {
      o <- repo@data[[ob]]
      if (isS4(o) && .hasSlot(o, "misc")) {
        repo@data[[ob]] <- .resolve_dataset_refs(o, verbose = verbose)
      }
    }
    mod@data[[rp]] <- repo
  }

  if (verbose) {
    cat("Model '", mod@name, "' loaded from '", path, "'\n", sep = "")
  }
  if (is.null(env)) return(mod)
  assign(mod@name, mod, envir = env)
  invisible(TRUE)
}
