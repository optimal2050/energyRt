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
  if (inherits(x, "S7_object")) {
    # the class object rides along as an attribute and carries closures and
    # environments that serialize differently in every session; the class
    # attribute names it, the properties are the content
    attr(x, "S7_class") <- NULL
    for (a in setdiff(names(attributes(x)), "class")) {
      attr(x, a) <- .strip_volatile(attr(x, a))
    }
    return(x)
  }
  if (isS4(x)) {
    if (.hasSlot(x, "misc")) x@misc <- list()
    if (.hasSlot(x, "inMemory")) x@inMemory <- TRUE
    for (s in slotNames(x)) {
      # an object serialized before a slot was added to its class carries no
      # such attribute; the class declares the slot, the instance lacks it,
      # and slot() would error. Upgrading is interp_mod()'s job, not the
      # hasher's -- here the absent slot simply contributes nothing.
      if (!.hasSlot(x, s)) next
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
#' @param object a model object.
#' @return character, the full hash. The store uses its first 8 characters
#'   in directory names (`<name>@<hash8>`).
#' @export
#' @examples
#' \dontrun{
#' model_hash(mod)
#' }
model_hash <- function(object) {
  stopifnot(is(object, "model"))
  rlang::hash(.strip_volatile(object))
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
#' @param object a model object.
#' @param name character, model name, `"name@hash8"`, or a store directory
#'   path.
#' @param hash character, full or short content hash to pin a version.
#' @param path character, explicit store directory (bypasses the registry).
#' @param mode `"ref"` or `"full"`; defaults to
#'   `getOption("energyRt.save_mode", "ref")`. `"ref"` references a
#'   repository whose exact content is a SEALED version in the repository
#'   store and copies the others in; `"full"` copies every repository and
#'   dataset in (a self-contained entry). An explicit `embed_repos` /
#'   `embed_datasets` overrides the mode for its level.
#' @param embed_repos `NULL` (default: by `mode`), `TRUE`, or `FALSE`.
#'   Controls whether the model's repositories are embedded in the store
#'   entry or referenced from the repository store (see [save_repository()]).
#'   `NULL`: reference a repository whose identical content is a sealed
#'   version, embed otherwise. `TRUE`: always embed. `FALSE`: require a
#'   sealed version for every repository. The model hash is always taken over the FULL model, so it is
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
#' @examples
#' \dontrun{
#' open_project(tempdir())
#'
#' ek  <- topia$modules$electricity$R3
#' mod <- newModel("TOPIA", data = ek$repo,
#'                 calendar = topia$modules$calendars$s4_h24,
#'                 region = ek$regions,
#'                 horizon = topia$modules$horizons$base)
#' mod <- setGeoscale(mod, ek$geoscale)
#'
#' # writes models/<name>/ and registers this version
#' save_model(mod)
#'
#' # read it back by name (the newest version)
#' mod2 <- load_model("TOPIA")
#'
#' # the registry lists every stored version, each addressable by its hash
#' load_registry()
#' }
save_model <- function(
    object,
    path = NULL,
    embed_repos = NULL,
    embed_datasets = NULL,
    mode = NULL,
    registry = TRUE,
    format = get_storage_format(),
    overwrite = FALSE,
    rehash = TRUE,
    verbose = TRUE) {
  stopifnot(is(object, "model"))
  if (!nzchar(object@name)) stop("The model must have a non-empty @name")
  if (identical(.save_mode(mode), "full")) {
    embed_repos <- embed_repos %||% TRUE
    embed_datasets <- embed_datasets %||% TRUE
  }
  # "arrow" and "ipc" are aliases for feather, as in save_scenario(). These
  # stores used to pin parquet here for hash stability; they no longer do.
  # dataset_hash()/model_hash() run .dataset_canonical() over the payload,
  # which reduces a data.frame to class + column values + nrow, so the on-disk
  # container cannot reach the hash: a feather and a parquet round-trip of the
  # same frame were measured to give the identical digest. Reads sniff the
  # extension, so stores already written as parquet keep loading.
  format <- if (tolower(format) %in% c("arrow", "ipc")) {
    "feather"
  } else {
    tolower(format)
  }
  # A thinned (on-disk) model hashes by its EMPTY slots — rehydrate first,
  # so re-saving a just-saved/just-loaded model is a genuine no-op and an
  # edited one a clean update (never a spurious new entry, never an entry
  # overwritten with empty data).
  object <- .deep_mem(object)
  # CRITICAL ORDER: the model hash is computed on the FULL model, before any
  # repository is replaced by a store reference below — the recorded hash
  # identifies the complete content regardless of how the repositories are
  # persisted (embedded vs referenced).
  if (is.null(path)) {
    path <- .store_entry_dir(get_models_path(), object@name, "", type = "model")
  }
  path <- gsub("[\\/]+", "/", path)
  mf_path <- fp(path, "model.yml")
  prev <- if (file.exists(mf_path)) {
    tryCatch(yaml::read_yaml(mf_path), error = function(e) NULL)
  }
  # the content hash only when the entry is sealed (no-op or refusal) or
  # lazy hashing is off; otherwise a save id
  hashed <- !.lazy_hash() || isTRUE(prev$sealed)
  h <- if (hashed) model_hash(object) else .save_id()
  h8 <- substr(h, 1, 8)
  hash_kept <- FALSE
  if (!is.null(prev)) {
    if (!overwrite && hashed && identical(prev$hash, h)) {
      if (verbose) {
        message("Model '", object@name, "' (", h8, ") is already in the store: ",
                path)
      }
      object@misc$hash <- h
      object@misc$path <- path
      return(invisible(object))
    }
    .seal_guard(prev, "model", object@name, "unseal_model")
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
    .store_entry_wipe(path, keep = c("reports", "levcost", names(prev$versions %||% list())))
  }

  # Repository references: with embed_repos = NULL (auto), each repository
  # whose exact content is already in the repository store (save_repository())
  # is replaced — in the SAVED copy only — by a stub carrying
  # `misc$repo_ref`; the manifest records every repository either way.
  # TRUE always embeds; FALSE requires a store hit for every repository.
  repo_entries <- list()
  full_repos <- NULL
  for (rp in names(object@data)) {
    repo <- object@data[[rp]]
    if (!is(repo, "repository")) next
    rh <- if (isFALSE(embed_repos) || (!isTRUE(embed_repos) && .has_sealed("repository", repo@name))) {
      tryCatch(repository_hash(repo), error = function(e) "")
    } else {
      ""
    }
    store_dir <- NULL
    if (!isTRUE(embed_repos) && nzchar(rh)) {
      store_dir <- .sealed_entry("repository", repo@name, rh)
    }
    sel <- if (is.null(store_dir) && !isTRUE(embed_repos)) .selection_of(repo)
    if (!is.null(store_dir)) {
      repo_entries[[rp]] <- list(name = repo@name, hash = rh,
                                 source = "ref",
                                 path = .registry_rel_path(store_dir))
    } else if (!is.null(sel)) {
      repo_entries[[rp]] <- sel
    } else if (isFALSE(embed_repos)) {
      stop("embed_repos = FALSE, but repository '", repo@name, "' (",
           substr(rh, 1, 8), ") is not a sealed version in the repository store ('",
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
    full_repos <- object@data
    for (rp in ref_slots) {
      stub <- new("repository")
      stub@name <- object@data[[rp]]@name
      stub@misc$repo_ref <- repo_entries[[rp]]
      object@data[[rp]] <- stub
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
    th <- .thin_dataset_slot(object@config, embed_datasets, id = "config",
                             verbose = verbose)
    if (!is.null(th$entry)) {
      object@config <- th$obj
      ds_entries[[length(ds_entries) + 1L]] <- th$entry
      full_config <- th$full
    }
    for (rp in setdiff(names(object@data), ref_slots)) {
      repo <- object@data[[rp]]
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
        if (is.null(full_repos)) full_repos <- object@data
        object@data[[rp]] <- repo
      }
    }
  }

  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  # thin the big data slots into parquet next to the manifest
  object <- obj2disk(object, path = path, format = format, verbose = verbose,
                     min_rows = getOption("energyRt.ondisk_min_rows", 1e5))
  object@misc$hash <- h
  # obj2disk records the path only when it wrote data slots; with everything
  # referenced there is nothing to write, so record it explicitly
  object@misc$path <- path
  save(object, file = fp(path, "mod.RData"))
  write("model", fp(path, "class"), append = FALSE)
  write(as.character(.SCENARIO_LAYOUT), fp(path, "layout"), append = FALSE)
  yaml::write_yaml(c(list(
    layout = .SCENARIO_LAYOUT,
    class = "model",
    name = object@name,
    hash = h,
    created = prev$created %||% .registry_now(),
    updated = .registry_now(),
    energyRt_version = as.character(utils::packageVersion("energyRt")),
    format = format,
    hashed = hashed,
    repositories = unname(repo_entries)
  ), if (length(ds_entries)) list(datasets = ds_entries),
     if (hash_kept) list(hash_kept = TRUE),
     if (length(prev$versions)) list(versions = prev$versions),
     .lifecycle_carry(prev)), mf_path)
  # the sealed repository versions it references record it as a dependent
  for (re in repo_entries) {
    if (!identical(re$source, "ref")) next
    .dependents_add("repository", .abs_store_path(re$path), re$hash,
                    list(type = "model", name = object@name,
                         path = .registry_rel_path(path), hash = h))
  }
  # the returned in-memory object keeps its full repositories and geoscale
  if (!is.null(full_repos)) object@data <- full_repos
  if (!is.null(full_config)) {
    object@config@geoscale <- full_config
    object@config@misc[["dataset_ref"]] <- NULL
  }
  if (verbose) {
    cat("Model '", object@name, "' (", h8, ") saved in '", path, "'\n", sep = "")
  }

  if (isTRUE(registry)) {
    tryCatch({
      reg <- .registry_open()
      reg <- add_to_registry(reg, "model", object@name,
                          path = .registry_rel_path(path), hash = h)
      save_registry(reg)
    }, error = function(e) {
      warning("Could not update the project registry (",
              conditionMessage(e), ")", call. = FALSE)
    })
  }
  invisible(object)
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

# Sealed versions of a model or repository entry. The entry folder holds the
# manifest and, while unsealed, the payload (`mod.RData` / `repo.RData` and
# its `data/`), updated in place. Sealing moves the payload into
# `<entry>/<hash>/` and records the version in the manifest's `versions`; a
# sealed version is never modified, and survives unsealing and later saves
# for as long as it is kept.
.store_meta_items <- c("model.yml", "repository.yml", "class", "layout",
                       "reports", "levcost")

# the payload items of an entry's root (everything but metadata and versions)
.store_payload_items <- function(path, mf) {
  ff <- list.files(path, all.files = TRUE, no.. = TRUE)
  setdiff(ff, c(.store_meta_items, names(mf$versions %||% list())))
}

# Move the root payload into `<entry>/<hash>/`; returns the manifest's
# `versions` with this version added. Sealing again a sealed version is a
# no-op.
.store_seal_version <- function(path, mf, payload) {
  h <- as.character(mf$hash %||% "")[1]
  versions <- mf$versions %||% list()
  vdir <- fp(path, h)
  if (!nzchar(h)) stop("The entry at '", path, "' has no hash to seal.", call. = FALSE)
  # nothing to move: already a sealed version, or a manifest-only entry
  if (!file.exists(fp(path, payload))) return(versions)
  # the same content sealed before: the version is there, the copy goes
  if (file.exists(fp(vdir, payload))) {
    unlink(fp(path, setdiff(.store_payload_items(path, mf), h)), recursive = TRUE)
    return(versions)
  }
  items <- setdiff(.store_payload_items(path, mf), h)
  dir.create(vdir, showWarnings = FALSE)
  for (it in items) {
    if (!file.rename(fp(path, it), fp(vdir, it))) {
      file.copy(fp(path, it), vdir, recursive = TRUE)
      unlink(fp(path, it), recursive = TRUE)
    }
  }
  versions[[h]] <- list(sealed_at = .registry_now(),
                        created = mf$updated %||% mf$created %||% .registry_now())
  versions
}

# The folder holding the payload to load: the sealed version a hash names,
# else the root payload (when it is the requested hash or none is asked
# for), else the current sealed version.
.store_payload_dir <- function(path, mf, payload, hash = NULL) {
  if (is.null(mf)) return(if (file.exists(fp(path, payload))) path else NULL)
  vers <- as.character(names(mf$versions %||% list()))
  hash <- if (is.null(hash)) NULL else as.character(hash)[1]
  pinned <- !is.null(hash) && !is.na(hash) && nzchar(hash)
  if (pinned) {
    hit <- vers[startsWith(vers, hash)]
    if (length(hit) == 1L && file.exists(fp(path, hit, payload))) return(fp(path, hit))
  }
  cur <- as.character(mf$hash %||% "")[1]
  if (file.exists(fp(path, payload)) && (!pinned || startsWith(cur, hash))) return(path)
  if (!pinned && nzchar(cur) && file.exists(fp(path, cur, payload))) return(fp(path, cur))
  NULL
}

# --- lazy hashing -----------------------------------------------------------
# An unsealed save records a save id (`u<time><random>`, `hashed: false` in
# the manifest) instead of a content hash: hashing a large model takes
# minutes and a full copy in memory. The content hash is computed only when a
# decision needs it: saving onto a sealed entry, comparing with the sealed
# versions of a name (reference or copy), and sealing.
# options(energyRt.lazy_hash = FALSE) hashes every save.
.lazy_hash <- function() isTRUE(getOption("energyRt.lazy_hash", TRUE))

.save_id <- function() {
  paste0("u", format(Sys.time(), "%Y%m%d%H%M%S"),
         paste(sprintf("%04x", sample.int(65536L, 2L) - 1L), collapse = ""))
}

# Does the store hold sealed content under this name (a sealed entry or
# sealed versions)? Only then is a content hash worth computing to compare.
.has_sealed <- function(type, name) {
  resolve <- switch(type, model = .model_store_resolve,
                    repository = .repo_store_resolve)
  path <- tryCatch(resolve(name, NULL), error = function(e) NULL)
  if (is.null(path)) return(FALSE)
  mf <- .entry_manifest(type, path)
  isTRUE(mf$sealed) || length(mf$versions %||% list()) > 0L
}

# --- save modes -------------------------------------------------------------
# `ref` (default): a component is referenced when its exact content is a
# SEALED version in its store, copied in otherwise; `full`: every component is
# copied in. Project default: options(energyRt.save_mode = "ref").
.save_mode <- function(mode = NULL) {
  mode <- mode %||% getOption("energyRt.save_mode", "ref")
  match.arg(as.character(mode)[1], c("ref", "full"))
}

# The store entry holding `hash` of `name` as a SEALED version (or a sealed
# entry whose current content it is), else NULL: only those are referenced.
.sealed_entry <- function(type, name, hash) {
  if (!nzchar(hash %||% "")) return(NULL)
  resolve <- switch(type, model = .model_store_resolve,
                    repository = .repo_store_resolve)
  path <- tryCatch(resolve(name, hash), error = function(e) NULL)
  if (is.null(path)) return(NULL)
  mf <- .entry_manifest(type, path)
  vers <- as.character(names(mf$versions %||% list()))
  if (any(startsWith(vers, hash))) return(path)
  if (isTRUE(mf$sealed) && startsWith(as.character(mf$hash %||% ""), hash)) return(path)
  NULL
}

# --- selections from sealed repositories -------------------------------------
# A sealed repository version records the hash of each of its objects. An
# object loaded from a sealed version carries its origin (`misc$origin`:
# repository and version hash; `@misc` is outside every hash). A model
# repository whose objects all come unchanged from one sealed version is
# saved as a reference to that version plus the object names, and the hash
# of the selected objects, verified at load.

# Bring a model, repository or object fully into memory: obj2mem() stops at a
# container that is itself in memory, while its repositories' objects can
# still hold their tables on disk.
.deep_mem <- function(x) {
  if (!isS4(x)) return(x)
  if (!isInMemory(x)) x <- obj2mem(x, verbose = FALSE)
  if (.hasSlot(x, "data") && is.list(x@data)) {
    for (k in seq_along(x@data)) {
      if (isS4(x@data[[k]])) x@data[[k]] <- .deep_mem(x@data[[k]])
    }
  }
  x
}

.repository_object_hashes <- function(repo) {
  if (!length(repo@data)) return(character())
  repo <- .deep_mem(repo)
  vapply(repo@data, function(o) rlang::hash(.strip_volatile(o)), character(1))
}

.selection_hash <- function(hashes) {
  rlang::hash(as.list(hashes[order(names(hashes))]))
}

# The selection reference for a model repository, or NULL when its objects
# are not all unchanged objects of one sealed repository version.
.selection_of <- function(repo) {
  objs <- repo@data
  if (!length(objs)) return(NULL)
  org <- lapply(objs, function(o) if (isS4(o) && .hasSlot(o, "misc")) o@misc$origin)
  if (any(vapply(org, is.null, logical(1)))) return(NULL)
  key <- unique(vapply(org, function(x) paste(x$repository, x$hash, sep = "@"), ""))
  if (length(key) != 1L) return(NULL)
  from <- org[[1]]
  path <- .sealed_entry("repository", from$repository, from$hash)
  if (is.null(path)) return(NULL)
  mf <- .entry_manifest("repository", path)
  rec <- unlist(mf$versions[[from$hash]]$objects %||% list())
  if (!length(rec) || !all(names(objs) %in% names(rec))) return(NULL)
  oh <- .repository_object_hashes(repo)
  if (!identical(unname(oh), unname(rec[names(oh)]))) return(NULL)
  list(name = from$repository, hash = from$hash, source = "ref",
       path = .registry_rel_path(path), as = repo@name,
       objects = names(oh), objects_hash = .selection_hash(oh))
}

# --- dependents of sealed versions -----------------------------------------
# A sealed version records the entries that reference it, in its `versions`
# record (`dependents`: type, name, path relative to the registry, the
# dependent's own hash): appended when a referencing model or scenario is
# saved; checked, followed transitively and pruned by list_dependents().

.abs_store_path <- function(p) {
  p <- as.character(p %||% "")[1]
  if (!nzchar(p)) return("")
  if (grepl("^([A-Za-z]:|/)", p)) return(gsub("[\\/]+", "/", p))
  gsub("[\\/]+", "/", fp(dirname(get_registry_file()), p))
}

# Record `dep` on the sealed version `hash` of an entry; nothing when `hash`
# is not a sealed version there.
.dependents_add <- function(type, entry_path, hash, dep) {
  mf_path <- fp(entry_path, .entry_kinds()[[type]]$manifest)
  mf <- tryCatch(yaml::read_yaml(mf_path), error = function(e) NULL)
  if (is.null(mf) || !nzchar(hash %||% "")) return(invisible(FALSE))
  vers <- as.character(names(mf$versions %||% list()))
  hit <- vers[startsWith(vers, hash)]
  if (length(hit) != 1L) return(invisible(FALSE))
  deps <- mf$versions[[hit]]$dependents %||% list()
  same <- vapply(deps, function(d) {
    identical(d$type, dep$type) && identical(d$path, dep$path)
  }, logical(1))
  mf$versions[[hit]]$dependents <- c(deps[!same], list(c(dep, list(at = .registry_now()))))
  yaml::write_yaml(mf, mf_path)
  invisible(TRUE)
}

# Does a recorded dependent still exist, and still reference `version` of the
# entry `name`? A sealed version of a model cannot have changed, so a
# dependent model pinned to one holds by construction.
.dependent_holds <- function(dep, name, version) {
  kind <- .entry_kinds()[[dep$type]]
  if (is.null(kind)) return(c(exists = FALSE, references = FALSE))
  p <- .abs_store_path(dep$path)
  if (!file.exists(fp(p, kind$manifest))) return(c(exists = FALSE, references = FALSE))
  mf <- tryCatch(yaml::read_yaml(fp(p, kind$manifest)), error = function(e) NULL)
  if (is.null(mf)) return(c(exists = FALSE, references = FALSE))
  dh <- as.character(dep$hash %||% "")
  if (identical(dep$type, "model") && nzchar(dh) &&
      !startsWith(as.character(mf$hash %||% ""), dh) &&
      any(startsWith(as.character(names(mf$versions %||% list())), dh))) {
    return(c(exists = TRUE, references = TRUE))
  }
  refs <- switch(dep$type, scenario = list(mf$model),
                 model = mf$repositories %||% list(), list())
  ok <- any(vapply(refs, function(e) {
    is.list(e) && identical(as.character(e$source %||% ""), "ref") &&
      identical(as.character(e$name %||% ""), name) &&
      nzchar(e$hash %||% "") && startsWith(version, as.character(e$hash))
  }, logical(1)))
  c(exists = TRUE, references = ok)
}

.radius_cols <- function() {
  tibble(level = integer(0), of_type = character(0), of_name = character(0),
         of_version = character(0), type = character(0), name = character(0),
         path = character(0), hash = character(0), exists = logical(0),
         references = logical(0), of_current = logical(0))
}

# The recorded dependents of an entry's sealed versions, verified, followed
# through dependents that are themselves sealed versions.
.blast_radius <- function(type, entry_path, name, hashes, level = 1L,
                          seen = character()) {
  mf <- .entry_manifest(type, entry_path)
  rows <- list()
  for (h in hashes) {
    for (d in mf$versions[[h]]$dependents %||% list()) {
      hv <- .dependent_holds(d, name, h)
      dp <- .abs_store_path(d$path)
      rows[[length(rows) + 1L]] <- tibble(
        level = level, of_type = type, of_name = name, of_version = h,
        of_current = identical(h, as.character(mf$hash %||% "")),
        type = as.character(d$type), name = as.character(d$name %||% ""),
        path = dp, hash = as.character(d$hash %||% ""),
        exists = unname(hv["exists"]), references = unname(hv["references"]))
      key <- paste(d$type, dp, d$hash %||% "", sep = "|")
      if (all(hv) && !key %in% seen && d$type %in% c("model", "repository")) {
        seen <- c(seen, key)
        dmf <- .entry_manifest(d$type, dp)
        dv <- as.character(names(dmf$versions %||% list()))
        dh <- as.character(d$hash %||% "")
        dv <- if (nzchar(dh)) dv[startsWith(dv, dh)] else character()
        if (length(dv)) {
          rows[[length(rows) + 1L]] <- .blast_radius(d$type, dp, d$name, dv,
                                                      level + 1L, seen)
        }
      }
    }
  }
  if (length(rows)) bind_rows(rows) else .radius_cols()
}

# --- versions: status, listing, deletion marks ---------------------------------

.parse_store_time <- function(x) {
  x <- as.character(x %||% NA_character_)[1]
  if (is.na(x) || !nzchar(x)) return(as.POSIXct(NA, tz = "UTC"))
  as.POSIXct(x, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
}

.dir_size_mb <- function(paths) {
  ff <- unlist(lapply(paths, function(p) {
    if (dir.exists(p)) list.files(p, recursive = TRUE, full.names = TRUE, all.files = TRUE)
    else if (file.exists(p)) p else character()
  }))
  if (!length(ff)) return(0)
  round(sum(file.info(ff)$size, na.rm = TRUE) / 2^20, 2)
}

.versions_cols <- function() {
  tibble(status = character(0), name = character(0), type = character(0),
         version = character(0), sealed_at = as.POSIXct(character(0), tz = "UTC"),
         refs = integer(0), size_mb = numeric(0),
         updated = as.POSIXct(character(0), tz = "UTC"), importance = numeric(0),
         dependents = character(0), hash = character(0), path = character(0))
}

# One row per version of one entry: the current unsealed payload, if any, and
# every sealed version, with its status and verified dependents. Status:
# `current` (the name's content), `in use` (superseded, referenced),
# `marked` (superseded, marked for deletion), `unused` (superseded, no
# references, not marked).
.entry_versions <- function(type, path) {
  mf <- .entry_manifest(type, path)
  if (is.null(mf)) return(.versions_cols())
  nm <- as.character(mf$name %||% basename(path))
  cur <- as.character(mf$hash %||% "")
  payload <- c(model = "mod.RData", repository = "repo.RData")[[type]]
  rows <- list()
  if (file.exists(fp(path, payload))) {
    rows[[1]] <- tibble(
      status = "current", name = nm, type = type,
      version = if (isFALSE(mf$hashed)) "(unsealed)" else substr(cur, 1, 8),
      sealed_at = as.POSIXct(NA, tz = "UTC"), refs = 0L,
      size_mb = .dir_size_mb(fp(path, .store_payload_items(path, mf))),
      updated = .parse_store_time(mf$updated), importance = NA_real_,
      dependents = "", hash = cur, path = path)
  }
  for (h in names(mf$versions %||% list())) {
    v <- mf$versions[[h]]
    deps <- Filter(function(d) all(.dependent_holds(d, nm, h)), v$dependents %||% list())
    status <- if (identical(h, cur)) "current" else if (length(deps)) "in use" else
      if (isTRUE(v$marked_delete)) "marked" else "unused"
    rows[[length(rows) + 1L]] <- tibble(
      status = status, name = nm, type = type, version = substr(h, 1, 8),
      sealed_at = .parse_store_time(v$sealed_at), refs = length(deps),
      size_mb = .dir_size_mb(fp(path, h)),
      updated = .parse_store_time(v$created),
      importance = if (isTRUE(v$marked_delete)) as.numeric(v$delete_importance %||% 0) else NA_real_,
      dependents = paste(vapply(deps, function(d) paste(d$type, d$name), ""),
                         collapse = "; "),
      hash = h, path = fp(path, h))
  }
  if (length(rows)) bind_rows(rows) else .versions_cols()
}

#' Versions held in the model and repository stores
#'
#' @description
#' One row per version: the current content of an entry and each sealed
#' version, with its status, when it was sealed, how many entries reference
#' it and its size.
#'
#' Status: `current` — the name's current content (an unsealed payload or
#' the current sealed version); `in use` — a superseded sealed version that
#' models or scenarios still reference; `marked` — a superseded version
#' marked for deletion ([mark_delete()] with `version =`); `unused` — a
#' superseded version nothing references and nobody marked. Versions are
#' deleted only when marked, by [delete_marked()].
#'
#' @param x optional: a model or repository, or its stored name. All entries
#'   of `type` when `NULL`.
#' @param type `"model"`, `"repository"`, or both (default).
#' @param details logical; `TRUE` adds `updated`, `importance`, the
#'   dependents' names, the full `hash` and the `path`.
#' @return a tibble: `status`, `name`, `type`, `version` (8 characters of the
#'   content hash; `(unsealed)` for a payload saved under a save id),
#'   `sealed_at`, `refs`, `size_mb`, and with `details` the columns above.
#' @seealso [list_dependents()], [mark_delete()], [delete_marked()]
#' @examples
#' \dontrun{
#' list_versions()                       # every model and repository
#' list_versions("TOPIA", type = "model", details = TRUE)
#' }
#' @export
list_versions <- function(x = NULL, type = c("model", "repository"), details = FALSE) {
  out <- if (!is.null(x)) {
    tp <- .entry_type_of(x, if (length(type) == 1L) type else NULL)
    .entry_versions(tp, .entry_resolve(tp, x)$path)
  } else {
    type <- match.arg(type, c("model", "repository"), several.ok = TRUE)
    rows <- list()
    for (tp in type) {
      root <- .entry_kinds()[[tp]]$root()
      if (!dir.exists(root)) next
      for (d in list.dirs(root, recursive = FALSE)) {
        if (!file.exists(fp(d, .entry_kinds()[[tp]]$manifest))) next
        rows[[length(rows) + 1L]] <- .entry_versions(tp, gsub("[\\/]+", "/", d))
      }
    }
    if (length(rows)) bind_rows(rows) else .versions_cols()
  }
  if (isTRUE(details)) out else
    out[, c("status", "name", "type", "version", "sealed_at", "refs", "size_mb")]
}

# verified dependents of any sealed version of an entry (delete guard)
.entry_version_dependents <- function(type, path) {
  if (!type %in% c("model", "repository")) return(0L)
  sum(.entry_versions(type, path)$refs)
}

# Set or clear the deletion mark of sealed versions of an entry. `version`:
# a hash (or prefix), several, or "unused" for every unused version.
.mark_versions <- function(type, path, name, version, fields, verbose, what) {
  if (!type %in% c("model", "repository")) {
    stop("Versions exist for models and repositories only.", call. = FALSE)
  }
  v <- .entry_versions(type, path)
  v <- v[!is.na(v$sealed_at) & v$status != "current", , drop = FALSE]
  hs <- if (identical(version, "unused")) {
    v$hash[v$status %in% c("unused", "marked")]
  } else {
    unique(unlist(lapply(version, function(p) v$hash[startsWith(v$hash, p)])))
  }
  if (!length(hs) && !identical(version, "unused")) {
    stop(type, " '", name, "' has no superseded sealed version ",
         paste(version, collapse = ", "), "; list_versions() lists them.",
         call. = FALSE)
  }
  mf_path <- fp(path, .entry_kinds()[[type]]$manifest)
  mf <- yaml::read_yaml(mf_path)
  for (h in hs) mf$versions[[h]][names(fields)] <- fields
  yaml::write_yaml(mf, mf_path)
  if (isTRUE(verbose)) {
    message(type, " '", name, "': ", length(hs), " version", if (length(hs) == 1L) "" else "s",
            " ", what, if (length(hs)) paste0(" (", paste(substr(hs, 1, 8), collapse = ", "), ")"))
  }
  invisible(hs)
}

#' What depends on a stored model, repository or dataset
#'
#' @description
#' Lists the store entries that *reference* `x` rather than embedding a copy:
#' those that would break if it were deleted. For models and repositories
#' the dependents recorded on their sealed versions are verified (does the
#' dependent still exist, does it still reference that version) and, with
#' `recursive = TRUE`, followed through models to the scenarios using them;
#' references found in the manifests are added (the only record for
#' datasets, and for references saved before versions recorded their
#' dependents). This is the check [unseal_model()] / [unseal_repository()]
#' make before releasing a name.
#'
#' @param x an energyRt object, or the name of a store entry.
#' @param type the entry type when `x` is a bare name: `"model"`,
#'   `"repository"`, `"dataset"` or `"scenario"`.
#' @param version optional hash (or prefix) of one version.
#' @param recursive logical; follow dependents that are themselves sealed
#'   versions (repository -> models -> scenarios).
#' @param prune logical; remove the records of `x`'s own versions whose
#'   dependent no longer exists or no longer references them.
#' @param details logical; `TRUE` returns every column and also the stale
#'   records (`exists` / `references` FALSE).
#' @return a tibble: `level` (1 = references `x` directly), the dependent's
#'   `type`, `name` and `path`, the `version` it references and whether that
#'   is `x`'s `current` one; with `details` also `of_type`, `of_name`,
#'   `hash` (the dependent's own), `exists`, `references` and `via`.
#' @seealso [list_versions()], [delete_marked()], [mark_delete()]
#' @examples
#' \dontrun{
#' list_dependents("TOPIA", type = "model")            # which scenarios use it
#' list_dependents("my_repo", type = "repository", recursive = TRUE)
#' }
#' @export
list_dependents <- function(x, type = NULL, version = NULL, recursive = FALSE,
                            prune = FALSE, details = FALSE) {
  type <- .entry_type_of(x, type)
  e <- .entry_resolve(type, x)
  mf <- .entry_manifest(type, e$path)
  cur <- as.character(mf$hash %||% "")
  rec <- .radius_cols()
  if (type %in% c("model", "repository")) {
    vers <- as.character(names(mf$versions %||% list()))
    if (!is.null(version)) vers <- vers[startsWith(vers, version)]
    rec <- .blast_radius(type, e$path, e$name, vers)
    if (!isTRUE(recursive)) rec <- rec[rec$level == 1L, , drop = FALSE]
    stale <- rec$level == 1L & !(rec$exists & rec$references)
    if (isTRUE(prune) && any(stale)) {
      for (h in unique(rec$of_version[stale])) {
        drop <- rec$path[stale & rec$of_version == h]
        mf$versions[[h]]$dependents <- Filter(function(d) {
          !(.abs_store_path(d$path) %in% drop)
        }, mf$versions[[h]]$dependents %||% list())
      }
      yaml::write_yaml(mf, fp(e$path, .entry_kinds()[[type]]$manifest))
      rec <- rec[!stale, , drop = FALSE]
    }
  }
  rec$via <- rec$of_type
  # references found in the manifests, not already recorded
  scan <- .dep_of(.store_dep_index(), type, e$name, cur)
  if (!is.null(version)) scan <- scan[startsWith(scan$hash, version), , drop = FALSE]
  scan <- scan[!scan$path %in% rec$path[rec$level == 1L], , drop = FALSE]
  if (nrow(scan)) {
    rec <- bind_rows(rec, tibble(
      level = 1L, of_type = type, of_name = e$name, of_version = scan$hash,
      type = scan$type, name = scan$name, path = scan$path, hash = "",
      exists = TRUE, references = TRUE, via = scan$via,
      of_current = nzchar(scan$hash) & startsWith(cur, scan$hash)))
  }
  rec$version <- substr(rec$of_version, 1, 8)
  # is the referenced version the current one of the entry it belongs to
  rec$current <- rec$of_current
  rec$of_current <- NULL
  if (isTRUE(details)) return(rec)
  rec <- rec[rec$exists & rec$references, , drop = FALSE]
  rec[, c("level", "type", "name", "version", "current", "path")]
}

# unseal: list what depends on the current sealed version, refuse unless forced
.unseal <- function(type, x, force, verbose) {
  e <- .entry_resolve(type, x)
  mf <- .entry_manifest(type, e$path)
  cur <- as.character(mf$hash %||% "")
  r <- if (cur %in% names(mf$versions %||% list())) {
    .blast_radius(type, e$path, e$name, cur)
  } else {
    .radius_cols()
  }
  r <- r[r$exists & r$references, , drop = FALSE]
  if (nrow(r)) {
    lines <- sprintf("  %s%s '%s' (%s)", strrep("  ", r$level - 1L), r$type,
                     r$name, r$path)
    msg <- paste0(type, " '", e$name, "' @", substr(cur, 1, 8),
                  " is referenced by ", nrow(r), " entr",
                  if (nrow(r) == 1L) "y" else "ies", ":\n",
                  paste(lines, collapse = "\n"))
    if (!isTRUE(force)) {
      stop(msg, "\nunseal_", type, "(force = TRUE) releases the name; the ",
           "sealed version stays for them.", call. = FALSE)
    }
    if (isTRUE(verbose)) message(msg)
  }
  .seal_set(type, x, FALSE, verbose)
  invisible(r)
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
    d <- unique(fp(dirname(get_registry_file()), hit$path))
    d <- d[file.exists(fp(d, manifest))]
    # Return only on a unique hit. Registry rows are keyed on path, so a name
    # with two version folders (`<slug>/` and `<slug>@<hash8>/`) yields two
    # rows, and `d[1]` would pick one silently. Falling through reaches the
    # store scan, which already refuses an ambiguous name and names the
    # versions.
    if (length(d) == 1L) return(gsub("[\\/]+", "/", d))
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
    keep <- vapply(dd, function(d) {
      mf <- tryCatch(yaml::read_yaml(fp(d, manifest)),
                     error = function(e) NULL)
      hs <- c(mf$hash %||% "", names(mf$versions %||% list()))
      any(startsWith(hs, hash))
    }, logical(1))
    if (length(dd) && !any(keep)) {
      mf_hash <- vapply(dd, function(d) {
        tryCatch(as.character(yaml::read_yaml(fp(d, manifest))$hash %||% "")[1],
                 error = function(e) "")
      }, character(1))
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
  pdir <- if (!is.null(path)) {
    .store_payload_dir(path, .entry_manifest("model", path), "mod.RData", hash)
  }
  if (is.null(pdir)) {
    stop("Model '", if (is.null(hash)) name else paste0(name, "@", hash),
         "' was not found in the registry or the model store ('",
         get_models_path(), "').\n",
         "  Run refresh_registry() to rescan, save_model() to store it, ",
         "or pass path= to a model directory.")
  }
  .mark_notice("model", path, name)
  e <- new.env(parent = emptyenv())
  nm <- load(fp(pdir, "mod.RData"), envir = e)
  if (length(nm) != 1L || !is(get(nm, envir = e), "model")) {
    stop("'", fp(pdir, "mod.RData"), "' must contain exactly one model object")
  }
  mod <- .model_rebase(get(nm, envir = e), pdir)
  # loaded from a sealed version: its identity is that version's hash (the
  # stored copy may carry the save id of the save before sealing)
  if (!identical(gsub("[\\/]+", "/", pdir), gsub("[\\/]+", "/", path))) {
    mod@misc$hash <- basename(pdir)
  }

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
      if (!is.null(.store_payload_dir(cand, .entry_manifest("repository", cand),
                                      "repo.RData", ref$hash))) {
        r <- tryCatch(load_repository(ref$name, hash = ref$hash, path = cand,
                                      verbose = FALSE),
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
    if (!is.null(ref$objects)) {
      objs <- unlist(ref$objects)
      miss <- setdiff(objs, names(r@data))
      if (length(miss)) {
        stop("Model '", mod@name, "': repository '", ref$name, "@",
             substr(ref$hash, 1, 8), "' lacks the selected object(s) ",
             paste(miss, collapse = ", "), ".", call. = FALSE)
      }
      r@data <- r@data[objs]
      od <- r@misc$onDisk$data
      if (!is.null(od)) r@misc$onDisk$data <- od[intersect(names(od), objs)]
      if (!identical(.selection_hash(.repository_object_hashes(r)), ref$objects_hash)) {
        stop("Model '", mod@name, "': the objects selected from repository '",
             ref$name, "@", substr(ref$hash, 1, 8), "' do not match their ",
             "recorded hash.", call. = FALSE)
      }
      r@name <- ref$as %||% r@name
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


# ---------------------------------------------------------------------------
# (was R/repo_store.R, merged 2026-09-20)
# ---------------------------------------------------------------------------

# =============================================================================#
# repo_store.R — content-addressed repository store (layout 3, stage S4c).
#
# `repositories/<name>/` holds one saved repository (hash in the manifest):
# `repository.yml`, the thinned `repo.RData`, and parquet stores for the big
# data slots — the model store's pattern (`.model_store_resolve()` above) one
# level down.
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
#' @examples
#' repo <- newRepository("r", newCommodity("ELC", unit = "MWh"))
#' repository_hash(repo)
#'
#' # the hash follows the content: adding an object changes it
#' repository_hash(add(repo, newCommodity("COA", unit = "MMBtu")))
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
#' @examples
#' \dontrun{
#' open_project(tempdir())
#'
#' repo <- newRepository("my_repo", newCommodity("ELC", unit = "MWh"))
#' save_repository(repo)
#'
#' load_repository("my_repo")
#' }
save_repository <- function(
    repo,
    path = NULL,
    embed_datasets = NULL,
    registry = TRUE,
    format = get_storage_format(),
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
  repo <- .deep_mem(repo)
  if (is.null(path)) {
    path <- .store_entry_dir(get_repositories_path(), repo@name, "",
                             type = "repository")
  }
  path <- gsub("[\\/]+", "/", path)
  mf_path <- fp(path, "repository.yml")
  prev <- if (file.exists(mf_path)) {
    tryCatch(yaml::read_yaml(mf_path), error = function(e) NULL)
  }
  # the content hash only when the entry is sealed or lazy hashing is off
  hashed <- !.lazy_hash() || isTRUE(prev$sealed)
  h <- if (hashed) repository_hash(repo) else .save_id()
  h8 <- substr(h, 1, 8)
  hash_kept <- FALSE
  if (!is.null(prev)) {
    if (!overwrite && hashed && identical(prev$hash, h)) {
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
    .store_entry_wipe(path, keep = c("reports", "levcost", names(prev$versions %||% list())))
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
    format = format,
    hashed = hashed
  ), if (length(ds_entries)) list(datasets = ds_entries),
     if (hash_kept) list(hash_kept = TRUE),
     if (length(prev$versions)) list(versions = prev$versions),
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
  pdir <- if (!is.null(path)) {
    .store_payload_dir(path, .entry_manifest("repository", path), "repo.RData", hash)
  }
  if (is.null(pdir)) {
    stop("Repository '",
         if (is.null(hash)) name else paste0(name, "@", hash),
         "' was not found in the registry or the repository store ('",
         get_repositories_path(), "').\n",
         "  Run refresh_registry() to rescan, save_repository() to store ",
         "it, or pass path= to a repository directory.")
  }
  .mark_notice("repository", path, name)
  e <- new.env(parent = emptyenv())
  nm <- load(fp(pdir, "repo.RData"), envir = e)
  if (length(nm) != 1L || !is(get(nm, envir = e), "repository")) {
    stop("'", fp(pdir, "repo.RData"),
         "' must contain exactly one repository object")
  }
  repo <- .repo_rebase(get(nm, envir = e), pdir)
  # objects of a sealed version carry their origin (see .selection_of())
  if (!identical(gsub("[\\/]+", "/", pdir), gsub("[\\/]+", "/", path))) {
    vh <- basename(pdir)
    repo@misc$hash <- vh
    for (ob in names(repo@data)) {
      o <- repo@data[[ob]]
      if (isS4(o) && .hasSlot(o, "misc")) {
        o@misc$origin <- list(repository = repo@name, hash = vh)
        repo@data[[ob]] <- o
      }
    }
  }
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


# ---------------------------------------------------------------------------
# (was R/dataset_store.R, merged 2026-09-20)
# ---------------------------------------------------------------------------

# =============================================================================#
# dataset_store.R — content-addressed dataset store (the fourth storage tier:
# datasets / repositories / models / scenarios).
#
# `datasets/<name>/` holds one saved dataset (hash in the manifest): a `dataset.yml`
# manifest and the payload — a parquet store under `data/` for tables
# (weather/demand series), or `payload.rds` for other objects (a Geoscale
# map). A dataset may instead RECORD A CALL (`fun` + `args`) that generates
# the data; by default the call is evaluated at save time and the result
# snapshot is stored alongside, so the identity stays a CONTENT hash either
# way and loading never requires the generating package.
#
# Sharing: repositories, models, and scenarios can reference a stored dataset
# instead of embedding the table into every saved version — the same
# `{name, hash}` stub protocol as repo/model refs, one level further down.
# The ref lives on the OWNING object's `@misc$dataset_ref`, keyed by slot;
# a live in-memory session never holds a stub (refs resolve eagerly on load).
# =============================================================================#

#' Content hash of a dataset payload
#'
#' @description
#' The dataset analogue of [model_hash()] / [repository_hash()]: a stable
#' content hash of the payload with volatile attributes stripped. Works for
#' data.frames (weather/demand tables), `geoscales::GeoScale` maps, and any
#' other R object.
#'
#' @param x the dataset payload.
#' @return character, the full hash (the store uses its first 8 characters).
#' @rdname dataset_store
#' @export
dataset_hash <- function(x) {
  rlang::hash(.dataset_canonical(.strip_volatile(x)))
}

# Canonical form for hashing arbitrary payloads. Two normalizations, both
# needed because serialization is sensitive to representation details that
# are not content:
# * an S7 object's `S7_class` attribute embeds the class definition
#   (functions, environments) whose serialization is NOT stable across
#   serialize/unserialize round-trips — a Geoscale re-read from rds would
#   hash differently every time; the class NAME identifies it, the
#   properties carry the content;
# * a data.frame's attribute ORDER and row-name representation differ
#   between a constructed table and one collected back from parquet.
.dataset_canonical <- function(x) {
  if (!is.null(attr(x, "S7_class"))) {
    at <- attributes(x)
    at$S7_class <- NULL
    return(lapply(at[order(names(at))], .dataset_canonical))
  }
  if (is.data.frame(x)) {
    return(list(.class = class(x), .cols = lapply(as.list(x), unname),
                .nrow = nrow(x)))
  }
  if (is.list(x) && length(x)) {
    return(lapply(x, .dataset_canonical))
  }
  x
}

# Validate/normalize the generator of a functional dataset: a namespaced
# "pkg::fun" string (functions themselves are not serializable identities).
.dataset_fun_string <- function(fun) {
  if (!is.character(fun) || length(fun) != 1L ||
      !grepl("^[A-Za-z][A-Za-z0-9.]*::[A-Za-z._][A-Za-z0-9._]*$", fun)) {
    stop("`fun` must be a namespaced function name as a string, ",
         "e.g. \"energyRt::topia_profiles\".")
  }
  fun
}

# Functional-dataset args must survive the yaml round-trip unchanged: the
# manifest is their only storage.
.dataset_args_check <- function(args) {
  if (is.null(args)) return(list())
  stopifnot(is.list(args))
  rt <- tryCatch(yaml::read_yaml(text = yaml::as.yaml(args)),
                 error = function(e) NULL)
  if (!identical(rt, args)) {
    stop("`args` must be a yaml-serializable list (names, atomic values, ",
         "nested lists); it did not survive a yaml round-trip unchanged.")
  }
  args
}

.dataset_eval <- function(fun, args) {
  fun <- .dataset_fun_string(fun)
  parts <- strsplit(fun, "::", fixed = TRUE)[[1]]
  if (!requireNamespace(parts[1], quietly = TRUE)) {
    stop("Package '", parts[1], "' is required to evaluate the dataset call ",
         fun, "() and is not installed.")
  }
  do.call(getExportedValue(parts[1], parts[2]), args %||% list())
}

# Write a payload into a store entry (tables -> data/ parquet; anything
# else -> payload.rds). Returns the manifest fields describing it.
.dataset_write_payload <- function(x, path, format) {
  if (is.data.frame(x)) {
    data2disk(x, path = fp(path, "data"), format = format)
    return(list(kind = "table",
                payload_class = as.list(class(x)),
                dim = as.list(dim(x)),
                cols = lapply(as.list(x), function(col) class(col)[1]),
                format = format))
  }
  saveRDS(x, fp(path, "payload.rds"))
  list(kind = "object", payload_class = as.list(class(x)), format = "rds")
}

# Write one version of a dataset: the parquet parts (or payload.rds) go
# directly into the version directory `<entry>/<hash>/`.
.dataset_write_version <- function(x, vdir, format) {
  if (is.data.frame(x)) {
    data2disk(x, path = vdir, format = format)
    return(list(kind = "table",
                payload_class = as.list(class(x)),
                dim = as.list(dim(x)),
                cols = lapply(as.list(x), function(col) class(col)[1]),
                format = format))
  }
  dir.create(vdir, recursive = TRUE, showWarnings = FALSE)
  saveRDS(x, fp(vdir, "payload.rds"))
  list(kind = "object", payload_class = as.list(class(x)), format = "rds")
}

# The versions an entry holds, from its manifest. A legacy entry (payload
# under `data/` or `payload.rds` beside the manifest, one version) is listed
# under its own hash with `dir = "."`-relative markers, so it reads like any
# other version and is never moved.
.dataset_versions_of <- function(path, mf) {
  v <- mf$versions %||% list()
  h <- mf$hash %||% ""
  legacy <- dir.exists(fp(path, "data")) || file.exists(fp(path, "payload.rds"))
  if (nzchar(h) && legacy && is.null(v[[h]])) {
    v[[h]] <- list(legacy = TRUE,
                   kind = mf$kind, format = mf$format, dim = mf$dim, cols = mf$cols,
                   payload_class = mf$payload_class,
                   fun = mf$fun, args = mf$args, result_hash = mf$result_hash,
                   snapshot = mf$snapshot,
                   created = mf$created, memo = mf$memo)
    v[[h]] <- v[[h]][!vapply(v[[h]], is.null, logical(1))]
  }
  v
}

# The directory holding a version's payload and that version's manifest
# fields. `hash` may be a prefix; NULL means the current version.
.dataset_version <- function(path, mf, hash = NULL) {
  v <- .dataset_versions_of(path, mf)
  h <- hash %||% mf$hash %||% ""
  key <- names(v)[startsWith(names(v), h)]
  if (length(key) > 1L) {
    stop("Dataset '", mf$name, "': the hash prefix '", h, "' matches ",
         length(key), " versions.")
  }
  if (length(key) == 1L) {
    vf <- v[[key]]
    if (isTRUE(vf$legacy)) {
      dir <- if (dir.exists(fp(path, "data"))) fp(path, "data") else path
      return(list(hash = key, dir = dir, fields = vf))
    }
    return(list(hash = key, dir = fp(path, key), fields = vf))
  }
  if (is.null(hash)) {
    # a manifest without a versions list and no hash: whatever lies beside it
    dir <- if (dir.exists(fp(path, "data"))) fp(path, "data") else path
    return(list(hash = h, dir = dir, fields = mf))
  }
  NULL
}

# Read a payload back per its manifest. Tables are coerced to the recorded
# class and column types (parquet does not round-trip e.g. integer years
# by itself). `hash` selects a version (prefix allowed); NULL is the current.
.dataset_read_payload <- function(path, mf, hash = NULL) {
  ver <- .dataset_version(path, mf, hash)
  if (is.null(ver)) {
    stop("Dataset '", mf$name, "' has no version '", hash, "'.")
  }
  vf <- ver$fields
  vdir <- ver$dir
  if (identical(vf$kind, "table") ||
      (identical(vf$kind, "function") && !file.exists(fp(vdir, "payload.rds")) &&
       length(list.files(vdir, pattern = "[.](parquet|feather|arrow)$")))) {
    d <- en_open_dataset(vdir) |> dplyr::collect()
    d <- as.data.frame(d)
    for (cn in intersect(names(vf$cols %||% list()), names(d))) {
      cl <- vf$cols[[cn]]
      cur <- class(d[[cn]])[1]
      if (!identical(cur, cl)) {
        d[[cn]] <- switch(cl,
          integer = as.integer(d[[cn]]),
          numeric = as.numeric(d[[cn]]),
          character = as.character(d[[cn]]),
          logical = as.logical(d[[cn]]),
          factor = as.factor(d[[cn]]),
          d[[cn]])
      }
    }
    cls <- unlist(vf$payload_class %||% list())
    if ("data.table" %in% cls) d <- data.table::as.data.table(d)
    # normalize the attribute order to R's construction order — serialization
    # (and thus repository_hash/model_hash of an owner holding this table)
    # is attribute-order sensitive, and arrow's collect() orders differently
    at <- attributes(d)
    ord <- intersect(c("names", "row.names", "class"), names(at))
    attributes(d) <- at[c(ord, setdiff(names(at), ord))]
    return(d)
  }
  if (file.exists(fp(vdir, "payload.rds"))) {
    return(readRDS(fp(vdir, "payload.rds")))
  }
  NULL
}

#' Save a dataset to / load a dataset from the dataset store
#'
#' @description
#' `save_dataset()` writes a payload — a data.frame (a weather or demand
#' series), a `geoscales::GeoScale` map, or any R object — into the
#' store `<datasets_path>/<name>/` (see [get_datasets_path()]). An entry
#' holds its versions by content: the payload of each version lives under
#' `<name>/<hash>/` and the entry's `dataset.yml` names the current version
#' and lists every version present. Saving identical content again is a
#' no-op; saving other content under the same name adds a version and
#' makes it current, nothing is overwritten; saving content already stored
#' as an older version makes that version current again. `load_dataset()`
#' takes the current version by name, or a pinned one by `hash` (full or a
#' prefix, also as `name@hash`). An entry written before versions, with its
#' payload beside the manifest, reads as its single version. A
#' stored dataset can then be REFERENCED by repositories, models, and
#' scenarios instead of embedded — see the `embed_datasets` argument of
#' [save_repository()], [save_model()] and [save_scenario()]: the table is
#' stored once, however many model or scenario versions use it.
#'
#' A FUNCTIONAL dataset records a generating call instead of typed-in data:
#' pass `fun = "pkg::fun"` and `args = list(...)` (no `x`). By default
#' (`materialize = TRUE`) the call is evaluated once at save time and the
#' result is stored as a snapshot: the dataset's identity is the CONTENT
#' hash of that result, loading uses the snapshot without the generating
#' package, and re-evaluation (`load_dataset(evaluate = TRUE)` on a
#' functional entry) can verify the recorded `result_hash` — a mismatch
#' warns (package drift is worth seeing), or errors with `strict = TRUE`.
#' `materialize = FALSE` stores no snapshot and hashes the normalized call
#' (`hash_of: call` in the manifest); loading then requires the package.
#'
#' `load_dataset()` resolves by `name` (optionally `"name@hash8"`, or with
#' `hash =`) via the registry first, then by scanning the store; `path`
#' bypasses resolution. It returns the bare payload.
#'
#' @param hash character, full or short content hash to pin a version; for
#'   `save_dataset()`, the content hash of `x` when the caller has computed
#'   it already (`dataset_hash()`), which spares a second pass.
#' @param name character, dataset name, `"name@hash8"`, or a store directory
#'   path.
#' @param fun character, a namespaced generator `"pkg::fun"` (functional
#'   datasets only).
#' @param args list, yaml-serializable arguments of `fun`.
#' @param materialize logical, evaluate `fun` at save time and store the
#'   result snapshot (default). `FALSE` records the call only.
#' @param hash character, full or short content hash to pin a version.
#' @param path character, explicit store directory.
#' @param registry logical, add/refresh the registry row on save.
#' @param format storage format for tables (as in [save_scenario()]).
#' @param overwrite logical, rewrite the store entry even when the hash
#'   matches.
#' @param rehash logical; `FALSE` keeps the DESTINATION entry's recorded
#'   hash while writing the changed content — for changes the user declares
#'   insignificant (a description, a memo). The manifest marks the
#'   exception (`hash_kept: true`); references then verify against the
#'   retained hash. With `rehash = FALSE` the hash is a user-managed
#'   version tag, not a content proof.
#' @param evaluate logical, for a functional entry: re-evaluate the recorded
#'   call (verifying `result_hash`) instead of only reading the snapshot.
#'   Falls back to the snapshot with a message when the package is missing.
#' @param strict logical, error (instead of warn) when a re-evaluated
#'   functional dataset no longer matches its recorded `result_hash`.
#' @param memo character, optional registry note.
#' @param verbose logical.
#'
#' @return `save_dataset()` returns `list(name, hash, kind, path)`,
#'   invisibly. `load_dataset()` returns the payload.
#'
#' @rdname dataset_store
#' @export
#' @examples
#' \dontrun{
#' open_project(tempdir())
#'
#' # a table a model refers to, stored once and referenced by hash
#' save_dataset(topia$weather, "topia_weather")
#' dataset_hash(topia$weather)
#' load_dataset("topia_weather")
#'
#' # or store the CALL that makes it -- `fun` is a namespaced name, and the
#' # payload is left out entirely -- and materialize it on demand
#' save_dataset(name = "profiles", fun = "energyRt::topia_profiles",
#'              args = list(regions = c("W1", "W2")), materialize = FALSE)
#' load_dataset("profiles", evaluate = TRUE)
#' }
save_dataset <- function(
    x,
    name,
    fun = NULL,
    args = NULL,
    materialize = TRUE,
    path = NULL,
    registry = TRUE,
    format = get_storage_format(),
    overwrite = FALSE,
    rehash = TRUE,
    memo = "",
    hash = NULL,
    verbose = TRUE) {
  stopifnot(is.character(name), length(name) == 1L, nzchar(name))
  # "arrow" and "ipc" are aliases for feather, as in save_scenario(). These
  # stores used to pin parquet here for hash stability; they no longer do.
  # dataset_hash()/model_hash() run .dataset_canonical() over the payload,
  # which reduces a data.frame to class + column values + nrow, so the on-disk
  # container cannot reach the hash: a feather and a parquet round-trip of the
  # same frame were measured to give the identical digest. Reads sniff the
  # extension, so stores already written as parquet keep loading.
  format <- if (tolower(format) %in% c("arrow", "ipc")) {
    "feather"
  } else {
    tolower(format)
  }

  fun_fields <- list()
  if (!is.null(fun)) {
    if (!missing(x)) {
      stop("Pass either `x` (a payload) or `fun` + `args` (a functional ",
           "dataset), not both.")
    }
    fun <- .dataset_fun_string(fun)
    args <- .dataset_args_check(args)
    if (isTRUE(materialize)) {
      x <- .dataset_eval(fun, args)
      h <- dataset_hash(x)
      fun_fields <- list(fun = fun, args = args, result_hash = h,
                         snapshot = TRUE)
    } else {
      h <- rlang::hash(list(fun = fun, args = args))
      fun_fields <- list(fun = fun, args = args, snapshot = FALSE)
    }
  } else {
    if (missing(x)) stop("Nothing to save: pass `x`, or `fun` + `args`.")
    # a caller that hashed the payload already passes the hash in
    h <- hash %||% dataset_hash(x)
  }
  h8 <- substr(h, 1, 8)
  if (is.null(path)) {
    path <- .store_entry_dir(get_datasets_path(), name, h, type = "dataset")
  }
  path <- gsub("[\\/]+", "/", path)

  # One entry per name, its versions by content beneath it: the manifest
  # names the current version and lists every version present; saving
  # different content under a name adds a version and moves the current
  # marker, nothing is overwritten; a version already present (current or
  # not) is made current without a write.
  mf_path <- fp(path, "dataset.yml")
  prev <- NULL
  hash_kept <- FALSE
  versions <- list()
  if (file.exists(mf_path)) {
    prev <- tryCatch(yaml::read_yaml(mf_path), error = function(e) NULL)
    versions <- .dataset_versions_of(path, prev %||% list())
    if (!overwrite && !is.null(prev) && identical(prev$hash, h)) {
      if (verbose) {
        message("Dataset '", name, "' (", h8,
                ") is already in the store: ", path)
      }
      return(invisible(list(name = name, hash = h, kind = prev$kind,
                            path = path)))
    }
    .seal_guard(prev, "dataset", name, "unseal_dataset")
    if (!overwrite && !is.null(versions[[h]])) {
      vf <- versions[[h]]
      yaml::write_yaml(c(
        prev[setdiff(names(prev), c("hash", "updated", "versions",
                                    "kind", "format", "dim", "cols", "payload_class",
                                    "fun", "args", "result_hash", "snapshot"))],
        list(hash = h), vf[setdiff(names(vf), c("legacy", "created", "memo"))],
        list(updated = .registry_now(), versions = versions)), mf_path)
      if (verbose) {
        message("Dataset '", name, "' (", h8, ") is a stored version: made current.")
      }
      kind <- vf$kind
      if (isTRUE(registry)) {
        tryCatch({
          reg <- .registry_open()
          reg <- add_to_registry(reg, "dataset", name,
                                 path = .registry_rel_path(path), hash = h, memo = memo)
          save_registry(reg)
        }, error = function(e) NULL)
      }
      return(invisible(list(name = name, hash = h, kind = kind, path = path)))
    }
    if (!isTRUE(rehash) && nzchar(prev$hash %||% "")) {
      # the content changed but the hash is kept: that version is rewritten
      h <- prev$hash
      h8 <- substr(h, 1, 8)
      hash_kept <- TRUE
      if (isTRUE(versions[[h]]$legacy)) {
        unlink(fp(path, c("data", "payload.rds")), recursive = TRUE, force = TRUE)
      } else {
        unlink(fp(path, h), recursive = TRUE, force = TRUE)
      }
      versions[[h]] <- NULL
    }
  }

  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  payload_fields <- if (!is.null(fun) && !isTRUE(materialize)) {
    list(kind = "function", format = "none")
  } else {
    pf <- .dataset_write_version(x, fp(path, h), format)
    if (!is.null(fun)) pf$kind <- "function"
    pf
  }
  kind <- payload_fields$kind
  versions[[h]] <- c(payload_fields, fun_fields,
                     list(created = .registry_now(), memo = memo))

  write("dataset", fp(path, "class"), append = FALSE)
  write(as.character(.SCENARIO_LAYOUT), fp(path, "layout"), append = FALSE)
  yaml::write_yaml(c(
    list(layout = .SCENARIO_LAYOUT,
         class = "dataset",
         name = name,
         hash = h,
         hash_of = if (!is.null(fun) && !isTRUE(materialize)) "call" else
           "content"),
    payload_fields,
    fun_fields,
    list(created = prev$created %||% .registry_now(),
         updated = .registry_now(),
         energyRt_version = as.character(utils::packageVersion("energyRt")),
         memo = memo,
         versions = versions),
    if (hash_kept) list(hash_kept = TRUE),
    .lifecycle_carry(prev)
  ), mf_path)
  if (verbose) {
    cat("Dataset '", name, "' (", h8, ", ", kind, ") saved in '", path,
        "'\n", sep = "")
  }

  if (isTRUE(registry)) {
    tryCatch({
      reg <- .registry_open()
      reg <- add_to_registry(reg, "dataset", name,
                             path = .registry_rel_path(path), hash = h,
                             memo = memo)
      save_registry(reg)
    }, error = function(e) {
      warning("Could not update the project registry (",
              conditionMessage(e), ")", call. = FALSE)
    })
  }
  invisible(list(name = name, hash = h, kind = kind, path = path))
}

.dataset_store_resolve <- function(name, hash = NULL) {
  .store_resolve(name, hash, reg_type = "dataset",
                 root = get_datasets_path(),
                 manifest = "dataset.yml")
}

# Find a store entry holding EXACTLY this content, whatever its name —
# the auto mode of `embed_datasets` refs by content, not by name.
.dataset_store_find_hash <- function(hash) {
  hit <- tryCatch({
    reg <- .registry_open()
    r <- find_in_registry(reg, type = "dataset", hash = hash)
    if (nrow(r)) {
      p <- fp(dirname(get_registry_file()), r$path[1])
      if (file.exists(fp(p, "dataset.yml"))) {
        list(name = r$name[1], path = gsub("[\\/]+", "/", p))
      } else NULL
    } else NULL
  }, error = function(e) NULL)
  if (!is.null(hit)) return(hit)
  root <- get_datasets_path()
  if (!dir.exists(root)) return(NULL)
  for (d in list.dirs(root, recursive = FALSE)) {
    mf <- tryCatch(yaml::read_yaml(fp(d, "dataset.yml")),
                   error = function(e) NULL)
    if (!is.null(mf) &&
        (identical(mf$hash, hash) || hash %in% names(mf$versions %||% list()))) {
      return(list(name = mf$name, path = gsub("[\\/]+", "/", d)))
    }
  }
  NULL
}

# The payload of a resolved store entry. Reference resolution prefers the
# snapshot: the owner's hash was computed over materialized content, and a
# re-evaluation that drifted would silently break that identity.
.dataset_payload <- function(path, mf, prefer_snapshot = TRUE,
                             strict = FALSE, verbose = TRUE, hash = NULL) {
  # a version other than the current one is read as stored
  if (!is.null(hash) && !startsWith(mf$hash %||% "", hash)) {
    return(.dataset_read_payload(path, mf, hash))
  }
  if (identical(mf$kind, "function")) {
    has_snapshot <- isTRUE(mf$snapshot)
    if (isTRUE(prefer_snapshot) && has_snapshot) {
      return(.dataset_read_payload(path, mf))
    }
    parts <- strsplit(mf$fun, "::", fixed = TRUE)[[1]]
    if (!requireNamespace(parts[1], quietly = TRUE)) {
      if (has_snapshot) {
        if (isTRUE(verbose)) {
          message("Package '", parts[1], "' is not installed; using the ",
                  "stored snapshot of dataset '", mf$name, "'.")
        }
        return(.dataset_read_payload(path, mf))
      }
      stop("Dataset '", mf$name, "' records the call ", mf$fun,
           "() with no snapshot, and package '", parts[1],
           "' is not installed.")
    }
    res <- .dataset_eval(mf$fun, mf$args)
    if (!is.null(mf$result_hash)) {
      rh <- dataset_hash(res)
      if (!identical(rh, mf$result_hash)) {
        msg <- paste0("Dataset '", mf$name, "': re-evaluating ", mf$fun,
                      "() no longer reproduces the stored result (package ",
                      "drift?).")
        if (isTRUE(strict)) stop(msg, call. = FALSE)
        warning(msg, " Using the fresh result; the snapshot holds the ",
                "original.", call. = FALSE)
      }
    }
    return(res)
  }
  .dataset_read_payload(path, mf)
}

#' @rdname dataset_store
#' @export
load_dataset <- function(name, hash = NULL, path = NULL, evaluate = FALSE,
                         strict = FALSE, verbose = TRUE) {
  # `name@hash` names a version
  if (is.null(path) && grepl("@", name, fixed = TRUE)) {
    parts <- strsplit(name, "@", fixed = TRUE)[[1]]
    name <- parts[1]
    if (is.null(hash)) hash <- parts[2]
  }
  if (is.null(path)) {
    if (dir.exists(name) && file.exists(fp(name, "dataset.yml"))) {
      path <- name
    } else {
      path <- .dataset_store_resolve(name, hash)
    }
  }
  mf <- if (!is.null(path)) {
    tryCatch(yaml::read_yaml(fp(path, "dataset.yml")),
             error = function(e) NULL)
  } else NULL
  if (is.null(mf)) {
    stop("Dataset '", if (is.null(hash)) name else paste0(name, "@", hash),
         "' was not found in the registry or the dataset store ('",
         get_datasets_path(), "').\n",
         "  Run refresh_registry() to rescan, save_dataset() to store it, ",
         "or pass path= to a dataset directory.")
  }
  .mark_notice("dataset", path, mf$name %||% name)
  x <- .dataset_payload(path, mf, prefer_snapshot = !isTRUE(evaluate),
                        strict = strict, verbose = verbose, hash = hash)
  if (verbose) {
    cat("Dataset '", mf$name, "' (", mf$kind, ") loaded from '", path,
        "'\n", sep = "")
  }
  x
}

# --------------------------------------------------------------------------- #
# Thinning owned data slots into dataset refs, and resolving them back.
#
# The data-carrying slots the store understands: the big table slots of
# weather/demand objects, and the geoscale map on config (settings inherits
# config, so scenarios are covered by the same entry).
# --------------------------------------------------------------------------- #
.dataset_slot_of <- function(obj) {
  if (is(obj, "weather")) return("weather")
  if (is(obj, "demand")) return("demand")
  if (is(obj, "config")) return("geoscale")   # incl. settings
  NULL
}

.dataset_slot_empty <- function(obj, slot_name) {
  v <- methods::slot(obj, slot_name)
  if (is.data.frame(v)) return(nrow(v) == 0)
  is.null(v)
}

# Thin ONE object: replace its data slot with an empty prototype + a
# `misc$dataset_ref` entry when the identical content is in the dataset
# store. Returns list(obj=, entry=, full=) — entry/full NULL when nothing
# was referenced (kept embedded).
.thin_dataset_slot <- function(obj, embed_datasets = NULL, id = NULL,
                               verbose = TRUE) {
  slot_name <- .dataset_slot_of(obj)
  if (is.null(id)) {
    id <- if (.hasSlot(obj, "name") && nzchar(obj@name)) obj@name else
      class(obj)[1]
  }
  none <- list(obj = obj, entry = NULL, full = NULL)
  if (is.null(slot_name) || .dataset_slot_empty(obj, slot_name)) return(none)
  if (isTRUE(embed_datasets)) return(none)
  payload <- methods::slot(obj, slot_name)
  h <- dataset_hash(payload)
  hit <- .dataset_store_find_hash(h)
  if (is.null(hit)) {
    if (isFALSE(embed_datasets)) {
      stop("embed_datasets = FALSE, but the '", slot_name, "' data of '",
           id, "' is not in the dataset store. ",
           "save_dataset() it first, or use embed_datasets = NULL/TRUE.")
    }
    return(none)   # auto mode: keep embedded
  }
  full <- payload
  if (is.data.frame(payload)) {
    methods::slot(obj, slot_name) <- payload[0, , drop = FALSE]
  } else {
    methods::slot(obj, slot_name) <- NULL
  }
  obj@misc$dataset_ref[[slot_name]] <- list(
    name = hit$name, hash = h,
    kind = if (is.data.frame(full)) "table" else "object",
    source = "ref", path = .registry_rel_path(hit$path))
  entry <- c(list(object = id, slot = slot_name),
             obj@misc$dataset_ref[[slot_name]])
  if (isTRUE(verbose)) {
    message("Dataset ref: '", entry$object, "' @", slot_name, " -> ",
            hit$name, "@", substr(h, 1, 8))
  }
  list(obj = obj, entry = entry, full = full)
}

# Resolve every dataset ref an object carries, assigning payloads back into
# their slots and dropping the refs — a live object never holds a stub.
.resolve_dataset_refs <- function(obj, strict = FALSE, verbose = TRUE) {
  refs <- obj@misc[["dataset_ref"]]
  if (is.null(refs) || !length(refs)) return(obj)
  for (slot_name in names(refs)) {
    ref <- refs[[slot_name]]
    path <- tryCatch(.dataset_store_resolve(ref$name, ref$hash),
                     error = function(e) NULL)
    if (is.null(path) && !is.null(ref$path)) {
      cand <- fp(dirname(get_registry_file()), ref$path)
      if (file.exists(fp(cand, "dataset.yml"))) {
        cand_h <- tryCatch(yaml::read_yaml(fp(cand, "dataset.yml"))$hash,
                           error = function(e) "")
        if (startsWith(cand_h %||% "", ref$hash %||% "")) path <- cand
      }
    }
    if (is.null(path)) {
      # exact version gone (entry updated in place): resolve by NAME, warn
      cur <- tryCatch(.dataset_store_resolve(ref$name, NULL),
                      error = function(e) NULL)
      if (!is.null(cur)) {
        cur_h <- tryCatch(yaml::read_yaml(fp(cur, "dataset.yml"))$hash,
                          error = function(e) "")
        warning("The '", slot_name, "' data references dataset '", ref$name,
                "' @", substr(ref$hash %||% "", 1, 8),
                "; the store now holds @", substr(cur_h %||% "", 1, 8),
                " — loading the current version. Results may not reproduce; ",
                "re-save the owner to adopt it, or seal_dataset() finished ",
                "inputs.", call. = FALSE)
        path <- cur
      }
    }
    if (is.null(path)) {
      id <- if (.hasSlot(obj, "name") && nzchar(obj@name)) obj@name else
        class(obj)[1]
      stop("The '", slot_name, "' data of '", id,
           "' references dataset '", ref$name, "@",
           substr(ref$hash, 1, 8), "', which was not found in the dataset ",
           "store ('", get_datasets_path(), "').\n",
           "  Run refresh_registry() to rescan, or restore the datasets/ ",
           "folder this object was saved with.")
    }
    mf <- yaml::read_yaml(fp(path, "dataset.yml"))
    payload <- .dataset_payload(path, mf, prefer_snapshot = TRUE,
                                strict = strict, verbose = FALSE,
                                hash = ref$hash)
    methods::slot(obj, slot_name) <- payload
  }
  obj@misc[["dataset_ref"]] <- NULL
  obj
}


# ---------------------------------------------------------------------------
# (was R/seal.R, merged 2026-09-20)
# ---------------------------------------------------------------------------

# =============================================================================#
# seal.R — store-entry lifecycle: seal / unseal, and mark-for-deletion.
#
# With the name-addressed stores an entry
# updates IN PLACE, so finished work needs a lock: SEALING freezes an entry
# against modification (identical re-saves stay silent no-ops; changed
# content errors, naming the unseal verb). The opposite end of the
# lifecycle is a soft-delete queue: MARKING an entry for deletion attaches
# an importance number (higher = more valuable = deleted later), and
# `delete_marked()` — dry-run by default — removes marks up to a threshold.
# Sealed entries are never deleted, even when marked. All state lives in
# the entry's manifest: `sealed`, `sealed_at`, `sealed_hash`,
# `marked_delete`, `delete_importance`, `marked_at`.
# =============================================================================#

.entry_kinds <- function() list(
  model = list(root = get_models_path, manifest = "model.yml",
               resolve = function(nm) .model_store_resolve(nm, NULL),
               unseal = "unseal_model"),
  repository = list(root = get_repositories_path, manifest = "repository.yml",
                    resolve = function(nm) .repo_store_resolve(nm, NULL),
                    unseal = "unseal_repository"),
  dataset = list(root = get_datasets_path, manifest = "dataset.yml",
                 resolve = function(nm) .dataset_store_resolve(nm, NULL),
                 unseal = "unseal_dataset"),
  scenario = list(root = get_scenarios_path, manifest = "scenario.yml",
                  resolve = function(nm) .scenario_resolve(nm),
                  unseal = "unseal_scenario")
)

# name or object -> the entry's folder; loud error when absent
.entry_resolve <- function(type, x) {
  nm <- if (isS4(x) && .hasSlot(x, "name")) x@name else as.character(x)
  if (is(x, "scenario") && length(x@path) == 1L && nzchar(x@path) &&
      dir.exists(x@path)) {
    return(list(name = nm, path = gsub("[\\/]+", "/", x@path)))
  }
  kind <- .entry_kinds()[[type]]
  path <- tryCatch(kind$resolve(nm), error = function(e) NULL)
  if (is.null(path) || !file.exists(fp(path, kind$manifest))) {
    stop(type, " '", nm, "' was not found in its store ('", kind$root(),
         "'). Run refresh_registry() to rescan, or save it first.")
  }
  list(name = nm, path = path)
}

.entry_manifest_update <- function(type, path, fields) {
  mf_path <- fp(path, .entry_kinds()[[type]]$manifest)
  mf <- tryCatch(yaml::read_yaml(mf_path), error = function(e) list())
  mf[names(fields)] <- fields
  yaml::write_yaml(mf, mf_path)
  invisible(mf)
}

.entry_manifest <- function(type, path) {
  tryCatch(yaml::read_yaml(fp(path, .entry_kinds()[[type]]$manifest)),
           error = function(e) NULL)
}

# Lifecycle fields carried across a manifest rewrite (an in-place update
# keeps an entry's deletion mark; seal state never reaches here because a
# sealed entry refuses the update).
.lifecycle_carry <- function(prev) {
  keep <- c("sealed", "sealed_at", "sealed_hash",
            "marked_delete", "delete_importance", "marked_at")
  if (is.null(prev)) return(list())
  prev[intersect(keep, names(prev))]
}

# Refuse modification of a sealed entry (called from the save paths and the
# scenario run/drop machinery). `mf` may be NULL (no manifest yet).
.seal_guard <- function(mf, type, name, unseal_fn) {
  if (!is.null(mf) && isTRUE(mf$sealed)) {
    stop(type, " '", name, "' is sealed (since ",
         mf$sealed_at %||% "?", "); ", unseal_fn, "(\"", name,
         "\") to edit, or save under a new name.", call. = FALSE)
  }
  invisible(TRUE)
}

# The scenario-side guard: no manifest yet = nothing to protect.
.scenario_seal_guard <- function(scen) {
  mfp <- fp(scen@path, "scenario.yml")
  if (length(mfp) != 1L || !file.exists(mfp)) return(invisible(TRUE))
  .seal_guard(tryCatch(yaml::read_yaml(mfp), error = function(e) NULL),
              "scenario", scen@name, "unseal_scenario")
}

# A loaded entry that is queued for deletion says so.
.mark_notice <- function(type, path, name) {
  mf <- .entry_manifest(type, path)
  if (!is.null(mf) && isTRUE(mf$marked_delete)) {
    message(type, " '", name, "' is marked for deletion (importance ",
            mf$delete_importance %||% 0, "); unmark_delete() to keep it.")
  }
  invisible(NULL)
}

.seal_set <- function(type, x, sealed, verbose = TRUE) {
  e <- .entry_resolve(type, x)
  mf <- .entry_manifest(type, e$path)
  fields <- if (sealed) {
    list(sealed = TRUE, sealed_at = .registry_now(),
         sealed_hash = mf$hash %||% "")
  } else {
    list(sealed = FALSE)
  }
  payload <- c(model = "mod.RData", repository = "repo.RData")[type]
  if (sealed && !is.na(payload)) {
    objs <- NULL
    if (file.exists(fp(e$path, payload))) {
      # the payload is loaded once: its content hash (for a lazily saved
      # entry) and, for a repository, its objects' hashes
      x <- if (identical(type, "repository")) {
        load_repository(e$name, path = e$path, verbose = FALSE)
      } else {
        load_model(e$name, path = e$path, verbose = FALSE)
      }
      # hashed on its full content, as saves hash it: a thinned object
      # hashes its empty slots, and a loaded container can be in memory
      # while its objects' tables are still on disk
      x <- .deep_mem(x)
      if (isFALSE(mf$hashed)) {
        mf$hash <- if (identical(type, "repository")) repository_hash(x) else model_hash(x)
        mf$hashed <- TRUE
        fields$sealed_hash <- mf$hash
        fields$hash <- mf$hash
        fields$hashed <- TRUE
      }
      if (identical(type, "repository")) objs <- .repository_object_hashes(x)
      rm(x)
    }
    fields$versions <- .store_seal_version(e$path, mf, payload)
    # a model saved under a save id re-records itself, under its content
    # hash, on the sealed repository versions it references
    if (identical(type, "model")) {
      for (re in mf$repositories %||% list()) {
        if (!identical(re$source, "ref")) next
        .dependents_add("repository", .abs_store_path(re$path), re$hash,
                        list(type = "model", name = e$name,
                             path = .registry_rel_path(e$path), hash = mf$hash))
      }
    }
    h <- as.character(mf$hash %||% "")
    if (length(objs) && h %in% names(fields$versions)) {
      fields$versions[[h]]$objects <- as.list(objs)
    }
  }
  .entry_manifest_update(type, e$path, fields)
  .registry_sync_entry(type, e$path, e$name)
  if (isTRUE(verbose)) {
    if (sealed) {
      message(type, " '", e$name, "' sealed — modification refused until ",
              "unseal_", type, "()")
    } else {
      message(type, " '", e$name, "' unsealed — editable again")
    }
  }
  invisible(e$path)
}

#' Seal and unseal store entries; mark them for deletion
#'
#' @description
#' Store entries update IN PLACE, so finished work can be locked:
#' `seal_model("TOPIA")` freezes the store entry — re-saving identical
#' content stays a silent no-op, but CHANGED content errors until
#' `unseal_model()`. A sealed scenario is an archive: it loads, `getData()`s
#' and reports freely, but refuses `save_scenario()`, new recorded solves,
#' and `drop_scenario_run()`. A sealed entry also can never trigger the
#' reference-mismatch warning — its content cannot drift.
#'
#' The other end of the lifecycle: `mark_delete(x, importance = )` queues an
#' entry for deletion (HIGHER importance = more valuable = deleted later),
#' `unmark_delete(x)` clears the mark, and [delete_marked()] — dry-run by
#' default — actually removes marked entries up to an importance threshold.
#' Sealed entries are never deleted, even when marked.
#'
#' All lifecycle state lives in the entry's manifest, so it survives moves
#' and syncs, and `refresh_registry()` never disturbs it.
#'
#' @param x the object (model/repository/scenario), or its registered name.
#'   For `mark_delete()`/`unmark_delete()` on a bare name, give `type =`.
#' @param verbose logical.
#' @param force logical; `unseal_model()` / `unseal_repository()` refuse while
#'   the current sealed version has dependents (see [list_dependents()]),
#'   listing them; `TRUE` releases the name anyway. The sealed version stays,
#'   so the dependents keep loading it.
#' @rdname seal
#' @export
#' @examples
#' \dontrun{
#' open_project(tempdir())
#'
#' save_repository(newRepository("my_repo", newCommodity("ELC", unit = "MWh")))
#'
#' # sealing and marking address the STORED version, by name: a sealed one
#' # refuses overwrites and deletion
#' seal_repository("my_repo")
#' unseal_repository("my_repo")
#'
#' # marking is the opposite -- flag now, sweep later
#' mark_delete("my_repo", type = "repository", importance = 0)
#' delete_marked(dry_run = TRUE)      # review what would go
#' delete_marked(dry_run = FALSE)
#' unmark_delete("my_repo", type = "repository")
#' }
seal_model <- function(x, verbose = TRUE) .seal_set("model", x, TRUE, verbose)

#' @rdname seal
#' @export
unseal_model <- function(x, force = FALSE, verbose = TRUE) {
  .unseal("model", x, force, verbose)
}

#' @rdname seal
#' @export
seal_repository <- function(x, verbose = TRUE) {
  .seal_set("repository", x, TRUE, verbose)
}

#' @rdname seal
#' @export
unseal_repository <- function(x, force = FALSE, verbose = TRUE) {
  .unseal("repository", x, force, verbose)
}

#' @rdname seal
#' @export
seal_dataset <- function(x, verbose = TRUE) {
  .seal_set("dataset", x, TRUE, verbose)
}

#' @rdname seal
#' @export
unseal_dataset <- function(x, verbose = TRUE) {
  .seal_set("dataset", x, FALSE, verbose)
}

#' @rdname seal
#' @export
seal_scenario <- function(x, verbose = TRUE) {
  .seal_set("scenario", x, TRUE, verbose)
}

#' @rdname seal
#' @export
unseal_scenario <- function(x, verbose = TRUE) {
  .seal_set("scenario", x, FALSE, verbose)
}

# ── mark for deletion ──────────────────────────────────────────────────────

.entry_type_of <- function(x, type = NULL) {
  if (!is.null(type)) {
    return(match.arg(type, names(.entry_kinds())))
  }
  if (is(x, "model")) return("model")
  if (is(x, "repository")) return("repository")
  if (is(x, "scenario")) return("scenario")
  stop("Give `type = ` (\"model\", \"repository\", \"dataset\" or ",
       "\"scenario\") when marking by name.")
}

.mark_set <- function(x, type, fields, verbose, what) {
  type <- .entry_type_of(x, type)
  e <- .entry_resolve(type, x)
  .entry_manifest_update(type, e$path, fields)
  # the registry caches the mark, so it has to hear about it now rather than
  # at the next refresh_registry()
  .registry_sync_entry(type, e$path, e$name)
  if (isTRUE(verbose)) {
    message(type, " '", e$name, "' ", what)
  }
  invisible(e$path)
}

#' @param version for `mark_delete()` / `unmark_delete()` on a model or
#'   repository: mark (or unmark) superseded sealed versions instead of the
#'   whole entry -- a hash or prefix, several, or `"unused"` for every version
#'   nothing references (see [list_versions()]). [delete_marked()] deletes
#'   marked versions that are still unreferenced.
#' @param importance numeric; HIGHER = more valuable = deleted later.
#'   [delete_marked()] removes only marks with importance at or below its
#'   `max_importance`.
#' @param type character, the entry type when `x` is a bare name.
#' @rdname seal
#' @export
mark_delete <- function(x, importance = 0, type = NULL, verbose = TRUE,
                        version = NULL) {
  if (!is.null(version)) {
    tp <- .entry_type_of(x, type)
    e <- .entry_resolve(tp, x)
    return(.mark_versions(tp, e$path, e$name, version,
                          list(marked_delete = TRUE,
                               delete_importance = as.numeric(importance),
                               marked_at = .registry_now()),
                          verbose, paste0("marked for deletion (importance ", importance, ")")))
  }
  p <- .mark_set(x, type,
                 list(marked_delete = TRUE,
                      delete_importance = as.numeric(importance),
                      marked_at = .registry_now()),
                 verbose,
                 paste0("marked for deletion (importance ", importance, ")"))
  # Marking is a queue, not a deletion: report what points at the entry, never
  # refuse. The refusal belongs to delete_marked(), where references are
  # re-checked against the state at sweep time.
  if (isTRUE(verbose)) {
    deps <- tryCatch(list_dependents(x, type), error = function(e) NULL)
    if (!is.null(deps) && nrow(deps)) {
      message("  ", nrow(deps), " entr", if (nrow(deps) == 1L) "y" else "ies",
              " reference it; delete_marked() will skip it unless ",
              "ignore_refs = TRUE. list_dependents() lists them.")
    }
  }
  invisible(p)
}

#' @rdname seal
#' @export
unmark_delete <- function(x, type = NULL, verbose = TRUE, version = NULL) {
  if (!is.null(version)) {
    tp <- .entry_type_of(x, type)
    e <- .entry_resolve(tp, x)
    return(.mark_versions(tp, e$path, e$name, version,
                          list(marked_delete = FALSE), verbose, "unmarked, kept"))
  }
  .mark_set(x, type, list(marked_delete = FALSE), verbose,
            "unmarked — kept")
}

#' Delete entries marked for deletion
#'
#' @description
#' Scans the four stores for entries queued by [mark_delete()] and removes
#' those at or below `max_importance`. **Dry-run by default**: it only
#' lists what would go. Sealed entries are never deleted, even when marked
#' (reported as `skipped_sealed`), and neither are entries that something
#' still references (`skipped_referenced`). After a real deletion the registry
#' is refreshed.
#'
#' @details
#' Since [save_scenario()] references the model store rather than embedding a
#' copy, deleting an entry can break the entries that point at it — and the
#' breakage would surface only later, at load time. So a marked entry with
#' dependents is skipped: [list_dependents()] lists them, and
#' `ignore_refs = TRUE` deletes anyway.
#'
#' In an **interactive** session a referenced entry is offered rather than
#' silently skipped: the dependents are named and the deletion confirmed one
#' entry at a time. A non-interactive session is never prompted — it takes the
#' refusal and `ignore_refs`, so a scripted sweep cannot hang.
#'
#' @param types character, which stores to sweep (default: all four).
#' @param max_importance numeric threshold; only marks at or below it are
#'   deleted (default 0 — the least valuable tier).
#' @param dry_run logical; `FALSE` actually deletes.
#' @param ignore_refs logical; `TRUE` deletes referenced entries too. Named
#'   apart from `force` because [drop_scenario_run()]'s `force` means
#'   something else (the active run).
#' @param verbose logical.
#' @return a tibble of the marked entries (type, name, importance, sealed,
#'   dependents, action, path), invisibly when deleting.
#' @rdname seal
#' @export
delete_marked <- function(types = NULL, max_importance = 0, dry_run = TRUE,
                          ignore_refs = FALSE, verbose = TRUE) {
  kinds <- .entry_kinds()
  types <- if (is.null(types)) names(kinds) else
    match.arg(types, names(kinds), several.ok = TRUE)
  # one walk for every entry considered below, not one per entry
  idx <- if (isTRUE(ignore_refs)) .dep_cols() else .store_dep_index()
  rows <- list()
  for (tp in types) {
    root <- kinds[[tp]]$root()
    if (!dir.exists(root)) next
    for (d in list.dirs(root, recursive = FALSE)) {
      mf <- tryCatch(yaml::read_yaml(fp(d, kinds[[tp]]$manifest)),
                     error = function(e) NULL)
      if (is.null(mf) || !isTRUE(mf$marked_delete)) next
      imp <- as.numeric(mf$delete_importance %||% 0)
      sealed <- isTRUE(mf$sealed)
      nm <- as.character(mf$name %||% basename(d))
      deps <- .dep_of(idx, tp, nm, as.character(mf$hash %||% ""))
      # dependents of the entry's sealed versions, verified
      vdeps <- if (isTRUE(ignore_refs)) 0L else .entry_version_dependents(tp, d)
      action <- if (imp > max_importance) "kept_importance" else
        if (sealed) "skipped_sealed" else
        if (nrow(deps) || vdeps > 0L) "skipped_referenced" else
        if (dry_run) "would_delete" else "deleted"
      # the dialogue, only where a person can answer it
      if (identical(action, "skipped_referenced") && !dry_run &&
          .dep_prompt(tp, nm, deps)) {
        action <- "deleted"
      }
      rows[[length(rows) + 1L]] <- tibble(
        type = tp, name = nm, importance = imp,
        sealed = sealed, dependents = max(nrow(deps), vdeps), action = action,
        path = gsub("[\\/]+", "/", d))
    }
  }
  # marked sealed versions of models and repositories
  for (tp in intersect(types, c("model", "repository"))) {
    v <- list_versions(type = tp, details = TRUE)
    v <- v[!is.na(v$importance) & v$status %in% c("marked", "in use"), , drop = FALSE]
    for (i in seq_len(nrow(v))) {
      action <- if (v$importance[i] > max_importance) "kept_importance" else
        if (v$refs[i] > 0L && !isTRUE(ignore_refs)) "skipped_referenced" else
        if (dry_run) "would_delete" else "deleted"
      rows[[length(rows) + 1L]] <- tibble(
        type = tp, name = v$name[i], version = v$version[i],
        importance = v$importance[i], sealed = TRUE, dependents = v$refs[i],
        action = action, path = v$path[i], hash = v$hash[i])
    }
  }
  out <- if (length(rows)) bind_rows(rows) else
    tibble(type = character(0), name = character(0), version = character(0),
           importance = numeric(0), sealed = logical(0),
           dependents = integer(0), action = character(0),
           path = character(0), hash = character(0))
  if (!"version" %in% names(out)) out$version <- NA_character_
  if (!"hash" %in% names(out)) out$hash <- NA_character_
  if (!dry_run) {
    for (i in which(out$action == "deleted")) {
      unlink(out$path[i], recursive = TRUE, force = TRUE)
      # a version: its record in the entry's manifest goes too
      if (!is.na(out$version[i])) {
        mf_path <- fp(dirname(out$path[i]), .entry_kinds()[[out$type[i]]]$manifest)
        mf <- yaml::read_yaml(mf_path)
        mf$versions[[out$hash[i]]] <- NULL
        yaml::write_yaml(mf, mf_path)
      }
    }
    if (any(out$action == "deleted")) {
      tryCatch(refresh_registry(), error = function(e) {
        warning("Deleted, but the registry could not be refreshed (",
                conditionMessage(e), ") — run refresh_registry().",
                call. = FALSE)
      })
    }
  }
  if (isTRUE(verbose)) {
    if (!nrow(out)) {
      message("No entries are marked for deletion.")
    } else if (dry_run) {
      message("Dry run — nothing deleted. ",
              sum(out$action == "would_delete"), " of ", nrow(out),
              " marked entr", if (nrow(out) == 1L) "y" else "ies",
              " within max_importance = ", max_importance,
              "; delete_marked(dry_run = FALSE) to delete.")
      print(out[, c("action", "type", "name", "version", "importance", "dependents")])
    } else {
      message(sum(out$action == "deleted"), " entr",
              if (sum(out$action == "deleted") == 1L) "y" else "ies",
              " deleted.")
    }
    nref <- sum(out$action == "skipped_referenced")
    if (nref > 0L) {
      message("  ", nref, " kept because ", if (nref == 1L) "it is" else
              "they are", " still referenced; list_dependents() lists what ",
              "points at ", if (nref == 1L) "it" else "them",
              ", ignore_refs = TRUE deletes anyway.")
    }
  }
  if (dry_run) out else invisible(out)
}
