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
  .entry_manifest_update(type, e$path, fields)
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
#' `seal_model("UTOPIA")` freezes the store entry — re-saving identical
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
#' @rdname seal
#' @export
seal_model <- function(x, verbose = TRUE) .seal_set("model", x, TRUE, verbose)

#' @rdname seal
#' @export
unseal_model <- function(x, verbose = TRUE) {
  .seal_set("model", x, FALSE, verbose)
}

#' @rdname seal
#' @export
seal_repository <- function(x, verbose = TRUE) {
  .seal_set("repository", x, TRUE, verbose)
}

#' @rdname seal
#' @export
unseal_repository <- function(x, verbose = TRUE) {
  .seal_set("repository", x, FALSE, verbose)
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
  if (isTRUE(verbose)) {
    message(type, " '", e$name, "' ", what)
  }
  invisible(e$path)
}

#' @param importance numeric; HIGHER = more valuable = deleted later.
#'   [delete_marked()] removes only marks with importance at or below its
#'   `max_importance`.
#' @param type character, the entry type when `x` is a bare name.
#' @rdname seal
#' @export
mark_delete <- function(x, importance = 0, type = NULL, verbose = TRUE) {
  .mark_set(x, type,
            list(marked_delete = TRUE,
                 delete_importance = as.numeric(importance),
                 marked_at = .registry_now()),
            verbose,
            paste0("marked for deletion (importance ", importance, ")"))
}

#' @rdname seal
#' @export
unmark_delete <- function(x, type = NULL, verbose = TRUE) {
  .mark_set(x, type, list(marked_delete = FALSE), verbose,
            "unmarked — kept")
}

#' Delete entries marked for deletion
#'
#' @description
#' Scans the four stores for entries queued by [mark_delete()] and removes
#' those at or below `max_importance`. **Dry-run by default**: it only
#' lists what would go. Sealed entries are never deleted, even when marked
#' (reported as `skipped_sealed`). After a real deletion the registry is
#' refreshed.
#'
#' @param types character, which stores to sweep (default: all four).
#' @param max_importance numeric threshold; only marks at or below it are
#'   deleted (default 0 — the least valuable tier).
#' @param dry_run logical; `FALSE` actually deletes.
#' @param verbose logical.
#' @return a tibble of the marked entries (type, name, importance, sealed,
#'   action, path), invisibly when deleting.
#' @rdname seal
#' @export
delete_marked <- function(types = NULL, max_importance = 0, dry_run = TRUE,
                          verbose = TRUE) {
  kinds <- .entry_kinds()
  types <- if (is.null(types)) names(kinds) else
    match.arg(types, names(kinds), several.ok = TRUE)
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
      action <- if (imp > max_importance) "kept_importance" else
        if (sealed) "skipped_sealed" else
        if (dry_run) "would_delete" else "deleted"
      rows[[length(rows) + 1L]] <- tibble(
        type = tp, name = mf$name %||% basename(d), importance = imp,
        sealed = sealed, action = action,
        path = gsub("[\\/]+", "/", d))
    }
  }
  out <- if (length(rows)) bind_rows(rows) else
    tibble(type = character(0), name = character(0),
           importance = numeric(0), sealed = logical(0),
           action = character(0), path = character(0))
  if (!dry_run) {
    for (p in out$path[out$action == "deleted"]) {
      unlink(p, recursive = TRUE, force = TRUE)
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
      print(out[, c("type", "name", "importance", "sealed", "action")])
    } else {
      message(sum(out$action == "deleted"), " entr",
              if (sum(out$action == "deleted") == 1L) "y" else "ies",
              " deleted.")
    }
  }
  if (dry_run) out else invisible(out)
}
