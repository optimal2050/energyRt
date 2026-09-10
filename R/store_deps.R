# =============================================================================#
# store_deps.R — the reference graph over the four stores.
#
# `save_scenario(embed_model = NULL)` stores the model and records
# `{name, hash, source}` rather than embedding a copy, so referencing is the
# normal state and removing a store entry can break entries that point at it.
# The edges are already written by the save paths; this reads them back the
# other way round (`prepare_for_sharing()` reads the same fields outbound).
#
#   scenario.yml     model:        {name, hash, source}
#                    datasets:     [{name, hash, source}, ...]
#   model.yml        repositories: [...]
#                    datasets:     [...]
#   repository.yml   datasets:     [...]
#
# Read from MANIFESTS, not the registry: the registry is a derived index that
# can be stale, and refreshing it writes. A delete guard cannot depend on that.
#
# `source == "embedded"` carries its own copy and is never a dependency — that
# distinction is the whole point of the guard.
# =============================================================================#

.dep_cols <- function() {
  tibble(type = character(0), name = character(0), path = character(0),
         via = character(0), target = character(0), hash = character(0))
}

# Outgoing references of one manifest, as list(via, target, hash) entries.
.dep_edges <- function(type, mf) {
  out <- list()
  add <- function(via, e) {
    if (!is.list(e) || !identical(as.character(e$source %||% ""), "ref")) {
      return(invisible(NULL))
    }
    nm <- as.character(e$name %||% "")
    if (!nzchar(nm)) return(invisible(NULL))
    out[[length(out) + 1L]] <<-
      list(via = via, target = nm, hash = as.character(e$hash %||% ""))
    invisible(NULL)
  }
  if (identical(type, "scenario")) add("model", mf$model)
  if (identical(type, "model")) {
    for (e in mf$repositories %||% list()) add("repository", e)
  }
  # datasets hang off scenarios, models and repositories alike
  for (e in mf$datasets %||% list()) add("dataset", e)
  out
}

# Every reference in the project, one row per edge. One first-level pass over
# the four store roots — the same walk delete_marked() and refresh_registry()
# already make.
.store_dep_index <- function() {
  kinds <- .entry_kinds()
  rows <- list()
  for (tp in names(kinds)) {
    root <- kinds[[tp]]$root()
    if (!dir.exists(root)) next
    for (d in list.dirs(root, recursive = FALSE)) {
      mf <- tryCatch(yaml::read_yaml(fp(d, kinds[[tp]]$manifest)),
                     error = function(e) NULL)
      if (is.null(mf)) next
      edges <- .dep_edges(tp, mf)
      if (!length(edges)) next
      nm <- as.character(mf$name %||% basename(d))
      for (e in edges) {
        rows[[length(rows) + 1L]] <- tibble(
          type = tp, name = nm, path = gsub("[\\/]+", "/", d),
          via = e$via, target = e$target, hash = e$hash)
      }
    }
  }
  if (length(rows)) bind_rows(rows) else .dep_cols()
}

# Rows of an index that point at one entry, with the version check applied.
# `current` is NA when the dependent recorded no hash to check against.
.dep_of <- function(idx, type, name, current_hash = "") {
  out <- idx[idx$via == type & idx$target == name, , drop = FALSE]
  out$target <- NULL
  out$current <- if (nrow(out)) {
    ifelse(!nzchar(out$hash), NA,
           nzchar(current_hash) & startsWith(current_hash, out$hash))
  } else {
    logical(0)
  }
  out[order(out$type, out$name), , drop = FALSE]
}

#' What depends on a stored model, repository or dataset
#'
#' @description
#' Lists the store entries that *reference* `x` rather than embedding a copy of
#' it — the entries that would break if it were deleted. Read-only.
#'
#' @details
#' Referencing is the default: [save_scenario()] stores the model and records
#' `{name, hash, source}` instead of embedding it, and the same holds one level
#' down for a model's repositories and for the big tables of the dataset store.
#' A dependent recorded with `source = "embedded"` holds its own copy and is
#' never listed here.
#'
#' The edges are read from the manifests (`scenario.yml`, `model.yml`,
#' `repository.yml`), not from the project registry, which is a derived index
#' that can be stale.
#'
#' `current` compares the hash the dependent recorded against the hash the
#' entry holds now. `FALSE` means the dependent is pinned to a superseded
#' version — store entries update in place — and will load the current one with
#' a "results may not reproduce" warning. `NA` means the dependent recorded no
#' hash to check.
#'
#' An entry with no dependents is an orphan as far as this project is
#' concerned: nothing here points at it.
#'
#' @param x an energyRt object, or the name of a store entry.
#' @param type the entry type when `x` is a bare name: `"model"`,
#'   `"repository"`, `"dataset"` or `"scenario"`. Inferred from an object.
#'
#' @return a tibble: `type` and `name` of the dependent, its `path`, `via` (the
#'   kind of reference), the `hash` it recorded and whether that is `current`.
#' @seealso [delete_marked()], [mark_delete()]
#' @examples
#' \dontrun{
#' store_dependents("UTOPIA", type = "model")   # which scenarios use it
#' nrow(store_dependents("UTOPIA", type = "model")) == 0   # an orphan?
#' }
#' @export
store_dependents <- function(x, type = NULL) {
  type <- .entry_type_of(x, type)
  e <- .entry_resolve(type, x)
  cur <- as.character(.entry_manifest(type, e$path)$hash %||% "")
  .dep_of(.store_dep_index(), type, e$name, cur)
}

# The interactive half of the delete guard. Never reached in a non-interactive
# session: menu() would read EOF and abort a scripted sweep, which is why the
# guard refuses first and asks second.
.dep_prompt <- function(type, name, deps) {
  if (!interactive()) return(FALSE)
  who <- paste0(deps$type, " '", deps$name, "'")
  shown <- utils::head(who, 3)
  message(type, " '", name, "' is referenced by ", nrow(deps), " entr",
          if (nrow(deps) == 1L) "y" else "ies", ":\n  ",
          paste(shown, collapse = ", "),
          if (nrow(deps) > 3L) {
            paste0(" ... (", nrow(deps) - 3L, " more)")
          } else "")
  ans <- tryCatch(
    utils::menu(c("Keep it", "Delete it anyway"),
                title = paste0("Delete ", type, " '", name, "'?")),
    error = function(e) 0L)
  identical(as.integer(ans), 2L)
}
