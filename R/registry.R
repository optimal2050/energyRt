# =============================================================================#
# registry.R — persisted project registry of models, scenarios, and runs.
#
# One CSV file per project (default `energyRt_registry.csv` at the project
# root, a sibling of the `scenarios/` and `models/` stores) indexes what is
# saved on disk: one row per model, scenario, or run. The on-disk manifests
# (`scenario.yml`, `model.yml`, `run.yml`) are the source of truth; the CSV is
# a rebuildable index — `registry_refresh()` rescans the stores and reconciles.
#
# The previous implementation (a wrapper over the CRAN `registry` package,
# in-memory only) is archived in `drafts/registry-cran-wrapper.R`; its public
# names live on as deprecated shims at the bottom of this file.
# =============================================================================#

# canonical column set; everything character for CSV round-trip stability
.registry_cols <- c(
  "type",             # "model" | "scenario" | "run"
  "name",             # object name; runs: "<variant>/<solve>"
  "hash",             # model content hash (model rows), "" otherwise
  "model_hash",       # scenario rows: hash of the referenced/embedded model
  "path",             # relative to the registry file's directory
  "parent",           # runs: scenario name; "" otherwise
  "created", "updated",  # UTC timestamps, "%Y-%m-%dT%H:%M:%SZ"
  "energyRt_version",
  "memo"
)

.registry_now <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

# a path relative to the registry file's directory when possible (portability)
.registry_rel_path <- function(p, reg_file = get_registry_file()) {
  p <- gsub("[\\/]+", "/", p)
  rn <- gsub("[\\/]+", "/", normalizePath(dirname(reg_file), winslash = "/",
                                          mustWork = FALSE))
  pn <- gsub("[\\/]+", "/", normalizePath(p, winslash = "/", mustWork = FALSE))
  if (startsWith(pn, paste0(rn, "/"))) substring(pn, nchar(rn) + 2L) else p
}

.registry_empty <- function() {
  as_tibble(setNames(
    lapply(.registry_cols, \(x) character(0)),
    .registry_cols
  ))
}

#' Project registry of models, scenarios, and runs
#'
#' @description
#' The registry is a per-project CSV file (see [get_registry_file()]) indexing
#' the saved models, scenarios, and runs: one row each, with type, name,
#' content hash (models), path, parent (runs), timestamps, and a memo.
#' It is a rebuildable index — the on-disk manifests are the source of truth,
#' and `registry_refresh()` reconstructs the registry by rescanning the
#' `scenarios/` and `models/` stores.
#'
#' * `registry_load()` reads the registry file into a tibble (an empty,
#'   correctly-typed tibble if the file does not exist yet).
#' * `registry_save()` writes the tibble back.
#' * `registry_add()` inserts or updates one row (keyed by `type` + `name` +
#'   `parent`) and returns the updated tibble; it does not write the file.
#' * `registry_find()` filters by any combination of `type`, `name`, `hash`,
#'   and `parent` (exact matches; `NULL` = no filter).
#' * `registry_refresh()` rescans the stores under `root` and rebuilds rows
#'   from what is actually on disk, preserving `created`/`memo` of surviving
#'   entries; with `write = TRUE` (default) the result is saved.
#'
#' @param file character, path to the registry CSV
#'   (default [get_registry_file()]).
#' @param reg a registry tibble as returned by `registry_load()`.
#' @param type character: `"model"`, `"scenario"`, or `"run"`.
#' @param name character, object name (for runs: `"<variant>/<solve>"`).
#' @param path character, object directory, relative to the registry file's
#'   directory.
#' @param hash character, model content hash (from `model_hash()`, arriving
#'   with the model store); `""` for scenario/run rows.
#' @param model_hash character, for scenario rows: hash of the referenced or
#'   embedded model.
#' @param parent character, for run rows: the scenario name.
#' @param memo character, optional free-form note.
#' @param root character, project root to rescan (the directory holding the
#'   `scenarios/` and `models/` stores).
#' @param write logical, save the refreshed registry to `file`.
#'
#' @return `registry_load()`, `registry_add()`, `registry_find()`, and
#'   `registry_refresh()` return the registry tibble; `registry_save()`
#'   returns `file` invisibly.
#'
#' @rdname registry
#' @export
#' @examples
#' reg <- registry_load(tempfile(fileext = ".csv"))
#' reg <- registry_add(reg, "scenario", "BASE", path = "scenarios/BASE")
#' registry_find(reg, type = "scenario")
registry_load <- function(file = get_registry_file()) {
  if (!file.exists(file)) return(.registry_empty())
  reg <- utils::read.csv(file, colClasses = "character",
                         stringsAsFactors = FALSE) |> as_tibble()
  missing_cols <- setdiff(.registry_cols, names(reg))
  for (m in missing_cols) reg[[m]] <- ""
  reg[, .registry_cols]
}

#' @rdname registry
#' @export
registry_save <- function(reg, file = get_registry_file()) {
  stopifnot(is.data.frame(reg))
  reg <- as_tibble(reg)[, .registry_cols]
  dir_ <- dirname(file)
  if (!dir.exists(dir_)) dir.create(dir_, recursive = TRUE)
  utils::write.csv(reg, file, row.names = FALSE, na = "")
  invisible(file)
}

#' @rdname registry
#' @export
registry_add <- function(reg, type, name, path,
                         hash = "", model_hash = "", parent = "",
                         memo = "") {
  type <- match.arg(type, c("model", "scenario", "run"))
  stopifnot(is.character(name), length(name) == 1L, nzchar(name))
  now <- .registry_now()
  ii <- which(reg$type == type & reg$name == name & reg$parent == parent)
  row <- tibble(
    type = type, name = name, hash = hash, model_hash = model_hash,
    path = path, parent = parent,
    created = if (length(ii)) reg$created[ii[1]] else now,
    updated = now,
    energyRt_version = as.character(utils::packageVersion("energyRt")),
    memo = if (nzchar(memo) || !length(ii)) memo else reg$memo[ii[1]]
  )
  if (length(ii)) reg <- reg[-ii, ]
  bind_rows(reg, row)
}

#' @rdname registry
#' @export
registry_find <- function(reg, type = NULL, name = NULL, hash = NULL,
                          parent = NULL) {
  if (!is.null(type)) reg <- reg[reg$type %in% type, ]
  if (!is.null(name)) reg <- reg[reg$name %in% name, ]
  if (!is.null(hash)) {
    # accept full or short (prefix) hashes
    reg <- reg[startsWith(reg$hash, hash[1]) & nzchar(reg$hash), ]
  }
  if (!is.null(parent)) reg <- reg[reg$parent %in% parent, ]
  reg
}

#' @rdname registry
#' @export
registry_refresh <- function(root = ".", file = get_registry_file(),
                             write = TRUE) {
  old <- registry_load(file)
  rel_to_reg <- function(p) .registry_rel_path(p, reg_file = file)
  keep_created <- function(reg, type, name, parent = "") {
    hit <- registry_find(old, type = type, name = name, parent = parent)
    if (nrow(hit)) {
      ii <- which(reg$type == type & reg$name == name & reg$parent == parent)
      reg$created[ii] <- hit$created[1]
      if (!nzchar(reg$memo[ii])) reg$memo[ii] <- hit$memo[1]
    }
    reg
  }

  reg <- .registry_empty()

  # scenarios: any first-level directory in the scenarios store carrying the
  # `class` marker file (layout >= 2) or a `scenario.yml` manifest (layout 3)
  scen_root <- fp(root, get_scenarios_path())
  if (dir.exists(scen_root)) {
    for (d in list.dirs(scen_root, recursive = FALSE)) {
      manifest <- fp(d, "scenario.yml")
      class_file <- fp(d, "class")
      nm <- NULL
      model_hash <- ""
      if (file.exists(manifest)) {
        mf <- tryCatch(yaml::read_yaml(manifest), error = \(e) NULL)
        if (!is.null(mf)) {
          nm <- mf$name %||% basename(d)
          model_hash <- mf$model$hash %||% ""
        }
      } else if (file.exists(class_file) &&
                 any(grepl("scenario", readLines(class_file, warn = FALSE)))) {
        nm <- basename(d)
      }
      if (is.null(nm)) next
      reg <- registry_add(reg, "scenario", nm, path = rel_to_reg(d),
                          model_hash = model_hash)
      reg <- keep_created(reg, "scenario", nm)
      # runs (layout 3): runs/<variant>/<solve>/run.yml
      for (ry in Sys.glob(fp(d, "runs", "*", "*", "run.yml"))) {
        rr <- tryCatch(yaml::read_yaml(ry), error = \(e) NULL)
        if (is.null(rr)) next
        run_dir <- dirname(ry)
        run_name <- fp(basename(dirname(run_dir)), basename(run_dir))
        reg <- registry_add(reg, "run", run_name, path = rel_to_reg(run_dir),
                            parent = nm)
        reg <- keep_created(reg, "run", run_name, parent = nm)
      }
    }
  }

  # models: models/<name>@<hash8>/ with a model.yml manifest (layout 3)
  mod_root <- fp(root, get_models_path())
  if (dir.exists(mod_root)) {
    for (d in list.dirs(mod_root, recursive = FALSE)) {
      manifest <- fp(d, "model.yml")
      if (!file.exists(manifest)) next
      mf <- tryCatch(yaml::read_yaml(manifest), error = \(e) NULL)
      if (is.null(mf)) next
      nm <- mf$name %||% sub("@.*$", "", basename(d))
      reg <- registry_add(reg, "model", nm, path = rel_to_reg(d),
                          hash = mf$hash %||% "")
      reg <- keep_created(reg, "model", nm)
    }
  }

  if (write) registry_save(reg, file)
  reg
}
