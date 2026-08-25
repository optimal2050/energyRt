# =============================================================================#
# registry.R — persisted project registry of models, scenarios, and runs.
#
# One CSV file per project (default `energyRt_registry.csv` at the project
# root, a sibling of the `scenarios/` and `models/` stores) indexes what is
# saved on disk: one row per model, scenario, or run. The on-disk manifests
# (`scenario.yml`, `model.yml`, `run.yml`) are the source of truth; the CSV is
# a rebuildable index — `refresh_registry()` rescans the stores and reconciles.
#
# The previous implementation (a wrapper over the CRAN `registry` package,
# in-memory only) is archived in `drafts/registry-cran-wrapper.R`; its public
# names live on as deprecated shims at the bottom of this file.
# =============================================================================#

# canonical column set; everything character for CSV round-trip stability
.registry_cols <- c(
  "type",             # "model" | "repository" | "scenario" | "run" | "dataset"
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
  .registry_class(as_tibble(setNames(
    lapply(.registry_cols, \(x) character(0)),
    .registry_cols
  )))
}

# the registry tibble carries an S3 subclass so generics (`add()`) can
# dispatch on it; tibble/dplyr verbs drop extra classes, so every function
# returning a registry re-attaches it
.registry_class <- function(reg) {
  class(reg) <- unique(c("ert_registry", class(reg)))
  reg
}

#' Project registry of models, scenarios, and runs
#'
#' @description
#' The registry is a per-project CSV file (see [get_registry_file()]) indexing
#' the saved models, scenarios, and runs: one row each, with type, name,
#' content hash (models), path, parent (runs), timestamps, and a memo.
#' It is a rebuildable index — the on-disk manifests are the source of truth,
#' and `refresh_registry()` reconstructs the registry by rescanning the
#' `scenarios/` and `models/` stores.
#'
#' * `load_registry()` reads the registry file into a tibble (an empty,
#'   correctly-typed tibble if the file does not exist yet).
#' * `save_registry()` writes the tibble back.
#' * `add_to_registry()` inserts or updates one row (keyed by `type` + `name` +
#'   `parent`) and returns the updated tibble; it does not write the file.
#' * `find_registry()` filters by any combination of `type`, `name`, `hash`,
#'   and `parent` (exact matches; `NULL` = no filter).
#' * `refresh_registry()` rescans the stores under `root` and rebuilds rows
#'   from what is actually on disk, preserving `created`/`memo` of surviving
#'   entries; with `write = TRUE` (default) the result is saved.
#'
#' @param file character, path to the registry CSV
#'   (default [get_registry_file()]).
#' @param reg a registry tibble as returned by `load_registry()`.
#' @param type character: `"model"`, `"repository"`, `"scenario"`, or `"run"`.
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
#' @return `load_registry()`, `add_to_registry()`, `find_registry()`, and
#'   `refresh_registry()` return the registry tibble; `save_registry()`
#'   returns `file` invisibly.
#'
#' @rdname registry
#' @export
#' @examples
#' reg <- load_registry(tempfile(fileext = ".csv"))
#' reg <- add_to_registry(reg, "scenario", "BASE", path = "scenarios/BASE")
#' find_registry(reg, type = "scenario")
load_registry <- function(file = get_registry_file()) {
  if (!file.exists(file)) return(.registry_empty())
  reg <- utils::read.csv(file, colClasses = "character",
                         stringsAsFactors = FALSE) |> as_tibble()
  missing_cols <- setdiff(.registry_cols, names(reg))
  for (m in missing_cols) reg[[m]] <- ""
  .registry_class(reg[, .registry_cols])
}

#' @rdname registry
#' @export
save_registry <- function(reg, file = get_registry_file()) {
  stopifnot(is.data.frame(reg))
  reg <- as_tibble(reg)[, .registry_cols]
  dir_ <- dirname(file)
  if (!dir.exists(dir_)) dir.create(dir_, recursive = TRUE)
  utils::write.csv(reg, file, row.names = FALSE, na = "")
  invisible(file)
}

#' @rdname registry
#' @export
add_to_registry <- function(reg, type, name, path,
                         hash = "", model_hash = "", parent = "",
                         memo = "") {
  type <- match.arg(type, c("model", "repository", "scenario", "run",
                            "dataset"))
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
  .registry_class(bind_rows(reg, row))
}

#' @details
#' `add()` dispatches on a loaded registry, so `add(reg, "scenario", "BASE",
#' path = "scenarios/BASE")` is `add_to_registry()` under the generic shared
#' with models and repositories.
#'
#' @rdname registry
#' @method add ert_registry
#' @export
add.ert_registry <- function(obj, ...) add_to_registry(obj, ...)

#' @rdname registry
#' @export
find_registry <- function(reg, type = NULL, name = NULL, hash = NULL,
                          parent = NULL) {
  if (!is.null(type)) reg <- reg[reg$type %in% type, ]
  if (!is.null(name)) reg <- reg[reg$name %in% name, ]
  if (!is.null(hash)) {
    # accept full or short (prefix) hashes
    reg <- reg[startsWith(reg$hash, hash[1]) & nzchar(reg$hash), ]
  }
  if (!is.null(parent)) reg <- reg[reg$parent %in% parent, ]
  .registry_class(reg)
}

#' @rdname registry
#' @export
refresh_registry <- function(root = ".", file = get_registry_file(),
                             write = TRUE) {
  old <- load_registry(file)
  rel_to_reg <- function(p) .registry_rel_path(p, reg_file = file)
  # store roots from the options may be absolute (e.g. redirected to a temp
  # dir); only prefix `root` onto relative ones
  root_join <- function(p) {
    if (grepl("^([A-Za-z]:|/|\\\\)", p)) gsub("[\\/]+", "/", p) else fp(root, p)
  }
  keep_created <- function(reg, type, name, parent = "") {
    hit <- find_registry(old, type = type, name = name, parent = parent)
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
  scen_root <- root_join(get_scenarios_path())
  if (dir.exists(scen_root)) {
    for (d in list.dirs(scen_root, recursive = FALSE)) {
      # dot-prefixed dirs are scratch by convention (e.g. levcost mini-models)
      if (startsWith(basename(d), ".")) next
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
      reg <- add_to_registry(reg, "scenario", nm, path = rel_to_reg(d),
                          model_hash = model_hash)
      reg <- keep_created(reg, "scenario", nm)
      # runs (layout 3): base runs at runs/<solve>/run.yml, variant runs at
      # runs/<variant>/<solve>/run.yml
      run_ymls <- c(Sys.glob(fp(d, "runs", "*", "run.yml")),
                    Sys.glob(fp(d, "runs", "*", "*", "run.yml")))
      for (ry in run_ymls) {
        rr <- tryCatch(yaml::read_yaml(ry), error = \(e) NULL)
        if (is.null(rr)) next
        run_dir <- dirname(ry)
        parent_dir <- basename(dirname(run_dir))
        run_name <- if (identical(parent_dir, "runs")) {
          basename(run_dir)                       # base run: "<solve>"
        } else {
          fp(parent_dir, basename(run_dir))       # variant run: "<variant>/<solve>"
        }
        reg <- add_to_registry(reg, "run", run_name, path = rel_to_reg(run_dir),
                            parent = nm)
        reg <- keep_created(reg, "run", run_name, parent = nm)
      }
    }
  }

  # models and repositories: content-addressed stores of <name>@<hash8>/
  # folders, each with a manifest (layout 3)
  stores <- list(
    list(type = "model", root = root_join(get_models_path()),
         manifest = "model.yml"),
    list(type = "repository", root = root_join(get_repositories_path()),
         manifest = "repository.yml"),
    list(type = "dataset", root = root_join(get_datasets_path()),
         manifest = "dataset.yml")
  )
  for (st in stores) {
    if (!dir.exists(st$root)) next
    for (d in list.dirs(st$root, recursive = FALSE)) {
      manifest <- fp(d, st$manifest)
      if (!file.exists(manifest)) next
      mf <- tryCatch(yaml::read_yaml(manifest), error = \(e) NULL)
      if (is.null(mf)) next
      nm <- mf$name %||% sub("@.*$", "", basename(d))
      reg <- add_to_registry(reg, st$type, nm, path = rel_to_reg(d),
                          hash = mf$hash %||% "")
      reg <- keep_created(reg, st$type, nm)
    }
  }

  if (write) save_registry(reg, file)
  reg
}
