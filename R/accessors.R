# =============================================================================#
# accessors.R — terse name-based access to registered objects.
#
# Two layers of the storage API:
#   load_* : explicit IO — always read from disk, return the object, no
#            session state (load_scenario/load_model/load_repository/
#            load_dataset all resolve registry names).
#   get*   : name-first accessors on top. getScenario() CACHES the thin
#            shell in the session bank `.scen` (created in .GlobalEnv at
#            attach, see start_msg.R) and reloads automatically when the
#            folder's manifest stamp drifts — a re-saved scenario is never
#            served stale. getModel/getRepository/getDataset are uncached
#            (content-addressed stores; a load is a cheap RData/parquet
#            read).
# =============================================================================#

# The session scenario bank; created at attach, re-created here defensively
# (non-attach contexts: Rscript -e with loadNamespace, some test harnesses).
.scen_bank <- function() {
  if (!exists(".scen", envir = .GlobalEnv, inherits = FALSE)) {
    assign(".scen", new.env(parent = .GlobalEnv), envir = .GlobalEnv)
  }
  get(".scen", envir = .GlobalEnv, inherits = FALSE)
}

# Freshness stamp of a scenario folder: the manifest's `updated` field,
# falling back to scen.RData's mtime for layout-2 folders.
.scenario_stamp <- function(path) {
  mf <- fp(path, "scenario.yml")
  if (file.exists(mf)) {
    up <- tryCatch(yaml::read_yaml(mf)$updated, error = function(e) NULL)
    if (!is.null(up) && nzchar(up)) return(up)
  }
  rd <- fp(path, "scen.RData")
  if (file.exists(rd)) return(as.character(file.mtime(rd)))
  ""
}

#' Access registered scenarios, models, repositories, and datasets by name
#'
#' @description
#' The short, name-first accessors over the project stores (see the
#' scenario-management article). `getScenario("base")` resolves the name
#' through the registry (falling back to a scan of the scenarios store),
#' loads the thin on-disk shell, and CACHES it in the session bank `.scen` —
#' a second call is free, and a scenario re-saved on disk is reloaded
#' automatically (the cache is stamped with the folder manifest's `updated`
#' field). [getData()] accepts names directly and uses this cache:
#' `getData(c("base", "policy"), name = "vTechOut", merge = TRUE)`.
#'
#' `getModel()`, `getRepository()` and `getDataset()` are the typed
#' companions — thin wrappers over [load_model()], [load_repository()] and
#' [load_dataset()] name resolution (uncached).
#'
#' `open_project(path)` anchors the whole session on one project folder:
#' the registry file, the scenarios/models/repositories/datasets store
#' roots, and the temporary tiers for derived artifacts (`reports/`,
#' `levcosts/`). Without it, everything resolves relative to the working
#' directory (the option defaults).
#'
#' @param name character, the object's registered name. Scenarios accept a
#'   name only; models/repositories/datasets also accept `"name@hash8"`.
#' @param registry an `ert_registry` tibble to resolve against (default:
#'   [load_registry()]).
#' @param run character, optional run id activated in the returned scenario
#'   via [read_solution()] (the cached copy keeps the default run).
#' @param refresh logical, reload from disk even when the cached copy is
#'   current.
#' @param hash character, content hash to pin a stored version.
#' @param path character, project root directory (must exist).
#' @param verbose logical.
#'
#' @return the requested object; `open_project()` returns the previous
#'   option values, invisibly.
#'
#' @rdname accessors
#' @export
#' @examples
#' \dontrun{
#' open_project("~/projects/mymodel")
#' base <- getScenario("base")
#' getData(c("base", "policy"), name = "vTechOut", merge = TRUE)
#' }
getScenario <- function(name, registry = NULL, run = NULL, refresh = FALSE,
                        verbose = FALSE) {
  stopifnot(is.character(name), length(name) == 1L, nzchar(name))
  path <- .scenario_resolve(name, registry = registry)
  if (is.null(path)) {
    stop("Scenario '", name, "' was not found in the registry ('",
         get_registry_file(), "') or the scenarios store ('",
         get_scenarios_path(), "').\n",
         "  Run refresh_registry() to rescan, or load_scenario(<path>) ",
         "directly.")
  }
  bank <- .scen_bank()
  stamp <- .scenario_stamp(path)
  sc <- NULL
  if (!isTRUE(refresh) && exists(name, envir = bank, inherits = FALSE)) {
    cached <- get(name, envir = bank, inherits = FALSE)
    if (is(cached, "scenario") &&
        identical(cached@misc[[".accessor_stamp"]] %||% "", stamp)) {
      sc <- cached
    }
  }
  if (is.null(sc)) {
    sc <- load_scenario(path, env = NULL, verbose = verbose)
    sc@misc$.accessor_stamp <- stamp
    assign(name, sc, envir = bank)
  }
  if (!is.null(run)) sc <- read_solution(sc, run = run, echo = FALSE)
  sc
}

#' @rdname accessors
#' @export
getModel <- function(name, hash = NULL, verbose = FALSE) {
  load_model(name, hash = hash, verbose = verbose)
}

#' @rdname accessors
#' @export
getRepository <- function(name, hash = NULL, verbose = FALSE) {
  load_repository(name, hash = hash, verbose = verbose)
}

#' @rdname accessors
#' @export
getDataset <- function(name, hash = NULL, verbose = FALSE) {
  load_dataset(name, hash = hash, verbose = verbose)
}

#' @rdname accessors
#' @export
open_project <- function(path = ".", verbose = TRUE) {
  path <- gsub("[\\/]+", "/",
               normalizePath(path, winslash = "/", mustWork = TRUE))
  old <- list(
    registry_file      = set_registry_file(fp(path, "energyRt_registry.csv")),
    scenarios_path     = set_scenarios_path(fp(path, "scenarios")),
    models_path        = set_models_path(fp(path, "models")),
    repositories_path  = set_repositories_path(fp(path, "repositories")),
    datasets_path      = set_datasets_path(fp(path, "datasets")),
    reports_path       = set_reports_path(fp(path, "reports")),
    levcost_cache_path = set_levcost_cache_path(fp(path, "levcosts"))
  )
  if (isTRUE(verbose)) {
    n_scen <- tryCatch(
      nrow(find_registry(load_registry(), type = "scenario")),
      error = function(e) 0L)
    message("energyRt project: ", path,
            " (", n_scen, " registered scenario",
            if (n_scen == 1L) "" else "s", ")")
  }
  invisible(old)
}
