# =============================================================================
# Interpolated tables as references into the dataset store
# =============================================================================
#
# A large table of a scenario's modInp (a weather or demand series taken from
# a model dataset, or a derived table such as an availability factor or a
# mapping over every timeslice) is written once into the dataset store, as
# a version of an entry named after the parameter, and the scenario keeps a
# REFERENCE to it in `@misc$param_ref`: the entry's name, the version's
# content hash, the origin that produced it, and (for a sampled scenario)
# the row filter that selects its rows from the full version. Every
# scenario and variant whose table has the same content points at the same
# version. Readers resolve the reference at path level, so the parameter's
# lazy readers (`get_lazy_data()`, `get_data_slot()`) work as for a table in
# the scenario's own store; a writer that changes the table drops the
# reference and writes locally (`.param_ref_clear()`).
#
# The record, in the vocabulary of `misc$dataset_ref`:
#
#   list(name   = "pTechAf",            # the entry: datasets/pTechAf/
#        hash   = "<dataset_hash>",     # the version: datasets/pTechAf/<hash>/
#        kind   = "table", source = "ref",
#        path   = "datasets/pTechAf",   # the entry, relative to the registry
#        nrow   = 96700000,
#        filter = NULL,                 # list(timeslice = <members>, region = <members>)
#        origin = list(kind = "derived" | "source", parameter = "pTechAf",
#                      class = "weather", objects = c(...),   # source tables
#                      pipeline = list(sparse =, folded =, pruned =)),
#        resolved = "<version dir>")    # this session's resolution, never saved as truth

# The parameters whose table is a model dataset's own series: resolvable from
# the model when the store entry is gone.
.param_source_classes <- c(pWeather = "weather", pDemand = "demand")

.param_ref_min_rows <- function(source = FALSE) {
  if (source) return(getOption("energyRt.ondisk_min_rows", 1e5))
  getOption("energyRt.param_ref_min_rows", 1e6)
}

#' Is a parameter's table one to reference rather than copy?
#' @keywords internal
#' @noRd
.param_ref_eligible <- function(param, nrow) {
  type <- as.character(param@type)
  if (!type %in% c("numpar", "bounds", "map")) return(FALSE)
  source <- param@name %in% names(.param_source_classes)
  !is.na(nrow) && nrow >= .param_ref_min_rows(source)
}

# the table of a parameter, wherever it is, canonical column classes
.param_table <- function(param) {
  d <- get_data_slot(param)
  if (is.null(d) || !nrow(d)) return(NULL)
  force_cols_classes(d)
}

# the model objects of a class, by name (repositories walked)
.model_object_names <- function(model, class) {
  if (is.null(model)) return(character())
  objs <- tryCatch(.model_objects(model), error = function(e) list())
  nm <- vapply(objs, function(o) {
    if (methods::is(o, class) && methods::.hasSlot(o, "name")) o@name else NA_character_
  }, character(1))
  nm[!is.na(nm)]
}

.param_origin <- function(param, scen) {
  pname <- param@name
  st <- scen@status
  pipeline <- list(sparse = isTRUE(st$sparse), folded = st$folded,
                   pruned = isTRUE(st$pruned), drop_default = st$drop_default)
  if (pname %in% names(.param_source_classes)) {
    cls <- unname(.param_source_classes[[pname]])
    return(list(kind = "source", parameter = pname, class = cls,
                objects = .model_object_names(scen@model, cls),
                pipeline = pipeline))
  }
  list(kind = "derived", parameter = pname, pipeline = pipeline)
}

#' Reference the large tables of a modInp into the dataset store
#'
#' Hashes every eligible table, finds the version by content or stores it,
#' and puts the reference on the parameter. The parameter's table itself is
#' left in place for the caller to drop or keep: `save_scenario()` skips
#' writing a referenced table into the scenario's own store.
#'
#' @param mi The `modInp`.
#' @param scen The scenario (for the origin).
#' @param format Storage format for a version written now.
#' @return `list(modInp =, entries =)`: the modInp with references set, and
#'   the manifest rows (`object`, `slot`, `via`, `name`, `hash`, `kind`,
#'   `source`, `path`) of the references.
#' @keywords internal
#' @noRd
.thin_param_tables <- function(mi, scen, format = get_storage_format(),
                               verbose = TRUE) {
  entries <- list()
  for (pname in names(mi@parameters)) {
    p <- mi@parameters[[pname]]
    ref <- p@misc$param_ref
    if (!is.null(ref)) {
      # referenced and unchanged: the manifest lists it again
      entries[[length(entries) + 1L]] <- list(
        object = pname, slot = "data", via = "parameter",
        name = ref$name, hash = ref$hash, kind = "table", source = "ref", path = ref$path)
      next
    }
    n <- .param_nrow(p)
    if (!.param_ref_eligible(p, n)) next
    tbl <- .param_table(p)
    if (is.null(tbl)) next
    h <- dataset_hash(tbl)
    hit <- .dataset_store_find_hash(h)
    if (is.null(hit)) {
      info <- save_dataset(tbl, pname, format = format, verbose = FALSE, hash = h,
                           memo = paste0("interpolated table of ", scen@name))
      hit <- list(name = info$name, path = info$path)
    }
    ref <- list(name = hit$name, hash = h, kind = "table", source = "ref",
                path = .registry_rel_path(hit$path), nrow = nrow(tbl),
                filter = NULL, origin = .param_origin(p, scen),
                resolved = .param_version_dir(hit$path, h))
    p@misc$param_ref <- ref
    # the store version is now the table's home: a copy in the scenario's
    # own store would double the disk and win over the reference at load
    local <- p@misc$path
    if (isOnDisk(p) && !is.null(local) && dir.exists(fp(local, "data"))) {
      unlink(fp(local, "data"), recursive = TRUE, force = TRUE)
    }
    mi@parameters[[pname]] <- p
    entries[[length(entries) + 1L]] <- list(
      object = pname, slot = "data", via = "parameter",
      name = ref$name, hash = h, kind = "table", source = "ref", path = ref$path)
    if (isTRUE(verbose)) {
      message("Parameter ref: '", pname, "' -> ", hit$name, "@", substr(h, 1, 8),
              " (", format(nrow(tbl), big.mark = ","), " rows)")
    }
  }
  list(modInp = mi, entries = entries)
}

# rows of a parameter's table without reading an on-disk one
.param_nrow <- function(p) {
  if (isOnDisk(p)) {
    d <- p@misc$onDisk$data$dim
    if (!is.null(d)) return(as.integer(d[1]))
    return(NA_integer_)
  }
  nrow(p@data) + sum(vapply(.pending_chunks(p), nrow, integer(1)))
}

# the version directory of an entry, for a hash
.param_version_dir <- function(entry, hash) {
  mf <- tryCatch(yaml::read_yaml(fp(entry, "dataset.yml")), error = function(e) NULL)
  if (is.null(mf)) return(NULL)
  v <- tryCatch(.dataset_version(entry, mf, hash), error = function(e) NULL)
  if (is.null(v)) NULL else gsub("[\\/]+", "/", v$dir)
}

#' Resolve a parameter's reference to the directory holding its rows
#' @return The directory, or `NULL` when the reference cannot be resolved.
#' @keywords internal
#' @noRd
.param_ref_data_dir <- function(p) {
  ref <- p@misc$param_ref
  if (is.null(ref)) return(NULL)
  r <- ref$resolved
  if (!is.null(r) && dir.exists(r)) return(r)
  entry <- tryCatch(.dataset_store_resolve(ref$name, ref$hash), error = function(e) NULL)
  if (is.null(entry) && !is.null(ref$path)) {
    cand <- fp(dirname(get_registry_file()), ref$path)
    if (file.exists(fp(cand, "dataset.yml"))) entry <- cand
  }
  if (is.null(entry)) return(NULL)
  .param_version_dir(entry, ref$hash)
}

#' The directory a parameter's rows are read from
#'
#' The scenario's own store when the table was written there (a writer ran
#' after the reference, or the table is not referenced); else the store
#' version the reference names.
#' @keywords internal
#' @noRd
.en_param_data_dir <- function(p) {
  local <- p@misc$path
  if (!is.null(local) && dir.exists(fp(local, "data"))) return(fp(local, "data"))
  .param_ref_data_dir(p)
}

#' Empty the in-memory tables a scenario holds that are already persisted
#'
#' A parameter whose rows are on disk (the scenario's store or a referenced
#' version) can be read back lazily at any time; once the matrix is
#' written, keeping its table in memory through a solve only holds RAM.
#' Tables that exist nowhere else (an unsaved scenario, pending chunks) are
#' kept.
#' @keywords internal
#' @noRd
.shed_persisted_tables <- function(scen) {
  mi <- scen@modInp
  for (pname in names(mi@parameters)) {
    p <- mi@parameters[[pname]]
    if (!isOnDisk(p) || is.null(p@data) || !nrow(p@data)) next
    if (length(.pending_chunks(p))) next
    if (is.null(.en_param_data_dir(p))) next
    p@data <- reset_slot(p@data)
    mi@parameters[[pname]] <- p
  }
  scen@modInp <- mi
  scen
}

#' Drop a parameter's reference: its table is about to change
#' @keywords internal
#' @noRd
.param_ref_clear <- function(p) {
  if (!is.null(p@misc$param_ref)) p@misc$param_ref <- NULL
  p
}

#' Resolve the parameter references of a loaded scenario
#'
#' Path level only, no table is read: a local table wins (a writer ran after
#' the reference), else the store version by name and hash, else the
#' recorded entry path when it holds the version, else a source table is
#' recomputed from the model, else an error of class
#' `energyRt_param_ref_missing` (a message and `FALSE` with
#' `ignore_errors = TRUE`).
#' @keywords internal
#' @noRd
.resolve_param_refs <- function(scen, ignore_errors = FALSE, verbose = TRUE) {
  mi <- scen@modInp
  for (pname in names(mi@parameters)) {
    p <- mi@parameters[[pname]]
    ref <- p@misc$param_ref
    if (is.null(ref)) next
    local <- p@misc$path
    if (!is.null(local) && dir.exists(fp(local, "data"))) {
      p@misc$param_ref <- NULL
      mi@parameters[[pname]] <- p
      next
    }
    p@misc$param_ref$resolved <- NULL
    dir <- .param_ref_data_dir(p)
    if (is.null(dir) && identical(ref$origin$kind, "source")) {
      dir <- tryCatch(.recompute_source_param(scen, pname, verbose = verbose),
                      error = function(e) NULL)
    }
    if (is.null(dir)) {
      msg <- paste0("Parameter '", pname, "' of scenario '", scen@name,
                    "' references dataset '", ref$name, "@", substr(ref$hash, 1, 8),
                    "', which was not found in the dataset store ('",
                    get_datasets_path(), "').\n",
                    "  Run refresh_registry() to rescan, or restore the datasets/ ",
                    "folder this scenario was saved with.")
      if (!ignore_errors) {
        stop(errorCondition(msg, class = c("energyRt_param_ref_missing", "energyRt_error")))
      }
      if (verbose) message(msg)
      return(FALSE)
    }
    p@misc$param_ref$resolved <- dir
    mi@parameters[[pname]] <- p
  }
  scen@modInp <- mi
  scen
}

#' Recompute a source table from the model and restore its store version
#'
#' For `pWeather` and `pDemand`: the objects named in the reference's origin
#' are taken from the scenario's model and run through the same steps the
#' interpolation applies to that one parameter; the result is checked
#' against the reference's hash, stored as that version, and its directory
#' returned. A drifted result is stored under its own hash with a warning.
#' @keywords internal
#' @noRd
.recompute_source_param <- function(scen, pname, verbose = TRUE) {
  p <- scen@modInp@parameters[[pname]]
  ref <- p@misc$param_ref
  origin <- ref$origin
  if (!identical(origin$kind, "source")) {
    stop("Parameter '", pname, "' is derived; it cannot be recomputed from the model.")
  }
  if (isFALSE(origin$pipeline$sparse)) {
    stop("Parameter '", pname, "' was interpolated with sparse = FALSE; ",
         "recomputing it is not supported. save_dataset() the table to restore it.")
  }
  if (is.null(scen@model)) stop("The scenario carries no model to recompute '", pname, "' from.")
  objs <- Filter(function(o) methods::is(o, origin$class) &&
                   methods::.hasSlot(o, "name") && o@name %in% origin$objects,
                 .model_objects(scen@model))
  if (!length(objs)) {
    stop("None of the ", origin$class, " objects '", paste(origin$objects, collapse = "', '"),
         "' are in the scenario's model.")
  }
  # a throwaway scenario holding this parameter alone, run through the
  # steps of the interpolation that shape it
  s2 <- scen
  fresh <- new("modInp")
  keep <- intersect(names(s2@modInp@parameters), c(pname, .param_membership_maps(pname)))
  s2@modInp@parameters <- s2@modInp@parameters[keep]
  s2@modInp@parameters[[pname]] <- fresh@parameters[[pname]]
  s2@modInp <- mark_inMemory(s2@modInp)
  for (o in objs) s2 <- ob2mi(s2, o, list())
  s2 <- .flush_pending_parameters(s2)
  s2 <- interpolate_parameters(s2, drop_default = isTRUE(origin$pipeline$drop_default))
  s2 <- .filter_params_by_declared_timeslices(s2)
  s2 <- .filter_params_by_declared_regions(s2)
  if (isTRUE(origin$pipeline$pruned)) s2 <- drop_default_values(s2)
  fd <- origin$pipeline$folded
  if (length(fd) && !isFALSE(fd)) s2 <- fold_parameters(s2, dims = fd)
  tbl <- .param_table(s2@modInp@parameters[[pname]])
  if (is.null(tbl)) stop("Recomputing '", pname, "' produced no rows.")
  h <- dataset_hash(tbl)
  if (!identical(h, ref$hash)) {
    warning("Recomputing '", pname, "' from the model gives a table that differs ",
            "from the one the scenario was saved with (", substr(h, 1, 8), " vs ",
            substr(ref$hash, 1, 8), "); the fresh table is stored and used.",
            call. = FALSE)
  }
  info <- save_dataset(tbl, ref$name, verbose = FALSE,
                       memo = paste0("recomputed for ", scen@name))
  if (verbose) message("Recomputed '", pname, "' from the model: ", info$name, "@", substr(h, 1, 8))
  .param_version_dir(info$path, h)
}

# the membership maps the fold of a parameter reads (none known: the fold
# helpers look them up by name and tolerate their absence)
.param_membership_maps <- function(pname) character()
