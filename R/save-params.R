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

#' Hash the large tables of a modInp and reference those of a sealed base
#'
#' A scenario built on a sealed base (`interpolate_model(base = )`) references
#' every large table identical to the base's -- by content hash -- instead of
#' storing a copy. A table the base itself references is referenced where it
#' lives. Every other table stays in the scenario's own store; its hash is
#' recorded so a scenario built on this one can match it.
#'
#' @param mi The `modInp`.
#' @param scen The scenario (its `misc$base` and the origin of a table).
#' @return `list(modInp =, entries =, tables =)`: the modInp with references
#'   set, the manifest rows of the references, and `tables` -- the hash and
#'   row count of each large table kept in the scenario's own store.
#' @keywords internal
#' @noRd
.thin_param_tables <- function(mi, scen, format = get_storage_format(),
                               verbose = TRUE) {
  entries <- list()
  tables <- list()
  base <- .sealed_base(scen, verbose = verbose)
  for (pname in names(mi@parameters)) {
    p <- mi@parameters[[pname]]
    if (!is.null(p@misc$rule)) {                      # a rule holds no table
      n0 <- .param_nrow(p)
      if (is.na(n0) || n0 == 0L) next
    }
    ref <- p@misc$param_ref
    if (!is.null(ref)) {
      # referenced and unchanged: the manifest lists it again
      entries[[length(entries) + 1L]] <- .param_ref_entry(pname, ref)
      next
    }
    n <- .param_nrow(p)
    if (!.param_ref_eligible(p, n)) next
    tbl <- .param_table(p)
    if (is.null(tbl)) next
    h <- dataset_hash(tbl)
    hit <- if (is.null(base)) NULL else base$tables[[pname]]
    if (!is.null(hit) && identical(hit$hash, h)) {
      ref <- base$refs[[pname]] %||% list(
        kind = "table", source = "scenario", scenario = base$name,
        path = base$rel, parameter = pname, hash = h, nrow = nrow(tbl),
        filter = NULL)
    } else {
      # a sample of the base (calendar or regions): the base's table filtered
      # to the sample, when that is this table
      ref <- if (!is.null(hit) && is.null(base$refs[[pname]]) &&
                 length(scen@misc$sample)) {
        .filtered_base_ref(p, tbl, base, pname, scen@misc$sample)
      } else {
        NULL
      }
      if (is.null(ref)) {
        tables[[pname]] <- list(hash = h, nrow = nrow(tbl))
        next
      }
    }
    ref$origin <- .param_origin(p, scen)
    ref$resolved <- .param_ref_data_dir(p, ref)
    p@misc$param_ref <- ref
    # the base's table is this table's home: a copy in the scenario's own
    # store would double the disk and win over the reference at load
    local <- p@misc$path
    if (isOnDisk(p) && !is.null(local) && dir.exists(fp(local, "data"))) {
      unlink(fp(local, "data"), recursive = TRUE, force = TRUE)
    }
    mi@parameters[[pname]] <- p
    entries[[length(entries) + 1L]] <- .param_ref_entry(pname, ref)
    if (isTRUE(verbose)) {
      message("Parameter ref: '", pname, "' -> ",
              if (identical(ref$source, "scenario")) "scenario " else "dataset ",
              ref$scenario %||% ref$name, " (", format(nrow(tbl), big.mark = ","),
              " rows)")
    }
  }
  list(modInp = mi, entries = entries, tables = tables)
}

# A reference to a base scenario's table filtered to a sample, when the
# filtered rows are exactly `tbl` (in any order); NULL otherwise. A table
# with no sampled column is matched whole.
.filtered_base_ref <- function(p, tbl, base, pname, sample) {
  ts <- sample$timeslice
  rg <- sample$region
  filter <- c(if (length(ts)) list(timeslice = ts, timeslicep = ts),
              if (length(rg)) list(region = rg, regionp = rg, src = rg, dst = rg))
  # no sampled column: the whole table, in another row order
  filter <- filter[intersect(names(filter), colnames(tbl))]
  if (!length(filter)) filter <- NULL
  ref <- list(kind = "table", source = "scenario", scenario = base$name,
              path = base$rel, parameter = pname, nrow = nrow(tbl),
              filter = filter)
  dir <- .param_ref_data_dir(p, ref)
  if (is.null(dir)) return(NULL)
  b <- tryCatch(en_open_dataset(dir) |> .en_apply_filter(filter, keep_na = TRUE) |>
                  dplyr::collect(), error = function(e) NULL)
  if (is.null(b) || nrow(b) != nrow(tbl)) return(NULL)
  b <- as.data.frame(force_cols_classes(as.data.frame(b)))[, colnames(tbl), drop = FALSE]
  if (!identical(.hash_unordered(b), .hash_unordered(tbl))) return(NULL)
  ref$hash <- dataset_hash(b)
  ref
}

# Content hash of a table regardless of its row order.
.hash_unordered <- function(d) {
  d <- as.data.frame(d)
  d <- d[do.call(order, unname(lapply(d, as.character))), , drop = FALSE]
  rownames(d) <- NULL
  dataset_hash(d)
}

# The manifest row of a parameter reference.
.param_ref_entry <- function(pname, ref) {
  out <- list(object = pname, slot = "data", via = "parameter",
              name = ref$name %||% pname, hash = ref$hash, kind = "table",
              source = ref$source %||% "ref", path = ref$path)
  if (identical(ref$source, "scenario")) {
    out$scenario <- ref$scenario
    out$parameter <- ref$parameter %||% pname
  }
  if (length(ref$filter)) out$filter <- ref$filter
  out
}

# The sealed base a scenario was built on, as its manifest describes it:
# `tables` (hash of every large table, kept or referenced) and `refs` (the
# base's own references, followed so a reference points where a table lives).
# NULL when the scenario has no base or the base is not saved and sealed.
.sealed_base <- function(scen, verbose = FALSE) {
  b <- scen@misc$base
  if (is.null(b) || !nzchar(b$path %||% "")) return(NULL)
  path <- .abs_store_path(b$path)
  mf_path <- fp(path, "scenario.yml")
  if (!file.exists(mf_path)) return(NULL)
  mf <- tryCatch(yaml::read_yaml(mf_path), error = function(e) NULL)
  if (is.null(mf)) return(NULL)
  if (!isTRUE(mf$sealed)) {
    if (isTRUE(verbose)) {
      message("Base scenario '", b$name, "' is not sealed: its tables are ",
              "copied, not referenced. seal_scenario() it to share them.")
    }
    return(NULL)
  }
  tables <- mf$tables %||% list()
  refs <- list()
  for (e in mf$datasets %||% list()) {
    if (!identical(e$via, "parameter")) next
    tables[[e$object]] <- list(hash = e$hash)
    refs[[e$object]] <- if (identical(e$source, "scenario")) {
      list(kind = "table", source = "scenario", scenario = e$scenario,
           path = e$path, parameter = e$parameter %||% e$object, hash = e$hash,
           filter = e$filter)
    } else {
      list(name = e$name, hash = e$hash, kind = "table", source = "ref",
           path = e$path, filter = e$filter)
    }
  }
  list(name = as.character(mf$name %||% b$name), path = path,
       rel = .registry_rel_path(path), tables = tables, refs = refs)
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
.param_ref_data_dir <- function(p, ref = p@misc$param_ref) {
  if (is.null(ref)) return(NULL)
  if (identical(ref$source, "scenario")) {
    # the table where the base scenario stores it
    d <- fp(.abs_store_path(ref$path), "modInp", "parameters",
            ref$parameter %||% p@name, "data")
    return(if (dir.exists(d)) gsub("[\\/]+", "/", d) else NULL)
  }
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
#' kept. A referenced table read back into memory is marked on disk again,
#' so its readers resolve the reference.
#' @keywords internal
#' @noRd
.shed_persisted_tables <- function(scen) {
  mi <- scen@modInp
  for (pname in names(mi@parameters)) {
    p <- mi@parameters[[pname]]
    if (is.null(p@data) || !nrow(p@data)) next
    if (length(.pending_chunks(p))) next
    referenced <- !is.null(p@misc$param_ref) && !is.null(getObjPath(p)) &&
      !is.null(.param_ref_data_dir(p))
    if (!isOnDisk(p) && !referenced) next
    if (is.null(.en_param_data_dir(p))) next
    p@data <- reset_slot(p@data)
    if (referenced) p@misc$inMemory <- FALSE
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
      msg <- if (identical(ref$source, "scenario")) paste0(
        "Parameter '", pname, "' of scenario '", scen@name,
        "' references the table of base scenario '", ref$scenario, "' ('",
        ref$path, "'), which was not found. Restore that scenario, or re-save ",
        "this one from a session that holds the table.") else
        paste0("Parameter '", pname, "' of scenario '", scen@name,
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
  # the built model names its objects as the sets do (one weather object per
  # tier or transform); the origin may list them as the model gives them
  set_nm <- c(weather = "weather", demand = "dem")[origin$class]
  want <- union(origin$objects, as.character(scen@modInp@sets[[set_nm]]))
  objs <- Filter(function(o) methods::is(o, origin$class) &&
                   methods::.hasSlot(o, "name") && o@name %in% want,
                 .model_objects(.scen_build_model(scen)))
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
