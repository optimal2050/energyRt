# =============================================================================#
# dataset_store.R — content-addressed dataset store (the fourth storage tier:
# datasets / repositories / models / scenarios).
#
# `datasets/<name>@<hash8>/` holds one dataset version: a `dataset.yml`
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
#' data.frames (weather/demand tables), `geoscales::Geoscale` maps, and any
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
         "e.g. \"energyRt::utopia_profiles\".")
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

# What kind of payload this is, and how it is stored.
.dataset_kind <- function(x) if (is.data.frame(x)) "table" else "object"

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

# Read a payload back per its manifest. Tables are coerced to the recorded
# class and column types (parquet does not round-trip e.g. integer years
# by itself).
.dataset_read_payload <- function(path, mf) {
  if (identical(mf$kind, "table") ||
      (identical(mf$kind, "function") && dir.exists(fp(path, "data")))) {
    d <- en_open_dataset(fp(path, "data")) |> dplyr::collect()
    d <- as.data.frame(d)
    for (cn in intersect(names(mf$cols %||% list()), names(d))) {
      cl <- mf$cols[[cn]]
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
    cls <- unlist(mf$payload_class %||% list())
    if ("data.table" %in% cls) d <- data.table::as.data.table(d)
    # normalize the attribute order to R's construction order — serialization
    # (and thus repository_hash/model_hash of an owner holding this table)
    # is attribute-order sensitive, and arrow's collect() orders differently
    at <- attributes(d)
    ord <- intersect(c("names", "row.names", "class"), names(at))
    attributes(d) <- at[c(ord, setdiff(names(at), ord))]
    return(d)
  }
  if (file.exists(fp(path, "payload.rds"))) {
    return(readRDS(fp(path, "payload.rds")))
  }
  NULL
}

#' Save a dataset to / load a dataset from the dataset store
#'
#' @description
#' `save_dataset()` writes a payload — a data.frame (a weather or demand
#' series), a `geoscales::Geoscale` map, or any R object — into the
#' content-addressed store `<datasets_path>/<name>@<hash8>/` (see
#' [get_datasets_path()]). Saving identical content again is a no-op. A
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
#' @param x the payload to store (omit for a functional dataset).
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
save_dataset <- function(
    x,
    name,
    fun = NULL,
    args = NULL,
    materialize = TRUE,
    path = NULL,
    registry = TRUE,
    format = get_arrow_format(),
    overwrite = FALSE,
    memo = "",
    verbose = TRUE) {
  stopifnot(is.character(name), length(name) == 1L, nzchar(name))
  format <- if (tolower(format) %in% c("feather", "arrow", "ipc")) {
    "parquet"
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
    h <- dataset_hash(x)
  }
  h8 <- substr(h, 1, 8)
  if (is.null(path)) {
    path <- fp(get_datasets_path(), paste0(.path_slug(name), "@", h8))
  }
  path <- gsub("[\\/]+", "/", path)

  mf_path <- fp(path, "dataset.yml")
  if (file.exists(mf_path) && !overwrite) {
    prev <- tryCatch(yaml::read_yaml(mf_path), error = function(e) NULL)
    if (!is.null(prev) && identical(prev$hash, h)) {
      if (verbose) {
        message("Dataset '", name, "' (", h8,
                ") is already in the store: ", path)
      }
      return(invisible(list(name = name, hash = h, kind = prev$kind,
                            path = path)))
    }
  }

  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  payload_fields <- if (!is.null(fun) && !isTRUE(materialize)) {
    list(kind = "function", format = "none")
  } else {
    pf <- .dataset_write_payload(x, path, format)
    if (!is.null(fun)) pf$kind <- "function"
    pf
  }
  kind <- payload_fields$kind

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
    list(created = .registry_now(),
         energyRt_version = as.character(utils::packageVersion("energyRt")),
         memo = memo)
  ), mf_path)
  if (verbose) {
    cat("Dataset '", name, "' (", h8, ", ", kind, ") saved in '", path,
        "'\n", sep = "")
  }

  if (isTRUE(registry)) {
    tryCatch({
      reg <- load_registry()
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
    reg <- load_registry()
    r <- find_registry(reg, type = "dataset", hash = hash)
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
    if (!is.null(mf) && identical(mf$hash, hash)) {
      return(list(name = mf$name, path = gsub("[\\/]+", "/", d)))
    }
  }
  NULL
}

# The payload of a resolved store entry. Reference resolution prefers the
# snapshot: the owner's hash was computed over materialized content, and a
# re-evaluation that drifted would silently break that identity.
.dataset_payload <- function(path, mf, prefer_snapshot = TRUE,
                             strict = FALSE, verbose = TRUE) {
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
  x <- .dataset_payload(path, mf, prefer_snapshot = !isTRUE(evaluate),
                        strict = strict, verbose = verbose)
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
    path <- .dataset_store_resolve(ref$name, ref$hash)
    if (is.null(path) && !is.null(ref$path)) {
      cand <- fp(dirname(get_registry_file()), ref$path)
      if (file.exists(fp(cand, "dataset.yml"))) path <- cand
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
                                strict = strict, verbose = FALSE)
    methods::slot(obj, slot_name) <- payload
  }
  obj@misc[["dataset_ref"]] <- NULL
  obj
}
