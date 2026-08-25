# Saved on disk scenario and model objects have the same structure as in memory  objects. The only difference is that data-frame slots are saved in `parquet`  format, while other "large" slots are saved in `RData` format.  Saved on disk objects have the same structure of directories and files as in  memory objects. The in-memory object stores the information about the path to  each slot on disk, the dimensions of the data-frame slots, the file format, and the file name. This information is stored in the `@misc` slot of the in-memory object. The `inMemory` slot of the object is set to `FALSE` to indicate that the object is saved on disk. The scenario or model object itself with stored  on disk slots is saved in the same directory as `scen.RData` or `mod.RData`. The access to the saved on disk slots is provided by the `getData` function. (toDo: add getSlot function and/or `slot` method)
# The structure of the information stored in the `@misc` slot of the in-memory part of an object is a nested `list`, where every level corresponds to a directory. The last level of the nested list contains the information about the file format, the file name, and the dimensions of the data-frame slots or length of the vector slot.


#' Save scenario object on disk in parquet format using `arrow` package.
#'
#' @param scen scenario object.
#' @param path character. Path to scenario directory.
#' @param embed_model `NULL` (default), `TRUE`, or `FALSE`. Controls whether
#'   the model is embedded in the saved scenario or referenced from the model
#'   store (see [save_model()]). `NULL`: reference when the identical model
#'   content is already in the store, embed otherwise. `TRUE`: always embed
#'   (self-contained folder). `FALSE`: require a store hit, error otherwise.
#'   A referenced save stores only `{name, hash, path}`; [load_scenario()]
#'   resolves it back via the registry / model store.
#' @param format file format (currently `parquet` only, arrow or feather will be implemented in further releases).
#' @param overwrite logical. Overwrite existing scenario directory.
#' @param clean_start logical. Clean scenario directory before saving.
#' @param write_log logical. Write (update) logfile.
#' @param verbose logical. Print messages.
#'
#' @return scenario object with most of the slots saved on disk.
#' @export
#'
#' @examples
#' \dontrun{
#' scen_BASE@path # check the scenarion directory
#' scen_BASE <- save_scenario(scen_BASE) # saving in the default directory
#' }
save_scenario <- function(
    scen,
    path = scen@path,
    embed_model = NULL,
    format = get_arrow_format(),
    overwrite = TRUE,
    clean_start = FALSE,
    write_log = TRUE,
    verbose = TRUE) {
  # On-disk STORAGE format. The Arrow exchange default is "feather", but feather
  # datasets are neither compressible nor lazily readable via write_dataset /
  # en_open_dataset, so any Arrow request is stored as parquet (compressed with
  # the global arrow_compression / arrow_compression_level options). "csv" stays
  # csv. (Exchange with the solvers still uses feather; see get_arrow_format().)
  format <- if (tolower(format) %in% c("feather", "arrow", "ipc")) {
    "parquet"
  } else {
    tolower(format)
  }
  # identify directories
  if (is.null(path)) {
    scen@path <- fp(get_scenarios_path(), scen@name)
    message("Scenarios directory: ", scen@path)
  } else {
    scen@path <- path
  }

  # An on-disk scenario (interpolate_model(ondisk = TRUE)) already has its
  # parquet stores in place — but the thin object, the layout markers, the
  # manifest, and the registry row still need writing, otherwise the folder
  # has data and no `scen.RData` and load_scenario() fails. So an on-disk
  # scenario skips only the obj2disk() data walk below, never the shell.
  ondisk_data <- isOnDisk(scen)

  # Model reference vs embedding. `embed_model = NULL` (auto): reference the
  # model store entry when this exact content is already stored (see
  # save_model()); embed otherwise. TRUE forces embedding; FALSE requires a
  # store hit. A referenced save replaces the model in the SAVED copy with a
  # stub carrying `misc$model_ref`; the returned in-memory object keeps its
  # model. Note a thinned (on-disk) model hashes differently from its
  # in-memory original, so ondisk scenarios normally embed.
  model_ref <- NULL
  model_hash_ <- ""
  if (!isTRUE(embed_model) && nzchar(scen@model@name %||% "")) {
    model_hash_ <- tryCatch(model_hash(scen@model), error = function(e) "")
    store_dir <- if (nzchar(model_hash_)) {
      tryCatch(.model_store_resolve(scen@model@name, model_hash_),
               error = function(e) NULL)
    }
    if (!is.null(store_dir)) {
      model_ref <- list(name = scen@model@name, hash = model_hash_,
                        source = "ref",
                        path = .registry_rel_path(store_dir))
    } else if (isFALSE(embed_model)) {
      stop("embed_model = FALSE, but model '", scen@model@name, "' (",
           substr(model_hash_, 1, 8), ") is not in the model store ('",
           get_models_path(), "'). save_model() it first, or use ",
           "embed_model = NULL/TRUE.")
    }
  }
  full_model <- NULL
  if (!is.null(model_ref)) {
    full_model <- scen@model
    stub <- new("model")
    stub@name <- scen@model@name
    stub@misc$model_ref <- model_ref
    scen@model <- stub
    if (verbose) {
      cat("Model '", model_ref$name, "' referenced from the store (",
          substr(model_ref$hash, 1, 8), "), not embedded\n", sep = "")
    }
  }

  # sourceCode blocks identical to the package's own templates are dropped
  # from the saved copy (~0.5 MB per scenario) and restored from the current
  # package on load; user-overridden blocks fail the identity test and are
  # kept verbatim.
  sourceCode_full <- scen@settings@sourceCode
  if (length(sourceCode_full)) {
    same <- vapply(
      names(sourceCode_full),
      function(k) identical(sourceCode_full[[k]], .modelCode[[k]]),
      logical(1)
    )
    if (any(same)) {
      scen@settings@sourceCode[names(sourceCode_full)[same]] <- NULL
      scen@misc$sourceCode_default <- names(sourceCode_full)[same]
    }
  }

  tictoc::tic("save_scenario")
  # clean directories
  if (clean_start) {
    if (verbose) message("Cleaning directory '", scen@path, "'")
    if (write_log) {
      ff <- list.files(scen@path, include.dirs = TRUE)
      ff <- ff[!(ff == "logfile.csv")]
      clear_status <- unlink(fp(scen@path, ff),
        recursive = TRUE,
        force = TRUE
      )
      if (clear_status != 0) {
        stop(
          "Cannot delete content of'", scen@path,
          "' directory"
        )
      }
      rm(ff)
    } else {
      clear_status <- unlink(fp(scen@path),
        force = TRUE,
        recursive = TRUE
      )
      if (clear_status != 0) stop("Cannot delete '", scen@path, "' directory")
    }
  }

  # create scenario directories
  if (!dir.exists(scen@path)) {
    if (verbose) cat("Creating directory '", scen@path, "'\n")
    dir.create(scen@path, recursive = TRUE)
  }

  # write format and log
  format_file <- fp(scen@path, "format")
  write(format, format_file, append = FALSE)
  class_file <- fp(scen@path, "class")
  write(class(scen), class_file, append = FALSE)
  # Directory-layout version. Bumped to 2 when `modOut@variables` became a list
  # of `variable` objects, moving each variable's data from
  # `variables/<v>/` down to `variables/<v>/data/`. Without the marker, new code
  # reading an old directory finds no `data/` sub-directory, and
  # `get_lazy_data()` returns NULL rather than erroring -- every result would
  # silently come back empty.
  write(as.character(.SCENARIO_LAYOUT), fp(scen@path, "layout"), append = FALSE)
  log_file <- (fp(scen@path, "logfile.csv"))
  write(paste(lubridate::now(tzone = "UTC"), "format", format, sep = ","),
    file = log_file, append = TRUE
  )

  if (!ondisk_data) {
    if (verbose) {
      cat("Saving large slots of scenario object",
        " '", scen@name, "' ", "on disk\n",
        sep = ""
      )
    }
    # Layout 3: the solution store belongs to its run — with an active run,
    # modOut is written under runs/[<variant>/]<solve>/modOut instead of the
    # top-level modOut/ (which remains the fallback for solved scenarios that
    # predate run records, e.g. loaded from layout 2). An OWN-PROBLEM VARIANT
    # additionally owns its modInp: the store goes to runs/<variant>/modInp/
    # (the scenario-level modInp/ stays the base problem's), plus the
    # variant.yml manifest and the problem swap file. The base problem gets
    # its symmetric swap file <scen>/problem.RData, enabling
    # read_solution(run=) to switch problems in both directions.
    run_label <- scen@misc$run %||% ""
    active_variant <- .run_variant(scen)
    mo <- NULL
    if (nzchar(run_label)) {
      mo <- scen@modOut
      scen@modOut <- new("modOut") # keep the main walk off the modOut slot
    }
    mi <- NULL
    if (nzchar(active_variant)) {
      mi <- scen@modInp
      scen@modInp <- new("modInp") # keep the main walk off the modInp slot
    }
    scen <- obj2disk(
      scen,
      path = scen@path,
      format = format,
      verbose = verbose
    )
    if (!is.null(mi)) {
      scen@modInp <- obj2disk(
        mi,
        path = fp(scen@path, "runs", active_variant, "modInp"),
        format = format,
        verbose = verbose
      )
    }
    if (!is.null(mo)) {
      scen@modOut <- obj2disk(
        mo,
        path = fp(.run_dir(scen, active_variant, run_label), "modOut"),
        format = format,
        verbose = verbose
      )
    }
    # swap file for the ACTIVE problem (variant.RData / problem.RData)
    .write_problem_swap(scen, scen@modInp)
  } else {
    if (verbose) {
      cat("Scenario data already on disk; refreshing the scenario shell ",
          "(scen.RData, manifest, registry)\n", sep = "")
    }
    # The data walk is skipped, but a solve AFTER an ondisk interpolation
    # leaves the solution in memory — park it into its run store so the thin
    # scen.RData stays thin.
    if (nzchar(.run_variant(scen))) {
      # A re-save of an already-stored variant is fine (its modInp store sits
      # under the variant dir). What is NOT supported is a variant problem
      # whose store was written to the scenario level by
      # interpolate_model(ondisk = TRUE) — that location belongs to the base.
      mi_path <- gsub("[\\/]+", "/", getObjPath(scen@modInp) %||% "")
      expected <- gsub("[\\/]+", "/", .problem_modinp_root(scen))
      if (!identical(mi_path, expected)) {
        stop("Own-problem variants of an ondisk-interpolated scenario are ",
             "not supported yet: interpolate_model(ondisk = TRUE) writes ",
             "the parameter store to the scenario level, which belongs to ",
             "the base problem. Interpolate the variant's problem in memory.")
      }
    }
    if (!isOnDisk(scen@modOut) && length(scen@modOut@variables)) {
      run_label <- scen@misc$run %||% ""
      modout_path <- if (nzchar(run_label)) {
        fp(.run_dir(scen, .run_variant(scen), run_label), "modOut")
      } else {
        fp(scen@path, "modOut")
      }
      scen@modOut <- obj2disk(
        scen@modOut,
        path = modout_path,
        format = format,
        verbose = verbose
      )
    }
    .write_problem_swap(scen, scen@modInp)
  }
  # message("Saving the thinned scenario object")
  save(scen, file = fp(scen@path, "scen.RData"))
  mf_model <- model_ref %||% list(
    name = scen@model@name %||% "",
    hash = model_hash_,
    source = "embedded"
  )
  .write_scenario_manifest(scen, format, model = mf_model)
  .registry_record_scenario(scen)
  # the returned in-memory object keeps its full model and sourceCode
  if (!is.null(full_model)) scen@model <- full_model
  scen@settings@sourceCode <- sourceCode_full
  cat("Scenario '", scen@name, "' saved in '", scen@path, "'\n", sep = "")
  dirsize <- dir_size(scen@path)
  cat("Directory size: ", round(dirsize / 1024^2, 2), " MB\n", sep = "")
  scen@misc$dirsize <- dirsize
  # browser()
  if (verbose) tictoc::toc()
  tictoc::tic.clear()
  return(invisible(scen))
}

if (F) {
  getObjPath(scen)
  scen_ondisk <- save_scenario(
    scen = scen,
    path = fp("tmp/scenarios", scen@name),
    verbose = T
  )
  isInMemory(scen_ondisk)
  isOnDisk(scen_ondisk)
  getObjPath(scen_ondisk)
  getObjPath(scen_ondisk@model)
  getObjPath(scen_ondisk@model@data$repo)
  getObjPath(scen_ondisk@model@data[[1]])
  getObjPath(scen_ondisk@modOut)
  scen_ondisk@modOut@misc
  scen_ondisk@modInp@misc
  scen_ondisk@modInp@parameters$region@misc
}

# mem2disk
# mem_to_disk
# disk2mem

data2disk <- function(
    obj,
    path = NULL,
    # format = "parquet",
    format = "csv",
    compression = get_arrow_compression(),
    compression_level = get_arrow_compression_level(),
    verbose = FALSE) {
  # saves certain type of data to disk, returns TRUE if saved, FALSE if not
  if (is.null(path)) path <- getObjPath(obj)
  stopifnot(!is.null(path))
  # dir.create(path, recursive = TRUE, showWarnings = FALSE)
  # browser()
  # obj_class <- class(obj)

  if (inherits(obj, "data.frame")) {
    obj <- as.data.table(obj)
    obj_class <- class(obj)
    # if (verbose) cat(path, format, "\n")
    if (anyDuplicatedSets(obj)) obj <- rename_duplicated_sets(obj)
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
    # Parquet supports compression (zstd/lz4 + level); csv/feather datasets do not
    # take a compression arg in write_dataset.
    if (format == "parquet" && !identical(tolower(compression), "uncompressed")) {
      arrow::write_dataset(obj, path = path, format = "parquet",
                           compression = compression,
                           compression_level = as.integer(compression_level))
    } else {
      arrow::write_dataset(obj, path = path, format = format)
    }
    # write(format, file = fp(path, "format"), append = FALSE)
    # write(obj_class, file = fp(path, "class"), append = FALSE)
    return(invisible(TRUE))
  } else if (inherits(obj, c("character", "numeric", "logical"))) {
    # if (verbose) cat(path, "csv", "\n")
    # if (anyDuplicatedSets(obj)) obj <- rename_duplicated_sets(obj)
    # arrow::write_dataset(obj, path = path, format = "csv")
    # browser()
    obj <- as.data.table(obj)
    data.table::setnames(obj, old = "obj", new = basename(path))
    # fwrite(obj, file = fp(path, "obj.csv"))
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
    arrow::write_dataset(obj, path = path, format = "csv")
    # write(obj_class, file = fp(path, "class"), append = FALSE)
    # write("csv", file = fp(path, "format"), append = FALSE)
    return(invisible(TRUE))
  }
  return(FALSE)
}

obj2disk <- function(
    obj,
    path = NULL,
    # format = "parquet",
    format = "csv",
    save_not_S4 = FALSE,
    force_save = FALSE,
    verbose = FALSE,
    delay = 0) {
  Sys.sleep(delay)
  # identifies which slots of S4 obj are savable,
  # proceeds with saving and wiping the saved slots with marks in @misc
  if (is.null(path)) path <- getObjPath(obj)
  stopifnot(!is.null(path))
  # dir.create(path, recursive = TRUE, showWarnings = FALSE)
  # browser()
  # obj_class <- class(obj)
  # if (inherits(obj, "list")) browser()
  # if (inherits(obj, "modOut")) browser()
  # if (inherits(obj, "weather")) browser()
  if (isOnDisk(obj)) {
    # stopifnot(dir.exists(path))
    # return(obj)
    if (!dir.exists(path)) {
      dir.create(path, recursive = TRUE, showWarnings = FALSE)
    }
  }
  isSaved <- FALSE
  if (isS4(obj)) {
    cl <- class(obj)[1]
    obj <- set_ondisk_slots(obj)
    ondsk <- get_ondisk_slots(obj)
    # browser()
    stopifnot(all(ondsk %in% slotNames(obj)))
    for (s in ondsk) { # slots to save
      if (isS4(slot(obj, s))) {
        # cat("slot ", s, ": \n", sep = "")
        slot(obj, s) <- obj2disk(
          slot(obj, s),
          path = fp(path, s),
          format = format,
          verbose = verbose
        )
        if (isOnDisk(slot(obj, s))) isSaved <- TRUE
      } else if (inherits(slot(obj, s), "list")) {
        # list of S4s (repo@data, modInp@parameters) or data.frames, ...
        if (inherits(obj, "repository")) {
          # cat("repository: '", obj@name, "'\n", sep = "")
          if (verbose) cat("model@data[['", obj@name, "']]\n", sep = "")
        } else if (inherits(obj, "model")) {
          # cat("model@data[['", s, "']]: \n", sep = "")
        } else {
          if (verbose) cat(cl, "@", s, "\n", sep = "")
        }
        # cat(cl, "@", s, ": \n", sep = "")
        nm <- names(obj@misc$onDisk[[s]])
        # dim_list <- vector("list", length(nm)); names(dim_list) <- nm
        # dim_list <- list()
        if (is(obj, "model") & s == "data") {
          make_progress_bar <- FALSE
        } else {
          if (verbose) {
            make_progress_bar <- TRUE
          } else {
            make_progress_bar <- FALSE
          }
        }
        # browser()
        if (make_progress_bar) p <- progressr::progressor(along = nm)
        for (i in nm) { # loop over list
          if (make_progress_bar) p(i)
          if (isS4(slot(obj, s)[[i]])) { # list of S4
            # cat("\n", s, i, "\n")
            slot(obj, s)[[i]] <- obj2disk(
              slot(obj, s)[[i]],
              path = fp(path, s, i),
              save_not_S4 = TRUE,
              format = format,
              verbose = verbose
            )
            # if (inherits(obj, "weather")) browser()
            if (isOnDisk(slot(obj, s)[[i]])) isSaved <- TRUE
          } else { # call data2disk for not S4 elements
            # if (i == "vObjective") browser()
            if (any(obj@misc$onDisk[[s]][[i]]$class %in% "data.frame")) {
              save_i <- obj@misc$onDisk[[s]][[i]]$dim[1] > 0
            } else {
              save_i <- obj@misc$onDisk[[s]][[i]]$length > 0
            }
            if (isTRUE(save_i)) {
              xs <- data2disk(
                # !!! check why not all data.frames are data.tables
                obj = as.data.table(slot(obj, s)[[i]]),
                path = fp(path, s, i),
                format = format,
                verbose = verbose
              )
              if (xs) {
                isSaved <- TRUE
                # dim_list[[i]] <- dim(slot(obj, s)[[i]])
                # browser()
                slot(obj, s)[[i]] <- reset_slot(slot(obj, s)[[i]])
                # slot(obj, s) <- setObjPath(slot(obj, s),
                # path = fp(path, s))
              }
            }
          }
        }
        # save dim_list
      } else { # obj@s slot is not S4
        if (any(obj@misc$onDisk[[s]]$class %in% "data.frame")) {
          save_i <- obj@misc$onDisk[[s]]$dim[1] > 0
        } else {
          save_i <- obj@misc$onDisk[[s]]$length > 0
        }
        if (isTRUE(save_i)) {
          xs <- data2disk(
            obj = slot(obj, s),
            path = fp(path, s),
            format = format,
            verbose = verbose
          )
          if (xs) {
            isSaved <- TRUE
            # browser()
            # store dim
            slot(obj, s) <- reset_slot(slot(obj, s))
            obj <- setObjPath(obj, path = fp(path))
          }
        }
      }
    }
  } else if (save_not_S4) {
    x <- data2disk(
      obj = obj,
      path = fp(path),
      format = format, verbose = verbose
    )
    if (x) {
      isSaved <- TRUE
      # store dim
      obj <- reset_slot(obj)
    }
  }
  # mark if any data is on disk
  # if (inherits(obj, "scenario")) browser()
  if (isSaved) {
    obj <- mark_ondisk(obj)
    obj <- setObjPath(obj, path = path)
    # } else {
    #   obj <- mark_inMemory(obj)
  }
  return(obj)
}

reset_slot <- function(x) {
  if (inherits(x, "data.frame")) {
    return(as.data.table(x)[0, ])
  }
  if (is.vector(x)) {
    return(x[0])
  }
  return(x)
}

if (F) {
  isOnDisk(scen)
  isInMemory(scen)
  scen_ondisk <- obj2disk(scen, fp("scenarios", scen@name), verbose = FALSE)
  isOnDisk(scen_ondisk)
  isInMemory(scen_ondisk)
  size(scen)
  size(scen_ondisk)
  fs::dir_info(fp("scenarios", scen@name), recurse = TRUE)$size |> sum()
  scen_ondisk2 <- obj2disk(scen_ondisk, fp("scenarios", scen@name),
    verbose = T
  )
  isInMemory(scen_ondisk2)
  fs::dir_info(fp("scenarios", scen@name), recurse = TRUE)$size |> sum()
  # obj2disk(scen@modOut, fp("scenarios", scen@name), verbose = TRUE)
}

rename_duplicated_sets <- function(x) {
  # x - table
  # browser()
  stopifnot(inherits(x, "data.frame"))
  nm <- colnames(x)
  # nm <- c("a", "b", "c", "b", "b", "a", "a", "a")
  ii <- duplicated(nm)
  if (any(ii)) {
    all_sets <- unique(nm)
    # !!! add check for numeric endings !!!
    # nm <- c("a", "b", "c", "b2", "b", "a", "a5", "a")
    for (s in all_sets) {
      jj <- nm %in% s
      if (length(nm[jj]) > 1) {
        nm2 <- c(s, paste0(s, seq(2, length(nm[jj]))))
        nm[jj] <- nm2
      }
    }
    colnames(x) <- nm
  }
  x
}

anyDuplicatedSets <- function(x) {
  if (!inherits(x, "data.frame")) {
    return(NULL)
  }
  any(duplicated(colnames(x)))
}

en_open_dataset <- function(path, format = NULL, engine = "arrow") {
  # if (basename(path) == "vObjective") browser()
  # identify format
  ff <- list.files(path)
  ext <- tools::file_ext(ff) |> unique()
  if (is.null(format)) {
    if (all(ext %in% "csv")) {
      format <- "csv"
    } else if (all(ext %in% "parquet")) {
      format <- "parquet"
    } else if (all(ext %in% "RData")) {
      format <- "RData"
    } else {
      stop(
        "Cannot identify format of the dataset\n     ",
        paste0(length(ff), " files or directories, extensions: '"),
        paste(ext, collapse = "', '"), "'"
      )
    }
  } else {
    # !!! check if files are consistent with the format
  }

  if (engine == "arrow") {
    if (format == "csv") {
      return(arrow::open_csv_dataset(path))
    }
    if (format == "parquet") {
      return(arrow::open_dataset(path))
    }
  }
  if (format == "RData") {
    # load
    browser() # not implemented yet
    return(NULL)
  }
}

if (F) {
  p <- "scenarios/base/sets/comm/"
  en_open_dataset(p)
  en_open_dataset("scenarios/base/variables/")
  a <- en_open_dataset("scenarios/base/variables/vTechOut")
  a |>
    filter(value > 0.1) |>
    collect()
}

#' Is object stored in memory?
#'
#' @param obj Object, checks
#'
#' @return Logical value, TRUE if object is stored in memory, FALSE if on disk.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' isInMemory(scen_BASE)
#' }
isInMemory <- function(obj) {
  if (!isS4(obj)) {
    has_path <- try({!is.null(obj$misc$inMemory)}, silent = TRUE)
    if (inherits(has_path, "try-error")) {
      return(TRUE)
    }
    if (is.character(has_path)) {
      return(!is.null(obj$path))
    }
    return(TRUE)
  }
  sts <- slotNames(obj)
  if (any(sts %in% "inMemory")) {
    return(obj@inMemory)
  } else if (any(sts %in% "misc")) {
    if (!is.null(obj@misc$inMemory)) {
      return(obj@misc$inMemory)
    }
  }
  return(TRUE)
}

isOnDisk <- function(obj) {
  !isInMemory(obj)
}

if (F) {
  isInMemory(scen)
  isInMemory(scen@model)
  scen@model@misc$inMemory <- FALSE
  isInMemory(scen@model)
}

getObjPath <- function(obj, path = NULL) {
  if (!isS4(obj)) {
    return(NULL)
  }
  sts <- slotNames(obj)
  if (any(sts %in% "path")) {
    return(obj@path)
  } else if (any(sts %in% "misc")) {
    if (!is.null(obj@misc$path)) {
      return(obj@misc$path)
    }
  }
  return(NULL)
}

setObjPath <- function(obj, path = NULL) {
  if (!isS4(obj)) {
    return(obj)
  }
  sts <- slotNames(obj)
  if (any(sts %in% "path")) {
    obj@path <- path
    return(obj)
  } else if (any(sts %in% "misc")) {
    obj@misc$path <- path
    return(obj)
    # }
  }
  return(obj)
}

if (F) {
  getObjPath(scen)
  getObjPath(scen@model)
  scen@model@misc$path <- "scenarios/base/model"
  getObjPath(scen@model)
}

get_lazy_data <- function(obj,
                          slot = NULL, element = NULL,
                          InMemory = isInMemory(obj),
                          path = NULL,
                          collect_data = TRUE,
                          default = NULL,
                          optional = FALSE
                          ) {
  # browser()
  # check if the object is "inMemory"
  if (InMemory) {
    if (is.null(slot)) {
      x <- obj
    } else {
      if (!.hasSlot(obj, slot)) {
        return(NULL)
      }
      x <- slot(obj, slot)
    }
    if (!is.null(element)) x <- x[[element]]
    return(x)
  }
  if (is.null(path)) path <- getObjPath(obj)
  stopifnot(!is.null(path))
  if (!is.null(slot) && !.hasSlot(obj, slot)) {
    return(NULL)
  }
  path <- paste(c(path, slot, element), collapse = "/")
  if (file.exists(path) || dir.exists(path)) path <- normalizePath(path)
  qu <- try(en_open_dataset(path), silent = TRUE)
  if (inherits(qu, "try-error")) {
    ff <- list.files(path)
    if (length(ff) == 0) {
      # Distinguish "never written" (an empty slot writes no dataset — normal)
      # from "written but not there" (the folder was moved/renamed): the
      # object's own onDisk bookkeeping records what was saved. Silently
      # returning NULL in the second case made every result of a moved
      # scenario come back as zero rows.
      expected <- tryCatch({
        od <- if (.hasSlot(obj, "misc")) obj@misc$onDisk else NULL
        if (!is.null(slot)) od <- od[[slot]]
        if (!is.null(element)) od <- od[[element]]
        isTRUE(od$dim[1] > 0) || isTRUE(od$length > 0)
      }, error = function(e) FALSE)
      if (expected && !optional) {
        stop("On-disk data expected but not found: ", path, "\n",
             "  The scenario/model folder may have been moved or renamed. ",
             "Re-load it with load_scenario() / load_model() to rebase ",
             "its paths.")
      }
      return(NULL)
    }
    stop("Cannot open dataset: ", path, "\n",
        "Files: ", paste(ff, collapse = ", "))
  }
  if (collect_data) {
    qu <- collect(qu)
  }
  if (is.null(qu)) {return(default)}
  return(qu)
}

get_lazy_dim_names <- function(obj, slot = NULL, element = NULL,
                               InMemory = isInMemory(obj),
                               path = NULL) {
  # returns dim and names of the object's slot if available
  # browser()
  # if (obj@name == "pTechStock") browser()
  ll <- list(
    dim = NULL,
    names = NULL
  )
  # check if the object is "inMemory"
  if (InMemory) {
    if (is.null(slot)) {
      x <- obj # slot is not assigned
    } else {
      if (!.hasSlot(obj, slot)) {
        return(ll)
      }
      x <- slot(obj, slot)
    }
    if (!is.null(element)) x <- x[[element]]
    ll$dim <- dim(x)
    ll$names <- colnames(x)
    return(ll)
  }
  # not inMemory object
  if (is.null(path)) path <- getObjPath(obj)
  stopifnot(!is.null(path))
  # browser() !!! Add path check

  if (!is.null(slot) && !.hasSlot(obj, slot)) {
    return(ll) # no data
  }
  # browser()
  # One branch for `parameter` and `variable` alike. The `modOut` container used
  # to be handled separately, reading `colnames()` off the list ELEMENT; once
  # those elements became S4 objects that returned NULL, which `findData()`'s
  # value-column filter reads as "no value column" -- silently dropping every
  # variable from `getData()`. Passing the object itself removes the trap.
  if (inherits(obj, "modelData")) {
    ll$dim <- obj@misc$onDisk[[slot]]$dim
    ll$names <- slot(obj, slot) |> colnames()
  } else {
    stop("get_lazy_dim_names: not implemented for object type ", class(obj))
  }
  # ll$names <- obj@misc$onDisk[[slot]]$dimnames
  # path <- paste(c(path, slot, element), collapse = "/") |> normalizePath()
  # qu <- try(en_open_dataset(path), silent = TRUE)
  # if (inherits(qu, "try-error")) {
  #   return(ll)
  # }
  return(ll)
}


if (F) {
  get_lazy_data(obj = scen, slot = "name")
  get_lazy_data(scen@modOut,
    slot = "variables",
    element = "vTechOut",
    InMemory = FALSE,
    path = "scenarios/base"
  ) |>
    collect() |>
    as.data.table()

  get_lazy_data(scen@modOut@variables, element = "vObjective", InMemory = TRUE) |>
    collect()
  get_lazy_data(scen@modOut@variables,
    element = "vObjective",
    InMemory = FALSE,
    path = "scenarios/base/variables"
  ) |>
    collect()
}

# On-disk directory layout version, written to `<scen>/layout` by
# `save_scenario()` and checked by `load_scenario()`.
#   1  modOut@variables stored as `variables/<v>/`      (data.frames)
#   2  modOut@variables stored as `variables/<v>/data/` (`variable` objects)
#   3  solve artifacts + the solution store live per run under
#      `runs/[<variant>/]<solve>/{run.yml, solver/, modOut/}`; the scenario
#      carries a `scenario.yml` manifest (default_variant, active run).
#      Layout 2 is still readable; `save_scenario()` writes only 3.
.SCENARIO_LAYOUT <- 3L

.save_slots <- list(
  weather = c("weather"),
  demand = c("demand"),
  repository = c("data"),
  model = c("data"),
  parameter = c("data"),
  # Without this entry `set_ondisk_slots()` finds no match for a `variable`,
  # `@misc$onDisk` is never recorded, nothing is written, and `save_scenario()`
  # reports success having stored no results at all.
  variable = c("data"),
  modInp = c("parameters"),
  modOut = c("variables"),
  scenario = c("model", "modInp", "modOut")
)

set_ondisk_slots <- function(obj) {
  # browser()
  # obj - object to be marked
  for (o in names(.save_slots)) {
    if (inherits(obj, o)) {
      if (.hasSlot(obj, "misc")) {
        obj@misc$onDisk <- list()
        for (s in .save_slots[[o]]) {
          obj@misc$onDisk[[s]] <- list()
          if (inherits(slot(obj, s), "list")) {
            for (i in names(slot(obj, s))) {
              obj@misc$onDisk[[s]][[i]] <- list()
              obj@misc$onDisk[[s]][[i]]$class <- class(slot(obj, s)[[i]])
              obj@misc$onDisk[[s]][[i]]$dim <- dim(slot(obj, s)[[i]])
              obj@misc$onDisk[[s]][[i]]$length <- length(slot(obj, s)[[i]])
              obj@misc$onDisk[[s]][[i]]$size <- object.size(slot(obj, s)[[i]])
            }
          } else {
            obj@misc$onDisk[[s]] <- list()
            obj@misc$onDisk[[s]]$class <- class(slot(obj, s))
            obj@misc$onDisk[[s]]$dim <- dim(slot(obj, s))
            obj@misc$onDisk[[s]]$length <- length(slot(obj, s))
            obj@misc$onDisk[[s]]$size <- size(slot(obj, s))
          }
        }
      } else {
        stop("Object has no slot 'misc'")
      }
    }
  }
  return(obj)
}

get_ondisk_slots <- function(obj) {
  if (!isS4(obj)) {
    return(NULL)
  }
  if (!.hasSlot(obj, "misc")) {
    return(NULL)
  }
  return(names(obj@misc$onDisk))
}

mark_ondisk <- function(obj) {
  sts <- slotNames(obj)
  if (any(sts %in% "inMemory")) {
    obj@inMemory <- FALSE
    return(obj)
  } else if (any(sts %in% "misc")) {
    obj@misc$inMemory <- FALSE
    return(obj)
  }
  return(obj)
}

mark_inMemory <- function(obj) {
  sts <- slotNames(obj)
  if (any(sts %in% "inMemory")) {
    obj@inMemory <- TRUE
    return(obj)
  } else if (any(sts %in% "misc")) {
    obj@misc$inMemory <- TRUE
    return(obj)
  }
  return(obj)
}

if (F) {
  mi <- scen@model
  mi@misc
  mi <- set_ondisk_slots(mi)
  mi@misc
}

# load_scenario <- function(path, inMemory = FALSE) {
#
# }

if (F) {
  findData(scen, "")
}

#' Load scenario (in progress)
#'
#' @param path character. Path to saved with function `save_scenario` scenario directory.
#' @param name character. Name to assign to the loaded scenario object.
#' By default, the name is taken from the loaded scenario object.
#' @param env environment. Environment to assign the loaded scenario object.
#' @param overwrite logical. Overwrite existing scenario object in the environment.
#' @param ignore_errors logical. Ignore load errors and continue execution.
#' This option is useful when some data is missing or corrupted.
#' @param verbose logical. Print messages.
#'
#' @return TRUE if scenario is loaded, FALSE if not.
#' @export
#'
#' @examples
#' \dontrun{
#' load_scenario("scenarios/base")
#' }
load_scenario <- function(
    path,
    name = NULL,
    env = .scen,
    overwrite = FALSE,
    ignore_errors = FALSE,
    verbose = TRUE) {
  # browser()
  if (!file.exists(path) & !dir.exists(path)) {
    msg <- paste0("File or directory '", path, "' does not exist")
    if (!ignore_errors) stop(msg)
    if (verbose) message(msg)
    return(invisible(FALSE))
  }
  finf <- file.info(path)
  if (finf$isdir) {
    # Layout check before anything is read. An older directory stores each
    # variable's data one level up from where this version looks for it, and
    # that reads as "no data" rather than as an error -- so refuse it outright.
    lay <- fp(path, "layout")
    ver <- if (file.exists(lay)) {
      suppressWarnings(as.integer(readLines(lay, warn = FALSE)[1]))
    } else {
      1L
    }
    if (is.na(ver) || !(ver %in% c(2L, 3L))) {
      msg <- paste0(
        "Scenario directory '", path, "' uses on-disk layout ",
        if (is.na(ver)) "?" else ver, ", this version of energyRt reads ",
        "layouts 2 and 3.\n",
        "  Layout 2 moved each variable's data from `variables/<name>/` to ",
        "`variables/<name>/data/` when `modOut@variables` became a list of ",
        "`variable` objects.\n",
        "  Re-run and re-save the scenario with this version."
      )
      if (!ignore_errors) stop(msg)
      if (verbose) message(msg)
      return(invisible(FALSE))
    }
    if (ver == 2L && verbose) {
      message(
        "Scenario '", basename(path), "' uses on-disk layout 2; it loads ",
        "fine. Run upgrade_scenario_layout('", path, "') to migrate it to ",
        "layout 3 (per-run folders with provenance under runs/)."
      )
    }
    scen_root <- path # the directory actually being loaded (for path rebase)
    path <- fp(path, "scen.RData")
    if (!file.exists(path)) {
      msg <- paste0("Scenario file '", path, "' has not been found.")
      if (!ignore_errors) stop(msg)
      if (verbose) message(msg)
      return(invisible(FALSE))
    }
  } else {
    scen_root <- dirname(path)
  }
  if (!(exists(".en_tmp") && is.environment(.en_tmp))) {
    .en_tmp <- new.env(parent = .GlobalEnv)
  }
  # on.exit(rm(.en_tmp))
  nm <- load(path, envir = .en_tmp)
  if (length(nm) != 1) {
    msg <- paste0(
      "Scenario file '", path,
      "' must contain only one (scenario) object",
      ", actual number of objects: ", length(nm)
    )
    if (!ignore_errors) stop(msg)
    if (verbose) message(msg)
    return(invisible(FALSE))
  }
  if (!inherits(get(nm, envir = .en_tmp), "scenario")) {
    msg <- paste0(
      path, " must contain a 'scenario' object; actual class: ",
      class(get(nm, envir = .en_tmp))
    )
    if (!ignore_errors) stop(msg)
    if (verbose) message(msg)
    return(invisible(FALSE))
  }
  scen_obj <- get(nm, envir = .en_tmp)

  # Rebase every stored path onto the folder actually loaded: the saved
  # object recorded save-time paths (relative to the then-working directory),
  # which break when the folder is moved or getwd() differs. Deterministic
  # reconstruction, so nothing depends on the stored strings.
  scen_obj <- tryCatch(
    .scenario_rebase_paths(scen_obj, scen_root),
    error = function(e) {
      warning("Could not rebase the scenario's on-disk paths (",
              conditionMessage(e), ")", call. = FALSE)
      scen_obj
    })

  # restore the sourceCode blocks dropped at save time (identical to the
  # package templates then; refilled from the CURRENT package — a re-solve
  # uses current code, and the run's actual script is preserved in its run
  # directory). User-overridden blocks were saved verbatim and stay.
  dk <- scen_obj@misc$sourceCode_default
  if (length(dk)) {
    for (k in dk) {
      if (is.null(scen_obj@settings@sourceCode[[k]])) {
        scen_obj@settings@sourceCode[[k]] <- .modelCode[[k]]
      }
    }
  }

  # resolve a model reference (scenario saved with embed_model ref)
  ref <- scen_obj@model@misc$model_ref
  if (!is.null(ref)) {
    mod <- tryCatch(
      load_model(ref$name, hash = ref$hash, verbose = FALSE),
      error = function(e) NULL)
    if (is.null(mod) && !is.null(ref$path)) {
      cand <- fp(dirname(get_registry_file()), ref$path)
      if (file.exists(fp(cand, "mod.RData"))) {
        mod <- tryCatch(load_model(ref$name, path = cand, verbose = FALSE),
                        error = function(e) NULL)
      }
    }
    if (is.null(mod)) {
      msg <- paste0(
        "Scenario '", scen_obj@name, "' references model '", ref$name, "@",
        substr(ref$hash %||% "", 1, 8), "' which is not in the registry or ",
        "the model store ('", get_models_path(), "').\n",
        "  Run refresh_registry() to rescan, or re-save the scenario with ",
        "embed_model = TRUE from a session that has the model."
      )
      if (!ignore_errors) stop(msg)
      if (verbose) message(msg)
      return(invisible(FALSE))
    }
    scen_obj@model <- mod
    if (verbose) {
      message("Model '", ref$name, "' resolved from the model store")
    }
  }
  assign(nm, scen_obj, envir = .en_tmp)

  if (is.null(name)) name <- get(nm, envir = .en_tmp)@name
  if (is.null(env)) {
    scen <- get(nm, envir = .en_tmp)
    return(scen)
  }
  if (exists(name, envir = env) & !overwrite) {
    msg <- paste0(
      "Scenario '", name,
      "' already exists in 'env' environment. \n",
      "Use 'overwrite = TRUE' or different name"
    )
    if (!ignore_errors) stop(msg)
    if (verbose) message(msg)
    return(invisible(FALSE))
  }
  if (!exists(name, envir = env) | overwrite) {
    assign(name, get(nm, envir = .en_tmp), envir = env)
    assign(nm, NULL, envir = .en_tmp)
    return(invisible(TRUE))
  }
  # assign(name, get(nm, envir = .en_tmp), envir = env)
  # assign(nm, NULL, envir = .en_tmp)
  # return(invisible(TRUE))
  # return(get(name, envir = env))
  # return(nm)
  return(invisible(FALSE))
}

## - DRAFTS -------------------------------------------------------####

#' Loads objects from disk to memory
#'
#' @param obj Object of S4 class, saved on disk (scenario, model, etc.)
#' @param verbose If TRUE, prints messages
#'
#' @return Object of the same S4 class as input object, with
#' all of the slots loaded in memory.
#' @export
#'
#' @examples
#' \dontrun{
#' obj2mem(scen_ondisk)
#' }
obj2mem <- function(obj, verbose = TRUE) {
  # browser()
  if (!isS4(obj)) {
    stop("Object must be of S4 class, actual class: ", class(obj))
  }
  if (isInMemory(obj)) return(invisible(obj))
  if (!.hasSlot(obj, "misc")) {
    stop("Object of class ", class(obj), " has no 'misc' slot")
  }
  sls <- names(obj@misc$onDisk)
  if (length(sls) == 0) browser()
  obj_pth <- getObjPath(obj)
  for (s in sls) {
    pth <- fp(obj_pth, s)
    if (isS4(slot(obj, s))) {
      # cat(getObjPath(slot(obj, s)), "\n")
      slot(obj, s) <- obj2mem(slot(obj, s))
    } else if (inherits(slot(obj, s), "list")) {
      sls2 <- names(obj@misc$onDisk[[s]])
      for (i in sls2) {
        if (isS4(slot(obj, s)[[i]])) {
          slot(obj, s)[[i]] <- obj2mem(slot(obj, s)[[i]])
        } else {
          if (obj@misc$onDisk[[s]][[i]]$dim[1] == 0) next
          pth2 <- fp(pth, i)
          if(verbose) cat(pth2, "\n")
          slot(obj, s)[[i]] <- en_open_dataset(pth2) |> collect()}
      }
      # cat(s, "\n")
    } else {
      if (obj@misc$onDisk[[s]]$dim[1] == 0) next
      if(verbose) cat(pth, "\n")
      slot(obj, s) <- en_open_dataset(pth) |> collect()
    }
  }
  obj <- mark_inMemory(obj)
  invisible(obj)
}

get_element <- function(obj, element) {
  if (isS4(obj)) {
    return(slot(obj, element))
  } else {
    return(obj[[element]])
  }
}


# compare_slots <- function(obj1, obj2) {
#
# }

if (F) {
  x <- scen_BASE@modOut@variables
  y <- obj2mem(scen_ondisk@modOut)@variables

  object.size(x)
  object.size(y)

  for (i in names(x)) {
    if (!all(dim(x[[i]]) == dim(y[[i]]))) {
      print(i)
      stop()
    }
    stopifnot(
      compare::compare(x[[i]], y[[i]], allowAll = TRUE)$result
      )
  }

  scen_inmem <- obj2mem(scen_ondisk)

}


