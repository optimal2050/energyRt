# Saved on disk scenario and model objects have the same structure as in memory  objects. The only difference is that data-frame slots are saved in `parquet`  format, while other "large" slots are saved in `RData` format.  Saved on disk objects have the same structure of directories and files as in  memory objects. The in-memory object stores the information about the path to  each slot on disk, the dimensions of the data-frame slots, the file format, and the file name. This information is stored in the `@misc` slot of the in-memory object. The `inMemory` slot of the object is set to `FALSE` to indicate that the object is saved on disk. The scenario or model object itself with stored  on disk slots is saved in the same directory as `scen.RData` or `mod.RData`. The access to the saved on disk slots is provided by the `getData` function. (toDo: add getSlot function and/or `slot` method)
# The structure of the information stored in the `@misc` slot of the in-memory part of an object is a nested `list`, where every level corresponds to a directory. The last level of the nested list contains the information about the file format, the file name, and the dimensions of the data-frame slots or length of the vector slot.


#' Save scenario object on disk in parquet format using `arrow` package.
#'
#' @param scen scenario object.
#' @param path character. Path to scenario directory.
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
    # save_model = FALSE,
    # save_modInp = TRUE,
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
    scen@path <- fp("scenarios", scen@name)
    message("Scenarios directory: ", scen@path)
  } else {
    scen@path <- path
  }

  if (isOnDisk(scen)) {
    stopifnot(dir.exists(path))
    cat("Scenario '", scen@name, "' is already saved on disk.\n")
    cat("Directory: '", scen@path, "'\n")
    # cat("Use 'overwrite = TRUE' to overwrite.\n")
    return(scen)
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
  log_file <- (fp(scen@path, "logfile.csv"))
  write(paste(lubridate::now(tzone = "UTC"), "format", format, sep = ","),
    file = log_file, append = TRUE
  )

  if (verbose) {
    cat("Saving large slots of scenario object",
      " '", scen@name, "' ", "on disk\n",
      sep = ""
    )
  }
  # message("Saving large data-frames on disk")
  scen <- obj2disk(
    scen,
    path = scen@path,
    format = format,
    verbose = verbose
  )
  # message("Saving the thinned scenario object")
  save(scen, file = fp(scen@path, "scen.RData"))
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
                          default = NULL
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
    if (length(ff) == 0) return(NULL)
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
  if (inherits(obj, "parameter")) {
    ll$dim <- obj@misc$onDisk[[slot]]$dim
    # ll$names <- obj@dimSets
    ll$names <- slot(obj, slot) |> colnames()
  } else if (inherits(obj, "modOut")) {
    # browser()
    ll$dim <- obj@misc$onDisk[[slot]][[element]]$dim
    ll$names <- slot(obj, slot)[[element]] |> colnames()
  } else {
    browser()
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

.save_slots <- list(
  weather = c("weather"),
  demand = c("dem"),
  repository = c("data"),
  model = c("data"),
  parameter = c("data"),
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
    path <- fp(path, "scen.RData")
    if (!file.exists(path)) {
      msg <- paste0("Scenario file '", path, "' has not been found.")
      if (!ignore_errors) stop(msg)
      if (verbose) message(msg)
      return(invisible(FALSE))
    }
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


