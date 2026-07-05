# =============================================================================#
# get_data_objects.R -- getData() methods for model *objects* (raw input data).
#
# getData(scenario) returns processed/solved data with a uniform (name, dims,
# value) schema (see get_data.R). The methods here return the RAW input stored
# in a model object's data.frame slots (pre-interpolation): technology,
# commodity, storage, supply, demand, trade, ... and, via containers, model and
# repository. With `interpolate = TRUE` the year-bearing slots are expanded over
# the target milestone years using each parameter's own default interpolation
# rule -- handy for quick plots that demonstrate what interpolation will do.
# =============================================================================#

# Object class -> its canonical set-dimension name (column that carries the
# object's name in the tidy output). Falls back to the class name itself.
.class_set_dim <- c(
  technology = "tech", commodity = "comm", storage = "stg",
  supply = "sup", demand = "dem", trade = "trade",
  import = "imp", export = "expp", weather = "weather"
)

# Known set (index) dimensions -- everything else numeric is a "value" column.
.known_set_dims <- c(
  "region", "regionp", "year", "yearp", "slice", "slicep", "comm", "commp",
  "acomm", "group", "tech", "techp", "stg", "sup", "dem", "trade", "expp",
  "imp", "impp", "weather", "src", "dst", "process"
)

# Reverse map (memoised): "class\rslot\rcolName" -> list(rule, defVal, param),
# read from the baked-in .modInp parameter template (each parameter records its
# origin object class/slot/column and its interpolation rule + default value).
.param_interp_map_cache <- new.env(parent = emptyenv())
.param_interp_map <- function() {
  if (!is.null(.param_interp_map_cache$map)) {
    return(.param_interp_map_cache$map)
  }
  mp <- list()
  params <- tryCatch(.modInp@parameters, error = function(e) NULL)
  if (!is.null(params)) {
    for (p in params) {
      ic <- tryCatch(p@inClass, error = function(e) NULL)
      if (is.null(ic) || nrow(ic) == 0) next
      for (i in seq_len(nrow(ic))) {
        key <- paste(ic$class[i], ic$slot[i], ic$colName[i], sep = "\r")
        mp[[key]] <- list(
          rule = p@interpolation, defVal = p@defVal, param = p@name
        )
      }
    }
  }
  .param_interp_map_cache$map <- mp
  mp
}

# Apply the default (per-parameter) interpolation rule to every year-bearing
# value column of one slot data.frame, over `years` (target milestones). Value
# columns without a mapped rule fall back to "back.inter.forth"/default.
.interp_object_slot <- function(cls, slot_name, d, years, dim_nm) {
  if (!("year" %in% names(d)) || nrow(d) == 0) {
    return(d)
  }
  val_cols <- names(d)[vapply(d, is.numeric, logical(1))]
  val_cols <- setdiff(val_cols, .known_set_dims) # drops year + set dims
  if (length(val_cols) == 0) {
    return(d)
  }
  yrs <- years
  if (is.null(yrs)) {
    yy <- suppressWarnings(as.integer(d$year))
    yy <- yy[!is.na(yy)]
    if (length(yy) == 0) {
      return(d)
    }
    yrs <- if (length(unique(yy)) > 1) seq(min(yy), max(yy)) else unique(yy)
  }
  mp <- .param_interp_map()
  set_cols <- setdiff(intersect(names(d), .known_set_dims), "year")
  res <- NULL
  for (vc in val_cols) {
    info <- mp[[paste(cls, slot_name, vc, sep = "\r")]]
    rule <- .interp_rule_token(if (!is.null(info)) info$rule else "back.inter.forth")
    dv <- if (!is.null(info)) info$defVal[1] else NA_real_
    raw <- d[, c(set_cols, "year", vc), drop = FALSE]
    names(raw)[names(raw) == vc] <- "value"
    raw$year <- suppressWarnings(as.integer(raw$year))
    ser <- tryCatch(
      .interp_one_series(raw, set_cols, yrs, rule, dv),
      error = function(e) NULL
    )
    if (is.null(ser) || nrow(ser) == 0) next
    names(ser)[names(ser) == "value"] <- vc
    res <- if (is.null(res)) {
      ser
    } else {
      suppressMessages(dplyr::full_join(res, ser))
    }
  }
  if (is.null(res)) {
    return(d)
  }
  if (!is.na(dim_nm) && dim_nm %in% names(d) && !(dim_nm %in% names(res))) {
    res[[dim_nm]] <- unique(d[[dim_nm]])[1]
  }
  as.data.frame(res)
}

# Filter a slot data.frame by `...` selectors: `col = values` (exact) and
# `col_ = pattern` (regex). Unknown columns are ignored.
.filter_object_df <- function(d, flt, ignore.case = TRUE) {
  if (length(flt) == 0 || nrow(d) == 0) {
    return(d)
  }
  keep <- rep(TRUE, nrow(d))
  for (k in names(flt)) {
    rgx <- grepl("_$", k)
    col <- sub("_$", "", k)
    if (!(col %in% names(d))) next
    if (rgx) {
      keep <- keep & grepl(flt[[k]], as.character(d[[col]]), ignore.case = ignore.case)
    } else {
      keep <- keep & (as.character(d[[col]]) %in% as.character(flt[[k]]))
    }
  }
  d[keep, , drop = FALSE]
}

# Pivot one slot data.frame to long tidy form: (set cols..., param, slot, value).
# Value columns that are entirely NA (unfilled sub-parameters) and NA-value rows
# are dropped so a merged/plotting frame carries only real data.
.slot_to_long <- function(d, slot_name) {
  set_cols <- intersect(names(d), c(.known_set_dims, "class"))
  val_cols <- setdiff(names(d)[vapply(d, is.numeric, logical(1))], .known_set_dims)
  val_cols <- val_cols[vapply(val_cols, function(vc) any(!is.na(d[[vc]])), logical(1))]
  if (length(val_cols) == 0) {
    return(NULL)
  }
  parts <- lapply(val_cols, function(vc) {
    x <- d[, c(set_cols, vc), drop = FALSE]
    names(x)[names(x) == vc] <- "value"
    x <- x[!is.na(x$value), , drop = FALSE]
    if (nrow(x) == 0) {
      return(NULL)
    }
    x$param <- vc
    x$slot <- slot_name
    x
  })
  dplyr::bind_rows(parts)
}

# Core object extractor shared by all brick methods.
.getData_object <- function(obj, name = NULL, ..., merge = FALSE,
                            interpolate = FALSE, years = NULL, process = FALSE,
                            asTibble = TRUE, ignore.case = TRUE, verbose = FALSE) {
  cls <- class(obj)[1]
  dim_nm <- unname(.class_set_dim[cls])
  if (is.na(dim_nm)) dim_nm <- cls
  obj_name <- tryCatch(obj@name, error = function(e) NA_character_)

  sl_types <- methods::getSlots(cls)
  df_slots <- names(sl_types)[sl_types == "data.frame"]
  if (!is.null(name)) df_slots <- df_slots[df_slots %in% name]
  flt <- list(...)

  out <- list()
  for (s in df_slots) {
    d <- tryCatch(as.data.frame(methods::slot(obj, s)), error = function(e) NULL)
    if (is.null(d)) next
    if (nrow(d) == 0 && !interpolate) next
    if (!is.na(obj_name) && nzchar(obj_name) && !(dim_nm %in% names(d))) {
      d[[dim_nm]] <- obj_name
    }
    if (isTRUE(interpolate)) d <- .interp_object_slot(cls, s, d, years, dim_nm)
    d <- .filter_object_df(d, flt, ignore.case)
    if (is.null(d) || nrow(d) == 0) next
    d$class <- cls
    out[[s]] <- d
  }

  if (isTRUE(process) && !is.na(dim_nm)) {
    out <- lapply(out, function(d) {
      if (dim_nm %in% names(d)) names(d)[names(d) == dim_nm] <- "process"
      d
    })
  }

  if (!merge) {
    if (isTRUE(asTibble)) out <- lapply(out, tibble::as_tibble)
    return(out)
  }
  if (length(out) == 0) {
    return(if (asTibble) tibble::tibble() else data.frame())
  }
  long <- dplyr::bind_rows(lapply(names(out), function(s) .slot_to_long(out[[s]], s)))
  if (isTRUE(asTibble)) long <- tibble::as_tibble(long)
  long
}

# Container extractor: stack the requested slot(s) across all contained objects.
.getData_container <- function(obj, name = NULL, ..., merge = FALSE,
                               interpolate = FALSE, years = NULL, process = FALSE,
                               asTibble = TRUE, ignore.case = TRUE, verbose = FALSE) {
  objs <- tryCatch(getObjects(obj, class = c()), error = function(e) list())
  objs <- objs[!vapply(objs, is.null, logical(1))]
  if (length(objs) == 0) {
    return(if (merge) {
      if (asTibble) tibble::tibble() else data.frame()
    } else {
      list()
    })
  }
  res <- lapply(objs, function(o) {
    tryCatch(
      .getData_object(o,
        name = name, ..., merge = merge, interpolate = interpolate,
        years = years, process = process, asTibble = asTibble,
        ignore.case = ignore.case, verbose = verbose
      ),
      error = function(e) if (merge) NULL else list()
    )
  })
  if (merge) {
    res <- res[!vapply(res, is.null, logical(1))]
    long <- dplyr::bind_rows(res)
    if (isTRUE(asTibble)) long <- tibble::as_tibble(long)
    return(long)
  }
  # named list keyed by object name, dropping objects with no matching data
  res <- res[vapply(res, function(x) length(x) > 0, logical(1))]
  res
}

#' @description
#' Model-object methods (\code{technology}, \code{commodity}, \code{storage},
#' \code{supply}, \code{demand}, \code{trade}) return the object's raw input
#' slot data. \code{model}/\code{repository} stack that across all contained
#' objects. Use \code{name} to select slot(s) (e.g. \code{"invcost"}), \code{...}
#' to filter (\code{region=}, \code{year=}, \code{comm=}, or \code{col_=}regex),
#' \code{merge=TRUE} for one long tidy frame (\code{param}/\code{value}), and
#' \code{interpolate=TRUE} to expand year-bearing slots over \code{years} (or the
#' observed year range) with each parameter's default interpolation rule.
#'
#' @param interpolate if TRUE, expand year-bearing object slots over the target
#'   years using each parameter's default interpolation rule (for quick
#'   demonstration plots). Default FALSE (raw data as stored). Only meaningful
#'   for object/model/repository methods.
#' @param years integer vector of target milestone years for \code{interpolate}
#'   (default: the yearly range observed in the data).
#' @rdname getData
#' @method getData technology
#' @export
getData.technology <- .getData_object
#' @rdname getData
#' @method getData commodity
#' @export
getData.commodity <- .getData_object
#' @rdname getData
#' @method getData storage
#' @export
getData.storage <- .getData_object
#' @rdname getData
#' @method getData supply
#' @export
getData.supply <- .getData_object
#' @rdname getData
#' @method getData demand
#' @export
getData.demand <- .getData_object
#' @rdname getData
#' @method getData trade
#' @export
getData.trade <- .getData_object
#' @rdname getData
#' @method getData import
#' @export
getData.import <- .getData_object
#' @rdname getData
#' @method getData export
#' @export
getData.export <- .getData_object
#' @rdname getData
#' @method getData weather
#' @export
getData.weather <- .getData_object

#' @rdname getData
#' @method getData model
#' @export
getData.model <- .getData_container
#' @rdname getData
#' @method getData repository
#' @export
getData.repository <- .getData_container
