# =========================================================================== #
# Changing where a model element is declared: `update()` on a `model`.
#
# A commodity's `@geoframe` says where it balances; a process's says where its
# totals are summed. Moving one is ONE SLOT WRITTEN IN SEVERAL OBJECTS, not an
# arithmetic operation. `.STRICT_LEVEL_CLASSES` (R/check_levels.R) are pinned to
# their commodity's frame -- those flows have no aggregation path, so a
# mismatched cell is written and never read -- which is why the frame moves for
# the commodity and its strict objects together, or for none of them.
#
# The verb therefore changes no value. Data that does not already sit at the
# requested frame is refused, naming the object and the column; moving it is a
# separate call, and upward only (`aggregate_model_regions()`), since the
# balance layer only aggregates. `force = TRUE` writes the frames anyway and
# warns, which is the sequential route: move the frame, then the data.
#
# `data =` is handed to each object's own `update()` BEFORE the frames move, so
# the one-call form -- new data plus the new frame -- has nothing to refuse.
#
# The LP maps are not touched: they live in `scen@modInp` and are built at
# interpolation, so what changes here is which maps the next interpolation
# writes.
# =========================================================================== #

#' @include class-model.R check_levels.R

# Classes whose frame is a free variable, per axis. Everything else is either
# pinned to its commodity (`.STRICT_LEVEL_CLASSES`) or carries no such slot.
#' @noRd
.LVL_NAMEABLE <- list(
  geoframe  = c("commodity", "technology", "storage"),
  timeframe = c("commodity", "technology", "weather")
)

# Slots that carry a unit name in a `comm` column rather than a commodity.
#' @noRd
.LVL_NOT_COMM_SLOTS <- c("units", "misc")

# The commodities one object touches, restricted to the model's own roster so a
# unit name in a `comm` column cannot pass for a commodity.
#' @noRd
.lvl_object_comms <- function(o, known) {
  out <- tryCatch(as.character(methods::slot(o, "commodity")),
                  error = function(e) character())
  for (sn in methods::slotNames(o)) {
    if (sn %in% .LVL_NOT_COMM_SLOTS) next
    v <- tryCatch(methods::slot(o, sn), error = function(e) NULL)
    if (!is.data.frame(v) || !nrow(v)) next
    for (cn in intersect(c("comm", "acomm"), names(v))) {
      out <- c(out, as.character(v[[cn]]))
    }
  }
  intersect(unique(out[!is.na(out) & nzchar(out)]), known)
}

# Every region code one object declares, by slot and column. `@region` is the
# object's own declaration and is reported as the slot it is.
#' @noRd
.lvl_region_decls <- function(o) {
  rows <- list()
  add <- function(slot, column, codes) {
    codes <- .nonblank(codes)
    if (!length(codes)) return(invisible(NULL))
    rows[[length(rows) + 1L]] <<- data.frame(
      slot = slot, column = column,
      declared = paste(sort(codes), collapse = ", "),
      n = length(codes), stringsAsFactors = FALSE)
    invisible(NULL)
  }
  add("region", NA_character_,
      tryCatch(as.character(methods::slot(o, "region")),
               error = function(e) character()))
  for (sn in methods::slotNames(o)) {
    if (sn %in% .LVL_NOT_COMM_SLOTS) next
    v <- tryCatch(methods::slot(o, sn), error = function(e) NULL)
    if (!is.data.frame(v) || !nrow(v)) next
    for (cn in intersect(c("region", "src", "dst"), names(v))) {
      add(sn, cn, v[[cn]])
    }
  }
  if (!length(rows)) return(NULL)
  do.call(rbind, rows)
}

# A frame request, as a named character vector. `list(ELC = "zone")` and
# `c(ELC = "zone")` are the same thing; an unnamed entry is a caller error,
# since the element is what is being named.
#' @noRd
.lvl_spec <- function(x, what) {
  if (is.null(x) || !length(x)) return(character())
  if (is.list(x)) {
    bad <- vapply(x, function(v) length(v) != 1L || !is.character(v),
                  logical(1))
    if (any(bad)) {
      stop(what, ": each entry must be a single frame name; ",
           paste(names(x)[bad], collapse = ", "), " is not", call. = FALSE)
    }
    x <- unlist(x)
  }
  if (!is.character(x) || is.null(names(x)) || any(!nzchar(names(x)))) {
    stop(what, " must be named by the element, e.g. ", what,
         ' = list(ELC = "zone")', call. = FALSE)
  }
  x
}

# The frames of an axis, coarsest first, and the members of one of them.
#' @noRd
.lvl_axis <- function(mod, axis) {
  if (identical(axis, "geoframe")) {
    gs <- tryCatch(mod@config@geoscale, error = function(e) NULL)
    if (is.null(gs) || !is_geoscale(gs)) {
      stop("update(geoframe = ) needs a geoscale on the model; ",
           "see setGeoscale()", call. = FALSE)
    }
    frames <- as.character(geoscales::geoscale_geoframes(gs))
    return(list(
      frames = frames,
      members = function(f) as.character(geoscales::geoscale_regions(gs, f)),
      all = unique(unlist(lapply(frames, function(f)
        as.character(geoscales::geoscale_regions(gs, f))))),
      declared = .lvl_region_decls,
      what = "region"))
  }
  cal <- tryCatch(mod@config@calendar, error = function(e) NULL)
  tf <- if (is.null(cal)) list() else cal@timeframes
  if (!length(tf)) {
    stop("update(timeframe = ) needs a calendar on the model", call. = FALSE)
  }
  list(
    frames = names(tf),
    members = function(f) as.character(tf[[f]]),
    all = unique(as.character(unlist(tf, use.names = FALSE))),
    declared = function(o) {
      s <- .declared_timeslices(o)
      if (!length(s)) return(NULL)
      data.frame(slot = "timeslice", column = NA_character_,
                 declared = paste(sort(unique(s)), collapse = ", "),
                 n = length(unique(s)), stringsAsFactors = FALSE)
    },
    what = "timeslice")
}

# A sample of the offending names, so a 105,120-row table reports four codes
# and a count rather than a screenful.
#' @noRd
.lvl_abbrev <- function(x, n = 4L) {
  x <- sort(unique(x))
  if (length(x) <= n) return(paste(x, collapse = ", "))
  paste0(paste(x[seq_len(n)], collapse = ", "), ", ... (", length(x), ")")
}

# One row per declaration that does not sit at the requested frame.
#' @noRd
.lvl_object_rows <- function(o, ax, frame, element, role) {
  dec <- ax$declared(o)
  if (is.null(dec) || !nrow(dec)) return(NULL)
  ok <- ax$members(frame)
  rows <- list()
  for (i in seq_len(nrow(dec))) {
    codes <- strsplit(dec$declared[i], ", ", fixed = TRUE)[[1]]
    # Only names the geoscale or the calendar KNOWS are judged: a name absent
    # altogether is how sampling works, not a level error.
    off <- setdiff(intersect(codes, ax$all), ok)
    if (!length(off)) next
    endpoint <- identical(ax$what, "region") &&
      !is.na(dec$column[i]) && dec$column[i] %in% c("src", "dst")
    rows[[length(rows) + 1L]] <- data.frame(
      element = element, frame = frame, object = .object_label(o),
      class = class(o)[1], role = role, axis = ax$what,
      slot = dec$slot[i],
      column = if (is.na(dec$column[i])) "" else dec$column[i],
      declared = paste(sort(off), collapse = ", "),
      n = length(off),
      status = if (endpoint) "endpoint" else "move",
      stringsAsFactors = FALSE)
  }
  if (!length(rows)) return(NULL)
  do.call(rbind, rows)
}

# Which objects a frame move reaches: the element itself, plus -- when the
# element is a commodity -- every strict-class object carrying it, whose frame
# is pinned to the commodity's and therefore moves with it.
#' @noRd
.lvl_scope <- function(objs, element, cls, known) {
  out <- list(list(name = element, role = "named"))
  if (!identical(cls, "commodity")) return(out)
  for (nm in names(objs)) {
    if (identical(nm, element)) next
    o <- objs[[nm]]
    if (!class(o)[1] %in% .STRICT_LEVEL_CLASSES) next
    if (element %in% .lvl_object_comms(o, known)) {
      out[[length(out) + 1L]] <- list(name = nm, role = "pinned")
    }
  }
  out
}

#' What a frame move would change, and what it refuses
#'
#' The pre-flight behind `update(model, geoframe = , timeframe = )`: it resolves
#' each request, collects the objects pinned to it, and reports every
#' declaration that does not already sit at the requested frame.
#'
#' @param mod a `model`.
#' @param geoframe,timeframe named frame requests, as the method takes them.
#' @return A list of class `model_levels_report`.
#' @noRd
.lvl_report <- function(mod, geoframe = NULL, timeframe = NULL) {
  geoframe <- .lvl_spec(geoframe, "geoframe")
  timeframe <- .lvl_spec(timeframe, "timeframe")
  objs <- .model_objects(mod)
  names(objs) <- vapply(objs, .object_label, character(1))
  known <- names(objs)[vapply(objs, function(o)
    identical(class(o)[1], "commodity"), logical(1))]

  req <- list()
  scp <- list()
  rows <- list()
  for (axis in c("geoframe", "timeframe")) {
    spec <- if (identical(axis, "geoframe")) geoframe else timeframe
    if (!length(spec)) next
    ax <- .lvl_axis(mod, axis)
    for (element in names(spec)) {
      frame <- spec[[element]]
      if (!element %in% names(objs)) {
        stop("no object named \"", element, "\" in the model", call. = FALSE)
      }
      if (!frame %in% ax$frames) {
        stop(axis, " \"", frame, "\" is not declared; the model has: ",
             paste(ax$frames, collapse = ", "), call. = FALSE)
      }
      o <- objs[[element]]
      cls <- class(o)[1]
      if (!cls %in% .LVL_NAMEABLE[[axis]]) {
        if (cls %in% .STRICT_LEVEL_CLASSES) {
          cms <- .lvl_object_comms(o, known)
          stop(element, " is a ", cls, ": its ", axis,
               " follows its commodity (", paste(cms, collapse = ", "),
               "), so move that instead -- those flows have no aggregation ",
               "path and cannot sit at a frame of their own", call. = FALSE)
        }
        stop(element, " is a ", cls, ", whose ", axis,
             " cannot be moved; nameable classes are: ",
             paste(.LVL_NAMEABLE[[axis]], collapse = ", "), call. = FALSE)
      }
      now <- tryCatch(as.character(methods::slot(o, axis)),
                      error = function(e) character())
      now <- if (!length(now) || is.na(now[1]) || !nzchar(now[1])) {
        ax$frames[length(ax$frames)]
      } else {
        now[1]
      }
      finer <- match(frame, ax$frames) > match(now, ax$frames)
      scope <- .lvl_scope(objs, element, cls, known)
      for (s in scope) {
        rows[[length(rows) + 1L]] <-
          .lvl_object_rows(objs[[s$name]], ax, frame, element, s$role)
      }
      req[[length(req) + 1L]] <- data.frame(
        axis = axis, element = element, class = cls, from = now, to = frame,
        finer = finer, objects = length(scope), stringsAsFactors = FALSE)
      # The frame is written on the pinned objects as well: their own slot is
      # what scopes them (`get_process_region()` narrows a wildcard to the
      # regions OF THAT LEVEL), so moving the commodity alone would leave them
      # at the finer frame and the model would not interpolate.
      scp[[length(scp) + 1L]] <- data.frame(
        axis = axis, element = element, frame = frame,
        object = vapply(scope, function(z) z$name, character(1)),
        role = vapply(scope, function(z) z$role, character(1)),
        stringsAsFactors = FALSE)
    }
  }
  rows <- do.call(rbind, rows)
  out <- list(
    requests = if (length(req)) do.call(rbind, req) else NULL,
    scope = if (length(scp)) do.call(rbind, scp) else NULL,
    rows = rows,
    ready = is.null(rows) || !nrow(rows))
  class(out) <- "model_levels_report"
  out
}

#' @method print model_levels_report
#' @export
print.model_levels_report <- function(x, ...) {
  rq <- x$requests
  if (is.null(rq)) {
    cat("Nothing requested.\n")
    return(invisible(x))
  }
  for (i in seq_len(nrow(rq))) {
    cat(sprintf("%s: %s %s -> %s (%d object%s)\n", rq$element[i], rq$axis[i],
                rq$from[i], rq$to[i], rq$objects[i],
                if (rq$objects[i] == 1L) "" else "s"))
  }
  if (isTRUE(x$ready)) {
    cat("Every declaration already sits at the requested frame.\n")
    return(invisible(x))
  }
  cat("\nData is not at the requested frame:\n")
  r <- x$rows
  # One line per OBJECT: a trade declares its endpoints in seven slots, and
  # seven lines saying the same thing hide the sixteen other objects. The list
  # is capped, because a model may pin dozens of objects to one commodity.
  where <- ifelse(nzchar(r$column), paste0(r$slot, "$", r$column), r$slot)
  nms <- unique(r$object)
  for (nm in utils::head(nms, 8L)) {
    i <- which(r$object == nm)
    cat(sprintf("  %-16s %-10s %-30s %s\n", nm, r$class[i[1]],
                .lvl_abbrev(where[i], 3L),
                .lvl_abbrev(unlist(strsplit(r$declared[i], ", ",
                                            fixed = TRUE)))))
  }
  if (length(nms) > 8L) {
    cat("  ... and", length(nms) - 8L, "more object(s):",
        .lvl_abbrev(nms[-seq_len(8L)], 6L), "\n")
  }
  if (any(r$status == "endpoint")) {
    cat("\nEndpoints (src/dst) are a network decision, not a recast: ",
        "one route per child or one shared route.\n", sep = "")
  }
  if (any(rq$finer)) {
    cat("\nThe request is FINER than the current frame, and the balance layer ",
        "only aggregates: there is no route down. Declare the finer data ",
        "first.\n", sep = "")
  } else {
    mover <- if (all(r$axis == "timeslice")) {
      "recast_model_calendar()"
    } else {
      "aggregate_model_regions()"
    }
    cat("\nMove the data first -- ", mover, ", or declare it at the frame ",
        "-- then update the frame. force = TRUE writes the frame now and ",
        "leaves the data to a later call.\n", sep = "")
  }
  invisible(x)
}

# Hand each `data =` entry to the object's own `update()`, which is where the
# per-slot validation lives. A data.frame slot is REPLACED, not merged, so the
# old keys go with the write.
#' @noRd
.lvl_apply_data <- function(mod, data) {
  if (is.null(data) || !length(data)) return(mod)
  if (is.null(names(data)) || any(!nzchar(names(data)))) {
    stop("data must be named by the object, e.g. ",
         "data = list(DEM_ELC = list(demand = dem_zone))", call. = FALSE)
  }
  have <- vapply(.model_objects(mod), .object_label, character(1))
  unknown <- setdiff(names(data), have)
  if (length(unknown)) {
    stop("no object named ", paste0("\"", unknown, "\"", collapse = ", "),
         " in the model", call. = FALSE)
  }
  for (nm in names(data)) {
    args <- data[[nm]]
    if (!is.list(args) || is.null(names(args)) || any(!nzchar(names(args)))) {
      stop("data$", nm, " must be a named list of slots, e.g. ",
           "list(demand = dem_zone)", call. = FALSE)
    }
    mod <- .lvl_edit(mod, nm, function(o) do.call(update, c(list(o), args)))
  }
  mod
}

# Replace one named object in the model, wherever it sits.
#' @noRd
.lvl_edit <- function(mod, name, f) {
  walk <- function(el) {
    if (methods::is(el, "repository")) {
      for (nm in names(el@data)) el@data[[nm]] <- walk(el@data[[nm]])
      return(el)
    }
    if (!isS4(el)) return(el)
    if (identical(.object_label(el), name)) return(f(el))
    el
  }
  for (i in seq_along(mod@data)) mod@data[[i]] <- walk(mod@data[[i]])
  mod
}

#' Update a model's elements and the frames they are declared at
#'
#' Writes `@geoframe` / `@timeframe` on the named elements and on every object
#' pinned to them, and the slots given in `data`. It changes no value: a
#' declaration that does not already sit at the requested frame is refused,
#' naming the object and the column.
#'
#' A commodity's frame is where it balances, and the strict classes carrying it
#' -- `demand`, `supply`, `import`, `export`, `trade`, `storage` -- are pinned
#' to it, so they move with it or the call refuses. Those flows have no
#' aggregation path, which is why they cannot sit at a frame of their own.
#'
#' `data` is applied BEFORE the frames move, so a single call carrying both the
#' re-keyed data and the new frame has nothing left to refuse. Moving data is
#' otherwise a separate operation, and upward only -- see
#' [aggregate_model_regions()] -- because the balance layer only aggregates.
#'
#' @param object a `model`.
#' @param geoframe named frame requests, `list(ELC = "zone")` or
#'   `c(ELC = "zone")`; the element is a `commodity`, `technology` or `storage`.
#' @param timeframe the same on the time axis, for a `commodity`, `technology`
#'   or `weather`.
#' @param data named list of per-object slot updates,
#'   `list(DEM_ELC = list(demand = dem_zone))`, each handed to that object's own
#'   `update()`.
#' @param force write the frames although data is not at them, with a warning
#'   naming what is left inconsistent -- the sequential route. The model will
#'   not interpolate until the data follows.
#' @param dryrun return the report instead of the model, changing nothing.
#' @param verbose report what moved.
#' @param ... unused.
#' @return The updated `model`, or the report when `dryrun = TRUE`.
#' @family model
#' @exportMethod update
#' @examples
#' \donttest{
#' kit <- topia$modules$electricity
#' mod <- newModel("TOPIA", data = kit$repo,
#'                 calendar = topia$modules$calendars$d365_h24,
#'                 region = kit$regions,
#'                 horizon = topia$modules$horizons$BY2025_50_by10,
#'                 discount = 0.05)
#' mod <- setGeoscale(mod, kit$geoscale)
#'
#' # what moving ELC to the zone would ask for
#' update(mod, geoframe = list(ELC = "zone"), dryrun = TRUE)
#' }
setMethod("update", signature(object = "model"),
  function(object, geoframe = NULL, timeframe = NULL, data = NULL,
           force = FALSE, dryrun = FALSE, verbose = isVerbose(), ...) {
    object <- .lvl_apply_data(object, data)
    if (is.null(geoframe) && is.null(timeframe)) {
      if (isTRUE(dryrun)) return(.lvl_report(object))
      return(object)
    }
    rep <- .lvl_report(object, geoframe = geoframe, timeframe = timeframe)
    if (isTRUE(dryrun)) return(rep)
    if (!rep$ready) {
      msg <- paste(utils::capture.output(print(rep)), collapse = "\n")
      if (!isTRUE(force)) {
        stop("data is not at the requested frame.\n", msg, call. = FALSE)
      }
      warning("frames written with force = TRUE; the model will not ",
              "interpolate until the data follows.\n", msg, call. = FALSE)
    }
    scp <- rep$scope
    for (i in seq_len(nrow(scp))) {
      axis <- scp$axis[i]
      frame <- scp$frame[i]
      object <- .lvl_edit(object, scp$object[i], function(o) {
        # A pinned class without the slot (none today) is left alone rather
        # than failing the write: its scope follows its data.
        if (axis %in% methods::slotNames(o)) methods::slot(o, axis) <- frame
        o
      })
    }
    object@misc$levels <- c(object@misc$levels, list(list(
      when = Sys.time(), requests = rep$requests, forced = isTRUE(force),
      outstanding = if (rep$ready) NULL else rep$rows)))
    if (isTRUE(verbose)) {
      rq <- rep$requests
      for (i in seq_len(nrow(rq))) {
        message(rq$element[i], ": ", rq$axis[i], " ", rq$from[i], " -> ",
                rq$to[i], " (", rq$objects[i], " object",
                if (rq$objects[i] == 1L) "" else "s", ")")
      }
    }
    object
  }
)
