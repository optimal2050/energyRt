# =========================================================================== #
# map-rules.R  —  map parameters defined by a rule instead of a table
#
# Several large maps are joins of small tables: every process with every
# timeslice of its timeframe, every weather series with every leaf timeslice.
# A rule records the small tables and the join; the tuples are enumerated
# when a reader asks for them (`get_data_slot()`), the matrix route gets the
# rule itself through the multimod import and enumerates it in integer codes.
# The builders still write the table during interpolation, so every builder
# that reads it sees what it read before; `.strip_rule_tables()` empties the
# tables of rule-carrying maps at the end of `interpolate_model()`.
#
# A rule names embedded tables only, never another parameter, so its
# evaluation is a chain of natural joins over its own tables.
# =========================================================================== #

# a rule: the natural join of `tables[of]` in order (tables without a shared
# column form a cross product), projected to `cols` and made unique
.map_rule <- function(of, tables, cols) {
  stopifnot(is.character(of), length(of) >= 1L, is.list(tables),
            !is.null(names(tables)), all(of %in% names(tables)),
            is.character(cols), length(cols) >= 1L)
  tables <- lapply(tables, function(t) {
    t <- as.data.frame(t, stringsAsFactors = FALSE)
    for (j in seq_along(t)) if (!is.character(t[[j]])) t[[j]] <- as.character(t[[j]])
    rownames(t) <- NULL
    unique(t)
  })
  r <- list(of = of, tables = tables, cols = cols)
  r$hash <- rlang::hash(r)
  r
}

# the tuples of a rule, a data.table of character columns `cols`
.rule_tuples <- function(rule) {
  out <- NULL
  for (nm in rule$of) {
    t <- rule$tables[[nm]]
    if (is.null(out)) { out <- t; next }
    shared <- intersect(names(out), names(t))
    out <- if (length(shared)) {
      dplyr::inner_join(out, t, by = shared, relationship = "many-to-many")
    } else {
      dplyr::cross_join(out, t)
    }
  }
  miss <- setdiff(rule$cols, names(out))
  if (length(miss)) {
    stop("map rule: column(s) ", paste(miss, collapse = ", "), " are in none of its tables.",
         call. = FALSE)
  }
  out <- dplyr::distinct(out[, rule$cols, drop = FALSE])
  rownames(out) <- NULL
  data.table::as.data.table(out)
}

# record a rule on a map parameter; its columns must be the parameter's dims
.set_map_rule <- function(scen, name, rule) {
  if (is.null(rule)) return(scen)
  p <- scen@modInp@parameters[[name]]
  if (is.null(p)) return(scen)
  if (!identical(as.character(rule$cols), as.character(p@dimSets))) {
    stop("map rule for '", name, "': cols (", paste(rule$cols, collapse = ", "),
         ") must equal the parameter's dims (", paste(p@dimSets, collapse = ", "), ").",
         call. = FALSE)
  }
  p@misc$rule <- rule
  scen@modInp@parameters[[name]] <- p
  scen
}

# names of the map parameters that carry a rule
.rule_params <- function(scen) {
  pp <- scen@modInp@parameters
  names(pp)[vapply(pp, function(p) !is.null(p@misc$rule), logical(1))]
}

# Empty the tables of rule-carrying maps: the rule is their definition from
# here on. An on-disk table is removed and the parameter marked in memory,
# so no reader looks for rows on disk; `options(energyRt.map_rules = FALSE)`
# keeps the tables.
.strip_rule_tables <- function(scen, verbose = FALSE) {
  if (!isTRUE(getOption("energyRt.map_rules", TRUE))) return(scen)
  nms <- .rule_params(scen)
  for (nm in nms) {
    p <- scen@modInp@parameters[[nm]]
    if (isOnDisk(p)) {
      pth <- getObjPath(p)
      if (!is.null(pth) && dir.exists(fp(pth, "data"))) {
        unlink(fp(pth, "data"), recursive = TRUE, force = TRUE)
      }
      p@misc$onDisk <- NULL
      p <- mark_inMemory(p)
    }
    p@misc$param_ref <- NULL
    p@data <- p@data[0, , drop = FALSE]
    scen@modInp@parameters[[nm]] <- p
  }
  if (isTRUE(verbose) && length(nms)) {
    message("Map rules: ", paste(nms, collapse = ", "), " kept as rules, tables dropped")
  }
  scen
}

# --- the join tables behind the timeslice maps -------------------------------

# (timeframe, timeslice) for the given timeframes: a frame's slices from the
# calendar, or the frame's own name when it lists none (as the table
# builders do)
.timeframe_timeslice_df <- function(scen, timeframes) {
  frames <- scen@settings@calendar@timeframes
  tfs <- unique(as.character(timeframes))
  rows <- lapply(tfs, function(tf) {
    sl <- frames[[tf]]
    if (length(sl) == 0) sl <- tf
    data.frame(timeframe = tf, timeslice = as.character(sl), stringsAsFactors = FALSE)
  })
  dplyr::bind_rows(rows)
}

# (process, timeframe) with the calendar's default for a process without one;
# memoised beside .process_timeslice_df()
.process_timeframe_df <- function(scen) {
  cal <- scen@settings@calendar
  .interp_memo(
    key = "process_timeframe_df",
    fingerprint = list(.interp_model_fp(scen), cal@name, cal@default_timeframe),
    compute = function() {
      ptf <- get_process_timeframe(scen)
      tf <- vapply(ptf, function(x) {
        if (length(x) == 0 || is.na(x[1]) || x[1] == "") cal@default_timeframe else as.character(x[1])
      }, character(1))
      data.frame(process = names(ptf), timeframe = unname(tf), stringsAsFactors = FALSE)
    }
  )
}

# the rule of a per-object timeslice map: processes of a class x the slices
# of their timeframe
.proc_timeslice_rule <- function(scen, cls, key) {
  pf <- .process_timeframe_df(scen)
  pclass <- get_process_class(scen)
  if (is.null(pf) || is.null(pclass) || nrow(pf) == 0) return(NULL)
  procs <- names(pclass)[vapply(pclass, function(x) identical(as.character(x)[1], cls), logical(1))]
  pf <- pf[pf$process %in% procs, , drop = FALSE]
  if (nrow(pf) == 0) return(NULL)
  names(pf)[names(pf) == "process"] <- key
  .map_rule(of = c("proc_frame", "frame_slice"),
            tables = list(proc_frame = pf, frame_slice = .timeframe_timeslice_df(scen, pf$timeframe)),
            cols = c(key, "timeslice"))
}

# the rule of mCommTimeslice: commodities x the slices of their timeframe
.comm_timeslice_rule <- function(scen) {
  cal <- scen@settings@calendar
  ctf <- map_comm_timeframe(scen)
  if (length(ctf) == 0) return(NULL)
  tf <- vapply(ctf, function(x) {
    if (length(x) == 0 || is.na(x[1]) || x[1] == "") cal@default_timeframe else as.character(x[1])
  }, character(1))
  cf <- data.frame(comm = names(ctf), timeframe = unname(tf), stringsAsFactors = FALSE)
  .map_rule(of = c("comm_frame", "frame_slice"),
            tables = list(comm_frame = cf, frame_slice = .timeframe_timeslice_df(scen, cf$timeframe)),
            cols = c("comm", "timeslice"))
}
