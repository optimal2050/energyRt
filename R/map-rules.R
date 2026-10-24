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
# column form a cross product), kept where every `gate` table matches and no
# `minus` table does (on the columns shared with the join), projected to
# `cols` and made unique
.map_rule <- function(of, tables, cols, gate = NULL, minus = NULL) {
  gate <- as.character(gate %||% character())
  minus <- as.character(minus %||% character())
  stopifnot(is.character(of), length(of) >= 1L, is.list(tables),
            !is.null(names(tables)), all(c(of, gate, minus) %in% names(tables)),
            is.character(cols), length(cols) >= 1L)
  tables <- lapply(tables, function(t) {
    t <- as.data.frame(t, stringsAsFactors = FALSE)
    for (j in seq_along(t)) if (!is.character(t[[j]])) t[[j]] <- as.character(t[[j]])
    rownames(t) <- NULL
    unique(t)
  })
  r <- list(of = of, gate = gate, minus = minus, tables = tables, cols = cols)
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
  # semi- and anti-joins on the shared columns, in the order of the join
  matched <- function(nm) {
    t <- rule$tables[[nm]]
    shared <- intersect(names(out), names(t))
    if (!length(shared)) stop("map rule: '", nm, "' shares no column with the join.", call. = FALSE)
    ut <- unique(t[, shared, drop = FALSE])
    !is.na(data.table::as.data.table(ut)[data.table::as.data.table(out), on = shared,
                                         which = TRUE, mult = "first"])
  }
  for (nm in rule$gate %||% character()) out <- out[matched(nm), , drop = FALSE]
  for (nm in rule$minus %||% character()) out <- out[!matched(nm), , drop = FALSE]
  miss <- setdiff(rule$cols, names(out))
  if (length(miss)) {
    stop("map rule: column(s) ", paste(miss, collapse = ", "), " are in none of its tables.",
         call. = FALSE)
  }
  out <- dplyr::distinct(out[, rule$cols, drop = FALSE])
  rownames(out) <- NULL
  # the classes a built table has (integer year, character members)
  data.table::as.data.table(force_cols_classes(out))
}

# record a rule on a map parameter; its columns must be the parameter's dims.
# A map that got no rows takes no rule: the table, empty, stays the definition.
.set_map_rule <- function(scen, name, rule) {
  if (is.null(rule)) return(scen)
  p <- scen@modInp@parameters[[name]]
  if (is.null(p)) return(scen)
  n <- .param_nrow(p)
  if (is.na(n) || n == 0L) return(scen)
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
  verify <- isTRUE(getOption("energyRt.map_rules_verify", FALSE))
  for (nm in nms) {
    p <- scen@modInp@parameters[[nm]]
    if (verify) .verify_map_rule(p, nm)
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

# `options(energyRt.map_rules_verify = TRUE)`: the rule must enumerate the
# table the builder wrote, tuple for tuple, else the interpolation stops.
.verify_map_rule <- function(p, nm) {
  tup <- function(d) {
    d <- as.data.frame(d)[, p@dimSets, drop = FALSE]
    for (j in seq_along(d)) d[[j]] <- as.character(d[[j]])
    d <- unique(d)
    d <- d[do.call(order, d), , drop = FALSE]
    rownames(d) <- NULL
    d
  }
  a <- tup(get_data_slot(p))
  b <- tup(.rule_tuples(p@misc$rule))
  if (!identical(a, b)) {
    stop("map rule for '", nm, "' enumerates ", nrow(b), " tuples where the table has ",
         nrow(a), ".", call. = FALSE)
  }
  invisible(TRUE)
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

# --- the activity, output and single-output domains ---------------------------
#
# mvTechAct = mTechSpan x the technology's timeslices, less the (tech, region)
# cells with an unavailable input; mvTechOut adds the output commodities and
# keeps the commodity's regions; meqTechActSng keeps the one-commodity
# technologies and drops the cells whose conversion factor is zero. The last
# two are exact only without coarse processes: a coarser process spreads or
# lifts its flows, which the rule does not express, so the table stays.

# no process coarser than a commodity it touches, on either axis
.no_coarse_processes <- function(scen) {
  region_ok <- !.coarse_axis_ready("region") || is.null(.flow_region_agg(scen))
  slice_ok <- !.coarse_axis_ready("timeslice") || is.null(.flow_timeslice_agg(scen))
  region_ok && slice_ok
}

.mvTechAct_rule <- function(scen) {
  span <- .gds(scen, "mTechSpan")
  ts <- .proc_timeslice_rule(scen, "technology", "tech")
  if (is.null(span) || is.null(ts)) return(NULL)
  tables <- c(list(span = span[, c("tech", "region", "year"), drop = FALSE]), ts$tables)
  minus <- NULL
  gaps <- .region_gap_pairs(scen, "tech", "technology")
  if (!is.null(gaps) && nrow(gaps) > 0) {
    tables$gaps <- gaps
    minus <- "gaps"
  }
  .map_rule(of = c("span", "proc_frame", "frame_slice"), tables = tables, minus = minus,
            cols = c("tech", "region", "year", "timeslice"))
}

.mvTechOut_rule <- function(scen) {
  if (!.no_coarse_processes(scen)) return(NULL)
  base <- .mvTechAct_rule(scen)
  out <- .gds(scen, "mTechOutComm")
  cr <- .gds(scen, "mCommReg")
  if (is.null(base) || is.null(out) || is.null(cr)) return(NULL)
  tables <- base$tables
  tables$outcomm <- out[, c("tech", "comm"), drop = FALSE]
  tables$commreg <- cr[, c("comm", "region"), drop = FALSE]
  .map_rule(of = c(base$of, "outcomm"), gate = "commreg", minus = base$minus, tables = tables,
            cols = c("tech", "comm", "region", "year", "timeslice"))
}

.meqTechActSng_rule <- function(scen) {
  if (!.no_coarse_processes(scen)) return(NULL)
  base <- .mvTechOut_rule(scen)
  one <- .gds(scen, "mTechOneComm")
  if (is.null(base) || is.null(one)) return(NULL)
  tables <- base$tables
  tables$onecomm <- one[, c("tech", "comm"), drop = FALSE]
  minus <- base$minus
  conv <- .gds(scen, "pTechCact2cout")
  if (!is.null(conv) && !is.null(conv$value)) {
    zero <- conv[!is.na(conv$value) & conv$value == 0, , drop = FALSE]
    keep <- intersect(c("tech", "comm", "region", "year", "timeslice"), colnames(zero))
    if (nrow(zero) > 0 && length(keep)) {
      tables$zero <- unique(zero[, keep, drop = FALSE])
      minus <- c(minus, "zero")
    }
  }
  .map_rule(of = c(base$of, "onecomm"), gate = base$gate, minus = minus, tables = tables,
            cols = c("tech", "comm", "region", "year", "timeslice"))
}

# --- constraint domains that are cells of the activity domain ----------------
#
# The availability-factor maps hold, for each (tech, region, year) cell they
# cover, either every timeslice of mvTechAct or none: the bound is one number
# per cell when pTechAf carries no per-timeslice rows. Such a map is
# mvTechAct gated by its cells. The builder's table decides: the rule is
# recorded only when the table equals the activity domain restricted to the
# table's own cells, which one semi-join counts.
.cells_rule <- function(scen, name, cell_cols = c("tech", "region", "year")) {
  tbl <- .gds(scen, name)
  act <- .gds(scen, "mvTechAct")
  base <- .mvTechAct_rule(scen)
  if (is.null(tbl) || is.null(act) || is.null(base)) return(NULL)
  if (!all(cell_cols %in% colnames(tbl)) || !all(cell_cols %in% colnames(act))) return(NULL)
  cells <- dplyr::distinct(as.data.frame(tbl)[, cell_cols, drop = FALSE])
  if (anyNA(cells)) return(NULL)
  a <- data.table::as.data.table(act)
  c2 <- data.table::as.data.table(force_cols_classes(cells))
  covered <- sum(!is.na(c2[a, on = cell_cols, which = TRUE, mult = "first"]))
  if (covered != nrow(tbl)) return(NULL)
  tables <- base$tables
  tables$cells <- cells
  .map_rule(of = base$of, gate = "cells", minus = base$minus, tables = tables,
            cols = c("tech", "region", "year", "timeslice"))
}

.set_cells_rules <- function(scen, names) {
  for (nm in names) scen <- .set_map_rule(scen, nm, .cells_rule(scen, nm))
  scen
}
