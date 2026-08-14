# =============================================================================
# combine_vintages() — merge per-vintage technology objects into one
# =============================================================================
# Workbooks and older catalogs (DEA, PyPSA technology-data) are often
# imported as one technology object PER vintage year (FDAC_S_VIN2030,
# FDAC_S_VIN2040, ...). The vintage machinery makes that one object with a
# `@vintage` table and vintage-keyed parameter rows; this is the
# combinator that gets you there from the per-vintage list.
#
# Policy (mirrors the techspec rules): nothing is invented — every value
# comes from one of the inputs, tagged with its vintage; rows identical
# across ALL vintages collapse to a single all-vintages row (vintage NA)
# so the combined tables stay readable.

#' Combine per-vintage technology objects into one vintaged technology
#'
#' Merges a list of `technology` objects, one per build-year vintage
#' (e.g. `FDAC_S_VIN2030`, `FDAC_S_VIN2040`, ...), into a single
#' technology with a [`@vintage`][newTechnology] table and vintage-keyed
#' parameter rows — the "one object, many vintages" form introduced with
#' the vintage machinery.
#'
#' * Ports (`input`/`output`/`aux`/`group`) are the union across
#'   vintages (a commodity present only in some vintages is fine: its
#'   `ceff` rows carry the vintage key).
#' * Structural attributes (`units`, `cap2act`, `timeframe`, `region`,
#'   `fullYear`) are taken from the first object; differences warn.
#' * Every parameter row is tagged with its object's vintage; rows that
#'   are identical across ALL vintages collapse to one all-vintages row
#'   (disable with `collapse_common = FALSE`).
#' * Investment windows: each vintage's `start` is kept (defaulting to
#'   the vintage year); with `close_windows = TRUE` a missing `end` is
#'   closed at the next vintage's start minus one (the last stays open).
#'
#' @param techs A named list of `technology` objects, one per vintage.
#'   Objects declaring clusters are not supported.
#' @param vintages Optional character vector of vintage labels (same
#'   length as `techs`). Default: parsed from `names(techs)`
#'   (`"..._VIN<label>"`), falling back to each object's
#'   `@vintage$start`.
#' @param name,desc Name/description of the combined technology.
#'   Defaults: the common `"..._VIN"` prefix of the input names, and the
#'   first object's description.
#' @param close_windows Close open-ended investment windows at the next
#'   vintage's start (default `TRUE`).
#' @param collapse_common Collapse rows identical across all vintages to
#'   a single all-vintages row (default `TRUE`).
#' @return A `technology` object with one `@vintage` row per input.
#' @examples
#' \dontrun{
#' dac <- make_dea_dac_techs()   # FDAC_S_VIN2020, FDAC_S_VIN2030, ...
#' fam <- split(dac, sub("_VIN[0-9]+$", "", names(dac)))
#' dac1 <- lapply(fam, combine_vintages)
#' draw(dac1$FDAC_S)             # vintage fan
#' }
#' @export
combine_vintages <- function(techs, vintages = NULL, name = NULL,
                             desc = NULL, close_windows = TRUE,
                             collapse_common = TRUE) {
  if (is(techs, "technology")) techs <- list(techs)
  if (!is.list(techs) || length(techs) == 0L ||
      !all(vapply(techs, is, logical(1), "technology"))) {
    stop("`techs` must be a non-empty list of technology objects",
         call. = FALSE)
  }
  for (tt in techs) {
    if (nrow(tt@cluster) > 0) {
      stop("combine_vintages() does not support clustered inputs (object '",
           tt@name, "' declares clusters)", call. = FALSE)
    }
  }

  # -- vintage labels ----------------------------------------------------------
  nms <- names(techs) %||% vapply(techs, function(t) t@name, character(1))
  if (is.null(nms) || any(!nzchar(nms))) {
    nms <- vapply(techs, function(t) t@name, character(1))
  }
  if (is.null(vintages)) {
    mm <- regexpr("_VIN([A-Za-z0-9]+)$", nms)
    vintages <- rep(NA_character_, length(nms))
    vintages[mm > 0] <- sub("^_VIN", "", regmatches(nms, mm))
    for (i in which(is.na(vintages))) {
      st <- techs[[i]]@vintage$start
      st <- st[!is.na(st)]
      if (length(st) > 0) vintages[i] <- as.character(st[1])
    }
    if (anyNA(vintages)) {
      stop("cannot infer vintage labels; give `vintages=` (no _VIN suffix ",
           "in names and no start year for: ",
           paste(nms[is.na(vintages)], collapse = ", "), ")",
           call. = FALSE)
    }
  }
  vintages <- as.character(vintages)
  if (length(vintages) != length(techs)) {
    stop("`vintages` must match `techs` in length", call. = FALSE)
  }
  if (anyDuplicated(vintages)) {
    stop("duplicate vintage labels: ",
         paste(unique(vintages[duplicated(vintages)]), collapse = ", "),
         call. = FALSE)
  }

  # sort by numeric label when possible
  ord <- order(suppressWarnings(as.numeric(vintages)), vintages)
  techs <- techs[ord]; vintages <- vintages[ord]; nms <- nms[ord]

  # -- identity ----------------------------------------------------------------
  base <- unique(sub("_VIN[A-Za-z0-9]+$", "", nms))
  if (is.null(name)) {
    if (length(base) != 1L) {
      stop("inputs have different base names (", paste(base, collapse = ", "),
           "); give `name=`", call. = FALSE)
    }
    name <- base
  }
  first <- techs[[1L]]
  if (is.null(desc)) {
    desc <- sub(",?\\s*vintage.*$", "", first@desc)
  }

  # -- structural attributes: first object, warn on differences ---------------
  .warn_differs <- function(what, differs) {
    if (differs) {
      warning("`", what, "` differs across vintages; using the first ",
              "object's value", call. = FALSE)
    }
  }
  for (tt in techs[-1L]) {
    .warn_differs("units", !identical(tt@units, first@units))
    .warn_differs("cap2act", !isTRUE(all.equal(tt@cap2act, first@cap2act)))
    .warn_differs("timeframe", !identical(tt@timeframe, first@timeframe))
    .warn_differs("region", !identical(tt@region, first@region))
    .warn_differs("fullYear", !identical(tt@fullYear, first@fullYear))
  }

  # -- ports: union across vintages -------------------------------------------
  .union_port <- function(slot_nm, key) {
    parts <- lapply(techs, function(t) methods::slot(t, slot_nm))
    parts <- parts[vapply(parts, nrow, integer(1)) > 0]
    if (length(parts) == 0L) return(NULL)
    all_rows <- do.call(rbind, parts)
    dup <- duplicated(all_rows[[key]])
    conflicting <- vapply(split(all_rows, all_rows[[key]]), function(g) {
      nrow(unique(g)) > 1L
    }, logical(1))
    if (any(conflicting)) {
      warning("port(s) with conflicting definitions across vintages ",
              "(keeping the first): ",
              paste(names(conflicting)[conflicting], collapse = ", "),
              call. = FALSE)
    }
    out <- all_rows[!dup, , drop = FALSE]
    rownames(out) <- NULL
    out
  }

  # -- vintage table -----------------------------------------------------------
  # each input's window/life is collected per REGION (region-specific rows
  # are kept, not flattened to the first value); the region column is
  # dropped again when no input uses it
  vt_parts <- lapply(seq_along(techs), function(i) {
    parts <- lapply(c("start", "end", "olife"), function(cc) {
      d <- .lifespan_resolve(techs[[i]], cc)
      d[, c("region", cc), drop = FALSE]
    })
    parts <- Filter(function(p) nrow(p) > 0L, parts)
    merged <- if (length(parts) > 0L) {
      Reduce(function(a, b) dplyr::full_join(a, b, by = "region"), parts)
    } else {
      data.frame(region = NA_character_, stringsAsFactors = FALSE)
    }
    for (cc in c("start", "end", "olife")) {
      if (!cc %in% names(merged)) merged[[cc]] <- NA_real_
    }
    merged$vintage <- vintages[i]
    merged
  })
  vt <- dplyr::bind_rows(vt_parts)[, c("vintage", "region", "start",
                                       "end", "olife")]
  num_lab <- suppressWarnings(as.numeric(vintages))
  lab_of <- match(vt$vintage, vintages)
  vt$start[is.na(vt$start)] <- num_lab[lab_of][is.na(vt$start)]
  if (isTRUE(close_windows) && length(techs) > 1L) {
    # next vintage's earliest start closes an open end
    nxt_start <- vapply(seq_along(vintages), function(i) {
      if (i == length(vintages)) return(NA_real_)
      s <- vt$start[vt$vintage == vintages[i + 1L]]
      s <- s[!is.na(s)]
      if (length(s) > 0) min(s) else num_lab[i + 1L]
    }, numeric(1))
    nxt <- nxt_start[lab_of] - 1
    fix <- is.na(vt$end) & !is.na(nxt)
    vt$end[fix] <- nxt[fix]
  }
  if (all(is.na(vt$region))) vt$region <- NULL
  rownames(vt) <- NULL

  # -- parameter slots: tag with vintage, rbind, collapse common rows ---------
  .combine_slot <- function(slot_nm) {
    parts <- list()
    for (i in seq_along(techs)) {
      df <- methods::slot(techs[[i]], slot_nm)
      if (!is.data.frame(df) || nrow(df) == 0L) next
      if ("vintage" %in% names(df) && any(!is.na(df$vintage))) {
        warning("object '", nms[i], "' already carries vintage keys in @",
                slot_nm, "; they are overwritten", call. = FALSE)
      }
      df$vintage <- vintages[i]
      parts[[length(parts) + 1L]] <- df
    }
    if (length(parts) == 0L) return(NULL)
    out <- do.call(rbind, parts)
    rownames(out) <- NULL
    if (isTRUE(collapse_common) && length(techs) > 1L) {
      rest <- setdiff(names(out), "vintage")
      key <- do.call(paste, c(lapply(out[rest], as.character), sep = "\r"))
      full <- names(table(key))[table(key) == length(techs)]
      is_full <- key %in% full
      shared <- out[is_full & !duplicated(key), , drop = FALSE]
      if (nrow(shared) > 0) shared$vintage <- NA_character_
      out <- rbind(shared, out[!is_full, , drop = FALSE])
      rownames(out) <- NULL
    }
    # drop all-NA columns; newTechnology re-adds the full prototypes
    keep <- vapply(out, function(x) any(!is.na(x)), logical(1))
    out <- out[, keep, drop = FALSE]
    if (ncol(out) == 0L || nrow(out) == 0L) NULL else out
  }

  # -- assemble ----------------------------------------------------------------
  args <- list(name = name, desc = desc, vintage = vt)
  for (p in list(c("input", "comm"), c("output", "comm"),
                 c("aux", "acomm"), c("group", "group"))) {
    u <- .union_port(p[1], p[2])
    if (!is.null(u)) args[[p[1]]] <- u
  }
  if (nrow(first@units) > 0) args$units <- first@units
  if (length(first@cap2act) == 1 && is.finite(first@cap2act)) {
    args$cap2act <- first@cap2act
  }
  if (length(first@timeframe) > 0 && nzchar(first@timeframe[1])) {
    args$timeframe <- first@timeframe
  }
  if (length(first@region) > 0) args$region <- first@region
  args$fullYear <- first@fullYear
  if (isTRUE(first@optimizeRetirement)) args$optimizeRetirement <- TRUE
  for (sl in c("ceff", "geff", "aeff", "af", "afs", "weather",
               "invcost", "fixom", "varom", "capacity")) {
    cs <- .combine_slot(sl)
    if (!is.null(cs)) args[[sl]] <- cs
  }
  misc <- first@misc
  misc$combined_from <- stats::setNames(as.list(nms), vintages)
  if (length(misc) > 0) args$misc <- misc

  do.call(newTechnology, args)
}
