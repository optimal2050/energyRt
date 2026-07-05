# Read/write `technology` objects to/from Excel workbooks ####################
#
# Layout: one technology per worksheet. Every slot of the class is written as a
# self-contained mini-table ("block"). Column A holds the slot name on the
# block's header row only; the block's columns (identical to the columns of the
# data.frame slot, i.e. `names(slot(tech, s))`) start in column B. Data rows
# leave column A empty, so the reader can locate blocks by scanning column A.
#
#   A          B        C       D       E
#   meta       field    value
#              name      LDV_GSL
#              desc      Gasoline Light Duty Vehicle
#              cap2act   12.1
#              ...
#   <blank>
#   input      comm      unit    group   combustion
#              GSL       PJ
#   <blank>
#   output     comm      unit    group
#              PLDVHWY   MPKm    o
#              PLDVCTY   MPKm    o
#   ...
#
# Scalar/atomic slots (name, desc, cap2act, region, timeframe, fullYear,
# optimizeRetirement, misc) live in the "meta" block as field/value pairs.
# Vector values (region, timeframe) are collapsed with " | "; `misc` is stored
# as a JSON string. data.frame slots become one block each.

# helpers ###################################################################

# data.frame slot names of the technology class, in declaration order
.tech_df_slots <- function() {
  sl <- getSlots("technology")
  names(sl)[sl == "data.frame"]
}

# blank = NA or empty/whitespace-only string
.is_blank <- function(x) {
  is.na(x) | !nzchar(trimws(as.character(x)))
}

# collapse a (possibly empty) atomic vector to a single cell string
.vec2cell <- function(x, sep = " | ") {
  if (length(x) == 0) return("")
  paste(as.character(x), collapse = sep)
}

# split a meta cell back into a character vector (character(0) when blank)
.cell2vec <- function(x, sep = "|") {
  if (length(x) == 0 || .is_blank(x)) return(character(0))
  trimws(strsplit(as.character(x), sep, fixed = TRUE)[[1]])
}

# coerce an all-character data.frame to the column types of a prototype slot
.coerce_to_proto <- function(df, proto) {
  if (is.null(df) || nrow(df) == 0) return(proto[0, , drop = FALSE])
  out <- proto[0, , drop = FALSE]
  for (col in names(df)) {
    v <- df[[col]]
    v[.is_blank(v)] <- NA
    target <- if (col %in% names(proto)) class(proto[[col]])[1] else "character"
    out[seq_len(nrow(df)), col] <- switch(target,
      integer   = as.integer(round(as.numeric(v))),
      numeric   = as.numeric(v),
      double    = as.numeric(v),
      logical   = as.logical(v),
      factor    = as.character(v),
      as.character(v)
    )
  }
  rownames(out) <- NULL
  out
}

# write ######################################################################
#' Write `technology` object(s) to an Excel workbook
#'
#' Serialises one or more [technology] objects to a `.xlsx` file, one technology
#' per worksheet. Each slot of the class is written as a labelled mini-table
#' whose header row matches the columns of the corresponding data.frame slot
#' (`names(slot(tech, s))`). Scalar slots (`name`, `desc`, `cap2act`, `region`,
#' `timeframe`, `fullYear`, `optimizeRetirement`, `misc`) are written into a
#' leading `meta` block. The workbook can be read back with
#' [read_technology_xlsx()].
#'
#' @param x A `technology` object, a (named) list of `technology` objects, or a
#'   `repository` (its `technology` members are written).
#' @param path Path to the output `.xlsx` file.
#' @param include_empty Logical; if `TRUE` (default) every data.frame slot is
#'   written, including empty ones (header only) so the full class structure is
#'   visible and the file can serve as an editable template. If `FALSE`, only
#'   slots with at least one row are written.
#' @param drop_empty_cols Logical; if `TRUE`, columns that are entirely empty
#'   (all `NA`) within a populated slot's mini-table are dropped, so only
#'   columns that carry data are written. Empty slots (no rows) keep their full
#'   header. Default `FALSE`. Round-trips safely: missing columns are restored
#'   from the class prototype on read.
#' @param header_fill Header-row background colour (hex string, e.g.
#'   `"#DDEBF7"`). Set to `NA` or `NULL` to disable the fill (headers stay
#'   bold). Default `"#DDEBF7"` (light blue).
#' @param info Logical; if `TRUE` (default) a leading `Info` worksheet is added
#'   documenting every slot and column of the `technology` class (names, types
#'   and descriptions, taken from the class documentation). [read_technology_xlsx()]
#'   ignores this sheet.
#' @param overwrite Logical; overwrite `path` if it exists (default `TRUE`).
#'
#' @return (Invisibly) `path`.
#' @seealso [read_technology_xlsx()]
#' @family technology process
#' @export
write_technology_xlsx <- function(x, path, include_empty = TRUE,
                                  drop_empty_cols = FALSE,
                                  header_fill = "#DDEBF7",
                                  info = TRUE,
                                  overwrite = TRUE) {
  if (!requireNamespace("openxlsx", quietly = TRUE))
    stop("Package 'openxlsx' is required: install.packages('openxlsx')")

  techs <- .as_technology_list(x)
  if (length(techs) == 0) stop("No `technology` objects found in `x`.")

  df_slots <- .tech_df_slots()
  wb <- openxlsx::createWorkbook()
  use_fill <- !is.null(header_fill) && !is.na(header_fill)
  # header-row style for the column names (and the meta field/value labels)
  hdr_styl <- if (use_fill)
    openxlsx::createStyle(textDecoration = "bold", fgFill = header_fill,
                          border = "bottom", borderColour = "#9DC3E6")
  else openxlsx::createStyle(textDecoration = "bold")
  # tag cell (column A) style: same fill, darker font to stand out
  tag_styl <- if (use_fill)
    openxlsx::createStyle(textDecoration = "bold", fontColour = "#1F4E78",
                          fgFill = header_fill, border = "bottom",
                          borderColour = "#9DC3E6")
  else openxlsx::createStyle(textDecoration = "bold", fontColour = "#1F4E78")

  # --- Info sheet (slot & column reference) --------------------------------
  if (isTRUE(info)) .write_info_sheet(wb, hdr_styl, tag_styl)

  used_sheets <- character(0)
  for (nm in names(techs)) {
    tech <- techs[[nm]]
    sheet <- .unique_sheet_name(nm, used_sheets)
    used_sheets <- c(used_sheets, sheet)
    openxlsx::addWorksheet(wb, sheet)

    r <- 1L
    # --- meta block (scalar slots) -----------------------------------------
    meta <- data.frame(
      field = c("name", "desc", "cap2act", "region", "timeframe",
                "fullYear", "optimizeRetirement", "misc"),
      value = c(
        tech@name,
        tech@desc,
        .vec2cell(tech@cap2act),
        .vec2cell(tech@region),
        .vec2cell(tech@timeframe),
        as.character(tech@fullYear),
        as.character(tech@optimizeRetirement),
        if (length(tech@misc) && requireNamespace("jsonlite", quietly = TRUE))
          as.character(jsonlite::toJSON(tech@misc, auto_unbox = TRUE,
                                        null = "null", na = "null"))
        else {
          if (length(tech@misc))
            warning("Slot `misc` of '", tech@name,
                    "' not written (package 'jsonlite' unavailable).")
          ""
        }
      ),
      stringsAsFactors = FALSE
    )
    r <- .write_block(wb, sheet, "meta", c("field", "value"), meta, r,
                      hdr_styl, tag_styl)

    # --- data.frame slots --------------------------------------------------
    for (s in df_slots) {
      d <- slot(tech, s)
      if (!include_empty && nrow(d) == 0) next
      if (drop_empty_cols && nrow(d) > 0) {
        keep <- vapply(d, function(col) any(!.is_blank(col)), logical(1))
        if (any(keep)) d <- d[, keep, drop = FALSE]
      }
      r <- .write_block(wb, sheet, s, names(d), d, r, hdr_styl, tag_styl)
    }

    openxlsx::setColWidths(wb, sheet, cols = 1:14,
                           widths = c(12, rep(14, 13)))
  }

  openxlsx::saveWorkbook(wb, path, overwrite = overwrite)
  invisible(path)
}

# write one block; return the next free row (one blank row left as separator)
.write_block <- function(wb, sheet, tag, headers, data, row, hdr_styl, tag_styl) {
  headers <- as.character(headers)
  ncol_blk <- length(headers)
  # header row: tag in col A, column names in cols B..
  hdr <- matrix(c(tag, headers), nrow = 1)
  openxlsx::writeData(wb, sheet, hdr, startCol = 1, startRow = row,
                      colNames = FALSE)
  openxlsx::addStyle(wb, sheet, tag_styl, rows = row, cols = 1)
  if (ncol_blk > 0)
    openxlsx::addStyle(wb, sheet, hdr_styl, rows = row,
                       cols = 2:(1 + ncol_blk), gridExpand = TRUE)
  # data rows: values from col B.. (col A left empty)
  if (!is.null(data) && nrow(data) > 0) {
    openxlsx::writeData(wb, sheet, data, startCol = 2, startRow = row + 1,
                        colNames = FALSE, keepNA = FALSE)
    row <- row + nrow(data)
  }
  row + 2L # one filled header/data span consumed + one blank separator
}

# slot/column documentation table for the technology class (or NULL)
.tech_class_doc <- function() {
  cl <- tryCatch(get(".classes", envir = asNamespace("energyRt")),
                 error = function(e) NULL)
  if (is.null(cl) || !"class" %in% names(cl)) return(NULL)
  cl[cl$class == "technology", , drop = FALSE]
}

# write the leading "Info" worksheet describing slots and columns
.write_info_sheet <- function(wb, hdr_styl, tag_styl) {
  doc <- .tech_class_doc()
  openxlsx::addWorksheet(wb, "Info")

  title <- "technology class — slot & column reference"
  openxlsx::writeData(wb, "Info", title, startCol = 1, startRow = 1,
                      colNames = FALSE)
  openxlsx::addStyle(wb, "Info",
                     openxlsx::createStyle(textDecoration = "bold",
                                           fontSize = 12, fontColour = "#1F4E78"),
                     rows = 1, cols = 1)

  header <- c("Slot", "Type", "Column", "Col.type", "Description")
  openxlsx::writeData(wb, "Info", matrix(header, nrow = 1),
                      startCol = 1, startRow = 3, colNames = FALSE)
  openxlsx::addStyle(wb, "Info", hdr_styl, rows = 3, cols = seq_along(header),
                     gridExpand = TRUE)

  if (is.null(doc) || nrow(doc) == 0) {
    openxlsx::writeData(wb, "Info",
                        "Class documentation unavailable in this build.",
                        startCol = 1, startRow = 4, colNames = FALSE)
    openxlsx::setColWidths(wb, "Info", cols = 1:5,
                           widths = c(16, 12, 14, 10, 80))
    return(invisible())
  }

  clean <- function(x) gsub("[\r\n]+", " ", trimws(as.character(x)))
  rows     <- list()
  slot_rows <- integer(0) # 1-based offsets within the body, for bold styling
  i <- 0L
  for (s in unique(doc$slotname)) {
    sd   <- doc[doc$slotname == s, , drop = FALSE]
    type <- sd$type[1]
    i <- i + 1L; slot_rows <- c(slot_rows, i)
    rows[[i]] <- c(s, type, "", "", clean(sd$description[1]))
    if (identical(type, "data.frame")) {
      cols <- sd[!is.na(sd$col.name), , drop = FALSE]
      for (j in seq_len(nrow(cols))) {
        i <- i + 1L
        rows[[i]] <- c("", "", clean(cols$col.name[j]),
                       clean(cols$col.type[j]), clean(cols$col.description[j]))
      }
    }
  }
  body <- do.call(rbind, rows)
  start <- 4L
  openxlsx::writeData(wb, "Info", body, startCol = 1, startRow = start,
                      colNames = FALSE)
  # bold + colour the slot-name rows so each block stands out
  openxlsx::addStyle(wb, "Info",
                     openxlsx::createStyle(textDecoration = "bold",
                                           fontColour = "#1F4E78"),
                     rows = start - 1L + slot_rows, cols = 1:2,
                     gridExpand = TRUE, stack = TRUE)
  openxlsx::addStyle(wb, "Info",
                     openxlsx::createStyle(wrapText = TRUE,
                                           valign = "top"),
                     rows = start:(start + nrow(body) - 1L), cols = 5,
                     gridExpand = TRUE, stack = TRUE)
  openxlsx::setColWidths(wb, "Info", cols = 1:5,
                         widths = c(16, 12, 14, 10, 90))
  openxlsx::freezePane(wb, "Info", firstActiveRow = 4)
  invisible()
}

# read #######################################################################
#' Read `technology` object(s) from an Excel workbook
#'
#' Inverse of [write_technology_xlsx()]. Reads the block-structured worksheets
#' produced by that function and rebuilds [technology] objects, coercing each
#' mini-table's columns back to the types declared by the class.
#'
#' @param path Path to the `.xlsx` file.
#' @param sheet Optional sheet name or index. If a single sheet is given, a
#'   single `technology` object is returned; otherwise all sheets are read.
#' @param as_repository Logical; if `TRUE`, wrap the result in a `repository`.
#'
#' @return A `technology` object (single sheet), a named list of `technology`
#'   objects, or a `repository` (when `as_repository = TRUE`).
#' @seealso [write_technology_xlsx()]
#' @family technology process
#' @export
read_technology_xlsx <- function(path, sheet = NULL, as_repository = FALSE) {
  if (!requireNamespace("openxlsx", quietly = TRUE))
    stop("Package 'openxlsx' is required: install.packages('openxlsx')")
  if (!file.exists(path)) stop("File not found: ", path)

  all_sheets <- openxlsx::getSheetNames(path)
  want <- if (is.null(sheet)) {
    # skip the documentation sheet when reading the whole workbook
    all_sheets[tolower(all_sheets) != "info"]
  } else {
    if (is.numeric(sheet)) all_sheets[sheet] else sheet
  }
  single <- !is.null(sheet) && length(want) == 1L

  techs <- lapply(want, function(s) .read_one_sheet(path, s))
  names(techs) <- vapply(techs, function(t) t@name, character(1))

  if (single) return(techs[[1]])
  if (as_repository) {
    repo <- newRepository("repo_technology")
    return(Reduce(add, techs, repo))
  }
  techs
}

# read a single worksheet into a technology object
.read_one_sheet <- function(path, sheet) {
  m <- openxlsx::read.xlsx(path, sheet = sheet, colNames = FALSE,
                           skipEmptyRows = FALSE, skipEmptyCols = FALSE)
  if (is.null(m) || nrow(m) == 0)
    stop("Worksheet '", sheet, "' is empty.")
  M <- as.matrix(m)
  storage.mode(M) <- "character"
  blocks <- .parse_blocks(M)

  proto <- new("technology")
  args  <- list(name = "")

  # meta block -> scalar slots
  if (!is.null(blocks[["meta"]])) {
    mb <- blocks[["meta"]]
    kv <- stats::setNames(mb[["value"]], mb[["field"]])
    get_kv <- function(k) if (k %in% names(kv)) kv[[k]] else NA_character_
    args$name <- if (.is_blank(get_kv("name"))) "" else get_kv("name")
    args$desc <- if (.is_blank(get_kv("desc"))) "" else get_kv("desc")
    if (!.is_blank(get_kv("cap2act")))
      args$cap2act <- as.numeric(get_kv("cap2act"))
    if (length(.cell2vec(get_kv("region"))))
      args$region <- .cell2vec(get_kv("region"))
    if (length(.cell2vec(get_kv("timeframe"))))
      args$timeframe <- .cell2vec(get_kv("timeframe"))
    if (!.is_blank(get_kv("fullYear")))
      args$fullYear <- as.logical(get_kv("fullYear"))
    if (!.is_blank(get_kv("optimizeRetirement")))
      args$optimizeRetirement <- as.logical(get_kv("optimizeRetirement"))
    mj <- get_kv("misc")
    if (!.is_blank(mj)) {
      if (requireNamespace("jsonlite", quietly = TRUE))
        args$misc <- jsonlite::fromJSON(mj, simplifyVector = FALSE,
                                        simplifyDataFrame = FALSE)
      else
        warning("Slot `misc` not restored (package 'jsonlite' unavailable).")
    }
  }

  # data.frame slots
  for (s in .tech_df_slots()) {
    if (is.null(blocks[[s]])) next
    df <- .coerce_to_proto(blocks[[s]], slot(proto, s))
    if (nrow(df) > 0) args[[s]] <- df
  }

  do.call(newTechnology, args)
}

# split a character matrix into named blocks keyed by the column-A tag
.parse_blocks <- function(M) {
  nr <- nrow(M); nc <- ncol(M)
  blocks <- list()
  r <- 1L
  while (r <= nr) {
    tag <- M[r, 1]
    if (.is_blank(tag)) { r <- r + 1L; next }
    # header columns: cells B.. on the tag row, up to the last non-blank one
    hdr <- M[r, -1, drop = TRUE]
    keep <- which(!.is_blank(hdr))
    headers <- if (length(keep)) as.character(hdr[seq_len(max(keep))]) else character(0)
    ncol_blk <- length(headers)
    # collect data rows until a blank-A non-data row breaks, or a new tag
    rows <- list()
    r2 <- r + 1L
    while (r2 <= nr && .is_blank(M[r2, 1])) {
      if (all(.is_blank(M[r2, ]))) break # blank separator row
      if (ncol_blk > 0)
        rows[[length(rows) + 1L]] <- M[r2, 2:(1 + ncol_blk), drop = TRUE]
      r2 <- r2 + 1L
    }
    if (ncol_blk > 0) {
      if (length(rows)) {
        body <- do.call(rbind, rows)
        df <- as.data.frame(body, stringsAsFactors = FALSE)
      } else {
        df <- as.data.frame(matrix(character(0), ncol = ncol_blk),
                            stringsAsFactors = FALSE)
      }
      names(df) <- headers
      blocks[[as.character(tag)]] <- df
    }
    r <- r2
  }
  blocks
}

# coercion of input `x` into a named list of technology objects
.as_technology_list <- function(x) {
  if (inherits(x, "technology")) {
    out <- list(x); names(out) <- x@name; return(out)
  }
  if (inherits(x, "repository")) x <- x@data
  if (is.list(x)) {
    keep <- vapply(x, function(o) inherits(o, "technology"), logical(1))
    x <- x[keep]
    nms <- vapply(x, function(o) o@name, character(1))
    nms[.is_blank(nms)] <- names(x)[.is_blank(nms)]
    names(x) <- nms
    return(x)
  }
  stop("`x` must be a technology, a list of technologies, or a repository.")
}

# Excel sheet names: <=31 chars, no []:*?/\ , unique within the workbook
.unique_sheet_name <- function(nm, used) {
  s <- gsub("[\\[\\]:*?/\\\\]", "_", nm, perl = TRUE)
  s <- substr(s, 1, 31)
  if (!s %in% used) return(s)
  base <- substr(s, 1, 27)
  for (i in 1:999) {
    cand <- paste0(base, "_", i)
    if (!cand %in% used) return(cand)
  }
  stop("Cannot create a unique sheet name for '", nm, "'.")
}
