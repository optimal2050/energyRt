# design: generate R-script that recreates an energyRt object ################

#' Generate an R script that recreates an energyRt object
#'
#' @description
#' `design()` takes an energyRt object and returns (and optionally writes to
#' a file) an R script that recreates it using the appropriate constructor
#' function (e.g. `newTechnology()`, `newCommodity()`, etc.).
#'
#' @param x An energyRt object (e.g. `technology`, `commodity`, `supply`, …).
#' @param file Optional character string. Path to a file to write the script
#'   to. If `NULL` (default), the script is printed to the console.
#' @param var Optional character string. Name of the R variable to assign the
#'   result to in the generated script. Defaults to `x@name`.
#' @param ... Reserved for future use / class-specific arguments.
#'
#' @return Invisibly returns the generated code as a character string.
#'
#' @examples
#' ECOAL <- newTechnology(
#'   name    = "ECOAL",
#'   desc    = "Coal power plant",
#'   input   = data.frame(comm = "COAL", unit = "MMBtu", combustion = 1),
#'   output  = data.frame(comm = "ELC",  unit = "MWh"),
#'   cap2act = 8760,
#'   ceff    = data.frame(comm = "COAL", cinp2use = 1/10),
#'   olife   = data.frame(olife = 30L),
#'   region  = c("R1", "R2")
#' )
#' design(ECOAL)
#'
#' @export
setGeneric("design", function(x, ...) standardGeneric("design"))

# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

# Serialize a single R value (non-data.frame) to a code string.
.val2code <- function(val) {
  paste(deparse(val, width.cutoff = 500L), collapse = "")
}

# Serialize a data.frame to a data.frame(...) code string.
# Columns that are entirely NA are dropped for cleaner output.
# outer_indent: the leading whitespace of the `slotname = ` line,
# so inner content gets outer_indent + "  ".
.df2code <- function(df, outer_indent = "  ") {
  # Drop all-NA columns
  has_data <- vapply(df, function(col) any(!is.na(col)), logical(1L))
  df <- df[, has_data, drop = FALSE]
  if (ncol(df) == 0L || nrow(df) == 0L) return(NULL)

  inner <- paste0(outer_indent, "  ")

  col_lines <- vapply(names(df), function(col) {
    val_code <- paste(deparse(df[[col]], width.cutoff = 60L),
                      collapse = paste0("\n", inner, "  "))
    paste0(inner, col, " = ", val_code)
  }, character(1L), USE.NAMES = FALSE)

  paste0(
    "data.frame(\n",
    paste(col_lines, collapse = ",\n"),
    "\n", outer_indent, ")"
  )
}

# Build a single `  arg = value` line (or multi-line if value spans lines).
.arg_line <- function(nm, code, indent = "  ") {
  paste0(indent, nm, " = ", code)
}

# Finalise and emit/return code.
.emit_code <- function(code, file) {
  if (!is.null(file)) {
    writeLines(code, file)
    invisible(code)
  } else {
    cat(code)
    invisible(code)
  }
}

# ---------------------------------------------------------------------------
# technology
# ---------------------------------------------------------------------------

#' @rdname design
#' @export
setMethod("design", "technology", function(x, file = NULL, var = NULL, ...) {
  var_name <- if (!is.null(var)) var else if (nzchar(x@name)) x@name else "tech"
  outer <- "  "

  # df slots in display order
  df_slots <- c(
    "input", "output", "aux", "units", "group",
    "geff", "ceff", "aeff", "af", "afs", "weather",
    "fixom", "varom", "invcost",
    "start", "end", "olife", "capacity"
  )

  args <- character()

  # name (always include)
  args["name"] <- .val2code(x@name)

  # desc
  if (length(x@desc) > 0L && nzchar(x@desc)) {
    args["desc"] <- .val2code(x@desc)
  }

  # data.frame slots
  for (sl in df_slots) {
    df <- slot(x, sl)
    if (nrow(df) > 0L) {
      code <- .df2code(df, outer)
      if (!is.null(code)) args[sl] <- code
    }
  }

  # cap2act (newTechnology default = 1)
  if (!isTRUE(all.equal(x@cap2act, 1))) {
    args["cap2act"] <- .val2code(x@cap2act)
  }

  # region
  if (length(x@region) > 0L) {
    args["region"] <- .val2code(x@region)
  }

  # timeframe
  if (length(x@timeframe) > 0L && nzchar(x@timeframe)) {
    args["timeframe"] <- .val2code(x@timeframe)
  }

  # fullYear (newTechnology default = TRUE — include only when FALSE)
  if (!isTRUE(x@fullYear)) {
    args["fullYear"] <- "FALSE"
  }

  # optimizeRetirement (newTechnology default = FALSE — include only when TRUE)
  if (isTRUE(x@optimizeRetirement)) {
    args["optimizeRetirement"] <- "TRUE"
  }

  # misc
  if (length(x@misc) > 0L) {
    args["misc"] <- paste(deparse(x@misc, width.cutoff = 60L),
                          collapse = paste0("\n", outer, "  "))
  }

  # format
  arg_lines <- mapply(
    .arg_line, names(args), args,
    MoreArgs = list(indent = outer),
    SIMPLIFY = TRUE, USE.NAMES = FALSE
  )

  code <- paste0(
    var_name, " <- newTechnology(\n",
    paste(arg_lines, collapse = ",\n"),
    "\n)\n"
  )

  .emit_code(code, file)
})
