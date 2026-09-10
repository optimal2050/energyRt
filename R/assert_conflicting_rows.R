# =============================================================================#
# Conflicting rows in an object's data slots
#
# NA in a key column means "for all members of that dimension", so two rows
# with the same key both claim the same parameter cell. Nothing resolves that:
# `.interp_one_series()` left-joins the value table onto the key grid, so a
# repeated key MULTIPLIES rows and the solver silently receives a duplicated
# parameter. Caught here, at construction, instead.
#
# Keys are per PARAMETER, not per slot: within `technology@aeff`, `act2ainp` is
# keyed [acomm, region, year, timeslice] while `cinp2ainp` also keys on `comm`.
# The parameter catalogue is the source of truth; `vintage` and `cluster` are
# added because variants expand by suffixing the process NAME, so they never
# appear in a parameter's dims but are keys in the object's slots.
# Columns that resolve to no parameter are not checked.
# =============================================================================#

.KEY_EXTRA_DIMS <- c("vintage", "cluster")

# Key columns for one value column of one slot, or NULL when the column is not
# a catalogued parameter (declaration columns, annotations, transform args).
.slot_col_keys <- function(cls, slot_name, col, cols) {
  mp <- .param_interp_map()
  e <- mp[[paste(cls, slot_name, col, sep = "\r")]]
  if (is.null(e)) return(NULL)
  intersect(cols, c(e$dims, .KEY_EXTRA_DIMS))
}

.fmt_key <- function(row, keys) {
  paste(vapply(keys, function(k) {
    v <- row[[k]]
    paste0(k, "=", if (length(v) == 0 || is.na(v)) "ALL" else as.character(v))
  }, ""), collapse = ", ")
}

# Refuse two rows that set the same parameter at the same key.
.assert_no_conflicting_rows <- function(df, cls, slot_name, name = NULL) {
  if (!is.data.frame(df) || nrow(df) < 2) return(invisible(TRUE))
  cols <- colnames(df)
  who <- paste0(cls, if (!is.null(name) && nzchar(name)) paste0(" '", name, "'") else "",
                "@", slot_name)
  for (col in cols) {
    keys <- .slot_col_keys(cls, slot_name, col, cols)
    if (is.null(keys)) next
    set <- !is.na(df[[col]])
    if (sum(set) < 2) next
    d <- df[set, , drop = FALSE]
    kk <- if (length(keys)) d[, keys, drop = FALSE] else
      data.frame(.all = rep(1L, nrow(d)))
    dup <- duplicated(kk) | duplicated(kk, fromLast = TRUE)
    if (!any(dup)) next
    bad <- d[dup, , drop = FALSE]
    bk <- if (length(keys)) bad[, keys, drop = FALSE] else
      data.frame(.all = rep(1L, nrow(bad)))
    g <- split(seq_len(nrow(bad)), do.call(paste, c(bk, sep = "|")))[[1]]
    vals <- bad[[col]][g]
    stop(
      who, ": column '", col, "' is set ", length(g),
      " times for the same key (", .fmt_key(bad[g[1], , drop = FALSE], keys),
      ").", "\n",
      "  values: ", paste(format(vals), collapse = ", "), "\n",
      "  NA in a key column means ALL members of that dimension, so these rows",
      " all claim the same cell.", "\n",
      "  Give each row a distinct ",
      if (length(keys)) paste(keys, collapse = "/") else "key",
      ", or keep one row. Left as is, the value is not chosen -- the",
      " interpolation join repeats the row once per duplicate and the solver",
      " receives a multiplied parameter.",
      call. = FALSE)
  }
  invisible(TRUE)
}
