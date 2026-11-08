## Auto-generates `.col_param`: the column <-> parameter relation, as data.
## Runnable on its own; also called from data-raw/DATASET.R.
##
## One row per slot column that holds a value, keyed by the class a LOOKUP uses
## (the catalogue's own `class` for a bound column, the declaring class for an
## unbound one). `.param_interp_map()` is this table, indexed; nothing scans
## the catalogue at run time any more.
##
## Total in both directions, and it errors rather than drop a row:
##   * a declared data column with no parameter must be listed in the
##     `columns:` section of parameters.yml, with a reason;
##   * a catalogue binding must name a column that exists, or be one of the
##     stated exceptions below.
y <- yaml::read_yaml("data-raw/classes-docs.yml")
p <- yaml::read_yaml("data-raw/parameters.yml")
orphan <- p$columns
p$columns <- NULL

# `settings` inherits config's slots (class-settings.R: `contains = "config"`),
# so a column documented on `config` is reachable on a `settings` object, which
# is the class the catalogue binds and the class the lookup uses.
.class_parents <- list(settings = "config")

# Slots whose columns classes-docs.yml does not itemise: the rule is declared, the
# column is not documented. Shrink this list by writing the `columns:` block.
# (`calendar@timeslice_share` and `config@debug` carry the columns in their S4
# prototype only.)
.undocumented <- c("calendar/timeslice_share/share",
                   "calendar/timeslice_share/weight",
                   "settings/debug/dummyImport",
                   "settings/debug/dummyExport")

# Numeric / integer columns that INDEX rather than measure. `year` is the only
# one -- every other set dimension is character in the schema -- and the
# assertion below holds that against `.dimSets` whenever it is in scope.
.index_cols <- "year"

key <- function(...) paste(..., sep = "/")

# ---- the declared data columns -------------------------------------------- #
decl <- list()
for (cn in names(y$class)) for (sn in names(y$class[[cn]])) {
  s <- y$class[[cn]][[sn]]
  if (!is.list(s) || is.null(s$columns)) next
  for (co in names(s$columns)) {
    tp <- s$columns[[co]]$type
    if (!identical(tp, "numeric") && !identical(tp, "integer")) next
    if (co %in% .index_cols) next
    decl[[key(cn, sn, co)]] <- list(class = cn, slot = sn, column = co,
                                    type = tp)
  }
}

if (exists(".dimSets", inherits = TRUE)) {
  idx <- intersect(vapply(decl, function(d) d$column, ""), .dimSets)
  if (length(idx) > 0) {
    stop("value column(s) named after a set dimension: ",
         paste(unique(idx), collapse = ", "))
  }
}

# ---- the catalogue bindings, expanded over their columns ------------------- #
rows <- list()
bound <- character(0)
for (nm in names(p)) {
  e <- p[[nm]]
  if (is.null(e$class) || is.null(e$slot) || is.null(e$colName)) next
  # `colAlias` is the slot's own column name where it differs from the
  # catalogue key (pDemand: colName `dem`, column `demand`).
  co <- e$colAlias %||% e$colName
  dims <- as.character(unlist(e$dimSets))
  dv <- suppressWarnings(as.numeric(unlist(e$defVal)))
  rule <- e$interpolation
  cols <- if (identical(e$type, "bounds") || length(dv) >= 2) {
    r1 <- if (length(rule) >= 1) rule[[1]] else "back.inter.forth"
    r2 <- if (length(rule) >= 2) rule[[2]] else r1
    list(c(co = paste0(co, ".lo"), bound = "lo", rule = r1, dv = dv[1]),
         c(co = paste0(co, ".up"), bound = "up", rule = r2,
           dv = if (length(dv) >= 2) dv[2] else NA_real_),
         c(co = paste0(co, ".fx"), bound = "fx", rule = r1, dv = NA_real_))
  } else {
    list(c(co = co, bound = NA_character_,
           rule = if (is.list(rule)) rule[[1]] else rule,
           dv = if (length(dv) >= 1) dv[1] else NA_real_))
  }
  for (x in cols) {
    # which class declares this column: the catalogue's own, or a parent
    k <- key(e$class, e$slot, x[["co"]])
    on <- if (!is.null(decl[[k]])) e$class else {
      pk <- .class_parents[[e$class]]
      if (!is.null(pk) && !is.null(decl[[key(pk, e$slot, x[["co"]])]])) {
        pk
      } else if (k %in% .undocumented) {
        NA_character_
      } else {
        stop("parameter '", nm, "' binds a column no class declares: ", k)
      }
    }
    bound <- c(bound, if (is.na(on)) k else key(on, e$slot, x[["co"]]))
    rows[[length(rows) + 1L]] <- data.frame(
      class = e$class, slot = e$slot, column = x[["co"]],
      parameter = nm, bound = x[["bound"]],
      rule = x[["rule"]], defVal = as.numeric(x[["dv"]]),
      declared_on = on, why = NA_character_,
      dims = I(list(dims)), stringsAsFactors = FALSE)
  }
}

# ---- the declared columns no parameter owns -------------------------------- #
unbound <- setdiff(names(decl), bound)
missing <- setdiff(unbound, names(orphan))
if (length(missing) > 0) {
  stop("data columns with no parameter and no `columns:` entry in ",
       "parameters.yml:\n  ", paste(missing, collapse = "\n  "))
}
stale <- setdiff(names(orphan), unbound)
if (length(stale) > 0) {
  stop("`columns:` entries that a parameter does own:\n  ",
       paste(stale, collapse = "\n  "))
}
for (k in unbound) {
  d <- decl[[k]]
  rows[[length(rows) + 1L]] <- data.frame(
    class = d$class, slot = d$slot, column = d$column,
    parameter = NA_character_, bound = NA_character_,
    rule = NA_character_, defVal = NA_real_,
    declared_on = d$class, why = orphan[[k]]$why,
    dims = I(list(character(0))), stringsAsFactors = FALSE)
}

.col_param <- do.call(rbind, rows)
.col_param <- .col_param[order(.col_param$class, .col_param$slot,
                               .col_param$column), ]
rownames(.col_param) <- NULL
stopifnot(!anyDuplicated(
  paste(.col_param$class, .col_param$slot, .col_param$column, sep = "/")))
# an unbound column without a reason is an undeclared one
stopifnot(all(nzchar(.col_param$why[is.na(.col_param$parameter)])))

cat("built .col_param:", nrow(.col_param), "columns,",
    sum(!is.na(.col_param$parameter)), "bound to",
    length(unique(na.omit(.col_param$parameter))), "parameters,",
    sum(is.na(.col_param$parameter)), "unbound,",
    sum(is.na(.col_param$declared_on)), "undocumented\n")
