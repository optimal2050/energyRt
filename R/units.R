# getUnits / get_units ############################################################
#' Get parameter units for energyRt class objects
#'
#' @description
#' Returns a data.frame describing each parameter in the object along with its
#' inferred unit. Units are derived from the unit slots stored on the object
#' (`@unit` for commodity, `@units` for technology) and the dimensional
#' semantics of each parameter (e.g. investment cost is `{costs}/{capacity}`).
#'
#' Unresolved base units are shown as placeholder tokens (`{capacity}`,
#' `{activity}`, `{costs}`) when the corresponding `@units` slot is not set.
#'
#' @param object A `commodity` or `technology` S4 object.
#' @param slots  Character vector of slot names to include. `NULL` (default)
#'   returns all populated slots.
#' @param ...    Reserved for future use.
#'
#' @return A data.frame with columns `slot`, `parameter`, `comm`, `description`,
#'   and `unit`.
#'
#' @examples
#' \dontrun{
#' coal <- newCommodity("COAL", unit = "PJ")
#' getUnits(coal)
#'
#' getUnits(PTL_RWGS_FT_SYN2030)
#' getUnits(PTL_RWGS_FT_SYN2030, slots = "ceff")
#' }
#'
#' @rdname getUnits
#' @include generics.R
#' @export
NULL

# ── Static unit formula lookup ─────────────────────────────────────────────────
# Keys are "slot.parameter"; value is a glue-style formula string.
# Tokens: {capacity}, {activity}, {use}, {costs}, {comm}, {acomm}, {group}
.tech_unit_formulas <- list(
  # scalars
  "cap2act"           = "{activity}/{capacity}",
  # costs
  "invcost.invcost"   = "{costs}/{capacity}",
  "invcost.wacc"      = "fraction",
  "invcost.eac"       = "{costs}/{capacity}",
  "invcost.retcost"   = "{costs}/{capacity}",
  "fixom.fixom"       = "{costs}/{capacity}",
  "varom.varom"       = "{costs}/{activity}",
  "varom.cvarom"      = "{costs}/{comm}",
  "varom.avarom"      = "{costs}/{acomm}",
  # life & schedule
  "olife.olife"       = "years",
  "start.start"       = "year",
  "end.end"           = "year",
  # capacity bounds
  "capacity.cap.lo"   = "{capacity}",
  "capacity.cap.up"   = "{capacity}",
  "capacity.cap.fx"   = "{capacity}",
  "capacity.ncap.lo"  = "{capacity}",
  "capacity.ncap.up"  = "{capacity}",
  "capacity.ncap.fx"  = "{capacity}",
  "capacity.ret.lo"   = "{capacity}",
  "capacity.ret.up"   = "{capacity}",
  "capacity.ret.fx"   = "{capacity}",
  "capacity.stock"    = "{capacity}",
  # availability
  "af.af.lo"          = "fraction",
  "af.af.up"          = "fraction",
  "af.af.fx"          = "fraction",
  "afs.afs.lo"        = "fraction",
  "afs.afs.up"        = "fraction",
  "afs.afs.fx"        = "fraction",
  # ceff — input side
  "ceff.cinp2use"     = "{comm}/{use}",
  "ceff.cinp2ginp"    = "{comm}/{group}",
  "ceff.use2cact"     = "{use}/{activity}",
  # ceff — output side
  "ceff.cact2cout"    = "{activity}/{comm}",
  # ceff — bounds (physical value of the commodity flow)
  "ceff.afc.lo"       = "{comm}",
  "ceff.afc.up"       = "{comm}",
  "ceff.afc.fx"       = "{comm}",
  # ceff — shares (dimensionless)
  "ceff.share.lo"     = "fraction",
  "ceff.share.up"     = "fraction",
  "ceff.share.fx"     = "fraction",
  # geff
  "geff.ginp2use"     = "{group}/{use}",
  # aeff — commodity-to-auxiliary
  "aeff.cinp2ainp"    = "{comm}/{acomm}",
  "aeff.cinp2aout"    = "{comm}/{acomm}",
  "aeff.cout2ainp"    = "{comm}/{acomm}",
  "aeff.cout2aout"    = "{comm}/{acomm}",
  # aeff — activity/capacity-to-auxiliary
  "aeff.act2ainp"     = "{activity}/{acomm}",
  "aeff.act2aout"     = "{activity}/{acomm}",
  "aeff.cap2ainp"     = "{capacity}/{acomm}",
  "aeff.cap2aout"     = "{capacity}/{acomm}",
  "aeff.ncap2ainp"    = "{capacity}/{acomm}",
  "aeff.ncap2aout"    = "{capacity}/{acomm}",
  # weather
  "weather.waf.lo"    = "factor",
  "weather.waf.up"    = "factor",
  "weather.waf.fx"    = "factor"
)

# ── Helpers ─────────────────────────────────────────────────────────────────────

# Extract base units from technology@units; return placeholders for missing ones.
.resolve_base_units <- function(object) {
  u <- list(
    capacity = "{capacity}",
    activity = "{activity}",
    use      = "{use}",
    costs    = "{costs}"
  )
  if (nrow(object@units) > 0) {
    for (nm in intersect(names(u), names(object@units))) {
      v <- object@units[[nm]][1]
      if (!is.na(v) && nzchar(v)) u[[nm]] <- v
    }
  }
  # use falls back to activity if unset
  if (identical(u$use, "{use}")) u$use <- u$activity
  u
}

# Build comm → unit lookup from input/output/aux/group slots.
.resolve_comm_units <- function(object) {
  out <- character(0)
  for (slot_nm in c("input", "output")) {
    df <- slot(object, slot_nm)
    if (nrow(df) > 0 && "comm" %in% names(df) && "unit" %in% names(df)) {
      vals <- setNames(df$unit, df$comm)
      vals <- vals[!is.na(names(vals))]
      out <- c(out, vals)
    }
  }
  if (nrow(object@aux) > 0 && "acomm" %in% names(object@aux) &&
      "unit" %in% names(object@aux)) {
    vals <- setNames(object@aux$unit, object@aux$acomm)
    vals <- vals[!is.na(names(vals))]
    out <- c(out, vals)
  }
  if (nrow(object@group) > 0 && "group" %in% names(object@group) &&
      "unit" %in% names(object@group)) {
    vals <- setNames(object@group$unit, object@group$group)
    vals <- vals[!is.na(names(vals))]
    out <- c(out, vals)
  }
  out
}

# Substitute {token} placeholders with resolved values.
.substitute_units <- function(formula, base, comm_units,
                               comm = NA, acomm = NA, group = NA) {
  s <- formula
  # Use regex mode (no fixed = TRUE) so \\{ / \\} correctly match literal braces
  s <- gsub("\\{capacity\\}", base$capacity, s)
  s <- gsub("\\{activity\\}", base$activity, s)
  s <- gsub("\\{use\\}",      base$use,      s)
  s <- gsub("\\{costs\\}",    base$costs,    s)
  if (!is.na(comm) && grepl("\\{comm\\}", s)) {
    cu <- if (comm %in% names(comm_units)) comm_units[[comm]]
          else paste0("{", comm, "}")
    s <- gsub("\\{comm\\}", cu, s)
  }
  if (!is.na(acomm) && grepl("\\{acomm\\}", s)) {
    cu <- if (acomm %in% names(comm_units)) comm_units[[acomm]]
          else paste0("{", acomm, "}")
    s <- gsub("\\{acomm\\}", cu, s)
  }
  if (!is.na(group) && grepl("\\{group\\}", s)) {
    cu <- if (group %in% names(comm_units)) comm_units[[group]]
          else paste0("{", group, "}")
    s <- gsub("\\{group\\}", cu, s)
  }
  s
}

# Pull a description from the .classes internal object; NA if not found.
.get_param_desc <- function(cls, slot_nm, col_nm = NA) {
  if (!exists(".classes", envir = asNamespace("energyRt"))) return(NA_character_)
  cl <- get(".classes", envir = asNamespace("energyRt"))
  rows <- cl[cl$class == cls & cl$slotname == slot_nm, ]
  if (nrow(rows) == 0) return(NA_character_)
  if (is.na(col_nm) || !("col.name" %in% names(cl))) {
    return(rows$description[1])
  }
  col_rows <- rows[!is.na(rows$col.name) & rows$col.name == col_nm, ]
  if (nrow(col_rows) > 0) col_rows$col.description[1]
  else rows$description[1]
}

# ── commodity method ─────────────────────────────────────────────────────────
#' @rdname getUnits
#' @export
setMethod("getUnits", "commodity", function(object, slots = NULL, ...) {
  rows <- list()

  all_slots <- c("unit", "emis", "agg")
  use_slots <- if (is.null(slots)) all_slots else intersect(slots, all_slots)

  if ("unit" %in% use_slots) {
    rows[[length(rows) + 1]] <- data.frame(
      slot        = "unit",
      parameter   = NA_character_,
      comm        = NA_character_,
      description = "Primary unit of the commodity",
      unit        = if (nzchar(object@unit)) object@unit else NA_character_,
      stringsAsFactors = FALSE
    )
  }

  if ("emis" %in% use_slots && nrow(object@emis) > 0) {
    for (i in seq_len(nrow(object@emis))) {
      eu <- object@emis$unit[i]
      cu <- if (nzchar(object@unit)) object@unit else "{unit}"
      rows[[length(rows) + 1]] <- data.frame(
        slot        = "emis",
        parameter   = "emis",
        comm        = object@emis$comm[i],
        description = "Emission factor",
        unit        = paste0(eu, "/", cu),
        stringsAsFactors = FALSE
      )
    }
  }

  if ("agg" %in% use_slots && nrow(object@agg) > 0) {
    for (i in seq_len(nrow(object@agg))) {
      au <- object@agg$unit[i]
      cu <- if (nzchar(object@unit)) object@unit else "{unit}"
      rows[[length(rows) + 1]] <- data.frame(
        slot        = "agg",
        parameter   = "agg",
        comm        = object@agg$comm[i],
        description = "Aggregation weight",
        unit        = paste0(au, "/", cu),
        stringsAsFactors = FALSE
      )
    }
  }

  result <- do.call(rbind, rows)
  if (is.null(result)) result <- .empty_units_df()
  class(result) <- c("energyRtUnits", "data.frame")
  attr(result, "object_name") <- object@name
  attr(result, "object_class") <- "commodity"
  result
})

# ── technology method ─────────────────────────────────────────────────────────
#' @rdname getUnits
#' @export
setMethod("getUnits", "technology", function(object, slots = NULL, ...) {
  base       <- .resolve_base_units(object)
  comm_units <- .resolve_comm_units(object)

  # slots that have a data.frame with value columns (non-dimension columns)
  df_slots <- c("ceff", "geff", "aeff", "af", "afs",
                "fixom", "varom", "invcost", "olife",
                "start", "end", "capacity", "weather")
  scalar_slots <- "cap2act"
  all_slots <- c(scalar_slots, df_slots)
  use_slots <- if (is.null(slots)) all_slots else intersect(slots, all_slots)

  rows <- list()

  # ── scalar: cap2act ──────────────────────────────────────────────────────────
  if ("cap2act" %in% use_slots && length(object@cap2act) > 0) {
    formula <- .tech_unit_formulas[["cap2act"]]
    rows[[length(rows) + 1]] <- data.frame(
      slot        = "cap2act",
      parameter   = NA_character_,
      comm        = NA_character_,
      description = .get_param_desc("technology", "cap2act"),
      unit        = .substitute_units(formula, base, comm_units),
      stringsAsFactors = FALSE
    )
  }

  # ── data.frame slots ─────────────────────────────────────────────────────────
  # Dimension columns to skip (not parameters, just indexing)
  dim_cols <- c("region", "year", "slice", "comm", "acomm", "group",
                "type", "comm2")

  for (sn in intersect(df_slots, use_slots)) {
    df <- slot(object, sn)
    if (!is.data.frame(df) || nrow(df) == 0) next

    val_cols <- setdiff(names(df), dim_cols)
    if (length(val_cols) == 0) next

    for (vc in val_cols) {
      key <- paste0(sn, ".", vc)
      formula <- .tech_unit_formulas[[key]]
      if (is.null(formula)) next  # unknown column, skip

      # comm-specific: expand one row per unique comm/acomm/group present
      has_comm  <- "comm"  %in% names(df)
      has_acomm <- "acomm" %in% names(df)
      has_group <- "group" %in% names(df)

      if (any(has_comm, has_acomm, has_group)) {
        # build unique combinations that have non-NA values
        val_col_data <- df[[vc]]
        idx <- which(!is.na(val_col_data))
        if (length(idx) == 0) next  # skip columns that are entirely NA

        sub_df <- df[idx, , drop = FALSE]
        comm_vals  <- if (has_comm)  unique(sub_df$comm)  else NA_character_
        acomm_vals <- if (has_acomm) unique(sub_df$acomm) else NA_character_
        group_vals <- if (has_group) unique(sub_df$group) else NA_character_

        # cross: for ceff we want comm × vc; for aeff we want acomm × vc
        iter_vals <- if (has_acomm) acomm_vals
                     else if (has_group) group_vals
                     else comm_vals
        iter_type <- if (has_acomm) "acomm"
                     else if (has_group) "group"
                     else "comm"

        for (iv in iter_vals) {
          u <- .substitute_units(
            formula, base, comm_units,
            comm  = if (iter_type == "comm")  iv else NA,
            acomm = if (iter_type == "acomm") iv else NA,
            group = if (iter_type == "group") iv else NA
          )
          rows[[length(rows) + 1]] <- data.frame(
            slot        = sn,
            parameter   = vc,
            comm        = iv,
            description = .get_param_desc("technology", sn, vc),
            unit        = u,
            stringsAsFactors = FALSE
          )
        }
      } else {
        if (all(is.na(df[[vc]]))) next  # skip entirely-NA scalar columns
        u <- .substitute_units(formula, base, comm_units)
        rows[[length(rows) + 1]] <- data.frame(
          slot        = sn,
          parameter   = vc,
          comm        = NA_character_,
          description = .get_param_desc("technology", sn, vc),
          unit        = u,
          stringsAsFactors = FALSE
        )
      }
    }
  }

  result <- do.call(rbind, rows)
  if (is.null(result)) result <- .empty_units_df()
  rownames(result) <- NULL
  class(result) <- c("energyRtUnits", "data.frame")
  attr(result, "object_name")  <- object@name
  attr(result, "object_class") <- "technology"
  result
})

# ── Empty result helper ─────────────────────────────────────────────────────
.empty_units_df <- function() {
  data.frame(
    slot        = character(),
    parameter   = character(),
    comm        = character(),
    description = character(),
    unit        = character(),
    stringsAsFactors = FALSE
  )
}

# ── print method ─────────────────────────────────────────────────────────────
#' @export
print.energyRtUnits <- function(x, ...) {
  nm  <- attr(x, "object_name")
  cls <- attr(x, "object_class")
  cat(sprintf("Units for %s '%s':\n\n", cls, nm))
  # Drop description for compact display; keep slot/parameter/comm/unit
  disp <- x[, c("slot", "parameter", "comm", "unit"), drop = FALSE]
  disp[is.na(disp)] <- "—"
  print(format(disp, justify = "left"), row.names = FALSE, quote = FALSE)
  invisible(x)
}

# ── snake_case alias ──────────────────────────────────────────────────────────
#' @rdname getUnits
#' @export
get_units <- function(object, ...) getUnits(object, ...)
