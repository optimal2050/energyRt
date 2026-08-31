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
#' @param complete (technology only) If `TRUE`, parameters with a known unit
#'   formula are reported even when they carry no data yet; commodity-linked
#'   units are then resolved from the declared ports when unambiguous and
#'   left as placeholder tokens otherwise. Default `FALSE` (populated
#'   parameters only).
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
#' @name getUnits
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
  "invcost.payback"   = "years",
  "invcost.eac"       = "{costs}/{capacity}",
  "invcost.retcost"   = "{costs}/{capacity}",
  "fixom.fixom"       = "{costs}/{capacity}",
  "varom.varom"       = "{costs}/{activity}",
  "varom.cvarom"      = "{costs}/{comm}",
  "varom.avarom"      = "{costs}/{acomm}",
  # life & schedule (merged into the `vintage` slot)
  "vintage.olife"     = "years",
  "vintage.start"     = "year",
  "vintage.end"       = "year",
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

# Formulas for the flow classes. These carry a single scalar `@unit` (the
# COMMODITY's unit), so `{comm}` is that scalar and `{costs}` stays a
# placeholder unless the caller supplies a currency -- the same convention
# `getUnits(complete = TRUE)` already renders for technology.
.flow_unit_formulas <- list(
  supply = list(
    "reserve.res.lo"  = "{comm}",
    "reserve.res.up"  = "{comm}",
    "reserve.res.fx"  = "{comm}",
    "supply.ava.lo"   = "{comm}/year",
    "supply.ava.up"   = "{comm}/year",
    "supply.ava.fx"   = "{comm}/year",
    "supply.cost"     = "{costs}/{comm}"
  ),
  import = list(
    "import.imp.lo"   = "{comm}/year",
    "import.imp.up"   = "{comm}/year",
    "import.imp.fx"   = "{comm}/year",
    "import.price"    = "{costs}/{comm}"
  ),
  export = list(
    "export.exp.lo"   = "{comm}/year",
    "export.exp.up"   = "{comm}/year",
    "export.exp.fx"   = "{comm}/year",
    "export.price"    = "{costs}/{comm}"
  ),
  demand = list(
    "demand.demand"   = "{comm}"
  )
)

# Storage: no `@units` slot (units live on the role declarations), and the
# parameters are part-prefixed inp./out./stg.
.storage_unit_formulas <- local({
  parts <- c("inp", "out", "stg")
  caps <- stats::setNames(
    as.list(rep("{capacity}", length(parts) * 10)),
    unlist(lapply(parts, function(p) paste0(
      "capacity.", p, ".",
      c("stock", "cap.lo", "cap.up", "cap.fx", "ncap.lo", "ncap.up",
        "ncap.fx", "ret.lo", "ret.up", "ret.fx")))))
  inv <- stats::setNames(
    as.list(rep("{costs}/{capacity}", length(parts) * 2)),
    unlist(lapply(parts, function(p)
      c(paste0("invcost.", p, ".invcost"), paste0("fixom.", p, ".fixom")))))
  c(caps, inv, list(
    "varom.inpcost"   = "{costs}/{comm}",
    "varom.outcost"   = "{costs}/{comm}",
    "varom.stgcost"   = "{costs}/{comm}",
    "seff.inpeff"     = "fraction",
    "seff.outeff"     = "fraction",
    "seff.stgeff"     = "fraction",
    "duration.duration"    = "hours",
    "duration.duration.lo" = "hours",
    "duration.duration.up" = "hours",
    "duration.duration.fx" = "hours",
    "startLevel.startLevel" = "fraction",
    "vintage.olife"   = "years",
    "vintage.start"   = "year",
    "vintage.end"     = "year"
  ))
})

# The formula registry for a class, or NULL when the class has none.
#' @noRd
.unit_formulas_for <- function(cls) {
  if (identical(cls, "technology")) return(.tech_unit_formulas)
  if (identical(cls, "storage")) return(.storage_unit_formulas)
  .flow_unit_formulas[[cls]]
}

# Base tokens + comm lookup for ANY supported class, so one resolver serves
# technology (rich `@units`), storage (role slots) and the flow classes
# (scalar `@unit`). `units` overrides tokens by name, e.g. c(costs = "MUSD").
#' @noRd
.unit_context <- function(object, units = NULL) {
  cls <- class(object)[1]
  if (identical(cls, "technology")) {
    base <- .resolve_base_units(object)
    comm_units <- .resolve_comm_units(object)
  } else {
    base <- list(capacity = "{capacity}", activity = "{activity}",
                 use = "{use}", costs = "{costs}")
    comm_units <- character(0)
    u <- tryCatch(slot(object, "unit"), error = function(e) NULL)
    if (length(u) == 1 && !is.na(u) && nzchar(u)) {
      # the scalar `@unit` IS the commodity unit of a flow object
      base$comm <- u
    }
    if (identical(cls, "storage")) {
      for (role in c("output", "input", "storage")) {
        df <- tryCatch(slot(object, role), error = function(e) NULL)
        if (is.data.frame(df) && nrow(df) > 0 && "unit" %in% names(df)) {
          v <- df$unit[!is.na(df$unit) & nzchar(df$unit)]
          if (length(v)) { base$comm <- v[1]; break }
        }
      }
    }
  }
  if (is.null(base$comm)) base$comm <- "{comm}"
  if (!is.null(units)) {
    for (nm in intersect(names(units), names(base))) {
      if (!is.na(units[[nm]]) && nzchar(units[[nm]])) base[[nm]] <- units[[nm]]
    }
  }
  list(base = base, comm_units = comm_units)
}

# Resolve one "slot.param" key to a unit string; NA when the class has no
# formula for it. Unresolved tokens are left in place as placeholders.
#' @noRd
.resolve_param_unit <- function(object, key, units = NULL, comm = NA) {
  f <- .unit_formulas_for(class(object)[1])
  if (is.null(f) || is.null(f[[key]])) return(NA_character_)
  ctx <- .unit_context(object, units)
  s <- .substitute_units(f[[key]], ctx$base, ctx$comm_units, comm = comm)
  # flow classes carry the commodity unit as a base token
  s <- gsub("\\{comm\\}", ctx$base$comm, s)
  s
}

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

  all_slots <- c("unit", "emis", "agg", "property")
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

  # Physical properties carry their own, self-contained unit ("GJ/t", "t/m3"),
  # so unlike `emis`/`agg` they are not composed with the commodity's `@unit`.
  if ("property" %in% use_slots) {
    prop <- .comm_property(object)
    for (i in seq_len(nrow(prop))) {
      def <- .property_def(prop$property[i])
      rows[[length(rows) + 1]] <- data.frame(
        slot        = "property",
        parameter   = prop$property[i],
        comm        = NA_character_,
        description = if (is.null(def)) "Physical property" else def$description,
        unit        = prop$unit[i],
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
setMethod("getUnits", "technology", function(object, slots = NULL,
                                             complete = FALSE, ...) {
  base       <- .resolve_base_units(object)
  comm_units <- .resolve_comm_units(object)

  # Unambiguous per-token port units: used to resolve leftover {comm} /
  # {acomm} / {group} tokens when the row itself does not pin a commodity
  # (all-commodities rows, or `complete` placeholders). Ambiguous -> NA,
  # the token stays visible.
  uniq_or_na <- function(x) {
    x <- unique(x[!is.na(x) & nzchar(x)])
    if (length(x) == 1) x else NA_character_
  }
  tok_unit <- list(
    comm  = uniq_or_na(c(
      if ("unit" %in% names(object@input)) object@input$unit,
      if ("unit" %in% names(object@output)) object@output$unit)),
    acomm = uniq_or_na(if ("unit" %in% names(object@aux)) object@aux$unit),
    group = uniq_or_na(if ("unit" %in% names(object@group))
      object@group$unit))
  fill_tokens <- function(u) {
    for (tok in names(tok_unit)) {
      if (!is.na(tok_unit[[tok]])) {
        u <- gsub(paste0("\\{", tok, "\\}"), tok_unit[[tok]], u)
      }
    }
    u
  }

  # slots that have a data.frame with value columns (non-dimension columns)
  df_slots <- c("ceff", "geff", "aeff", "af", "afs",
                "fixom", "varom", "invcost", "vintage",
                "capacity", "weather")
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
  dim_cols <- c("region", "year", "timeslice", "comm", "acomm", "group",
                "type", "comm2", "vintage", "cluster")

  for (sn in intersect(df_slots, use_slots)) {
    df <- slot(object, sn)
    if (!is.data.frame(df)) next
    if (nrow(df) == 0 && !complete) next

    val_cols <- setdiff(names(df), dim_cols)
    if (length(val_cols) == 0) next

    for (vc in val_cols) {
      key <- paste0(sn, ".", vc)
      formula <- .tech_unit_formulas[[key]]
      if (is.null(formula)) next  # unknown column, skip

      has_comm  <- "comm"  %in% names(df)
      has_acomm <- "acomm" %in% names(df)
      has_group <- "group" %in% names(df)

      idx <- if (nrow(df) > 0) which(!is.na(df[[vc]])) else integer()

      if (length(idx) == 0) {
        # entirely-NA / empty parameter: only reported with `complete`,
        # with commodity tokens resolved via the unambiguous port units
        if (!complete) next
        u <- fill_tokens(.substitute_units(formula, base, comm_units))
        rows[[length(rows) + 1]] <- data.frame(
          slot        = sn,
          parameter   = vc,
          comm        = NA_character_,
          description = .get_param_desc("technology", sn, vc),
          unit        = u,
          stringsAsFactors = FALSE
        )
        next
      }

      if (any(has_comm, has_acomm, has_group)) {
        # one row per unique comm/acomm/group combination carrying values,
        # substituting all commodity dimensions of the row jointly (an
        # aeff row resolves BOTH {comm} and {acomm} from its own cells)
        sub_df <- df[idx, , drop = FALSE]
        combo_cols <- c(if (has_comm) "comm", if (has_acomm) "acomm",
                        if (has_group) "group")
        combos <- unique(sub_df[, combo_cols, drop = FALSE])
        for (k in seq_len(nrow(combos))) {
          cmb <- combos[k, , drop = FALSE]
          u <- .substitute_units(
            formula, base, comm_units,
            comm  = if (has_comm)  as.character(cmb$comm)  else NA,
            acomm = if (has_acomm) as.character(cmb$acomm) else NA,
            group = if (has_group) as.character(cmb$group) else NA
          )
          u <- fill_tokens(u)
          lab <- unlist(cmb, use.names = FALSE)
          lab <- paste(lab[!is.na(lab)], collapse = "/")
          rows[[length(rows) + 1]] <- data.frame(
            slot        = sn,
            parameter   = vc,
            comm        = if (nzchar(lab)) lab else NA_character_,
            description = .get_param_desc("technology", sn, vc),
            unit        = u,
            stringsAsFactors = FALSE
          )
        }
      } else {
        u <- fill_tokens(.substitute_units(formula, base, comm_units))
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
# `print` is an S4 generic here (R/print.R), so a plain @export registers the
# function but NOT the method, and dispatch falls through to the data.frame
# default -- printing the wide `description` column this method exists to drop.
# See the same trap for print.commodity in AGENTS.md.
#' @exportS3Method print energyRtUnits
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

# get_units() is deprecated -> R/legacy_api_shims.R

# ── Facet labels for plot_process_year() ───────────────────────────────────────

# A ggplot2 labeller turning facet keys ("ava", "cost", "invcost") into
# "ava [GWh/year]". `raw` supplies the parameter names behind each facet
# base; members of a bound family (ava.lo/up/fx) share a unit, so the first
# resolvable one speaks for the facet. Facets with no formula, or whose unit
# is a bare placeholder, are left unlabelled rather than showing "{costs}".
#' @noRd
.unit_labeller <- function(object, raw, units = NULL) {
  slot_of <- .process_year_slot[[class(object)[1]]]
  unit_for <- function(base) {
    params <- unique(raw$param[raw$base == base])
    for (sl in slot_of) {
      for (pm in params) {
        u <- .resolve_param_unit(object, paste0(sl, ".", pm), units = units)
        if (!is.na(u) && nzchar(u) && !grepl("^[{][^}]*[}]$", u)) return(u)
      }
    }
    NA_character_
  }
  function(labels) {
    key <- labels[[1]]
    lab <- vapply(as.character(key), function(b) {
      u <- tryCatch(unit_for(b), error = function(e) NA_character_)
      if (is.na(u)) b else paste0(b, " [", u, "]")
    }, character(1), USE.NAMES = FALSE)
    labels[[1]] <- lab
    labels
  }
}

# ── getUnits() for the flow and storage classes ────────────────────────────────
# One method body serves supply/demand/import/export/storage: walk the class's
# formula registry, keep the parameters the object actually populates, and
# resolve each through the shared engine.
#' @noRd
.get_units_process <- function(object, slots = NULL, complete = FALSE,
                               units = NULL, ...) {
  cls <- class(object)[1]
  f <- .unit_formulas_for(cls)
  if (is.null(f)) {
    stop("getUnits(): no unit formulas for class '", cls, "'.", call. = FALSE)
  }
  keys <- names(f)
  sl <- sub("[.].*$", "", keys)
  pm <- sub("^[^.]*[.]", "", keys)
  keep <- rep(TRUE, length(keys))
  if (!complete) {
    keep <- mapply(function(s, p) {
      d <- tryCatch(methods::slot(object, s), error = function(e) NULL)
      is.data.frame(d) && nrow(d) > 0 && p %in% names(d) &&
        any(!is.na(d[[p]]))
    }, sl, pm)
  }
  if (!is.null(slots)) keep <- keep & sl %in% slots
  if (!any(keep)) {
    out <- data.frame(slot = character(), parameter = character(),
                      comm = character(), description = character(),
                      unit = character(), stringsAsFactors = FALSE)
  } else {
    out <- data.frame(
      slot = sl[keep], parameter = pm[keep],
      comm = NA_character_,
      description = vapply(which(keep), function(i)
        .get_param_desc(cls, sl[i], pm[i]), character(1)),
      unit = vapply(which(keep), function(i)
        .resolve_param_unit(object, keys[i], units = units), character(1)),
      stringsAsFactors = FALSE
    )
  }
  attr(out, "object_name") <- tryCatch(object@name, error = function(e) NA)
  attr(out, "object_class") <- cls
  class(out) <- c("energyRtUnits", "data.frame")
  out
}

#' @rdname getUnits
#' @export
setMethod("getUnits", "supply", .get_units_process)

#' @rdname getUnits
#' @export
setMethod("getUnits", "demand", .get_units_process)

#' @rdname getUnits
#' @export
setMethod("getUnits", "import", .get_units_process)

#' @rdname getUnits
#' @export
setMethod("getUnits", "export", .get_units_process)

#' @rdname getUnits
#' @export
setMethod("getUnits", "storage", .get_units_process)
