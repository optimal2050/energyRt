# getUnits / get_units ############################################################
#' Get parameter units for energyRt class objects
#'
#' @description
#' Returns a data.frame describing each parameter in the object along with its
#' inferred unit. Units are derived from the unit slots stored on the object
#' (`@unit` for commodity, `@units` for technology) and the dimensional
#' semantics of each parameter (e.g. investment cost is `{currency}/{capacity}`).
#'
#' Unresolved base units are shown as placeholder tokens (`{capacity}`,
#' `{activity}`, `{currency}`) when the corresponding `@units` slot is not set.
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
#' @param units named list of base units (`capacity`, `activity`, `use`,
#'   `currency`) for the classes that carry no `@units` slot. `NULL` (default)
#'   resolves what it can from the object and leaves the rest as placeholder
#'   tokens.
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

# -- The unit of a parameter, from the catalogue ----------------------------- #
# A unit is declared once, as `unit:` on the parameter in
# data-raw/parameters.yml, and baked into `.col_param` by data-raw/col_param.R.
# Before 0.92 the formulas were three hand-written lists keyed "slot.column"
# here, which drifted: 22 keys addressed columns that no longer existed (the
# storage `aeff.inp.*` family was filed under technology, `ceff.share.*` had
# been renamed `grp.share.*`), and `trade` had no table at all.
#
# Tokens: {capacity}, {activity}, {use}, {currency}, {comm}, {acomm}, {group},
# resolved against the object by `.unit_context()`.
#
# The shape this returns is unchanged, so the three call sites are too: a named
# list "slot.column" -> formula, plus "slot" alone for a scalar slot such as
# `technology@cap2act`. NULL when the class declares no unit anywhere.
.unit_formulas_cache <- new.env(parent = emptyenv())

#' @noRd
# `config` has no catalogue rows of its own: `settings` CONTAINS `config`, and
# `data-raw/col_param.R` files the slots they share under the child's name
# (`.class_parents`). A unit lookup for `config` therefore reads `settings`.
#' @noRd
.unit_formula_alias <- c(config = "settings")

.unit_formulas_for <- function(cls) {
  if (length(cls) != 1L || is.na(cls)) return(NULL)
  hit <- .unit_formulas_cache[[cls]]
  if (!is.null(hit)) return(hit)
  cp <- tryCatch(.col_param, error = function(e) NULL)
  if (!is.data.frame(cp)) return(NULL)
  look <- if (cls %in% names(.unit_formula_alias))
    .unit_formula_alias[[cls]] else cls
  j <- which(cp$class == look & !is.na(cp$unit))
  if (!length(j)) return(NULL)
  keys <- ifelse(is.na(cp$column[j]), cp$slot[j],
                 paste(cp$slot[j], cp$column[j], sep = "."))
  out <- stats::setNames(as.list(cp$unit[j]), keys)
  .unit_formulas_cache[[cls]] <- out
  out
}

#' Unit of an object's own measure
#'
#' The one quantity an object measures itself in, as opposed to the table of
#' parameter units [getUnits()] reports: the commodity a flow is counted in
#' for `commodity`, `demand`, `import`, `export`, `supply` and `trade`, and
#' the value of the series for `weather`.
#'
#' It is stored in `@units` -- the `comm` column, or `wval` for `weather` --
#' and set with the `unit =` argument of the constructors. Up to version 0.91
#' it was a slot of its own, `@unit`; that slot is gone, and this accessor
#' replaces every read of it.
#'
#' @param object an object with a `@units` slot.
#' @param ... not used.
#'
#' @return A single character value, or `NA_character_` when the object
#'   declares no unit.
#'
#' @family units
#' @export
#' @examples
#' getUnit(newCommodity(name = "ELC", desc = "Electricity", unit = "GWh"))
#' getUnit(newCommodity(name = "ELC"))
setGeneric("getUnit", function(object, ...) standardGeneric("getUnit"))

#' @noRd
.get_unit_own <- function(object, ...) {
  own <- .unit_arg_column(class(object)[1])
  # an object serialized before the slot existed has no attribute for
  # it, and only `.upgrade_missing_slots()` fills one; a stored fixture
  # reaches a plot without passing through that
  u <- tryCatch(methods::slot(object, "units"),
                error = function(e) NULL)
  v <- if (is.data.frame(u) && nrow(u) > 0 && own %in% names(u))
    u[[own]][1] else NA_character_
  if (length(v) != 1 || is.na(v) || !nzchar(as.character(v))) {
    # an object serialized before the slot was removed still carries it as a
    # plain attribute; see `.upgrade_unit_slot()`
    v <- attr(object, "unit", exact = TRUE)
    if (length(v) != 1 || is.na(v) || !nzchar(as.character(v)))
      return(NA_character_)
  }
  as.character(v)
}

#' @rdname getUnit
#' @export
setMethod("getUnit", "commodity", .get_unit_own)

#' @rdname getUnit
#' @export
setMethod("getUnit", "demand", .get_unit_own)

#' @rdname getUnit
#' @export
setMethod("getUnit", "import", .get_unit_own)

#' @rdname getUnit
#' @export
setMethod("getUnit", "export", .get_unit_own)

#' @rdname getUnit
#' @export
setMethod("getUnit", "supply", .get_unit_own)

#' @rdname getUnit
#' @export
setMethod("getUnit", "weather", .get_unit_own)

#' @rdname getUnit
#' @export
setMethod("getUnit", "technology", .get_unit_own)

#' @rdname getUnit
#' @export
setMethod("getUnit", "storage", .get_unit_own)

#' @rdname getUnit
#' @export
setMethod("getUnit", "trade", .get_unit_own)

#' @rdname getUnit
#' @export
setMethod("getUnit", "tax", .get_unit_own)

#' @rdname getUnit
#' @export
setMethod("getUnit", "subsidy", .get_unit_own)

# Base tokens + comm lookup for ANY supported class, so one resolver serves
# technology (rich `@units`), storage (role slots) and the flow classes
# (`@units$comm`). `units` overrides tokens by name, e.g. c(currency = "MUSD").
#' @noRd
.unit_context <- function(object, units = NULL) {
  cls <- class(object)[1]
  if (identical(cls, "technology")) {
    base <- .resolve_base_units(object)
    comm_units <- .resolve_comm_units(object)
  } else {
    # `supply`, `storage` and `trade` carry a `@units` table of their own, so
    # the same resolver fills their tokens; the columns they lack stay
    # placeholders. Guarded because objects serialized before those slots
    # existed have no `units` attribute at all.
    base <- if ("units" %in% names(attributes(object)))
      .resolve_base_units(object) else
      list(capacity = "{capacity}", activity = "{activity}",
           use = "{use}", currency = "{currency}")
    comm_units <- character(0)
    # a flow class's own measure comes from `@units$comm`, which
    # `.resolve_base_units()` has already read
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
    # `costs` is the pre-0.92 spelling of `currency`; accepted here too, so a
    # caller passing `units = c(costs = "MUSD")` still resolves the token
    nms <- names(units)
    if ("costs" %in% nms && !("currency" %in% nms)) {
      nms[nms == "costs"] <- "currency"
      names(units) <- nms
    }
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

# Extract base units from an object's `@units`; placeholders for the rest.
.resolve_base_units <- function(object) {
  u <- list(
    capacity = "{capacity}",
    activity = "{activity}",
    use      = "{use}",
    currency = "{currency}"
  )
  if (nrow(object@units) > 0) {
    for (nm in intersect(names(u), names(object@units))) {
      v <- object@units[[nm]][1]
      if (!is.na(v) && nzchar(v)) u[[nm]] <- v
    }
    # `costs` is the pre-0.92 spelling of `currency`: a saved object carries it
    # inside a data.frame, which `.upgrade_missing_slots()` cannot rename, so
    # it is accepted on READ. Not a second place to declare one.
    if (identical(u$currency, "{currency}") &&
          "costs" %in% names(object@units)) {
      v <- object@units[["costs"]][1]
      if (!is.na(v) && nzchar(v)) u$currency <- v
    }
  }
  # use falls back to activity if unset
  if (identical(u$use, "{use}")) u$use <- u$activity

  # The object's OWN measure, which propagates nowhere: `comm` for a commodity
  # or a flow class, `wval` for a weather series, which measures no commodity.
  # Set only when declared, so `.unit_context()` can still default `{comm}`.
  own <- .unit_arg_column(class(object)[1])
  if (nrow(object@units) > 0 && own %in% names(object@units)) {
    v <- object@units[[own]][1]
    if (!is.na(v) && nzchar(v)) u[[own]] <- v
  }
  if (is.null(u[[own]])) {
    # the scalar `@unit` slot of 0.91 and earlier: removed from the class, but
    # still an attribute on an object serialized with it, and so still
    # readable. `.upgrade_unit_slot()` migrates it; this covers one that has
    # not passed through the upgrade yet.
    v <- attr(object, "unit", exact = TRUE)
    if (length(v) == 1 && !is.na(v) && nzchar(as.character(v)))
      u[[own]] <- as.character(v)
  }
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
  s <- gsub("\\{currency\\}", base$currency, s)
  # `{wval}` is a weather series own measure, dimensionless unless
  # the object declares one; the placeholder stays when it does not
  if (!is.null(base$wval)) s <- gsub("\\{wval\\}", base$wval, s)
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

  all_slots <- c("units", "emis", "agg", "property")
  # `unit` was the slot's name up to 0.91 and stays an accepted filter spelling
  if (!is.null(slots)) slots[slots == "unit"] <- "units"
  use_slots <- if (is.null(slots)) all_slots else intersect(slots, all_slots)
  own <- getUnit(object)

  if ("units" %in% use_slots) {
    rows[[length(rows) + 1]] <- data.frame(
      slot        = "units",
      parameter   = NA_character_,
      comm        = NA_character_,
      description = "Primary unit of the commodity",
      unit        = own,
      stringsAsFactors = FALSE
    )
  }

  # `@emis$unit` and `@agg$unit` are RATES, declared whole: the emission
  # factor's numerator is the EMITTED commodity's unit and its denominator this
  # one's, while an aggregation coefficient's numerator is this one's and its
  # denominator the member's. Neither is composed here -- doing so
  # double-divided the first and inverted the second.
  if ("emis" %in% use_slots && nrow(object@emis) > 0) {
    for (i in seq_len(nrow(object@emis))) {
      rows[[length(rows) + 1]] <- data.frame(
        slot        = "emis",
        parameter   = "emis",
        comm        = object@emis$comm[i],
        description = "Emission factor",
        unit        = object@emis$unit[i],
        stringsAsFactors = FALSE
      )
    }
  }

  if ("agg" %in% use_slots && nrow(object@agg) > 0) {
    for (i in seq_len(nrow(object@agg))) {
      rows[[length(rows) + 1]] <- data.frame(
        slot        = "agg",
        parameter   = "agg",
        comm        = object@agg$comm[i],
        description = "Aggregation weight",
        unit        = object@agg$unit[i],
        stringsAsFactors = FALSE
      )
    }
  }

  # Physical properties carry their own, self-contained unit ("GJ/t", "t/m3"),
  # so unlike `emis`/`agg` they are not composed with the commodity's own.
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
    formula <- .unit_formulas_for("technology")[["cap2act"]]
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
      formula <- .unit_formulas_for("technology")[[key]]
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

# ── Facet labels for plot_process_year() ───────────────────────────────────────

# A ggplot2 labeller turning facet keys ("ava", "cost", "invcost") into
# "ava [GWh/year]". `raw` supplies the parameter names behind each facet
# base; members of a bound family (ava.lo/up/fx) share a unit, so the first
# resolvable one speaks for the facet. Facets with no formula, or whose unit
# is a bare placeholder, are left unlabelled rather than showing "{currency}".
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

# `.get_units_process()` is keyed on the class only through `.unit_formulas_for()`
# and `methods::slot()`, so a class needs no code of its own once its columns
# are catalogued. `trade` has 39 of them and had no method at all.
#' @rdname getUnits
#' @export
setMethod("getUnits", "trade", .get_units_process)

#' @rdname getUnits
#' @export
setMethod("getUnits", "tax", .get_units_process)

#' @rdname getUnits
#' @export
setMethod("getUnits", "subsidy", .get_units_process)

#' @rdname getUnits
#' @export
setMethod("getUnits", "weather", .get_units_process)

#' @rdname getUnits
#' @export
setMethod("getUnits", "calendar", .get_units_process)

#' @rdname getUnits
#' @export
setMethod("getUnits", "config", .get_units_process)

#' @rdname getUnits
#' @export
setMethod("getUnits", "settings", .get_units_process)


# `units` overrides the axis label's base unit PER COMMODITY, so it is a named
# list: units = list(ELC = "GWh"). A bare value is the `param =` mistake --
# `units = "cost"` reads as the name of a parameter to draw, and was accepted
# and then dropped, so the chart answered a question nobody asked.
#' @noRd
.check_units_arg <- function(units) {
  if (is.null(units)) return(invisible(NULL))
  nm <- names(units)
  if (!length(nm) || anyNA(nm) || !all(nzchar(nm))) {
    stop("`units` names a base unit per commodity, e.g. ",
         "units = list(ELC = \"GWh\"). To choose WHICH parameter is drawn, ",
         "use `param =`.", call. = FALSE)
  }
  invisible(NULL)
}

# A style that labels from the object cannot honour `units`. Saying so beats
# dropping it: the argument was given for a reason.
#' @noRd
.units_unused <- function(units, style) {
  if (is.null(units)) return(invisible(NULL))
  warning("`units` labels the profile chart's axis and has no effect on ",
          "style = \"", style, "\".", call. = FALSE)
  invisible(NULL)
}
