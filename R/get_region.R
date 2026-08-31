#' Collect the regions an object operates in
#'
#' Generic, reflective accessor that walks every slot of an S4 model object
#' (except `misc`) and gathers the regions it refers to. Regions are read from:
#'   * any atomic slot named `region`, `src`, or `dst`, and
#'   * the `region`, `src`, and `dst` columns of any `data.frame` slot.
#'
#' The result is the set of unique, non-missing, non-empty region labels. The
#' function is intentionally schema-agnostic so it keeps working as region
#' information is added to classes that do not yet carry an explicit `@region`
#' slot (e.g. `import` / `export`).
#'
#' @param obj a model object (S4) such as `technology`, `storage`, `trade`,
#'   `import`, or `export`.
#'
#' @returns a character vector of region labels (possibly empty).
#'
#' @family model
#' @export
get_region <- function(obj) {
  if (!isS4(obj)) {
    return(character(0))
  }
  # A model declares its regions on `@config`, not on a slot of its own, so the
  # reflective walk below finds nothing there. Its declared regions are the
  # answer a caller wants, and reading them should not require `@config@region`.
  if (methods::.hasSlot(obj, "config")) {
    cfg <- methods::slot(obj, "config")
    if (isS4(cfg) && methods::.hasSlot(cfg, "region")) {
      out <- as.character(methods::slot(cfg, "region"))
      return(unique(out[!is.na(out) & nzchar(out)]))
    }
  }
  keys <- c("region", "src", "dst")
  out <- character(0)
  for (sn in .instance_slots(obj)) {
    if (identical(sn, "misc")) next
    v <- methods::slot(obj, sn)
    if (is.data.frame(v)) {
      for (cc in intersect(keys, colnames(v))) {
        out <- c(out, as.character(v[[cc]]))
      }
    } else if (is.atomic(v) && sn %in% keys) {
      out <- c(out, as.character(v))
    }
  }
  out <- out[!is.na(out) & nzchar(out)]
  unique(out)
}

# Guard: error if any region referenced in the model's objects is not declared.
# `model@data` is a list of repositories (or bare objects); regions are gathered
# reflectively via `get_region()` (covers region / src / dst, atomic or
# data.frame). NA / "" entries are wildcards ("all declared regions") and are
# ignored. Run early in `interp_mod()` so a stray region fails fast with a clear
# message rather than as an out-of-domain error in a solver writer.
.check_declared_regions <- function(model, declared) {
  declared <- as.character(declared)
  declared <- declared[!is.na(declared) & nzchar(declared)]
  used <- character(0)
  for (rp in model@data) {
    objs <- if (methods::is(rp, "repository")) rp@data else list(rp)
    for (o in objs) used <- c(used, get_region(o))
  }
  undeclared <- setdiff(unique(used), declared)
  if (length(undeclared) > 0) {
    stop(
      "The model references undeclared region(s): ",
      paste(sort(undeclared), collapse = ", "),
      ".\nDeclare them in the model's regions (config/settings) or remove them ",
      "from the affected objects' data.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' Collect the weather objects an object refers to
#'
#' Reflective accessor that walks every slot of an S4 model object (except
#' `misc`) and gathers the names in any `weather` column. A `technology`,
#' `storage` or `supply` links to a weather profile by name; the profile itself
#' is a separate `weather` object in the repository.
#'
#' @param obj a model object (S4) such as `technology`, `storage` or `supply`.
#'
#' @returns a character vector of weather object names (possibly empty).
#'
#' @family model
#' @export
get_weather <- function(obj) {
  if (!isS4(obj)) {
    return(character(0))
  }
  out <- character(0)
  for (sn in .instance_slots(obj)) {
    if (identical(sn, "misc")) next
    v <- methods::slot(obj, sn)
    if (is.data.frame(v) && "weather" %in% colnames(v)) {
      out <- c(out, as.character(v[["weather"]]))
    }
  }
  out <- out[!is.na(out) & nzchar(out)]
  unique(out)
}

# Guard: error if a weather profile referenced by name has no object behind it.
#
# `collect_set_elements()` builds the `weather` set from the REFERENCES, not
# from the declared objects, so a dangling name enters the set, contributes no
# `pWeather` rows, and the availability limit it carries is dropped without
# error -- the process then runs unconstrained.
.check_declared_objects <- function(model) {
  declared <- character(0)
  used <- list()
  for (rp in model@data) {
    objs <- if (methods::is(rp, "repository")) rp@data else list(rp)
    for (o in objs) {
      if (methods::is(o, "weather")) declared <- c(declared, o@name)
      w <- get_weather(o)
      if (length(w) > 0) {
        used[[length(used) + 1L]] <- data.frame(
          object = .object_label(o), weather = w, stringsAsFactors = FALSE)
      }
    }
  }
  if (length(used) == 0L) {
    return(invisible(TRUE))
  }
  used <- do.call(rbind, used)
  miss <- used[!used$weather %in% declared, , drop = FALSE]
  if (nrow(miss) > 0L) {
    by_w <- tapply(miss$object, miss$weather, function(x)
      paste(unique(x), collapse = ", "))
    stop(
      "The model references weather object(s) that are not in the repository: ",
      paste(sprintf("%s (from %s)", names(by_w), by_w), collapse = "; "),
      ".\nAdd them with `add()`, or remove the reference from the affected ",
      "objects' `weather` slot. A dangling reference does not fail on its own: ",
      "the availability limit it carries is silently dropped.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

# Guard: error if a technology's `geff` names an input group that no commodity
# belongs to.
#
# `@group` itself is optional -- groups are collected from the `group` column of
# `@input` / `@output`, and a model with the same groups declared and undeclared
# gives an identical answer -- so membership, not declaration, is what can be
# checked. A `geff` row for an empty group builds a group-efficiency constraint
# over no commodities, making the problem infeasible; the solver reports only
# "PROBLEM HAS NO PRIMAL FEASIBLE SOLUTION", which does not name the group.
.check_group_members <- function(model) {
  bad <- list()
  for (rp in model@data) {
    objs <- if (methods::is(rp, "repository")) rp@data else list(rp)
    for (o in objs) {
      if (!methods::is(o, "technology")) next
      ge <- methods::slot(o, "geff")
      if (!is.data.frame(ge) || !"group" %in% colnames(ge)) next
      used <- .nonblank(ge[["group"]])
      if (length(used) == 0L) next
      members <- unique(c(.slot_col(o, "input", "group"),
                          .slot_col(o, "output", "group")))
      orphan <- setdiff(used, members)
      if (length(orphan) > 0L) {
        bad[[length(bad) + 1L]] <- sprintf("%s: %s", .object_label(o),
                                           paste(sort(orphan), collapse = ", "))
      }
    }
  }
  if (length(bad) > 0L) {
    stop(
      "The `geff` of the following technology/ies names input group(s) that no ",
      "commodity belongs to: ", paste(unlist(bad), collapse = "; "),
      ".\nPut a commodity in the group via the `group` column of `@input` or ",
      "`@output`, or drop the `geff` row. A group with no members yields an ",
      "infeasible problem, not a wrong one.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.slot_col <- function(o, slot, col) {
  v <- tryCatch(methods::slot(o, slot), error = function(e) NULL)
  if (!is.data.frame(v) || !col %in% colnames(v)) return(character(0))
  .nonblank(v[[col]])
}

.nonblank <- function(x) {
  x <- as.character(x)
  unique(x[!is.na(x) & nzchar(x)])
}

# A name for the error message. Every model object has `@name`, but an unnamed
# one would make the message useless, so fall back to the class.
.object_label <- function(o) {
  nm <- tryCatch(as.character(methods::slot(o, "name")), error = function(e) "")
  if (length(nm) != 1L || is.na(nm) || !nzchar(nm)) class(o)[1] else nm
}

# =========================================================================== #
# Cost-slot region guards.
#
# Trade capacity is region-free -- one number per corridor -- but its costs are
# region-indexed, and `eqTradeInv`/`eqTradeEac`/`eqTradeFixom`/`eqTradeRetCost`
# apply the FULL capacity in every region the cost names, with no share factor.
# That is the intended convention (`invcost` is what each endpoint bears), but it
# has two edges a user cannot see:
#
#   * an UNREGIONED row broadcasts to every endpoint, so a two-endpoint
#     corridor is charged twice;
#   * a COARSER geoscale level is charged ONCE at that cell -- not at each
#     child -- which is usually what a national cost datum means, but is a
#     different number from naming the children individually.
#
# Both are reported, not corrected.
#
# The unregioned case is a MESSAGE. It looked like an ambiguity worth refusing
# until the arithmetic was read properly: `invcost` is a RATE PER ENDPOINT, so
# an unregioned row is not a doubled total, it is the rate applied at each end.
# That is also what makes it right -- a corridor over A-B-C-D at 25 each costs
# 100 whole and 50 in a B-C subset, so a partial-region study bears its own
# share instead of the whole corridor. Refusing it would force allocation that
# buys nothing and produces the same numbers.
#
# The coarse-level case was a WARNING while such a cost never reached the
# objective at all. It does now (`mvTotalCost` carries the cells a cost was
# actually declared at), so it is a MESSAGE: legal, meaningful, and worth
# naming only because the arithmetic differs from the per-child form.
#
# Scope is the COST slots only. Trade `@aeff` also has `src`/`dst`, but there
# they are ROUTE SELECTORS -- "NA for every source region on the route" -- not
# a cost attribution, so an NA is meaningful there and stays allowed.
# =========================================================================== #

# Region column of a cost slot, or character(0) when the slot is absent/empty.
.cost_slot_regions <- function(obj, slot) {
  if (!.hasSlot(obj, slot)) return(character(0))
  d <- methods::slot(obj, slot)
  if (!is.data.frame(d) || !nrow(d) || !("region" %in% names(d))) return(character(0))
  as.character(d$region)
}

# `model_regions` = the regions costs can actually accrue in (`.model_regions()`
# in the scenario); `known_regions` additionally carries the geoscale's coarser
# levels. A cost region in the second but not the first is silently free.
.check_cost_regions <- function(model, model_regions, known_regions,
                                slots = c("invcost", "fixom")) {
  model_regions <- as.character(model_regions)
  coarse_hits <- list()
  broadcast <- character(0)

  for (rp in model@data) {
    objs <- if (methods::is(rp, "repository")) rp@data else list(rp)
    for (o in objs) {
      nm <- tryCatch(o@name, error = function(e) NA_character_)
      for (sl in slots) {
        rg <- .cost_slot_regions(o, sl)
        if (!length(rg)) next
        # coarse: named, declared, but not a region costs accrue in
        cs <- setdiff(unique(rg[!is.na(rg)]), model_regions)
        cs <- intersect(cs, as.character(known_regions))
        if (length(cs)) coarse_hits[[length(coarse_hits) + 1L]] <-
          paste0(nm, "@", sl, ": ", paste(sort(cs), collapse = ", "))
        # broadcast: an NA region on a multi-endpoint object (only trade has one)
        if (any(is.na(rg)) && methods::is(o, "trade")) {
          ends <- unique(c(as.character(o@routes$src), as.character(o@routes$dst)))
          ends <- ends[!is.na(ends) & nzchar(ends)]
          if (length(ends) > 1L)
            broadcast <- c(broadcast,
                           paste0(nm, "@", sl, " (", length(ends), " endpoints: ",
                                  paste(sort(ends), collapse = ", "), ")"))
        }
      }
    }
  }

  if (length(coarse_hits)) {
    message(
      "Cost declared at a coarse geoscale level:\n  ",
      paste(unlist(coarse_hits), collapse = "\n  "),
      "\nThe cost is charged ONCE at that cell rather than at each ",
      "child region, and is discounted at the children's rate. That is ",
      "usually what a national cost datum means -- said here because it is ",
      "a different number from naming the children individually.")
  }
  if (length(broadcast)) {
    message(
      "Trade cost with no `region` on a multi-endpoint route:\n  ",
      paste(sort(unique(broadcast)), collapse = "\n  "),
      "\nTrade costs are a rate borne by EACH endpoint, so an unregioned ",
      "row applies at every endpoint of the route: a two-region corridor at ",
      "100 costs 200 in total. Name the regions to vary the rate between ",
      "them, e.g.\n",
      "    invcost = data.frame(region = c(\"A\", \"B\"), invcost = c(60, 40))\n",
      "This is reported, not corrected: the rate is the same one a region ",
      "subset would see, so it is what makes a partial-region study bear its ",
      "own share rather than the whole corridor.")
  }
  invisible(NULL)
}
