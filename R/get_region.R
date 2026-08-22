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

# =========================================================================== #
# Cost-slot region guards.
#
# Trade capacity is region-free -- one number per corridor -- but its costs are
# region-indexed, and `eqTradeInv`/`eqTradeEac`/`eqTradeFixom`/`eqTradeRetCost`
# apply the FULL capacity in every region the cost names, with no share factor.
# That is the intended convention (`invcost` is what each endpoint bears), but it
# has two edges a user cannot see:
#
#   * an UNREGIONED row broadcasts to every endpoint, so a two-endpoint corridor
#     is charged twice -- measured: fixom 1 on a 100/80/0 fleet costs 360, not
#     180;
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
