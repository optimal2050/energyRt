# check_levels ################################################################
# Declared resolution must match the commodity's resolution.
#
# A commodity declares where it is BALANCED: `@timeframe` in time, `@geoframe`
# in space. Processes must respect that, and the rules differ by class:
#
#  * demand / supply / import / export / trade / storage -- must MATCH. These
#    have no aggregation path of their own: their flows enter `mvInpTot` /
#    `mvOutTot` directly, so a timeslice or region outside the commodity's level
#    produces a cell nothing reads. The parameter is written, the equation is
#    generated at the commodity's level, the lookup misses, and the term
#    silently evaluates to ZERO. The model stays feasible and is wrong.
#
#  * technology -- may DIFFER, but never be coarser than a commodity it
#    touches. `get_process_timeframe()` pins a process to the FINEST of its
#    commodities and the aggregation only flows fine -> coarse
#    (`mTechOutCommAgg`/`AggTimeslice` in time, `mRegionFamily` in space), so a
#    process pinned coarser cannot meet the finer balance.
#
# Note only `technology` even has a `@timeframe` slot; for every other class the
# level is implied by the timeslices and regions appearing in its DECLARED DATA
# (`dem$timeslice`, `availability$region`, ...). That is why this check reads the
# objects rather than the derived parameters -- it reports the mistake where it
# was written, not where it later vanishes.
#
# NA / empty timeslices and regions are wildcards ("all of them") and always pass.

#' @include geoscale.R mapping-builders.R
NULL

# Timeslices appearing in an object's data.frame slots.
#' @noRd
.declared_timeslices <- function(obj) {
  out <- character()
  for (sn in .instance_slots(obj)) {
    if (identical(sn, "misc")) next
    v <- methods::slot(obj, sn)
    if (is.data.frame(v) && "timeslice" %in% colnames(v)) {
      out <- c(out, as.character(v[["timeslice"]]))
    } else if (is.atomic(v) && identical(sn, "timeslice")) {
      out <- c(out, as.character(v))
    }
  }
  unique(out[!is.na(out) & nzchar(out)])
}

# The calendar the DECLARATIONS were written against.
#
# A sampled calendar supplied at interpolation (`scenario@settings@calendar`)
# legitimately contains only some of the model's timeslices -- `subset_timeslices()`
# reweights the data onto them. Validating declarations against the sampled
# calendar would flag every sampled model, so the model's own calendar is the
# reference: the question is whether the author declared consistently with the
# model they wrote, not with a scenario-stage transformation of it.
#' @noRd
.declaration_calendar <- function(scen) {
  cal <- tryCatch(scen@model@config@calendar, error = function(e) NULL)
  if (is.null(cal) || length(cal@timeframes) == 0) cal <- scen@settings@calendar
  cal
}

# The members of a commodity's own level, in each dimension. NULL means
# "unconstrained" (no calendar level found / no geoscale attached).
#' @noRd
.comm_level_members <- function(scen) {
  cal <- .declaration_calendar(scen)
  ctf <- map_comm_timeframe(scen)
  # the MODEL's own hierarchy, not the (possibly sampled) settings one --
  # the geo mirror of `.declaration_calendar()` above
  hier <- .declaration_geo_hierarchy(scen)
  cgl <- map_comm_geoframe(scen)

  comms <- union(names(ctf), names(cgl))
  stats::setNames(lapply(comms, function(cm) {
    tf <- ctf[[cm]]
    if (length(tf) == 0 || is.na(tf) || !nzchar(tf)) tf <- cal@default_timeframe
    timeslices <- if (!is.null(tf) && tf %in% names(cal@timeframes)) {
      as.character(cal@timeframes[[tf]])
    } else NULL
    regions <- if (is.null(hier)) NULL else .geo_level_regions(hier, cgl[[cm]])
    list(timeframe = tf, timeslices = timeslices, regions = regions)
  }), comms)
}

# Classes whose flows have no aggregation path: they must match exactly.
#' @noRd
.STRICT_LEVEL_CLASSES <- c("demand", "supply", "import", "export", "trade",
                           "storage")

#' Check declared resolutions against their commodities
#'
#' Errors when a process declares timeslices or regions outside the level its
#' commodity is balanced at. See the file header for the per-class rules.
#'
#' @param scen a `scenario` mid-interpolation.
#' @return Invisibly `NULL`; called for the error.
#' @noRd
.check_process_levels <- function(scen) {
  lv <- .comm_level_members(scen)
  if (length(lv) == 0) return(invisible(NULL))
  pcomm <- get_process_comm(scen)
  pclass <- get_process_class(scen)
  preg <- get_process_region(scen)

  # Every timeslice / region the model knows, across ALL levels. Anything outside
  # these is not a level error (see the loop below).
  cal <- .declaration_calendar(scen)
  known_timeslices <- unique(unlist(cal@timeframes, use.names = FALSE))
  hier <- .declaration_geo_hierarchy(scen)
  known_regions <- if (is.null(hier)) .model_regions(scen) else hier$region

  bad <- list()
  add <- function(process, class, comm, dim, declared, allowed) {
    bad[[length(bad) + 1L]] <<- data.frame(
      process = process, class = class, comm = comm, dim = dim,
      declared = paste(utils::head(sort(declared), 4), collapse = ", "),
      expected = paste(utils::head(sort(allowed), 4), collapse = ", "),
      stringsAsFactors = FALSE)
  }

  objs <- .level_check_objects(scen)
  # Roles, for the coarse-direction exemption below. `outstg` is the union of
  # produced and stored commodities: both must stay at-or-coarser.
  inp_of <- tryCatch(get_process_inputs(scen), error = function(e) list())
  aux_of <- tryCatch(get_process_aux(scen), error = function(e) list())
  outstg_of <- local({
    o <- tryCatch(get_process_outputs(scen), error = function(e) list())
    st <- tryCatch(get_process_stored(scen), error = function(e) list())
    ks <- union(names(o), names(st))
    stats::setNames(lapply(ks, function(k)
      unique(c(as.character(o[[k]]), as.character(st[[k]])))), ks)
  })
  for (nm in names(objs)) {
    cls <- pclass[[nm]]
    if (is.null(cls) || !(cls %in% .STRICT_LEVEL_CLASSES)) next
    cms <- intersect(pcomm[[nm]], names(lv))
    if (length(cms) == 0) next

    timeslices <- .declared_timeslices(objs[[nm]])
    regions <- as.character(preg[[nm]])

    for (cm in cms) {
      # Only timeslices/regions the calendar or geoscale actually KNOWS are judged.
      # A declaration naming something absent entirely is not a level error --
      # that is how sampling works: a model may be built on a sampled calendar
      # while its data still carries the full timeslice set, and `subset_timeslices()`
      # reweights onto what remains. The genuine defect is a name that EXISTS
      # but sits at a different level than the commodity's, because that cell
      # is written and then never read.
      allowed_s <- lv[[cm]]$timeslices
      if (!is.null(allowed_s) && length(timeslices) > 0) {
        off <- setdiff(intersect(timeslices, known_timeslices), allowed_s)
        if (length(off) > 0) {
          add(nm, cls, cm, "timeslice", off, allowed_s)
        }
      }
      # The REGION axis is per-class: a storage may sit at a region level
      # COARSER than its input, which .assert_process_geoframe() admits and the
      # mStorageProcAggRegion branch sums. A region FINER than the commodity is
      # still a mismatch for every strict class, so only the coarse direction
      # is exempt, and only for storage.
      # The REGION axis is exempt in the COARSE direction for a commodity the
      # object only CONSUMES -- the no-disaggregation rule (R/process-levels.R):
      # the input is drawn from the finer cells beneath the object and summed.
      # The test is on the ROLE, not on check ordering: a commodity that is
      # also produced or stored keeps the strict match, so widening this never
      # silently admits an output.
      allowed_r <- lv[[cm]]$regions
      if (!is.null(allowed_r) && length(regions) > 0) {
        off <- setdiff(intersect(regions, known_regions), allowed_r)
        # An AUXILIARY commodity is exempt in BOTH directions, unlike a main
        # flow. Its equation is written at the process's own cell and pins the
        # SUM of the child variables to the coefficient, so the aggregation
        # path this message is about exists either way: nothing is written at a
        # cell that is never read. A main output has no such equation, which is
        # why it stays strict.
        is_aux <- cm %in% aux_of[[nm]] &&
          !(cm %in% inp_of[[nm]]) && !(cm %in% outstg_of[[nm]])
        if (length(off) > 0 && .coarse_axis_ready("region") &&
            cls %in% .coarse_classes_supported &&
            (is_aux || (cm %in% inp_of[[nm]] && !(cm %in% outstg_of[[nm]])))) {
          off <- off[!.region_is_coarser(scen, off, allowed_r)]
        }
        if (length(off) > 0) {
          add(nm, cls, cm, "region", off, allowed_r)
        }
      }
    }
  }

  if (length(bad) == 0) return(invisible(NULL))
  tab <- do.call(rbind, bad)
  stop(
    "Declared resolution does not match the commodity's own level.\n",
    "These flows have no aggregation path, so the mismatched cells would be\n",
    "written but never read, and the term would silently evaluate to zero:\n\n   ",
    paste(utils::capture.output(print(tab, row.names = FALSE)),
          collapse = "\n   "),
    "\n\nDeclare them at the commodity's level, or change the commodity's ",
    "`@timeframe`/`@geoframe`.",
    call. = FALSE
  )
}

# --- resolving a summand's level to actual variable indices ----------------- #
#
# A constraint is an EXPRESSION, not a declaration, so naming a level the
# variable is not indexed by is not an error -- it means "aggregate to there".
# Two readings are possible and the variable's own domain decides which:
#
#   * the variable occupies SEVERAL levels (e.g. `vOutTot` for a commodity with
#     a coarse `@geoframe`, materialised at both the region and the nation):
#     RESTRICT to the named level, or summing would double-count.
#   * the variable occupies ONE level (e.g. `vTechOut`, only ever at process
#     regions): EXPAND the named level to its members and sum them. This is
#     what lets a user constrain "emissions in province P" without knowing
#     which level CO2 happens to be balanced at.
#
# Anything that resolves to nothing is an error: a free variable with no
# defining equation would make the constraint trivially satisfiable.

# Every region at `level` plus every region beneath it, walking the adjacent
# family table down. The spatial counterpart of a timeslice's descendants in
# `calendar@timeslice_ancestry`.
# Every region beneath ONE region (not a whole level), walking the family down.
#' @noRd
.geo_descendants_of <- function(hier, region) {
  fam <- hier$family
  out <- character()
  frontier <- as.character(region)
  for (i in seq_along(hier$levels)) {
    kids <- unique(as.character(fam$regionp[as.character(fam$region) %in% frontier]))
    kids <- setdiff(kids, out)
    if (length(kids) == 0) break
    out <- c(out, kids)
    frontier <- kids
  }
  out
}

#' @noRd
.geo_descendants <- function(hier, level) {
  own <- hier$members[[level]]
  if (is.null(own)) return(character())
  fam <- hier$family                 # region = parent, regionp = child
  out <- own
  frontier <- own
  for (i in seq_along(hier$levels)) {
    kids <- unique(as.character(fam$regionp[as.character(fam$region) %in% frontier]))
    kids <- setdiff(kids, out)
    if (length(kids) == 0) break
    out <- c(out, kids)
    frontier <- kids
  }
  out
}

# The gating map of a variable, e.g. vTechOut -> "mvTechOut". The mapping entry
# is a string "vX( dims ) $ mvX( dims )".
#' @noRd
.variable_gate_map <- function(variable) {
  s <- tryCatch(.variable_mapping[[variable]], error = function(e) NULL)
  if (is.null(s) || !nzchar(s) || !grepl("\\$", s)) return(NULL)
  nm <- sub(".*\\$\\s*([A-Za-z0-9_.]+)\\s*\\(.*$", "\\1", s)
  if (identical(nm, s)) NULL else nm
}

# Distinct values of one dimension in a variable's domain, as already built.
#' @noRd
.variable_domain_values <- function(prec, variable, dim) {
  nm <- .variable_gate_map(variable)
  if (is.null(nm)) return(NULL)
  p <- prec@parameters[[nm]]
  if (is.null(p)) return(NULL)
  d <- get_data_slot(p)
  if (is.null(d) || nrow(d) == 0 || !(dim %in% names(d))) return(NULL)
  unique(as.character(as.data.frame(d)[[dim]]))
}

# Rows of a variable's domain (its gate map), as already built.
#' @noRd
.variable_domain_rows <- function(prec, variable) {
  nm <- .variable_gate_map(variable)
  if (is.null(nm)) return(NULL)
  p <- prec@parameters[[nm]]
  if (is.null(p)) return(NULL)
  d <- get_data_slot(p)
  if (is.null(d) || nrow(d) == 0) return(NULL)
  as.data.frame(d)
}

# Resolve `level` to the indices of `variable`'s `dim` that represent it.
#
# `members` is a named list level -> its own members (coarsest first);
# `descendants` is `level`'s own members plus everything beneath them.
#
# The decision is made PER GROUP -- the distinct combinations of the variable's
# OTHER dimensions -- because the pooled column cannot tell two situations
# apart. `mvOutTot` for one commodity carries rows at several levels and the
# coarse row IS the roll-up, so pinning is right and summing both levels would
# double count. `mvTechOut` in a model with one annual and one seasonal
# commodity carries both levels too, but across DIFFERENT technologies, where
# nothing aggregates anything and pinning silently drops the seasonal one.
#
# Only groups with a row AT `level` OR FINER take part. "Finer" means deeper in
# the hierarchy -- with ANNUAL < SEASON < HOUR, the set for SEASON is the seasons
# and the hours under them, NOT the annual slice. A group sitting entirely
# COARSER than `level` has nothing at that resolution and correctly contributes
# nothing: `tm_core` asked at SEASON has no seasonal coal, which is not an error.
#
#   every participating group has a row AT `level` -> pin
#   none does                                      -> sum what lies beneath
#   some do and some do not                        -> ambiguous; caller errors
#
# Returns `list(mode, values, missing)`; `missing` names the groups that lack a
# row at `level` so the caller can report them.
#' @noRd
.resolve_level_indices <- function(domain, level, members, descendants,
                                   rows = NULL, dim = NULL) {
  own <- members[[level]]
  if (is.null(own)) return(list(mode = "pin", values = NULL, missing = character()))
  if (is.null(domain)) {
    return(list(mode = "pin", values = own, missing = character()))
  }
  .pin <- list(mode = "pin", values = intersect(own, domain), missing = character())
  .sum <- list(mode = "sum", values = intersect(descendants, domain),
               missing = character())

  if (is.null(rows) || is.null(dim) || !(dim %in% names(rows))) return(.sum)
  other <- setdiff(names(rows), c(dim, "value"))
  if (length(other) == 0) return(.pin)

  vals <- as.character(rows[[dim]])
  key <- do.call(paste, c(lapply(rows[other], as.character), list(sep = " / ")))
  keep <- vals %in% descendants
  part <- unique(key[keep])                    # groups at `level` or finer
  at <- unique(key[vals %in% own])             # groups with a row AT `level`
  if (length(part) == 0) return(.sum)          # nothing there; caller errors

  # Pinning is right for a group that HAS a row at the level: by the roll-up
  # that row already equals its finer ones. So if every participating group has
  # one, pin -- and never sum across the two, which would double count.
  if (all(part %in% at)) return(.pin)

  # Otherwise summing the whole descendant set is right UNLESS some group holds
  # rows at two levels within it, which is the only way a sum can count the same
  # quantity twice. A group sitting at exactly one level contributes its own
  # rows once, whatever level that is -- so a variable whose members each live
  # at a single level aggregates correctly even when the members disagree about
  # which level that is. That is the common case for a per-process variable in a
  # model mixing annual and sub-annual commodities, and it needs no parent rows.
  lvl_of <- rep(NA_character_, length(vals))
  for (lv in names(members)) lvl_of[vals %in% members[[lv]]] <- lv
  spans <- tapply(lvl_of[keep], key[keep],
                  function(z) length(unique(z[!is.na(z)])) > 1L)
  if (!any(spans, na.rm = TRUE)) return(.sum)

  # Some group has only finer rows while another holds several levels: pinning
  # drops the first, summing double counts the second, and a flat `for.sum`
  # cannot do both.
  list(mode = "mixed", values = character(),
       missing = utils::head(sort(names(spans)[which(spans)]), 8))
}

# Is each of `regs` at a level COARSER than the commodity's own regions? Used
# to exempt the coarse direction from the strict level check for a class whose
# flows are summed over the finer cells beneath it.
#' @noRd
.region_is_coarser <- function(scen, regs, comm_regs) {
  h <- tryCatch(.scen_geo_hierarchy(scen), error = function(e) NULL)
  rr <- .geo_region_rank(h)
  if (is.null(rr) || length(comm_regs) == 0) {
    return(rep(FALSE, length(regs)))
  }
  cr <- suppressWarnings(min(rr[unique(as.character(comm_regs))], na.rm = TRUE))
  if (!is.finite(cr)) return(rep(FALSE, length(regs)))
  r <- unname(rr[as.character(regs)])
  !is.na(r) & r < cr
}

# Expand coarse indices named in `for.sum` down to the variable's own.
#
# The case this exists for: "constrain CO2 from these technologies in province
# P". The user names P, a region the emission variable is not indexed by --
# emissions live at counties. Naming P should sum P's counties, not build a
# constraint over an index that does not exist (which would leave a free
# variable and a trivially satisfiable constraint).
#
# A value the variable IS indexed by is kept as-is, so a commodity balanced at
# a coarse level still resolves to that level rather than being expanded past
# it. A value that resolves to nothing is an error naming the alternative.
#' @noRd
.expand_for_sum <- function(vals, dom, descend, what, variable, stop_fn) {
  if (is.null(vals) || length(vals) == 0 || is.null(dom)) return(vals)
  keep_as_is <- vals[is.na(vals) | !nzchar(as.character(vals))]
  vals <- as.character(vals)
  vals <- vals[!is.na(vals) & nzchar(vals)]         # NA/"" are wildcards
  if (length(vals) == 0) return(keep_as_is)
  out <- unlist(lapply(vals, function(v) {
    if (v %in% dom) return(v)                       # a real index: keep
    d <- intersect(descend(v), dom)                 # coarse: expand beneath it
    if (length(d) > 0) return(d)
    stop_fn(paste0(
      what, ' "', v, '" is not an index of variable "', variable,
      '" and has nothing beneath it there. It exists at: ',
      paste(utils::head(sort(dom), 6), collapse = ", "),
      '.\n  Write the sum explicitly in `for.sum` instead.'))
  }), use.names = FALSE)
  unique(c(keep_as_is, out))
}

# The model objects themselves, keyed by name -- needed because the declared
# timeslices live in the object's slots, not in any collected table.
#' @noRd
.level_check_objects <- function(scen) {
  out <- list()
  data <- scen@model@data
  for (rp in data) {
    objs <- if (methods::is(rp, "repository")) rp@data else list(rp)
    for (o in objs) {
      if (!isS4(o) || !methods::.hasSlot(o, "name")) next
      out[[o@name]] <- o
    }
  }
  out
}
