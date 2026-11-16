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
      # a row giving only a bound over a parent timeslice (`afs.*`) is
      # declared at that coarser slice on purpose
      agg <- intersect(c("afs.lo", "afs.up", "afs.fx"), colnames(v))
      if (length(agg) && nrow(v)) {
        num <- setdiff(colnames(v)[vapply(v, is.numeric, logical(1))], c(agg, "year"))
        only_agg <- rowSums(!is.na(v[, agg, drop = FALSE])) > 0 &
          (if (length(num)) rowSums(!is.na(v[, num, drop = FALSE])) == 0 else TRUE)
        v <- v[!only_agg, , drop = FALSE]
      }
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
  tab <- .level_violations(scen)
  if (is.null(tab)) return(invisible(NULL))
  stop("Declared resolution does not match the commodity's own level.
",
       "These flows have no aggregation path, so the mismatched cells would ",
       "be
written but never read, and the term would silently evaluate to ",
       "zero:

   ",
       paste(utils::capture.output(print(tab, row.names = FALSE)),
             collapse = "
   "),
       "

Declare them at the commodity's level, or change the commodity's ",
       "`@timeframe`/`@geoframe`.", call. = FALSE)
}

# The violations themselves, so the same rules can be REPORTED and not only
# raised: `check_model_levels()` collects every one of them.
#' @noRd
.level_violations <- function(scen) {
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

  if (length(bad) == 0) return(NULL)
  do.call(rbind, bad)
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
  data <- .scen_build_model(scen)@data
  for (rp in data) {
    objs <- if (methods::is(rp, "repository")) rp@data else list(rp)
    for (o in objs) {
      if (!isS4(o) || !methods::.hasSlot(o, "name")) next
      out[[o@name]] <- o
    }
  }
  out
}

# ---------------------------------------------------------------------------
# check_model_levels(): the same rules, reported rather than raised
# ---------------------------------------------------------------------------

#' @noRd
.level_empty <- function() {
  data.frame(process = character(), class = character(), comm = character(),
             dim = character(), declared = character(), expected = character(),
             stringsAsFactors = FALSE)
}

# Every object of a model, flat and named -- the model-side twin of
# `.level_check_objects()`, which needs a scenario to build one first.
#' @noRd
.level_model_objects <- function(mod) {
  out <- list()
  for (rp in mod@data) {
    objs <- if (methods::is(rp, "repository")) rp@data else list(rp)
    for (o in objs) {
      if (!isS4(o) || !methods::.hasSlot(o, "name")) next
      nm <- as.character(o@name)[1]
      if (is.na(nm) || !nzchar(nm)) next
      out[[nm]] <- o
    }
  }
  out
}

# Regions a declaration names: the `@region` slot plus every region-keyed
# column, endpoints included -- a corridor's level is its endpoints'.
#' @noRd
.level_declared_regions <- function(obj) {
  out <- tryCatch(as.character(methods::slot(obj, "region")),
                  error = function(e) character())
  for (sn in .instance_slots(obj)) {
    if (identical(sn, "misc")) next
    v <- tryCatch(methods::slot(obj, sn), error = function(e) NULL)
    if (!is.data.frame(v) || !nrow(v)) next
    for (cn in intersect(c("region", "src", "dst"), colnames(v))) {
      out <- c(out, as.character(v[[cn]]))
    }
  }
  unique(out[!is.na(out) & nzchar(out)])
}

# Commodities by role, read off the slots rather than from interpolation maps.
#' @noRd
.level_roles <- function(obj) {
  col <- function(sl, cn) {
    v <- tryCatch(methods::slot(obj, sl), error = function(e) NULL)
    if (!is.data.frame(v) || !nrow(v) || !cn %in% names(v)) return(character())
    unique(as.character(v[[cn]][!is.na(v[[cn]])]))
  }
  list(inp = col("input", "comm"),
       outstg = unique(c(col("output", "comm"), col("storage", "comm"))),
       aux = unique(c(col("aux", "acomm"), col("aeff", "acomm"))))
}

#' @noRd
.level_violations_model <- function(mod) {
  objs <- .level_model_objects(mod)
  comms <- objs[vapply(objs, function(o) methods::is(o, "commodity"),
                       logical(1))]
  if (!length(comms)) return(NULL)
  cal <- tryCatch(mod@config@calendar, error = function(e) NULL)
  gs <- tryCatch(mod@config@geoscale, error = function(e) NULL)
  has_gs <- !is.null(gs) && is_geoscale(gs)
  frames <- if (has_gs) geoscales::geoscale_geoframes(gs) else character()
  mregs <- as.character(mod@config@region)
  tf_all <- if (is.null(cal)) list() else cal@timeframes
  known_s <- unique(unlist(tf_all, use.names = FALSE))
  known_r <- mregs
  ltg <- NULL
  if (has_gs) {
    ltg <- as.data.frame(geoscales::geoscale_leaftable(gs))
    known_r <- unique(c(as.character(ltg$region),
                        unlist(lapply(frames, function(f)
                          as.character(ltg[[f]])), use.names = FALSE)))
    known_r <- known_r[!is.na(known_r) & nzchar(known_r)]
  }
  bad <- list()
  add <- function(...) bad[[length(bad) + 1L]] <<- data.frame(
    ..., stringsAsFactors = FALSE)
  fmt <- function(x) paste(utils::head(sort(unique(x)), 4), collapse = ", ")
  # Level members per commodity, plus the pre-flight neither axis had: a frame
  # a commodity names that the model's geoscale or calendar does not have.
  lv <- list()
  for (cm in names(comms)) {
    o <- comms[[cm]]
    gf <- as.character(methods::slot(o, "geoframe"))
    gf <- gf[!is.na(gf) & nzchar(gf)]
    tf <- as.character(methods::slot(o, "timeframe"))
    tf <- tf[!is.na(tf) & nzchar(tf)]
    regs <- mregs
    if (length(gf)) {
      if (!gf[1] %in% frames) {
        add(process = cm, class = "commodity", comm = cm, dim = "geoframe",
            declared = gf[1],
            expected = if (length(frames)) fmt(frames) else "<no geoscale>")
        regs <- NULL
      } else {
        regs <- unique(as.character(ltg[[gf[1]]]))
        regs <- regs[!is.na(regs) & nzchar(regs)]
      }
    }
    slices <- NULL
    if (length(tf)) {
      if (!tf[1] %in% names(tf_all)) {
        add(process = cm, class = "commodity", comm = cm, dim = "timeframe",
            declared = tf[1],
            expected = if (length(tf_all)) fmt(names(tf_all)) else
              "<no calendar>")
      } else {
        slices <- as.character(tf_all[[tf[1]]])
      }
    }
    lv[[cm]] <- list(regions = regs, timeslices = slices)
  }
  for (nm in names(objs)) {
    o <- objs[[nm]]
    cls <- class(o)[1]
    if (!cls %in% .STRICT_LEVEL_CLASSES) next
    cms <- intersect(.lvl_object_comms(o, names(comms)), names(lv))
    if (!length(cms)) next
    regions <- .level_declared_regions(o)
    slices <- .declared_timeslices(o)
    roles <- .level_roles(o)
    for (cm in cms) {
      allowed_s <- lv[[cm]]$timeslices
      if (!is.null(allowed_s) && length(slices)) {
        off <- setdiff(intersect(slices, known_s), allowed_s)
        if (length(off)) {
          add(process = nm, class = cls, comm = cm, dim = "timeslice",
              declared = fmt(off), expected = fmt(allowed_s))
        }
      }
      allowed_r <- lv[[cm]]$regions
      if (!is.null(allowed_r) && length(regions)) {
        off <- setdiff(intersect(regions, known_r), allowed_r)
        # The coarse direction is exempt for a commodity this object only
        # CONSUMES, or carries as `@aux`, and only where the class has an
        # aggregation path at all -- the same test the assertion makes.
        is_aux <- cm %in% roles$aux && !(cm %in% roles$inp) &&
          !(cm %in% roles$outstg)
        exempt <- cls %in% .coarse_classes_supported &&
          (is_aux || (cm %in% roles$inp && !(cm %in% roles$outstg)))
        if (length(off) && exempt && has_gs) {
          rk_ok <- suppressWarnings(min(geoscales::geoscale_rank(
            gs, stats::na.omit(.agg_frames_of(allowed_r, gs))), na.rm = TRUE))
          rk_off <- geoscales::geoscale_rank(gs, .agg_frames_of(off, gs))
          if (is.finite(rk_ok)) off <- off[!(!is.na(rk_off) & rk_off < rk_ok)]
        }
        if (length(off)) {
          add(process = nm, class = cls, comm = cm, dim = "region",
              declared = fmt(off), expected = fmt(allowed_r))
        }
      }
    }
  }
  if (!length(bad)) return(NULL)
  do.call(rbind, bad)
}

#' Check declared resolutions against the commodities they serve
#'
#' The rule `interpolate_model()` enforces, reported instead of raised. Six
#' classes -- `demand`, `supply`, `import`, `export`, `trade`, `storage` --
#' have no aggregation path between a flow and its commodity, so a region or a
#' timeslice declared at a different level than the one the commodity balances
#' at is a cell that is written and never read: the term silently evaluates to
#' zero. Interpolation stops at the first call that finds any; this collects
#' every one, on a model that has not been interpolated yet.
#'
#' Also reported, which the assertion cannot see from inside interpolation: a
#' commodity naming a `@geoframe` the model's geoscale does not have, or a
#' `@timeframe` its calendar does not have.
#'
#' The coarse direction stays exempt exactly as at interpolation -- for a
#' commodity the object only consumes or carries as `@aux`, and only where the
#' class has an aggregation path.
#'
#' @param object a [model] or a `scenario`.
#'
#' @return A `data.frame` with one row per object, commodity and axis, holding
#'   the declared and the expected members; no rows when every declaration
#'   matches. Printed as a short verdict.
#' @family validation
#' @seealso [check_geoscale_regions()], [check_feasibility()],
#'   [aggregate_model_regions()]
#' @export
#' @examples
#' repo <- newRepository(
#'   "mini",
#'   newCommodity("ELC", unit = "MWh"),
#'   newDemand("DEM", commodity = "ELC",
#'             demand = data.frame(region = "R1", year = 2020, demand = 10))
#' )
#' mod <- newModel("MINI", data = repo, calendar = calendars$annual,
#'                 region = "R1", horizon = horizons$Y2020_2060_by_5)
#' check_model_levels(mod)
check_model_levels <- function(object) {
  tab <- if (methods::is(object, "scenario")) .level_violations(object) else
    if (methods::is(object, "model")) .level_violations_model(object) else
      stop("`object` must be a model or a scenario.", call. = FALSE)
  if (is.null(tab)) tab <- .level_empty()
  rownames(tab) <- NULL
  class(tab) <- c("model_levels_check", class(tab))
  tab
}

#' @method print model_levels_check
#' @export
print.model_levels_check <- function(x, ...) {
  n <- nrow(x)
  if (n == 0L) {
    cat("Levels: every declaration matches its commodity.\n")
    return(invisible(x))
  }
  cat("Levels: ", n, " mismatch(es).\n", sep = "")
  print(as.data.frame(unclass(x)), row.names = FALSE)
  cat("\nDeclare them at the commodity's level, or change the commodity's",
      "`@geoframe` / `@timeframe`.\n")
  invisible(x)
}
