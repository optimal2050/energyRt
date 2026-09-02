# =========================================================================== #
# region_gaps.R -- processes that cannot operate in a region they are declared
# in, because a commodity they require is not available there.
#
# `mCommReg` is the commodity-region closure: where each commodity can exist.
# The activity domains (`mvTechAct`, and the storage analogue) are built from
# the process's SPAN and carry no commodity, so they are not narrowed by it --
# only the per-commodity flow domains are (`.filt_cr`). A process declared where
# its input is unavailable therefore keeps its activity and output rows while
# losing its input rows: the input constraint is not violated, it does not
# exist, and the process produces from nothing at zero cost. The solution is
# feasible, optimal, and passes every `verify_solution()` invariant, because the
# balance it would violate was never generated.
#
# The gaps are computed once here, next to the closure that decides
# availability, and the activity domains subtract them.
# =========================================================================== #

#' @include map_closure.R
NULL

# Input requirements of one technology, split into commodities that are each
# required on their own and groups of which ANY ONE member suffices.
#
# A grouped input is a substitution set (co-firing, dual-fuel): the technology
# runs on whichever member is available, so requiring all of them would refuse
# models that work.
.tech_input_requirements <- function(scen) {
  one <- .gds(scen, "mTechOneComm")      # (tech, comm) -- ungrouped, any role
  inp <- .gds(scen, "mTechInpComm")      # (tech, comm) -- every input
  grp <- .gds(scen, "mTechGroupComm")    # (tech, group, comm)
  igr <- .gds(scen, "mTechInpGroup")     # (tech, group) -- groups typed input
  if (is.null(inp)) return(list(single = NULL, group = NULL))
  inp <- as.data.frame(inp)

  single <- if (is.null(one)) inp else
    dplyr::semi_join(inp, as.data.frame(one), by = c("tech", "comm"))

  group <- NULL
  if (!is.null(grp) && !is.null(igr)) {
    group <- dplyr::semi_join(as.data.frame(grp), as.data.frame(igr),
                              by = c("tech", "group"))
    if (nrow(group) == 0) group <- NULL
  }
  list(single = single, group = group)
}

# (process, class, region, missing) for every declared cell a process cannot
# operate in. `missing` names the commodities that are unavailable there.
.process_region_gaps <- function(scen) {
  cr <- .gds(scen, "mCommReg")
  if (is.null(cr) || nrow(cr) == 0) return(NULL)
  cr <- dplyr::distinct(as.data.frame(cr)[, c("comm", "region")])

  # `have`: the cells that DO work. A cell is missing when its span row has no
  # match here.
  gaps <- list()

  add_gap <- function(span, req, key, cls) {
    if (is.null(span) || is.null(req) || nrow(span) == 0 || nrow(req) == 0) {
      return(NULL)
    }
    span <- dplyr::distinct(as.data.frame(span)[, c(key, "region")])
    need <- dplyr::inner_join(span, as.data.frame(req), by = key,
                              relationship = "many-to-many")
    if (nrow(need) == 0) return(NULL)
    short <- dplyr::anti_join(need, cr, by = c("comm", "region"))
    if (nrow(short) == 0) return(NULL)
    short |>
      dplyr::summarise(missing = paste(sort(unique(.data$comm)), collapse = ", "),
                       .by = dplyr::all_of(c(key, "region"))) |>
      dplyr::transmute(process = .data[[key]], class = cls,
                       region = .data$region, missing = .data$missing)
  }

  # -- technology: every ungrouped input, plus every input group that has no
  #    available member at all.
  tspan <- .gds(scen, "mTechSpan")
  treq  <- .tech_input_requirements(scen)
  gaps[[length(gaps) + 1L]] <- add_gap(tspan, treq$single, "tech", "technology")

  if (!is.null(tspan) && !is.null(treq$group)) {
    span <- dplyr::distinct(as.data.frame(tspan)[, c("tech", "region")])
    g <- dplyr::inner_join(span, treq$group, by = "tech",
                           relationship = "many-to-many") |>
      dplyr::left_join(dplyr::mutate(cr, .ok = TRUE), by = c("comm", "region")) |>
      dplyr::summarise(any_ok = any(!is.na(.data$.ok)),
                       missing = paste(sort(unique(.data$comm)), collapse = " | "),
                       .by = dplyr::all_of(c("tech", "region", "group"))) |>
      dplyr::filter(!.data$any_ok)
    if (nrow(g) > 0) {
      gaps[[length(gaps) + 1L]] <- g |>
        dplyr::summarise(missing = paste0("group(", paste(.data$missing,
                                                          collapse = "); group("),
                                          ")"),
                         .by = dplyr::all_of(c("tech", "region"))) |>
        dplyr::transmute(process = .data$tech, class = "technology",
                         region = .data$region, missing = .data$missing)
    }
  }

  # -- storage: the input role only. A storage with no input commodity
  #    available cannot be charged, so it cannot operate.
  gaps[[length(gaps) + 1L]] <-
    add_gap(.gds(scen, "mStorageSpan"), .gds(scen, "mStorageInpComm"),
            "stg", "storage")

  out <- dplyr::bind_rows(gaps)
  if (is.null(out) || nrow(out) == 0) return(NULL)
  out <- out[order(out$class, out$process, out$region), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# Record the gaps on the scenario and report them once. Called from the closure
# recipe, after `mCommReg` exists; `map_mvTechAct()` / `.filter_storage_act()`
# subtract what it records.
.record_region_gaps <- function(scen) {
  g <- .process_region_gaps(scen)
  scen@misc$region_gaps <- g
  if (is.null(g)) return(scen)
  n <- nrow(g)
  head_n <- utils::head(g, 10)
  warning(
    "Dropped ", n, " process-region cell", if (n > 1) "s" else "",
    ": a required commodity is not available there.\n",
    "Without this the process would keep its activity and output while losing\n",
    "its input, and produce from nothing.\n   ",
    paste(utils::capture.output(print(head_n, row.names = FALSE)),
          collapse = "\n   "),
    if (n > nrow(head_n)) paste0("\n   ... and ", n - nrow(head_n), " more"),
    "\n\nSee `scenario@misc$region_gaps` for the full table. Declare the ",
    "missing supply/import/trade, or narrow the process `@region`.",
    call. = FALSE
  )
  scen
}

# The (key, region) pairs to subtract from an activity domain.
.region_gap_pairs <- function(scen, key, cls) {
  g <- scen@misc$region_gaps
  if (is.null(g) || nrow(g) == 0) return(NULL)
  g <- g[g$class == cls, , drop = FALSE]
  if (nrow(g) == 0) return(NULL)
  stats::setNames(data.frame(g$process, g$region, stringsAsFactors = FALSE),
                  c(key, "region"))
}

# Drop them from an activity-domain frame.
.drop_region_gaps <- function(scen, df, key, cls) {
  pairs <- .region_gap_pairs(scen, key, cls)
  if (is.null(pairs) || is.null(df) || nrow(df) == 0) return(df)
  as.data.frame(dplyr::anti_join(as.data.frame(df), pairs,
                                 by = c(key, "region")))
}

# --------------------------------------------------------------------------- #
# Declaration-level check (no interpolation)
# --------------------------------------------------------------------------- #

# Every element object of a model, flattened out of its repositories.
.model_elements <- function(mod) {
  out <- list()
  for (rep in mod@data) {
    d <- tryCatch(rep@data, error = function(e) NULL)
    if (is.null(d)) next
    out <- c(out, d)
  }
  out
}

# The regions an object is declared in, per R1: `@region` when populated,
# every model region otherwise. Trade scopes on its route endpoints instead.
.declared_regions_of <- function(obj, regions) {
  if (is(obj, "trade")) {
    r <- tryCatch(unique(c(as.character(obj@routes$src),
                           as.character(obj@routes$dst))),
                  error = function(e) character())
    r <- r[!is.na(r) & nzchar(r)]
    return(if (length(r) == 0) regions else intersect(r, regions))
  }
  r <- if (.hasSlot(obj, "region")) as.character(obj@region) else character()
  r <- r[!is.na(r) & nzchar(r)]
  if (length(r) == 0) regions else intersect(r, regions)
}

# Input / output commodities of one process, with input groups kept apart:
# a group is a substitution set, so any one available member satisfies it.
.declared_io <- function(obj) {
  single <- character(); groups <- list(); outs <- character()
  if (is(obj, "technology")) {
    ct <- tryCatch(checkInpOut(obj)$comm, error = function(e) NULL)
    gt <- tryCatch(checkInpOut(obj)$group, error = function(e) NULL)
    if (is.null(ct) || nrow(ct) == 0) return(list(single = single, groups = groups,
                                                  outs = outs))
    cm <- rownames(ct)
    is_in  <- !is.na(ct$type) & ct$type == "input"
    is_out <- !is.na(ct$type) & ct$type == "output"
    outs <- cm[is_out]
    single <- cm[is_in & is.na(ct$group)]
    gnames <- unique(ct$group[is_in & !is.na(ct$group)])
    for (g in gnames) groups[[g]] <- cm[is_in & !is.na(ct$group) & ct$group == g]
  } else if (is(obj, "storage")) {
    gi <- function(sl) {
      d <- tryCatch(methods::slot(obj, sl), error = function(e) NULL)
      if (is.data.frame(d) && "comm" %in% names(d)) as.character(d$comm) else
        as.character(obj@commodity)
    }
    single <- unique(gi("input")); outs <- unique(gi("output"))
  } else if (is(obj, "supply") || is(obj, "import")) {
    outs <- as.character(obj@commodity)
  } else if (is(obj, "trade")) {
    outs <- as.character(obj@commodity); single <- as.character(obj@commodity)
  }
  list(single = unique(single[nzchar(single)]), groups = groups,
       outs = unique(outs[nzchar(outs)]))
}

#' Processes declared where a commodity they need is unavailable
#'
#' @description
#' Reports `(process, region)` cells a model declares but cannot support,
#' because a commodity the process consumes is produced nowhere in that region
#' and cannot be traded in. Works on the DECLARATIONS alone -- no interpolation
#' and no solver -- so it can be run on a model while it is being built.
#'
#' @details
#' Availability is grown to a fixed point: a commodity is available in a region
#' if a supply or import declares it there, if a process whose own inputs are
#' available there produces it, or if a trade route reaches that region from one
#' where it is available. A process is then short in a region when an ungrouped
#' input is unavailable, or when no member of one of its input groups is.
#'
#' `interpolate_model()` performs the same check on the interpolated model and
#' drops the cells it finds, recording them in `scenario@misc$region_gaps`.
#' This function is the declaration-time counterpart: it answers the same
#' question earlier and without the pipeline.
#'
#' @param mod a model object.
#'
#' @return a data.frame with columns `process`, `class`, `region` and `missing`,
#'   or `NULL` when every declared cell is supportable.
#'
#' Internal until a report template consumes it: templates call the package
#' through `energyRt::`, so the export goes in with the wiring, and the return
#' columns are still free to change until then.
#' @noRd
model_region_gaps <- function(mod) {
  stopifnot(is(mod, "model"))
  regions <- as.character(get_region(mod))
  regions <- regions[!is.na(regions) & nzchar(regions)]
  objs <- .model_elements(mod)
  if (length(regions) == 0 || length(objs) == 0) return(NULL)

  info <- list()
  for (o in objs) {
    if (!.hasSlot(o, "name") || is(o, "commodity")) next
    io <- .declared_io(o)
    if (length(io$single) == 0 && length(io$groups) == 0 &&
        length(io$outs) == 0) next
    info[[length(info) + 1L]] <- list(
      name = o@name, cls = class(o)[1],
      regions = .declared_regions_of(o, regions), io = io,
      routes = if (is(o, "trade")) tryCatch(o@routes, error = function(e) NULL)
    )
  }
  if (length(info) == 0) return(NULL)

  key <- function(cm, rg) paste(cm, rg, sep = "\r")
  avail <- character(0)
  for (i in info) {
    if (i$cls %in% c("supply", "import")) {
      avail <- c(avail, key(rep(i$io$outs, each = length(i$regions)),
                            rep(i$regions, times = length(i$io$outs))))
    }
  }
  avail <- unique(avail)

  can_run <- function(i, rg) {
    if (length(i$io$single) > 0 &&
        !all(key(i$io$single, rg) %in% avail)) return(FALSE)
    for (g in i$io$groups) if (!any(key(g, rg) %in% avail)) return(FALSE)
    TRUE
  }

  repeat {
    n0 <- length(avail)
    for (i in info) {
      if (i$cls == "trade") {
        rt <- i$routes
        if (is.null(rt) || nrow(rt) == 0) next
        for (k in seq_len(nrow(rt))) {
          if (key(i$io$outs, rt$src[k]) %in% avail) {
            avail <- unique(c(avail, key(i$io$outs, rt$dst[k])))
          }
        }
        next
      }
      if (length(i$io$outs) == 0) next
      for (rg in i$regions) {
        if (can_run(i, rg)) avail <- unique(c(avail, key(i$io$outs, rg)))
      }
    }
    if (length(avail) == n0) break
  }

  rows <- list()
  for (i in info) {
    if (i$cls %in% c("supply", "import", "export", "demand", "trade")) next
    for (rg in i$regions) {
      if (can_run(i, rg)) next
      miss <- i$io$single[!key(i$io$single, rg) %in% avail]
      for (g in names(i$io$groups)) {
        gm <- i$io$groups[[g]]
        if (!any(key(gm, rg) %in% avail)) {
          miss <- c(miss, paste0("group(", paste(gm, collapse = " | "), ")"))
        }
      }
      rows[[length(rows) + 1L]] <- data.frame(
        process = i$name, class = i$cls, region = rg,
        missing = paste(miss, collapse = ", "), stringsAsFactors = FALSE)
    }
  }
  if (length(rows) == 0) return(NULL)
  out <- do.call(rbind, rows)
  out <- out[order(out$class, out$process, out$region), , drop = FALSE]
  rownames(out) <- NULL
  out
}
