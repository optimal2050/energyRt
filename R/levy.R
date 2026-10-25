# =========================================================================== #
# Levy targeting: resolving a tax or subsidy to the processes it charges.
#
# A levy reaches the LP by one of two routes, chosen per rate column:
#
#   commodity route  `inp`/`out`/`bal` with NO process targeting, charged on the
#                    commodity totals vInpTot/vOutTot/vBalance via
#                    pTaxCost{Inp,Out,Bal} -- every process at once.
#   process route    `act`/`cap`/`ncap` always, and `inp`/`out` when the levy
#                    carries process targeting, charged on the process's own
#                    variables via pTaxProc{Act,Inp,Out,Cap,Ncap}.
#
# The routes are disjoint by construction: `inp`/`out` go to exactly one of
# them, never both, or one unit of flow would be charged twice.
#
# Resolution runs inside `ob2mi`, which interpolation reaches long after
# `expand_variants()`, so the roster holds REALIZED names (`ECOA_VIN2030_CL1`).
# `process`/`process_` therefore match the base name as well, and `vintage` /
# `cluster` join `modInp@sets$variant` rather than parsing the mangled name --
# the separator is user-configurable (`.variant_prefix_default()`).
# =========================================================================== #

# Process classes a levy can charge. Slice 1: technology and storage, the two
# classes whose activity/flow/capacity variables all exist on every backend.
.levy_classes <- c("technology", "storage")

# Rate columns, by route. `bal` belongs to the commodity alone: a balance is
# production minus consumption across every process, so no single process owns
# it and it cannot be combined with process targeting.
.levy_comm_cols <- c("inp", "out", "bal")
.levy_proc_cols <- c("act", "inp", "out", "cap", "ncap")
# Columns that force the process route regardless of targeting: no commodity
# equivalent exists for them.
.levy_proc_only_cols <- c("act", "cap", "ncap")

# The targeting slots, in the order errors report them.
.levy_filter_slots <- c("process", "process_", "classes",
                        "vintage", "vintage_", "cluster", "cluster_")

# The subset `.target_filter()` resolves; `classes` is applied by each caller
# when it builds the pool.
.levy_target_slots <- c("process", "process_",
                        "vintage", "vintage_", "cluster", "cluster_")

# parameter per (base, route), keyed by the object's class
.levy_proc_params <- list(
  tax     = c(act = "pTaxProcAct", inp = "pTaxProcInp", out = "pTaxProcOut",
              cap = "pTaxProcCap", ncap = "pTaxProcNcap"),
  subsidy = c(act = "pSubProcAct", inp = "pSubProcInp", out = "pSubProcOut",
              cap = "pSubProcCap", ncap = "pSubProcNcap")
)
.levy_comm_params <- list(
  tax     = c(inp = "pTaxCostInp", out = "pTaxCostOut", bal = "pTaxCostBal"),
  subsidy = c(inp = "pSubCostInp", out = "pSubCostOut", bal = "pSubCostBal")
)

# Read a targeting slot from a levy that may predate it.
#
# An object deserialised from a model, scenario or dataset saved before the
# targeting slots existed carries only the original seven. `slotNames()` answers
# from the CURRENT class definition, so it reports all fifteen and gives no
# warning, while `slot()` on a stored-absent name errors outright. Such an
# object has no process targeting by construction, so an absent slot reads as
# empty and the levy takes the commodity route exactly as it did before.
.levy_slot <- function(obj, name) {
  if (!methods::.hasSlot(obj, name)) return(character())
  v <- slot(obj, name)
  if (is.null(v)) character() else v
}

#' Does this levy name a process target?
#' @keywords internal
#' @noRd
.levy_has_target <- function(obj) {
  any(vapply(.levy_filter_slots,
             function(s) {
               v <- .levy_slot(obj, s)
               length(v) > 0 && any(nzchar(v))
             },
             logical(1)))
}

# =========================================================================== #
# Shared process targeting. Used by levies (below) and by constraint summands
# (R/class-constraint.R). Both resolve AFTER `expand_variants()`, so the pool
# holds REALIZED names (`ECOA_VIN2030_CL1`) -- which is why `process` matches
# the base name as well, and why `vintage`/`cluster` join the provenance table
# instead of parsing the mangled name (the separator is user-configurable).
# =========================================================================== #

# Members of the named process classes, for the `classes` filter. Each caller
# applies this to its own pool: a levy pools `sets$tech`/`sets$stg`, a
# constraint pools the variable's own domain.
#' @noRd
.class_set_key <- c(technology = "tech", storage = "stg", supply = "sup",
                    demand = "dem", trade = "trade", import = "imp",
                    export = "expp")

.class_members <- function(sets, classes, label = "classes") {
  classes <- classes[!is.na(classes) & nzchar(classes)]
  if (length(classes) == 0) return(NULL)
  k <- unname(.class_set_key[classes])
  # `.class_set_key[["nope"]]` errors rather than returning NULL, so an unknown
  # class must be caught here or it surfaces as "subscript out of bounds".
  if (anyNA(k)) {
    stop(label, ": unknown class ",
         paste0('"', classes[is.na(k)], '"', collapse = ", "),
         ". Available: ", paste(names(.class_set_key), collapse = ", "),
         call. = FALSE)
  }
  out <- unlist(lapply(k, function(x) as.character(sets[[x]])), use.names = FALSE)
  unique(out[!is.na(out) & nzchar(out)])
}

# A target that resolves to nothing is an error, never a silent zero: the rate
# or coefficient was written down to be applied, and quietly ignoring it
# reports a policy that was never imposed.
#' @noRd
.stop_target_filter <- function(label, which, asked, available) {
  asked <- unique(as.character(asked[!is.na(asked)]))
  avail <- unique(as.character(available))
  stop(label, ": `", which, "` = ",
       paste0('"', asked, '"', collapse = ", "),
       " selects no process, so it would be silently ignored.",
       if (length(avail)) paste0(
         " Available: ", paste0('"', utils::head(avail, 8), '"', collapse = ", "),
         if (length(avail) > 8) ", ..." else "") else "",
       call. = FALSE)
}

#' Filter a pool of realized process names by the targeting filters
#'
#' @param pool character, realized process names to filter.
#' @param prov `modInp@sets$variant`: data.frame(name, class, base, vintage,
#'   cluster), or NULL when no process has variants.
#' @param f named list with any of `process`, `process_`, `vintage`,
#'   `vintage_`, `cluster`, `cluster_`; absent or empty entries do not filter.
#' @param label how to name the owner in an error, e.g. `"levy 'CO2TAX'"`.
#' @return the kept subset of `pool`, never empty (errors instead).
#' @keywords internal
#' @noRd
.target_filter <- function(pool, prov, f, label) {
  pool <- unique(as.character(pool))
  pool <- pool[nzchar(pool)]
  if (length(pool) == 0) return(pool)
  get_f <- function(n) {
    v <- f[[n]]
    if (is.null(v)) return(character())
    v <- as.character(v)
    v[!is.na(v) & nzchar(v)]
  }

  # base / vintage / cluster per pooled name; a process with no variants is its
  # own base and carries neither
  base <- pool
  vin <- rep(NA_character_, length(pool))
  clu <- rep(NA_character_, length(pool))
  if (!is.null(prov) && nrow(prov) > 0) {
    i <- match(pool, prov$name)
    hit <- !is.na(i)
    base[hit] <- as.character(prov$base[i[hit]])
    vin[hit] <- as.character(prov$vintage[i[hit]])
    clu[hit] <- as.character(prov$cluster[i[hit]])
  }

  keep <- rep(TRUE, length(pool))
  # `process` matches the realized OR the base name, so naming a vintaged
  # process selects all of its variants -- what a user means by "tax ECOA".
  ask <- get_f("process")
  if (length(ask) > 0) {
    k <- pool %in% ask | base %in% ask
    if (!any(k)) .stop_target_filter(label, "process", ask, unique(c(pool, base)))
    keep <- keep & k
  }
  ask <- get_f("process_")
  if (length(ask) > 0) {
    rx <- function(v) Reduce(`|`, lapply(ask, function(p) grepl(p, v)))
    k <- rx(pool) | rx(base)
    if (!any(k)) .stop_target_filter(label, "process_", ask, unique(c(pool, base)))
    keep <- keep & k
  }
  for (sl in c("vintage", "cluster")) {
    ask <- get_f(sl)
    if (length(ask) == 0) next
    v <- if (sl == "vintage") vin else clu
    k <- !is.na(v) & v %in% ask
    if (!any(k)) .stop_target_filter(label, sl, ask, stats::na.omit(unique(v)))
    keep <- keep & k
  }
  for (sl in c("vintage_", "cluster_")) {
    ask <- get_f(sl)
    if (length(ask) == 0) next
    v <- if (sl == "vintage_") vin else clu
    k <- !is.na(v) &
      Reduce(`|`, lapply(ask, function(p) grepl(p, ifelse(is.na(v), "", v))))
    if (!any(k)) .stop_target_filter(label, sl, ask, stats::na.omit(unique(v)))
    keep <- keep & k
  }

  out <- pool[keep]
  if (length(out) == 0) {
    set <- names(f)[vapply(names(f), function(n) length(get_f(n)) > 0, logical(1))]
    stop(label, ": the combination of ",
         paste0("`", set, "`", collapse = " + "),
         " selects no process, so it would be silently ignored.", call. = FALSE)
  }
  out
}

# A levy that resolves to nothing is an error, never a silent zero: the rate was
# written down to be charged, and a model that quietly ignores it reports a
# policy it did not apply.
.stop_levy_filter <- function(obj, which, asked, available) {
  asked <- unique(as.character(asked[!is.na(asked)]))
  avail <- unique(as.character(available))
  stop("levy '", obj@name, "': `", which, "` = ",
       paste0('"', asked, '"', collapse = ", "),
       " selects no process, so the rate would be silently zero.",
       if (length(avail)) paste0(
         " Available: ", paste0('"', utils::head(avail, 8), '"', collapse = ", "),
         if (length(avail) > 8) ", ..." else "") else "",
       call. = FALSE)
}

#' Realized process names a levy charges
#'
#' @param scen interpolated-in-progress scenario; `@sets$tech`, `@sets$stg` and
#'   `@sets$variant` must already be populated (they are, well before `ob2mi`).
#' @param obj a `tax` or `subsidy`.
#' @return character vector of realized process names, never empty.
#' @keywords internal
#' @noRd
.levy_roster <- function(scen, obj) {
  sets <- scen@modInp@sets
  want <- .levy_slot(obj, "classes")
  want <- want[nzchar(want)]
  if (length(want) == 0) want <- .levy_classes
  bad <- setdiff(want, .levy_classes)
  if (length(bad) > 0) {
    stop("levy '", obj@name, "': `classes` = ",
         paste0('"', bad, '"', collapse = ", "),
         " is not supported yet. A levy can charge ",
         paste0('"', .levy_classes, '"', collapse = " or "),
         "; supply, demand, trade, import and export bases are not implemented.",
         call. = FALSE)
  }

  pool <- .class_members(sets, want)
  if (length(pool) == 0) {
    stop("levy '", obj@name, "': the model has no ",
         paste(want, collapse = " or "), " to charge.", call. = FALSE)
  }

  .target_filter(
    pool, sets$variant,
    f = stats::setNames(lapply(.levy_target_slots, function(s) .levy_slot(obj, s)),
                        .levy_target_slots),
    label = paste0("levy '", obj@name, "'"))
}

#' Refuse the rate/target combinations that have no meaning
#' @keywords internal
#' @noRd
.levy_validate <- function(obj, slot_name) {
  rt <- as.data.frame(slot(obj, slot_name))
  cols <- names(rt)
  rate_cols <- intersect(cols, c(.levy_comm_cols, "act", "cap", "ncap", "rate"))
  # a column present but all-NA was never set by the user
  set_cols <- rate_cols[vapply(rate_cols,
                               function(c) any(!is.na(rt[[c]])), logical(1))]
  has_target <- .levy_has_target(obj)

  .vbl <- .levy_slot(obj, "variable")
  if (length(.vbl) > 0 && any(nzchar(.vbl))) {
    stop("levy '", obj@name, "': `variable` (the model-variable escape hatch) ",
         "is not implemented yet. Use the `act`/`inp`/`out`/`cap`/`ncap` ",
         "columns of `", slot_name, "`.", call. = FALSE)
  }
  if ("rate" %in% set_cols) {
    stop("levy '", obj@name, "': the `rate` column is only used with ",
         "`variable`, which is not implemented yet.", call. = FALSE)
  }
  if ("bal" %in% set_cols && has_target) {
    stop("levy '", obj@name, "': `bal` cannot be combined with process ",
         "targeting. A commodity balance is production minus consumption ",
         "across every process, so no single process owns it -- drop the ",
         "process filter, or levy `inp`/`out` instead.", call. = FALSE)
  }
  if ("act" %in% set_cols &&
      identical(sort(unique(.levy_slot(obj, "classes")[
        nzchar(.levy_slot(obj, "classes"))])), "storage")) {
    stop("levy '", obj@name, "': `act` is not defined for storage, which has ",
         "no activity variable. Levy `inp`/`out` instead.", call. = FALSE)
  }
  if (length(set_cols) == 0) {
    stop("levy '", obj@name, "': `", slot_name, "` sets no rate. Supply one of ",
         paste0("`", c(.levy_comm_cols, "act", "cap", "ncap"), "`",
                collapse = ", "), ".", call. = FALSE)
  }
  set_cols
}

# `make_data_param()` reads only $name, $colName and $type from the metadata;
# every dimension comes from the parameter's own @dimSets. The process
# parameters deliberately carry no class/slot/colName in the registry, so
# `get_slot_meta()` cannot feed the shared `inp`/`out` columns to both routes.
.levy_par_meta <- function(par_name, col) {
  list(name = par_name, colName = col, type = "numpar")
}

#' Emit a levy's parameters, routing each rate column to exactly one parameter
#'
#' @param scen scenario under interpolation.
#' @param obj a `tax` or `subsidy`.
#' @param slot_name the rate-table slot (`"tax"` / `"subsidy"`).
#' @keywords internal
#' @noRd
.levy_emit <- function(scen, obj, slot_name) {
  cls <- class(obj)[1]
  set_cols <- .levy_validate(obj, slot_name)
  has_target <- .levy_has_target(obj)

  rt <- as.data.frame(get_lazy_data(obj, slot_name))
  # The commodity is carried on the object (@comm), not in the rate table; an
  # explicit `comm` column there wins (per-commodity rates on one instrument).
  if (!("comm" %in% names(rt)) || all(is.na(rt$comm))) {
    cm <- obj@comm[nzchar(obj@comm)]
    rt$comm <- if (length(cm) == 1) cm else NA_character_
    if (length(cm) > 1) rt <- tidyr::crossing(rt[setdiff(names(rt), "comm")],
                                              comm = cm)
  }
  # An empty @region means every region: kept NA here and expanded when the
  # mTaxCost/mSubCost map is built.
  if (length(obj@region) > 0) {
    rt <- rt |> select(-any_of("region")) |> tidyr::crossing(region = obj@region)
  }

  # which columns take which route
  proc_cols <- intersect(set_cols,
                         if (has_target) .levy_proc_cols else .levy_proc_only_cols)
  comm_cols <- setdiff(intersect(set_cols, .levy_comm_cols), proc_cols)

  if (length(proc_cols) > 0) {
    roster <- .levy_roster(scen, obj)
    # rows naming a process keep it; rows that do not are charged on the whole
    # roster. Expanding explicitly matters: an absent `process` column would
    # become an NA wildcard, i.e. EVERY process, including the supply and trade
    # classes that have no such variables.
    if ("process" %in% names(rt) && any(!is.na(rt$process))) {
      named <- unique(stats::na.omit(rt$process))
      unknown <- setdiff(named, roster)
      if (length(unknown) > 0) {
        .stop_levy_filter(obj, paste0(slot_name, "$process"), unknown, roster)
      }
      pr <- rt[!is.na(rt$process), , drop = FALSE]
      rest <- rt[is.na(rt$process), setdiff(names(rt), "process"), drop = FALSE]
      rt_proc <- dplyr::bind_rows(
        pr,
        if (nrow(rest) > 0) tidyr::crossing(rest, process = roster) else NULL)
    } else {
      rt_proc <- tidyr::crossing(
        rt[setdiff(names(rt), "process")], process = roster)
    }
    rt_proc <- .levy_place(rt_proc, .levy_proc_regions(scen, unique(rt_proc$process)),
                           "process", obj)
    for (col in proc_cols) {
      par_name <- .levy_proc_params[[cls]][[col]]
      dat <- make_data_param(
        scen = scen, obj_name = obj@name, slot_data = rt_proc,
        par_meta = .levy_par_meta(par_name, col), class_col = "levy")
      scen <- update_parameter(scen, par_name, dat)
    }
  }

  if (length(comm_cols) > 0) {
    own <- .comm_region_df(scen)          # NULL without a geoscale
    if (!is.null(own) && nrow(own) > 0) rt <- .levy_place(rt, own, "comm", obj)
    for (col in comm_cols) {
      par_name <- .levy_comm_params[[cls]][[col]]
      dat <- make_data_param(
        scen = scen, obj_name = obj@name, slot_data = rt,
        par_meta = .levy_par_meta(par_name, col), class_col = "levy")
      scen <- update_parameter(scen, par_name, dat)
    }
  }

  scen
}

# The regions a process is declared in, one row per (process, region).
.levy_proc_regions <- function(scen, processes) {
  pr <- get_process_region(scen, process = processes, return_list = FALSE)
  as.data.frame(pr)[, c("process", "region")]
}

# A levy charges a flow once, at the level the flow is defined at: a commodity
# rate at the commodity's own regions (`mCommRegion`), a process rate at the
# regions the process is declared in. A coarser region holds the roll-up of its
# sub-regions, which a rate there would charge a second time.
#
# Rows without a region take every region of their key's level; a named region
# off that level is an error.
#' @param rt rate rows.
#' @param own `data.frame(<key>, region)`: the level of each key.
#' @param key `"comm"` or `"process"`.
#' @keywords internal
#' @noRd
.levy_place <- function(rt, own, key, obj) {
  rt <- as.data.frame(rt)
  own <- unique(as.data.frame(own)[, c(key, "region")])
  if (!"region" %in% names(rt)) rt$region <- NA_character_
  named <- rt[!is.na(rt$region), , drop = FALSE]
  bad <- unique(named[!paste(named[[key]], named$region) %in%
                        paste(own[[key]], own$region), c(key, "region")])
  if (nrow(bad) > 0) {
    lvl <- own$region[own[[key]] == bad[[key]][1]]
    stop("levy '", obj@name, "': ", key, " '", bad[[key]][1], "' is ",
         if (key == "comm") "balanced" else "declared", " at ",
         paste0("'", utils::head(lvl, 6), "'", collapse = ", "),
         if (length(lvl) > 6) ", ..." else "", "; region '", bad$region[1],
         "' is not one of them. A coarser region holds the sum of its ",
         "sub-regions, which would be charged twice; a region with no such ",
         "flow would charge nothing.", call. = FALSE)
  }
  open <- rt[is.na(rt$region), setdiff(names(rt), "region"), drop = FALSE]
  placed <- if (nrow(open) > 0) {
    dplyr::inner_join(open, own, by = key, relationship = "many-to-many")
  } else {
    NULL
  }
  dplyr::bind_rows(named, placed)
}
