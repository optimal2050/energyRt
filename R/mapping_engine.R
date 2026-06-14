# =========================================================================== #
# mapping_engine.R
#
# Spec-driven creation of the model's mapping parameters (`m*` / `meq*`).
#
# The mapping catalogue lives in `.mapping_spec` (built from
# `data-raw/mapping_spec.yml` and baked into `R/sysdata.rda`). Every mapping is
# tagged with a `recipe` that determines HOW it is built and, by construction,
# WHEN it is built. `build_mappings()` evaluates the recipes in dependency
# order so each recipe's inputs already exist:
#
#   1 membership  - object-slot -> 2-column membership map
#   2 calendar    - calendar / horizon structure (model-independent)
#   closure       - mCommReg commodity<->region reachability fixpoint
#   3 lifespan    - new / span / eac investment windows (.process_lifespan)
#   4 value       - value-derived maps (where a parameter has a value)
#   5 filter      - activity / flow / aggregation / aux-conversion domains
#   6 constraint  - meq* and bound m* from user constraint objects
#   7 cost_agg    - top-level cost aggregations
#
# Used-set members in a dimension `d` of a mapping `m` are, by definition,
# `unique(m[[d]])` - the projection of the mapping's tuples onto that column.
# =========================================================================== #

`%||%` <- function(a, b) if (is.null(a)) b else a

# Recipe evaluation order (recipe == build tier).
.mapping_recipe_order <- c(
  "membership", "calendar", "closure",
  "lifespan", "value", "filter", "constraint", "cost_agg"
)

#' Load the mapping specification
#'
#' @returns named list of mapping specs (see `data-raw/mapping_spec.yml`).
#' @export
load_mapping_spec <- function() {
  spec <- get0(".mapping_spec", envir = asNamespace("energyRt"),
               ifnotfound = NULL)
  if (is.null(spec)) {
    stop(
      "`.mapping_spec` not found. Rebuild package data with ",
      "`source('data-raw/DATASET.R')` after generating ",
      "`data-raw/mapping_spec.yml`."
    )
  }
  spec
}

#' Group mapping specs by recipe, in evaluation order
#'
#' @param spec mapping specification (defaults to `load_mapping_spec()`).
#' @returns named list keyed by recipe, each a character vector of mapping
#'   names, ordered per `.mapping_recipe_order`.
#' @export
mappings_by_recipe <- function(spec = load_mapping_spec()) {
  recipes <- vapply(spec, function(x) x$recipe %||% NA_character_, character(1))
  out <- lapply(.mapping_recipe_order, function(rc) names(recipes)[recipes == rc])
  names(out) <- .mapping_recipe_order
  # Carry along any recipe not in the canonical order (e.g. UNCLASSIFIED).
  extra <- setdiff(unique(recipes), c(.mapping_recipe_order, NA))
  for (rc in extra) out[[rc]] <- names(recipes)[recipes == rc]
  out
}

# --------------------------------------------------------------------------- #
# Recipe 1: membership maps (object slot -> 2-column map)
# --------------------------------------------------------------------------- #

# `recipe_membership` + `.membership_map_def` + `.build_membership_map` (the
# never-wired engine membership impl) ARCHIVED to drafts/legacy-mapping/membership.R.
# The 12 core membership maps are built by R/map_membership.R (registry, reading
# the `*_comm` sets populated in interp_mod); this no-op fallback covers the ~18
# specialised membership-tagged maps (groups / agg / same-slice / emission-fuel /
# weather-region) which are built later in the filter recipe.
recipe_membership <- function(scen, names, fmp) scen

# --------------------------------------------------------------------------- #
# Recipe "closure": mCommReg commodity<->region reachability fixpoint
# --------------------------------------------------------------------------- #

# Trade edges as a (comm, src, dst) table: each traded commodity can be moved
# from its source region(s) to its destination region(s).
.trade_edges <- function(scen) {
  lst <- apply_to_scenario_data(
    scen = scen, classes = "trade", as_list = TRUE,
    func = function(x) {
      rt <- x@routes
      comms <- as.character(x@commodity)
      if (is.null(rt) || nrow(rt) == 0 || length(comms) == 0) {
        return(list())
      }
      out <- list()
      out[[x@name]] <- tidyr::crossing(comm = comms) |>
        dplyr::cross_join(dplyr::select(rt, src, dst))
      out
    }
  )
  if (length(lst) == 0) {
    return(dplyr::tibble(
      comm = character(), src = character(), dst = character()
    ))
  }
  dplyr::bind_rows(lst) |>
    dplyr::filter(!is.na(src), !is.na(dst)) |>
    dplyr::distinct()
}

# Compute the (comm, region) availability set by fixpoint iteration. A commodity
# is available in a region if it is a primary (supply/import) output there, an
# output of a process whose inputs are all available there, or reachable by
# trade from a region where it is available.
.commreg_closure <- function(scen) {
  preg <- get_process_region(scen, return_list = FALSE) |>
    dplyr::as_tibble()
  proc_reg <- split(preg$region, preg$process)

  prod_classes <- c("technology", "storage", "process")
  ins <- named_list_to_df(
    get_process_inputs(scen, classes = prod_classes),
    col_names = c("process", "comm")
  ) |> dplyr::as_tibble()
  outs <- named_list_to_df(
    get_process_outputs(scen, classes = prod_classes),
    col_names = c("process", "comm")
  ) |> dplyr::as_tibble()
  proc_in  <- split(ins$comm,  ins$process)
  proc_out <- split(outs$comm, outs$process)

  # Primary availability: supply + import outputs in their regions.
  primary <- dplyr::bind_rows(
    named_list_to_df(get_process_outputs(scen, classes = "supply"),
                     col_names = c("process", "comm")),
    named_list_to_df(get_process_outputs(scen, classes = "import"),
                     col_names = c("process", "comm"))
  ) |> dplyr::as_tibble()
  if (nrow(primary) > 0) {
    avail <- primary |>
      dplyr::inner_join(preg, by = "process") |>
      dplyr::select(comm, region) |>
      dplyr::distinct()
  } else {
    avail <- dplyr::tibble(comm = character(), region = character())
  }

  edges <- .trade_edges(scen)

  # Propagate a process's outputs into any region where all of its inputs are
  # already available. Returns the augmented availability table.
  propagate_processes <- function(avail, proc_map) {
    new_rows <- lapply(names(proc_map), function(p) {
      regs <- proc_reg[[p]]
      out_c <- proc_map[[p]]
      if (is.null(regs) || length(out_c) == 0) return(NULL)
      in_c <- proc_in[[p]]
      active_regs <- Filter(function(r) {
        length(in_c) == 0 ||
          all(in_c %in% dplyr::filter(avail, region == r)$comm)
      }, regs)
      if (length(active_regs) == 0) return(NULL)
      tidyr::crossing(comm = out_c, region = active_regs)
    })
    dplyr::bind_rows(avail, new_rows) |> dplyr::distinct()
  }

  # Fixpoint over availability (trade back-edge + process propagation).
  repeat {
    n_before <- nrow(avail)

    if (nrow(edges) > 0 && nrow(avail) > 0) {
      prop <- edges |>
        dplyr::inner_join(avail, by = c("comm", "src" = "region")) |>
        dplyr::transmute(comm, region = dst)
      avail <- dplyr::bind_rows(avail, prop) |> dplyr::distinct()
    }

    avail <- propagate_processes(avail, proc_out)

    if (nrow(avail) == n_before) break
  }

  # Auxiliary commodities: available where the owning process is active
  # (all of its main inputs are available in the region).
  aux <- named_list_to_df(
    get_process_aux(scen, classes = c(prod_classes, "trade")),
    col_names = c("process", "comm")
  ) |> dplyr::as_tibble()
  if (nrow(aux) > 0) {
    avail <- propagate_processes(avail, split(aux$comm, aux$process))
  }

  # Emission commodities: available wherever their parent commodity is.
  emiss_comm <- apply_to_scenario_data(
    scen = scen, classes = "commodity",
    func = function(x) {
      ll <- list()
      ll[[x@name]] <- x@emis$comm
      ll
    }
  ) |>
    named_list_to_df(col_names = c("comm", "emission")) |>
    dplyr::as_tibble()

  if (nrow(emiss_comm) > 0) {
    emiss_region <- emiss_comm |>
      dplyr::inner_join(avail, by = "comm") |>
      dplyr::transmute(comm = emission, region) |>
      dplyr::distinct()
    avail <- dplyr::bind_rows(avail, emiss_region) |> dplyr::distinct()
  }

  # Demand commodities: must be available in the demanding region.
  dem <- named_list_to_df(
    get_process_inputs(scen, classes = "demand"),
    col_names = c("process", "comm")
  ) |> dplyr::as_tibble()
  if (nrow(dem) > 0) {
    demand_comm_region <- dem |>
      dplyr::inner_join(preg, by = "process") |>
      dplyr::select(comm, region) |>
      dplyr::distinct()
    missing_dem <- demand_comm_region |>
      dplyr::anti_join(avail, by = c("comm", "region"))
    if (nrow(missing_dem) > 0) {
      stop(
        "There is no supply, production, interregional trade, or import for ",
        "demand-commodities in regions:\n   ",
        paste(utils::capture.output(print(as.data.frame(missing_dem))),
              collapse = "\n   "),
        "\nThe model will be infeasible.\n"
      )
    }
    avail <- dplyr::bind_rows(avail, demand_comm_region) |> dplyr::distinct()
  }

  avail |> dplyr::arrange(comm, region)
}

#' Build the mCommReg closure mapping (recipe "closure")
#'
#' @param scen scenario object.
#' @param names character vector of closure mapping names (expects `mCommReg`).
#' @param fmp function mapping a parameter name to its on-disk path.
#' @returns updated scenario object.
#' @keywords internal
recipe_closure <- function(scen, names, fmp) {
  if (!"mCommReg" %in% names) return(scen)

  comm_region <- .commreg_closure(scen)

  # Validate that every commodity is declared (when the comm set is known).
  declared <- scen@modInp@sets$comm
  if (!is.null(declared)) {
    undeclared <- comm_region |> dplyr::filter(!(comm %in% declared))
    if (nrow(undeclared) > 0) {
      stop(
        "The following commodities are not declared in the model:\n   ",
        paste(utils::capture.output(print(as.data.frame(undeclared))),
              collapse = "\n   "),
        "\nUse `newCommodity()` to create commodity objects to add to the ",
        "model.\n"
      )
    }
    orphan <- declared[!(declared %in% unique(comm_region$comm))]
    if (length(orphan) > 0) {
      warning(
        "The following commodities are not associated with any process:\n   ",
        paste(orphan, collapse = ", "),
        "\nand will be ignored.\n"
      )
    }
  }

  scen@modInp@sets$comm_region <-
    split(comm_region$region, comm_region$comm)
  scen@modInp@parameters$mCommReg <-
    d2p(scen@modInp@parameters$mCommReg, as.data.frame(comm_region),
        fmp("mCommReg"))
  scen
}

# --------------------------------------------------------------------------- #
# Recipe "calendar": model-independent calendar / horizon structure maps
# --------------------------------------------------------------------------- #

# Global calendar maps derived purely from `scen@settings` (calendar + horizon +
# region), plus the per-object / per-commodity slice maps (mCommSlice,
# mTechSlice, mSupSlice, mTradeSlice, mImpSlice, mExpSlice) which assign to each
# object the leaf slices of its own (finest) timeframe level, and the
# commodity slice-or-parent aggregation map (mCommSliceOrParent). The remaining
# slice maps (mWeatherSlice, mStorageFullYear, mTechFullYear) depend on
# interpolation data and are deferred.
.set_calendar_map <- function(scen, name, df, fmp) {
  scen@modInp@parameters[[name]] <-
    d2p(scen@modInp@parameters[[name]], as.data.frame(df), fmp(name))
  scen
}

# Slices of each commodity = the leaf slices of the commodity's own timeframe
# level (`cal@timeframes[[timeframe]]`).
.comm_slice_df <- function(scen) {
  cal    <- scen@settings@calendar
  frames <- cal@timeframes            # named list: timeframe -> slices
  ctf    <- map_comm_timeframe(scen)  # named list: comm -> timeframe
  rows <- lapply(names(ctf), function(cm) {
    tf <- ctf[[cm]]
    if (length(tf) == 0 || is.na(tf) || tf == "") tf <- cal@default_timeframe
    sl <- frames[[tf]]
    if (length(sl) == 0) sl <- tf
    data.frame(comm = cm, slice = as.character(sl), stringsAsFactors = FALSE)
  })
  dplyr::bind_rows(rows)
}

# Slices of each process = the leaf slices of its operating (finest) timeframe.
.process_slice_df <- function(scen) {
  cal    <- scen@settings@calendar
  frames <- cal@timeframes
  ptf    <- get_process_timeframe(scen)  # named list: process -> timeframe
  rows <- lapply(names(ptf), function(p) {
    tf <- ptf[[p]]
    if (length(tf) == 0 || is.na(tf) || tf == "") tf <- cal@default_timeframe
    sl <- frames[[tf]]
    if (length(sl) == 0) sl <- tf
    data.frame(process = p, slice = as.character(sl), stringsAsFactors = FALSE)
  })
  dplyr::bind_rows(rows)
}

# Project the process->slice table onto a single object class, renaming the
# `process` column to the map's key (e.g. `tech`, `sup`).
.proc_slice_for <- function(proc_slice, pclass, cls, key) {
  if (is.null(proc_slice) || is.null(pclass) || nrow(proc_slice) == 0) {
    return(NULL)
  }
  procs <- names(pclass)[vapply(pclass,
    function(x) identical(as.character(x)[1], cls), logical(1))]
  df <- proc_slice[proc_slice$process %in% procs, , drop = FALSE]
  if (nrow(df) == 0) return(NULL)
  names(df)[names(df) == "process"] <- key
  df
}

# `recipe_calendar` superseded by R/map_calendar.R (registry) and ARCHIVED to
# drafts/legacy-mapping/calendar.R. Retained as a no-op fallback for the three
# calendar-tagged maps (mWeatherSlice, mStorageFullYear, mTechFullYear) that are
# built in later recipes; the calendar slice helpers above remain live.
recipe_calendar <- function(scen, names, fmp) scen

# --------------------------------------------------------------------------- #
# Recipe "lifespan": investment / operation windows from start / end / olife
# --------------------------------------------------------------------------- #

# The investment ("New") and operation ("Span") windows are derived from the
# raw object lifespan (start/end/olife) and existing stock by the new-pipeline
# helpers `get_process_invest_years()` (= New) and `get_process_years()`
# (= invest-years UNION stock-years = Span). The early-amortisation ("Eac")
# window equals Span (legacy behaviour). These are interpolation-independent.
#
# NOTE: `.lifespan_family_def` moved to R/map_lifespan.R (authoritative). The
# lifespan window/retirement helpers below remain shared infrastructure used by
# the per-mapping builders in R/map_lifespan.R.

# Store a derived lifespan map: drop region for region-free families, reorder
# columns to the parameter's dimSets, and write via `d2p`.
.set_lifespan_map <- function(scen, name, df, key, fmp) {
  if (is.null(name) || is.na(name) || is.null(df) || nrow(df) == 0) return(scen)
  p <- scen@modInp@parameters[[name]]
  if (is.null(p)) return(scen)
  df <- df |>
    dplyr::rename(!!key := "process") |>
    dplyr::select(dplyr::any_of(p@dimSets)) |>
    dplyr::distinct()
  scen@modInp@parameters[[name]] <-
    d2p(p, as.data.frame(df), fmp(name))
  scen
}

# (obj, region) / (obj) tuples whose operational life is INFINITE. The model
# equations treat a vintage as never retiring by age when the object is in the
# `*OlifeInf` set (eqTechCap / eqStorageCap / eqTradeCap:
# "ordYear[y] < olife + ordYear[yp] OR (t,r) in mOlifeInf").
.lifespan_olife_inf <- function(scen, cls, key, region, regions) {
  res <- apply_to_scenario_data(
    scen = scen, classes = cls, as_list = TRUE,
    func = function(obj) {
      ol <- as.data.frame(obj@olife)
      if (nrow(ol) == 0 || is.null(ol$olife)) return(NULL)
      isinf <- is.infinite(ol$olife)
      if (!any(isinf)) return(NULL)
      if (region) {
        if ("region" %in% names(ol)) {
          regs <- ol$region[isinf]
          if (anyNA(regs)) regs <- unique(c(regs[!is.na(regs)], regions))
          regs <- regs[regs %in% regions]
        } else {
          regs <- regions
        }
        if (length(regs) == 0) return(NULL)
        df <- data.frame(process = obj@name, region = regs,
                         stringsAsFactors = FALSE)
      } else {
        df <- data.frame(process = obj@name, stringsAsFactors = FALSE)
      }
      out <- list(); out[[obj@name]] <- df; out
    }
  )
  if (length(res) == 0) return(NULL)
  dplyr::bind_rows(res) |> dplyr::distinct()
}

# --------------------------------------------------------------------------- #
# Lifespan retirement maps (technology only; optional capacity retirement)
# --------------------------------------------------------------------------- #
# Built only when the *global* `scen@settings@optimizeRetirement` is TRUE, and
# restricted to technologies whose own `@optimizeRetirement` slot is TRUE (the
# documented "both must be TRUE to be effective" rule). Three maps:
#   meqTechRetiredNewCap [tech, region, year]        = investment window
#   mvTechRetiredStock   [tech, region, year]        = pre-existing-stock years
#   mvTechRetiredNewCap  [tech, region, year, year]  = (invest, operation) pairs
#     kept when invest < operation < invest + olife (legacy obj2modInp.R logic).

# Names of retirement technologies (own slot TRUE).
.retirement_techs <- function(scen) {
  res <- apply_to_scenario_data(
    scen = scen, classes = "technology", as_list = TRUE,
    func = function(obj) {
      if (!isTRUE(obj@optimizeRetirement)) return(NULL)
      out <- list(); out[[obj@name]] <- data.frame(tech = obj@name,
                                                    stringsAsFactors = FALSE)
      out
    }
  )
  if (length(res) == 0) return(character(0))
  unique(dplyr::bind_rows(res)$tech)
}

# Operational life per (tech, region) from the raw `@olife` slot. NA region is
# expanded to the technology's regions; missing data uses the default olife = 1.
.tech_olife <- function(scen, techs, regions, process_region) {
  res <- apply_to_scenario_data(
    scen = scen, classes = "technology", as_list = TRUE,
    func = function(obj) {
      if (!(obj@name %in% techs)) return(NULL)
      tregs <- process_region[[obj@name]]
      if (is.null(tregs)) tregs <- regions
      ol <- as.data.frame(obj@olife)
      if (nrow(ol) == 0 || is.null(ol$olife)) {
        df <- data.frame(tech = obj@name, region = tregs, olife = 1,
                         stringsAsFactors = FALSE)
      } else if (!("region" %in% names(ol))) {
        df <- data.frame(tech = obj@name, region = tregs, olife = ol$olife[1],
                         stringsAsFactors = FALSE)
      } else {
        reg_rows <- ol[!is.na(ol$region), , drop = FALSE]
        na_rows  <- ol[is.na(ol$region), , drop = FALSE]
        parts <- list()
        if (nrow(reg_rows) > 0) {
          parts$r <- data.frame(tech = obj@name, region = reg_rows$region,
                                olife = reg_rows$olife, stringsAsFactors = FALSE)
        }
        if (nrow(na_rows) > 0) {
          miss <- setdiff(tregs, reg_rows$region)
          if (length(miss) > 0) {
            parts$n <- data.frame(tech = obj@name, region = miss,
                                  olife = na_rows$olife[1],
                                  stringsAsFactors = FALSE)
          }
        }
        df <- dplyr::bind_rows(parts)
      }
      df <- df[df$region %in% tregs, , drop = FALSE]
      if (nrow(df) == 0) return(NULL)
      out <- list(); out[[obj@name]] <- df; out
    }
  )
  if (length(res) == 0) return(NULL)
  dplyr::bind_rows(res) |> dplyr::distinct()
}

# Write the [tech, region, year, year] retirement map. This map has a duplicate
# `year` dimension, so its data columns are (tech, region, year, year.1) and we
# must align on `colnames(@data)` rather than the de-duplicated `@dimSets`.
.set_lifespan_retired_newcap <- function(scen, pairs, fmp) {
  name <- "mvTechRetiredNewCap"
  if (is.null(pairs) || nrow(pairs) == 0) return(scen)
  p <- scen@modInp@parameters[[name]]
  if (is.null(p)) return(scen)
  df <- pairs |>
    dplyr::rename(tech = "process") |>
    dplyr::select(dplyr::all_of(colnames(p@data))) |>
    dplyr::distinct()
  scen@modInp@parameters[[name]] <- d2p(p, as.data.frame(df), fmp(name))
  scen
}

# Build the three technology retirement maps. `new_df` / `span_df` are the
# (process, region, year) invest / span windows for the technology family.
.lifespan_retirement_tech <- function(scen, names, fmp, new_df, span_df, regions) {
  ret_names <- c("meqTechRetiredNewCap", "mvTechRetiredStock",
                 "mvTechRetiredNewCap")
  if (!any(ret_names %in% names)) return(scen)
  # Global gate: retirement maps are only effective when enabled model-wide.
  if (!isTRUE(scen@settings@optimizeRetirement)) return(scen)

  techs <- .retirement_techs(scen)
  if (length(techs) == 0) return(scen)

  new_r  <- new_df  |> dplyr::filter(.data$process %in% techs)
  span_r <- span_df |> dplyr::filter(.data$process %in% techs)

  # meqTechRetiredNewCap [tech, region, year] = investment window.
  if ("meqTechRetiredNewCap" %in% names) {
    scen <- .set_lifespan_map(scen, "meqTechRetiredNewCap", new_r, "tech", fmp)
  }

  # mvTechRetiredStock [tech, region, year] = pre-existing-stock operation years.
  if ("mvTechRetiredStock" %in% names) {
    stock_y <- get_process_stock_years(scen) |>
      dplyr::as_tibble() |>
      dplyr::filter(.data$process %in% techs)
    scen <- .set_lifespan_map(scen, "mvTechRetiredStock", stock_y, "tech", fmp)
  }

  # mvTechRetiredNewCap [tech, region, year(invest), year.1(operation)].
  if ("mvTechRetiredNewCap" %in% names && nrow(new_r) > 0) {
    olife <- .tech_olife(scen, techs, regions, scen@modInp@sets[["process_region"]])
    pairs <- new_r |>
      dplyr::select(dplyr::all_of(c("process", "region", "year"))) |>
      dplyr::inner_join(
        span_r |> dplyr::select("process", "region", year.1 = "year"),
        by = c("process", "region"), relationship = "many-to-many"
      ) |>
      dplyr::left_join(
        olife |> dplyr::rename(process = "tech"),
        by = c("process", "region")
      ) |>
      dplyr::mutate(olife = dplyr::coalesce(.data$olife, 1)) |>
      dplyr::filter(.data$year < .data$year.1,
                    .data$year + .data$olife > .data$year.1) |>
      dplyr::mutate(year = as.integer(.data$year),
                    year.1 = as.integer(.data$year.1)) |>
      dplyr::select(dplyr::all_of(c("process", "region", "year", "year.1"))) |>
      dplyr::distinct()
    scen <- .set_lifespan_retired_newcap(scen, pairs, fmp)
  }
  scen
}

# NOTE: `recipe_lifespan` superseded by R/map_lifespan.R and ARCHIVED to
# drafts/legacy-mapping/lifespan.R. The shared lifespan helpers above remain.

# --------------------------------------------------------------------------- #
# Shared set-reduction helpers (faithful ports of the legacy `.make_mapping`
# closures in R/write.R). Used by the value and filter recipes to build the
# derived total / balance / cost maps.
# --------------------------------------------------------------------------- #

# Drop duplicate rows (legacy `reduce.duplicate`).
.reduce_dup <- function(x) {
  if (is.null(x)) return(x)
  x <- as.data.frame(x)
  x[!duplicated(x), , drop = FALSE]
}

# Project onto a set of columns (optional) and drop duplicate rows (legacy
# `reduce.sect`).
.reduce_sect <- function(x, set = NULL) {
  if (is.null(x)) return(x)
  x <- as.data.frame(x)
  if (!is.null(set)) {
    x <- dplyr::select(x, dplyr::all_of(set)) |>
      dplyr::relocate(dplyr::all_of(set))
  }
  x[!duplicated(x), , drop = FALSE]
}

# Row-bind the projections of several data.frames onto a common column set and
# de-duplicate (legacy `reduce.sect.merge.unique`).
.reduce_sect_merge_unique <- function(tx, set) {
  gg <- NULL
  for (x in tx) {
    if (is.null(x) || nrow(as.data.frame(x)) == 0) next
    gg <- rbind(gg, .reduce_sect(x, set))
  }
  if (is.null(gg)) return(NULL)
  unique(gg)
}

# Aggregate a fine-slice flow map up to each commodity's native slice level via
# the commodity slice-or-parent map (legacy `reduce_total_map`). `yy` must carry
# a `comm` and a `slice` column; the incoming `slice` is treated as the fine
# (`slicep`) resolution and replaced by the commodity's own `slice`.
.reduce_total_map <- function(yy, comm_slice_or_parent) {
  if (is.null(yy)) return(yy)
  yy <- as.data.frame(yy)
  if (nrow(yy) == 0) return(yy)
  if (is.null(comm_slice_or_parent) ||
      nrow(as.data.frame(comm_slice_or_parent)) == 0) {
    return(yy[0, , drop = FALSE])
  }
  yy$slicep <- yy$slice
  yy$slice <- NULL
  out <- merge0(yy, as.data.frame(comm_slice_or_parent),
                by = c("comm", "slicep"))
  out$slicep <- NULL
  .reduce_dup(out)
}

# Build a derived inter-regional trade aux flow map (mTradeIrCsrc2Ainp and
# siblings) from an interpolated aux-coefficient parameter. `pdat` is the folded
# parameter data (dims trade, acomm, src, dst, year, slice + value); `mTradeIr`
# is the trade flow domain. Faithful port of obj2modInp.R L3270-3388: keep the
# nonzero rows, drop the placeholder (all-NA / unspecified) index columns, rename
# acomm -> comm, then materialise the missing dimensions through `mTradeIr`.
.trade_aux_derived <- function(pdat, mTradeIr) {
  if (is.null(pdat) || is.null(mTradeIr) || nrow(mTradeIr) == 0) return(NULL)
  pdat <- as.data.frame(pdat)
  if (!"value" %in% names(pdat)) return(NULL)
  pdat <- pdat[!is.na(pdat$value) & pdat$value != 0, , drop = FALSE]
  if (nrow(pdat) == 0) return(NULL)
  keep <- intersect(c("trade", "acomm", "src", "dst", "year", "slice"),
                    names(pdat))
  pdat <- pdat[, keep, drop = FALSE]
  # drop placeholder dimension columns (folded wildcards: all-NA or unspecified)
  drop <- vapply(pdat, function(col) {
    inherits(col, "vctrs_unspecified") || all(is.na(col))
  }, logical(1))
  pdat <- pdat[, !drop, drop = FALSE]
  if ("acomm" %in% names(pdat)) {
    names(pdat)[names(pdat) == "acomm"] <- "comm"
  }
  pdat <- .reduce_dup(pdat)
  out <- as.data.frame(merge0(pdat, as.data.frame(mTradeIr)))
  out <- out[, c("trade", "comm", "src", "dst", "year", "slice"),
             drop = FALSE]
  .reduce_dup(out)
}

# Build the import / export "row" domain maps for one trade direction from the
# interpolated bounds parameter `prow` (long: key, region, year, slice, type,
# value), the slice-membership map `slice_map` (key, slice), the commodity
# membership map `comm_map` (key, comm) and the cumulative-reserve numpar `pres`
# (key, value). `key` is "imp" (import) or "expp" (export). Returns a named list
# with the four map data.frames (`row`, `up`, `lo`, `cumup`); any element may be
# NULL. Faithful port of the legacy import / export `.obj2modInp` methods, with
# the legacy `mImportRowCumUp` bug (using export's key / commodity) corrected.
.io_row_maps <- function(key, slice_map, comm_map, prow, pres,
                         regions, milestones) {
  if (is.null(slice_map) || is.null(comm_map) ||
      length(regions) == 0 || length(milestones) == 0) {
    return(list())
  }
  slice_map <- as.data.frame(slice_map)
  comm_map  <- as.data.frame(comm_map)
  out_cols  <- c(key, "comm", "region", "year", "slice")

  # full domain: (key, slice) x region x milestone, tagged with comm
  base <- merge0(slice_map,
                 data.frame(region = regions, stringsAsFactors = FALSE))
  base <- merge0(as.data.frame(base),
                 data.frame(year = as.integer(milestones)))
  base <- as.data.frame(merge0(as.data.frame(base), comm_map))
  base <- base[, intersect(out_cols, colnames(base)), drop = FALSE]

  zero_keys <- up_keys <- lo_keys <- NULL
  if (!is.null(prow)) {
    prow <- as.data.frame(prow)
    kc   <- intersect(c(key, "region", "year"), colnames(prow))
    is_up <- prow$type == "up" & !is.na(prow$value)
    is_lo <- prow$type == "lo" & !is.na(prow$value)
    z <- unique(prow[is_up & prow$value == 0, kc, drop = FALSE])
    u <- unique(prow[is_up & is.finite(prow$value) & prow$value != 0, kc,
                     drop = FALSE])
    l <- unique(prow[is_lo & prow$value != 0, kc, drop = FALSE])
    if (nrow(z) > 0) zero_keys <- z
    if (nrow(u) > 0) up_keys   <- u
    if (nrow(l) > 0) lo_keys   <- l
  }

  m_row <- base
  if (!is.null(zero_keys)) {
    m_row <- dplyr::anti_join(m_row, zero_keys, by = colnames(zero_keys))
  }
  m_row <- .reduce_dup(m_row[, intersect(out_cols, colnames(m_row)),
                            drop = FALSE])

  m_up <- meq_lo <- m_cumup <- NULL
  if (!is.null(up_keys) && nrow(m_row) > 0) {
    j <- dplyr::inner_join(m_row, up_keys, by = colnames(up_keys))
    if (nrow(j) > 0) m_up <- j
  }
  if (!is.null(lo_keys) && nrow(m_row) > 0) {
    j <- dplyr::inner_join(m_row, lo_keys, by = colnames(lo_keys))
    if (nrow(j) > 0) meq_lo <- j
  }
  if (!is.null(pres)) {
    pres <- as.data.frame(pres)
    pres <- pres[!is.na(pres$value) & is.finite(pres$value), , drop = FALSE]
    if (nrow(pres) > 0) {
      cu <- as.data.frame(merge0(pres[, key, drop = FALSE], comm_map))
      cu <- .reduce_dup(cu[, c(key, "comm"), drop = FALSE])
      if (nrow(cu) > 0) m_cumup <- cu
    }
  }

  list(row   = if (nrow(m_row) > 0) m_row else NULL,
       up    = m_up,
       lo    = meq_lo,
       cumup = m_cumup)
}

# Build one auxiliary-conversion domain map (e.g. mTechAct2AInp,
# mStorageStg2AOut, mTechCinp2AInp) from an interpolated conversion-factor
# parameter `pdat` and the relevant flow domain `flow_dom`. `pdat` carries the
# object key (tech / stg), the auxiliary commodity `acomm`, optionally the
# driving flow commodity `comm`, the (folded) region / year / slice and a
# `value`. Rows with NA / zero conversion factor carry no domain. The auxiliary
# commodity becomes the map's `comm`; for the technology cinp/cout maps the
# original flow commodity is retained as a second `comm.1` dimension. Faithful
# port of the aeff loops in the legacy technology / storage `.obj2modInp`
# methods (obj2modInp.R L2233-2305 / L1170-1195).
.aux_conv_map <- function(pdat, flow_dom, second_comm = FALSE) {
  if (is.null(pdat) || is.null(flow_dom) ||
      nrow(as.data.frame(flow_dom)) == 0) {
    return(NULL)
  }
  pdat <- as.data.frame(pdat)
  if (!"value" %in% names(pdat) || !"acomm" %in% names(pdat)) return(NULL)
  pdat <- pdat[!is.na(pdat$value) & pdat$value != 0, , drop = FALSE]
  if (nrow(pdat) == 0) return(NULL)
  pdat$value <- NULL
  # drop folded placeholder dimension columns (all-NA / unspecified wildcards)
  drop <- vapply(pdat, function(col) {
    inherits(col, "vctrs_unspecified") || all(is.na(col))
  }, logical(1))
  pdat <- pdat[, !drop, drop = FALSE]
  if (second_comm) {
    # `comm` here is the driving flow commodity: join the flow domain on it to
    # materialise region / year / slice, THEN relabel acomm -> comm and the
    # original flow commodity -> comm.1.
    out <- as.data.frame(merge0(.reduce_dup(pdat), as.data.frame(flow_dom)))
    if (nrow(out) == 0) return(NULL)
    out$comm.1 <- out$comm
    out$comm <- out$acomm
  } else {
    # the auxiliary commodity is the only commodity dimension.
    names(pdat)[names(pdat) == "acomm"] <- "comm"
    out <- as.data.frame(merge0(.reduce_dup(pdat), as.data.frame(flow_dom)))
    if (nrow(out) == 0) return(NULL)
  }
  out$acomm <- NULL
  .reduce_dup(out)
}


# --------------------------------------------------------------------------- #
# Recipe 5: value maps (parameter-value-derived domains)
#
# A value map is the set of index tuples on which a cost / value parameter is
# defined, restricted to the relevant lifespan window. The legacy pipeline
# builds them as `merge0(<window map>, <source parameter, value != 0>)` and
# drops the value column (e.g. `mTechInv = mTechNew |X| pTechInvcost`,
# `mTechFixom = pTechFixom(value != 0) |X| mTechSpan`).
#
# Because the m* <-> source p* link is not carried in the spec, it is declared
# explicitly here. Regular maps follow the "project source onto the map's dims,
# inner-join with the window map" pattern (`.value_map_def`); irregular maps use
# bespoke builders (`.value_map_builders`). Unhandled names fall through and are
# reported as pending.
# --------------------------------------------------------------------------- #

# NOTE: `.value_map_def` (value map -> source p* params + window) moved to
# R/map_value.R, which is now authoritative (it backs the per-mapping map_*
# builders and is read by interp.R `.param_value_maps()` for parameter trimming).

# Generic writer for derived maps: keep the columns present in the parameter's
# data template (its dimension sets), de-duplicate, and persist via `d2p`.
.set_map <- function(scen, name, df, fmp) {
  if (is.null(name) || is.null(df) || nrow(df) == 0) return(scen)
  p <- scen@modInp@parameters[[name]]
  if (is.null(p)) return(scen)
  cols <- colnames(p@data)
  df <- as.data.frame(df) |>
    dplyr::select(dplyr::any_of(cols)) |>
    dplyr::distinct()
  if (!all(cols %in% colnames(df))) return(scen)
  df <- df[, cols, drop = FALSE]
  if (nrow(df) == 0) return(scen)
  scen@modInp@parameters[[name]] <- d2p(p, df, fmp(name))
  scen
}

# NOTE: the value-family builders (.build_value_map_std, .build_mSupSpan,
# .build_mTechRetirement, .build_policy_cost, .build_mTaxCost, .build_mSubCost,
# .build_weather_map, .value_map_builders) were superseded by the per-mapping
# functions in R/map_value.R and ARCHIVED to drafts/legacy-mapping/value.R.

#' Build value mappings (recipe 5)
#'
#' @param scen scenario object.
#' @param names character vector of value mapping names to build.
#' @param fmp function mapping a parameter name to its on-disk path.
#' @returns updated scenario object.
#' @keywords internal
# Value maps whose derivation depends on filter-recipe maps and are therefore
# built inside recipe_filter (which runs after recipe_value would). Listed here
# so recipe_value does not report them as pending.
.value_maps_built_in_filter <- c(
  "mInpSub", "mOutSub",
  "mDummyImportCost", "mDummyExportCost",
  "mImportRowCost", "mExportRowCost",
  "mImportIrCost", "mExportIrCost"
)

# Value maps for features whose source object slot is not implemented in the
# current class definitions (legacy code commented out -> always empty), e.g.
# mTechUpgrade (technology@upgrade.technology). Skipped silently.
.value_maps_unsupported <- c("mTechUpgrade")

# Fallback for value-family names NOT handled by the registry (R/map_value.R):
# the filter-deferred maps and unsupported ones are intentional no-ops; anything
# else is reported pending. (The real value maps are built by their map_*().)
recipe_value <- function(scen, names, fmp) {
  pending <- setdiff(unique(names),
                     c(.value_maps_built_in_filter, .value_maps_unsupported))
  if (length(pending) > 0) {
    message("recipe 'value': ", length(pending),
            " mapping(s) pending engine implementation: ",
            paste(pending, collapse = ", "))
  }
  scen
}

# --------------------------------------------------------------------------- #
# Recipe "filter": activity / flow domains (variable index sets)
# --------------------------------------------------------------------------- #

# These maps define the actual (region, year, slice, commodity) domains over
# which the activity / flow variables are defined. They are derived by joining
# the operation windows (lifespan recipe: mTechSpan / mStorageSpan), the
# membership commodity maps (membership recipe: mTechInpComm / mTechOutComm /
# mTechAInp / mTechAOut / mSupComm / mStorageComm / mStorageAInp / mStorageAOut)
# and the per-object slice maps (calendar recipe: mTechSlice / mSupSlice), then
# restricting commodity flows to feasible (commodity, region) pairs via the
# commodity-region closure map (closure recipe: mCommReg).
#
# This first implementation covers the core technology, supply and storage
# activity / flow domains. Aggregation totals, aux-conversion, trade flows,
# demand, import/export-row and lower-bound (`*2Lo`) maps are reported as
# pending and added in subsequent rounds.

# Core activity / flow maps handled by this recipe so far.
.filter_core_maps <- c(
  "mvTechAct", "mvTechInp", "mvTechOut", "mTechOutRY",
  "mvTechAInp", "mvTechAOut",
  "mSupAva",
  "mvStorageStore", "mvStorageAInp", "mvStorageAOut"
)

.build_filter_core <- function(scen, want, fmp) {
  # read a stored mapping as a data.frame (NULL when absent / empty)
  gds <- function(nm) {
    p <- scen@modInp@parameters[[nm]]
    if (is.null(p)) return(NULL)
    d <- get_data_slot(p)
    if (is.null(d) || nrow(d) == 0) return(NULL)
    as.data.frame(d)
  }
  # persist a map only when it was requested
  setm <- function(scen, nm, df) {
    if (!nm %in% want || is.null(df) || nrow(df) == 0) return(scen)
    .set_map(scen, nm, df, fmp)
  }
  # restrict commodity flows to feasible (commodity, region) pairs
  cr <- gds("mCommReg")
  filt <- function(df, region_col = "region") {
    if (is.null(df) || nrow(df) == 0) return(df)
    df <- as.data.frame(df)
    if (is.null(cr) || nrow(cr) == 0) return(df[0, , drop = FALSE])
    by <- if (region_col == "region") {
      c("comm", "region")
    } else {
      c("comm", stats::setNames("region", region_col))
    }
    dplyr::semi_join(df, cr, by = by)
  }

  milestones <- as.integer(scen@settings@horizon@intervals$mid)
  proc_slice <- .process_slice_df(scen)
  pclass     <- get_process_class(scen)

  ## --- technology ---------------------------------------------------------
  tech_span  <- gds("mTechSpan")
  tech_slice <- .proc_slice_for(proc_slice, pclass, "technology", "tech")
  act <- NULL
  if (!is.null(tech_span) && !is.null(tech_slice)) {
    act <- as.data.frame(merge0(tech_span, tech_slice))
    scen <- setm(scen, "mvTechAct", act)
  }
  if (!is.null(act)) {
    inp <- gds("mTechInpComm")
    if (!is.null(inp)) {
      scen <- setm(scen, "mvTechInp", filt(as.data.frame(merge0(act, inp))))
    }
    out <- gds("mTechOutComm")
    if (!is.null(out)) {
      mvTechOut <- filt(as.data.frame(merge0(act, out)))
      scen <- setm(scen, "mvTechOut", mvTechOut)
      ry <- dplyr::distinct(dplyr::select(mvTechOut,
        dplyr::any_of(c("tech", "comm", "region", "year"))))
      scen <- setm(scen, "mTechOutRY", ry)
    }
    ainp <- gds("mTechAInp")
    if (!is.null(ainp)) {
      scen <- setm(scen, "mvTechAInp", filt(as.data.frame(merge0(act, ainp))))
    }
    aout <- gds("mTechAOut")
    if (!is.null(aout)) {
      scen <- setm(scen, "mvTechAOut", filt(as.data.frame(merge0(act, aout))))
    }
  }

  ## --- supply -------------------------------------------------------------
  sup_span  <- gds("mSupSpan")
  sup_comm  <- gds("mSupComm")
  sup_slice <- .proc_slice_for(proc_slice, pclass, "supply", "sup")
  if (!is.null(sup_span) && !is.null(sup_comm) && !is.null(sup_slice) &&
      length(milestones) > 0) {
    ava <- merge0(sup_span, sup_comm)
    ava <- merge0(ava, sup_slice)
    ava <- merge0(as.data.frame(ava), data.frame(year = milestones))
    scen <- setm(scen, "mSupAva", filt(as.data.frame(ava)))
  }

  ## --- storage ------------------------------------------------------------
  stg_span  <- gds("mStorageSpan")
  stg_comm  <- gds("mStorageComm")
  stg_slice <- .proc_slice_for(proc_slice, pclass, "storage", "stg")
  store <- NULL
  if (!is.null(stg_span) && !is.null(stg_comm) && !is.null(stg_slice)) {
    store <- merge0(stg_span, stg_comm)
    store <- as.data.frame(merge0(store, stg_slice))
    scen <- setm(scen, "mvStorageStore", filt(store))
  }
  if (!is.null(store)) {
    base <- dplyr::select(store, -dplyr::any_of("comm"))
    sa_inp <- gds("mStorageAInp")
    if (!is.null(sa_inp)) {
      scen <- setm(scen, "mvStorageAInp",
                   filt(as.data.frame(merge0(base, sa_inp))))
    }
    sa_out <- gds("mStorageAOut")
    if (!is.null(sa_out)) {
      scen <- setm(scen, "mvStorageAOut",
                   filt(as.data.frame(merge0(base, sa_out))))
    }
  }
  scen
}

# --------------------------------------------------------------------------- #
# Derived total / aggregation / balance maps (recipe "filter", source derived)
# --------------------------------------------------------------------------- #
# These maps are not flow-domain primitives but aggregations / unions of the
# core flow maps, restricted to each commodity's native slice resolution via the
# commodity slice-or-parent map (`mCommSliceOrParent`). They mirror the legacy
# `.make_mapping` derivations in R/write.R: the per-process input / output
# totals (`mTech*Tot`, `mSupOutTot`, `mStorage*Tot`), the emission and
# aggregate-output totals, the inter-regional trade aux totals, the commodity
# balance domains (`mvInpTot` / `mvOutTot` / `mvBalance`) and their
# year-resolution projections (`m*TotRY`, `mBalanceRY`).

# Maps handled by the derived-totals builder.
.filter_derived_maps <- c(
  "mvDemInp",
  "mTechInpTot", "mTechOutTot", "mSupOutTot",
  "mStorageInpTot", "mStorageOutTot",
  "mEmsFuelTot", "mAggOut", "mAggregateFactor",
  "mImportRow", "mImportRowUp",
  "mExportRow", "mExportRowUp",
  "mDummyImport", "mDummyExport",
  "mExport", "mImport",
  "mSupAvaUp",
  "mTechAct2AInp", "mTechAct2AOut",
  "mTechCap2AInp", "mTechCap2AOut",
  "mTechNCap2AInp", "mTechNCap2AOut",
  "mTechCinp2AInp", "mTechCinp2AOut",
  "mTechCout2AInp", "mTechCout2AOut",
  "mStorageStg2AInp", "mStorageStg2AOut",
  "mStorageCinp2AInp", "mStorageCinp2AOut",
  "mStorageCout2AInp", "mStorageCout2AOut",
  "mStorageCap2AInp", "mStorageCap2AOut",
  "mStorageNCap2AInp", "mStorageNCap2AOut",
  "mTradeIr", "mvTradeIr",
  "mTradeIrAInp", "mTradeIrAOut",
  "mTradeIrCsrc2Ainp", "mTradeIrCdst2Ainp",
  "mTradeIrCsrc2Aout", "mTradeIrCdst2Aout",
  "mvTradeIrAInp", "mvTradeIrAOut",
  "mvTradeIrAInpTot", "mvTradeIrAOutTot",
  "mInp2Lo", "mOut2Lo", "mvInp2Lo", "mvOut2Lo",
  "mvInpTot", "mvOutTot", "mvBalance",
  "mInpTotRY", "mOutTotRY", "mBalanceRY"
)

.build_filter_derived <- function(scen, want, fmp) {
  # read a stored mapping / parameter as a data.frame (NULL when absent / empty)
  gds <- function(nm) {
    p <- scen@modInp@parameters[[nm]]
    if (is.null(p)) return(NULL)
    d <- get_data_slot(p)
    if (is.null(d) || nrow(d) == 0) return(NULL)
    as.data.frame(d)
  }
  setm <- function(scen, nm, df) {
    if (!nm %in% want || is.null(df) || nrow(df) == 0) return(scen)
    .set_map(scen, nm, df, fmp)
  }
  # store unconditionally (regardless of `want`) when the parameter slot exists;
  # used for intermediates that other maps in this recipe depend on.
  setm_any <- function(scen, nm, df) {
    if (is.null(df) || nrow(df) == 0) return(scen)
    if (is.null(scen@modInp@parameters[[nm]])) return(scen)
    .set_map(scen, nm, df, fmp)
  }

  csop       <- gds("mCommSliceOrParent")
  cr         <- gds("mCommReg")
  comm_slice <- gds("mCommSlice")
  regions    <- as.character(scen@settings@region)
  milestones <- as.integer(scen@settings@horizon@intervals$mid)
  ry_cols    <- c("comm", "region", "year", "slice")

  # restrict a (comm, region, ...) total to feasible commodity-region pairs
  filt_cr <- function(df) {
    if (is.null(df) || nrow(df) == 0) return(df)
    if (is.null(cr) || nrow(cr) == 0) return(df[0, , drop = FALSE])
    dplyr::inner_join(df, cr, by = c("comm", "region"))
  }
  # row-bind several frames projected to the (comm, region, year, slice) domain,
  # dropping NULL / empty pieces, then de-duplicate
  bind_ry <- function(...) {
    fr <- Filter(function(x) !is.null(x) && nrow(as.data.frame(x)) > 0,
                 list(...))
    if (length(fr) == 0) return(NULL)
    fr <- lapply(fr, function(x) {
      x <- as.data.frame(x)
      x[, intersect(ry_cols, colnames(x)), drop = FALSE]
    })
    .reduce_dup(dplyr::bind_rows(fr))
  }

  ## --- demand input domain ------------------------------------------------
  # mvDemInp: each demand commodity over its own slices x region x milestones.
  if ("mvDemInp" %in% want) {
    dc <- gds("mDemComm")
    if (!is.null(dc) && !is.null(comm_slice) &&
        length(regions) > 0 && length(milestones) > 0) {
      di <- merge0(dc, comm_slice)
      di <- merge0(as.data.frame(di),
                   data.frame(region = regions, stringsAsFactors = FALSE))
      di <- merge0(as.data.frame(di), data.frame(year = milestones))
      scen <- setm(scen, "mvDemInp", di)
    }
  }

  ## --- technology input / output totals -----------------------------------
  mvTechInp  <- gds("mvTechInp")
  mvTechAInp <- gds("mvTechAInp")
  if ("mTechInpTot" %in% want &&
      (!is.null(mvTechInp) || !is.null(mvTechAInp))) {
    pieces <- lapply(Filter(Negate(is.null), list(mvTechInp, mvTechAInp)),
                     function(x) dplyr::select(x, -dplyr::any_of("tech")))
    tot <- .reduce_total_map(.reduce_sect(dplyr::bind_rows(pieces)), csop)
    scen <- setm(scen, "mTechInpTot", filt_cr(tot))
  }
  mvTechOut  <- gds("mvTechOut")
  mvTechAOut <- gds("mvTechAOut")
  if ("mTechOutTot" %in% want &&
      (!is.null(mvTechOut) || !is.null(mvTechAOut))) {
    pieces <- lapply(Filter(Negate(is.null), list(mvTechOut, mvTechAOut)),
                     function(x) dplyr::select(x, -dplyr::any_of("tech")))
    tot <- .reduce_total_map(.reduce_sect(dplyr::bind_rows(pieces)), csop)
    scen <- setm(scen, "mTechOutTot", filt_cr(tot))
  }

  ## --- supply output total ------------------------------------------------
  # drop the leading `sup` column of mSupAva (legacy mSupAva[, -1]).
  mSupAva <- gds("mSupAva")
  if ("mSupOutTot" %in% want && !is.null(mSupAva)) {
    scen <- setm(scen, "mSupOutTot",
                 .reduce_sect(mSupAva[, -1, drop = FALSE]))
  }

  ## --- supply availability bound domains ----------------------------------
  # mSupAvaUp: restrict the supply availability domain mSupAva to the
  # (sup, comm, region, year) keys with a finite non-default upper bound
  # (legacy obj2modInp.R L1467-1475). The lower-bound counterpart meqSupAvaLo is
  # a constraint-recipe map sharing the same derivation, built here and skipped
  # by recipe_constraint. The bounds in pSupAva are folded over `slice`, so the
  # join carries mSupAva's slices.
  if (!is.null(mSupAva)) {
    pSupAva <- gds("pSupAva")
    if (!is.null(pSupAva)) {
      sk <- intersect(c("sup", "comm", "region", "year"), colnames(pSupAva))
      up <- unique(pSupAva[pSupAva$type == "up" & !is.na(pSupAva$value) &
                             is.finite(pSupAva$value) & pSupAva$value != 0,
                           sk, drop = FALSE])
      if (nrow(up) > 0) {
        scen <- setm(scen, "mSupAvaUp",
                     filt_cr(.reduce_dup(dplyr::inner_join(mSupAva, up,
                                                           by = sk))))
      }
      lo <- unique(pSupAva[pSupAva$type == "lo" & !is.na(pSupAva$value) &
                             pSupAva$value != 0, sk, drop = FALSE])
      if (nrow(lo) > 0) {
        scen <- setm_any(scen, "meqSupAvaLo",
                         filt_cr(.reduce_dup(dplyr::inner_join(mSupAva, lo,
                                                               by = sk))))
      }
    }
  }

  ## --- row (import / export) flow domains ---------------------------------
  # mImportRow / mExportRow and their upper-bound restrictions, built from the
  # interpolated bounds parameters via .io_row_maps. The lower-bound and
  # cumulative-reserve counterparts are constraint-recipe maps, built later.
  imp_maps <- .io_row_maps("imp", gds("mImpSlice"), gds("mImpComm"),
                           gds("pImportRow"), gds("pImportRowRes"),
                           regions, milestones)
  scen <- setm(scen, "mImportRow",   filt_cr(imp_maps$row))
  scen <- setm(scen, "mImportRowUp", filt_cr(imp_maps$up))
  # constraint-recipe counterparts (lower bound / cumulative reserve); built
  # here because they share the .io_row_maps derivation. recipe_constraint
  # skips them (see .constraint_maps_built_in_filter).
  scen <- setm_any(scen, "meqImportRowLo",  filt_cr(imp_maps$lo))
  scen <- setm_any(scen, "mImportRowCumUp", imp_maps$cumup)

  exp_maps <- .io_row_maps("expp", gds("mExpSlice"), gds("mExpComm"),
                           gds("pExportRow"), gds("pExportRowRes"),
                           regions, milestones)
  scen <- setm(scen, "mExportRow",   filt_cr(exp_maps$row))
  scen <- setm(scen, "mExportRowUp", filt_cr(exp_maps$up))
  scen <- setm_any(scen, "meqExportRowLo",  filt_cr(exp_maps$lo))
  scen <- setm_any(scen, "mExportRowCumUp", exp_maps$cumup)

  ## --- dummy import / export domains --------------------------------------
  # mDummyImport / mDummyExport: the (comm, region, year, slice) tuples with a
  # finite dummy slack cost (default Inf -> empty), restricted to feasible
  # commodity-region pairs (legacy write.R L802-826).
  build_dummy <- function(scen, param, nm) {
    if (!nm %in% want) return(scen)
    d <- gds(param)
    if (is.null(d)) return(scen)
    d <- d[!is.na(d$value) & is.finite(d$value), , drop = FALSE]
    if (nrow(d) == 0) return(scen)
    d <- d[, intersect(c("comm", "region", "year", "slice"), colnames(d)),
           drop = FALSE]
    setm(scen, nm, filt_cr(.reduce_dup(d)))
  }
  scen <- build_dummy(scen, "pDummyImportCost", "mDummyImport")
  scen <- build_dummy(scen, "pDummyExportCost", "mDummyExport")

  ## --- auxiliary-conversion domains ---------------------------------------
  # mTech*2A* / mStorage*2A*: the (key, comm, region, year, slice) tuples on
  # which an activity / capacity / flow drives an auxiliary commodity flow.
  # Built from the interpolated conversion-factor parameters (pTechAct2AInp,
  # ...) by relabelling the auxiliary commodity as `comm` and materialising the
  # spatial / temporal dimensions through the relevant flow domain (legacy
  # technology / storage aeff loops). These maps are unreferenced by the model
  # equations, so they are pure index sets. Empty unless an object declares an
  # auxiliary commodity with a non-zero conversion factor.
  mvTechAct    <- gds("mvTechAct")
  mvStorageAInp <- gds("mvStorageAInp")
  mvStorageAOut <- gds("mvStorageAOut")
  aux_specs <- list(
    # tech activity / capacity drivers (single commodity = auxiliary) -> mvTechAct
    list("mTechAct2AInp",  "pTechAct2AInp",  mvTechAct, FALSE),
    list("mTechAct2AOut",  "pTechAct2AOut",  mvTechAct, FALSE),
    list("mTechCap2AInp",  "pTechCap2AInp",  mvTechAct, FALSE),
    list("mTechCap2AOut",  "pTechCap2AOut",  mvTechAct, FALSE),
    list("mTechNCap2AInp", "pTechNCap2AInp", mvTechAct, FALSE),
    list("mTechNCap2AOut", "pTechNCap2AOut", mvTechAct, FALSE),
    # tech commodity-flow drivers (second commodity retained as comm.1).
    # The driving flow commodity lives in the input domain for *2AInp and the
    # output domain for *2AOut (legacy obj2modInp.R L2253-2258).
    list("mTechCinp2AInp", "pTechCinp2AInp", mvTechInp, TRUE),
    list("mTechCinp2AOut", "pTechCinp2AOut", mvTechOut, TRUE),
    list("mTechCout2AInp", "pTechCout2AInp", mvTechInp, TRUE),
    list("mTechCout2AOut", "pTechCout2AOut", mvTechOut, TRUE),
    # storage drivers (single commodity = auxiliary) -> aux flow domain.
    list("mStorageStg2AInp",  "pStorageStg2AInp",  mvStorageAInp,  FALSE),
    list("mStorageStg2AOut",  "pStorageStg2AOut",  mvStorageAOut,  FALSE),
    list("mStorageCinp2AInp", "pStorageCinp2AInp", mvStorageAInp,  FALSE),
    list("mStorageCinp2AOut", "pStorageCinp2AOut", mvStorageAOut,  FALSE),
    list("mStorageCout2AInp", "pStorageCout2AInp", mvStorageAInp,  FALSE),
    list("mStorageCout2AOut", "pStorageCout2AOut", mvStorageAOut,  FALSE),
    list("mStorageCap2AInp",  "pStorageCap2AInp",  mvStorageAInp,  FALSE),
    list("mStorageCap2AOut",  "pStorageCap2AOut",  mvStorageAOut,  FALSE),
    list("mStorageNCap2AInp", "pStorageNCap2AInp", mvStorageAInp,  FALSE),
    list("mStorageNCap2AOut", "pStorageNCap2AOut", mvStorageAOut,  FALSE)
  )
  for (sp in aux_specs) {
    nm <- sp[[1]]
    if (!nm %in% want) next
    m <- .aux_conv_map(gds(sp[[2]]), sp[[3]], second_comm = sp[[4]])
    scen <- setm(scen, nm, filt_cr(m))
  }


  # legacy uses rbind(mvStorageAInp[, -1], mvStorageStore[, -1]) for BOTH the
  # input and output totals.
  mvStorageAInp  <- gds("mvStorageAInp")
  mvStorageStore <- gds("mvStorageStore")
  if (!is.null(mvStorageAInp) || !is.null(mvStorageStore)) {
    pieces <- lapply(Filter(Negate(is.null),
                            list(mvStorageAInp, mvStorageStore)),
                     function(x) x[, -1, drop = FALSE])
    stot <- .reduce_sect(dplyr::bind_rows(pieces))
    scen <- setm(scen, "mStorageInpTot", stot)
    scen <- setm(scen, "mStorageOutTot", stot)
  }

  ## --- emission-fuel total ------------------------------------------------
  # mTechEmsFuel = (tech, comm, comm.1, region, year, slice) linking each
  # technology's input commodity to the emission commodity it generates, then
  # aggregated to mEmsFuelTot. Empty unless emission factors are present.
  # Membership follows every technology consuming a fuel that has an emission
  # factor; the combustion coefficient (pTechEmisComm, default 1) only scales
  # the emission, so a tech is excluded only when combustion is explicitly 0.
  if ("mEmsFuelTot" %in% want) {
    effac <- gds("pEmissionFactor")
    if (!is.null(mvTechInp) && !is.null(effac)) {
      effac <- effac[!is.na(effac$value) & effac$value != 0, , drop = FALSE]
      effac <- .reduce_dup(effac)
      if (nrow(effac) > 0) {
        # mvTechInp has comm = consumed fuel; treat it as commp for the join.
        ti <- dplyr::rename(mvTechInp, commp = "comm")
        emis <- gds("pTechEmisComm")
        if (!is.null(emis)) {
          zero <- emis[!is.na(emis$value) & emis$value == 0,
                       c("tech", "comm"), drop = FALSE]
          if (nrow(zero) > 0) {
            zero <- dplyr::rename(zero, commp = "comm")
            ti <- dplyr::anti_join(ti, zero, by = c("tech", "commp"))
          }
        }
        ems <- merge0(effac, ti, by = "commp")
        keep <- intersect(c("tech", "comm", "commp", "region", "year", "slice"),
                          colnames(ems))
        ems <- as.data.frame(ems)[, keep, drop = FALSE]
        ems <- filt_cr(ems)
        if (!is.null(ems) && nrow(ems) > 0) {
          ems_fuel <- dplyr::rename(dplyr::distinct(ems), comm.1 = "commp")
          scen <- .set_map(scen, "mTechEmsFuel", ems_fuel, fmp)
          tot <- .reduce_total_map(.reduce_sect(ems, ry_cols), csop)
          scen <- setm(scen, "mEmsFuelTot", tot)
        }
      }
    }
  }

  ## --- aggregate-factor membership ----------------------------------------
  # mAggregateFactor = (comm, comm.1) linking each aggregate commodity to its
  # component commodity (legacy write.R: pAggregateFactor value != 0).
  if ("mAggregateFactor" %in% want) {
    agg <- gds("pAggregateFactor")
    if (!is.null(agg)) {
      agg <- agg[!is.na(agg$value) & agg$value != 0, , drop = FALSE]
      if (nrow(agg) > 0) {
        af <- dplyr::distinct(dplyr::rename(
          agg[, c("comm", "commp"), drop = FALSE], comm.1 = "commp"))
        scen <- .set_map(scen, "mAggregateFactor", af, fmp)
      }
    }
  }

  ## --- aggregate-output total ---------------------------------------------
  if ("mAggOut" %in% want) {
    agg <- gds("pAggregateFactor")
    if (!is.null(agg) && length(regions) > 0 && length(milestones) > 0 &&
        !is.null(comm_slice)) {
      slices <- unique(comm_slice$slice)
      a <- .reduce_sect(agg, "comm")
      a <- merge0(a, data.frame(region = regions, stringsAsFactors = FALSE))
      a <- merge0(as.data.frame(a), data.frame(year = milestones))
      a <- merge0(as.data.frame(a),
                  data.frame(slice = as.character(slices),
                             stringsAsFactors = FALSE))
      scen <- setm(scen, "mAggOut",
                   .reduce_total_map(.reduce_dup(a), csop))
    }
  }

  ## --- inter-regional trade route / flow domain ---------------------------
  # mTradeRoutes: per-trade (src, dst) pairs, straight from each trade object's
  # `routes` slot. Tagged recipe "constraint" in the spec, but built here as an
  # intermediate because the inter-regional flow maps below depend on it.
  # mTradeIr:  trade flow domain = routes x slices x years. The year domain is
  #            the trade's operation span (mTradeSpan) for capacity-variable
  #            trades, otherwise all milestone years. Faithful port of
  #            obj2modInp.R L3258.
  # mvTradeIr: mTradeIr tagged with the traded commodity (via mTradeComm).
  trade_slice <- gds("mTradeSlice")
  trade_span  <- gds("mTradeSpan")
  trade_comm  <- gds("mTradeComm")
  trade_info <- apply_to_scenario_data(
    scen = scen, classes = "trade", as_list = TRUE,
    func = function(x) {
      rt <- x@routes
      if (is.null(rt) || nrow(rt) == 0) return(list())
      o <- list()
      o[[x@name]] <- list(
        routes = data.frame(
          trade = x@name, src = as.character(rt$src),
          dst = as.character(rt$dst), stringsAsFactors = FALSE
        ),
        capvar = isTRUE(x@capacityVariable)
      )
      o
    }
  )
  if (length(trade_info) > 0) {
    routes_all <- do.call(rbind, lapply(trade_info, `[[`, "routes"))
    rownames(routes_all) <- NULL
    routes_all <- .reduce_dup(routes_all)
    scen <- setm_any(scen, "mTradeRoutes", routes_all)

    if (!is.null(trade_slice)) {
      ir_pieces <- list()
      for (nm in names(trade_info)) {
        rt <- trade_info[[nm]]$routes
        sl <- trade_slice[trade_slice$trade == nm, , drop = FALSE]
        if (nrow(sl) == 0) next
        base <- as.data.frame(merge0(rt, sl))     # trade, src, dst, slice
        if (trade_info[[nm]]$capvar && !is.null(trade_span)) {
          yrs <- trade_span$year[trade_span$trade == nm]
        } else {
          yrs <- milestones
        }
        if (length(yrs) == 0) next
        ir_pieces[[nm]] <- as.data.frame(
          merge0(base, data.frame(year = as.integer(yrs)))
        )
      }
      if (length(ir_pieces) > 0) {
        mTradeIr <- .reduce_dup(do.call(rbind, ir_pieces))
        mTradeIr <- mTradeIr[, c("trade", "src", "dst", "year", "slice"),
                             drop = FALSE]
        scen <- setm(scen, "mTradeIr", mTradeIr)
        if (!is.null(trade_comm)) {
          mvTradeIr <- as.data.frame(merge0(mTradeIr, trade_comm))
          mvTradeIr <- mvTradeIr[, c("trade", "comm", "src", "dst",
                                     "year", "slice"), drop = FALSE]
          scen <- setm(scen, "mvTradeIr", mvTradeIr)
        }

        ## --- inter-regional trade aux flow chain ----------------------
        # Aux commodities consumed (AInp) / produced (AOut) by a trade flow,
        # plus the per-route coefficient domains and their region projections.
        # Faithful port of obj2modInp.R L2920-2989 (membership) and
        # L3270-3388 (derived maps). The interpolated aux-coefficient params
        # (pTradeIrCsrc2Ainp etc.) are read back and projected onto mTradeIr.
        pCsrc2Ainp <- gds("pTradeIrCsrc2Ainp")
        pCdst2Ainp <- gds("pTradeIrCdst2Ainp")
        pCsrc2Aout <- gds("pTradeIrCsrc2Aout")
        pCdst2Aout <- gds("pTradeIrCdst2Aout")

        nz_comm <- function(p) {
          if (is.null(p)) return(character(0))
          p <- as.data.frame(p)
          if (!all(c("acomm", "value") %in% names(p))) return(character(0))
          unique(p$acomm[!is.na(p$value) & p$value != 0])
        }
        nz_trade_comm <- function(p) {
          if (is.null(p)) return(NULL)
          p <- as.data.frame(p)
          p <- p[!is.na(p$value) & p$value != 0, , drop = FALSE]
          if (nrow(p) == 0) return(NULL)
          .reduce_dup(p[, c("trade", "acomm"), drop = FALSE])
        }

        # membership: mTradeIrAInp / mTradeIrAOut (trade, comm)
        ainp_m <- .reduce_dup(rbind(nz_trade_comm(pCsrc2Ainp),
                                    nz_trade_comm(pCdst2Ainp)))
        aout_m <- .reduce_dup(rbind(nz_trade_comm(pCsrc2Aout),
                                    nz_trade_comm(pCdst2Aout)))
        if (!is.null(ainp_m) && nrow(ainp_m) > 0) {
          names(ainp_m)[names(ainp_m) == "acomm"] <- "comm"
          scen <- setm(scen, "mTradeIrAInp", ainp_m)
        }
        if (!is.null(aout_m) && nrow(aout_m) > 0) {
          names(aout_m)[names(aout_m) == "acomm"] <- "comm"
          scen <- setm(scen, "mTradeIrAOut", aout_m)
        }

        # derived per-route coefficient maps + region projections (mvTradeIr*)
        proj_region <- function(m, dim_col) {
          if (is.null(m) || nrow(m) == 0) return(NULL)
          a <- .reduce_dup(m[, c("trade", "comm", dim_col, "year", "slice"),
                             drop = FALSE])
          names(a)[names(a) == dim_col] <- "region"
          a
        }
        if (length(nz_comm(pCsrc2Ainp)) > 0 ||
            length(nz_comm(pCdst2Ainp)) > 0) {
          mCsrc2Ainp <- .trade_aux_derived(pCsrc2Ainp, mTradeIr)
          mCdst2Ainp <- .trade_aux_derived(pCdst2Ainp, mTradeIr)
          scen <- setm(scen, "mTradeIrCsrc2Ainp", mCsrc2Ainp)
          scen <- setm(scen, "mTradeIrCdst2Ainp", mCdst2Ainp)
          mvAInp <- .reduce_dup(rbind(proj_region(mCsrc2Ainp, "src"),
                                      proj_region(mCdst2Ainp, "dst")))
          scen <- setm(scen, "mvTradeIrAInp", mvAInp)
        }
        if (length(nz_comm(pCsrc2Aout)) > 0 ||
            length(nz_comm(pCdst2Aout)) > 0) {
          mCsrc2Aout <- .trade_aux_derived(pCsrc2Aout, mTradeIr)
          mCdst2Aout <- .trade_aux_derived(pCdst2Aout, mTradeIr)
          scen <- setm(scen, "mTradeIrCsrc2Aout", mCsrc2Aout)
          scen <- setm(scen, "mTradeIrCdst2Aout", mCdst2Aout)
          mvAOut <- .reduce_dup(rbind(proj_region(mCsrc2Aout, "src"),
                                      proj_region(mCdst2Aout, "dst")))
          scen <- setm(scen, "mvTradeIrAOut", mvAOut)
        }
      }
    }
  }

  ## --- inter-regional trade aux totals ------------------------------------
  mvTradeIrAInp <- gds("mvTradeIrAInp")
  if ("mvTradeIrAInpTot" %in% want && !is.null(mvTradeIrAInp)) {
    scen <- setm(scen, "mvTradeIrAInpTot",
                 .reduce_total_map(.reduce_sect(mvTradeIrAInp, ry_cols), csop))
  }
  mvTradeIrAOut <- gds("mvTradeIrAOut")
  if ("mvTradeIrAOutTot" %in% want && !is.null(mvTradeIrAOut)) {
    scen <- setm(scen, "mvTradeIrAOutTot",
                 .reduce_total_map(.reduce_sect(mvTradeIrAOut, ry_cols), csop))
  }

  ## --- import / export to Rest-of-World flow domains ----------------------
  # mExport / mImport: the (comm, region, year, slice) domains for the
  # Rest-of-World row exchange (mExportRow / mImportRow) unioned with the
  # inter-regional trade flows (mTradeIr x mTradeComm), each remapped to the
  # commodity's own slice resolution via mCommSliceOrParent. Faithful port of
  # legacy write.R L876-940. Export keeps the source region (src) and drops the
  # destination; import keeps the destination (dst) and drops the source. Built
  # unconditionally because the balance / coarse-slice maps below depend on
  # them; recipe reporting is satisfied via `.filter_derived_maps`.
  csop2 <- csop
  if (!is.null(csop2)) {
    names(csop2)[names(csop2) == "slicep"] <- "slice.1"
  }
  mTradeComm_d <- gds("mTradeComm")
  mTradeIr_d   <- gds("mTradeIr")
  io_row <- function(row_nm) {
    d <- gds(row_nm)
    if (is.null(d) || is.null(csop2)) return(NULL)
    x <- .reduce_sect(as.data.frame(d)[, ry_cols, drop = FALSE], ry_cols)
    names(x)[names(x) == "slice"] <- "slice.1"
    out <- as.data.frame(merge0(x, csop2))   # by comm, slice.1 -> native slice
    if (nrow(out) == 0) return(NULL)
    .reduce_dup(out[, ry_cols, drop = FALSE])
  }
  io_ir <- function(region_col, drop_col) {
    if (is.null(mTradeComm_d) || is.null(mTradeIr_d) || is.null(csop2)) {
      return(NULL)
    }
    trd <- as.data.frame(merge0(mTradeComm_d, mTradeIr_d))
    names(trd)[names(trd) == "slice"] <- "slice.1"
    trd[[drop_col]] <- NULL
    names(trd)[names(trd) == region_col] <- "region"
    out <- as.data.frame(merge0(trd, csop2))  # by comm, slice.1 -> native slice
    if (nrow(out) == 0) return(NULL)
    .reduce_dup(out[, ry_cols, drop = FALSE])
  }
  mExport <- .reduce_dup(dplyr::bind_rows(io_row("mExportRow"),
                                          io_ir("src", "dst")))
  scen <- setm_any(scen, "mExport", mExport)
  mImport <- .reduce_dup(dplyr::bind_rows(io_row("mImportRow"),
                                          io_ir("dst", "src")))
  scen <- setm_any(scen, "mImport", mImport)

  ## --- coarser-than-native flow substitution chain ------------------------
  # for2Lo: for each commodity, its native slice's *parent* slice (one level
  # up). Used to detect flow totals tagged at a coarser slice than the
  # commodity's own resolution (e.g. an ANNUAL output of a seasonal commodity),
  # which must be redistributed to the commodity's native slices through a
  # substitution variable. Faithful port of legacy write.R L1024-1090.
  #
  # mInp2Lo / mOut2Lo  : flow-total rows whose slice is coarser than native.
  # mvInp2Lo / mvOut2Lo: those rows expanded to (coarse slice, native slice.1).
  # mInpSub / mOutSub  : the (comm, region, year, native slice) substitution
  #                      domain (recipe "value" in the spec, but built here
  #                      because it depends on these filter maps; recipe_value
  #                      skips them).
  spc <- gds("mSliceParentChild")          # (slice = parent, slicep = child)
  if (!is.null(comm_slice) && !is.null(spc)) {
    a1 <- comm_slice
    colnames(a1)[2] <- "slicep"
    for2Lo <- as.data.frame(merge0(a1, spc, by = "slicep"))
    for2Lo$slicep <- NULL
    for2Lo <- .reduce_dup(for2Lo)            # (comm, slice = parent)

    build_2lo <- function(pieces) {
      fr <- Filter(function(x) !is.null(x) && nrow(as.data.frame(x)) > 0,
                   pieces)
      if (length(fr) == 0 || is.null(for2Lo) || nrow(for2Lo) == 0) return(NULL)
      fr <- lapply(fr, function(x) {
        x <- as.data.frame(x)
        x[, ry_cols, drop = FALSE]
      })
      base <- .reduce_dup(dplyr::bind_rows(fr))
      out <- as.data.frame(merge0(base, for2Lo, by = c("comm", "slice")))
      out <- out[, ry_cols, drop = FALSE]
      out <- out[!is.na(out$comm) & !is.na(out$slice), , drop = FALSE]
      if (nrow(out) == 0) return(NULL)
      # drop rows already at the commodity's native slice level
      key  <- paste0(out$comm, "#", out$slice)
      nkey <- paste0(comm_slice$comm, "#", comm_slice$slice)
      out <- out[!(key %in% nkey), , drop = FALSE]
      if (nrow(out) == 0) NULL else out
    }
    expand_2lo <- function(m2lo) {
      if (is.null(m2lo) || nrow(m2lo) == 0) return(NULL)
      mv <- as.data.frame(merge0(m2lo, spc))
      mv <- mv[, c("comm", "region", "year", "slice", "slicep"),
               drop = FALSE]
      cs2 <- comm_slice
      colnames(cs2)[2] <- "slicep"
      mv <- as.data.frame(merge0(mv, cs2))
      colnames(mv)[colnames(mv) == "slicep"] <- "slice.1"
      mv[, c("comm", "region", "year", "slice", "slice.1"), drop = FALSE]
    }
    sub_from <- function(mv) {
      if (is.null(mv) || nrow(mv) == 0) return(NULL)
      s <- mv[!duplicated(mv[, -4]), -4, drop = FALSE]
      colnames(s)[4] <- "slice"
      s
    }

    m_inp2lo <- build_2lo(list(
      gds("mTechInpTot"), gds("mStorageInpTot"),
      gds("mExport"), gds("mvTradeIrAInpTot")
    ))
    m_out2lo <- build_2lo(list(
      gds("mSupOutTot"), gds("mEmsFuelTot"), gds("mAggOut"),
      gds("mTechOutTot"), gds("mStorageOutTot"),
      gds("mImport"), gds("mvTradeIrAOutTot")
    ))
    scen <- setm(scen, "mInp2Lo", m_inp2lo)
    scen <- setm(scen, "mOut2Lo", m_out2lo)

    mv_inp2lo <- expand_2lo(m_inp2lo)
    mv_out2lo <- expand_2lo(m_out2lo)
    scen <- setm(scen, "mvInp2Lo", mv_inp2lo)
    scen <- setm(scen, "mvOut2Lo", mv_out2lo)

    scen <- setm_any(scen, "mInpSub", sub_from(mv_inp2lo))
    scen <- setm_any(scen, "mOutSub", sub_from(mv_out2lo))
  }

  ## --- commodity balance domains ------------------------------------------
  # mvInpTot / mvOutTot: union of all input- / output-side flow totals,
  # restricted to each commodity's own slice level via mCommSlice. Upstream
  # pieces still pending (mDummyExport/Import, mExport/mImport, mInpSub/mOutSub)
  # are absent here and simply contribute no rows.
  restrict_comm_slice <- function(df) {
    if (is.null(df) || nrow(df) == 0 || is.null(comm_slice)) return(df)
    dplyr::distinct(as.data.frame(merge0(df, comm_slice)))
  }
  inptot <- bind_ry(
    gds("mvDemInp"), gds("mDummyExport"), gds("mTechInpTot"),
    gds("mStorageInpTot"), gds("mExport"), gds("mvTradeIrAInpTot"),
    gds("mInpSub")
  )
  inptot <- restrict_comm_slice(inptot)
  outtot <- bind_ry(
    gds("mDummyImport"), gds("mSupOutTot"), gds("mEmsFuelTot"),
    gds("mAggOut"), gds("mTechOutTot"), gds("mStorageOutTot"),
    gds("mImport"), gds("mvTradeIrAOutTot"), gds("mOutSub")
  )
  outtot <- restrict_comm_slice(outtot)

  scen <- setm(scen, "mvInpTot", inptot)
  scen <- setm(scen, "mvOutTot", outtot)

  if ("mInpTotRY" %in% want && !is.null(inptot) && nrow(inptot) > 0) {
    scen <- setm(scen, "mInpTotRY",
                 dplyr::distinct(dplyr::select(inptot, -dplyr::any_of("slice"))))
  }
  if ("mOutTotRY" %in% want && !is.null(outtot) && nrow(outtot) > 0) {
    scen <- setm(scen, "mOutTotRY",
                 dplyr::distinct(dplyr::select(outtot, -dplyr::any_of("slice"))))
  }

  bal <- bind_ry(inptot, outtot)
  scen <- setm(scen, "mvBalance", bal)
  if ("mBalanceRY" %in% want && !is.null(bal) && nrow(bal) > 0) {
    scen <- setm(scen, "mBalanceRY",
                 dplyr::distinct(dplyr::select(bal, -dplyr::any_of("slice"))))
  }

  ## --- import / export / dummy / trade cost domains -----------------------
  # Cost maps (recipe "value" in the spec) derived from the flow-domain filter
  # maps. Built here because recipe_value runs before recipe_filter, so the
  # source maps are not yet available there; recipe_value skips them (see
  # .value_maps_built_in_filter). Faithful port of legacy write.R L832-846
  # (dummy) and L1101-1145 (row / inter-regional).
  dregionyear <- if (length(regions) > 0 && length(milestones) > 0) {
    expand.grid(region = as.character(regions), year = as.integer(milestones),
                stringsAsFactors = FALSE)
  } else {
    NULL
  }
  # mDummy*Cost: project the dummy flow domain onto (comm, region, year).
  cost_sect <- function(scen, src_nm, nm) {
    d <- gds(src_nm)
    p <- scen@modInp@parameters[[nm]]
    if (is.null(d) || is.null(p)) return(scen)
    setm_any(scen, nm, .reduce_sect(d, p@dimSets))
  }
  scen <- cost_sect(scen, "mDummyImport", "mDummyImportCost")
  scen <- cost_sect(scen, "mDummyExport", "mDummyExportCost")
  # mImportRowCost / mExportRowCost: unique (key, region, year) of the row flow
  # domain, intersected with the model's region x year grid.
  cost_row <- function(scen, src_nm, nm) {
    d <- gds(src_nm)
    p <- scen@modInp@parameters[[nm]]
    if (is.null(d) || is.null(p) || is.null(dregionyear)) return(scen)
    x <- .reduce_sect(d, p@dimSets)
    setm_any(scen, nm, as.data.frame(merge0(x, dregionyear)))
  }
  scen <- cost_row(scen, "mImportRow", "mImportRowCost")
  scen <- cost_row(scen, "mExportRow", "mExportRowCost")
  # mImportIrCost / mExportIrCost: trade flow domain with the destination /
  # source region renamed to `region`, projected onto (trade, region, year).
  cost_ir <- function(scen, ren_from, nm) {
    mti <- gds("mTradeIr")
    p <- scen@modInp@parameters[[nm]]
    if (is.null(mti) || is.null(p) || is.null(dregionyear)) return(scen)
    names(mti)[names(mti) == ren_from] <- "region"
    keep <- intersect(p@dimSets, colnames(mti))
    x <- .reduce_sect(mti[, keep, drop = FALSE])
    setm_any(scen, nm, as.data.frame(merge0(x, dregionyear)))
  }
  scen <- cost_ir(scen, "dst", "mImportIrCost")
  scen <- cost_ir(scen, "src", "mExportIrCost")

  scen
}

recipe_filter <- function(scen, names, fmp) {
  want <- intersect(names, .filter_core_maps)
  if (length(want) > 0) {
    scen <- .build_filter_core(scen, want, fmp)
  }
  want_der <- intersect(names, .filter_derived_maps)
  if (length(want_der) > 0) {
    scen <- .build_filter_derived(scen, want_der, fmp)
  }
  pending <- setdiff(names, c(.filter_core_maps, .filter_derived_maps))
  if (length(pending) > 0) {
    message("recipe 'filter': ", length(pending),
            " mapping(s) pending engine implementation: ",
            paste(pending, collapse = ", "))
  }
  scen
}

# --------------------------------------------------------------------------- #
# Recipe "constraint": equation index domains derived from the activity / flow
# domains built by earlier recipes.
# --------------------------------------------------------------------------- #

# Constraint-recipe maps whose derivation is shared with a filter-recipe map and
# are therefore built inside recipe_filter (which runs before recipe_constraint).
# Listed here so recipe_constraint does not report them as pending.
.constraint_maps_built_in_filter <- c(
  "meqImportRowLo", "meqExportRowLo",
  "mImportRowCumUp", "mExportRowCumUp",
  "meqSupAvaLo"
)

# Constraint-recipe maps that are produced by earlier stages of the pipeline
# (commodity ob2mi / membership / trade-route assembly) rather than here.
.constraint_maps_built_elsewhere <- c(
  "mLoComm", "mUpComm", "mFxComm", "mTradeRoutes"
)

# Constraint-recipe maps that are declared as GLPK index sets but were never
# populated by the legacy pipeline (no `.dat2par` assignment exists for them in
# obj2modInp.R / write.R). They must stay empty to reproduce legacy behaviour.
.constraint_maps_empty_legacy <- c(
  "mTechAfUp", "mTechAfcUp"
)

# TODO(deprecate LEC): the "linked extreme capacity" (LEC) feature is being
# removed. Until the equation and its variables are deleted, emit these index
# maps as empty so the constraint never materialises. No mapping logic is
# derived for them on purpose. See data-raw/mapping_spec.yml (deprecated: yes).
.constraint_maps_deprecated <- c(
  "meqLECActivity", "mLECRegion"
)

# --------------------------------------------------------------------------- #
# Generic "domain x filtered-source" constraint-map registry.
#
# Most constraint index maps are the de-duplicated projection of either
#   * a base activity/flow domain (`domain`),
#   * a bound parameter or membership map (`source`), optionally filtered by
#     `type` (lo/up) and a value predicate (`drop`), or
#   * the intersection of the two via `merge0()`.
# onto the map's own dimension sets. `.build_constraint_join_map()` implements
# all three shapes; `.constraint_map_def` lists the regular cases.
#
# Entry fields:
#   domain : base map name whose rows seed the index, or NULL.
#   source : bound parameter / membership map name to intersect, or NULL.
#   types  : character vector of `type` values to keep from `source`, or NULL.
#   drop   : value predicate on `source` -- "lo" keeps value != 0,
#            "up" keeps value != Inf, NULL applies no value filter.
# --------------------------------------------------------------------------- #
.constraint_map_def <- list(
  # C1 commodity balance: restrict the balance domain to commodities that
  # carry a lower / upper / fixed external balance bound.
  meqBalLo = list(domain = "mvBalance", source = "mLoComm"),
  meqBalUp = list(domain = "mvBalance", source = "mUpComm"),
  meqBalFx = list(domain = "mvBalance", source = "mFxComm"),

  # C2 technology availability factors.
  meqTechAfLo     = list(domain = "mvTechAct", source = "pTechAf",
                         types = "lo", drop = "lo"),
  meqTechAfUp     = list(domain = "mvTechAct", source = "pTechAf",
                         types = "up", drop = "up"),
  meqTechAfsLo    = list(domain = "mTechSpan", source = "pTechAfs",
                         types = "lo", drop = "lo"),
  meqTechAfsUp    = list(domain = "mTechSpan", source = "pTechAfs",
                         types = "up", drop = "up"),
  meqTechAfcInpLo = list(domain = "mvTechInp", source = "pTechAfc",
                         types = "lo", drop = "lo"),
  meqTechAfcInpUp = list(domain = "mvTechInp", source = "pTechAfc",
                         types = "up", drop = "up"),
  meqTechAfcOutLo = list(domain = "mvTechOut", source = "pTechAfc",
                         types = "lo", drop = "lo"),
  meqTechAfcOutUp = list(domain = "mvTechOut", source = "pTechAfc",
                         types = "up", drop = "up"),

  # C4 technology capacity / retirement bounds (no value predicate: legacy
  # keeps every row after the type filter).
  mTechCapLo    = list(domain = "mTechSpan", source = "pTechCap", types = "lo"),
  mTechCapUp    = list(domain = "mTechSpan", source = "pTechCap", types = "up"),
  mTechNewCapLo = list(domain = "mTechNew",  source = "pTechNewCap",
                       types = "lo"),
  mTechNewCapUp = list(domain = "mTechNew",  source = "pTechNewCap",
                       types = "up"),
  # Legacy gates mTechRet* on the global config flag `optimizeRetirement`
  # (storage / trade retirement maps are NOT gated this way).
  mTechRetLo    = list(domain = "mTechSpan", source = "pTechRet", types = "lo",
                       gate = "optimizeRetirement"),
  mTechRetUp    = list(domain = "mTechSpan", source = "pTechRet", types = "up",
                       gate = "optimizeRetirement"),

  # C5 storage activity bounds (the storage balance map meqStorageStore is
  # bespoke -- see `.build_meqStorageStore`).
  meqStorageAfLo  = list(domain = "mvStorageStore", source = "pStorageAf",
                         types = "lo", drop = "lo"),
  meqStorageAfUp  = list(domain = "mvStorageStore", source = "pStorageAf",
                         types = "up", drop = "up"),
  meqStorageInpLo = list(domain = "mvStorageStore", source = "pStorageCinp",
                         types = "lo", drop = "lo"),
  meqStorageInpUp = list(domain = "mvStorageStore", source = "pStorageCinp",
                         types = "up", drop = "up"),
  meqStorageOutLo = list(domain = "mvStorageStore", source = "pStorageCout",
                         types = "lo", drop = "lo"),
  meqStorageOutUp = list(domain = "mvStorageStore", source = "pStorageCout",
                         types = "up", drop = "up"),

  # C6 storage capacity / retirement bounds.
  mStorageCapLo    = list(domain = "mStorageSpan", source = "pStorageCap",
                          types = "lo"),
  mStorageCapUp    = list(domain = "mStorageSpan", source = "pStorageCap",
                          types = "up"),
  mStorageNewCapLo = list(domain = "mStorageNew",  source = "pStorageNewCap",
                          types = "lo"),
  mStorageNewCapUp = list(domain = "mStorageNew",  source = "pStorageNewCap",
                          types = "up"),
  mStorageRetLo    = list(domain = "mStorageSpan", source = "pStorageRet",
                          types = "lo"),
  mStorageRetUp    = list(domain = "mStorageSpan", source = "pStorageRet",
                          types = "up"),

  # C7 trade capacity / retirement bounds (trade flow & cap-flow maps are
  # bespoke / pending -- see recipe_constraint).
  mTradeCapLo    = list(domain = "mTradeSpan", source = "pTradeCap",
                        types = "lo"),
  mTradeCapUp    = list(domain = "mTradeSpan", source = "pTradeCap",
                        types = "up"),
  mTradeNewCapLo = list(domain = "mTradeNew",  source = "pTradeNewCap",
                        types = "lo"),
  mTradeNewCapUp = list(domain = "mTradeNew",  source = "pTradeNewCap",
                        types = "up"),
  mTradeRetLo    = list(domain = "mTradeSpan", source = "pTradeRet",
                        types = "lo"),
  mTradeRetUp    = list(domain = "mTradeSpan", source = "pTradeRet",
                        types = "up"),

  # C7 trade inter-regional flow bounds: restrict the trade-flow domain to the
  # routes / slices that carry a lower / upper flow bound. The flow parameter
  # carries no `comm`; it is supplied by the `mvTradeIr` domain via merge0.
  meqTradeFlowLo = list(domain = "mvTradeIr", source = "pTradeIr",
                        types = "lo", drop = "lo"),
  meqTradeFlowUp = list(domain = "mvTradeIr", source = "pTradeIr",
                        types = "up", drop = "up"),

  # C8 supply reserve margins: direct projection of the bound parameter onto
  # its (sup, comm, region) dimensions -- no base domain to intersect.
  meqSupReserveLo = list(domain = NULL, source = "pSupReserve",
                         types = "lo", drop = "lo"),
  mSupReserveUp   = list(domain = NULL, source = "pSupReserve",
                         types = "up", drop = "up")
)

# Build one regular constraint map from a `.constraint_map_def` entry.
.build_constraint_join_map <- function(scen, name, def, fmp) {
  p <- scen@modInp@parameters[[name]]
  if (is.null(p)) return(scen)
  if (!is.null(def$gate) && !isTRUE(slot(scen@settings, def$gate))) return(scen)

  # Optional bound parameter / membership source, type- and value-filtered.
  src <- NULL
  if (!is.null(def$source)) {
    sp <- scen@modInp@parameters[[def$source]]
    if (is.null(sp)) return(scen)
    src <- get_data_slot(sp)
    if (is.null(src) || nrow(src) == 0) return(scen)
    src <- as.data.frame(src)
    if (!is.null(def$types) && !is.null(src$type)) {
      src <- src[src$type %in% def$types, , drop = FALSE]
    }
    if (!is.null(src$value)) {
      if (identical(def$drop, "lo")) {
        src <- src[src$value != 0, , drop = FALSE]
      } else if (identical(def$drop, "up")) {
        src <- src[src$value != Inf, , drop = FALSE]
      }
    }
    if (nrow(src) == 0) return(scen)
    src <- src[, setdiff(colnames(src), c("type", "value")), drop = FALSE]
    src <- dplyr::distinct(src)
  }

  # Optional base activity / flow domain.
  dom <- NULL
  if (!is.null(def$domain)) {
    dp <- scen@modInp@parameters[[def$domain]]
    if (is.null(dp)) return(scen)
    dom <- get_data_slot(dp)
    if (is.null(dom) || nrow(dom) == 0) return(scen)
    dom <- as.data.frame(dom)
  }

  res <- if (is.null(dom)) {
    src
  } else if (is.null(src)) {
    dom
  } else {
    as.data.frame(merge0(dom, src))
  }
  if (is.null(res) || nrow(res) == 0) return(scen)

  df <- res |>
    dplyr::select(dplyr::any_of(p@dimSets)) |>
    dplyr::distinct()
  .set_map(scen, name, df, fmp)
}

# meqStorageStore: storage-balance index. Each storing slice is paired with the
# next slice (the inter-temporal storage link). Mirrors the legacy join in
# obj2modInp.R: mvStorageStore.slice == mSliceNext.slicep, taking mSliceNext's
# own slice as the "next" slice (renamed `slicep`).
.build_meqStorageStore <- function(scen, fmp) {
  name <- "meqStorageStore"
  p <- scen@modInp@parameters[[name]]
  if (is.null(p)) return(scen)
  store_par <- scen@modInp@parameters[["mvStorageStore"]]
  next_par <- scen@modInp@parameters[["mSliceNext"]]
  if (is.null(store_par) || is.null(next_par)) return(scen)
  store <- get_data_slot(store_par)
  snext <- get_data_slot(next_par)
  if (is.null(store) || nrow(store) == 0) return(scen)
  if (is.null(snext) || nrow(snext) == 0) return(scen)
  store <- as.data.frame(store)
  snext <- as.data.frame(snext)
  df <- store |>
    dplyr::left_join(snext, by = c(slice = "slicep"), suffix = c(".x", ".y")) |>
    dplyr::rename(slicep = "slice.y") |>
    dplyr::select(dplyr::any_of(p@dimSets)) |>
    dplyr::distinct()
  .set_map(scen, name, df, fmp)
}

# meqTradeCapFlow: links the trade capacity span to each traded slice and
# commodity. Mirrors the legacy composition merge0(mTradeSpan, mTradeSlice)
# with the commodity supplied by mTradeComm (legacy set comm = trd@commodity).
.build_meqTradeCapFlow <- function(scen, fmp) {
  name <- "meqTradeCapFlow"
  p <- scen@modInp@parameters[[name]]
  if (is.null(p)) return(scen)
  span_par <- scen@modInp@parameters[["mTradeSpan"]]
  slice_par <- scen@modInp@parameters[["mTradeSlice"]]
  comm_par <- scen@modInp@parameters[["mTradeComm"]]
  if (is.null(span_par) || is.null(slice_par) || is.null(comm_par)) return(scen)
  span <- get_data_slot(span_par)
  slc <- get_data_slot(slice_par)
  cmm <- get_data_slot(comm_par)
  if (is.null(span) || nrow(span) == 0) return(scen)
  if (is.null(slc) || nrow(slc) == 0) return(scen)
  if (is.null(cmm) || nrow(cmm) == 0) return(scen)
  df <- merge0(as.data.frame(span), as.data.frame(slc))
  df <- merge0(as.data.frame(df), as.data.frame(cmm))
  df <- as.data.frame(df) |>
    dplyr::select(dplyr::any_of(p@dimSets)) |>
    dplyr::distinct()
  .set_map(scen, name, df, fmp)
}

# mTradeCapacityVariable: trades whose capacity is an explicit decision
# variable (slot `capacityVariable == TRUE`). Membership-style projection of
# the trade name; mirrors the legacy per-trade `data.table(trade = trd@name)`.
.build_mTradeCapacityVariable <- function(scen, fmp) {
  name <- "mTradeCapacityVariable"
  p <- scen@modInp@parameters[[name]]
  if (is.null(p)) return(scen)
  res <- apply_to_scenario_data(
    scen = scen, classes = "trade", as_list = TRUE,
    func = function(obj) {
      if (!isTRUE(obj@capacityVariable)) return(NULL)
      out <- list()
      out[[obj@name]] <- data.frame(trade = obj@name, stringsAsFactors = FALSE)
      out
    }
  )
  if (length(res) == 0) return(scen)
  .set_map(scen, name, dplyr::bind_rows(res), fmp)
}

# --------------------------------------------------------------------------- #
# C3 technology commodity-grouping / share constraints.
#
# A technology may route several commodities through named input / output
# "groups". The equation index maps distinguish single-commodity ("Sng") from
# grouped ("Grp") inputs and outputs, plus the share bounds within a group.
# All maps are global de-duplicated joins of membership maps, activity / flow
# domains and the interpolated share / conversion parameters, mirroring the
# per-technology legacy derivation in obj2modInp.R (merge0 keyed on `tech`
# preserves the per-technology grouping in the global join).
# --------------------------------------------------------------------------- #
.build_tech_group_maps <- function(scen, names, fmp) {
  gdf <- function(nm) {
    p <- scen@modInp@parameters[[nm]]
    if (is.null(p)) return(NULL)
    d <- get_data_slot(p)
    if (is.null(d) || nrow(d) == 0) return(NULL)
    as.data.frame(d)
  }

  mvTechAct  <- gdf("mvTechAct")
  mvTechInp  <- gdf("mvTechInp")
  mvTechOut  <- gdf("mvTechOut")
  mTechOneComm   <- gdf("mTechOneComm")
  mTechInpGroup  <- gdf("mTechInpGroup")
  mTechOutGroup  <- gdf("mTechOutGroup")
  mTechGroupComm <- gdf("mTechGroupComm")
  pTechShare     <- gdf("pTechShare")
  pTechCinp2use  <- gdf("pTechCinp2use")
  pTechCact2cout <- gdf("pTechCact2cout")

  # Restrict a single-commodity domain to cells carrying a non-zero conversion
  # factor (mirrors the optional refinement in the legacy `techSing*`).
  refine <- function(dom, conv) {
    if (is.null(dom) || is.null(conv)) return(dom)
    if (is.null(conv$value)) return(dom)
    conv <- conv[conv$value != 0, , drop = FALSE]
    if (nrow(conv) == 0) return(NULL)
    keep <- intersect(colnames(dom), colnames(conv))
    conv <- dplyr::distinct(dplyr::select(conv, dplyr::all_of(keep)))
    out <- as.data.frame(merge0(dom, conv))
    if (nrow(out) == 0) return(NULL)
    out
  }
  nz <- function(df) if (is.null(df) || nrow(df) == 0) NULL else df

  # Single-commodity input / output domains.
  techSingInp <- if (!is.null(mvTechInp) && !is.null(mTechOneComm)) {
    nz(refine(as.data.frame(merge0(mvTechInp, mTechOneComm)), pTechCinp2use))
  } else NULL
  techSingOut <- if (!is.null(mvTechOut) && !is.null(mTechOneComm)) {
    nz(refine(as.data.frame(merge0(mvTechOut, mTechOneComm)), pTechCact2cout))
  } else NULL

  # Grouped input / output domains (domain x group-membership x group-commodity).
  techGroupInp <- if (!is.null(mvTechInp) && !is.null(mTechInpGroup) &&
                      !is.null(mTechGroupComm)) {
    nz(as.data.frame(merge0(as.data.frame(merge0(mvTechInp, mTechInpGroup)),
                            mTechGroupComm)))
  } else NULL
  techGroupOut <- if (!is.null(mvTechOut) && !is.null(mTechOutGroup) &&
                      !is.null(mTechGroupComm)) {
    nz(as.data.frame(merge0(as.data.frame(merge0(mvTechOut, mTechOutGroup)),
                            mTechGroupComm)))
  } else NULL

  # Share bounds (drop the value/type columns once filtered).
  share_map <- function(type) {
    if (is.null(pTechShare) || is.null(pTechShare$type)) return(NULL)
    s <- pTechShare[pTechShare$type == type & pTechShare$value > 0, ,
                    drop = FALSE]
    if (nrow(s) == 0) return(NULL)
    s[, setdiff(colnames(s), c("value", "type")), drop = FALSE]
  }
  has_groups <- !is.null(mTechInpGroup) || !is.null(mTechOutGroup)
  mpTechShareLo <- if (has_groups) share_map("lo") else NULL
  mpTechShareUp <- if (has_groups) share_map("up") else NULL

  set_if <- function(scen, nm, df) {
    if (!(nm %in% names) || is.null(df) || nrow(df) == 0) return(scen)
    .set_map(scen, nm, df, fmp)
  }

  # Activity index maps.
  scen <- set_if(scen, "meqTechActSng", techSingOut)
  if (!is.null(mTechOutGroup) && !is.null(mvTechAct)) {
    scen <- set_if(scen, "meqTechActGrp",
                   as.data.frame(merge0(mvTechAct, mTechOutGroup)))
  }

  # Cross input/output coupling maps.
  if (!is.null(mTechInpGroup) && !is.null(techSingOut)) {
    scen <- set_if(scen, "meqTechGrp2Sng",
                   as.data.frame(merge0(mTechInpGroup, techSingOut)))
  }
  if (!is.null(mTechOutGroup) && !is.null(techSingInp)) {
    scen <- set_if(scen, "meqTechSng2Grp",
                   as.data.frame(merge0(mTechOutGroup, techSingInp)))
  }
  if (!is.null(techSingInp) && !is.null(techSingOut)) {
    scen <- set_if(scen, "meqTechSng2Sng",
                   as.data.frame(merge0(
                     techSingInp, techSingOut,
                     by = c("tech", "region", "year", "slice"),
                     suffixes = c("", ".1"))))
  }
  if (!is.null(mTechInpGroup) && !is.null(mTechOutGroup) &&
      !is.null(mvTechAct)) {
    g2g <- as.data.frame(merge0(
      as.data.frame(merge0(mTechInpGroup, mTechOutGroup,
                           by = "tech", suffixes = c("", ".1"))),
      mvTechAct))
    scen <- set_if(scen, "meqTechGrp2Grp", g2g)
  }

  # Share index maps.
  if (!is.null(mpTechShareLo) && !is.null(techGroupOut)) {
    scen <- set_if(scen, "meqTechShareOutLo",
                   as.data.frame(merge0(mpTechShareLo, techGroupOut)))
  }
  if (!is.null(mpTechShareUp) && !is.null(techGroupOut)) {
    scen <- set_if(scen, "meqTechShareOutUp",
                   as.data.frame(merge0(mpTechShareUp, techGroupOut)))
  }
  if (!is.null(mpTechShareLo) && !is.null(techGroupInp)) {
    scen <- set_if(scen, "meqTechShareInpLo",
                   as.data.frame(merge0(mpTechShareLo, techGroupInp)))
  }
  if (!is.null(mpTechShareUp) && !is.null(techGroupInp)) {
    scen <- set_if(scen, "meqTechShareInpUp",
                   as.data.frame(merge0(mpTechShareUp, techGroupInp)))
  }
  scen
}

# Names handled by the C3 group/share builder.
.tech_group_maps <- c(
  "meqTechActSng", "meqTechActGrp", "meqTechGrp2Sng", "meqTechSng2Grp",
  "meqTechSng2Sng", "meqTechGrp2Grp", "meqTechShareInpLo", "meqTechShareInpUp",
  "meqTechShareOutLo", "meqTechShareOutUp"
)

# --------------------------------------------------------------------------- #
# Ramping constraints (C-ramp).
#
# `mTechRampUp` / `mTechRampDown` index the inter-slice ramping limits. Each map
# is the ramp parameter domain (tech/region/year/slice), restricted to the
# activity domain, with the "next" slice (`slicep`) appended. The next-slice map
# depends on whether a technology operates on the full-year cycle
# (`mSliceFYearNext`) or the regular slice cycle (`mSliceNext`); `fullYear` is a
# per-technology flag, so techs are split accordingly (mirrors the per-tech
# legacy `.add_ramp0`).
# --------------------------------------------------------------------------- #
.ramp_maps <- c("mTechRampUp", "mTechRampDown")

.build_ramp_maps <- function(scen, names, fmp) {
  gdf <- function(nm) {
    p <- scen@modInp@parameters[[nm]]
    if (is.null(p)) return(NULL)
    d <- get_data_slot(p)
    if (is.null(d) || nrow(d) == 0) return(NULL)
    as.data.frame(d)
  }

  # Per-technology fullYear flag.
  fy <- apply_to_scenario_data(
    scen = scen, classes = "technology", as_list = TRUE,
    func = function(obj) {
      out <- list()
      out[[obj@name]] <- data.frame(tech = obj@name,
                                    fullYear = isTRUE(obj@fullYear),
                                    stringsAsFactors = FALSE)
      out
    }
  )
  fy <- if (length(fy) == 0) NULL else dplyr::bind_rows(fy)
  fy_techs <- if (is.null(fy)) character(0) else fy$tech[fy$fullYear]

  mvTechAct   <- gdf("mvTechAct")
  sliceNextFY <- gdf("mSliceFYearNext")
  sliceNext   <- gdf("mSliceNext")

  build_one <- function(scen, src_name, map_name) {
    if (!(map_name %in% names)) return(scen)
    p <- scen@modInp@parameters[[map_name]]
    src <- gdf(src_name)
    if (is.null(src)) return(.set_map(scen, map_name, NULL, fmp))
    m <- src[, setdiff(colnames(src), "value"), drop = FALSE]
    # Restrict to the activity domain when the source carries fewer dimensions
    # than the target map (i.e. before `slicep` is appended).
    if (!is.null(mvTechAct) && ncol(m) != length(p@dimSets)) {
      m <- as.data.frame(merge0(m, mvTechAct))
    }
    if (nrow(m) == 0) return(.set_map(scen, map_name, NULL, fmp))
    join_next <- function(df, nextmap) {
      if (is.null(nextmap) || nrow(df) == 0) return(NULL)
      dplyr::left_join(df, nextmap, by = "slice")
    }
    fy_part <- if (length(fy_techs) > 0) {
      join_next(m[m$tech %in% fy_techs, , drop = FALSE], sliceNextFY)
    } else NULL
    no_part <- join_next(m[!(m$tech %in% fy_techs), , drop = FALSE], sliceNext)
    res <- dplyr::bind_rows(fy_part, no_part)
    if (is.null(res) || nrow(res) == 0) {
      return(.set_map(scen, map_name, NULL, fmp))
    }
    .set_map(scen, map_name,
             dplyr::distinct(dplyr::select(res, dplyr::any_of(p@dimSets))), fmp)
  }

  scen <- build_one(scen, "pTechRampUp", "mTechRampUp")
  scen <- build_one(scen, "pTechRampDown", "mTechRampDown")
  scen
}

#' Build constraint mappings (recipe 7)
#'
#' @param scen scenario object.
#' @param names character vector of constraint mapping names to build.
#' @param fmp function mapping a parameter name to its on-disk path.
#' @returns updated scenario object.
#' @keywords internal
recipe_constraint <- function(scen, names, fmp) {
  names <- unique(names)

  # 1. Regular "domain x filtered-source" maps from the registry.
  for (nm in intersect(names, names(.constraint_map_def))) {
    scen <- .build_constraint_join_map(scen, nm, .constraint_map_def[[nm]], fmp)
  }

  # 2. Bespoke maps.
  if ("meqStorageStore" %in% names) {
    scen <- .build_meqStorageStore(scen, fmp)
  }
  if ("meqTradeCapFlow" %in% names) {
    scen <- .build_meqTradeCapFlow(scen, fmp)
  }
  if ("mTradeCapacityVariable" %in% names) {
    scen <- .build_mTradeCapacityVariable(scen, fmp)
  }

  # C3 technology group / share maps (computed together from shared intermediates).
  if (length(intersect(names, .tech_group_maps)) > 0) {
    scen <- .build_tech_group_maps(scen, names, fmp)
  }

  # Ramping maps (per-technology next-slice attachment).
  if (length(intersect(names, .ramp_maps)) > 0) {
    scen <- .build_ramp_maps(scen, names, fmp)
  }

  # 3. Deprecated LEC maps: intentionally left empty (see TODO above).
  deprecated <- intersect(names, .constraint_maps_deprecated)
  if (length(deprecated) > 0) {
    message("recipe 'constraint': ", length(deprecated),
            " deprecated LEC map(s) emitted empty: ",
            paste(deprecated, collapse = ", "))
  }

  # 4. Report any remaining maps not yet implemented in the engine.
  handled <- c(
    names(.constraint_map_def), "meqStorageStore",
    "meqTradeCapFlow", "mTradeCapacityVariable", .tech_group_maps, .ramp_maps,
    .constraint_maps_built_in_filter, .constraint_maps_built_elsewhere,
    .constraint_maps_empty_legacy, .constraint_maps_deprecated
  )
  pending <- setdiff(names, handled)
  if (length(pending) > 0) {
    message("recipe 'constraint': ", length(pending),
            " mapping(s) pending engine implementation: ",
            paste(pending, collapse = ", "))
  }
  scen
}

# --------------------------------------------------------------------------- #
# Recipe: cost_agg (top-level cost aggregation domains)
# --------------------------------------------------------------------------- #

# NOTE: `recipe_cost_agg` + `.cost_agg_maps_empty_legacy` superseded by
# R/map_costagg.R (registry) and ARCHIVED to drafts/legacy-mapping/costagg.R.

# --------------------------------------------------------------------------- #
# Driver
# --------------------------------------------------------------------------- #

#' Build mapping parameters for a scenario, in recipe (dependency) order
#'
#' @param scen scenario object with sets already populated from model objects.
#' @param fmp function mapping a parameter name to its on-disk path. When
#'   `NULL`, parameters are kept in memory.
#' @param spec mapping specification (defaults to `load_mapping_spec()`).
#' @param recipes character vector of recipes to run (defaults to all, in
#'   `.mapping_recipe_order`).
#' @returns updated scenario object.
#' @export
build_mappings <- function(scen, fmp = NULL,
                           spec = load_mapping_spec(),
                           recipes = .mapping_recipe_order) {
  if (is.null(fmp)) fmp <- function(x) NULL
  by_recipe <- mappings_by_recipe(spec)
  builders <- .get_mapping_builders()

  for (rc in recipes) {
    nms <- by_recipe[[rc]]
    if (length(nms) == 0) next
    # Per-mapping registry takes precedence; registered names are built by their
    # map_<Name>() function, the rest fall back to this family's recipe_*().
    # Iterate in REGISTRY order (not spec order) so families with intra-family
    # dependencies (e.g. filter: mvTechAct before mvTechInp) build correctly via
    # their `.<family>_builders` list order.
    reg <- intersect(names(builders), nms)
    for (nm in reg) scen <- builders[[nm]](scen, fmp)
    nms <- setdiff(nms, reg)
    if (length(nms) == 0) next
    scen <- switch(rc,
      membership = recipe_membership(scen, nms, fmp),
      closure    = recipe_closure(scen, nms, fmp),
      calendar   = recipe_calendar(scen, nms, fmp),
      # lifespan fully migrated to R/map_lifespan.R (registry); no fallback.
      value      = recipe_value(scen, nms, fmp),
      filter     = recipe_filter(scen, nms, fmp),
      constraint = recipe_constraint(scen, nms, fmp),
      # cost_agg fully migrated to R/map_costagg.R (registry); no fallback.
      # The remaining recipes are implemented incrementally; until each is
      # wired in, report the pending mappings rather than failing the pipeline.
      {
        message("recipe '", rc, "': ", length(nms),
                " mapping(s) pending engine implementation")
        scen
      }
    )
  }
  scen
}
