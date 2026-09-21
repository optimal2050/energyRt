# =============================================================================#
# fold.R
#
# Reversible "fold / unfold" of modInp parameters along trimmable dimensions.
#
# A trimmable dimension (default: region, timeslice, vintage) is "folded" to a
# single wildcard (NA) row whenever, for a given entity, the parameter value
# does not vary across the *full* membership of that dimension. The reverse
# operation ("unfold") materialises the wildcard rows back to explicit members
# using the same per-entity membership maps.
#
# Mode "wildcard" (A, the default) keeps the dimension column in @dimSets and
# stores NA in the folded rows (exported as ANY* tokens later). Mode "drop"
# (B, reducing the parameter arity) is reserved for a later round.
#
# Fold metadata is stored in `@misc$fold_info`, mirroring multimod's shape:
#   list(folded, mode, original_dims, wildcard_dims, original_rows,
#        folded_rows, tol)
# =============================================================================#

# Default trimmable dimensions, in stable folding order.
.fold_trim_dims <- c("region", "timeslice", "vintage")

# All dimensions that MAY be folded (whole-column), in stable order. region/timeslice
# are the original trimmable dims; year/comm/tech/stg/trade are opt-in via the
# `fold` argument. The artificial-member layer (fold_artificial.R `.fold_any`)
# must define a wildcard for each.
.foldable_dims <- c("region", "timeslice", "year", "comm", "tech", "stg", "trade")

# Identity / axis dimensions that must NEVER be folded.
.fold_protected_dims <- c(
  "commp", "process", "sup", "imp", "expp", "dem", "weather", "yearp", "type"
)

# Unfold order: entity dims first (their membership is the full set), then
# region, then the dims whose membership is keyed on entity and region.
.unfold_order <- c("comm", "tech", "stg", "trade", "region", "timeslice",
                   "year", "vintage")

# -----------------------------------------------------------------------------#
# .read_map: read a mapping/set parameter's data as a plain data.frame.
# Returns NULL when the map is absent or empty.
# -----------------------------------------------------------------------------#
.read_map <- function(scen, nm) {
  p <- scen@modInp@parameters[[nm]]
  if (is.null(p)) {
    return(NULL)
  }
  d <- get_data_slot(p)
  if (is.null(d) || nrow(d) == 0) {
    return(NULL)
  }
  as.data.frame(d)
}

# -----------------------------------------------------------------------------#
# .timeslice_allowed: per-entity timeslice membership for the given identity key.
# Returns a data.frame(<key>, timeslice) or NULL when unavailable.
# -----------------------------------------------------------------------------#
.timeslice_allowed <- function(scen, key) {
  pick <- function(df, cols) {
    if (is.null(df)) {
      return(NULL)
    }
    if (!all(cols %in% names(df))) {
      return(NULL)
    }
    dplyr::distinct(df[, cols, drop = FALSE])
  }
  switch(key,
    tech  = pick(.read_map(scen, "mTechTimeslice"), c("tech", "timeslice")),
    sup   = pick(.read_map(scen, "mSupTimeslice"), c("sup", "timeslice")),
    trade = pick(.read_map(scen, "mTradeTimeslice"), c("trade", "timeslice")),
    imp   = pick(.read_map(scen, "mImpTimeslice"), c("imp", "timeslice")),
    expp  = pick(.read_map(scen, "mExpTimeslice"), c("expp", "timeslice")),
    comm  = pick(.read_map(scen, "mCommTimeslice"), c("comm", "timeslice")),
    stg   = {
      sc <- .read_map(scen, "mStorageComm")
      cs <- .read_map(scen, "mCommTimeslice")
      if (is.null(sc) || is.null(cs)) {
        NULL
      } else if (!all(c("stg", "comm") %in% names(sc)) ||
                 !all(c("comm", "timeslice") %in% names(cs))) {
        NULL
      } else {
        dplyr::distinct(dplyr::inner_join(
          sc[, c("stg", "comm")], cs[, c("comm", "timeslice")],
          by = "comm"
        )[, c("stg", "timeslice")])
      }
    },
    NULL
  )
}

# -----------------------------------------------------------------------------#
# .region_allowed: per-entity operative regions for the given identity key.
# Returns a data.frame(<key>, region) or NULL when unavailable.
# -----------------------------------------------------------------------------#
.region_allowed <- function(scen, key) {
  pick <- function(nm, cols) {
    df <- .read_map(scen, nm)
    if (is.null(df) || !all(cols %in% names(df))) {
      return(NULL)
    }
    dplyr::distinct(df[, cols, drop = FALSE])
  }
  switch(key,
    tech  = pick("mTechSpan", c("tech", "region")),
    stg   = pick("mStorageSpan", c("stg", "region")),
    sup   = pick("mSupSpan", c("sup", "region")),
    comm  = pick("mCommReg", c("comm", "region")),
    trade = {
      # A trade object operates over both endpoints of its routes, so its
      # region membership is the union of the source and destination regions
      # (`region in trade object = src + dst`). A NA region in a trade cost
      # therefore unfolds to every region the trade connects.
      rt <- .read_map(scen, "mTradeRoutes")
      if (is.null(rt) || !all(c("trade", "src", "dst") %in% names(rt))) {
        NULL
      } else {
        dplyr::distinct(dplyr::bind_rows(
          dplyr::transmute(rt, trade = .data$trade, region = .data$src),
          dplyr::transmute(rt, trade = .data$trade, region = .data$dst)
        ))
      }
    },
    NULL
  )
}

# -----------------------------------------------------------------------------#
# .year_allowed: per-entity operative years (from the lifespan span maps).
# Returns a data.frame(<keys>, year) or NULL.
# -----------------------------------------------------------------------------#
.year_allowed <- function(scen, key) {
  pick <- function(nm, cols) {
    df <- .read_map(scen, nm)
    if (is.null(df) || !all(cols %in% names(df))) return(NULL)
    dplyr::distinct(df[, cols, drop = FALSE])
  }
  switch(key,
    tech  = pick("mTechSpan",    c("tech", "region", "year")),
    stg   = pick("mStorageSpan", c("stg", "region", "year")),
    trade = pick("mTradeSpan",   c("trade", "year")),
    NULL
  )
}

# .entity_allowed: full set of a top-level entity dim (tech / stg / trade / comm),
# read from the set parameter. A param folds on such a dim only when it is uniform
# across the ENTIRE set, so the wildcard never over-claims a missing member.
.entity_allowed <- function(scen, dim) {
  p <- scen@modInp@parameters[[dim]]
  if (is.null(p)) return(NULL)
  d <- get_data_slot(p)
  if (is.null(d) || nrow(d) == 0) return(NULL)
  stats::setNames(dplyr::distinct(as.data.frame(d)[, 1, drop = FALSE]), dim)
}

# -----------------------------------------------------------------------------#
# .fold_member_sets: build per-dimension membership maps for one parameter's
# data. Picks the most specific identity key present for each trimmable dim.
# Returns a named list: dim -> data.frame(<key>, <dim>).
# -----------------------------------------------------------------------------#
.fold_member_sets <- function(scen, data, dims = .fold_trim_dims) {
  ms <- list()
  cols <- names(data)

  if ("timeslice" %in% dims && "timeslice" %in% cols) {
    for (k in c("tech", "sup", "stg", "trade", "imp", "expp", "comm")) {
      if (!k %in% cols) next
      a <- .timeslice_allowed(scen, k)
      if (!is.null(a)) {
        ms$timeslice <- a
        break
      }
    }
  }

  if ("region" %in% dims && "region" %in% cols) {
    for (k in c("tech", "stg", "sup", "trade", "comm")) {
      if (!k %in% cols) next
      a <- .region_allowed(scen, k)
      if (!is.null(a)) {
        ms$region <- a
        break
      }
    }
  }

  if ("year" %in% dims && "year" %in% cols) {
    for (k in c("tech", "stg", "trade")) {
      if (!k %in% cols) next
      a <- .year_allowed(scen, k)
      if (!is.null(a)) {
        ms$year <- a
        break
      }
    }
  }

  # Top-level entity dims (comm / tech / stg / trade): membership is the full set;
  # the whole-column fold then fires only when the parameter is uniform across the
  # ENTIRE entity set (handled by the global branch of `.fold_one_dim`).
  for (dd in c("comm", "tech", "stg", "trade")) {
    if (dd %in% dims && dd %in% cols) {
      a <- .entity_allowed(scen, dd)
      if (!is.null(a)) ms[[dd]] <- a
    }
  }

  # Full-set fallback for region / timeslice / year when no per-entity membership was
  # found (e.g. weather parameters, which apply to every region/year). Folding a
  # column that covers the COMPLETE set uniformly can never over-claim, so it is
  # safe: e.g. `pWeather`, identical across all years, folds year -> the wildcard.
  for (dd in intersect(dims, c("region", "timeslice", "year"))) {
    if (dd %in% cols && is.null(ms[[dd]])) {
      a <- .entity_allowed(scen, dd)
      if (!is.null(a)) ms[[dd]] <- a
    }
  }

  ms
}

# -----------------------------------------------------------------------------#
# .fold_one_dim: fold a single dimension to a wildcard (NA) row, per group.
#
# A group is the combination of all columns except `dim` and the value column.
# Within a group the dimension is folded iff:
#   - no member is already NA / ANY* (group is fully explicit),
#   - the present members equal the entity's full allowed set, and
#   - the value is uniform across the group (range <= tol).
#
# Returns list(data = <data.frame>, folded = <logical>).
# -----------------------------------------------------------------------------#
.fold_one_dim <- function(data, dim, allowed, tol = 1e-10,
                          value_col = "value") {
  if (!dim %in% names(data) || is.null(allowed) || !dim %in% names(allowed)) {
    return(list(data = data, folded = FALSE))
  }
  if (!value_col %in% names(data)) {
    return(list(data = data, folded = FALSE))
  }

  d <- as.data.frame(data)
  group_cols <- setdiff(names(d), c(dim, value_col))
  shared <- intersect(names(allowed), group_cols)
  # A shared key whose column is already entirely wildcard (folded earlier)
  # cannot join the membership. Project the membership over the remaining keys:
  # the wildcard row stands for every member of that key, so its allowed set is
  # the union across it. The whole-column rule below still applies, so the fold
  # stays conservative, and the result no longer depends on the order in which
  # the dimensions are folded (region then year, or the reverse).
  wild_keys <- shared[vapply(shared, function(k) {
    all(is.na(d[[k]]) | is_any(d[[k]]))
  }, logical(1))]
  if (length(wild_keys)) {
    shared <- setdiff(shared, wild_keys)
    allowed <- dplyr::distinct(
      allowed[, setdiff(names(allowed), wild_keys), drop = FALSE])
  }
  # Global membership: `allowed` is the full entity set (only the `dim` column, no
  # parent key). The fold then fires per group only when the group covers the
  # ENTIRE set uniformly.
  global <- length(setdiff(names(allowed), dim)) == 0
  if ((!global && length(shared) == 0) || length(group_cols) == 0) {
    return(list(data = d, folded = FALSE))
  }

  is_wild <- is.na(d[[dim]]) | is_any(d[[dim]])

  # Groups that already contain a wildcard member are ambiguous -> skip.
  na_groups <- dplyr::distinct(d[is_wild, group_cols, drop = FALSE])

  # Value uniformity per group (only explicit rows matter for folding).
  uni <- d |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
    dplyr::summarise(
      .rng = if (dplyr::n() <= 1) 0 else diff(range(.data[[value_col]])),
      .groups = "drop"
    ) |>
    dplyr::filter(.data$.rng <= tol)

  # Present (explicit) members per group.
  pres <- dplyr::distinct(
    d[!is_wild, c(group_cols, dim), drop = FALSE]
  )
  if (nrow(pres) == 0) {
    return(list(data = d, folded = FALSE))
  }
  pres_cnt <- pres |>
    dplyr::count(dplyr::across(dplyr::all_of(group_cols)), name = ".npres")

  allw <- dplyr::distinct(allowed[, unique(c(shared, dim)), drop = FALSE])

  nmatch <- pres |>
    dplyr::inner_join(allw, by = c(shared, dim)) |>
    dplyr::count(dplyr::across(dplyr::all_of(group_cols)), name = ".nmatch")

  if (global) {
    nall_g <- nrow(allw)
    fold_groups <- pres_cnt |>
      dplyr::left_join(nmatch, by = group_cols) |>
      dplyr::mutate(.nmatch = dplyr::coalesce(.data$.nmatch, 0L)) |>
      dplyr::filter(.data$.npres == nall_g, .data$.nmatch == nall_g)
  } else {
    allw_cnt <- allw |>
      dplyr::count(dplyr::across(dplyr::all_of(shared)), name = ".nall")
    fold_groups <- pres_cnt |>
      dplyr::left_join(allw_cnt, by = shared) |>
      dplyr::left_join(nmatch, by = group_cols) |>
      dplyr::mutate(.nmatch = dplyr::coalesce(.data$.nmatch, 0L)) |>
      dplyr::filter(
        !is.na(.data$.nall),
        .data$.npres == .data$.nall,
        .data$.nmatch == .data$.nall
      )
  }
  fold_groups <- fold_groups |>
    dplyr::semi_join(uni, by = group_cols) |>
    dplyr::anti_join(na_groups, by = group_cols) |>
    dplyr::select(dplyr::all_of(group_cols))

  if (nrow(fold_groups) == 0) {
    return(list(data = d, folded = FALSE))
  }

  # Whole-column fold only: the dimension folds for this parameter ONLY when its
  # ENTIRE column collapses to the wildcard (every explicit group folds). A mixed
  # NA / explicit column cannot be represented by the single artificial set member
  # (`ANYREGION` / `ANYTIMESLICE`) the writers substitute, so if any explicit group is
  # left un-folded, fold nothing for this dimension.
  explicit_groups <- dplyr::distinct(d[!is_wild, group_cols, drop = FALSE])
  if (nrow(fold_groups) < nrow(explicit_groups)) {
    return(list(data = d, folded = FALSE))
  }

  na_val <- as(NA, class(d[[dim]])[1])
  folded <- d |>
    dplyr::semi_join(fold_groups, by = group_cols) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
    dplyr::summarise(
      !!value_col := dplyr::first(.data[[value_col]]),
      .groups = "drop"
    ) |>
    dplyr::mutate(!!dim := na_val)

  kept <- dplyr::anti_join(d, fold_groups, by = group_cols)
  out <- dplyr::bind_rows(kept, folded)
  out <- as.data.frame(out)[, names(d), drop = FALSE]

  list(data = out, folded = TRUE)
}

# -----------------------------------------------------------------------------#
# .fold_write_back: persist new parameter data, mirroring interpolate_parameters.
# -----------------------------------------------------------------------------#
.fold_write_back <- function(param, new_data) {
  new_data <- new_data |>
    force_cols_classes() |>
    as.data.frame() |>
    (\(d) d[, colnames(param@data), drop = FALSE])()

  if (isOnDisk(param)) {
    ppath <- getObjPath(param)
    if (is.null(ppath)) {
      stop("On-disk parameter '", param@name, "' has no path for fold write-back.")
    }
    data_dir <- file.path(ppath, "data")
    # Keep the store's own codec (see .interp_write_param): re-deriving it
    # recognised only parquet-or-csv, so a feather store got CSV written
    # beside its `.arrow` files and stopped being a readable dataset.
    fmt <- .store_format(data_dir)
    unlink(data_dir, recursive = TRUE)
    data2disk(data.table::as.data.table(new_data), path = data_dir,
              format = fmt)
    param@data <- reset_slot(data.table::as.data.table(new_data))
  } else {
    param@data <- as.data.frame(new_data)
  }
  param
}

# -----------------------------------------------------------------------------#
# fold_parameter: fold trimmable dimensions of a single parameter to wildcards.
#
# `member_sets` is a named list mapping a trimmable dim to a data.frame holding
# the allowed members per entity (e.g. list(timeslice = mTechTimeslice)). Dims without
# a member set are left untouched (safe no-op).
# -----------------------------------------------------------------------------#
fold_parameter <- function(param, member_sets = list(),
                           dims = .fold_trim_dims, mode = "wildcard",
                           tol = 1e-10) {
  mode <- match.arg(mode, c("wildcard", "drop"))
  if (mode == "drop") {
    stop("fold_parameter(mode = 'drop') is not implemented yet.")
  }

  data <- get_data_slot(param)
  if (is.null(data) || nrow(data) == 0) {
    return(param)
  }
  data <- as.data.frame(data)
  original_rows <- nrow(data)

  cand <- intersect(dims, names(data))
  cand <- setdiff(cand, .fold_protected_dims)
  cand <- intersect(cand, names(member_sets))
  if (length(cand) == 0) {
    return(param)
  }

  wildcard <- character(0)
  repeat {
    changed <- FALSE
    for (dm in cand) {
      res <- .fold_one_dim(data, dm, member_sets[[dm]], tol = tol)
      if (res$folded) {
        data <- res$data
        changed <- TRUE
        wildcard <- union(wildcard, dm)
      }
    }
    if (!changed) break
  }

  if (length(wildcard) == 0) {
    return(param)
  }

  param <- .fold_write_back(param, data)

  if (!is.list(param@misc)) {
    param@misc <- list()
  }
  param@misc$fold_info <- list(
    folded = TRUE,
    mode = mode,
    original_dims = param@dimSets,
    wildcard_dims = wildcard,
    original_rows = original_rows,
    folded_rows = nrow(data),
    tol = tol
  )

  param
}

# -----------------------------------------------------------------------------#
# unfold_parameter: materialise wildcard rows back to explicit members.
#
# Returns the expanded data.frame (read-time worker; does not mutate the
# parameter). Explicit rows take precedence over expanded wildcard rows.
# -----------------------------------------------------------------------------#
unfold_parameter <- function(param, member_sets = list(), value_col = "value") {
  data <- get_data_slot(param)
  if (is.null(data) || nrow(data) == 0) {
    return(as.data.frame(data))
  }
  data <- as.data.frame(data)

  # A per-entity membership joins only once its key columns are explicit, so
  # entity dims go first, then region, then timeslice / year. Unfolding region
  # ahead of tech would join the (NA tech) row on nothing.
  dims_order <- c(intersect(.unfold_order, names(member_sets)),
                  setdiff(names(member_sets), .unfold_order))
  for (dim in dims_order) {
    if (!dim %in% names(data)) next
    allowed <- member_sets[[dim]]
    if (is.null(allowed) || !dim %in% names(allowed)) next

    # An all-NA wildcard column read back from disk may be logical /
    # vctrs_unspecified; coerce to character so joins and is_any() behave.
    if (!is.character(data[[dim]])) {
      data[[dim]] <- as.character(data[[dim]])
    }

    is_wild <- is.na(data[[dim]]) | is_any(data[[dim]])
    if (!any(is_wild)) next

    expl <- data[!is_wild, , drop = FALSE]
    wild <- data[is_wild, , drop = FALSE]

    other_cols <- setdiff(names(data), c(dim, value_col))
    shared <- intersect(names(allowed), other_cols)
    # A key still entirely wild here (a source wildcard, or a dim outside this
    # pass) cannot join: project the membership over the remaining keys, the
    # union across the wild key.
    wild_keys <- shared[vapply(shared, function(k) {
      all(is.na(data[[k]]) | is_any(data[[k]]))
    }, logical(1))]
    if (length(wild_keys)) {
      shared <- setdiff(shared, wild_keys)
      allowed <- dplyr::distinct(
        allowed[, setdiff(names(allowed), wild_keys), drop = FALSE])
    }

    wild_nodim <- wild[, setdiff(names(wild), dim), drop = FALSE]
    if (length(shared) == 0) {
      # Full-set membership: `allowed` carries only the `dim` column (the
      # region/timeslice/year fallback for settings / weather parameters, which apply
      # to EVERY member regardless of the other dims). Cross-join each wild row
      # with every member -- a join on shared keys would expand nothing and leave
      # the wildcard in place. Cross-join via a transient key for portability.
      allw <- dplyr::distinct(allowed[, dim, drop = FALSE])
      # `data[[dim]]` was coerced to character above; match it so the membership
      # value (e.g. an INTEGER `year`) binds with the explicit rows.
      allw[[dim]] <- as.character(allw[[dim]])
      wild_nodim[[".xk"]] <- 1L
      allw[[".xk"]] <- 1L
      exp <- dplyr::inner_join(wild_nodim, allw, by = ".xk",
                               relationship = "many-to-many")
      exp[[".xk"]] <- NULL
    } else {
      allw <- dplyr::distinct(allowed[, c(shared, dim), drop = FALSE])
      allw[[dim]] <- as.character(allw[[dim]])  # match the coerced data[[dim]]
      exp <- dplyr::inner_join(wild_nodim, allw, by = shared,
                               relationship = "many-to-many")
    }

    # Explicit rows win: drop expanded rows already specified explicitly.
    if (nrow(expl) > 0) {
      key_cols <- setdiff(names(data), value_col)
      exp <- dplyr::anti_join(exp, expl, by = key_cols)
    }

    data <- dplyr::bind_rows(expl, exp)
    data <- as.data.frame(data)[, names(expl), drop = FALSE]
  }

  data
}

# -----------------------------------------------------------------------------#
# fold_scenario_parameters: fold all numpar/bounds parameters of a scenario.
# Used during interpolation (interp_mod). Returns the updated scenario.
# -----------------------------------------------------------------------------#
fold_scenario_parameters <- function(scen, dims = c("region", "timeslice"),
                                     tol = 1e-10, verbose = FALSE) {
  dims <- .rename_slice_compat(dims, "dims")
  pnames <- names(scen@modInp@parameters)
  for (pn in pnames) {
    p <- scen@modInp@parameters[[pn]]
    if (!inherits(p, "parameter")) next
    if (!(as.character(p@type) %in% c("numpar", "bounds"))) next
    # User-constraint / user-cost parameters are referenced from the user
    # equation strings (`scen@modInp@user_constraints[[i]]$equation`,
    # `user_costs`), which no backend's write-time rewrite touches: a folded
    # `pCnsRhs*(region, year)` would be looked up at its explicit key and read
    # the default. Left unfolded.
    if (grepl("^p(Cns|Costs)", pn)) next
    data <- get_data_slot(p)
    if (is.null(data) || nrow(data) == 0) next
    ms <- .fold_member_sets(scen, as.data.frame(data), dims = dims)
    if (length(ms) == 0) next
    before <- nrow(data)
    p2 <- fold_parameter(p, ms, dims = dims, tol = tol)
    scen@modInp@parameters[[pn]] <- p2
    if (verbose) {
      after <- nrow(get_data_slot(p2))
      if (after < before) {
        message(sprintf(
          "  fold %-20s %d -> %d rows [%s]", pn, before, after,
          paste(p2@misc$fold_info$wildcard_dims, collapse = ", ")
        ))
      }
    }
  }
  scen
}

# -----------------------------------------------------------------------------#
# unfold_scenario_parameter: read-time helper used by getData. Builds the
# membership maps for one parameter and returns its expanded data.frame.
# -----------------------------------------------------------------------------#
unfold_scenario_parameter <- function(scen, param,
                                      dims = c(.foldable_dims, "vintage")) {
  data <- get_data_slot(param)
  if (is.null(data) || nrow(data) == 0) {
    return(as.data.frame(data))
  }
  ms <- .fold_member_sets(scen, as.data.frame(data), dims = dims)
  if (length(ms) == 0) {
    return(as.data.frame(data))
  }
  unfold_parameter(param, ms)
}

# -----------------------------------------------------------------------------#
# unfold_scenario_parameters: materialise wildcard (NA) rows of all
# numpar/bounds parameters back to explicit members and write the expanded data
# into each parameter. The in-place counterpart of `fold_scenario_parameters`,
# used by `interp_mod(fold = FALSE)` so the written model carries no NA
# wildcards in the trimmable dimensions. Returns the updated scenario.
# -----------------------------------------------------------------------------#
unfold_scenario_parameters <- function(scen, dims = .foldable_dims,
                                       types = c("numpar", "bounds", "map"),
                                       verbose = FALSE) {
  dims <- .rename_slice_compat(dims, "dims")
  pnames <- names(scen@modInp@parameters)
  for (pn in pnames) {
    p <- scen@modInp@parameters[[pn]]
    if (!inherits(p, "parameter")) next
    if (!(as.character(p@type) %in% types)) next
    data <- get_data_slot(p)
    if (is.null(data) || nrow(data) == 0) next
    data <- as.data.frame(data)
    # Only act when a trimmable dimension actually carries a wildcard (NA / ANY).
    wild_dims <- intersect(dims, names(data))
    has_wild <- any(vapply(wild_dims, function(d) {
      x <- data[[d]]
      any(is.na(x) | is_any(x))
    }, logical(1)))
    if (!has_wild) next
    ms <- .fold_member_sets(scen, data, dims = dims)
    if (length(ms) == 0) next
    before <- nrow(data)
    expanded <- unfold_parameter(p, ms)
    if (is.null(expanded) || nrow(expanded) == 0) next
    p2 <- .fold_write_back(p, expanded)
    scen@modInp@parameters[[pn]] <- p2
    if (verbose) {
      message(sprintf("  unfold %-20s %d -> %d rows [%s]", pn, before,
                      nrow(expanded), paste(names(ms), collapse = ", ")))
    }
  }
  scen
}

# -----------------------------------------------------------------------------#
# unfold_trade_routes: materialise wildcard (NA) inter-regional route dimensions
# (`src`, `dst`) of trade parameters back to the explicit route pairs of each
# trade object.
#
# Unlike `region` / `timeslice`, the route endpoints are not foldable dimensions:
# a parameter row with `src = NA` / `dst = NA` is a wildcard meaning "applies to
# every route of this trade". Such a row is expanded to one row per (src, dst)
# pair of the trade (from `mTradeRoutes`, keyed on `trade`). Rows that already
# carry explicit endpoints are kept unchanged and win over the expansion. This
# must run for both folded and unfolded scenarios, since the equations look the
# parameters up over maps that carry the explicit route endpoints; an
# unmaterialised wildcard would silently resolve to the solver default.
# Returns the updated scenario.
# -----------------------------------------------------------------------------#
unfold_trade_routes <- function(scen, verbose = FALSE) {
  routes <- .read_map(scen, "mTradeRoutes")
  if (is.null(routes) || !all(c("trade", "src", "dst") %in% names(routes))) {
    return(scen)
  }
  routes <- dplyr::distinct(routes[, c("trade", "src", "dst"), drop = FALSE])

  for (pn in names(scen@modInp@parameters)) {
    p <- scen@modInp@parameters[[pn]]
    if (!inherits(p, "parameter")) next
    if (!(as.character(p@type) %in% c("numpar", "bounds", "map"))) next
    data <- get_data_slot(p)
    if (is.null(data) || nrow(data) == 0) next
    data <- as.data.frame(data)
    if (!all(c("trade", "src", "dst") %in% names(data))) next
    wild <- is.na(data$src) | is_any(data$src) |
      is.na(data$dst) | is_any(data$dst)
    if (!any(wild)) next

    explicit <- data[!wild, , drop = FALSE]
    other_cols <- setdiff(names(data), c("src", "dst"))
    wild_rows <- data[wild, other_cols, drop = FALSE]
    # Expand wildcard rows to one row per route pair of their trade. A row whose
    # `trade` is itself a wildcard (folded across every trade) stands for all
    # trades, so it takes every route; joined on the NA key it would vanish.
    trade_wild <- is.na(wild_rows$trade) | is_any(wild_rows$trade)
    expanded <- dplyr::inner_join(
      wild_rows[!trade_wild, , drop = FALSE], routes, by = "trade"
    )
    if (any(trade_wild)) {
      allr <- wild_rows[trade_wild, setdiff(other_cols, "trade"), drop = FALSE]
      allr[[".xk"]] <- 1L
      rt <- routes
      rt[[".xk"]] <- 1L
      exp_all <- dplyr::inner_join(allr, rt, by = ".xk",
                                   relationship = "many-to-many")
      exp_all[[".xk"]] <- NULL
      expanded <- dplyr::bind_rows(expanded, exp_all)
    }
    # Re-order columns to the original layout.
    expanded <- expanded[, names(data), drop = FALSE]
    # Explicit endpoints win over the expansion at the same full key.
    if (nrow(explicit) > 0) {
      key <- names(data)[names(data) != .fold_value_col_of(data)]
      expanded <- dplyr::anti_join(expanded, explicit, by = key)
    }
    out <- dplyr::bind_rows(explicit, expanded)
    before <- nrow(data)
    p2 <- .fold_write_back(p, out)
    # `trade` is explicit again: keep the fold record truthful
    fi <- p2@misc[["fold_info"]]
    if (any(trade_wild) && !is.null(fi) && "trade" %in% fi$wildcard_dims) {
      fi$wildcard_dims <- setdiff(fi$wildcard_dims, "trade")
      fi$folded <- length(fi$wildcard_dims) > 0
      fi$folded_rows <- nrow(out)
      p2@misc[["fold_info"]] <- fi
    }
    scen@modInp@parameters[[pn]] <- p2
    if (verbose) {
      message(sprintf("  unfold_routes %-20s %d -> %d rows", pn, before,
                      nrow(out)))
    }
  }
  scen
}

# Value/aux column name to exclude from a key (the only non-id column in
# numpar/bounds parameter data, when present).
.fold_value_col_of <- function(data) {
  if ("value" %in% names(data)) "value" else character(0)
}


# ---------------------------------------------------------------------------
# (was R/fold_artificial.R, merged 2026-09-21)
# ---------------------------------------------------------------------------

# =========================================================================== #
# fold_artificial.R  —  make a folded scenario solver-ready.
#
# interp_mod(fold = TRUE) collapses a parameter's region / timeslice column to NA when
# the value is invariant across that dimension over its WHOLE domain (whole-column
# fold). NA is not a set member, so no solver accepts it. This pass replaces the
# NA wildcard with an artificial set member (ANYREGION / ANYTIMESLICE) and rewrites the
# model code so every folded-parameter lookup indexes that member:
#   pX[t,c,r,y,s]  ->  pX[t,c,'ANYREGION',y,s]      (region folded)
# The member is added to the SET only (never to a membership map). Every variable,
# equation and sum in the model is map-gated, so the member is inert to model
# STRUCTURE -- it exists solely to hold each folded parameter's single value.
#
# Substitution is position-based (the index aliases differ per equation, e.g.
# `r`, `region`), using each parameter's dimSets to locate the folded position.
# GLPK declarations use `{}` and only `[]` usages are rewritten; JuMP and Pyomo
# lookups have their own call shapes; GAMS spells a declaration and a use
# alike, so its rewrite skips declaration lines. The member is a property of the
# WRITTEN files: `revert_fold_artificial()` takes it back out of the scenario.
# =========================================================================== #

# Map a solver language to its `.modelCode` block name.
.fold_code_block <- function(lang) {
  lang <- tolower(as.character(lang))
  if (length(lang) == 0 || is.na(lang)) return("GLPK")
  if (grepl("gams", lang)) "GAMS"
  else if (grepl("jump", lang)) "JuMP"
  else if (grepl("pyomo", lang)) "PYOMOConcrete"
  else "GLPK"
}

# dim -> artificial set member written into the data / set, and whether it is a
# quoted string in the model code. `year` is INTEGER in energyRt (2025, 2030, ...),
# so its wildcard is the integer 0 (never a real milestone), written unquoted; the
# string dims use a quoted ANY* token.
.fold_any <- list(
  region = list(member = "ANYREGION", quote = TRUE),
  timeslice  = list(member = "ANYTIMESLICE",  quote = TRUE),
  year   = list(member = 0L,          quote = FALSE),
  comm   = list(member = "ANYCOMM",   quote = TRUE),
  tech   = list(member = "ANYTECH",   quote = TRUE),
  stg    = list(member = "ANYSTG",    quote = TRUE),
  trade  = list(member = "ANYTRADE",  quote = TRUE)
)

# Split a string on top-level commas, respecting () and [] nesting.
.split_top_commas <- function(s) {
  parts <- character(0); cur <- ""; depth <- 0L
  for (k in seq_len(nchar(s))) {
    ch <- substr(s, k, k)
    if (ch %in% c("[", "(")) depth <- depth + 1L
    else if (ch %in% c("]", ")")) depth <- depth - 1L
    if (ch == "," && depth == 0L) { parts <- c(parts, cur); cur <- "" }
    else cur <- paste0(cur, ch)
  }
  c(parts, cur)
}

# Replace the `pos`-th comma-separated index of every `<prefix><open> ... <close>`
# usage in `code` with `member`. `prefix` is a literal string ending right before
# the bracket that opens the index list; the char before a `prefix` match must be a
# non-identifier (so `pX` does not match inside `vpX`). Matching close found by
# bracket depth, so nested brackets and commas are safe.
#
# `skip` (optional) is a logical vector over `code` marking lines the rewrite
# must not touch: GAMS declaration blocks (`.gams_decl_lines`), where a use and
# a declaration are spelled alike.
.subst_indexed <- function(code, prefix, open, close, pos, member, skip = NULL) {
  hit <- which(vapply(code, function(l) grepl(prefix, l, fixed = TRUE), logical(1)))
  if (!is.null(skip)) hit <- hit[!skip[hit]]
  for (li in hit) {
    line <- code[li]; res <- ""; rest <- line
    repeat {
      m <- regexpr(prefix, rest, fixed = TRUE)
      if (m < 0) { res <- paste0(res, rest); break }
      pre  <- substr(rest, 1, m - 1)
      aft  <- substr(rest, m + nchar(prefix), nchar(rest))   # right after prefix
      # word-boundary: prefix must not continue an identifier on its left
      if (nchar(pre) > 0 && grepl("[A-Za-z0-9_.]$", substr(pre, nchar(pre), nchar(pre)))) {
        res <- paste0(res, pre, prefix); rest <- aft; next
      }
      if (substr(aft, 1, 1) != open) { res <- paste0(res, pre, prefix); rest <- aft; next }
      depth <- 0L; endi <- NA_integer_
      for (k in seq_len(nchar(aft))) {
        ch <- substr(aft, k, k)
        if (ch == open) depth <- depth + 1L
        else if (ch == close) { depth <- depth - 1L; if (depth == 0L) { endi <- k; break } }
      }
      if (is.na(endi)) { res <- paste0(res, pre, prefix); rest <- aft; next }
      inner <- substr(aft, 2, endi - 1)
      args  <- trimws(.split_top_commas(inner))
      if (length(args) >= pos) args[pos] <- member
      res  <- paste0(res, pre, prefix, open, paste(args, collapse = ","), close)
      rest <- substr(aft, endi + 1, nchar(aft))
    }
    code[li] <- res
  }
  code
}

# Per-backend index-usage patterns for a (possibly Up/Lo-suffixed) parameter name.
.subst_patterns <- function(backend, name) {
  if (backend == "GLPK")
    list(list(prefix = name, open = "[", close = "]"))
  else if (backend == "JuMP")
    list(list(prefix = paste0(name, "["),         open = "(", close = ")"),
         list(prefix = paste0("haskey(", name, ", "), open = "(", close = ")"))
  else if (grepl("PYOMO", backend))
    list(list(prefix = paste0(name, ".get("),     open = "(", close = ")"))
  else if (backend == "GAMS")
    # GAMS spells a declaration and a use identically (`p(tech, region, year)`
    # is both), so the pattern is GLPK's over `()` and the rewrite is confined
    # to non-declaration lines by `.gams_decl_lines()`.
    list(list(prefix = name, open = "(", close = ")"))
  else list()
}

# Lines belonging to a GAMS DECLARATION block: a block keyword at line start
# through the terminating `;`. Declarations must be left alone -- the artificial
# member is a real member of its set, so the declared domain
# `p(tech, region, year, timeslice)` already covers the wildcard key, while
# rewriting it to `p(tech, 'ANYREGION', year, timeslice)` is not a valid domain
# (a quoted label is an element, not a set) and would also break the `$loadm`
# GDX read that the declaration governs.
#
# A `*` comment inside a block may carry a `;`; treating that as the terminator
# drops the rest of the block and every declaration after it gets rewritten.
# `$ontext` / `$offtext` blocks likewise terminate nothing.
.gams_decl_lines <- function(code) {
  hdr <- paste0("^[[:space:]]*(sets?|parameters?|scalars?|table|equations?|",
                "((free|positive|negative|binary|integer)[[:space:]]+)?",
                "variables?)([[:space:]]|$)")
  out <- logical(length(code))
  inblk <- FALSE
  intext <- FALSE
  for (i in seq_along(code)) {
    ln <- code[i]
    if (grepl("^[[:space:]]*[$]ontext", ln, ignore.case = TRUE)) {
      intext <- TRUE; out[i] <- TRUE; next
    }
    if (intext) {
      out[i] <- TRUE
      if (grepl("^[[:space:]]*[$]offtext", ln, ignore.case = TRUE)) intext <- FALSE
      next
    }
    # `*` in column 1 is a full-line GAMS comment
    if (grepl("^[*]", ln)) { out[i] <- TRUE; next }
    if (!inblk && grepl(hdr, ln, ignore.case = TRUE)) inblk <- TRUE
    if (inblk) {
      out[i] <- TRUE
      # a block ends at the first `;`, which may sit on the header line itself
      if (grepl(";", ln, fixed = TRUE)) inblk <- FALSE
    }
  }
  out
}

# Member literal as written in each backend's model code, matching how that
# backend keys a REAL member of the dimension. Pyomo stringifies every set member
# (so the integer `year` wildcard is the string "0"); GLPK keeps `year` numeric
# (unquoted) and single-quotes string members. JuMP keys the `year` slot of its
# parameter Dicts numerically (the `as.character(year)` coercion in write_jump.R
# is disabled) while keying string dims (region/timeslice) as strings -- so the year
# wildcard must be the bare integer `0` for JuMP (a quoted "0" never matches the
# stored integer key, silently returning the default), but string wildcards stay
# double-quoted. `quote == FALSE` marks the numeric (`year`) wildcard.
.fold_member_literal <- function(backend, dim) {
  a <- .fold_any[[dim]]
  str_backend <- backend %in% c("JuMP", "PYOMOConcrete", "PYOMOAbstract")
  if (str_backend) {
    if (backend == "JuMP" && !isTRUE(a$quote)) return(as.character(a$member))
    return(paste0('"', a$member, '"'))
  }
  # GAMS: a label in an index position is always quoted, the numeric `year`
  # wildcard included (`'0'`; a bare 0 is a number, not a label).
  if (backend == "GAMS") return(paste0("'", a$member, "'"))
  if (!isTRUE(a$quote)) as.character(a$member) else paste0("'", a$member, "'")
}

# Identify which value parameters are whole-column folded on each foldable dim.
.folded_params <- function(scen, dims = names(.fold_any)) {
  out <- stats::setNames(vector("list", length(dims)), dims)
  for (nm in names(scen@modInp@parameters)) {
    p <- scen@modInp@parameters[[nm]]
    if (is.null(p) || p@type %in% c("set", "map")) next
    d <- as.data.frame(get_data_slot(p))
    if (is.null(d) || nrow(d) == 0) next
    for (dim in dims) {
      if (dim %in% names(d) && all(is.na(d[[dim]]))) out[[dim]] <- c(out[[dim]], nm)
    }
  }
  out
}

# -----------------------------------------------------------------------------#
# .partial_wildcard_params: parameters whose wildcard column is PARTIAL, i.e.
# NA in SOME rows and explicit in others.
#
# `.folded_params()` deliberately registers only WHOLE columns, and `fold.R`
# (`.fold_one_dim`) deliberately never creates a partial one -- because the code
# rewrite is per-PARAMETER: rewriting every `pX[...]` lookup to index
# 'ANYREGION' would strand the explicit rows.
#
# But a partial column can still arrive from the SOURCE data: a wildcard that
# `unfold_scenario_parameters()` could not materialise (no membership row for
# that entity) and that the fold pass then correctly declined to fold. It is
# nobody's output, so nothing converts it, nothing rewrites it, and
# `validate_scenario_parameters()` exempts the trimmable dims from its NA check.
# The raw NA reaches the solver, where it is not a set member, so every lookup
# that should hit it misses and silently takes the parameter's default.
#
# Measured consequence (IB_PTL50_CU50_P10, fold = TRUE): pTechEac carried NA in
# 106 of 208 region rows; only 3 of 141 mTechNew tuples found a value; capital
# cost effectively vanished and the model returned a NEGATIVE objective.
# -----------------------------------------------------------------------------#
.partial_wildcard_params <- function(scen, dims = names(.fold_any)) {
  out <- stats::setNames(vector("list", length(dims)), dims)
  for (nm in names(scen@modInp@parameters)) {
    p <- scen@modInp@parameters[[nm]]
    if (is.null(p) || p@type %in% c("set", "map")) next
    d <- as.data.frame(get_data_slot(p))
    if (is.null(d) || nrow(d) == 0) next
    for (dim in dims) {
      if (!dim %in% names(d)) next
      n <- sum(is.na(d[[dim]]))
      if (n > 0 && n < nrow(d)) out[[dim]] <- c(out[[dim]], nm)
    }
  }
  out
}

# -----------------------------------------------------------------------------#
# .materialise_partial_wildcards: expand partial wildcard rows to explicit
# members, so an UNREWRITTEN lookup finds them.
#
# This is the only correct treatment: the artificial member cannot represent a
# partial column (see above), so the rows have to become real. Values are
# unchanged -- one wildcard row becomes one row per allowed member.
#
# NOTE this is NOT undone by `revert_fold_artificial()`, and cannot be: the fold
# pass would decline to re-fold a partial column, so there is nothing to fold
# back to. For an on-disk parameter `.fold_write_back()` therefore leaves the
# stored data expanded. That is deliberate and safe -- the values are identical
# and these rows were never compressed in the first place (the fold pass refused
# them) -- but it does mean the row count of such a parameter grows once, on the
# first write after this fix.
# -----------------------------------------------------------------------------#
.materialise_partial_wildcards <- function(scen, dims = names(.fold_any),
                                           verbose = FALSE) {
  partial <- .partial_wildcard_params(scen, dims)
  todo <- unique(unlist(partial, use.names = FALSE))
  if (length(todo) == 0) return(scen)

  for (nm in todo) {
    p <- scen@modInp@parameters[[nm]]
    d <- as.data.frame(get_data_slot(p))
    ms <- .fold_member_sets(scen, d, dims = intersect(dims, names(d)))
    if (length(ms) == 0) next
    out <- tryCatch(unfold_parameter(p, ms), error = function(e) NULL)
    if (is.null(out) || nrow(out) == 0) next
    if (verbose) {
      message(sprintf("  materialise partial wildcard %-22s %d -> %d rows",
                      nm, nrow(d), nrow(out)))
    }
    scen@modInp@parameters[[nm]] <- .fold_write_back(p, as.data.frame(out))
  }
  scen
}

# -----------------------------------------------------------------------------#
# .assert_no_raw_wildcards: nothing may reach a writer with a raw NA in a
# foldable index column.
#
# NA is not a set member in any backend. GLPK/Pyomo/JuMP all resolve a missed
# key to the parameter's default, so the model stays feasible and solves to a
# confidently wrong answer -- there is no error to notice. This turns that into
# a build-time failure.
#
# It fires only on the broken case: measured across two unfolded production runs
# (613 and 603 written parameter files) the count of raw NAs is 0, while the
# folded run that produced the wrong objective had exactly 4.
# -----------------------------------------------------------------------------#
.assert_no_raw_wildcards <- function(scen, dims = names(.fold_any)) {
  bad <- character()
  for (nm in names(scen@modInp@parameters)) {
    p <- scen@modInp@parameters[[nm]]
    if (is.null(p) || p@type %in% c("set", "map")) next
    d <- as.data.frame(get_data_slot(p))
    if (is.null(d) || nrow(d) == 0) next
    for (dim in intersect(dims, names(d))) {
      n <- sum(is.na(d[[dim]]))
      if (n > 0) {
        bad <- c(bad, sprintf("  %s$%s: %d of %d rows", nm, dim, n, nrow(d)))
      }
    }
  }
  if (length(bad) == 0) return(invisible(TRUE))
  stop("fold: ", length(bad), " parameter column(s) still hold a raw NA ",
       "wildcard and cannot be written.
",
       paste(bad, collapse = "
"),
       "
  NA is not a set member: every lookup that should hit these rows ",
       "would miss and silently take the parameter's default, so the model ",
       "would solve to a wrong answer rather than fail.
",
       "  They could not be expanded to explicit members (no membership rows ",
       "for those entities). Fix the source data, or re-interpolate with ",
       "fold = FALSE.", call. = FALSE)
}

# Replace NA wildcards with the artificial set member, register the member in the
# set, and rewrite the model code of `backends` so folded lookups index it.
apply_fold_artificial <- function(scen, backends = "GLPK",
                                  dims = names(.fold_any), verbose = FALSE) {
  # A PARTIAL wildcard column cannot be represented by the artificial member --
  # the rewrite is per-parameter, so pointing every lookup at 'ANYREGION' would
  # strand the explicit rows. Expand those to real members first, leaving only
  # the whole-column case the rewrite below is built for.
  scen <- .materialise_partial_wildcards(scen, dims, verbose = verbose)

  folded <- .folded_params(scen, dims)
  if (all(lengths(folded) == 0)) {
    .assert_no_raw_wildcards(scen, dims)
    return(scen)
  }

  for (dim in names(folded)) {
    if (length(folded[[dim]]) == 0) next
    member <- .fold_any[[dim]]$member

    # 1. add the artificial member to the set parameter, EXCEPT the `year`
    #    wildcard when targeting JuMP. JuMP parameters are plain Julia `Dict`s, so
    #    the wildcard only has to be a Dict KEY (handled in step 2 + the lookup
    #    rewrite) -- it must NOT join the `year` set that constraint loops iterate.
    #    JuMP gate conditions hard-index year params, e.g.
    #    `ordYear[(yp)] for yp in year`, so a spurious `0` in the `year` set throws
    #    `KeyError: key 0 not found`. GLPK/Pyomo declare params over the set and
    #    default missing keys, so they need the member in the set and tolerate it
    #    in the loop. (Assumes single-backend calls, as from solve_scenario's `.blk`.)
    skip_set_member <- dim == "year" && all(backends == "JuMP")
    if (!skip_set_member) {
      setp <- scen@modInp@parameters[[dim]]
      sd <- as.data.frame(get_data_slot(setp))
      if (!member %in% sd[[dim]]) {
        sd <- rbind(sd, stats::setNames(data.frame(member, stringsAsFactors = FALSE), dim))
        scen@modInp@parameters[[dim]] <- .fold_write_back(setp, sd)
      }
    }

    # 2. replace NA -> member in every folded parameter's data
    for (nm in folded[[dim]]) {
      p <- scen@modInp@parameters[[nm]]
      d <- as.data.frame(get_data_slot(p))
      d[[dim]][is.na(d[[dim]])] <- member
      scen@modInp@parameters[[nm]] <- .fold_write_back(p, d)
    }
  }

  # 3. rewrite the model code: index each folded parameter at the member literal
  for (bk in backends) {
    code <- scen@settings@sourceCode[[bk]]
    if (is.null(code)) next
    # Computed once: substitution rewrites lines in place, so the line count --
    # and hence the mask -- stays valid across the loop below.
    skip <- if (bk == "GAMS") .gams_decl_lines(code) else NULL
    for (dim in names(folded)) {
      lit <- .fold_member_literal(bk, dim)
      for (nm in folded[[dim]]) {
        p <- scen@modInp@parameters[[nm]]
        pos <- match(dim, p@dimSets)
        if (is.na(pos)) next
        # bounds parameters are emitted in the model code with Up / Lo / Fx
        # suffixes (the `type` column is not part of `dimSets`, so the folded
        # position is unchanged); numpar parameters keep their bare name.
        targets <- if (as.character(p@type) == "bounds")
          paste0(nm, c("Up", "Lo", "Fx")) else nm
        for (tg in targets) {
          for (pat in .subst_patterns(bk, tg)) {
            code <- .subst_indexed(code, pat$prefix, pat$open, pat$close, pos,
                                   lit, skip = skip)
          }
        }
      }
    }
    scen@settings@sourceCode[[bk]] <- code
  }
  # what `revert_fold_artificial()` has to take back out
  scen@misc$fold_artificial <- names(folded)[lengths(folded) > 0]
  # Last line of defence: after the conversion above, a surviving raw NA is a
  # wildcard nothing can represent, and writing it produces a wrong answer with
  # no error. Fail here instead.
  .assert_no_raw_wildcards(scen, dims)
  scen
}

# Undo `apply_fold_artificial()` on the scenario object: the artificial member
# back to the NA wildcard in every folded value parameter, and out of each set.
# Left in, the region set reads `R1 R2 ANYREGION` and the year set `2020 0`; a
# read-time unfold then expands the region wildcard over ANYREGION too, and the
# year wildcard `0` (neither NA nor ANY*) is not expanded at all. Idempotent.
# The rewritten model source stays: it is re-copied at interpolation and the
# substitution rewrites an already substituted position to the same literal.
revert_fold_artificial <- function(scen, dims = scen@misc$fold_artificial) {
  dims <- intersect(dims, names(.fold_any))
  if (length(dims) == 0) return(scen)
  members <- lapply(.fold_any[dims], `[[`, "member")
  for (dim in dims) {
    setp <- scen@modInp@parameters[[dim]]
    if (is.null(setp)) next
    sd <- as.data.frame(get_data_slot(setp))
    if (nrow(sd) > 0 && any(sd[[dim]] %in% members[[dim]])) {
      scen@modInp@parameters[[dim]] <-
        .fold_write_back(setp, sd[!sd[[dim]] %in% members[[dim]], , drop = FALSE])
    }
  }
  # one pass over the value parameters, every substituted dim at once
  for (nm in names(scen@modInp@parameters)) {
    p <- scen@modInp@parameters[[nm]]
    if (is.null(p) || p@type %in% c("set", "map")) next
    d <- as.data.frame(get_data_slot(p))
    if (is.null(d) || nrow(d) == 0) next
    touched <- FALSE
    for (dim in intersect(dims, names(d))) {
      hit <- !is.na(d[[dim]]) & d[[dim]] %in% members[[dim]]
      if (!any(hit)) next
      d[[dim]][hit] <- NA
      touched <- TRUE
    }
    if (touched) scen@modInp@parameters[[nm]] <- .fold_write_back(p, d)
  }
  scen@misc$fold_artificial <- NULL
  scen
}
