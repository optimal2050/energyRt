# =============================================================================#
# fold.R
#
# Reversible "fold / unfold" of modInp parameters along trimmable dimensions.
#
# A trimmable dimension (default: region, slice, vintage) is "folded" to a
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
.fold_trim_dims <- c("region", "slice", "vintage")

# All dimensions that MAY be folded (whole-column), in stable order. region/slice
# are the original trimmable dims; year/comm/tech/stg/trade are opt-in via the
# `fold` argument. The artificial-member layer (fold_artificial.R `.fold_any`)
# must define a wildcard for each.
.foldable_dims <- c("region", "slice", "year", "comm", "tech", "stg", "trade")

# Identity / axis dimensions that must NEVER be folded.
.fold_protected_dims <- c(
  "commp", "process", "sup", "imp", "expp", "dem", "weather", "yearp", "type"
)

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
# .slice_allowed: per-entity slice membership for the given identity key.
# Returns a data.frame(<key>, slice) or NULL when unavailable.
# -----------------------------------------------------------------------------#
.slice_allowed <- function(scen, key) {
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
    tech  = pick(.read_map(scen, "mTechSlice"), c("tech", "slice")),
    sup   = pick(.read_map(scen, "mSupSlice"), c("sup", "slice")),
    trade = pick(.read_map(scen, "mTradeSlice"), c("trade", "slice")),
    imp   = pick(.read_map(scen, "mImpSlice"), c("imp", "slice")),
    expp  = pick(.read_map(scen, "mExpSlice"), c("expp", "slice")),
    comm  = pick(.read_map(scen, "mCommSlice"), c("comm", "slice")),
    stg   = {
      sc <- .read_map(scen, "mStorageComm")
      cs <- .read_map(scen, "mCommSlice")
      if (is.null(sc) || is.null(cs)) {
        NULL
      } else if (!all(c("stg", "comm") %in% names(sc)) ||
                 !all(c("comm", "slice") %in% names(cs))) {
        NULL
      } else {
        dplyr::distinct(dplyr::inner_join(
          sc[, c("stg", "comm")], cs[, c("comm", "slice")],
          by = "comm"
        )[, c("stg", "slice")])
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

  if ("slice" %in% dims && "slice" %in% cols) {
    for (k in c("tech", "sup", "stg", "trade", "imp", "expp", "comm")) {
      if (!k %in% cols) next
      a <- .slice_allowed(scen, k)
      if (!is.null(a)) {
        ms$slice <- a
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

  # Full-set fallback for region / slice / year when no per-entity membership was
  # found (e.g. weather parameters, which apply to every region/year). Folding a
  # column that covers the COMPLETE set uniformly can never over-claim, so it is
  # safe: e.g. `pWeather`, identical across all years, folds year -> the wildcard.
  for (dd in intersect(dims, c("region", "slice", "year"))) {
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
  # (`ANYREGION` / `ANYSLICE`) the writers substitute, so if any explicit group is
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
    existing <- list.files(data_dir, recursive = TRUE)
    fmt <- if (any(grepl("\\.parquet$", existing))) "parquet" else "csv"
    unlink(data_dir, recursive = TRUE)
    data2disk(data.table::as.data.table(new_data), path = data_dir,
              format = fmt)
    param@data <- reset_slot(data.table::as.data.table(new_data))
  } else {
    param@data <- as.data.frame(new_data)
  }
  # Sync the row-count cache; writers truncate `@data` to `@misc$nValues` and a
  # stale (larger) count pads with NA rows (e.g. Pyomo `tmp[('NA','NA')] = NA`).
  param@misc$nValues <- nrow(new_data)
  param
}

# -----------------------------------------------------------------------------#
# fold_parameter: fold trimmable dimensions of a single parameter to wildcards.
#
# `member_sets` is a named list mapping a trimmable dim to a data.frame holding
# the allowed members per entity (e.g. list(slice = mTechSlice)). Dims without
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

  for (dim in names(member_sets)) {
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

    wild_nodim <- wild[, setdiff(names(wild), dim), drop = FALSE]
    if (length(shared) == 0) {
      # Full-set membership: `allowed` carries only the `dim` column (the
      # region/slice/year fallback for settings / weather parameters, which apply
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
fold_scenario_parameters <- function(scen, dims = c("region", "slice"),
                                     tol = 1e-10, verbose = FALSE) {
  pnames <- names(scen@modInp@parameters)
  for (pn in pnames) {
    p <- scen@modInp@parameters[[pn]]
    if (!inherits(p, "parameter")) next
    if (!(as.character(p@type) %in% c("numpar", "bounds"))) next
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
                                      dims = c("region", "slice", "vintage")) {
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
unfold_scenario_parameters <- function(scen, dims = c("region", "slice"),
                                       types = c("numpar", "bounds", "map"),
                                       verbose = FALSE) {
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
    if (!is.list(p2@misc)) p2@misc <- list()
    p2@misc$nValues <- nrow(expanded)
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
# Unlike `region` / `slice`, the route endpoints are not foldable dimensions:
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
    # Expand wildcard rows to one row per route pair of their trade.
    expanded <- dplyr::inner_join(
      data[wild, other_cols, drop = FALSE], routes, by = "trade"
    )
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
    if (!is.list(p2@misc)) p2@misc <- list()
    p2@misc$nValues <- nrow(out)
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
