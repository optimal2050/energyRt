# ========================================================================== #
# Technology variants: vintage and cluster expansion
#
# A `technology` carrying `vintage` and/or `cluster` values describes a GROUP of
# similar processes rather than a single one:
#
#   vintage  capacity built in a given year keeps the characteristics of that
#            build year (capex, efficiency, lifetime) for its whole life;
#   cluster  parallel sub-processes -- resource class, geography, fleet type --
#            each with its own capacity, availability factor and costs.
#
# Rather than adding two indices to every tech variable and equation (which
# would mean rewriting `gams/energyRt.gms` and all four solver templates), the
# group is EXPANDED into one ordinary `technology` per (vintage, cluster) cell
# before interpolation. Each variant is indistinguishable from a hand-written
# technology, so the whole model core -- equations, mappings, templates, the
# `.dimSets` whitelist -- is untouched.
#
# A vintage is exactly a variant with a one-period investment window, which is
# why `@vintage` doubles as the lifespan table: `start`/`end` default to the
# vintage year.
#
# The link back from a variant to its base technology is kept in a provenance
# table (`scen@modInp@sets$tech_variant`), NOT in the mangled name: reporting
# joins that table and never parses strings.
# ========================================================================== #

# -------------------------------------------------------------------------- #
# Per-class registry
#
# `slots`      slots whose rows may be selected by vintage/cluster. Everything
#              else is structural -- it defines the process's topology and must
#              be identical across variants, otherwise it is a different process.
# `dims`       the variant dimensions this class supports. All three classes
#              support both. For `trade` a cluster is a LOSS TRANCHE: a parallel
#              part of one line, with its own share of the capacity and its own
#              `teff`, approximating the quadratic loss curve piecewise.
# `bound_var`  capacity variables a group-aggregate ("TOTAL") bound constrains.
# `bound_dims` dimensions available to such a bound. `vTradeCap{trade, year}`
#              carries no region index, so a trade bound can only span years.
# -------------------------------------------------------------------------- #
.variant_classes <- list(
  technology = list(
    key = "tech",
    dims = c("vintage", "cluster"),
    slots = c("vintage", "capacity", "ceff", "geff", "aeff",
              "af", "afs", "weather", "fixom", "varom", "invcost"),
    bound_var = list(cap = "vTechCap", ncap = "vTechNewCap"),
    bound_dims = c("region", "year"),
    # Availability factors that may be declared for a GROUP of variants: the
    # summed activity of the group against its summed capacity. `afs` sums the
    # activity over the DESCENDANTS of the named slice, `af` takes it at that
    # slice -- the same difference the two slots have per variant.
    af_bounds = list(
      list(slot = "af",  stem = "af",  act = "vTechAct", cap = "vTechCap",
           sum_children = FALSE),
      list(slot = "afs", stem = "afs", act = "vTechAct", cap = "vTechCap",
           sum_children = TRUE)
    )
  ),
  storage = list(
    key = "stg",
    dims = c("vintage", "cluster"),
    slots = c("vintage", "capacity", "invcost", "fixom", "varom",
              "af", "seff", "charge", "aeff", "weather", "duration"),
    # A storage keeps one capacity per PART, so its `@capacity` columns read
    # `out.cap.up`, not `cap.up`, and each part has its own variable. Keyed on
    # the FULL stem: keying on `cap`/`ncap` behind a single `out.` prefix made
    # `inp.*` and `stg.*` match no column at all, so a TOTAL on either was
    # silently dropped -- no variant (the token is filtered out of the level
    # set) and no bound.
    bound_var = list(
      out.cap  = "vStorageOutCap", out.ncap = "vStorageOutNewCap",
      inp.cap  = "vStorageInpCap", inp.ncap = "vStorageInpNewCap",
      stg.cap  = "vStorageStgCap", stg.ncap = "vStorageStgNewCap"
    ),
    # Still the discharger for `.cluster_fixed_caps()`, which needs ONE primary
    # capacity column to check share proportions against.
    bound_prefix = "out.",
    bound_dims = c("region", "year")
  ),
  trade = list(
    key = "trade",
    # `@routes` gives one object several LINES; `cluster` gives one line several
    # parallel TRANCHES on the same (src, dst) pair, each with its own capacity
    # and its own `teff`. That is what a piecewise-linear approximation of the
    # quadratic loss curve needs, and routes cannot express it -- so the two
    # axes are not redundant. `@routes` stays out of `slots` below: it is
    # structural, and every tranche must keep BOTH directed routes or it fails
    # the KVL "declared in one direction only" check.
    dims = c("vintage", "cluster"),
    slots = c("vintage", "capacity", "invcost", "fixom", "varom", "aeff",
              "trade"),
    bound_var = list(cap = "vTradeCap", ncap = "vTradeNewCap"),
    # `vTradeCap{trade, year}` has no region index, so a group bound spans years
    # only; a region on a TOTAL row errors rather than being silently replicated.
    bound_dims = "year",
    # Absolute FLOW bounds that may be declared for a group of variants. Needed
    # by loss tranches: a line split into tranches has its capacity split too, so
    # a per-tranche `ava.up` is NOT the line's limit -- broadcasting 50 to two
    # tranches permits 100, and splitting it 25/25 forces flow onto the dear
    # tranche and DOUBLES the modelled loss. Only an aggregate is correct.
    #
    # `keys` are pinned in `for.sum` (one constraint each) rather than in
    # `for.each`, because `src`/`dst` are dimension names, not sets.
    flow_bounds = list(
      list(slot = "trade", stems = "ava", var = "vTradeIr",
           keys = c("src", "dst"), cells = c("year", "timeslice"))
    ),
    # `af` on a tranched line has the same problem `ava` does: the capacity is
    # split, so a per-tranche relative bound forces flow onto the dearer tranche.
    af_bounds = list(
      list(slot = "trade", stem = "af", act = "vTradeIr", cap = "vTradeCap",
           sum_children = FALSE)
    )
  ),
  # supply / export / import have NO capacity variable at all -- quantity is
  # limited purely by parameters (pSupAvaUp, pSupReserveUp, pImportRowUp, ...).
  # So `bound_var` is empty and both constraint builders bail on their own
  # guards: .variant_group_constraints() on `!.hasSlot(tech, "capacity")` and
  # .variant_share_constraints() on `is.null(bvar)`. A cluster here is a PRICE
  # STEP of a stepped curve, and nothing needs tying -- the helper writes the
  # quantity split straight into the bounds.
  #
  # `slots` must list EVERY slot holding a quantity or a price. A slot left out
  # is copied identically to every step, so an N-step curve would silently hold
  # N times the resource. `reserve` is the trap: it is the cumulative cap.
  supply = list(
    key = "sup",
    dims = "cluster",
    slots = c("supply", "reserve", "weather"),
    bound_var = list(),
    bound_dims = character()
  ),
  export = list(
    key = "expp",
    dims = "cluster",
    slots = c("export", "reserve"),
    bound_var = list(),
    bound_dims = character()
  ),
  import = list(
    key = "imp",
    dims = "cluster",
    slots = c("import", "reserve"),
    bound_var = list(),
    bound_dims = character()
  )
)

# Every variant selector column across all classes. Used for the naming prefixes
# (a model-wide setting) as opposed to what one class actually supports.
.variant_dims <- c("vintage", "cluster")

# Registry entry for an object, or NULL when its class has no variants.
.variant_def <- function(obj) {
  .variant_classes[[class(obj)[1]]]
}

# The slots / dims of the class an object belongs to (empty for a non-variant
# class, so the generic helpers degrade to no-ops).
.variant_slots_of <- function(obj) {
  d <- .variant_def(obj)
  if (is.null(d)) character() else d$slots
}
.variant_dims_of <- function(obj) {
  d <- .variant_def(obj)
  if (is.null(d)) character() else d$dims
}

# -------------------------------------------------------------------------- #
# Variant name prefixes
#
# The prefixes inserted into an expanded variant's name become solver set-member
# names, so they are a model-wide naming convention held in `config@variant_prefix`
# and inherited by `settings`. Everything below is merge-based, which makes it
# idempotent, tolerant of partial overrides (`c(cluster = "_RC")`) and tolerant of
# objects serialised before the slot existed.
# -------------------------------------------------------------------------- #

# The built-in defaults. Single source of truth: `config`'s prototype must match,
# which a unit test asserts so the two cannot drift.
.variant_prefix_default <- function() c(vintage = "_VIN", cluster = "_CL")

.variant_prefix_validate <- function(p) {
  if (!all(nzchar(p))) {
    stop("`variant_prefix` entries must be non-empty; got ",
         paste0(names(p), ' = "', p, '"', collapse = ", "), ".")
  }
  bad <- names(p)[!grepl("^[[:alnum:]_]+$", p)]
  if (length(bad) > 0L) {
    stop("`variant_prefix` must match ^[[:alnum:]_]+$ because it becomes part of ",
         "a solver set-member name; offending entr",
         if (length(bad) == 1L) "y: " else "ies: ",
         paste0(bad, ' = "', p[bad], '"', collapse = ", "), ".")
  }
  if (p[["vintage"]] == p[["cluster"]]) {
    stop('The vintage and cluster prefixes must differ; both are "',
         p[["vintage"]], '". Otherwise a vintage and a cluster label could ',
         "produce the same variant name.")
  }
  # NOTE deliberately NOT enforced: "one prefix must not be a prefix of the
  # other". c(vintage = "_V", cluster = "_VC") can in principle collide, but
  # forbidding it would reject reasonable schemes; the mint-then-check collision
  # detector in expand_tech_variants() catches the actual collision with a far
  # more useful message.
  invisible(TRUE)
}

# Normalise a user-supplied value and merge it over `base`.
.variant_prefix_merge <- function(x, base = .variant_prefix_default()) {
  if (is.null(x) || length(x) == 0L) {
    .variant_prefix_validate(base)
    return(base[.variant_dims])
  }
  nms <- names(x)          # capture BEFORE as.character(), which strips them
  x <- as.character(x)
  names(x) <- nms
  if (is.null(nms) || any(!nzchar(nms))) {
    if (length(x) == length(.variant_dims)) {
      names(x) <- .variant_dims           # positional: (vintage, cluster)
    } else {
      stop("`variant_prefix` must be named, e.g. c(vintage = \"_VIN\", ",
           "cluster = \"_CL\"); an unnamed vector of length ", length(x),
           " is ambiguous (only a full unnamed pair is accepted positionally).")
    }
  }
  unknown <- setdiff(names(x), .variant_dims)
  if (length(unknown) > 0L) {
    stop("Unknown variant dimension(s) in `variant_prefix`: ",
         paste0('"', unknown, '"', collapse = ", "),
         ". Valid: ", paste(.variant_dims, collapse = ", "), ".")
  }
  if (anyDuplicated(names(x)) > 0L) {
    stop("Duplicated variant dimension(s) in `variant_prefix`: ",
         paste(unique(names(x)[duplicated(names(x))]), collapse = ", "), ".")
  }
  out <- base
  out[names(x)] <- x
  .variant_prefix_validate(out)
  out[.variant_dims]                      # canonical order -> identical() safe
}

# Resolve the prefixes from a config / settings / model / scenario.
.variant_prefix <- function(x) {
  if (methods::is(x, "scenario")) x <- x@settings
  else if (methods::is(x, "model")) x <- x@config
  raw <- if (methods::.hasSlot(x, "variant_prefix")) slot(x, "variant_prefix") else NULL
  .variant_prefix_merge(raw)
}

# Bound columns for which the reserved "TOTAL" token means a group aggregate.
.variant_bound_cols <- c(
  "cap.lo", "cap.up", "cap.fx",
  "ncap.lo", "ncap.up", "ncap.fx",
  "ret.lo", "ret.up", "ret.fx"
)

# -------------------------------------------------------------------------- #
# helpers
# -------------------------------------------------------------------------- #

# The declared cluster table (@cluster), or NULL when the slot is empty.
# When present it is AUTHORITATIVE for which clusters exist.
.declared_clusters <- function(tech) {
  if (!.hasSlot(tech, "cluster")) return(NULL)
  d <- as.data.frame(tech@cluster)
  if (nrow(d) == 0L || !"cluster" %in% names(d)) return(NULL)
  d$cluster <- as.character(d$cluster)
  d <- d[!is.na(d$cluster) & nzchar(d$cluster), , drop = FALSE]
  if (nrow(d) == 0L) return(NULL)
  if (!"order" %in% names(d)) d$order <- NA_integer_
  if (!"region" %in% names(d)) d$region <- NA_character_
  d
}

# Per-slot occurrences of a selector column, used both to build the level set and
# to spot labels that appear in some slots but not others (the typo signature).
.variant_levels_by_slot <- function(tech, dim) {
  sl <- .variant_slots_of(tech)
  out <- lapply(sl, function(s) {
    if (!.hasSlot(tech, s)) return(NULL)
    d <- as.data.frame(slot(tech, s))
    if (nrow(d) == 0L || !dim %in% names(d)) return(NULL)
    v <- as.character(d[[dim]])
    v <- v[!is.na(v) & v != .VARIANT_TOTAL]
    if (length(v) == 0L) return(NULL)
    unique(v)
  })
  names(out) <- sl
  out[!vapply(out, is.null, logical(1))]
}

# Distinct real (non-NA, non-TOTAL) values of a selector column across all
# variant-aware slots of one technology. For `cluster`, a populated @cluster slot
# overrides the harvest: declaring clusters is enough to create the variants,
# even if no slot carries per-cluster data yet.
.variant_levels <- function(tech, dim) {
  if (identical(dim, "cluster")) {
    dc <- .declared_clusters(tech)
    if (!is.null(dc)) {
      ord <- order(dc$order, dc$cluster, na.last = TRUE)
      return(unique(dc$cluster[ord]))
    }
  }
  vals <- unlist(.variant_levels_by_slot(tech, dim), use.names = FALSE)
  # sorted so harvested levels are deterministic regardless of slot order
  sort(unique(vals))
}

# Regions a declared cluster exists in, or NULL for "all regions".
# `region = NA` in the declaration means everywhere (the usual NA-broadcast
# convention); one row per region restricts the cluster to those named.
.cluster_regions <- function(dc, cl) {
  if (is.null(dc)) return(NULL)
  rows <- dc[dc$cluster == cl, , drop = FALSE]
  if (nrow(rows) == 0L) return(NULL)
  regs <- as.character(rows$region)
  regs <- regs[!is.na(regs)]
  if (length(regs) == 0L) NULL else unique(regs)
}

# Build the (vintage, cluster) grid for one technology; NULL when there is
# nothing to expand.
.variant_grid <- function(tech) {
  vins <- .variant_levels(tech, "vintage")
  clus <- .variant_levels(tech, "cluster")
  if (length(vins) == 0L && length(clus) == 0L) return(NULL)
  # `.variant_levels()` already returns the intended order -- declared `order`
  # for clusters, sorted otherwise -- so do NOT re-sort here, or a declared
  # ranking (best, mid, poor) would be shuffled back to alphabetical.
  expand.grid(
    vintage = if (length(vins)) sort(vins) else NA_character_,
    cluster = if (length(clus)) clus else NA_character_,
    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
  )
}

# Reduce a label to the token used inside a variant name.
.variant_token <- function(x) gsub("[^[:alnum:]]", "", as.character(x))

# Variant set name. Base names already satisfy `check_name()`; labels are
# sanitised to alphanumerics so the result still does.
.variant_name <- function(base, vintage = NA, cluster = NA,
                          prefix = .variant_prefix_default()) {
  out <- base
  if (!is.na(vintage)) {
    out <- paste0(out, prefix[["vintage"]], .variant_token(vintage))
  }
  if (!is.na(cluster)) {
    out <- paste0(out, prefix[["cluster"]], .variant_token(cluster))
  }
  out
}

# Rows of one slot belonging to a given cell. NA in a selector column broadcasts
# to every variant; "TOTAL" rows are group aggregates and are dropped here (they
# become constraints instead). The selector columns are then pinned to the cell.
.variant_timeslice <- function(d, vintage, cluster) {
  d <- as.data.frame(d)
  if (nrow(d) == 0L) return(d)
  keep <- rep(TRUE, nrow(d))
  for (dm in .variant_dims) {
    if (!dm %in% names(d)) next
    v <- as.character(d[[dm]])
    cell <- if (dm == "vintage") vintage else cluster
    keep <- keep & (is.na(v) | (v != .VARIANT_TOTAL & (is.na(cell) | v == cell)))
  }
  d <- d[keep, , drop = FALSE]
  # nothing left for this cell (e.g. the slot held only "TOTAL" group-bound
  # rows): return the empty frame as-is -- assigning a length-1 value into a
  # zero-row column would error.
  if (nrow(d) == 0L) {
    rownames(d) <- NULL
    return(d)
  }
  for (dm in .variant_dims) {
    if (dm %in% names(d)) {
      d[[dm]] <- if (dm == "vintage") as.character(vintage) else as.character(cluster)
    }
  }
  rownames(d) <- NULL
  d
}

# Investment-window semantics: `start`/`end` are user-defined and may
# overlap across vintages. An NA `start` means "all
# years up to `end`", an NA `end` means "all years from `start` on", and
# both NA means the full model horizon — exactly the pre-merge `@start`/
# `@end` semantics, now uniform for vintaged and plain technologies.
# (The earlier behaviour — defaulting empty fields to the vintage year —
# is retired; declare `start = end = <year>` for a point window.)
.variant_default_window <- function(vin_df, vintage) {
  vin_df
}

# Validate one process object's variant declaration before expanding it.
.variant_validate <- function(tech) {
  nm <- tech@name
  cls <- class(tech)[1]
  # "TOTAL" is only meaningful on the bound columns of @capacity. Elsewhere it
  # is a user error -- and it must be caught, because `.variant_levels()`
  # deliberately filters the token out, so such a row would otherwise be
  # silently ignored rather than becoming a variant.
  # Slots where the token IS meaningful: `capacity` (stock bounds) plus any slot
  # declaring flow bounds. The builders check the COLUMN, so a TOTAL row landing
  # in an accepted slot but on a non-boundable column is caught there rather
  # than dropped.
  vdef   <- .variant_def(tech)
  fb_def <- vdef$flow_bounds
  ab_def <- vdef$af_bounds
  tot_ok <- c("capacity",
              if (length(fb_def)) vapply(fb_def, function(x) x$slot,
                                         character(1)) else character(),
              if (length(ab_def)) vapply(ab_def, function(x) x$slot,
                                         character(1)) else character())
  # A TOTAL row landing in an accepted slot must carry a value some builder can
  # use. The token is filtered out of the variant levels, so a row that no
  # builder claims creates neither a variant nor a bound -- it does nothing at
  # all. Checked here, once, because a slot may feed several builders (`@trade`
  # carries `ava.*` for the flow builder and `af.*` for the availability one).
  acc <- list()
  for (x in fb_def) acc[[x$slot]] <- c(acc[[x$slot]],
    as.vector(outer(x$stems, c(".lo", ".up", ".fx"), paste0)))
  for (x in ab_def) acc[[x$slot]] <- c(acc[[x$slot]],
    paste0(x$stem, c(".lo", ".up", ".fx")))
  for (sl in names(acc)) {
    if (!.hasSlot(tech, sl)) next
    d <- as.data.frame(slot(tech, sl))
    if (!nrow(d)) next
    dm <- intersect(.variant_dims, names(d))
    if (!length(dm)) next
    tot <- Reduce(`|`, lapply(dm, function(x)
      !is.na(d[[x]]) & d[[x]] == .VARIANT_TOTAL))
    if (!any(tot)) next
    cols <- intersect(acc[[sl]], names(d))
    for (i in which(tot)) {
      if (length(cols) && any(!is.na(unlist(d[i, cols, drop = FALSE])))) next
      stop('A "', .VARIANT_TOTAL, '" row in `', sl, '` of ', cls, ' "', nm,
           '" carries no group-boundable value. On this slot the token applies ',
           'to ', paste(sort(acc[[sl]]), collapse = ", "),
           '; a row without one of those creates neither a variant nor a bound.',
           call. = FALSE)
    }
  }

  for (s in setdiff(.variant_slots_of(tech), tot_ok)) {
    if (!.hasSlot(tech, s)) next
    d <- as.data.frame(slot(tech, s))
    if (nrow(d) == 0L) next
    for (dm in intersect(.variant_dims, names(d))) {
      if (any(!is.na(d[[dm]]) & d[[dm]] == .VARIANT_TOTAL)) {
        stop('"', .VARIANT_TOTAL, '" in `', dm, '` is only supported on the ',
             'bound columns of `capacity` (', paste(.variant_bound_cols,
             collapse = ", "), '), not in slot `', s, '`: ', cls, ' "', nm, '".')
      }
    }
  }
  # stock must not be broadcast across variants
  if (.hasSlot(tech, "capacity")) {
    cap <- as.data.frame(tech@capacity)
    if (nrow(cap) > 0L && "stock" %in% names(cap)) {
      st <- cap[!is.na(cap$stock), , drop = FALSE]
      if (nrow(st) > 0L) {
        if ("cluster" %in% names(st) && length(.variant_levels(tech, "cluster")) > 0L &&
            any(is.na(st$cluster))) {
          stop('Existing `stock` must name its `cluster` once ', cls, ' "', nm,
               '" is clustered: stock cannot be broadcast across clusters ',
               '(that would duplicate it).')
        }
        if ("stock" %in% names(st) && "vintage" %in% names(st) &&
            any(is.na(st$vintage)) && length(.variant_levels(tech, "vintage")) > 0L) {
          message("Technology '", nm, "': `stock` rows without a `vintage` are ",
                  "assigned to the earliest vintage.")
        }
      }
    }
  }
  # ── cluster labels ─────────────────────────────────────────────────────────
  dc      <- .declared_clusters(tech)
  by_slot <- .variant_levels_by_slot(tech, "cluster")
  used    <- unique(unlist(by_slot, use.names = FALSE))

  if (!is.null(dc)) {
    # Declared clusters are authoritative: an undeclared label is a typo, not an
    # extra variant. Left unchecked it becomes a technology with whatever data
    # happens to carry the misspelling and DEFAULTS for everything else -- e.g.
    # no investment cost and no capacity bound, i.e. free and unlimited.
    undeclared <- setdiff(used, dc$cluster)
    if (length(undeclared) > 0L) {
      where <- vapply(undeclared, function(u) {
        paste(names(by_slot)[vapply(by_slot, function(z) u %in% z, logical(1))],
              collapse = ", ")
      }, character(1))
      stop('Undeclared cluster label(s) in ', cls, ' "', nm, '": ',
           paste0('"', undeclared, '" (in ', where, ')', collapse = "; "),
           '. Declared clusters are: ',
           paste0('"', dc$cluster, '"', collapse = ", "),
           '. Add the label to the `cluster` slot or fix the spelling.')
    }
    if (anyDuplicated(dc$cluster) > 0L) {
      stop('Duplicated cluster label(s) in the `cluster` slot of ', cls, ' "',
           nm, '": ',
           paste(unique(dc$cluster[duplicated(dc$cluster)]), collapse = ", "))
    }
  } else if (length(used) > 1L && length(by_slot) > 1L) {
    # No declaration to check against: fall back to cross-slot consistency. A
    # label present in some variant-varying slots but missing from others is the
    # signature of a typo (a real cluster is normally given a cost AND a limit).
    missing_in <- lapply(used, function(u) {
      names(by_slot)[!vapply(by_slot, function(z) u %in% z, logical(1))]
    })
    names(missing_in) <- used
    partial <- used[lengths(missing_in) > 0L]
    if (length(partial) > 0L) {
      msg <- paste0('"', partial, '" missing from: ',
                    vapply(missing_in[partial], paste, character(1),
                           collapse = ", "), collapse = "; ")
      stop('Inconsistent cluster labels in ', cls, ' "', nm, '": ', msg,
           '. Every cluster should appear in each slot that differentiates ',
           'clusters, otherwise a misspelling silently creates an extra ',
           'technology with default (often unbounded) parameters. Declare the ',
           'clusters in the `cluster` slot to state the intended set ',
           'explicitly, or give each cluster a value in every listed slot.')
    }
  }

  # A class with no `@region` slot has no region scope for a cluster to
  # restrict. Refused here, earlier and more legibly than the expansion loop
  # could -- though `trade@cluster` has no `region` column at all, so the usual
  # route to this is a hand-built declaration frame.
  if (!is.null(dc) && !.hasSlot(tech, "region") &&
      "region" %in% names(dc) && any(!is.na(dc$region))) {
    stop(cls, ' "', nm, '" has no `region` slot, so a `region` on its `cluster` ',
         'declaration cannot restrict anything. Remove the column.',
         call. = FALSE)
  }

  # Cluster SHARES: the fractions of one process's capacity its parallel parts
  # occupy. Used by loss tranches, where the derived efficiencies
  # `lambda_t = loss_full * (alpha_{t-1} + alpha_t)` are calibrated ONLY when
  # the shares sum to 1 -- shares summing to 0.9 understate total losses by 10%
  # with no other symptom, which is why this is an error and not a warning.
  if (!is.null(dc) && "share" %in% names(dc)) {
    sh <- dc$share
    if (any(!is.na(sh))) {
      if (anyNA(sh)) {
        stop('Either every declared cluster of ', cls, ' "', nm, '" carries a ',
             '`share` or none does; got ', sum(!is.na(sh)), ' of ', length(sh),
             '. A partly-shared set has no defensible reading.', call. = FALSE)
      }
      if (any(!is.finite(sh) | sh <= 0)) {
        stop('The `share` values of ', cls, ' "', nm, '" must be finite and ',
             'greater than zero; got ',
             paste(format(sh), collapse = ", "), '.', call. = FALSE)
      }
      if (abs(sum(sh) - 1) > 1e-8) {
        stop('The `share` values of ', cls, ' "', nm, '" sum to ',
             format(sum(sh)), ', not 1. Shares are FRACTIONS of a capacity, ',
             'not absolute capacities -- the tranche efficiencies were derived ',
             'from them and would be wrong.', call. = FALSE)
      }
    }
  }

  # @vintage must not reference a (cluster, region) the declaration excludes
  if (!is.null(dc) && .hasSlot(tech, "vintage")) {
    v <- as.data.frame(tech@vintage)
    if (nrow(v) > 0L && all(c("cluster", "region") %in% names(v))) {
      chk <- v[!is.na(v$cluster) & v$cluster != .VARIANT_TOTAL &
                 !is.na(v$region), , drop = FALSE]
      for (i in seq_len(nrow(chk))) {
        regs <- .cluster_regions(dc, chk$cluster[i])
        if (!is.null(regs) && !chk$region[i] %in% regs) {
          stop('`vintage` row for cluster "', chk$cluster[i], '" names region "',
               chk$region[i], '", but the `cluster` slot restricts that cluster ',
               'to: ', paste(regs, collapse = ", "), ' (', cls, ' "', nm, '").')
        }
      }
    }
  }

  # at most one NA-region row per (vintage, cluster) cell in @vintage
  if (.hasSlot(tech, "vintage")) {
    v <- as.data.frame(tech@vintage)
    if (nrow(v) > 0L && "region" %in% names(v)) {
      na_reg <- v[is.na(v$region), , drop = FALSE]
      if (nrow(na_reg) > 1L) {
        key <- paste(na_reg$vintage, na_reg$cluster, sep = "\r")
        if (any(duplicated(key))) {
          stop('More than one region-agnostic row for the same ',
               '(vintage, cluster) in `@vintage` of ', cls, ' "', nm, '".')
        }
      }
    }
  }
  invisible(TRUE)
}

# -------------------------------------------------------------------------- #
# group-aggregate bounds ("TOTAL")
#
# A bound row whose `vintage`/`cluster` is the reserved token "TOTAL" applies to
# the SUM over that dimension instead of to each variant separately. Since the
# variants are ordinary technologies, such a bound is just a user constraint
# summing `vTechCap`/`vTechNewCap` over the variant subset -- it rides the
# existing `newConstraint()` path and needs no new equation.
#
#   cluster = "TOTAL", vintage = NA   -> one constraint per vintage,
#                                        summing that vintage's clusters
#   vintage = "TOTAL", cluster = NA   -> one constraint per cluster
#   both    = "TOTAL"                 -> a single constraint over everything
# -------------------------------------------------------------------------- #

# bound column stem -> variable it constrains, per class (see `.variant_classes`)
# bound suffix -> relation
.variant_bound_eq <- c(lo = ">=", up = "<=", fx = "==")

# Constraints tying the capacities of a process's clusters to fixed PROPORTIONS
# of one another, from the `share` column of its `@cluster` declaration.
#
# This is what makes loss tranches hold their shape when capacity is a decision.
# A group-aggregate ("TOTAL") bound is NOT sufficient and is not a substitute: it
# caps the SUM over variants -- the corridor size -- and says nothing about the
# SPLIT, which is the entire content of the model. With a TOTAL cap of 4 and
# three tranches the optimum is (4, 0, 0): tranche 1 has the best `teff` at the
# same per-unit capex, so the piecewise-linear loss curve silently collapses back
# to a single low-loss straight line, leaving the model MORE optimistic about
# losses than the flat-`teff` model it replaced.
#
# Emitted here rather than by the user-facing helper because the variant names
# are minted inside `expand_variants()` from `config@variant_prefix`; a
# pre-computed name would be wrong for any model that overrides the prefix, and
# would quietly reference a `trade` set member that does not exist.
#
# Ties are WITHIN a vintage, never across: each vintage is a physically separate
# line. Two vintages x three tranches is 2 x 2 = 4 constraints, not 5.
.variant_share_constraints <- function(tech, prov, regions, years) {
  def <- .variant_def(tech)
  if (is.null(def)) return(list())
  dc <- .declared_clusters(tech)
  if (is.null(dc) || !"share" %in% names(dc)) return(list())
  dc <- dc[!is.na(dc$share), , drop = FALSE]
  if (nrow(dc) < 2L) return(list())
  bvar <- def$bound_var$cap
  if (is.null(bvar)) return(list())
  cls <- class(tech)[1]
  dc <- dc[order(dc$order, dc$cluster, na.last = TRUE), , drop = FALSE]

  # A fully FIXED set of capacities already determines the proportions, so a tie
  # would be a redundant equality stacked on two fixed bounds. Verify instead --
  # editing the shares and forgetting the capacities is the likeliest silent
  # error, and it leaves the ratings disagreeing with the efficiencies.
  fx <- .cluster_fixed_caps(tech, dc$cluster, def)
  if (!is.null(fx)) {
    if (anyNA(fx)) {
      stop(cls, ' "', tech@name, '": some clusters have a fixed capacity and ',
           'some do not (', paste(dc$cluster[is.na(fx)], collapse = ", "),
           ' missing). A partly-fixed set of shares has no defensible reading ',
           '-- fix all of them or none.', call. = FALSE)
    }
    ratio <- fx / sum(fx)
    if (any(abs(ratio - dc$share) > 1e-6)) {
      stop(cls, ' "', tech@name, '": the fixed capacities ',
           paste(format(fx), collapse = ", "), ' are in proportion ',
           paste(format(round(ratio, 6)), collapse = ", "),
           ', but the declared shares are ',
           paste(format(dc$share), collapse = ", "),
           '. The tranche efficiencies were derived from the shares, so the ',
           'ratings and the losses now disagree.', call. = FALSE)
    }
    return(list())
  }

  cell_args <- list(year = years)
  if ("region" %in% def$bound_dims) {
    cell_args <- c(list(region = regions), cell_args)
  }
  cells <- do.call(expand.grid, c(cell_args,
    list(KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)))

  vins <- unique(prov$vintage[prov$base == tech@name])
  if (!length(vins)) vins <- NA_character_
  out <- list()
  for (vin in vins) {
    pick <- function(cl) {
      sel <- prov$base == tech@name & !is.na(prov$cluster) & prov$cluster == cl &
        (if (is.na(vin)) is.na(prov$vintage) else
           (!is.na(prov$vintage) & prov$vintage == vin))
      as.character(prov$name[sel])
    }
    nref <- pick(dc$cluster[1])
    if (length(nref) != 1L) next
    for (i in seq_len(nrow(dc))[-1]) {
      nvar <- pick(dc$cluster[i])
      if (length(nvar) != 1L) next
      nm <- paste0(.VARIANT_SHARE_PREFIX, .variant_token(tech@name),
                   if (!is.na(vin)) .variant_token(vin) else "",
                   .variant_token(dc$cluster[i]))
      cns <- newConstraint(
        name = nm,
        desc = paste0("Capacity share tie: ", dc$cluster[i], " / ",
                      dc$cluster[1], " = ", format(dc$share[i]), " / ",
                      format(dc$share[1]), " for ", tech@name),
        eq = "==", for.each = cells, rhs = 0, defVal = 0,
        # A share ratio is exact, not a quantity to smooth between milestones.
        interpolation = "inter",
        t1 = list(variable = bvar,
                  for.sum = stats::setNames(list(nvar), def$key),
                  mult = dc$share[1]),
        t2 = list(variable = bvar,
                  for.sum = stats::setNames(list(nref), def$key),
                  mult = -dc$share[i]))
      cns@misc$.variant_source <- tech@name
      cns@misc$.variant_class <- cls
      out[[nm]] <- cns
    }
  }
  out
}

# Per-cluster FIXED capacity, or NULL when the class/slot cannot carry one.
# Returns one value per requested cluster, NA where that cluster has no `.fx`.
.cluster_fixed_caps <- function(tech, clusters, def) {
  if (!.hasSlot(tech, "capacity")) return(NULL)
  cap <- as.data.frame(tech@capacity)
  if (nrow(cap) == 0L || !"cluster" %in% names(cap)) return(NULL)
  bpre <- if (is.null(def$bound_prefix)) "" else def$bound_prefix
  col <- paste0(bpre, "cap.fx")
  if (!col %in% names(cap)) return(NULL)
  vals <- vapply(clusters, function(cl) {
    r <- cap[!is.na(cap$cluster) & cap$cluster == cl, , drop = FALSE]
    v <- r[[col]][!is.na(r[[col]])]
    if (length(v)) as.numeric(v[1]) else NA_real_
  }, numeric(1))
  if (all(is.na(vals))) return(NULL)
  unname(vals)
}

# Group-aggregate bounds on a FLOW, from a `"TOTAL"` token in the vintage or
# cluster column of a non-capacity slot.
#
# Capacity group bounds (above) constrain a stock: one summand, one variable, no
# selector columns. A flow bound differs in two ways. It can vary by TIMESLICE,
# which the capacity cell grid has no room for; and its variable may carry
# selector dimensions -- `vTradeIr`'s `src`/`dst` -- that identify WHICH flow is
# bounded. Those are pinned inside `for.sum`, not `for.each`, because they are
# dimension names rather than sets, and one constraint is emitted per key
# combination.
#
# Why this is needed rather than splitting the bound across variants: a tranched
# line already has its CAPACITY split, so a per-tranche `ava.up` of half the
# line's limit forces flow onto the dearer tranche before the cheap one is full.
# On a 100-unit line in two tranches with a 50 flow limit that doubles the
# modelled loss (2 instead of the true 1). Only the aggregate is correct.
.variant_flow_group_constraints <- function(tech, prov, regions, years, slices) {
  def <- .variant_def(tech)
  if (is.null(def) || is.null(def$flow_bounds)) return(list())
  cls <- class(tech)[1]
  out <- list()

  for (fb in def$flow_bounds) {
    if (!.hasSlot(tech, fb$slot)) next
    d <- as.data.frame(slot(tech, fb$slot))
    dims <- intersect(.variant_dims, names(d))
    if (nrow(d) == 0L || length(dims) == 0L) next

    is_tot <- Reduce(`|`, lapply(dims, function(x)
      !is.na(d[[x]]) & d[[x]] == .VARIANT_TOTAL))
    rows <- d[is_tot, , drop = FALSE]
    if (nrow(rows) == 0L) next

    bcols <- as.vector(outer(fb$stems, c(".lo", ".up", ".fx"), paste0))
    lv <- lapply(stats::setNames(dims, dims),
                 function(x) .variant_levels(tech, x))

    for (i in seq_len(nrow(rows))) {
      r <- rows[i, , drop = FALSE]
      tot   <- dims[vapply(dims, function(x) !is.na(r[[x]]) &&
                             r[[x]] == .VARIANT_TOTAL, logical(1))]
      per   <- dims[vapply(dims, function(x) is.na(r[[x]]), logical(1))]
      fixed <- setdiff(dims, c(tot, per))
      per   <- per[vapply(per, function(x) length(lv[[x]]) > 0L, logical(1))]

      hit <- intersect(bcols, names(r))
      hit <- hit[vapply(hit, function(bc) !is.na(r[[bc]]), logical(1))]
      # A row this builder cannot use may still be for another one -- `@trade`
      # carries both `ava.*` (here) and `af.*`. The "usable by nobody" case is
      # caught once, in .variant_validate().
      if (!length(hit)) next

      # Which key combinations (e.g. routes) this row speaks for: the value it
      # names, or every one the object declares.
      keyvals <- lapply(stats::setNames(fb$keys, fb$keys), function(k) {
        if (k %in% names(r) && !is.na(r[[k]])) as.character(r[[k]])
        else unique(as.character(d[[k]][!is.na(d[[k]])]))
      })
      keygrid <- if (length(fb$keys) && all(lengths(keyvals) > 0)) {
        expand.grid(keyvals, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
      } else data.frame(.one = 1L)

      grid <- if (length(per)) {
        expand.grid(lv[per], KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
      } else data.frame(.one = 1L)

      for (bc in hit) {
        val <- r[[bc]]
        sfx <- sub("^.*[.]", "", bc)
        for (g in seq_len(nrow(grid))) {
          keep <- rep(TRUE, nrow(prov))
          for (x in per)   keep <- keep & !is.na(prov[[x]]) & prov[[x]] == grid[[x]][g]
          for (x in fixed) keep <- keep & !is.na(prov[[x]]) & prov[[x]] == r[[x]]
          variants <- prov$name[keep]
          if (length(variants) < 1L) next

          for (kk in seq_len(nrow(keygrid))) {
            # `for.each` carries only the cell dims the bound may vary over,
            # restricted to what the row named.
            cell_args <- list()
            for (cd in fb$cells) {
              pool <- switch(cd, year = years, timeslice = slices,
                             region = regions, NULL)
              if (is.null(pool)) next
              cell_args[[cd]] <- if (cd %in% names(r) && !is.na(r[[cd]]))
                (if (cd == "year") as.integer(r[[cd]]) else as.character(r[[cd]]))
                else pool
            }
            cells <- do.call(expand.grid, c(cell_args,
              list(KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)))
            if (!nrow(cells)) next

            fs <- c(stats::setNames(list(variants), def$key),
                    lapply(stats::setNames(fb$keys, fb$keys),
                           function(k) keygrid[[k]][kk]))
            nm <- paste0(.VARIANT_GROUP_PREFIX, .variant_token(tech@name),
                         .variant_token(bc),
                         if (length(per)) paste0(vapply(per, function(x)
                           .variant_token(grid[[x]][g]), character(1)),
                           collapse = "") else "",
                         if (length(fb$keys)) paste0(vapply(fb$keys, function(k)
                           .variant_token(keygrid[[k]][kk]), character(1)),
                           collapse = "") else "")
            cns <- newConstraint(
              name = nm,
              desc = paste0("Group ", sfx, " bound on ", sub("[.].*$", "", bc),
                            " across ", paste(tot, collapse = "+"), " for ",
                            tech@name),
              eq = .variant_bound_eq[[sfx]],
              for.each = cells,
              term1 = list(variable = fb$var, for.sum = fs),
              defVal = as.numeric(val))
            cns@misc$.variant_source <- tech@name
            cns@misc$.variant_class <- cls
            out[[nm]] <- cns
          }
        }
      }
    }
  }
  out
}

# Group-aggregate AVAILABILITY-FACTOR bounds: a `"TOTAL"` token on `@af` /
# `@afs`, bounding the summed ACTIVITY of a group of variants against their
# summed CAPACITY.
#
#   sum_k act_k  <op>  af * cap2act * share_s * sum_k cap_k
#
# Unlike a capacity group bound this has TWO summands and a coefficient, and the
# coefficient varies by timeslice (`pTimesliceShare[s]`) -- carried in a
# timeslice-keyed `mult`, which is only usable because the data.frame-`mult`
# defect on multi-indexed variables was fixed.
#
# `afs` differs from `af` in one way: the activity side is summed over the
# DESCENDANTS of the named slice rather than taken at that slice, so the
# constraint is emitted once per named parent with the descendants listed in
# `for.sum`. `timeslice_ancestry` is a transitive closure, so that is a lookup.
#
# WHAT THIS IS AND IS NOT. A group bound is a constraint in its own right --
# "however good the weather, the fleet cannot exceed X% of nameplate". It is NOT
# the sum of the per-variant limits: those carry each variant's own weather
# factor, a `prod{}` only the solver template can evaluate. That sum is still
# enforced, because the per-variant constraints remain. A group bound with its
# OWN weather profile would need a model equation and is not attempted here.
.variant_af_group_constraints <- function(tech, prov, regions, years, slices,
                                          shares, ancestry) {
  def <- .variant_def(tech)
  if (is.null(def) || is.null(def$af_bounds)) return(list())
  cls <- class(tech)[1]
  c2a <- tryCatch(as.numeric(tech@cap2act), error = function(e) 1)
  if (!length(c2a) || !is.finite(c2a)) c2a <- 1
  shr <- function(x) {
    v <- shares$share[match(as.character(x), as.character(shares$timeslice))]
    ifelse(is.na(v), 1, v)
  }
  out <- list()

  for (ab in def$af_bounds) {
    if (!.hasSlot(tech, ab$slot)) next
    d <- as.data.frame(slot(tech, ab$slot))
    dims <- intersect(.variant_dims, names(d))
    if (nrow(d) == 0L || length(dims) == 0L) next
    is_tot <- Reduce(`|`, lapply(dims, function(x)
      !is.na(d[[x]]) & d[[x]] == .VARIANT_TOTAL))
    rows <- d[is_tot, , drop = FALSE]
    if (nrow(rows) == 0L) next

    bcols <- paste0(ab$stem, c(".lo", ".up", ".fx"))
    lv <- lapply(stats::setNames(dims, dims),
                 function(x) .variant_levels(tech, x))

    for (i in seq_len(nrow(rows))) {
      r <- rows[i, , drop = FALSE]
      tot   <- dims[vapply(dims, function(x) !is.na(r[[x]]) &&
                             r[[x]] == .VARIANT_TOTAL, logical(1))]
      per   <- dims[vapply(dims, function(x) is.na(r[[x]]), logical(1))]
      fixed <- setdiff(dims, c(tot, per))
      per   <- per[vapply(per, function(x) length(lv[[x]]) > 0L, logical(1))]

      hit <- intersect(bcols, names(r))
      hit <- hit[vapply(hit, function(bc) !is.na(r[[bc]]), logical(1))]
      if (!length(hit)) next

      # Which timeslice cells this row speaks for. For `af` each named slice is
      # its own cell; for `afs` the named slice is a PARENT and the activity is
      # summed over its descendants.
      parents <- if ("timeslice" %in% names(r) && !is.na(r$timeslice))
        as.character(r$timeslice) else slices
      regs <- if ("region" %in% names(r) && !is.na(r$region))
        as.character(r$region) else regions
      yrs  <- if ("year" %in% names(r) && !is.na(r$year))
        as.integer(r$year) else years

      grid <- if (length(per)) {
        expand.grid(lv[per], KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
      } else data.frame(.one = 1L)

      for (bc in hit) {
        val <- as.numeric(r[[bc]])
        sfx <- sub("^.*[.]", "", bc)
        for (g in seq_len(nrow(grid))) {
          keep <- rep(TRUE, nrow(prov))
          for (x in per)   keep <- keep & !is.na(prov[[x]]) & prov[[x]] == grid[[x]][g]
          for (x in fixed) keep <- keep & !is.na(prov[[x]]) & prov[[x]] == r[[x]]
          variants <- prov$name[keep]
          if (length(variants) < 1L) next

          for (ps in parents) {
            act_slices <- if (isTRUE(ab$sum_children)) {
              unique(c(ps, ancestry$child[as.character(ancestry$parent) == ps]))
            } else ps
            # The capacity side is weighted by the share of the NAMED slice --
            # the parent's own share for `afs`, the slice's for `af`.
            coef <- -val * c2a * shr(ps)
            cells <- expand.grid(region = regs, year = yrs,
                                 KEEP.OUT.ATTRS = FALSE,
                                 stringsAsFactors = FALSE)
            if (!nrow(cells)) next
            nm <- paste0(.VARIANT_GROUP_PREFIX, .variant_token(tech@name),
                         .variant_token(bc),
                         if (length(per)) paste0(vapply(per, function(x)
                           .variant_token(grid[[x]][g]), character(1)),
                           collapse = "") else "",
                         .variant_token(ps))
            cns <- newConstraint(
              name = nm,
              desc = paste0("Group ", sfx, " ", ab$stem, " bound across ",
                            paste(tot, collapse = "+"), " for ", tech@name,
                            " at ", ps),
              eq = .variant_bound_eq[[sfx]],
              for.each = cells,
              term1 = list(
                variable = ab$act,
                for.sum = c(stats::setNames(list(variants), def$key),
                            list(timeslice = act_slices))),
              term2 = list(
                variable = ab$cap,
                for.sum = stats::setNames(list(variants), def$key),
                mult = coef),
              rhs = 0, defVal = 0, interpolation = "inter")
            cns@misc$.variant_source <- tech@name
            cns@misc$.variant_class <- cls
            out[[nm]] <- cns
          }
        }
      }
    }
  }
  out
}

.variant_group_constraints <- function(tech, prov, regions, years) {
  def <- .variant_def(tech)
  if (is.null(def) || !.hasSlot(tech, "capacity")) return(list())
  cls <- class(tech)[1]
  bvar <- def$bound_var
  bdims <- def$bound_dims

  cap <- as.data.frame(tech@capacity)
  dims <- intersect(.variant_dims, names(cap))
  if (nrow(cap) == 0L || length(dims) == 0L) return(list())

  is_tot <- Reduce(`|`, lapply(dims, function(d)
    !is.na(cap[[d]]) & cap[[d]] == .VARIANT_TOTAL))
  rows <- cap[is_tot, , drop = FALSE]
  if (nrow(rows) == 0L) return(list())

  lv <- lapply(stats::setNames(dims, dims), function(d) .variant_levels(tech, d))
  out <- list()
  acc <- list()   # constraint name -> accumulated cells + summands

  for (i in seq_len(nrow(rows))) {
    r <- rows[i, , drop = FALSE]
    tot   <- dims[vapply(dims, function(d) !is.na(r[[d]]) &&
                           r[[d]] == .VARIANT_TOTAL, logical(1))]
    per   <- dims[vapply(dims, function(d) is.na(r[[d]]), logical(1))]
    fixed <- setdiff(dims, c(tot, per))
    per   <- per[vapply(per, function(d) length(lv[[d]]) > 0L, logical(1))]

    # Scan EVERY bound-shaped column the row carries, and look the stem up in
    # `bound_var`. Deriving the scan list from a prefix instead let a column the
    # class does have -- storage's `inp.*` / `stg.*` -- match nothing and be
    # skipped in silence. An unsupported stem must be loud.
    for (bc in grep("[.](lo|up|fx)$", names(r), value = TRUE)) {
      val <- r[[bc]]
      if (is.na(val)) next
      stem <- sub("[.](lo|up|fx)$", "", bc)
      sfx  <- sub("^.*[.]", "", bc)
      if (is.null(bvar[[stem]])) {
        stop('Group-aggregate ("', .VARIANT_TOTAL, '") bounds on ', cls, ' "',
             tech@name, '" are supported on ',
             paste0(names(bvar), ".*", collapse = ", "),
             ' but not on `', bc, '`. The retirement variables carry a second ',
             'year index (and no retirement equation exists for storage or ',
             'trade), so a group retirement bound must be written explicitly ',
             'with `newConstraint()`.', call. = FALSE)
      }
      grid <- if (length(per)) {
        expand.grid(lv[per], KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
      } else {
        data.frame(.one = 1L)
      }

      for (g in seq_len(nrow(grid))) {
        keep <- rep(TRUE, nrow(prov))
        for (d in per)   keep <- keep & !is.na(prov[[d]]) & prov[[d]] == grid[[d]][g]
        for (d in fixed) keep <- keep & !is.na(prov[[d]]) & prov[[d]] == r[[d]]
        variants <- prov$name[keep]
        if (length(variants) < 1L) next

        # The name identifies (technology, bound column, per-level cell) plus
        # region/year ONLY when the user restricted them.
        #
        # Ideally region and year would be pure data -- one constraint whose RHS
        # is a (region, year) table. `newConstraint()` cannot represent that:
        # its RHS interpolation collapses the region dimension and AVERAGES the
        # values (a per-region 5 and 8 come back as region = NA, value = 6.5),
        # so a region-varying bound is silently wrong. Until that is fixed, a
        # restricted region/year is part of the identity, which keeps every
        # bound correct and distinct. The common case -- one bound for all
        # regions and years -- still yields exactly one constraint.
        rg <- if ("region" %in% names(r) && !is.na(r$region))
          as.character(r$region) else NA_character_
        yr <- if ("year" %in% names(r) && !is.na(r$year)) as.integer(r$year) else NA_integer_
        # A class whose capacity variable has no region index cannot carry a
        # region-restricted group bound: `newConstraint()` auto-sums an unmatched
        # for.each dimension, so it would silently replicate the same region-free
        # constraint once per region (and be plain wrong for `.fx`).
        if (!is.na(rg) && !"region" %in% bdims) {
          stop('A group-aggregate `', bc, '` bound for ', cls, ' "', tech@name,
               '" names region "', rg, '", but ', bvar[[stem]],
               ' has no region index -- a ', cls,
               ' group bound can only span years. Drop the `region` value.')
        }
        nm <- paste0(.VARIANT_GROUP_PREFIX, .variant_token(tech@name),
                     .variant_token(bc),
                     if (length(per)) paste0(vapply(per, function(d)
                       .variant_token(grid[[d]][g]), character(1)),
                       collapse = "") else "",
                     if (!is.na(rg)) paste0("R", .variant_token(rg)) else "",
                     if (!is.na(yr)) paste0("Y", .variant_token(yr)) else "")

        # `for.each` carries only the dimensions the capacity variable has.
        cell_args <- list(year = if (!is.na(yr)) yr else years)
        if ("region" %in% bdims) {
          cell_args <- c(list(region = if (!is.na(rg)) rg else regions), cell_args)
        }
        cells <- do.call(expand.grid, c(cell_args,
          list(KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)))

        # Same identity twice => the same cells bounded by two different values.
        if (!is.null(acc[[nm]])) {
          stop('Contradictory group-aggregate `', bc, '` bounds for ', cls, ' "',
               tech@name, '": ', format(acc[[nm]]$val), ' and ', format(val),
               ' both apply to ',
               if (is.na(rg)) "all regions" else paste0('region "', rg, '"'),
               " / ",
               if (is.na(yr)) "all years" else paste0("year ", yr),
               ". Give one bound per (region, year).")
        }
        acc[[nm]] <- list(cells = cells, variants = variants, bc = bc,
                          sfx = sfx, stem = stem, tot = tot, val = val)
      }
    }
  }

  for (nm in names(acc)) {
    a <- acc[[nm]]
    cns <- newConstraint(
      name = nm,
      desc = paste0("Group ", a$sfx, " bound on ", a$stem, " across ",
                    paste(a$tot, collapse = "+"), " for ", tech@name),
      eq = .variant_bound_eq[[a$sfx]],
      # for.each is exactly the bounded cells: there is no non-binding default
      # for `.fx`, so spanning every cell would pin the sum to the default
      # wherever the user gave no bound.
      for.each = a$cells,
      term1 = list(variable = bvar[[a$stem]],
                   for.sum = stats::setNames(list(a$variants), def$key)),
      defVal = as.numeric(a$val)
    )
    cns@misc$.variant_source <- tech@name
    cns@misc$.variant_class <- cls
    out[[nm]] <- cns
  }
  out
}

# Labels are reduced to alphanumerics for the variant name, and that reduction is
# many-to-one: "best-1" and "best 1" both become "best1". Two labels that collapse
# would mint the same variant name and one process would silently replace the
# other, so refuse up front. Must run on the RAW labels, hence per object.
.variant_check_tokens <- function(tech) {
  for (dm in .variant_dims_of(tech)) {
    lv <- .variant_levels(tech, dm)
    if (length(lv) < 1L) next
    tok <- .variant_token(lv)
    empty <- lv[!nzchar(tok)]
    if (length(empty) > 0L) {
      stop('The ', dm, ' label(s) ', paste0('"', empty, '"', collapse = ", "),
           ' of ', class(tech)[1], ' "', tech@name, '" contain no letters or digits, so ',
           'they reduce to an empty variant-name token. Rename them.')
    }
    if (anyDuplicated(tok) > 0L) {
      dups <- unique(tok[duplicated(tok)])
      msg <- vapply(dups, function(t) {
        paste0(paste0('"', lv[tok == t], '"', collapse = " and "),
               ' both reduce to "', t, '"')
      }, character(1))
      stop("Distinct ", dm, " labels of ", class(tech)[1], " \"", tech@name,
           "\" collapse to the same variant-name token: ",
           paste(msg, collapse = "; "),
           ". They would produce one variant instead of two -- rename one, or ",
           "use labels that differ in letters or digits.")
    }
  }
  invisible(TRUE)
}

# -------------------------------------------------------------------------- #
# expansion
# -------------------------------------------------------------------------- #

# Expand one process object into its variants. Returns a list of objects of the
# same class (the original, unchanged, when there is nothing to expand) plus the
# provenance rows.
.expand_one_process <- function(tech, prefix = .variant_prefix_default()) {
  if (is.null(.variant_def(tech))) {
    return(list(objects = list(tech), provenance = NULL))
  }
  grid <- .variant_grid(tech)
  if (is.null(grid)) {
    return(list(objects = list(tech), provenance = NULL))
  }
  .variant_validate(tech)
  .variant_check_tokens(tech)

  earliest <- {
    vv <- .variant_levels(tech, "vintage")
    if (length(vv)) sort(vv)[1] else NA_character_
  }

  dc <- .declared_clusters(tech)

  objs <- vector("list", nrow(grid))
  for (i in seq_len(nrow(grid))) {
    vin <- grid$vintage[i]
    clu <- grid$cluster[i]
    v <- tech
    v@name <- .variant_name(tech@name, vin, clu, prefix)

    # Region-scoped clusters: restrict this variant to the regions the cluster
    # exists in, intersected with any restriction already on the technology.
    if (!is.na(clu)) {
      cregs <- .cluster_regions(dc, clu)
      if (!is.null(cregs)) {
        # A class with no `@region` slot has no region scope to intersect with,
        # and `tech@region` would error rather than say so. `trade` is the case:
        # its scope comes from the route endpoints. Generic on purpose -- the
        # branch below is unchanged byte-for-byte for technology and storage,
        # which both declare the slot.
        if (!.hasSlot(tech, "region")) {
          stop('Cluster "', clu, '" of ', class(tech)[1], ' "', tech@name,
               '" is restricted to region(s) ', paste(cregs, collapse = ", "),
               ', but a ', class(tech)[1], ' has no `region` slot, so there is ',
               'nothing for a cluster region to intersect with. Its scope comes ',
               'from ',
               if (methods::is(tech, "trade")) 'the route endpoints' else
                 'the `region` column of its own data slots',
               '. Drop the `region` column from the `cluster` declaration.',
               call. = FALSE)
        }
        base_regs <- as.character(tech@region)
        v@region <- if (length(base_regs) == 0L) cregs else
          intersect(base_regs, cregs)
        if (length(v@region) == 0L) {
          stop('Cluster "', clu, '" of ', class(tech)[1], ' "', tech@name, '" exists in ',
               'region(s) ', paste(cregs, collapse = ", "),
               ', which do not overlap the technology\'s own `region` (',
               paste(base_regs, collapse = ", "), ').')
        }
      }
    }
    # Keep only this variant's own declaration row. Leaving the whole table on
    # every variant would make `.variant_levels()` report all clusters again, so
    # the expanded model would fail `.assert_variants_expanded()`.
    if (!is.null(dc)) {
      cd <- as.data.frame(tech@cluster)
      v@cluster <- if (is.na(clu)) cd[0, , drop = FALSE] else
        cd[!is.na(cd$cluster) & cd$cluster == clu, , drop = FALSE]
      rownames(v@cluster) <- NULL
    }

    for (s in .variant_slots_of(tech)) {
      if (!.hasSlot(tech, s)) next
      # Every variant slot must be a data.frame: `.variant_timeslice()` coerces
      # with as.data.frame() and the assignment below would then fail with
      # "assignment of an object of class data.frame is not valid for @'...'".
      # Loud here rather than cryptic there.
      if (!is.data.frame(slot(tech, s))) {
        stop('Variant slot "', s, '" of ', class(tech)[1], ' "', tech@name,
             '" is a ', class(slot(tech, s))[1], ', not a data.frame. A slot ',
             'listed in `.variant_classes$', class(tech)[1], '$slots` must be a ',
             'table, or its rows cannot be selected per variant.', call. = FALSE)
      }
      d <- .variant_timeslice(slot(tech, s), vin, clu)
      if (s == "vintage") d <- .variant_default_window(d, vin)
      if (s == "capacity" && "stock" %in% names(d) && !is.na(vin)) {
        # stock without a vintage belongs to the earliest vintage only
        if ("vintage" %in% names(as.data.frame(slot(tech, s)))) {
          raw <- as.data.frame(slot(tech, s))
          orphan <- is.na(raw$vintage) & !is.na(raw$stock)
          if (any(orphan) && !identical(vin, earliest)) {
            d$stock[d$stock %in% raw$stock[orphan]] <- NA_real_
          }
        }
      }
      slot(v, s) <- d
    }
    objs[[i]] <- v
  }

  prov <- data.frame(
    name = vapply(objs, function(z) z@name, character(1)),
    class = class(tech)[1],
    base = tech@name,
    vintage = grid$vintage,
    cluster = grid$cluster,
    stringsAsFactors = FALSE
  )
  list(objects = objs, provenance = prov)
}

# Back-compat wrapper: `dev-scripts/*` and downstream repos call this directly.
.expand_one_tech <- function(tech, prefix = .variant_prefix_default()) {
  .expand_one_process(tech, prefix)
}

# Walk a model's repositories and replace every vintaged/clustered process object
# (any class in `.variant_classes`) with its variants. Non-destructive: the
# caller's model is not modified in place, the returned one is used for the build.
#
# @return list(model = <expanded model>, provenance = <data.frame|NULL>)
# @noRd
expand_variants <- function(mod, prefix = .variant_prefix(mod),
                            verbose = isVerbose()) {
  prefix <- .variant_prefix_merge(prefix)

  regions <- as.character(mod@config@region)
  years   <- tryCatch(as.integer(mod@config@horizon@intervals$mid),
                      error = function(e) integer())
  # Flow bounds (unlike capacity bounds) can vary by timeslice, so the cell grid
  # needs the calendar's slices too.
  slices  <- tryCatch(unique(as.character(
                        mod@config@calendar@timeslice_share$timeslice)),
                      error = function(e) character())
  shares  <- tryCatch(as.data.frame(mod@config@calendar@timeslice_share),
                      error = function(e)
                        data.frame(timeslice = character(), share = numeric()))
  # A transitive closure, so a named parent reaches every descendant in one
  # lookup -- which is what an `afs` group bound sums over.
  ancestry <- tryCatch(as.data.frame(mod@config@calendar@timeslice_ancestry),
                       error = function(e)
                         data.frame(parent = character(), child = character()))

  # ---- 1. COLLECT ---------------------------------------------------------- #
  # Pure traversal: expand each object but mutate nothing yet. Inserting as we go
  # would hide collisions, because `out[[name]] <- obj` on a named list REPLACES
  # a same-named entry -- so two variants that mangle to one name would silently
  # become one and the duplicate check below could never see it.
  #
  # Every class is collected into the SAME `minted`/`survivor` tables, so the
  # cross-class checks are symmetric: a storage variant colliding with a
  # technology (they share `sets$process`) is caught from either direction.
  expanded <- list()   # repo path -> base name -> expansion result
  minted   <- list()   # rows: name, class, base, repo
  survivor <- list()   # rows: name, class, repo  (everything NOT replaced)

  walk <- function(r, path = "") {
    if (!methods::is(r, "repository")) return(invisible(NULL))
    for (nm in names(r@data)) {
      el <- r@data[[nm]]
      if (methods::is(el, "repository")) {
        walk(el, paste0(path, "/", nm))
        next
      }
      res <- if (is.null(.variant_def(el))) NULL else .expand_one_process(el, prefix)
      if (is.null(res) || is.null(res$provenance)) {
        survivor[[length(survivor) + 1L]] <<- data.frame(
          name = el@name, class = class(el)[1], repo = path,
          stringsAsFactors = FALSE)
      } else {
        expanded[[path]][[nm]] <<- res
        minted[[length(minted) + 1L]] <<- data.frame(
          name = res$provenance$name, class = class(el)[1],
          base = el@name, repo = path, stringsAsFactors = FALSE)
      }
    }
    invisible(NULL)
  }
  for (i in seq_along(mod@data)) walk(mod@data[[i]], names(mod@data)[i])

  minted <- if (length(minted)) bind_rows(minted) else NULL

  # ---- 2. CHECK (before anything is inserted) ------------------------------ #
  if (!is.null(minted)) {
    survivor <- if (length(survivor)) bind_rows(survivor) else
      data.frame(name = character(), class = character(), repo = character())

    # 2a. two variants of the same base
    for (b in unique(minted$base)) {
      nmb <- minted$name[minted$base == b]
      if (anyDuplicated(nmb) > 0L) {
        cls_b <- minted$class[minted$base == b][1]
        stop(cls_b, ' "', b, '" produces the same variant name more than ',
             'once: ', paste(unique(nmb[duplicated(nmb)]), collapse = ", "),
             ". Rename a vintage/cluster label.")
      }
    }
    # 2b. variants of DIFFERENT bases colliding with each other -- possibly from
    #     different classes, since they all share `sets$process`
    if (anyDuplicated(minted$name) > 0L) {
      d <- unique(minted$name[duplicated(minted$name)])
      det <- vapply(d, function(n) {
        i <- minted$name == n
        paste0('"', n, '" from ',
               paste(unique(paste0(minted$class[i], ' "', minted$base[i], '"')),
                     collapse = " and "))
      }, character(1))
      stop("Variant expansion produces the same name from more than one ",
           "process: ", paste(det, collapse = "; "),
           ". Rename a base object, rename a label, or change `variant_prefix`.")
    }
    # 2c. variants colliding with an object that is NOT being replaced. All
    #     process classes share `sets$process`, so a clash with a storage or a
    #     trade is as fatal as one with a technology.
    hit <- merge(minted, survivor, by = "name")
    if (nrow(hit) > 0L) {
      det <- vapply(seq_len(nrow(hit)), function(i) {
        sprintf('"%s" (from %s "%s") collides with an existing %s%s',
                hit$name[i], hit$class.x[i], hit$base[i], hit$class.y[i],
                if (nzchar(hit$repo.y[i])) paste0(' in repository "',
                                                  hit$repo.y[i], '"') else "")
      }, character(1))
      stop("Variant name collision: ", paste(det, collapse = "; "),
           ". Rename the base object, rename the vintage/cluster label, or ",
           "change `variant_prefix`.")
    }
    # 2d. validity as a set member
    bad <- unique(minted$name[!vapply(minted$name, check_name, logical(1))])
    if (length(bad) > 0L) {
      stop("Variant name(s) are not valid set members: ",
           paste(bad, collapse = ", "), ".")
    }
  }

  # ---- 3. INSERT ----------------------------------------------------------- #
  prov <- list()
  cns  <- list()
  insert <- function(r, path = "") {
    if (!methods::is(r, "repository")) return(r)
    out <- list()
    for (nm in names(r@data)) {
      el <- r@data[[nm]]
      if (methods::is(el, "repository")) {
        out[[nm]] <- insert(el, paste0(path, "/", nm))
      } else if (!is.null(expanded[[path]][[nm]])) {
        res <- expanded[[path]][[nm]]
        for (v in res$objects) out[[v@name]] <- v
        prov[[length(prov) + 1L]] <<- res$provenance
        gb <- .variant_group_constraints(el, res$provenance, regions, years)
        if (length(gb)) cns[[length(cns) + 1L]] <<- gb
        fb <- .variant_flow_group_constraints(el, res$provenance, regions,
                                              years, slices)
        if (length(fb)) cns[[length(cns) + 1L]] <<- fb
        ab <- .variant_af_group_constraints(el, res$provenance, regions, years,
                                            slices, shares, ancestry)
        if (length(ab)) cns[[length(cns) + 1L]] <<- ab
        sb <- .variant_share_constraints(el, res$provenance, regions, years)
        if (length(sb)) cns[[length(cns) + 1L]] <<- sb
      } else {
        out[[nm]] <- el
      }
    }
    r@data <- out
    r
  }
  for (i in seq_along(mod@data)) {
    mod@data[[i]] <- insert(mod@data[[i]], names(mod@data)[i])
  }

  # group-aggregate bounds become ordinary user constraints on the model
  cns <- unlist(cns, recursive = FALSE, use.names = TRUE)
  if (length(cns)) {
    # Two objects whose names collapse under sanitisation mint the same
    # constraint name; `add(overwrite = TRUE)` below would silently drop one.
    # They may be of different classes, since the constraint namespace is shared.
    if (anyDuplicated(names(cns)) > 0L) {
      d <- unique(names(cns)[duplicated(names(cns))])
      src <- vapply(d, function(n) {
        srcs <- vapply(cns[names(cns) == n], function(k) {
          tt <- k@misc$.variant_source
          cc <- k@misc$.variant_class
          if (is.null(tt)) NA_character_ else
            paste0(if (is.null(cc)) "" else paste0(cc, " "), '"', tt, '"')
        }, character(1))
        paste0('"', n, '" from ', paste(unique(srcs), collapse = " and "))
      }, character(1))
      stop("Group-aggregate bound constraints collide on name: ",
           paste(src, collapse = "; "),
           ". The object names reduce to the same token; rename one.")
    }
    mod <- add(mod, cns, overwrite = TRUE)
    if (verbose) {
      # Names are mangled tokens and there can be hundreds of them (455 on a
      # 41-node tranched network), so report the families and leave the objects
      # to `getObject(scen, class = "constraint")`, which carries a readable
      # `desc` and `misc$.variant_source` for each.
      n_share <- sum(startsWith(names(cns), .VARIANT_SHARE_PREFIX))
      n_group <- length(cns) - n_share
      message("Added ", length(cns), " generated constraint(s): ",
              n_group, " group bound(s), ", n_share, " capacity-share tie(s).")
    }
  }

  prov <- if (length(prov)) bind_rows(prov) else NULL
  if (!is.null(prov) && verbose) {
    n_base <- length(unique(paste(prov$class, prov$base)))
    message("Expanded ", n_base, " process object",
            if (n_base == 1L) "" else "s", " into ", nrow(prov),
            " variants (vintage/cluster).")
  }
  list(model = mod, provenance = prov)
}

# Back-compat wrapper for the technology-only name.
# @noRd
expand_tech_variants <- function(mod, prefix = .variant_prefix(mod),
                                verbose = isVerbose()) {
  expand_variants(mod, prefix, verbose = verbose)
}

# -------------------------------------------------------------------------- #
# reporting
# -------------------------------------------------------------------------- #

#' Process variants of a solved (or interpolated) scenario
#'
#' @description
#' Lists the vintage/cluster variants a scenario was built from, i.e. the
#' provenance table `scenario@modInp@sets$variant`: which base object each
#' variant came from, of which class, and which `(vintage, cluster)` cell it
#' represents.
#'
#' Variant names are an implementation detail of the solver-facing sets; use this
#' table (or `getData(..., variants = TRUE)`) to analyse results by vintage or
#' cluster rather than parsing the names.
#'
#' @param scen a `scenario` object.
#' @param class optional character, keep only variants of these process classes
#'   (e.g. `"technology"`, `"storage"`).
#'
#' @return A data.frame with columns `name`, `class`, `base`, `vintage`,
#'   `cluster`, or `NULL` when the model has no variants.
#' @family technology process
#' @export
getVariants <- function(scen, class = NULL) {
  tv <- tryCatch(scen@modInp@sets$variant, error = function(e) NULL)
  if (is.null(tv) || NROW(tv) == 0) return(NULL)
  tv <- as.data.frame(tv)
  if (!is.null(class)) {
    tv <- tv[tv$class %in% class, , drop = FALSE]
    if (nrow(tv) == 0L) return(NULL)
    rownames(tv) <- NULL
  }
  tv
}

#' Summarise a result variable across technology variants
#'
#' @description
#' Aggregates a solved variable over the variants of each base technology and
#' reports the spread across them: total, mean, standard deviation, min and max,
#' plus the number of variants contributing. This is the "analyse by cluster and
#' vintage" view -- e.g. the spread of capacity factors across wind resource
#' clusters, or of capacity across vintages.
#'
#' @param scen a solved `scenario`.
#' @param name character, name of the variable or parameter (e.g. `"vTechCap"`).
#' @param by character vector, variant dimensions to keep as grouping columns:
#'   any of `"vintage"` and `"cluster"`. Empty (default) summarises across all
#'   variants of a base technology.
#' @param weight optional character, name of a variable to use as weights for
#'   the mean (e.g. `"vTechCap"` to capacity-weight a per-unit quantity). When
#'   `NULL` the mean is unweighted.
#' @param ... further filters passed to [getData()] (e.g. `year = 2050`).
#'
#' @return A data.frame with the grouping columns plus `n`, `total`, `mean`,
#'   `sd`, `min` and `max`.
#' @family technology process
#' @export
variantSummary <- function(scen, name, by = character(), weight = NULL, ...) {
  by <- intersect(by, c("vintage", "cluster"))
  d <- getData(scen, name = name, merge = TRUE, variants = TRUE, ...)
  if (NROW(d) == 0) {
    stop("No data for '", name, "' in scenario '", scen@name, "'.")
  }
  if (!"base" %in% names(d)) {
    stop("Scenario '", scen@name, "' has no process variants.")
  }
  d <- d |> filter(!is.na(.data$base))
  if (nrow(d) == 0) {
    stop("Variable '", name, "' carries no variant processes.")
  }
  # the id column is `tech` / `stg` / `trade`, or `process` when renamed
  idcol <- intersect(c("tech", "stg", "trade", "sup", "expp", "imp",
                       "process"), names(d))[1]
  if (!is.null(weight)) {
    w <- getData(scen, name = weight, merge = TRUE, variants = TRUE, ...)
    keys <- intersect(names(d), names(w))
    keys <- setdiff(keys, c("value", "name", "scenario"))
    w <- w |> select(all_of(c(keys, "value"))) |> rename(.w = "value")
    d <- d |> left_join(w, by = keys)
  } else {
    d$.w <- 1
  }
  grp <- c("base", by, intersect(c("region", "year"), names(d)))
  d |>
    group_by(across(all_of(grp))) |>
    summarise(
      n     = dplyr::n_distinct(.data[[idcol]]),
      total = sum(.data$value, na.rm = TRUE),
      mean  = if (all(is.na(.data$.w)) || sum(.data$.w, na.rm = TRUE) == 0) {
        mean(.data$value, na.rm = TRUE)
      } else {
        stats::weighted.mean(.data$value, .data$.w, na.rm = TRUE)
      },
      sd    = stats::sd(.data$value, na.rm = TRUE),
      min   = min(.data$value, na.rm = TRUE),
      max   = max(.data$value, na.rm = TRUE),
      .groups = "drop"
    ) |>
    as.data.frame()
}

# The process classes are unioned into a single `sets$process`, so a name used
# twice there silently merges two different processes into one solver entity.
# Nothing else in the pipeline notices, so check the assembled namespace once.
#
# Scope note: `comm`, `group` and `weather` are DELIBERATELY excluded. They are
# separate sets, and a model may legitimately use one string for both a commodity
# and a technology; widening this check would break working models for no gain.
# @noRd
.check_process_names <- function(sets) {
  cls <- c(sup = "supply", dem = "demand", tech = "technology", stg = "storage",
           expp = "export", imp = "import", trade = "trade")
  cls <- cls[names(cls) %in% names(sets)]
  if (length(cls) == 0L) return(invisible(TRUE))

  nm  <- unlist(lapply(names(cls), function(s) as.character(sets[[s]])),
                use.names = FALSE)
  cl  <- rep(unname(cls), vapply(names(cls), function(s) length(sets[[s]]),
                                 integer(1)))
  if (length(nm) == 0L) return(invisible(TRUE))

  dup <- unique(nm[duplicated(nm)])
  if (length(dup) == 0L) return(invisible(TRUE))

  within  <- character()
  between <- character()
  for (d in dup) {
    k <- cl[nm == d]
    if (length(unique(k)) == 1L) {
      within <- c(within, sprintf('"%s" (%s, x%d)', d, unique(k), length(k)))
    } else {
      between <- c(between, sprintf('"%s" (%s)', d,
                                    paste(sort(unique(k)), collapse = ", ")))
    }
  }
  msg <- character()
  if (length(within) > 0L) {
    msg <- c(msg, paste0("Duplicated process name(s) within one class: ",
                         paste(within, collapse = "; "),
                         ". Object names must be unique within a class."))
  }
  if (length(between) > 0L) {
    msg <- c(msg, paste0("Process name(s) used by more than one process class: ",
                         paste(between, collapse = "; "),
                         ". supply/demand/technology/storage/export/import/trade ",
                         "share the single `process` set, so a duplicate ",
                         "silently merges two processes into one solver entity."))
  }
  stop(paste(msg, collapse = " "))
}

# Guard: nothing carrying more than one distinct (vintage, cluster) cell may
# reach `ob2mi`, because `make_data_param()` silently ignores unknown slot
# columns -- an unexpanded object would produce quietly wrong data rather than
# an error. Covers every class in `.variant_classes`.
# @noRd
.assert_variants_expanded <- function(mod) {
  for (cls in names(.variant_classes)) {
    objs <- try(getObjects(mod, cls), silent = TRUE)
    if (inherits(objs, "try-error") || length(objs) == 0L) next
    for (t in objs) {
      n_v <- length(.variant_levels(t, "vintage"))
      n_c <- length(.variant_levels(t, "cluster"))
      if (n_v > 1L || n_c > 1L) {
        stop(cls, ' "', t@name, '" still carries ', n_v, " vintage(s) and ",
             n_c, " cluster(s) at interpolation time -- variant expansion did ",
             "not run. This would silently drop the vintage/cluster distinction.")
      }
      # the lifespan columns must resolve to one value per
      # (vintage, region, cluster) key before parameters are packed;
      # `.lifespan_resolve()` errors on conflicting duplicate keys
      if (.hasSlot(t, "vintage")) {
        for (cc in .vintage_val_cols) .lifespan_resolve(t, cc)
      }
    }
  }
  invisible(TRUE)
}
