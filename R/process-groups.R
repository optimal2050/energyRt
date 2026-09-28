# =========================================================================== #
# Grouping processes when a model's regions are coarsened.
#
# What is grouped are PROCESSES, not regions. Eleven coal plants over eleven
# regions become one plant carrying seven parallel sub-processes inside three
# zones (`technology@cluster`: "a parallel sub-process of the same technology
# ... with its own capacity, availability and costs"). The regions are what the
# grouping is keyed BY, not what it produces.
#
# `aggregate_model_regions()` without `clusters` collapses every region inside a
# coarser geoframe into ONE value per object: a weighted mean of the intensive
# parameters, a sum of the extensive ones. That is the right move when the
# regions' processes are alike and the wrong one when they are not -- a fleet
# built over forty years does not have one efficiency.
#
# Three jobs, in this order:
#
#   find groups  get_process_groups()      which processes are the SAME process
#   cluster      .cl_crosswalk()    which of them merge, keyed by region
#   merge        .cl_merge_group()  apply it, one object with sub-processes
#
# `.cl_cluster_model()` is the driver that walks the families and calls the
# other two; `aggregate_model_regions(clusters = )` is the only way in.
#
# `k` spans two ends:
#
#   k = one per coarse region  -> identical to plain aggregation
#   k = one per fine region    -> 1:1, nothing is averaged at all
#
# and anything between trades size for fidelity.
#
# SCOPE: technologies only. Supply, storage and trade carry a `@cluster` slot
# and are NOT grouped here -- they take the plain aggregation. `weather` is
# deliberately excluded: a profile has to be grouped together with the
# processes that use it, not on its own, so the merge resolves a weather link to
# the cluster medoid's instead.
#
# The clustering itself is `clusterscales`, over a `multiscales` scale built
# from the geoscale. Contiguity does the confinement: with no adjacency edge
# crossing a boundary of the target geoframe, a connected cluster cannot
# straddle one, so the groups fall inside their parent by construction.
# =========================================================================== #

#' @include region.R
NULL

# -- structural signature ----------------------------------------------------
# Only structurally identical technologies can become clusters of ONE object:
# same inputs, outputs, aux commodities and input groups. The NAME is not part
# of the signature -- it is the label, taken from the common stem afterwards.
#' @noRd
.proc_signature <- function(o) {
  cls <- class(o)[1]
  g <- function(sl, col) {
    if (!methods::.hasSlot(o, sl)) return(character())
    v <- methods::slot(o, sl)
    if (!is.data.frame(v) || !nrow(v) || !col %in% names(v)) return(character())
    sort(unique(as.character(v[[col]][!is.na(v[[col]])])))
  }
  # The class LEADS the signature: a supply and a technology on the same
  # commodity are not the same process, however alike their tables look.
  sig <- switch(
    cls,
    technology = ,
    storage = paste(
      paste0("in:",  paste(g("input",  "comm"),  collapse = "+")),
      paste0("out:", paste(g("output", "comm"),  collapse = "+")),
      paste0("aux:", paste(g("aux",    "acomm"), collapse = "+")),
      paste0("grp:", paste(g("input",  "group"), collapse = "+")),
      sep = "|"),
    # commodity + unit is the whole identity of a supply-shaped process
    supply = , import = , export = paste(
      paste0("comm:", paste(sort(unique(methods::slot(o, "commodity"))),
                            collapse = "+")),
      paste0("unit:", paste(sort(unique(methods::slot(o, "unit"))),
                            collapse = "+")),
      sep = "|"),
    NA_character_)
  if (is.na(sig)) return(NA_character_)
  paste0(cls, "|", sig)
}

# Classes a region grouping can turn into clusters. `demand` is absent because
# it has no `@cluster` slot -- a demand is a requirement, not a process with
# archetypes, and summing it over a zone is the right answer. `trade` is absent
# because it has no `@region` slot: its regions are `src`/`dst` pairs in
# `@routes`, so grouping ITS regions means clustering corridors, which is a
# different operation. `weather` is absent because a profile has to be grouped
# with the processes that use it.
#' @noRd
.cl_groupable <- c("technology", "supply", "storage", "import", "export")

# Every region a technology names, from `@region` and from the `region` column
# of any slot. A technology may arrive either way: one object per region (a
# kit built plant by plant) or ONE object whose slots carry a `region` column
# (the idiomatic shape for region-varying parameters). Both cluster.
#' @noRd
.tech_regions <- function(o) {
  r <- unique(c(as.character(methods::slot(o, "region")),
                unlist(lapply(methods::slotNames(o), function(sl) {
                  v <- methods::slot(o, sl)
                  if (is.data.frame(v) && nrow(v) && "region" %in% names(v))
                    as.character(v$region) else NULL
                }))))
  unique(r[!is.na(r) & nzchar(r)])
}

# The one region a SINGLE-region technology lives in; NA when it spans several.
# Used where a value must be attributed to a region -- a slot that omits the
# `region` column belongs to that region, and only a single-region object can
# say which.
#' @noRd
.tech_region <- function(o) {
  r <- .tech_regions(o)
  if (length(r) == 1L) r else NA_character_
}

# Longest common prefix of the member names, trimmed of a trailing separator.
#' @noRd
.name_stem <- function(nms) {
  if (length(nms) == 1L) return(nms)
  ch <- strsplit(nms, "", fixed = TRUE)
  n <- min(lengths(ch)); k <- 0L
  while (k < n && length(unique(vapply(ch, function(x) x[k + 1L], ""))) == 1L)
    k <- k + 1L
  stem <- sub("[^A-Za-z0-9]+$", "", substr(nms[1], 1, k))
  if (nzchar(stem)) stem else nms[1]
}

#' Technologies that may be merged into one clustered object
#'
#' Groups a model's single-region technologies by STRUCTURE -- input, output
#' and aux commodities, and input groups. Only technologies with the same
#' structure can become clusters of one object, because a cluster is a variant
#' of the same process, not a different one: a coal plant and a gas plant have
#' different inputs and stay separate however alike their costs look.
#'
#' This is the first step of a clustered aggregation: it names the families
#' [aggregate_model_regions()] can turn into clusters, and those names are the
#' keys of its `clusters` argument.
#'
#' @param x a `model`, `repository`, or a list of objects.
#' @param regions optional character vector; keep only technologies living in
#'   these regions.
#'
#' @return A data.frame, one row per group, with `group` (the common name
#'   stem, and the name the merged object will take), `signature`, `n`,
#'   `members` and `regions` (comma-separated). Technologies that span several
#'   regions are not grouped and do not appear.
#'
#' @seealso [process_cluster_sweep()] to choose each family's `k`,
#'   [aggregate_model_regions()] to apply it.
#' @family geoscale
#' @export
get_process_groups <- function(x, regions = NULL) {
  objs <- .cl_all_objects(x)
  tech <- objs[vapply(objs, function(o) class(o)[1] %in% .cl_groupable,
                      logical(1))]
  if (!length(tech)) return(.cl_empty_groups())
  sig <- vapply(tech, .proc_signature, "")
  tech <- tech[!is.na(sig)]; sig <- sig[!is.na(sig)]
  if (!length(tech)) return(.cl_empty_groups())
  regs <- lapply(tech, .tech_regions)
  if (!is.null(regions))
    regs <- lapply(regs, function(r) intersect(r, regions))
  keep <- lengths(regs) > 0L
  tech <- tech[keep]; sig <- sig[keep]; regs <- regs[keep]
  if (!length(tech)) return(.cl_empty_groups())
  # A technology that already spans several regions is a group on its own: its
  # rows carry the spread, so there is nothing to merge it WITH, and grouping
  # it with single-region siblings would double-count the regions they share.
  span <- lengths(regs) > 1L
  key <- ifelse(span, paste0("@", names(tech)), sig)
  out <- do.call(rbind, lapply(split(seq_along(tech), key), function(i) {
    r <- unique(unlist(regs[i], use.names = FALSE))
    data.frame(group = .name_stem(names(tech)[i]),
               class = sub("[|].*$", "", sig[i[1]]),
               signature = sub("^[^|]*[|]", "", sig[i[1]]),
               n = length(r), members = paste(names(tech)[i], collapse = ","),
               regions = paste(r, collapse = ","),
               stringsAsFactors = FALSE)
  }))
  rownames(out) <- NULL
  out[order(-out$n, out$group), , drop = FALSE]
}

# `add()` appends a NEW repository rather than growing the first, so a model
# can hold several and reading only `@data[[1]]` silently ignores everything
# added after the model was built. Later repositories win on a name clash,
# which is what `add(..., overwrite = TRUE)` means.
#' @noRd
.cl_all_objects <- function(x) {
  # a single process object is a list of one, not a list to iterate over: the
  # S4 object would otherwise be treated as one and fail on coercion
  if (methods::is(x, "technology"))
    return(stats::setNames(list(x), methods::slot(x, "name")))
  if (methods::is(x, "repository")) return(x@data)
  if (!methods::is(x, "model")) return(x)
  out <- list()
  for (repo in x@data) {
    d <- repo@data
    out[names(d)] <- d
  }
  out
}

#' @noRd
.cl_empty_groups <- function() {
  data.frame(group = character(), class = character(),
             signature = character(), n = integer(),
             members = character(), regions = character(),
             stringsAsFactors = FALSE)
}

# -- the clustering ----------------------------------------------------------

# Adjacency over the fine regions, with every edge that crosses a `level`
# boundary removed. `cluster_contiguous()` cannot merge below the number of
# connected components, so this both confines the clusters and sets `k`'s floor.
#' @noRd
.cl_adjacency <- function(gs, level) {
  check_package("sf")
  g <- attr(gs, "geometry")
  if (!is.null(g) && !inherits(g, "sfc")) {
    stop("the geoscale's geometry is a plain list, not an `sfc`. ",
         "`geoscales::filter_geoscale()` degrades it that way when the `sf` ",
         "NAMESPACE is not loaded at the time it runs -- subsetting an `sfc` ",
         "without `[.sfc` drops the class. Call ",
         "`requireNamespace(\"sf\")` (or `library(sf)`) before filtering, ",
         "and rebuild the geoscale.", call. = FALSE)
  }
  m <- geoscales::geoscale_geometry(gs, "region")
  reg <- as.character(m$region)
  tt <- sf::st_touches(m)
  a <- matrix(FALSE, length(reg), length(reg), dimnames = list(reg, reg))
  for (i in seq_along(reg)) a[i, tt[[i]]] <- TRUE
  # a region that touches nothing (drawn offshore) joins its nearest neighbour;
  # a distance TOLERANCE cannot do this job -- one wide enough to bridge the
  # gap also links every pair one cell apart on a gridded layout
  if (any(lengths(tt) == 0)) {
    d <- sf::st_distance(m)
    units(d) <- NULL
    for (i in which(lengths(tt) == 0)) {
      v <- d[i, ]; v[i] <- Inf; j <- which.min(v)
      a[i, j] <- a[j, i] <- TRUE
    }
  }
  lt <- as.data.frame(geoscales::geoscale_leaftable(gs))
  parent <- stats::setNames(as.character(lt[[level]]), as.character(lt$region))
  a & !outer(parent[reg], parent[reg], "!=")
}

#' @noRd
.cl_scale <- function(gs, level) {
  check_package("multiscales")
  lt <- as.data.frame(geoscales::geoscale_leaftable(gs))
  frames <- geoscales::geoscale_geoframes(gs)
  frames <- frames[seq_len(match(level, frames))]
  multiscales::scale_from_leaftable(
    lt[, c(frames, "region"), drop = FALSE],
    frames = c(frames, "region"), key = "region", name = "cluster_regions")
}

# Features -> the long table clusterscales reads. Each feature is z-scored, so
# an invcost in the hundreds does not drown an efficiency in [0, 1]; a constant
# feature contributes nothing rather than NaN.
#' @noRd
.cl_features <- function(feat) {
  cols <- setdiff(names(feat), "region")
  do.call(rbind, lapply(cols, function(f) {
    v <- as.numeric(feat[[f]])
    s <- stats::sd(v, na.rm = TRUE)
    data.frame(region = feat$region, f = f,
               v = if (is.finite(s) && s > 0)
                     (v - mean(v, na.rm = TRUE)) / s else 0,
               stringsAsFactors = FALSE)
  }))
}

# `k = "auto"`: sweep and take the best average silhouette. clusterscales ships
# no `best_k()` ON PURPOSE -- "the numbers inform a judgement that stays yours"
# -- so the rule lives here, and the sweep table is returned rather than hidden
# so the judgement can be checked. Under a contiguity constraint the silhouette
# is routinely negative at small `k`; that is the constraint, not a failure.
#' @noRd
.cl_choose_k <- function(long, sc, adj, kmin, kmax) {
  if (kmax <= kmin) return(list(k = kmin, sweep = NULL))
  sw <- as.data.frame(clusterscales::cluster_sweep(
    long, sc, ks = kmin:kmax, frame = "region", key = "region", value = "v",
    FUN = clusterscales::cluster_contiguous, adjacency = adj))
  list(k = sw$k[which.max(sw$silhouette)], sweep = sw)
}

# region -> (target region, cluster)
# Label a region-cluster by the regions in it, so a solved result reads
# `ECOA_CLW1` rather than `ECOA_CLc03` and a 1:1 aggregation says which region
# each variant came from without a lookup. Cluster ids become set elements on
# every backend, so a label is only used while it satisfies the NAME RULE and
# stays short; otherwise that cluster keeps its `c01` id and `@cluster$desc`
# carries the membership, as it does either way.
#' @noRd
.cl_label_by_members <- function(region, cluster) {
  parts <- split(region, cluster)
  lab <- vapply(parts, function(r) {
    cand <- paste(sort(unique(r)), collapse = "_")
    if (nchar(cand) <= 24L && isTRUE(check_name(cand))) cand else NA_character_
  }, character(1))
  # a label that collides with another cluster's would merge two of them
  lab[duplicated(lab) | duplicated(lab, fromLast = TRUE)] <- NA_character_
  out <- unname(lab[cluster])
  ifelse(is.na(out), cluster, out)
}

#' @noRd
.cl_crosswalk <- function(long, sc, adj, k, lt, level) {
  cl <- clusterscales::cluster_contiguous(
    long, sc, k = k, frame = "region", key = "region", value = "v",
    adjacency = adj)
  a <- as.data.frame(attr(cl, "clustering"))
  names(a)[1] <- "region"
  a$cluster <- .cl_label_by_members(as.character(a$region),
                                    as.character(a$cluster))
  data.frame(region = as.character(a$region),
             cluster = as.character(a$cluster),
             # a medoid is a REAL member, not an average -- which is what a
             # link column (`weather`, ...) needs, since averaging the name of
             # another object is meaningless
             medoid = if (is.null(a$medoid)) FALSE else as.logical(a$medoid),
             target = as.character(lt[[level]])[
               match(as.character(a$region), as.character(lt$region))],
             stringsAsFactors = FALSE)
}

# -- the merge ---------------------------------------------------------------

# A single-region technology may omit the `region` column from a slot and rely
# on `@region` instead (`ceff` usually does). Key those rows by the object's
# own region before aggregating, or eleven efficiencies collide into one.
#' @noRd
.cl_inject_region <- function(df, r) {
  if (!is.data.frame(df) || !nrow(df)) return(df)
  # `r` is NA for a technology that spans several regions: a slot of its that
  # omits `region` applies to ALL of them, so it stays a wildcard rather than
  # being pinned to one.
  if (is.na(r)) {
    if (!"region" %in% names(df)) df$region <- NA_character_
    return(df)
  }
  if (!"region" %in% names(df)) df$region <- r
  df$region[is.na(df$region)] <- r
  df
}

# Aggregate one slot to (target region, cluster). Extensive columns sum,
# intensive ones take a weighted mean -- the same split the plain aggregation
# uses, read from the same `.agg_rules()`, so the two paths cannot drift apart.
#' @noRd
# Columns naming ANOTHER object rather than carrying a value. They cannot be
# summed or averaged, and treating them as keys makes one cluster sprout a row
# per member -- six `weather` rows on a six-region cluster. Resolve them to the
# cluster medoid's value instead: a real member's profile, not a fabricated one.
.CL_LINK_COLS <- c("weather", "transform")

#' @noRd
.cl_resolve_links <- function(df, xw) {
  cols <- intersect(.CL_LINK_COLS, names(df))
  cols <- cols[vapply(cols, function(cc) !is.numeric(df[[cc]]), logical(1))]
  if (!length(cols)) return(df)
  med <- xw$region[xw$medoid]
  names(med) <- xw$cluster[xw$medoid]
  cl_of <- xw$cluster[match(df$region, xw$region)]
  for (cc in cols) {
    v <- df[[cc]]
    pick <- vapply(unique(stats::na.omit(cl_of)), function(k) {
      mr <- med[[k]]
      if (is.null(mr)) return(NA_character_)
      hit <- which(cl_of == k & df$region == mr & !is.na(v))
      if (length(hit)) as.character(v[hit[1]]) else NA_character_
    }, character(1), USE.NAMES = TRUE)
    keep <- !is.na(cl_of) & !is.na(pick[cl_of])
    v[keep] <- pick[cl_of[keep]]
    df[[cc]] <- v
  }
  df
}

# A region-less row applies everywhere, but one keyed by a pre-existing cluster
# still names a variant -- and after composition that variant is `GOOD_W1`, not
# `GOOD`. A bare label would match no declared cluster; NA would make the row
# apply to every one, so `GOOD`'s `af.up` and `POOR`'s would collide. Expand it
# onto the composed labels it stands for.
#' @noRd
.cl_wild <- function(wild, wpre, xw, cols) {
  if (!nrow(wild)) return(NULL)
  labs <- unique(xw$cluster)
  out <- do.call(rbind, lapply(seq_len(nrow(wild)), function(i) {
    row <- wild[if (is.na(wpre[i])) i else rep(i, length(labs)), , drop = FALSE]
    row$cluster <- if (is.na(wpre[i])) NA_character_ else
      paste0(wpre[i], "_", labs)
    row
  }))
  rownames(out) <- NULL
  unique(out[, cols, drop = FALSE])
}

.cl_agg_slot <- function(df, xw, w) {
  # A technology may ALREADY be clustered -- wind by site grade, a fleet by
  # build type -- and moving it up a region level adds a SECOND grouping. The
  # two compose: `GOOD` in `W1` becomes `GOOD_W1`. Dropping the incoming column
  # (which is what this did) averaged the grades away silently, so eight
  # variants came back as four.
  pre <- if ("cluster" %in% names(df)) as.character(df$cluster) else
    rep(NA_character_, nrow(df))
  pre[!nzchar(pre)] <- NA_character_
  df$cluster <- NULL
  df <- .cl_resolve_links(df, xw)
  vals <- .agg_values(df)
  keys <- setdiff(names(df), c(vals, "region"))
  # rows with no region apply everywhere: they are not aggregated, they are
  # carried through once, or a multi-region technology loses its `olife`,
  # `af` and every other slot that did not need a region
  keep_row <- !is.na(df$region)
  wild <- df[!keep_row, , drop = FALSE]
  wpre <- pre[!keep_row]
  pre <- pre[keep_row]
  df <- df[keep_row, , drop = FALSE]
  df$.tgt <- xw$target[match(df$region, xw$region)]
  df$.cl <- xw$cluster[match(df$region, xw$region)]
  # the pre-existing grouping leads, so the original label stays readable
  df$.cl <- ifelse(is.na(pre), df$.cl, paste0(pre, "_", df$.cl))
  df <- df[!is.na(df$.tgt), , drop = FALSE]
  if (!nrow(df)) {
    return(.cl_wild(wild, wpre, xw,
                    c("region", "cluster",
                      setdiff(names(wild), c("region", "cluster")))))
  }
  df$.w <- unname(w[match(df$region, names(w))])
  df$.w[!is.finite(df$.w) | df$.w <= 0] <- 1
  rules <- .agg_rules(vals)
  by <- df[, c(".tgt", ".cl", keys), drop = FALSE]
  id <- do.call(paste, c(unname(as.list(by)), sep = "\r"))
  out <- do.call(rbind, lapply(split(seq_len(nrow(df)), id), function(i) {
    row <- by[i[1], , drop = FALSE]
    for (v in vals) {
      x <- df[[v]][i]; ww <- df$.w[i]; ok <- !is.na(x)
      row[[v]] <- if (!any(ok)) NA_real_
        else if (identical(unname(rules[[v]]), "sum")) sum(x[ok])
        else sum(x[ok] * ww[ok]) / sum(ww[ok])
    }
    row
  }))
  names(out)[names(out) == ".tgt"] <- "region"
  names(out)[names(out) == ".cl"] <- "cluster"
  out <- out[, c("region", "cluster", keys, vals), drop = FALSE]
  out <- rbind(out, .cl_wild(wild, wpre, xw, names(out)))
  rownames(out) <- NULL
  out
}

# One weight per region: the size the intensive means are weighted by. Capacity
# if the technology declares one, otherwise equal weights -- never a
# year-varying quantity, which would make an efficiency's weighting depend on
# the year it is read for.
#' @noRd
.cl_weights <- function(techs, regions) {
  w <- stats::setNames(rep(NA_real_, length(regions)), regions)
  size <- .cl_size_cols(techs[[1]])
  for (o in techs) {
    if (!methods::.hasSlot(o, size$slot)) next
    d <- methods::slot(o, size$slot)
    if (!is.data.frame(d) || !nrow(d)) next
    cols <- intersect(size$cols, names(d))
    if (!length(cols)) next
    # a slot may be keyed by region (one object over many) or not (one object
    # per region); either way the weight belongs to the region it describes
    reg <- if ("region" %in% names(d)) as.character(d$region) else
      rep(NA_character_, nrow(d))
    reg[is.na(reg)] <- .tech_region(o)
    v <- suppressWarnings(apply(as.matrix(d[, cols, drop = FALSE]), 1, max,
                                na.rm = TRUE))
    for (i in seq_along(reg)) {
      if (is.na(reg[i]) || !reg[i] %in% regions) next
      if (is.finite(v[i]) && v[i] > 0) w[[reg[i]]] <- v[i]
    }
  }
  if (all(is.na(w))) w[] <- 1
  w[is.na(w)] <- stats::median(w, na.rm = TRUE)
  w
}

#' @noRd
.cl_merge_group <- function(techs, xw, w, name, desc = NULL) {
  base <- techs[[1]]
  proto <- methods::new(class(base)[1])
  # Every slot that carries per-region or per-cluster DATA, derived rather than
  # listed: a supply's `@supply`, a storage's part-keyed `@capacity` and a
  # technology's `@geff` all qualify without a new entry here. `@cluster` is
  # excluded -- it is the declaration, rebuilt below, not data to aggregate --
  # and `@input`/`@output` fall out because they carry only commodity and unit,
  # which the group's signature already holds identical.
  varying <- Filter(function(sl) {
    if (identical(sl, "cluster")) return(FALSE)
    v <- methods::slot(proto, sl)
    is.data.frame(v) && any(c("region", "cluster") %in% names(v))
  }, methods::slotNames(base))
  out <- base
  methods::slot(out, "name") <- name
  if (!is.null(desc)) methods::slot(out, "desc") <- desc
  methods::slot(out, "region") <- character()
  for (sl in intersect(varying, methods::slotNames(base))) {
    dfs <- lapply(techs, function(o) {
      v <- methods::slot(o, sl)
      if (!is.data.frame(v) || !nrow(v)) return(NULL)
      .cl_inject_region(v, .tech_region(o))
    })
    dfs <- dfs[!vapply(dfs, is.null, logical(1))]
    if (!length(dfs)) next
    cols <- unique(unlist(lapply(dfs, names)))
    dfs <- lapply(dfs, function(d) {
      d[setdiff(cols, names(d))] <- NA
      d[cols]
    })
    agg <- .cl_agg_slot(do.call(rbind, dfs), xw, w)
    if (is.null(agg)) next
    if (!"cluster" %in% names(methods::slot(proto, sl)))
      agg <- agg[, setdiff(names(agg), "cluster"), drop = FALSE]
    if (!"region" %in% names(methods::slot(proto, sl)))
      agg <- unique(agg[, setdiff(names(agg), "region"), drop = FALSE])
    methods::slot(out, sl) <- agg
  }
  # `@cluster` is AUTHORITATIVE -- a label used in a slot but not declared here
  # is an error -- so derive it from the labels the slots actually carry rather
  # than from the region grouping alone. With a pre-existing dimension those
  # labels are composed (`GOOD_W1`), and declaring only the region half would
  # reject the object's own data.
  seen <- unique(unlist(lapply(setdiff(methods::slotNames(out), "cluster"),
                               function(sl) {
    v <- methods::slot(out, sl)
    if (is.data.frame(v) && nrow(v) && "cluster" %in% names(v))
      as.character(v$cluster) else NULL
  }), use.names = FALSE))
  seen <- seen[!is.na(seen) & nzchar(seen)]
  if (!length(seen)) seen <- unique(xw$cluster)

  # which region-group each label belongs to: the longest region-cluster id the
  # label is, or ends with after an underscore
  regcl <- unique(xw$cluster)
  owner <- vapply(seen, function(lab) {
    hit <- regcl[regcl == lab | endsWith(lab, paste0("_", regcl))]
    if (length(hit)) hit[which.max(nchar(hit))] else NA_character_
  }, character(1))

  tgt <- xw$target[match(owner, xw$cluster)]
  mem <- vapply(owner, function(k)
    if (is.na(k)) NA_character_ else
      paste(sort(unique(xw$region[xw$cluster == k])), collapse = "+"),
    character(1))
  ord <- order(tgt, seen)
  methods::slot(out, "cluster") <- data.frame(
    cluster = seen[ord], desc = unname(mem[ord]), region = unname(tgt[ord]),
    order = seq_along(ord), stringsAsFactors = FALSE)
  out
}

# -- the driver --------------------------------------------------------------

#' Sweep a technology family's clusterings, to choose `k` by looking
#'
#' Clusters one family over a range of `k` and reports what each one buys:
#' within-cluster dispersion, average silhouette and the cluster sizes. Nothing
#' is changed -- the point is to see where the structure is before naming a `k`
#' in [aggregate_model_regions()]`(clusters = )`.
#'
#' The sweep uses the same features and the same contiguity graph the
#' aggregation would, so what you inspect is what you would get. Two families
#' will generally disagree about where their structure is, which is why `k` is
#' set per family and not once for the model.
#'
#' Read it as a scree plot: dispersion falls with `k` and always will, so take
#' the knee rather than the minimum, and prefer a `k` whose silhouette is high
#' and whose sizes are not one big cluster and a row of singletons. There is
#' deliberately no "best k" here, and none in `clusterscales` either -- the
#' number depends on what the model is for.
#'
#' @param mod a `model`.
#' @param group a process group name, as [get_process_groups()] reports it.
#' @param ks integer vector of cluster counts. `NULL` (default) sweeps the
#'   whole admissible range: from one cluster per `level` region to one per
#'   region.
#' @param geoscale a [geoscales::Geoscale]; defaults to the model's own.
#' @param level the target geoframe, e.g. `"zone"`.
#' @param features feature spec, as in [aggregate_model_regions()].
#'
#' @return A data.frame, one row per `k`, with the dispersion and silhouette
#'   columns `clusterscales::cluster_sweep()` returns plus `sizes`, the cluster
#'   sizes as a comma-separated string.
#'
#' @seealso [get_process_groups()], [aggregate_model_regions()]
#' @family geoscale
#' @export
process_cluster_sweep <- function(mod, group, ks = NULL, geoscale = NULL, level,
                               features = NULL) {
  stopifnot(inherits(mod, "model"))
  check_package("clusterscales")
  gs <- geoscale %||% tryCatch(mod@config@geoscale, error = function(e) NULL)
  if (is.null(gs) || !is_geoscale(gs)) {
    stop("no geoscale: pass `geoscale=`, or attach one with setGeoscale().",
         call. = FALSE)
  }
  gdf <- get_process_groups(mod)
  if (!isTRUE(group %in% gdf$group)) {
    stop("`group` = \"", group, "\" is not a process group of this model; ",
         "one of: ", paste(gdf$group, collapse = ", "), call. = FALSE)
  }
  g <- gdf[gdf$group == group, , drop = FALSE]
  regs <- strsplit(g$regions, ",")[[1]]
  objs <- .cl_all_objects(mod)
  long <- .cl_features(.cl_feature_table(objs[strsplit(g$members, ",")[[1]]],
                                        regs, features))
  lt <- as.data.frame(geoscales::geoscale_leaftable(gs))
  kmin <- length(unique(lt[[level]]))
  ks <- as.integer(ks %||% kmin:length(regs))
  ks <- ks[ks >= kmin & ks <= length(regs)]
  if (!length(ks)) {
    stop("no admissible `ks`: clusters cannot straddle a `", level,
         "` region, so k runs from ", kmin, " to ", length(regs), ".",
         call. = FALSE)
  }
  adj <- .cl_adjacency(gs, level)
  sc <- .cl_scale(gs, level)
  sw <- as.data.frame(clusterscales::cluster_sweep(
    long, sc, ks = ks, frame = "region", key = "region", value = "v",
    FUN = clusterscales::cluster_contiguous, adjacency = adj))
  sw$sizes <- vapply(ks, function(k)
    paste(sort(table(.cl_crosswalk(long, sc, adj, k, lt, level)$cluster),
               decreasing = TRUE), collapse = ","), character(1))
  sw
}

#' The clustering behind an aggregated model
#'
#' Reports what [aggregate_model_regions()] did when it was given `clusters`:
#' which regions were merged into which cluster, the `k` it used, the sweep it
#' chose from when `k` was `"auto"`, and a geoscale carrying a `cluster`
#' geoframe so the grouping can be plotted with [plot_geoscale()].
#'
#' @param mod a `model` returned by [aggregate_model_regions()].
#'
#' @return A named list, one element per process group, each with `k`,
#'   `crosswalk` (`region`, `cluster`, `medoid`, `target`), `sweep` and
#'   `geoscale`. An empty list when the model was aggregated plainly.
#'
#' @examplesIf FALSE
#' cl <- model_clusters(m)
#' plot_geoscale(cl$ECOA$geoscale, type = "map", geoframe = "cluster")
#' plot_geoscale(cl$ECOA$geoscale, type = "icicle")
#'
#' @seealso [aggregate_model_regions()], [process_cluster_sweep()]
#' @family geoscale
#' @export
model_clusters <- function(mod) {
  stopifnot(inherits(mod, "model"))
  attr(mod, "clustering") %||% list()
}


# A geoscale carrying the grouping as a `cluster` geoframe, so the clustering
# can be SEEN with the views that already exist: plot_geoscale(type = "map",
# geoframe = "cluster") colours the atoms by their group, and `type = "icicle"`
# walks nation > zone > cluster > region. No new plotting code.
#
# This is a view, not the aggregated model's geoscale: that one is pruned to
# `level` and its atoms must keep matching the model's regions, or
# .spatial_sample_mode() mis-sorts it.
#' @noRd
.cl_cluster_geoscale <- function(gs, level, xw, group) {
  lt <- as.data.frame(geoscales::geoscale_leaftable(gs))
  frames <- geoscales::geoscale_geoframes(gs)
  fine <- geoscales::geoscale_geoframes(gs, finest = TRUE)
  up <- frames[seq_len(match(level, frames))]
  i <- match(as.character(lt[[fine]]), xw$region)
  keep <- !is.na(i)
  out <- lt[keep, up, drop = FALSE]
  out$cluster <- xw$cluster[i[keep]]
  out[[fine]] <- as.character(lt[[fine]])[keep]
  # the weight columns come along: geoscale_autoplot(type = "icicle") sizes its
  # bands by one, and refuses outright when the geoscale declares none
  wts <- intersect(geoscales::geoscale_weights(gs), names(lt))
  out[wts] <- lt[keep, wts, drop = FALSE]
  g <- tryCatch(S7::prop(gs, "geometry"), error = function(e) NULL)
  if (!is.null(g) && length(g) == nrow(lt)) g <- g[keep] else g <- NULL
  geoscales::geoscale_from_leaftable(
    out, geoframes = c(up, "cluster", fine), key = fine, geometry = g,
    weights = if (length(wts)) wts else NULL,
    name = paste0(group, "_clusters"),
    desc = paste0("regions of ", group, " grouped into clusters at `", level,
                  "`"))
}

# -- the driver, reached through aggregate_model_regions(clusters=) ----------

# Group the fine regions of each technology family into clusters and carry them
# into `technology@cluster`, instead of collapsing each coarse region to a
# single value. Everything that is not a grouped technology goes through the
# ordinary aggregation, so the two paths agree on every other object.
#' @noRd
.cl_cluster_model <- function(mod, geoscale = NULL, level, clusters,
                              as = "clusters", name = NULL,
                              verbose = isVerbose()) {
  stopifnot(inherits(mod, "model"))
  check_package("geoscales")
  check_package("clusterscales")
  check_package("multiscales")
  gs <- geoscale %||% tryCatch(mod@config@geoscale, error = function(e) NULL)
  if (is.null(gs) || !is_geoscale(gs)) {
    stop("no geoscale: pass `geoscale=`, or attach one with setGeoscale().",
         call. = FALSE)
  }
  frames <- geoscales::geoscale_geoframes(gs)
  if (!isTRUE(level %in% frames)) {
    stop("`level` = \"", level, "\" is not a geoframe of the geoscale; ",
         "one of: ",
         paste(frames, collapse = ", "), call. = FALSE)
  }
  objs <- .cl_all_objects(mod)

  gdf <- get_process_groups(mod)
  if (!nrow(gdf)) {
    stop("no groupable technologies found: every technology either spans ",
         "several regions or is structurally unique, so there is nothing to ",
         "turn into clusters. Drop `clusters` to aggregate plainly.",
         call. = FALSE)
  }
  spec <- .cl_spec(clusters, gdf$group)
  # a group left out of `clusters` is not clustered -- it takes the plain
  # weighted mean, so clustering is opted into one family at a time
  gdf <- gdf[gdf$group %in% names(spec), , drop = FALSE]
  if (!nrow(gdf)) {
    stop("`clusters` names no process group of this model. Groups: ",
         paste(get_process_groups(mod)$group, collapse = ", "), call. = FALSE)
  }

  lt <- as.data.frame(geoscales::geoscale_leaftable(gs))
  adj <- .cl_adjacency(gs, level)
  sc <- .cl_scale(gs, level)
  kmin <- length(unique(lt[[level]]))

  # everything that is NOT a grouped technology goes through the ordinary path
  merged_members <- unlist(strsplit(gdf$members, ","), use.names = FALSE)
  rest <- mod
  # flatten to ONE repository: `aggregate_model_regions()` reads `@data[[1]]`
  # only, so a model built up with add() would lose its later repositories
  # here. `[1]` not `list([[1]])`: the list is NAMED, and `expand_variants()`
  # reads `names(mod@data)[i]` -- an unnamed list makes it fail far away.
  rest@data <- mod@data[1]
  rest@data[[1]]@data <- objs[setdiff(names(objs), merged_members)]
  agg <- aggregate_model_regions(rest, gs, level = level, verbose = FALSE)
  out_objs <- agg@data[[1]]@data

  diag <- list()
  for (i in seq_len(nrow(gdf))) {
    gname <- gdf$group[i]
    members <- strsplit(gdf$members[i], ",")[[1]]
    regs <- strsplit(gdf$regions[i], ",")[[1]]
    techs <- objs[members]
    s <- spec[[gname]]
    feat <- .cl_feature_table(techs, regs, s$features)
    long <- .cl_features(feat)
    kk <- s$k
    sw <- NULL
    if (identical(kk, "auto")) {
      ch <- .cl_choose_k(long, sc, adj, kmin, length(regs))
      kk <- ch$k
      sw <- ch$sweep
    }
    kk <- as.integer(kk)
    # Check `k` HERE rather than letting clusterscales refuse it: its message
    # is in its own vocabulary ("units", "groups", "the constraint") and names
    # neither the geoframe nor the way out.
    if (is.na(kk) || kk < kmin || kk > length(regs)) {
      stop("k = ", if (is.na(kk)) "NA" else kk, " is out of range for group '",
           gname, "': clusters cannot straddle a `", level, "` region, so k ",
           "runs from ", kmin, " (one cluster per `", level,
           "` region -- what aggregate_model_regions() produces) to ",
           length(regs), " (one per region, nothing averaged).", call. = FALSE)
    }
    xw <- .cl_crosswalk(long, sc, adj, kk, lt, level)
    w <- .cl_weights(techs, regs)
    if (identical(as, "objects")) {
      for (lab in unique(xw$cluster)) {
        nm <- paste(gname, lab, sep = "_")
        one <- xw[xw$cluster == lab, , drop = FALSE]
        o <- .cl_unsuffix(.cl_merge_group(techs, one, w, name = nm), lab)
        # a clustered object leaves `@region` empty because `@cluster` carries
        # the regions; stripping the declaration would leave this one nowhere
        methods::slot(o, "region") <- unique(one$target)
        out_objs[[nm]] <- o
      }
    } else {
      out_objs[[gname]] <- .cl_merge_group(techs, xw, w, name = gname)
    }
    diag[[gname]] <- list(
      k = kk, crosswalk = xw, sweep = sw,
      geoscale = tryCatch(.cl_cluster_geoscale(gs, level, xw, gname),
                          error = function(e) NULL))
    if (isTRUE(verbose)) {
      message("  ", gname, ": ", length(regs), " regions -> ", kk,
              " cluster(s)")
    }
  }

  agg@data[[1]]@data <- out_objs
  if (!is.null(name)) agg@config@name <- name
  attr(agg, "clustering") <- diag
  agg
}

# `clusters` is keyed by process group on purpose. One `k` across families is
# meaningless: a coal fleet groups on cost and efficiency, a wind fleet on
# resource quality and profile SHAPE, and the right number differs with the
# spread and with what the family is for. A bare setting is accepted only where
# there is nothing to be ambiguous about -- a single group.
#' @noRd
.cl_spec <- function(clusters, groups) {
  if (is.null(clusters)) return(list())
  if (!is.list(clusters)) {
    if (length(clusters) != 1L) {
      stop("`clusters` must be a single setting or a list keyed by technology ",
           "group.", call. = FALSE)
    }
    if (length(groups) != 1L) {
      stop("`clusters = ", deparse(clusters), "` sets one number for all ",
           length(groups), " process groups of this model (",
           paste(groups, collapse = ", "), "), which is rarely what you ",
           "want: each family clusters on different parameters and has its ",
           "own structure, so it has its own k. Name them:
",
           "    clusters = list(", groups[1], " = ", deparse(clusters), ", ",
           groups[2], " = ...)
",
           "  Use get_process_groups() to see what can be merged, and ",
           "process_cluster_sweep() to choose each k.", call. = FALSE)
    }
    return(stats::setNames(list(list(k = clusters)), groups))
  }
  nm <- names(clusters)
  if (is.null(nm) || any(is.na(nm)) || !all(nzchar(nm))) {
    stop("`clusters` must be a NAMED list, keyed by process group: ",
         "clusters = list(", groups[1], " = list(k = 3)). Groups: ",
         paste(groups, collapse = ", "), call. = FALSE)
  }
  bad <- setdiff(nm, groups)
  if (length(bad)) {
    stop("`clusters` names ", paste(bad, collapse = ", "),
         ", which is not a process group of this model. Groups: ",
         paste(groups, collapse = ", "), " (see get_process_groups()).",
         call. = FALSE)
  }
  lapply(clusters, function(x) if (is.list(x)) x else list(k = x))
}

# `as = "objects"`: one object per (family x cluster), each keeping its OWN
# clusters. The merge composes the region grouping onto them, so strip that
# part back off -- `GOOD_W1` in object `EWIN_W1` is just `GOOD` again.
#' @noRd
.cl_unsuffix <- function(o, lab) {
  suf <- paste0("_", lab)
  for (sl in methods::slotNames(o)) {
    d <- methods::slot(o, sl)
    if (!is.data.frame(d) || !nrow(d) || !"cluster" %in% names(d)) next
    cl <- as.character(d$cluster)
    hit <- !is.na(cl) & (cl == lab | endsWith(cl, suf))
    cl[hit] <- ifelse(cl[hit] == lab, NA_character_, sub(suf, "", cl[hit],
                                                         fixed = TRUE))
    d$cluster <- cl
    if (identical(sl, "cluster")) d <- d[!is.na(d$cluster), , drop = FALSE]
    methods::slot(o, sl) <- d
  }
  o
}

# What the clustering may measure a family on, and what "size" means for it.
# Both are per class: a supply has no capacity and no efficiency, and a storage
# keeps its numbers under part prefixes. Each entry is
# (slot, column, feature name).
#' @noRd
.cl_candidates <- function(o) {
  switch(
    class(o)[1],
    technology = list(c("invcost", "invcost", "invcost"),
                      c("fixom", "fixom", "fixom"),
                      c("ceff", "cinp2use", "eff"),
                      c("af", "af.up", "af")),
    storage = list(c("invcost", "stg.invcost", "invcost"),
                   c("invcost", "inp.invcost", "inpcost"),
                   c("fixom", "stg.fixom", "fixom"),
                   c("seff", "stgeff", "eff"),
                   c("duration", "duration", "duration"),
                   c("af", "af.up", "af")),
    # a supply curve's shape IS its cost and how much is there
    supply = list(c("supply", "cost", "cost"),
                  c("supply", "ava.up", "ava"),
                  c("reserve", "res.up", "reserve")),
    import = list(c("import", "price", "cost"),
                  c("import", "imp.up", "ava"),
                  c("reserve", "res.up", "reserve")),
    export = list(c("export", "price", "cost"),
                  c("export", "exp.up", "ava"),
                  c("reserve", "res.up", "reserve")),
    list())
}

# The extensive quantity the intensive means are weighted by. Never a cost:
# weighting a mean cost by cost is circular.
#' @noRd
.cl_size_cols <- function(o) {
  switch(
    class(o)[1],
    technology = list(slot = "capacity",
                      cols = c("stock", "cap.up", "cap.fx", "cap.lo")),
    storage = list(slot = "capacity",
                   cols = c("stg.stock", "stg.cap.up", "stg.cap.fx",
                            "out.cap.up", "inp.cap.up")),
    supply = list(slot = "supply", cols = c("ava.up", "ava.fx", "ava.lo")),
    import = list(slot = "import", cols = c("imp.up", "imp.fx", "imp.lo")),
    export = list(slot = "export", cols = c("exp.up", "exp.fx", "exp.lo")),
    list(slot = "capacity", cols = c("stock", "cap.up")))
}

# Default features: the technology's own scalar parameters. A `function` spec
# is called with the member technologies and their regions, which is how a
# weather or af PROFILE gets in -- the scalars alone cannot see shape.
#' @noRd
.cl_feature_table <- function(techs, regs, features) {
  if (is.function(features)) {
    ft <- features(techs, regs)
    if (!is.data.frame(ft) || !"region" %in% names(ft)) {
      stop("`features` function must return a data.frame with a `region` ",
           "column and one numeric column per feature.", call. = FALSE)
    }
    return(ft[match(regs, ft$region), , drop = FALSE])
  }
  # Read each parameter PER (CLUSTER, REGION), not per object and not per
  # region alone. A family may arrive as one object per region or as one whose
  # slots carry a `region` column, and it may already be clustered by site
  # grade. Taking "the region's invcost" would return whichever grade sorted
  # first, so two regions identical in their GOOD sites but far apart in their
  # POOR ones would look the same distance apart as two identical regions.
  pick <- function(sl, col) {
    out <- NULL
    for (o in techs) {
      if (!methods::.hasSlot(o, sl)) next
      d <- methods::slot(o, sl)
      if (!is.data.frame(d) || !nrow(d) || !col %in% names(d)) next
      reg <- if ("region" %in% names(d)) as.character(d$region) else
        rep(NA_character_, nrow(d))
      reg[is.na(reg)] <- .tech_region(o)
      cl <- if ("cluster" %in% names(d)) as.character(d$cluster) else
        rep(NA_character_, nrow(d))
      cl[!nzchar(cl)] <- NA_character_
      out <- rbind(out, data.frame(
        region = reg, cluster = cl,
        value = suppressWarnings(as.numeric(d[[col]])),
        stringsAsFactors = FALSE))
    }
    if (is.null(out)) return(NULL)
    out[is.finite(out$value) & !is.na(out$region) & out$region %in% regs, ,
        drop = FALSE]
  }
  # One column per (parameter, cluster). An unclustered parameter keeps its
  # bare name; a row with no cluster applies to every one of the family's, so
  # it broadcasts rather than being dropped. A parameter that splits by cluster
  # therefore weighs more in the distance than one that does not -- which is
  # the intent: it carries more of what tells the regions apart.
  spread <- function(sl, col, nm) {
    d <- pick(sl, col)
    if (is.null(d) || !nrow(d)) return(NULL)
    labs <- sort(unique(d$cluster[!is.na(d$cluster)]))
    if (!length(labs)) {
      v <- d$value[match(regs, d$region)]
      return(stats::setNames(list(v), nm))
    }
    cols <- lapply(labs, function(k) {
      dk <- d[is.na(d$cluster) | d$cluster == k, , drop = FALSE]
      dk <- dk[order(is.na(dk$cluster)), , drop = FALSE]
      dk$value[match(regs, dk$region)]
    })
    stats::setNames(cols, paste(nm, labs, sep = "_"))
  }
  cand <- .cl_candidates(techs[[1]])
  cols <- unlist(lapply(cand, function(x) spread(x[1], x[2], x[3])),
                 recursive = FALSE)
  # the parameter a column came from, so `features = "invcost"` keeps every
  # grade's invcost column rather than none of them
  nms <- vapply(cand, function(x) x[3], character(1))
  base <- vapply(names(cols), function(n) {
    hit <- nms[n == nms | startsWith(n, paste0(nms, "_"))]
    if (length(hit)) hit[which.max(nchar(hit))] else n
  }, character(1))
  ft <- data.frame(region = regs, cols, stringsAsFactors = FALSE,
                   check.names = FALSE)
  usable <- names(which(vapply(ft[-1], function(v)
    any(is.finite(v)) && stats::sd(v, na.rm = TRUE) > 0, logical(1))))
  keep <- c("region", usable)
  if (!is.null(features)) {
    keep <- c("region", names(cols)[base %in% features | names(cols) %in%
                                      features])
  }
  if (length(keep) < 2L) {
    stop("no usable clustering features: every candidate parameter is ",
         "constant or absent across the group's regions. Pass `features=`.",
         call. = FALSE)
  }
  ft[, keep, drop = FALSE]
}
