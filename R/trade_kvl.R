# =========================================================================== #
# AC / DC line constructors and the Kirchhoff voltage-law cycle basis.
#
# energyRt's `trade` is a TRANSPORT model: the flow on a route is chosen by the
# optimiser. That is exactly right for an HVDC link, a pipeline, a shipping lane
# or a rail corridor, all of which are controllable. It is a RELAXATION for an
# AC line, where the flow is not chosen at all -- it is set by impedance, and
# the network divides a transfer between parallel paths whether the optimiser
# likes the split or not.
#
# The missing physics is Kirchhoff's VOLTAGE law. (The current law is already
# enforced: the commodity balance IS KCL.) Under the linearised DC power-flow
# approximation, KVL says that around any closed cycle the reactance-weighted
# net flows sum to zero:
#
#     sum over lines l in the cycle of  s_l * x_l * f_l  =  0
#
# with `s_l = +/-1` the line's orientation around the cycle and `f_l` its NET
# flow -- which is why a line under KVL must be declared in both directions:
# `f_l = vTradeIr[src->dst] - vTradeIr[dst->src]`.
#
# A cycle formulation needs no new variable, only a basis of independent cycles.
# On a connected graph with N nodes and E edges there are E - N + 1 of them, and
# any spanning tree gives one: each non-tree edge closes exactly one cycle with
# the tree path between its endpoints. Constraining a basis constrains every
# cycle, because every cycle is a sum of basis cycles.
#
# NOTE ON HONESTY. This closes the gap to power-system models like PyPSA -- it
# does not close the gap to reality. Linearised DC power flow assumes flat
# voltage, small angle differences and no reactive power, and is lossless by
# construction; `teff` then bolts on a FIXED loss fraction where the physics is
# quadratic. Non-linear AC formulations outperform all of this. What KVL buys
# here is comparability and validation against models that make the same
# approximation.
# =========================================================================== #

#' Create an AC transmission line
#'
#' @description
#' A `trade` object for a passive AC branch: a line whose flow is set by
#' impedance rather than chosen. `r lifecycle::badge("experimental")`
#'
#' @details
#' The constructor differs from [newTrade()] in three ways, all of which follow
#' from an AC line being a physical two-way branch rather than an arbitrary set
#' of routes:
#'
#' * it takes the two endpoints as `from` / `to` and builds BOTH directed rows,
#'   because Kirchhoff's voltage law constrains the net flow;
#' * `reactance` is required and is written to both rows, so it is symmetric by
#'   construction;
#' * every other per-route parameter passed through `trade` is applied to both
#'   directions unless it already names `src` / `dst`.
#'
#' A finite `reactance` is what marks the route as subject to KVL. The object is
#' an ordinary `trade` in every other respect, and behaves identically to one
#' unless the model is interpolated with `kvl = TRUE` -- so adding reactances to
#' a working model changes nothing until you ask for the physics.
#'
#' Where several physical circuits have been merged into one modelled line, pass
#' the EQUIVALENT reactance `1/x_eq = sum(1/x_i)`, not any one circuit's.
#'
#' @param name character, name of the object, used in sets.
#' @param commodity character, the transmitted commodity (normally electricity).
#' @param from,to character, the two endpoint regions of the line.
#' @param reactance numeric, series reactance `x`, strictly positive.
#' @param resistance numeric, series resistance `r`. Recorded but not used for
#'   losses -- losses are the fixed fraction `teff`.
#' @param trade data.frame of further per-route parameters (`ava.*`, `af.*`,
#'   `teff`), applied to both directions unless it names `src` / `dst`.
#' @param ... further arguments passed to [newTrade()].
#'
#' @return Object of class `trade`.
#' @seealso [newDCLink()], [newTrade()]
#' @family class trade
#' @export
newACLine <- function(name, commodity, from, to, reactance,
                      resistance = NA_real_, trade = data.frame(), ...) {
  if (missing(reactance)) {
    stop("newACLine(): `reactance` is required -- it is what makes the line an ",
         "AC branch rather than a controllable link. Use newDCLink() for a ",
         "link, or newTrade() for a route with no electrical identity.",
         call. = FALSE)
  }
  .line_common(name, commodity, from, to, reactance, resistance, trade,
               both = TRUE, what = "newACLine", ...)
}

#' Create a DC transmission link
#'
#' @description
#' A `trade` object for a controllable DC link -- the behaviour energyRt's
#' `trade` already had, named. `r lifecycle::badge("experimental")`
#'
#' @details
#' A DC link's flow is a control decision, which is what a transport model
#' represents natively, so this constructor adds no physics: it exists so that a
#' network reads as what it is, and so that a translation from a power-system
#' model maps one-to-one (PyPSA's `Line` to [newACLine()], its `Link` to this).
#'
#' A DC link carries NO reactance and is therefore never part of the KVL cycle
#' basis, however the model is interpolated. `resistance` is recorded for
#' round-tripping only; losses are the fixed fraction `teff`.
#'
#' @inheritParams newACLine
#' @param bidirectional logical, `TRUE` (default) declares both directions and
#'   `FALSE` only `from` -> `to`. Note both directions still share ONE capacity.
#'
#' @return Object of class `trade`.
#' @seealso [newACLine()], [newTrade()]
#' @family class trade
#' @export
newDCLink <- function(name, commodity, from, to, resistance = NA_real_,
                      trade = data.frame(), bidirectional = TRUE, ...) {
  .line_common(name, commodity, from, to, NA_real_, resistance, trade,
               both = bidirectional, what = "newDCLink", ...)
}

# Shared body of the two constructors. Kept together because the only real
# difference is whether a reactance is carried.
.line_common <- function(name, commodity, from, to, reactance, resistance,
                         trade, both, what, ...) {
  if (length(from) != 1L || length(to) != 1L) {
    stop(what, "(): `from` and `to` are the two endpoints of ONE line, so each ",
         "must be a single region. For several routes on one object use ",
         "newTrade(routes = ...) -- but note they would then share one ",
         "capacity; see the trade chapter on packaging.", call. = FALSE)
  }
  if (identical(as.character(from), as.character(to))) {
    stop(what, "(): a line's endpoints must differ (both are '", from, "').",
         call. = FALSE)
  }
  if (!is.na(reactance) && !(is.finite(reactance) && reactance > 0)) {
    stop(what, "(): `reactance` must be finite and strictly positive; got ",
         format(reactance), ". A zero-impedance line is not representable -- ",
         "merge the regions instead, or leave the reactance out to model the ",
         "line as a controllable link.", call. = FALSE)
  }
  routes <- if (both) {
    data.frame(src = c(from, to), dst = c(to, from), stringsAsFactors = FALSE)
  } else {
    data.frame(src = from, dst = to, stringsAsFactors = FALSE)
  }
  if (!is.data.frame(trade)) {
    stop(what, "(): `trade` must be a data.frame.", call. = FALSE)
  }
  # Per-route parameters that do not already name an endpoint apply to both
  # directions; ones that do are left exactly as written.
  named <- any(c("src", "dst") %in% names(trade))
  if (nrow(trade) && !named) {
    trade <- do.call(rbind, lapply(seq_len(nrow(routes)), function(i)
      cbind(routes[i, , drop = FALSE], trade, row.names = NULL)))
  }
  # The electrical characteristics are written to every declared direction, so
  # the symmetry KVL requires holds by construction rather than by validation.
  elec <- cbind(routes, reactance = reactance, resistance = resistance,
                row.names = NULL)
  trade <- if (nrow(trade)) {
    merge(trade, elec, by = intersect(names(trade), names(elec)), all = TRUE)
  } else {
    elec
  }
  newTrade(name = name, commodity = commodity, routes = routes,
           trade = trade, ...)
}


# =========================================================================== #
# The KVL network: which routes take part, and the cycle basis over them
# =========================================================================== #

# Every route of every trade object that carries a finite, positive reactance,
# as UNDIRECTED PHYSICAL lines. Returns one row per line with its canonical
# orientation (`from` < `to` lexically) so that two directed rows collapse to one
# edge, plus a `members` list column naming the trade objects that make it up.
#
# `members` is what makes loss tranches work. A tranched line is several `trade`
# objects -- parallel sub-lines with different `teff` -- that are ONE physical
# line sharing one reactance, so they must contribute one edge to the graph and
# one summand per direction to each cycle. `prov` is the variant provenance
# table (`name`, `base`, ...) from `expand_variants()`; objects sharing a `base`
# are tranches of one line. Without it every object is its own line, which is
# the un-expanded behaviour and still correct.
#
# Validation is strict and never silently repairs: an asymmetric reactance and a
# one-way AC line are both modelling errors whose "obvious" fix (take the mean,
# assume the reverse) would change the physics without saying so.
.kvl_lines <- function(objs, prov = NULL) {
  # name -> base; identity for anything expansion did not touch.
  lookup <- NULL
  if (!is.null(prov) && nrow(prov) && all(c("name", "base") %in% names(prov))) {
    lookup <- stats::setNames(as.character(prov$base), as.character(prov$name))
  }
  base_of <- function(nm) {
    if (is.null(lookup)) return(nm)
    b <- unname(lookup[nm])
    if (is.na(b)) nm else b
  }

  rows <- list()
  for (o in objs) {
    if (!methods::is(o, "trade")) next
    tr <- o@trade
    if (!nrow(tr) || is.null(tr$reactance)) next
    tr <- tr[is.finite(tr$reactance), , drop = FALSE]
    if (!nrow(tr)) next
    if (any(tr$reactance <= 0)) {
      stop("trade '", o@name, "': reactance must be strictly positive; found ",
           format(min(tr$reactance)), ".", call. = FALSE)
    }
    if (is.null(tr$src) || is.null(tr$dst) || anyNA(tr$src) || anyNA(tr$dst)) {
      stop("trade '", o@name, "': a reactance must name both `src` and `dst`. ",
           "An unrouted reactance cannot be placed on a line.", call. = FALSE)
    }
    key <- ifelse(as.character(tr$src) < as.character(tr$dst),
                  paste(tr$src, tr$dst, sep = "\r"),
                  paste(tr$dst, tr$src, sep = "\r"))
    for (k in unique(key)) {
      part <- tr[key == k, , drop = FALSE]
      ends <- strsplit(k, "\r", fixed = TRUE)[[1]]
      dirs <- unique(paste(part$src, part$dst, sep = "\r"))
      if (length(dirs) < 2L) {
        stop("trade '", o@name, "': the AC line ", ends[1], " -- ", ends[2],
             " is declared in one direction only. Kirchhoff's voltage law ",
             "constrains the NET flow, so both directed routes are required. ",
             "newACLine() builds both; newTrade() does not.", call. = FALSE)
      }
      x <- unique(round(part$reactance, 12))
      if (length(x) > 1L) {
        stop("trade '", o@name, "': the AC line ", ends[1], " -- ", ends[2],
             " has different reactances by direction (",
             paste(format(x), collapse = ", "),
             "). A line has one reactance; it is not a directional property.",
             call. = FALSE)
      }
      rows[[length(rows) + 1L]] <- data.frame(
        trade = base_of(o@name), member = o@name,
        from = ends[1], to = ends[2], x = x[1],
        stringsAsFactors = FALSE)
    }
  }
  empty <- data.frame(trade = character(), from = character(),
                      to = character(), x = numeric(),
                      stringsAsFactors = FALSE)
  empty$members <- list()
  if (!length(rows)) return(empty)
  raw <- do.call(rbind, rows)

  # Collapse the tranches of one physical line into a single edge. The key is
  # (base, from, to): tranches share a base, independent circuits do not.
  grp <- paste(raw$trade, raw$from, raw$to, sep = "\r")
  keep <- !duplicated(grp)
  out <- raw[keep, c("trade", "from", "to", "x"), drop = FALSE]
  rownames(out) <- NULL
  # Assign the list column AFTER the rbind -- `rbind` on data.frames mangles one.
  out$members <- lapply(out$trade, function(b) character())
  for (i in seq_len(nrow(out))) {
    sel <- raw$trade == out$trade[i] & raw$from == out$from[i] &
      raw$to == out$to[i]
    out$members[[i]] <- sort(unique(raw$member[sel]))
    # A reactance is a property of the LINE, not of a tranche: the tranches of
    # one line differ in loss rate and in capacity, never in impedance.
    xs <- unique(round(raw$x[sel], 12))
    if (length(xs) > 1L) {
      stop("trade '", out$trade[i], "': the AC line ", out$from[i], " -- ",
           out$to[i], " has different reactances on its parallel parts (",
           paste(format(xs), collapse = ", "), "). Reactance is a property of ",
           "the line, not of a tranche -- declare it once, with no `cluster`, ",
           "so it broadcasts to every tranche.", call. = FALSE)
    }
  }

  # Two DIFFERENT objects claiming one corridor are independent circuits, which
  # must be merged by the user with the equivalent reactance. Keyed on the base,
  # so tranches (already collapsed above) never reach this.
  dup <- duplicated(paste(out$from, out$to, sep = "\r"))
  if (any(dup)) {
    stop("the line(s) ",
         paste(unique(paste(out$from[dup], "--", out$to[dup])), collapse = ", "),
         " carry a reactance on more than one trade object. Parallel circuits ",
         "must be merged into one line with the equivalent reactance ",
         "(1/x_eq = sum(1/x_i)); two objects on one corridor would each be ",
         "constrained as if it were the whole line. (Loss TRANCHES of one line ",
         "are not this: they share a base object and are collapsed ",
         "automatically -- this is two different objects.)", call. = FALSE)
  }
  out
}

# A fundamental cycle basis of the KVL network, from a spanning forest.
#
# `lines` is the output of `.kvl_lines()`. Returns a long data.frame with one
# row per (cycle, line): `cycle`, `trade`, `src`, `dst`, `x` and `sign`, where
# `(src, dst)` is the line's canonical direction and `sign` is +1 if the cycle
# traverses it that way and -1 if against.
#
# The basis has E - N + C members (C = connected components) and is empty for a
# radial network -- which is the sharpest available check that it is right, and
# the reason a tree topology is unaffected by `kvl = TRUE`.
.kvl_cycle_basis <- function(lines) {
  empty <- data.frame(cycle = character(), trade = character(),
                      src = character(), dst = character(),
                      x = numeric(), sign = numeric(),
                      stringsAsFactors = FALSE)
  if (!nrow(lines)) return(empty)

  nodes <- unique(c(lines$from, lines$to))
  # Union-find over the endpoints: an edge joining two already-connected nodes
  # is a non-tree edge and closes exactly one cycle.
  parent <- stats::setNames(seq_along(nodes), nodes)
  find <- function(i) { while (parent[i] != i) i <- parent[i]; i }

  tree <- rep(FALSE, nrow(lines))
  # Adjacency of the spanning forest, built as we accept edges.
  adj <- stats::setNames(vector("list", length(nodes)), nodes)
  for (i in seq_len(nrow(lines))) {
    a <- find(match(lines$from[i], nodes)); b <- find(match(lines$to[i], nodes))
    if (a != b) {
      parent[a] <- b
      tree[i] <- TRUE
      adj[[lines$from[i]]] <- c(adj[[lines$from[i]]], i)
      adj[[lines$to[i]]]   <- c(adj[[lines$to[i]]], i)
    }
  }

  # Path between two nodes through the forest, as edge indices, by breadth-first
  # search. The forest is acyclic so the path is unique when it exists.
  tree_path <- function(from, to) {
    seen <- stats::setNames(rep(FALSE, length(nodes)), nodes)
    came <- stats::setNames(rep(NA_integer_, length(nodes)), nodes)
    queue <- from; seen[from] <- TRUE
    while (length(queue)) {
      cur <- queue[1]; queue <- queue[-1]
      if (identical(cur, to)) break
      for (e in adj[[cur]]) {
        nxt <- if (identical(lines$from[e], cur)) lines$to[e] else lines$from[e]
        if (!seen[nxt]) { seen[nxt] <- TRUE; came[nxt] <- e; queue <- c(queue, nxt) }
      }
    }
    if (!seen[to]) return(NULL)
    path <- integer(); cur <- to
    while (!identical(cur, from)) {
      e <- came[[cur]]
      path <- c(e, path)
      cur <- if (identical(lines$from[e], cur)) lines$to[e] else lines$from[e]
    }
    path
  }

  out <- list()
  extra <- which(!tree)
  for (n in seq_along(extra)) {
    i <- extra[n]
    # Walk the cycle in a consistent direction: from the non-tree edge's `to`
    # back to its `from` along the tree, then across the edge itself.
    path <- tree_path(lines$to[i], lines$from[i])
    if (is.null(path)) next   # cannot happen: the edge closed a cycle
    walk <- c(path, i)
    at <- lines$to[i]
    sgn <- numeric(length(walk))
    for (k in seq_along(walk)) {
      e <- walk[k]
      # +1 when the cycle traverses the line in its canonical direction.
      sgn[k] <- if (identical(lines$from[e], at)) 1 else -1
      at <- if (identical(lines$from[e], at)) lines$to[e] else lines$from[e]
    }
    d <- data.frame(
      cycle = sprintf("KVL_%d", n),
      trade = lines$trade[walk],
      src   = lines$from[walk],
      dst   = lines$to[walk],
      x     = lines$x[walk],
      sign  = sgn,
      stringsAsFactors = FALSE)
    # After the data.frame() call, not inside it -- a list column passed to
    # data.frame() is expanded into one column per element.
    d$members <- if (is.null(lines$members)) as.list(lines$trade[walk]) else
      lines$members[walk]
    out[[length(out) + 1L]] <- d
  }
  if (!length(out)) return(empty)
  do.call(rbind, out)
}


# One `constraint` object per independent cycle.
#
# `cyc` is `.kvl_cycle_basis()` output and `each` the (comm, year, timeslice)
# frame the law is enforced over -- KVL holds instant by instant, so `timeslice`
# MUST be there; leaving it out would sum the flows over the year first and
# constrain only the average.
#
# Each line contributes TWO summands, one per declared direction, because the
# law is about the NET flow: `f = ir[src->dst] - ir[dst->src]`. `for.sum` names
# the endpoints separately -- which is possible only because `vTradeIr`'s two
# region dimensions are now `src` and `dst` rather than `region` twice.
#
# A line split into loss TRANCHES is still one line: every tranche carries the
# same coefficient `sign * x`, because the law constrains the total line flow
# `f = sum_t f_t`. So the tranches go into ONE summand as a vector `for.sum` on
# the `trade` dimension (which becomes a membership map, exactly as the variant
# group bounds already do) and the equation stays at two summands per line
# however many tranches there are.
.kvl_constraints <- function(cyc, each) {
  out <- list()
  for (cy in unique(cyc$cycle)) {
    part <- cyc[cyc$cycle == cy, , drop = FALSE]
    terms <- list()
    for (k in seq_len(nrow(part))) {
      co <- part$sign[k] * part$x[k]
      mem <- if (is.null(part$members)) part$trade[k] else part$members[[k]]
      terms[[length(terms) + 1L]] <- list(
        variable = "vTradeIr",
        for.sum = list(trade = mem, src = part$src[k],
                       dst = part$dst[k]),
        mult = co)
      terms[[length(terms) + 1L]] <- list(
        variable = "vTradeIr",
        for.sum = list(trade = mem, src = part$dst[k],
                       dst = part$src[k]),
        mult = -co)
    }
    names(terms) <- paste0("t", seq_along(terms))
    out[[cy]] <- do.call(newConstraint, c(
      list(name = cy,
           desc = paste0("Kirchhoff voltage law around ",
                         paste(unique(c(part$src, part$dst)), collapse = "-")),
           eq = "==", for.each = each, rhs = 0, defVal = 0,
           # A KVL coefficient is an exact reactance, not a quantity that wants
           # smoothing between milestones.
           interpolation = "inter"),
      terms))
  }
  out
}

# Build the KVL constraints for a model, or return an empty list.
#
# Called from `interpolate_model(..., kvl = TRUE)`. Returns early and silently
# on a model with no reactances at all, so the flag is safe to leave on.
.kvl_build <- function(mod, prov = NULL, settings = NULL, verbose = FALSE) {
  objs <- unlist(lapply(mod@data, function(rp)
    if (methods::is(rp, "repository")) rp@data else list()), recursive = FALSE)
  lines <- .kvl_lines(objs, prov)
  if (!nrow(lines)) {
    if (verbose) message("KVL: no trade object carries a reactance; nothing to do.")
    return(list())
  }
  cyc <- .kvl_cycle_basis(lines)
  nnode <- length(unique(c(lines$from, lines$to)))
  ncyc <- length(unique(cyc$cycle))
  if (!ncyc) {
    # A radial network has no independent cycle, so KVL says nothing about it.
    # This is not a failure and not a silent no-op either -- it is the sharpest
    # available confirmation that the basis is right.
    if (verbose) {
      message("KVL: the network of ", nrow(lines), " line(s) over ", nnode,
              " region(s) is radial, so it has no cycles and KVL is vacuous.")
    }
    return(list())
  }
  if (verbose) {
    message("KVL: ", ncyc, " independent cycle(s) over ", nrow(lines),
            " line(s) and ", nnode, " region(s).")
  }
  # Match on the MEMBERS, not on the line's base name: a tranched line's base is
  # not itself a trade object any more. Miss this and the commodity is not
  # found, `for.each` comes back empty, and the constraint covers zero cells --
  # a silent no-op rather than an error.
  members <- unique(unlist(lines$members))
  comms <- unique(unlist(lapply(objs, function(o)
    if (methods::is(o, "trade") && o@name %in% members) o@commodity)))
  each <- .kvl_for_each(mod, comms, settings)
  .kvl_constraints(cyc, each)
}

# The (comm, year, timeslice) frame KVL is enforced over: every milestone, and
# every timeslice the commodity is balanced at.
#
# Prefers the SCENARIO's horizon and calendar: either may have been replaced by
# an argument passed through `interpolate_model(...)`, in which case the model's
# own would index the constraint on timeslices the scenario does not declare.
# Falls back to the model config so the direct unit tests keep working.
.kvl_for_each <- function(mod, comms, settings = NULL) {
  src <- if (!is.null(settings)) settings else mod@config
  yrs <- sort(unique(as.integer(src@horizon@intervals$mid)))
  cal <- src@calendar
  out <- list()
  for (cm in comms) {
    tf <- NULL
    for (rp in mod@data) {
      if (!methods::is(rp, "repository")) next
      for (o in rp@data) {
        if (methods::is(o, "commodity") && identical(o@name, cm)) tf <- o@timeframe
      }
    }
    sl <- .kvl_frame_slices(cal, tf)
    out[[cm]] <- expand.grid(comm = cm, year = yrs, timeslice = sl,
                             stringsAsFactors = FALSE)
  }
  do.call(rbind, out)
}


# Timeslices at a calendar's `tf` timeframe, by descending the family tree from
# the root once per rank. The calendar records a rank per frame and a one-step
# parent/child table but no explicit level column, so the level is walked rather
# than looked up. Falls back to every timeslice the calendar knows if the frame
# is missing or unnamed.
.kvl_frame_slices <- function(cal, tf) {
  all_sl <- unique(as.character(cal@timeslice_share$timeslice))
  if (is.null(tf) || length(tf) != 1L || is.na(tf)) return(all_sl)
  rk <- cal@timeframe_rank
  if (is.null(rk[[tf]])) return(all_sl)
  fam <- cal@timeslice_family
  if (is.null(fam) || !nrow(fam)) return(all_sl)
  cur <- setdiff(unique(as.character(fam$parent)), unique(as.character(fam$child)))
  steps <- as.integer(rk[[tf]]) - min(as.integer(unlist(rk)))
  for (k in seq_len(max(0L, steps))) {
    nxt <- unique(as.character(fam$child[as.character(fam$parent) %in% cur]))
    if (!length(nxt)) break
    cur <- nxt
  }
  if (!length(cur)) all_sl else cur
}


# Does this scenario carry KVL cycle constraints? They are ordinary user
# constraints named `KVL_<n>`, so no extra state has to be stored on the
# scenario for this to be answerable.
.kvl_scenario_cycles <- function(scen) {
  eqs <- tryCatch(names(scen@modInp@gams.equation), error = function(e) NULL)
  grep("^KVL_[0-9]+$", eqs, value = TRUE)
}

# Reconcile what a solve asked for against what the scenario was built with.
# `kvl = NULL` (the default) means "no opinion": report if the scenario carries
# KVL, so the solve is never silently different from expectation, but never
# refuse.
.kvl_guard <- function(scen, kvl) {
  cy <- .kvl_scenario_cycles(scen)
  has <- length(cy) > 0
  if (is.null(kvl)) {
    if (has) {
      message("Kirchhoff voltage law is ENGAGED on this scenario (",
              length(cy), " cycle constraint(s)); flows are set by reactance, ",
              "not chosen.")
    }
    return(invisible(NULL))
  }
  if (isTRUE(kvl) && !has) {
    stop("solve_scenario(kvl = TRUE): this scenario carries no KVL constraints, so ",
         "the solve would silently return the transport relaxation. KVL is ",
         "built at interpolation time -- rebuild with ",
         "`interpolate_model(mod, ..., kvl = TRUE)`. (If the network is radial ",
         "it has no cycles and KVL is vacuous; interpolating with kvl = TRUE ",
         "will say so.)", call. = FALSE)
  }
  if (isFALSE(kvl) && has) {
    stop("solve_scenario(kvl = FALSE): this scenario WAS built with KVL (",
         length(cy), " cycle constraint(s)) and they cannot be removed at ",
         "solve time. Re-interpolate without `kvl = TRUE` for the transport ",
         "relaxation.", call. = FALSE)
  }
  invisible(NULL)
}
