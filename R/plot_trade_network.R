#' Plot trade as a network: capacities and flows between regions
#'
#' @description
#' Draws trade objects as a network of regions (nodes) and links, one link per
#' pair of regions and commodity, its width set by a value: the input
#' capacities of the trade objects, the solved trade capacity (`vTradeCap`), or
#' the solved flows (`vTradeIr`). Links are quadratic curves whose bends are
#' flipped to reduce crossings.
#'
#' Nodes are placed at region centroids when a `map` (an `sf` object or a
#' `geoscales::GeoScale`) is given or the model carries a geoscale; otherwise
#' they are laid out on a circle and no geography is needed.
#'
#' @param object a `trade`, a list of trades, or a `repository`, `model` or
#'   `scenario` (all of whose trade objects are drawn). `value = "capacity"` and
#'   `"flow"` need a solved `scenario`.
#' @param map optional `sf` object with `region` (and `x`, `y`) columns, or a
#'   `geoscales::GeoScale`. When `NULL`, the object's geoscale is used if it has
#'   one, else a circular layout.
#' @param value what sets the link width:
#'   * `"input"`: the trade objects' `@capacity` in `year`, by what the link
#'     IS rather than by one category -- `stock` (capacity standing, no way to
#'     add more) solid; `stock + new` (standing capacity that may also be
#'     expanded, through `ncap.up`, `cap.up` or an `@invcost` row) long-dashed;
#'     `new` (nothing standing, investable only) dotted. Width is the standing
#'     capacity, or the ceiling for a link with none, with a faint band out to
#'     `cap.up` where one is declared;
#'   * `"capacity"`: the solved `vTradeCap` in `year`;
#'   * `"flow"`: the solved `vTradeIr` in `year`, netted per pair; the arrow
#'     points in the net direction.
#' @param comm draw only the trades carrying this commodity (or these). `NULL`
#'   (default) draws every one. Commodities that are not in the same capacity
#'   unit cannot share one width scale, which is what makes this the usual
#'   way to read a mixed repository.
#' @param year the year to draw; default the first year present.
#' @param timeslice for `value = "flow"`: `"ANNUAL"` (default), the year's total
#'   as `getData(timeframe = "lowest")` returns it (weighted to the full year on
#'   a sampled calendar); or any timeslice of the calendar -- a day, an hour --
#'   for that snapshot.
#' @param level for a geoscale `map`: the level to draw; routes are lifted to it
#'   and those inside one region dropped. `NULL` (default) draws every level,
#'   so corridors declared at the level their commodity is balanced on are
#'   kept whatever that level is.
#' @param curvature bend of the links as a share of their length (0 = straight).
#' @param labels draw region names.
#' @param node_size scale the node points by the capacity connected to each
#'   region. `FALSE` (default) draws them at one size: the sum of the links
#'   touching a region is the links read a second way, and beside the
#'   corridors themselves it says the same thing twice.
#' @param ... unused.
#'
#' @return a `ggplot` object.
#' @export
#' @examples
#' \dontrun{
#' plot_trade_network(repo)                                  # inputs, circular layout
#' plot_trade_network(scen, value = "capacity", year = 2050) # solved capacity on the map
#' plot_trade_network(scen, value = "flow", timeslice = "d180_h18")
#' }
plot_trade_network <- function(object, map = NULL,
                               value = c("input", "capacity", "flow"),
                               comm = NULL,
                               year = NULL, timeslice = "ANNUAL", level = NULL,
                               curvature = 0.15, labels = TRUE,
                               node_size = FALSE, ...) {
  check_package("ggplot2")
  value <- match.arg(value)
  trades <- .filter_trade_comm(.as_trade_list(object), comm)
  if (!length(trades)) {
    .say("No trade objects to plot.")
    return(invisible(NULL))
  }
  if (value != "input" && !methods::is(object, "scenario")) {
    stop("plot_trade_network: value = \"", value, "\" needs a solved scenario.",
         call. = FALSE)
  }

  # routes of every trade object, endpoints lifted to `level` when drawn there
  routes <- do.call(rbind, lapply(trades, function(t) {
    r <- as.data.frame(t@routes)
    if (!nrow(r) || !all(c("src", "dst") %in% names(r))) return(NULL)
    data.frame(trade = t@name, comm = .trade_comm(t), src = as.character(r$src),
               dst = as.character(r$dst), stringsAsFactors = FALSE)
  }))
  if (is.null(routes) || !nrow(routes)) {
    .say("No routes to plot.")
    return(invisible(NULL))
  }
  if (is.null(map)) map <- tryCatch(getGeoscale(object), error = function(e) NULL)
  route_gs <- if (!is.null(map) && is_geoscale(map)) map else NULL
  routes <- .routes_to_level(routes, route_gs, level)

  links <- switch(value,
    input = .tn_input(trades, routes, year),
    capacity = .tn_capacity(object, routes, year),
    flow = .tn_flow(object, routes, year, timeslice))
  links <- links[is.finite(links$value) & links$value > 0, , drop = FALSE]
  if (!nrow(links)) {
    .say("No positive ", value, " to plot.")
    return(invisible(NULL))
  }

  # node positions: centroids of the map, else a circle
  regions <- sort(unique(c(links$a, links$b)))
  shapes <- NULL
  if (!is.null(map)) {
    # The polygons are one level -- a background cannot be two -- but the NODES
    # need not be: a model declares its corridors at the level each commodity
    # is balanced on, so electricity may run between regions while a fuel runs
    # between zones. Naming one level silently dropped every route declared at
    # another, which is the same defect `plot_trade_map()` was fixed for.
    shapes <- .trade_map_normalise(map, level = level)
    ce <- as.data.frame(shapes)
    nodes <- data.frame(region = as.character(ce$region), x = ce$x, y = ce$y)
    if (is.null(level) && !is.null(route_gs)) {
      ca <- .trade_centers_all(route_gs)
      if (!is.null(ca)) nodes <- ca[, c("region", "x", "y")]
    }
    miss <- setdiff(regions, nodes$region)
    if (length(miss)) {
      warning("Dropping links of ", length(miss), " region(s) not in `map`: ",
              paste(utils::head(miss, 5), collapse = ", "), call. = FALSE)
      links <- links[!links$a %in% miss & !links$b %in% miss, , drop = FALSE]
    }
    nodes <- nodes[nodes$region %in% c(links$a, links$b), , drop = FALSE]
  } else {
    ang <- pi / 2 - 2 * pi * (seq_along(regions) - 1) / length(regions)
    nodes <- data.frame(region = regions, x = cos(ang), y = sin(ang))
  }
  links <- .tn_geometry(links, nodes, curvature)

  # What the width is measured in. Keyed by COMMODITY and resolved against the
  # links that survived, not against every trade handed in: a region map drops
  # the zone-level fuel corridors, and their unit must not be reported for a
  # picture they are not in.
  cu <- vapply(trades, function(t)
    .object_units(t, "capacity") %||% NA_character_, character(1))
  names(cu) <- vapply(trades, .trade_comm, character(1))
  cu <- cu[!duplicated(names(cu))]
  cap_unit <- unique(stats::na.omit(unname(cu[as.character(links$comm)])))
  if (length(cap_unit) > 1L) {
    warning("the links carry more than one capacity unit (",
            paste(cap_unit, collapse = ", "), "), and one width scale cannot ",
            "measure both -- their widths are not comparable. Draw one ",
            "commodity at a time, or declare them in the same unit.",
            call. = FALSE)
  }
  .t <- .trade_plot_title(object)
  .tn_draw(links, nodes, shapes, value = value, year = attr(links, "year"),
           timeslice = timeslice, labels = labels, node_size = node_size,
           cap_unit = if (length(cap_unit)) cap_unit else NULL,
           ttl = .t$title, sub = .t$subtitle)
}

# the commodity a trade object carries (one per trade object)
#' @noRd
.trade_comm <- function(t) {
  cm <- tryCatch(as.character(t@commodity), error = function(e) character())
  if (length(cm) && nzchar(cm[1])) cm[1] else NA_character_
}

# unordered pair key: a < b
#' @noRd
.tn_pair <- function(src, dst) {
  data.frame(a = pmin(src, dst), b = pmax(src, dst), stringsAsFactors = FALSE)
}

# the year to draw: the requested one, else the first present
#' @noRd
.tn_year <- function(years, year) {
  years <- sort(unique(stats::na.omit(years)))
  if (!is.null(year)) return(year)
  if (length(years)) years[1] else NA
}

# value = "input": stock, upper bound and status per trade, spread over its pairs
#' @noRd
.tn_input <- function(trades, routes, year) {
  cap <- do.call(rbind, lapply(trades, function(t) {
    d <- as.data.frame(t@capacity)
    if (!nrow(d)) d <- data.frame(year = NA_integer_)
    d$trade <- t@name
    d$invest <- nrow(as.data.frame(t@invcost)) > 0
    d
  }))
  y <- .tn_year(cap$year, year)
  cap <- cap[is.na(cap$year) | cap$year %in% y, , drop = FALSE]
  num <- function(d, cn) if (cn %in% names(d)) suppressWarnings(as.numeric(d[[cn]])) else rep(NA_real_, nrow(d))
  per <- do.call(rbind, lapply(split(cap, cap$trade), function(d) {
    stock <- sum(num(d, "stock"), na.rm = TRUE)
    up <- suppressWarnings(max(num(d, "cap.up"), na.rm = TRUE))
    nup <- suppressWarnings(max(num(d, "ncap.up"), na.rm = TRUE))
    if (!is.finite(up) && is.finite(nup)) up <- stock + nup
    expandable <- (is.finite(nup) && nup > 0) || (is.finite(up) && up > stock) ||
      (any(d$invest) && !identical(nup, 0))
    # Three states, because a corridor commonly has BOTH: capacity standing
    # today and the right to build more. Collapsing that to "existing" hid
    # every investable link behind the stock it already has.
    data.frame(trade = d$trade[1], stock = stock,
               upper = if (is.finite(up)) up else NA_real_,
               status = if (stock > 0 && expandable) "stock + new"
                        else if (stock > 0) "stock"
                        else if (expandable) "new"
                        else "none")
  }))
  l <- merge(cbind(routes, .tn_pair(routes$src, routes$dst)), per, by = "trade")
  l <- unique(l[, c("trade", "comm", "a", "b", "stock", "upper", "status")])
  # The line's width is what STANDS; the band behind it is the headroom, drawn
  # only when a ceiling says how much. A link with no stock has nothing to
  # stand, so its width is the ceiling it could reach.
  has_stock <- l$status %in% c("stock", "stock + new")
  l$value <- ifelse(has_stock, l$stock, l$upper)
  l$band <- ifelse(has_stock & !is.na(l$upper) & l$upper > l$stock, l$upper, NA_real_)
  out <- stats::aggregate(cbind(value, stock) ~ comm + a + b + status, data = l, FUN = sum, na.action = stats::na.pass)
  if (any(!is.na(l$band))) {
    bnd <- stats::aggregate(band ~ comm + a + b + status, data = l[!is.na(l$band), , drop = FALSE], FUN = sum)
    out <- merge(out, bnd, all.x = TRUE)
  } else out$band <- NA_real_
  out$dir <- "both"
  attr(out, "year") <- y
  out
}

# value = "capacity": solved vTradeCap per trade, spread over its pairs
#' @noRd
.tn_capacity <- function(scen, routes, year) {
  d <- as.data.frame(getData(scen, "vTradeCap", merge = TRUE))
  if (!nrow(d)) stop("plot_trade_network: no vTradeCap in the scenario.", call. = FALSE)
  y <- .tn_year(d$year, year)
  d <- d[d$year %in% y, , drop = FALSE]
  d <- stats::aggregate(value ~ trade, data = d, FUN = sum)
  l <- merge(cbind(routes, .tn_pair(routes$src, routes$dst)), d, by = "trade")
  l <- unique(l[, c("trade", "comm", "a", "b", "value")])
  out <- stats::aggregate(value ~ comm + a + b, data = l, FUN = sum)
  out$status <- "solved"
  out$dir <- "both"
  attr(out, "year") <- y
  out
}

# value = "flow": solved vTradeIr at one timeslice of any calendar level --
# "ANNUAL" (getData's own roll-up, weighted to the full year on a sampled
# calendar), a day, an hour -- netted per pair; dir = the net direction
#' @noRd
.tn_flow <- function(scen, routes, year, timeslice) {
  annual <- identical(toupper(timeslice), "ANNUAL")
  d <- as.data.frame(getData(scen, "vTradeIr", merge = TRUE,
                             timeframe = if (annual) "lowest" else "all"))
  if (!nrow(d)) stop("plot_trade_network: no vTradeIr in the scenario.", call. = FALSE)
  # decoders differ: multimod writes src/dst, the Pyomo route region/regionp
  if (!"src" %in% names(d) && "region" %in% names(d)) names(d)[names(d) == "region"] <- "src"
  if (!"dst" %in% names(d) && "regionp" %in% names(d)) names(d)[names(d) == "regionp"] <- "dst"
  y <- .tn_year(d$year, year)
  d <- d[d$year %in% y, , drop = FALSE]
  if (!annual) {
    if (!timeslice %in% d$timeslice) {
      stop("plot_trade_network: timeslice '", timeslice, "' not in the results.", call. = FALSE)
    }
    d <- d[d$timeslice %in% timeslice, , drop = FALSE]
  }
  if (!"comm" %in% names(d)) d$comm <- routes$comm[match(d$trade, routes$trade)]
  f <- stats::aggregate(value ~ comm + src + dst, data = d, FUN = sum)
  p <- .tn_pair(f$src, f$dst)
  f$signed <- ifelse(f$src == p$a, f$value, -f$value)     # + means a -> b
  f <- cbind(f, p)
  out <- stats::aggregate(cbind(signed, value) ~ comm + a + b, data = f, FUN = sum)
  names(out)[names(out) == "value"] <- "gross"
  out$value <- abs(out$signed)
  out$dir <- ifelse(out$signed >= 0, "ab", "ba")
  out$status <- "solved"
  attr(out, "year") <- y
  out
}

# Bezier curve per link; parallel links of one pair bend to opposite sides, then
# greedy passes flip bends where that removes crossings
#' @noRd
.tn_geometry <- function(links, nodes, curvature, n = 30) {
  yr <- attr(links, "year")
  pa <- as.matrix(nodes[match(links$a, nodes$region), c("x", "y")])
  pb <- as.matrix(nodes[match(links$b, nodes$region), c("x", "y")])
  key <- paste(links$a, links$b)
  rank <- stats::ave(seq_along(key), key, FUN = seq_along)
  links$bend <- ifelse(rank %% 2 == 1, 1, -1) * curvature * ceiling(rank / 2)
  tt <- seq(0, 1, length.out = n)
  curve <- function(i, b) {
    v <- pb[i, ] - pa[i, ]
    nrm <- c(-v[2], v[1])
    ctrl <- (pa[i, ] + pb[i, ]) / 2 + 2 * b * nrm / 2
    cbind(x = (1 - tt)^2 * pa[i, 1] + 2 * (1 - tt) * tt * ctrl[1] + tt^2 * pb[i, 1],
          y = (1 - tt)^2 * pa[i, 2] + 2 * (1 - tt) * tt * ctrl[2] + tt^2 * pb[i, 2])
  }
  crosses <- function(m1, m2) {
    s1 <- m1[-c(1, nrow(m1)), , drop = FALSE]; s2 <- m2[-c(1, nrow(m2)), , drop = FALSE]
    if (nrow(s1) < 2 || nrow(s2) < 2) return(FALSE)
    i <- rep(seq_len(nrow(s1) - 1), times = nrow(s2) - 1)
    j <- rep(seq_len(nrow(s2) - 1), each = nrow(s1) - 1)
    o <- function(p, q, r) (q[, 1] - p[, 1]) * (r[, 2] - p[, 2]) - (q[, 2] - p[, 2]) * (r[, 1] - p[, 1])
    A <- s1[i, , drop = FALSE]; B <- s1[i + 1, , drop = FALSE]
    C <- s2[j, , drop = FALSE]; D <- s2[j + 1, , drop = FALSE]
    any(o(A, B, C) * o(A, B, D) < 0 & o(C, D, A) * o(C, D, B) < 0)
  }
  geo <- lapply(seq_len(nrow(links)), function(i) curve(i, links$bend[i]))
  if (curvature != 0 && nrow(links) > 1) {
    ncross <- function(i, m) sum(vapply(setdiff(seq_along(geo), i), function(j) crosses(m, geo[[j]]), TRUE))
    for (pass in 1:3) {
      changed <- FALSE
      for (i in seq_along(geo)) {
        alt <- curve(i, -links$bend[i])
        if (ncross(i, alt) < ncross(i, geo[[i]])) {
          geo[[i]] <- alt; links$bend[i] <- -links$bend[i]; changed <- TRUE
        }
      }
      if (!changed) break
    }
  }
  # arrows run along the curve: a -> b as drawn, reversed for "ba"
  # directed links stop short of the node so the arrowhead shows
  trim <- function(m) { k <- max(1, round(nrow(m) * 0.06)); m[1:(nrow(m) - k), , drop = FALSE] }
  links$geom <- lapply(seq_along(geo), function(i) {
    m <- if (identical(links$dir[i], "ba")) geo[[i]][nrow(geo[[i]]):1, ] else geo[[i]]
    if (links$dir[i] %in% c("ab", "ba")) trim(m) else m
  })
  attr(links, "year") <- yr
  links
}

#' @noRd
.tn_draw <- function(links, nodes, shapes, value, year, timeslice, labels,
                     node_size, cap_unit = NULL, ttl = NULL, sub = NULL) {
  have_sf <- !is.null(shapes) && requireNamespace("sf", quietly = TRUE) && methods::is(shapes, "sf")
  crs <- if (have_sf) sf::st_crs(shapes) else NA
  as_sf <- have_sf && !is.na(crs)
  links$id <- seq_len(nrow(links))
  links$kind <- switch(value, input = links$status, "solved")
  paths <- do.call(rbind, lapply(seq_len(nrow(links)), function(i)
    data.frame(id = i, x = links$geom[[i]][, "x"], y = links$geom[[i]][, "y"])))
  attrs <- links[, intersect(c("id", "comm", "value", "band", "kind", "dir"), names(links))]
  multi_comm <- length(unique(stats::na.omit(links$comm))) > 1
  colour_aes <- if (multi_comm) "comm" else NULL
  nodes$total <- vapply(nodes$region, function(r) sum(links$value[links$a == r | links$b == r]), 0)

  arr <- grid::arrow(type = "closed", angle = 18, length = grid::unit(0.09, "inches"))
  as_layer <- function(df) {
    if (!as_sf) return(df)
    g <- lapply(split(df, df$id), function(d) sf::st_linestring(as.matrix(d[, c("x", "y")])))
    sf::st_sf(attrs[match(as.integer(names(g)), attrs$id), , drop = FALSE], geometry = sf::st_sfc(g, crs = crs))
  }
  path_layer <- function(df, mapping, ...) {
    if (as_sf) ggplot2::geom_sf(data = as_layer(df), mapping = mapping, ...)
    else ggplot2::geom_path(data = merge(df, attrs, by = "id"),
                            mapping = utils::modifyList(ggplot2::aes(x = .data$x, y = .data$y, group = .data$id), mapping), ...)
  }
  p <- ggplot2::ggplot()
  if (have_sf) p <- p + ggplot2::geom_sf(data = shapes, fill = "grey95", colour = "grey75", linewidth = 0.25)
  # headroom bands behind existing links (inputs)
  band <- paths[paths$id %in% links$id[!is.na(links$band %||% NA)], , drop = FALSE]
  if (nrow(band)) {
    p <- p + path_layer(band, ggplot2::aes(linewidth = .data$band), colour = "grey55", alpha = 0.25, lineend = "round")
  }
  # Two different statements, so two layers rather than one line trying to
  # carry both. What STANDS is solid and scaled by its capacity. What may be
  # BUILT has no capacity to scale -- only a permission -- so it is a thin
  # black dot-dash laid over the same route: present on an expandable link
  # that already has stock, and alone on one that is only a proposal.
  mapping <- ggplot2::aes(linewidth = .data$value)
  if (!is.null(colour_aes)) mapping$colour <- ggplot2::aes(colour = .data$comm)$colour
  directed <- links$id[links$dir %in% c("ab", "ba")]
  std_id <- links$id[!links$kind %in% "new"]
  new_id <- links$id[links$kind %in% c("stock + new", "new")]
  line_args <- list(lineend = "round", alpha = 0.85)
  if (is.null(colour_aes)) line_args$colour <- "#2C7FB8"
  std <- paths[paths$id %in% std_id, , drop = FALSE]
  undir <- std[!std$id %in% directed, , drop = FALSE]
  dirp <- std[std$id %in% directed, , drop = FALSE]
  if (nrow(undir)) p <- p + do.call(path_layer, c(list(undir, mapping), line_args))
  if (nrow(dirp)) p <- p + do.call(path_layer, c(list(dirp, mapping, arrow = arr), line_args))
  nw <- paths[paths$id %in% new_id, , drop = FALSE]
  if (nrow(nw) && identical(value, "input")) {
    p <- p + path_layer(nw, ggplot2::aes(linetype = "new"),
                        colour = "grey10", linewidth = 0.35,
                        lineend = "butt")
  }

  pt <- if (node_size) ggplot2::aes(x = .data$x, y = .data$y, size = .data$total) else ggplot2::aes(x = .data$x, y = .data$y)
  p <- p + ggplot2::geom_point(data = nodes, mapping = pt, shape = 21, fill = "white", colour = "grey20", stroke = 0.5)
  if (isTRUE(labels)) {
    p <- p + ggplot2::geom_text(data = nodes, ggplot2::aes(x = .data$x, y = .data$y, label = .data$region),
                                vjust = -1.1, size = 2.8, colour = "grey15")
  }
  # What the picture measures is on the legends; the caption carries the year
  # and, for a flow, the slice it was read at. The title names the OBJECT.
  cap <- paste(c(if (!is.na(year)) paste("year:", year),
                 if (identical(value, "flow"))
                   paste("flow:", if (identical(toupper(timeslice), "ANNUAL"))
                     "annual" else timeslice)),
               collapse = " · ")
  if (!nzchar(cap)) cap <- NULL
  # `value` names the MODE ("input" is where the number came from, not what it
  # is). The width is a capacity in every mode but `flow`, so the legend says
  # that -- with the unit the trades declare, where they agree on one.
  wlab <- switch(value, flow = "flow", "capacity")
  if (!is.null(cap_unit)) wlab <- paste0(wlab, ", ", paste(cap_unit, collapse = " / "))
  p <- p + ggplot2::scale_linewidth_continuous(wlab, range = c(0.4, 3.2), trans = "sqrt") +
    ggplot2::scale_linetype_manual(NULL,
      values = c(new = "1636"), na.translate = FALSE) +
    ggplot2::scale_size_area(if (node_size) paste("connected", wlab) else NULL,
                             max_size = 5) +
    ggplot2::labs(title = ttl, subtitle = sub, caption = cap) +
    ggplot2::theme_void() +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 10), plot.title.position = "plot",
                   legend.position = "right", legend.box = "vertical")
  if (multi_comm) p <- p + ggplot2::labs(colour = "commodity")
  if (as_sf) p <- p + ggplot2::coord_sf(crs = crs) else if (is.null(shapes)) p <- p + ggplot2::coord_equal(clip = "off")
  p
}
