# levcost.R
# Levelized cost of energy (production) for energyRt technology, scenario, and
# model objects.
#
# For a technology object the function builds a minimal single-technology
# energyRt model with unit demand, solves it with GLPK (or a user-supplied
# solver), and returns a per-year LCOE table plus an NPV-weighted scalar,
# together with a structured cost-component breakdown.s
#
# Ported and adapted from IDEEA project (ideea_levcost.R).

# ── S4 generic ─────────────────────────────────────────────────────────────────

#' Levelized cost of commodity production
#'
#' Computes the levelized cost of energy (LCOE) for a \code{technology},
#' \code{repository}, \code{model}, or \code{scenario} object.
#'
#' \describe{
#'   \item{\code{technology}}{a minimal single-technology energyRt model is built
#'     around the technology, solved, and the LCOE derived from the resulting
#'     cost and production variables.}
#'   \item{\code{repository} / \code{model}}{give the technology \code{name}; the
#'     method selects the related commodity and supply objects from the container
#'     and prices the named technology as above. If an input commodity has no
#'     supply in the container, it returns \code{NULL} with a message unless
#'     \code{autocomplete = TRUE} (which adds a zero-cost / \code{fuel_costs}
#'     supply). The \code{model} method also takes the calendar, region, horizon
#'     and discount rate from the model's configuration (each overridable).}
#'   \item{\code{scenario}}{an \emph{ex-post} cost of the named process in the
#'     \emph{solved} scenario: the discounted sum of its own costs (annualised
#'     investment \code{vTechEac}, \code{vTechFixom}, \code{vTechVarom}, plus
#'     attributed fuel cost) divided by its discounted output. Fuel is attributed
#'     from \code{vTechInp}; a technology with a \emph{grouped} input (whose
#'     per-commodity consumption is not a solution variable) therefore reports no
#'     fuel component.}
#' }
#'
#' @param object  A \code{technology} (or list thereof), \code{repository},
#'   \code{model}, or solved \code{scenario} object.
#' @param comm    Character vector or \code{NULL}.  Output commodity(ies) to
#'   use for LCOE normalisation.  \code{NULL} uses all commodities in the
#'   resolved output group.
#' @param name    Character. For \code{technology}, a name tag; for
#'   \code{repository}/\code{model}/\code{scenario}, the name of the technology /
#'   process to price.
#' @param ...     Additional arguments passed to the underlying implementation.
#'   For \code{technology} objects the most useful are:
#'   \describe{
#'     \item{\code{group}}{Character or \code{NULL}.  Output group name.}
#'     \item{\code{repo}}{A \code{repository} or list of energyRt objects
#'       (supplies, commodities, weather) to supplement the mini-model.}
#'     \item{\code{fuel_costs}}{Named numeric vector \code{[commodity -> cost]}
#'       for input commodities not found in \code{repo}.}
#'     \item{\code{autocomplete}}{Logical, default \code{FALSE}
#'       (\code{repository}/\code{model} methods). When \code{TRUE}, input
#'       commodities without a supply in the container are auto-supplied
#'       (zero-cost, or priced via \code{fuel_costs}) instead of returning
#'       \code{NULL}.}
#'     \item{\code{discount}}{Numeric (0–1), default \code{0.05}.}
#'     \item{\code{base_year}}{Integer or \code{NULL}.}
#'     \item{\code{horizon}}{A \code{horizon} object, numeric year vector, or
#'       \code{NULL} (derives from \code{@olife}).}
#'     \item{\code{calendar}}{A \code{calendar} object or \code{NULL}.}
#'     \item{\code{timeframe}}{\code{"ANNUAL"} (default) or \code{"native"}.
#'       \code{"ANNUAL"} prices the technology on a single annual time-slice: any
#'       weather profile is collapsed to an annual capacity factor (applied as the
#'       technology's annual availability), so capacity is sized to serve unit
#'       annual demand at that factor (textbook LCOE). \code{"native"} keeps the
#'       supplied (sub-annual) calendar and normalises by total generation --
#'       useful when the technology is analysed together with storage or
#'       transmission, where sub-annual dispatch matters.}
#'     \item{\code{backstop}}{Logical, default \code{TRUE}. Enables a very
#'       expensive dummy-import slack on the output commodity balance so the
#'       mini-model always solves even when the technology cannot serve a slice on
#'       its own; the slack cost is excluded from the LCOE.}
#'     \item{\code{region}}{Character or \code{NULL}.}
#'     \item{\code{weather}}{A \code{weather} object, list of weather objects,
#'       or \code{NULL}.}
#'     \item{\code{frontier}}{Logical, default \code{FALSE}.  When \code{TRUE}
#'       additional solves are performed to map the production frontier for
#'       technologies with multi-commodity grouped output and share constraints.}
#'     \item{\code{solver}}{Solver spec list, default
#'       \code{solver_options$glpk}.}
#'     \item{\code{as_scenario}}{Logical, default \code{FALSE}.  When
#'       \code{TRUE} the full solved \code{scenario} is returned with LCOE
#'       tables attached to \code{scenario@@misc}.}
#'     \item{\code{verbose}}{Logical, default \code{TRUE}.}
#'   }
#'
#' @return
#'   For \code{technology} input: a list of class \code{"levcost"} with fields:
#'   \describe{
#'     \item{\code{$levcost}}{data.frame – total levelized cost by year.}
#'     \item{\code{$levcost_npv}}{Named numeric – NPV-weighted average LCOE.}
#'     \item{\code{$cost_breakdown}}{data.frame – tidy cost components by year.
#'       Components: \code{eac}, \code{fixom}, \code{varom}, \code{supply},
#'       \code{import}, \code{export} (negative, a credit).}
#'     \item{\code{$cost_breakdown_npv}}{data.frame – NPV-weighted component
#'       breakdown.}
#'     \item{\code{$cost_yearly}}{data.frame – wide undiscounted cost table with
#'       activity, capacity, per-commodity outputs and inputs.}
#'     \item{\code{$levcost_per_act}}{data.frame or \code{NULL}.}
#'     \item{\code{$frontier}}{data.frame or \code{NULL} (requires
#'       \code{frontier = TRUE} and multi-commodity grouped output).}
#'     \item{\code{$scenario}}{The solved \code{scenario} object.}
#'   }
#'   For a list of technology objects: a named list of class
#'   \code{"levcost_list"}.
#'
#' @export
#'
#' @include solve.R
#' @examples
#' \dontrun{
#' lc <- levcost(my_tech, discount = 0.07, base_year = 2025)
#' lc$levcost_npv
#' lc$cost_breakdown
#' autoplot(lc)
#' autoplot(lc, type = "npv")
#'
#' # List of technologies (each solved independently):
#' lc_list <- levcost(list(tech1, tech2), discount = 0.07)
#' autoplot(lc_list, type = "npv")
#' }
setGeneric("levcost", function(object, comm, name, ...) {
  standardGeneric("levcost")
})

setMethod("levcost", "technology", function(object, comm, name, ...) {
  comm_arg <- if (missing(comm)) NULL else comm
  levcost_technology_(object, comm = comm_arg, ...)
})

# ── levcost() for containers: repository & model ─────────────────────────────
# Find a named technology inside a repository/model/scenario.
.levcost_find_tech <- function(container, name) {
  techs <- tryCatch(getObjects(container, "technology"), error = function(e) list())
  if (!is.null(name) && name %in% names(techs)) techs[[name]] else NULL
}

# Commodities a technology consumes (main + grouped inputs).
.levcost_input_comms <- function(tech) {
  ic <- character(0)
  if (nrow(tech@input) > 0) ic <- unique(as.character(tech@input$comm))
  unique(ic[!is.na(ic) & nzchar(ic)])
}

# Commodities that have a supply object in the container.
.levcost_supplied_comms <- function(container) {
  sups <- tryCatch(getObjects(container, "supply"), error = function(e) list())
  unique(unlist(lapply(sups, function(s) as.character(s@commodity))))
}

# Regions a technology spans (from its region-bearing data slots), so the caller
# can price it in one of them.
.levcost_tech_regions <- function(tech) {
  regs <- character(0)
  for (sl in methods::slotNames(tech)) {
    x <- tryCatch(methods::slot(tech, sl), error = function(e) NULL)
    if (sl == "region" && is.character(x)) regs <- c(regs, x)
    else if (is.data.frame(x) && "region" %in% names(x)) regs <- c(regs, as.character(x$region))
  }
  unique(regs[!is.na(regs) & nzchar(regs)])
}

# The LCOE mini-model is single-region; a kit technology may carry data for
# several regions. Subset every region-bearing slot to `region` (keeping
# region-agnostic NA rows) and pin the @region slot, so the mini-model declares
# exactly the one region it prices.
.levcost_subset_tech_region <- function(tech, region) {
  region <- region[1]
  for (sl in methods::slotNames(tech)) {
    x <- tryCatch(methods::slot(tech, sl), error = function(e) NULL)
    if (sl == "region" && is.character(x) && length(x) > 0) {
      methods::slot(tech, sl) <- region
    } else if (is.data.frame(x) && "region" %in% names(x) && nrow(x) > 0) {
      keep <- is.na(x$region) | as.character(x$region) == region
      methods::slot(tech, sl) <- x[keep, , drop = FALSE]
    }
  }
  tech
}

# Annual (share-weighted) capacity factor of a weather object, by region & year.
# `slice_share` (from the native calendar) supplies the per-slice weights; when
# absent a plain mean is used. Returns a data.frame(region, year, cf).
.levcost_weather_annual_cf <- function(wobj, slice_share) {
  wd <- tryCatch(wobj@weather, error = function(e) NULL)
  if (is.null(wd) || nrow(wd) == 0 || !"wval" %in% names(wd))
    return(data.frame(region = character(), year = integer(), cf = numeric()))
  wcol <- NULL
  if (!is.null(slice_share) && "slice" %in% names(slice_share)) {
    wcol <- if ("share" %in% names(slice_share)) "share"
            else if ("weight" %in% names(slice_share)) "weight" else NULL
  }
  # NA-safe grouping key over region x year (weather year is often NA = all
  # years; `split()` on a factor would silently drop NA groups).
  agg_by <- function(df, wc) {
    yk  <- ifelse(is.na(df$year), "\rNA", as.character(df$year))
    key <- paste(as.character(df$region), yk, sep = "\r")
    do.call(rbind, lapply(split(df, key), function(d) {
      cf <- if (is.null(wc)) mean(d$wval) else sum(d$wval * d[[wc]]) / sum(d[[wc]])
      data.frame(region = d$region[1], year = d$year[1], cf = cf,
                 stringsAsFactors = FALSE)
    }))
  }
  if (!is.null(wcol)) {
    m <- merge(wd, as.data.frame(slice_share)[, c("slice", wcol)], by = "slice")
    if (nrow(m) > 0) return(agg_by(m, wcol))
  }
  agg_by(wd, NULL)
}

# Collapse a technology's weather dependence to an annual availability factor.
# Each @weather row (weather name + waf.lo/up/fx coefficient) is turned into the
# matching af.lo/up/fx = coefficient x annual-CF row on @af; the @weather slot is
# then cleared so the mini-model needs no weather object or sub-annual calendar.
.levcost_weatherize_annual <- function(tech, weather_objects, slice_share,
                                       verbose = TRUE) {
  wdf <- tech@weather
  if (nrow(wdf) == 0) return(tech)
  wmap <- list()
  for (w in weather_objects) if (isS4(w) && .hasSlot(w, "name")) wmap[[w@name]] <- w
  af_proto <- tech@af[0, , drop = FALSE]
  af_cols  <- names(af_proto)
  new_rows <- list()
  for (i in seq_len(nrow(wdf))) {
    wr    <- wdf[i, , drop = FALSE]
    wname <- as.character(wr$weather)
    wobj  <- wmap[[wname]]
    if (is.null(wobj)) {
      if (verbose) message("levcost(): no weather object '", wname,
                           "' for annual CF of '", tech@name, "'; skipping.")
      next
    }
    cf <- .levcost_weather_annual_cf(wobj, slice_share)
    if (nrow(cf) == 0) next
    bound <- NULL
    for (b in c("fx", "up", "lo")) {
      col <- paste0("waf.", b)
      if (col %in% names(wr) && !is.na(wr[[col]])) {
        bound <- b; coef <- as.numeric(wr[[col]]); break
      }
    }
    if (is.null(bound)) { bound <- "up"; coef <- 1 }
    r <- af_proto[rep(1L, nrow(cf)), , drop = FALSE]
    if (nrow(r) == 0) {                       # empty @af: build a fresh frame
      r <- data.frame(region = cf$region, year = cf$year, slice = NA_character_,
                      af.lo = NA_real_, af.up = NA_real_, af.fx = NA_real_,
                      rampup = NA_real_, rampdown = NA_real_,
                      stringsAsFactors = FALSE)
      af_cols <- names(r)
    } else {
      r$region <- cf$region; r$year <- cf$year
      if ("slice" %in% af_cols) r$slice <- NA_character_
    }
    afc <- paste0("af.", bound)
    if (afc %in% af_cols) r[[afc]] <- coef * cf$cf
    new_rows[[length(new_rows) + 1L]] <- r
  }
  if (length(new_rows) > 0) {
    add <- do.call(rbind, new_rows)
    tech@af <- if (nrow(tech@af) > 0) rbind(tech@af, add[, names(tech@af)]) else add
  }
  tech@weather <- tech@weather[0, , drop = FALSE]
  tech
}

# Subset a weather object's @region slot and @weather table to a single region.
.levcost_subset_weather_region <- function(w, region) {
  region <- region[1]
  if (.hasSlot(w, "region") && is.character(w@region) && length(w@region) > 0)
    w@region <- region
  if (.hasSlot(w, "weather") && is.data.frame(w@weather) &&
      "region" %in% names(w@weather) && nrow(w@weather) > 0) {
    keep <- is.na(w@weather$region) | as.character(w@weather$region) == region
    w@weather <- w@weather[keep, , drop = FALSE]
  }
  w
}

# Shared LCOE for a technology named `name` inside a repository/model. Selects
# the related commodity, supply and weather objects from the container; if an
# input commodity has no supply, returns NULL with a message unless
# `autocomplete = TRUE` (which lets the mini-model add a zero-cost /
# `fuel_costs` supply).
.levcost_container <- function(container, name, comm = NULL, autocomplete = FALSE,
                               fuel_costs = NULL, verbose = TRUE, ...) {
  if (is.null(name) || !nzchar(name)) {
    message("levcost(): please give the technology `name = ` to price.")
    return(invisible(NULL))
  }
  tech <- .levcost_find_tech(container, name)
  if (is.null(tech)) {
    message("levcost(): technology '", name, "' not found in the ",
            class(container)[1], ".")
    return(invisible(NULL))
  }
  in_comms <- .levcost_input_comms(tech)
  # a commodity is "covered" if the container supplies it, an IMPORT prices it
  # (rest-of-world fuel at its import price), or `fuel_costs` prices it
  imps <- tryCatch(getObjects(container, "import"), error = function(e) list())
  for (im in imps) {
    cm <- tryCatch(as.character(im@commodity)[1], error = function(e) NA_character_)
    if (is.na(cm) || cm %in% names(fuel_costs)) next
    pr <- tryCatch(suppressWarnings(as.numeric(im@imp$price)), error = function(e) NULL)
    pr <- pr[is.finite(pr)]
    if (length(pr) > 0) fuel_costs[cm] <- mean(pr)
  }
  covered  <- unique(c(.levcost_supplied_comms(container), names(fuel_costs)))
  missing  <- setdiff(in_comms, covered)
  if (length(missing) > 0 && !isTRUE(autocomplete)) {
    message("levcost(): technology '", name, "' consumes commodity(ies) with no ",
            "supply in the ", class(container)[1], ": ",
            paste(missing, collapse = ", "), ".\n",
            "  Set autocomplete = TRUE to add zero-cost supplies, or pass e.g. ",
            "fuel_costs = c(", missing[1], " = <price>).")
    return(invisible(NULL))
  }
  dots <- list(...)
  # the mini-model is single-region: pick one region the tech spans and subset
  # its data to it (a multi-region kit tech would otherwise reference regions the
  # mini-model has not declared, or mismatch demand rows)
  reg1 <- dots$region
  if (is.null(reg1)) {
    tr <- .levcost_tech_regions(tech)
    reg1 <- if (length(tr) > 0) tr[1] else NULL
  } else {
    reg1 <- reg1[1]
  }
  if (!is.null(reg1)) {
    tech <- .levcost_subset_tech_region(tech, reg1)
    dots$region <- reg1
  }
  # attach ONLY the weather object(s) this technology references (thermal techs
  # have an empty @weather slot and need none), region-subset to `reg1` so they
  # do not reference regions the single-region mini-model has not declared
  if (is.null(dots$weather)) {
    wneed <- tryCatch(unique(as.character(tech@weather$weather)), error = function(e) character(0))
    wneed <- wneed[!is.na(wneed) & nzchar(wneed)]
    if (length(wneed) > 0) {
      wall <- tryCatch(getObjects(container, "weather"), error = function(e) list())
      wsel <- wall[intersect(names(wall), wneed)]
      if (length(wsel) > 0) {
        if (!is.null(reg1)) wsel <- lapply(wsel, .levcost_subset_weather_region, region = reg1)
        dots$weather <- unname(wsel)
      }
    }
  }
  # the container's commodities + supplies become the levcost `repo`
  repo <- c(tryCatch(getObjects(container, "commodity"), error = function(e) list()),
            tryCatch(getObjects(container, "supply"),     error = function(e) list()))
  do.call(levcost_technology_, c(list(tech, comm = comm, repo = repo,
    fuel_costs = fuel_costs, verbose = verbose), dots))
}

#' @rdname levcost
setMethod("levcost", "repository", function(object, comm, name, ...) {
  comm_arg <- if (missing(comm)) NULL else comm
  name_arg <- if (missing(name)) NULL else name
  .levcost_container(object, name = name_arg, comm = comm_arg, ...)
})

# Extract a scalar discount rate from a model's config (wacc / sdr / legacy).
.levcost_model_discount <- function(cfg, default = 0.05) {
  d <- tryCatch(cfg@discount, error = function(e) NULL)
  if (is.null(d) || !is.data.frame(d) || nrow(d) == 0) return(default)
  for (col in c("sdr", "wacc", "discount")) {
    if (col %in% names(d)) {
      v <- suppressWarnings(as.numeric(d[[col]]))
      v <- v[is.finite(v) & v > 0]
      if (length(v) > 0) return(mean(v))
    }
  }
  default
}

#' @rdname levcost
setMethod("levcost", "model", function(object, comm, name, ...) {
  comm_arg <- if (missing(comm)) NULL else comm
  name_arg <- if (missing(name)) NULL else name
  cfg  <- object@config
  dots <- list(...)
  # take calendar / region / horizon / discount from the model unless overridden
  cal <- if (!is.null(dots$calendar)) dots$calendar else {
    x <- tryCatch(cfg@calendar, error = function(e) NULL)
    if (!is.null(x) && length(x@timeframe_rank) > 1) x else NULL
  }
  reg <- if (!is.null(dots$region)) dots$region else {
    x <- tryCatch(cfg@region, error = function(e) NULL)
    if (length(x) > 0) x else NULL      # all model regions (mini-model declares them)
  }
  hor <- if (!is.null(dots$horizon)) dots$horizon else {
    x <- tryCatch(cfg@horizon, error = function(e) NULL)
    if (!is.null(x) && nrow(x@intervals) > 0) x else NULL
  }
  disc <- if (!is.null(dots$discount)) dots$discount else .levcost_model_discount(cfg)
  dots$calendar <- dots$region <- dots$horizon <- dots$discount <- NULL
  do.call(.levcost_container, c(list(object, name = name_arg, comm = comm_arg,
    calendar = cal, region = reg, horizon = hor, discount = disc), dots))
})

#' @rdname levcost
setMethod("levcost", "scenario", function(object, comm, name, ...) {
  .levcost_scenario(object, name = if (missing(name)) NULL else name,
                    comm = if (missing(comm)) NULL else comm, ...)
})

# Fuel cost of a process in a solved scenario: sum over inputs of
# vTechInp[tech, comm] x mean supply price of that commodity, by year.
.levcost_scenario_fuel <- function(scen, name) {
  inp <- tryCatch(getData(scen, "vTechInp", tech = name, merge = TRUE),
                  error = function(e) NULL)
  if (is.null(inp) || !nrow(inp)) return(setNames(numeric(0), character(0)))
  sups <- tryCatch(getObjects(scen@model, "supply"), error = function(e) list())
  price <- list()
  for (s in sups) {
    cm <- as.character(s@commodity)
    av <- tryCatch(as.data.frame(s@availability), error = function(e) NULL)
    if (!is.null(av) && "cost" %in% names(av) && nrow(av) > 0)
      price[[cm]] <- mean(suppressWarnings(as.numeric(av$cost)), na.rm = TRUE)
  }
  pr <- unlist(price[as.character(inp$comm)]); pr[is.na(pr)] <- 0
  a <- aggregate(inp$value * pr, list(year = as.integer(inp$year)), sum, na.rm = TRUE)
  setNames(a$x, as.character(a$year))
}

# Ex-post levelized cost of a process in a SOLVED scenario: the discounted sum of
# the process's own costs (annualised investment `vTechEac`, `vTechFixom`,
# `vTechVarom`, plus attributed fuel cost) divided by its discounted output.
.levcost_scenario <- function(scen, name, comm = NULL, discount = NULL,
                              base_year = NULL, verbose = TRUE, ...) {
  if (is.null(name) || !nzchar(name)) {
    message("levcost(): please give the process `name = ` to price."); return(invisible(NULL))
  }
  if (!isTRUE(scen@status$interpolated) ||
      is.null(tryCatch(scen@modOut, error = function(e) NULL)) ||
      length(scen@modOut@variables) == 0) {
    message("levcost(): the scenario is not solved -- solve it first."); return(invisible(NULL))
  }
  out_all <- tryCatch(getData(scen, "vTechOut", tech = name, merge = TRUE),
                      error = function(e) NULL)
  if (is.null(out_all) || !nrow(out_all)) {
    message("levcost(): process '", name, "' has no output in the solved scenario."); return(invisible(NULL))
  }
  if (!is.null(comm)) out_all <- out_all[as.character(out_all$comm) %in% comm, , drop = FALSE]
  ms <- suppressWarnings(as.integer(scen@modInp@sets$year))
  ms <- ms[is.finite(ms)]; if (!length(ms)) ms <- sort(unique(as.integer(out_all$year)))
  if (is.null(base_year)) base_year <- min(ms)
  if (is.null(discount)) discount <- .levcost_model_discount(scen@settings)

  by_year <- function(df) {
    if (is.null(df) || !nrow(df)) return(setNames(numeric(0), character(0)))
    a <- aggregate(df$value, list(year = as.integer(df$year)), sum, na.rm = TRUE)
    setNames(a$x, as.character(a$year))
  }
  gy <- function(v) by_year(tryCatch(getData(scen, v, tech = name, merge = TRUE),
                                     error = function(e) NULL))
  eac <- gy("vTechEac"); fixom <- gy("vTechFixom"); varom <- gy("vTechVarom")
  fuel <- .levcost_scenario_fuel(scen, name)
  out  <- by_year(out_all)

  yrs <- sort(unique(as.integer(c(names(eac), names(fixom), names(varom),
                                  names(fuel), names(out)))))
  pick <- function(v, y) { x <- v[as.character(y)]; x[is.na(x)] <- 0; unname(x) }
  comp <- data.frame(
    year  = yrs,
    eac   = pick(eac, yrs),   fixom = pick(fixom, yrs),
    varom = pick(varom, yrs), fuel  = pick(fuel, yrs),
    output = pick(out, yrs), stringsAsFactors = FALSE)
  comp$total <- comp$eac + comp$fixom + comp$varom + comp$fuel
  disc <- (1 + discount)^(comp$year - base_year)
  npv_cost <- sum(comp$total / disc)
  npv_out  <- sum(comp$output / disc)
  lcoe_npv <- if (is.finite(npv_out) && npv_out > 0) npv_cost / npv_out else NA_real_

  comp$levcost <- ifelse(comp$output > 0, comp$total / comp$output, NA_real_)
  # component name "supply" matches the technology levcost / autoplot convention
  breakdown <- data.frame(
    year = rep(comp$year, 4),
    component = rep(c("eac", "fixom", "varom", "supply"), each = nrow(comp)),
    value = c(comp$eac, comp$fixom, comp$varom, comp$fuel), stringsAsFactors = FALSE)
  bd_npv <- aggregate(value ~ component,
    transform(breakdown, value = value / rep((1 + discount)^(comp$year - base_year), 4)),
    sum)
  bd_npv$value <- bd_npv$value / npv_out

  structure(list(
    levcost = data.frame(tech = name, comm = paste(unique(out_all$comm), collapse = "+"),
                         year = comp$year, levcost = comp$levcost, stringsAsFactors = FALSE),
    levcost_npv = setNames(lcoe_npv, name),
    cost_breakdown = breakdown, cost_breakdown_npv = bd_npv,
    cost_yearly = comp, units = list(costs = "MEUR", activity = "PJ"),
    scenario = scen), class = "levcost")
}

# ── tech_share_frontier ─────────────────────────────────────────────────────────
# Standalone helper: extract feasible share ranges for ALL grouped inputs and
# outputs of a technology.  No solve required – purely geometric.
#
# Returns a data.frame with columns:
#   tech, direction ("input"/"output"), group, comm, others,
#   share_lo, share_hi           – direct per-commodity constraints
#   share_lo_eff, share_hi_eff   – effective range after intersecting all group
#                                  member constraints (the true feasible band)
#
# Only groups where at least one commodity has an explicit share.up or
# share.lo > 0 are included (skip fully-unconstrained groups).

#' Extract feasible share ranges for grouped inputs/outputs
#'
#' Purely geometric helper — no model solve required.  Returns the feasible
#' share band for each commodity in each constrained input or output group of
#' the technology.
#'
#' @param object A \code{technology} S4 object.
#' @return A \code{data.frame} or \code{NULL} if no constrained groups found.
#' @export
tech_share_frontier <- function(object) {
  stopifnot(inherits(object, "technology"))
  tech_name <- if (nzchar(object@name)) object@name else "TECH"

  .extract_shares <- function(comms) {
    su <- setNames(rep(NA_real_, length(comms)), comms)
    sl <- setNames(rep(0,        length(comms)), comms)
    if (nrow(object@ceff) == 0) return(list(up = su, lo = sl))
    ci <- object@ceff[object@ceff$comm %in% comms, , drop = FALSE]
    if ("share.up" %in% names(ci))
      for (cm in comms) {
        r <- ci[ci$comm == cm & !is.na(ci$share.up), , drop = FALSE]
        if (nrow(r) > 0) su[[cm]] <- r$share.up[1]
      }
    if ("share.lo" %in% names(ci))
      for (cm in comms) {
        r <- ci[ci$comm == cm & !is.na(ci$share.lo), , drop = FALSE]
        if (nrow(r) > 0) sl[[cm]] <- r$share.lo[1]
      }
    if ("share.fx" %in% names(ci))
      for (cm in comms) {
        r <- ci[ci$comm == cm & !is.na(ci$share.fx), , drop = FALSE]
        if (nrow(r) > 0) { su[[cm]] <- r$share.fx[1]; sl[[cm]] <- r$share.fx[1] }
      }
    list(up = su, lo = sl)
  }

  .eff_range <- function(su, sl) {
    comms  <- names(su)
    hi     <- ifelse(is.finite(su), su, 1)
    lo     <- sl
    lo_eff <- setNames(numeric(length(comms)), comms)
    hi_eff <- setNames(numeric(length(comms)), comms)
    for (cm in comms) {
      others_hi_sum <- sum(hi[names(hi) != cm])
      others_lo_sum <- sum(lo[names(lo) != cm])
      raw_lo <- if (is.finite(sl[[cm]])) sl[[cm]] else 0
      raw_hi <- if (is.finite(su[[cm]])) su[[cm]] else 1
      lo_eff[[cm]] <- max(raw_lo, 1 - others_hi_sum)
      hi_eff[[cm]] <- min(raw_hi, 1 - others_lo_sum)
    }
    list(lo_eff = lo_eff, hi_eff = hi_eff)
  }

  all_rows <- list()

  # Input groups
  in_df     <- object@input
  in_groups <- character(0)
  if ("group" %in% names(in_df))
    in_groups <- unique(in_df$group[!is.na(in_df$group) & nzchar(as.character(in_df$group))])

  for (grp in in_groups) {
    gc  <- in_df$comm[!is.na(in_df$group) & as.character(in_df$group) == grp]
    sh  <- .extract_shares(gc)
    if (!any(is.finite(sh$up[gc]) | (sh$lo[gc] > 0))) next
    eff <- .eff_range(sh$up, sh$lo)
    for (cm in gc) {
      all_rows[[paste("in", grp, cm, sep = "_")]] <- data.frame(
        tech         = tech_name, direction  = "input",  group = grp, comm = cm,
        others       = paste(setdiff(gc, cm), collapse = "+"),
        n_in_group   = length(gc),
        share_lo     = if (is.finite(sh$lo[[cm]])) sh$lo[[cm]] else 0,
        share_hi     = if (is.finite(sh$up[[cm]])) sh$up[[cm]] else 1,
        share_lo_eff = max(0, eff$lo_eff[[cm]]),
        share_hi_eff = min(1, eff$hi_eff[[cm]]),
        stringsAsFactors = FALSE
      )
    }
  }

  # Output groups
  out_df     <- object@output
  out_groups <- character(0)
  if ("group" %in% names(out_df))
    out_groups <- unique(out_df$group[!is.na(out_df$group) & nzchar(as.character(out_df$group))])

  for (grp in out_groups) {
    gc  <- out_df$comm[!is.na(out_df$group) & as.character(out_df$group) == grp]
    if (length(gc) < 2) next
    sh  <- .extract_shares(gc)
    if (!any(is.finite(sh$up[gc]) | (sh$lo[gc] > 0))) next
    eff <- .eff_range(sh$up, sh$lo)
    for (cm in gc) {
      all_rows[[paste("out", grp, cm, sep = "_")]] <- data.frame(
        tech         = tech_name, direction  = "output", group = grp, comm = cm,
        others       = paste(setdiff(gc, cm), collapse = "+"),
        n_in_group   = length(gc),
        share_lo     = if (is.finite(sh$lo[[cm]])) sh$lo[[cm]] else 0,
        share_hi     = if (is.finite(sh$up[[cm]])) sh$up[[cm]] else 1,
        share_lo_eff = max(0, eff$lo_eff[[cm]]),
        share_hi_eff = min(1, eff$hi_eff[[cm]]),
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(all_rows) == 0L) return(NULL)
  df <- do.call(rbind, all_rows)
  rownames(df) <- NULL
  df
}

# ── levcost_chain_ ──────────────────────────────────────────────────────────────
# Private implementation for chain LCOE: builds ONE model from an ordered list
# of technology objects, connecting intermediate commodities automatically.

levcost_chain_ <- function(
    object,
    comm        = NULL,
    repo        = NULL,
    fuel_costs  = NULL,
    discount    = 0.05,
    base_year   = NULL,
    horizon     = NULL,
    calendar    = NULL,
    region      = NULL,
    solver      = solver_options$glpk,
    verbose     = TRUE,
    ...
) {
  if (length(object) == 0) stop("`object` is an empty list.")
  if (!all(sapply(object, inherits, "technology")))
    stop("All elements of `object` must be energyRt 'technology' objects.")

  tech_names <- sapply(object, function(t) {
    nm <- t@name
    if (length(nm) == 1 && nzchar(nm)) nm else NA_character_
  })
  chain_name <- paste(tech_names, collapse = "+")

  # ── 1. Classify commodities ────────────────────────────────────────────────
  all_inputs  <- unique(unlist(lapply(object, function(t) t@input$comm)))
  all_outputs <- unique(unlist(lapply(object, function(t) t@output$comm)))

  intermediate  <- intersect(all_inputs, all_outputs)
  external_inps <- setdiff(all_inputs,  all_outputs)   # need supply
  terminal_outs <- setdiff(all_outputs, all_inputs)    # need demand

  if (verbose) {
    message("Chain: ", chain_name)
    if (length(intermediate)  > 0) message("  Intermediate:    ", paste(intermediate,  collapse = ", "))
    if (length(external_inps) > 0) message("  External inputs: ", paste(external_inps, collapse = ", "))
    if (length(terminal_outs) > 0) message("  Terminal outputs: ", paste(terminal_outs, collapse = ", "))
  }

  # ── 2. Resolve terminal output (normaliser) ───────────────────────────────
  if (!is.null(comm)) {
    if (!comm %in% terminal_outs)
      stop("Commodity '", comm, "' is not a terminal output of the chain.\n",
           "Terminal outputs: ", paste(terminal_outs, collapse = ", "))
    norm_comm <- comm
  } else if (length(terminal_outs) == 1) {
    norm_comm <- terminal_outs
  } else if (length(terminal_outs) == 0) {
    stop("No terminal output found (all outputs are consumed within the chain). ",
         "Check that `object` is ordered correctly.")
  } else {
    stop("Multiple terminal outputs: ", paste(terminal_outs, collapse = ", "),
         ".\nSpecify one via `comm = `.")
  }

  # ── 3. Resolve region ─────────────────────────────────────────────────────
  if (is.null(region)) {
    reg_cands <- unique(unlist(lapply(object, function(t) {
      r <- unique(na.omit(t@region))
      r[nzchar(as.character(r))]
    })))
    region <- if (length(reg_cands) > 0) reg_cands[1] else "REGION"
    if (verbose) {
      if (length(reg_cands) == 0)  message("No region found; using 'REGION'.")
      else if (length(reg_cands) > 1) message("Multiple regions; using '", region, "'.")
    }
  }

  # ── 4. Resolve horizon ────────────────────────────────────────────────────
  if (inherits(horizon, "horizon")) {
    hor       <- horizon
    hor_years <- sort(hor@period)
  } else if (is.numeric(horizon) || is.integer(horizon)) {
    hor_years <- sort(as.integer(horizon))
    hor       <- newHorizon(period = hor_years)
  } else {
    olife_vals <- sapply(object, function(t) {
      if (nrow(t@olife) == 0) return(NA_real_)
      ol_col <- intersect(c("olife", "value"), names(t@olife))
      if (length(ol_col) == 0) return(NA_real_)
      max(t@olife[[ol_col[1]]], na.rm = TRUE)
    })
    olife_val <- if (any(is.finite(olife_vals))) max(olife_vals, na.rm = TRUE) else 20
    if (!is.finite(olife_val) || olife_val <= 0) {
      olife_val <- 20
      if (verbose) message("No olife found; using default horizon of 20 years.")
    }
    start_vals <- sapply(object, function(t) {
      if (nrow(t@start) == 0) return(NA_integer_)
      st_col <- intersect(c("start", "value"), names(t@start))
      if (length(st_col) == 0) return(NA_integer_)
      as.integer(min(t@start[[st_col[1]]], na.rm = TRUE))
    })
    by <- if (!is.null(base_year)) {
      as.integer(base_year)
    } else if (any(is.finite(start_vals))) {
      min(start_vals, na.rm = TRUE)
    } else {
      as.integer(format(Sys.Date(), "%Y"))
    }
    hor_years <- seq(by, by + as.integer(olife_val) - 1L)
    hor       <- newHorizon(period = hor_years)
    if (verbose)
      message("Auto-created horizon: ", min(hor_years), "\u2013", max(hor_years),
              " (", length(hor_years), " years) from maximum technology olife.")
  }
  if (is.null(base_year)) base_year <- min(hor_years)

  # ── 5. Calendar ───────────────────────────────────────────────────────────
  if (is.null(calendar)) calendar <- newCalendar()

  # ── 6. Pull supplies / commodities from repo ──────────────────────────────
  repo_supplies <- list()
  repo_comms    <- list()
  if (!is.null(repo)) {
    objs <- if (inherits(repo, "repository")) repo@data else
      if (is.list(repo)) repo else
        stop("`repo` must be a 'repository' object or a named list.")
    for (obj in objs) {
      if (inherits(obj, "supply"))    repo_supplies[[if (nzchar(obj@commodity)) obj@commodity else obj@name]] <- obj
      if (inherits(obj, "commodity")) repo_comms[[obj@name]] <- obj
    }
  }

  # ── 7. Build commodity objects ────────────────────────────────────────────
  all_comms <- unique(c(all_inputs, all_outputs))
  commodity_objects <- list()
  for (cm in all_comms) {
    if (!is.null(repo_comms[[cm]])) {
      cm_obj <- repo_comms[[cm]]
      if (isS4(cm_obj) && .hasSlot(cm_obj, "emis") && nrow(cm_obj@emis) > 0)
        cm_obj@emis <- cm_obj@emis[0L, , drop = FALSE]
      commodity_objects[[cm]] <- cm_obj
    } else {
      unit_val <- ""
      for (t in object) {
        in_row  <- t@input [t@input$comm  == cm, , drop = FALSE]
        out_row <- t@output[t@output$comm == cm, , drop = FALSE]
        if (nrow(in_row)  > 0 && "unit" %in% names(in_row)  && !is.na(in_row$unit[1])  && nzchar(in_row$unit[1]))  unit_val <- in_row$unit[1]
        if (nrow(out_row) > 0 && "unit" %in% names(out_row) && !is.na(out_row$unit[1]) && nzchar(out_row$unit[1])) unit_val <- out_row$unit[1]
      }
      commodity_objects[[cm]] <- newCommodity(name = cm, timeframe = "ANNUAL",
                                              unit = unit_val)
      if (verbose) message("Created commodity '", cm, "'.")
    }
  }

  # ── 8. Build supply objects for external inputs ───────────────────────────
  supply_objects <- list()
  for (cm in external_inps) {
    if (!is.null(repo_supplies[[cm]])) {
      sup <- repo_supplies[[cm]]
      if (isS4(sup)) {
        if (.hasSlot(sup, "region") && length(sup@region) > 0) sup@region <- region
        if (.hasSlot(sup, "availability") && nrow(sup@availability) > 0 &&
            "region" %in% names(sup@availability))
          sup@availability$region <- region
      }
      supply_objects[[cm]] <- sup
    } else {
      fc_val <- if (!is.null(fuel_costs) && !is.null(fuel_costs[[cm]]) &&
                    is.finite(as.numeric(fuel_costs[[cm]])))
        as.numeric(fuel_costs[[cm]]) else 0
      supply_objects[[cm]] <- newSupply(
        name         = paste0("SUP_", cm),
        commodity    = cm,
        region       = region,
        availability = data.frame(region = region, year = as.integer(base_year),
                                  cost = fc_val, stringsAsFactors = FALSE)
      )
      if (verbose) message("Created supply for '", cm, "' (cost = ", fc_val, ").")
    }
  }

  # ── 9. Build demands / sinks for terminal outputs ─────────────────────────
  # norm_comm → unit demand (normaliser); other terminal outputs → zero-cost supply sink
  demand_objects <- list()
  for (cm in terminal_outs) {
    if (cm == norm_comm) {
      out_unit <- ""
      for (t in object) {
        row <- t@output[t@output$comm == cm, , drop = FALSE]
        if (nrow(row) > 0 && "unit" %in% names(row) && !is.na(row$unit[1]) && nzchar(row$unit[1]))
          out_unit <- row$unit[1]
      }
      demand_objects[[cm]] <- newDemand(
        name      = paste0("DEM_", cm),
        commodity = cm,
        unit      = out_unit,
        region    = region,
        dem       = data.frame(region = region, year = as.integer(base_year),
                               dem = 1, stringsAsFactors = FALSE)
      )
      if (verbose) message("Created unit demand for '", cm, "'.")
    } else {
      # by-product: zero-cost supply sink keeps model feasible
      supply_objects[[cm]] <- newSupply(
        name         = paste0("SUP_", cm),
        commodity    = cm,
        region       = region,
        availability = data.frame(region = region, year = as.integer(base_year),
                                  cost = 0, stringsAsFactors = FALSE)
      )
      if (verbose) message("Created supply sink for by-product '", cm, "'.")
    }
  }

  # ── 10. Strip capacity constraints ────────────────────────────────────────
  cap_cols <- c("stock", "cap.lo", "cap.up", "cap.fx",
                "ncap.lo", "ncap.up", "ncap.fx", "ret.lo", "ret.up", "ret.fx")
  object <- lapply(object, function(t) {
    if (nrow(t@capacity) > 0)
      for (col in intersect(cap_cols, names(t@capacity)))
        t@capacity[[col]] <- NA_real_
    t
  })

  # ── 11. Build and solve model ─────────────────────────────────────────────
  chain_id <- paste(tech_names, collapse = "_")
  sn  <- paste0("lc_chain_", chain_id)
  mdl <- newModel(
    name     = paste0("levcost_chain_", chain_id),
    desc     = paste0("Chain LCOE model: ", chain_name),
    data     = newRepository("repo_lc_chain",
                 c(unname(commodity_objects), unname(object),
                   unname(supply_objects), unname(demand_objects))),
    region   = region,
    discount = discount,
    calendar = calendar,
    horizon  = hor
  )
  # New mapping pipeline: interpolate in memory (unfolded, so the writers see
  # explicit rows) then solve. `solve_scen()` writes, runs and reads the solution
  # in one call (replacing the legacy write_sc / solve_scenario / read_solution).
  scen <- interpolate_model(mdl, name = sn, ondisk = FALSE, fold = FALSE, ...)
  scen <- solve_scen(scen, solver = solver)

  # ── 12. Extract results ───────────────────────────────────────────────────
  sfget <- function(v) tryCatch({
    d <- getData(scen, name = v, merge = TRUE, drop.zeros = FALSE)
    if (is.null(d) || nrow(d) == 0) return(NULL)
    as.data.frame(d)
  }, error = function(e) NULL)

  agg_yr <- function(df, filter_tech = NULL) {
    if (is.null(df) || nrow(df) == 0 || !"year" %in% names(df)) return(NULL)
    df$year <- as.integer(df$year)
    if (!is.null(filter_tech) && "tech" %in% names(df))
      df <- df[df$tech == filter_tech, , drop = FALSE]
    if (nrow(df) == 0) return(NULL)
    out <- aggregate(df[["value"]], by = list(year = df$year), FUN = sum, na.rm = TRUE)
    names(out)[2] <- "value"
    out
  }

  tc <- sfget("vTotalCost")
  if (is.null(tc) || nrow(tc) == 0) {
    warning("Chain LCOE solve returned no cost data.")
    return(NULL)
  }
  tc$year <- as.integer(tc$year)
  tc_agg  <- aggregate(value ~ year, data.frame(year = tc$year, value = tc$value),
                       sum, na.rm = TRUE)

  # LCOE table (cost per unit terminal output, which is 1 per year)
  lc_tbl <- data.frame(tech = chain_name, comm = norm_comm, region = region,
                        year = tc_agg$year, levcost = tc_agg$value,
                        stringsAsFactors = FALSE)
  rownames(lc_tbl) <- NULL

  # NPV LCOE
  te_npv  <- tc_agg$year - as.integer(base_year)
  dsc_tc  <- (1 + discount)^te_npv
  npv_num <- sum(ifelse(is.finite(dsc_tc) & dsc_tc > 0, tc_agg$value / dsc_tc, 0))
  npv_den <- sum(ifelse(is.finite(dsc_tc) & dsc_tc > 0, 1 / dsc_tc, 0))
  levcost_npv <- if (is.finite(npv_den) && npv_den > 0) npv_num / npv_den else NA_real_

  # Per-tech cost breakdown
  cost_breakdown <- do.call(rbind, c(
    lapply(tech_names, function(tn) {
      cmp <- list(eac   = agg_yr(sfget("vTechEac"),   tn),
                  fixom = agg_yr(sfget("vTechFixom"), tn),
                  varom = agg_yr(sfget("vTechVarom"), tn))
      do.call(rbind, Filter(Negate(is.null), lapply(names(cmp), function(nm) {
        d <- cmp[[nm]]
        if (is.null(d) || nrow(d) == 0) return(NULL)
        data.frame(tech = tn, chain = chain_name, comm = norm_comm, region = region,
                   year = as.integer(d$year), component = nm, value = d$value,
                   stringsAsFactors = FALSE)
      })))
    }),
    list(local({
      sup <- agg_yr(sfget("vSupCost"))
      if (is.null(sup)) return(NULL)
      data.frame(tech = "supply", chain = chain_name, comm = norm_comm, region = region,
                 year = as.integer(sup$year), component = "supply", value = sup$value,
                 stringsAsFactors = FALSE)
    }))
  ))
  if (!is.null(cost_breakdown)) rownames(cost_breakdown) <- NULL

  # Per-tech NPV share
  chain_breakdown <- do.call(rbind, c(
    lapply(tech_names, function(tn) {
      npv_t <- 0
      for (pn in c("vTechEac", "vTechFixom", "vTechVarom")) {
        d <- agg_yr(sfget(pn), tn)
        if (!is.null(d) && nrow(d) > 0) {
          dsc_d <- (1 + discount)^(as.integer(d$year) - as.integer(base_year))
          npv_t <- npv_t + sum(ifelse(is.finite(dsc_d) & dsc_d > 0, d$value / dsc_d, 0))
        }
      }
      data.frame(tech = tn, npv_cost = npv_t, share = NA_real_, stringsAsFactors = FALSE)
    }),
    list(local({
      sup <- agg_yr(sfget("vSupCost"))
      if (is.null(sup)) return(NULL)
      dsc_s <- (1 + discount)^(as.integer(sup$year) - as.integer(base_year))
      npv_s <- sum(ifelse(is.finite(dsc_s) & dsc_s > 0, sup$value / dsc_s, 0))
      data.frame(tech = "supply", npv_cost = npv_s, share = NA_real_, stringsAsFactors = FALSE)
    }))
  ))
  if (!is.null(chain_breakdown)) {
    rownames(chain_breakdown) <- NULL
    tot <- sum(chain_breakdown$npv_cost, na.rm = TRUE)
    if (is.finite(tot) && tot > 0)
      chain_breakdown$share <- chain_breakdown$npv_cost / tot
  }

  # ── 13. Return ────────────────────────────────────────────────────────────
  result <- list(
    levcost         = lc_tbl,
    levcost_npv     = setNames(levcost_npv, chain_name),
    cost_breakdown  = cost_breakdown,
    chain_breakdown = chain_breakdown,
    discount        = discount,
    base_year       = as.integer(base_year),
    tech_names      = tech_names,
    norm_comm       = norm_comm,
    scenario        = scen
  )
  class(result) <- c("levcost_chain", "levcost", "list")
  result
}

# ── levcost_technology_ ─────────────────────────────────────────────────────────
# Private implementation called by setMethod("levcost", "technology", ...).

levcost_technology_ <- function(
    object,
    comm           = NULL,
    group          = NULL,
    repo           = NULL,
    fuel_costs     = NULL,
    discount       = 0.05,
    base_year      = NULL,
    horizon        = NULL,
    calendar       = NULL,
    timeframe      = c("ANNUAL", "native"),
    region         = NULL,
    weather        = NULL,
    frontier       = FALSE,
    backstop       = TRUE,
    solver         = solver_options$glpk,
    as_scenario    = FALSE,
    verbose        = TRUE,
    ...
) {

  # ── 0. Handle list of technologies → chain LCOE ────────────────────────────
  if (is.list(object) && !inherits(object, "technology")) {
    if (length(object) == 0) stop("`object` is an empty list.")
    if (!all(sapply(object, inherits, "technology")))
      stop("All elements of `object` must be energyRt 'technology' objects.")

    return(levcost_chain_(
      object,
      comm       = comm,
      repo       = repo,
      fuel_costs = fuel_costs,
      discount   = discount,
      base_year  = base_year,
      horizon    = horizon,
      calendar   = calendar,
      region     = region,
      solver     = solver,
      verbose    = verbose,
      ...
    ))
  }

  # ── 1. Validate input ───────────────────────────────────────────────────────
  if (!inherits(object, "technology"))
    stop("`object` must be an energyRt 'technology' object or a list thereof.")
  tech_name <- if (nzchar(object@name)) object@name else "TECH"
  timeframe  <- match.arg(timeframe)
  # native slice shares of the supplied calendar, used to weight weather into an
  # annual capacity factor when `timeframe = "ANNUAL"` (captured before the
  # calendar is overridden to annual below).
  native_slice_share <- if (!is.null(calendar) && .hasSlot(calendar, "slice_share"))
    calendar@slice_share else NULL

  # ── 1b. Strip capacity constraints (distort unit-demand mini-model) ─────────
  if (nrow(object@capacity) > 0) {
    cap_cols <- c("stock", "cap.lo", "cap.up", "cap.fx",
                  "ncap.lo", "ncap.up", "ncap.fx",
                  "ret.lo",  "ret.up",  "ret.fx")
    for (col in intersect(cap_cols, names(object@capacity)))
      object@capacity[[col]] <- NA_real_
    if (verbose) message("Capacity constraints stripped for LCOE mini-model.")
  }

  # ── 2. Resolve region ───────────────────────────────────────────────────────
  if (is.null(region)) {
    reg_candidates <- unique(na.omit(object@region))
    if (length(reg_candidates) == 0 && nrow(object@capacity) > 0 &&
        "region" %in% names(object@capacity))
      reg_candidates <- unique(na.omit(object@capacity$region))
    reg_candidates <- reg_candidates[nzchar(as.character(reg_candidates)) &
                                       !is.na(reg_candidates)]
    if (length(reg_candidates) == 0) {
      region <- "REGION"
      if (verbose) message("No region found in technology; using 'REGION'.")
    } else {
      region <- reg_candidates[1]
      if (length(reg_candidates) > 1 && verbose)
        message("Multiple regions in technology; using first: '", region, "'.")
    }
  } else {
    region <- region[1]
  }
  # The mini-model is single-region; subset a multi-region technology's data to
  # the resolved region so it does not reference undeclared regions.
  if (length(.levcost_tech_regions(object)) > 1)
    object <- .levcost_subset_tech_region(object, region)

  # ── 3. Resolve output group / commodities ───────────────────────────────────
  out_df       <- object@output
  if (nrow(out_df) == 0) stop("Technology '", tech_name, "' has no output commodities.")
  avail_groups <- unique(out_df$group)
  avail_groups <- avail_groups[!is.na(avail_groups) & nzchar(as.character(avail_groups))]

  if (!is.null(group)) {
    if (!(group %in% avail_groups))
      stop("Output group '", group, "' not found in technology '", tech_name, "'.\n",
           "Available groups: ", paste(avail_groups, collapse = ", "))
    out_df_grp <- out_df[out_df$group == group, , drop = FALSE]
  } else if (length(avail_groups) > 1) {
    stop("Technology '", tech_name, "' has multiple output groups: ",
         paste(avail_groups, collapse = ", "),
         ".\nSpecify one via the `group = ` parameter.")
  } else if (length(avail_groups) == 1) {
    group      <- avail_groups
    out_df_grp <- out_df[!is.na(out_df$group) & out_df$group == avail_groups, , drop = FALSE]
    if (nrow(out_df_grp) == 0) out_df_grp <- out_df
  } else {
    out_df_grp <- out_df
  }

  if (!is.null(comm)) {
    miss_comm <- setdiff(comm, out_df_grp$comm)
    if (length(miss_comm) > 0)
      stop("Commodity/ies not found in resolved output: ", paste(miss_comm, collapse = ", "),
           ".\nAvailable: ", paste(out_df_grp$comm, collapse = ", "))
    out_comms <- comm
  } else {
    out_comms <- out_df_grp$comm
  }
  all_out_comms      <- out_df_grp$comm
  has_grouped_output <- !is.null(group)

  # ── 3.5. Extract output share constraints ───────────────────────────────────
  share_up <- setNames(rep(NA_real_, length(out_comms)), out_comms)
  share_lo <- setNames(rep(0,        length(out_comms)), out_comms)
  if (has_grouped_output && nrow(object@ceff) > 0) {
    ceff_out <- object@ceff[object@ceff$comm %in% out_comms, , drop = FALSE]
    for (slot_name in c("share.up", "share.lo", "share.fx")) {
      if (!slot_name %in% names(ceff_out)) next
      for (cm in out_comms) {
        rows <- ceff_out[ceff_out$comm == cm & !is.na(ceff_out[[slot_name]]), , drop = FALSE]
        if (nrow(rows) == 0) next
        if (slot_name == "share.up") share_up[[cm]] <- rows[[slot_name]][1]
        if (slot_name == "share.lo") share_lo[[cm]] <- rows[[slot_name]][1]
        if (slot_name == "share.fx") { share_up[[cm]] <- rows[[slot_name]][1]
                                       share_lo[[cm]] <- rows[[slot_name]][1] }
      }
    }
  }
  do_frontier <- frontier &&
    has_grouped_output &&
    length(out_comms) >= 2 &&
    any(is.finite(share_up))

  # ── 4. Collect input commodities + input group share constraints ─────────────
  in_comms <- unique(object@input$comm)
  in_df    <- object@input
  in_groups <- if ("group" %in% names(in_df))
    unique(in_df$group[!is.na(in_df$group) & nzchar(as.character(in_df$group))])
  else character(0)

  in_group_comms <- list()
  in_share_up    <- list()
  in_share_lo    <- list()
  for (.grp in in_groups) {
    .gc <- in_df$comm[!is.na(in_df$group) & as.character(in_df$group) == .grp]
    in_group_comms[[.grp]] <- .gc
    .su <- setNames(rep(NA_real_, length(.gc)), .gc)
    .sl <- setNames(rep(0,        length(.gc)), .gc)
    if (nrow(object@ceff) > 0) {
      .ci <- object@ceff[object@ceff$comm %in% .gc, , drop = FALSE]
      for (.sn in c("share.up", "share.lo", "share.fx")) {
        if (!.sn %in% names(.ci)) next
        for (.cm in .gc) {
          .r <- .ci[.ci$comm == .cm & !is.na(.ci[[.sn]]), , drop = FALSE]
          if (nrow(.r) == 0) next
          if (.sn == "share.up") .su[[.cm]] <- .r[[.sn]][1]
          if (.sn == "share.lo") .sl[[.cm]] <- .r[[.sn]][1]
          if (.sn == "share.fx") { .su[[.cm]] <- .r[[.sn]][1]; .sl[[.cm]] <- .r[[.sn]][1] }
        }
      }
    }
    in_share_up[[.grp]] <- .su
    in_share_lo[[.grp]] <- .sl
  }
  rm(list = c(".grp", ".gc", ".su", ".sl", ".sn", ".cm", ".r"))

  # ── 4.5. Auxiliary commodities (from @aux / @aeff) ───────────────────────────
  # Collect all auxiliary commodity names declared in @aux$acomm so that they
  # can be registered as commodity objects and — when consumed as inputs — also
  # get zero-cost supply objects in the mini-model.
  aux_comms <- character(0)
  if (.hasSlot(object, "aux") && nrow(object@aux) > 0 &&
      "acomm" %in% names(object@aux))
    aux_comms <- unique(na.omit(as.character(object@aux$acomm)))

  # Which aux commodities are consumed as inputs?  Look for non-NA values in
  # any of the input-type @aeff columns (cinp2ainp, act2ainp, cap2ainp, etc.).
  inp_aux_cols  <- c("cinp2ainp", "cout2ainp", "act2ainp", "cap2ainp", "ncap2ainp",
                     "sinp2ainp", "sout2ainp", "stg2ainp")
  aux_inp_comms <- character(0)
  if (.hasSlot(object, "aeff") && nrow(object@aeff) > 0 && length(aux_comms) > 0) {
    aeff_inp_cols <- intersect(inp_aux_cols, names(object@aeff))
    if (length(aeff_inp_cols) > 0) {
      for (.ac in aux_comms) {
        .rows <- object@aeff[!is.na(object@aeff$acomm) & object@aeff$acomm == .ac,
                              aeff_inp_cols, drop = FALSE]
        if (any(!is.na(.rows))) aux_inp_comms <- c(aux_inp_comms, .ac)
      }
    }
  }

  # ── 5. Resolve horizon ──────────────────────────────────────────────────────
  if (inherits(horizon, "horizon")) {
    hor       <- horizon
    hor_years <- sort(hor@period)
  } else if (is.numeric(horizon) || is.integer(horizon)) {
    hor_years <- sort(as.integer(horizon))
    hor       <- newHorizon(period = hor_years)
  } else {
    olife_val <- NULL
    if (nrow(object@olife) > 0) {
      ol_col <- intersect(c("olife", "value"), names(object@olife))
      if (length(ol_col) > 0)
        olife_val <- max(object@olife[[ol_col[1]]], na.rm = TRUE)
    }
    if (is.null(olife_val) || !is.finite(olife_val) || olife_val <= 0) {
      olife_val <- 20
      if (verbose) message("No olife found; using default horizon of 20 years.")
    }
    by        <- if (!is.null(base_year)) as.integer(base_year) else
      as.integer(format(Sys.Date(), "%Y"))
    hor_years <- seq(by, by + as.integer(olife_val) - 1L)
    hor       <- newHorizon(period = hor_years)
    if (verbose)
      message("Auto-created horizon: ", min(hor_years), "\u2013", max(hor_years),
              " (", length(hor_years), " years) from technology olife.")
  }
  if (is.null(base_year)) base_year <- min(hor_years)

  # ── 6. Calendar ─────────────────────────────────────────────────────────────
  if (is.null(calendar)) calendar <- newCalendar()

  # ── 7. Pull supplies / commodities from repo ─────────────────────────────────
  repo_supplies <- list()
  repo_comms    <- list()
  if (!is.null(repo)) {
    objs <- if (inherits(repo, "repository")) repo@data else
      if (is.list(repo)) repo else
        stop("`repo` must be a 'repository' object or a named list.")
    for (obj in objs) {
      if (inherits(obj, "supply")) {
        key <- if (nzchar(obj@commodity)) obj@commodity else obj@name
        repo_supplies[[key]] <- obj
      }
      if (inherits(obj, "commodity")) repo_comms[[obj@name]] <- obj
    }
  }

  # ── 7b. Weather validation ───────────────────────────────────────────────────
  tech_has_weather <- nrow(object@weather) > 0 &&
    any(sapply(object@weather, function(col) any(nzchar(as.character(col)))))
  supply_has_weather <- any(sapply(repo_supplies, function(s) {
    nrow(s@weather) > 0 &&
      any(sapply(s@weather, function(col) any(nzchar(as.character(col)))))
  }))
  if ((tech_has_weather || supply_has_weather) && is.null(weather)) {
    who <- character(0)
    if (tech_has_weather) who <- c(who, paste0("technology '", tech_name, "'"))
    if (supply_has_weather) {
      wnames <- names(Filter(function(s) {
        nrow(s@weather) > 0 &&
          any(sapply(s@weather, function(col) any(nzchar(as.character(col)))))
      }, repo_supplies))
      who <- c(who, paste0("supply '", wnames, "'"))
    }
    stop("The following objects have a non-empty @weather slot and require a ",
         "weather object:\n  ", paste(who, collapse = "\n  "),
         "\nPlease supply one via `weather = `.")
  }
  weather_objects <- list()
  if (!is.null(weather)) {
    if (inherits(weather, "weather"))           weather_objects <- list(weather)
    else if (is.list(weather))                  weather_objects <- weather
    else stop("`weather` must be a 'weather' object or a list of 'weather' objects.")
  }

  # ── 7b. Annual timeframe (default) ──────────────────────────────────────────
  # `levcost` measures the cost of dispatchable generation, so by default it
  # prices the technology on an ANNUAL timeframe: any weather profile is
  # collapsed to an annual capacity factor (share-weighted mean, applied as the
  # technology's annual availability), the weather objects are dropped, and a
  # single-slice annual calendar is used. This sizes capacity to serve unit
  # annual demand at the technology's capacity factor (textbook LCOE) and avoids
  # the sub-annual must-run overbuild artefact. `timeframe = "native"` keeps the
  # supplied (sub-annual) calendar and normalises by total generation — useful
  # when the technology is studied together with storage or transmission.
  if (identical(timeframe, "ANNUAL")) {
    if (nrow(object@weather) > 0) {
      wobjs <- lapply(weather_objects, .levcost_subset_weather_region,
                      region = region)
      object <- .levcost_weatherize_annual(object, wobjs,
                                           native_slice_share, verbose = verbose)
    }
    weather_objects <- list()
    calendar <- newCalendar()          # annual, single ANNUAL slice
  }

  # ── 8. Auto-create commodity objects ─────────────────────────────────────────
  # Include aux_comms so auxiliary commodities referenced in @aeff are declared
  # in the mini-model commodity set even when not in @input / @output.
  all_comms_needed <- unique(c(in_comms, all_out_comms, aux_comms))
  commodity_objects <- list()
  for (cm in all_comms_needed) {
    if (!is.null(repo_comms[[cm]])) {
      cm_obj <- repo_comms[[cm]]
      if (isS4(cm_obj) && .hasSlot(cm_obj, "emis") && nrow(cm_obj@emis) > 0)
        cm_obj@emis <- cm_obj@emis[0L, , drop = FALSE]
      # On the annual timeframe every commodity must resolve to the single ANNUAL
      # slice; a repo commodity carrying a sub-annual timeframe would mismatch the
      # calendar (mCommSlice).
      if (identical(timeframe, "ANNUAL") && .hasSlot(cm_obj, "timeframe"))
        cm_obj@timeframe <- "ANNUAL"
      commodity_objects[[cm]] <- cm_obj
    } else {
      unit_val <- ""
      in_row   <- object@input [object@input$comm  == cm, , drop = FALSE]
      out_row  <- object@output[object@output$comm == cm, , drop = FALSE]
      if (nrow(in_row)  > 0 && "unit" %in% names(in_row))  unit_val <- in_row$unit[1]
      if (nrow(out_row) > 0 && "unit" %in% names(out_row)) unit_val <- out_row$unit[1]
      commodity_objects[[cm]] <- newCommodity(
        name      = cm,
        timeframe = "ANNUAL",
        unit      = if (!is.na(unit_val)) unit_val else ""
      )
      if (verbose) message("Created commodity '", cm, "'.")
    }
  }

  # ── 9. Auto-create supply objects ────────────────────────────────────────────
  supply_objects <- list()
  for (cm in in_comms) {
    if (!is.null(repo_supplies[[cm]])) {
      sup <- repo_supplies[[cm]]
      if (isS4(sup)) {
        if (.hasSlot(sup, "region") && length(sup@region) > 0)
          sup@region <- region
        if (.hasSlot(sup, "availability") &&
            nrow(sup@availability) > 0 &&
            "region" %in% names(sup@availability))
          sup@availability$region <- region
      }
      supply_objects[[cm]] <- sup
    } else {
      fc_val <- if (!is.null(fuel_costs) && !is.null(fuel_costs[[cm]]) &&
                    is.finite(as.numeric(fuel_costs[[cm]])))
        as.numeric(fuel_costs[[cm]]) else 0
      supply_objects[[cm]] <- newSupply(
        name         = paste0("SUP_", cm),
        commodity    = cm,
        region       = region,
        availability = data.frame(
          region = region, year = as.integer(base_year),
          cost   = fc_val, stringsAsFactors = FALSE
        )
      )
      if (verbose) message("Created supply for '", cm, "' (cost = ", fc_val, ").")
    }
  }

  # ── 9.5. Zero-cost supply for input-type auxiliary commodities ─────────────
  # Auxiliary commodities consumed as inputs (cinp2ainp, act2ainp, etc.) need a
  # supply object so the mini-model balances; reuse from repo when possible.
  for (.cm in aux_inp_comms) {
    if (!is.null(supply_objects[[.cm]])) next   # already handled in step 9
    if (!is.null(repo_supplies[[.cm]])) {
      sup <- repo_supplies[[.cm]]
      if (isS4(sup)) {
        if (.hasSlot(sup, "region") && length(sup@region) > 0)
          sup@region <- region
        if (.hasSlot(sup, "availability") &&
            nrow(sup@availability) > 0 &&
            "region" %in% names(sup@availability))
          sup@availability$region <- region
      }
      supply_objects[[.cm]] <- sup
    } else {
      supply_objects[[.cm]] <- newSupply(
        name         = paste0("SUP_", .cm),
        commodity    = .cm,
        region       = region,
        availability = data.frame(
          region = region, year = as.integer(base_year),
          cost   = 0, stringsAsFactors = FALSE
        )
      )
      if (verbose) message("Created zero-cost aux supply for '", .cm, "'.")
    }
  }

  # ── 9.9. Backstop slack (dummy import) ─────────────────────────────────────
  # A unit-demand mini-model can be infeasible when the technology cannot serve
  # every slice on its own (e.g. solar at night on a sub-annual calendar). Enable
  # the model's built-in dummy-import slack (via config `@debug`) at a very high
  # cost for each output commodity, so the balance always closes; the technology
  # is still dispatched to its physical limit (the slack is far dearer than any
  # real cost), and the slack's cost is excluded from the LCOE, which is
  # normalised by the technology's own output.
  BACKSTOP_PRICE  <- 1e6
  backstop_debug  <- NULL
  if (isTRUE(backstop) && length(all_out_comms) > 0) {
    backstop_debug <- data.frame(
      comm = all_out_comms, region = NA_character_, year = NA_integer_,
      slice = NA_character_, dummyImport = BACKSTOP_PRICE, dummyExport = Inf,
      stringsAsFactors = FALSE)
  }

  # ── 10. Inner helpers ────────────────────────────────────────────────────────
  make_demands_ <- function(dvals) {
    lapply(setNames(names(dvals), names(dvals)), function(cm) {
      dv       <- max(dvals[[cm]], 1e-9)
      unit_val <- ""
      out_row  <- object@output[object@output$comm == cm, , drop = FALSE]
      if (nrow(out_row) > 0 && "unit" %in% names(out_row)) unit_val <- out_row$unit[1]
      newDemand(
        name      = paste0("DEM_", cm),
        commodity = cm,
        unit      = if (!is.na(unit_val)) unit_val else "",
        region    = region,
        dem       = data.frame(region = region, year = as.integer(base_year),
                               dem = dv, stringsAsFactors = FALSE)
      )
    })
  }

  build_and_solve_ <- function(demand_objs, suffix = "") {
    mdl <- newModel(
      name     = paste0("levcost_", tech_name, suffix),
      desc     = paste0("Mini model for levelized cost of '", tech_name, "'"),
      data     = newRepository("repo_lc",
                   c(unname(commodity_objects), list(object),
                     unname(supply_objects), unname(demand_objs),
                     weather_objects)),
      region   = region,
      discount = discount,
      calendar = calendar,
      horizon  = hor
    )
    if (!is.null(backstop_debug)) mdl@config@debug <- backstop_debug
    sn   <- paste0("lc_", tech_name, suffix)
    # New mapping pipeline (see levcost_chain_): interpolate in memory, unfolded,
    # then write + run + read via solve_scen() in one call.
    scen <- interpolate_model(mdl, name = sn, ondisk = FALSE, fold = FALSE, ...)
    solve_scen(scen, solver = solver)
  }

  # ── 11. Inner closure: extract LCOE from a solved scenario ──────────────────
  comm_label <- paste(out_comms, collapse = "+")
  group_val  <- if (!is.null(group)) group else NA_character_

  levcost_extract_ <- function(sc, dem_vals, primary_comm = NULL) {
    sfget <- function(v) tryCatch({
      d <- getData(sc, name = v, merge = TRUE, drop.zeros = FALSE)
      if (is.null(d) || nrow(d) == 0) return(NULL)
      as.data.frame(d)
    }, error = function(e) NULL)

    agg_yr <- function(df) {
      if (is.null(df) || nrow(df) == 0 || !"year" %in% names(df)) return(NULL)
      df$year <- as.integer(df$year)
      out <- aggregate(df[["value"]], by = list(year = df$year), FUN = sum, na.rm = TRUE)
      names(out)[2] <- "value"; out
    }

    use_act_norm <- is.null(primary_comm) && has_grouped_output
    comm_lbl <- if (!is.null(primary_comm)) primary_comm else
                  if (use_act_norm) "activity" else comm_label

    tc <- sfget("vTotalCost")
    if (is.null(tc) || nrow(tc) == 0) {
      lc_tbl <- data.frame(tech = tech_name, group = group_val, comm = comm_lbl,
                            region = region, year = NA_integer_, levcost = NA_real_,
                            stringsAsFactors = FALSE)
      return(list(levcost = lc_tbl, levcost_npv = NA_real_,
                  cost_breakdown = NULL, cost_breakdown_npv = NULL,
                  cost_yearly = NULL, levcost_per_act = NULL, scen = sc))
    }

    # Own system cost = total minus the backstop dummy-import cost. The slack only
    # covers slices the technology physically cannot serve; its (deliberately
    # huge) cost is not the technology's and must not enter the LCOE.
    tc_df  <- agg_yr(as.data.frame(tc))            # year, value
    imp_yr <- agg_yr(sfget("vDummyImportCost"))
    if (!is.null(imp_yr) && nrow(imp_yr) > 0) {
      mm <- merge(tc_df, imp_yr, by = "year", all.x = TRUE, suffixes = c("", "_imp"))
      mm$value_imp[is.na(mm$value_imp)] <- 0
      tc_df <- data.frame(year = as.integer(mm$year),
                          value = mm$value - mm$value_imp, stringsAsFactors = FALSE)
    }
    tc_df$year <- as.integer(tc_df$year)

    # Resolve normaliser: the technology's OWN output, so the LCOE is cost per
    # unit the technology actually produces (not per unit of demand, part of
    # which the backstop may serve).
    norm_yr  <- NULL
    prim_dem <- 1
    if (use_act_norm) {
      act_ag <- agg_yr(sfget("vTechAct"))
      if (!is.null(act_ag) && nrow(act_ag) > 0)
        norm_yr <- setNames(act_ag$value, as.character(act_ag$year))
    } else if (!is.null(primary_comm)) {
      prim_dem <- max(dem_vals[[primary_comm]], 1e-9)
    } else {
      out_d <- sfget("vTechOut")
      if (!is.null(out_d) && "comm" %in% names(out_d))
        out_d <- out_d[out_d$comm %in% out_comms, , drop = FALSE]
      out_ag <- agg_yr(out_d)
      if (!is.null(out_ag) && nrow(out_ag) > 0 &&
          all(is.finite(out_ag$value)) && all(out_ag$value > 0))
        norm_yr <- setNames(out_ag$value, as.character(out_ag$year))
      else
        prim_dem <- max(sum(unlist(dem_vals)), 1e-9)
    }

    normalise_ <- function(yr_int, val) {
      if (!is.null(norm_yr)) {
        nrm <- norm_yr[as.character(yr_int)]
        ifelse(is.finite(nrm) & nrm > 0, val / nrm, NA_real_)
      } else {
        val / prim_dem
      }
    }

    lc_tbl <- tc_df
    names(lc_tbl)[names(lc_tbl) == "value"] <- "levcost"
    lc_tbl$levcost <- normalise_(lc_tbl$year, lc_tbl$levcost)
    lc_tbl$tech  <- tech_name; lc_tbl$group <- group_val; lc_tbl$comm <- comm_lbl
    lc_tbl$region <- region
    cf <- c("tech", "group", "comm", "region", "year", "levcost")
    lc_tbl <- lc_tbl[, c(cf, setdiff(names(lc_tbl), cf)), drop = FALSE]

    # NPV LCOE
    tc_agg  <- aggregate(value ~ year,
                         data.frame(year = as.integer(tc_df$year), value = tc_df$value),
                         sum, na.rm = TRUE)
    yr_int  <- tc_agg$year
    te_npv  <- yr_int - as.integer(base_year)
    dsc_tc  <- (1 + discount)^te_npv
    npv_num <- sum(ifelse(is.finite(dsc_tc) & dsc_tc > 0, tc_agg$value / dsc_tc, 0))

    npv_act_for_act <- NULL
    if (!is.null(norm_yr)) {
      qty_yr  <- norm_yr[as.character(yr_int)]
      npv_den <- sum(ifelse(is.finite(dsc_tc) & dsc_tc > 0 & is.finite(qty_yr),
                            qty_yr / dsc_tc, 0))
      npv_act_for_act <- npv_den
    } else if (!is.null(primary_comm)) {
      cact2cout_val <- NA_real_
      if (nrow(object@ceff) > 0 && "cact2cout" %in% names(object@ceff)) {
        r <- object@ceff$cact2cout[object@ceff$comm == primary_comm &
                                   !is.na(object@ceff$cact2cout)]
        if (length(r) > 0) cact2cout_val <- as.numeric(r[1])
      }
      act_fr <- agg_yr(sfget("vTechAct"))
      if (!is.null(act_fr) && nrow(act_fr) > 0 &&
          is.finite(cact2cout_val) && cact2cout_val > 0) {
        dsc_fr  <- (1 + discount)^(act_fr$year - as.integer(base_year))
        npv_act <- sum(ifelse(is.finite(dsc_fr) & dsc_fr > 0 & is.finite(act_fr$value),
                              act_fr$value / dsc_fr, 0))
        npv_den <- npv_act * cact2cout_val
        npv_act_for_act <- npv_act
      } else {
        npv_den <- prim_dem * sum(ifelse(is.finite(dsc_tc) & dsc_tc > 0, 1 / dsc_tc, 0))
      }
    } else {
      npv_den <- prim_dem * sum(ifelse(is.finite(dsc_tc) & dsc_tc > 0, 1 / dsc_tc, 0))
    }
    lc_npv <- if (is.finite(npv_den) && npv_den > 0) npv_num / npv_den else NA_real_

    # Cost components
    param_var_cost_ <- function(pname, vname) {
      p <- sfget(pname); v <- sfget(vname)
      if (is.null(p) || is.null(v) || nrow(p) == 0 || nrow(v) == 0) return(NULL)
      jc <- intersect(c("year", "region", "tech", "slice"), intersect(names(p), names(v)))
      m  <- merge(p[, c(jc, "value"), drop = FALSE],
                  v[, c(jc, "value"), drop = FALSE],
                  by = jc, suffixes = c("_p", "_v"))
      if (nrow(m) == 0) return(NULL)
      agg_yr(data.frame(year = m$year, value = m$value_p * m$value_v,
                        stringsAsFactors = FALSE))
    }

    fixom_val <- agg_yr(sfget("vTechFixom"))
    varom_val <- agg_yr(sfget("vTechVarom"))
    if (is.null(fixom_val)) fixom_val <- param_var_cost_("pTechFixom", "vTechCap")
    if (is.null(varom_val)) varom_val <- param_var_cost_("pTechVarom", "vTechAct")

    cmp_raw <- list(eac = agg_yr(sfget("vTechEac")),
                    fixom = fixom_val,
                    varom = varom_val,
                    supply = agg_yr(sfget("vSupCost")))
    # NB: vDummyImportCost is the backstop slack and is deliberately excluded — it
    # is not part of the technology's own levelized cost.
    ex <- sfget("vExportRowCost")
    if (!is.null(ex)) {
      ea <- agg_yr(ex)
      if (!is.null(ea)) { ea$value <- -ea$value; cmp_raw[["export"]] <- ea }
    }

    cbd <- do.call(rbind, Filter(Negate(is.null), lapply(names(cmp_raw), function(nm) {
      d <- cmp_raw[[nm]]; if (is.null(d)) return(NULL)
      data.frame(tech = tech_name, group = group_val, comm = comm_lbl,
                 region = region, year = as.integer(d$year),
                 component = nm, value = normalise_(as.integer(d$year), d$value),
                 stringsAsFactors = FALSE)
    })))
    if (!is.null(cbd)) rownames(cbd) <- NULL

    # Wide yearly table
    cost_yearly <- data.frame(tech = tech_name, group = group_val,
                              region = region, year = as.integer(tc_df$year),
                              total = tc_df$value, stringsAsFactors = FALSE)
    for (cnm in c("eac", "fixom", "varom", "supply", "import", "export")) {
      d <- cmp_raw[[cnm]]
      if (!is.null(d) && nrow(d) > 0) {
        dd <- data.frame(year = as.integer(d$year), v__ = d$value, stringsAsFactors = FALSE)
        names(dd)[2] <- cnm
        cost_yearly <- merge(cost_yearly, dd, by = "year", all.x = TRUE)
      }
    }
    act_raw <- agg_yr(sfget("vTechAct"))
    if (!is.null(act_raw) && nrow(act_raw) > 0) {
      cost_yearly <- merge(cost_yearly,
        data.frame(year = as.integer(act_raw$year), activity = act_raw$value,
                   stringsAsFactors = FALSE), by = "year", all.x = TRUE)
    }
    cap_raw <- agg_yr(sfget("vTechCap"))
    if (!is.null(cap_raw) && nrow(cap_raw) > 0) {
      cost_yearly <- merge(cost_yearly,
        data.frame(year = as.integer(cap_raw$year), capacity = cap_raw$value,
                   stringsAsFactors = FALSE), by = "year", all.x = TRUE)
    }
    out_raw <- sfget("vTechOut")
    if (!is.null(out_raw) && nrow(out_raw) > 0) {
      out_raw$year <- as.integer(out_raw$year)
      for (cm in unique(out_raw$comm)) {
        oa <- aggregate(value ~ year, out_raw[out_raw$comm == cm, ], sum, na.rm = TRUE)
        names(oa)[2] <- paste0("out_", cm)
        cost_yearly <- merge(cost_yearly, oa, by = "year", all.x = TRUE)
      }
    }
    inp_raw <- sfget("vTechInp")
    if (!is.null(inp_raw) && nrow(inp_raw) > 0) {
      inp_raw$year <- as.integer(inp_raw$year)
      for (cm in unique(inp_raw$comm)) {
        ia <- aggregate(value ~ year, inp_raw[inp_raw$comm == cm, ], sum, na.rm = TRUE)
        names(ia)[2] <- paste0("inp_", cm)
        cost_yearly <- merge(cost_yearly, ia, by = "year", all.x = TRUE)
      }
    }
    cy_front <- c("tech", "group", "region", "year")
    cost_yearly <- cost_yearly[, c(cy_front, setdiff(names(cost_yearly), cy_front)),
                               drop = FALSE]
    rownames(cost_yearly) <- NULL

    # NPV component breakdown
    cbd_npv <- NULL
    if (is.finite(lc_npv) && is.finite(npv_den) && npv_den > 0) {
      npv_parts <- Filter(Negate(is.null), lapply(names(cmp_raw), function(nm) {
        d <- cmp_raw[[nm]]; if (is.null(d) || nrow(d) == 0) return(NULL)
        d$te_c  <- as.integer(d$year) - as.integer(base_year)
        d$dsc_c <- (1 + discount)^d$te_c
        vld_c   <- is.finite(d$value) & is.finite(d$dsc_c) & d$dsc_c > 0
        pv      <- if (any(vld_c)) sum(d$value[vld_c] / d$dsc_c[vld_c]) else NA_real_
        data.frame(tech = tech_name, group = group_val, comm = comm_lbl,
                   component = nm, value = pv / npv_den, stringsAsFactors = FALSE)
      }))
      if (length(npv_parts) > 0) {
        cbd_npv <- do.call(rbind, npv_parts)
        rownames(cbd_npv) <- NULL
      }
    }

    # Per-activity NPV breakdown (for frontier scenarios)
    cbd_npv_per_act <- NULL
    if (!is.null(npv_act_for_act) && is.finite(npv_act_for_act) && npv_act_for_act > 0) {
      if (isTRUE(all.equal(npv_act_for_act, npv_den, tolerance = 1e-9))) {
        cbd_npv_per_act <- cbd_npv
      } else {
        pa_parts <- Filter(Negate(is.null), lapply(names(cmp_raw), function(nm) {
          d <- cmp_raw[[nm]]; if (is.null(d) || nrow(d) == 0) return(NULL)
          d$te_c  <- as.integer(d$year) - as.integer(base_year)
          d$dsc_c <- (1 + discount)^d$te_c
          vld_c   <- is.finite(d$value) & is.finite(d$dsc_c) & d$dsc_c > 0
          pv      <- if (any(vld_c)) sum(d$value[vld_c] / d$dsc_c[vld_c]) else NA_real_
          data.frame(tech = tech_name, group = group_val, comm = comm_lbl,
                     component = nm, value = pv / npv_act_for_act,
                     stringsAsFactors = FALSE)
        }))
        if (length(pa_parts) > 0) {
          cbd_npv_per_act <- do.call(rbind, pa_parts)
          rownames(cbd_npv_per_act) <- NULL
        }
      }
    }

    # Per-activity LCOE (grouped-output only)
    lc_act <- NULL
    if (has_grouped_output) {
      act_ag2 <- if (!is.null(norm_yr))
        data.frame(year = as.integer(names(norm_yr)), value = unname(norm_yr),
                   stringsAsFactors = FALSE)
      else agg_yr(sfget("vTechAct"))
      if (!is.null(act_ag2) && nrow(act_ag2) > 0) {
        mrg <- merge(tc_df[, c("year", "value"), drop = FALSE], act_ag2,
                     by = "year", suffixes = c("_cost", "_act"))
        mrg$lca <- ifelse(is.finite(mrg$value_act) & mrg$value_act != 0,
                          mrg$value_cost / mrg$value_act, NA_real_)
        lc_act <- data.frame(
          tech = tech_name, group = group_val, comm = comm_lbl, region = region,
          year = as.integer(mrg$year), activity = mrg$value_act,
          cost = mrg$value_cost, levcost_per_act = mrg$lca,
          stringsAsFactors = FALSE
        )
      }
    }

    list(levcost = lc_tbl, levcost_npv = lc_npv,
         cost_breakdown = cbd, cost_breakdown_npv = cbd_npv,
         cost_breakdown_npv_per_act = cbd_npv_per_act,
         cost_yearly = cost_yearly,
         levcost_per_act = lc_act, scen = sc)
  }

  # ── 12. Base solve ───────────────────────────────────────────────────────────
  # set_default_solver(solver)
  base_model_dvals <- setNames(rep(1, length(all_out_comms)), all_out_comms)
  base_lcoe_dvals  <- base_model_dvals[out_comms]
  demand_objects   <- make_demands_(base_model_dvals)
  if (verbose) for (cm in all_out_comms) message("Created unit demand for '", cm, "'.")
  scen <- build_and_solve_(demand_objects)
  if (!isTRUE(scen@status$optimal))
    warning("Mini model for '", tech_name, "' did not solve to optimality. ",
            "LCOE results may be unreliable.")
  base_res           <- levcost_extract_(scen, base_lcoe_dvals)
  levcost_tbl        <- base_res$levcost
  levcost_npv        <- base_res$levcost_npv
  cost_breakdown     <- base_res$cost_breakdown
  cost_breakdown_npv <- base_res$cost_breakdown_npv
  cost_yearly        <- base_res$cost_yearly
  levcost_per_act    <- base_res$levcost_per_act

  # ── 12.5. Frontier solves: one per output commodity corner ──────────────────
  frontier_raw <- list()
  if (do_frontier) {
    for (prim in out_comms) {
      si      <- if (is.finite(share_up[[prim]])) share_up[[prim]] else 1 / length(out_comms)
      n_other <- length(out_comms) - 1
      dv_oth  <- if (n_other > 0) (1 - si) / n_other else 0
      dem_fr  <- setNames(rep(max(dv_oth, 1e-6), length(out_comms)), out_comms)
      dem_fr[[prim]] <- si
      dobj_fr  <- make_demands_(dem_fr)
      scen_fr  <- build_and_solve_(dobj_fr, suffix = paste0("_fr_", prim))
      if (!isTRUE(scen_fr@status$optimal))
        warning("Frontier scenario max_", prim, " for '", tech_name,
                "' did not solve to optimality.")
      fr_res <- levcost_extract_(scen_fr, dem_fr, primary_comm = prim)
      frontier_raw[[paste0("max_", prim)]] <- c(fr_res,
        list(primary_comm = prim, primary_input = NA_character_, dem_vals = dem_fr))
    }
  }

  # ── 12.7. Input frontier solves ──────────────────────────────────────────────
  if (do_frontier && length(in_groups) > 0) {
    inp_frontier_groups <- Filter(function(g) length(in_group_comms[[g]]) >= 2, in_groups)
    if (length(inp_frontier_groups) > 0) {
      original_ceff <- object@ceff
      for (prim_out in out_comms) {
        si_out  <- if (is.finite(share_up[[prim_out]])) share_up[[prim_out]] else
          1 / length(out_comms)
        n_oth   <- length(out_comms) - 1
        dv_oth  <- if (n_oth > 0) (1 - si_out) / n_oth else 0
        dem_ifr <- setNames(rep(max(dv_oth, 1e-6), length(out_comms)), out_comms)
        dem_ifr[[prim_out]] <- si_out
        dobj_ifr <- make_demands_(dem_ifr)

        for (grp in inp_frontier_groups) {
          grp_comms <- in_group_comms[[grp]]
          already_fixed <- character(0)
          if ("share.fx" %in% names(original_ceff)) {
            for (cm in grp_comms) {
              rf <- original_ceff[original_ceff$comm == cm &
                !is.na(original_ceff$share.fx), , drop = FALSE]
              if (nrow(rf) > 0) already_fixed <- c(already_fixed, cm)
            }
          }
          vary_comms <- setdiff(grp_comms, already_fixed)
          if (length(vary_comms) < 2) next

          for (prim_inp in vary_comms) {
            object@ceff <- original_ceff
            inp_su  <- in_share_up[[grp]][[prim_inp]]
            fx_val  <- if (is.finite(inp_su)) inp_su else 1.0
            other_comms <- setdiff(grp_comms, prim_inp)
            other_fx    <- if (length(other_comms) > 0) (1 - fx_val) / length(other_comms) else 0
            if (!"share.fx" %in% names(object@ceff)) object@ceff$share.fx <- NA_real_
            for (cm in grp_comms) {
              rows <- which(object@ceff$comm == cm)
              if (length(rows) > 0)
                object@ceff$share.fx[rows] <- if (cm == prim_inp) fx_val else other_fx
            }
            suffix   <- paste0("_fr_", prim_out, "_", prim_inp)
            scen_ifr <- build_and_solve_(dobj_ifr, suffix = suffix)
            if (!isTRUE(scen_ifr@status$optimal))
              warning("Input frontier scenario max_", prim_out, "_max_", prim_inp,
                      " for '", tech_name, "' did not solve to optimality.")
            ifr_res <- levcost_extract_(scen_ifr, dem_ifr, primary_comm = prim_out)
            frontier_raw[[paste0("max_", prim_out, "_max_", prim_inp)]] <-
              c(ifr_res, list(primary_comm = prim_out,
                              primary_input = prim_inp,
                              dem_vals = dem_ifr))
            if (verbose) message("Input frontier: max_", prim_out, "_max_", prim_inp)
          }
        }
      }
      object@ceff <- original_ceff
    }
  }

  # ── 13. Frontier result assembly ─────────────────────────────────────────────
  frontier_df        <- NULL
  levcost_by_comm    <- NULL
  levcost_by_act     <- NULL
  levcost_by_act_cbd <- NULL

  if (length(frontier_raw) > 0) {
    bc_rows <- lapply(names(frontier_raw), function(sc_nm) {
      fr     <- frontier_raw[[sc_nm]]
      npv_df <- fr$cost_breakdown_npv
      if (is.null(npv_df)) return(NULL)
      npv_df$scenario      <- sc_nm
      npv_df$comm          <- fr$primary_comm
      npv_df$primary_input <- if (!is.null(fr$primary_input)) fr$primary_input else NA_character_
      npv_df
    })
    levcost_by_comm <- do.call(rbind, Filter(Negate(is.null), bc_rows))
    if (!is.null(levcost_by_comm)) rownames(levcost_by_comm) <- NULL

    act_rows <- lapply(names(frontier_raw), function(sc_nm) {
      fr     <- frontier_raw[[sc_nm]]
      lca_df <- fr$levcost_per_act
      if (is.null(lca_df) || nrow(lca_df) == 0) return(NULL)
      lca_df$te  <- lca_df$year - as.integer(base_year)
      lca_df$dsc <- (1 + discount)^lca_df$te
      vld_cost <- is.finite(lca_df$cost)     & is.finite(lca_df$dsc) & lca_df$dsc > 0
      vld_act  <- is.finite(lca_df$activity) & is.finite(lca_df$dsc) & lca_df$dsc > 0
      npv_cost <- if (any(vld_cost)) sum(lca_df$cost[vld_cost]     / lca_df$dsc[vld_cost]) else NA_real_
      npv_act  <- if (any(vld_act))  sum(lca_df$activity[vld_act]  / lca_df$dsc[vld_act])  else NA_real_
      npv_lca  <- if (is.finite(npv_act) && npv_act > 0) npv_cost / npv_act else NA_real_
      data.frame(
        tech = lca_df$tech[1], group = lca_df$group[1],
        scenario = sc_nm, primary_comm  = fr$primary_comm,
        primary_input = if (!is.null(fr$primary_input)) fr$primary_input else NA_character_,
        npv_act = npv_lca, stringsAsFactors = FALSE
      )
    })
    levcost_by_act <- do.call(rbind, Filter(Negate(is.null), act_rows))
    if (!is.null(levcost_by_act)) rownames(levcost_by_act) <- NULL

    act_cbd_rows <- lapply(names(frontier_raw), function(sc_nm) {
      fr     <- frontier_raw[[sc_nm]]
      cbd_pa <- fr$cost_breakdown_npv_per_act
      if (is.null(cbd_pa) || nrow(cbd_pa) == 0) return(NULL)
      cbd_pa$scenario      <- sc_nm
      cbd_pa$primary_comm  <- fr$primary_comm
      cbd_pa$primary_input <- if (!is.null(fr$primary_input)) fr$primary_input else NA_character_
      cbd_pa
    })
    levcost_by_act_cbd <- do.call(rbind, Filter(Negate(is.null), act_cbd_rows))
    if (!is.null(levcost_by_act_cbd)) rownames(levcost_by_act_cbd) <- NULL

    # 2D production frontier table (only for 2-output-commodity grouped techs)
    if (length(out_comms) == 2) {
      c1 <- out_comms[1]; c2 <- out_comms[2]
      sfget_fr <- function(sc, v) tryCatch({
        d <- getData(sc, name = v, merge = TRUE, drop.zeros = FALSE)
        if (is.null(d) || nrow(d) == 0) return(NULL); as.data.frame(d)
      }, error = function(e) NULL)

      out_only_names <- names(frontier_raw)[vapply(frontier_raw, function(fr)
        is.null(fr$primary_input) || is.na(fr$primary_input), logical(1))]
      fr_pts <- do.call(rbind, lapply(out_only_names, function(sc_nm) {
        fr   <- frontier_raw[[sc_nm]]
        sc   <- fr$scen
        tout <- sfget_fr(sc, "vTechOut")
        if (is.null(tout)) return(NULL)
        tout$year <- as.integer(tout$year)
        agg <- aggregate(value ~ comm + year, tout, sum, na.rm = TRUE)
        tc_f <- sfget_fr(sc, "vTotalCost")
        if (is.null(tc_f)) return(NULL)
        tc_f$year <- as.integer(tc_f$year)
        do.call(rbind, lapply(unique(agg$year), function(yr) {
          p1 <- agg$value[agg$comm == c1 & agg$year == yr]
          p2 <- agg$value[agg$comm == c2 & agg$year == yr]
          tc <- tc_f$value[tc_f$year == yr]
          data.frame(
            scenario   = sc_nm, year = yr,
            prod_comm1 = if (length(p1) > 0) p1[1] else NA_real_,
            prod_comm2 = if (length(p2) > 0) p2[1] else NA_real_,
            total_cost = if (length(tc) > 0) tc[1] else NA_real_,
            stringsAsFactors = FALSE
          )
        }))
      }))
      if (!is.null(fr_pts) && nrow(fr_pts) > 0) {
        fr_pts$tech           <- tech_name
        fr_pts$comm1          <- c1
        fr_pts$comm2          <- c2
        fr_pts$share_up_comm1 <- share_up[[c1]]
        fr_pts$share_up_comm2 <- share_up[[c2]]
        frontier_df <- fr_pts; rownames(frontier_df) <- NULL
      }
    }
  }

  # ── 13.5. Geometric share constraint segments ─────────────────────────────────
  input_frontier <- tech_share_frontier(object)

  # ── 13.9. Technology units ────────────────────────────────────────────────────
  costs_unit    <- if (nrow(object@units) > 0 && "costs"    %in% names(object@units)) {
    v <- object@units$costs[1]; if (!is.na(v) && nzchar(v)) v else ""
  } else ""
  activity_unit <- if (nrow(object@units) > 0 && "activity" %in% names(object@units)) {
    v <- object@units$activity[1]; if (!is.na(v) && nzchar(v)) v else ""
  } else ""
  output_units  <- setNames(
    vapply(out_comms, function(cm) {
      if (nrow(object@output) > 0 && "unit" %in% names(object@output)) {
        r <- object@output$unit[object@output$comm == cm]
        if (length(r) > 0 && !is.na(r[1]) && nzchar(r[1])) return(r[1])
      }
      ""
    }, character(1)), out_comms)
  tech_units <- list(costs = costs_unit, activity = activity_unit, output = output_units)

  # ── 14. Return ────────────────────────────────────────────────────────────────
  if (as_scenario) {
    scen@misc$levcost            <- levcost_tbl
    scen@misc$levcost_npv        <- levcost_npv
    scen@misc$cost_breakdown     <- cost_breakdown
    scen@misc$cost_breakdown_npv <- cost_breakdown_npv
    scen@misc$cost_yearly        <- cost_yearly
    scen@misc$levcost_per_act    <- levcost_per_act
    scen@misc$frontier           <- frontier_df
    scen@misc$levcost_by_comm    <- levcost_by_comm
    scen@misc$levcost_by_act     <- levcost_by_act
    scen@misc$levcost_by_act_cbd <- levcost_by_act_cbd
    scen@misc$input_frontier     <- input_frontier
    return(scen)
  }

  lc_name <- tech_name
  result <- list(
    levcost            = levcost_tbl,
    levcost_npv        = setNames(levcost_npv, lc_name),
    cost_breakdown     = cost_breakdown,
    cost_breakdown_npv = cost_breakdown_npv,
    levcost_per_act    = levcost_per_act,
    cost_yearly        = cost_yearly,
    frontier           = frontier_df,
    levcost_by_comm    = levcost_by_comm,
    levcost_by_act     = levcost_by_act,
    levcost_by_act_cbd = levcost_by_act_cbd,
    input_frontier     = input_frontier,
    discount           = discount,
    base_year          = as.integer(base_year),
    units              = tech_units,
    scenario           = scen
  )
  class(result) <- c("levcost", "list")
  result
}

# ── plot_share_frontier ─────────────────────────────────────────────────────────

#' Plot feasible share-mix diagram for technology inputs/outputs
#'
#' Builds the diagonal share-mix chart from a \code{\link{tech_share_frontier}}
#' data.frame.  Returns a named list of class \code{"share_frontier_plots"} of
#' \code{ggplot2} objects, one per (direction × group).
#'
#' @param df     data.frame from \code{tech_share_frontier()}.
#' @param title  Optional character string prepended to each plot title.
#' @param base_size Integer. Base font size passed to \code{theme_bw()}.
#' @return A list of class \code{"share_frontier_plots"}, or \code{NULL}.
#' @export
plot_share_frontier <- function(df, title = NULL, base_size = 11L) {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("Package 'ggplot2' is required for plot_share_frontier().")

  if (!"share_lo_eff" %in% names(df)) df$share_lo_eff <- df$share_lo
  if (!"share_hi_eff" %in% names(df)) df$share_hi_eff <- df$share_hi
  if (!"n_in_group"  %in% names(df)) df$n_in_group    <- NA_integer_

  dir_cols <- c(input = "#E84B35", output = "#4E79A7")

  .diagonal_layers <- function(col) {
    list(
      ggplot2::annotate("segment",
        x = 0, xend = 1, y = 1, yend = 0,
        colour = "grey80", linewidth = 0.5, linetype = "solid"),
      ggplot2::geom_rect(
        ggplot2::aes(xmin = share_lo_eff, xmax = share_hi_eff, ymin = 0, ymax = 1),
        fill = col, alpha = 0.12),
      ggplot2::geom_segment(
        ggplot2::aes(x = share_lo_eff, xend = share_hi_eff,
                     y = 1 - share_lo_eff, yend = 1 - share_hi_eff),
        colour = col, linewidth = 2.2),
      ggplot2::geom_point(
        ggplot2::aes(x = share_lo_eff, y = 1 - share_lo_eff),
        colour = col, size = 2.5, shape = 16),
      ggplot2::geom_point(
        ggplot2::aes(x = share_hi_eff, y = 1 - share_hi_eff),
        colour = col, size = 2.5, shape = 16),
      ggplot2::geom_text(
        ggplot2::aes(x = share_lo_eff, y = 1 - share_lo_eff,
                     label = paste0(round(share_lo_eff * 100), "%")),
        hjust = 1.25, vjust = 0.5, size = 2.5, colour = col),
      ggplot2::geom_text(
        ggplot2::aes(x = share_hi_eff, y = 1 - share_hi_eff,
                     label = paste0(round(share_hi_eff * 100), "%")),
        hjust = -0.25, vjust = 0.5, size = 2.5, colour = col)
    )
  }

  .base_scales <- function(x_name, y_name) {
    list(
      ggplot2::coord_fixed(ratio = 1, xlim = c(0, 1), ylim = c(0, 1),
                           expand = TRUE, clip = "off"),
      ggplot2::scale_x_continuous(
        name = x_name, breaks = c(0, 0.25, 0.5, 0.75, 1),
        labels = function(x) paste0(round(x * 100), "%")),
      ggplot2::scale_y_continuous(
        name = y_name, breaks = c(0, 0.25, 0.5, 0.75, 1),
        labels = function(x) paste0(round(x * 100), "%")),
      ggplot2::theme_bw(base_size = base_size),
      ggplot2::theme(
        panel.grid.minor = ggplot2::element_blank(),
        strip.text       = ggplot2::element_text(size = 7),
        axis.title       = ggplot2::element_text(size = 8, face = "bold"),
        plot.title       = ggplot2::element_text(size = 8, colour = "grey30",
                                                 hjust = 0.5, face = "bold"),
        plot.margin      = ggplot2::margin(4, 8, 4, 4))
    )
  }

  all_plots <- list()
  for (dir in c("input", "output")) {
    sub_dir <- df[df$direction == dir, , drop = FALSE]
    if (nrow(sub_dir) == 0L) next
    col <- dir_cols[[dir]]
    for (grp in unique(sub_dir$group)) {
      sub <- sub_dir[sub_dir$group == grp, , drop = FALSE]
      n_g <- if (!is.na(sub$n_in_group[1])) sub$n_in_group[1] else nrow(sub)
      plot_title <- if (!is.null(title)) paste0(title, "  |  ", dir, ": ", grp)
                    else paste0(dir, ": ", grp)
      if (n_g == 2L) {
        row1 <- sub[1L, , drop = FALSE]
        p <- ggplot2::ggplot(row1) +
          .diagonal_layers(col) +
          .base_scales(x_name = row1$comm[1], y_name = row1$others[1]) +
          ggplot2::labs(title = plot_title)
      } else {
        sub$facet_lab <- paste0(sub$comm, "\nvs. ", sub$others)
        p <- ggplot2::ggplot(sub) +
          .diagonal_layers(col) +
          ggplot2::facet_wrap(~facet_lab) +
          .base_scales(x_name = "Share of commodity", y_name = "Share of rest") +
          ggplot2::labs(title = plot_title)
      }
      all_plots[[paste(dir, grp, sep = ":::")]] <- p
    }
  }
  if (length(all_plots) == 0L) return(NULL)
  n_panels <- length(all_plots)
  attr(all_plots, "n_per_row") <- min(n_panels, 4L)
  attr(all_plots, "n_panels")  <- n_panels
  class(all_plots) <- c("share_frontier_plots", "list")
  all_plots
}

#' @export
print.share_frontier_plots <- function(x, ...) {
  if (requireNamespace("patchwork", quietly = TRUE)) {
    n_per_row <- attr(x, "n_per_row")
    if (is.null(n_per_row)) n_per_row <- min(length(x), 4L)
    print(patchwork::wrap_plots(x, ncol = n_per_row))
  } else {
    for (p in x) print(p)
  }
  invisible(x)
}

# ── autoplot / print methods ────────────────────────────────────────────────────
# autoplot() is the ggplot2 generic; energyRt registers its methods against it
# with the fully-qualified S3method(ggplot2::autoplot, <class>) form (delayed
# registration, no hard ggplot2 dependency). Users call it via library(ggplot2).

.levcost_comp_order  <- c("eac", "fixom", "varom", "supply", "import", "export")
.levcost_comp_labels <- c(eac    = "EAC (Annualised Inv.)",
                           fixom  = "Fixed O&M",
                           varom  = "Variable O&M",
                           supply = "Supply / Fuel",
                           import = "Import",
                           export = "Export (credit)")

#' @export
print.levcost <- function(x, ...) {
  cat("levcost\n")
  cat("  Technology:  ", names(x$levcost_npv), "\n", sep = "")
  cat("  NPV LCOE:    ", round(x$levcost_npv, 4), "\n", sep = "")
  if (!is.null(x$cost_breakdown_npv) && nrow(x$cost_breakdown_npv) > 0) {
    cat("  Components:  ")
    cat(paste(x$cost_breakdown_npv$component, collapse = ", "), "\n")
  }
  yr <- x$levcost$year[!is.na(x$levcost$year)]
  if (length(yr) > 0)
    cat("  Years:       ", min(yr), "\u2013", max(yr), " (", length(yr), ")\n", sep = "")
  cat("  Discount:    ", x$discount * 100, "%\n", sep = "")
  cat("  Frontier:    ", if (!is.null(x$frontier)) "yes" else "no (run with frontier=TRUE)", "\n", sep = "")
  invisible(x)
}

#' @export
print.levcost_list <- function(x, ...) {
  cat("levcost_list (", length(x), " technologies)\n", sep = "")
  for (nm in names(x)) {
    npv <- tryCatch(x[[nm]]$levcost_npv, error = function(e) NA_real_)
    cat("  ", nm, ": ", round(npv, 4), "\n", sep = "")
  }
  invisible(x)
}

#' @exportS3Method ggplot2::autoplot
autoplot.levcost <- function(object,
                             type = c("components", "npv", "totals",
                                      "frontier", "input_frontier"),
                             year = NULL,
                             cost_unit = NULL,
                             cost_unit_comm = NULL,
                             ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("Package 'ggplot2' is required for autoplot.")
  type  <- match.arg(type)
  npv   <- as.numeric(object$levcost_npv)
  title <- paste0("Levelized Cost: ", names(object$levcost_npv))

  .parse_cu <- function(cu) {
    m <- regmatches(cu, regexec("^(.+)/\\((.+)\\)$", cu))[[1]]
    if (length(m) == 3) list(currency = m[2], denom = m[3])
    else list(currency = cu, denom = "")
  }
  .cost_axis_label <- function(cu, cu_comm = NULL) {
    p <- .parse_cu(cu)
    lbl <- if (nzchar(p$denom)) paste(p$currency, "per", p$denom) else p$currency
    if (!is.null(cu_comm) && nzchar(cu_comm)) {
      pc <- .parse_cu(cu_comm)
      if (nzchar(pc$denom)) lbl <- paste0(lbl, " or ", pc$denom)
    }
    lbl
  }
  if (!is.null(cost_unit) && nzchar(cost_unit)) {
    y_lbl_act  <- .cost_axis_label(cost_unit)
    y_lbl_both <- .cost_axis_label(cost_unit, cost_unit_comm)
  } else {
    cu <- if (!is.null(object$units$costs)    && nzchar(object$units$costs))    object$units$costs    else ""
    au <- if (!is.null(object$units$activity) && nzchar(object$units$activity)) object$units$activity else ""
    costs_lbl  <- if (nzchar(cu) && nzchar(au)) paste0(cu, " / ", au)
                  else if (nzchar(cu)) cu else ""
    y_lbl_act  <- if (nzchar(costs_lbl)) paste0("LCOE [", costs_lbl, "]") else "Levelized Cost"
    y_lbl_both <- y_lbl_act
  }

  # ── frontier type ────────────────────────────────────────────────────────────
  if (type == "frontier") {
    df <- object$frontier
    if (is.null(df) || nrow(df) == 0) {
      message("No frontier data available. Run levcost() with frontier = TRUE ",
              "and a grouped technology with >= 2 output commodities with share.up.")
      return(invisible(NULL))
    }
    yr_sel <- if (!is.null(year)) as.integer(year) else min(df$year, na.rm = TRUE)
    df_yr  <- df[df$year == yr_sel, , drop = FALSE]
    c1_lbl <- df_yr$comm1[1]; c2_lbl <- df_yr$comm2[1]
    su1    <- df_yr$share_up_comm1[1]; su2 <- df_yr$share_up_comm2[1]
    df_yr$label <- ifelse(
      df_yr$scenario == paste0("max_", c1_lbl),
      paste0("max ", c1_lbl, " (share \u2264 ", round(su1 * 100), "%)"),
      paste0("max ", c2_lbl, " (share \u2264 ", round(su2 * 100), "%)")
    )
    lca_df <- object$levcost_by_act
    df_yr$levcost_label <- sapply(df_yr$scenario, function(sc_nm) {
      if (is.null(lca_df)) return("")
      base_val  <- lca_df$npv_act[match(sc_nm, lca_df$scenario)]
      prim_comm <- sub("^max_", "", sc_nm)
      inp_rows  <- lca_df[!is.na(lca_df$primary_input) &
                          lca_df$primary_comm == prim_comm, , drop = FALSE]
      if (!is.finite(base_val)) return("")
      if (nrow(inp_rows) > 0) {
        all_vals <- c(base_val, inp_rows$npv_act[is.finite(inp_rows$npv_act)])
        lo <- min(all_vals, na.rm = TRUE); hi <- max(all_vals, na.rm = TRUE)
        if (abs(hi - lo) > 1e-6) paste0("LCOE = ", round(lo, 3), "\u2013", round(hi, 3))
        else                     paste0("LCOE = ", round(base_val, 3))
      } else {
        paste0("LCOE = ", round(base_val, 3))
      }
    })
    p <- ggplot2::ggplot(df_yr, ggplot2::aes(x = prod_comm1, y = prod_comm2, colour = label)) +
      ggplot2::geom_line(colour = "grey50", linewidth = 0.8,
                         data = df_yr[order(df_yr$prod_comm1), ]) +
      ggplot2::geom_point(size = 4) +
      ggplot2::geom_text(ggplot2::aes(label = levcost_label),
                         vjust = -1, size = 3, show.legend = FALSE) +
      ggplot2::scale_colour_brewer(palette = "Set1") +
      ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
      ggplot2::labs(
        title    = paste0("Production Frontier: ", names(object$levcost_npv)),
        subtitle = paste0("Year ", yr_sel),
        x        = paste0("Production share of ", c1_lbl),
        y        = paste0("Production share of ", c2_lbl),
        colour   = "Operating extreme"
      ) + ggplot2::theme_bw()
    return(p)
  }

  # ── input_frontier type ───────────────────────────────────────────────────────
  if (type == "input_frontier") {
    df <- object$input_frontier
    p  <- plot_share_frontier(df, title = paste0("Share Mix: ", names(object$levcost_npv)))
    if (is.null(p)) {
      message("No share constraint data available. The technology may have no ",
              "grouped inputs or outputs with share.up / share.lo constraints.")
      return(invisible(NULL))
    }
    return(p)
  }

  # ── totals type ───────────────────────────────────────────────────────────────
  if (type == "totals") {
    df <- object$levcost
    df <- df[!is.na(df$year) & !is.na(df$levcost), ]
    p <- ggplot2::ggplot(df, ggplot2::aes(x = factor(year), y = levcost)) +
      ggplot2::geom_col(fill = "#4E79A7") +
      ggplot2::geom_hline(yintercept = npv, linetype = "dashed", colour = "red") +
      ggplot2::annotate("text", x = Inf, y = npv,
                        label = paste0("NPV LCOE = ", round(npv, 3)),
                        hjust = 1.05, vjust = -0.4, size = 3, colour = "red") +
      ggplot2::labs(title = title, x = "Year", y = y_lbl_act) +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
    return(p)
  }

  # ── npv type ──────────────────────────────────────────────────────────────────
  if (type == "npv") {
    has_frontier_data <- !is.null(object$levcost_by_comm) &&
                         nrow(object$levcost_by_comm) > 0
    if (has_frontier_data) {
      npv_rows <- list()
      if (!is.null(object$levcost_by_act) && nrow(object$levcost_by_act) > 0) {
        has_act_cbd <- !is.null(object$levcost_by_act_cbd) &&
                       nrow(object$levcost_by_act_cbd) > 0
        for (i in seq_len(nrow(object$levcost_by_act))) {
          row_i    <- object$levcost_by_act[i, ]
          prim     <- row_i$primary_comm
          sc_nm    <- row_i$scenario
          prim_inp <- if ("primary_input" %in% names(row_i) && !is.na(row_i$primary_input))
            row_i$primary_input else NA_character_
          lbl <- if (!is.na(prim_inp)) paste0("Activity\n(max ", prim, " + ", prim_inp, ")")
                 else paste0("Activity\n(max ", prim, ")")
          if (has_act_cbd) {
            cbd_i <- object$levcost_by_act_cbd[
              object$levcost_by_act_cbd$scenario == sc_nm, , drop = FALSE]
            if (nrow(cbd_i) > 0) {
              cbd_i$comm  <- "Activity"; cbd_i$label <- lbl
              npv_rows[[paste0("act_sc_", i)]] <- cbd_i[,
                c("tech", "group", "comm", "label", "component", "value"), drop = FALSE]
              next
            }
          }
          npv_rows[[paste0("act_sc_", i)]] <- data.frame(
            tech = row_i$tech, group = row_i$group,
            comm = "Activity", label = lbl,
            component = "total", value = row_i$npv_act, stringsAsFactors = FALSE
          )
        }
      }
      fr_df <- object$levcost_by_comm
      for (sc_nm in unique(fr_df$scenario)) {
        sub <- fr_df[fr_df$scenario == sc_nm, , drop = FALSE]
        prim_inp_c <- if ("primary_input" %in% names(sub) && !is.na(sub$primary_input[1]))
          sub$primary_input[1] else NA_character_
        sub$scenario      <- NULL
        sub$primary_input <- NULL
        lbl <- if (!is.na(prim_inp_c))
          paste0(sub$comm[1], "\n(max ", sub$comm[1], " + ", prim_inp_c, ")")
        else
          paste0(sub$comm[1], "\n(max ", sub$comm[1], ")")
        sub$label <- lbl
        npv_rows[[paste0("comm_sc_", sc_nm)]] <- sub[,
          c("tech", "group", "comm", "label", "component", "value"), drop = FALSE]
      }
      plot_df <- do.call(rbind, Filter(Negate(is.null), npv_rows))
      if (is.null(plot_df) || nrow(plot_df) == 0) {
        message("No NPV data to plot."); return(invisible(NULL))
      }
      rownames(plot_df) <- NULL
      comp_ord <- intersect(.levcost_comp_order, unique(plot_df$component))
      plot_df$component <- factor(plot_df$component,
                                  levels = c(comp_ord, setdiff(unique(plot_df$component), comp_ord)))
      plot_df$label <- factor(plot_df$label, levels = unique(plot_df$label))
      p <- ggplot2::ggplot(plot_df[plot_df$value > 0, ],
                           ggplot2::aes(x = label, y = value, fill = component)) +
        ggplot2::geom_col(position = "stack") +
        ggplot2::geom_col(data = plot_df[!is.na(plot_df$value) & plot_df$value < 0, ],
                          ggplot2::aes(x = label, y = value, fill = component),
                          position = "stack") +
        ggplot2::scale_fill_brewer(palette = "Set2",
                                   labels = .levcost_comp_labels[levels(plot_df$component)]) +
        ggplot2::labs(title = paste0("NPV LCOE breakdown: ", names(object$levcost_npv)),
                      x = "Metric", y = y_lbl_both, fill = "Component") +
        ggplot2::theme_bw() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))
      return(p)
    }
    # No frontier data – use base NPV breakdown
    cbd_npv <- object$cost_breakdown_npv
    if (is.null(cbd_npv) || nrow(cbd_npv) == 0) {
      message("No NPV cost breakdown available.")
      return(invisible(NULL))
    }
    comp_ord <- intersect(.levcost_comp_order, unique(cbd_npv$component))
    cbd_npv$component <- factor(cbd_npv$component,
                                levels = c(comp_ord, setdiff(unique(cbd_npv$component), comp_ord)))
    p <- ggplot2::ggplot(cbd_npv, ggplot2::aes(x = "", y = value, fill = component)) +
      ggplot2::geom_col(position = "stack") +
      ggplot2::scale_fill_brewer(palette = "Set2",
                                 labels = .levcost_comp_labels[levels(cbd_npv$component)]) +
      ggplot2::labs(title = paste0("NPV LCOE: ", names(object$levcost_npv)),
                    x = NULL, y = y_lbl_act, fill = "Component") +
      ggplot2::theme_bw()
    return(p)
  }

  # ── components type (default) ────────────────────────────────────────────────
  cbd <- object$cost_breakdown
  if (is.null(cbd) || nrow(cbd) == 0) {
    message("No cost breakdown data available.")
    return(invisible(NULL))
  }
  cbd <- cbd[!is.na(cbd$year), ]
  comp_ord    <- intersect(.levcost_comp_order, unique(cbd$component))
  cbd$component <- factor(cbd$component,
                          levels = c(comp_ord, setdiff(unique(cbd$component), comp_ord)))
  total_yr <- stats::aggregate(value ~ year, cbd, sum, na.rm = TRUE)
  p <- ggplot2::ggplot(cbd[cbd$value > 0 | cbd$component == "export", ],
                       ggplot2::aes(x = factor(year), y = value, fill = component)) +
    ggplot2::geom_col(position = "stack") +
    ggplot2::geom_line(data = data.frame(year = factor(total_yr$year), value = total_yr$value),
                       ggplot2::aes(x = year, y = value, group = 1),
                       inherit.aes = FALSE, colour = "grey30", linewidth = 0.7) +
    ggplot2::geom_hline(yintercept = npv, linetype = "dashed", colour = "red") +
    ggplot2::annotate("text", x = Inf, y = npv,
                      label = paste0("NPV = ", round(npv, 3)),
                      hjust = 1.05, vjust = -0.4, size = 3, colour = "red") +
    ggplot2::scale_fill_brewer(palette = "Set2",
                               labels = .levcost_comp_labels[levels(cbd$component)]) +
    ggplot2::labs(title = title, x = "Year", y = y_lbl_act, fill = "Component") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  p
}

#' @exportS3Method ggplot2::autoplot
autoplot.levcost_list <- function(object,
                                  type = c("components", "npv", "totals",
                                           "frontier", "input_frontier"),
                                  year = NULL, cost_unit = NULL,
                                  cost_unit_comm = NULL, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("Package 'ggplot2' is required for autoplot.")
  type <- match.arg(type)

  # Collect NPV + component breakdowns across technologies
  npv_rows <- lapply(names(object), function(nm) {
    lc  <- object[[nm]]
    cbd <- lc$cost_breakdown_npv
    if (is.null(cbd) || nrow(cbd) == 0) {
      data.frame(tech = nm, component = "total",
                 value = as.numeric(lc$levcost_npv),
                 stringsAsFactors = FALSE)
    } else {
      cbd$tech <- nm; cbd
    }
  })
  plot_df <- do.call(rbind, Filter(Negate(is.null), npv_rows))
  if (is.null(plot_df) || nrow(plot_df) == 0) {
    message("No NPV data available for any technology."); return(invisible(NULL))
  }
  comp_ord <- intersect(.levcost_comp_order, unique(plot_df$component))
  plot_df$component <- factor(plot_df$component,
                              levels = c(comp_ord, setdiff(unique(plot_df$component), comp_ord)))
  ggplot2::ggplot(plot_df, ggplot2::aes(x = tech, y = value, fill = component)) +
    ggplot2::geom_col(position = "stack") +
    ggplot2::scale_fill_brewer(palette = "Set2",
                               labels = .levcost_comp_labels[levels(plot_df$component)]) +
    ggplot2::labs(title = "NPV LCOE comparison", x = "Technology",
                  y = "Levelized Cost", fill = "Component") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))
}
