## Pre-solve contradiction checks.
##
## An infeasible solve tells you only that no feasible point exists. It names
## no constraint, and on a large model it costs a full solver run to learn
## nothing. Some contradictions, though, are arithmetic in the declarations
## and need no LP at all: a quantity that MUST be consumed, against a supply
## that CANNOT grow, in the same region and year.
##
## That is the class this file checks, and the only class it claims. Both
## checks compare a forced requirement with a hard ceiling, and report only
## where the requirement is unavoidable AND the ceiling has no way round it:
##
##   aux-from-stock  exogenous capacity consumes an auxiliary commodity
##                   (`cap2ainp * stock`) that a bounded supply cannot cover
##   demand-vs-supply  a declared demand exceeds the bounded supply of the
##                   commodity that serves it
##
## A finding is a defect in the data. A CLEAN RESULT IS NOT FEASIBILITY:
## everything that depends on a decision -- whether capacity can serve the
## load, whether a bound binds after optimisation, whether the dispatch
## closes in every timeslice -- is the solver's question and is not asked
## here. `print()` says so, because a check that is read as more than it is
## does more harm than no check.

#' @include check.R
NULL

# Regions that can bring `comm` into `region` from outside: an import, or a
# trade route arriving. Either one makes a local ceiling non-binding, so a
# shortfall against it proves nothing.
#' @noRd
.feas_backstops <- function(objs, comm) {
  imp <- unlist(lapply(objs, function(o) {
    if (!methods::is(o, "import")) return(NULL)
    if (!identical(as.character(o@commodity), comm)) return(NULL)
    d <- as.data.frame(o@import)
    if ("region" %in% names(d)) as.character(d$region) else as.character(o@region)
  }), use.names = FALSE)
  trd <- unlist(lapply(objs, function(o) {
    if (!methods::is(o, "trade")) return(NULL)
    if (!identical(as.character(o@commodity), comm)) return(NULL)
    as.character(as.data.frame(o@routes)$dst)
  }), use.names = FALSE)
  unique(c(imp, trd))
}

# The ceiling a supply puts on `comm` per region: `ava.fx` where given, else
# `ava.up`. A commodity with no bounded supply at all has no ceiling, and is
# not reported on.
#' @noRd
.feas_supply_ceiling <- function(objs, comm) {
  parts <- lapply(objs, function(o) {
    if (!methods::is(o, "supply")) return(NULL)
    if (!identical(as.character(o@commodity), comm)) return(NULL)
    d <- as.data.frame(o@supply)
    if (!nrow(d) || !("region" %in% names(d))) return(NULL)
    v <- if ("ava.fx" %in% names(d) && any(!is.na(d$ava.fx))) d$ava.fx else NULL
    if (is.null(v)) v <- if ("ava.up" %in% names(d)) d$ava.up else NA_real_
    yr <- if ("year" %in% names(d)) d$year else NA_integer_
    cl <- if ("cluster" %in% names(d)) as.character(d$cluster) else NA_character_
    data.frame(region = as.character(d$region), year = as.integer(yr),
               cluster = cl, value = as.numeric(v), stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, Filter(Negate(is.null), parts))
  if (is.null(out)) return(NULL)
  out <- out[!is.na(out$value) & is.finite(out$value), , drop = FALSE]
  if (!nrow(out)) return(NULL)
  # A bound declared for no particular year is the common case (land, a flat
  # reserve) and `year` is then NA throughout. The formula method drops NA
  # groups, which would silently report "no ceiling" for exactly those, so
  # the year is carried as a label and restored.
  # A tiered commodity is several balances, not one pool: `LANDW` tier `LO`
  # cannot serve a process declared on tier `HI`. Summing the tiers together
  # would hide exactly the contradiction tiers exist to express.
  yk <- ifelse(is.na(out$year), "..any", as.character(out$year))
  ck <- ifelse(is.na(out$cluster), "..any", out$cluster)
  agg <- stats::aggregate(list(value = out$value),
                          list(region = out$region, ykey = yk, ckey = ck),
                          sum, na.rm = TRUE)
  data.frame(region = agg$region,
             year = suppressWarnings(as.integer(
               ifelse(agg$ykey == "..any", NA, agg$ykey))),
             cluster = ifelse(agg$ckey == "..any", NA_character_, agg$ckey),
             value = agg$value, stringsAsFactors = FALSE)
}

# What exogenous capacity forces a process to consume: `cap2ainp` times the
# `stock` standing in that year. Both sides are declarations -- no decision
# is involved -- so the requirement cannot be avoided by optimising.
#' @noRd
.feas_aux_from_stock <- function(objs) {
  parts <- lapply(objs, function(o) {
    if (!methods::.hasSlot(o, "aeff") || !methods::.hasSlot(o, "capacity")) {
      return(NULL)
    }
    e <- as.data.frame(o@aeff)
    if (!nrow(e) || !("cap2ainp" %in% names(e))) return(NULL)
    e <- e[!is.na(e$cap2ainp) & e$cap2ainp > 0, , drop = FALSE]
    if (!nrow(e)) return(NULL)
    cp <- as.data.frame(o@capacity)
    if (!nrow(cp) || !("stock" %in% names(cp))) return(NULL)
    cp <- cp[!is.na(cp$stock) & cp$stock > 0, , drop = FALSE]
    if (!nrow(cp)) return(NULL)
    # both slots carry `year`, and `@aeff` may carry `region` too, so each
    # side is cut to what it contributes before the join
    key <- intersect(c("vintage", "cluster"), intersect(names(e), names(cp)))
    cp <- cp[, intersect(c(key, "region", "year", "stock"), names(cp)),
             drop = FALSE]
    e <- e[, intersect(c(key, "acomm", "cap2ainp"), names(e)), drop = FALSE]
    j <- if (length(key)) merge(cp, e, by = key) else merge(cp, e)
    if (!nrow(j) || !all(c("region", "acomm") %in% names(j))) return(NULL)
    if (!("year" %in% names(j))) j$year <- NA_integer_
    data.frame(process = o@name, acomm = as.character(j$acomm),
               region = as.character(j$region),
               year = as.integer(j$year),
               cluster = if ("cluster" %in% names(j))
                 as.character(j$cluster) else NA_character_,
               value = j$stock * j$cap2ainp, stringsAsFactors = FALSE)
  })
  do.call(rbind, Filter(Negate(is.null), parts))
}

# A declared demand is a requirement by construction.
#' @noRd
.feas_demand <- function(objs) {
  parts <- lapply(objs, function(o) {
    if (!methods::is(o, "demand")) return(NULL)
    d <- as.data.frame(o@demand)
    if (!nrow(d) || !("demand" %in% names(d))) return(NULL)
    d <- d[!is.na(d$demand), , drop = FALSE]
    if (!nrow(d) || !("region" %in% names(d))) return(NULL)
    yr <- if ("year" %in% names(d)) d$year else NA_integer_
    agg <- stats::aggregate(
      list(value = d$demand),
      list(region = as.character(d$region), year = as.integer(yr)),
      sum, na.rm = TRUE)
    agg$process <- o@name
    agg$acomm <- as.character(o@commodity)
    agg$cluster <- NA_character_
    agg[, c("process", "acomm", "region", "year", "cluster", "value")]
  })
  do.call(rbind, Filter(Negate(is.null), parts))
}


# ---- the same two checks, after interpolation -------------------------------
#
# The declared check reads slots; this one reads the parameter tables the
# solver will see. That is a different and stronger question. Interpolation
# fills the years between the declared ones, ages the stock through its
# vintages, and expands clusters into their own commodities -- so a ceiling
# crossed only at 2030, or a tier that exists only after expansion, is
# visible here and cannot be seen in the declarations.
#
# Names are already expanded (`EWIN_VIN2015_CLHI`, `LANDW_CLHI`), so the
# tiers separate themselves and no cluster matching is needed.
#' @noRd
.feas_par <- function(mi, k) {
  d <- tryCatch(as.data.frame(mi@parameters[[k]]@data), error = function(e) NULL)
  if (is.null(d) || !nrow(d)) NULL else d
}

#' @noRd
.feas_interp <- function(mi, years = NULL) {
  key <- c("comm", "region", "year", "timeslice")
  req <- NULL

  # forced: exogenous capacity times its per-capacity aux draw
  st <- .feas_par(mi, "pTechStock")
  cf <- .feas_par(mi, "pTechCap2AInp")
  if (!is.null(st) && !is.null(cf)) {
    j <- merge(st, cf, by = c("tech", "region", "year"),
               suffixes = c(".stock", ".coef"))
    if (nrow(j)) {
      req <- data.frame(check = "aux-from-stock", comm = j$acomm,
                        region = j$region, year = j$year,
                        timeslice = j$timeslice,
                        value = j$value.stock * j$value.coef,
                        forced_by = sub("_(VIN|CL).*$", "", j$tech),
                        stringsAsFactors = FALSE)
    }
  }
  # forced: declared demand
  dm <- .feas_par(mi, "pDemand")
  if (!is.null(dm)) {
    req <- rbind(req, data.frame(
      check = "demand-vs-supply", comm = dm$comm, region = dm$region,
      year = dm$year, timeslice = dm$timeslice, value = dm$value,
      forced_by = dm$dem, stringsAsFactors = FALSE))
  }
  empty <- data.frame(check = character(), commodity = character(),
                      region = character(), year = integer(),
                      required = numeric(), available = numeric(),
                      shortfall = numeric(), forced_by = character(),
                      stringsAsFactors = FALSE)
  if (is.null(req) || !nrow(req)) return(empty)
  if (!is.null(years)) {
    req <- req[req$year %in% as.integer(years), , drop = FALSE]
    if (!nrow(req)) return(empty)
  }
  req <- stats::aggregate(
    list(required = req$value),
    list(check = req$check, comm = req$comm, region = req$region,
         year = req$year, timeslice = req$timeslice,
         forced_by = req$forced_by),
    sum, na.rm = TRUE)

  av <- .feas_par(mi, "pSupAva")
  if (is.null(av)) return(empty)
  av <- av[av$type %in% c("up", "fx"), , drop = FALSE]
  if (!nrow(av)) return(empty)
  av <- stats::aggregate(list(available = av$value),
                         list(comm = av$comm, region = av$region,
                              year = av$year, timeslice = av$timeslice),
                         sum, na.rm = TRUE)

  # anything that can arrive from elsewhere makes a local ceiling non-binding
  safe <- unique(rbind(
    {
      im <- .feas_par(mi, "pImportRow")
      if (is.null(im) || !all(c("comm", "region") %in% names(im))) NULL else
        im[, c("comm", "region")]
    },
    {
      tr <- .feas_par(mi, "pTradeIr")
      d <- intersect(c("dst", "region"), names(tr))[1]
      if (is.null(tr) || is.na(d) || !("comm" %in% names(tr))) NULL else
        stats::setNames(tr[, c("comm", d)], c("comm", "region"))
    }))

  m <- merge(req, av, by = key)
  if (!nrow(m)) return(empty)
  if (!is.null(safe) && nrow(safe)) {
    m <- m[!(paste(m$comm, m$region) %in% paste(safe$comm, safe$region)), ,
           drop = FALSE]
  }
  m <- m[m$required > m$available + 1e-9, , drop = FALSE]
  if (!nrow(m)) return(empty)
  out <- data.frame(check = m$check, commodity = m$comm, region = m$region,
                    year = m$year, required = m$required,
                    available = m$available,
                    shortfall = m$required - m$available,
                    forced_by = m$forced_by, stringsAsFactors = FALSE)
  out[order(out$check, out$commodity, out$year, out$region), , drop = FALSE]
}

#' Check a model for contradictions that need no solver
#'
#' @description
#' Compares requirements that are FORCED by the declarations against ceilings
#' that CANNOT be raised, per commodity, region and year. A finding is a
#' contradiction in the data: no solver setting, starting point or tolerance
#' will make such a model feasible, and an infeasible solve will not say
#' which constraint was to blame.
#'
#' Two checks run:
#'
#' \describe{
#'   \item{`aux-from-stock`}{exogenous capacity (`@capacity$stock`) consumes
#'     an auxiliary commodity through `@aeff$cap2ainp`. The stock is given,
#'     so the consumption is unavoidable; if the commodity's supply is
#'     bounded and cannot be imported or traded in, a shortfall is a
#'     contradiction.}
#'   \item{`demand-vs-supply`}{a declared `demand` exceeds the bounded supply
#'     of its commodity in the same region and year, again with no import and
#'     no inbound route.}
#' }
#'
#' @section What a clean result does NOT mean:
#' It does not mean the model is feasible. Everything that depends on a
#' DECISION is the solver's question and is not asked here: whether capacity
#' can serve the load, whether a bound binds once the model optimises,
#' whether the dispatch closes in every timeslice, whether a corridor is big
#' enough. This function proves the absence of one kind of contradiction,
#' not the presence of a solution.
#'
#' @param object a `model`, `scenario` or `repository`.
#' @param years milestone years to check. `NULL` (default) checks every year
#'   present in the declarations.
#' @param verbose print the report. The findings are returned either way.
#'
#' @return A data.frame of findings -- `check`, `commodity`, `region`,
#'   `year`, `required`, `available`, `shortfall`, `forced_by` -- with zero
#'   rows when nothing is contradicted. Returned invisibly when `verbose`.
#'
#' @examples
#' \dontrun{
#' check_feasibility(mod)
#' # or, before a solve that would otherwise fail without saying why:
#' interpolate_model(mod, name = "BASE", check = TRUE)
#' }
#' @family check
#' @export
check_feasibility <- function(object, years = NULL, verbose = TRUE) {
  # A scenario that has been interpolated carries the tables the solver will
  # actually read; check those, since they answer the stronger question.
  mi <- tryCatch(object@modInp, error = function(e) NULL)
  if (!is.null(mi) && length(tryCatch(mi@sets$year, error = function(e) NULL))) {
    out <- .feas_interp(mi, years)
    rownames(out) <- NULL
    return(.feas_report(out, verbose, where = "interpolated"))
  }
  objs <- tryCatch(getObject(object), error = function(e) NULL)
  if (!length(objs)) {
    stop("check_feasibility(): `object` holds no model objects.", call. = FALSE)
  }
  # `cbind(NULL, check = "x")` is a one-row frame, not nothing, so a model
  # that declares only one of the two sources would rbind mismatched columns
  tag <- function(d, what) {
    if (is.null(d) || !nrow(d)) return(NULL)
    d$check <- what
    d
  }
  req <- rbind(tag(.feas_aux_from_stock(objs), "aux-from-stock"),
               tag(.feas_demand(objs), "demand-vs-supply"))
  empty <- data.frame(check = character(), commodity = character(),
                      region = character(), year = integer(),
                      required = numeric(), available = numeric(),
                      shortfall = numeric(), forced_by = character(),
                      stringsAsFactors = FALSE)
  if (is.null(req) || !nrow(req)) return(.feas_report(empty, verbose))
  if (!is.null(years)) {
    req <- req[is.na(req$year) | req$year %in% as.integer(years), ,
               drop = FALSE]
  }
  found <- lapply(unique(req$acomm), function(cm) {
    ceil <- .feas_supply_ceiling(objs, cm)
    if (is.null(ceil)) return(NULL)          # no ceiling, nothing to contradict
    safe <- .feas_backstops(objs, cm)
    r <- req[req$acomm == cm & !(req$region %in% safe), , drop = FALSE]
    if (!nrow(r)) return(NULL)
    r$ckey <- ifelse(is.na(r$cluster), "..any", r$cluster)
    r <- stats::aggregate(
      list(required = r$value),
      list(check = r$check, region = r$region, year = r$year,
           ckey = r$ckey, forced_by = r$process),
      sum, na.rm = TRUE)
    # A ceiling given for no particular year applies to every year, and one
    # given for no particular tier serves every tier.
    ceil$ckey <- ifelse(is.na(ceil$cluster), "..any", ceil$cluster)
    m <- merge(r, ceil[, c("region", "year", "ckey", "value")],
               by = c("region", "year", "ckey"), all.x = TRUE)
    fill <- function(m, sub, keys) {
      if (!nrow(sub)) return(m)
      i <- is.na(m$value)
      if (!any(i)) return(m)
      k <- do.call(paste, c(m[i, keys, drop = FALSE], sep = ""))
      sk <- do.call(paste, c(sub[, keys, drop = FALSE], sep = ""))
      m$value[i] <- sub$value[match(k, sk)]
      m
    }
    m <- fill(m, ceil[is.na(ceil$year), c("region", "ckey", "value")],
              c("region", "ckey"))
    m <- fill(m, ceil[ceil$ckey == "..any", c("region", "year", "value")],
              c("region", "year"))
    m <- fill(m, ceil[is.na(ceil$year) & ceil$ckey == "..any",
                      c("region", "value")], "region")
    m <- m[!is.na(m$value), , drop = FALSE]
    m <- m[m$required > m$value + 1e-9, , drop = FALSE]
    if (!nrow(m)) return(NULL)
    data.frame(check = m$check, commodity = cm, region = m$region,
               year = m$year, required = m$required, available = m$value,
               shortfall = m$required - m$value, forced_by = m$forced_by,
               stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, Filter(Negate(is.null), found))
  if (is.null(out)) out <- empty
  out <- out[order(out$check, out$commodity, out$year, out$region), ,
             drop = FALSE]
  rownames(out) <- NULL
  .feas_report(out, verbose, where = "declared")
}

#' @noRd
.feas_report <- function(out, verbose, where = "declared") {
  if (!verbose) return(out)
  if (!nrow(out)) {
    message("check_feasibility() [", where, "]: no contradiction found.\n",
            "  This is NOT a feasibility guarantee -- only the solver can ",
            "say whether a solution exists.")
    return(invisible(out))
  }
  message("check_feasibility() [", where, "]: ", nrow(out),
          " contradiction(s).\n",
          "  A forced requirement exceeds a ceiling that cannot be raised, ",
          "with no import\n  and no route in. No solver setting will make ",
          "this feasible.")
  print(out, row.names = FALSE)
  invisible(out)
}
