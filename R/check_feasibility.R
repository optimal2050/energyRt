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
    d <- .feas_slot(o, "import")
    if ("region" %in% names(d)) as.character(d$region) else as.character(o@region)
  }), use.names = FALSE)
  trd <- unlist(lapply(objs, function(o) {
    if (!methods::is(o, "trade")) return(NULL)
    if (!identical(as.character(o@commodity), comm)) return(NULL)
    as.character(.feas_slot(o, "routes")$dst)
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
    d <- .feas_slot(o, "supply")
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
# The same gate read from the declarations: the model-wide
# `config@optimizeRetirement` AND the process's own slot, both required. A
# `ret.*` bound on its own is inert and is not retirement.
#' @noRd
.feas_ret_declared <- function(objs, cfg = NULL) {
  if (!isTRUE(tryCatch(cfg@optimizeRetirement, error = function(e) FALSE))) {
    return(FALSE)
  }
  any(vapply(objs, function(o) {
    methods::.hasSlot(o, "optimizeRetirement") &&
      isTRUE(o@optimizeRetirement)
  }, logical(1)))
}

#' @noRd
.feas_aux_from_stock <- function(objs, ret_on = FALSE) {
  parts <- lapply(objs, function(o) {
    if (!methods::.hasSlot(o, "aeff") || !methods::.hasSlot(o, "capacity")) {
      return(NULL)
    }
    e <- .feas_slot(o, "aeff")
    if (!nrow(e) || !("cap2ainp" %in% names(e))) return(NULL)
    e <- e[!is.na(e$cap2ainp) & e$cap2ainp > 0, , drop = FALSE]
    if (!nrow(e)) return(NULL)
    cp <- .feas_slot(o, "capacity")
    if (!nrow(cp) || !("stock" %in% names(cp))) return(NULL)
    cp <- cp[!is.na(cp$stock) & cp$stock > 0, , drop = FALSE]
    if (!nrow(cp)) return(NULL)
    # under retirement, only what the bounds still pin is forced to stand --
    # and only for a process whose own gate is on. With no bound the whole
    # stock may go.
    if (ret_on && methods::.hasSlot(o, "optimizeRetirement") &&
        isTRUE(o@optimizeRetirement)) {
      bnd <- intersect(c("ret.up", "ret.fx"), names(cp))
      rt <- if (!length(bnd)) rep(Inf, nrow(cp)) else {
        r <- rep(NA_real_, nrow(cp))
        for (k in bnd) r <- pmax(r, cp[[k]], na.rm = TRUE)
        ifelse(is.na(r), Inf, r)
      }
      cp$stock <- pmax(cp$stock - rt, 0)
      cp <- cp[cp$stock > 0, , drop = FALSE]
      if (!nrow(cp)) return(NULL)
    }
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
    d <- .feas_slot(o, "demand")
    if (!nrow(d) || !("demand" %in% names(d))) return(NULL)
    d <- d[!is.na(d$demand), , drop = FALSE]
    if (!nrow(d) || !("region" %in% names(d))) return(NULL)
    yr <- if ("year" %in% names(d)) as.integer(d$year) else NA_integer_
    # A demand given for no particular year applies to every year, so it has
    # nothing to group on, and aggregate() drops NA groups. It travels on the
    # same "..any" sentinel the ceiling match uses further down.
    ykey <- ifelse(is.na(yr), "..any", as.character(yr))
    agg <- stats::aggregate(
      list(value = d$demand),
      list(region = as.character(d$region), ykey = ykey),
      sum, na.rm = TRUE)
    agg$year <- suppressWarnings(as.integer(agg$ykey))
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
# Every data query here goes through the package's own accessors, never
# through a slot. `get_data_slot()` resolves a parameter whether its table is
# in memory, parked in the scenario's store (`save_scenario()` empties `@data`
# above `energyRt.ondisk_min_rows`), still pending in a bulk write, or defined
# by a RULE and so held as no table at all. Reading `@data` saw none of those
# and reported "no contradiction" instead of checking.
#' @noRd
.feas_par <- function(mi, k) {
  prm <- tryCatch(mi@parameters[[k]], error = function(e) NULL)
  if (is.null(prm)) return(NULL)
  d <- tryCatch(as.data.frame(get_data_slot(prm, optional = TRUE)),
                error = function(e) NULL)
  if (is.null(d) || !nrow(d)) NULL else d
}

# The same for a slot of a DECLARED object, which may equally be on disk.
# Shaped like `as.data.frame(o@slot)` so the callers' own emptiness checks
# keep working.
#' @noRd
.feas_slot <- function(o, nm) {
  d <- tryCatch(get_lazy_data(o, slot = nm), error = function(e) NULL)
  if (is.null(d)) return(data.frame())
  as.data.frame(d)
}

# Where capacity may be scrapped EARLY, which decides whether a requirement
# that follows from standing capacity is forced at all. Natural ageing
# (`pTechStockSurv`) is not retirement: it is what the stock does on its own.
# Economic retirement is a DECISION, so a cell that can be retired cannot
# carry a "no solver needed" finding -- the optimiser can simply shed it.
#
# The gate is NOT the `ret.*` bounds. Retirement is built only when
# `settings@optimizeRetirement` is TRUE *and* the process's own
# `@optimizeRetirement` slot is TRUE (R/mapping-builders.R:2928,
# R/mapping-engine.R:471), and the mappings below exist only then -- so a
# `ret.up` declared without the gate is INERT and must not count as
# retirement. `mvTechRetiredStock` is the set of cells whose standing stock
# may be shed, which is exactly the question here.
#
# Returns one row per retirable (tech, region, year) with `ret` = how much
# may go, or NA where no bound caps it and the whole stock may be shed. An
# empty result means retirement is off, which is the common case.
#' @noRd
.feas_retirable <- function(mi) {
  empty <- data.frame(tech = character(), region = character(),
                      year = integer(), ret = numeric(),
                      stringsAsFactors = FALSE)
  cells <- .feas_par(mi, "mvTechRetiredStock")
  if (is.null(cells) || !nrow(cells) ||
      !all(c("tech", "region", "year") %in% names(cells))) {
    return(empty)
  }
  out <- data.frame(tech = as.character(cells$tech),
                    region = as.character(cells$region),
                    year = as.integer(cells$year), ret = NA_real_,
                    stringsAsFactors = FALSE)
  b <- .feas_par(mi, "pTechRet")
  if (!is.null(b) && nrow(b) && "value" %in% names(b)) {
    if ("type" %in% names(b)) b <- b[b$type %in% c("up", "fx"), , drop = FALSE]
    b <- b[!is.na(b$value), , drop = FALSE]
    if (nrow(b)) {
      for (cl in c("tech", "region", "year")) {
        if (!(cl %in% names(b))) b[[cl]] <- NA
      }
      agg <- stats::aggregate(
        list(ret = b$value),
        list(tech = as.character(b$tech), region = as.character(b$region),
             year = as.integer(b$year)), sum, na.rm = TRUE)
      out$ret <- agg$ret[match(paste(out$tech, out$region, out$year),
                               paste(agg$tech, agg$region, agg$year))]
    }
  }
  out
}

# ---- user constraints: forced bounds and derived bounds --------------------
#
# A user constraint is written as one GAMS-style statement, e.g.
#
#   eqCnsNO_BY_INV(year)$mCnsForEachNO_BY_INV(year)..
#     sum((tech, region)$mTechNew(tech, region, year),
#         vTechNewCap(tech, region, year)) =l= pCnsRhsNO_BY_INV(year);
#
# Two things can be read off it without a solver.
#
# DERIVED BOUNDS. A sum of NON-NEGATIVE variables held at or below zero pins
# every member to zero: `sum(vTechNewCap) =l= 0` means nothing may be built,
# anywhere in the sum. That is a bound the per-process parameters cannot
# express, and other checks need it -- the emission chain cannot bound a
# zero-emission fleet without it.
#
# FORCED MINIMA. Where the summed variable carries its own lower bounds, the
# sum cannot fall below their total; a right-hand side beneath that total is
# a contradiction in the declarations.
#
# Only the single-term form is read. A constraint with several terms, a
# coefficient, or a variable this cannot resolve is skipped -- silently for
# the bounds (a missing bound only makes other checks abstain) and explicitly
# for the findings.
#' @noRd
.feas_cns_parse <- function(eq) {
  if (!length(eq) || !nzchar(eq)) return(NULL)
  e <- gsub("[[:space:]]+", " ", eq)
  # sum((<dims>)$<map>(...), <var>(<dims>)) =<sense>= <rhs>(<dims>)
  m <- regmatches(e, regexec(
    paste0("sum\\(\\(([^)]*)\\)\\$([A-Za-z_][A-Za-z0-9_]*)\\([^)]*\\), ?",
           "(v[A-Za-z0-9_]*)\\(([^)]*)\\)\\) ?=(l|e|g)= ?",
           "(p[A-Za-z0-9_]*)\\(([^)]*)\\)"), e))[[1]]
  if (!length(m)) return(NULL)
  trim <- function(x) {
    x <- unlist(strsplit(x, ",")); x <- trimws(x)
    x[nzchar(x)]
  }
  list(summed = trim(m[2]), gate = m[3], var = m[4],
       var_dims = trim(m[5]), sense = m[6], rhs_par = m[7],
       rhs_dims = trim(m[8]))
}

# Variables energyRt declares non-negative. A sum of these at or below zero
# is what lets a zero right-hand side become a per-member bound; for a
# variable that may go negative the same algebra proves nothing.
.feas_nonneg_vars <- c(
  "vTechNewCap", "vTechCap", "vTechStockCap", "vTechAct", "vTechInp",
  "vTechOut", "vTechAInp", "vTechAOut", "vTechInv", "vTechEac",
  "vTechFixom", "vTechPhaseOut", "vTechStockPhaseOut", "vTechRet",
  "vStorageInpNewCap", "vStorageOutNewCap", "vStorageStgNewCap",
  "vStorageInpCap", "vStorageOutCap", "vStorageStgCap", "vStorageInp",
  "vStorageOut", "vStorageLevel", "vTradeNewCap", "vTradeCap", "vTradeIr",
  "vSupOut", "vSupReserve", "vImportRow", "vDemInp", "vEmsFuelTot",
  "vOutTot", "vInpTot")

# Every (variable, for.each tuple) a constraint pins to zero. Returned as a
# list keyed by variable name, each element the right-hand side's own tuples
# (e.g. a `year` column) at which the sum is held to zero.
#' @noRd
.feas_derived_zero <- function(mi) {
  ucs <- tryCatch(mi@user_constraints, error = function(e) NULL)
  if (!length(ucs)) return(list())
  out <- list()
  for (nm in names(ucs)) {
    p <- .feas_cns_parse(ucs[[nm]]$equation %||% "")
    if (is.null(p)) next
    if (!(p$sense %in% c("l", "e"))) next
    if (!(p$var %in% .feas_nonneg_vars)) next
    rhs <- .feas_par(mi, p$rhs_par)
    if (is.null(rhs) || !("value" %in% names(rhs))) next
    z <- rhs[!is.na(rhs$value) & rhs$value <= 0, , drop = FALSE]
    if (!nrow(z)) next
    keep <- setdiff(names(z), "value")
    z <- z[, keep, drop = FALSE]
    z$constraint <- nm
    out[[p$var]] <- unique(rbind(out[[p$var]], z))
  }
  out
}

# Is `var` pinned to zero in `year`? The question the other checks ask.
#' @noRd
.feas_zero_at <- function(dz, var, year) {
  z <- dz[[var]]
  if (is.null(z) || !("year" %in% names(z))) return(FALSE)
  any(as.integer(z$year) == as.integer(year))
}

# A constraint whose right-hand side sits below the total its own terms are
# already forced to reach. The forced minimum comes from the summed
# variable's own `lo`/`fx` bounds -- `vTechCap` from `pTechCap`, and so on --
# which are declarations, so the total is forced whatever the solver does.
#' @noRd
.feas_constraint_forced <- function(mi, years = NULL) {
  out <- data.frame(check = character(), commodity = character(),
                    region = character(), year = integer(),
                    required = numeric(), available = numeric(),
                    shortfall = numeric(), forced_by = character(),
                    stringsAsFactors = FALSE)
  ucs <- tryCatch(mi@user_constraints, error = function(e) NULL)
  if (!length(ucs)) return(out)
  for (nm in names(ucs)) {
    p <- .feas_cns_parse(ucs[[nm]]$equation %||% "")
    if (is.null(p) || !identical(p$sense, "l")) next
    rhs <- .feas_par(mi, p$rhs_par)
    if (is.null(rhs) || !all(c("value", "year") %in% names(rhs))) next
    # the summed variable's own bounds, if it has a parameter of that name
    bnd <- .feas_par(mi, sub("^v", "p", p$var))
    if (is.null(bnd) || !all(c("value", "type") %in% names(bnd))) next
    lo <- bnd[bnd$type %in% c("lo", "fx") & !is.na(bnd$value) &
                bnd$value > 0, , drop = FALSE]
    if (!nrow(lo) || !("year" %in% names(lo))) next
    rhs$year <- as.integer(rhs$year)
    if (!is.null(years)) {
      rhs <- rhs[rhs$year %in% as.integer(years), , drop = FALSE]
    }
    for (i in seq_len(nrow(rhs))) {
      y <- rhs$year[i]
      forced <- sum(lo$value[as.integer(lo$year) == y], na.rm = TRUE)
      if (forced <= rhs$value[i] + 1e-9) next
      out[nrow(out) + 1L, ] <- list("constraint-vs-forced", p$var, "(all)",
                                    y, forced, rhs$value[i],
                                    forced - rhs$value[i], nm)
    }
  }
  out
}

# ---- emission caps ---------------------------------------------------------
#
# A cap on an emission is NOT the "forced requirement vs hard ceiling" shape
# the two checks above use: it bounds a sum over DISPATCH decisions, and
# nothing in the declarations fixes that sum. What the declarations do fix is
# a chain:
#
#   demand must be served
#     -> the technologies that can serve it emission-free have a capacity
#        ceiling
#        -> the residue must come from emitting plant
#           -> so emissions are at least that residue times the cleanest
#              intensity available
#
# That lower bound is comparable with the cap, and a cap below it is a
# contradiction no solver setting can resolve.
#
# The chain is a PROOF only when every link is bounded, so this abstains --
# visibly, through the `abstained` attribute -- whenever it is not: when a
# zero-emission producer's new capacity has no upper bound (it could simply
# be built), or when an emitting producer's intensity cannot be computed (a
# cleaner one might exist). Abstaining is the discipline of the whole
# function: a cap that cannot be disproved must not be reported.
#
# Everything is compared at ANNUAL resolution. A per-timeslice gap is never
# smaller than the annual one, so the annual figure is the conservative
# choice and cannot manufacture a finding.
#' @noRd
.feas_emission_cap <- function(mi, years = NULL) {
  out <- data.frame(check = character(), commodity = character(),
                    region = character(), year = integer(),
                    required = numeric(), available = numeric(),
                    shortfall = numeric(), forced_by = character(),
                    stringsAsFactors = FALSE)
  ucs <- tryCatch(mi@user_constraints, error = function(e) NULL)
  fac <- .feas_par(mi, "pEmissionFactor")
  dem <- .feas_par(mi, "pDemand")
  outc <- .feas_par(mi, "mTechOutComm")
  inpc <- .feas_par(mi, "mTechInpComm")
  eff <- .feas_par(mi, "pTechCinp2use")
  c2a <- .feas_par(mi, "pTechCap2act")
  if (!length(ucs) || is.null(fac) || is.null(dem) || is.null(outc) ||
      is.null(inpc) || is.null(eff) || is.null(c2a)) {
    return(out)
  }
  stock <- .feas_par(mi, "pTechStock")
  ncap <- .feas_par(mi, "pTechNewCap")
  af <- .feas_par(mi, "pTechAf")
  # a constraint holding total new capacity at zero is a bound no
  # per-process parameter carries, and without it a zero-emission fleet
  # reads as freely expandable (see .feas_derived_zero)
  dz <- .feas_derived_zero(mi)
  model_years <- sort(unique(as.integer(mi@sets$year)))
  ab <- NULL
  note <- function(cns, year, reason) {
    ab <<- rbind(ab, data.frame(constraint = cns, year = year,
                                reason = reason, stringsAsFactors = FALSE))
  }

  # commodities a supply offers (primary) and commodities a technology makes
  primary_comm <- if (is.null(.feas_par(mi, "pSupAva"))) character() else {
    unique(as.character(.feas_par(mi, "pSupAva")$comm))
  }
  made_comm <- unique(as.character(outc$comm))
  dem$year <- as.integer(dem$year)
  dem_a <- stats::aggregate(list(demand = dem$value),
                            list(comm = as.character(dem$comm),
                                 region = as.character(dem$region),
                                 year = dem$year), sum, na.rm = TRUE)
  cap2act <- stats::setNames(c2a$value, as.character(c2a$tech))

  # `af` enters as its MAXIMUM up/fx value (1 when none is given), which
  # over-states the annual ceiling and so under-states the gap
  af_max <- function(t) {
    if (is.null(af) || !("type" %in% names(af))) return(1)
    a <- af[af$tech == t & af$type %in% c("up", "fx"), , drop = FALSE]
    if (!nrow(a) || all(is.na(a$value))) return(1)
    max(a$value, na.rm = TRUE)
  }
  # cumulative new capacity the bounds still permit up to `y`; NA as soon as
  # ONE model year carries no upper bound, because the ceiling is then open
  ncap_cum <- function(t, y) {
    yrs <- model_years[model_years <= y]
    # a year whose total new capacity is pinned to zero contributes zero and
    # needs no per-process bound
    zero_y <- vapply(yrs, function(yy) {
      .feas_zero_at(dz, "vTechNewCap", yy)
    }, logical(1))
    open <- yrs[!zero_y]
    if (!length(open)) return(0)
    if (is.null(ncap)) return(NA_real_)
    u <- ncap[ncap$tech == t & ncap$type %in% c("up", "fx") &
                as.integer(ncap$year) %in% open, , drop = FALSE]
    if (!nrow(u) || !all(open %in% as.integer(u$year))) return(NA_real_)
    sum(u$value, na.rm = TRUE)
  }
  # A technology drawing a BOUNDED auxiliary commodity per unit of capacity
  # is itself bounded: land, water or any other per-capacity input caps how
  # much of it can ever stand, however free its own `ncap` is. The whole
  # resource is credited to each technology separately, which over-states
  # every one of them and so under-states the gap -- the safe direction.
  cap2ainp <- .feas_par(mi, "pTechCap2AInp")
  supava <- .feas_par(mi, "pSupAva")
  aux_bound <- function(t, r, y) {
    if (is.null(cap2ainp) || is.null(supava)) return(NA_real_)
    cc <- cap2ainp[cap2ainp$tech == t &
                     (is.na(cap2ainp$region) | cap2ainp$region == r) &
                     (is.na(cap2ainp$year) | as.integer(cap2ainp$year) == y) &
                     !is.na(cap2ainp$value) & cap2ainp$value > 0, ,
                   drop = FALSE]
    if (!nrow(cc)) return(NA_real_)
    av <- supava[supava$type %in% c("up", "fx"), , drop = FALSE]
    bs <- vapply(unique(as.character(cc$acomm)), function(ac) {
      a <- av[av$comm == ac & (is.na(av$region) | av$region == r) &
                (is.na(av$year) | as.integer(av$year) == y), , drop = FALSE]
      if (!nrow(a)) return(NA_real_)
      sum(a$value, na.rm = TRUE) / max(cc$value[cc$acomm == ac], na.rm = TRUE)
    }, numeric(1))
    if (all(is.na(bs))) NA_real_ else min(bs, na.rm = TRUE)
  }
  # A technology burning a BOUNDED fuel is bounded in output by that fuel,
  # whatever its capacity: biomass with no land draw and no investment limit
  # is still capped by how much biomass exists. Output per unit of fuel is
  # the efficiency `pTechCinp2use` carries.
  out_from_fuel <- function(t, r, y) {
    if (is.null(eff) || is.null(supava)) return(NA_real_)
    if (!any(inpc$tech == t)) return(NA_real_)   # no fuel at all (wind, sun)
    e <- eff[eff$tech == t & !is.na(eff$value) & eff$value > 0, , drop = FALSE]
    if (!nrow(e)) return(NA_real_)       # fuel, but no efficiency to apply
    av <- supava[supava$type %in% c("up", "fx"), , drop = FALSE]
    bs <- vapply(unique(as.character(e$comm)), function(fc) {
      a <- av[av$comm == fc & (is.na(av$region) | av$region == r) &
                (is.na(av$year) | as.integer(av$year) == y), , drop = FALSE]
      if (!nrow(a)) return(NA_real_)
      sum(a$value, na.rm = TRUE) * max(e$value[e$comm == fc], na.rm = TRUE)
    }, numeric(1))
    if (all(is.na(bs))) NA_real_ else min(bs, na.rm = TRUE)
  }
  # the largest capacity a technology can reach: its own bounds where they
  # exist, otherwise whatever a bounded per-capacity resource allows
  cap_max <- function(t, r, y) {
    nc <- ncap_cum(t, y)
    own <- if (is.na(nc)) NA_real_ else stock_at(t, r, y) + nc
    ab <- aux_bound(t, r, y)
    if (!is.na(own) && !is.na(ab)) return(min(own, ab))
    if (!is.na(own)) return(own)
    ab
  }
  stock_at <- function(t, r, y) {
    if (is.null(stock)) return(0)
    v <- stock$value[stock$tech == t &
                       (is.na(stock$region) | stock$region == r) &
                       as.integer(stock$year) == y]
    if (!length(v)) 0 else sum(v, na.rm = TRUE)
  }
  # the cleanest intensity an emitting producer can offer, per unit of the
  # commodity it makes: the emission factor of its fuel over its efficiency
  intensity <- function(t, ec, fuels) {
    e <- eff[eff$tech == t & eff$comm %in% fuels, , drop = FALSE]
    if (!nrow(e)) return(NA_real_)
    vals <- vapply(unique(as.character(e$comm)), function(f) {
      ev <- e$value[e$comm == f]
      fv <- fac$value[fac$comm == ec & fac$commp == f]
      if (!length(ev) || !length(fv) || any(ev <= 0, na.rm = TRUE)) {
        return(NA_real_)
      }
      max(fv) / min(ev)
    }, numeric(1))
    if (anyNA(vals)) NA_real_ else min(vals)
  }

  for (nm in names(ucs)) {
    eq <- ucs[[nm]]$equation %||% ""
    if (!grepl("vEmsFuelTot", eq, fixed = TRUE)) next
    if (!grepl("=l=", tolower(eq), fixed = TRUE)) next    # a cap, not a floor
    rhs <- .feas_par(mi, paste0("pCnsRhs", nm))
    if (is.null(rhs) || !all(c("value", "year") %in% names(rhs))) next
    fe <- .feas_par(mi, paste0("mCnsForEach", nm))
    ecs <- if (!is.null(fe) && "comm" %in% names(fe)) {
      unique(as.character(fe$comm))
    } else {
      unique(as.character(fac$comm))
    }
    rhs$year <- as.integer(rhs$year)
    if (!is.null(years)) {
      rhs <- rhs[rhs$year %in% as.integer(years), , drop = FALSE]
    }
    for (ec in ecs) {
      fuels <- unique(as.character(fac$commp[fac$comm == ec]))
      if (!length(fuels)) next
      for (i in seq_len(nrow(rhs))) {
        y <- rhs$year[i]
        cap <- rhs$value[i]
        dy <- dem_a[dem_a$year == y, , drop = FALSE]
        if (!nrow(dy)) next
        floor_tot <- 0
        ok <- TRUE
        for (dc in unique(as.character(dy$comm))) {
          prod <- unique(as.character(outc$tech[outc$comm == dc]))
          if (!length(prod)) next
          emitters <- unique(as.character(inpc$tech[inpc$comm %in% fuels]))
          emitting <- intersect(prod, emitters)
          clean <- setdiff(prod, emitting)
          # A technology whose every input is an INTERMEDIATE -- a commodity
          # no supply offers, made by other technologies -- transports or
          # converts energy rather than adding any: a transmission converter
          # returns the electricity that entered it. Counting its output as
          # extra clean supply would double-count the plants that produced
          # that input, so it is left out of the ceiling.
          clean <- clean[!vapply(clean, function(t) {
            ic <- unique(as.character(inpc$comm[inpc$tech == t]))
            length(ic) > 0 && !any(ic %in% primary_comm) &&
              all(ic %in% made_comm)
          }, logical(1))]
          if (!length(emitting)) next    # servable without this emission
          int <- vapply(emitting, intensity, numeric(1), ec = ec,
                        fuels = fuels)
          if (anyNA(int)) {
            note(nm, y, paste0("intensity unknown for ", sum(is.na(int)),
                               " of ", length(int),
                               " emitting producer(s) of ", dc))
            ok <- FALSE
            break
          }
          regs <- unique(as.character(dy$region[dy$comm == dc]))
          # a producer is unbounded only when NEITHER its capacity nor its
          # fuel is bounded in any region that demands the commodity
          unb <- vapply(clean, function(t) {
            all(vapply(regs, function(r) {
              is.na(cap_max(t, r, y)) && is.na(out_from_fuel(t, r, y))
            }, logical(1)))
          }, logical(1))
          if (length(unb) && any(unb)) {
            note(nm, y, paste0(sum(unb), " of ", length(clean),
                               " zero-emission producer(s) of ", dc,
                               " have unbounded capacity"))
            ok <- FALSE
            break
          }
          # Totals over regions, not a sum of per-region gaps: a route in
          # lets one region's clean output serve another's load, so only the
          # national residue is forced. max(SUM d - SUM ceiling, 0) is never
          # larger than the sum of the per-region gaps.
          d_tot <- sum(dy$demand[dy$comm == dc], na.rm = TRUE)
          ceil_tot <- if (!length(clean)) 0 else sum(vapply(regs, function(r) {
            sum(vapply(clean, function(t) {
              cm <- cap_max(t, r, y)
              by_cap <- if (is.na(cm)) {
                NA_real_
              } else {
                cm * (cap2act[[t]] %||% 0) * af_max(t)
              }
              by_fuel <- out_from_fuel(t, r, y)
              v <- suppressWarnings(min(c(by_cap, by_fuel), na.rm = TRUE))
              if (!is.finite(v)) 0 else v
            }, numeric(1)), na.rm = TRUE)
          }, numeric(1)), na.rm = TRUE)
          floor_tot <- floor_tot + max(d_tot - ceil_tot, 0) * min(int)
        }
        if (!ok || floor_tot <= cap + 1e-9) next
        out[nrow(out) + 1L, ] <- list("emission-cap", ec, "(all)", y,
                                      floor_tot, cap, floor_tot - cap, nm)
      }
    }
  }
  attr(out, "abstained") <- ab
  out
}

#' @noRd
.feas_interp <- function(mi, years = NULL) {
  key <- c("comm", "region", "year", "timeslice")
  req <- NULL

  # forced: exogenous capacity times its per-capacity aux draw.
  #
  # Standing capacity forces that draw only for as long as it must stand.
  # Economic retirement turns "must stand" into a DECISION, so the
  # requirement is computed under BOTH settings -- retirement off (the whole
  # stock stands) and retirement on (only what the bounds still pin) -- and
  # every finding carries the setting it belongs to. Which one the model
  # actually has travels back as the `retirement_actual` attribute, so a
  # reader can tell the live answer from the counterfactual.
  retb <- .feas_retirable(mi)
  settings <- if (nrow(retb)) c("off", "on") else "off"
  st <- .feas_par(mi, "pTechStock")
  cf <- .feas_par(mi, "pTechCap2AInp")
  aux_req <- function(stock) {
    if (is.null(stock) || !nrow(stock) || is.null(cf)) return(NULL)
    j <- merge(stock, cf, by = c("tech", "region", "year"),
               suffixes = c(".stock", ".coef"))
    if (!nrow(j)) return(NULL)
    data.frame(check = "aux-from-stock", comm = j$acomm,
               region = j$region, year = j$year,
               timeslice = j$timeslice,
               value = j$value.stock * j$value.coef,
               forced_by = sub("_(VIN|CL).*$", "", j$tech),
               stringsAsFactors = FALSE)
  }
  # what still stands once the retirement bounds are taken up in full; a
  # bound given for no particular year applies to every year
  st_on <- if (!nrow(retb) || is.null(st)) st else {
    retb$retirable <- TRUE
    m <- merge(st, retb, by = c("tech", "region", "year"), all.x = TRUE)
    anyy <- retb[is.na(retb$year), c("tech", "region", "ret"), drop = FALSE]
    if (nrow(anyy)) {
      i <- is.na(m$ret)
      m$ret[i] <- anyy$ret[match(paste(m$tech[i], m$region[i]),
                                 paste(anyy$tech, anyy$region))]
    }
    # a retirable cell with no bound can shed everything; `ret` is NA only
    # for cells `mvTechRetiredStock` lists, so a non-retirable cell keeps 0
    m$ret[is.na(m$ret) & !is.na(m$retirable)] <- Inf
    m$ret[is.na(m$ret)] <- 0
    m$value <- pmax(m$value - m$ret, 0)
    m[, setdiff(names(m), c("ret", "retirable")), drop = FALSE]
  }
  # forced: declared demand. Independent of retirement -- a demand is a
  # requirement whatever happens to capacity -- so it enters both settings,
  # which keeps each setting's report complete on its own.
  dm <- .feas_par(mi, "pDemand")
  dem_req <- if (is.null(dm)) NULL else data.frame(
    check = "demand-vs-supply", comm = dm$comm, region = dm$region,
    year = dm$year, timeslice = dm$timeslice, value = dm$value,
    forced_by = dm$dem, stringsAsFactors = FALSE)
  req <- do.call(rbind, lapply(settings, function(sg) {
    r <- rbind(aux_req(if (identical(sg, "on")) st_on else st), dem_req)
    if (is.null(r) || !nrow(r)) return(NULL)
    r$retirement <- sg
    r
  }))
  empty <- data.frame(check = character(), commodity = character(),
                      region = character(), year = integer(),
                      required = numeric(), available = numeric(),
                      shortfall = numeric(), forced_by = character(),
                      retirement = character(), stringsAsFactors = FALSE)
  attr(empty, "retirement_actual") <- if (nrow(retb)) "on" else "off"
  # a cap finding stands on its own: it must survive a path that finds
  # nothing else, so the emission rows are added before every early return
  emis <- .feas_emis_rows(mi, years, settings)
  emis_ab <- attr(emis, "abstained")
  cf <- .feas_constraint_forced(mi, years)
  if (nrow(cf)) {
    cf <- do.call(rbind, lapply(settings, function(sg) {
      cf$retirement <- sg
      cf
    }))
    emis <- rbind(emis, cf)
  }
  fin <- function(x) {
    if (nrow(emis)) x <- rbind(x, emis)
    attr(x, "retirement_actual") <- if (nrow(retb)) "on" else "off"
    attr(x, "abstained") <- emis_ab
    x <- x[order(x$retirement, x$check, x$commodity, x$year, x$region), ,
           drop = FALSE]
    rownames(x) <- NULL
    x
  }
  if (is.null(req) || !nrow(req)) return(fin(empty))
  if (!is.null(years)) {
    req <- req[req$year %in% as.integer(years), , drop = FALSE]
    if (!nrow(req)) return(fin(empty))
  }
  req <- stats::aggregate(
    list(required = req$value),
    list(check = req$check, comm = req$comm, region = req$region,
         year = req$year, timeslice = req$timeslice,
         forced_by = req$forced_by, retirement = req$retirement),
    sum, na.rm = TRUE)

  av <- .feas_par(mi, "pSupAva")
  if (is.null(av)) return(fin(empty))
  av <- av[av$type %in% c("up", "fx"), , drop = FALSE]
  if (!nrow(av)) return(fin(empty))
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
  if (!nrow(m)) return(fin(empty))
  if (!is.null(safe) && nrow(safe)) {
    m <- m[!(paste(m$comm, m$region) %in% paste(safe$comm, safe$region)), ,
           drop = FALSE]
  }
  m <- m[m$required > m$available + 1e-9, , drop = FALSE]
  if (!nrow(m)) return(fin(empty))
  out <- data.frame(check = m$check, commodity = m$comm, region = m$region,
                    year = m$year, required = m$required,
                    available = m$available,
                    shortfall = m$required - m$available,
                    forced_by = m$forced_by, retirement = m$retirement,
                    stringsAsFactors = FALSE)
  fin(out)
}

# An emission cap does not depend on retirement -- the chain runs from demand,
# not from standing capacity -- so the same rows belong to every setting. The
# abstentions travel back as an attribute; `<<-` would reach the namespace,
# not the caller.
#' @noRd
.feas_emis_rows <- function(mi, years, settings) {
  e <- .feas_emission_cap(mi, years)
  ab <- attr(e, "abstained")
  rows <- if (nrow(e)) {
    do.call(rbind, lapply(settings, function(sg) {
      e$retirement <- sg
      e
    }))
  } else {
    cbind(e, retirement = character())
  }
  attr(rows, "abstained") <- ab
  rows
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
    ra <- attr(out, "retirement_actual")
    rownames(out) <- NULL
    attr(out, "retirement_actual") <- ra
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
  # the same two settings as the interpolated path: what is forced when the
  # stock must stand, and what is still forced once the retirement bounds are
  # taken up. A declared demand is forced either way.
  ret_any <- .feas_ret_declared(objs, tryCatch(object@config,
                                               error = function(e) NULL))
  settings <- if (ret_any) c("off", "on") else "off"
  req <- do.call(rbind, lapply(settings, function(sg) {
    r <- rbind(
      tag(.feas_aux_from_stock(objs, ret_on = identical(sg, "on")),
          "aux-from-stock"),
      tag(.feas_demand(objs), "demand-vs-supply"))
    if (is.null(r) || !nrow(r)) return(NULL)
    r$retirement <- sg
    r
  }))
  empty <- data.frame(check = character(), commodity = character(),
                      region = character(), year = integer(),
                      required = numeric(), available = numeric(),
                      shortfall = numeric(), forced_by = character(),
                      retirement = character(), stringsAsFactors = FALSE)
  attr(empty, "retirement_actual") <- if (ret_any) "on" else "off"
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
    # The year is carried as a label here for the same reason the ceiling
    # carries one: a requirement declared for no particular year is NA
    # throughout, and the formula method would drop it to nothing.
    r$ykey <- ifelse(is.na(r$year), "..any", as.character(r$year))
    r <- stats::aggregate(
      list(required = r$value),
      list(check = r$check, region = r$region, ykey = r$ykey,
           ckey = r$ckey, forced_by = r$process, retirement = r$retirement),
      sum, na.rm = TRUE)
    r$year <- suppressWarnings(as.integer(
      ifelse(r$ykey == "..any", NA, r$ykey)))
    # A ceiling given for no particular year applies to every year, and one
    # given for no particular tier serves every tier.
    ceil$ckey <- ifelse(is.na(ceil$cluster), "..any", ceil$cluster)
    m <- merge(r, ceil[, c("region", "year", "ckey", "value")],
               by = c("region", "year", "ckey"), all.x = TRUE)
    fill <- function(m, sub, keys) {
      if (!nrow(sub)) return(m)
      i <- is.na(m$value)
      if (!any(i)) return(m)
      k <- do.call(paste, c(m[i, keys, drop = FALSE], sep = "
"))
      sk <- do.call(paste, c(sub[, keys, drop = FALSE], sep = "
"))
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
               retirement = m$retirement, stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, Filter(Negate(is.null), found))
  if (is.null(out)) out <- empty
  out <- out[order(out$retirement, out$check, out$commodity, out$year,
                   out$region), , drop = FALSE]
  rownames(out) <- NULL
  attr(out, "retirement_actual") <- if (ret_any) "on" else "off"
  .feas_report(out, verbose, where = "declared")
}

#' @noRd
.feas_report <- function(out, verbose, where = "declared") {
  if (!verbose) return(out)
  ra <- attr(out, "retirement_actual") %||% "off"
  # A requirement that follows from standing capacity is forced only while
  # that capacity must stand, so findings are reported per retirement setting
  # and the model's own setting is named. With retirement off there is one
  # setting and nothing to distinguish.
  ret_line <- if (identical(ra, "on")) {
    paste0("  Economic retirement is ON here: rows with retirement = \"on\" ",
           "are the live\n  answer; \"off\" rows are what would be forced if ",
           "capacity could not be\n  scrapped early.")
  } else {
    paste0("  Economic retirement is OFF here, so standing capacity is ",
           "forced to stand\n  and there is one setting to report.")
  }
  # An emission cap that could not be disproved is reported as such:
  # silence would read as "checked and clean", the opposite of the truth.
  ab <- attr(out, "abstained")
  ab_line <- if (!is.null(ab) && nrow(ab)) {
    u <- unique(ab[, c("constraint", "reason")])
    paste0("\n  ", nrow(ab), " emission-cap check(s) ABSTAINED -- the ",
           "chain is not bounded, so nothing\n  is proved either way:\n",
           paste0("    ", u$constraint, ": ", u$reason, collapse = "\n"))
  } else {
    ""
  }
  if (!nrow(out)) {
    message("check_feasibility() [", where, "]: no contradiction found.\n",
            "  This is NOT a feasibility guarantee -- only the solver can ",
            "say whether a solution exists.\n", ret_line, ab_line)
    return(invisible(out))
  }
  n_live <- sum(out$retirement == ra)
  message("check_feasibility() [", where, "]: ", n_live,
          " contradiction(s) under the model's own setting (retirement = ",
          ra, "); ", nrow(out) - n_live, " under the other.\n",
          "  A forced requirement exceeds a ceiling that cannot be raised, ",
          "with no import\n  and no route in. No solver setting will make ",
          "this feasible.\n", ret_line, ab_line)
  print(out, row.names = FALSE)
  invisible(out)
}
