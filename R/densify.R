# =========================================================================== #
# densify.R  —  materialise default-valued tuples (the `sparse = FALSE` path).
#
# energyRt stores parameters SPARSE: a tuple absent from `@data` reads back as the
# parameter's `defVal`. Backends with a native default (MathProg / JuMP / Pyomo)
# handle that; GAMS has no default and reads an absent tuple as 0, so any
# parameter with a non-zero `defVal` must be MATERIALISED over its domain before a
# GAMS write. `densify_parameters()` does that — the live-path reconnection of the
# legacy `fullsets` behaviour (`interpolation_funs.R`), which the `ob2mi` rewrite
# dropped.
#
# Only (param, type) pairs with a FINITE NON-ZERO `defVal` are filled — a `0`
# default needs nothing (absent already reads 0, GAMS included) and an `Inf`
# default is a trivial bound whose equation is never generated where absent. For
# bounds the fill is PER TYPE (`defVal = [lo, up]`, e.g. `pTechAf = [0, 1]` fills
# only `up`).
#
# Domain = the union of every map parameter whose dimensions COVER the param's,
# projected to the param's dims (a safe superset: a tuple no equation reads is
# inert). Falls back to the Cartesian product of the dimension member-sets when no
# covering map exists (e.g. calendar globals). Explicit values always win.
# =========================================================================== #

# Per-parameter domain override: the single variable/equation map that defines
# where the parameter is actually read, used instead of the union of all covering
# maps (which over-covers comm-bearing coefficients via the larger aux maps). A
# too-narrow choice under-covers and the sparse/dense spine test fails, so this is
# self-checking. Params absent here fall back to the covering-map union.
.densify_domain_map <- list(
  pTechAf        = "mvTechAct",
  pTechCinp2use  = "mvTechInp",
  pTechUse2cact  = "mvTechOut",        # use->activity domain is the OUTPUT comm
  pTechCact2cout = "mvTechOut",
  pStorageAf     = "mvStorageStore",
  # combustion factor (defVal 1) is read only on the tech input-commodity set;
  # GAMS has no native default, so absent tuples read as 0 and zero out all
  # fuel-combustion emissions. Materialise 1.0 over mTechInpComm (explicit 0s,
  # e.g. non-combusting feedstock, are preserved by .densify_one).
  pTechEmisComm  = "mTechInpComm",
  pStorageInpEff = "mvStorageStore",
  pStorageOutEff = "mvStorageStore",
  pStorageStgEff = "mvStorageStore",
  # weather coefficients: domain is the (empty-when-unused) weather link map, so a
  # model that does not use a given weather bound densifies to nothing.
  pTechWeatherAf      = "mTechWeatherAfUp",
  pTechWeatherAfs     = "mTechWeatherAfsUp",
  pTechWeatherAfc     = "mTechWeatherAfcUp",
  pSupWeather         = "mSupWeatherUp",
  pStorageWeatherAf   = "mStorageWeatherAfUp",
  pStorageWeatherCinp = "mStorageWeatherCinpUp",
  pStorageWeatherCout = "mStorageWeatherCoutUp"
)

# Parameters never densified by default-fill: their absent tuples are NOT the
# defVal in a uniform sense. `pTechShare` / `pTechCinp2ginp` are read only on the
# (here empty) share / commodity-group equation domains, which the covering-map
# union over-covers. Until a precise gating map is wired, leave them sparse.
# (`pTechEmisComm` now has a precise domain override — mTechInpComm — above.)
.densify_exclude <- c("pTechShare", "pTechCinp2ginp")

# All map-type parameters as (name, dimSets, data) — the domain pool.
.densify_map_pool <- function(scen) {
  pool <- list()
  for (nm in names(scen@modInp@parameters)) {
    p <- scen@modInp@parameters[[nm]]
    if (is.null(p) || p@type != "map") next
    d <- as.data.frame(get_data_slot(p))
    if (is.null(d) || nrow(d) == 0) next
    pool[[nm]] <- list(dims = p@dimSets, data = d)
  }
  pool
}

# Domain for a parameter: union of covering maps projected to `dims`; else the
# Cartesian product of each dimension's member set.
.densify_domain <- function(scen, dims, pool, pn = NULL) {
  # explicit override is AUTHORITATIVE: use only that map's domain. An absent or
  # empty override map (e.g. an unused weather link) yields no domain, so the
  # parameter densifies to nothing — never falling through to the over-covering
  # union below.
  ov <- if (!is.null(pn)) .densify_domain_map[[pn]] else NULL
  if (!is.null(ov)) {
    mp <- pool[[ov]]
    if (is.null(mp) || !all(dims %in% mp$dims)) return(NULL)
    dom <- dplyr::distinct(mp$data[, dims, drop = FALSE])
    return(dom[stats::complete.cases(dom[, dims, drop = FALSE]), , drop = FALSE])
  }
  parts <- list()
  for (mp in pool) {
    if (!all(dims %in% mp$dims)) next
    parts[[length(parts) + 1L]] <- dplyr::distinct(mp$data[, dims, drop = FALSE])
  }
  if (length(parts) == 0) return(NULL)
  dom <- dplyr::distinct(dplyr::bind_rows(parts))
  # a wildcard (NA) is not a real member — drop any tuple with an NA dim
  dom[stats::complete.cases(dom[, dims, drop = FALSE]), , drop = FALSE]
}

.densify_one <- function(scen, pn, pool) {
  param <- scen@modInp@parameters[[pn]]
  dims  <- param@dimSets
  dv    <- param@defVal
  types <- if (param@type == "bounds") c("lo", "up") else NA_character_
  # which (type) slots need filling: finite & non-zero
  fill_idx <- which(is.finite(dv) & dv != 0)
  if (length(fill_idx) == 0) return(scen)

  dom <- .densify_domain(scen, dims, pool, pn)
  if (is.null(dom) || nrow(dom) == 0) return(scen)

  existing <- as.data.frame(get_data_slot(param))
  add <- list()
  for (i in fill_idx) {
    block <- dom
    if (param@type == "bounds") block$type <- types[i]
    block$value <- dv[i]
    add[[length(add) + 1L]] <- block
  }
  add <- dplyr::bind_rows(add)

  # explicit values win: drop filled rows whose key already exists
  key <- c(dims, if (param@type == "bounds") "type")
  if (nrow(existing) > 0) {
    ak <- do.call(paste, c(add[, key, drop = FALSE], sep = "\r"))
    ek <- do.call(paste, c(existing[, key, drop = FALSE], sep = "\r"))
    add <- add[!(ak %in% ek), , drop = FALSE]
  }
  if (nrow(add) == 0) return(scen)

  merged <- dplyr::bind_rows(existing, add)
  scen@modInp@parameters[[pn]] <- .fold_write_back(param, merged)
  scen
}

# Materialise every densifiable parameter (finite non-zero defVal). Runs only on
# the `sparse = FALSE` path, after interpolation + maps, before writing.
densify_parameters <- function(scen, verbose = FALSE) {
  pool <- .densify_map_pool(scen)
  for (pn in names(scen@modInp@parameters)) {
    p <- scen@modInp@parameters[[pn]]
    if (is.null(p) || p@type %in% c("set", "map")) next
    if (pn %in% .densify_exclude) next
    if (!any(is.finite(p@defVal) & p@defVal != 0)) next
    before <- nrow(as.data.frame(get_data_slot(p)))
    scen <- .densify_one(scen, pn, pool)
    if (isTRUE(verbose)) {
      after <- nrow(as.data.frame(get_data_slot(scen@modInp@parameters[[pn]])))
      if (after != before) message("densify '", pn, "': ", before, " -> ", after)
    }
  }
  scen
}
