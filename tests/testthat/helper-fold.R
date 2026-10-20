# =========================================================================== #
# Shared pieces of the fold suites (test-fold-*.R): the two-region fixture
# whose `afs` carries a region wildcard, the written-source reader, and the
# normalisers the invariant checks compare parameter data with.
# =========================================================================== #

# afs declared WITHOUT a region -> pTechAfs carries a region wildcard, while
# meqTechAfsUp is expanded over every region of mTechSpan.
fold_model <- function(horizon = newHorizon(2020, 1)) {
  ELC <- newCommodity("ELC", unit = "GWh", timeframe = "ANNUAL")
  COA <- newCommodity("COA", unit = "GWh", timeframe = "ANNUAL")
  SUP <- newSupply("SCOA", commodity = "COA", unit = "GWh",
                   supply = data.frame(cost = 1))
  TEC <- newTechnology(
    "TPP", input = list(comm = "COA", unit = "GWh"),
    output = list(comm = "ELC", unit = "GWh"),
    ceff = data.frame(comm = "COA", cinp2use = 0.4),
    afs = data.frame(timeslice = "ANNUAL", afs.up = 0.5),
    invcost = data.frame(invcost = 100), cap2act = 8760)
  DEM <- newDemand("DEM", commodity = "ELC", unit = "GWh",
                   demand = data.frame(region = c("R1", "R2"),
                                       demand = c(10, 20)))
  newModel(name = "foldsub", region = c("R1", "R2"), discount = 0.05,
           calendar = newCalendar(), horizon = horizon,
           data = newRepository("r", ELC, COA, SUP, TEC, DEM))
}

# every dimension the fold may collapse
FOLD_ALL <- c("region", "timeslice", "year", "comm", "tech", "stg", "trade")

# model source as written for `solver` (GLPK / JuMP / Pyomo / GAMS)
written_source <- function(scen, solver) {
  d <- withr::local_tempdir(.local_envir = parent.frame())
  write_script(scen, solver.dir = d, solver = solver)
  fs <- list.files(d, pattern = "[.](jl|mod|py|gms)$", recursive = TRUE,
                   full.names = TRUE)
  unlist(lapply(fs, readLines, warn = FALSE))
}

# a parameter's data as a plain, type-normalised, key-sorted data.frame
fold_norm <- function(d) {
  if (is.null(d)) return(NULL)
  d <- as.data.frame(d)
  if (nrow(d) == 0) return(NULL)
  for (j in names(d)) if (is.factor(d[[j]])) d[[j]] <- as.character(d[[j]])
  if ("year" %in% names(d)) d$year <- as.integer(d$year)
  if ("value" %in% names(d)) d$value <- as.numeric(d$value)
  keys <- setdiff(names(d), "value")
  d <- d[do.call(order, lapply(d[keys], as.character)),
         intersect(c(keys, "value"), names(d)), drop = FALSE]
  rownames(d) <- NULL
  d
}

fold_param_data <- function(scen, nm) {
  p <- scen@modInp@parameters[[nm]]
  if (is.null(p)) return(NULL)
  fold_norm(get_data_slot(p))
}

# names of the value parameters folded in `scen`, with their fold_info
fold_folded <- function(scen) {
  out <- list()
  for (nm in names(scen@modInp@parameters)) {
    p <- scen@modInp@parameters[[nm]]
    fi <- p@misc[["fold_info"]]
    if (!is.null(fi) && isTRUE(fi[["folded"]])) out[[nm]] <- fi
  }
  out
}

fold_objective <- function(scen) {
  sum(as.data.frame(getData(scen, "vObjective", merge = TRUE))$value,
      na.rm = TRUE)
}
