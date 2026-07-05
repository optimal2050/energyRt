# =========================================================================== #
# prune.R  —  drop droppable (default-valued) parameter tuples.
#
# A parameter flagged `prune: { value: v }` in modInp.yml (carried to
# `param@misc$prune`) has its rows equal to `v` removed. Because an absent tuple
# reads back as the parameter's default, this is LOSSLESS whenever `v == defVal`:
# e.g. `pWeather` (defVal 0) — a dropped 0 still makes the weather up-bound
# `pTechWeatherAf* x pWeather = 0`, forcing the activity to 0 exactly as the
# explicit 0 did. Halves `pWeather` (672 -> 336) with no change to the LP.
#
# This drops only PARAMETER rows. Removing the dependent variable columns (the
# activity tuples forced to 0) is the equation-graph cascade owned by multimod's
# `trim` (mark empty-domain vars, untrim those still referenced) — not done here,
# because a partial mvTechAct removal in energyRt would leave dangling vTechAct
# references in the equations that read it.
# =========================================================================== #

# Drop `value == prune$value` rows from every parameter flagged in modInp.yml.
prune_parameters <- function(scen, verbose = FALSE) {
  for (pn in names(scen@modInp@parameters)) {
    param <- scen@modInp@parameters[[pn]]
    pr <- param@misc$prune
    if (is.null(pr)) next
    pv <- if (is.null(pr$value)) 0 else pr$value
    d <- as.data.frame(get_data_slot(param))
    if (is.null(d) || nrow(d) == 0 || !"value" %in% colnames(d)) next
    keep <- d[d$value != pv, , drop = FALSE]
    if (nrow(keep) == nrow(d)) next
    if (isTRUE(verbose)) {
      message("prune '", pn, "': ", nrow(d), " -> ", nrow(keep),
              " rows (dropped value == ", pv, ")")
    }
    scen@modInp@parameters[[pn]] <- .fold_write_back(param, keep)
  }
  scen
}
