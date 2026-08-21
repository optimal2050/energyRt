# =========================================================================== #
# Fixtures for the invcost/eac parity suite.
#
# `@invcost$eac` supplies an annuity directly, bypassing invcost/wacc/olife.
# The property under test is that it is EQUIVALENT to supplying `invcost` with
# the rate and lifetime that produce the same annuity -- for every class that
# has a capacity and an eac: technology, storage and trade.
#
# This was never covered end-to-end. `test-eac-input.R` checks the parameter
# values but never solves, so a backend that silently drops the annuity term
# went unnoticed (storage did exactly that; see test-eac-parity.R).
# =========================================================================== #

# Discount rate and lifetime shared by every pair. Uniform and scalar on
# purpose: `.eac_one()` computes eac per (process, region, year), so a
# region- or year-varying rate would force the expected table to replicate that
# expansion, which tests the arithmetic of the test rather than the package.
EAC_WACC  <- 0.05
EAC_OLIFE <- 30L

# The annuity the pair must agree on. Uses rt_crf() from helper-rates.R --
# helper-*.R files are all sourced into one testthat environment -- so the
# formula is stated once, independently of `.crf()` in R/eac.R.
eac_of <- function(invcost, wacc = EAC_WACC, olife = EAC_OLIFE) {
  invcost * rt_crf(wacc, as.numeric(olife))
}

# Build the two halves of a parity pair for one class.
#
# `inv`: @invcost$invcost = `invcost`, annuity DERIVED at the model rate.
# `eac`: @invcost$eac     = invcost * crf(wacc, olife), nothing else.
#
# Capacity is PINNED (cap.fx) rather than left economic, so the objective delta
# between a working and a broken annuity is exactly the charge on a known
# capacity -- deterministic, and it fails loudly instead of depending on whether
# the object happens to enter the merit order.
#
# `olife` is always explicit: pXOlife defaults to 1, but an unset olife makes
# `.eac_one()` use Inf (a perpetual annuity) while the solver charges over the
# olife window, so the pair would diverge for a reason unrelated to eac.
# `payback` is deliberately never set (unsupported outside GLPK).
eac_pair <- function(class = c("technology", "storage", "trade"),
                     invcost = 1000, cap = 5) {
  class <- match.arg(class)
  vin <- data.frame(olife = EAC_OLIFE)
  a <- eac_of(invcost)

  mk <- function(cost_df) {
    switch(class,
      technology = newTechnology(
        "EEAC", output = list(comm = "ELC"),
        afs = data.frame(timeslice = "ANNUAL", afs.up = 0.4),
        invcost = cost_df, vintage = vin, cap2act = 1,
        capacity = data.frame(cap.fx = cap)),
      storage = newStorage(
        "SEAC", commodity = "ELC",
        invcost = cost_df, vintage = vin, duration = 4,
        capacity = data.frame(cap.fx = cap)),
      trade = newTrade(
        "TEAC", commodity = "ELC",
        routes = data.frame(src = "R1", dst = "R2"),
        trade  = data.frame(src = "R1", dst = "R2", teff = 0.95),
        invcost = cost_df, vintage = vin, cap2act = 1,
        capacity = data.frame(cap.fx = cap))
    )
  }

  wrap <- function(obj, nm) {
    if (class == "trade") vt_trade_model(obj, name = nm)
    else if (class == "storage") vt_stg_model(obj, name = nm)
    else vt_model(obj, name = nm)
  }

  list(
    inv = wrap(mk(data.frame(invcost = invcost)), "inv"),
    eac = wrap(mk(data.frame(eac = a)), "eac"),
    annuity = a,
    cap = cap
  )
}

# Total objective of a solved scenario.
eac_obj <- function(sol) {
  sum(suppressMessages(getData(sol, "vObjective", merge = TRUE))$value)
}

# Total of an annuity variable (vTechEac / vStorageEac / vTradeEac). Returns 0
# when the variable is absent or empty, so a DROPPED annuity reads as 0 rather
# than erroring -- which is precisely the failure this suite must detect.
eac_var <- function(sol, v) {
  d <- tryCatch(suppressMessages(getData(sol, v, merge = TRUE)),
                error = function(e) NULL)
  if (is.null(d) || !NROW(d)) return(0)
  sum(d$value)
}

# The eqXEac statement for one class out of one backend's template text.
#
# COMMENTS ARE STRIPPED FIRST. The [eac-fix] notes left in the templates quote
# the removed guard verbatim ("pStorageInvcost <> 0"), so a naive text search
# matches the explanation rather than the code -- which is exactly what this
# invariant must not do.
#
# The statement is bounded by the next equation definition rather than by a
# terminator, because the four backends do not share one: GLPK/GAMS end at `;`,
# Julia at a `)` block, Python at a dedent.
eac_equation <- function(code, class) {
  eq <- switch(class, technology = "eqTechEac", storage = "eqStorageEac",
               trade = "eqTradeEac")
  lines <- unlist(strsplit(paste(code, collapse = "\n"), "\n", fixed = TRUE))
  # Drop whole-line comments: "#" (GLPK/Julia/Python) and "*" in col 1 (GAMS).
  lines <- lines[!grepl("^[[:space:]]*#", lines) & !grepl("^[*]", lines)]
  i <- grep(eq, lines, fixed = TRUE)
  if (!length(i)) return(NA_character_)
  i <- i[1]
  # End just before the next equation definition, if any.
  nxt <- grep("eq[A-Z][A-Za-z]*(Eac|Inv|Fixom|Varom|Cap|Cost)", lines[-seq_len(i)])
  end <- if (length(nxt)) i + nxt[1] - 1L else length(lines)
  paste(lines[i:end], collapse = "\n")
}
