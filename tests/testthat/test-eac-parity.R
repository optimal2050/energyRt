# =========================================================================== #
# invcost/eac parity, for every class that has a capacity and an annuity.
#
# `@invcost$eac` is documented as supplying the annuity directly, in place of
# invcost/wacc/olife. That contract held for technology and trade but NOT for
# storage: every backend gated the storage annuity on
#
#     and pStorageOutInvcost[...] <> 0
#
# so supplying `eac` alone dropped the capital cost entirely and the storage was
# built for free. eqTechEac and eqTradeEac never carried that gate.
#
# It surfaced converting PyPSA-Eur, whose `capital_cost` is already annuitised,
# so the converter supplies `eac` only: energyRt built 13.5 GW of battery at
# zero cost, inflating solar 2.27x. The model solved, reported OPTIMAL and
# reconciled its own objective -- only an external comparison exposed it.
# Hence this file asserts BEHAVIOUR (a solve), not just parameter values the
# way test-eac-input.R does.
# =========================================================================== #

# --- 1. template invariant: no annuity is gated on its investment cost -------
#
# Solver-free and always runs. This is the guard that catches the real-world
# failure mode: editing a template WITHOUT re-running data-raw/DATASET.R is a
# no-op, because the templates are baked into R/sysdata.rda. Reading .modelCode
# tests what the package will actually emit.
test_that("no eqXEac gates its annuity on the investment cost", {
  mc <- getFromNamespace(".modelCode", "energyRt")
  blocks <- intersect(c("GLPK", "GAMS", "JuMP", "PYOMOConcrete"), names(mc))
  expect_gt(length(blocks), 0)

  for (blk in blocks) {
    for (cls in c("technology", "storage", "trade")) {
      eq <- eac_equation(mc[[blk]], cls)
      if (is.na(eq)) next
      expect_false(grepl("Invcost", eq, fixed = TRUE),
                   info = paste0(blk, " / ", cls,
                                 ": annuity must not be gated on invcost"))
    }
  }
})

# --- 2. sysdata is in step with the on-disk templates ------------------------
#
# Source-tree only: glpk/ gams/ julia/ pyomo/ are .Rbuildignore'd, so an
# installed package cannot run this. Catches template/sysdata drift generally.
test_that("baked .modelCode matches the template files", {
  root <- NULL
  for (cand in c(".", "..", "../..", "../../..")) {
    if (file.exists(file.path(cand, "glpk", "energyRt.mod"))) { root <- cand; break }
  }
  skip_if(is.null(root), "template sources not available (installed package)")

  mc <- getFromNamespace(".modelCode", "energyRt")
  files <- c(GLPK = "glpk/energyRt.mod", GAMS = "gams/energyRt.gms",
             JuMP = "julia/energyRt.jl", PYOMOConcrete = "pyomo/energyRtConcrete.py")
  for (blk in intersect(names(files), names(mc))) {
    disk <- readLines(file.path(root, files[[blk]]), warn = FALSE)
    expect_identical(as.character(mc[[blk]]), as.character(disk),
                     info = paste(blk, "sysdata is stale -- re-run data-raw/DATASET.R"))
  }
})

# --- 3. behavioural parity: invcost and its equivalent eac agree -------------
for (cls in c("technology", "storage", "trade")) {
  test_that(paste0("invcost and an equivalent eac give the same answer (", cls, ")"), {
    skip_if_no_solver()
    p <- eac_pair(cls)

    s_inv <- vt_solve(vt_interp(p$inv, name = paste0("inv_", cls)))
    s_eac <- vt_solve(vt_interp(p$eac, name = paste0("eac_", cls)))

    v <- switch(cls, technology = "vTechEac", storage = "vStorageEac",
                trade = "vTradeEac")
    a_inv <- eac_var(s_inv, v)
    a_eac <- eac_var(s_eac, v)

    # The `> 0` half is what makes this a COST test. Without it, a dropped
    # annuity on both sides would read as "0 == 0" and pass.
    expect_gt(a_inv, 0)
    expect_gt(a_eac, 0)
    expect_equal(a_eac, a_inv, tolerance = 1e-8)
    expect_equal(eac_obj(s_eac), eac_obj(s_inv), tolerance = 1e-8)
  })
}
