# `unfold_parameter` must reverse a fold for FULL-SET-fallback parameters --
# settings / weather params folded over the COMPLETE region/slice/year set, with
# no per-entity membership (`pDiscountFactor`, `pDiscount`, `pSliceWeight`,
# `pWeather`, ...). Two bugs lived here, both surfaced by the GAMS write path
# (which unfolds + densifies a sparse/folded scenario):
#   (1) a full-set membership carries only the `dim` column, so the join on
#       shared keys expanded nothing and the wildcard stayed -- densify then
#       mis-filled the default (e.g. pDiscountFactor 0.78 -> 1.0). Fixed by
#       cross-joining the wild rows with every member.
#   (2) expanding a YEAR wildcard clashed `character` (the coerced wildcard
#       column) with `integer` (the membership year) in bind_rows. Fixed by
#       coercing the membership dim column to character.
# This guards the fold -> unfold -> densify round-trip the GAMS writer relies on.

.fu_find <- function(rel) {
  for (cand in c(rel, file.path("..", "..", rel))) if (file.exists(cand)) return(cand)
  NULL
}

test_that("fold -> unfold + densify recovers the dense build for full-set params", {
  tm <- .fu_find("data-raw/testing-models.R")
  skip_if(is.null(tm), "data-raw/testing-models.R not available")
  source(tm, local = TRUE)
  mod <- tm_weather()   # multi-year + repeated weather -> pWeather folds year

  dn <- suppressWarnings(suppressMessages(
    interp_mod(mod, name = "fu_dn", ondisk = FALSE, sparse = FALSE, fold = FALSE)))
  fo <- suppressWarnings(suppressMessages(
    interp_mod(mod, name = "fu_fo", ondisk = FALSE, sparse = TRUE,
               fold = c("region", "slice", "year"))))

  # The GAMS-write transform: unfold value params, densify defaults, trim.
  tr <- unfold_scenario_parameters(fo, dims = energyRt:::.foldable_dims,
                                   types = c("numpar", "bounds"))
  tr <- densify_parameters(tr)
  tr <- trim_parameters_by_maps(tr)

  sorted <- function(s, p) {
    d <- as.data.frame(get_data_slot(s@modInp@parameters[[p]]))
    if (is.null(d) || nrow(d) == 0) return(NULL)
    d <- d[do.call(order, lapply(d, as.character)), , drop = FALSE]
    rownames(d) <- NULL
    d
  }
  for (p in c("pDiscountFactor", "pDiscount", "pSliceWeight", "pWeather")) {
    expect_true(isTRUE(all.equal(sorted(dn, p), sorted(tr, p),
                                 check.attributes = FALSE)),
                info = paste("full-set param round-trip:", p))
  }

  # No wildcard NA survives in a value parameter, and `year` stays integer
  # (the unfold's transient character coercion must be re-normalised).
  pw <- as.data.frame(get_data_slot(tr@modInp@parameters$pWeather))
  expect_false(anyNA(pw$year))
  expect_false(anyNA(pw$region))
  expect_true(is.integer(pw$year))
})
