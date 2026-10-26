# =========================================================================== #
# Coverage-matrix integrity: the generator assembles from package metadata and
# every `# @covers` tag in the test suite resolves to a known row name.
# The generator lives in tools/coverage/build_matrix.R (Rbuildignored), so
# these tests skip on an installed package without the source tree.
# =========================================================================== #

.matrix_tool <- (function() {
  candidates <- c(
    testthat::test_path("..", "..", "tools", "coverage", "build_matrix.R"),
    file.path("tools", "coverage", "build_matrix.R")
  )
  for (p in candidates) if (file.exists(p)) return(normalizePath(p))
  NULL
})()

skip_if_no_matrix_tool <- function() {
  if (is.null(.matrix_tool)) {
    testthat::skip("tools/coverage/build_matrix.R not available (installed package)")
  }
}

.matrix_env <- new.env(parent = globalenv())
.matrix_loaded <- FALSE
load_matrix_tool <- function() {
  if (!.matrix_loaded) {
    sys.source(.matrix_tool, envir = .matrix_env)
    assign(".matrix_loaded", TRUE, envir = parent.env(environment()))
  }
  .matrix_env
}

test_that("matrix rows assemble from package metadata with expected arithmetic", {
  skip_if_no_matrix_tool()
  env <- load_matrix_tool()
  rows <- env$assemble_rows()
  counts <- table(rows$kind)
  # 513 .modInp entries (15 set + 309 map + 151 numpar + 38 bounds)
  # + 150 equations + 95 variables = 758
  # (15 sets = 13 + `process`, the union of the seven process classes, declared
  # on every backend and the GAMS parent of tech/stg/sup/dem/expp/imp/trade)
  # (297 = 285 + the 12 mStorage*Comm{SameTimeslice,Agg,AggTimeslice} maps
  # added with the storage coarse-timeframe aggregation fix, 2026-08-25;
  # 308/141/38/150 = + inp2stg (1 bound, 3 maps, 2 equations) + the per-part
  # aux capacity couplings (12 params and maps replacing 4), 2026-09-03;
  # 309 maps = + mWeatherRegionAt, which shares a parent-level weather series
  # down to its child regions -- read by the af/afs/afc equations on all four
  # backends, 2026-09-21)
  # (15 sets = + `levy`, the shared index of tax and subsidy instruments; it
  # re-dimensions the six pTaxCost*/pSubCost* parameters rather than adding any
  # -- 2026-10-08)
  # (151 numpar = 141 + the ten pTaxProc*/pSubProc* rate tables that carry a
  # levy onto a process: act/inp/out/cap/ncap for each of tax and subsidy,
  # 2026-10-08)
  # (313 maps / 151 equations = + meqTechAfFx / eqTechAfFx, a fixed availability
  # bound as one equality row under `fx_equ`, 2026-10-16)
  # (345 maps / 182 equations = + the Fx map and equation of 32 more bound
  # families under `fx_equ`; eqTradeIrAfFx, like eqTradeIrAfLo, has no
  # description entry and is not counted, 2026-10-16)
  # (348 maps = + mStorageOutCap, mStorageOutNew, mStorageInpOutCap: the
  # discharger exists only with data, 2026-10-17)
  # (352 maps = + meqTechAInp/AOut and meqStorageAInp/AOut, the equation
  # domains of the aux flows: a process coarser than its aux commodity holds
  # the variables in the finer cells beneath it while the equation stays on
  # its own cell, so the two domains part company. No equation is added --
  # the four aux equations gained a summed term, 2026-10-18)
  # (159 numpars = + pTaxProc{Inp,Out}{Cap,Ncap} and pSubProc{Inp,Out}{Cap,Ncap},
  # levies on a storage's charger and discharger, 2026-10-26)
  # (355 maps, 39 bounds = + meqTradeIrAfsLo/Up/Fx and pTradeIrAfs, a route's
  # flow over a parent timeslice, 2026-10-26)
  # (359 maps, 41 bounds = + mTradeWeatherAf*/Afs* Lo/Up and pTradeWeatherAf/Afs,
  # weather factors of the route ratings, 2026-10-26)
  expect_equal(unname(counts[["set"]]), 15)
  expect_equal(unname(counts[["map"]]), 359)
  expect_equal(unname(counts[["numpar"]]), 159)
  expect_equal(unname(counts[["bounds"]]), 41)
  expect_equal(unname(counts[["equation"]]), 182)
  expect_gte(unname(counts[["variable"]]), 93)
  expect_false(any(duplicated(rows$name[rows$kind != "variable"])))
  # every bounds row expands to a Lo/Up pXxx pair
  b <- rows[rows$kind == "bounds", ]
  expect_true(all(grepl("Lo,.*Up$", b$pXxx_expanded)))
  # no numeric parameter falls through to a fallback family (maps/eqs/vars may)
  np <- rows[rows$kind %in% c("numpar", "bounds"), ]
  expect_false(any(grepl("^other", np$family)))
})

test_that("all @covers tags in the suite resolve to known names", {
  skip_if_no_matrix_tool()
  env <- load_matrix_tool()
  expect_true(isTRUE(env$check_tags(dir = testthat::test_path("."))))
})

# --------------------------------------------------------------------------- #
# Registry <-> template agreement.
#
# `.variable_mapping` (from data-raw/maps.R) says which variables exist and what
# guards their domain; the backend templates declare them. Nothing checked that
# the two agree, and the failure is silent in both directions: a registered name
# with no declaration reads back as an all-zero variable, and a declared name
# with no registration is written by the solver and never read.
#
# This is not hypothetical. `data-raw/variable_mapping.yml` is a dump that
# nothing reads (see data-raw/variables.yml), and reverse-engineering the
# storage capacity variables from equation text produced four invented names in
# place of six correct registrations. Either direction of this check catches
# that immediately.
# --------------------------------------------------------------------------- #

# Names declared by `var <name>{...}` / `var <name> >= 0;` in a MathProg model.
# Deliberately backreference-free: split on the characters that may follow a
# declared name instead.
.mp_declared_vars <- function(code) {
  l <- grep("^var ", code, value = TRUE)
  nm <- vapply(strsplit(trimws(substring(l, 5L)), "[{;, >=]"),
               function(x) x[nzchar(x)][1], character(1))
  unique(nm[!is.na(nm)])
}

test_that("every registered variable is declared in the GLPK template, and back", {
  registered <- names(energyRt:::.variable_mapping)
  declared <- .mp_declared_vars(energyRt:::.modelCode$GLPK)
  expect_gt(length(registered), 50L)   # guard against an empty-vs-empty pass
  expect_gt(length(declared), 50L)
  expect_equal(sort(setdiff(registered, declared)), character(0),
               label = "registered in .variable_mapping but not declared in GLPK")
  expect_equal(sort(setdiff(declared, registered)), character(0),
               label = "declared in GLPK but absent from .variable_mapping")
})

test_that("each registered variable's guard mapping is a declared set", {
  # `vX( dims ) $ mY( dims )` -- the guard `mY` must be a set the template
  # declares, or the variable's domain silently never resolves.
  mp <- energyRt:::.variable_mapping
  code <- energyRt:::.modelCode$GLPK
  sets <- vapply(strsplit(trimws(substring(
    grep("^set ", code, value = TRUE), 5L)), "[{;, ]"),
    function(x) x[nzchar(x)][1], character(1))
  sets <- unique(sets[!is.na(sets)])
  guards <- vapply(mp, function(s) {
    p <- strsplit(s, " $ ", fixed = TRUE)[[1]]
    if (length(p) < 2) return(NA_character_)
    trimws(strsplit(p[2], "(", fixed = TRUE)[[1]][1])
  }, character(1))
  guards <- guards[!is.na(guards) & nzchar(guards)]
  expect_gt(length(guards), 50L)
  expect_equal(sort(unique(setdiff(guards, sets))), character(0),
               label = "guard mappings named in .variable_mapping but never declared")
})
