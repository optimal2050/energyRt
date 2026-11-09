# =========================================================================== #
# Interpolation rules: what each one does, per parameter, and under a user
# override.
#
# A rule is a dot-joined subset of `back` / `inter` / `forth`
# (`R/class-parameter.R:101-105`), applied over the YEAR axis only. Before this
# file the only rule test asserted a DIRECTION -- that overriding `dem` to
# `inter` covers fewer years than the default (`test-family-interp-overrides.R`
# :83, `expect_lt(length(yrs1), length(yrs0))`) -- so a rule that extrapolated
# the wrong way, or an override that silently did nothing, would have passed.
#
# Every expectation below is a measured year SET, taken from one anchor at the
# middle milestone of 2025 / 2030 / 2040 so that `back` and `forth` are
# distinguishable, plus a two-anchor case that shows `inter` itself.
#
# @covers pDemand depth=I forks=default
# =========================================================================== #

ir_horizon <- function() newHorizon(
  intervals = data.frame(start = c(2025L, 2026L, 2031L),
                         mid   = c(2025L, 2030L, 2040L),
                         end   = c(2025L, 2030L, 2040L)),
  force_BY_interval_to_1_year = FALSE, name = "ir_h")

# One demand, one anchor year (or two), one rule. `rule = NULL` keeps the
# shipped default so the default itself is pinned.
ir_model <- function(rule = NULL, anchors = 2030L, vals = 100) {
  m <- newModel("IR", region = "R1", discount = 0.05,
                calendar = energyRt::calendars$annual, horizon = ir_horizon(),
                data = newRepository("ir") |> add(
                  newCommodity("ELC", desc = "Electricity", unit = "GWh"),
                  newImport("IMP", desc = "Backstop", commodity = "ELC",
                            unit = "GWh", import = data.frame(price = 10)),
                  newDemand("DG", desc = "Demand under test",
                            commodity = "ELC", unit = "GWh",
                            demand = data.frame(year = anchors,
                                                timeslice = "ANNUAL",
                                                demand = vals))))
  if (!is.null(rule)) m@config@interpolation$pDemand <- rule
  m
}

ir_years <- function(rule = NULL, anchors = 2030L, vals = 100, tag) {
  scen <- suppressMessages(suppressWarnings(interpolate_model(
    ir_model(rule, anchors, vals), name = tag, overwrite = TRUE,
    verbose = FALSE)))
  d <- tryCatch(as.data.frame(get_data_slot(
    scen@modInp@parameters[["pDemand"]], optional = TRUE)),
    error = function(e) NULL)
  list(years = if (is.null(d)) integer(0) else sort(unique(as.integer(d$year))),
       values = if (is.null(d)) numeric(0) else d$value[order(d$year)],
       effective = unique(scen@modInp@parameters[["pDemand"]]@interpolation))
}

# --------------------------------------------------------------------------- #

test_that("each rule covers exactly the years its tokens promise", {
  skip_if_tier_below("fast")
  # measured 2026-11-04; one anchor at 2030 over milestones 2025/2030/2040
  want <- list(
    "back.inter.forth" = c(2025L, 2030L, 2040L),   # the shipped default
    "inter.forth"      = c(2030L, 2040L),
    "back.inter"       = c(2025L, 2030L),
    "inter"            = 2030L,
    "back"             = c(2025L, 2030L),
    "forth"            = c(2030L, 2040L)
  )
  for (r in names(want)) {
    got <- ir_years(r, tag = paste0("ir_", gsub("[.]", "", r)))
    expect_equal(got$effective, r,
                 label = paste0("'", r, "': the override did not reach the ",
                                "parameter"))
    expect_equal(got$years, want[[r]],
                 label = paste0("'", r, "': covered years"))
    expect_true(all(abs(got$values - 100) < 1e-9),
                label = paste0("'", r, "': a filled year changed the value"))
  }
})

test_that("the shipped default extrapolates both ways", {
  skip_if_tier_below("fast")
  got <- ir_years(NULL, tag = "ir_default")
  expect_equal(got$effective, "back.inter.forth")
  expect_equal(got$years, c(2025L, 2030L, 2040L))
})

test_that("inter fills between two anchors and extrapolates past neither", {
  skip_if_tier_below("fast")
  # 50 at 2025 and 150 at 2040: the 2030 milestone is 5/12 of the way, so
  # 50 + (150 - 50) * 5 / 15 = 83.333...
  got <- ir_years("inter", anchors = c(2025L, 2040L), vals = c(50, 150),
                  tag = "ir_two_inter")
  expect_equal(got$years, c(2025L, 2030L, 2040L))
  expect_equal(got$values, c(50, 250 / 3, 150), tolerance = 1e-6)
})

test_that("an override to a narrower rule removes the years it no longer fills", {
  skip_if_tier_below("fast")
  # The upgrade of test-family-interp-overrides.R:83 from a direction to a set.
  base <- ir_years(NULL, tag = "ir_ov_base")
  narrow <- ir_years("inter", tag = "ir_ov_narrow")
  expect_equal(base$years, c(2025L, 2030L, 2040L))
  expect_equal(narrow$years, 2030L)
  expect_setequal(setdiff(base$years, narrow$years), c(2025L, 2040L))
})

# --------------------------------------------------------------------------- #
# Metadata: the override table against the catalogue.

ir_cat <- function() {
  mp <- energyRt:::.param_interp_map()
  do.call(rbind, lapply(names(mp), function(k) {
    e <- mp[[k]]
    parts <- strsplit(k, "\r", fixed = TRUE)[[1]]
    data.frame(class = parts[1], slot = parts[2], col = parts[3],
               param = if (is.null(e$param)) NA_character_ else e$param,
               rule = if (is.null(e$rule) || !length(e$rule)) NA_character_
                      else as.character(e$rule)[1],
               stringsAsFactors = FALSE)
  }))
}

# @covers depth=C
test_that("an empty rule means no interpolation at all, not `inter`", {
  skip_if_tier_below("fast")
  # Ruled 2026-11-09. `.interp_rule_token()` no longer coerces "" to "inter":
  # with no tokens `.interp_one_series()` leaves `approx_rule` at c(1, 1) and
  # `interp_within` FALSE, so the declared years stand and every other year
  # falls to the parameter's default. Deliberate for a storage initial charge,
  # which is a starting condition rather than a series to extend.
  one <- ir_years("", tag = "ir_empty_one")
  expect_equal(one$years, 2030L)

  # the behavioural change: two anchors stay two rows. `inter` would fill
  # 2030 with 250/3; nothing does.
  two <- ir_years("", anchors = c(2025L, 2040L), vals = c(50, 150),
                  tag = "ir_empty_two")
  expect_equal(two$years, c(2025L, 2040L))
  expect_equal(two$values, c(50, 150), tolerance = 1e-9)
  expect_false(2030L %in% two$years)

  # and `inter` on the same anchors still fills, so the two are distinguishable
  inter <- ir_years("inter", anchors = c(2025L, 2040L), vals = c(50, 150),
                    tag = "ir_empty_vs_inter")
  expect_equal(inter$years, c(2025L, 2030L, 2040L))

  # the two that can fire. The ten `*StockNew` / `*StockSurv` parameters also
  # ship "" but are filled by `compute_stock_parameters()` AFTER
  # `interpolate_parameters()`, so their rule never applies and they carry no
  # bound column for `.param_interp_map()` to key.
  cat_tbl <- ir_cat()
  empty <- cat_tbl[!is.na(cat_tbl$rule) & cat_tbl$rule == "", ]
  expect_setequal(empty$param, c("pStorageNCap2Stg", "pStorageStartLevel"))
})

# @covers depth=C
test_that("every parameter's rule is overridable, by parameter name", {
  # Until 0.92 an override was keyed by COLUMN name and only worked where that
  # column had a key in the `.defInt` baseline, which 103 of 190 catalogued
  # columns did not -- the whole storage inp.* / out.* / stg.* family among
  # them. The key is the parameter now, so reachability is total by
  # construction and this test holds it there.
  cp <- energyRt:::.col_param
  pars <- unique(cp$parameter[!is.na(cp$parameter)])
  expect_gt(length(pars), 150L)

  mi <- energyRt:::.modInp
  keyed <- unlist(lapply(pars, function(nm) {
    e <- mi[[nm]]
    if (identical(e$type, "bounds")) paste0(nm, c(".lo", ".up")) else nm
  }))
  expect_false(anyDuplicated(keyed) > 0)

  # the storage family that used to be unreachable now has keys like any other
  # (all three are numpar, so the key is the bare parameter name)
  expect_true(all(c("pStorageStgInvcost", "pStorageOutInvcost",
                    "pStorageInpInvcost") %in% keyed))

  # and a key that names no parameter is refused rather than ignored
  m <- ir_model()
  m@config@interpolation$af.up <- "inter"
  expect_error(
    suppressMessages(suppressWarnings(interpolate_model(
      m, name = "ir_badkey", overwrite = TRUE, verbose = FALSE))),
    "keyed by parameter name"
  )
})

# --------------------------------------------------------------------------- #
# The vocabulary. Canonical `back`/`inter`/`forth`; every alias accepted and
# normalised at the two doors into a rule, so the engines match exact tokens.
# Until 0.92 the token set was defined three times -- a literal strip in the
# validator, a `for|fwd|frw` regex in the modern engine, literal greps in the
# legacy one -- and the aliases the documentation promised errored.

# @covers depth=C
test_that("every accepted spelling normalises to the canonical rule", {
  n <- energyRt:::.normalise_interp_rule
  expect_equal(n("back.inter.forth"), "back.inter.forth")
  expect_equal(n("bwd.mid.fwd"), "back.inter.forth")
  expect_equal(n("bwd.int.fwd"), "back.inter.forth")
  expect_equal(n("forward.backward"), "back.forth")   # canonical ORDER too
  expect_equal(n("forw"), "forth")
  expect_equal(n("frw"), "forth")
  # `.inter.` was `pTechStock`'s shipped spelling: empty tokens, tolerated by
  # the old substring-stripping validator
  expect_equal(n(".inter."), "inter")
  # an empty rule stays empty -- `.interp_rule_token()` resolves it downstream
  expect_equal(n(""), "")
  expect_true(is.na(n(NA_character_)))
  expect_error(n("sideways"), "Unknown interpolation token")
  expect_error(n("back.sideways"), "sideways")
})

# @covers depth=C
test_that("every catalogued rule is canonical, and a rule is stored canonical", {
  rs <- unlist(lapply(energyRt:::.modInp, function(e) e$interpolation))
  rs <- rs[!is.na(rs)]
  expect_equal(unname(energyRt:::.normalise_interp_rule(rs)), unname(rs),
               label = "the catalogue ships canonical spellings")
  expect_setequal(unique(rs), c("back.inter.forth", "inter.forth", "inter", ""))

  # whatever spelling is given, the parameter stores canonical
  p <- newParameter("pAliasNumpar", c("region", "year"), type = "numpar",
                    defVal = 0, interpolation = "bwd.mid.fwd")
  expect_equal(p@interpolation, "back.inter.forth")
  b <- newParameter("pAliasBounds", c("region", "year"), type = "bounds",
                    defVal = c(0, 1), interpolation = c("bwd.fwd", "mid"))
  expect_equal(b@interpolation, c("back.forth", "inter"))
  expect_error(
    newParameter("pBadRule", c("region", "year"), type = "numpar",
                 defVal = 0, interpolation = "sideways"),
    "Unknown interpolation token"
  )
})

# @covers pDemand depth=I forks=default
test_that("an alias override interpolates exactly as its canonical twin", {
  skip_if_tier_below("fast")
  # the override path writes `@interpolation` directly, bypassing the
  # constructor, so this is the integration that proves it normalises too
  canon <- ir_years("inter", tag = "ir_alias_canon")
  alias <- ir_years("mid", tag = "ir_alias_mid")
  expect_equal(alias$effective, "inter")
  expect_equal(alias$years, canon$years)
  expect_equal(alias$values, canon$values)

  wide <- ir_years("bwd.mid.fwd", tag = "ir_alias_wide")
  expect_equal(wide$effective, "back.inter.forth")
  expect_equal(wide$years, c(2025L, 2030L, 2040L))
})
