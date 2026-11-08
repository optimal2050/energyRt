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
test_that("an empty rule behaves as `inter`, which is what the catalogue means", {
  skip_if_tier_below("fast")
  # `.interp_rule_token()` (R/interpolation.R) coerces "" to "inter", so an
  # empty rule interpolates between anchors but extrapolates past neither.
  # PINNED, not endorsed: two catalogued parameters ship an empty rule
  # (`pStorageNCap2Stg`, `pStorageStartLevel`) and whether they should instead
  # fill nothing at all is a per-parameter ruling. This test fails loudly if
  # that changes.
  one <- ir_years("", tag = "ir_empty_one")
  expect_equal(one$years, 2030L)
  two <- ir_years("", anchors = c(2025L, 2040L), vals = c(50, 150),
                  tag = "ir_empty_two")
  expect_equal(two$values, c(50, 250 / 3, 150), tolerance = 1e-6)

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
