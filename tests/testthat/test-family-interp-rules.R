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
  if (!is.null(rule)) m@config@interpolation$dem <- rule
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
  # fill nothing at all is a per-parameter ruling -- see
  # dev/interp-empty-rule-review.md. This test fails loudly if that changes.
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
test_that("the override table and the catalogue agree, and the gap is stated", {
  defint <- energyRt:::.defInt
  cat_tbl <- ir_cat()
  cols <- sort(unique(cat_tbl$col))

  # 1. Reachability. A rule can only be overridden where the column has a key
  # in `.defInt` (`.apply_settings_param_overrides()` compares against that
  # baseline and skips a key it does not hold). 103 of 190 catalogued colNames
  # have none -- the entire storage `inp.*` / `out.*` / `stg.*` family among
  # them -- so those rules are effectively fixed. Pinned so the set shrinks
  # deliberately. (`demand` is reachable despite being listed: the catalogue
  # names the column `demand` while the override key is the alias `dem`.)
  unreachable <- setdiff(cols, names(defint))
  expect_equal(length(unreachable), 103L)
  expect_true(all(c("stg.invcost", "out.invcost", "inp.invcost", "stg.stock",
                    "out.cap.up", "inp.cap.up") %in% unreachable),
              label = "the storage triplet family is no longer unreachable")

  # 2. Drift. Where a column IS overridable, its `.defInt` baseline must match
  # the rule the catalogue gives its parameters, or an override silently
  # compares against the wrong baseline. Exactly one column drifts today:
  # `stock`, because `pTechStock` ships `.inter.` while `pTradeStock` and
  # `.defInt` both say `back.inter.forth`.
  drift <- Filter(Negate(is.null), lapply(intersect(cols, names(defint)),
    function(c) {
      rules <- unique(cat_tbl$rule[cat_tbl$col == c])
      rules <- rules[!is.na(rules)]
      if (!length(rules) || all(rules == as.character(defint[[c]]))) NULL else c
    }))
  expect_equal(unlist(drift), "stock")
})
