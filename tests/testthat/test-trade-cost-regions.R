# Trade capacity is region-free -- one number per corridor -- but its costs are
# region-indexed, and the equations apply the FULL capacity in every region the
# cost names, with no share factor. That is the intended convention: `invcost`
# is what each endpoint bears, and the `region` column is how a user splits it.
#
# It has two edges the user cannot see, both measured here rather than asserted:
#   * an unregioned row broadcasts to every endpoint, charging the corridor twice
#   * a coarser geoscale level passes validation but never reaches the objective
#
# `test-eac-parity.R` compares the invcost form against the eac form, so a
# doubling that affects both is invisible to it. These are ABSOLUTE numbers.

tcr_gs <- function() {
  skip_if_not_installed("geoscales")
  geoscales::geoscale_from_leaftable(
    data.frame(nation = "NAT", region = c("R1", "R2")),
    geoframes = c("nation", "region"), key = "region", name = "tcr")
}

# A corridor of 100 / 80 / 0 over three one-year periods at fixom 1 per unit-year
# costs 180 if charged once. Nothing else in the model costs anything, so the
# objective IS the trade fixom -- which is what makes the factor readable.
tcr_model <- function(fx, name, geo = FALSE) {
  m <- newModel(
    name = name, region = c("R1", "R2"), horizon = sp_hor(),
    discount = 0, calendar = sp_cal(),
    repo = newRepository("tcr_repo",
      newCommodity("ELC", timeframe = "HOUR"),
      newTrade("TRD", commodity = "ELC", cap2act = 1,
               routes = data.frame(src = "R1", dst = "R2"),
               capacity = data.frame(year = SP_YEARS, stock = c(100, 80, 0)),
               fixom = fx, vintage = data.frame(olife = 50L)),
      newSupply("SUP", commodity = "ELC", supply = data.frame(cost = 1))))
  if (geo) setGeoscale(m, tcr_gs()) else m
}
tcr_obj <- function(fx, name, geo = FALSE) {
  sol <- suppressMessages(suppressWarnings(
    solve_scenario(interpolate_model(tcr_model(fx, name, geo), name = name))))
  getData(sol, "vObjective", merge = TRUE)$value[1]
}

test_that("a trade cost is borne by each endpoint named", {
  skip_if_no_solver()
  one   <- tcr_obj(data.frame(region = "R1", fixom = 1), "tcr1")
  split <- tcr_obj(data.frame(region = c("R1", "R2"), fixom = c(0.5, 0.5)), "tcr2")
  full  <- tcr_obj(data.frame(region = c("R1", "R2"), fixom = c(1, 1)), "tcr3")

  expect_equal(one, 180)     # charged at one endpoint
  expect_equal(split, 180)   # split across two -- the documented way to say 180
  expect_equal(full, 360)    # 1 at EACH endpoint really is twice the cost
  expect_equal(full, 2 * one)
})

test_that("an unregioned trade cost is reported, and a named one is silent", {
  skip_if_no_solver()
  # Reported, not refused. An unregioned row is the rate applied at every
  # endpoint -- the same rate a region subset would see -- so it is a legitimate
  # declaration that is merely easy to misread as a corridor total.
  expect_message(
    interpolate_model(tcr_model(data.frame(fixom = 1), "tcr4"), name = "tcr4"),
    "applies at every endpoint")
  expect_no_message(
    interpolate_model(
      tcr_model(data.frame(region = c("R1", "R2"), fixom = c(0.5, 0.5)), "tcr5"),
      name = "tcr5"),
    message = "applies at every endpoint")
})

test_that("the per-endpoint rate is what makes a region subset bear its own share", {
  skip_if_no_solver()
  # The property the whole convention rests on, stated as arithmetic rather than
  # prose: the rate is per endpoint, so dropping endpoints drops their cost and
  # keeps the survivors' -- which is what a partial-region study wants.
  one  <- tcr_obj(data.frame(region = "R1", fixom = 1), "tcr8")
  both <- tcr_obj(data.frame(region = c("R1", "R2"), fixom = c(1, 1)), "tcr9")
  expect_equal(both, 2 * one)
  # A four-endpoint corridor at the same rate would cost 4x, and any two-endpoint
  # subset of it 2x -- i.e. exactly `both`. The scaling IS the subsetting rule.
})

test_that("a cost at a coarse geoscale level is charged once, at that cell", {
  skip_if_no_solver()
  skip_if_not_installed("geoscales")
  # Until `mvTotalCost` carried coarse cells this cost was computed into
  # vTradeFixom and then dropped -- the corridor was silently FREE, measured at 0.
  # It now costs the same as naming one child region: charged once, at NAT.
  expect_equal(tcr_obj(data.frame(region = "NAT", fixom = 1), "tcr6", TRUE), 180)
  expect_equal(tcr_obj(data.frame(region = "R1", fixom = 1), "tcr6b", TRUE), 180)
  expect_message(
    interpolate_model(tcr_model(data.frame(region = "NAT", fixom = 1), "tcr7", TRUE),
                      name = "tcr7"),
    "charged ONCE at that cell")
})

test_that("a coarse cost lands in one cell, not the parent AND every child", {
  skip_if_no_solver()
  skip_if_not_installed("geoscales")
  # The failure this guards against is the obvious one: adding coarse rows to
  # mvTotalCost could make eqCost visit the parent as well as its children and
  # count the same cost twice. It must appear at NAT only.
  sol <- suppressMessages(solve_scenario(interpolate_model(
    tcr_model(data.frame(region = "NAT", fixom = 1), "tcr10", TRUE), name = "tcr10")))
  d <- as.data.frame(getData(sol, "vTradeFixom", merge = TRUE))
  d <- d[d$value != 0, ]
  expect_setequal(unique(as.character(d$region)), "NAT")
  # two years carry capacity (100 and 80); the third is 0
  expect_equal(sort(d$value), c(80, 100))
})

test_that("a coarse cost survives a dense scenario and every back-end", {
  skip_if_no_solver()
  skip_if_not_installed("geoscales")
  # `sparse = FALSE` is the GAMS path, and it is stricter: the sparse writer
  # folds a duplicated parameter row away, while GLPK rejects it outright
  # ("pDiscountFactor[R1,2020] already defined"). Extending the discount factor
  # to a coarse cell has to APPEND only the new rows -- `.dat2par()` appends --
  # and this is the case that catches getting that wrong.
  mod <- tcr_model(data.frame(region = "NAT", fixom = 1), "tcrs", TRUE)
  for (sp in c(TRUE, FALSE)) {
    sc <- suppressMessages(suppressWarnings(
      interpolate_model(mod, name = paste0("tcrs", sp), sparse = sp)))
    expect_equal(getData(solve_scenario(sc), "vObjective", merge = TRUE)$value[1],
                 180, info = paste("sparse =", sp))
  }
})

test_that("a coarse cell inherits its children's discount rate, or refuses", {
  skip_if_not_installed("geoscales")
  # eqObjective weights every mvTotalCost row by pDiscountFactor[r,y]. A coarse
  # cell has no rate of its own. Agreeing children are inherited from; disagreeing
  # ones have no defensible answer, so energyRt names them rather than averaging.
  # `config@discount` carries both rates by region and year; `sdr` is the one
  # that discounts the objective.
  rates <- function(r1, r2) {
    expand.grid(region = c("R1", "R2"), year = SP_YEARS,
                stringsAsFactors = FALSE) |>
      within({
        wacc <- 0
        sdr <- ifelse(region == "R1", r1, r2)
      })
  }
  same <- tcr_model(data.frame(region = "NAT", fixom = 1), "tcrd1", TRUE)
  same@config@discount <- rates(0.05, 0.05)
  expect_no_error(suppressMessages(interpolate_model(same, name = "tcrd1")))

  diff <- tcr_model(data.frame(region = "NAT", fixom = 1), "tcrd2", TRUE)
  diff@config@discount <- rates(0.05, 0.10)
  expect_error(
    suppressMessages(interpolate_model(diff, name = "tcrd2")),
    "do not share one discount factor")
})

test_that("the trade slot docs no longer deny implemented features", {
  # All three of `@capacity`, `@fixom` and `@varom` were documented as
  # "(not implemented!)" while all three were live. `@varom` took the longest to
  # pin down: a dead parameter `pTradeVarom` shared its name, and the slot is
  # implemented under `pTradeIrCost` / `pTradeIrMarkup`.
  for (slot in c("capacity", "fixom", "varom")) {
    expect_false(grepl("not implemented", get_slot_doc("trade", slot),
                       fixed = TRUE), info = slot)
  }
  # and the conventions are written down where a user will meet them
  expect_match(get_slot_doc("trade", "invcost"), "endpoint")
  expect_match(get_slot_doc("trade", "varom"), "PER ROUTE")
})
