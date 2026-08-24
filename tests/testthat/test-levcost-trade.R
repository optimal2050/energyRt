# Levelized cost of transmission.
#
# Same parity contract as storage and technology, plus the two things that are
# specific to `trade` and that a plausible-looking implementation gets wrong:
# both endpoints pay the full capital charge, and `@varom` is inert in every
# backend and so must not be priced.

.lct_tol <- 5e-5

.lct_line <- function(...) {
  args <- list(
    name = "TRD_ELC", commodity = "ELC",
    routes = data.frame(src = c("R1", "R2"), dst = c("R2", "R1")),
    trade = data.frame(src = c("R1", "R2"), dst = c("R2", "R1"),
                       teff = 0.95, af.up = 0.6),
    invcost = data.frame(invcost = 800),
    fixom = data.frame(fixom = 12),
    vintage = data.frame(olife = 40L), cap2act = 8760)
  ov <- list(...)
  for (nm in names(ov)) args[[nm]] <- ov[[nm]]
  do.call(newTrade, args)
}

test_that("analytic equals solve on a lossy corridor, per year and NPV", {
  skip_if_no_solver()
  h <- newHorizon(period = 2020:2024)
  a <- levcost(.lct_line(), discount = 0.05, horizon = h, method = "analytic",
               verbose = FALSE)
  s <- suppressMessages(suppressWarnings(
    levcost(.lct_line(), discount = 0.05, horizon = h, method = "solve",
            verbose = FALSE)))
  expect_equal(unname(a$levcost_npv), unname(s$levcost_npv),
               tolerance = .lct_tol)
  expect_equal(a$levcost$levcost, s$levcost$levcost, tolerance = .lct_tol)
  m <- merge(a$cost_breakdown_npv, s$cost_breakdown_npv, by = "component")
  expect_setequal(m$component, c("eac", "fixom"))
  expect_equal(m$value.x, m$value.y, tolerance = .lct_tol)
})

test_that("LCOT matches the closed form computed by hand", {
  h <- newHorizon(period = 2020L)
  lc <- levcost(.lct_line(), discount = 0.05, horizon = h,
                method = "analytic", verbose = FALSE)
  crf <- energyRt:::.crf(0.05, 40)
  # BOTH endpoints pay, and the denominator is what one unit of capacity
  # actually delivers
  expect_equal(lc$levcost$levcost[1],
               2 * (800 * crf + 12) / (8760 * 0.6 * 0.95),
               tolerance = 1e-10)
})

test_that("both endpoints pay: splitting the rate between them changes nothing", {
  # An unregioned rate applies at EVERY endpoint, so 50 twice is the same
  # corridor as 60 and 40. Picking one endpoint would halve the answer.
  h <- newHorizon(period = 2020L)
  flat  <- .lct_line(invcost = data.frame(invcost = 50),
                     fixom = data.frame(fixom = 6))
  split <- .lct_line(invcost = data.frame(region = c("R1", "R2"),
                                          invcost = c(60, 40)),
                     fixom = data.frame(region = c("R1", "R2"),
                                        fixom = c(8, 4)))
  l1 <- levcost(flat,  discount = 0.05, horizon = h, method = "analytic",
                verbose = FALSE)
  l2 <- levcost(split, discount = 0.05, horizon = h, method = "analytic",
                verbose = FALSE)
  expect_equal(unname(l1$levcost_npv), unname(l2$levcost_npv),
               tolerance = 1e-10)
  # and it really is the sum, not one endpoint
  one <- levcost(.lct_line(invcost = data.frame(invcost = 25),
                           fixom = data.frame(fixom = 3)),
                 discount = 0.05, horizon = h, method = "analytic",
                 verbose = FALSE)
  expect_equal(unname(l1$levcost_npv), 2 * unname(one$levcost_npv),
               tolerance = 1e-10)
})

test_that("utilisation defaults to af.up and can be overridden", {
  h <- newHorizon(period = 2020L)
  from_slot <- levcost(.lct_line(), discount = 0.05, horizon = h,
                       method = "analytic", verbose = FALSE)
  explicit <- levcost(.lct_line(trade = data.frame(src = c("R1", "R2"),
                                                   dst = c("R2", "R1"),
                                                   teff = 0.95)),
                      utilisation = 0.6, discount = 0.05, horizon = h,
                      method = "analytic", verbose = FALSE)
  expect_equal(unname(from_slot$levcost_npv), unname(explicit$levcost_npv),
               tolerance = 1e-10)

  # halving utilisation doubles the cost per unit delivered
  half <- levcost(.lct_line(), utilisation = 0.3, discount = 0.05, horizon = h,
                  method = "analytic", verbose = FALSE)
  expect_equal(unname(half$levcost_npv), 2 * unname(from_slot$levcost_npv),
               tolerance = 1e-10)
  expect_error(levcost(.lct_line(), utilisation = 1.5, verbose = FALSE),
               "utilisation")
})

test_that("losses shrink the delivered denominator", {
  h <- newHorizon(period = 2020L)
  lossy <- levcost(.lct_line(), discount = 0.05, horizon = h,
                   method = "analytic", verbose = FALSE)
  clean <- levcost(.lct_line(trade = data.frame(src = c("R1", "R2"),
                                                dst = c("R2", "R1"),
                                                teff = 1, af.up = 0.6)),
                   discount = 0.05, horizon = h, method = "analytic",
                   verbose = FALSE)
  expect_equal(unname(lossy$levcost_npv), unname(clean$levcost_npv) / 0.95,
               tolerance = 1e-10)
})

test_that("@varom is refused with a warning, not silently priced", {
  # no backend reads pTradeVarom (glpk/energyRt.mod:535); pricing it here would
  # make the analytic engine disagree with every solver
  h <- newHorizon(period = 2020L)
  with_vo <- .lct_line(varom = data.frame(src = "R1", dst = "R2", varom = 5))
  expect_warning(
    lv <- levcost(with_vo, discount = 0.05, horizon = h, method = "analytic",
                  verbose = FALSE),
    "no backend reads|excluded")
  plain <- levcost(.lct_line(), discount = 0.05, horizon = h,
                   method = "analytic", verbose = FALSE)
  expect_equal(unname(lv$levcost_npv), unname(plain$levcost_npv),
               tolerance = 1e-10)
})

test_that("the priced direction is selectable and validated", {
  h <- newHorizon(period = 2020L)
  fwd <- levcost(.lct_line(), route = c("R1", "R2"), discount = 0.05,
                 horizon = h, method = "analytic", verbose = FALSE)
  expect_equal(fwd$levcost$region[1], "R1->R2")
  rev <- levcost(.lct_line(), route = c("R2", "R1"), discount = 0.05,
                 horizon = h, method = "analytic", verbose = FALSE)
  expect_equal(rev$levcost$region[1], "R2->R1")
  # symmetric corridor: same cost either way
  expect_equal(unname(fwd$levcost_npv), unname(rev$levcost_npv),
               tolerance = 1e-10)
  expect_error(levcost(.lct_line(), route = c("R1", "R9"), verbose = FALSE),
               "not among")
})

test_that("an asymmetric corridor is priced per direction", {
  h <- newHorizon(period = 2020L)
  asym <- .lct_line(trade = data.frame(src = c("R1", "R2"),
                                       dst = c("R2", "R1"),
                                       teff = c(0.95, 0.80),
                                       af.up = c(0.6, 0.6)))
  fwd <- levcost(asym, route = c("R1", "R2"), discount = 0.05, horizon = h,
                 method = "analytic", verbose = FALSE)
  rev <- levcost(asym, route = c("R2", "R1"), discount = 0.05, horizon = h,
                 method = "analytic", verbose = FALSE)
  expect_equal(unname(rev$levcost_npv), unname(fwd$levcost_npv) * 0.95 / 0.80,
               tolerance = 1e-10)
})

test_that("a container prices a trade across both endpoints, not one", {
  ELC <- newCommodity("ELC", timeframe = "ANNUAL")
  repo <- newRepository("REPO", ELC, .lct_line())
  h <- newHorizon(period = 2020L)
  from_repo <- levcost(repo, name = "TRD_ELC", discount = 0.05, horizon = h,
                       verbose = FALSE)
  direct <- levcost(.lct_line(), discount = 0.05, horizon = h,
                    method = "analytic", verbose = FALSE)
  # the container must NOT have region-subset the trade on its way in
  expect_equal(unname(from_repo$levcost_npv), unname(direct$levcost_npv),
               tolerance = 1e-10)
})

test_that("a container prices every class, and `classes` narrows it", {
  ELC <- newCommodity("ELC", timeframe = "ANNUAL")
  PP <- newTechnology(
    "PP", region = "R1", input = list(comm = "ELC"),
    output = list(comm = "HEAT"),
    ceff = data.frame(comm = c("ELC", "HEAT"), cinp2use = c(0.9, NA),
                      cact2cout = c(NA, 1)),
    invcost = data.frame(invcost = 100), af = data.frame(af.up = 0.9),
    vintage = data.frame(olife = 20L), cap2act = 1)
  repo <- newRepository("REPO", ELC, newCommodity("HEAT", timeframe = "ANNUAL"),
                        PP, .lct_line())
  h <- newHorizon(period = 2020L)

  all <- suppressMessages(suppressWarnings(
    levcost(repo, horizon = h, discount = 0.05, autocomplete = TRUE,
            verbose = FALSE)))
  expect_s3_class(all, "levcost_list")
  expect_setequal(names(all), c("PP", "TRD_ELC"))

  only <- levcost(repo, classes = "trade", horizon = h, discount = 0.05,
                  verbose = FALSE)
  expect_equal(names(only), "TRD_ELC")
  expect_equal(unname(only$TRD_ELC$levcost_npv),
               unname(all$TRD_ELC$levcost_npv), tolerance = 1e-10)

  expect_error(levcost(repo, classes = "supply"), "cannot price")
})
