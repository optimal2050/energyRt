# =========================================================================== #
# The objective weight: pObjWeight[region, year].
#
# `eqObjective` sums `vTotalCost * pPeriodLen * pDiscountFactor * pObjWeight`
# over the cells of `mvTotalCost`, so a per-year QUALITY weight and a
# per-region PRIORITY weight are one more factor on the cell the objective
# already sums over -- not two features. The two halves are declared apart and
# multiply: `horizon@intervals$weight` and `config@objweight`.
#
# The property these tests lean on is within-cell neutrality: the weight scales
# the WHOLE cost cell, so a UNIFORM weight rescales the objective exactly and
# changes no decision. That makes the whole pipeline -- two declarations, the
# product, four solver templates, the recompute -- checkable against an exact
# number rather than a direction.
# =========================================================================== #

source(test_path("fixtures", "testing-models.R"))

.ow_mod <- function() tm_core()
.ow_mid <- function(m) as.integer(getHorizon(m)@intervals$mid)

.ow_scen <- function(m, name) {
  suppressWarnings(suppressMessages(
    interpolate_model(m, name = name, overwrite = TRUE, verbose = FALSE)))
}
.ow_solve <- function(m, name, solver = "glpk") {
  suppressWarnings(suppressMessages(
    solve_scenario(.ow_scen(m, name), solver = solver, verbose = FALSE)))
}
.ow_obj <- function(s) as.numeric(get_variable(s, "vObjective",
                                               data = TRUE)$value[1])
.ow_par <- function(s) {
  d <- as.data.frame(get_data_slot(s@modInp@parameters[["pObjWeight"]]))
  if (is.null(d) || !nrow(d)) return(d)
  d[order(d$region, d$year), , drop = FALSE]
}


# --------------------------------------------------------------------------- #
# 1. the declarations
# --------------------------------------------------------------------------- #

test_that("the year half is written on the horizon's intervals", {
  m <- .ow_mod()
  mid <- .ow_mid(m)
  m <- setObjectiveWeights(m, years = stats::setNames(0.5, mid[2]))
  iv <- as.data.frame(getHorizon(m)@intervals)
  expect_true("weight" %in% names(iv))
  expect_equal(iv$weight[iv$mid == mid[2]], 0.5)
  expect_equal(iv$weight[iv$mid == mid[1]], 1)
  # a second call keeps the first: the column is merged, not replaced
  m <- setObjectiveWeights(m, years = stats::setNames(0.8, mid[1]))
  iv <- as.data.frame(getHorizon(m)@intervals)
  expect_equal(iv$weight[order(iv$mid)], c(0.8, 0.5))
})

test_that("an unnamed year weight is one per milestone, or one for all", {
  m <- .ow_mod()
  n <- nrow(as.data.frame(getHorizon(m)@intervals))
  expect_equal(as.data.frame(getHorizon(
    setObjectiveWeights(m, years = 0.4))@intervals)$weight, rep(0.4, n))
  expect_error(setObjectiveWeights(m, years = rep(0.4, n + 1L)),
               "one weight per milestone")
})

test_that("a year the horizon has no milestone for is refused", {
  m <- .ow_mod()
  expect_error(setObjectiveWeights(m, years = c("1999" = 0.5)),
               "no milestone for")
})

test_that("the region half is written on the config", {
  m <- setObjectiveWeights(.ow_mod(), regions = c(R2 = 0.6))
  ow <- m@config@objweight
  expect_equal(nrow(ow), 1L)
  expect_equal(ow$region, "R2")
  expect_true(is.na(ow$year))
  expect_equal(ow$weight, 0.6)
  # a data.frame carries the year axis too
  m <- setObjectiveWeights(.ow_mod(), regions = data.frame(
    region = "R1", year = .ow_mid(.ow_mod())[1], weight = 0.9))
  expect_equal(m@config@objweight$weight, 0.9)
  expect_false(is.na(m@config@objweight$year))
})

test_that("a region the model does not declare is refused", {
  expect_error(setObjectiveWeights(.ow_mod(), regions = c(R9 = 0.5)),
               "does not declare")
  expect_error(setObjectiveWeights(.ow_mod(), regions = 0.5),
               "must be named by region")
})

test_that("a weight of zero or less is refused, by either route", {
  m <- .ow_mod()
  mid <- .ow_mid(m)
  for (w in c(0, -1)) {
    expect_error(setObjectiveWeights(m, years = stats::setNames(w, mid[1])),
                 "must be finite and > 0")
    expect_error(setObjectiveWeights(m, regions = c(R1 = w)),
                 "must be finite and > 0")
  }
  # and the horizon refuses it even when written straight into `intervals`
  iv <- as.data.frame(getHorizon(m)@intervals)
  iv$weight <- 0
  expect_error(energyRt:::.check_intervals(iv), "must be finite and > 0")
})

test_that("reset clears a half, and both", {
  m <- setObjectiveWeights(.ow_mod(), years = 0.5, regions = c(R2 = 0.6))
  m0 <- setObjectiveWeights(m, reset = TRUE)
  expect_false("weight" %in% names(as.data.frame(getHorizon(m0)@intervals)))
  expect_equal(nrow(m0@config@objweight), 0L)
  # one half only
  m1 <- setObjectiveWeights(m, years = 0.7, reset = TRUE)
  expect_equal(as.data.frame(getHorizon(m1)@intervals)$weight[1], 0.7)
  expect_equal(nrow(m1@config@objweight), 0L)
})


# --------------------------------------------------------------------------- #
# 2. the product
# --------------------------------------------------------------------------- #

test_that("an undeclared model writes no weight at all", {
  # The neutrality gate: nothing declared means no rows, every backend reads
  # the defVal of 1, and the scenario is what it would have been before the
  # weight existed.
  s <- .ow_scen(.ow_mod(), "ow0")
  p <- s@modInp@parameters[["pObjWeight"]]
  expect_equal(as.numeric(p@defVal[[1]]), 1)
  expect_equal(nrow(.ow_par(s)), 0L)
})

test_that("the two halves multiply into pObjWeight", {
  m <- .ow_mod()
  mid <- .ow_mid(m)
  m <- setObjectiveWeights(m, years = stats::setNames(0.5, mid[2]),
                           regions = c(R2 = 0.6))
  d <- .ow_par(.ow_scen(m, "ow1"))
  got <- stats::setNames(d$value, paste(d$region, d$year))
  expect_equal(got[[paste("R1", mid[2])]], 0.5)      # year half alone
  expect_equal(got[[paste("R2", mid[1])]], 0.6)      # region half alone
  expect_equal(got[[paste("R2", mid[2])]], 0.3)      # the product
  # (R1, first milestone) is 1, which sparse interpolation drops as the default
  expect_false(paste("R1", mid[1]) %in% names(got))
})

test_that("a more specific region row wins over a wildcard", {
  m <- .ow_mod()
  mid <- .ow_mid(m)
  m <- setObjectiveWeights(m, regions = data.frame(
    region = c(NA, "R2"), year = c(NA_integer_, NA_integer_),
    weight = c(0.9, 0.4)))
  d <- .ow_par(.ow_scen(m, "ow2"))
  got <- stats::setNames(d$value, paste(d$region, d$year))
  expect_equal(got[[paste("R1", mid[1])]], 0.9)
  expect_equal(got[[paste("R2", mid[1])]], 0.4)
})

test_that("the same cell declared twice is refused, not ordered", {
  m <- .ow_mod()
  m@config@objweight <- data.frame(region = c("R1", "R1"),
                                   year = c(NA_integer_, NA_integer_),
                                   weight = c(0.5, 0.7))
  expect_error(.ow_scen(m, "ow3"), "declares the same cell twice")
})

test_that("a declaration reaching no cell of the model is reported", {
  m <- .ow_mod()
  m@config@objweight <- data.frame(region = "R1", year = 1999L, weight = 0.5)
  expect_warning(
    suppressMessages(interpolate_model(m, name = "ow4", overwrite = TRUE,
                                       verbose = FALSE)),
    "matching no")
})


test_that("a weight declared after a base still reaches the derived scenario", {
  # The weight lives on the model (config + horizon), so a derived build cannot
  # inherit the base's unweighted parameter: the model it is given is a
  # different model.
  m <- .ow_mod()
  base <- .ow_scen(m, "owb0")
  expect_equal(nrow(.ow_par(base)), 0L)
  m2 <- setObjectiveWeights(m, regions = c(R2 = 0.6))
  d <- suppressWarnings(suppressMessages(interpolate_model(
    m2, name = "owb1", base = base, overwrite = TRUE, verbose = FALSE)))
  w <- .ow_par(d)
  expect_equal(nrow(w), 2L)
  expect_true(all(w$region == "R2"))
  expect_true(all(w$value == 0.6))
})


# --------------------------------------------------------------------------- #
# 3. the solve
# --------------------------------------------------------------------------- #

test_that("a uniform weight rescales the objective exactly", {
  skip_if_no_solver()
  # The whole-cell property, as an exact number: if every cell scales by w,
  # nothing inside any cell changes, so the optimum is the same plan and the
  # objective is w times the plain one. This is the end-to-end check of the two
  # declarations, the product, the template factor and the solve together.
  plain <- .ow_obj(.ow_solve(.ow_mod(), "owU0"))
  for (w in c(0.5, 2)) {
    m <- setObjectiveWeights(.ow_mod(), regions = c(R1 = w, R2 = w))
    # the label must be name-safe: a scenario name is a directory name
    lab <- paste0("owU", sub("[.]", "p", format(w)))
    expect_equal(.ow_obj(.ow_solve(m, lab)), w * plain,
                 tolerance = 1e-6, info = paste("w =", w))
  }
  # the same through the year half
  m <- setObjectiveWeights(.ow_mod(), years = 0.5)
  expect_equal(.ow_obj(.ow_solve(m, "owUy")), 0.5 * plain, tolerance = 1e-6)
})

test_that("the solved objective is the weighted sum, not the plain one", {
  skip_if_no_solver()
  m <- .ow_mod()
  mid <- .ow_mid(m)
  m <- setObjectiveWeights(m, years = stats::setNames(0.5, mid[2]),
                           regions = c(R2 = 0.6))
  s <- .ow_solve(m, "owS")
  ow <- objective_weights(s)
  expect_equal(ow$objective, ow$weighted, tolerance = 1e-6)
  expect_true(ow$weighted < ow$unweighted)   # every weight here is below 1
  expect_equal(ow$ratio, ow$weighted / ow$unweighted)
  expect_true(ow$weights_declared)
})

test_that("verify_solution checks the objective WITH the weight", {
  skip_if_no_solver()
  # Without the factor in the check, every weighted run would report a
  # violation; with it, the identity holds to solver tolerance.
  m <- setObjectiveWeights(.ow_mod(), years = 0.5, regions = c(R2 = 0.6))
  v <- verify_solution(.ow_solve(m, "owV"), verbose = FALSE)
  d <- v$divergence
  expect_equal(d$status[d$check == "objective"], "ok")
  expect_equal(d$n_violated[d$check == "objective"], 0L)
})

test_that("objective_weights reports the unweighted NPV of the plan it solved", {
  skip_if_no_solver()
  # The weight is a device for uneven resolution, so the number to report is
  # the same sum without it. It is a DECODE of the solution, not a second
  # solve: `vTotalCost` is exported per cell.
  plain <- objective_weights(.ow_solve(.ow_mod(), "owN0"))
  expect_equal(plain$weighted, plain$unweighted)
  expect_false(plain$weights_declared)
  # a uniform weight changes no decision, so the unweighted NPV is unchanged
  m <- setObjectiveWeights(.ow_mod(), regions = c(R1 = 0.5, R2 = 0.5))
  wtd <- objective_weights(.ow_solve(m, "owN1"))
  expect_equal(wtd$unweighted, plain$unweighted, tolerance = 1e-6)
  expect_equal(wtd$weighted, 0.5 * plain$unweighted, tolerance = 1e-6)
  expect_s3_class(wtd, "objective_weights")
  expect_output(print(wtd), "unweight")
})

test_that("objective_weights refuses what it cannot decode", {
  expect_error(objective_weights(.ow_mod()), "must be a scenario")
})


# --------------------------------------------------------------------------- #
# 4. the other backends
#
# Four templates carry the factor, so a weighted model must reach one objective
# on all of them. Tier "cross" -- opt-in, since each needs its own toolchain.
# --------------------------------------------------------------------------- #

test_that("every backend applies the weight the same way", {
  skip_if_no_solver()
  m <- setObjectiveWeights(.ow_mod(), years = 0.5, regions = c(R2 = 0.6))
  ref <- .ow_obj(.ow_solve(m, "owB0", solver = "glpk"))

  got <- list()
  try_backend <- function(label, guard, solver) {
    ok <- tryCatch({ guard(); TRUE },
                   condition = function(e) FALSE)
    if (!ok) return(invisible(NULL))
    v <- tryCatch(.ow_obj(.ow_solve(m, paste0("owB_", label),
                                    solver = solver)),
                  error = function(e) NA_real_)
    got[[label]] <<- v
  }
  try_backend("pyomo", skip_if_no_pyomo, solver_options$pyomo_cbc)
  try_backend("julia", skip_if_no_julia_highs, solver_options$julia_highs)
  try_backend("mm", skip_if_no_multimod, solver_options$mm_highs)

  if (!length(got)) skip("no second backend available in this tier")
  for (nm in names(got)) {
    expect_equal(got[[nm]], ref, tolerance = 1e-5, info = nm)
  }
})


# --------------------------------------------------------------------------- #
# 5. a coarse cost cell
# --------------------------------------------------------------------------- #

# A cost declared at a coarse region is one more `mvTotalCost` cell, and
# `eqObjective` weights it like any other. The coarse cell has no weight of its
# own, so it inherits its children's -- when they agree.
.ow_coarse <- function(name) {
  skip_if_not_installed("geoscales")
  gs <- geoscales::geoscale_from_leaftable(
    data.frame(nation = "NAT", region = c("R1", "R2")),
    geoframes = c("nation", "region"), key = "region", name = "ow")
  m <- newModel(
    name = name, region = c("R1", "R2"), horizon = sp_hor(),
    discount = 0, calendar = sp_cal(),
    repo = newRepository("ow_repo",
      newCommodity("ELC", timeframe = "HOUR"),
      newTrade("TRD", commodity = "ELC", cap2act = 1,
               routes = data.frame(src = "R1", dst = "R2"),
               capacity = data.frame(year = SP_YEARS, stock = c(100, 80, 0)),
               fixom = data.frame(region = "NAT", fixom = 1),
               vintage = data.frame(olife = 50L)),
      newSupply("SUP", commodity = "ELC", supply = data.frame(cost = 1))))
  setGeoscale(m, gs)
}

test_that("a coarse cost cell inherits one weight its children agree on", {
  m <- .ow_coarse("owc1")
  m <- setObjectiveWeights(m, regions = c(R1 = 0.5, R2 = 0.5))
  d <- .ow_par(.ow_scen(m, "owc1"))
  expect_true("NAT" %in% d$region)
  expect_equal(unique(d$value[d$region == "NAT"]), 0.5)
})

test_that("children that disagree are named rather than averaged", {
  m <- .ow_coarse("owc2")
  m <- setObjectiveWeights(m, regions = c(R1 = 0.5, R2 = 0.9))
  expect_error(.ow_scen(m, "owc2"), "do not share one objective weight")
})

test_that("a coarse cost cell with nothing declared stays silent", {
  m <- .ow_coarse("owc3")
  expect_equal(nrow(.ow_par(.ow_scen(m, "owc3"))), 0L)
})
