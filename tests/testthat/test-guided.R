# Guided perfect foresight (R/solve_guided.R + the target primitives in
# R/carry.R): cheap endpoint problems produce capacity TARGETS that steer an
# expensive full-horizon solve.
#
# The property that makes this testable in a way solve_myopic() is not: the
# final stage is one ordinary full-horizon solve, so its objective is directly
# comparable to true perfect foresight and the comparison has a known sign --
# guided >= PF, because bounds can only raise an optimum. Fixtures from
# helper-myopic.R (zero discount, one-year intervals, no intertemporal
# coupling), so a non-binding guide must reproduce PF exactly.

gd_path <- function(name) {
  gsub("[\\/]+", "/", file.path(tempdir(), "guided-suite", name))
}

gd_local <- function(name, env = parent.frame()) {
  unlink(gd_path(name), recursive = TRUE)
  old <- set_scenarios_path(gd_path(name))
  do.call(on.exit, list(bquote(set_scenarios_path(.(old))), add = TRUE),
          envir = env)
  invisible(NULL)
}

# --- window construction ---------------------------------------------------- #

test_that("guided_windows pairs each interior milestone with the endpoint", {
  h <- newHorizon(2020:2024, intervals = rep(1L, 5))
  w <- guided_windows(h)
  expect_length(w, 3L)                       # 2021, 2022, 2023 (not BY, not LY)
  expect_identical(vapply(w, function(x) x$focus, integer(1)),
                   c(2023L, 2022L, 2021L))   # backward by default
  expect_true(all(vapply(w, function(x) x$endpoint, integer(1)) == 2024L))
  # every window is exactly two milestones, the second being the endpoint
  for (x in w) {
    expect_identical(nrow(x$horizon@intervals), 2L)
    expect_identical(as.integer(x$horizon@intervals$mid[2]), 2024L)
  }
  expect_identical(vapply(guided_windows(h, direction = "forward"),
                          function(x) x$focus, integer(1)),
                   c(2021L, 2022L, 2023L))
  expect_length(guided_windows(h, include_base = TRUE), 4L)
})

test_that("a pair's focus interval is widened to span the gap", {
  # pPeriodLen is row-local: slicing rows out of a horizon would leave the
  # intervening years unweighted, so the focus row absorbs them.
  h <- newHorizon(2020:2024, intervals = rep(1L, 5))
  w <- guided_windows(h)
  pair_2021 <- Filter(function(x) x$focus == 2021L, w)[[1]]
  iv <- as.data.frame(pair_2021$horizon@intervals)
  expect_identical(as.integer(iv$start[1]), 2021L)
  expect_identical(as.integer(iv$end[1]), 2023L)   # widened up to the endpoint
  expect_equal(sum(iv$end - iv$start + 1L), 4)  # 2021..2024 fully covered

  # the base year is never widened: its one-year length is what commissions
  # the whole exogenous fleet there
  wb <- guided_windows(h, include_base = TRUE)
  base <- Filter(function(x) x$focus == 2020L, wb)[[1]]
  ivb <- as.data.frame(base$horizon@intervals)
  expect_identical(as.integer(ivb$end[1]), 2020L)
})

test_that("guided_windows refuses a single-milestone horizon", {
  expect_error(guided_windows(newHorizon(2020)), "at least two milestone")
})

# --- targets: extraction and application ------------------------------------ #

test_that("solution_targets harvests standing capacity per milestone", {
  skip_if_no_solver()
  gd_local("tg")
  yrs <- 2020:2022
  mod <- my_mod(years = yrs, olife = 50L, demand = 1, name = "tg")
  pf <- solve_scenario(interpolate_model(mod, name = "tg_pf"), echo = FALSE)

  tg <- solution_targets(pf)
  expect_s3_class(tg, "capacity_targets")
  expect_setequal(names(tg), c("family", "proc", "region", "year", "value"))
  expect_setequal(tg$year, yrs)
  expect_true(all(tg$family == "tech"))
  expect_equal(sort(tg$value), unname(my_by_year(pf, "vTechCap", yrs)))

  # a subset of years, and a refusal on an unsolved scenario
  expect_identical(nrow(solution_targets(pf, years = 2022L)), 1L)
  expect_error(solution_targets(interpolate_model(mod, name = "tg_ns")),
               "optimal solution")
})

test_that("apply_targets writes cap.lo rows with the slack applied", {
  # no solver needed: the target table is the contract, not the solve
  mod <- my_mod(years = 2020:2022, olife = 50L, demand = 1, name = "ap")
  h <- mod@config@horizon
  tg <- structure(
    tibble::tibble(family = "tech", proc = "TECH", region = "R1",
                   year = 2020:2022, value = c(10, 20, 30)),
    class = c("capacity_targets", class(tibble::tibble())))

  mod2 <- apply_targets(mod, tg, h, slack = 0.2)
  tech <- Filter(function(o) is(o, "technology"), mod2@data[[1]]@data)[[1]]
  cap <- as.data.frame(tech@capacity)
  expect_true("cap.lo" %in% names(cap))
  got <- cap$cap.lo[match(2020:2022, cap$year)]
  expect_equal(got, c(8, 16, 24))            # (1 - slack) * value
  expect_equal(cap$region[match(2020, cap$year)], "R1")

  # slack = 1 disables the guidance entirely
  expect_identical(apply_targets(mod, tg, h, slack = 1), mod)
  expect_error(apply_targets(mod, tg, h, slack = 2), "must be one number")

  # mode = "fx" writes the fixed column instead
  mod3 <- apply_targets(mod, tg, h, slack = 0, mode = "fx")
  t3 <- Filter(function(o) is(o, "technology"), mod3@data[[1]]@data)[[1]]
  expect_true("cap.fx" %in% names(as.data.frame(t3@capacity)))
})

test_that("the ncap basis divides by the period length", {
  # REGRESSION: `eqTechNewCapLo` compares vTechNewCap against
  # pTechNewCapLo * pPeriodLen, while vTechNewCap is ALREADY an annual rate.
  # A target written without dividing by plen asks for plen times too much --
  # 5x on a five-year milestone.
  mod <- my_mod(years = c(2020, 2025), olife = 50L, demand = 1, name = "pl")
  h <- newHorizon(intervals = data.frame(start = c(2020L, 2021L),
                                         mid = c(2020L, 2025L),
                                         end = c(2020L, 2025L)),
                  force_BY_interval_to_1_year = FALSE)
  expect_equal(as.numeric(h@intervals$end - h@intervals$start + 1L), c(1, 5))

  tg <- structure(
    tibble::tibble(family = "tech", proc = "TECH", region = "R1",
                   year = c(2020L, 2025L), value = c(10, 10)),
    class = c("capacity_targets", class(tibble::tibble())))

  cap_mod <- apply_targets(mod, tg, h, slack = 0, basis = "cap")
  ncap_mod <- apply_targets(mod, tg, h, slack = 0, basis = "ncap")
  cap_of <- function(m, col) {
    tt <- Filter(function(o) is(o, "technology"), m@data[[1]]@data)[[1]]
    d <- as.data.frame(tt@capacity)
    d[[col]][match(c(2020L, 2025L), d$year)]
  }
  expect_equal(cap_of(cap_mod, "cap.lo"), c(10, 10))    # fleet: no plen factor
  expect_equal(cap_of(ncap_mod, "ncap.lo"), c(10, 2))   # rate: divided by plen
})

test_that("apply_targets warns rather than silently dropping or over-binding", {
  mod <- my_mod(years = 2020:2022, olife = 50L, demand = 1, name = "wn")
  h <- mod@config@horizon
  mk <- function(years, value) structure(
    tibble::tibble(family = "tech", proc = "TECH", region = "R1",
                   year = as.integer(years), value = value),
    class = c("capacity_targets", class(tibble::tibble())))

  # a year that is not a milestone cannot bind: the bound parameter is
  # narrowed to milestones during interpolation
  expect_warning(apply_targets(mod, mk(c(2020, 2099), c(5, 5)), h),
                 "not\\s+milestones")
  # a target set stopping short of the horizon end still binds every later
  # milestone, because inter.forth forward-fills the last anchor
  expect_warning(apply_targets(mod, mk(2020, 5), h), "forward-filled")
  # a process the model does not contain
  expect_warning(apply_targets(mod, mk(2020, 5) |>
                                 (\(x) {x$proc <- "NOPE"; x})(), h),
                 "does not contain")
})

# --- the driver -------------------------------------------------------------- #

test_that("a non-binding guide reproduces perfect foresight exactly", {
  skip_if_no_solver()
  gd_local("eq")
  yrs <- 2020:2024
  mod <- my_mod(years = yrs, olife = 50L, demand = 1, name = "gq")

  pf <- solve_scenario(interpolate_model(mod, name = "gq_pf"), echo = FALSE)
  obj_pf <- as.numeric(getData(pf, "vObjective", merge = TRUE)$value[1])

  g <- solve_guided(mod, name = "gq_guided", verbose = FALSE)
  expect_s3_class(g, "guided")
  expect_false(g$failed)
  expect_true(all(g$stages$status == "solved"))
  expect_true(is(g$final, "scenario"))

  # the fixture has no intertemporal coupling, so the guide never binds
  expect_equal(my_by_year(g, "vTechCap", yrs), my_by_year(pf, "vTechCap", yrs))
  gg <- guided_gap(g, pf)
  expect_equal(gg$guided, obj_pf, tolerance = 1e-6)
  expect_equal(gg$gap, 0, tolerance = 1e-6)
})

test_that("the guided objective is never below perfect foresight", {
  skip_if_no_solver()
  gd_local("gap")
  yrs <- 2020:2024
  mod <- my_mod(years = yrs, olife = 50L, demand = 1, name = "gp")
  pf <- solve_scenario(interpolate_model(mod, name = "gp_pf"), echo = FALSE)
  obj_pf <- as.numeric(getData(pf, "vObjective", merge = TRUE)$value[1])

  # slack = 0 pins the guide hard against the endpoint's answer
  g <- solve_guided(mod, name = "gp_guided", slack = 0, verbose = FALSE)
  gg <- guided_gap(g, obj_pf)
  # bounds can only raise an optimum: the sign is structural, not empirical
  expect_gte(gg$gap, -1e-9)
})

test_that("a guided run stores one variant per stage and is reloadable", {
  skip_if_no_solver()
  gd_local("st")
  mod <- my_mod(years = 2020:2022, olife = 50L, demand = 1, name = "gs")
  g <- solve_guided(mod, name = "gs_guided",
                    stages = c("endpoint", "final"), verbose = FALSE)

  runs <- scenario_runs(g$final)
  expect_true(all(c("s1_endpoint", "s4_final") %in% runs$variant))

  # the driver metadata survives in variant.yml
  vy <- file.path(g$final@path, "runs", "s1_endpoint", "variant.yml")
  expect_true(file.exists(vy))
  mf <- yaml::read_yaml(vy)
  expect_identical(mf$type, "guided_stage")
  expect_identical(mf$stage, "endpoint")
  expect_identical(mf$sequence, "gs_guided")
})

test_that("getData routes to the final solve and errors without one", {
  skip_if_no_solver()
  gd_local("gdta")
  mod <- my_mod(years = 2020:2022, olife = 50L, demand = 1, name = "gt")
  g <- solve_guided(mod, name = "gt_guided",
                    stages = c("endpoint", "final"), verbose = FALSE)
  expect_equal(getData(g, "vObjective", merge = TRUE)$value[1],
               getData(g$final, "vObjective", merge = TRUE)$value[1])

  g2 <- solve_guided(mod, name = "gt_noend", stages = "endpoint",
                     verbose = FALSE)
  expect_null(g2$final)
  expect_error(getData(g2, "vTechCap"), "no final full-horizon solve")
  expect_error(guided_gap(g2, 1), "no final full-horizon solve")
})

test_that("solve_guided refuses a horizon it cannot decompose", {
  mod <- my_mod(years = 2020, olife = 50L, demand = 1, name = "g1")
  expect_error(solve_guided(mod, name = "g1_guided"),
               "at least two milestone")
})
