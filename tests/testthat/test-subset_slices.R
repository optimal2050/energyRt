# =========================================================================== #
# Calendar SAMPLING validation.
#
# A sampled (reduced) calendar must reproduce the FULL model's objective exactly,
# because the slice weights annualise the sample (a subset calendar has
# year_fraction < 1; the top slice carries weight = 1/year_fraction, and the
# agg-rewrite aggregates fine->coarse via the scale-invariant ratio
# pSliceAgg = pSliceWeight[child]/pSliceWeight[parent], so the uniform expansion
# is self-consistent). This is the ground-truth check that cross-backend
# agreement CANNOT provide - all backends implement the same math, so they agree
# even if the weight scaling were wrong.
#
# Method: grouped-IDENTICAL slices. All fine slices (or all days) carry identical
# data, so solving at full resolution vs a subset of m<K representatives
# (year_fraction = m/K) must give the same objective.
#
# Standalone dev versions: dev-scripts/sampling_validation*.R
# =========================================================================== #

obj_glpk <- function(scen, tag) {
  td <- file.path(tempdir(), paste0("smpl_", tag))
  unlink(td, recursive = TRUE)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  s <- tryCatch(suppressWarnings(suppressMessages(
    solve_scenario(scen, solver = solver_options$glpk,
                   tmp.dir = td, tmp.del = TRUE, force = TRUE))),
    error = function(e) NULL)
  if (is.null(s)) return(NA_real_)
  getData(s, "vObjective", merge = TRUE)$value
}

# --------------------------------------------------------------------------- #
test_that("flat grouped-identical sampling reproduces the full objective", {
  skip_if(!nzchar(Sys.which("glpsol")) &&
            (is.null(get_glpk_path()) || !nzchar(get_glpk_path())),
          "GLPK (glpsol) not available")

  K  <- 12
  sl <- sprintf("s%02d", 1:K)
  tt <- make_timetable(struct = list(ANNUAL = "ANNUAL", SLICE = sl))

  mk <- function(cal) newModel("smpltest",
    repo = newRepository("r",
      newCommodity("COA", timeframe = "ANNUAL"),
      newCommodity("ELC", timeframe = "SLICE"),
      newSupply("SUP_COA", commodity = "COA",
                availability = data.frame(region = "R1", cost = 5)),
      newTechnology("ECOA", input = list(comm = "COA"), output = list(comm = "ELC"),
                    af = data.frame(af.up = 0.5),
                    invcost = data.frame(region = "R1", invcost = 1000),
                    olife = list(olife = 30), cap2act = 1),
      newDemand("DEM_ELC", commodity = "ELC",
                dem = data.frame(region = "R1", slice = sl, dem = 10))),
    calendar = cal, region = "R1", horizon = newHorizon(2020), discount = 0.05)

  scen_full <- suppressMessages(interpolate_model(mk(newCalendar(timetable = tt, name = "full")),
                                           "flat_full", ondisk = FALSE))
  of <- obj_glpk(scen_full, "flat_full")
  expect_false(is.na(of))

  for (m in c(2, 3, 4, 6)) {                 # single-element non-ANNUAL levels rejected
    tt1   <- tt[tt$SLICE %in% sl[seq_len(m)], ]
    cal_s <- newCalendar(timetable = tt1, name = paste0("s", m),
                         year_fraction = sum(tt1$share))
    scen  <- suppressMessages(interpolate_model(mk(cal_s), paste0("flat_s", m),
                                         cal_s, ondisk = FALSE))
    os <- obj_glpk(scen, paste0("flat_s", m))
    expect_equal(os, of, tolerance = 1e-6,
                 info = sprintf("sampled m=%d/%d (weight x%g)", m, K, K / m))
  }
})

# --------------------------------------------------------------------------- #
test_that("typical-day sampling (intra-day shape + storage) reproduces full objective", {
  skip_if(!nzchar(Sys.which("glpsol")) &&
            (is.null(get_glpk_path()) || !nzchar(get_glpk_path())),
          "GLPK (glpsol) not available")

  G <- 6; H <- 6
  day  <- paste0("d", 1:G)
  hour <- sprintf("h%02d", 0:(H - 1))        # ZERO-PADDED -> correct mSliceNext order
  shape <- c(2, 2, 4, 8, 6, 3)[1:H]          # peaky hourly demand shape
  tt <- make_timetable(struct = list(ANNUAL = "ANNUAL", DAY = day, HOUR = hour))
  dem_df <- do.call(rbind, lapply(day, function(d)
    data.frame(region = "R1", slice = paste0(d, "_", hour), dem = shape)))

  mk <- function(cal) newModel("typday",
    repo = newRepository("r",
      newCommodity("COA", timeframe = "ANNUAL"),
      newCommodity("ELC", timeframe = "HOUR"),
      newSupply("SUP_COA", commodity = "COA",
                availability = data.frame(region = "R1", cost = 5)),
      newTechnology("ECOA", input = list(comm = "COA"), output = list(comm = "ELC"),
                    invcost = data.frame(region = "R1", invcost = 800),
                    olife = list(olife = 30), cap2act = 1),
      newStorage("STG_ELC", commodity = "ELC",
                 invcost = list(invcost = 20), olife = list(olife = 15)),
      newDemand("DEM_ELC", commodity = "ELC", dem = dem_df)),
    calendar = cal, region = "R1", horizon = newHorizon(2020), discount = 0.05)

  scen_full <- suppressMessages(interpolate_model(mk(newCalendar(timetable = tt, name = "full")),
                                           "td_full", ondisk = FALSE))
  of <- obj_glpk(scen_full, "td_full")
  expect_false(is.na(of))

  for (m in c(2, 3)) {                        # keep m identical days (>=2)
    tt1   <- tt[tt$DAY %in% day[seq_len(m)], ]
    cal_s <- newCalendar(timetable = tt1, name = paste0("s", m),
                         year_fraction = sum(tt1$share))
    scen  <- suppressMessages(interpolate_model(mk(cal_s), paste0("td_s", m),
                                         cal_s, ondisk = FALSE))
    os <- obj_glpk(scen, paste0("td_s", m))
    expect_equal(os, of, tolerance = 1e-6,
                 info = sprintf("sampled %d/%d days (weight x%g)", m, G, G / m))
  }
})
