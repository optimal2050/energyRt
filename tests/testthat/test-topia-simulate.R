# =========================================================================== #
# topia_simulate(): synthetic TOPIA weather and demand profiles.
#
# The function authors shapes, not levels: every field is scaled to the mean
# capacity factor the measured set records for that resource, cluster and
# province, so the first test below is the one that matters. If it ever needs
# a wide tolerance, the calibration has stopped being read.
# =========================================================================== #

ts_meas <- function() {
  w <- as.data.frame(topia$weather)
  w[as.character(w$dataset) == "sample", , drop = FALSE]
}

ts_key <- function(df) {
  paste(as.character(df$resource),
        ifelse(is.na(df$cluster), "", as.character(df$cluster)),
        as.character(df$region))
}

# Covers the R API: topia_simulate()
test_that("levels reproduce the measured calibration", {
  p <- topia_simulate(calendar = "s4_h24")
  meas <- tapply(ts_meas()$wval, ts_key(ts_meas()), mean)
  gen <- tapply(p$weather$wval, ts_key(p$weather), mean)
  cmp <- intersect(names(meas), names(gen))
  expect_gt(length(cmp), 40L)
  expect_equal(as.numeric(gen[cmp]), as.numeric(meas[cmp]), tolerance = 0.02)
})

test_that("the roster is the one the weather objects declare", {
  p <- topia_simulate(calendar = "s4_h24")
  got <- unique(paste(p$weather$resource,
                      ifelse(is.na(p$weather$cluster), "", p$weather$cluster)))
  expect_setequal(got, c("WHYD ", "WSOL FL", "WSOL TH",
                         "WWIN HI", "WWIN MD", "WWIN LO"))
  expect_true(all(p$weather$wval >= 0 & p$weather$wval <= 1))
  expect_false(anyNA(p$weather$wval))
  expect_false(anyNA(p$demand$load))
})

# Covers the R API: topia_simulate(clusters=)
test_that("a grade is a different resource, not another ceiling", {
  p <- topia_simulate(regions = c("R01", "R02"), calendar = "s4_h24")
  shp <- function(res, reg) {
    z <- p$weather[p$weather$resource == res & p$weather$region == reg, ]
    lapply(split(z, z$cluster), function(v) v$wval / mean(v$wval))
  }
  w <- shp("WWIN", "R01")
  expect_false(isTRUE(all.equal(w$HI, w$MD, tolerance = 1e-3)))
  s <- shp("WSOL", "R02")
  expect_false(isTRUE(all.equal(s$FL, s$TH, tolerance = 1e-3)))
})

test_that("clusters = FALSE gives one ungraded series per resource", {
  p <- topia_simulate(regions = "R01", calendar = "s4_h24", clusters = FALSE)
  expect_true(all(is.na(p$weather$cluster)))
  expect_setequal(unique(p$weather$resource), c("WSOL", "WWIN", "WHYD"))
})

test_that("a roster given by hand is honoured", {
  ros <- data.frame(resource = "WWIN", cluster = "HI", region = "R10",
                    stringsAsFactors = FALSE)
  p <- topia_simulate(regions = "R10", calendar = "s4_h24", clusters = ros)
  expect_equal(unique(p$weather$resource), "WWIN")
  expect_equal(unique(p$weather$cluster), "HI")
  expect_error(topia_simulate(clusters = data.frame(a = 1)), "data.frame of")
})

# Covers the R API: topia_simulate(calendar=)
test_that("the target calendar's own timetable places the shape", {
  expect_length(unique(topia_simulate(regions = "R01",
                                      calendar = "d365_h24")$weather$timeslice),
                8760L)
  expect_length(unique(topia_simulate(regions = "R01",
                                 calendar = "d365_h24_1dps")$weather$timeslice),
                96L)
  # A calendar with no hourly frame cannot carry a diurnal shape at all.
  expect_error(topia_simulate(regions = "R01", calendar = "m12"), "HOUR")
})

# Covers the R API: topia_simulate(jitter=), topia_simulate(seed=)
test_that("jitter is reproducible on a seed and moves the series", {
  a <- topia_simulate(regions = "R01", calendar = "s4_h24",
                      jitter = 0.08, seed = 1)
  b <- topia_simulate(regions = "R01", calendar = "s4_h24",
                      jitter = 0.08, seed = 1)
  c3 <- topia_simulate(regions = "R01", calendar = "s4_h24",
                       jitter = 0.08, seed = 2)
  expect_equal(a$weather$wval, b$weather$wval)
  expect_false(isTRUE(all.equal(a$weather$wval, c3$weather$wval)))
})

test_that("the frames carry what generated them", {
  p <- topia_simulate(regions = "R01", calendar = "s4_h24")
  expect_equal(attr(p$weather, "dataset"), "simulated")
  q <- topia_simulate(regions = "R01", calendar = "s4_h24",
                      wind_season = rep(1, 12))
  expect_match(attr(q$weather, "dataset"), "wind_season changed")
  expect_false(isTRUE(all.equal(
    sort(p$weather$wval[p$weather$resource == "WWIN"]),
    sort(q$weather$wval[q$weather$resource == "WWIN"]))))
})

test_that("a zone with no residential share is refused", {
  expect_error(topia_simulate(regions = "R01", calendar = "s4_h24",
                              residential = c(Z9 = 0.5)),
               "no share for zone")
})

# Covers the R API: build_topia(weather=) fed from topia_simulate()
test_that("the output feeds build_topia at the model's calendar", {
  skip_if_not_installed("geoscales")
  p <- topia_simulate(calendar = "d365_h24")
  k <- build_topia(weather = p$weather, demand = p$demand, quiet = TRUE)
  ob <- getObject(k$repo)
  expect_gt(nrow(ob$WWIN@weather), 0L)
  cf <- function(o) tapply(o$WWIN@weather$wval,
                           as.character(o$WWIN@weather$cluster), mean)
  expect_equal(as.numeric(cf(ob)),
               as.numeric(cf(getObject(topia$modules$electricity$repo))),
               tolerance = 0.02)
})
