# Build TOPIA's per-region input profiles from measured 2025 series.
#
# Sources (maintainer's machine only; the built object is committed):
#   wind, solar  reneuro cf-workflow cf_cells_{onwind,solar}.parquet, hourly
#   inflow       GloFAS avg_dis, daily, five largest rivers per territory
#   load         reneuro load_base_2025 per country, hourly
#
# Geography: eleven sites on a land transect from the Black Sea coast to the
# Irish Sea. Each site owns the cells nearest it (Voronoi, 2.2 deg cap); its
# wind and solar series are the mean of the twelve best cells in that
# territory. One continental clock (UTC) is kept, so the 28.5 deg of longitude
# appear as a ~1.9 h solar phase shift between the ends of the chain.

CFDIR <- "C:/Users/admin/Documents/R/reneuro/data-raw/cf-workflow/outputs"
GFDIR <- "C:/Users/admin/Documents/R/reneuro/data-raw/glofas"
LDDIR <- "C:/Users/admin/Documents/R/reneuro/projects/countries/inputs"

REG <- c("W1", "W2", "C1", "C2", "C3", "C4", "C5", "C6", "E1", "E2", "E3")
SITE <- data.frame(
  region = REG,
  lon = c(25.6, 22.8, 20.5, 18.0, 15.5, 13.0, 10.5, 6.0, 4.5, 0.5, -7.0),
  lat = c(40.9, 42.7, 44.3, 45.5, 46.8, 48.0, 49.3, 50.4, 51.8, 52.4, 53.3),
  cc = c("GR", "BG", "RS", "HR", "SI", "AT", "DE", "BE", "NL", "GB", "IE"),
  stringsAsFactors = FALSE)
MAXD <- 2.2   # deg, cap on a territory's radius
NTOP <- 12    # best cells per territory, per resource
NRIV <- 5     # rivers per territory
HYDCF <- 0.354  # annual mean inflow factor; GloFAS gives the shape, not a level
SEP <- 0.5    # deg, minimum separation between rivers

HOURS <- seq(as.POSIXct("2025-01-01 00:00", tz = "UTC"),
             as.POSIXct("2025-12-31 23:00", tz = "UTC"), by = "hour")
DAYS <- seq(as.Date("2025-01-01"), as.Date("2025-12-31"), by = "day")
stopifnot(length(HOURS) == 8760L, length(DAYS) == 365L)

.own <- function(lon, lat) {
  d <- sqrt(((SITE$lon - lon) * cospi(lat / 180))^2 + (SITE$lat - lat)^2)
  c(which.min(d), min(d))
}

.fill <- function(v) {
  if (!anyNA(v)) return(v)
  stats::approx(seq_along(v)[!is.na(v)], v[!is.na(v)], seq_along(v),
                rule = 2)$y
}

# ---- wind and solar ---------------------------------------------------------

.cf_sites <- function() {
  cells <- as.data.frame(arrow::read_parquet(
    file.path(CFDIR, "cells_eu.parquet")))
  ok <- Reduce(intersect, lapply(c("onwind", "solar"), function(nm)
    as.data.frame(dplyr::collect(dplyr::distinct(arrow::open_dataset(
      file.path(CFDIR, paste0("cf_cells_", nm, ".parquet"))),
      cell_id)))$cell_id))
  cells <- cells[cells$cell_id %in% ok, ]
  D <- sapply(seq_len(nrow(SITE)), function(i)
    sqrt(((cells$lon - SITE$lon[i]) * cospi(SITE$lat[i] / 180))^2 +
           (cells$lat - SITE$lat[i])^2))
  cells$own <- apply(D, 1, which.min)
  cells$dmin <- apply(D, 1, min)
  cells <- cells[cells$dmin <= MAXD, ]
  out <- list()
  for (nm in c("onwind", "solar")) {
    ann <- as.data.frame(dplyr::collect(dplyr::summarise(dplyr::group_by(
      arrow::open_dataset(file.path(CFDIR, paste0("cf_cells_", nm,
                                                  ".parquet"))),
      cell_id), cf = mean(cf))))
    sel <- list()
    for (i in seq_len(nrow(SITE))) {
      a <- ann[ann$cell_id %in% cells$cell_id[cells$own == i], ]
      a <- a[order(-a$cf), ][seq_len(min(NTOP, nrow(a))), ]
      sel[[i]] <- data.frame(region = SITE$region[i], cell_id = a$cell_id,
                             ann_cf = a$cf, stringsAsFactors = FALSE)
    }
    out[[nm]] <- do.call(rbind, sel)
  }
  out
}

.cf_series <- function(nm, map) {
  x <- as.data.frame(dplyr::collect(dplyr::select(dplyr::filter(
    arrow::open_dataset(file.path(CFDIR, paste0("cf_cells_", nm, ".parquet"))),
    cell_id %in% map$cell_id), cell_id, time, cf)))
  x$region <- map$region[match(x$cell_id, map$cell_id)]
  a <- stats::aggregate(cf ~ region + time, data = x, FUN = mean)
  m <- matrix(NA_real_, 8760L, length(REG), dimnames = list(NULL, REG))
  for (r in REG) {
    v <- a[a$region == r, ]
    m[match(as.POSIXct(v$time, tz = "UTC"), HOURS), r] <- v$cf
    m[, r] <- .fill(m[, r])
  }
  m
}

# ---- inflow -----------------------------------------------------------------

.rivers <- function() {
  u <- ncdf4::nc_open(file.path(GFDIR, "uparea_glofas_v4_0.nc"))
  on.exit(ncdf4::nc_close(u))
  ULON <- u$dim$longitude$vals
  ULAT <- u$dim$latitude$vals
  out <- list()
  for (i in seq_len(nrow(SITE))) {
    ix <- which(abs(ULON - SITE$lon[i]) <= 2.5)
    iy <- which(abs(ULAT - SITE$lat[i]) <= 2.5)
    a <- ncdf4::ncvar_get(u, "uparea", start = c(min(ix), min(iy)),
                          count = c(length(ix), length(iy)))
    a <- matrix(as.vector(a), nrow = length(ix))
    a[is.na(a)] <- -Inf
    got <- list()
    tries <- 0L
    while (length(got) < NRIV && tries < 4000L) {
      tries <- tries + 1L
      k <- which.max(a)
      if (!is.finite(a[k])) break
      px <- ULON[min(ix) + ((k - 1L) %% length(ix))]
      py <- ULAT[min(iy) + ((k - 1L) %/% length(ix))]
      o <- .own(px, py)
      if (o[1] == i && o[2] <= MAXD) {
        got[[length(got) + 1L]] <- data.frame(region = SITE$region[i],
          lon = px, lat = py, uparea_km2 = round(a[k] / 1e6, 0))
        a[abs(ULON[min(ix):max(ix)] - px) <= SEP,
          abs(ULAT[min(iy):max(iy)] - py) <= SEP] <- -Inf
      } else {
        a[k] <- -Inf
      }
    }
    out[[i]] <- do.call(rbind, got)
  }
  do.call(rbind, out)
}

.inflow_series <- function(rv) {
  n1 <- ncdf4::nc_open(file.path(GFDIR, "glofas_dis24_2025_01.nc"))
  GLON <- n1$dim$longitude$vals
  GLAT <- n1$dim$latitude$vals
  ncdf4::nc_close(n1)
  rv$gx <- sapply(rv$lon, function(v) which.min(abs(GLON - v)))
  rv$gy <- sapply(rv$lat, function(v) which.min(abs(GLAT - v)))
  rv$riv <- seq_len(nrow(rv))
  acc <- list()
  for (m in 1:12) {
    n <- ncdf4::nc_open(file.path(GFDIR,
                                  sprintf("glofas_dis24_2025_%02d.nc", m)))
    tv <- as.Date(as.POSIXct(n$dim$valid_time$vals, origin = "1970-01-01",
                             tz = "UTC"))
    for (j in rv$riv) {
      v <- ncdf4::ncvar_get(n, "avg_dis", start = c(rv$gx[j], rv$gy[j], 1),
                            count = c(1, 1, length(tv)))
      acc[[length(acc) + 1L]] <- data.frame(region = rv$region[j], riv = j,
        date = tv, dis = as.numeric(v))
    }
    ncdf4::nc_close(n)
  }
  d <- do.call(rbind, acc)
  d <- d[d$date %in% DAYS, ]
  d$sh <- stats::ave(d$dis, d$riv, FUN = function(x) x / mean(x))
  a <- stats::aggregate(sh ~ region + date, data = d, FUN = mean)
  m <- matrix(NA_real_, 365L, length(REG), dimnames = list(NULL, REG))
  for (r in REG) {
    v <- a[a$region == r, ]
    m[match(v$date, DAYS), r] <- v$sh
    m[, r] <- .fill(m[, r])
    # inflow above nameplate is spilled, so the factor is capped rather than
    # rescaled; the annual mean then falls below HYDCF in flashy regions
    m[, r] <- pmin(1, HYDCF * m[, r] / mean(m[, r]))
  }
  m
}

# ---- load -------------------------------------------------------------------

.load_series <- function() {
  m <- matrix(NA_real_, 8760L, length(REG), dimnames = list(NULL, REG))
  for (i in seq_len(nrow(SITE))) {
    v <- utils::read.csv(file.path(LDDIR, SITE$cc[i], "load_base_2025.csv"))
    tt <- as.POSIXct(v$utc, tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ")
    m[match(tt, HOURS), i] <- v$mw
    m[, i] <- .fill(m[, i])
    m[, i] <- m[, i] / mean(m[, i])
  }
  m
}

# ---- reduce to the kit's calendars ------------------------------------------

YDAY <- as.integer(format(HOURS, "%j"))
HOUR <- as.integer(format(HOURS, "%H"))

.keys <- function(calendar) {
  if (calendar == "d365_h24") {
    sprintf("d%03d_h%02d", YDAY, HOUR)
  } else {
    energyRt:::.topia_timeslice_key(YDAY, HOUR, calendar)
  }
}

.reduce <- function(m, calendar) {
  key <- .keys(calendar)
  do.call(rbind, lapply(REG, function(r) {
    agg <- stats::aggregate(m[, r], by = list(timeslice = key), FUN = mean)
    data.frame(region = r, timeslice = agg$timeslice, value = agg$x,
               stringsAsFactors = FALSE)
  }))
}

topia_real_profiles <- function(
    calendars = c("d365_h24", "s4_h24", "m12_h24", "topia_seasons")) {
  maps <- .cf_sites()
  W <- .cf_series("onwind", maps$onwind)
  S <- .cf_series("solar", maps$solar)
  rv <- .rivers()
  Hd <- .inflow_series(rv)
  H <- Hd[YDAY, , drop = FALSE]        # daily inflow held over the day
  L <- .load_series()
  wx <- list()
  dx <- list()
  for (cal in calendars) {
    wx[[cal]] <- rbind(
      data.frame(calendar = cal, resource = "WSOL", .reduce(S, cal)),
      data.frame(calendar = cal, resource = "WWIN", .reduce(W, cal)),
      data.frame(calendar = cal, resource = "WHYD", .reduce(H, cal)))
    dx[[cal]] <- data.frame(calendar = cal, .reduce(L, cal))
  }
  weather <- do.call(rbind, wx)
  names(weather)[names(weather) == "value"] <- "wval"
  demand <- do.call(rbind, dx)
  names(demand)[names(demand) == "value"] <- "load"
  sites <- merge(SITE, stats::aggregate(ann_cf ~ region, maps$onwind, mean),
                 by = "region", all.x = TRUE)
  names(sites)[names(sites) == "ann_cf"] <- "wind_cf"
  sites <- merge(sites, stats::aggregate(ann_cf ~ region, maps$solar, mean),
                 by = "region", all.x = TRUE)
  names(sites)[names(sites) == "ann_cf"] <- "solar_cf"
  sites <- sites[match(REG, sites$region), ]
  list(
    weather = weather[, c("calendar", "resource", "region", "timeslice",
                          "wval")],
    demand = demand[, c("calendar", "region", "timeslice", "load")],
    sites = sites,
    rivers = rv,
    hourly = list(WSOL = S, WWIN = W, WHYD = H, load = L, hours = HOURS))
}

# ── the committed artefact ──────────────────────────────────────────────────
# The sources above live on the maintainer's machine only, so this script
# writes its result next to itself and data-raw/topia_data.R reads that. The
# dataset can therefore be rebuilt anywhere; only REFRESHING the sample needs
# the archives. Run with ENERGYRT_TOPIA_SAMPLE=refresh to rebuild it.
if (identical(Sys.getenv("ENERGYRT_TOPIA_SAMPLE"), "refresh")) {
  .p <- topia_real_profiles(calendars = "d365_h24")
  topia_sample <- list(weather = .p$weather, demand = .p$demand)
  saveRDS(topia_sample,
          "data-raw/topia_profiles_sample.rds", compress = "xz")
  cat("wrote data-raw/topia_profiles_sample.rds:",
      nrow(topia_sample$weather), "weather rows,",
      nrow(topia_sample$demand), "demand rows\n")
  rm(.p)
}
