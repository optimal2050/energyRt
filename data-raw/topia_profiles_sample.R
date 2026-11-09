# Build TOPIA's per-region input profiles from measured 2025 series.
#
# Sources (maintainer's machine only; the built object is committed):
#   wind, solar  reneuro cf-workflow cf_cells_{onwind,solar}.parquet, hourly
#   inflow       GloFAS avg_dis, daily, five largest rivers per territory
#   load         reneuro load_base_2025 per country, hourly
#
# Geography: twelve sites. Eleven lie on a land transect from the Black Sea
# coast to the Irish Sea; the twelfth sits well to the south-west of them, the
# arid end of the set, and is far enough from the transect that it shares no
# cell with it. Each site owns the cells nearest it (Voronoi, 2.2 deg cap);
# its wind and solar series are the mean of the twelve best cells in that
# territory -- one rule for every site, which is what keeps the resource
# levels comparable across regions. One continental clock (UTC) is kept, so
# longitude appears as a real solar phase shift along the chain.
#
# Sites are keyed by the MODEL's region codes, assigned so that each frozen
# zone is a coherent group of archetypes: `R01`-`R03` the arid west,
# `R04`-`R06` firm and industrial, `R07`-`R09` the resource belt, `R10`-`R12`
# the windy end. `R01` is the remote region and carries no transmission.

CFDIR <- "C:/Users/admin/Documents/R/reneuro/data-raw/cf-workflow/outputs"
GFDIR <- "C:/Users/admin/Documents/R/reneuro/data-raw/glofas"
LDDIR <- "C:/Users/admin/Documents/R/reneuro/projects/countries/inputs"

REG <- sprintf("R%02d", 1:12)

# Twelve territories, chosen so each ZONE of the frozen geoscale is one
# archetype: `Z1` the windy dark island that carries no wire, `Z2` the sun,
# `Z3` and the inland half of `Z4` the becalmed interior, and the coastal half
# of `Z4` with `Z5` the windy end. Sites are placed to produce that, then the
# resource grades below are READ OFF the cells rather than assigned.
SITE <- data.frame(
  region = REG,
  lon = c(-2.0, -8.5, -3.5, 14.0,  9.5, 11.5, 21.0, 24.0, 15.5,  9.0, -4.5, -9.0),
  lat = c(59.5, 39.5, 38.5, 49.0, 45.5, 44.5, 42.5, 42.5, 54.0, 56.5, 55.5, 53.5),
  # `cc` picks the measured LOAD series only, so it is chosen for a plausible
  # distinct shape rather than to match the cell territory exactly: the island
  # takes a northern winter-peaking profile, and no two regions share one.
  cc = c("NO", "PT", "ES", "CZ", "CH", "IT", "RS", "BG", "PL", "DK", "GB", "IE"),
  stringsAsFactors = FALSE)
stopifnot(nrow(SITE) == 12L, !anyDuplicated(SITE$cc))

# ---- resource grades --------------------------------------------------------
# Wind grades are ABSOLUTE bands on a cell's own annual capacity factor, the
# same thresholds everywhere. A per-region tier would call a becalmed region's
# best sites "HI", which is not what a grade means; with absolute bands a
# region simply has no HI cells. Measured over the 9,003 cells carrying both
# resources: 336 are HI, 944 MD, the rest LO.
WIND_BANDS <- c(HI = 0.45, MD = 0.30)   # lower edge of each; below MD is LO
NMIN <- 5L                              # cells a band needs to be worth a cluster

# What each region offers. The bands above decide what EXISTS; this table is
# the design on top of them -- the windy coast and the island carry no
# low-grade sites, and the belt carries no mid-grade, so the grades are not
# merely a quality ladder repeated in every region.
WIND_CL <- list(
  R01 = c("HI", "MD"), R02 = c("MD", "LO"), R03 = "LO", R04 = "LO",
  R05 = "LO",          R06 = "LO",          R07 = "LO", R08 = "LO",
  R09 = c("HI", "LO"), R10 = "HI",          R11 = c("HI", "MD"),
  R12 = c("HI", "MD"))

# Solar is a TECHNOLOGY pair, not a resource grade. Within a territory the
# best twelve cells and the next twelve differ by 1-6%, so grading cells would
# give two near-identical clusters -- the correlated-shapes defect. Fixed tilt
# against horizontal single-axis tracking is +20.9% on the SAME cells, with a
# broader shoulder and a flatter midday. (`cf_cells_solar_opt` is not a third
# option: it matches the fixed file to -0.1%, so the shipped file is already
# optimally tilted.)
SOLAR_SRC <- c(FIX = "solar", TRK = "hsat")
SOLAR_MIN <- 0.14      # best-12 CF below which tracking is not offered

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
  src <- unique(c("onwind", unname(SOLAR_SRC)))
  ok <- Reduce(intersect, lapply(src, function(nm)
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

  ann <- function(nm) as.data.frame(dplyr::collect(dplyr::summarise(
    dplyr::group_by(arrow::open_dataset(
      file.path(CFDIR, paste0("cf_cells_", nm, ".parquet"))), cell_id),
    cf = mean(cf))))

  # WIND: band every owned cell by its own annual CF, then take the best NTOP
  # within each band the region is declared to offer. A declared band with
  # fewer than NMIN cells is dropped and reported -- an empty cluster would
  # reach the model as a technology that can never run.
  aw <- ann("onwind")
  wsel <- list()
  for (i in seq_len(nrow(SITE))) {
    r <- SITE$region[i]
    a <- aw[aw$cell_id %in% cells$cell_id[cells$own == i], ]
    a$band <- ifelse(a$cf >= WIND_BANDS[["HI"]], "HI",
                     ifelse(a$cf >= WIND_BANDS[["MD"]], "MD", "LO"))
    for (cl in WIND_CL[[r]]) {
      v <- a[a$band == cl, ]
      if (nrow(v) < NMIN) {
        warning("wind cluster ", cl, " dropped in ", r, ": ", nrow(v),
                " cells, below NMIN = ", NMIN, call. = FALSE)
        next
      }
      navail <- nrow(v)        # the band's whole stock, not just the best NTOP
      v <- v[order(-v$cf), ][seq_len(min(NTOP, nrow(v))), ]
      wsel[[length(wsel) + 1L]] <- data.frame(
        region = r, cluster = cl, cell_id = v$cell_id, ann_cf = v$cf,
        avail = navail, stringsAsFactors = FALSE)
    }
  }

  # SOLAR: the same cells under two technologies. The cell pick is made once,
  # on the fixed-tilt file, so FIX and TRK stand on the same ground and differ
  # only by what is mounted there.
  as_ <- ann(SOLAR_SRC[["FIX"]])
  ssel <- list()
  for (i in seq_len(nrow(SITE))) {
    r <- SITE$region[i]
    a <- as_[as_$cell_id %in% cells$cell_id[cells$own == i], ]
    navail <- nrow(a)
    a <- a[order(-a$cf), ][seq_len(min(NTOP, nrow(a))), ]
    cls <- if (mean(a$cf) >= SOLAR_MIN) names(SOLAR_SRC) else "FIX"
    for (cl in cls) {
      ssel[[length(ssel) + 1L]] <- data.frame(
        region = r, cluster = cl, cell_id = a$cell_id, ann_cf = a$cf,
        avail = navail, stringsAsFactors = FALSE)
    }
  }
  list(onwind = do.call(rbind, wsel), solar = do.call(rbind, ssel))
}

# Hourly mean over each (region, cluster) cell set. Columns are keyed
# "<region>.<cluster>"; `.reduce()` splits them back out.
.cf_series <- function(nm, map) {
  x <- as.data.frame(dplyr::collect(dplyr::select(dplyr::filter(
    arrow::open_dataset(file.path(CFDIR, paste0("cf_cells_", nm, ".parquet"))),
    cell_id %in% unique(map$cell_id)), cell_id, time, cf)))
  key <- paste(map$region, map$cluster, sep = ".")
  out <- NULL
  for (k in unique(key)) {
    ids <- map$cell_id[key == k]
    v <- x[x$cell_id %in% ids, ]
    a <- stats::aggregate(cf ~ time, data = v, FUN = mean)
    col <- rep(NA_real_, 8760L)
    col[match(as.POSIXct(a$time, tz = "UTC"), HOURS)] <- a$cf
    out <- cbind(out, .fill(col))
    colnames(out)[ncol(out)] <- k
  }
  out
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

# Columns are either "<region>" (inflow, load) or "<region>.<cluster>"
# (wind, solar); the cluster comes back out as its own column so the shipped
# table is keyed the way the model reads it.
.reduce <- function(m, calendar) {
  key <- .keys(calendar)
  do.call(rbind, lapply(colnames(m), function(cc) {
    agg <- stats::aggregate(m[, cc], by = list(timeslice = key), FUN = mean)
    p <- strsplit(cc, ".", fixed = TRUE)[[1]]
    data.frame(region = p[1],
               cluster = if (length(p) > 1L) p[2] else NA_character_,
               timeslice = agg$timeslice, value = agg$x,
               stringsAsFactors = FALSE)
  }))
}

topia_real_profiles <- function(
    calendars = "d365_h24") {
  maps <- .cf_sites()
  W <- .cf_series("onwind", maps$onwind)
  # one read per solar technology; both stand on the same cells
  S <- NULL
  for (cl in names(SOLAR_SRC)) {
    mp <- maps$solar[maps$solar$cluster == cl, , drop = FALSE]
    if (!nrow(mp)) next
    S <- cbind(S, .cf_series(SOLAR_SRC[[cl]], mp))
  }
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
  demand$cluster <- NULL
  # one row per (region, resource, cluster): the grade table the model's
  # caps and stock are sized from
  .grade_tab <- function(res, mp) {
    g <- stats::aggregate(cbind(ann_cf, avail) ~ region + cluster, mp, mean)
    g$cells <- stats::aggregate(cell_id ~ region + cluster, mp, length)$cell_id
    data.frame(resource = res, g)
  }
  grades <- rbind(.grade_tab("WWIN", maps$onwind),
                  .grade_tab("WSOL", maps$solar))
  grades <- grades[order(grades$region, grades$resource, -grades$ann_cf), ]
  sites <- SITE
  list(
    weather = weather[, c("calendar", "resource", "region", "cluster",
                          "timeslice", "wval")],
    demand = demand[, c("calendar", "region", "timeslice", "load")],
    sites = sites,
    grades = grades,
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
  # `grades` ships with the series: one row per (region, resource, cluster)
  # with its mean capacity factor and the number of cells behind it, which is
  # what the model's potential caps are sized from. It names no place -- only
  # the model's own region codes.
  topia_sample <- list(weather = .p$weather, demand = .p$demand,
                       grades = .p$grades)
  saveRDS(topia_sample,
          "data-raw/topia_profiles_sample.rds", compress = "xz")
  cat("wrote data-raw/topia_profiles_sample.rds:",
      nrow(topia_sample$weather), "weather rows,",
      nrow(topia_sample$demand), "demand rows\n")
  rm(.p)
}
