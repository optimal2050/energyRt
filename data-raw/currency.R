# currency_codes / currency_fx / currency_deflator, and the regenerated
# `convert_data$base$Currency` coefficient row.
#
# Money is the one unit whose value moves in time, so a currency needs a year.
# Two tables are needed and one will not do: an exchange rate moves between
# currencies at one point in time, a deflator moves between points in time
# within one currency.
#
# `currency_codes` normalises spelling away so both tables are keyed by ISO
# 4217 code only. `scale` is how many of the ISO unit a spelling denotes, which
# covers sub-units (`cents` = 0.01 USD) and super-units (`crore INR` = 1e7 INR)
# with one mechanism. Before this, the eight crore spellings carried eight
# copies of one coefficient with nothing relating them to INR.
#
# Build is offline, from the cached pulls in `data-raw/currency/`. Refreshing
# those is a separate, dated, network step -- `currency_fetch()` -- so
# `DATASET.R` stays reproducible and the licence attribution lives beside the
# bytes it covers.
#
# Source: World Bank World Development Indicators, CC BY 4.0.
#   PA.NUS.FCRF     official exchange rate, LCU per USD, period average
#   NY.GDP.DEFL.ZS  GDP deflator (index)
#   FP.CPI.TOTL     consumer price index
# PA.NUS.FCRF credits IMF International Financial Statistics as its source.

CURRENCY_DIR <- file.path("data-raw", "currency")

WDI_INDICATORS <- c(fx  = "PA.NUS.FCRF",
                    gdp = "NY.GDP.DEFL.ZS",
                    cpi = "FP.CPI.TOTL")

# ---------------------------------------------------------------------------
# 1. The spelling table
# ---------------------------------------------------------------------------

# Every spelling `convert_data$base$Currency` carries, in its existing column
# order. Order is load-bearing: `.prefix_find_unit()` scans columns and the
# first match wins, so reordering changes which spelling a string resolves to.
# Non-ASCII spellings are written as escapes to keep this file ASCII.
#
# `si_prefixes` is carried through unchanged from the shipped array. It is 0 for
# every scaled spelling, which is what keeps `Mcents` unresolvable.
currency_spellings <- function() {
  data.frame(
    spelling = c(
      "USD", "cents", "mills", "RMB", "EUR",
      "cr. ₹", "cr.₹", "crore ₹", "crore INR",
      "INR (in cr.)", "INR", "₹", "crore_INR", "cr.INR", "cr. INR",
      "€", "Euro",
      "JPY", "Japanese yen", "JP¥", "円",
      "CNY", "Chinese yuan", "CN¥", "元"),
    code = c(
      "USD", "USD", "USD", "CNY", "EUR",
      "INR", "INR", "INR", "INR",
      "INR", "INR", "INR", "INR", "INR", "INR",
      "EUR", "EUR",
      "JPY", "JPY", "JPY", "JPY",
      "CNY", "CNY", "CNY", "CNY"),
    scale = c(
      1, 0.01, 0.001, 1, 1,
      1e7, 1e7, 1e7, 1e7,
      1e7, 1, 1, 1e7, 1e7, 1e7,
      1, 1,
      1, 1, 1, 1,
      1, 1, 1, 1),
    si_prefixes = c(
      TRUE, FALSE, FALSE, TRUE, TRUE,
      FALSE, FALSE, FALSE, FALSE,
      FALSE, TRUE, TRUE, FALSE, FALSE, FALSE,
      TRUE, TRUE,
      TRUE, TRUE, TRUE, TRUE,
      TRUE, TRUE, TRUE, TRUE),
    stringsAsFactors = FALSE)
}

# ---------------------------------------------------------------------------
# 2. The deflator area per currency
# ---------------------------------------------------------------------------

# A deflator is national; a currency need not be. FX is shared across the
# members of a currency union, so one rate serves them all, but there is no
# "XOF deflator" -- so each code names a declared representative area, visible
# here and overridable by the user.
#
# Only 14 of 153 codes are used by more than one country. Eleven have an
# unambiguous sovereign. The four judgement calls are recorded as such.
# A value may list candidates in preference order, separated by "|"; the build
# takes the first one the cached pull actually carries. The euro area is the
# reason: the API gives it `id = "EMU"` and `iso2Code = "XC"`, and the merge
# keys on `countryiso3code`, so which of the two appears is a fact about the
# response rather than something to assume. Germany is the fallback -- after
# 1999 a euro-area member's rate IS the euro rate.
CURRENCY_AREA_OVERRIDE <- c(
  EUR = "EMU|XC|DEU",
  USD = "USA",
  AUD = "AUS", CHF = "CHE", DKK = "DNK", GBP = "GBR",
  MAD = "MAR", NOK = "NOR", NZD = "NZL", XCG = "CUW",
  XAF = "CMR",  # CEMAC: Cameroon, the largest member
  XOF = "CIV",  # WAEMU: Cote d'Ivoire, the largest member
  XCD = "LCA",  # ECCU: no WDI aggregate; Saint Lucia as representative
  XPF = "NCL"   # CFP franc: New Caledonia
)

# code -> representative area, for every code `countrycode` knows. A code used
# by exactly one country takes that country; the rest come from the override.
currency_areas <- function() {
  cl <- countrycode::codelist
  d <- cl[!is.na(cl$iso3c) & !is.na(cl$iso4217c), c("iso3c", "iso4217c")]
  tb <- table(d$iso4217c)
  single <- names(tb)[tb == 1L]
  out <- stats::setNames(d$iso3c[match(single, d$iso4217c)], single)
  out <- c(out, CURRENCY_AREA_OVERRIDE[!(names(CURRENCY_AREA_OVERRIDE) %in%
                                           names(out))])
  multi <- setdiff(names(tb)[tb > 1L], names(CURRENCY_AREA_OVERRIDE))
  if (length(multi)) {
    stop("no declared deflator area for multi-country code(s): ",
         paste(multi, collapse = ", "))
  }
  out[order(names(out))]
}

# ---------------------------------------------------------------------------
# 3. Fetch (network; not run by DATASET.R)
# ---------------------------------------------------------------------------

# One indicator, every country, every year, paginated. The API caps `per_page`,
# so the page loop is not optional.
.wb_fetch_indicator <- function(indicator, per_page = 20000L) {
  base <- paste0("https://api.worldbank.org/v2/country/all/indicator/",
                 indicator, "?format=json&per_page=", per_page)
  page <- 1L
  acc <- list()
  repeat {
    u <- paste0(base, "&page=", page)
    r <- jsonlite::fromJSON(u)
    if (!is.list(r) || length(r) < 2L || is.null(r[[2]])) break
    d <- r[[2]]
    acc[[length(acc) + 1L]] <- data.frame(
      area = d$countryiso3code,
      year = suppressWarnings(as.integer(d$date)),
      value = suppressWarnings(as.numeric(d$value)),
      stringsAsFactors = FALSE)
    total <- as.integer(r[[1]]$pages)
    if (is.na(total) || page >= total) break
    page <- page + 1L
  }
  out <- do.call(rbind, acc)
  out <- out[!is.na(out$area) & nzchar(out$area) & !is.na(out$year), ,
             drop = FALSE]
  out[order(out$area, out$year), , drop = FALSE]
}

# Refresh every cached pull and stamp the date. Network step, run by hand.
currency_fetch <- function(dir = CURRENCY_DIR,
                           indicators = WDI_INDICATORS) {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  stamp <- character()
  for (nm in names(indicators)) {
    ind <- indicators[[nm]]
    message("fetching ", ind, " ...")
    d <- .wb_fetch_indicator(ind)
    f <- file.path(dir, paste0("wdi_", ind, ".csv"))
    utils::write.csv(d, f, row.names = FALSE, na = "")
    stamp <- c(stamp, sprintf("%s  %s  rows=%d  years=%d-%d", ind, nm,
                              nrow(d), min(d$year), max(d$year)))
  }
  writeLines(c(
    paste("fetched:", format(Sys.Date())),
    "source: World Bank World Development Indicators",
    "licence: CC BY 4.0 -- https://datacatalog.worldbank.org/public-licenses",
    "attribution: The World Bank: World Development Indicators",
    "note: PA.NUS.FCRF credits IMF International Financial Statistics",
    "", stamp), file.path(dir, "FETCHED"))
  invisible(TRUE)
}

# code -> the first candidate area the cached pull carries DATA for. `available`
# must be built from rows with a value, not rows present: the API returns a row
# per area and year whether or not it has a number, so testing presence alone
# resolves to an area that then contributes nothing and the fallback never
# fires.
#
# Resolution is per table because a code may have an exchange rate under one
# area and a deflator under another. The euro is the live case: `EMU` carries
# the euro's own rate from 1999 but no deflator in either series, and no WDI
# aggregate does, so EUR deflates on its largest member.
#
# An unresolved code is dropped with a warning rather than silently producing
# an empty merge.
.resolve_areas <- function(areas, available, what) {
  cand <- strsplit(unname(areas), "|", fixed = TRUE)
  got <- vapply(cand, function(x) {
    hit <- x[x %in% available]
    if (length(hit)) hit[1] else NA_character_
  }, character(1))
  names(got) <- names(areas)
  if (anyNA(got)) {
    warning(what, ": no area present for ", sum(is.na(got)), " code(s): ",
            paste(names(got)[is.na(got)], collapse = ", "), call. = FALSE)
  }
  got[!is.na(got)]
}

.read_cache <- function(dir, indicator) {
  f <- file.path(dir, paste0("wdi_", indicator, ".csv"))
  if (!file.exists(f)) {
    stop("no cached pull for ", indicator, " at ", f,
         "\nRun currency_fetch() once (needs network).", call. = FALSE)
  }
  utils::read.csv(f, stringsAsFactors = FALSE)
}

# ---------------------------------------------------------------------------
# 4. Build (offline)
# ---------------------------------------------------------------------------

# The base year for untagged money: the latest year the FX table covers for a
# quorum of currencies. A trailing year in which only a handful of countries
# have reported is not a usable base.
.currency_base_year <- function(fx, quorum = 0.5) {
  n <- length(unique(fx$code))
  per <- stats::aggregate(list(k = fx$code), by = list(year = fx$year),
                          FUN = function(x) length(unique(x)))
  ok <- per$year[per$k >= quorum * n]
  if (!length(ok)) stop("no year meets the coverage quorum")
  max(ok)
}

currency_build <- function(dir = CURRENCY_DIR) {
  codes <- currency_spellings()
  areas <- currency_areas()

  # --- FX: LCU per USD, keyed by currency rather than by country ----------
  # Members of a union report the same rate, so the representative area's row
  # is the currency's row. USD is the numeraire and is forced to 1.
  raw_fx <- .read_cache(dir, WDI_INDICATORS[["fx"]])
  fx_areas <- .resolve_areas(areas, unique(raw_fx$area[!is.na(raw_fx$value)]),
                             WDI_INDICATORS[["fx"]])
  pick <- data.frame(code = names(fx_areas), area = unname(fx_areas),
                     stringsAsFactors = FALSE)
  fx <- merge(pick, raw_fx, by = "area")
  fx <- fx[!is.na(fx$value) & fx$value > 0, , drop = FALSE]
  fx <- data.frame(code = fx$code, year = fx$year, lcu_per_usd = fx$value,
                   source = WDI_INDICATORS[["fx"]], stringsAsFactors = FALSE)
  fx <- fx[fx$code != "USD", , drop = FALSE]
  yrs <- sort(unique(fx$year))
  fx <- rbind(fx, data.frame(code = "USD", year = yrs, lcu_per_usd = 1,
                             source = "numeraire", stringsAsFactors = FALSE))
  fx <- fx[order(fx$code, fx$year), , drop = FALSE]
  rownames(fx) <- NULL

  # --- deflators: both bases, index as published -------------------------
  defl <- list()
  for (b in c("gdp", "cpi")) {
    raw <- .read_cache(dir, WDI_INDICATORS[[b]])
    da <- .resolve_areas(areas, unique(raw$area[!is.na(raw$value)]),
                         WDI_INDICATORS[[b]])
    d <- merge(data.frame(code = names(da), area = unname(da),
                          stringsAsFactors = FALSE), raw, by = "area")
    d <- d[!is.na(d$value) & d$value > 0, , drop = FALSE]
    defl[[b]] <- data.frame(code = d$code, area = d$area, year = d$year,
                            basis = b, index = d$value,
                            source = WDI_INDICATORS[[b]],
                            stringsAsFactors = FALSE)
  }
  deflator <- do.call(rbind, defl)
  deflator <- deflator[order(deflator$code, deflator$basis, deflator$year), ,
                       drop = FALSE]
  rownames(deflator) <- NULL

  base_year <- .currency_base_year(fx)
  attr(fx, "base_year") <- base_year

  # --- the regenerated Currency coefficient row --------------------------
  # `coef` is USD per unit; the series is LCU per USD. The reciprocal is the
  # whole reason this column is named `lcu_per_usd`.
  at <- fx[fx$year == base_year, , drop = FALSE]
  r <- at$lcu_per_usd[match(codes$code, at$code)]
  if (anyNA(r)) {
    stop("no ", base_year, " rate for: ",
         paste(unique(codes$code[is.na(r)]), collapse = ", "))
  }
  coef <- codes$scale / r
  currency_array <- array(
    rbind(coef, as.numeric(codes$si_prefixes)),
    dim = c(2L, nrow(codes)),
    dimnames = list(c("coef", "SI_prefixes"), codes$spelling))

  list(currency_codes = codes, currency_fx = fx,
       currency_deflator = deflator, currency_base_year = base_year,
       currency_array = currency_array)
}

# ---------------------------------------------------------------------------
# 5. Checks that must hold before anything is written
# ---------------------------------------------------------------------------

# The spelling set and its order must be exactly what ships today, or a string
# that resolves one way now resolves another way after the rebuild.
currency_check <- function(built, shipped) {
  old <- shipped$base$Currency
  new <- built$currency_array
  stopifnot(identical(colnames(old), colnames(new)))
  stopifnot(identical(old["SI_prefixes", ], new["SI_prefixes", ]))
  stopifnot(identical(colnames(new)[1], "USD"))
  stopifnot(isTRUE(all.equal(unname(new["coef", "USD"]), 1)))
  # every scaled spelling is its code's rate times its scale, by construction
  cd <- built$currency_codes
  for (i in seq_len(nrow(cd))) {
    one <- new["coef", cd$spelling[cd$code == cd$code[i] & cd$scale == 1][1]]
    stopifnot(isTRUE(all.equal(unname(new["coef", i]),
                               unname(one * cd$scale[i]))))
  }
  invisible(TRUE)
}

# ---------------------------------------------------------------------------
# 6. Write
# ---------------------------------------------------------------------------

#   source("data-raw/currency.R"); currency_write()
#
# Builds from the cache, checks the result against what ships, then writes the
# three datasets and the regenerated `convert_data`.
#
# `convert_data` had no generator before this. Its currency rates were typed by
# hand into an `if (F)` block in dev-local/log.Rmd, under argument names that no
# longer match `add_to_convert()`, so the shipped table could not be rebuilt --
# which is how it came to hold rates from several different years at once.
currency_write <- function(dir = CURRENCY_DIR) {
  built <- currency_build(dir)
  e <- new.env()
  load("data/convert_data.rda", envir = e)
  shipped <- e$convert_data
  currency_check(built, shipped)

  currency_codes <- built$currency_codes
  currency_fx <- built$currency_fx
  currency_deflator <- built$currency_deflator
  convert_data <- shipped
  convert_data$base$Currency <- built$currency_array

  usethis::use_data(currency_codes, currency_fx, currency_deflator,
                    convert_data,
                    internal = FALSE, overwrite = TRUE, compress = "xz")
  message("currency base year: ", built$currency_base_year)
  invisible(built)
}
