# Extracted from test-levcost-variants.R:170

# prequel ----------------------------------------------------------------------
lv_hor <- function() newHorizon(2020:2060)
lv_tech <- function(vintages = NULL, invcost = 300, clusters = NULL,
                    name = "PWR") {
  args <- list(
    name    = name,
    input   = data.frame(comm = "COA", unit = "PJ"),
    output  = data.frame(comm = "ELC", unit = "PJ"),
    ceff    = data.frame(comm = c("COA", "ELC"), cinp2use = c(1, NA),
                         use2cact = c(NA, 0.4)),
    cap2act = 1,
    fixom   = list(fixom = 10)
  )
  if (is.null(vintages) && is.null(clusters)) {
    args$vintage <- data.frame(olife = 20L)
    args$invcost <- list(invcost = invcost)
  } else if (is.null(clusters)) {
    args$vintage <- data.frame(vintage = as.character(vintages), olife = 20L)
    args$invcost <- data.frame(vintage = as.character(vintages), invcost = invcost)
  } else {
    grid <- expand.grid(vintage = as.character(vintages),
                        cluster = as.character(clusters),
                        stringsAsFactors = FALSE)
    args$vintage <- data.frame(vintage = as.character(vintages), olife = 20L)
    args$cluster <- data.frame(cluster = as.character(clusters))
    grid$invcost <- invcost
    args$invcost <- grid
  }
  do.call(newTechnology, args)
}
lv_repo <- function() {
  newRepository(
    "lv",
    newCommodity("COA", timeframe = "ANNUAL"),
    newCommodity("ELC", timeframe = "ANNUAL"),
    newSupply("SUP_COA", commodity = "COA",
              supply = data.frame(region = "R1", cost = 1))
  )
}
lv_run <- function(tech, ...) {
  suppressMessages(suppressWarnings(
    levcost(tech, repo = lv_repo(), region = "R1", horizon = lv_hor(),
            calendar = newCalendar(), verbose = FALSE, ...)
  ))
}

# test -------------------------------------------------------------------------
tech <- lv_tech(c(2020, 2030, 2040), invcost = c(300, 200, 100))
ex   <- getFromNamespace(".expand_one_process", "energyRt")(tech)
dv   <- getFromNamespace(".levcost_devintage", "energyRt")(ex$objects[[2]], "R1_VIN2030")
expect_equal(nrow(dv@vintage), 1L)
expect_true(is.na(dv@vintage$vintage))
expect_true(is.na(dv@vintage$cluster))
expect_equal(dv@vintage$start, 2030L)
expect_equal(dv@vintage$end,   2030L)
