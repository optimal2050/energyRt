# combine_vintages(): per-vintage objects -> one vintaged technology --------

.cv_tech <- function(year, invcost, extra_input = NULL) {
  inp <- data.frame(comm = c("ELC", extra_input),
                    unit = rep("GWh", 1 + length(extra_input)))
  ceff <- data.frame(comm = c(inp$comm, "CO2"),
                     cinp2use = c(rep(1, nrow(inp)), NA),
                     cact2cout = c(rep(NA, nrow(inp)), 1))
  newTechnology(
    name = paste0("FDAC_S_VIN", year),
    desc = paste0("S-DAC, vintage ", year),
    input = inp,
    output = data.frame(comm = "CO2", unit = "kt"),
    units = data.frame(capacity = "kt/h", activity = "kt", costs = "MEUR"),
    ceff = ceff,
    invcost = list(invcost = invcost),
    fixom = list(fixom = 50),
    start = data.frame(start = year),
    olife = data.frame(olife = 25L),
    cap2act = 8760
  )
}

test_that("per-vintage objects combine into one vintaged technology", {
  techs <- list(.cv_tech(2030, 1800), .cv_tech(2040, 1500),
                .cv_tech(2050, 1200))
  names(techs) <- vapply(techs, function(t) t@name, character(1))
  cmb <- combine_vintages(techs)

  expect_s4_class(cmb, "technology")
  expect_equal(cmb@name, "FDAC_S")
  vt <- cmb@vintage
  expect_equal(vt$vintage, c("2030", "2040", "2050"))
  expect_equal(vt$start, c(2030, 2040, 2050))
  # windows closed at the next vintage; the last stays open
  expect_equal(vt$end, c(2039, 2049, NA))
  expect_equal(vt$olife, c(25, 25, 25))

  # invcost differs by vintage: one keyed row each
  ic <- cmb@invcost[!is.na(cmb@invcost$invcost), ]
  expect_equal(nrow(ic), 3L)
  expect_setequal(ic$vintage, c("2030", "2040", "2050"))
  expect_equal(ic$invcost[ic$vintage == "2040"], 1500)

  # ceff/fixom identical across vintages: collapsed to all-vintage rows
  expect_true(all(is.na(cmb@ceff$vintage)))
  fx <- cmb@fixom[!is.na(cmb@fixom$fixom), ]
  expect_equal(nrow(fx), 1L)
  expect_true(is.na(fx$vintage))

  # provenance retained
  expect_equal(names(cmb@misc$combined_from),
               c("2030", "2040", "2050"))
  # the variant machinery sees the vintages (draw fan / levcost variants)
  expect_false(is.null(energyRt:::.tech_variants(cmb)))
})

test_that("port unions and per-vintage differences are vintage-keyed", {
  techs <- list(.cv_tech(2030, 1800, extra_input = "HEA100"),
                .cv_tech(2040, 1500))   # no heat from 2040 on
  names(techs) <- vapply(techs, function(t) t@name, character(1))
  cmb <- combine_vintages(techs)
  expect_setequal(cmb@input$comm, c("ELC", "HEA100"))
  # the heat efficiency row exists only for vintage 2030
  hea <- cmb@ceff[!is.na(cmb@ceff$comm) & cmb@ceff$comm == "HEA100" &
                    !is.na(cmb@ceff$cinp2use), ]
  expect_equal(hea$vintage, "2030")
  # the shared ELC row collapsed
  elc <- cmb@ceff[!is.na(cmb@ceff$comm) & cmb@ceff$comm == "ELC" &
                    !is.na(cmb@ceff$cinp2use), ]
  expect_true(all(is.na(elc$vintage)))
})

test_that("labels, options and guards", {
  t1 <- .cv_tech(2030, 1800); t2 <- .cv_tech(2040, 1500)
  # explicit labels override name parsing
  cmb <- combine_vintages(list(a = t1, b = t2),
                          vintages = c("2030", "2040"), name = "X")
  expect_equal(cmb@vintage$vintage, c("2030", "2040"))
  # collapse_common = FALSE keeps every row keyed
  cmb2 <- combine_vintages(list(t1, t2), collapse_common = FALSE)
  expect_false(any(is.na(cmb2@ceff$vintage[!is.na(cmb2@ceff$comm)])))
  # close_windows = FALSE leaves ends open
  cmb3 <- combine_vintages(list(t1, t2), close_windows = FALSE)
  expect_true(all(is.na(cmb3@vintage$end)))
  # duplicate labels error
  expect_error(combine_vintages(list(t1, t1)), "duplicate")
  # differing cap2act warns
  t3 <- .cv_tech(2040, 1500); t3@cap2act <- 1
  expect_warning(combine_vintages(list(t1, t3)), "cap2act")
})
