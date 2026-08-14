# techspec: YAML process specifications + designer plumbing ------------------

.ex <- function(f) {
  p <- system.file("techspec", "examples", f, package = "energyRt")
  if (!nzchar(p)) skip("techspec examples not installed")
  p
}

test_that("a complete spec validates clean and builds", {
  f <- .ex("coal_power.yml")
  iss <- tech_spec_issues(f)
  expect_equal(nrow(iss), 0L)

  tech <- tech_from_spec(f)
  expect_s4_class(tech, "technology")
  expect_equal(tech@name, "ECOA")
  expect_setequal(tech@input$comm, "COA")
  expect_setequal(tech@output$comm, "ELC")
  expect_setequal(tech@aux$acomm, "CO2")
  expect_equal(tech@vintage$olife, 40)
})

test_that("partial vintage coverage is reported, never auto-filled", {
  f <- .ex("ldv_bev.yml")
  iss <- tech_spec_issues(f)
  w <- iss[iss$severity == "warning" & iss$section == "invcost", ]
  expect_equal(nrow(w), 1L)
  expect_match(w$message, "missing: 2035, 2040")
  expect_match(w$message, "NOT auto-filled")

  tech <- suppressWarnings(tech_from_spec(f))
  # exactly the two declared invcost rows: nothing invented
  expect_equal(nrow(tech@invcost), 2L)
  expect_setequal(tech@invcost$vintage, c("2020", "2030"))
  # windows landed; last vintage open-ended
  v <- tech@vintage
  expect_equal(v$start[v$vintage == "2030"], 2030)
  expect_equal(v$end[v$vintage == "2030"], 2034)
  expect_true(is.na(v$end[v$vintage == "2040"]))
})

test_that("unknown sections/columns and undeclared vintages are errors", {
  spec <- read_techspec(.ex("ldv_bev.yml"))

  bad1 <- spec; bad1$mystery <- list()
  expect_true(any(grepl("unknown section",
                        tech_spec_issues(bad1)$message)))

  bad2 <- spec; bad2$invcost[[1]]$cost_usd <- 9
  i2 <- tech_spec_issues(bad2)
  expect_true(any(i2$severity == "error" &
                    grepl("unknown column.*cost_usd", i2$message)))
  expect_error(tech_from_spec(bad2), "unknown column")

  bad3 <- spec; bad3$fixom <- list(list(vintage = 2077, fixom = 1))
  i3 <- tech_spec_issues(bad3)
  expect_true(any(i3$severity == "error" &
                    grepl("undeclared vintage.*2077", i3$message)))
})

test_that("efficiency commodities must be declared ports", {
  spec <- read_techspec(.ex("coal_power.yml"))
  spec$efficiency[[1]]$comm <- "GHOST"
  iss <- tech_spec_issues(spec)
  expect_true(any(iss$severity == "error" &
                    grepl("not among declared ports", iss$message)))
})

test_that("end-before-start vintage windows are errors", {
  spec <- read_techspec(.ex("ldv_bev.yml"))
  spec$vintages[[2]]$end <- 2029   # start 2030
  iss <- tech_spec_issues(spec)
  expect_true(any(iss$severity == "error" &
                    grepl("before start", iss$message)))
})

test_that("tech_to_spec round-trips losslessly", {
  t1 <- suppressWarnings(tech_from_spec(.ex("ldv_bev.yml")))
  s  <- tech_to_spec(t1)
  t2 <- suppressWarnings(tech_from_spec(s))
  for (sl in c("input", "output", "ceff", "invcost", "vintage",
               "capacity")) {
    expect_equal(slot(t1, sl), slot(t2, sl), info = sl)
  }
})

test_that("tech_spec_code() emits parseable R that names newTechnology", {
  code <- tech_spec_code(.ex("coal_power.yml"))
  expect_match(code, "^newTechnology\\(")
  expect_silent(parse(text = code))
})

test_that("the designer app constructs (shiny+DT available)", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("DT")
  app_dir <- system.file("designer", package = "energyRt")
  skip_if(!nzchar(app_dir), "designer app not installed")
  app <- shiny::shinyAppFile(file.path(app_dir, "app.R"))
  expect_s3_class(app, "shiny.appobj")
})

test_that("clusters section maps to the cluster slot", {
  spec <- read_techspec(.ex("coal_power.yml"))
  spec$clusters <- list(list(cluster = "CL1", desc = "class 1", order = 1),
                        list(cluster = "CL2", desc = "class 2", order = 2))
  tech <- tech_from_spec(spec)
  expect_equal(tech@cluster$cluster, c("CL1", "CL2"))
  expect_equal(tech@cluster$order, c(1, 2))
  # round-trip keeps it
  s2 <- tech_to_spec(tech)
  expect_equal(length(s2$clusters), 2L)
})

test_that("image scalar round-trips through misc", {
  spec <- read_techspec(.ex("coal_power.yml"))
  spec$image <- "https://example.org/coal.png"
  tech <- tech_from_spec(spec)
  expect_equal(tech@misc$image, "https://example.org/coal.png")
  s2 <- tech_to_spec(tech)
  expect_equal(s2$image, "https://example.org/coal.png")
  # and it is a known scalar, not an unknown-section error
  expect_equal(nrow(tech_spec_issues(spec)), 0L)
})

test_that("the designer uses report()'s template family", {
  # the app has no template directory of its own any more
  tpl_dir <- system.file("templates", package = "energyRt")
  skip_if(!nzchar(tpl_dir), "templates not installed")
  fls <- list.files(tpl_dir, pattern = "^report_.*[.]Rmd$")
  expect_true(all(c("report_generic.Rmd", "report_summary.Rmd") %in% fls))
})

test_that("report(template = 'summary') renders the one-pager", {
  skip_if_not_installed("rmarkdown")
  skip_if(!rmarkdown::pandoc_available(), "pandoc unavailable")
  tech <- tech_from_spec(.ex("coal_power.yml"))
  out <- suppressMessages(report(tech, template = "summary",
                                 format = "html", open = FALSE,
                                 levcost = FALSE,
                                 file = tempfile("repsum")))
  expect_true(file.exists(out))
  html <- readChar(out, file.size(out), useBytes = TRUE)
  expect_match(html, "Key facts")
})

test_that("afs section maps to the afs slot", {
  spec <- read_techspec(.ex("coal_power.yml"))
  spec$afs <- list(list(timeslice = "ANNUAL", afs.up = 0.8))
  tech <- tech_from_spec(spec)
  expect_equal(tech@afs$afs.up, 0.8)
  expect_equal(tech@afs$timeslice, "ANNUAL")
})

test_that("weather section and scalar flags round-trip", {
  spec <- read_techspec(.ex("coal_power.yml"))
  spec$weather <- list(list(weather = "WSOL", waf.up = 0.9),
                       list(weather = "WSOL", comm = "ELC", wafc.up = 1))
  spec$optimizeRetirement <- TRUE
  spec$region <- c("R1", "R2")
  tech <- tech_from_spec(spec)
  expect_equal(nrow(energyRt::tech_spec_issues(spec)), 0L)
  expect_equal(tech@weather$weather, c("WSOL", "WSOL"))
  expect_equal(tech@weather$waf.up[1], 0.9)
  expect_true(tech@optimizeRetirement)
  expect_equal(tech@region, c("R1", "R2"))
  s2 <- tech_to_spec(tech)
  expect_equal(length(s2$weather), 2L)
  expect_true(s2$optimizeRetirement)
  expect_equal(s2$region, c("R1", "R2"))
  # defaults are not exported
  s0 <- tech_to_spec(tech_from_spec(read_techspec(.ex("coal_power.yml"))))
  expect_null(s0$optimizeRetirement)
  expect_null(s0$fullYear)
})

test_that("report template scalar round-trips through misc", {
  spec <- read_techspec(.ex("coal_power.yml"))
  spec$report <- "summary"
  tech <- tech_from_spec(spec)
  expect_equal(tech@misc$report, "summary")
  expect_equal(tech_to_spec(tech)$report, "summary")
  expect_equal(nrow(tech_spec_issues(spec)), 0L)
})

test_that("process_designer exists; tech_designer is a deprecated alias", {
  expect_true(is.function(process_designer))
  expect_warning(
    tryCatch(tech_designer(dir = "definitely-missing-dir"),
             error = function(e) invisible(NULL)),
    "deprecated")
})

test_that("JSON is an equivalent techspec container", {
  skip_if_not_installed("jsonlite")
  for (ex in c("coal_power.yml", "ldv_bev.yml")) {
    t_yml <- suppressWarnings(tech_from_spec(.ex(ex)))
    jf <- tempfile(fileext = ".json")
    tech_to_spec(t_yml, file = jf)
    spec_json <- read_techspec(jf)
    # identical validation report
    expect_identical(tech_spec_issues(spec_json),
                     tech_spec_issues(tech_to_spec(t_yml)), info = ex)
    t_json <- suppressWarnings(tech_from_spec(spec_json))
    for (sl in c("input", "output", "aux", "ceff", "aeff", "invcost",
                 "fixom", "vintage", "capacity", "units")) {
      expect_equal(slot(t_yml, sl), slot(t_json, sl),
                   info = paste(ex, sl))
    }
    expect_equal(t_yml@cap2act, t_json@cap2act, info = ex)
    unlink(jf)
  }
})

test_that("JSON round-trip preserves full numeric precision", {
  skip_if_not_installed("jsonlite")
  spec <- read_techspec(.ex("coal_power.yml"))
  spec$efficiency[[1]]$cinp2use <- 1798.61111112345
  t1 <- tech_from_spec(spec)
  jf <- tempfile(fileext = ".json")
  tech_to_spec(t1, file = jf)
  t2 <- tech_from_spec(read_techspec(jf))
  v <- t2@ceff$cinp2use[!is.na(t2@ceff$cinp2use)]
  expect_identical(v[1], 1798.61111112345)
  unlink(jf)
})
