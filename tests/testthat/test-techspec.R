# techspec: YAML process specifications + designer plumbing ------------------

.ex <- function(f) {
  p <- system.file("techspec", "examples", f, package = "energyRt")
  if (!nzchar(p)) skip("techspec examples not installed")
  p
}

test_that("a complete spec validates clean and builds", {
  f <- .ex("coal_power.yml")
  iss <- process_spec_issues(f)
  expect_equal(nrow(iss), 0L)

  tech <- process_from_spec(f)
  expect_s4_class(tech, "technology")
  expect_equal(tech@name, "ECOA")
  expect_setequal(tech@input$comm, "COA")
  expect_setequal(tech@output$comm, "ELC")
  expect_setequal(tech@aux$acomm, "CO2")
  expect_equal(tech@vintage$olife, 40)
})

test_that("partial vintage coverage is reported, never auto-filled", {
  f <- .ex("ldv_bev.yml")
  iss <- process_spec_issues(f)
  w <- iss[iss$severity == "warning" & iss$section == "invcost", ]
  expect_equal(nrow(w), 1L)
  expect_match(w$message, "missing: 2035, 2040")
  expect_match(w$message, "NOT auto-filled")

  tech <- suppressWarnings(process_from_spec(f))
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
  spec <- read_procspec(.ex("ldv_bev.yml"))

  bad1 <- spec; bad1$mystery <- list()
  expect_true(any(grepl("unknown section",
                        process_spec_issues(bad1)$message)))

  bad2 <- spec; bad2$invcost[[1]]$cost_usd <- 9
  i2 <- process_spec_issues(bad2)
  expect_true(any(i2$severity == "error" &
                    grepl("unknown column.*cost_usd", i2$message)))
  expect_error(process_from_spec(bad2), "unknown column")

  bad3 <- spec; bad3$fixom <- list(list(vintage = 2077, fixom = 1))
  i3 <- process_spec_issues(bad3)
  expect_true(any(i3$severity == "error" &
                    grepl("undeclared vintage.*2077", i3$message)))
})

test_that("efficiency commodities must be declared ports", {
  spec <- read_procspec(.ex("coal_power.yml"))
  spec$efficiency[[1]]$comm <- "GHOST"
  iss <- process_spec_issues(spec)
  expect_true(any(iss$severity == "error" &
                    grepl("not among declared ports", iss$message)))
})

test_that("end-before-start vintage windows are errors", {
  spec <- read_procspec(.ex("ldv_bev.yml"))
  spec$vintages[[2]]$end <- 2029   # start 2030
  iss <- process_spec_issues(spec)
  expect_true(any(iss$severity == "error" &
                    grepl("before start", iss$message)))
})

test_that("tech_to_spec round-trips losslessly", {
  t1 <- suppressWarnings(process_from_spec(.ex("ldv_bev.yml")))
  s  <- process_to_spec(t1)
  t2 <- suppressWarnings(process_from_spec(s))
  for (sl in c("input", "output", "ceff", "invcost", "vintage",
               "capacity")) {
    expect_equal(slot(t1, sl), slot(t2, sl), info = sl)
  }
})

test_that("process_spec_code() emits parseable R that names newTechnology", {
  code <- process_spec_code(.ex("coal_power.yml"))
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
  spec <- read_procspec(.ex("coal_power.yml"))
  spec$clusters <- list(list(cluster = "CL1", desc = "class 1", order = 1),
                        list(cluster = "CL2", desc = "class 2", order = 2))
  tech <- process_from_spec(spec)
  expect_equal(tech@cluster$cluster, c("CL1", "CL2"))
  expect_equal(tech@cluster$order, c(1, 2))
  # round-trip keeps it
  s2 <- process_to_spec(tech)
  expect_equal(length(s2$clusters), 2L)
})

test_that("image scalar round-trips through misc", {
  spec <- read_procspec(.ex("coal_power.yml"))
  spec$image <- "https://example.org/coal.png"
  tech <- process_from_spec(spec)
  expect_equal(tech@misc$image, "https://example.org/coal.png")
  s2 <- process_to_spec(tech)
  expect_equal(s2$image, "https://example.org/coal.png")
  # and it is a known scalar, not an unknown-section error
  expect_equal(nrow(process_spec_issues(spec)), 0L)
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
  tech <- process_from_spec(.ex("coal_power.yml"))
  out <- suppressMessages(report(tech, template = "summary",
                                 format = "html", open = FALSE,
                                 levcost = FALSE,
                                 file = tempfile("repsum")))
  expect_true(file.exists(out))
  html <- readChar(out, file.size(out), useBytes = TRUE)
  expect_match(html, "Key facts")
})

test_that("afs section maps to the afs slot", {
  spec <- read_procspec(.ex("coal_power.yml"))
  spec$afs <- list(list(timeslice = "ANNUAL", afs.up = 0.8))
  tech <- process_from_spec(spec)
  expect_equal(tech@afs$afs.up, 0.8)
  expect_equal(tech@afs$timeslice, "ANNUAL")
})

test_that("weather section and scalar flags round-trip", {
  spec <- read_procspec(.ex("coal_power.yml"))
  spec$weather <- list(list(weather = "WSOL", waf.up = 0.9),
                       list(weather = "WSOL", comm = "ELC", wafc.up = 1))
  spec$optimizeRetirement <- TRUE
  spec$region <- c("R1", "R2")
  tech <- process_from_spec(spec)
  expect_equal(nrow(energyRt::process_spec_issues(spec)), 0L)
  expect_equal(tech@weather$weather, c("WSOL", "WSOL"))
  expect_equal(tech@weather$waf.up[1], 0.9)
  expect_true(tech@optimizeRetirement)
  expect_equal(tech@region, c("R1", "R2"))
  s2 <- process_to_spec(tech)
  expect_equal(length(s2$weather), 2L)
  expect_true(s2$optimizeRetirement)
  expect_equal(s2$region, c("R1", "R2"))
  # defaults are not exported
  s0 <- process_to_spec(process_from_spec(read_procspec(.ex("coal_power.yml"))))
  expect_null(s0$optimizeRetirement)
  expect_null(s0$fullYear)
})

test_that("report template scalar round-trips through misc", {
  spec <- read_procspec(.ex("coal_power.yml"))
  spec$report <- "summary"
  tech <- process_from_spec(spec)
  expect_equal(tech@misc$report, "summary")
  expect_equal(process_to_spec(tech)$report, "summary")
  expect_equal(nrow(process_spec_issues(spec)), 0L)
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
    t_yml <- suppressWarnings(process_from_spec(.ex(ex)))
    jf <- tempfile(fileext = ".json")
    process_to_spec(t_yml, file = jf)
    spec_json <- read_procspec(jf)
    # identical validation report
    expect_identical(process_spec_issues(spec_json),
                     process_spec_issues(process_to_spec(t_yml)), info = ex)
    t_json <- suppressWarnings(process_from_spec(spec_json))
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
  spec <- read_procspec(.ex("coal_power.yml"))
  spec$efficiency[[1]]$cinp2use <- 1798.61111112345
  t1 <- process_from_spec(spec)
  jf <- tempfile(fileext = ".json")
  process_to_spec(t1, file = jf)
  t2 <- process_from_spec(read_procspec(jf))
  v <- t2@ceff$cinp2use[!is.na(t2@ceff$cinp2use)]
  expect_identical(v[1], 1798.61111112345)
  unlink(jf)
})

# ============================================================================ #
# Multi-class specs: storage and trade
#
# The spec layer is driven by two per-class registries (`.SPEC_SECTIONS`,
# `.SPEC_SCALAR_ARGS`), so these tests are as much about the registries being
# in step with the classes as about any one conversion. A slot renamed in
# class-storage.R without a matching registry edit shows up here first.
# ============================================================================ #

ex_path <- function(f) {
  p <- system.file("techspec", "examples", f, package = "energyRt")
  if (!nzchar(p)) p <- file.path("../../inst/techspec/examples", f)
  p
}

test_that("every section maps to a real slot of its class", {
  for (cls in energyRt:::.SPEC_CLASSES) {
    obj <- new(cls)
    secs <- energyRt:::.spec_sections(cls)
    for (sec in names(secs)) {
      arg <- secs[[sec]]$arg
      expect_true(arg %in% slotNames(obj),
                  info = paste(cls, sec, "->", arg))
      v <- slot(obj, arg)
      if (!is.data.frame(v)) next
      # every declared column must exist on the slot: a column the class
      # dropped would be accepted by the reader and then rejected by the
      # constructor, which is the failure mode this guards
      expect_setequal(setdiff(secs[[sec]]$cols, names(v)), character())
    }
  }
})

test_that("scalar spec keys are real constructor arguments", {
  ctors <- c(technology = "newTechnology", storage = "newStorage",
             trade = "newTrade")
  for (cls in energyRt:::.SPEC_CLASSES) {
    fm <- names(formals(get(ctors[[cls]], envir = asNamespace("energyRt"))))
    for (k in names(energyRt:::.SPEC_SCALAR_ARGS[[cls]])) {
      expect_true(k %in% fm, info = paste(cls, k))
    }
  }
})

test_that("a storage spec round-trips faithfully", {
  f <- ex_path("battery.yml")
  skip_if(!file.exists(f), "example spec not available")
  obj <- process_from_spec(f)
  expect_s4_class(obj, "storage")
  expect_identical(obj@name, "STG_BTR")
  # the three parts, and the per-part costs that used to live inside them
  expect_identical(as.character(obj@storage$comm), "ELC")
  expect_equal(obj@invcost$out.invcost, 12144)
  expect_equal(obj@invcost$stg.invcost, 8081)
  expect_equal(obj@seff$inpeff, 0.95)
  expect_equal(nrow(obj@vintage), 2L)

  back <- process_from_spec(process_to_spec(obj))
  expect_identical(back@name, obj@name)
  expect_equal(back@invcost$stg.invcost, obj@invcost$stg.invcost)
  expect_equal(nrow(back@vintage), nrow(obj@vintage))
})

test_that("logical flags round-trip against constructor defaults", {
  # both constructors default fullYear = TRUE, optimizeRetirement =
  # FALSE; the writer records only departures, resolved from the
  # constructor formals per class (the technology PROTOTYPE disagrees
  # with the constructor on both flags and must not be consulted)
  tech <- newTechnology(
    name = "TFLAG",
    input = data.frame(comm = "GAS", unit = "GWh"),
    output = data.frame(comm = "ELC", unit = "GWh"),
    ceff = data.frame(comm = "GAS", cinp2use = 0.5),
    optimizeRetirement = TRUE
  )
  spec <- process_to_spec(tech)
  expect_identical(spec$optimizeRetirement, TRUE)
  expect_null(spec$fullYear) # default TRUE, not recorded
  expect_true(process_from_spec(spec)@optimizeRetirement)

  stg <- newStorage(
    name = "SFLAG",
    commodity = "ELC",
    seff = data.frame(inpeff = 0.9, outeff = 0.9),
    fullYear = FALSE
  )
  spec <- process_to_spec(stg)
  expect_identical(spec$fullYear, FALSE)
  expect_null(spec$optimizeRetirement) # default FALSE, not recorded
  expect_false(process_from_spec(spec)@fullYear)

  # all-default objects: neither flag appears in the spec
  s0 <- process_to_spec(update(tech, optimizeRetirement = FALSE))
  expect_null(s0$optimizeRetirement)
  expect_null(s0$fullYear)
  s1 <- process_to_spec(update(stg, fullYear = TRUE))
  expect_null(s1$fullYear)
  expect_null(s1$optimizeRetirement)
})

test_that("a trade spec round-trips faithfully", {
  f <- ex_path("interconnector.yml")
  skip_if(!file.exists(f), "example spec not available")
  obj <- process_from_spec(f)
  expect_s4_class(obj, "trade")
  expect_identical(obj@name, "TRD_ELC")
  expect_equal(nrow(obj@routes), 2L)
  expect_setequal(as.character(obj@routes$src), c("R1", "R2"))
  expect_equal(obj@cap2act, 8760)

  back <- process_from_spec(process_to_spec(obj))
  expect_equal(nrow(back@routes), nrow(obj@routes))
  expect_equal(back@cap2act, obj@cap2act)
})

test_that("generated code re-creates the object, for every class", {
  for (f in c("coal_power.yml", "battery.yml", "interconnector.yml")) {
    p <- ex_path(f)
    if (!file.exists(p)) next
    obj  <- process_from_spec(p)
    code <- process_spec_code(p)
    expect_true(startsWith(
      code, paste0(energyRt:::.SPEC_CTOR[[class(obj)]], "(")))
    rebuilt <- eval(parse(text = code), envir = asNamespace("energyRt"))
    expect_identical(class(rebuilt), class(obj))
    expect_identical(rebuilt@name, obj@name)
  }
})

test_that("class-specific validation catches what each class requires", {
  # unknown class: reported once, and nothing else is attempted
  iss <- process_spec_issues(list(class = "supply", name = "X"))
  expect_equal(nrow(iss), 1L)
  expect_match(iss$message, "unknown class")

  # storage: needs the commodity shorthand or a declared part
  iss <- process_spec_issues(list(class = "storage", name = "S"))
  expect_true(any(grepl("needs `commodity`", iss$message)))
  expect_equal(nrow(process_spec_issues(
    list(class = "storage", name = "S", commodity = "ELC"))), 0L)

  # trade: needs a commodity and at least one route, and src != dst
  iss <- process_spec_issues(list(class = "trade", name = "T"))
  expect_true(any(grepl("needs `commodity`", iss$message)))
  expect_true(any(grepl("at least one route", iss$message)))
  iss <- process_spec_issues(list(class = "trade", name = "T",
                                  commodity = "ELC",
                                  routes = list(list(src = "R1", dst = "R1"))))
  expect_true(any(grepl("same region", iss$message)))
})

test_that("a flat cost on a storage is an error, not a silent drop", {
  # the mistake the prefixed slots make easy: `invcost` rather than
  # `stg.invcost`. It must be rejected by name, before the constructor runs.
  iss <- process_spec_issues(list(
    class = "storage", name = "S", commodity = "ELC",
    invcost = list(list(invcost = 5))))
  expect_true(any(iss$severity == "error"))
  expect_true(any(grepl("unknown column", iss$message)))
})

test_that("the deprecated tech_* aliases still work, with a warning", {
  f <- ex_path("coal_power.yml")
  skip_if(!file.exists(f), "example spec not available")
  expect_warning(a <- tech_from_spec(f), "deprecated|process_from_spec")
  expect_s4_class(a, "technology")
  expect_warning(tech_spec_issues(f), "deprecated|process_spec_issues")
  expect_warning(tech_to_spec(a), "deprecated|process_to_spec")
})
