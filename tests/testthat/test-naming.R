# Object-name rule + path slugs (R/zzz.R .assert_object_name, check_name;
# R/solve.R .path_slug): names use letters/digits/underscore and start with
# a letter (one leading dot marks internal scratch); slugs join parts with
# "-", which valid names cannot contain — underscores pass through VERBATIM.

test_that("path slugs keep underscores and join parts with dashes", {
  slug <- energyRt:::.path_slug
  expect_identical(slug("s4_h24"), "s4_h24")
  expect_identical(slug("BASE", "UTOPIA_R11", "s4_h24"),
                   "BASE-UTOPIA_R11-s4_h24")
  expect_identical(slug("a b.c_d"), "a_b_c_d")     # unsafe chars -> "_"
  expect_identical(slug("BASE", "BASE", "smc"), "BASE-smc")  # dedup
  expect_identical(slug("", NA, "x"), "x")
})

test_that("check_name enforces the backend-identifier rule", {
  expect_true(check_name("s4_h24"))
  expect_true(check_name("UTOPIA_R11"))
  expect_false(check_name("bad-name"))
  expect_false(check_name("1abc"))
  expect_false(check_name("a b"))
  expect_false(check_name(".lc_x"))                 # dot only with dot=TRUE
  expect_true(check_name(".lc_x", dot = TRUE))
  expect_false(check_name("a.b", dot = TRUE))       # dot only LEADING
})

test_that("constructors reject invalid names, naming the rule", {
  expect_error(
    newTechnology("bad-name", output = list(comm = "ELC")),
    "letters, digits and underscore")
  expect_error(newCommodity("EL C"), "letters, digits and underscore")
  expect_error(
    interpolate_model(sp_tech(c(50, 50, 50), optret = FALSE, name = "nm"),
                      name = "bad-scen"),
    "letters, digits and underscore")
  # run/variant LABELS are not object names — dashes stay legal there
  # (exercised throughout test-runs.R with run = "glpk-alt")
})

test_that("the scenario folder carries names verbatim, dash-separated", {
  old <- set_scenarios_path(file.path(tempdir(), "naming-suite"))
  on.exit(set_scenarios_path(old), add = TRUE)
  sc <- interpolate_model(sp_tech(c(50, 50, 50), optret = FALSE,
                                  name = "nm_x"), name = "BASE_X")
  # {scenario}-{model}-{calendar}: underscores inside each name untouched
  expect_identical(basename(sc@path), "BASE_X-nm_x-spc")
})

# ---- prefix rule (CONTRIBUTING.md "Naming") ---------------------------------
# `en` is the ONLY permitted namespace prefix. Package initials and project
# names (`ert`, `ideea`) are banned everywhere a name is claimed: exported
# functions, S3 class tags and methods, CSS classes, LaTeX colour names.
# Whitelist: report_vehicle.Rmd still carries the IDEEA vocabulary it was
# ported with -- drop the exemption when that template is converted.

test_that("no foreign package-initial prefixes in code or exports", {
  banned <- "\\b(ert|ideea)[-_][A-Za-z0-9]"
  pkg <- testthat::test_path("..", "..")

  # The rule is about names energyRt DEFINES. Referring to another package's
  # API (`IDEEA::ideea_modules`) or naming the source file a routine was
  # ported from (`ideea_levcost.R`) is not claiming a prefix.
  strip <- function(x) {
    x <- gsub("[A-Za-z0-9.]+::(ert|ideea)[A-Za-z0-9_.]*", "", x)
    gsub("\\b(ert|ideea)_[A-Za-z0-9_]*[.][Rr]\\b", "", x)
  }

  # exports and S3 methods
  ns <- strip(readLines(file.path(pkg, "NAMESPACE"), warn = FALSE))
  expect_false(any(grepl(banned, ns)),
               label = "NAMESPACE free of ert_/ideea_ names")

  # R sources
  r_files <- list.files(file.path(pkg, "R"), pattern = "[.]R$",
                        full.names = TRUE)
  hits <- unlist(lapply(r_files, function(f) {
    x <- strip(readLines(f, warn = FALSE))
    if (any(grepl(banned, x))) basename(f) else NULL
  }))
  expect_equal(hits, NULL, label = "R/ free of ert_/ideea_ names")

  # shipped templates (report_vehicle.Rmd exempt for now)
  tpl <- setdiff(
    list.files(file.path(pkg, "inst", "templates"), pattern = "[.]Rmd$",
               full.names = TRUE),
    file.path(pkg, "inst", "templates", "report_vehicle.Rmd"))
  hits_t <- unlist(lapply(tpl, function(f) {
    x <- strip(readLines(f, warn = FALSE))
    if (any(grepl(banned, x))) basename(f) else NULL
  }))
  expect_equal(hits_t, NULL, label = "templates free of ert_/ideea_ names")
})
