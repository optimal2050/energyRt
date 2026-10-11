# Scenario identity: a name is a label, the folder is the identity.
#
# A scenario's folder is {scenario}-{model}-{calendar}-{horizon} while its
# @name is only {scenario}, so a 48-country pass can save every scenario as
# "BY25A_F4". The registry used to key rows on (type, name, parent, variant),
# which cannot represent that: it kept ONE row whose path each save overwrote,
# and a later lookup by name loaded whichever folder had been written last.
# One country's report was rendered from another country's results.
#
# Rows are now keyed on path, so both survive, and a name that matches more
# than one live folder is refused rather than resolved.

# @covers add_to_registry refresh_registry getScenario load_scenario depth=S backends=glpk

# Expect a tidyselect deprecation ("Using an external vector in selections")
# against most expectations here. It predates this work -- test-seal.R and
# test-store-deps.R carry the same entries and the counts match at HEAD -- and
# it is not raised by anything in R/ (there is no magrittr pipe left in the
# package), so the call site is in a dependency. Not silenced: a blanket
# suppressWarnings() here would also hide a real one.

si_root <- function(...) {
  gsub("[\\\\/]+", "/", file.path(tempdir(), "ident-suite", ...))
}

si_local <- function(env = parent.frame()) {
  old <- list(
    s = set_scenarios_path(si_root("scenarios")),
    m = set_models_path(si_root("models")),
    r = set_registry_file(si_root("energyRt_registry.csv")))
  expr <- bquote({
    set_scenarios_path(.(old$s)); set_models_path(.(old$m))
    set_registry_file(.(old$r))
  })
  do.call(on.exit, list(expr, add = TRUE), envir = env)
  invisible(NULL)
}

# Two one-region models with DIFFERENT regions, so a mix-up shows up in the
# data and not only in the path -- that is how the real failure was found.
# Mirrors sp_tech() (helper-stock-path.R) with the region parameterised.
si_model <- function(region) {
  cap <- data.frame(region = region, year = SP_YEARS, stock = 100)
  TECH <- newTechnology(
    "TECH", output = data.frame(comm = "ELC", unit = "GWh"), cap2act = 1,
    capacity = cap, fixom = data.frame(fixom = 1),
    vintage = data.frame(olife = 50L), optimizeRetirement = FALSE)
  newModel(
    name = paste0("CTRY_", region), region = region, horizon = sp_hor(),
    discount = 0, calendar = sp_cal(), optimizeRetirement = FALSE,
    repo = newRepository(
      "spr", newCommodity("ELC", timeframe = "HOUR"), TECH,
      newSupply("BACK", commodity = "ELC", supply = data.frame(cost = 1000)),
      newDemand("DEM", commodity = "ELC", demand = data.frame(demand = 1))))
}

# Both scenarios are deliberately given the SAME @name.
si_save <- function(region, name = "BY25_F4") {
  mod <- si_model(region)
  sc <- interpolate_model(mod, name = name)
  suppressMessages(save_scenario(sc, verbose = FALSE))
}

test_that("two scenarios sharing a name both reach the registry", {
  si_local()
  unlink(si_root(), recursive = TRUE)
  a <- si_save("AAA")
  b <- si_save("BBB")
  expect_false(identical(a@path, b@path))

  reg <- load_registry()
  rows <- find_in_registry(reg, type = "scenario", name = "BY25_F4")
  # the whole point: one row per folder, not one per name
  expect_identical(nrow(rows), 2L)
  expect_setequal(rows$model, c("CTRY_AAA", "CTRY_BBB"))
  expect_identical(length(unique(rows$path)), 2L)

  # recorded BY THE SAVE, not only by a later refresh_registry()
  expect_true(all(nzchar(rows$model)))
})

test_that("a name matching two folders is refused, not resolved", {
  si_local()
  unlink(si_root(), recursive = TRUE)
  si_save("AAA")
  si_save("BBB")
  # returning either one silently is what rendered Ukraine's report from
  # Switzerland's folder
  expect_error(getScenario("BY25_F4"), "matches 2 folders")
  expect_error(getScenario("BY25_F4"), "model = ")
})

test_that("model = picks the right one, and its data proves it", {
  si_local()
  unlink(si_root(), recursive = TRUE)
  si_save("AAA")
  si_save("BBB")

  sa <- getScenario("BY25_F4", model = "CTRY_AAA")
  sb <- getScenario("BY25_F4", model = "CTRY_BBB")
  expect_identical(sa@model@name, "CTRY_AAA")
  expect_identical(sb@model@name, "CTRY_BBB")
  # the region guard the reneuro reports had to add by hand
  expect_identical(get_region(sa@model), "AAA")
  expect_identical(get_region(sb@model), "BBB")
})

test_that("the accessor cache cannot serve one for the other", {
  si_local()
  unlink(si_root(), recursive = TRUE)
  si_save("AAA")
  si_save("BBB")
  # cached on the resolved path: the stamp is scenario.yml$updated at ONE
  # SECOND resolution, so two scenarios written in the same second of a
  # parallel pass share it and a name-keyed cache would hand back the wrong one
  for (i in 1:2) {
    expect_identical(getScenario("BY25_F4", model = "CTRY_AAA")@model@name,
                     "CTRY_AAA")
    expect_identical(getScenario("BY25_F4", model = "CTRY_BBB")@model@name,
                     "CTRY_BBB")
  }
})

test_that("refresh_registry keeps both rows and prunes only what is gone", {
  si_local()
  unlink(si_root(), recursive = TRUE)
  si_save("AAA")
  b <- si_save("BBB")

  reg <- refresh_registry(root = si_root())
  expect_identical(
    nrow(find_in_registry(reg, type = "scenario", name = "BY25_F4")), 2L)

  unlink(b@path, recursive = TRUE)
  reg <- refresh_registry(root = si_root())
  rows <- find_in_registry(reg, type = "scenario", name = "BY25_F4")
  expect_identical(nrow(rows), 1L)
  expect_identical(rows$model, "CTRY_AAA")
})

test_that("re-saving the same scenario still upserts, keeping created", {
  si_local()
  unlink(si_root(), recursive = TRUE)
  s <- si_save("AAA", name = "ONCE")
  created <- load_registry()$created[load_registry()$name == "ONCE"]
  Sys.sleep(1.1)                      # timestamps have 1 s resolution
  suppressMessages(save_scenario(s, verbose = FALSE))

  reg <- refresh_registry(root = si_root())
  rows <- find_in_registry(reg, type = "scenario", name = "ONCE")
  expect_identical(nrow(rows), 1L)    # one folder, one row
  expect_identical(rows$created, created)
})

test_that("a loaded scenario reads the same from any working directory", {
  si_local()
  unlink(si_root(), recursive = TRUE)
  s <- si_save("AAA", name = "RELPATH")
  ref <- as.data.frame(getData(s, "pTimesliceWeight", merge = TRUE))

  # load by a RELATIVE path, as a driver script does
  wd <- getwd()
  on.exit(setwd(wd), add = TRUE)
  setwd(dirname(s@path))
  rel <- basename(s@path)
  back <- suppressMessages(load_scenario(rel, env = NULL, verbose = FALSE))

  # ... then move away: on-disk slots rebased onto a relative root would now
  # fail with "On-disk data expected but not found"
  setwd(tempdir())
  expect_equal(as.data.frame(getData(back, "pTimesliceWeight", merge = TRUE)), ref)
})

test_that("a failed on-disk read is distinguishable from absent data", {
  si_local()
  unlink(si_root(), recursive = TRUE)
  s <- si_save("AAA", name = "CLASSED")
  expect_gt(nrow(as.data.frame(getData(s, "pTimesliceWeight", merge = TRUE))), 0L)

  # break the store the way a moved folder does
  moved <- paste0(s@path, "-moved")
  file.rename(s@path, moved)
  on.exit(file.rename(moved, s@path), add = TRUE)

  err <- tryCatch(getData(s, "pTimesliceWeight", merge = TRUE),
                  error = function(e) e)
  skip_if_not(inherits(err, "error"),
              "this parameter is not read through the on-disk path")
  expect_s3_class(err, "energyRt_ondisk_missing")
  # the message is unchanged, so anything matching on it keeps working
  expect_match(conditionMessage(err), "On-disk data expected but not found")
})

test_that("a loaded scenario's slots stay on disk, and getData() still reads", {
  # Documented on load_scenario(): @slot reads come back EMPTY on a loaded
  # scenario while getData() works. Pinned here because the symptom is an
  # empty table rather than an error, which is how it reached a report.
  #
  # There is no supported materialise step to test: obj2mem() is exported but
  # fails on a loaded scenario -- it treats a table-metadata record
  # (class/dim/length/size) as if its keys were list elements, and reads $dim
  # off entries that do not carry one. Fixing that is its own change.
  si_local()
  unlink(si_root(), recursive = TRUE)
  s <- si_save("AAA", name = "ONDISK")
  back <- suppressMessages(load_scenario(s@path, env = NULL, verbose = FALSE))

  expect_false(isInMemory(back))
  expect_gt(nrow(as.data.frame(
    getData(back, "pTimesliceWeight", merge = TRUE))), 0L)
})

test_that("an ambiguous model name is refused rather than picked", {
  si_local()
  unlink(si_root(), recursive = TRUE)
  mod <- si_model("AAA")
  suppressMessages(save_model(mod, verbose = FALSE))
  # a second folder for the same model name, the versioned layout
  d <- .model_store_resolve("CTRY_AAA", NULL)
  d2 <- paste0(d, "@deadbeef")
  dir.create(d2, showWarnings = FALSE)
  file.copy(fp(d, "model.yml"), fp(d2, "model.yml"))
  on.exit(unlink(d2, recursive = TRUE), add = TRUE)

  # the registry branch used to return path[1]; it must now fall through to
  # the store scan, which refuses and names the versions
  refresh_registry(root = si_root())
  expect_error(suppressMessages(load_model("CTRY_AAA", verbose = FALSE)),
               "versions")
})
