# =========================================================================== #
# `process` as a real model set: the union of the seven process classes,
# declared on every backend and selectable in getData().
#
# The set is ADDITIVE -- nothing indexes over it yet -- so no solution may move.
# Fixtures from helper-stock-path.R.
# =========================================================================== #

ps_path <- function(name) {
  gsub("[\\/]+", "/", file.path(tempdir(), "ps-suite", name))
}

# --- catalog --------------------------------------------------------------- #

test_that("`process` is a declared set, ahead of its children", {
  expect_true("process" %in% energyRt:::.dimSets)
  expect_true("process" %in% names(energyRt:::.modInp))
  expect_identical(energyRt:::.modInp$process$type, "set")
  expect_identical(energyRt:::.modInp$process$dimSets, "process")

  mi <- methods::new("modInp")
  expect_true("process" %in% names(mi@parameters))

  # GAMS domain-checks a subset against its parent, and sets.gms follows catalog
  # order, so `process` must be declared before every child set.
  ord <- match(c("process", "sup", "dem", "tech", "stg", "expp", "imp", "trade"),
               names(mi@parameters))
  expect_false(anyNA(ord))
  expect_true(all(ord[1] < ord[-1]))
})

# --- templates (read the baked source, no solve needed) -------------------- #

test_that("every backend that declares sets declares `process`", {
  code <- energyRt:::.modelCode
  # GLPK is the one backend where a missing declaration is fatal: the .dat
  # carries `set process := ...;` for an undeclared symbol.
  expect_true(any(grepl("^set process;", code$GLPK)))
  expect_true(any(grepl("^process ", code$GAMS)))
  # Julia and Pyomo declare no sets at all -- theirs come from the data file.
  expect_false(any(grepl("^set process", code$JuMP)))
  expect_false(any(grepl("^model[.]process", code$PYOMOConcrete)))
})

test_that("GAMS declares the seven classes as subsets of `process`", {
  gms <- energyRt:::.modelCode$GAMS
  for (child in c("sup", "dem", "tech", "stg", "trade", "expp", "imp")) {
    expect_true(any(grepl(paste0("^", child, "[(]process[)]"), gms)),
                label = paste0(child, "(process) declared"))
  }
  # `group` and `weather` are NOT processes and must stay root sets.
  expect_true(any(grepl("^group ", gms)))
  expect_false(any(grepl("^group[(]process[)]", gms)))
  expect_false(any(grepl("^weather[(]process[)]", gms)))
})

# --- membership ------------------------------------------------------------ #

test_that("the set is the union of the seven process classes", {
  sc <- interpolate_model(sp_tech(c(50, 50, 50), optret = FALSE, name = "psu"),
                          name = "psu", path = ps_path("psu"))
  st <- sc@modInp@sets
  expect_setequal(
    st$process,
    unique(c(st$sup, st$dem, st$tech, st$stg, st$expp, st$imp, st$trade))
  )
  expect_equal(anyDuplicated(st$process), 0L)

  # @sets never leaves R; @parameters is what the writers emit.
  par <- as.data.frame(sc@modInp@parameters$process@data)
  expect_setequal(par$process, st$process)
})

# --- getData selection ----------------------------------------------------- #

test_that("getData(process =) takes a logical, a name, or a regex", {
  skip_if_no_solver()
  sc <- interpolate_model(sp_tech(c(50, 50, 50), optret = FALSE, name = "psg"),
                          name = "psg", path = ps_path("psg"))
  sol <- solve_scenario(sc, transient = TRUE)

  # TRUE: rename only -- today's behaviour, unchanged.
  d1 <- getData(sol, name = "vTechOut", merge = TRUE, process = TRUE)
  expect_true("process" %in% names(d1))
  expect_false("tech" %in% names(d1))
  expect_gt(nrow(d1), 0)
  procs <- unique(as.character(d1$process))

  # A character value SELECTS and renames.
  pick <- procs[1]
  d2 <- getData(sol, name = "vTechOut", merge = TRUE, process = pick)
  expect_true("process" %in% names(d2))
  expect_setequal(unique(as.character(d2$process)), pick)
  expect_gt(nrow(d2), 0)

  # The regex form reaches `...` untouched and is applied the same way.
  d3 <- getData(sol, name = "vTechOut", merge = TRUE,
                process_ = paste0("^", pick, "$"))
  expect_true("process" %in% names(d3))
  expect_setequal(unique(as.character(d3$process)), pick)

  # Same rows either way.
  expect_equal(nrow(d2), nrow(d3))

  # A name that matches nothing yields no rows -- and must not error.
  d4 <- getData(sol, name = "vTechOut", merge = TRUE,
                process = "NO_SUCH_PROCESS")
  expect_equal(nrow(d4), 0L)
})

test_that("a process selection does not leak rows from other classes", {
  skip_if_no_solver()
  sc <- interpolate_model(sp_tech(c(50, 50, 50), optret = FALSE, name = "psx"),
                          name = "psx", path = ps_path("psx"))
  sol <- solve_scenario(sc, transient = TRUE)

  all_out <- getData(sol, name = "vTechOut", merge = TRUE, process = TRUE)
  pick <- unique(as.character(all_out$process))[1]
  one <- getData(sol, name = "vTechOut", merge = TRUE, process = pick)
  expect_lte(nrow(one), nrow(all_out))
  expect_setequal(unique(as.character(one$process)), pick)
})

# --- the invariant that keeps the union honest ------------------------------ #

test_that("a name shared across two process classes is still refused", {
  # A single `process` set is only safe because a name cannot be reused across
  # classes. Two layers enforce it, and the union relies on both.
  base <- function() {
    newModel(
      name = "psd", region = "R1", horizon = sp_hor(), calendar = sp_cal(),
      repo = newRepository(
        "r1",
        newCommodity("ELC", timeframe = "HOUR"),
        newTechnology("DUP", output = data.frame(comm = "ELC", unit = "GWh"),
                      cap2act = 1)
      )
    )
  }

  # (1) within one repository: caught at construction
  expect_error(
    newRepository("r", newTechnology("DUP", output = data.frame(comm = "ELC",
                                                               unit = "GWh")),
                  newSupply("DUP", commodity = "ELC")),
    "already exists"
  )

  # (2) across repositories, different classes: caught at the model level, which
  # is the check `sets$process` actually depends on
  expect_error(
    add(base(), newRepository("r2", newSupply("DUP", commodity = "ELC"))),
    "Duplicated objects"
  )

  # a distinct name is accepted, so the errors above are about the name
  expect_s4_class(
    add(base(), newRepository("r2", newSupply("SUP1", commodity = "ELC"))),
    "model"
  )
})
