# =========================================================================== #
# The `get_process_*()` family takes a `process` filter. Six of the eight
# accepted it and ignored it: `get_process_inputs()`, `_outputs()`, `_aux()`,
# `_region()`, `_invest_window()` and `_timeframe()` returned every process,
# and `get_process_comm()` inherited that through the three it delegates to.
# A caller asking for one process got the whole model, silently -- the shape
# was right, so the answer looked right.
#
# @covers depth=S backends=none
# =========================================================================== #

pa_scen <- local({
  mod <- ut_build("R3", calendar = "topia_seasons")
  suppressMessages(suppressWarnings(
    interpolate_model(mod, name = "pa_accessors", overwrite = TRUE)))
})

# the list-returning members, and the processes to ask each one for
pa_list_fns <- list(
  get_process_class     = c("ECOA", "STG_ELC"),
  get_process_comm      = c("ECOA", "STG_ELC"),
  get_process_inputs    = c("ECOA", "EGAS"),
  get_process_outputs   = c("ECOA", "SUP_COA"),
  get_process_aux       = "ECOA",
  get_process_stored    = "STG_ELC",
  get_process_timeframe = c("ECOA", "SUP_COA")
)

test_that("every list-returning process accessor honours `process`", {
  for (nm in names(pa_list_fns)) {
    f <- get(nm, envir = asNamespace("energyRt"))
    want <- pa_list_fns[[nm]]
    all_of_them <- f(pa_scen)
    picked <- f(pa_scen, process = want)
    expect_setequal(names(picked), want)
    # a subset, never a recomputation: `get_process_stored()` sees only the
    # kit's single storage, so this is >=, and the strict reduction is
    # asserted below on a family with many members
    expect_true(length(picked) <= length(all_of_them),
                label = paste0(nm, ": filtered result is larger"))
    for (p in want) expect_equal(picked[[p]], all_of_them[[p]], label = nm)
  }
})

test_that("asking for two of many processes returns two", {
  # the regression itself: before the fix these returned the whole model
  expect_length(get_process_inputs(pa_scen, process = c("ECOA", "EGAS")), 2L)
  expect_true(length(get_process_inputs(pa_scen)) > 2L)
  expect_length(get_process_comm(pa_scen, process = c("ECOA", "EGAS")), 2L)
  expect_true(length(get_process_comm(pa_scen)) > 2L)
})

test_that("`get_process_region()` honours `process` in both return shapes", {
  reg_list <- get_process_region(pa_scen, process = "ECOA")
  expect_named(reg_list, "ECOA")

  reg_df <- get_process_region(pa_scen, process = "ECOA", return_list = FALSE)
  expect_setequal(unique(reg_df$process), "ECOA")
  expect_setequal(reg_df$region, reg_list[["ECOA"]])
})

test_that("`get_process_invest_window()` honours `process`", {
  win <- get_process_invest_window(pa_scen, process = c("ECOA", "EWIN"))
  expect_setequal(unique(win$process), c("ECOA", "EWIN"))
  expect_true(nrow(get_process_invest_window(pa_scen)) > nrow(win))
})

test_that("a process that does not exist filters to nothing, it does not fall back", {
  # the failure mode this family had: an unmatched filter returning everything
  expect_length(get_process_inputs(pa_scen, process = "NO_SUCH_PROCESS"), 0L)
  expect_length(get_process_comm(pa_scen, process = "NO_SUCH_PROCESS"), 0L)
  expect_equal(nrow(get_process_invest_window(pa_scen,
                                              process = "NO_SUCH_PROCESS")), 0L)
  expect_equal(nrow(get_process_region(pa_scen, process = "NO_SUCH_PROCESS",
                                       return_list = FALSE)), 0L)
})

test_that("`process` and `classes` compose", {
  # ECOA is a technology, STG_ELC a storage: asking for both but keeping only
  # storages leaves one
  expect_named(get_process_class(pa_scen, process = c("ECOA", "STG_ELC"),
                                 classes = "storage"), "STG_ELC")
  expect_named(get_process_stored(pa_scen, process = "STG_ELC",
                                  classes = "storage"), "STG_ELC")
})
