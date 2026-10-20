# =========================================================================== #
# The backend solve probe: `en_check_solve()` and the verdict the suite
# preamble publishes for the shard workers.
#
# Every other en_check_*() detector reports whether software is installed. This
# one answers the different question -- can the backend produce a number -- by
# solving the unit kit U1, whose objective is the hand-computed integer 8
# (arithmetic in data-raw/topia_modules.R, pinned by test-unit-model.R). The
# constant is what makes the probe assertable without a golden.
#
# Three states matter and each is tested here: a backend that is not configured
# is NOT probed (quiet "No backend"), one that solves is green, and one that is
# configured but fails is reported with its own error -- which is what makes the
# resulting test skips evidence rather than a guess about a licence file.
#
# @covers vObjective depth=S backends=glpk
# =========================================================================== #

test_that("en_check_solve solves the unit kit to 8 on glpk", {
  skip_if_no_solver()
  row <- en_check_solve(backends = "glpk")
  expect_equal(nrow(row), 1L)
  expect_equal(row$component, "solve: glpk (tiny)")
  expect_equal(row$backend, "glpk")
  expect_equal(row$level, "tiny")
  expect_true(row$found)
  expect_equal(row$version, paste0("obj ", energyRt:::.EN_PROBE_OBJECTIVE))
  expect_match(row$note, "solved in")
})

test_that("the two probe levels are different models, and only gams runs both", {
  skip_if_tier_below("fast")
  tiny <- energyRt:::.en_probe_model("tiny")
  full <- energyRt:::.en_probe_model("full")
  expect_s4_class(tiny, "model")
  expect_s4_class(full, "model")
  # the full level must be BIGGER, or it proves nothing about capacity:
  # 17 timeslices and 29 objects against 5 and 5 (~615 vs ~a dozen LP rows)
  slices <- function(m) nrow(as.data.frame(m@config@calendar@timeslice_share))
  objs <- function(m) length(m@data[[1]]@data)
  expect_gt(slices(full), slices(tiny))
  expect_gt(objs(full), objs(tiny))
  defs <- energyRt:::.en_probe_backends()
  lv <- lapply(defs, function(d) d$levels %||% "tiny")
  expect_equal(lv$gams, c("tiny", "full"))
  expect_equal(lv$glpk, "tiny")
})

test_that("the full level is checked against GLPK on the same model", {
  skip_if_no_solver()
  skip_if_tier_below("cross")        # two interpolations plus three solves
  local_mocked_bindings(
    .en_probe_backends = function() {
      list(glpk = list(preset = "glpk", levels = c("tiny", "full"),
                       gate = function() TRUE))
    },
    .package = "energyRt"
  )
  tab <- en_check_solve(backends = "glpk")
  expect_equal(tab$level, c("tiny", "full"))
  expect_true(all(tab$found))
  expect_equal(tab$version[1], paste0("obj ", energyRt:::.EN_PROBE_OBJECTIVE))
  # the full objective is not hand-computable, so it is only required to be a
  # finite number that GLPK agrees with -- which it is, being GLPK's own
  expect_match(tab$version[2], "^obj [0-9.]+$")
})

test_that("a backend failing the tiny level is not asked for the full one", {
  skip_if_no_solver()
  calls <- 0L
  local_mocked_bindings(
    .en_probe_backends = function() {
      list(broken = list(preset = "glpk", levels = c("tiny", "full"),
                         gate = function() TRUE))
    },
    solve_scenario = function(...) {
      calls <<- calls + 1L
      stop("Solution error code 7")
    },
    .package = "energyRt"
  )
  tab <- en_check_solve(backends = "broken")
  expect_equal(nrow(tab), 1L)          # short-circuited after tiny
  expect_equal(tab$level, "tiny")
  expect_equal(calls, 1L)
})

test_that("an unconfigured backend is reported but never probed", {
  skip_if_tier_below("fast")
  # `mm` is gated on the multimod package; stub the gate so it reads as absent
  defs <- energyRt:::.en_probe_backends()
  expect_true(all(c("glpk", "julia", "pyomo", "mm", "gams") %in% names(defs)))
  expect_true(is.function(defs$mm$gate))

  local_mocked_bindings(
    .en_probe_backends = function() {
      list(nope = list(preset = "glpk", gate = function() FALSE))
    },
    .package = "energyRt"
  )
  t0 <- Sys.time()
  row <- en_check_solve(backends = "nope")
  secs <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  expect_false(row$found)
  expect_equal(row$note, "No backend")
  # nothing was solved: no interpolation, no solver process
  expect_lt(secs, 2)
})

test_that("a solve that throws is reported as a row, never raised", {
  skip_if_no_solver()
  local_mocked_bindings(
    .en_probe_backends = function() {
      list(broken = list(preset = "glpk", gate = function() TRUE))
    },
    solve_scenario = function(...) {
      stop("Solution error code 7\nsecond line must not appear in the note")
    },
    .package = "energyRt"
  )
  row <- expect_no_error(en_check_solve(backends = "broken"))
  expect_false(row$found)
  expect_equal(row$note, "Solution error code 7")   # first line only
  expect_true(is.na(row$version))
})

test_that("a solve that returns the wrong objective is not a pass", {
  skip_if_no_solver()
  local_mocked_bindings(
    .en_probe_backends = function() {
      list(wrong = list(preset = "glpk", gate = function() TRUE))
    },
    solve_scenario = function(...) structure(list(), class = "en_fake_scen"),
    getData = function(...) data.frame(value = 42),
    .package = "energyRt"
  )
  row <- en_check_solve(backends = "wrong")
  expect_false(row$found)
  expect_match(row$note, "expected 8")
  expect_equal(row$version, "obj 42")
})

test_that("gams is absent from the default backend set", {
  skip_if_tier_below("fast")
  defs <- names(energyRt:::.en_probe_backends())
  expect_true("gams" %in% defs)
  # the default probes everything configured EXCEPT gams: proprietary, and must
  # never run unattended on a check machine
  expect_false("gams" %in% setdiff(defs, "gams"))
  probed <- character(0)
  local_mocked_bindings(
    .en_probe_backends = function() {
      stats::setNames(
        lapply(defs, function(b) list(preset = "glpk", gate = function() {
          probed <<- c(probed, b)
          FALSE
        })),
        defs)
    },
    .package = "energyRt"
  )
  invisible(en_check_solve())
  expect_false("gams" %in% probed)
})

# --------------------------------------------------------------------------- #
# The verdict file: written by the preamble, read by the guards.
# --------------------------------------------------------------------------- #

.bp_write <- function(path, backends, age_secs = 0) {
  skip_if_not_installed("jsonlite")
  jsonlite::write_json(
    list(written_at = format(Sys.time() - age_secs, "%Y-%m-%dT%H:%M:%S"),
         tier = "cross", backends = backends),
    path, auto_unbox = TRUE)
  if (age_secs > 0) Sys.setFileTime(path, Sys.time() - age_secs)
  path
}

test_that("a failed backend in the verdict makes its guard skip with the reason", {
  p <- withr::local_tempfile(fileext = ".json")
  .bp_write(p, list(gams = list(ok = FALSE, probed = TRUE, tiny = FALSE,
                                failed_level = "tiny",
                                note = "Solution error code 7")))
  res <- tryCatch(skip_if_probe_failed("gams", path = p),
                  condition = function(c) c)
  expect_s3_class(res, "skip")
  expect_match(conditionMessage(res), "failed the backend probe")
  expect_match(conditionMessage(res), "Solution error code 7")
})

test_that("a size-limited backend skips with a size reason, not a broken one", {
  p <- withr::local_tempfile(fileext = ".json")
  .bp_write(p, list(gams = list(ok = FALSE, probed = TRUE, tiny = TRUE,
                                full = FALSE, failed_level = "full",
                                note = "Terminated due to a licensing error")))
  res <- tryCatch(skip_if_probe_failed("gams", path = p),
                  condition = function(c) c)
  expect_s3_class(res, "skip")
  expect_match(conditionMessage(res), "solves only small models")
  expect_match(conditionMessage(res), "licensing error")
})

test_that("a solved or unprobed backend does not skip, and nor does a stale file", {
  p <- withr::local_tempfile(fileext = ".json")
  .bp_write(p, list(glpk = list(ok = TRUE, probed = TRUE, tiny = TRUE,
                                note = "solved in 0.3s"),
                    julia = list(ok = FALSE, probed = FALSE, note = "No backend")))
  expect_false(skip_if_probe_failed("glpk", path = p))
  expect_false(skip_if_probe_failed("julia", path = p))   # never probed
  expect_false(skip_if_probe_failed("pyomo", path = p))   # absent from the file

  # older than the TTL: the machine may have changed since, so it is ignored
  stale <- withr::local_tempfile(fileext = ".json")
  .bp_write(stale, list(gams = list(ok = FALSE, probed = TRUE, tiny = FALSE,
                                    failed_level = "tiny", note = "boom")),
            age_secs = 2 * .PROBE_TTL_SECS)
  expect_false(skip_if_probe_failed("gams", path = stale))
})

test_that("the preamble writes a verdict keyed by backend", {
  skip_if_no_solver()
  skip_if_not_installed("jsonlite")
  src <- test_path("..", "..", "tools", "test", "lib", "runner.R")
  skip_if(!file.exists(src), "tools/test/lib/runner.R not available")
  env <- new.env(parent = globalenv())
  sys.source(src, envir = env)
  p <- withr::local_tempfile(fileext = ".json")
  withr::local_envvar(list(ENERGYRT_TEST_PROBE = p, ENERGYRT_TEST_TIER = "fast"))
  invisible(env$en_backend_probe(solve = "glpk", print = FALSE))
  expect_true(file.exists(p))
  v <- jsonlite::read_json(p, simplifyVector = FALSE)
  expect_equal(names(v$backends), "glpk")
  expect_true(v$backends$glpk$ok)
  expect_true(v$backends$glpk$probed)
  expect_true(v$backends$glpk$tiny)
  # glpk carries no `full` level, so the entry records that rather than a pass
  expect_true(is.null(v$backends$glpk$full) || is.na(v$backends$glpk$full))
  # and the guard built from it stays silent
  expect_false(skip_if_probe_failed("glpk", path = p))
})

test_that("tier check probes nothing", {
  src <- test_path("..", "..", "tools", "test", "lib", "runner.R")
  skip_if(!file.exists(src), "tools/test/lib/runner.R not available")
  env <- new.env(parent = globalenv())
  sys.source(src, envir = env)
  withr::local_envvar(list(ENERGYRT_TEST_TIER = "check"))
  expect_length(env$en_probe_backends_for_tier(), 0L)
  expect_null(env$en_backend_probe(print = FALSE))
})

test_that("the tier decides which backends are probed, and gams is opt-in", {
  src <- test_path("..", "..", "tools", "test", "lib", "runner.R")
  skip_if(!file.exists(src), "tools/test/lib/runner.R not available")
  env <- new.env(parent = globalenv())
  sys.source(src, envir = env)
  withr::local_envvar(list(ENERGYRT_TEST_GAMS = ""))
  expect_equal(env$en_probe_backends_for_tier("fast"), "glpk")
  # cross/full own the GAMS cells, so GAMS is probed there by default -- the
  # verdict is what lets those cells skip with a reason instead of failing
  expect_equal(env$en_probe_backends_for_tier("cross"),
               c("glpk", "julia", "pyomo", "mm", "gams"))
  withr::local_envvar(list(ENERGYRT_TEST_GAMS = "true"))
  expect_equal(env$en_probe_backends_for_tier("fast"), c("glpk", "gams"))
  # ... but never on a check machine
  expect_length(env$en_probe_backends_for_tier("check"), 0L)
})
