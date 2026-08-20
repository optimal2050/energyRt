# ============================================================================ #
# mosox backend
#
# mosox is a standalone GMPL matrix generator with a HiGHS solver attached
# (`compile` -> MPS, `solve` -> HiGHS, plus `normalize`/`compare` for diffing
# two matrices). energyRt wires it in as a RUNNER only: `en_mosox_run()` shells
# out to the executable. It is NOT a `solver_options` backend, so nothing here
# goes through `solve_scen()` except the integration check in group C.
#
# Measured against **mosox 0.6.2**. Three groups, deliberately separated
# (same convention as test-convert.R):
#
#   A. contract      the wrapper's own behaviour; needs no executable.
#   B. with mosox    real runs; skipped when mosox is not installed.
#   C. KNOWN-WRONG   behaviour of mosox 0.6.2 (or of the wrapper) that is wrong
#                    or missing today. Each pins the current answer with the
#                    intended one in a FIXME, so the fix flips it in one obvious
#                    diff. A failure here after a mosox upgrade is the goal, not
#                    a regression.
#
# Numbers were MEASURED against 0.6.2, not derived. Do not "tidy" one without
# re-measuring.
# ============================================================================ #

# -- helpers and fixtures ----------------------------------------------------

skip_if_no_mosox <- function() {
  testthat::skip_on_cran()
  exe <- getFromNamespace(".find_mosox", "energyRt")()
  # `.find_mosox()` returns the bare "mosox" when it found nothing, so test both
  # a resolved path and PATH before deciding it is absent.
  if (!file.exists(exe) && !nzchar(Sys.which(exe))) {
    testthat::skip("mosox not available (PATH or ~/.energyRt/config.yml)")
  }
  invisible(exe)
}

# Set `mosox_path` for one test and put back whatever the config file applied.
# The raw option is used rather than `set_mosox_path()` because the latter
# refuses a directory that does not exist, which some of these tests need.
mx_with_path <- function(path, code) {
  saved <- getOption("en.mosox_path")
  on.exit(options(en.mosox_path = saved), add = TRUE)
  options(en.mosox_path = path)
  force(code)
}

# A tiny LP with distinct primal values AND distinct duals, so a shifted,
# reversed or transposed solution vector cannot pass by coincidence:
#   min 2x + 5y + 9z   s.t.  x >= 3, y >= 7, z >= 11
#   objective 140; x,y,z = 3,7,11; duals 2,5,9; reduced costs 0.
mx_lp <- c(
  "var x >= 0;",
  "var y >= 0;",
  "var z >= 0;",
  "minimize obj: 2*x + 5*y + 9*z;",
  "s.t. c1: x >= 3;",
  "s.t. c2: y >= 7;",
  "s.t. c3: z >= 11;",
  "end;"
)

# The same problem split across a model and a data file, to exercise the
# optional `data` argument and set/param resolution:
#   min sum c[i] x[i]  s.t. sum x[i] >= 10, c = (2, 5)  ->  objective 20.
mx_sep_mod <- c(
  "set I;",
  "param c{I};",
  "param b;",
  "var x{i in I} >= 0;",
  "minimize obj: sum{i in I} c[i] * x[i];",
  "s.t. dem: sum{i in I} x[i] >= b;",
  "end;"
)
mx_sep_dat <- c(
  "set I := a b;",
  "param c := a 2 b 5;",
  "param b := 10;",
  "end;"
)

# Write a fixture into a directory and return its path.
mx_write <- function(dir, file, lines) {
  path <- file.path(dir, file)
  writeLines(lines, path)
  path
}

# `-f csv` output as a data frame, plus pickers.
mx_csv <- function(path) utils::read.csv(path, stringsAsFactors = FALSE)
mx_val <- function(d, type, name) d$value[d$type == type & d$name %in% name]
mx_marg <- function(d, type, name) d$marginal[d$type == type & d$name %in% name]


# ============================================================================ #
# A. contract -- no executable needed
# ============================================================================ #

test_that("mosox path helpers and wrapper expose a simple API", {
  expect_true(exists("set_mosox_path", mode = "function"))
  expect_true(exists("get_mosox_path", mode = "function"))
  expect_true(exists("en_mosox_run", mode = "function"))
  expect_true(exists("mosox_run", mode = "function"))
  expect_true(all(c("set_mosox_path", "get_mosox_path", "en_mosox_run",
                    "mosox_run") %in% getNamespaceExports("energyRt")))
})

test_that("set_mosox_path()/get_mosox_path() round-trip and clear", {
  dir <- normalizePath(tempdir(), winslash = "/")
  mx_with_path(NULL, {
    set_mosox_path(dir)
    expect_identical(get_mosox_path(), get_solver_path("mosox"))
    expect_match(get_mosox_path(), "/$")
    set_mosox_path(NULL)
    expect_null(get_mosox_path())
  })
})

test_that("`model` must be a non-empty path", {
  expect_error(en_mosox_run(), "non-empty path")
  expect_error(en_mosox_run(model = ""), "non-empty path")
})

test_that("`command` is compile or solve", {
  expect_error(en_mosox_run(model = "m.mod", command = "wat"),
               "should be one of")
})

test_that("a configured directory wins over PATH discovery", {
  dir <- withr::local_tempdir()
  # `.find_mosox()` probes "mosox" then "mosox.exe" in the configured directory
  # and takes the first that exists, so only the .exe is planted here: it is the
  # candidate that resolves identically on both platforms.
  file.create(file.path(dir, "mosox.exe"))
  mx_with_path(dir, {
    exe <- getFromNamespace(".find_mosox", "energyRt")()
    expect_true(file.exists(exe))
    expect_identical(basename(exe), "mosox.exe")
    expect_identical(normalizePath(dirname(exe), winslash = "/"),
                     normalizePath(dir, winslash = "/"))
  })
})

test_that("missing mosox binary errors clearly", {
  # An empty directory sends `.find_mosox()` down its fallback path. Whether it
  # ends at the bare "mosox" (nothing installed) or finds a real mosox on PATH,
  # the run must fail loudly -- the model file does not exist either.
  mx_with_path(tempdir(), {
    expect_error(
      en_mosox_run(model = tempfile(fileext = ".mod"), check = TRUE),
      regexp = "mosox executable was not found|mosox failed"
    )
  })
})


# ============================================================================ #
# B. with mosox installed
# ============================================================================ #

test_that("compile turns a GMPL model into an MPS matrix", {
  skip_if_no_mosox()
  dir <- withr::local_tempdir()
  mod <- mx_write(dir, "lp.mod", mx_lp)
  out <- file.path(dir, "lp.mps")

  res <- en_mosox_run(model = mod, output = out, command = "compile")
  expect_true(res$success)
  expect_identical(res$command, "compile")
  expect_true(file.exists(out))

  mps <- readLines(out)
  expect_true(all(c("ROWS", "COLUMNS", "RHS", "BOUNDS", "ENDATA") %in% mps))
  # The problem name is taken from the model file stem.
  expect_true("NAME lp" %in% mps)
  # One N row for the objective and one G row per constraint.
  expect_identical(sum(grepl("^ N  ", mps)), 1L)
  expect_identical(sum(grepl("^ G  c[123]$", mps)), 3L)
  # Objective and constraint coefficients survive the trip.
  expect_true(all(c(" x obj 2", " y obj 5", " z obj 9") %in% mps))
  expect_true(all(c(" x c1 1", " y c2 1", " z c3 1") %in% mps))
  expect_true(all(c(" RHS1 c1 3", " RHS1 c2 7", " RHS1 c3 11") %in% mps))
})

test_that("compile reads a separate data file", {
  skip_if_no_mosox()
  dir <- withr::local_tempdir()
  mod <- mx_write(dir, "sep.mod", mx_sep_mod)
  dat <- mx_write(dir, "sep.dat", mx_sep_dat)
  out <- file.path(dir, "sep.mps")

  res <- en_mosox_run(model = mod, data = dat, output = out,
                      command = "compile")
  expect_true(res$success)
  expect_true(any(grepl("sep\\.dat$", res$args)))

  mps <- readLines(out)
  # The set I resolved, so the indexed variable expanded to one column each ...
  expect_true(all(c(" x[a] obj 2", " x[b] obj 5") %in% mps))
  # ... and `param b` reached the RHS.
  expect_true(" RHS1 dem 10" %in% mps)
})

test_that("an omitted output path gets a per-command temporary file", {
  skip_if_no_mosox()
  dir <- withr::local_tempdir()
  mod <- mx_write(dir, "lp.mod", mx_lp)

  cmp <- en_mosox_run(model = mod, command = "compile")
  expect_match(cmp$output, "\\.mps$")
  expect_true(file.exists(cmp$output))

  slv <- en_mosox_run(model = mod, command = "solve")
  expect_match(slv$output, "\\.txt$")
  expect_true(file.exists(slv$output))
})

test_that("a missing output directory is created", {
  skip_if_no_mosox()
  dir <- withr::local_tempdir()
  mod <- mx_write(dir, "lp.mod", mx_lp)
  out <- file.path(dir, "no", "such", "dir", "lp.mps")

  expect_false(dir.exists(dirname(out)))
  res <- en_mosox_run(model = mod, output = out, command = "compile")
  expect_true(res$success)
  expect_true(file.exists(out))
})

test_that("solve reports the optimal objective", {
  skip_if_no_mosox()
  dir <- withr::local_tempdir()
  mod <- mx_write(dir, "lp.mod", mx_lp)
  out <- file.path(dir, "lp.txt")

  res <- en_mosox_run(model = mod, output = out, command = "solve")
  expect_true(res$success)
  sol <- readLines(out)
  expect_identical(sol[1], "OBJECTIVE VALUE")
  expect_equal(as.numeric(sol[2]), 140)
  # The run's own log carries it too, which is what a caller greps when the
  # output file is thrown away.
  expect_true(any(grepl("Objective value: 140", res$stdout)))
})

test_that("solve reads a separate data file", {
  skip_if_no_mosox()
  dir <- withr::local_tempdir()
  mod <- mx_write(dir, "sep.mod", mx_sep_mod)
  dat <- mx_write(dir, "sep.dat", mx_sep_dat)
  out <- file.path(dir, "sep.txt")

  res <- en_mosox_run(model = mod, data = dat, output = out, command = "solve")
  expect_true(res$success)
  expect_equal(as.numeric(readLines(out)[2]), 20)
})

test_that("extra_args reach the CLI: `-f csv` and `-c KEY=VALUE`", {
  skip_if_no_mosox()
  dir <- withr::local_tempdir()
  mod <- mx_write(dir, "lp.mod", mx_lp)
  out <- file.path(dir, "lp.csv")

  res <- en_mosox_run(model = mod, output = out, command = "solve",
                      extra_args = c("-f", "csv", "-c", "threads=1"))
  expect_true(res$success)
  expect_true(all(c("-f", "csv", "-c", "threads=1") %in% res$args))

  d <- mx_csv(out)
  expect_identical(names(d), c("type", "name", "value", "marginal"))
  expect_equal(mx_val(d, "objective", "obj"), 140)
  # Constraint activities and duals are both correct at 0.6.2.
  expect_equal(mx_val(d, "constraint", c("c1", "c2", "c3")), c(3, 7, 11))
  expect_equal(mx_marg(d, "constraint", c("c1", "c2", "c3")), c(2, 5, 9))
})

test_that("a solver option mosox rejects is a failure, not a silent default", {
  skip_if_no_mosox()
  dir <- withr::local_tempdir()
  mod <- mx_write(dir, "lp.mod", mx_lp)

  res <- suppressWarnings(
    en_mosox_run(model = mod, command = "solve", check = FALSE,
                 extra_args = c("-c", "no_such_highs_option=1")))
  expect_false(res$success)
  expect_true(any(grepl("no_such_highs_option", res$stdout)))
})

test_that("check = FALSE keeps mosox's diagnostics instead of throwing", {
  skip_if_no_mosox()
  dir <- withr::local_tempdir()
  bad <- mx_write(dir, "bad.mod", c("var x >= 0;", "this is not GMPL;", "end;"))

  expect_error(
    suppressWarnings(en_mosox_run(model = bad, command = "compile",
                                  check = TRUE)),
    "mosox failed with exit status")

  res <- suppressWarnings(
    en_mosox_run(model = bad, command = "compile", check = FALSE))
  expect_false(res$success)
  expect_true(any(grepl("Error", res$stdout)))
})

test_that("mosox_run() is en_mosox_run()", {
  skip_if_no_mosox()
  dir <- withr::local_tempdir()
  mod <- mx_write(dir, "lp.mod", mx_lp)

  a <- en_mosox_run(model = mod, output = file.path(dir, "a.mps"))
  b <- mosox_run(model = mod, output = file.path(dir, "b.mps"))
  expect_true(b$success)
  expect_identical(readLines(a$output), readLines(b$output))
})


# ============================================================================ #
# C. KNOWN-WRONG / KNOWN-GAP -- pinned behaviour of mosox 0.6.2
# ============================================================================ #

test_that("KNOWN-WRONG: solve() shifts every variable value by one", {
  skip_if_no_mosox()
  dir <- withr::local_tempdir()
  mod <- mx_write(dir, "lp.mod", mx_lp)
  out <- file.path(dir, "lp.csv")
  en_mosox_run(model = mod, output = out, command = "solve",
               extra_args = c("-f", "csv"))
  d <- mx_csv(out)

  # The objective (140) and every constraint row are right, so the solve itself
  # is right; only the variable block is misread. 0.6.2 emits a spurious leading
  # 1.000 and then reports each variable's value one slot late, dropping the
  # last: reported == c(1, true[-length(true)]).
  expect_equal(mx_val(d, "var", c("x", "y", "z")),
               c(1, 3, 7))              # FIXME: c(3, 7, 11)
  # The consequence, stated plainly: the reported levels do not price out to the
  # reported objective, so any result read back from mosox would be nonsense.
  expect_equal(sum(mx_val(d, "var", c("x", "y", "z")) * c(2, 5, 9)),
               80)                      # FIXME: 140
})

test_that("KNOWN-WRONG: solve()'s txt output shifts the same way", {
  skip_if_no_mosox()
  dir <- withr::local_tempdir()
  mod <- mx_write(dir, "lp.mod", mx_lp)
  out <- file.path(dir, "lp.txt")
  en_mosox_run(model = mod, output = out, command = "solve")
  sol <- readLines(out)

  vars <- sol[seq(grep("^VARS$", sol) + 1L, length(sol))]
  vars <- vars[nzchar(vars)]
  expect_identical(
    vars,
    c("x = 1.000; marginal = 0.000",    # FIXME: x = 3.000
      "y = 3.000; marginal = 0.000",    # FIXME: y = 7.000
      "z = 7.000; marginal = 0.000")    # FIXME: z = 11.000
  )
})

test_that("KNOWN-WRONG: mosox cannot parse energyRt's GMPL (`prod{}`)", {
  # The GLPK template multiplies the weather factors into the availability
  # equations with GMPL's iterated product. mosox 0.6.2 has no `prod` aggregate,
  # so it stops at the first one -- meaning NO energyRt model compiles today,
  # whether or not it uses weather.
  code <- getFromNamespace(".modelCode", "energyRt")[["GLPK"]]
  expect_gt(length(grep("prod[{]", code)), 0)

  skip_if_no_mosox()
  dir <- withr::local_tempdir()
  mod <- mx_write(dir, "prod.mod", c(
    "set W;",
    "param p{W};",
    "var x >= 0;",
    "minimize obj: x;",
    "s.t. c1: x * prod{w in W} p[w] >= 1;",
    "end;"
  ))
  dat <- mx_write(dir, "prod.dat",
                  c("set W := w1;", "param p := w1 2;", "end;"))

  res <- suppressWarnings(
    en_mosox_run(model = mod, data = dat, command = "compile", check = FALSE))
  expect_false(res$success)                                  # FIXME: TRUE
  expect_true(any(grepl("Syntax error", res$stdout)))        # FIXME: none
})

test_that("KNOWN-WRONG: a real energyRt model does not compile", {
  # The end-to-end shape of the check above: write a scenario with the GLPK
  # writer and hand mosox the very files glpsol would read. This is the test
  # that flips green the day mosox becomes a usable energyRt backend.
  skip_if_no_mosox()
  dir <- withr::local_tempdir()

  cal <- newCalendar(
    timetable = make_timetable(
      struct = list(ANNUAL = "ANNUAL", SEASON = c("WIN", "SUM"))),
    name = "mx_cal")
  mod <- newModel(
    name = "mx", desc = "mosox smoke model", calendar = cal, region = "R1",
    horizon = newHorizon(2020:2030, intervals = 5), discount = 0.05,
    repo = newRepository(
      "mx_repo",
      newCommodity("COA", timeframe = "ANNUAL"),
      newCommodity("ELC", timeframe = "SEASON"),
      newSupply("SCOA", commodity = "COA", supply = data.frame(cost = 1)),
      newTechnology("ECOA", input = list(comm = "COA"),
                    output = list(comm = "ELC"),
                    invcost = data.frame(invcost = 100),
                    olife = list(olife = 20)),
      newDemand("DELC", commodity = "ELC", demand = data.frame(demand = 1))))

  scen <- suppressMessages(suppressWarnings(
    interpolate_model(mod, name = "mx")))
  suppressMessages(suppressWarnings(
    write_sc(scen, tmp.dir = dir, solver = solver_options$glpk, echo = FALSE)))
  expect_true(file.exists(file.path(dir, "energyRt.mod")))
  expect_true(file.exists(file.path(dir, "energyRt.dat")))

  res <- suppressWarnings(en_mosox_run(
    model = file.path(dir, "energyRt.mod"),
    data = file.path(dir, "energyRt.dat"),
    output = file.path(dir, "energyRt.mps"),
    command = "compile", check = FALSE))
  expect_false(res$success)                                    # FIXME: TRUE
  expect_true(any(grepl("Syntax error", res$stdout)))          # FIXME: none
  expect_false(file.exists(file.path(dir, "energyRt.mps")))    # FIXME: TRUE
})

test_that("KNOWN-GAP: en_mosox_run() cannot reach normalize or compare", {
  # mosox 0.6.2 ships four subcommands; the wrapper's match.arg() admits two, so
  # the MPS-diffing pair that would make cross-backend matrix comparison
  # possible is unreachable from R.
  expect_error(en_mosox_run(model = "m.mps", command = "normalize"),
               "should be one of")                       # FIXME: runs
  expect_error(en_mosox_run(model = "a.mps", command = "compare"),
               "should be one of")                       # FIXME: runs
})

test_that("KNOWN-WRONG: `compare` reports differences but exits 0", {
  # Anything wiring `compare` into R must parse stdout: a mismatch is NOT
  # visible in the exit status, so the usual `check = TRUE` contract would call
  # a failed comparison a success.
  skip_if_no_mosox()
  exe <- getFromNamespace(".find_mosox", "energyRt")()
  dir <- withr::local_tempdir()
  mod <- mx_write(dir, "lp.mod", mx_lp)

  a <- file.path(dir, "a.mps")
  en_mosox_run(model = mod, output = a, command = "compile")
  n <- file.path(dir, "a.norm.mps")
  system2(exe, c("normalize", a, n), stdout = TRUE, stderr = TRUE)
  expect_true(file.exists(n))

  # A one-coefficient edit, far outside the default 0.02 epsilon.
  b <- file.path(dir, "b.norm.mps")
  writeLines(sub("^z obj 9$", "z obj 900", readLines(n)), b)

  same <- system2(exe, c("compare", n, n), stdout = TRUE, stderr = TRUE)
  expect_true(any(grepl("Files match", same)))
  expect_identical(as.integer(attr(same, "status") %||% 0L), 0L)

  diff <- system2(exe, c("compare", n, b), stdout = TRUE, stderr = TRUE)
  expect_true(any(grepl("total differences", diff)))
  expect_identical(as.integer(attr(diff, "status") %||% 0L),
                   0L)                  # FIXME: non-zero on a mismatch
})

test_that("KNOWN-WRONG: `model = NULL` errors opaquely", {
  # `missing(model) || !nzchar(model)` folds a zero-length nzchar() into `||`,
  # so NULL never reaches the clear message that "" gets.
  expect_error(en_mosox_run(model = NULL),
               "missing value where TRUE/FALSE needed")
               # FIXME: "`model` must be a non-empty path."
})
