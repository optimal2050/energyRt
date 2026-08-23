# Tests for the session-options registry, the toolchain path accessors and the
# persisted configuration file (R/options.R, R/config_file.R, R/zzz.R).

# Save every option, run `code`, restore. The registry is global session state,
# so a test that leaks a value changes the behaviour of every test after it.
with_clean_options <- function(code) {
  nms <- .en_option_names()
  saved <- lapply(paste0("en.", nms), function(n) getOption(n))
  names(saved) <- paste0("en.", nms)
  applied <- .en_state$config_applied
  on.exit({
    do.call(options, saved)
    .en_state$config_applied <- applied
  }, add = TRUE)
  force(code)
}

# Option and environment-variable names ---------------------------------------

test_that("every option is read from en.<name>", {
  nms <- .en_option_names()
  expect_gt(length(nms), 10)
  with_clean_options({
    for (n in nms) {
      options(structure(list("SENTINEL"), names = paste0("en.", n)))
      expect_identical(get_option(n), "SENTINEL",
                       info = paste("option name for", n))
    }
  })
})

test_that("every option falls back to ENERGYRT_<NAME>", {
  # `neos_email` is the deliberate exception: `set_neos_email()` exports the
  # bare NEOS_EMAIL so the Pyomo subprocess inherits it.
  nms <- setdiff(.en_option_names(), "neos_email")
  with_clean_options({
    for (n in nms) {
      var <- toupper(paste0("ENERGYRT_", n))
      options(structure(list(NULL), names = paste0("en.", n)))
      old <- Sys.getenv(var, unset = NA)
      do.call(Sys.setenv, structure(list("SENTINEL"), names = var))
      expect_identical(get_option(n), "SENTINEL",
                       info = paste("envvar name for", n))
      if (is.na(old)) Sys.unsetenv(var) else {
        do.call(Sys.setenv, structure(list(old), names = var))
      }
    }
  })
})

test_that("neos_email reads the bare NEOS_EMAIL environment variable", {
  with_clean_options({
    options(en.neos_email = NULL)
    old <- Sys.getenv("NEOS_EMAIL", unset = NA)
    on.exit({
      if (is.na(old)) Sys.unsetenv("NEOS_EMAIL")
      else Sys.setenv(NEOS_EMAIL = old)
    }, add = TRUE)
    Sys.setenv(NEOS_EMAIL = "someone@example.org")
    expect_identical(get_neos_email(), "someone@example.org")
  })
})

test_that(".en_option_names() keeps options whose value is NULL", {
  # `names(options::opts())` drops them, because assigning NULL to a list
  # element removes it. This is what the helper exists to avoid.
  nms <- .en_option_names()
  expect_true(all(c("gams_path", "julia_path", "python_path", "glpk_path",
                    "gdxlib_path", "mosox_path", "neos_email") %in% nms))
})

test_that("declared defaults are what the getters return", {
  with_clean_options({
    for (n in .en_option_names()) {
      options(structure(list(NULL), names = paste0("en.", n)))
    }
    expect_identical(get_scenarios_path(), "scenarios/")
    expect_identical(get_arrow_format(), "feather")
    expect_identical(get_arrow_compression(), "zstd")
    expect_identical(get_arrow_compression_level(), 15L)
    expect_identical(get_default_solver(), list(name = "glpk", lang = "GLPK"))
    expect_identical(get_registry_file(), "energyRt_registry.csv")
    expect_identical(get_models_path(), "models/")
    expect_false(isVerbose())
    expect_false(isDebug())
  })
})

test_that("the default solver matches the solver_options preset it names", {
  # `lang` used to be "glpk" here and "GLPK" in the preset, so the two compared
  # unequal and only a case-insensitive dispatch saved it.
  expect_identical(get_default_solver()$lang, solver_options$glpk$lang)
  expect_identical(get_default_solver()$name, solver_options$glpk$name)
})

# Precedence -------------------------------------------------------------------

test_that("an R option beats an environment variable beats the default", {
  with_clean_options({
    options(en.arrow_format = NULL)
    withr_env <- Sys.getenv("ENERGYRT_ARROW_FORMAT", unset = NA)
    on.exit({
      if (is.na(withr_env)) Sys.unsetenv("ENERGYRT_ARROW_FORMAT")
      else Sys.setenv(ENERGYRT_ARROW_FORMAT = withr_env)
    }, add = TRUE)

    Sys.unsetenv("ENERGYRT_ARROW_FORMAT")
    expect_identical(.en_opt_source("arrow_format"), "default")
    expect_identical(get_arrow_format(), "feather")

    Sys.setenv(ENERGYRT_ARROW_FORMAT = "csv")
    expect_identical(.en_opt_source("arrow_format"), "envvar")
    expect_identical(get_arrow_format(), "csv")

    options(en.arrow_format = "parquet")
    expect_identical(.en_opt_source("arrow_format"), "option")
    expect_identical(get_arrow_format(), "parquet")
  })
})

# Toolchain paths --------------------------------------------------------------

test_that("set_solver_path() and the per-backend wrappers agree", {
  dir <- normalizePath(tempdir(), winslash = "/")
  pairs <- list(
    gams   = list(set_gams_path,   get_gams_path),
    gdxlib = list(set_gdxlib_path, get_gdxlib_path),
    glpk   = list(set_glpk_path,   get_glpk_path),
    python = list(set_python_path, get_python_path),
    julia  = list(set_julia_path,  get_julia_path),
    mosox  = list(set_mosox_path,  get_mosox_path)
  )
  with_clean_options({
    for (backend in names(pairs)) {
      set_solver_path(backend, dir)
      expect_identical(pairs[[backend]][[2]](), get_solver_path(backend),
                       info = backend)
      set_solver_path(backend, NULL)
      expect_null(get_solver_path(backend), info = backend)

      pairs[[backend]][[1]](dir)
      expect_identical(get_solver_path(backend), pairs[[backend]][[2]](),
                       info = backend)
      pairs[[backend]][[1]](NULL)
    }
  })
})

test_that("a solver path always ends in a slash and must exist", {
  dir <- sub("/$", "", normalizePath(tempdir(), winslash = "/"))
  with_clean_options({
    set_solver_path("gams", dir)
    expect_match(get_solver_path("gams"), "/$")
    set_solver_path("gams", NULL)
  })
  expect_error(set_solver_path("glpk", file.path(tempdir(), "no-such-dir")),
               "does not exist")
  expect_error(get_solver_path("not-a-backend"))
})

test_that("deprecated un-prefixed path env vars still work, warning once", {
  dir <- normalizePath(tempdir(), winslash = "/")
  with_clean_options({
    options(en.julia_path = NULL)
    Sys.unsetenv("ENERGYRT_JULIA_PATH")
    old <- Sys.getenv("JULIA_PATH", unset = NA)
    on.exit({
      if (is.na(old)) Sys.unsetenv("JULIA_PATH") else Sys.setenv(JULIA_PATH = old)
    }, add = TRUE)

    Sys.setenv(JULIA_PATH = dir)
    .en_deprecate_reset()
    expect_warning(res <- get_julia_path(), "deprecated")
    expect_identical(res, dir)
    # second read is silent
    expect_silent(get_julia_path())
  })
})

# Verbosity --------------------------------------------------------------------

test_that("isVerbose()/isDebug() accept levels and logicals", {
  with_clean_options({
    options(en.verbose = 0)
    expect_false(isVerbose())

    options(en.verbose = TRUE)
    expect_true(isVerbose())
    expect_false(isVerbose(2))

    options(en.verbose = 2)
    expect_true(isVerbose())
    expect_true(isVerbose(2))
    expect_false(isVerbose(3))

    options(en.debug = 1)
    expect_true(isDebug())
    expect_false(isDebug(2))
  })
})

test_that("the deprecated energyRt.verbose option is honoured once", {
  with_clean_options({
    old <- getOption("energyRt.verbose")
    on.exit(options(energyRt.verbose = old), add = TRUE)

    options(en.verbose = NULL, energyRt.verbose = TRUE)
    .en_deprecate_reset()
    expect_warning(res <- isVerbose(), "deprecated")
    expect_true(res)
    expect_silent(isVerbose())

    # an explicit en.verbose wins over the deprecated option
    options(en.verbose = 0)
    expect_false(isVerbose())
  })
})

# Configuration file -----------------------------------------------------------

test_that("en_config_write()/en_config_read() round-trip", {
  skip_if_not_installed("yaml")
  wd <- file.path(tempdir(), "en-config-test")
  dir.create(wd, showWarnings = FALSE, recursive = TRUE)
  owd <- setwd(wd)
  on.exit({
    setwd(owd)
    unlink(wd, recursive = TRUE)
  }, add = TRUE)

  with_clean_options({
    cfg <- list(arrow_format = "parquet", scenarios_path = "scen/")
    expect_silent(suppressMessages(en_config_write(cfg, global = FALSE)))
    expect_true(file.exists(en_config_path(global = FALSE)))
    expect_identical(en_config_read(global = FALSE), cfg)
  })
})

test_that("the config file fills only options the user has not set", {
  skip_if_not_installed("yaml")
  wd <- file.path(tempdir(), "en-config-test2")
  dir.create(wd, showWarnings = FALSE, recursive = TRUE)
  owd <- setwd(wd)
  on.exit({
    setwd(owd)
    unlink(wd, recursive = TRUE)
  }, add = TRUE)

  with_clean_options({
    suppressMessages(en_config_write(
      list(arrow_format = "parquet", arrow_compression = "lz4"),
      global = FALSE
    ))

    # nothing set -> the config supplies both
    options(en.arrow_format = NULL, en.arrow_compression = NULL)
    Sys.unsetenv(c("ENERGYRT_ARROW_FORMAT", "ENERGYRT_ARROW_COMPRESSION"))
    .en_apply_config()
    expect_identical(get_arrow_format(), "parquet")
    expect_identical(get_arrow_compression(), "lz4")

    # an env var outranks the config file
    options(en.arrow_format = NULL, en.arrow_compression = NULL)
    Sys.setenv(ENERGYRT_ARROW_FORMAT = "csv")
    on.exit(Sys.unsetenv("ENERGYRT_ARROW_FORMAT"), add = TRUE)
    .en_apply_config()
    expect_identical(get_arrow_format(), "csv")
    expect_identical(get_arrow_compression(), "lz4")
  })
})

test_that("en_config_write() warns about unknown option names", {
  skip_if_not_installed("yaml")
  wd <- file.path(tempdir(), "en-config-test3")
  dir.create(wd, showWarnings = FALSE, recursive = TRUE)
  owd <- setwd(wd)
  on.exit({
    setwd(owd)
    unlink(wd, recursive = TRUE)
  }, add = TRUE)

  expect_warning(suppressMessages(
    en_config_write(list(not_an_option = 1), global = FALSE)
  ), "Unknown")
})

test_that("a malformed config file does not stop the package loading", {
  wd <- file.path(tempdir(), "en-config-test4")
  dir.create(wd, showWarnings = FALSE, recursive = TRUE)
  owd <- setwd(wd)
  on.exit({
    setwd(owd)
    unlink(wd, recursive = TRUE)
  }, add = TRUE)

  writeLines(c("this: [is", "  not: valid yaml"), ".energyRt.yml")
  expect_no_error(suppressWarnings(.en_apply_config()))
})

test_that("en_config_show() reports the source of every option", {
  with_clean_options({
    options(en.scenarios_path = "elsewhere/")
    res <- suppressMessages(en_config_show())
    expect_true(is.data.frame(res))
    expect_setequal(res$option, .en_option_names())
    expect_identical(res$source[res$option == "scenarios_path"], "option")
    expect_identical(res$source[res$option == "arrow_compression_level"],
                     "default")
  })
})
