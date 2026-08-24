# =========================================================================== #
# Helpers for the mapping-engine tests (test-mapping-engine.R).
#
# The tm_*() tier builders live in tests/testthat/fixtures/testing-models.R
# (moved from data-raw/ so the catalog ships in the source tarball and the
# fixture-gated tests run under R CMD check too). These helpers locate and
# lazily source that catalog; when it is unavailable (e.g. an installed,
# tests-stripped package) the dependent tests skip cleanly.
# =========================================================================== #

# Resolve the path to fixtures/testing-models.R from a few candidate roots
# (legacy data-raw/ locations kept as fallback for older checkouts).
.mapping_fixture_file <- function() {
  candidates <- c(
    testthat::test_path("fixtures", "testing-models.R"),
    file.path("tests", "testthat", "fixtures", "testing-models.R"),
    testthat::test_path("..", "..", "data-raw", "testing-models.R"),
    file.path("data-raw", "testing-models.R")
  )
  for (f in candidates) {
    if (file.exists(f)) {
      return(normalizePath(f, winslash = "/", mustWork = FALSE))
    }
  }
  NA_character_
}

# Source the catalog once into a dedicated environment (cached across tests).
.mapping_fixture_env <- local({
  cache <- NULL
  function() {
    if (!is.null(cache)) {
      return(cache)
    }
    f <- .mapping_fixture_file()
    if (is.na(f)) {
      return(NULL)
    }
    e <- new.env(parent = globalenv())
    suppressMessages(suppressWarnings(sys.source(f, envir = e)))
    cache <<- e
    e
  }
})

skip_if_no_fixtures <- function() {
  if (is.null(.mapping_fixture_env())) {
    testthat::skip("fixtures/testing-models.R not available")
  }
}

# Interpolate a tier builder by name (in-memory, quiet).
interp_tier <- function(tier) {
  env <- .mapping_fixture_env()
  suppressMessages(suppressWarnings({
    mod <- env[[tier]]()
    interpolate_model(mod, name = "t", ondisk = FALSE)
  }))
}

# Row count of a single mapping parameter (0 when absent or empty).
map_nrow <- function(scen, nm) {
  gds <- getFromNamespace("get_data_slot", "energyRt")
  p <- scen@modInp@parameters[[nm]]
  if (is.null(p)) {
    return(0L)
  }
  d <- gds(p)
  if (is.null(d)) 0L else nrow(d)
}

# Named-integer vector of row counts for every non-empty mapping parameter
# (names beginning with "m"), sorted by name. Used as the regression snapshot.
mapping_counts <- function(scen) {
  pars <- scen@modInp@parameters
  mapnames <- grep("^m", names(pars), value = TRUE)
  rows <- vapply(mapnames, function(nm) map_nrow(scen, nm), integer(1))
  rows <- rows[rows > 0]
  rows[order(names(rows))]
}
