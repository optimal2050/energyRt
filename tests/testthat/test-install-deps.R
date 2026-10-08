# =========================================================================== #
# The installation layer's package lists must match DESCRIPTION.
#
# Both installers used to carry hand-copied lists, and both drifted: the
# bootstrap installed four packages energyRt had dropped (tictoc, zoo, glue,
# plyr) while missing glpsol / nestedscales / multimod, and the Julia default
# set named four packages no preset uses while omitting GLPK, which
# `julia_glpk` needs. These tests pin the lists to their sources.
# =========================================================================== #

# Covers the R API: en_install_deps(), en_install_julia_pkgs(), en_check_packages()

.desc_field <- function(field) {
  v <- utils::packageDescription("energyRt", fields = field)
  if (is.na(v)) return(character())
  x <- trimws(gsub("[(][^)]*[)]", "", strsplit(v, ",")[[1]]))
  setdiff(x[nzchar(x)], "R")
}

test_that("the non-CRAN sibling list covers every non-CRAN dependency", {
  sibs <- energyRt:::.en_sibling_pkgs()
  # geoscales/timescales are Imports; the rest are Suggests. None is on CRAN,
  # so each must be in the sibling list or install.packages() would try CRAN.
  expect_true(all(c("geoscales", "timescales") %in% sibs))
  expect_true(all(c("nestedscales", "glpsol", "multimod", "mmcloud") %in% sibs))
  # every entry is a GitHub ref under the org
  expect_true(all(grepl("^optimal2050/", energyRt:::.en_sibling_refs())))
  expect_equal(length(sibs), length(unique(sibs)))
})

test_that("the bootstrap installer agrees with DESCRIPTION and with the sibling list", {
  f <- system.file("install.R", package = "energyRt")
  skip_if(!nzchar(f) || !file.exists(f), "inst/install.R not installed")
  e <- parse(f)
  fn <- NULL
  for (x in e) {
    if (is.call(x) && identical(as.character(x[[1]])[1], "<-") &&
        identical(as.character(x[[2]]), "install_energyRt")) fn <- eval(x[[3]])
  }
  expect_false(is.null(fn))
  grab <- function(nm) {
    for (st in as.list(body(fn)))
      if (is.call(st) && identical(as.character(st[[1]])[1], "<-") &&
          identical(as.character(st[[2]]), nm)) return(eval(st[[3]]))
    NULL
  }
  deps <- grab("deps")
  sib  <- grab("siblings")
  expect_false(is.null(deps)); expect_false(is.null(sib))

  # 1. it must not install packages energyRt no longer depends on
  known <- c(.desc_field("Imports"), .desc_field("Suggests"), .desc_field("Depends"))
  expect_equal(setdiff(deps, known), character(0))

  # 2. its non-CRAN layer must match .en_sibling_refs() exactly
  expect_setequal(vapply(sib, `[[`, "", "ref"), energyRt:::.en_sibling_refs())

  # 3. timescales is in Imports, so its failure must block the install
  req <- vapply(sib, function(s) isTRUE(s$required), TRUE)
  expect_equal(sub(".*/", "", vapply(sib[req], `[[`, "", "ref")), "timescales")
})

test_that("the Julia default set matches the solvers the presets name", {
  need <- unique(unlist(lapply(
    grep("^julia_", names(solver_options), value = TRUE),
    function(n) as.character(solver_options[[n]]$solver)[1])))
  need <- need[!is.na(need) & nzchar(need)]
  dflt <- eval(formals(en_install_julia_pkgs)$pkgs)
  if (is.null(dflt)) {
    # the default is computed in the body; pull it out of the first assignment
    b <- as.list(body(en_install_julia_pkgs))
    dflt <- NULL
    walk <- function(x) {
      if (is.call(x) && identical(as.character(x[[1]])[1], "<-") &&
          identical(as.character(x[[2]]), "pkgs")) dflt <<- eval(x[[3]])
      if (is.call(x)) lapply(as.list(x), walk)
      invisible(NULL)
    }
    lapply(b, walk)
  }
  expect_false(is.null(dflt))
  # Every freely installable solver a preset names must be installed by default.
  # CPLEX is licensed and cuOpt is GPU-only, so both are opt-in (documented).
  expect_true(all(setdiff(need, c("CPLEX", "cuOpt")) %in% dflt))
  # And nothing in the default set should be unused by any preset or writer.
  infra <- c("JuMP", "Arrow", "RData", "DataFrames")
  expect_equal(setdiff(dflt, c(need, infra)), character(0))
})

test_that("en_check_packages() reads its extras off Suggests, not a copied list", {
  p <- en_check_packages(external = FALSE)
  comp <- p$component
  # every non-CRAN sibling is reported -- the old hard-coded list missed four
  expect_true(all(energyRt:::.en_sibling_pkgs() %in% comp))
  # dev-only tooling is not reported as a missing "extra"
  expect_false(any(c("pak", "testthat", "devtools", "withr") %in% comp))
  # nothing is reported that is neither a dependency nor the parked `gm` extra
  known <- c(.desc_field("Imports"), .desc_field("Suggests"), "gm")
  pkg_rows <- comp[comp %in% c(known, "LaTeX engine")]
  expect_setequal(setdiff(comp[!grepl(" ", comp)], known), character(0))
})

test_that("install hints for non-CRAN siblings name the GitHub ref", {
  ref <- stats::setNames(energyRt:::.en_sibling_refs(),
                         energyRt:::.en_sibling_pkgs())
  # a bare pak::pkg_install('glpsol') would send the user to CRAN, where it
  # is not published
  expect_equal(ref[["glpsol"]], "optimal2050/glpsol")
  expect_equal(ref[["nestedscales"]], "optimal2050/nestedscales")
  expect_true(all(grepl("^optimal2050/", ref)))
})
