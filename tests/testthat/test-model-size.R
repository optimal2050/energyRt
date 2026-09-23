# model_size() reports parameter rows, a variable/constraint estimate from the
# gating maps, and the rows saved by the fold step.

test_that("model_size reports rows, estimates and fold saving", {
  tm_file <- NULL
  for (cand in c(testthat::test_path("fixtures", "testing-models.R"),
                 "data-raw/testing-models.R",
                 file.path("..", "..", "data-raw", "testing-models.R"))) {
    if (file.exists(cand)) { tm_file <- cand; break }
  }
  skip_if(is.null(tm_file), "fixtures/testing-models.R not available")
  source(tm_file, local = TRUE)
  mod <- tm_weather()

  folded <- suppressWarnings(suppressMessages(
    interpolate_model(mod, name = "ms_fold", ondisk = FALSE,
               fold = c("region", "timeslice", "year"), sparse = TRUE)))
  ms <- model_size(folded)

  expect_s3_class(ms, "model_size")
  # total rows == sum of value-parameter rows
  vp <- Filter(function(p) as.character(p@type) %in% c("numpar", "bounds"),
               folded@modInp@parameters)
  expect_equal(ms$param_rows,
               sum(vapply(vp, function(p) nrow(as.data.frame(get_data_slot(p))), 0L)))
  # fold step recorded a positive saving and the variable/constraint estimate is set
  expect_true(!is.na(ms$rows_saved) && ms$rows_saved >= 0)
  expect_true(ms$before_fold >= ms$param_rows)
  expect_gt(ms$n_var_est, 0)
  expect_gt(ms$n_con_est, 0)

  # an unfolded build reports no fold step
  unfolded <- suppressWarnings(suppressMessages(
    interpolate_model(mod, name = "ms_unfold", ondisk = FALSE, fold = FALSE)))
  expect_true(is.na(model_size(unfolded)$before_fold))

  expect_output(print(ms), "model_size")
})

# --------------------------------------------------------------------------- #
# audit_coefficients(): the written GLPK model re-emitted by glpsol as free
# MPS, summarised per equation family. The fold fixture (cap2act 8760 on an
# annual calendar) pins the numbers: eqTechAfUp carries 8760 on vTechCap
# against 1 on vTechAct, eqTechAfsUp 4380 (afs.up 0.5).

test_that("the free-MPS reader takes one- and two-pair COLUMNS lines", {
  f <- withr::local_tempfile(fileext = ".mps")
  writeLines(c(
    "* comment", "NAME test", "ROWS", " N obj", " L r1[a]", " E r2[b]",
    "COLUMNS",
    " x[1] obj 1 r1[a] 2.5",
    " x[1] r2[b] -0.001",
    " y[2] r1[a] 8760",
    "RHS", " RHS r1[a] 3", "ENDATA"), f)
  m <- energyRt:::.read_free_mps(f)
  expect_equal(m$rows$type, c("N", "L", "E"))
  expect_equal(nrow(m$entries), 4L)
  expect_setequal(m$entries$value[m$entries$row == "r1[a]"], c(2.5, 8760))
  au <- audit_coefficients(f)
  expect_s3_class(au, "coefficient_audit")
  expect_equal(au$summary$nnz, 4L)
  expect_equal(au$families$equation[1], "r1")
  expect_equal(au$families$ratio_max[1], 8760 / 2.5)
  expect_true("objective" %in% au$families$equation)
})

test_that("a scenario audits to the coefficients GLPK writes", {
  skip_if_no_solver()
  scen <- interpolate_model(fold_model(), name = "au_fx", verbose = FALSE)
  au <- audit_coefficients(scen)
  expect_s3_class(au, "coefficient_audit")
  expect_true(file.exists(au$source))
  f <- au$families
  expect_equal(f$max_abs[f$equation == "eqTechAfUp"], 8760)
  expect_equal(f$ratio_max[f$equation == "eqTechAfUp"], 8760)
  expect_equal(f$max_abs[f$equation == "eqTechAfsUp"], 4380)
  expect_equal(f$widest_max[f$equation == "eqTechAfUp"], "vTechCap = 8760")
  expect_equal(f$widest_min[f$equation == "eqTechAfUp"], "vTechAct = 1")
  expect_equal(sum(au$decades$n), au$summary$nnz)
  expect_equal(au$summary$rows_wide, 0L)
  expect_output(print(au), "coefficient audit")
  # a run directory audits the same
  d <- dirname(au$source)
  au2 <- audit_coefficients(d)
  expect_equal(au2$summary$nnz, au$summary$nnz)
})
