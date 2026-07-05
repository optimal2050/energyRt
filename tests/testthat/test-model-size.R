# model_size() reports parameter rows, a variable/constraint estimate from the
# gating maps, and the rows saved by the fold step.

test_that("model_size reports rows, estimates and fold saving", {
  tm_file <- NULL
  for (cand in c("data-raw/testing-models.R",
                 file.path("..", "..", "data-raw", "testing-models.R"))) {
    if (file.exists(cand)) { tm_file <- cand; break }
  }
  skip_if(is.null(tm_file), "data-raw/testing-models.R not available")
  source(tm_file, local = TRUE)
  mod <- tm_weather()

  folded <- suppressWarnings(suppressMessages(
    interp_mod(mod, name = "ms_fold", ondisk = FALSE,
               fold = c("region", "slice", "year"), sparse = TRUE)))
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
    interp_mod(mod, name = "ms_unfold", ondisk = FALSE, fold = FALSE)))
  expect_true(is.na(model_size(unfolded)$before_fold))

  expect_output(print(ms), "model_size")
})
