test_that("mosox path helpers and wrapper expose a simple API", {
  expect_true(exists("set_mosox_path", mode = "function"))
  expect_true(exists("get_mosox_path", mode = "function"))
  expect_true(exists("en_mosox_run", mode = "function"))
  expect_true(exists("mosox_run", mode = "function"))
})

test_that("missing mosox binary errors clearly", {
  old_path <- get_mosox_path()
  on.exit({
    if (is.null(old_path)) {
      options::opt_set("mosox_path", NULL, env = "energyRt")
    } else {
      options::opt_set("mosox_path", old_path, env = "energyRt")
    }
  }, add = TRUE)
  options::opt_set("mosox_path", tempdir(), env = "energyRt")

  expect_error(
    en_mosox_run(model = tempfile(fileext = ".mod"), check = TRUE),
    regexp = "mosox executable was not found|mosox failed"
  )
})
