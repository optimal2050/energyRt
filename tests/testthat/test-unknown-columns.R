# =========================================================================== #
# A column a slot does not have is refused, with the object, the slot, the
# columns that look meant and the slot's own columns; an object whose slot was
# assigned directly (`@<-`, which skips the constructors' checks) is refused
# when it enters a repository or a model.
#
# @covers depth=S backends=none
# =========================================================================== #

test_that("an unknown column names the object, the slot and what looks meant", {
  e <- tryCatch(newStorage("S", commodity = "ELC", capacity = data.frame(stock = 1)),
                error = function(e) conditionMessage(e))
  expect_match(e, 'storage "S": unknown column `stock` in @capacity', fixed = TRUE)
  expect_match(e, "Did you mean `out.stock`, `inp.stock`, `stg.stock`?", fixed = TRUE)
  expect_match(e, "Columns of @capacity: vintage, cluster, region", fixed = TRUE)
  # a near spelling
  expect_error(newStorage("S", commodity = "ELC", capacity = data.frame(stg.stck = 1)),
               "Did you mean `stg.stock`", fixed = TRUE)
  # update() goes through the same check
  s <- newStorage("S", commodity = "ELC")
  expect_error(update(s, capacity = data.frame(stock = 1)), "unknown column `stock`", fixed = TRUE)
  # a technology given a storage's part-prefixed name
  expect_error(newTechnology("T", output = data.frame(comm = "ELC"), capacity = data.frame(out.stock = 1)),
               'technology "T": unknown column `out.stock` in @capacity', fixed = TRUE)
})

test_that("a slot assigned directly is checked when the object enters a repository or a model", {
  s <- newStorage("S", commodity = "ELC")
  s@capacity <- data.frame(stock = 1)
  expect_error(newRepository("r", s), 'storage "S" is not valid: .*unknown column stock')
  expect_error(add(newRepository("r"), s), "is not valid")
  ELC <- newCommodity("ELC", unit = "MWh")
  mod <- newModel("m", region = "R1", horizon = newHorizon(2025),
                  data = newRepository("r", ELC, newStorage("S", commodity = "ELC")))
  # a replacement of an object already in the model
  expect_error(add(mod, s, overwrite = TRUE), "is not valid")
  # a valid object passes
  expect_no_error(add(newRepository("r"), newStorage("S2", commodity = "ELC")))
})
