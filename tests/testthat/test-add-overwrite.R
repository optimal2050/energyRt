# =========================================================================== #
# add(model, ..., overwrite =) REPLACEMENT semantics across repositories.
#
# Historic defect: a bare object whose class+name already lived in one of the
# model's repositories was silently appended to the default repository NEXT TO
# the original (the bare-object path returned before the cross-repository
# duplicate check), and interpolation then used both -- e.g. a supply curve
# added over its flat base supply left the unbounded flat one in the model.
# Now: same class+name in any repository is a REPLACEMENT with
# `overwrite = TRUE` and an ERROR without it; an incoming repository with
# `overwrite = TRUE` likewise supersedes same-named objects in other repos.
# =========================================================================== #

.ao_model <- function() {
  ELC <- newCommodity("ELC")
  S <- newSupply("S1", commodity = "ELC",
                 supply = data.frame(cost = 1))
  newModel("ao", data = newRepository("base", ELC, S),
           region = "R1", horizon = newHorizon(2025), discount = 0)
}

# every (repository, name) holding an object of `cls` named `nm`
.ao_find <- function(mod, cls, nm) {
  hits <- character(0)
  for (r in mod@data) {
    for (o in r@data) {
      if (methods::is(o, cls) && identical(o@name, nm)) {
        hits <- c(hits, r@name)
      }
    }
  }
  hits
}

test_that("a bare same-name object errors without overwrite", {
  mod <- .ao_model()
  S2 <- newSupply("S1", commodity = "ELC", supply = data.frame(cost = 9))
  expect_error(add(mod, S2), "already exists.*overwrite")
})

test_that("a bare same-name object replaces in place with overwrite = TRUE", {
  mod <- .ao_model()
  S2 <- newSupply("S1", commodity = "ELC", supply = data.frame(cost = 9))
  mod2 <- add(mod, S2, overwrite = TRUE)
  expect_identical(.ao_find(mod2, "supply", "S1"), "base")  # one copy, in situ
  expect_identical(mod2@data$base@data$S1@supply$cost, 9)
})

test_that("same name, different class is not a collision", {
  mod <- .ao_model()
  # a technology named S1 must coexist with the supply named S1
  TS <- newTechnology("S1", input = list(comm = "ELC"),
                      output = list(comm = "ELC"))
  mod2 <- add(mod, TS, overwrite = FALSE)
  expect_identical(.ao_find(mod2, "supply", "S1"), "base")
  expect_length(.ao_find(mod2, "technology", "S1"), 1L)
})

test_that("an incoming repository supersedes same-named objects elsewhere", {
  mod <- .ao_model()
  S2 <- newSupply("S1", commodity = "ELC", supply = data.frame(cost = 9))
  D <- newDemand("D1", commodity = "ELC", demand = data.frame(demand = 1))
  inc <- newRepository("module", S2, D)
  # without overwrite the cross-repo duplicate check still refuses
  expect_error(add(mod, inc), "Duplicated")
  mod2 <- add(mod, inc, overwrite = TRUE)
  expect_identical(.ao_find(mod2, "supply", "S1"), "module")  # base copy gone
  expect_identical(mod2@data$module@data$S1@supply$cost, 9)
  expect_length(.ao_find(mod2, "demand", "D1"), 1L)
})
