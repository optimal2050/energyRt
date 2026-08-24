# =========================================================================== #
# `add(model, <object>)` must work more than once.
#
# Objects added straight to a model, rather than to a repository the caller
# names, land in an auto-created repository. That repository used to be built
# with `new("repository", name = ...)` -- and the class's `initialize` method
# discards `...` outright (it only fills `@permit`), so the name was silently
# dropped and every such repository came out called "".
#
# An empty name cannot be found: `obj@data[[""]]` is always NULL, so the "does
# it exist already?" test was always TRUE and EVERY add() appended ANOTHER
# empty-named repository. With two of them present, `ff == repo_name` matched
# two positions and `obj@data[[fl]]` became RECURSIVE indexing --
# `obj@data[[c(2, 3)]]` -- which fails with "subscript out of bounds".
#
# Net effect: a model could be given exactly ONE object this way. The second
# add() errored, with a message about selecting a method for `add`, naming
# neither repositories nor names. The same applied to an explicit `repo_name`,
# which was dropped by the identical mechanism.
# =========================================================================== #

am_cns <- function(nm) newConstraint(
  name = nm, eq = "<=", defVal = 1e6,
  for.each = data.frame(year = 2020L),
  t1 = list(variable = "vTradeCap"))

am_repos <- function(m) unname(c(sapply(m@data, function(x) x@name),
                                 recursive = TRUE))

test_that("a model takes more than one object through add()", {
  m <- tr_mesh_one(name = "am1")
  expect_equal(am_repos(m), "mesh_repo")

  # The second call is the one that used to fail.
  m <- add(m, am_cns("C1"))
  m <- add(m, am_cns("C2"))
  m <- add(m, am_cns("C3"))

  # One auto-created repository, not three, and all three objects inside it.
  expect_equal(am_repos(m), c("mesh_repo", "default_repository"))
  expect_equal(length(m@data[["default_repository"]]@data), 3)
})

test_that("the auto-created repository has a usable name", {
  # "" was never a legal repository name -- `add(model, <repository>)` has
  # always refused it ("Empty repository name is not allowed") -- so the
  # auto-created one used to break the class's own rule.
  m <- add(tr_mesh_one(name = "am2"), am_cns("C1"))
  expect_false(any(am_repos(m) == ""))
  expect_true("default_repository" %in% am_repos(m))
  # The name is what makes it addressable, which is the whole fix.
  expect_s4_class(m@data[["default_repository"]], "repository")
})

test_that("an explicit `repo_name` is honoured and reused", {
  # Dropped by the same mechanism, so this was equally unrepeatable.
  m <- tr_mesh_one(name = "am3")
  m <- add(m, am_cns("D1"), repo_name = "mine")
  m <- add(m, am_cns("D2"), repo_name = "mine")
  expect_equal(am_repos(m), c("mesh_repo", "mine"))
  expect_equal(length(m@data[["mine"]]@data), 2)

  # Named and unnamed adds coexist without colliding.
  m <- add(m, am_cns("E1"))
  expect_equal(am_repos(m), c("mesh_repo", "mine", "default_repository"))
  expect_equal(length(m@data[["mine"]]@data), 2)
  expect_equal(length(m@data[["default_repository"]]@data), 1)
})

test_that("several objects in ONE add() still work", {
  # The path that always worked, so the fix must not disturb it.
  m <- add(tr_mesh_one(name = "am4"), am_cns("F1"), am_cns("F2"))
  expect_equal(length(m@data[["default_repository"]]@data), 2)
})

test_that("the objects survive to the solver", {
  skip_if_no_solver()
  # Two constraints added one at a time both have to reach the model, not merely
  # sit in a repository -- so this asserts on an objective, not on structure.
  # The mesh baseline is 10; the caps are 6 on total flow and 4 on the direct
  # leg, and the binding one is the second.
  m <- tr_mesh_one(name = "am5")
  m <- add(m, newConstraint(name = "CAP_ALL", eq = "<=", defVal = 6,
                            for.each = data.frame(comm = "ELC", year = 2020L),
                            t1 = list(variable = "vTradeIr")))
  m <- add(m, newConstraint(name = "CAP_DIRECT", eq = "<=", defVal = 4,
                            for.each = data.frame(comm = "ELC", year = 2020L),
                            t1 = list(variable = "vTradeIr",
                                      for.sum = list(src = "R1", dst = "R3"))))
  scen <- suppressMessages(interpolate_model(m, name = "am5"))
  expect_true(all(c("CAP_ALL", "CAP_DIRECT") %in%
                    names(scen@modInp@gams.equation)))

  # 4 direct at cost 1, plus 2 more of the 6-unit budget carrying R2's own
  # supply at 5, and R3 buys the last 4 from itself at 100.
  expect_equal(getData(suppressMessages(solve_scen(scen)), "vObjective",
                       merge = TRUE)$value[1],
               4 * 1 + 2 * 5 + 4 * 100)
})
