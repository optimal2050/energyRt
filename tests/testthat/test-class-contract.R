# =========================================================================== #
# The class contract: construction, container laws, and authoritative defaults.
#
# Each block pins one promise
# the classes make but did not keep:
#
#   1. a model is a container, so `[[`, `[`, `$`, `names()` and `length()`
#      behave as they do for every other R container (both `[[` and `[` used
#      to return a list, and `$` / `names()` resolved to the `repository`
#      methods, so a model had no interface of its own);
#   2. `newModel(data = list(...))` honours the contract its own error message
#      states, and `setHorizon(model, horizon)` takes a horizon;
#   3. `new("<class>", slot = value)` refuses rather than silently discarding
#      the value, while a bare `new("<class>")` still yields a prototype;
#   4. a prototype and its public constructor agree on defaults, because a
#      slot missing from an older serialized object is filled from the
#      prototype;
#   5. a repository-storable brick warns about an empty name (an error in a
#      later release), while containers keep accepting one.
#
# Covers the R API: newModel, newScenario, setHorizon, `[`, `[[`, `$`, names,
# length, new(), .assert_object_name. No `@covers` tags: the coverage matrix
# speaks model vocabulary (parameters, maps, equations, variables), not R
# function names -- see tests/README.md.
# =========================================================================== #

cc_bricks <- function() list(
  newCommodity("CC_C", desc = "a commodity"),
  newCommodity("CC_G", desc = "another commodity")
)

cc_model <- function() {
  newModel("CC_M", data = do.call(newRepository, c(list("cc_r"), cc_bricks())),
           calendar = energyRt::calendars$annual,
           horizon = newHorizon(2025), region = "R1")
}

# --------------------------------------------------------------------------- #
# 1. container laws

test_that("a model extracts like any other R container", {
  m <- cc_model()
  expect_s4_class(m[["CC_C"]], "commodity")
  expect_equal(m[["CC_C"]]@name, "CC_C")
  expect_s4_class(m$CC_C, "commodity")
  expect_setequal(names(m), c("CC_C", "CC_G"))
  expect_equal(length(m), 2L)
})

test_that("`[` returns a same-kind subset that keeps the configuration", {
  m <- cc_model()
  one <- m["CC_C"]
  expect_s4_class(one, "model")
  expect_equal(names(one), "CC_C")
  expect_equal(length(one), 1L)
  # the configuration travels with the subset
  expect_equal(getHorizon(one)@period, getHorizon(m)@period)
  expect_equal(one@config@calendar@name, m@config@calendar@name)
  # and the original is untouched
  expect_equal(length(m), 2L)
})

test_that("a repository keeps the behaviour a model now shares", {
  r <- do.call(newRepository, c(list("cc_r"), cc_bricks()))
  expect_s4_class(r[["CC_C"]], "commodity")
  expect_s4_class(r$CC_C, "commodity")
  expect_setequal(names(r), c("CC_C", "CC_G"))
})

# --------------------------------------------------------------------------- #
# 2. newModel(data =) and setHorizon()

test_that("newModel accepts a list of objects, as its message promises", {
  from_list <- newModel("CC_L", data = cc_bricks(),
                        calendar = energyRt::calendars$annual,
                        horizon = newHorizon(2025), region = "R1")
  expect_setequal(names(from_list), c("CC_C", "CC_G"))
  # a single object is accepted too, and a stray type is still refused
  one <- newModel("CC_1", data = newCommodity("CC_C"),
                  calendar = energyRt::calendars$annual,
                  horizon = newHorizon(2025), region = "R1")
  expect_equal(names(one), "CC_C")
  expect_error(newModel("CC_B", data = 42), "repository")
})

test_that("setHorizon(model, horizon) assigns the horizon object", {
  m <- cc_model()
  h <- newHorizon(c(2030, 2040))
  m2 <- setHorizon(m, h)
  expect_s4_class(m2@config@horizon, "horizon")
  expect_equal(getHorizon(m2)@period, h@period)
  # the numeric form still works
  m3 <- setHorizon(m, period = c(2025, 2035))
  expect_s4_class(m3@config@horizon, "horizon")
})

# --------------------------------------------------------------------------- #
# 3. direct new() cannot discard user input

test_that("new() refuses slot values and names the constructor", {
  for (cl in c("commodity", "demand", "supply", "technology", "storage",
               "trade", "weather", "constraint", "model", "scenario",
               "repository", "tax", "subsidy", "horizon")) {
    expect_error(new(cl, name = "X"), "does not accept slot values",
                 label = paste0("new('", cl, "', name = 'X')"))
  }
  # the message says what to use instead
  expect_error(new("technology", name = "X"), "newTechnology")
  expect_error(new("tax", name = "X"), "newTax")
  # `modOut` has no public constructor, so the message points at the pipeline
  expect_error(new("modOut", sets = list()), "does not accept slot values")
  expect_error(new("modOut", sets = list()), "solve pipeline")
  # and a bare prototype is still constructible, as ~29 internal places need
  for (cl in c("commodity", "technology", "model", "repository", "tax",
               "subsidy", "horizon", "modOut")) {
    expect_s4_class(new(cl), cl)
  }
})

test_that("the generated classes keep decoding dots", {
  # `variable` and `parameter` define no initialize() of their own, so the
  # default S4 method assigns the slots; `newParameter()` and
  # `new("variable", ...)` are built on that and must not start refusing.
  expect_equal(new("variable", name = "vX")@name, "vX")
})

# --------------------------------------------------------------------------- #
# 4. prototypes agree with constructors

test_that("the technology prototype carries the constructor's defaults", {
  proto <- new("technology")
  ctor <- newTechnology("CC_T", output = list(comm = "CC_C"))
  expect_equal(proto@optimizeRetirement, ctor@optimizeRetirement)
  expect_equal(proto@fullYear, ctor@fullYear)
  # the values themselves, so a future flip is a deliberate edit
  expect_false(proto@optimizeRetirement)
  expect_true(proto@fullYear)
  # `storage` already agreed and must stay that way
  sproto <- new("storage")
  sctor <- newStorage("CC_S", commodity = "CC_C")
  expect_equal(sproto@optimizeRetirement, sctor@optimizeRetirement)
  expect_equal(sproto@fullYear, sctor@fullYear)
})

test_that("no scalar slot of technology or storage disagrees unnoticed", {
  # A sweep rather than a list, so a new slot joins the check automatically.
  for (cl in c("technology", "storage")) {
    proto <- new(cl)
    ctor <- if (cl == "technology") {
      newTechnology("CC_T2", output = list(comm = "CC_C"))
    } else {
      newStorage("CC_S2", commodity = "CC_C")
    }
    bad <- character(0)
    for (sl in slotNames(cl)) {
      a <- slot(proto, sl); b <- slot(ctor, sl)
      if (is.data.frame(a) || is.data.frame(b) || is.list(a) || is.list(b)) next
      if (sl %in% c("name", "desc", "commodity")) next   # set by the caller
      if (length(a) == 0 && length(b) == 0) next
      if (!isTRUE(all.equal(a, b))) {
        bad <- c(bad, sprintf("%s@%s: prototype %s vs constructor %s", cl, sl,
                              paste(deparse(a), collapse = ""),
                              paste(deparse(b), collapse = "")))
      }
    }
    expect_equal(bad, character(0))
  }
})

# --------------------------------------------------------------------------- #
# 5. newScenario returns a valid object

test_that("newScenario returns an object validObject() accepts", {
  s <- newScenario("CC_S1", path = NULL)
  expect_true(isTRUE(validObject(s)))
  expect_s4_class(s@model, "model")
  expect_s4_class(s@settings, "settings")
  expect_s4_class(s@modInp, "modInp")
  expect_s4_class(s@modOut, "modOut")
  expect_type(s@desc, "character")
})

test_that("newScenario reads what it documents and refuses the rest", {
  s <- newScenario("CC_S2", path = NULL, desc = "a description",
                   misc = list(note = "kept"))
  expect_equal(s@desc, "a description")
  expect_equal(s@misc$note, "kept")
  # a model and a settings object are assigned where they belong
  s2 <- newScenario("CC_S3", path = NULL, cc_model())
  expect_setequal(names(s2@model), c("CC_C", "CC_G"))
  # a stray object passed positionally lands in the `model` parameter, and is
  # refused there with a message naming what it should be
  expect_error(newScenario("CC_S4", path = NULL, newCommodity("CC_C")),
               "must be a `model` object")
  # a genuinely unused named argument is refused rather than silently dropped
  expect_error(newScenario("CC_S5", path = NULL, nonsense = 1),
               "unused argument")
  # the name becomes a solver symbol and a directory, so it is validated
  expect_error(newScenario("bad name", path = NULL), "Invalid scenario name")
})

# --------------------------------------------------------------------------- #
# 5b. the validity rule: constructors valid, prototypes drafts
#
# Priority 2 item 7 asked the package to choose one rule and document it; the
# ruling (2026-11-05) is the one in `?energyRt-validity`: a public constructor
# returns an object `validObject()` accepts, while a bare `new("<class>")`
# prototype is an internal draft. The draft set is pinned EXACTLY, so a class
# joins or leaves it only by a visible edit here.

cc_ctor_cases <- function() list(
  commodity  = newCommodity("CC_C", desc = "d"),
  demand     = newDemand("CC_D", commodity = "CC_C", desc = "d"),
  supply     = newSupply("CC_U", commodity = "CC_C", desc = "d"),
  technology = newTechnology("CC_T", desc = "d", output = list(comm = "CC_C")),
  storage    = newStorage("CC_G", desc = "d", commodity = "CC_C"),
  trade      = newTrade("CC_R", desc = "d", commodity = "CC_C"),
  weather    = newWeather("CC_W", desc = "d"),
  constraint = suppressWarnings(newConstraint("CC_K", desc = "d")),
  costs      = suppressWarnings(newCosts("CC_O", variable = "vTechCap",
                                         desc = "d")),
  tax        = newTax("CC_X", desc = "d", comm = "CC_C"),
  subsidy    = newSubsidy("CC_B", desc = "d", comm = "CC_C"),
  horizon    = newHorizon(2025),
  calendar   = energyRt::calendars$annual,
  repository = newRepository("cc_rp"),
  model      = cc_model(),
  scenario   = newScenario("CC_SV", path = NULL)
)

test_that("every public constructor returns a valid object", {
  bad <- character(0)
  for (nm in names(cc_ctor_cases())) {
    o <- cc_ctor_cases()[[nm]]
    ok <- tryCatch({ validObject(o); TRUE }, error = function(e) {
      bad <<- c(bad, paste0(nm, ": ", conditionMessage(e))); FALSE })
    expect_s4_class(o, nm)
  }
  expect_equal(bad, character(0))
})

test_that("the prototypes that are drafts are exactly the documented ones", {
  # every class with a prototype, so a newly invalid one cannot hide
  classes <- c("commodity", "demand", "supply", "import", "export",
               "technology", "storage", "trade", "weather", "constraint",
               "costs", "tax", "subsidy", "horizon", "calendar", "config",
               "settings", "repository", "model", "scenario", "modInp",
               "modOut")
  drafts <- character(0)
  for (cl in classes) {
    p <- tryCatch(new(cl), error = function(e) NULL)
    expect_s4_class(p, cl)
    if (is.null(p)) next
    if (!isTRUE(tryCatch({ validObject(p); TRUE }, error = function(e) FALSE)))
      drafts <- c(drafts, cl)
  }
  # `scenario` alone: its `model`/`settings`/`modInp`/`modOut` slots are NULL
  # in the prototype because valid empties cost ~360 ms at package load.
  # Four classes left this set on 2026-11-05, all the same one-line defect --
  # a NULL prototype in a "character" slot: `config@region`,
  # `trade@commodity`, `constraint@name` and `costs@name`.
  expect_equal(drafts, "scenario")
})

test_that("a default config and model are valid", {
  expect_true(isTRUE(validObject(new("config"))))
  expect_true(isTRUE(validObject(new("model"))))
  expect_equal(as.character(new("config")@region), character(0))
})

# --------------------------------------------------------------------------- #
# 6. empty names

test_that("an empty name warns for a storable brick and not for a container", {
  expect_warning(newCommodity(""), "non-empty name")
  expect_warning(newTechnology("", output = list(comm = "CC_C")),
                 "non-empty name")
  # containers and prototypes keep empty names without complaint
  expect_silent(newRepository(""))
  expect_silent(new("commodity"))
})
