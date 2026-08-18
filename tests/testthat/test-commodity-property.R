# commodity@property: physical properties for unit conversion and reference ####

COA_PROP <- data.frame(
  property = c("lhv", "hhv", "density", "density_true", "frac_C", "frac_ash"),
  value    = c(25.8, 27.1, 0.85, 1.35, 0.665, 0.100),
  unit     = c("GJ/t", "GJ/t", "t/m3", "t/m3", "kg/kg", "kg/kg"),
  comment  = c("IEA average, other bituminous", "", "bulk, as stockpiled",
               "apparent particle density", "ultimate analysis", ""),
  stringsAsFactors = FALSE
)

new_coa <- function(...) {
  newCommodity("COA", desc = "Steam coal", unit = "PJ",
               property = COA_PROP, ...)
}

# -- the slot itself ---------------------------------------------------------

test_that("commodity has a `property` slot with the documented columns", {
  expect_true("property" %in% slotNames("commodity"))
  expect_identical(
    names(new("commodity")@property),
    c("property", "value", "min", "max", "sd", "dist", "unit", "comment")
  )
  expect_equal(nrow(new("commodity")@property), 0)
})

test_that("a property table round-trips through newCommodity()", {
  COA <- new_coa()
  expect_equal(nrow(COA@property), nrow(COA_PROP))
  expect_identical(COA@property$property, COA_PROP$property)
  expect_equal(COA@property$value, COA_PROP$value)
  expect_identical(COA@property$unit, COA_PROP$unit)
  # columns the user did not supply are present and empty, not dropped
  expect_true(all(is.na(COA@property$min)))
  expect_true(all(is.na(COA@property$dist)))
})

test_that("the uncertainty columns are optional but usable", {
  # the common case: no uncertainty at all
  bare <- newCommodity("X", property = data.frame(
    property = "lhv", value = 25.8, unit = "GJ/t"))
  expect_equal(bare@property$value, 25.8)
  expect_true(is.na(bare@property$sd))

  full <- newCommodity("X", property = data.frame(
    property = c("lhv", "density"),
    value = c(25.8, 0.85), min = c(24.1, 0.75), max = c(27.2, 0.95),
    dist = c("uniform", "triangular"), unit = c("GJ/t", "t/m3")))
  expect_equal(full@property$min, c(24.1, 0.75))
  expect_identical(full@property$dist, c("uniform", "triangular"))
})

test_that("a list of equal-length vectors is accepted", {
  COA <- newCommodity("COA", property = list(
    property = c("lhv", "density"), value = c(25.8, 0.85),
    unit = c("GJ/t", "t/m3")))
  expect_equal(nrow(COA@property), 2)
  expect_equal(COA@property$value, c(25.8, 0.85))
})

test_that("an unknown column is an error", {
  expect_error(
    newCommodity("COA", property = data.frame(property = "lhv", vallue = 1)),
    "Unknown column"
  )
})

test_that("update() replaces the property table", {
  COA <- new_coa()
  up <- update(COA, property = data.frame(
    property = "lhv", value = 30, unit = "GJ/t"))
  expect_equal(nrow(up@property), 1)
  expect_equal(up@property$value, 30)
  # untouched slots survive
  expect_equal(up@unit, "PJ")
})

# -- accessors ---------------------------------------------------------------

test_that("commodity_property() reads values, units and ranges", {
  COA <- newCommodity("COA", unit = "PJ", property = data.frame(
    property = c("lhv", "density"), value = c(25.8, 0.85),
    min = c(24.1, 0.75), max = c(27.2, 0.95),
    dist = c("uniform", "triangular"), unit = c("GJ/t", "t/m3")))

  expect_equal(commodity_property(COA, "lhv"), 25.8)
  expect_equal(commodity_property(COA, "lhv", "range"), c(24.1, 27.2))
  expect_identical(commodity_property(COA, "lhv", "unit"), "GJ/t")
  expect_identical(commodity_property(COA, "density", "dist"), "triangular")
  expect_equal(nrow(commodity_property(COA, "lhv", "row")), 1)
  expect_identical(commodity_property(COA), COA@property)

  # absent properties give NA of the right type, not an error
  expect_true(is.na(commodity_property(COA, "molar_mass")))
  expect_equal(commodity_property(COA, "molar_mass", "range"),
               c(NA_real_, NA_real_))
  expect_true(is.na(commodity_property(COA, "molar_mass", "unit")))
  expect_equal(nrow(commodity_property(COA, "molar_mass", "row")), 0)

  expect_error(commodity_property("not a commodity", "lhv"), "commodity object")
})

test_that("commodity_properties() registry resolves names and prefix rules", {
  reg <- commodity_properties()
  expect_true(all(c("property", "quantity", "default_unit", "bridges",
                    "description") %in% names(reg)))
  expect_true("lhv" %in% reg$property)

  expect_identical(.property_def("lhv")$quantity, "Energy/Mass")
  # prefix rules
  expect_identical(.property_def("frac_C")$property, "frac_*")
  expect_identical(.property_def("density_liquid")$property, "density_*")
  # bare `density` must resolve to the exact entry, not the prefix rule
  expect_identical(.property_def("density")$property, "density")
  expect_identical(.property_def("density")$bridges, "yes")
  expect_identical(.property_def("density_liquid")$bridges, "on_ask")
  expect_null(.property_def("wibble"))
  expect_null(.property_def("frac_"))  # stem alone is not a property
})

# -- validation (warn, never block) ------------------------------------------

test_that("questionable property tables warn but are still built", {
  expect_warning(
    obj <- newCommodity("X", property = data.frame(
      property = "wibble", value = 1, unit = "GJ/t")),
    "unrecognised property name"
  )
  # the row is kept -- validation is advisory, not a filter
  expect_equal(nrow(suppressWarnings(newCommodity("X", property = data.frame(
    property = "wibble", value = 1, unit = "GJ/t")))@property), 1)

  expect_warning(newCommodity("X", property = data.frame(
    property = c("lhv", "lhv"), value = c(1, 2), unit = "GJ/t")),
    "duplicated property name")
  expect_warning(newCommodity("X", property = data.frame(
    property = "lhv", value = -1, unit = "GJ/t")), "negative value")
  expect_warning(newCommodity("X", property = data.frame(
    property = "lhv", value = 1)), "missing unit")
  expect_warning(newCommodity("X", property = data.frame(
    property = c("frac_C", "frac_H"), value = c(0.9, 0.9), unit = "kg/kg")),
    "sum to")
})

test_that("emission-like property names are redirected to @emis", {
  expect_warning(
    newCommodity("X", property = data.frame(
      property = "co2_content", value = 0.0946, unit = "t/GJ")),
    "belongs in `@emis`"
  )
  # and not also reported as merely unrecognised -- one message, not two
  ww <- character()
  withCallingHandlers(
    newCommodity("X", property = data.frame(
      property = "co2_content", value = 0.0946, unit = "t/GJ")),
    warning = function(w) {
      ww <<- c(ww, conditionMessage(w)); invokeRestart("muffleWarning")
    })
  expect_length(ww, 1)
})

test_that("each uncertainty rule warns on its own violation", {
  p <- function(...) data.frame(property = "lhv", unit = "GJ/t", ...)
  expect_warning(newCommodity("X", property = p(value = 1, dist = "wat")),
                 "unknown distribution")
  expect_warning(newCommodity("X", property = p(value = 1, min = 5, max = 9,
                                                dist = "uniform")),
                 "outside \\[min, max\\]")
  expect_warning(newCommodity("X", property = p(value = 1, sd = 0,
                                                dist = "normal")),
                 "non-positive sd")
  expect_warning(newCommodity("X", property = p(value = 0, sd = 1,
                                                dist = "lognormal")),
                 "non-positive value")
  expect_warning(newCommodity("X", property = p(value = 1, dist = "uniform")),
                 "needs both `min` and `max`")
  expect_warning(newCommodity("X", property = p(value = 1, dist = "normal")),
                 "needs `sd`")
  expect_warning(newCommodity("X", property = p(value = 1, min = 0, max = 2)),
                 "without `dist`")
})

test_that("a well-formed table is silent", {
  expect_silent(new_coa())
  expect_silent(newCommodity("ELC", desc = "Electricity", unit = "GWh"))
  expect_silent(newCommodity("X", property = data.frame(
    property = "lhv", value = 25.8, min = 24.1, max = 27.2,
    dist = "uniform", unit = "GJ/t")))
})

# -- images ------------------------------------------------------------------

test_that("image and icon land in misc and are classified", {
  COA <- new_coa(image = "https://example.org/coal.png", icon = "i.svg")
  expect_identical(COA@misc$image, "https://example.org/coal.png")
  expect_identical(COA@misc$icon, "i.svg")

  expect_identical(object_image(COA)$status, "url")
  expect_identical(object_image(COA)$path, "https://example.org/coal.png")
  expect_identical(object_image(COA, "icon")$status, "missing")
  expect_identical(object_image(newCommodity("X"))$status, "none")
  expect_null(object_image(newCommodity("X"))$path)

  f <- withr::local_tempfile(fileext = ".png")
  writeBin(as.raw(rep(0, 8)), f)
  expect_identical(object_image(newCommodity("X", image = f))$status, "ok")

  # report() only takes local files, so a URL must not be offered to it
  expect_null(.object_image_file(COA))
  expect_identical(.object_image_file(newCommodity("X", image = f)), f)
})

test_that("update() merges image/icon into existing misc", {
  COA <- new_coa(image = "a.png")
  COA@misc$color <- "black"
  up <- update(COA, icon = "b.svg")
  expect_identical(up@misc$image, "a.png")
  expect_identical(up@misc$icon, "b.svg")
  expect_identical(up@misc$color, "black")
})

# -- integration with the rest of the package --------------------------------

test_that("getUnits() reports the property rows", {
  u <- getUnits(new_coa())
  pr <- u[u$slot == "property", ]
  expect_equal(nrow(pr), nrow(COA_PROP))
  # a property's unit is self-contained and must NOT be composed with @unit
  expect_identical(pr$unit[pr$parameter == "lhv"], "GJ/t")
  expect_identical(pr$description[pr$parameter == "lhv"],
                   "Lower (net) heating value")
  # slot filtering still works
  expect_equal(nrow(getUnits(new_coa(), slots = "unit")), 1)
})

test_that("getData() returns the property rows", {
  d <- getData(new_coa(), name = "property")
  expect_true("property" %in% names(d))
  expect_equal(nrow(d$property), nrow(COA_PROP))
  expect_true(all(d$property$comm == "COA"))
})

test_that("print.commodity is registered and shows the new slot", {
  # it was dead code before: `@export` alone does not register an S3 method on
  # an S4 generic, so dispatch fell through to the raw slot dump
  expect_true(!is.null(getS3method("print", "commodity", optional = TRUE)))
  out <- capture.output(print(new_coa(image = "https://example.org/c.png")))
  expect_true(any(grepl("unit:\\s+PJ", out)))
  expect_true(any(grepl("^ property", out)))
  expect_true(any(grepl("lhv", out)))
  expect_true(any(grepl("image:.*\\(url\\)", out)))
  # all-NA columns are suppressed, so an empty `sd` does not show
  expect_false(any(grepl("\\bsd\\b", out)))
})

# -- backward compatibility --------------------------------------------------

test_that("objects serialised before the slot existed still work", {
  # save_scenario()/load_scenario() use plain save()/load() with no
  # updateObject(), so a commodity from an older session lacks the slot
  old <- new_coa()
  attributes(old)$property <- NULL
  expect_false(methods::.hasSlot(old, "property"))

  expect_silent(p <- .comm_property(old))
  expect_equal(nrow(p), 0)
  expect_identical(
    names(p),
    c("property", "value", "min", "max", "sd", "dist", "unit", "comment")
  )
  expect_true(is.na(commodity_property(old, "lhv")))
  expect_identical(object_image(old)$status, "none")
  expect_equal(nrow(getUnits(old)[getUnits(old)$slot == "property", ]), 0)
})
