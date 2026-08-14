# `config@variant_prefix` controls the infixes inserted into expanded variant
# names. It is model-wide because those become solver set-member names, and
# `settings` inherits it (settings `contains = "config"`).

test_that("the prototype default matches the internal default", {
  expect_identical(new("config")@variant_prefix, vt_prefix_def())
  expect_identical(vt_prefix(new("settings")), vt_prefix_def())
})

test_that("partial overrides merge over the defaults", {
  expect_identical(vt_prefix_mrg(c(cluster = "_RC")),
                   c(vintage = "_VIN", cluster = "_RC"))
  expect_identical(vt_prefix_mrg(c(vintage = "_V", cluster = "_C")),
                   c(vintage = "_V", cluster = "_C"))
  expect_identical(vt_prefix_mrg(c("_V", "_C")),      # unnamed pair, positional
                   c(vintage = "_V", cluster = "_C"))
  expect_identical(vt_prefix_mrg(NULL), vt_prefix_def())
  # canonical order, so identical() comparisons are order-insensitive
  expect_identical(vt_prefix_mrg(c(cluster = "_C", vintage = "_V")),
                   vt_prefix_mrg(c(vintage = "_V", cluster = "_C")))
})

test_that("invalid prefixes are rejected with their own message", {
  expect_error(vt_prefix_mrg(c(vintage = "")), "non-empty")
  expect_error(vt_prefix_mrg(c(vintage = "_V!")), "alnum")
  expect_error(vt_prefix_mrg(c(vintage = "_X", cluster = "_X")), "must differ")
  expect_error(vt_prefix_mrg(c(vintages = "_V")), "Unknown variant dimension")
  expect_error(vt_prefix_mrg("_V"), "ambiguous")   # length-1 unnamed
})

test_that("the value round-trips through newModel / update / settings", {
  # NB a region is required: config's prototype has region = NULL and
  # .config_to_settings() cannot assign NULL into settings' character slot.
  m <- newModel("t", region = "R1", variant_prefix = c(cluster = "_RC"))
  expect_identical(m@config@variant_prefix, c(vintage = "_VIN", cluster = "_RC"))
  expect_identical(vt_prefix(vt_cfg2set(m@config)), m@config@variant_prefix)

  cfg <- update(new("config"), variant_prefix = c(vintage = "_V"))
  expect_identical(cfg@variant_prefix, c(vintage = "_V", cluster = "_CL"))
})

test_that("an object saved before the slot existed still works", {
  cfg <- update(new("config"), region = "R1")
  attr(cfg, "variant_prefix") <- NULL              # simulate a stale object
  expect_false(methods::.hasSlot(cfg, "variant_prefix"))
  expect_identical(vt_prefix(cfg), vt_prefix_def())
  # `.config_to_settings()` loops slotNames("config") and would otherwise raise
  # "no slot of name" on the main interpolation path.
  stt <- expect_no_error(vt_cfg2set(cfg))
  expect_identical(vt_prefix(stt), vt_prefix_def())
})

test_that("a custom prefix is used in the minted variant names", {
  WIN <- newTechnology(
    "EWIN", output = list(comm = "ELC"),
    vintage = data.frame(vintage = "2030", olife = 25L),
    invcost = data.frame(cluster = c("best", "mid"), invcost = c(150, 130)),
    cap2act = 1)
  mod <- vt_model(WIN, name = "pfx")
  mod@config <- update(mod@config, variant_prefix = c(cluster = "_RC"))
  sc <- vt_interp(mod, "pfx")
  expect_true("EWIN_VIN2030_RCbest" %in% sc@modInp@sets$tech)
  # reporting joins the provenance table, so it is unaffected by the prefix
  expect_true(all(sc@modInp@sets$tech_variant$base == "EWIN"))
  expect_setequal(sc@modInp@sets$tech_variant$cluster, c("best", "mid"))
})

test_that("model config and scenario settings must agree", {
  mod <- vt_model(name = "dis")
  mod@config <- update(mod@config, variant_prefix = c(cluster = "_RC"))
  # a settings object built from the stock defaults disagrees with the model
  stt <- vt_cfg2set(update(new("config"), region = c("R1", "R2")))
  expect_error(
    suppressMessages(suppressWarnings(
      interpolate_model(mod, stt, name = "dis", fold = TRUE))),
    "variant_prefix.*disagrees"
  )
})

test_that("check_name() rejects non-scalar and non-character input", {
  # the guards used to be OR'd into the "valid" expression
  expect_true(check_name("a1_b"))
  expect_false(check_name("1abc"))
  expect_false(check_name(c("a", "b")))
  expect_false(check_name(1))
})
