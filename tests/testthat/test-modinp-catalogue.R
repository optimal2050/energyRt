# The `.modInp` catalogue (data-raw/modInp.yml) wires each parameter to the
# `class@slot` and column it is extracted from. A wired name that no longer
# resolves is invisible: `get_slot_meta()` filters on it and `next`s on no
# match, so the parameter is simply never filled by the generic `ob2mi` path.
#
# Four classes drifted this way (supply/import/export `@availability`, demand
# `@dem`) and each acquired a hand-written `ob2mi` method instead, which is how
# their region handling diverged. These checks keep the wiring honest.

# @covers pSupCost pSupAva pDemand pImportRow pImportRowPrice pExportRow pExportRowPrice depth=S

test_that("every catalogued parameter names a real slot of its class", {
  bad <- character(0)
  for (p in energyRt:::.modInp) {
    if (!is.list(p) || is.null(p$class) || is.null(p$slot)) next
    if (!methods::isClass(p$class)) {
      bad <- c(bad, sprintf("%s: no such class '%s'", p$name, p$class))
      next
    }
    if (!p$slot %in% methods::slotNames(p$class)) {
      bad <- c(bad, sprintf("%s: %s@%s is not a slot", p$name, p$class, p$slot))
    }
  }
  expect_equal(bad, character(0))
})

test_that("every catalogued colName is a column of the slot it names", {
  # `pDemand` is the one accepted exception: its colName is "dem" while
  # `demand@demand` holds "demand". The catalogue name is the key of the
  # `config@defVal` / `@interpolation` override columns
  # (test-family-interp-overrides.R), so renaming it is a user-facing break.
  # `.param_interp_map()` maps it instead.
  known <- "pDemand"
  bad <- character(0)
  for (p in energyRt:::.modInp) {
    if (!is.list(p) || is.null(p$class) || is.null(p$slot) ||
        is.null(p$colName)) next
    if (p$name %in% known) next
    if (!methods::isClass(p$class) ||
        !p$slot %in% methods::slotNames(p$class)) next
    d <- tryCatch(methods::slot(methods::new(p$class), p$slot),
                  error = function(e) NULL)
    if (!is.data.frame(d)) next
    cn <- p$colName
    if (!(cn %in% names(d) ||
          any(paste0(cn, c(".lo", ".up", ".fx")) %in% names(d)))) {
      bad <- c(bad, sprintf("%s: colName '%s' not in %s@%s",
                            p$name, cn, p$class, p$slot))
    }
  }
  expect_equal(bad, character(0))
})

test_that("the interpolation map resolves the hand-written classes", {
  # These four are filled by bespoke `ob2mi` methods, so a broken catalogue
  # entry costs them their defaults and interpolation rules -- and nothing
  # else -- which is why the drift went unnoticed.
  mp <- energyRt:::.param_interp_map()
  expect_identical(mp[["supply\rsupply\rcost"]]$param, "pSupCost")
  expect_identical(mp[["supply\rsupply\rava.up"]]$defVal, Inf)
  expect_identical(mp[["demand\rdemand\rdemand"]]$param, "pDemand")
  expect_identical(mp[["import\rimport\rprice"]]$param, "pImportRowPrice")
  expect_identical(mp[["export\rexport\rexp.up"]]$defVal, Inf)
})
