# `.col_param` is the column <-> parameter relation, baked by
# data-raw/col_param.R. It replaced a run-time scan of the catalogue plus a
# hard-coded `demand`/`dem` rename, so what used to be implicit is asserted
# here: the relation is total over the S4 schema, every column that has no
# parameter says why, and a column name shared by two parameters resolves per
# class instead of colliding.

cp <- energyRt:::.col_param

test_that(".col_param is well formed and keyed uniquely", {
  expect_s3_class(cp, "data.frame")
  expect_true(all(c("class", "slot", "column", "parameter", "bound", "rule",
                    "defVal", "declared_on", "why", "dims") %in% colnames(cp)))
  key <- paste(cp$class, cp$slot, cp$column, sep = "/")
  expect_false(anyDuplicated(key) > 0)
  expect_true(all(nzchar(cp$class) & nzchar(cp$slot) & nzchar(cp$column)))
  expect_true(all(cp$bound[!is.na(cp$bound)] %in% c("lo", "up", "fx")))
})

test_that("every mapped column exists in its class prototype", {
  # the relation is checked against the S4 classes, not against the YAML it was
  # generated from, so a slot renamed in R/class-*.R without a catalogue edit
  # fails here rather than silently losing a parameter's rule
  for (cl in unique(cp$class)) {
    ob <- new(cl)
    sub <- cp[cp$class == cl, ]
    for (sn in unique(sub$slot)) {
      expect_true(sn %in% slotNames(ob),
                  info = paste0(cl, "@", sn, ": no such slot"))
      v <- slot(ob, sn)
      expect_true(is.data.frame(v),
                  info = paste0(cl, "@", sn, ": not a data.frame slot"))
      expect_setequal(setdiff(sub$column[sub$slot == sn], colnames(v)),
                      character(0))
    }
  }
})

test_that("every bound column names a catalogued parameter, and vice versa", {
  b <- cp[!is.na(cp$parameter), ]
  expect_true(all(b$parameter %in% names(energyRt:::.modInp)))
  expect_false(any(is.na(b$rule)))

  # An empty rule is a live state, not a gap: the modern engine coerces "" to
  # "inter" while the legacy one fills with defVal, and which of the two wins
  # is a per-parameter ruling that has not been made. Pinned so the set cannot
  # grow unnoticed.
  expect_setequal(
    b$parameter[!nzchar(b$rule)],
    c("pStorageNCap2Stg", "pStorageStartLevel")
  )

  # the other direction: a parameter that declares a column must appear
  catalogued <- vapply(energyRt:::.modInp, function(e) {
    !is.null(e$class) && !is.null(e$slot) && !is.null(e$colName)
  }, logical(1))
  expect_setequal(unique(b$parameter), names(energyRt:::.modInp)[catalogued])
})

test_that(".param_interp_map() is exactly the bound rows", {
  mp <- energyRt:::.param_interp_map()
  b <- cp[!is.na(cp$parameter), ]
  expect_equal(length(mp), nrow(b))
  expect_setequal(names(mp),
                  paste(b$class, b$slot, b$column, sep = intToUtf8(13)))
  k <- paste("technology", "capacity", "stock", sep = intToUtf8(13))
  expect_equal(mp[[k]]$param, "pTechStock")
  expect_equal(mp[[k]]$dims, c("tech", "region", "year"))
})

test_that("a column name shared by two parameters resolves per class", {
  # `stock` is the one colName whose two parameters ship DIFFERENT rules. Keyed
  # by bare name -- as the config_default_*.yml override tables still are --
  # one of them has to lose; keyed by class/slot/column, neither does.
  tech <- cp[cp$class == "technology" & cp$slot == "capacity" &
               cp$column == "stock", ]
  trd <- cp[cp$class == "trade" & cp$slot == "capacity" &
              cp$column == "stock", ]
  expect_equal(tech$parameter, "pTechStock")
  expect_equal(trd$parameter, "pTradeStock")
  expect_equal(tech$rule, ".inter.")
  expect_equal(trd$rule, "back.inter.forth")
})

test_that("pDemand reaches its column through the declared alias", {
  # catalogued as colName "dem"; the column of `demand@demand` is "demand"
  d <- cp[cp$class == "demand" & cp$slot == "demand" & cp$column == "demand", ]
  expect_equal(nrow(d), 1L)
  expect_equal(d$parameter, "pDemand")
  expect_equal(energyRt:::.modInp$pDemand$colAlias, "demand")
  expect_false(any(cp$column == "dem"))
})

# Honesty guards: both lists are stated so they shrink deliberately. Shortening
# either one is the point -- lengthening it needs a reason in parameters.yml.

test_that("the columns no parameter owns are exactly the declared set", {
  unbound <- c(
    "commodity/cluster/order",
    "commodity/property/max", "commodity/property/min",
    "commodity/property/sd", "commodity/property/value",
    "demand/cluster/dem.share.fx", "demand/cluster/order",
    "export/cluster/act.share.fx", "export/cluster/act.share.lo",
    "export/cluster/act.share.up", "export/cluster/order",
    "export/reserve/res.fx", "export/reserve/res.lo",
    "export/reserve/res.up",
    "import/cluster/act.share.fx", "import/cluster/act.share.lo",
    "import/cluster/act.share.up", "import/cluster/order",
    "import/reserve/res.fx", "import/reserve/res.lo",
    "import/reserve/res.up",
    "storage/cluster/act.share.fx", "storage/cluster/act.share.lo",
    "storage/cluster/act.share.up", "storage/cluster/cap.share.fx",
    "storage/cluster/order",
    "storage/duration/duration", "storage/inp2out/inp2out",
    "storage/inp2stg/inp2stg",
    "storage/vintage/end", "storage/vintage/start",
    "subsidy/subsidy/act", "subsidy/subsidy/cap",
    "subsidy/subsidy/inp.cap", "subsidy/subsidy/inp.ncap",
    "subsidy/subsidy/ncap", "subsidy/subsidy/out.cap",
    "subsidy/subsidy/out.ncap", "subsidy/subsidy/rate",
    "subsidy/subsidy/stg.cap", "subsidy/subsidy/stg.ncap",
    "supply/cluster/act.share.fx", "supply/cluster/act.share.lo",
    "supply/cluster/act.share.up", "supply/cluster/order",
    "tax/tax/act", "tax/tax/cap", "tax/tax/inp.cap", "tax/tax/inp.ncap",
    "tax/tax/ncap", "tax/tax/out.cap", "tax/tax/out.ncap", "tax/tax/rate",
    "tax/tax/stg.cap", "tax/tax/stg.ncap",
    "technology/cluster/act.share.fx", "technology/cluster/act.share.lo",
    "technology/cluster/act.share.up", "technology/cluster/cap.share.fx",
    "technology/cluster/order",
    "technology/vintage/end", "technology/vintage/start",
    "trade/cluster/act.share.fx", "trade/cluster/act.share.lo",
    "trade/cluster/act.share.up", "trade/cluster/cap.share.fx",
    "trade/cluster/order",
    "trade/trade/reactance", "trade/trade/resistance",
    "trade/vintage/end", "trade/vintage/start",
    "weather/cluster/order"
  )
  u <- cp[is.na(cp$parameter), ]
  expect_setequal(paste(u$class, u$slot, u$column, sep = "/"), unbound)
  expect_true(all(nzchar(u$why)))
  # three of them are declared but unreadable: the slot carries a bare column
  # beside the .lo/.up/.fx triple a `bounds` parameter actually reads
  bare <- u[u$slot == u$column, ]
  expect_setequal(bare$slot, c("duration", "inp2out", "inp2stg"))
})

test_that("the columns no class documents are exactly the known four", {
  # `declared_on` is NA where the slot has no `columns:` block in classes-docs.yml
  # and the columns live only in the S4 prototype
  d <- cp[is.na(cp$declared_on), ]
  expect_setequal(
    paste(d$class, d$slot, d$column, sep = "/"),
    c("calendar/timeslice_share/share", "calendar/timeslice_share/weight",
      "settings/debug/dummyExport", "settings/debug/dummyImport")
  )
  expect_true(all(!is.na(d$parameter)))
})
