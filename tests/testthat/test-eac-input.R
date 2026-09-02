# A user may hand the model a precomputed annuity in `@invcost$eac` instead of
# letting it annuitise `invcost`. modInp.yml binds pXEac to `colName: eac`, so
# the slot loop seeds the parameter with exactly what was supplied, and
# `.eac_one()` fills only the rows the user left empty.

test_that("@invcost carries the financial columns on all three classes", {
  fin <- c("invcost", "wacc", "payback", "eac", "retcost")
  for (cls in c("technology", "trade")) {
    expect_true(all(fin %in% names(new(cls)@invcost)), info = cls)
  }
  # `storage` prices three parts separately -- the charger, the reservoir and
  # the discharger -- so the same five columns appear once per part under an
  # `inp.`/`stg.`/`out.` prefix. A bare `invcost` there would be ambiguous.
  nm <- names(new("storage")@invcost)
  for (p in c("out", "inp", "stg")) {
    expect_true(all(paste(p, fin, sep = ".") %in% nm), info = p)
  }
  expect_false(any(fin %in% nm))
})

test_that("a supplied eac is used verbatim, ignoring invcost and wacc", {
  skip_if_no_solver()
  sc <- rt_interp(rt_model(
    invcost = data.frame(invcost = 1000, wacc = 0.08, eac = 42),
    name = "e1", discount = 0.05), "e1")
  expect_equal(rt_val(sc, "pTechEac"), 42)
  # invcost is still reported -- it is what vTechInv and the salvage value use.
  expect_equal(rt_val(sc, "pTechInvcost"), 1000)
})

test_that("supplied and computed values coexist, the supplied one winning", {
  skip_if_no_solver()
  # ECOA hands over its annuity; EGAS lets the model compute one. Note the
  # precedence is per ROW, and `back.inter.forth` fills a supplied value across
  # every year, so a partially-specified `eac` for ONE process wins outright --
  # the coexistence to test is across processes.
  mod <- rt_model(invcost = data.frame(invcost = 1000, eac = 42),
                  name = "e2", discount = 0.05)
  mod <- add(mod, newTechnology(
    "EGAS", input = list(comm = "COA"), output = list(comm = "ELC"),
    invcost = data.frame(invcost = 2000),
    vintage = data.frame(olife = 40L), cap2act = 1))
  d <- suppressMessages(suppressWarnings(
    getData(rt_interp(mod, "e2"), "pTechEac", merge = TRUE)))
  expect_equal(d$value[d$tech == "ECOA"][1], 42)
  expect_equal(d$value[d$tech == "EGAS"][1], 2000 * rt_crf(0.05, 40))
})

test_that("an object whose @invcost predates the new columns still builds", {
  skip_if_no_solver()
  # `make_data_param()` renames the column it reads; a slot data.frame saved
  # before `eac`/`payback` existed has neither, and rename() would error.
  tech <- newTechnology("EOLD", input = list(comm = "COA"),
                        output = list(comm = "ELC"),
                        invcost = data.frame(invcost = 1000),
                        vintage = data.frame(olife = 40L), cap2act = 1)
  tech@invcost <- tech@invcost[, c("region", "year", "invcost"), drop = FALSE]
  expect_false(any(c("eac", "payback") %in% names(tech@invcost)))

  mod <- add(rt_model(name = "e3", discount = 0.05), tech)
  sc <- rt_interp(mod, "e3")
  d <- suppressMessages(suppressWarnings(getData(sc, "pTechEac", merge = TRUE)))
  expect_equal(d$value[d$tech == "EOLD"][1], 1000 * rt_crf(0.05, 40))
  expect_length(rt_val(sc, "pTechPayback"), 0L)
})

test_that("pXEac reads the eac column, not invcost", {
  mi <- getFromNamespace(".modInp", "energyRt")
  for (p in c("pTechEac", "pTradeEac")) {
    expect_equal(mi[[p]]$colName, "eac", info = p)
  }
  for (p in c("pTechInvcost", "pTradeInvcost")) {
    expect_equal(mi[[p]]$colName, "invcost", info = p)
  }
  # storage carries a part prefix -- the discharger is the `out.` one
  expect_equal(mi[["pStorageOutEac"]]$colName, "out.eac")
  expect_equal(mi[["pStorageOutInvcost"]]$colName, "out.invcost")
  expect_equal(mi[["pStorageStgEac"]]$colName, "stg.eac")
  expect_equal(mi[["pStorageInpInvcost"]]$colName, "inp.invcost")
})

# A perpetual annuity at a zero rate is undefined, not zero. `.crf()` computes
# it as 1/Inf, so an `invcost` with neither a rate nor a life would leave the
# objective silently -- the same shape as a costless supply.

test_that(".crf is a perpetuity at an infinite life and zero at neither", {
  crf <- energyRt:::.crf
  expect_equal(crf(0.07, Inf), 0.07)                       # perpetuity
  expect_equal(crf(0.07, 25), 0.07 * 1.07^25 / (1.07^25 - 1))
  expect_equal(crf(0, 25), 1 / 25)                         # straight line
  expect_equal(crf(0, Inf), 0)                             # the undefined case
})

test_that("a zero rate with no operational life is refused, not charged as 0", {
  df <- data.frame(tech = "E_X", rate = 0, life = Inf,
                   invcost = 1000, ueac = NA_real_,
                   stringsAsFactors = FALSE)
  expect_error(energyRt:::.check_perpetual_annuity(df, "tech"), "E_X")
  expect_error(energyRt:::.check_perpetual_annuity(df, "tech"), "olife")

  # A supplied eac is what the row will actually use, so it is not refused.
  ok <- df; ok$ueac <- 123
  expect_silent(energyRt:::.check_perpetual_annuity(ok, "tech"))
  # Nor is a zero invcost, whose annuity is legitimately zero.
  ok2 <- df; ok2$invcost <- 0
  expect_silent(energyRt:::.check_perpetual_annuity(ok2, "tech"))
  # Nor either half of the combination on its own.
  expect_silent(energyRt:::.check_perpetual_annuity(
    transform(df, rate = 0.05), "tech"))
  expect_silent(energyRt:::.check_perpetual_annuity(
    transform(df, life = 30), "tech"))
})
