# The mTechVarom domain (value_on_window) must treat a NA key as a PER-ROW
# wildcard: one technology's year-NA varom row beside another's year-keyed
# rows makes the `year` column mixed, and a column-level fully-NA test
# silently dropped the NA rows from the window join -- the flat-varom
# technology's O&M (or a negative-varom credit) vanished from the LP while
# its pTechVarom values sat in the store looking correct.

test_that("a year-NA varom row survives the window join beside year-keyed rows", {
  A <- newTechnology(
    "TKEY", output = list(comm = "ELC"),
    varom = data.frame(year = c(2020, 2030), varom = c(2, 3)),
    invcost = data.frame(invcost = 100),
    vintage = data.frame(olife = 30L), cap2act = 1)
  B <- newTechnology(
    "TFLAT", output = list(comm = "ELC"),
    varom = data.frame(varom = -5),   # flat year-NA row -- the credit shape
    invcost = data.frame(invcost = 100),
    vintage = data.frame(olife = 30L), cap2act = 1)
  sc <- vt_interp(vt_model(A, B, name = "vw"), "vw")

  m <- as.data.frame(get_data_slot(sc@modInp@parameters$mTechVarom))
  expect_true("TKEY" %in% m$tech)
  # dropped before the row-wise wildcard fix:
  expect_true("TFLAT" %in% m$tech)
  # the window supplies the wildcarded years -- every milestone is covered
  expect_setequal(unique(m$year[m$tech == "TFLAT"]),
                  unique(m$year[m$tech == "TKEY"]))

  p <- as.data.frame(get_data_slot(sc@modInp@parameters$pTechVarom))
  expect_true(all(p$value[p$tech == "TFLAT"] == -5))
})
