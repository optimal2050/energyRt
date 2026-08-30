# Extracted from test-storage-variants.R:66

# prequel ----------------------------------------------------------------------
vt_stg_model <- function(stg, name = "st") {
  vt_model(
    newTechnology("EWIN", output = list(comm = "ELC"),
                  invcost = data.frame(invcost = 150),
                  afs = data.frame(timeslice = "ANNUAL", afs.up = 0.4),
                  vintage = data.frame(olife = 25L), cap2act = 1),
    stg, name = name)
}

# test -------------------------------------------------------------------------
BATT <- newStorage(
    "BATT", commodity = "ELC",
    vintage = data.frame(vintage = c("2020", "2030"), olife = c(15L, 15L)),
    invcost = data.frame(vintage = c("2020", "2030"), invcost = c(300, 180)),
    cap2stg = 4)
sc <- vt_interp(vt_stg_model(BATT, "sv"), "sv")
prov <- getVariants(sc, class = "storage")
expect_setequal(prov$name, c("BATT_VIN2020", "BATT_VIN2030"))
expect_true(all(prov$class == "storage"))
expect_setequal(sc@modInp@sets$stg, c("BATT_VIN2020", "BATT_VIN2030"))
win <- sc@modInp@sets$process_invest_window
w20 <- win[win$process == "BATT_VIN2020", ]
expect_true(all(w20$start == 2020) && all(w20$end == 2020))
