# Extracted from test-trade-variants.R:83

# prequel ----------------------------------------------------------------------
vt_trade_model <- function(trd, name = "tr") {
  vt_model(
    newTechnology("EWIN", output = list(comm = "ELC"),
                  invcost = data.frame(invcost = 150),
                  afs = data.frame(timeslice = "ANNUAL", afs.up = 0.4),
                  vintage = data.frame(olife = 25L), cap2act = 1),
    trd, name = name)
}

# test -------------------------------------------------------------------------
rt <- data.frame(src = c("R1", "R2"), dst = c("R2", "R1"))
TBD <- newTrade(
    "TBD_ELC", commodity = "ELC", routes = rt,
    trade = data.frame(src = c("R1", "R2"), dst = c("R2", "R1"), teff = 0.95),
    vintage = data.frame(vintage = c("2020", "2030"), olife = c(40L, 40L)),
    invcost = data.frame(vintage = c("2020", "2030"), invcost = c(200, 140)),
    cap2act = 1)
r <- vt_expand_one(TBD)
expect_length(r$objects, 2L)
expect_setequal(r$provenance$name, c("TBD_ELC_VIN2020", "TBD_ELC_VIN2030"))
expect_true(all(r$provenance$class == "trade"))
expect_true(all(is.na(r$provenance$cluster)))
for (o in r$objects) expect_equal(o@routes[, c("src", "dst")], rt)
sc <- vt_interp(vt_trade_model(TBD, "tv"), "tv")
expect_setequal(sc@modInp@sets$trade,
                  c("TBD_ELC_VIN2020", "TBD_ELC_VIN2030"))
win <- sc@modInp@sets$process_invest_window
w20 <- win[win$process == "TBD_ELC_VIN2020", ]
expect_true(all(w20$start == 2020) && all(w20$end == 2020))
