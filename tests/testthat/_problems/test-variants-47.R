# Extracted from test-variants.R:47

# test -------------------------------------------------------------------------
GAS <- newTechnology(
    "EGAS", input = list(comm = "COA"), output = list(comm = "ELC"),
    vintage = data.frame(vintage = c("2020", "2030"), olife = c(30L, 30L)),
    invcost = data.frame(vintage = c("2020", "2030"), invcost = c(1200, 1000)),
    ceff    = data.frame(vintage = c("2020", "2030"), comm = "COA",
                         cinp2use = c(0.35, 0.45)),
    cap2act = 1)
sv <- vt_interp(vt_model(GAS, name = "vin"), "vin")
pv <- sv@modInp@sets$tech_variant
expect_setequal(pv$tech, c("EGAS_VIN2020", "EGAS_VIN2030"))
win <- sv@modInp@sets$process_invest_window
w20 <- win[win$process == "EGAS_VIN2020", ]
expect_true(all(w20$start == 2020) && all(w20$end == 2020))
