head_val = {
    "vTechInv": "tech,region,year,value",
    "vTechEac": "tech,region,year,value",
    "vTechRetCost": "tech,region,year,value",
    "vTechFixom": "tech,region,year,value",
    "vTechVarom": "tech,region,year,value",
    "vSupCost": "sup,region,year,value",
    "vEmsFuelTot": "comm,region,year,timeslice,value",
    "vBalance": "comm,region,year,timeslice,value",
    "vBalanceRY": "comm,region,year,value",
    "vTotalCost": "region,year,value",
    "vObjective": "value",
    "vTaxCost": "comm,region,year,value",
    "vSubsCost": "comm,region,year,value",
    "vAggOutTot": "comm,region,year,timeslice,value",
    "vDummyImportCost": "comm,region,year,value",
    "vDummyExportCost": "comm,region,year,value",
    "vStorageFixom": "stg,region,year,value",
    "vStorageVarom": "stg,region,year,value",
    "vTradeEac": "trade,region,year,value",
    "vTradeFixom": "trade,region,year,value",
    "vImportIrCost": "trade,region,year,value",
    "vExportIrCost": "trade,region,year,value",
    "vImportRowCost": "imp,region,year,value",
    "vExportRowCost": "expp,region,year,value",
    "vTechNewCap": "tech,region,year,value",
    "vTechRetiredStockCum": "tech,region,year,value",
    "vTechRetiredStock": "tech,region,year,value",
    "vTechRetiredNewCap": "tech,region,year,year,value",
    "vTechCap": "tech,region,year,value",
    "vTechAct": "tech,region,year,timeslice,value",
    "vTechInp": "tech,comm,region,year,timeslice,value",
    "vTechOut": "tech,comm,region,year,timeslice,value",
    "vTechOutRY": "tech,comm,region,year,value",
    "vTechAInp": "tech,comm,region,year,timeslice,value",
    "vTechAOut": "tech,comm,region,year,timeslice,value",
    "vSupOut": "sup,comm,region,year,timeslice,value",
    "vSupReserve": "sup,comm,region,value",
    "vDemInp": "comm,region,year,timeslice,value",
    "vOutTot": "comm,region,year,timeslice,value",
    "vOutTotRY": "comm,region,year,value",
    "vInpTot": "comm,region,year,timeslice,value",
    "vInpTotRY": "comm,region,year,value",
    "vInp2Lo": "comm,region,year,timeslice,timeslice,value",
    "vOut2Lo": "comm,region,year,timeslice,timeslice,value",
    "vSupOutTot": "comm,region,year,timeslice,value",
    "vTechInpTot": "comm,region,year,timeslice,value",
    "vTechOutTot": "comm,region,year,timeslice,value",
    "vStorageInpTot": "comm,region,year,timeslice,value",
    "vStorageOutTot": "comm,region,year,timeslice,value",
    "vStorageAInp": "stg,comm,region,year,timeslice,value",
    "vStorageAOut": "stg,comm,region,year,timeslice,value",
    "vDummyImport": "comm,region,year,timeslice,value",
    "vDummyExport": "comm,region,year,timeslice,value",
    "vStorageInp": "stg,comm,region,year,timeslice,value",
    "vStorageOut": "stg,comm,region,year,timeslice,value",
    "vStorageLevel": "stg,comm,region,year,timeslice,value",
    "vStorageInv": "stg,region,year,value",
    "vStorageEac": "stg,region,year,value",
    "vStorageOutCap": "stg,region,year,value",
    "vStorageOutNewCap": "stg,region,year,value",
    "vImportTot": "comm,region,year,timeslice,value",
    "vExportTot": "comm,region,year,timeslice,value",
    "vTradeIr": "trade,comm,region,region,year,timeslice,value",
    "vTradeIrAInp": "trade,comm,region,year,timeslice,value",
    "vTradeIrAInpTot": "comm,region,year,timeslice,value",
    "vTradeIrAOut": "trade,comm,region,year,timeslice,value",
    "vTradeIrAOutTot": "comm,region,year,timeslice,value",
    "vExportRowCum": "expp,comm,value",
    "vExportRow": "expp,comm,region,year,timeslice,value",
    "vImportRowCum": "imp,comm,value",
    "vImportRow": "imp,comm,region,year,timeslice,value",
    "vTradeCap": "trade,year,value",
    "vTradeInv": "trade,region,year,value",
    "vTradeNewCap": "trade,year,value",
    "vTotalUserCosts": "region,year,value",
}
flist = open("output/variable_list.csv", "w")
flist.write("value\n")
for v in instance.component_objects(Var):
    if str(v) != "fornontriv":
        f = open("output/" + str(v) + ".csv", "w")
        f.write(head_val[str(v)] + "\n")
        flist.write(str(v) + "\n")
        for index in v:
            if not v[index].stale and v[index].value != 0:
                u = ""
                if index != None:
                    for i in index:
                        u = u + str(i) + ","
                    u = u + str(v[index].value)
                else:
                    u = str(v[index].value)
                f.write(u + "\n")
        f.close()
flist.close()
