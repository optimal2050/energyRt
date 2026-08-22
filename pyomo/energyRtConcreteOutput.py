flist = open("output/variable_list.csv", "w")
flist.write("value\n")
flist.write("vTechInv\n")
f = open("output/vTechInv.csv", "w")
f.write("tech,region,year,value\n")
for t, r, y in mTechInv:
    if model.vTechInv[(t, r, y)].value != 0:
        f.write(
            str(t)
            + ","
            + str(r)
            + ","
            + str(y)
            + ","
            + str(model.vTechInv[(t, r, y)].value)
            + "\n"
        )
f.close()
flist.write("vTechEac\n")
f = open("output/vTechEac.csv", "w")
f.write("tech,region,year,value\n")
for t, r, y in mTechEac:
    if model.vTechEac[(t, r, y)].value != 0:
        f.write(
            str(t)
            + ","
            + str(r)
            + ","
            + str(y)
            + ","
            + str(model.vTechEac[(t, r, y)].value)
            + "\n"
        )
f.close()
flist.write("vTechRetCost\n")
f = open("output/vTechRetCost.csv", "w")
f.write("tech,region,year,value\n")
for t, r, y in mTechRetCost:
    if model.vTechRetCost[(t, r, y)].value != 0:
        f.write(
            str(t)
            + ","
            + str(r)
            + ","
            + str(y)
            + ","
            + str(model.vTechRetCost[(t, r, y)].value)
            + "\n"
        )
f.close()
flist.write("vTechFixom\n")
f = open("output/vTechFixom.csv", "w")
f.write("tech,region,year,value\n")
for t, r, y in mTechFixom:
    if model.vTechFixom[(t, r, y)].value != 0:
        f.write(
            str(t)
            + ","
            + str(r)
            + ","
            + str(y)
            + ","
            + str(model.vTechFixom[(t, r, y)].value)
            + "\n"
        )
f.close()
flist.write("vTechVarom\n")
f = open("output/vTechVarom.csv", "w")
f.write("tech,region,year,value\n")
for t, r, y in mTechVarom:
    if model.vTechVarom[(t, r, y)].value != 0:
        f.write(
            str(t)
            + ","
            + str(r)
            + ","
            + str(y)
            + ","
            + str(model.vTechVarom[(t, r, y)].value)
            + "\n"
        )
f.close()
flist.write("vSupCost\n")
f = open("output/vSupCost.csv", "w")
f.write("sup,region,year,value\n")
for s1, r, y in mvSupCost:
    if model.vSupCost[(s1, r, y)].value != 0:
        f.write(
            str(s1)
            + ","
            + str(r)
            + ","
            + str(y)
            + ","
            + str(model.vSupCost[(s1, r, y)].value)
            + "\n"
        )
f.close()
flist.write("vEmsFuelTot\n")
f = open("output/vEmsFuelTot.csv", "w")
f.write("comm,region,year,timeslice,value\n")
for c, r, y, s in mEmsFuelTot:
    if model.vEmsFuelTot[(c, r, y, s)].value != 0:
        f.write(
            str(c)
            + ","
            + str(r)
            + ","
            + str(y)
            + ","
            + str(s)
            + ","
            + str(model.vEmsFuelTot[(c, r, y, s)].value)
            + "\n"
        )
f.close()
flist.write("vBalance\n")
f = open("output/vBalance.csv", "w")
f.write("comm,region,year,timeslice,value\n")
for c, r, y, s in mvBalance:
    if model.vBalance[(c, r, y, s)].value != 0:
        f.write(
            str(c)
            + ","
            + str(r)
            + ","
            + str(y)
            + ","
            + str(s)
            + ","
            + str(model.vBalance[(c, r, y, s)].value)
            + "\n"
        )
f.close()
# [agg-rewrite] vBalanceRY output retired
flist.write("vTotalCost\n")
f = open("output/vTotalCost.csv", "w")
f.write("region,year,value\n")
for r, y in mvTotalCost:
    if model.vTotalCost[(r, y)].value != 0:
        f.write(
            str(r) + "," + str(y) + "," + str(model.vTotalCost[(r, y)].value) + "\n"
        )
f.close()
flist.write("vObjective\n")
f = open("output/vObjective.csv", "w")
f.write("value\n" + str(model.vObjective.value) + "\n")
f.close()
flist.write("vTaxCost\n")
f = open("output/vTaxCost.csv", "w")
f.write("comm,region,year,value\n")
for c, r, y in mTaxCost:
    if model.vTaxCost[(c, r, y)].value != 0:
        f.write(
            str(c)
            + ","
            + str(r)
            + ","
            + str(y)
            + ","
            + str(model.vTaxCost[(c, r, y)].value)
            + "\n"
        )
f.close()
flist.write("vSubsCost\n")
f = open("output/vSubsCost.csv", "w")
f.write("comm,region,year,value\n")
for c, r, y in mSubCost:
    if model.vSubsCost[(c, r, y)].value != 0:
        f.write(
            str(c)
            + ","
            + str(r)
            + ","
            + str(y)
            + ","
            + str(model.vSubsCost[(c, r, y)].value)
            + "\n"
        )
f.close()
flist.write("vAggOutTot\n")
f = open("output/vAggOutTot.csv", "w")
f.write("comm,region,year,timeslice,value\n")
for c, r, y, s in mAggOut:
    if model.vAggOutTot[(c, r, y, s)].value != 0:
        f.write(
            str(c)
            + ","
            + str(r)
            + ","
            + str(y)
            + ","
            + str(s)
            + ","
            + str(model.vAggOutTot[(c, r, y, s)].value)
            + "\n"
        )
f.close()
flist.write("vDummyImportCost\n")
f = open("output/vDummyImportCost.csv", "w")
f.write("comm,region,year,value\n")
for c, r, y in mDummyImportCost:
    if model.vDummyImportCost[(c, r, y)].value != 0:
        f.write(
            str(c)
            + ","
            + str(r)
            + ","
            + str(y)
            + ","
            + str(model.vDummyImportCost[(c, r, y)].value)
            + "\n"
        )
f.close()
flist.write("vDummyExportCost\n")
f = open("output/vDummyExportCost.csv", "w")
f.write("comm,region,year,value\n")
for c, r, y in mDummyExportCost:
    if model.vDummyExportCost[(c, r, y)].value != 0:
        f.write(
            str(c)
            + ","
            + str(r)
            + ","
            + str(y)
            + ","
            + str(model.vDummyExportCost[(c, r, y)].value)
            + "\n"
        )
f.close()
flist.write("vStorageFixom\n")
f = open("output/vStorageFixom.csv", "w")
f.write("stg,region,year,value\n")
for st1, r, y in mStorageFixom:
    if model.vStorageFixom[(st1, r, y)].value != 0:
        f.write(
            str(st1)
            + ","
            + str(r)
            + ","
            + str(y)
            + ","
            + str(model.vStorageFixom[(st1, r, y)].value)
            + "\n"
        )
f.close()
flist.write("vStorageVarom\n")
f = open("output/vStorageVarom.csv", "w")
f.write("stg,region,year,value\n")
for st1, r, y in mStorageVarom:
    if model.vStorageVarom[(st1, r, y)].value != 0:
        f.write(
            str(st1)
            + ","
            + str(r)
            + ","
            + str(y)
            + ","
            + str(model.vStorageVarom[(st1, r, y)].value)
            + "\n"
        )
f.close()
flist.write("vTradeEac\n")
f = open("output/vTradeEac.csv", "w")
f.write("trade,region,year,value\n")
for t1, r, y in mTradeEac:
    if model.vTradeEac[(t1, r, y)].value != 0:
        f.write(
            str(t1)
            + ","
            + str(r)
            + ","
            + str(y)
            + ","
            + str(model.vTradeEac[(t1, r, y)].value)
            + "\n"
        )
f.close()
flist.write("vTradeFixom\n")
f = open("output/vTradeFixom.csv", "w")
f.write("trade,region,year,value\n")
for t1, r, y in mTradeFixom:
    if model.vTradeFixom[(t1, r, y)].value != 0:
        f.write(
            str(t1)
            + ","
            + str(r)
            + ","
            + str(y)
            + ","
            + str(model.vTradeFixom[(t1, r, y)].value)
            + "\n"
        )
f.close()
flist.write("vImportIrCost\n")
f = open("output/vImportIrCost.csv", "w")
f.write("trade,region,year,value\n")
for t1, r, y in mImportIrCost:
    if model.vImportIrCost[(t1, r, y)].value != 0:
        f.write(
            str(t1)
            + ","
            + str(r)
            + ","
            + str(y)
            + ","
            + str(model.vImportIrCost[(t1, r, y)].value)
            + "\n"
        )
f.close()
flist.write("vExportIrCost\n")
f = open("output/vExportIrCost.csv", "w")
f.write("trade,region,year,value\n")
for t1, r, y in mExportIrCost:
    if model.vExportIrCost[(t1, r, y)].value != 0:
        f.write(
            str(t1)
            + ","
            + str(r)
            + ","
            + str(y)
            + ","
            + str(model.vExportIrCost[(t1, r, y)].value)
            + "\n"
        )
f.close()
flist.write("vImportRowCost\n")
f = open("output/vImportRowCost.csv", "w")
f.write("imp,region,year,value\n")
for i, r, y in mImportRowCost:
    if model.vImportRowCost[(i, r, y)].value != 0:
        f.write(
            str(i)
            + ","
            + str(r)
            + ","
            + str(y)
            + ","
            + str(model.vImportRowCost[(i, r, y)].value)
            + "\n"
        )
f.close()
flist.write("vExportRowCost\n")
f = open("output/vExportRowCost.csv", "w")
f.write("expp,region,year,value\n")
for e, r, y in mExportRowCost:
    if model.vExportRowCost[(e, r, y)].value != 0:
        f.write(
            str(e)
            + ","
            + str(r)
            + ","
            + str(y)
            + ","
            + str(model.vExportRowCost[(e, r, y)].value)
            + "\n"
        )
f.close()
flist.write("vTechNewCap\n")
f = open("output/vTechNewCap.csv", "w")
f.write("tech,region,year,value\n")
for t, r, y in mTechNew:
    if model.vTechNewCap[(t, r, y)].value != 0:
        f.write(
            str(t)
            + ","
            + str(r)
            + ","
            + str(y)
            + ","
            + str(model.vTechNewCap[(t, r, y)].value)
            + "\n"
        )
f.close()
flist.write("vStorageOutStockCap\n")
f = open("output/vStorageOutStockCap.csv", "w")
f.write("stg,region,year,value\n")
for st1, r, y in mStorageSpan:
    val = model.vStorageOutStockCap[(st1, r, y)].value
    if val is not None and val != 0:
        f.write(str(st1) + "," + str(r) + "," + str(y) + "," + str(val) + "\n")
f.close()
flist.write("vStorageOutRetiredStock\n")
f = open("output/vStorageOutRetiredStock.csv", "w")
f.write("stg,region,year,value\n")
for st1, r, y in mvStorageRetiredStock:
    if model.vStorageOutRetiredStock[(st1, r, y)].value is not None and model.vStorageOutRetiredStock[(st1, r, y)].value != 0:
        f.write(
            str(st1)
            + ","
            + str(r)
            + ","
            + str(y)
            + ","
            + str(model.vStorageOutRetiredStock[(st1, r, y)].value)
            + "\n"
        )
f.close()
flist.write("vStorageOutRetiredNewCap\n")
f = open("output/vStorageOutRetiredNewCap.csv", "w")
f.write("stg,region,year,yearp,value\n")
for st1, r, y, yp in mvStorageRetiredNewCap:
    if model.vStorageOutRetiredNewCap[(st1, r, y, yp)].value is not None and model.vStorageOutRetiredNewCap[(st1, r, y, yp)].value != 0:
        f.write(
            str(st1)
            + ","
            + str(r)
            + ","
            + str(y)
            + ","
            + str(yp)
            + ","
            + str(model.vStorageOutRetiredNewCap[(st1, r, y, yp)].value)
            + "\n"
        )
f.close()
flist.write("vStorageInpStockCap\n")
f = open("output/vStorageInpStockCap.csv", "w")
f.write("stg,region,year,value\n")
for st1, r, y in mStorageInpCap:
    val = model.vStorageInpStockCap[(st1, r, y)].value
    if val is not None and val != 0:
        f.write(str(st1) + "," + str(r) + "," + str(y) + "," + str(val) + "\n")
f.close()
flist.write("vStorageInpRetiredStock\n")
f = open("output/vStorageInpRetiredStock.csv", "w")
f.write("stg,region,year,value\n")
for st1, r, y in mvStorageRetiredStock:
    if model.vStorageInpRetiredStock[(st1, r, y)].value is not None and model.vStorageInpRetiredStock[(st1, r, y)].value != 0:
        f.write(
            str(st1)
            + ","
            + str(r)
            + ","
            + str(y)
            + ","
            + str(model.vStorageInpRetiredStock[(st1, r, y)].value)
            + "\n"
        )
f.close()
flist.write("vStorageInpRetiredNewCap\n")
f = open("output/vStorageInpRetiredNewCap.csv", "w")
f.write("stg,region,year,yearp,value\n")
for st1, r, y, yp in mvStorageRetiredNewCap:
    if model.vStorageInpRetiredNewCap[(st1, r, y, yp)].value is not None and model.vStorageInpRetiredNewCap[(st1, r, y, yp)].value != 0:
        f.write(
            str(st1)
            + ","
            + str(r)
            + ","
            + str(y)
            + ","
            + str(yp)
            + ","
            + str(model.vStorageInpRetiredNewCap[(st1, r, y, yp)].value)
            + "\n"
        )
f.close()
flist.write("vStorageStgStockCap\n")
f = open("output/vStorageStgStockCap.csv", "w")
f.write("stg,region,year,value\n")
for st1, r, y in mStorageStgCap:
    val = model.vStorageStgStockCap[(st1, r, y)].value
    if val is not None and val != 0:
        f.write(str(st1) + "," + str(r) + "," + str(y) + "," + str(val) + "\n")
f.close()
flist.write("vStorageStgRetiredStock\n")
f = open("output/vStorageStgRetiredStock.csv", "w")
f.write("stg,region,year,value\n")
for st1, r, y in mvStorageRetiredStock:
    if model.vStorageStgRetiredStock[(st1, r, y)].value is not None and model.vStorageStgRetiredStock[(st1, r, y)].value != 0:
        f.write(
            str(st1)
            + ","
            + str(r)
            + ","
            + str(y)
            + ","
            + str(model.vStorageStgRetiredStock[(st1, r, y)].value)
            + "\n"
        )
f.close()
flist.write("vStorageStgRetiredNewCap\n")
f = open("output/vStorageStgRetiredNewCap.csv", "w")
f.write("stg,region,year,yearp,value\n")
for st1, r, y, yp in mvStorageRetiredNewCap:
    if model.vStorageStgRetiredNewCap[(st1, r, y, yp)].value is not None and model.vStorageStgRetiredNewCap[(st1, r, y, yp)].value != 0:
        f.write(
            str(st1)
            + ","
            + str(r)
            + ","
            + str(y)
            + ","
            + str(yp)
            + ","
            + str(model.vStorageStgRetiredNewCap[(st1, r, y, yp)].value)
            + "\n"
        )
f.close()
flist.write("vStorageRetCost\n")
f = open("output/vStorageRetCost.csv", "w")
f.write("stg,region,year,value\n")
for st1, r, y in mStorageRetCost:
    if model.vStorageRetCost[(st1, r, y)].value is not None and model.vStorageRetCost[(st1, r, y)].value != 0:
        f.write(
            str(st1)
            + ","
            + str(r)
            + ","
            + str(y)
            + ","
            + str(model.vStorageRetCost[(st1, r, y)].value)
            + "\n"
        )
f.close()
flist.write("vTradeStockCap\n")
f = open("output/vTradeStockCap.csv", "w")
f.write("trade,year,value\n")
for t1, y in mTradeSpan:
    val = model.vTradeStockCap[(t1, y)].value
    if val is not None and val != 0:
        f.write(str(t1) + "," + str(y) + "," + str(val) + "\n")
f.close()
flist.write("vTradeRetiredStock\n")
f = open("output/vTradeRetiredStock.csv", "w")
f.write("trade,year,value\n")
for t1, y in mvTradeRetiredStock:
    if model.vTradeRetiredStock[(t1, y)].value is not None and model.vTradeRetiredStock[(t1, y)].value != 0:
        f.write(
            str(t1)
            + ","
            + str(y)
            + ","
            + str(model.vTradeRetiredStock[(t1, y)].value)
            + "\n"
        )
f.close()
flist.write("vTradeRetiredNewCap\n")
f = open("output/vTradeRetiredNewCap.csv", "w")
f.write("trade,year,yearp,value\n")
for t1, y, yp in mvTradeRetiredNewCap:
    if model.vTradeRetiredNewCap[(t1, y, yp)].value is not None and model.vTradeRetiredNewCap[(t1, y, yp)].value != 0:
        f.write(
            str(t1)
            + ","
            + str(y)
            + ","
            + str(yp)
            + ","
            + str(model.vTradeRetiredNewCap[(t1, y, yp)].value)
            + "\n"
        )
f.close()
flist.write("vTradeRetCost\n")
f = open("output/vTradeRetCost.csv", "w")
f.write("trade,region,year,value\n")
for t1, r, y in mTradeRetCost:
    if model.vTradeRetCost[(t1, r, y)].value is not None and model.vTradeRetCost[(t1, r, y)].value != 0:
        f.write(
            str(t1)
            + ","
            + str(r)
            + ","
            + str(y)
            + ","
            + str(model.vTradeRetCost[(t1, r, y)].value)
            + "\n"
        )
f.close()
flist.write("vTechPhaseOut\n")
f = open("output/vTechPhaseOut.csv", "w")
f.write("tech,region,year,value\n")
for t, r, y in mvTechPhaseOut:
    if model.vTechPhaseOut[(t, r, y)].value is not None and model.vTechPhaseOut[(t, r, y)].value != 0:
        f.write(
            str(t)
            + ","
            + str(r)
            + ","
            + str(y)
            + ","
            + str(model.vTechPhaseOut[(t, r, y)].value)
            + "\n"
        )
f.close()
flist.write("vStoragePhaseOut\n")
f = open("output/vStoragePhaseOut.csv", "w")
f.write("stg,region,year,value\n")
for st1, r, y in mvStoragePhaseOut:
    if model.vStoragePhaseOut[(st1, r, y)].value is not None and model.vStoragePhaseOut[(st1, r, y)].value != 0:
        f.write(
            str(st1)
            + ","
            + str(r)
            + ","
            + str(y)
            + ","
            + str(model.vStoragePhaseOut[(st1, r, y)].value)
            + "\n"
        )
f.close()
flist.write("vStorageStockPhaseOut\n")
f = open("output/vStorageStockPhaseOut.csv", "w")
f.write("stg,region,year,value\n")
for st1, r, y in mvStoragePhaseOut:
    val = model.vStorageStockPhaseOut[(st1, r, y)].value
    if val is not None and val != 0:
        f.write(str(st1) + "," + str(r) + "," + str(y) + "," + str(val) + "\n")
f.close()
flist.write("vTechStockCap\n")
f = open("output/vTechStockCap.csv", "w")
f.write("tech,region,year,value\n")
for t, r, y in mTechSpan:
    val = model.vTechStockCap[(t, r, y)].value
    if val is not None and val != 0:
        f.write(str(t) + "," + str(r) + "," + str(y) + "," + str(val) + "\n")
f.close()
flist.write("vTechStockPhaseOut\n")
f = open("output/vTechStockPhaseOut.csv", "w")
f.write("tech,region,year,value\n")
for t, r, y in mvTechPhaseOut:
    val = model.vTechStockPhaseOut[(t, r, y)].value
    if val is not None and val != 0:
        f.write(str(t) + "," + str(r) + "," + str(y) + "," + str(val) + "\n")
f.close()
flist.write("vTechRetiredStock\n")
f = open("output/vTechRetiredStock.csv", "w")
f.write("tech,region,year,value\n")
for t, r, y in mvTechRetiredStock:
    if model.vTechRetiredStock[(t, r, y)].value != 0:
        f.write(
            str(t)
            + ","
            + str(r)
            + ","
            + str(y)
            + ","
            + str(model.vTechRetiredStock[(t, r, y)].value)
            + "\n"
        )
f.close()
flist.write("vTechRetiredNewCap\n")
f = open("output/vTechRetiredNewCap.csv", "w")
f.write("tech,region,year,yearp,value\n")
for t, r, y, yp in mvTechRetiredNewCap:
    if model.vTechRetiredNewCap[(t, r, y, yp)].value != 0:
        f.write(
            str(t)
            + ","
            + str(r)
            + ","
            + str(y)
            + ","
            + str(yp)
            + ","
            + str(model.vTechRetiredNewCap[(t, r, y, yp)].value)
            + "\n"
        )
f.close()
flist.write("vTechCap\n")
f = open("output/vTechCap.csv", "w")
f.write("tech,region,year,value\n")
for t, r, y in mTechSpan:
    if model.vTechCap[(t, r, y)].value != 0:
        f.write(
            str(t)
            + ","
            + str(r)
            + ","
            + str(y)
            + ","
            + str(model.vTechCap[(t, r, y)].value)
            + "\n"
        )
f.close()
flist.write("vTechAct\n")
f = open("output/vTechAct.csv", "w")
f.write("tech,region,year,timeslice,value\n")
for t, r, y, s in mvTechAct:
    if model.vTechAct[(t, r, y, s)].value != 0:
        f.write(
            str(t)
            + ","
            + str(r)
            + ","
            + str(y)
            + ","
            + str(s)
            + ","
            + str(model.vTechAct[(t, r, y, s)].value)
            + "\n"
        )
f.close()
flist.write("vTechInp\n")
f = open("output/vTechInp.csv", "w")
f.write("tech,comm,region,year,timeslice,value\n")
for t, c, r, y, s in mvTechInp:
    if model.vTechInp[(t, c, r, y, s)].value != 0:
        f.write(
            str(t)
            + ","
            + str(c)
            + ","
            + str(r)
            + ","
            + str(y)
            + ","
            + str(s)
            + ","
            + str(model.vTechInp[(t, c, r, y, s)].value)
            + "\n"
        )
f.close()
flist.write("vTechOut\n")
f = open("output/vTechOut.csv", "w")
f.write("tech,comm,region,year,timeslice,value\n")
for t, c, r, y, s in mvTechOut:
    if model.vTechOut[(t, c, r, y, s)].value != 0:
        f.write(
            str(t)
            + ","
            + str(c)
            + ","
            + str(r)
            + ","
            + str(y)
            + ","
            + str(s)
            + ","
            + str(model.vTechOut[(t, c, r, y, s)].value)
            + "\n"
        )
f.close()
# [agg-rewrite] vTechOutRY output retired
flist.write("vTechAInp\n")
f = open("output/vTechAInp.csv", "w")
f.write("tech,comm,region,year,timeslice,value\n")
for t, c, r, y, s in mvTechAInp:
    if model.vTechAInp[(t, c, r, y, s)].value != 0:
        f.write(
            str(t)
            + ","
            + str(c)
            + ","
            + str(r)
            + ","
            + str(y)
            + ","
            + str(s)
            + ","
            + str(model.vTechAInp[(t, c, r, y, s)].value)
            + "\n"
        )
f.close()
flist.write("vTechAOut\n")
f = open("output/vTechAOut.csv", "w")
f.write("tech,comm,region,year,timeslice,value\n")
for t, c, r, y, s in mvTechAOut:
    if model.vTechAOut[(t, c, r, y, s)].value != 0:
        f.write(
            str(t)
            + ","
            + str(c)
            + ","
            + str(r)
            + ","
            + str(y)
            + ","
            + str(s)
            + ","
            + str(model.vTechAOut[(t, c, r, y, s)].value)
            + "\n"
        )
f.close()
flist.write("vSupOut\n")
f = open("output/vSupOut.csv", "w")
f.write("sup,comm,region,year,timeslice,value\n")
for s1, c, r, y, s in mSupAva:
    if model.vSupOut[(s1, c, r, y, s)].value != 0:
        f.write(
            str(s1)
            + ","
            + str(c)
            + ","
            + str(r)
            + ","
            + str(y)
            + ","
            + str(s)
            + ","
            + str(model.vSupOut[(s1, c, r, y, s)].value)
            + "\n"
        )
f.close()
flist.write("vSupReserve\n")
f = open("output/vSupReserve.csv", "w")
f.write("sup,comm,region,value\n")
for s1, c, r in mvSupReserve:
    if model.vSupReserve[(s1, c, r)].value != 0:
        f.write(
            str(s1)
            + ","
            + str(c)
            + ","
            + str(r)
            + ","
            + str(model.vSupReserve[(s1, c, r)].value)
            + "\n"
        )
f.close()
flist.write("vDemInp\n")
f = open("output/vDemInp.csv", "w")
f.write("comm,region,year,timeslice,value\n")
for c, r, y, s in mvDemInp:
    if model.vDemInp[(c, r, y, s)].value != 0:
        f.write(
            str(c)
            + ","
            + str(r)
            + ","
            + str(y)
            + ","
            + str(s)
            + ","
            + str(model.vDemInp[(c, r, y, s)].value)
            + "\n"
        )
f.close()
flist.write("vOutTot\n")
f = open("output/vOutTot.csv", "w")
f.write("comm,region,year,timeslice,value\n")
for c, r, y, s in mvOutTot:
    if model.vOutTot[(c, r, y, s)].value != 0:
        f.write(
            str(c)
            + ","
            + str(r)
            + ","
            + str(y)
            + ","
            + str(s)
            + ","
            + str(model.vOutTot[(c, r, y, s)].value)
            + "\n"
        )
f.close()
# [agg-rewrite] vOutTotRY output retired
flist.write("vInpTot\n")
f = open("output/vInpTot.csv", "w")
f.write("comm,region,year,timeslice,value\n")
for c, r, y, s in mvInpTot:
    if model.vInpTot[(c, r, y, s)].value != 0:
        f.write(
            str(c)
            + ","
            + str(r)
            + ","
            + str(y)
            + ","
            + str(s)
            + ","
            + str(model.vInpTot[(c, r, y, s)].value)
            + "\n"
        )
f.close()
# [agg-rewrite] vInpTotRY output retired
# [agg-rewrite] vInp2Lo/vOut2Lo output extraction removed (variables retired)
flist.write("vSupOutTot\n")
f = open("output/vSupOutTot.csv", "w")
f.write("comm,region,year,timeslice,value\n")
for c, r, y, s in mSupOutTot:
    if model.vSupOutTot[(c, r, y, s)].value != 0:
        f.write(
            str(c)
            + ","
            + str(r)
            + ","
            + str(y)
            + ","
            + str(s)
            + ","
            + str(model.vSupOutTot[(c, r, y, s)].value)
            + "\n"
        )
f.close()
flist.write("vTechInpTot\n")
f = open("output/vTechInpTot.csv", "w")
f.write("comm,region,year,timeslice,value\n")
for c, r, y, s in mTechInpTot:
    if model.vTechInpTot[(c, r, y, s)].value != 0:
        f.write(
            str(c)
            + ","
            + str(r)
            + ","
            + str(y)
            + ","
            + str(s)
            + ","
            + str(model.vTechInpTot[(c, r, y, s)].value)
            + "\n"
        )
f.close()
flist.write("vTechOutTot\n")
f = open("output/vTechOutTot.csv", "w")
f.write("comm,region,year,timeslice,value\n")
for c, r, y, s in mTechOutTot:
    if model.vTechOutTot[(c, r, y, s)].value != 0:
        f.write(
            str(c)
            + ","
            + str(r)
            + ","
            + str(y)
            + ","
            + str(s)
            + ","
            + str(model.vTechOutTot[(c, r, y, s)].value)
            + "\n"
        )
f.close()
flist.write("vStorageInpTot\n")
f = open("output/vStorageInpTot.csv", "w")
f.write("comm,region,year,timeslice,value\n")
for c, r, y, s in mStorageInpTot:
    if model.vStorageInpTot[(c, r, y, s)].value != 0:
        f.write(
            str(c)
            + ","
            + str(r)
            + ","
            + str(y)
            + ","
            + str(s)
            + ","
            + str(model.vStorageInpTot[(c, r, y, s)].value)
            + "\n"
        )
f.close()
flist.write("vStorageOutTot\n")
f = open("output/vStorageOutTot.csv", "w")
f.write("comm,region,year,timeslice,value\n")
for c, r, y, s in mStorageOutTot:
    if model.vStorageOutTot[(c, r, y, s)].value != 0:
        f.write(
            str(c)
            + ","
            + str(r)
            + ","
            + str(y)
            + ","
            + str(s)
            + ","
            + str(model.vStorageOutTot[(c, r, y, s)].value)
            + "\n"
        )
f.close()
flist.write("vStorageAInp\n")
f = open("output/vStorageAInp.csv", "w")
f.write("stg,comm,region,year,timeslice,value\n")
for st1, c, r, y, s in mvStorageAInp:
    if model.vStorageAInp[(st1, c, r, y, s)].value != 0:
        f.write(
            str(st1)
            + ","
            + str(c)
            + ","
            + str(r)
            + ","
            + str(y)
            + ","
            + str(s)
            + ","
            + str(model.vStorageAInp[(st1, c, r, y, s)].value)
            + "\n"
        )
f.close()
flist.write("vStorageAOut\n")
f = open("output/vStorageAOut.csv", "w")
f.write("stg,comm,region,year,timeslice,value\n")
for st1, c, r, y, s in mvStorageAOut:
    if model.vStorageAOut[(st1, c, r, y, s)].value != 0:
        f.write(
            str(st1)
            + ","
            + str(c)
            + ","
            + str(r)
            + ","
            + str(y)
            + ","
            + str(s)
            + ","
            + str(model.vStorageAOut[(st1, c, r, y, s)].value)
            + "\n"
        )
f.close()
flist.write("vDummyImport\n")
f = open("output/vDummyImport.csv", "w")
f.write("comm,region,year,timeslice,value\n")
for c, r, y, s in mDummyImport:
    if model.vDummyImport[(c, r, y, s)].value != 0:
        f.write(
            str(c)
            + ","
            + str(r)
            + ","
            + str(y)
            + ","
            + str(s)
            + ","
            + str(model.vDummyImport[(c, r, y, s)].value)
            + "\n"
        )
f.close()
flist.write("vDummyExport\n")
f = open("output/vDummyExport.csv", "w")
f.write("comm,region,year,timeslice,value\n")
for c, r, y, s in mDummyExport:
    if model.vDummyExport[(c, r, y, s)].value != 0:
        f.write(
            str(c)
            + ","
            + str(r)
            + ","
            + str(y)
            + ","
            + str(s)
            + ","
            + str(model.vDummyExport[(c, r, y, s)].value)
            + "\n"
        )
f.close()
flist.write("vStorageInp\n")
f = open("output/vStorageInp.csv", "w")
f.write("stg,comm,region,year,timeslice,value\n")
for st1, c, r, y, s in mvStorageInp:
    if model.vStorageInp[(st1, c, r, y, s)].value != 0:
        f.write(
            str(st1)
            + ","
            + str(c)
            + ","
            + str(r)
            + ","
            + str(y)
            + ","
            + str(s)
            + ","
            + str(model.vStorageInp[(st1, c, r, y, s)].value)
            + "\n"
        )
f.close()
flist.write("vStorageOut\n")
f = open("output/vStorageOut.csv", "w")
f.write("stg,comm,region,year,timeslice,value\n")
for st1, c, r, y, s in mvStorageOut:
    if model.vStorageOut[(st1, c, r, y, s)].value != 0:
        f.write(
            str(st1)
            + ","
            + str(c)
            + ","
            + str(r)
            + ","
            + str(y)
            + ","
            + str(s)
            + ","
            + str(model.vStorageOut[(st1, c, r, y, s)].value)
            + "\n"
        )
f.close()
flist.write("vStorageLevel\n")
f = open("output/vStorageLevel.csv", "w")
f.write("stg,comm,region,year,timeslice,value\n")
for st1, c, r, y, s in mvStorageLevel:
    if model.vStorageLevel[(st1, c, r, y, s)].value != 0:
        f.write(
            str(st1)
            + ","
            + str(c)
            + ","
            + str(r)
            + ","
            + str(y)
            + ","
            + str(s)
            + ","
            + str(model.vStorageLevel[(st1, c, r, y, s)].value)
            + "\n"
        )
f.close()
flist.write("vStorageInv\n")
f = open("output/vStorageInv.csv", "w")
f.write("stg,region,year,value\n")
for st1, r, y in mStorageNew:
    if model.vStorageInv[(st1, r, y)].value != 0:
        f.write(
            str(st1)
            + ","
            + str(r)
            + ","
            + str(y)
            + ","
            + str(model.vStorageInv[(st1, r, y)].value)
            + "\n"
        )
f.close()
flist.write("vStorageEac\n")
f = open("output/vStorageEac.csv", "w")
f.write("stg,region,year,value\n")
for st1, r, y in mStorageEac:
    if model.vStorageEac[(st1, r, y)].value != 0:
        f.write(
            str(st1)
            + ","
            + str(r)
            + ","
            + str(y)
            + ","
            + str(model.vStorageEac[(st1, r, y)].value)
            + "\n"
        )
f.close()
flist.write("vStorageOutCap\n")
flist.write("vStorageStgCap\n")
flist.write("vStorageStgNewCap\n")
flist.write("vStorageInpCap\n")
flist.write("vStorageInpNewCap\n")
f = open("output/vStorageInpCap.csv", "w")
f.write("stg,region,year,value\n")
for st1, r, y in mStorageInpCap:
    if model.vStorageInpCap[(st1, r, y)].value != 0:
        f.write(
            str(st1)
            + ","
            + str(r)
            + ","
            + str(y)
            + ","
            + str(model.vStorageInpCap[(st1, r, y)].value)
            + "\n"
        )
f.close()
f = open("output/vStorageInpNewCap.csv", "w")
f.write("stg,region,year,value\n")
for st1, r, y in mStorageInpNew:
    if model.vStorageInpNewCap[(st1, r, y)].value != 0:
        f.write(
            str(st1)
            + ","
            + str(r)
            + ","
            + str(y)
            + ","
            + str(model.vStorageInpNewCap[(st1, r, y)].value)
            + "\n"
        )
f.close()
f = open("output/vStorageStgCap.csv", "w")
f.write("stg,region,year,value\n")
for st1, r, y in mStorageStgCap:
    if model.vStorageStgCap[(st1, r, y)].value != 0:
        f.write(
            str(st1)
            + ","
            + str(r)
            + ","
            + str(y)
            + ","
            + str(model.vStorageStgCap[(st1, r, y)].value)
            + "\n"
        )
f.close()
f = open("output/vStorageStgNewCap.csv", "w")
f.write("stg,region,year,value\n")
for st1, r, y in mStorageStgNew:
    if model.vStorageStgNewCap[(st1, r, y)].value != 0:
        f.write(
            str(st1)
            + ","
            + str(r)
            + ","
            + str(y)
            + ","
            + str(model.vStorageStgNewCap[(st1, r, y)].value)
            + "\n"
        )
f.close()
f = open("output/vStorageOutCap.csv", "w")
f.write("stg,region,year,value\n")
for st1, r, y in mStorageSpan:
    if model.vStorageOutCap[(st1, r, y)].value != 0:
        f.write(
            str(st1)
            + ","
            + str(r)
            + ","
            + str(y)
            + ","
            + str(model.vStorageOutCap[(st1, r, y)].value)
            + "\n"
        )
f.close()
flist.write("vStorageOutNewCap\n")
f = open("output/vStorageOutNewCap.csv", "w")
f.write("stg,region,year,value\n")
for st1, r, y in mStorageNew:
    if model.vStorageOutNewCap[(st1, r, y)].value != 0:
        f.write(
            str(st1)
            + ","
            + str(r)
            + ","
            + str(y)
            + ","
            + str(model.vStorageOutNewCap[(st1, r, y)].value)
            + "\n"
        )
f.close()
flist.write("vImportTot\n")
f = open("output/vImportTot.csv", "w")
f.write("comm,region,year,timeslice,value\n")
for c, r, y, s in mImport:
    if model.vImportTot[(c, r, y, s)].value != 0:
        f.write(
            str(c)
            + ","
            + str(r)
            + ","
            + str(y)
            + ","
            + str(s)
            + ","
            + str(model.vImportTot[(c, r, y, s)].value)
            + "\n"
        )
f.close()
flist.write("vExportTot\n")
f = open("output/vExportTot.csv", "w")
f.write("comm,region,year,timeslice,value\n")
for c, r, y, s in mExport:
    if model.vExportTot[(c, r, y, s)].value != 0:
        f.write(
            str(c)
            + ","
            + str(r)
            + ","
            + str(y)
            + ","
            + str(s)
            + ","
            + str(model.vExportTot[(c, r, y, s)].value)
            + "\n"
        )
f.close()
flist.write("vTradeIr\n")
f = open("output/vTradeIr.csv", "w")
f.write("trade,comm,region,regionp,year,timeslice,value\n")
for t1, c, r, rp, y, s in mvTradeIr:
    if model.vTradeIr[(t1, c, r, rp, y, s)].value != 0:
        f.write(
            str(t1)
            + ","
            + str(c)
            + ","
            + str(r)
            + ","
            + str(rp)
            + ","
            + str(y)
            + ","
            + str(s)
            + ","
            + str(model.vTradeIr[(t1, c, r, rp, y, s)].value)
            + "\n"
        )
f.close()
flist.write("vTradeIrAInp\n")
f = open("output/vTradeIrAInp.csv", "w")
f.write("trade,comm,region,year,timeslice,value\n")
for t1, c, r, y, s in mvTradeIrAInp:
    if model.vTradeIrAInp[(t1, c, r, y, s)].value != 0:
        f.write(
            str(t1)
            + ","
            + str(c)
            + ","
            + str(r)
            + ","
            + str(y)
            + ","
            + str(s)
            + ","
            + str(model.vTradeIrAInp[(t1, c, r, y, s)].value)
            + "\n"
        )
f.close()
flist.write("vTradeIrAInpTot\n")
f = open("output/vTradeIrAInpTot.csv", "w")
f.write("comm,region,year,timeslice,value\n")
for c, r, y, s in mvTradeIrAInpTot:
    if model.vTradeIrAInpTot[(c, r, y, s)].value != 0:
        f.write(
            str(c)
            + ","
            + str(r)
            + ","
            + str(y)
            + ","
            + str(s)
            + ","
            + str(model.vTradeIrAInpTot[(c, r, y, s)].value)
            + "\n"
        )
f.close()
flist.write("vTradeIrAOut\n")
f = open("output/vTradeIrAOut.csv", "w")
f.write("trade,comm,region,year,timeslice,value\n")
for t1, c, r, y, s in mvTradeIrAOut:
    if model.vTradeIrAOut[(t1, c, r, y, s)].value != 0:
        f.write(
            str(t1)
            + ","
            + str(c)
            + ","
            + str(r)
            + ","
            + str(y)
            + ","
            + str(s)
            + ","
            + str(model.vTradeIrAOut[(t1, c, r, y, s)].value)
            + "\n"
        )
f.close()
flist.write("vTradeIrAOutTot\n")
f = open("output/vTradeIrAOutTot.csv", "w")
f.write("comm,region,year,timeslice,value\n")
for c, r, y, s in mvTradeIrAOutTot:
    if model.vTradeIrAOutTot[(c, r, y, s)].value != 0:
        f.write(
            str(c)
            + ","
            + str(r)
            + ","
            + str(y)
            + ","
            + str(s)
            + ","
            + str(model.vTradeIrAOutTot[(c, r, y, s)].value)
            + "\n"
        )
f.close()
flist.write("vExportRowCum\n")
f = open("output/vExportRowCum.csv", "w")
f.write("expp,comm,value\n")
for e, c in mExpComm:
    if model.vExportRowCum[(e, c)].value != 0:
        f.write(
            str(e) + "," + str(c) + "," + str(model.vExportRowCum[(e, c)].value) + "\n"
        )
f.close()
flist.write("vExportRow\n")
f = open("output/vExportRow.csv", "w")
f.write("expp,comm,region,year,timeslice,value\n")
for e, c, r, y, s in mExportRow:
    if model.vExportRow[(e, c, r, y, s)].value != 0:
        f.write(
            str(e)
            + ","
            + str(c)
            + ","
            + str(r)
            + ","
            + str(y)
            + ","
            + str(s)
            + ","
            + str(model.vExportRow[(e, c, r, y, s)].value)
            + "\n"
        )
f.close()
flist.write("vImportRowCum\n")
f = open("output/vImportRowCum.csv", "w")
f.write("imp,comm,value\n")
for i, c in mImpComm:
    if model.vImportRowCum[(i, c)].value != 0:
        f.write(
            str(i) + "," + str(c) + "," + str(model.vImportRowCum[(i, c)].value) + "\n"
        )
f.close()
flist.write("vImportRow\n")
f = open("output/vImportRow.csv", "w")
f.write("imp,comm,region,year,timeslice,value\n")
for i, c, r, y, s in mImportRow:
    if model.vImportRow[(i, c, r, y, s)].value != 0:
        f.write(
            str(i)
            + ","
            + str(c)
            + ","
            + str(r)
            + ","
            + str(y)
            + ","
            + str(s)
            + ","
            + str(model.vImportRow[(i, c, r, y, s)].value)
            + "\n"
        )
f.close()
flist.write("vTradeCap\n")
f = open("output/vTradeCap.csv", "w")
f.write("trade,year,value\n")
for t1, y in mTradeSpan:
    if model.vTradeCap[(t1, y)].value != 0:
        f.write(
            str(t1) + "," + str(y) + "," + str(model.vTradeCap[(t1, y)].value) + "\n"
        )
f.close()
flist.write("vTradeInv\n")
f = open("output/vTradeInv.csv", "w")
f.write("trade,region,year,value\n")
for t1, r, y in mTradeEac:
    if model.vTradeInv[(t1, r, y)].value != 0:
        f.write(
            str(t1)
            + ","
            + str(r)
            + ","
            + str(y)
            + ","
            + str(model.vTradeInv[(t1, r, y)].value)
            + "\n"
        )
f.close()
flist.write("vTradeNewCap\n")
f = open("output/vTradeNewCap.csv", "w")
f.write("trade,year,value\n")
for t1, y in mTradeNew:
    if model.vTradeNewCap[(t1, y)].value != 0:
        f.write(
            str(t1) + "," + str(y) + "," + str(model.vTradeNewCap[(t1, y)].value) + "\n"
        )
f.close()
flist.write("vTotalUserCosts\n")
f = open("output/vTotalUserCosts.csv", "w")
f.write("region,year,value\n")
for r, y in mvTotalUserCosts:
    if model.vTotalUserCosts[(r, y)].value != 0:
        f.write(
            str(r)
            + ","
            + str(y)
            + ","
            + str(model.vTotalUserCosts[(r, y)].value)
            + "\n"
        )
f.close()
f = open("output/raw_data_set.csv", "w")
f.write("set,value\n")
for i in comm:
    f.write("comm," + str(i) + "\n")
for i in region:
    f.write("region," + str(i) + "\n")
for i in year:
    f.write("year," + str(i) + "\n")
for i in timeslice:
    f.write("timeslice," + str(i) + "\n")
for i in sup:
    f.write("sup," + str(i) + "\n")
for i in dem:
    f.write("dem," + str(i) + "\n")
for i in tech:
    f.write("tech," + str(i) + "\n")
for i in stg:
    f.write("stg," + str(i) + "\n")
for i in trade:
    f.write("trade," + str(i) + "\n")
for i in expp:
    f.write("expp," + str(i) + "\n")
for i in imp:
    f.write("imp," + str(i) + "\n")
for i in group:
    f.write("group," + str(i) + "\n")
for i in weather:
    f.write("weather," + str(i) + "\n")
f.close()
flist.close()
