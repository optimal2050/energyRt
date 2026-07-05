import datetime

print("start time: " + str(datetime.datetime.now()) + "\n")
flog = open("output/log.csv", "w")
flog.write("parameter,value,time\n")
flog.write('"model language",PyomoConcrete,"' + str(datetime.datetime.now()) + '"\n')
# Import
import time

seconds = time.time()
from pyomo.environ import *
from pyomo.opt import SolverFactory
import pyomo.environ as pyo

model = AbstractModel()
exec(open("inc1.py").read())
##### decl par #####
model.vTechInv = Var(model.mTechInv, doc="Overnight investment costs")
model.vTechEac = Var(model.mTechEac, doc="Annualized investment costs")
model.vTechRetCost = Var(model.mTechRetCost, doc="Early retirement costs")
model.vTechFixom = Var(model.mTechFixom, doc="Fixed O&M costs")
model.vTechVarom = Var(
    model.mTechVarom, doc="Variable O&M costs (AVarom + CVarom + ActVarom)"
)
model.vSupCost = Var(model.mvSupCost, doc="Supply costs (weighted)")
model.vEmsFuelTot = Var(
    model.mEmsFuelTot, doc="Total emissions from fuels combustion (technologies)"
)
model.vBalance = Var(model.mvBalance, doc="Net commodity balance (all sources)")
model.vBalanceRY = Var(
    model.mBalanceRY, doc="Net commodity balance by region and year (weighted)"
)
model.vTotalCost = Var(model.mvTotalCost, doc="Regional annual total costs (weighted)")
model.vObjective = Var(doc="Objective costs")
model.vTaxCost = Var(model.mTaxCost, doc="Total tax levies (tax costs)")
model.vSubsCost = Var(model.mSubCost, doc="Total subsidies (substracted from costs)")
model.vAggOutTot = Var(model.mAggOut, doc="Aggregated commodity output (weighted)")
model.vDummyImportCost = Var(
    model.mDummyImportCost, doc="Dummy import costs (weighted)"
)
model.vDummyExportCost = Var(
    model.mDummyExportCost, doc="Dummy export costs (weighted)"
)
model.vStorageFixom = Var(model.mStorageFixom, doc="Storage fixed O&M costs")
model.vStorageVarom = Var(model.mStorageVarom, doc="Storage variable O&M costs")
model.vTradeEac = Var(
    model.mTradeEac, doc="Annualized investments in Interregional trade capacity"
)
model.vTradeFixom = Var(model.mTradeFixom, doc="Interregional trade fixed O&M costs")
model.vImportIrCost = Var(model.mImportIrCost, doc="Import costs from other regions")
model.vExportIrCost = Var(
    model.mExportIrCost, doc="Credits (revenue) for export to other regions"
)
model.vImportRowCost = Var(model.mImportRowCost, doc="Import costs from the ROW")
model.vExportRowCost = Var(model.mExportRowCost, doc="Credits for export to the ROW")
model.vTechNewCap = Var(model.mTechNew, domain=pyo.NonNegativeReals, doc="New capacity")
model.vTechRetiredStockCum = Var(
    model.mvTechRetiredStock, domain=pyo.NonNegativeReals, doc="Early retired stock"
)
model.vTechRetiredStock = Var(
    model.mvTechRetiredStock, domain=pyo.NonNegativeReals, doc="Early retired stock"
)
model.vTechRetiredNewCap = Var(
    model.mvTechRetiredNewCap,
    domain=pyo.NonNegativeReals,
    doc="Early retired new capacity",
)
model.vTechCap = Var(
    model.mTechSpan, domain=pyo.NonNegativeReals, doc="Total capacity of the technology"
)
model.vTechAct = Var(
    model.mvTechAct, domain=pyo.NonNegativeReals, doc="Activity level of technology"
)
model.vTechInp = Var(model.mvTechInp, domain=pyo.NonNegativeReals, doc="Input level")
model.vTechOut = Var(
    model.mvTechOut,
    domain=pyo.NonNegativeReals,
    doc="Commodity output from technology - tech timeframe",
)
model.vTechOutRY = Var(
    model.mTechOutRY,
    domain=pyo.NonNegativeReals,
    doc="Commodity output from technology - tech timeframe",
)
model.vTechAInp = Var(
    model.mvTechAInp, domain=pyo.NonNegativeReals, doc="Auxiliary commodity input"
)
model.vTechAOut = Var(
    model.mvTechAOut, domain=pyo.NonNegativeReals, doc="Auxiliary commodity output"
)
model.vSupOut = Var(model.mSupAva, domain=pyo.NonNegativeReals, doc="Output of supply")
model.vSupReserve = Var(
    model.mvSupReserve, domain=pyo.NonNegativeReals, doc="Cumulative supply (weighted)"
)
model.vDemInp = Var(model.mvDemInp, domain=pyo.NonNegativeReals, doc="Input to demand")
model.vOutTot = Var(
    model.mvOutTot,
    domain=pyo.NonNegativeReals,
    doc="Total commodity output (all processes) (weighted)",
)
model.vOutTotRY = Var(
    model.mOutTotRY,
    domain=pyo.NonNegativeReals,
    doc="Total commodity output (all processes) (weighted)",
)
model.vInpTot = Var(
    model.mvInpTot,
    domain=pyo.NonNegativeReals,
    doc="Total commodity input (all processes) (weighted)",
)
model.vInpTotRY = Var(
    model.mInpTotRY,
    domain=pyo.NonNegativeReals,
    doc="Total commodity input (all processes) (weighted)",
)
model.vInp2Lo = Var(
    model.mvInp2Lo,
    domain=pyo.NonNegativeReals,
    doc="Desagregation of slices for input parent to (grand)child",
)
model.vOut2Lo = Var(
    model.mvOut2Lo,
    domain=pyo.NonNegativeReals,
    doc="Desagregation of slices for output parent to (grand)child",
)
model.vSupOutTot = Var(
    model.mSupOutTot,
    domain=pyo.NonNegativeReals,
    doc="Total commodity supply (weighted)",
)
model.vTechInpTot = Var(
    model.mTechInpTot,
    domain=pyo.NonNegativeReals,
    doc="Total commodity (main & aux) input to technologies (weighted)",
)
model.vTechOutTot = Var(
    model.mTechOutTot,
    domain=pyo.NonNegativeReals,
    doc="Total commodity (main & aux) output from technologies (weighted)",
)
model.vStorageInpTot = Var(
    model.mStorageInpTot,
    domain=pyo.NonNegativeReals,
    doc="Total commodity (main & aux) input to storage (weighted)",
)
model.vStorageOutTot = Var(
    model.mStorageOutTot,
    domain=pyo.NonNegativeReals,
    doc="Total commodity (main & aux) output from storage (weighted)",
)
model.vStorageAInp = Var(
    model.mvStorageAInp,
    domain=pyo.NonNegativeReals,
    doc="Aux-commodity input to storage",
)
model.vStorageAOut = Var(
    model.mvStorageAOut,
    domain=pyo.NonNegativeReals,
    doc="Aux-commodity input from storage",
)
model.vDummyImport = Var(
    model.mDummyImport, domain=pyo.NonNegativeReals, doc="Dummy import (for debugging)"
)
model.vDummyExport = Var(
    model.mDummyExport, domain=pyo.NonNegativeReals, doc="Dummy export (for debugging)"
)
model.vStorageInp = Var(
    model.mvStorageStore, domain=pyo.NonNegativeReals, doc="Storage input"
)
model.vStorageOut = Var(
    model.mvStorageStore, domain=pyo.NonNegativeReals, doc="Storage output"
)
model.vStorageStore = Var(
    model.mvStorageStore, domain=pyo.NonNegativeReals, doc="Storage level"
)
model.vStorageInv = Var(
    model.mStorageNew, domain=pyo.NonNegativeReals, doc="Storage investments"
)
model.vStorageEac = Var(
    model.mStorageEac, domain=pyo.NonNegativeReals, doc="Storage EAC investments"
)
model.vStorageCap = Var(
    model.mStorageSpan, domain=pyo.NonNegativeReals, doc="Storage capacity"
)
model.vStorageNewCap = Var(
    model.mStorageNew, domain=pyo.NonNegativeReals, doc="Storage new capacity"
)
model.vImportTot = Var(
    model.mImport,
    domain=pyo.NonNegativeReals,
    doc="Total regional import (Ir + ROW) (weighted)",
)
model.vExportTot = Var(
    model.mExport,
    domain=pyo.NonNegativeReals,
    doc="Total regional export (Ir + ROW) (weighted)",
)
model.vTradeIr = Var(
    model.mvTradeIr,
    domain=pyo.NonNegativeReals,
    doc="Total physical trade flows between regions",
)
model.vTradeIrAInp = Var(
    model.mvTradeIrAInp, domain=pyo.NonNegativeReals, doc="Trade auxilari input"
)
model.vTradeIrAInpTot = Var(
    model.mvTradeIrAInpTot,
    domain=pyo.NonNegativeReals,
    doc="Trade total auxilari input (weighted)",
)
model.vTradeIrAOut = Var(
    model.mvTradeIrAOut, domain=pyo.NonNegativeReals, doc="Trade auxilari output"
)
model.vTradeIrAOutTot = Var(
    model.mvTradeIrAOutTot,
    domain=pyo.NonNegativeReals,
    doc="Trade auxilari output total (weighted)",
)
model.vExportRowCum = Var(
    model.mExpComm, domain=pyo.NonNegativeReals, doc="Cumulative export to the ROW"
)
model.vExportRow = Var(
    model.mExportRow, domain=pyo.NonNegativeReals, doc="Export to the ROW"
)
model.vImportRowCum = Var(
    model.mImpComm, domain=pyo.NonNegativeReals, doc="Cumulative import from the ROW"
)
model.vImportRow = Var(
    model.mImportRow, domain=pyo.NonNegativeReals, doc="Import from the ROW"
)
model.vTradeCap = Var(
    model.mTradeSpan, domain=pyo.NonNegativeReals, doc="Trade capacity"
)
model.vTradeInv = Var(
    model.mTradeEac,
    domain=pyo.NonNegativeReals,
    doc="Investment in trade capacity (overnight)",
)
model.vTradeNewCap = Var(
    model.mTradeNew, domain=pyo.NonNegativeReals, doc="New trade capacity"
)
model.vTotalUserCosts = Var(
    model.mvTotalUserCosts,
    domain=pyo.NonNegativeReals,
    doc="Total additional costs (set by user)",
)
# eqTechSng2Sng(tech, region, comm, commp, year, slice)$meqTechSng2Sng(tech, region, comm, commp, year, slice)
model.eqTechSng2Sng = Constraint(
    model.meqTechSng2Sng,
    rule=lambda model, t, r, c, cp, y, s: model.vTechInp[t, c, r, y, s]
    * model.pTechCinp2use[t, c, r, y, s]
    == (model.vTechOut[t, cp, r, y, s])
    / (model.pTechUse2cact[t, cp, r, y, s] * model.pTechCact2cout[t, cp, r, y, s]),
)
# eqTechGrp2Sng(tech, region, group, commp, year, slice)$meqTechGrp2Sng(tech, region, group, commp, year, slice)
model.eqTechGrp2Sng = Constraint(
    model.meqTechGrp2Sng,
    rule=lambda model, t, r, g, cp, y, s: model.pTechGinp2use[t, g, r, y, s]
    * sum(
        (
            (model.vTechInp[t, c, r, y, s] * model.pTechCinp2ginp[t, c, r, y, s])
            if (t, c, r, y, s) in model.mvTechInp
            else 0
        )
        for c in model.comm
        if (t, g, c) in model.mTechGroupComm
    )
    == (model.vTechOut[t, cp, r, y, s])
    / (model.pTechUse2cact[t, cp, r, y, s] * model.pTechCact2cout[t, cp, r, y, s]),
)
# eqTechSng2Grp(tech, region, comm, groupp, year, slice)$meqTechSng2Grp(tech, region, comm, groupp, year, slice)
model.eqTechSng2Grp = Constraint(
    model.meqTechSng2Grp,
    rule=lambda model, t, r, c, gp, y, s: model.vTechInp[t, c, r, y, s]
    * model.pTechCinp2use[t, c, r, y, s]
    == sum(
        (
            (
                (model.vTechOut[t, cp, r, y, s])
                / (
                    model.pTechUse2cact[t, cp, r, y, s]
                    * model.pTechCact2cout[t, cp, r, y, s]
                )
            )
            if (t, cp, r, y, s) in model.mvTechOut
            else 0
        )
        for cp in model.comm
        if (t, gp, cp) in model.mTechGroupComm
    ),
)
# eqTechGrp2Grp(tech, region, group, groupp, year, slice)$meqTechGrp2Grp(tech, region, group, groupp, year, slice)
model.eqTechGrp2Grp = Constraint(
    model.meqTechGrp2Grp,
    rule=lambda model, t, r, g, gp, y, s: model.pTechGinp2use[t, g, r, y, s]
    * sum(
        (
            (model.vTechInp[t, c, r, y, s] * model.pTechCinp2ginp[t, c, r, y, s])
            if (t, c, r, y, s) in model.mvTechInp
            else 0
        )
        for c in model.comm
        if (t, g, c) in model.mTechGroupComm
    )
    == sum(
        (
            (
                (model.vTechOut[t, cp, r, y, s])
                / (
                    model.pTechUse2cact[t, cp, r, y, s]
                    * model.pTechCact2cout[t, cp, r, y, s]
                )
            )
            if (t, cp, r, y, s) in model.mvTechOut
            else 0
        )
        for cp in model.comm
        if (t, gp, cp) in model.mTechGroupComm
    ),
)
# eqTechShareInpLo(tech, region, group, comm, year, slice)$meqTechShareInpLo(tech, region, group, comm, year, slice)
model.eqTechShareInpLo = Constraint(
    model.meqTechShareInpLo,
    rule=lambda model, t, r, g, c, y, s: model.vTechInp[t, c, r, y, s]
    >= model.pTechShareLo[t, c, r, y, s]
    * sum(
        (model.vTechInp[t, cp, r, y, s] if (t, cp, r, y, s) in model.mvTechInp else 0)
        for cp in model.comm
        if (t, g, cp) in model.mTechGroupComm
    ),
)
# eqTechShareInpUp(tech, region, group, comm, year, slice)$meqTechShareInpUp(tech, region, group, comm, year, slice)
model.eqTechShareInpUp = Constraint(
    model.meqTechShareInpUp,
    rule=lambda model, t, r, g, c, y, s: model.vTechInp[t, c, r, y, s]
    <= model.pTechShareUp[t, c, r, y, s]
    * sum(
        (model.vTechInp[t, cp, r, y, s] if (t, cp, r, y, s) in model.mvTechInp else 0)
        for cp in model.comm
        if (t, g, cp) in model.mTechGroupComm
    ),
)
# eqTechShareOutLo(tech, region, group, comm, year, slice)$meqTechShareOutLo(tech, region, group, comm, year, slice)
model.eqTechShareOutLo = Constraint(
    model.meqTechShareOutLo,
    rule=lambda model, t, r, g, c, y, s: model.vTechOut[t, c, r, y, s]
    >= model.pTechShareLo[t, c, r, y, s]
    * sum(
        (model.vTechOut[t, cp, r, y, s] if (t, cp, r, y, s) in model.mvTechOut else 0)
        for cp in model.comm
        if (t, g, cp) in model.mTechGroupComm
    ),
)
# eqTechShareOutUp(tech, region, group, comm, year, slice)$meqTechShareOutUp(tech, region, group, comm, year, slice)
model.eqTechShareOutUp = Constraint(
    model.meqTechShareOutUp,
    rule=lambda model, t, r, g, c, y, s: model.vTechOut[t, c, r, y, s]
    <= model.pTechShareUp[t, c, r, y, s]
    * sum(
        (model.vTechOut[t, cp, r, y, s] if (t, cp, r, y, s) in model.mvTechOut else 0)
        for cp in model.comm
        if (t, g, cp) in model.mTechGroupComm
    ),
)
# eqTechAInp(tech, comm, region, year, slice)$mvTechAInp(tech, comm, region, year, slice)
model.eqTechAInp = Constraint(
    model.mvTechAInp,
    rule=lambda model, t, c, r, y, s: model.vTechAInp[t, c, r, y, s]
    == (
        (model.vTechAct[t, r, y, s] * model.pTechAct2AInp[t, c, r, y, s])
        if (t, c, r, y, s) in model.mTechAct2AInp
        else 0
    )
    + (
        (
            (model.vTechCap[t, r, y] * model.pTechCap2AInp[t, c, r, y, s])
            / (model.pTechCap2act[t])
        )
        if (t, c, r, y, s) in model.mTechCap2AInp
        else 0
    )
    + (
        (model.vTechNewCap[t, r, y] * model.pTechNCap2AInp[t, c, r, y, s])
        if (t, c, r, y, s) in model.mTechNCap2AInp
        else 0
    )
    + sum(
        model.pTechCinp2AInp[t, c, cp, r, y, s] * model.vTechInp[t, cp, r, y, s]
        for cp in model.comm
        if (t, c, cp, r, y, s) in model.mTechCinp2AInp
    )
    + sum(
        model.pTechCout2AInp[t, c, cp, r, y, s] * model.vTechOut[t, cp, r, y, s]
        for cp in model.comm
        if (t, c, cp, r, y, s) in model.mTechCout2AInp
    ),
)
# eqTechAOut(tech, comm, region, year, slice)$mvTechAOut(tech, comm, region, year, slice)
model.eqTechAOut = Constraint(
    model.mvTechAOut,
    rule=lambda model, t, c, r, y, s: model.vTechAOut[t, c, r, y, s]
    == (
        (model.vTechAct[t, r, y, s] * model.pTechAct2AOut[t, c, r, y, s])
        if (t, c, r, y, s) in model.mTechAct2AOut
        else 0
    )
    + (
        (
            (model.vTechCap[t, r, y] * model.pTechCap2AOut[t, c, r, y, s])
            / (model.pTechCap2act[t])
        )
        if (t, c, r, y, s) in model.mTechCap2AOut
        else 0
    )
    + (
        (model.vTechNewCap[t, r, y] * model.pTechNCap2AOut[t, c, r, y, s])
        if (t, c, r, y, s) in model.mTechNCap2AOut
        else 0
    )
    + sum(
        model.pTechCinp2AOut[t, c, cp, r, y, s] * model.vTechInp[t, cp, r, y, s]
        for cp in model.comm
        if (t, c, cp, r, y, s) in model.mTechCinp2AOut
    )
    + sum(
        model.pTechCout2AOut[t, c, cp, r, y, s] * model.vTechOut[t, cp, r, y, s]
        for cp in model.comm
        if (t, c, cp, r, y, s) in model.mTechCout2AOut
    ),
)
# eqTechAfLo(tech, region, year, slice)$meqTechAfLo(tech, region, year, slice)
model.eqTechAfLo = Constraint(
    model.meqTechAfLo,
    rule=lambda model, t, r, y, s: model.pTechAfLo[t, r, y, s]
    * model.pTechCap2act[t]
    * model.vTechCap[t, r, y]
    * model.pSliceShare[s]
    * prod(
        model.pTechWeatherAfLo[wth1, t] * model.pWeather[wth1, r, y, s]
        for wth1 in model.weather
        if (wth1, t) in model.mTechWeatherAfLo
    )
    <= model.vTechAct[t, r, y, s],
)
# eqTechAfUp(tech, region, year, slice)$meqTechAfUp(tech, region, year, slice)
model.eqTechAfUp = Constraint(
    model.meqTechAfUp,
    rule=lambda model, t, r, y, s: model.vTechAct[t, r, y, s]
    <= model.pTechAfUp[t, r, y, s]
    * model.pTechCap2act[t]
    * model.vTechCap[t, r, y]
    * model.pSliceShare[s]
    * prod(
        model.pTechWeatherAfUp[wth1, t] * model.pWeather[wth1, r, y, s]
        for wth1 in model.weather
        if (wth1, t) in model.mTechWeatherAfUp
    ),
)
# eqTechAfsLo(tech, region, year, slice)$meqTechAfsLo(tech, region, year, slice)
model.eqTechAfsLo = Constraint(
    model.meqTechAfsLo,
    rule=lambda model, t, r, y, s: model.pTechAfsLo[t, r, y, s]
    * model.pTechCap2act[t]
    * model.vTechCap[t, r, y]
    * model.pSliceShare[s]
    * prod(
        model.pTechWeatherAfsLo[wth1, t] * model.pWeather[wth1, r, y, s]
        for wth1 in model.weather
        if (wth1, t) in model.mTechWeatherAfsLo
    )
    <= sum(
        (model.vTechAct[t, r, y, sp] if (t, r, y, sp) in model.mvTechAct else 0)
        for sp in model.slice
        if (s, sp) in model.mSliceParentChildE
    ),
)
# eqTechAfsUp(tech, region, year, slice)$meqTechAfsUp(tech, region, year, slice)
model.eqTechAfsUp = Constraint(
    model.meqTechAfsUp,
    rule=lambda model, t, r, y, s: sum(
        (model.vTechAct[t, r, y, sp] if (t, r, y, sp) in model.mvTechAct else 0)
        for sp in model.slice
        if (s, sp) in model.mSliceParentChildE
    )
    <= model.pTechAfsUp[t, r, y, s]
    * model.pTechCap2act[t]
    * model.vTechCap[t, r, y]
    * model.pSliceShare[s]
    * prod(
        model.pTechWeatherAfsUp[wth1, t] * model.pWeather[wth1, r, y, s]
        for wth1 in model.weather
        if (wth1, t) in model.mTechWeatherAfsUp
    ),
)
# eqTechRampUp(tech, region, year, slice, slicep)$mTechRampUp(tech, region, year, slice, slicep)
model.eqTechRampUp = Constraint(
    model.mTechRampUp,
    rule=lambda model, t, r, y, s, sp: (model.vTechAct[t, r, y, s])
    / (model.pSliceShare[s])
    - (model.vTechAct[t, r, y, sp]) / (model.pSliceShare[sp])
    <= (
        model.pSliceShare[s]
        * model.pTechCap2act[t]
        * model.pTechCap2act[t]
        * model.vTechCap[t, r, y]
    )
    / (model.pTechRampUp[t, r, y, s]),
)
# eqTechRampDown(tech, region, year, slice, slicep)$mTechRampDown(tech, region, year, slice, slicep)
model.eqTechRampDown = Constraint(
    model.mTechRampDown,
    rule=lambda model, t, r, y, s, sp: (model.vTechAct[t, r, y, sp])
    / (model.pSliceShare[sp])
    - (model.vTechAct[t, r, y, s]) / (model.pSliceShare[s])
    <= (
        model.pSliceShare[s]
        * model.pTechCap2act[t]
        * model.pTechCap2act[t]
        * model.vTechCap[t, r, y]
    )
    / (model.pTechRampDown[t, r, y, s]),
)
# eqTechActSng(tech, comm, region, year, slice)$meqTechActSng(tech, comm, region, year, slice)
model.eqTechActSng = Constraint(
    model.meqTechActSng,
    rule=lambda model, t, c, r, y, s: model.vTechAct[t, r, y, s]
    == (model.vTechOut[t, c, r, y, s]) / (model.pTechCact2cout[t, c, r, y, s]),
)
# eqTechActGrp(tech, group, region, year, slice)$meqTechActGrp(tech, group, region, year, slice)
model.eqTechActGrp = Constraint(
    model.meqTechActGrp,
    rule=lambda model, t, g, r, y, s: model.vTechAct[t, r, y, s]
    == sum(
        (
            ((model.vTechOut[t, c, r, y, s]) / (model.pTechCact2cout[t, c, r, y, s]))
            if (t, c, r, y, s) in model.mvTechOut
            else 0
        )
        for c in model.comm
        if (t, g, c) in model.mTechGroupComm
    ),
)
# eqTechAfcOutLo(tech, region, comm, year, slice)$meqTechAfcOutLo(tech, region, comm, year, slice)
model.eqTechAfcOutLo = Constraint(
    model.meqTechAfcOutLo,
    rule=lambda model, t, r, c, y, s: model.pTechCact2cout[t, c, r, y, s]
    * model.pTechAfcLo[t, c, r, y, s]
    * model.pTechCap2act[t]
    * model.vTechCap[t, r, y]
    * model.pSliceShare[s]
    * prod(
        model.pTechWeatherAfcLo[wth1, t, c] * model.pWeather[wth1, r, y, s]
        for wth1 in model.weather
        if (wth1, t, c) in model.mTechWeatherAfcLo
    )
    <= model.vTechOut[t, c, r, y, s],
)
# eqTechAfcOutUp(tech, region, comm, year, slice)$meqTechAfcOutUp(tech, region, comm, year, slice)
model.eqTechAfcOutUp = Constraint(
    model.meqTechAfcOutUp,
    rule=lambda model, t, r, c, y, s: model.vTechOut[t, c, r, y, s]
    <= model.pTechCact2cout[t, c, r, y, s]
    * model.pTechAfcUp[t, c, r, y, s]
    * model.pTechCap2act[t]
    * model.vTechCap[t, r, y]
    * prod(
        model.pTechWeatherAfcUp[wth1, t, c] * model.pWeather[wth1, r, y, s]
        for wth1 in model.weather
        if (wth1, t, c) in model.mTechWeatherAfcUp
    ),
)
# eqTechAfcInpLo(tech, region, comm, year, slice)$meqTechAfcInpLo(tech, region, comm, year, slice)
model.eqTechAfcInpLo = Constraint(
    model.meqTechAfcInpLo,
    rule=lambda model, t, r, c, y, s: model.pTechAfcLo[t, c, r, y, s]
    * model.pTechCap2act[t]
    * model.vTechCap[t, r, y]
    * model.pSliceShare[s]
    * prod(
        model.pTechWeatherAfcLo[wth1, t, c] * model.pWeather[wth1, r, y, s]
        for wth1 in model.weather
        if (wth1, t, c) in model.mTechWeatherAfcLo
    )
    <= model.vTechInp[t, c, r, y, s],
)
# eqTechAfcInpUp(tech, region, comm, year, slice)$meqTechAfcInpUp(tech, region, comm, year, slice)
model.eqTechAfcInpUp = Constraint(
    model.meqTechAfcInpUp,
    rule=lambda model, t, r, c, y, s: model.vTechInp[t, c, r, y, s]
    <= model.pTechAfcUp[t, c, r, y, s]
    * model.pTechCap2act[t]
    * model.vTechCap[t, r, y]
    * model.pSliceShare[s]
    * prod(
        model.pTechWeatherAfcUp[wth1, t, c] * model.pWeather[wth1, r, y, s]
        for wth1 in model.weather
        if (wth1, t, c) in model.mTechWeatherAfcUp
    ),
)
# eqTechCap(tech, region, year)$mTechSpan(tech, region, year)
model.eqTechCap = Constraint(
    model.mTechSpan,
    rule=lambda model, t, r, y: model.vTechCap[t, r, y]
    == model.pTechStock[t, r, y]
    - (
        model.vTechRetiredStockCum[t, r, y]
        if (t, r, y) in model.mvTechRetiredStock
        else 0
    )
    + sum(
        model.pPeriodLen[yp]
        * (
            model.vTechNewCap[t, r, yp]
            - sum(
                model.vTechRetiredNewCap[t, r, yp, ye]
                for ye in model.year
                if (
                    (t, r, yp, ye) in model.mvTechRetiredNewCap
                    and model.ordYear[y] >= model.ordYear[ye]
                )
            )
        )
        for yp in model.year
        if (
            (t, r, yp) in model.mTechNew
            and model.ordYear[y] >= model.ordYear[yp]
            and (
                model.ordYear[y] < model.pTechOlife[t, r] + model.ordYear[yp]
                or (t, r) in model.mTechOlifeInf
            )
        )
    ),
)
# eqTechCapLo(tech, region, year)$mTechCapLo(tech, region, year)
model.eqTechCapLo = Constraint(
    model.mTechCapLo,
    rule=lambda model, t, r, y: model.vTechCap[t, r, y] >= model.pTechCapLo[t, r, y],
)
# eqTechCapUp(tech, region, year)$mTechCapUp(tech, region, year)
model.eqTechCapUp = Constraint(
    model.mTechCapUp,
    rule=lambda model, t, r, y: model.vTechCap[t, r, y] <= model.pTechCapUp[t, r, y],
)
# eqTechNewCapLo(tech, region, year)$mTechNewCapLo(tech, region, year)
model.eqTechNewCapLo = Constraint(
    model.mTechNewCapLo,
    rule=lambda model, t, r, y: model.vTechNewCap[t, r, y]
    >= model.pTechNewCapLo[t, r, y] * model.pPeriodLen[y],
)
# eqTechNewCapUp(tech, region, year)$mTechNewCapUp(tech, region, year)
model.eqTechNewCapUp = Constraint(
    model.mTechNewCapUp,
    rule=lambda model, t, r, y: model.vTechNewCap[t, r, y]
    <= model.pTechNewCapUp[t, r, y] * model.pPeriodLen[y],
)
# eqTechRetiredNewCap(tech, region, year)$meqTechRetiredNewCap(tech, region, year)
model.eqTechRetiredNewCap = Constraint(
    model.meqTechRetiredNewCap,
    rule=lambda model, t, r, y: sum(
        model.vTechRetiredNewCap[t, r, y, yp] * model.pPeriodLen[yp]
        for yp in model.year
        if (t, r, y, yp) in model.mvTechRetiredNewCap
    )
    <= model.vTechNewCap[t, r, y] * model.pPeriodLen[y],
)
# eqTechRetiredStockCum(tech, region, year)$mvTechRetiredStock(tech, region, year)
model.eqTechRetiredStockCum = Constraint(
    model.mvTechRetiredStock,
    rule=lambda model, t, r, y: model.vTechRetiredStockCum[t, r, y]
    <= model.pTechStock[t, r, y],
)
# eqTechRetiredStock(tech, region, year)$mvTechRetiredStock(tech, region, year)
model.eqTechRetiredStock = Constraint(
    model.mvTechRetiredStock,
    rule=lambda model, t, r, y: model.vTechRetiredStock[t, r, y] * model.pPeriodLen[y]
    == model.vTechRetiredStockCum[t, r, y]
    - sum(
        model.vTechRetiredStockCum[t, r, yp]
        for yp in model.year
        if (yp, y) in model.mMilestoneNext
    ),
)
# eqTechRetUp(tech, region, year)$mTechRetUp(tech, region, year)
model.eqTechRetUp = Constraint(
    model.mTechRetUp,
    rule=lambda model, t, r, y: (
        model.vTechRetiredStock[t, r, y] if (t, r, y) in model.mvTechRetiredStock else 0
    )
    + sum(
        model.vTechRetiredNewCap[t, r, y, yp]
        for yp in model.year
        if (t, r, y, yp) in model.mvTechRetiredNewCap
    )
    <= model.pTechRetUp[t, r, y] * model.pPeriodLen[y],
)
# eqTechRetLo(tech, region, year)$mTechRetLo(tech, region, year)
model.eqTechRetLo = Constraint(
    model.mTechRetLo,
    rule=lambda model, t, r, y: (
        model.vTechRetiredStock[t, r, y] if (t, r, y) in model.mvTechRetiredStock else 0
    )
    + sum(
        model.vTechRetiredNewCap[t, r, y, yp]
        for yp in model.year
        if (t, r, y, yp) in model.mvTechRetiredNewCap
    )
    >= model.pTechRetLo[t, r, y] * model.pPeriodLen[y],
)
# eqTechRetCost(tech, region, year)$mTechRetCost(tech, region, year)
model.eqTechRetCost = Constraint(
    model.mTechRetCost,
    rule=lambda model, t, r, y: model.vTechRetCost[t, r, y]
    == model.pTechRetCost[t, r, y]
    * (model.vTechRetiredStock[t, r, y] if (t, r, y) in model.mvTechRetiredStock else 0)
    + sum(
        model.pTechRetCost[t, r, y]
        * (
            model.vTechRetiredNewCap[t, r, yp, y]
            if (t, r, yp, y) in model.mvTechRetiredNewCap
            else 0
        )
        for yp in model.year
        if (t, r, yp, y) in model.mvTechRetiredNewCap
    ),
)
# eqTechEac(tech, region, year)$mTechSpan(tech, region, year)
model.eqTechEac = Constraint(
    model.mTechSpan,
    rule=lambda model, t, r, y: model.vTechEac[t, r, y]
    == model.pTechEac[t, r, y] * model.vTechCap[t, r, y],
)
# eqTechInv(tech, region, year)$mTechInv(tech, region, year)
model.eqTechInv = Constraint(
    model.mTechInv,
    rule=lambda model, t, r, y: model.vTechInv[t, r, y]
    == model.pTechInvcost[t, r, y] * model.vTechNewCap[t, r, y],
)
# eqTechFixom(tech, region, year)$mTechFixom(tech, region, year)
model.eqTechFixom = Constraint(
    model.mTechFixom,
    rule=lambda model, t, r, y: model.vTechFixom[t, r, y]
    == model.pTechFixom[t, r, y] * model.vTechCap[t, r, y],
)
# eqTechVarom(tech, region, year)$mTechVarom(tech, region, year)
model.eqTechVarom = Constraint(
    model.mTechVarom,
    rule=lambda model, t, r, y: model.vTechVarom[t, r, y]
    == sum(
        model.pTechVarom[t, r, y, s]
        * model.pSliceWeight[y, s]
        * model.vTechAct[t, r, y, s]
        + sum(
            model.pTechCvarom[t, c, r, y, s]
            * model.pSliceWeight[y, s]
            * model.vTechInp[t, c, r, y, s]
            for c in model.comm
            if (t, c) in model.mTechInpComm
        )
        + sum(
            model.pTechCvarom[t, c, r, y, s]
            * model.pSliceWeight[y, s]
            * model.vTechOut[t, c, r, y, s]
            for c in model.comm
            if (t, c) in model.mTechOutComm
        )
        + sum(
            model.pTechAvarom[t, c, r, y, s]
            * model.pSliceWeight[y, s]
            * model.vTechAOut[t, c, r, y, s]
            for c in model.comm
            if (t, c, r, y, s) in model.mvTechAOut
        )
        + sum(
            model.pTechAvarom[t, c, r, y, s]
            * model.pSliceWeight[y, s]
            * model.vTechAInp[t, c, r, y, s]
            for c in model.comm
            if (t, c, r, y, s) in model.mvTechAInp
        )
        for s in model.slice
        if (t, s) in model.mTechSlice
    ),
)
# eqSupAvaUp(sup, comm, region, year, slice)$mSupAvaUp(sup, comm, region, year, slice)
model.eqSupAvaUp = Constraint(
    model.mSupAvaUp,
    rule=lambda model, s1, c, r, y, s: model.vSupOut[s1, c, r, y, s]
    <= model.pSupAvaUp[s1, c, r, y, s]
    * prod(
        model.pSupWeatherUp[wth1, s1] * model.pWeather[wth1, r, y, s]
        for wth1 in model.weather
        if (wth1, s1) in model.mSupWeatherUp
    ),
)
# eqSupAvaLo(sup, comm, region, year, slice)$meqSupAvaLo(sup, comm, region, year, slice)
model.eqSupAvaLo = Constraint(
    model.meqSupAvaLo,
    rule=lambda model, s1, c, r, y, s: model.vSupOut[s1, c, r, y, s]
    >= model.pSupAvaLo[s1, c, r, y, s]
    * prod(
        model.pSupWeatherLo[wth1, s1] * model.pWeather[wth1, r, y, s]
        for wth1 in model.weather
        if (wth1, s1) in model.mSupWeatherLo
    ),
)
# eqSupReserve(sup, comm, region)$mvSupReserve(sup, comm, region)
model.eqSupReserve = Constraint(
    model.mvSupReserve,
    rule=lambda model, s1, c, r: model.vSupReserve[s1, c, r]
    == sum(
        model.pPeriodLen[y] * model.pSliceWeight[y, s] * model.vSupOut[s1, c, r, y, s]
        for y in model.year
        for s in model.slice
        if (s1, c, r, y, s) in model.mSupAva
    ),
)
# eqSupReserveUp(sup, comm, region)$mSupReserveUp(sup, comm, region)
model.eqSupReserveUp = Constraint(
    model.mSupReserveUp,
    rule=lambda model, s1, c, r: model.vSupReserve[s1, c, r]
    <= model.pSupReserveUp[s1, c, r],
)
# eqSupReserveLo(sup, comm, region)$meqSupReserveLo(sup, comm, region)
model.eqSupReserveLo = Constraint(
    model.meqSupReserveLo,
    rule=lambda model, s1, c, r: model.vSupReserve[s1, c, r]
    >= model.pSupReserveLo[s1, c, r],
)
# eqSupCost(sup, region, year)$mvSupCost(sup, region, year)
model.eqSupCost = Constraint(
    model.mvSupCost,
    rule=lambda model, s1, r, y: model.vSupCost[s1, r, y]
    == sum(
        model.pSupCost[s1, c, r, y, s]
        * model.pSliceWeight[y, s]
        * model.vSupOut[s1, c, r, y, s]
        for c in model.comm
        for s in model.slice
        if (s1, c, r, y, s) in model.mSupAva
    ),
)
# eqDemInp(comm, region, year, slice)$mvDemInp(comm, region, year, slice)
model.eqDemInp = Constraint(
    model.mvDemInp,
    rule=lambda model, c, r, y, s: model.vDemInp[c, r, y, s]
    == sum(model.pDemand[d, c, r, y, s] for d in model.dem if (d, c) in model.mDemComm),
)
# eqAggOutTot(comm, region, year, slice)$mAggOut(comm, region, year, slice)
model.eqAggOutTot = Constraint(
    model.mAggOut,
    rule=lambda model, c, r, y, s: model.vAggOutTot[c, r, y, s]
    == sum(
        model.pAggregateFactor[c, cp]
        * sum(
            (model.vOutTot[cp, r, y, sp] if (cp, r, y, sp) in model.mvOutTot else 0)
            for sp in model.slice
            if (
                (c, r, y, sp) in model.mvOutTot
                and (s, sp) in model.mSliceParentChildE
                and (cp, sp) in model.mCommSlice
            )
        )
        for cp in model.comm
        if (c, cp) in model.mAggregateFactor
    ),
)
# eqEmsFuelTot(comm, region, year, slice)$mEmsFuelTot(comm, region, year, slice)
model.eqEmsFuelTot = Constraint(
    model.mEmsFuelTot,
    rule=lambda model, c, r, y, s: model.vEmsFuelTot[c, r, y, s]
    == sum(
        model.pEmissionFactor[c, cp]
        * sum(
            model.pTechEmisComm[t, cp]
            * sum(
                (
                    model.vTechInp[t, cp, r, y, sp]
                    if (t, c, cp, r, y, sp) in model.mTechEmsFuel
                    else 0
                )
                for sp in model.slice
                if (c, s, sp) in model.mCommSliceOrParent
            )
            for t in model.tech
            if (t, cp) in model.mTechInpComm
        )
        for cp in model.comm
        if (model.pEmissionFactor[c, cp] > 0)
    ),
)
# eqStorageAInp(stg, comm, region, year, slice)$mvStorageAInp(stg, comm, region, year, slice)
model.eqStorageAInp = Constraint(
    model.mvStorageAInp,
    rule=lambda model, st1, c, r, y, s: model.vStorageAInp[st1, c, r, y, s]
    == sum(
        (
            (
                model.pStorageStg2AInp[st1, c, r, y, s]
                * model.vStorageStore[st1, cp, r, y, s]
            )
            if (st1, c, r, y, s) in model.mStorageStg2AInp
            else 0
        )
        + (
            (
                model.pStorageCinp2AInp[st1, c, r, y, s]
                * model.vStorageInp[st1, cp, r, y, s]
            )
            if (st1, c, r, y, s) in model.mStorageCinp2AInp
            else 0
        )
        + (
            (
                model.pStorageCout2AInp[st1, c, r, y, s]
                * model.vStorageOut[st1, cp, r, y, s]
            )
            if (st1, c, r, y, s) in model.mStorageCout2AInp
            else 0
        )
        + (
            (model.pStorageCap2AInp[st1, c, r, y, s] * model.vStorageCap[st1, r, y])
            if (st1, c, r, y, s) in model.mStorageCap2AInp
            else 0
        )
        + (
            (model.pStorageNCap2AInp[st1, c, r, y, s] * model.vStorageNewCap[st1, r, y])
            if (st1, c, r, y, s) in model.mStorageNCap2AInp
            else 0
        )
        for cp in model.comm
        if (st1, cp) in model.mStorageComm
    ),
)
# eqStorageAOut(stg, comm, region, year, slice)$mvStorageAOut(stg, comm, region, year, slice)
model.eqStorageAOut = Constraint(
    model.mvStorageAOut,
    rule=lambda model, st1, c, r, y, s: model.vStorageAOut[st1, c, r, y, s]
    == sum(
        (
            (
                model.pStorageStg2AOut[st1, c, r, y, s]
                * model.vStorageStore[st1, cp, r, y, s]
            )
            if (st1, c, r, y, s) in model.mStorageStg2AOut
            else 0
        )
        + (
            (
                model.pStorageCinp2AOut[st1, c, r, y, s]
                * model.vStorageInp[st1, cp, r, y, s]
            )
            if (st1, c, r, y, s) in model.mStorageCinp2AOut
            else 0
        )
        + (
            (
                model.pStorageCout2AOut[st1, c, r, y, s]
                * model.vStorageOut[st1, cp, r, y, s]
            )
            if (st1, c, r, y, s) in model.mStorageCout2AOut
            else 0
        )
        + (
            (model.pStorageCap2AOut[st1, c, r, y, s] * model.vStorageCap[st1, r, y])
            if (st1, c, r, y, s) in model.mStorageCap2AOut
            else 0
        )
        + (
            (model.pStorageNCap2AOut[st1, c, r, y, s] * model.vStorageNewCap[st1, r, y])
            if (st1, c, r, y, s) in model.mStorageNCap2AOut
            else 0
        )
        for cp in model.comm
        if (st1, cp) in model.mStorageComm
    ),
)
# eqStorageStore(stg, comm, region, year, slicep, slice)$meqStorageStore(stg, comm, region, year, slicep, slice)
model.eqStorageStore = Constraint(
    model.meqStorageStore,
    rule=lambda model, st1, c, r, y, sp, s: model.vStorageStore[st1, c, r, y, s]
    == model.pStorageCharge[st1, c, r, y, s]
    + (
        (model.pStorageNCap2Stg[st1, c, r, y, s] * model.vStorageNewCap[st1, r, y])
        if (st1, r, y) in model.mStorageNew
        else 0
    )
    + model.pStorageInpEff[st1, c, r, y, sp] * model.vStorageInp[st1, c, r, y, sp]
    + ((model.pStorageStgEff[st1, c, r, y, s]) ** (model.pSliceShare[s]))
    * model.vStorageStore[st1, c, r, y, sp]
    - (model.vStorageOut[st1, c, r, y, sp]) / (model.pStorageOutEff[st1, c, r, y, sp]),
)
# eqStorageAfLo(stg, comm, region, year, slice)$meqStorageAfLo(stg, comm, region, year, slice)
model.eqStorageAfLo = Constraint(
    model.meqStorageAfLo,
    rule=lambda model, st1, c, r, y, s: model.vStorageStore[st1, c, r, y, s]
    >= model.pStorageAfLo[st1, r, y, s]
    * model.pStorageCap2stg[st1]
    * model.vStorageCap[st1, r, y]
    * prod(
        model.pStorageWeatherAfLo[wth1, st1] * model.pWeather[wth1, r, y, s]
        for wth1 in model.weather
        if (wth1, st1) in model.mStorageWeatherAfLo
    ),
)
# eqStorageAfUp(stg, comm, region, year, slice)$meqStorageAfUp(stg, comm, region, year, slice)
model.eqStorageAfUp = Constraint(
    model.meqStorageAfUp,
    rule=lambda model, st1, c, r, y, s: model.vStorageStore[st1, c, r, y, s]
    <= model.pStorageAfUp[st1, r, y, s]
    * model.pStorageCap2stg[st1]
    * model.vStorageCap[st1, r, y]
    * prod(
        model.pStorageWeatherAfUp[wth1, st1] * model.pWeather[wth1, r, y, s]
        for wth1 in model.weather
        if (wth1, st1) in model.mStorageWeatherAfUp
    ),
)
# eqStorageClear(stg, comm, region, year, slice)$mvStorageStore(stg, comm, region, year, slice)
model.eqStorageClear = Constraint(
    model.mvStorageStore,
    rule=lambda model, st1, c, r, y, s: (model.vStorageOut[st1, c, r, y, s])
    / (model.pStorageOutEff[st1, c, r, y, s])
    <= model.vStorageStore[st1, c, r, y, s],
)
# eqStorageInpUp(stg, comm, region, year, slice)$meqStorageInpUp(stg, comm, region, year, slice)
model.eqStorageInpUp = Constraint(
    model.meqStorageInpUp,
    rule=lambda model, st1, c, r, y, s: model.vStorageInp[st1, c, r, y, s]
    <= model.vStorageCap[st1, r, y]
    * model.pStorageCinpUp[st1, c, r, y, s]
    * prod(
        model.pStorageWeatherCinpUp[wth1, st1] * model.pWeather[wth1, r, y, s]
        for wth1 in model.weather
        if (wth1, st1) in model.mStorageWeatherCinpUp
    ),
)
# eqStorageInpLo(stg, comm, region, year, slice)$meqStorageInpLo(stg, comm, region, year, slice)
model.eqStorageInpLo = Constraint(
    model.meqStorageInpLo,
    rule=lambda model, st1, c, r, y, s: model.vStorageInp[st1, c, r, y, s]
    >= model.vStorageCap[st1, r, y]
    * model.pStorageCinpLo[st1, c, r, y, s]
    * prod(
        model.pStorageWeatherCinpLo[wth1, st1] * model.pWeather[wth1, r, y, s]
        for wth1 in model.weather
        if (wth1, st1) in model.mStorageWeatherCinpLo
    ),
)
# eqStorageOutUp(stg, comm, region, year, slice)$meqStorageOutUp(stg, comm, region, year, slice)
model.eqStorageOutUp = Constraint(
    model.meqStorageOutUp,
    rule=lambda model, st1, c, r, y, s: model.vStorageOut[st1, c, r, y, s]
    <= model.vStorageCap[st1, r, y]
    * model.pStorageCoutUp[st1, c, r, y, s]
    * prod(
        model.pStorageWeatherCoutUp[wth1, st1] * model.pWeather[wth1, r, y, s]
        for wth1 in model.weather
        if (wth1, st1) in model.mStorageWeatherCoutUp
    ),
)
# eqStorageOutLo(stg, comm, region, year, slice)$meqStorageOutLo(stg, comm, region, year, slice)
model.eqStorageOutLo = Constraint(
    model.meqStorageOutLo,
    rule=lambda model, st1, c, r, y, s: model.vStorageOut[st1, c, r, y, s]
    >= model.vStorageCap[st1, r, y]
    * model.pStorageCoutLo[st1, c, r, y, s]
    * prod(
        model.pStorageWeatherCoutLo[wth1, st1] * model.pWeather[wth1, r, y, s]
        for wth1 in model.weather
        if (wth1, st1) in model.mStorageWeatherCoutLo
    ),
)
# eqStorageCap(stg, region, year)$mStorageSpan(stg, region, year)
model.eqStorageCap = Constraint(
    model.mStorageSpan,
    rule=lambda model, st1, r, y: model.vStorageCap[st1, r, y]
    == model.pStorageStock[st1, r, y]
    + sum(
        model.pPeriodLen[yp] * model.vStorageNewCap[st1, r, yp]
        for yp in model.year
        if (
            model.ordYear[y] >= model.ordYear[yp]
            and (
                (st1, r) in model.mStorageOlifeInf
                or model.ordYear[y] < model.pStorageOlife[st1, r] + model.ordYear[yp]
            )
            and (st1, r, yp) in model.mStorageNew
        )
    ),
)
# eqStorageCapLo(stg, region, year)$mStorageCapLo(stg, region, year)
model.eqStorageCapLo = Constraint(
    model.mStorageCapLo,
    rule=lambda model, st1, r, y: model.vStorageCap[st1, r, y]
    >= model.pStorageCapLo[st1, r, y],
)
# eqStorageCapUp(stg, region, year)$mStorageCapUp(stg, region, year)
model.eqStorageCapUp = Constraint(
    model.mStorageCapUp,
    rule=lambda model, st1, r, y: model.vStorageCap[st1, r, y]
    <= model.pStorageCapUp[st1, r, y],
)
# eqStorageNewCapLo(stg, region, year)$mStorageNewCapLo(stg, region, year)
model.eqStorageNewCapLo = Constraint(
    model.mStorageNewCapLo,
    rule=lambda model, st1, r, y: model.vStorageNewCap[st1, r, y]
    >= model.pStorageNewCapLo[st1, r, y] * model.pPeriodLen[y],
)
# eqStorageNewCapUp(stg, region, year)$mStorageNewCapUp(stg, region, year)
model.eqStorageNewCapUp = Constraint(
    model.mStorageNewCapUp,
    rule=lambda model, st1, r, y: model.vStorageNewCap[st1, r, y]
    <= model.pStorageNewCapUp[st1, r, y] * model.pPeriodLen[y],
)
# eqStorageInv(stg, region, year)$mStorageNew(stg, region, year)
model.eqStorageInv = Constraint(
    model.mStorageNew,
    rule=lambda model, st1, r, y: model.vStorageInv[st1, r, y]
    == model.pStorageInvcost[st1, r, y] * model.vStorageNewCap[st1, r, y],
)
# eqStorageEac(stg, region, year)$mStorageEac(stg, region, year)
model.eqStorageEac = Constraint(
    model.mStorageEac,
    rule=lambda model, st1, r, y: model.vStorageEac[st1, r, y]
    == model.pStorageEac[st1, r, y] * model.vStorageCap[st1, r, y],
)
# eqStorageFixom(stg, region, year)$mStorageFixom(stg, region, year)
model.eqStorageFixom = Constraint(
    model.mStorageFixom,
    rule=lambda model, st1, r, y: model.vStorageFixom[st1, r, y]
    == model.pStorageFixom[st1, r, y] * model.vStorageCap[st1, r, y],
)
# eqStorageVarom(stg, region, year)$mStorageVarom(stg, region, year)
model.eqStorageVarom = Constraint(
    model.mStorageVarom,
    rule=lambda model, st1, r, y: model.vStorageVarom[st1, r, y]
    == sum(
        sum(
            model.pStorageCostInp[st1, r, y, s]
            * model.pSliceWeight[y, s]
            * model.vStorageInp[st1, c, r, y, s]
            + model.pStorageCostOut[st1, r, y, s]
            * model.pSliceWeight[y, s]
            * model.vStorageOut[st1, c, r, y, s]
            + model.pStorageCostStore[st1, r, y, s]
            * model.pSliceWeight[y, s]
            * model.vStorageStore[st1, c, r, y, s]
            for s in model.slice
            if (c, s) in model.mCommSlice
        )
        for c in model.comm
        if (st1, c) in model.mStorageComm
    ),
)
# eqImportTot(comm, dst, year, slice)$mImport(comm, dst, year, slice)
model.eqImportTot = Constraint(
    model.mImport,
    rule=lambda model, c, dst, y, s: model.vImportTot[c, dst, y, s]
    == sum(
        sum(
            (
                (
                    model.pTradeIrEff[t1, src, dst, y, s]
                    * model.vTradeIr[t1, c, src, dst, y, s]
                )
                if (t1, c, src, dst, y, s) in model.mvTradeIr
                else 0
            )
            for src in model.region
            if (t1, src, dst) in model.mTradeRoutes
        )
        for t1 in model.trade
        if (t1, c) in model.mTradeComm
    )
    + sum(
        (
            model.vImportRow[i, c, dst, y, s]
            if (i, c, dst, y, s) in model.mImportRow
            else 0
        )
        for i in model.imp
        if (i, c) in model.mImpComm
    ),
)
# eqExportTot(comm, src, year, slice)$mExport(comm, src, year, slice)
model.eqExportTot = Constraint(
    model.mExport,
    rule=lambda model, c, src, y, s: model.vExportTot[c, src, y, s]
    == sum(
        sum(
            (
                model.vTradeIr[t1, c, src, dst, y, s]
                if (t1, c, src, dst, y, s) in model.mvTradeIr
                else 0
            )
            for dst in model.region
            if (t1, src, dst) in model.mTradeRoutes
        )
        for t1 in model.trade
        if (t1, c) in model.mTradeComm
    )
    + sum(
        (
            model.vExportRow[e, c, src, y, s]
            if (e, c, src, y, s) in model.mExportRow
            else 0
        )
        for e in model.expp
        if (e, c) in model.mExpComm
    ),
)
# eqTradeFlowUp(trade, comm, src, dst, year, slice)$meqTradeFlowUp(trade, comm, src, dst, year, slice)
model.eqTradeFlowUp = Constraint(
    model.meqTradeFlowUp,
    rule=lambda model, t1, c, src, dst, y, s: model.vTradeIr[t1, c, src, dst, y, s]
    <= model.pTradeIrUp[t1, src, dst, y, s],
)
# eqTradeFlowLo(trade, comm, src, dst, year, slice)$meqTradeFlowLo(trade, comm, src, dst, year, slice)
model.eqTradeFlowLo = Constraint(
    model.meqTradeFlowLo,
    rule=lambda model, t1, c, src, dst, y, s: model.vTradeIr[t1, c, src, dst, y, s]
    >= model.pTradeIrLo[t1, src, dst, y, s],
)
# eqImportIrCost(trade, region, year)$mImportIrCost(trade, region, year)
model.eqImportIrCost = Constraint(
    model.mImportIrCost,
    rule=lambda model, t1, r, y: model.vImportIrCost[t1, r, y]
    == sum(
        sum(
            sum(
                (
                    (
                        (
                            model.pTradeIrCost[t1, src, r, y, s]
                            + model.pTradeIrMarkup[t1, src, r, y, s]
                        )
                        * model.vTradeIr[t1, c, src, r, y, s]
                        * model.pSliceWeight[y, s]
                    )
                    if (t1, c, src, r, y, s) in model.mvTradeIr
                    else 0
                )
                for s in model.slice
                if (t1, s) in model.mTradeSlice
            )
            for c in model.comm
            if (t1, c) in model.mTradeComm
        )
        for src in model.region
        if (t1, src, r) in model.mTradeRoutes
    ),
)
# eqExportIrCost(trade, region, year)$mExportIrCost(trade, region, year)
model.eqExportIrCost = Constraint(
    model.mExportIrCost,
    rule=lambda model, t1, r, y: model.vExportIrCost[t1, r, y]
    == -sum(
        sum(
            sum(
                (
                    (
                        (
                            model.pTradeIrCost[t1, r, dst, y, s]
                            + model.pTradeIrMarkup[t1, r, dst, y, s]
                        )
                        * model.vTradeIr[t1, c, r, dst, y, s]
                        * model.pSliceWeight[y, s]
                    )
                    if (t1, c, r, dst, y, s) in model.mvTradeIr
                    else 0
                )
                for s in model.slice
                if (t1, s) in model.mTradeSlice
            )
            for c in model.comm
            if (t1, c) in model.mTradeComm
        )
        for dst in model.region
        if (t1, r, dst) in model.mTradeRoutes
    ),
)
# eqExportRowUp(expp, comm, region, year, slice)$mExportRowUp(expp, comm, region, year, slice)
model.eqExportRowUp = Constraint(
    model.mExportRowUp,
    rule=lambda model, e, c, r, y, s: model.vExportRow[e, c, r, y, s]
    <= model.pExportRowUp[e, r, y, s],
)
# eqExportRowLo(expp, comm, region, year, slice)$meqExportRowLo(expp, comm, region, year, slice)
model.eqExportRowLo = Constraint(
    model.meqExportRowLo,
    rule=lambda model, e, c, r, y, s: model.vExportRow[e, c, r, y, s]
    >= model.pExportRowLo[e, r, y, s],
)
# eqExportRowCum(expp, comm)$mExpComm(expp, comm)
model.eqExportRowCum = Constraint(
    model.mExpComm,
    rule=lambda model, e, c: model.vExportRowCum[e, c]
    == sum(
        model.pPeriodLen[y] * model.pSliceWeight[y, s] * model.vExportRow[e, c, r, y, s]
        for r in model.region
        for y in model.year
        for s in model.slice
        if (e, c, r, y, s) in model.mExportRow
    ),
)
# eqExportRowResUp(expp, comm)$mExportRowCumUp(expp, comm)
model.eqExportRowResUp = Constraint(
    model.mExportRowCumUp,
    rule=lambda model, e, c: model.vExportRowCum[e, c] <= model.pExportRowRes[e],
)
# eqExportRowCost(expp, region, year)$mExportRowCost(expp, region, year)
model.eqExportRowCost = Constraint(
    model.mExportRowCost,
    rule=lambda model, e, r, y: model.vExportRowCost[e, r, y]
    == -sum(
        model.pExportRowPrice[e, r, y, s]
        * model.pSliceWeight[y, s]
        * model.vExportRow[e, c, r, y, s]
        for c in model.comm
        for s in model.slice
        if (e, c, r, y, s) in model.mExportRow
    ),
)
# eqImportRowUp(imp, comm, region, year, slice)$mImportRowUp(imp, comm, region, year, slice)
model.eqImportRowUp = Constraint(
    model.mImportRowUp,
    rule=lambda model, i, c, r, y, s: model.vImportRow[i, c, r, y, s]
    <= model.pImportRowUp[i, r, y, s],
)
# eqImportRowLo(imp, comm, region, year, slice)$meqImportRowLo(imp, comm, region, year, slice)
model.eqImportRowLo = Constraint(
    model.meqImportRowLo,
    rule=lambda model, i, c, r, y, s: model.vImportRow[i, c, r, y, s]
    >= model.pImportRowLo[i, r, y, s],
)
# eqImportRowCum(imp, comm)$mImpComm(imp, comm)
model.eqImportRowCum = Constraint(
    model.mImpComm,
    rule=lambda model, i, c: model.vImportRowCum[i, c]
    == sum(
        model.pPeriodLen[y] * model.pSliceWeight[y, s] * model.vImportRow[i, c, r, y, s]
        for r in model.region
        for y in model.year
        for s in model.slice
        if (i, c, r, y, s) in model.mImportRow
    ),
)
# eqImportRowResUp(imp, comm)$mImportRowCumUp(imp, comm)
model.eqImportRowResUp = Constraint(
    model.mImportRowCumUp,
    rule=lambda model, i, c: model.vImportRowCum[i, c] <= model.pImportRowRes[i],
)
# eqImportRowCost(imp, region, year)$mImportRowCost(imp, region, year)
model.eqImportRowCost = Constraint(
    model.mImportRowCost,
    rule=lambda model, i, r, y: model.vImportRowCost[i, r, y]
    == sum(
        model.pImportRowPrice[i, r, y, s]
        * model.pSliceWeight[y, s]
        * model.vImportRow[i, c, r, y, s]
        for c in model.comm
        for s in model.slice
        if (i, c, r, y, s) in model.mImportRow
    ),
)
# eqTradeCapFlow(trade, comm, year, slice)$meqTradeCapFlow(trade, comm, year, slice)
model.eqTradeCapFlow = Constraint(
    model.meqTradeCapFlow,
    rule=lambda model, t1, c, y, s: model.pSliceShare[s]
    * model.pTradeCap2Act[t1]
    * model.vTradeCap[t1, y]
    >= sum(
        model.vTradeIr[t1, c, src, dst, y, s]
        for src in model.region
        for dst in model.region
        if (t1, c, src, dst, y, s) in model.mvTradeIr
    ),
)
# eqTradeCap(trade, year)$mTradeSpan(trade, year)
model.eqTradeCap = Constraint(
    model.mTradeSpan,
    rule=lambda model, t1, y: model.vTradeCap[t1, y]
    == model.pTradeStock[t1, y]
    + sum(
        model.pPeriodLen[yp] * model.vTradeNewCap[t1, yp]
        for yp in model.year
        if (
            (t1, yp) in model.mTradeNew
            and model.ordYear[y] >= model.ordYear[yp]
            and (
                model.ordYear[y] < model.pTradeOlife[t1] + model.ordYear[yp]
                or t1 in model.mTradeOlifeInf
            )
        )
    ),
)
# eqTradeCapLo(trade, year)$mTradeCapLo(trade, year)
model.eqTradeCapLo = Constraint(
    model.mTradeCapLo,
    rule=lambda model, t1, y: model.vTradeCap[t1, y] >= model.pTradeCapLo[t1, y],
)
# eqTradeCapUp(trade, year)$mTradeCapUp(trade, year)
model.eqTradeCapUp = Constraint(
    model.mTradeCapUp,
    rule=lambda model, t1, y: model.vTradeCap[t1, y] <= model.pTradeCapUp[t1, y],
)
# eqTradeNewCapLo(trade, year)$mTradeNewCapLo(trade, year)
model.eqTradeNewCapLo = Constraint(
    model.mTradeNewCapLo,
    rule=lambda model, t1, y: model.vTradeNewCap[t1, y] * model.pPeriodLen[y]
    >= model.pTradeNewCapLo[t1, y],
)
# eqTradeNewCapUp(trade, year)$mTradeNewCapUp(trade, year)
model.eqTradeNewCapUp = Constraint(
    model.mTradeNewCapUp,
    rule=lambda model, t1, y: model.vTradeNewCap[t1, y] * model.pPeriodLen[y]
    <= model.pTradeNewCapUp[t1, y],
)
# eqTradeInv(trade, region, year)$mTradeInv(trade, region, year)
model.eqTradeInv = Constraint(
    model.mTradeInv,
    rule=lambda model, t1, r, y: model.vTradeInv[t1, r, y]
    == model.pTradeInvcost[t1, r, y] * model.vTradeNewCap[t1, y],
)
# eqTradeEac(trade, region, year)$mTradeEac(trade, region, year)
model.eqTradeEac = Constraint(
    model.mTradeEac,
    rule=lambda model, t1, r, y: model.vTradeEac[t1, r, y]
    == model.pTradeEac[t1, r, y] * model.vTradeCap[t1, y],
)
# eqTradeFixom(trade, region, year)$mTradeFixom(trade, region, year)
model.eqTradeFixom = Constraint(
    model.mTradeFixom,
    rule=lambda model, t1, r, y: model.vTradeFixom[t1, r, y]
    == model.pTradeFixom[t1, r, y] * model.vTradeCap[t1, y],
)
# eqTradeIrAInp(trade, comm, region, year, slice)$mvTradeIrAInp(trade, comm, region, year, slice)
model.eqTradeIrAInp = Constraint(
    model.mvTradeIrAInp,
    rule=lambda model, t1, c, r, y, s: model.vTradeIrAInp[t1, c, r, y, s]
    == sum(
        model.pTradeIrCsrc2Ainp[t1, c, r, dst, y, s]
        * sum(
            model.vTradeIr[t1, cp, r, dst, y, s]
            for cp in model.comm
            if (t1, cp) in model.mTradeComm
        )
        for dst in model.region
        if (t1, c, r, dst, y, s) in model.mTradeIrCsrc2Ainp
    )
    + sum(
        model.pTradeIrCdst2Ainp[t1, c, src, r, y, s]
        * sum(
            model.vTradeIr[t1, cp, src, r, y, s]
            for cp in model.comm
            if (t1, cp) in model.mTradeComm
        )
        for src in model.region
        if (t1, c, src, r, y, s) in model.mTradeIrCdst2Ainp
    ),
)
# eqTradeIrAOut(trade, comm, region, year, slice)$mvTradeIrAOut(trade, comm, region, year, slice)
model.eqTradeIrAOut = Constraint(
    model.mvTradeIrAOut,
    rule=lambda model, t1, c, r, y, s: model.vTradeIrAOut[t1, c, r, y, s]
    == sum(
        model.pTradeIrCsrc2Aout[t1, c, r, dst, y, s]
        * sum(
            model.vTradeIr[t1, cp, r, dst, y, s]
            for cp in model.comm
            if (t1, cp) in model.mTradeComm
        )
        for dst in model.region
        if (t1, c, r, dst, y, s) in model.mTradeIrCsrc2Aout
    )
    + sum(
        model.pTradeIrCdst2Aout[t1, c, src, r, y, s]
        * sum(
            model.vTradeIr[t1, cp, src, r, y, s]
            for cp in model.comm
            if (t1, cp) in model.mTradeComm
        )
        for src in model.region
        if (t1, c, src, r, y, s) in model.mTradeIrCdst2Aout
    ),
)
# eqTradeIrAInpTot(comm, region, year, slice)$mvTradeIrAInpTot(comm, region, year, slice)
model.eqTradeIrAInpTot = Constraint(
    model.mvTradeIrAInpTot,
    rule=lambda model, c, r, y, s: model.vTradeIrAInpTot[c, r, y, s]
    == sum(
        model.vTradeIrAInp[t1, c, r, y, sp]
        for t1 in model.trade
        for sp in model.slice
        if (
            (c, s, sp) in model.mCommSliceOrParent
            and (t1, c, r, y, sp) in model.mvTradeIrAInp
        )
    ),
)
# eqTradeIrAOutTot(comm, region, year, slice)$mvTradeIrAOutTot(comm, region, year, slice)
model.eqTradeIrAOutTot = Constraint(
    model.mvTradeIrAOutTot,
    rule=lambda model, c, r, y, s: model.vTradeIrAOutTot[c, r, y, s]
    == sum(
        model.vTradeIrAOut[t1, c, r, y, sp]
        for t1 in model.trade
        for sp in model.slice
        if (
            (c, s, sp) in model.mCommSliceOrParent
            and (t1, c, r, y, sp) in model.mvTradeIrAOut
        )
    ),
)
# eqBalLo(comm, region, year, slice)$meqBalLo(comm, region, year, slice)
model.eqBalLo = Constraint(
    model.meqBalLo, rule=lambda model, c, r, y, s: model.vBalance[c, r, y, s] >= 0
)
# eqBalUp(comm, region, year, slice)$meqBalUp(comm, region, year, slice)
model.eqBalUp = Constraint(
    model.meqBalUp, rule=lambda model, c, r, y, s: model.vBalance[c, r, y, s] <= 0
)
# eqBalFx(comm, region, year, slice)$meqBalFx(comm, region, year, slice)
model.eqBalFx = Constraint(
    model.meqBalFx, rule=lambda model, c, r, y, s: model.vBalance[c, r, y, s] == 0
)
# eqBal(comm, region, year, slice)$mvBalance(comm, region, year, slice)
model.eqBal = Constraint(
    model.mvBalance,
    rule=lambda model, c, r, y, s: model.vBalance[c, r, y, s]
    == (model.vOutTot[c, r, y, s] if (c, r, y, s) in model.mvOutTot else 0)
    - (model.vInpTot[c, r, y, s] if (c, r, y, s) in model.mvInpTot else 0),
)
# eqBalanceRY(comm, region, year)$mBalanceRY(comm, region, year)
model.eqBalanceRY = Constraint(
    model.mBalanceRY,
    rule=lambda model, c, r, y: model.vBalanceRY[c, r, y]
    == sum(
        model.pSliceWeight[y, s]
        * (model.vBalance[c, r, y, s] if (c, r, y, s) in model.mvBalance else 0)
        for s in model.slice
        if (c, r, y, s) in model.mvBalance
    ),
)
# eqOutTot(comm, region, year, slice)$mvOutTot(comm, region, year, slice)
model.eqOutTot = Constraint(
    model.mvOutTot,
    rule=lambda model, c, r, y, s: model.vOutTot[c, r, y, s]
    == (model.vDummyImport[c, r, y, s] if (c, r, y, s) in model.mDummyImport else 0)
    + (model.vSupOutTot[c, r, y, s] if (c, r, y, s) in model.mSupOutTot else 0)
    + (model.vEmsFuelTot[c, r, y, s] if (c, r, y, s) in model.mEmsFuelTot else 0)
    + (model.vAggOutTot[c, r, y, s] if (c, r, y, s) in model.mAggOut else 0)
    + (model.vTechOutTot[c, r, y, s] if (c, r, y, s) in model.mTechOutTot else 0)
    + (model.vStorageOutTot[c, r, y, s] if (c, r, y, s) in model.mStorageOutTot else 0)
    + (model.vImportTot[c, r, y, s] if (c, r, y, s) in model.mImport else 0)
    + (
        model.vTradeIrAOutTot[c, r, y, s]
        if (c, r, y, s) in model.mvTradeIrAOutTot
        else 0
    )
    + (
        sum(
            model.vOut2Lo[c, r, y, sp, s]
            for sp in model.slice
            if (
                (sp, s) in model.mSliceParentChild
                and (c, r, y, sp, s) in model.mvOut2Lo
            )
        )
        if (c, r, y, s) in model.mOutSub
        else 0
    ),
)
# eqOutTotRY(comm, region, year)$mOutTotRY(comm, region, year)
model.eqOutTotRY = Constraint(
    model.mOutTotRY,
    rule=lambda model, c, r, y: model.vOutTotRY[c, r, y]
    == sum(
        model.pSliceWeight[y, s]
        * (model.vOutTot[c, r, y, s] if (c, r, y, s) in model.mvOutTot else 0)
        for s in model.slice
        if (c, r, y, s) in model.mvOutTot
    ),
)
# eqOut2Lo(comm, region, year, slice)$mOut2Lo(comm, region, year, slice)
model.eqOut2Lo = Constraint(
    model.mOut2Lo,
    rule=lambda model, c, r, y, s: sum(
        model.vOut2Lo[c, r, y, s, sp]
        for sp in model.slice
        if (c, r, y, s, sp) in model.mvOut2Lo
    )
    == (model.vSupOutTot[c, r, y, s] if (c, r, y, s) in model.mSupOutTot else 0)
    + (model.vEmsFuelTot[c, r, y, s] if (c, r, y, s) in model.mEmsFuelTot else 0)
    + (model.vAggOutTot[c, r, y, s] if (c, r, y, s) in model.mAggOut else 0)
    + (model.vTechOutTot[c, r, y, s] if (c, r, y, s) in model.mTechOutTot else 0)
    + (model.vStorageOutTot[c, r, y, s] if (c, r, y, s) in model.mStorageOutTot else 0)
    + (model.vImportTot[c, r, y, s] if (c, r, y, s) in model.mImport else 0)
    + (
        model.vTradeIrAOutTot[c, r, y, s]
        if (c, r, y, s) in model.mvTradeIrAOutTot
        else 0
    ),
)
# eqInpTot(comm, region, year, slice)$mvInpTot(comm, region, year, slice)
model.eqInpTot = Constraint(
    model.mvInpTot,
    rule=lambda model, c, r, y, s: model.vInpTot[c, r, y, s]
    == (model.vDemInp[c, r, y, s] if (c, r, y, s) in model.mvDemInp else 0)
    + (model.vDummyExport[c, r, y, s] if (c, r, y, s) in model.mDummyExport else 0)
    + (model.vTechInpTot[c, r, y, s] if (c, r, y, s) in model.mTechInpTot else 0)
    + (model.vStorageInpTot[c, r, y, s] if (c, r, y, s) in model.mStorageInpTot else 0)
    + (model.vExportTot[c, r, y, s] if (c, r, y, s) in model.mExport else 0)
    + (
        model.vTradeIrAInpTot[c, r, y, s]
        if (c, r, y, s) in model.mvTradeIrAInpTot
        else 0
    )
    + (
        sum(
            model.vInp2Lo[c, r, y, sp, s]
            for sp in model.slice
            if (
                (sp, s) in model.mSliceParentChild
                and (c, r, y, sp, s) in model.mvInp2Lo
            )
        )
        if (c, r, y, s) in model.mInpSub
        else 0
    ),
)
# eqInpTotRY(comm, region, year)$mInpTotRY(comm, region, year)
model.eqInpTotRY = Constraint(
    model.mInpTotRY,
    rule=lambda model, c, r, y: model.vInpTotRY[c, r, y]
    == sum(
        model.pSliceWeight[y, s]
        * (model.vInpTot[c, r, y, s] if (c, r, y, s) in model.mvInpTot else 0)
        for s in model.slice
        if (c, r, y, s) in model.mvInpTot
    ),
)
# eqInp2Lo(comm, region, year, slice)$mInp2Lo(comm, region, year, slice)
model.eqInp2Lo = Constraint(
    model.mInp2Lo,
    rule=lambda model, c, r, y, s: sum(
        model.vInp2Lo[c, r, y, s, sp]
        for sp in model.slice
        if (c, r, y, s, sp) in model.mvInp2Lo
    )
    == (model.vTechInpTot[c, r, y, s] if (c, r, y, s) in model.mTechInpTot else 0)
    + (model.vStorageInpTot[c, r, y, s] if (c, r, y, s) in model.mStorageInpTot else 0)
    + (model.vExportTot[c, r, y, s] if (c, r, y, s) in model.mExport else 0)
    + (
        model.vTradeIrAInpTot[c, r, y, s]
        if (c, r, y, s) in model.mvTradeIrAInpTot
        else 0
    ),
)
# eqSupOutTot(comm, region, year, slice)$mSupOutTot(comm, region, year, slice)
model.eqSupOutTot = Constraint(
    model.mSupOutTot,
    rule=lambda model, c, r, y, s: model.vSupOutTot[c, r, y, s]
    == sum(
        model.vSupOut[s1, c, r, y, s] for s1 in model.sup if (s1, c) in model.mSupComm
    ),
)
# eqTechInpTot(comm, region, year, slice)$mTechInpTot(comm, region, year, slice)
model.eqTechInpTot = Constraint(
    model.mTechInpTot,
    rule=lambda model, c, r, y, s: model.vTechInpTot[c, r, y, s]
    == sum(
        (model.vTechInp[t, c, r, y, s] if (t, c, r, y, s) in model.mvTechInp else 0)
        for t in model.tech
        if (t, c) in model.mTechInpCommSameSlice
    )
    + sum(
        sum(
            (
                model.vTechInp[t, c, r, y, sp]
                if (t, c, r, y, sp) in model.mvTechInp
                else 0
            )
            for sp in model.slice
            if (t, c, sp, s) in model.mTechInpCommAggSlice
        )
        for t in model.tech
        if (t, c) in model.mTechInpCommAgg
    )
    + sum(
        (model.vTechAInp[t, c, r, y, s] if (t, c, r, y, s) in model.mvTechAInp else 0)
        for t in model.tech
        if (t, c) in model.mTechAInpCommSameSlice
    )
    + sum(
        sum(
            (
                model.vTechAInp[t, c, r, y, sp]
                if (t, c, r, y, sp) in model.mvTechAInp
                else 0
            )
            for sp in model.slice
            if (t, c, sp, s) in model.mTechAInpCommAggSlice
        )
        for t in model.tech
        if (t, c) in model.mTechAInpCommAgg
    ),
)
# eqTechOutTot(comm, region, year, slice)$mTechOutTot(comm, region, year, slice)
model.eqTechOutTot = Constraint(
    model.mTechOutTot,
    rule=lambda model, c, r, y, s: model.vTechOutTot[c, r, y, s]
    == sum(
        (model.vTechOut[t, c, r, y, s] if (t, c, r, y, s) in model.mvTechOut else 0)
        for t in model.tech
        if (t, c) in model.mTechOutCommSameSlice
    )
    + sum(
        sum(
            (
                model.vTechOut[t, c, r, y, sp]
                if (t, c, r, y, sp) in model.mvTechOut
                else 0
            )
            for sp in model.slice
            if (t, c, sp, s) in model.mTechOutCommAggSlice
        )
        for t in model.tech
        if (t, c) in model.mTechOutCommAgg
    )
    + sum(
        (model.vTechAOut[t, c, r, y, s] if (t, c, r, y, s) in model.mvTechAOut else 0)
        for t in model.tech
        if (t, c) in model.mTechAOutCommSameSlice
    )
    + sum(
        sum(
            (
                model.vTechAOut[t, c, r, y, sp]
                if (t, c, r, y, sp) in model.mvTechAOut
                else 0
            )
            for sp in model.slice
            if (t, c, sp, s) in model.mTechAOutCommAggSlice
        )
        for t in model.tech
        if (t, c) in model.mTechAOutCommAgg
    ),
)
# eqTechOutRY(tech, comm, region, year)$mTechOutRY(tech, comm, region, year)
model.eqTechOutRY = Constraint(
    model.mTechOutRY,
    rule=lambda model, t, c, r, y: model.vTechOutRY[t, c, r, y]
    == sum(
        model.pSliceWeight[y, s]
        * (model.vTechOut[t, c, r, y, s] if (t, c, r, y, s) in model.mvTechOut else 0)
        for s in model.slice
        if (c, r, y, s) in model.mTechOutTot
    ),
)
# eqStorageInpTot(comm, region, year, slice)$mStorageInpTot(comm, region, year, slice)
model.eqStorageInpTot = Constraint(
    model.mStorageInpTot,
    rule=lambda model, c, r, y, s: model.vStorageInpTot[c, r, y, s]
    == sum(
        model.vStorageInp[st1, c, r, y, s]
        for st1 in model.stg
        if (st1, c, r, y, s) in model.mvStorageStore
    )
    + sum(
        model.vStorageAInp[st1, c, r, y, s]
        for st1 in model.stg
        if (st1, c, r, y, s) in model.mvStorageAInp
    ),
)
# eqStorageOutTot(comm, region, year, slice)$mStorageOutTot(comm, region, year, slice)
model.eqStorageOutTot = Constraint(
    model.mStorageOutTot,
    rule=lambda model, c, r, y, s: model.vStorageOutTot[c, r, y, s]
    == sum(
        model.vStorageOut[st1, c, r, y, s]
        for st1 in model.stg
        if (st1, c, r, y, s) in model.mvStorageStore
    )
    + sum(
        model.vStorageAOut[st1, c, r, y, s]
        for st1 in model.stg
        if (st1, c, r, y, s) in model.mvStorageAOut
    ),
)
# eqDummyImportCost(comm, region, year)$mDummyImportCost(comm, region, year)
model.eqDummyImportCost = Constraint(
    model.mDummyImportCost,
    rule=lambda model, c, r, y: model.vDummyImportCost[c, r, y]
    == sum(
        model.pSliceWeight[y, s]
        * model.pDummyImportCost[c, r, y, s]
        * (model.vDummyImport[c, r, y, s] if (c, r, y, s) in model.mDummyImport else 0)
        for s in model.slice
        if (c, r, y, s) in model.mDummyImport
    ),
)
# eqDummyExportCost(comm, region, year)$mDummyExportCost(comm, region, year)
model.eqDummyExportCost = Constraint(
    model.mDummyExportCost,
    rule=lambda model, c, r, y: model.vDummyExportCost[c, r, y]
    == sum(
        model.pSliceWeight[y, s]
        * model.pDummyExportCost[c, r, y, s]
        * (model.vDummyExport[c, r, y, s] if (c, r, y, s) in model.mDummyExport else 0)
        for s in model.slice
        if (c, r, y, s) in model.mDummyExport
    ),
)
# eqTaxCost(comm, region, year)$mTaxCost(comm, region, year)
model.eqTaxCost = Constraint(
    model.mTaxCost,
    rule=lambda model, c, r, y: model.vTaxCost[c, r, y]
    == sum(
        model.pTaxCostOut[c, r, y, s]
        * model.pSliceWeight[y, s]
        * model.vOutTot[c, r, y, s]
        for s in model.slice
        if ((c, r, y, s) in model.mvOutTot and (c, s) in model.mCommSlice)
    )
    + sum(
        model.pTaxCostInp[c, r, y, s]
        * model.pSliceWeight[y, s]
        * model.vInpTot[c, r, y, s]
        for s in model.slice
        if ((c, r, y, s) in model.mvInpTot and (c, s) in model.mCommSlice)
    )
    + sum(
        model.pTaxCostBal[c, r, y, s]
        * model.pSliceWeight[y, s]
        * model.vBalance[c, r, y, s]
        for s in model.slice
        if ((c, r, y, s) in model.mvBalance and (c, s) in model.mCommSlice)
    ),
)
# eqSubsCost(comm, region, year)$mSubCost(comm, region, year)
model.eqSubsCost = Constraint(
    model.mSubCost,
    rule=lambda model, c, r, y: model.vSubsCost[c, r, y]
    == -sum(
        model.pSubCostOut[c, r, y, s]
        * model.pSliceWeight[y, s]
        * model.vOutTot[c, r, y, s]
        for s in model.slice
        if ((c, r, y, s) in model.mvOutTot and (c, s) in model.mCommSlice)
    )
    - sum(
        model.pSubCostInp[c, r, y, s]
        * model.pSliceWeight[y, s]
        * model.vInpTot[c, r, y, s]
        for s in model.slice
        if ((c, r, y, s) in model.mvInpTot and (c, s) in model.mCommSlice)
    )
    - sum(
        model.pSubCostBal[c, r, y, s]
        * model.pSliceWeight[y, s]
        * model.vBalance[c, r, y, s]
        for s in model.slice
        if ((c, r, y, s) in model.mvBalance and (c, s) in model.mCommSlice)
    ),
)
# eqCost(region, year)$mvTotalCost(region, year)
model.eqCost = Constraint(
    model.mvTotalCost,
    rule=lambda model, r, y: model.vTotalCost[r, y]
    == +sum(
        (model.vSupCost[s1, r, y] if (s1, r, y) in model.mvSupCost else 0)
        for s1 in model.sup
        if (s1, r, y) in model.mvSupCost
    )
    + sum(
        (model.vTechEac[t, r, y] if (t, r, y) in model.mTechEac else 0)
        for t in model.tech
        if (t, r, y) in model.mTechEac
    )
    + sum(
        (model.vTechRetCost[t, r, y] if (t, r, y) in model.mTechRetCost else 0)
        for t in model.tech
        if (t, r, y) in model.mTechRetCost
    )
    + sum(
        (model.vTechFixom[t, r, y] if (t, r, y) in model.mTechFixom else 0)
        for t in model.tech
        if (t, r, y) in model.mTechFixom
    )
    + sum(
        (model.vTechVarom[t, r, y] if (t, r, y) in model.mTechVarom else 0)
        for t in model.tech
        if (t, r, y) in model.mTechVarom
    )
    + sum(
        (model.vStorageEac[st1, r, y] if (st1, r, y) in model.mStorageEac else 0)
        for st1 in model.stg
        if (st1, r, y) in model.mStorageEac
    )
    + sum(
        (model.vStorageFixom[st1, r, y] if (st1, r, y) in model.mStorageFixom else 0)
        for st1 in model.stg
        if (st1, r, y) in model.mStorageFixom
    )
    + sum(
        (model.vStorageVarom[st1, r, y] if (st1, r, y) in model.mStorageVarom else 0)
        for st1 in model.stg
        if (st1, r, y) in model.mStorageVarom
    )
    + sum(
        (model.vImportRowCost[i, r, y] if (i, r, y) in model.mImportRowCost else 0)
        for i in model.imp
        if (i, r, y) in model.mImportRowCost
    )
    + sum(
        (model.vExportRowCost[e, r, y] if (e, r, y) in model.mExportRowCost else 0)
        for e in model.expp
        if (e, r, y) in model.mExportRowCost
    )
    + sum(
        (model.vTradeEac[t1, r, y] if (t1, r, y) in model.mTradeEac else 0)
        for t1 in model.trade
        if (t1, r, y) in model.mTradeEac
    )
    + sum(
        (model.vTradeFixom[t1, r, y] if (t1, r, y) in model.mTradeFixom else 0)
        for t1 in model.trade
        if (t1, r, y) in model.mTradeFixom
    )
    + sum(
        (model.vImportIrCost[t1, r, y] if (t1, r, y) in model.mImportIrCost else 0)
        for t1 in model.trade
        if (t1, r, y) in model.mImportIrCost
    )
    + sum(
        (model.vExportIrCost[t1, r, y] if (t1, r, y) in model.mExportIrCost else 0)
        for t1 in model.trade
        if (t1, r, y) in model.mExportIrCost
    )
    + sum(
        (model.vTaxCost[c, r, y] if (c, r, y) in model.mTaxCost else 0)
        for c in model.comm
        if (c, r, y) in model.mTaxCost
    )
    + sum(
        (model.vSubsCost[c, r, y] if (c, r, y) in model.mSubCost else 0)
        for c in model.comm
        if (c, r, y) in model.mSubCost
    )
    + (model.vTotalUserCosts[r, y] if (r, y) in model.mvTotalUserCosts else 0)
    + sum(
        (model.vDummyImportCost[c, r, y] if (c, r, y) in model.mDummyImportCost else 0)
        for c in model.comm
        if (c, r, y) in model.mDummyImportCost
    )
    + sum(
        (model.vDummyExportCost[c, r, y] if (c, r, y) in model.mDummyExportCost else 0)
        for c in model.comm
        if (c, r, y) in model.mDummyExportCost
    ),
)
# eqObjective
model.eqObjective = Constraint(
    rule=lambda model: model.vObjective
    == sum(
        model.vTotalCost[r, y] * model.pPeriodLen[y] * model.pDiscountFactor[r, y]
        for r in model.region
        for y in model.year
        if (r, y) in model.mvTotalCost
    )
)
# eqLECActivity(tech, region, year)$meqLECActivity(tech, region, year)
model.eqLECActivity = Constraint(
    model.meqLECActivity,
    rule=lambda model, t, r, y: sum(
        model.vTechAct[t, r, y, s] for s in model.slice if (t, s) in model.mTechSlice
    )
    >= model.pLECLoACT[r],
)
model.fornontriv = Var(domain=pyo.NonNegativeReals)
model.eqnontriv = Constraint(rule=lambda model: model.fornontriv == 0)
exec(open("inc_constraints.py").read())
exec(open("inc_costs.py").read())
model.obj = Objective(rule=lambda model: model.vObjective, sense=minimize)


print("model.create_instance begin ", round(time.time() - seconds, 2))
exec(open("inc2.py").read())
flog.write('"load data",,"' + str(datetime.datetime.now()) + '"\n')
instance = model.create_instance("data.dat")
print("model.create_instance end ", round(time.time() - seconds, 2))

exec(open("inc_solver.py").read())
# opt = SolverFactory('cplex');
flog.write('"solver",,"' + str(datetime.datetime.now()) + '"\n')
exec(open("inc3.py").read())
slv = opt.solve(instance)
exec(open("inc4.py").read())
print("opt solve ", round(time.time() - seconds, 2))
flog.write(
    '"solution status",'
    + str((slv.solver.status == SolverStatus.ok) * 1)
    + ',"'
    + str(datetime.datetime.now())
    + '"\n'
)
flog.write('"export results",,"' + str(datetime.datetime.now()) + '"\n')
exec(open("output.py").read())
flog.write('"done",,"' + str(datetime.datetime.now()) + '"\n')
flog.close()
exec(open("inc5.py").read())
