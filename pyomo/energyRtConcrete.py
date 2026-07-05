verbose = True
import datetime, sys

print("start time: " + str(datetime.datetime.now().strftime("%H:%M:%S")) + "\n")
flog = open("output/log.csv", "w")
flog.write("parameter,value,time\n")
flog.write(
    '"model language",PyomoConcrete,"'
    + str(datetime.datetime.now().strftime("%H:%M:%S"))
    + '"\n'
)
flog.write('"load data",,"' + str(datetime.datetime.now().strftime("%H:%M:%S")) + '"\n')
import time

seconds = time.time()
import itertools
from pyomo.environ import *
from pyomo.opt import SolverFactory
import pyomo.environ as pyo


class toPar:
    def __init__(self, val, default):
        self.default = default
        self.val = val

    def get(self, key):
        if key in self.val:
            return self.val[key]
        return self.default


exec(open("inc1.py").read())
model = ConcreteModel()
import pandas as pd

# Data source: "sqlite" (input/data.db) or "arrow" (input/<name>.arrow). The R
# writer (energyRt) rewrites this line at write time based on export_format.
_DATA_FORMAT = "sqlite"
if _DATA_FORMAT == "sqlite":
    import sqlite3

    _con = sqlite3.connect("input/data.db")

    def _read_tbl(name):
        return pd.read_sql_query(f'SELECT * FROM "{name}"', _con)

else:
    import pyarrow.feather as _feather

    def _read_tbl(name):
        return _feather.read_feather("input/" + name + ".arrow")


def read_set(name):
    tbl = _read_tbl(name)
    if tbl.shape[1] > 1:
        return tbl.to_records(index=False).tolist()
    else:
        return list(tbl.iloc[:, 0])


def read_dict(name):
    tbl = _read_tbl(name)
    if tbl.shape[1] > 2:
        idx = pd.MultiIndex.from_frame(tbl.drop(columns="value"))
    else:
        idx = tbl.iloc[:, 0].tolist()
    tbl = pd.DataFrame(tbl.value.tolist(), index=idx, columns=["value"])
    return tbl.to_dict()["value"]


print("loading model parameters...")
##### decl par #####
sys.stdout.flush()
print("Building Pyomo model")
print(
    "variables... "
    + str(datetime.datetime.now().strftime("%H:%M:%S"))
    + " ("
    + str(round(time.time() - seconds, 2))
    + " s)"
)
model.vTechInv = Var(mTechInv, doc="Overnight investment costs")
model.vTechEac = Var(mTechEac, doc="Annualized investment costs")
model.vTechRetCost = Var(mTechRetCost, doc="Early retirement costs")
model.vTechFixom = Var(mTechFixom, doc="Fixed O&M costs")
model.vTechVarom = Var(
    mTechVarom, doc="Variable O&M costs (AVarom + CVarom + ActVarom)"
)
model.vSupCost = Var(mvSupCost, doc="Supply costs (weighted)")
model.vEmsFuelTot = Var(
    mEmsFuelTot, doc="Total emissions from fuels combustion (technologies)"
)
model.vBalance = Var(mvBalance, doc="Net commodity balance (all sources)")
model.vTotalCost = Var(mvTotalCost, doc="Regional annual total costs (weighted)")
model.vObjective = Var(doc="Objective costs")
model.vTaxCost = Var(mTaxCost, doc="Total tax levies (tax costs)")
model.vSubsCost = Var(mSubCost, doc="Total subsidies (substracted from costs)")
model.vAggOutTot = Var(mAggOut, doc="Aggregated commodity output (weighted)")
model.vDummyImportCost = Var(mDummyImportCost, doc="Dummy import costs (weighted)")
model.vDummyExportCost = Var(mDummyExportCost, doc="Dummy export costs (weighted)")
model.vStorageFixom = Var(mStorageFixom, doc="Storage fixed O&M costs")
model.vStorageVarom = Var(mStorageVarom, doc="Storage variable O&M costs")
model.vTradeEac = Var(
    mTradeEac, doc="Annualized investments in Interregional trade capacity"
)
model.vTradeFixom = Var(mTradeFixom, doc="Interregional trade fixed O&M costs")
model.vImportIrCost = Var(mImportIrCost, doc="Import costs from other regions")
model.vExportIrCost = Var(
    mExportIrCost, doc="Credits (revenue) for export to other regions"
)
model.vImportRowCost = Var(mImportRowCost, doc="Import costs from the ROW")
model.vExportRowCost = Var(mExportRowCost, doc="Credits for export to the ROW")
model.vTechNewCap = Var(mTechNew, domain=pyo.NonNegativeReals, doc="New capacity")
model.vTechRetiredStockCum = Var(
    mvTechRetiredStock, domain=pyo.NonNegativeReals, doc="Early retired stock"
)
model.vTechRetiredStock = Var(
    mvTechRetiredStock, domain=pyo.NonNegativeReals, doc="Early retired stock"
)
model.vTechRetiredNewCap = Var(
    mvTechRetiredNewCap, domain=pyo.NonNegativeReals, doc="Early retired new capacity"
)
model.vTechCap = Var(
    mTechSpan, domain=pyo.NonNegativeReals, doc="Total capacity of the technology"
)
model.vTechAct = Var(
    mvTechAct, domain=pyo.NonNegativeReals, doc="Activity level of technology"
)
model.vTechInp = Var(mvTechInp, domain=pyo.NonNegativeReals, doc="Input level")
model.vTechOut = Var(
    mvTechOut,
    domain=pyo.NonNegativeReals,
    doc="Commodity output from technology - tech timeframe",
)
model.vTechAInp = Var(
    mvTechAInp, domain=pyo.NonNegativeReals, doc="Auxiliary commodity input"
)
model.vTechAOut = Var(
    mvTechAOut, domain=pyo.NonNegativeReals, doc="Auxiliary commodity output"
)
model.vSupOut = Var(mSupAva, domain=pyo.NonNegativeReals, doc="Output of supply")
model.vSupReserve = Var(
    mvSupReserve, domain=pyo.NonNegativeReals, doc="Cumulative supply (weighted)"
)
model.vDemInp = Var(mvDemInp, domain=pyo.NonNegativeReals, doc="Input to demand")
model.vOutTot = Var(
    mvOutTot,
    domain=pyo.NonNegativeReals,
    doc="Total commodity output (all processes) (weighted)",
)
model.vInpTot = Var(
    mvInpTot,
    domain=pyo.NonNegativeReals,
    doc="Total commodity input (all processes) (weighted)",
)
# [agg-rewrite] vInp2Lo/vOut2Lo retired (up-aggregation in eqInpTot/eqOutTot)
model.vSupOutTot = Var(
    mSupOutTot, domain=pyo.NonNegativeReals, doc="Total commodity supply (weighted)"
)
model.vTechInpTot = Var(
    mTechInpTot,
    domain=pyo.NonNegativeReals,
    doc="Total commodity (main & aux) input to technologies (weighted)",
)
model.vTechOutTot = Var(
    mTechOutTot,
    domain=pyo.NonNegativeReals,
    doc="Total commodity (main & aux) output from technologies (weighted)",
)
model.vStorageInpTot = Var(
    mStorageInpTot,
    domain=pyo.NonNegativeReals,
    doc="Total commodity (main & aux) input to storage (weighted)",
)
model.vStorageOutTot = Var(
    mStorageOutTot,
    domain=pyo.NonNegativeReals,
    doc="Total commodity (main & aux) output from storage (weighted)",
)
model.vStorageAInp = Var(
    mvStorageAInp, domain=pyo.NonNegativeReals, doc="Aux-commodity input to storage"
)
model.vStorageAOut = Var(
    mvStorageAOut, domain=pyo.NonNegativeReals, doc="Aux-commodity input from storage"
)
model.vDummyImport = Var(
    mDummyImport, domain=pyo.NonNegativeReals, doc="Dummy import (for debugging)"
)
model.vDummyExport = Var(
    mDummyExport, domain=pyo.NonNegativeReals, doc="Dummy export (for debugging)"
)
model.vStorageInp = Var(
    mvStorageStore, domain=pyo.NonNegativeReals, doc="Storage input"
)
model.vStorageOut = Var(
    mvStorageStore, domain=pyo.NonNegativeReals, doc="Storage output"
)
model.vStorageStore = Var(
    mvStorageStore, domain=pyo.NonNegativeReals, doc="Storage level"
)
model.vStorageInv = Var(
    mStorageNew, domain=pyo.NonNegativeReals, doc="Storage investments"
)
model.vStorageEac = Var(
    mStorageEac, domain=pyo.NonNegativeReals, doc="Storage EAC investments"
)
model.vStorageCap = Var(
    mStorageSpan, domain=pyo.NonNegativeReals, doc="Storage capacity"
)
model.vStorageNewCap = Var(
    mStorageNew, domain=pyo.NonNegativeReals, doc="Storage new capacity"
)
model.vImportTot = Var(
    mImport,
    domain=pyo.NonNegativeReals,
    doc="Total regional import (Ir + ROW) (weighted)",
)
model.vExportTot = Var(
    mExport,
    domain=pyo.NonNegativeReals,
    doc="Total regional export (Ir + ROW) (weighted)",
)
model.vTradeIr = Var(
    mvTradeIr,
    domain=pyo.NonNegativeReals,
    doc="Total physical trade flows between regions",
)
model.vTradeIrAInp = Var(
    mvTradeIrAInp, domain=pyo.NonNegativeReals, doc="Trade auxilari input"
)
model.vTradeIrAInpTot = Var(
    mvTradeIrAInpTot,
    domain=pyo.NonNegativeReals,
    doc="Trade total auxilari input (weighted)",
)
model.vTradeIrAOut = Var(
    mvTradeIrAOut, domain=pyo.NonNegativeReals, doc="Trade auxilari output"
)
model.vTradeIrAOutTot = Var(
    mvTradeIrAOutTot,
    domain=pyo.NonNegativeReals,
    doc="Trade auxilari output total (weighted)",
)
model.vExportRowCum = Var(
    mExpComm, domain=pyo.NonNegativeReals, doc="Cumulative export to the ROW"
)
model.vExportRow = Var(mExportRow, domain=pyo.NonNegativeReals, doc="Export to the ROW")
model.vImportRowCum = Var(
    mImpComm, domain=pyo.NonNegativeReals, doc="Cumulative import from the ROW"
)
model.vImportRow = Var(
    mImportRow, domain=pyo.NonNegativeReals, doc="Import from the ROW"
)
model.vTradeCap = Var(mTradeSpan, domain=pyo.NonNegativeReals, doc="Trade capacity")
model.vTradeInv = Var(
    mTradeEac,
    domain=pyo.NonNegativeReals,
    doc="Investment in trade capacity (overnight)",
)
model.vTradeNewCap = Var(
    mTradeNew, domain=pyo.NonNegativeReals, doc="New trade capacity"
)
model.vTotalUserCosts = Var(
    mvTotalUserCosts,
    domain=pyo.NonNegativeReals,
    doc="Total additional costs (set by user)",
)
exec(open("inc2.py").read())
print(
    "equations... "
    + str(datetime.datetime.now().strftime("%H:%M:%S"))
    + " ("
    + str(round(time.time() - seconds, 2))
    + " s)"
)
# eqTechSng2Sng(tech, region, comm, commp, year, slice)$meqTechSng2Sng(tech, region, comm, commp, year, slice)
if verbose:
    print("eqTechSng2Sng ", end="")
sys.stdout.flush()
model.eqTechSng2Sng = Constraint(
    meqTechSng2Sng,
    rule=lambda model, t, r, c, cp, y, s: model.vTechInp[t, c, r, y, s]
    * pTechCinp2use.get((t, c, r, y, s))
    == (model.vTechOut[t, cp, r, y, s])
    / (pTechUse2cact.get((t, cp, r, y, s)) * pTechCact2cout.get((t, cp, r, y, s))),
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqTechGrp2Sng(tech, region, group, commp, year, slice)$meqTechGrp2Sng(tech, region, group, commp, year, slice)
if verbose:
    print("eqTechGrp2Sng ", end="")
sys.stdout.flush()
model.eqTechGrp2Sng = Constraint(
    meqTechGrp2Sng,
    rule=lambda model, t, r, g, cp, y, s: pTechGinp2use.get((t, g, r, y, s))
    * sum(
        (
            (model.vTechInp[t, c, r, y, s] * pTechCinp2ginp.get((t, c, r, y, s)))
            if (t, c, r, y, s) in mvTechInp
            else 0
        )
        for c in comm
        if (t, g, c) in mTechGroupComm
    )
    == (model.vTechOut[t, cp, r, y, s])
    / (pTechUse2cact.get((t, cp, r, y, s)) * pTechCact2cout.get((t, cp, r, y, s))),
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqTechSng2Grp(tech, region, comm, groupp, year, slice)$meqTechSng2Grp(tech, region, comm, groupp, year, slice)
if verbose:
    print("eqTechSng2Grp ", end="")
sys.stdout.flush()
model.eqTechSng2Grp = Constraint(
    meqTechSng2Grp,
    rule=lambda model, t, r, c, gp, y, s: model.vTechInp[t, c, r, y, s]
    * pTechCinp2use.get((t, c, r, y, s))
    == sum(
        (
            (
                (model.vTechOut[t, cp, r, y, s])
                / (
                    pTechUse2cact.get((t, cp, r, y, s))
                    * pTechCact2cout.get((t, cp, r, y, s))
                )
            )
            if (t, cp, r, y, s) in mvTechOut
            else 0
        )
        for cp in comm
        if (t, gp, cp) in mTechGroupComm
    ),
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqTechGrp2Grp(tech, region, group, groupp, year, slice)$meqTechGrp2Grp(tech, region, group, groupp, year, slice)
if verbose:
    print("eqTechGrp2Grp ", end="")
sys.stdout.flush()
model.eqTechGrp2Grp = Constraint(
    meqTechGrp2Grp,
    rule=lambda model, t, r, g, gp, y, s: pTechGinp2use.get((t, g, r, y, s))
    * sum(
        (
            (model.vTechInp[t, c, r, y, s] * pTechCinp2ginp.get((t, c, r, y, s)))
            if (t, c, r, y, s) in mvTechInp
            else 0
        )
        for c in comm
        if (t, g, c) in mTechGroupComm
    )
    == sum(
        (
            (
                (model.vTechOut[t, cp, r, y, s])
                / (
                    pTechUse2cact.get((t, cp, r, y, s))
                    * pTechCact2cout.get((t, cp, r, y, s))
                )
            )
            if (t, cp, r, y, s) in mvTechOut
            else 0
        )
        for cp in comm
        if (t, gp, cp) in mTechGroupComm
    ),
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqTechShareInpLo(tech, region, group, comm, year, slice)$meqTechShareInpLo(tech, region, group, comm, year, slice)
if verbose:
    print("eqTechShareInpLo ", end="")
sys.stdout.flush()
model.eqTechShareInpLo = Constraint(
    meqTechShareInpLo,
    rule=lambda model, t, r, g, c, y, s: model.vTechInp[t, c, r, y, s]
    >= pTechShareLo.get((t, c, r, y, s))
    * sum(
        (model.vTechInp[t, cp, r, y, s] if (t, cp, r, y, s) in mvTechInp else 0)
        for cp in comm
        if (t, g, cp) in mTechGroupComm
    ),
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqTechShareInpUp(tech, region, group, comm, year, slice)$meqTechShareInpUp(tech, region, group, comm, year, slice)
if verbose:
    print("eqTechShareInpUp ", end="")
sys.stdout.flush()
model.eqTechShareInpUp = Constraint(
    meqTechShareInpUp,
    rule=lambda model, t, r, g, c, y, s: model.vTechInp[t, c, r, y, s]
    <= pTechShareUp.get((t, c, r, y, s))
    * sum(
        (model.vTechInp[t, cp, r, y, s] if (t, cp, r, y, s) in mvTechInp else 0)
        for cp in comm
        if (t, g, cp) in mTechGroupComm
    ),
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqTechShareOutLo(tech, region, group, comm, year, slice)$meqTechShareOutLo(tech, region, group, comm, year, slice)
if verbose:
    print("eqTechShareOutLo ", end="")
sys.stdout.flush()
model.eqTechShareOutLo = Constraint(
    meqTechShareOutLo,
    rule=lambda model, t, r, g, c, y, s: model.vTechOut[t, c, r, y, s]
    >= pTechShareLo.get((t, c, r, y, s))
    * sum(
        (model.vTechOut[t, cp, r, y, s] if (t, cp, r, y, s) in mvTechOut else 0)
        for cp in comm
        if (t, g, cp) in mTechGroupComm
    ),
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqTechShareOutUp(tech, region, group, comm, year, slice)$meqTechShareOutUp(tech, region, group, comm, year, slice)
if verbose:
    print("eqTechShareOutUp ", end="")
sys.stdout.flush()
model.eqTechShareOutUp = Constraint(
    meqTechShareOutUp,
    rule=lambda model, t, r, g, c, y, s: model.vTechOut[t, c, r, y, s]
    <= pTechShareUp.get((t, c, r, y, s))
    * sum(
        (model.vTechOut[t, cp, r, y, s] if (t, cp, r, y, s) in mvTechOut else 0)
        for cp in comm
        if (t, g, cp) in mTechGroupComm
    ),
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqTechAInp(tech, comm, region, year, slice)$mvTechAInp(tech, comm, region, year, slice)
if verbose:
    print("eqTechAInp ", end="")
sys.stdout.flush()
model.eqTechAInp = Constraint(
    mvTechAInp,
    rule=lambda model, t, c, r, y, s: model.vTechAInp[t, c, r, y, s]
    == (
        (model.vTechAct[t, r, y, s] * pTechAct2AInp.get((t, c, r, y, s)))
        if (t, c, r, y, s) in mTechAct2AInp
        else 0
    )
    + (
        (
            (model.vTechCap[t, r, y] * pTechCap2AInp.get((t, c, r, y, s)))
            / (pTechCap2act.get((t)))
        )
        if (t, c, r, y, s) in mTechCap2AInp
        else 0
    )
    + (
        (model.vTechNewCap[t, r, y] * pTechNCap2AInp.get((t, c, r, y, s)))
        if (t, c, r, y, s) in mTechNCap2AInp
        else 0
    )
    + sum(
        pTechCinp2AInp.get((t, c, cp, r, y, s)) * model.vTechInp[t, cp, r, y, s]
        for cp in comm
        if (t, c, cp, r, y, s) in mTechCinp2AInp
    )
    + sum(
        pTechCout2AInp.get((t, c, cp, r, y, s)) * model.vTechOut[t, cp, r, y, s]
        for cp in comm
        if (t, c, cp, r, y, s) in mTechCout2AInp
    ),
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqTechAOut(tech, comm, region, year, slice)$mvTechAOut(tech, comm, region, year, slice)
if verbose:
    print("eqTechAOut ", end="")
sys.stdout.flush()
model.eqTechAOut = Constraint(
    mvTechAOut,
    rule=lambda model, t, c, r, y, s: model.vTechAOut[t, c, r, y, s]
    == (
        (model.vTechAct[t, r, y, s] * pTechAct2AOut.get((t, c, r, y, s)))
        if (t, c, r, y, s) in mTechAct2AOut
        else 0
    )
    + (
        (
            (model.vTechCap[t, r, y] * pTechCap2AOut.get((t, c, r, y, s)))
            / (pTechCap2act.get((t)))
        )
        if (t, c, r, y, s) in mTechCap2AOut
        else 0
    )
    + (
        (model.vTechNewCap[t, r, y] * pTechNCap2AOut.get((t, c, r, y, s)))
        if (t, c, r, y, s) in mTechNCap2AOut
        else 0
    )
    + sum(
        pTechCinp2AOut.get((t, c, cp, r, y, s)) * model.vTechInp[t, cp, r, y, s]
        for cp in comm
        if (t, c, cp, r, y, s) in mTechCinp2AOut
    )
    + sum(
        pTechCout2AOut.get((t, c, cp, r, y, s)) * model.vTechOut[t, cp, r, y, s]
        for cp in comm
        if (t, c, cp, r, y, s) in mTechCout2AOut
    ),
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqTechAfLo(tech, region, year, slice)$meqTechAfLo(tech, region, year, slice)
if verbose:
    print("eqTechAfLo ", end="")
sys.stdout.flush()
model.eqTechAfLo = Constraint(
    meqTechAfLo,
    rule=lambda model, t, r, y, s: pTechAfLo.get((t, r, y, s))
    * pTechCap2act.get((t))
    * model.vTechCap[t, r, y]
    * pSliceShare.get((s))
    * prod(
        pTechWeatherAfLo.get((wth1, t)) * pWeather.get((wth1, r, y, s))
        for wth1 in weather
        if (wth1, t) in mTechWeatherAfLo
    )
    <= model.vTechAct[t, r, y, s],
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqTechAfUp(tech, region, year, slice)$meqTechAfUp(tech, region, year, slice)
if verbose:
    print("eqTechAfUp ", end="")
sys.stdout.flush()
model.eqTechAfUp = Constraint(
    meqTechAfUp,
    rule=lambda model, t, r, y, s: model.vTechAct[t, r, y, s]
    <= pTechAfUp.get((t, r, y, s))
    * pTechCap2act.get((t))
    * model.vTechCap[t, r, y]
    * pSliceShare.get((s))
    * prod(
        pTechWeatherAfUp.get((wth1, t)) * pWeather.get((wth1, r, y, s))
        for wth1 in weather
        if (wth1, t) in mTechWeatherAfUp
    ),
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqTechAfsLo(tech, region, year, slice)$meqTechAfsLo(tech, region, year, slice)
if verbose:
    print("eqTechAfsLo ", end="")
sys.stdout.flush()
model.eqTechAfsLo = Constraint(
    meqTechAfsLo,
    rule=lambda model, t, r, y, s: pTechAfsLo.get((t, r, y, s))
    * pTechCap2act.get((t))
    * model.vTechCap[t, r, y]
    * pSliceShare.get((s))
    * prod(
        pTechWeatherAfsLo.get((wth1, t)) * pWeather.get((wth1, r, y, s))
        for wth1 in weather
        if (wth1, t) in mTechWeatherAfsLo
    )
    <= sum(
        (model.vTechAct[t, r, y, sp] if (t, r, y, sp) in mvTechAct else 0)
        for sp in slice
        if (s, sp) in mSliceParentChildE
    ),
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqTechAfsUp(tech, region, year, slice)$meqTechAfsUp(tech, region, year, slice)
if verbose:
    print("eqTechAfsUp ", end="")
sys.stdout.flush()
model.eqTechAfsUp = Constraint(
    meqTechAfsUp,
    rule=lambda model, t, r, y, s: sum(
        (model.vTechAct[t, r, y, sp] if (t, r, y, sp) in mvTechAct else 0)
        for sp in slice
        if (s, sp) in mSliceParentChildE
    )
    <= pTechAfsUp.get((t, r, y, s))
    * pTechCap2act.get((t))
    * model.vTechCap[t, r, y]
    * pSliceShare.get((s))
    * prod(
        pTechWeatherAfsUp.get((wth1, t)) * pWeather.get((wth1, r, y, s))
        for wth1 in weather
        if (wth1, t) in mTechWeatherAfsUp
    ),
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqTechRampUp(tech, region, year, slice, slicep)$mTechRampUp(tech, region, year, slice, slicep)
if verbose:
    print("eqTechRampUp ", end="")
sys.stdout.flush()
model.eqTechRampUp = Constraint(
    mTechRampUp,
    rule=lambda model, t, r, y, s, sp: (model.vTechAct[t, r, y, s])
    / (pSliceShare.get((s)))
    - (model.vTechAct[t, r, y, sp]) / (pSliceShare.get((sp)))
    <= (
        pSliceShare.get((s))
        * pTechCap2act.get((t))
        * pTechCap2act.get((t))
        * model.vTechCap[t, r, y]
    )
    / (pTechRampUp.get((t, r, y, s))),
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqTechRampDown(tech, region, year, slice, slicep)$mTechRampDown(tech, region, year, slice, slicep)
if verbose:
    print("eqTechRampDown ", end="")
sys.stdout.flush()
model.eqTechRampDown = Constraint(
    mTechRampDown,
    rule=lambda model, t, r, y, s, sp: (model.vTechAct[t, r, y, sp])
    / (pSliceShare.get((sp)))
    - (model.vTechAct[t, r, y, s]) / (pSliceShare.get((s)))
    <= (
        pSliceShare.get((s))
        * pTechCap2act.get((t))
        * pTechCap2act.get((t))
        * model.vTechCap[t, r, y]
    )
    / (pTechRampDown.get((t, r, y, s))),
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqTechActSng(tech, comm, region, year, slice)$meqTechActSng(tech, comm, region, year, slice)
if verbose:
    print("eqTechActSng ", end="")
sys.stdout.flush()
model.eqTechActSng = Constraint(
    meqTechActSng,
    rule=lambda model, t, c, r, y, s: model.vTechAct[t, r, y, s]
    == (model.vTechOut[t, c, r, y, s]) / (pTechCact2cout.get((t, c, r, y, s))),
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqTechActGrp(tech, group, region, year, slice)$meqTechActGrp(tech, group, region, year, slice)
if verbose:
    print("eqTechActGrp ", end="")
sys.stdout.flush()
model.eqTechActGrp = Constraint(
    meqTechActGrp,
    rule=lambda model, t, g, r, y, s: model.vTechAct[t, r, y, s]
    == sum(
        (
            ((model.vTechOut[t, c, r, y, s]) / (pTechCact2cout.get((t, c, r, y, s))))
            if (t, c, r, y, s) in mvTechOut
            else 0
        )
        for c in comm
        if (t, g, c) in mTechGroupComm
    ),
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqTechAfcOutLo(tech, region, comm, year, slice)$meqTechAfcOutLo(tech, region, comm, year, slice)
if verbose:
    print("eqTechAfcOutLo ", end="")
sys.stdout.flush()
model.eqTechAfcOutLo = Constraint(
    meqTechAfcOutLo,
    rule=lambda model, t, r, c, y, s: pTechCact2cout.get((t, c, r, y, s))
    * pTechAfcLo.get((t, c, r, y, s))
    * pTechCap2act.get((t))
    * model.vTechCap[t, r, y]
    * pSliceShare.get((s))
    * prod(
        pTechWeatherAfcLo.get((wth1, t, c)) * pWeather.get((wth1, r, y, s))
        for wth1 in weather
        if (wth1, t, c) in mTechWeatherAfcLo
    )
    <= model.vTechOut[t, c, r, y, s],
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqTechAfcOutUp(tech, region, comm, year, slice)$meqTechAfcOutUp(tech, region, comm, year, slice)
if verbose:
    print("eqTechAfcOutUp ", end="")
sys.stdout.flush()
model.eqTechAfcOutUp = Constraint(
    meqTechAfcOutUp,
    rule=lambda model, t, r, c, y, s: model.vTechOut[t, c, r, y, s]
    <= pTechCact2cout.get((t, c, r, y, s))
    * pTechAfcUp.get((t, c, r, y, s))
    * pTechCap2act.get((t))
    * model.vTechCap[t, r, y]
    * prod(
        pTechWeatherAfcUp.get((wth1, t, c)) * pWeather.get((wth1, r, y, s))
        for wth1 in weather
        if (wth1, t, c) in mTechWeatherAfcUp
    ),
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqTechAfcInpLo(tech, region, comm, year, slice)$meqTechAfcInpLo(tech, region, comm, year, slice)
if verbose:
    print("eqTechAfcInpLo ", end="")
sys.stdout.flush()
model.eqTechAfcInpLo = Constraint(
    meqTechAfcInpLo,
    rule=lambda model, t, r, c, y, s: pTechAfcLo.get((t, c, r, y, s))
    * pTechCap2act.get((t))
    * model.vTechCap[t, r, y]
    * pSliceShare.get((s))
    * prod(
        pTechWeatherAfcLo.get((wth1, t, c)) * pWeather.get((wth1, r, y, s))
        for wth1 in weather
        if (wth1, t, c) in mTechWeatherAfcLo
    )
    <= model.vTechInp[t, c, r, y, s],
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqTechAfcInpUp(tech, region, comm, year, slice)$meqTechAfcInpUp(tech, region, comm, year, slice)
if verbose:
    print("eqTechAfcInpUp ", end="")
sys.stdout.flush()
model.eqTechAfcInpUp = Constraint(
    meqTechAfcInpUp,
    rule=lambda model, t, r, c, y, s: model.vTechInp[t, c, r, y, s]
    <= pTechAfcUp.get((t, c, r, y, s))
    * pTechCap2act.get((t))
    * model.vTechCap[t, r, y]
    * pSliceShare.get((s))
    * prod(
        pTechWeatherAfcUp.get((wth1, t, c)) * pWeather.get((wth1, r, y, s))
        for wth1 in weather
        if (wth1, t, c) in mTechWeatherAfcUp
    ),
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqTechCap(tech, region, year)$mTechSpan(tech, region, year)
if verbose:
    print("eqTechCap ", end="")
sys.stdout.flush()
model.eqTechCap = Constraint(
    mTechSpan,
    rule=lambda model, t, r, y: model.vTechCap[t, r, y]
    == pTechStock.get((t, r, y))
    - (model.vTechRetiredStockCum[t, r, y] if (t, r, y) in mvTechRetiredStock else 0)
    + sum(
        pPeriodLen.get((yp))
        * (
            model.vTechNewCap[t, r, yp]
            - sum(
                model.vTechRetiredNewCap[t, r, yp, ye]
                for ye in year
                if (
                    (t, r, yp, ye) in mvTechRetiredNewCap
                    and ordYear.get((y)) >= ordYear.get((ye))
                )
            )
        )
        for yp in year
        if (
            (t, r, yp) in mTechNew
            and ordYear.get((y)) >= ordYear.get((yp))
            and (
                ordYear.get((y)) < pTechOlife.get((t, r)) + ordYear.get((yp))
                or (t, r) in mTechOlifeInf
            )
        )
    ),
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqTechCapLo(tech, region, year)$mTechCapLo(tech, region, year)
if verbose:
    print("eqTechCapLo ", end="")
sys.stdout.flush()
model.eqTechCapLo = Constraint(
    mTechCapLo,
    rule=lambda model, t, r, y: model.vTechCap[t, r, y] >= pTechCapLo.get((t, r, y)),
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqTechCapUp(tech, region, year)$mTechCapUp(tech, region, year)
if verbose:
    print("eqTechCapUp ", end="")
sys.stdout.flush()
model.eqTechCapUp = Constraint(
    mTechCapUp,
    rule=lambda model, t, r, y: model.vTechCap[t, r, y] <= pTechCapUp.get((t, r, y)),
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqTechNewCapLo(tech, region, year)$mTechNewCapLo(tech, region, year)
if verbose:
    print("eqTechNewCapLo ", end="")
sys.stdout.flush()
model.eqTechNewCapLo = Constraint(
    mTechNewCapLo,
    rule=lambda model, t, r, y: model.vTechNewCap[t, r, y]
    >= pTechNewCapLo.get((t, r, y)) * pPeriodLen.get((y)),
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqTechNewCapUp(tech, region, year)$mTechNewCapUp(tech, region, year)
if verbose:
    print("eqTechNewCapUp ", end="")
sys.stdout.flush()
model.eqTechNewCapUp = Constraint(
    mTechNewCapUp,
    rule=lambda model, t, r, y: model.vTechNewCap[t, r, y]
    <= pTechNewCapUp.get((t, r, y)) * pPeriodLen.get((y)),
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqTechRetiredNewCap(tech, region, year)$meqTechRetiredNewCap(tech, region, year)
if verbose:
    print("eqTechRetiredNewCap ", end="")
sys.stdout.flush()
model.eqTechRetiredNewCap = Constraint(
    meqTechRetiredNewCap,
    rule=lambda model, t, r, y: sum(
        model.vTechRetiredNewCap[t, r, y, yp] * pPeriodLen.get((yp))
        for yp in year
        if (t, r, y, yp) in mvTechRetiredNewCap
    )
    <= model.vTechNewCap[t, r, y] * pPeriodLen.get((y)),
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqTechRetiredStockCum(tech, region, year)$mvTechRetiredStock(tech, region, year)
if verbose:
    print("eqTechRetiredStockCum ", end="")
sys.stdout.flush()
model.eqTechRetiredStockCum = Constraint(
    mvTechRetiredStock,
    rule=lambda model, t, r, y: model.vTechRetiredStockCum[t, r, y]
    <= pTechStock.get((t, r, y)),
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqTechRetiredStock(tech, region, year)$mvTechRetiredStock(tech, region, year)
if verbose:
    print("eqTechRetiredStock ", end="")
sys.stdout.flush()
model.eqTechRetiredStock = Constraint(
    mvTechRetiredStock,
    rule=lambda model, t, r, y: model.vTechRetiredStock[t, r, y] * pPeriodLen.get((y))
    == model.vTechRetiredStockCum[t, r, y]
    - sum(
        model.vTechRetiredStockCum[t, r, yp] for yp in year if (yp, y) in mMilestoneNext
    ),
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqTechRetUp(tech, region, year)$mTechRetUp(tech, region, year)
if verbose:
    print("eqTechRetUp ", end="")
sys.stdout.flush()
model.eqTechRetUp = Constraint(
    mTechRetUp,
    rule=lambda model, t, r, y: (
        model.vTechRetiredStock[t, r, y] if (t, r, y) in mvTechRetiredStock else 0
    )
    + sum(
        model.vTechRetiredNewCap[t, r, y, yp]
        for yp in year
        if (t, r, y, yp) in mvTechRetiredNewCap
    )
    <= pTechRetUp.get((t, r, y)) * pPeriodLen.get((y)),
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqTechRetLo(tech, region, year)$mTechRetLo(tech, region, year)
if verbose:
    print("eqTechRetLo ", end="")
sys.stdout.flush()
model.eqTechRetLo = Constraint(
    mTechRetLo,
    rule=lambda model, t, r, y: (
        model.vTechRetiredStock[t, r, y] if (t, r, y) in mvTechRetiredStock else 0
    )
    + sum(
        model.vTechRetiredNewCap[t, r, y, yp]
        for yp in year
        if (t, r, y, yp) in mvTechRetiredNewCap
    )
    >= pTechRetLo.get((t, r, y)) * pPeriodLen.get((y)),
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqTechRetCost(tech, region, year)$mTechRetCost(tech, region, year)
if verbose:
    print("eqTechRetCost ", end="")
sys.stdout.flush()
model.eqTechRetCost = Constraint(
    mTechRetCost,
    rule=lambda model, t, r, y: model.vTechRetCost[t, r, y]
    == pTechRetCost.get((t, r, y))
    * (model.vTechRetiredStock[t, r, y] if (t, r, y) in mvTechRetiredStock else 0)
    + sum(
        pTechRetCost.get((t, r, y))
        * (
            model.vTechRetiredNewCap[t, r, yp, y]
            if (t, r, yp, y) in mvTechRetiredNewCap
            else 0
        )
        for yp in year
        if (t, r, yp, y) in mvTechRetiredNewCap
    ),
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqTechEac(tech, region, year)$mTechSpan(tech, region, year)
if verbose:
    print("eqTechEac ", end="")
sys.stdout.flush()
# [eac-fix] vintaged new-capacity form (pTechEac applies to NEW capacity only);
# reverted from the simplified pTechEac*vTechCap (which charged annuity on stock too).
model.eqTechEac = Constraint(
    mTechEac,
    rule=lambda model, t, r, y: model.vTechEac[t, r, y]
    == sum(
        pTechEac.get((t, r, yp))
        * (
            model.vTechNewCap[t, r, yp]
            - sum(
                model.vTechRetiredNewCap[t, r, yp, ye]
                for ye in year
                if (
                    (t, r, yp, ye) in mvTechRetiredNewCap
                    and ordYear.get((y)) >= ordYear.get((ye))
                )
            )
        )
        for yp in year
        if (
            (t, r, yp) in mTechNew
            and ordYear.get((y)) >= ordYear.get((yp))
            and (
                ordYear.get((y)) < pTechOlife.get((t, r)) + ordYear.get((yp))
                or (t, r) in mTechOlifeInf
            )
        )
    ),
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqTechInv(tech, region, year)$mTechInv(tech, region, year)
if verbose:
    print("eqTechInv ", end="")
sys.stdout.flush()
model.eqTechInv = Constraint(
    mTechInv,
    rule=lambda model, t, r, y: model.vTechInv[t, r, y]
    == pTechInvcost.get((t, r, y)) * model.vTechNewCap[t, r, y],
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqTechFixom(tech, region, year)$mTechFixom(tech, region, year)
if verbose:
    print("eqTechFixom ", end="")
sys.stdout.flush()
model.eqTechFixom = Constraint(
    mTechFixom,
    rule=lambda model, t, r, y: model.vTechFixom[t, r, y]
    == pTechFixom.get((t, r, y)) * model.vTechCap[t, r, y],
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqTechVarom(tech, region, year)$mTechVarom(tech, region, year)
if verbose:
    print("eqTechVarom ", end="")
sys.stdout.flush()
model.eqTechVarom = Constraint(
    mTechVarom,
    rule=lambda model, t, r, y: model.vTechVarom[t, r, y]
    == sum(
        pTechVarom.get((t, r, y, s))
        * pSliceWeight.get((y, s))
        * model.vTechAct[t, r, y, s]
        + sum(
            pTechCvarom.get((t, c, r, y, s))
            * pSliceWeight.get((y, s))
            * model.vTechInp[t, c, r, y, s]
            for c in comm
            if (t, c) in mTechInpComm
        )
        + sum(
            pTechCvarom.get((t, c, r, y, s))
            * pSliceWeight.get((y, s))
            * model.vTechOut[t, c, r, y, s]
            for c in comm
            if (t, c) in mTechOutComm
        )
        + sum(
            pTechAvarom.get((t, c, r, y, s))
            * pSliceWeight.get((y, s))
            * model.vTechAOut[t, c, r, y, s]
            for c in comm
            if (t, c, r, y, s) in mvTechAOut
        )
        + sum(
            pTechAvarom.get((t, c, r, y, s))
            * pSliceWeight.get((y, s))
            * model.vTechAInp[t, c, r, y, s]
            for c in comm
            if (t, c, r, y, s) in mvTechAInp
        )
        for s in slice
        if (t, s) in mTechSlice
    ),
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqSupAvaUp(sup, comm, region, year, slice)$mSupAvaUp(sup, comm, region, year, slice)
if verbose:
    print("eqSupAvaUp ", end="")
sys.stdout.flush()
model.eqSupAvaUp = Constraint(
    mSupAvaUp,
    rule=lambda model, s1, c, r, y, s: model.vSupOut[s1, c, r, y, s]
    <= pSupAvaUp.get((s1, c, r, y, s))
    * prod(
        pSupWeatherUp.get((wth1, s1)) * pWeather.get((wth1, r, y, s))
        for wth1 in weather
        if (wth1, s1) in mSupWeatherUp
    ),
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqSupAvaLo(sup, comm, region, year, slice)$meqSupAvaLo(sup, comm, region, year, slice)
if verbose:
    print("eqSupAvaLo ", end="")
sys.stdout.flush()
model.eqSupAvaLo = Constraint(
    meqSupAvaLo,
    rule=lambda model, s1, c, r, y, s: model.vSupOut[s1, c, r, y, s]
    >= pSupAvaLo.get((s1, c, r, y, s))
    * prod(
        pSupWeatherLo.get((wth1, s1)) * pWeather.get((wth1, r, y, s))
        for wth1 in weather
        if (wth1, s1) in mSupWeatherLo
    ),
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqSupReserve(sup, comm, region)$mvSupReserve(sup, comm, region)
if verbose:
    print("eqSupReserve ", end="")
sys.stdout.flush()
model.eqSupReserve = Constraint(
    mvSupReserve,
    rule=lambda model, s1, c, r: model.vSupReserve[s1, c, r]
    == sum(
        pPeriodLen.get((y)) * pSliceWeight.get((y, s)) * model.vSupOut[s1, c, r, y, s]
        for y in year
        for s in slice
        if (s1, c, r, y, s) in mSupAva
    ),
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqSupReserveUp(sup, comm, region)$mSupReserveUp(sup, comm, region)
if verbose:
    print("eqSupReserveUp ", end="")
sys.stdout.flush()
model.eqSupReserveUp = Constraint(
    mSupReserveUp,
    rule=lambda model, s1, c, r: model.vSupReserve[s1, c, r]
    <= pSupReserveUp.get((s1, c, r)),
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqSupReserveLo(sup, comm, region)$meqSupReserveLo(sup, comm, region)
if verbose:
    print("eqSupReserveLo ", end="")
sys.stdout.flush()
model.eqSupReserveLo = Constraint(
    meqSupReserveLo,
    rule=lambda model, s1, c, r: model.vSupReserve[s1, c, r]
    >= pSupReserveLo.get((s1, c, r)),
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqSupCost(sup, region, year)$mvSupCost(sup, region, year)
if verbose:
    print("eqSupCost ", end="")
sys.stdout.flush()
model.eqSupCost = Constraint(
    mvSupCost,
    rule=lambda model, s1, r, y: model.vSupCost[s1, r, y]
    == sum(
        pSupCost.get((s1, c, r, y, s))
        * pSliceWeight.get((y, s))
        * model.vSupOut[s1, c, r, y, s]
        for c in comm
        for s in slice
        if (s1, c, r, y, s) in mSupAva
    ),
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqDemInp(comm, region, year, slice)$mvDemInp(comm, region, year, slice)
if verbose:
    print("eqDemInp ", end="")
sys.stdout.flush()
model.eqDemInp = Constraint(
    mvDemInp,
    rule=lambda model, c, r, y, s: model.vDemInp[c, r, y, s]
    == sum(pDemand.get((d, c, r, y, s)) for d in dem if (d, c) in mDemComm),
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqAggOutTot(comm, region, year, slice)$mAggOut(comm, region, year, slice)
if verbose:
    print("eqAggOutTot ", end="")
sys.stdout.flush()
model.eqAggOutTot = Constraint(
    mAggOut,
    rule=lambda model, c, r, y, s: model.vAggOutTot[c, r, y, s]
    == sum(
        pAggregateFactor.get((c, cp))
        * sum(
            (model.vOutTot[cp, r, y, sp] if (cp, r, y, sp) in mvOutTot else 0)
            for sp in slice
            if (
                (c, r, y, sp) in mvOutTot
                and (s, sp) in mSliceParentChildE
                and (cp, sp) in mCommSlice
            )
        )
        for cp in comm
        if (c, cp) in mAggregateFactor
    ),
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqEmsFuelTot(comm, region, year, slice)$mEmsFuelTot(comm, region, year, slice)
if verbose:
    print("eqEmsFuelTot ", end="")
sys.stdout.flush()
model.eqEmsFuelTot = Constraint(
    mEmsFuelTot,
    rule=lambda model, c, r, y, s: model.vEmsFuelTot[c, r, y, s]
    == sum(
        pEmissionFactor.get((c, cp))
        * sum(
            pTechEmisComm.get((t, cp))
            * sum(
                (
                    model.vTechInp[t, cp, r, y, sp]
                    if (t, c, cp, r, y, sp) in mTechEmsFuel
                    else 0
                )
                for sp in slice
                if (c, s, sp) in mCommSliceOrParent
            )
            for t in tech
            if (t, cp) in mTechInpComm
        )
        for cp in comm
        if (pEmissionFactor.get((c, cp)) > 0)
    ),
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqStorageAInp(stg, comm, region, year, slice)$mvStorageAInp(stg, comm, region, year, slice)
if verbose:
    print("eqStorageAInp ", end="")
sys.stdout.flush()
model.eqStorageAInp = Constraint(
    mvStorageAInp,
    rule=lambda model, st1, c, r, y, s: model.vStorageAInp[st1, c, r, y, s]
    == sum(
        (
            (
                pStorageStg2AInp.get((st1, c, r, y, s))
                * model.vStorageStore[st1, cp, r, y, s]
            )
            if (st1, c, r, y, s) in mStorageStg2AInp
            else 0
        )
        + (
            (
                pStorageCinp2AInp.get((st1, c, r, y, s))
                * model.vStorageInp[st1, cp, r, y, s]
            )
            if (st1, c, r, y, s) in mStorageCinp2AInp
            else 0
        )
        + (
            (
                pStorageCout2AInp.get((st1, c, r, y, s))
                * model.vStorageOut[st1, cp, r, y, s]
            )
            if (st1, c, r, y, s) in mStorageCout2AInp
            else 0
        )
        + (
            (pStorageCap2AInp.get((st1, c, r, y, s)) * model.vStorageCap[st1, r, y])
            if (st1, c, r, y, s) in mStorageCap2AInp
            else 0
        )
        + (
            (pStorageNCap2AInp.get((st1, c, r, y, s)) * model.vStorageNewCap[st1, r, y])
            if (st1, c, r, y, s) in mStorageNCap2AInp
            else 0
        )
        for cp in comm
        if (st1, cp) in mStorageComm
    ),
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqStorageAOut(stg, comm, region, year, slice)$mvStorageAOut(stg, comm, region, year, slice)
if verbose:
    print("eqStorageAOut ", end="")
sys.stdout.flush()
model.eqStorageAOut = Constraint(
    mvStorageAOut,
    rule=lambda model, st1, c, r, y, s: model.vStorageAOut[st1, c, r, y, s]
    == sum(
        (
            (
                pStorageStg2AOut.get((st1, c, r, y, s))
                * model.vStorageStore[st1, cp, r, y, s]
            )
            if (st1, c, r, y, s) in mStorageStg2AOut
            else 0
        )
        + (
            (
                pStorageCinp2AOut.get((st1, c, r, y, s))
                * model.vStorageInp[st1, cp, r, y, s]
            )
            if (st1, c, r, y, s) in mStorageCinp2AOut
            else 0
        )
        + (
            (
                pStorageCout2AOut.get((st1, c, r, y, s))
                * model.vStorageOut[st1, cp, r, y, s]
            )
            if (st1, c, r, y, s) in mStorageCout2AOut
            else 0
        )
        + (
            (pStorageCap2AOut.get((st1, c, r, y, s)) * model.vStorageCap[st1, r, y])
            if (st1, c, r, y, s) in mStorageCap2AOut
            else 0
        )
        + (
            (pStorageNCap2AOut.get((st1, c, r, y, s)) * model.vStorageNewCap[st1, r, y])
            if (st1, c, r, y, s) in mStorageNCap2AOut
            else 0
        )
        for cp in comm
        if (st1, cp) in mStorageComm
    ),
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqStorageStore(stg, comm, region, year, slicep, slice)$meqStorageStore(stg, comm, region, year, slicep, slice)
if verbose:
    print("eqStorageStore ", end="")
sys.stdout.flush()
model.eqStorageStore = Constraint(
    meqStorageStore,
    rule=lambda model, st1, c, r, y, sp, s: model.vStorageStore[st1, c, r, y, s]
    == pStorageCharge.get((st1, c, r, y, s))
    + (
        (pStorageNCap2Stg.get((st1, c, r, y, s)) * model.vStorageNewCap[st1, r, y])
        if (st1, r, y) in mStorageNew
        else 0
    )
    + pStorageInpEff.get((st1, c, r, y, sp)) * model.vStorageInp[st1, c, r, y, sp]
    + ((pStorageStgEff.get((st1, c, r, y, s))) ** (pSliceShare.get((s))))
    * model.vStorageStore[st1, c, r, y, sp]
    - (model.vStorageOut[st1, c, r, y, sp]) / (pStorageOutEff.get((st1, c, r, y, sp))),
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqStorageAfLo(stg, comm, region, year, slice)$meqStorageAfLo(stg, comm, region, year, slice)
if verbose:
    print("eqStorageAfLo ", end="")
sys.stdout.flush()
model.eqStorageAfLo = Constraint(
    meqStorageAfLo,
    rule=lambda model, st1, c, r, y, s: model.vStorageStore[st1, c, r, y, s]
    >= pStorageAfLo.get((st1, r, y, s))
    * pStorageCap2stg.get((st1))
    * model.vStorageCap[st1, r, y]
    * prod(
        pStorageWeatherAfLo.get((wth1, st1)) * pWeather.get((wth1, r, y, s))
        for wth1 in weather
        if (wth1, st1) in mStorageWeatherAfLo
    ),
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqStorageAfUp(stg, comm, region, year, slice)$meqStorageAfUp(stg, comm, region, year, slice)
if verbose:
    print("eqStorageAfUp ", end="")
sys.stdout.flush()
model.eqStorageAfUp = Constraint(
    meqStorageAfUp,
    rule=lambda model, st1, c, r, y, s: model.vStorageStore[st1, c, r, y, s]
    <= pStorageAfUp.get((st1, r, y, s))
    * pStorageCap2stg.get((st1))
    * model.vStorageCap[st1, r, y]
    * prod(
        pStorageWeatherAfUp.get((wth1, st1)) * pWeather.get((wth1, r, y, s))
        for wth1 in weather
        if (wth1, st1) in mStorageWeatherAfUp
    ),
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqStorageClear(stg, comm, region, year, slice)$mvStorageStore(stg, comm, region, year, slice)
if verbose:
    print("eqStorageClear ", end="")
sys.stdout.flush()
model.eqStorageClear = Constraint(
    mvStorageStore,
    rule=lambda model, st1, c, r, y, s: (model.vStorageOut[st1, c, r, y, s])
    / (pStorageOutEff.get((st1, c, r, y, s)))
    <= model.vStorageStore[st1, c, r, y, s],
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqStorageInpUp(stg, comm, region, year, slice)$meqStorageInpUp(stg, comm, region, year, slice)
if verbose:
    print("eqStorageInpUp ", end="")
sys.stdout.flush()
model.eqStorageInpUp = Constraint(
    meqStorageInpUp,
    rule=lambda model, st1, c, r, y, s: model.vStorageInp[st1, c, r, y, s]
    <= model.vStorageCap[st1, r, y]
    * pStorageCinpUp.get((st1, c, r, y, s))
    * prod(
        pStorageWeatherCinpUp.get((wth1, st1)) * pWeather.get((wth1, r, y, s))
        for wth1 in weather
        if (wth1, st1) in mStorageWeatherCinpUp
    ),
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqStorageInpLo(stg, comm, region, year, slice)$meqStorageInpLo(stg, comm, region, year, slice)
if verbose:
    print("eqStorageInpLo ", end="")
sys.stdout.flush()
model.eqStorageInpLo = Constraint(
    meqStorageInpLo,
    rule=lambda model, st1, c, r, y, s: model.vStorageInp[st1, c, r, y, s]
    >= model.vStorageCap[st1, r, y]
    * pStorageCinpLo.get((st1, c, r, y, s))
    * prod(
        pStorageWeatherCinpLo.get((wth1, st1)) * pWeather.get((wth1, r, y, s))
        for wth1 in weather
        if (wth1, st1) in mStorageWeatherCinpLo
    ),
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqStorageOutUp(stg, comm, region, year, slice)$meqStorageOutUp(stg, comm, region, year, slice)
if verbose:
    print("eqStorageOutUp ", end="")
sys.stdout.flush()
model.eqStorageOutUp = Constraint(
    meqStorageOutUp,
    rule=lambda model, st1, c, r, y, s: model.vStorageOut[st1, c, r, y, s]
    <= model.vStorageCap[st1, r, y]
    * pStorageCoutUp.get((st1, c, r, y, s))
    * prod(
        pStorageWeatherCoutUp.get((wth1, st1)) * pWeather.get((wth1, r, y, s))
        for wth1 in weather
        if (wth1, st1) in mStorageWeatherCoutUp
    ),
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqStorageOutLo(stg, comm, region, year, slice)$meqStorageOutLo(stg, comm, region, year, slice)
if verbose:
    print("eqStorageOutLo ", end="")
sys.stdout.flush()
model.eqStorageOutLo = Constraint(
    meqStorageOutLo,
    rule=lambda model, st1, c, r, y, s: model.vStorageOut[st1, c, r, y, s]
    >= model.vStorageCap[st1, r, y]
    * pStorageCoutLo.get((st1, c, r, y, s))
    * prod(
        pStorageWeatherCoutLo.get((wth1, st1)) * pWeather.get((wth1, r, y, s))
        for wth1 in weather
        if (wth1, st1) in mStorageWeatherCoutLo
    ),
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqStorageCap(stg, region, year)$mStorageSpan(stg, region, year)
if verbose:
    print("eqStorageCap ", end="")
sys.stdout.flush()
model.eqStorageCap = Constraint(
    mStorageSpan,
    rule=lambda model, st1, r, y: model.vStorageCap[st1, r, y]
    == pStorageStock.get((st1, r, y))
    + sum(
        pPeriodLen.get((yp)) * model.vStorageNewCap[st1, r, yp]
        for yp in year
        if (
            ordYear.get((y)) >= ordYear.get((yp))
            and (
                (st1, r) in mStorageOlifeInf
                or ordYear.get((y)) < pStorageOlife.get((st1, r)) + ordYear.get((yp))
            )
            and (st1, r, yp) in mStorageNew
        )
    ),
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqStorageCapLo(stg, region, year)$mStorageCapLo(stg, region, year)
if verbose:
    print("eqStorageCapLo ", end="")
sys.stdout.flush()
model.eqStorageCapLo = Constraint(
    mStorageCapLo,
    rule=lambda model, st1, r, y: model.vStorageCap[st1, r, y]
    >= pStorageCapLo.get((st1, r, y)),
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqStorageCapUp(stg, region, year)$mStorageCapUp(stg, region, year)
if verbose:
    print("eqStorageCapUp ", end="")
sys.stdout.flush()
model.eqStorageCapUp = Constraint(
    mStorageCapUp,
    rule=lambda model, st1, r, y: model.vStorageCap[st1, r, y]
    <= pStorageCapUp.get((st1, r, y)),
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqStorageNewCapLo(stg, region, year)$mStorageNewCapLo(stg, region, year)
if verbose:
    print("eqStorageNewCapLo ", end="")
sys.stdout.flush()
model.eqStorageNewCapLo = Constraint(
    mStorageNewCapLo,
    rule=lambda model, st1, r, y: model.vStorageNewCap[st1, r, y]
    >= pStorageNewCapLo.get((st1, r, y)) * pPeriodLen.get((y)),
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqStorageNewCapUp(stg, region, year)$mStorageNewCapUp(stg, region, year)
if verbose:
    print("eqStorageNewCapUp ", end="")
sys.stdout.flush()
model.eqStorageNewCapUp = Constraint(
    mStorageNewCapUp,
    rule=lambda model, st1, r, y: model.vStorageNewCap[st1, r, y]
    <= pStorageNewCapUp.get((st1, r, y)) * pPeriodLen.get((y)),
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqStorageInv(stg, region, year)$mStorageNew(stg, region, year)
if verbose:
    print("eqStorageInv ", end="")
sys.stdout.flush()
model.eqStorageInv = Constraint(
    mStorageNew,
    rule=lambda model, st1, r, y: model.vStorageInv[st1, r, y]
    == pStorageInvcost.get((st1, r, y)) * model.vStorageNewCap[st1, r, y],
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqStorageEac(stg, region, year)$mStorageEac(stg, region, year)
if verbose:
    print("eqStorageEac ", end="")
sys.stdout.flush()
# [eac-fix] vintaged new-capacity form (pStorageEac applies to NEW capacity only).
model.eqStorageEac = Constraint(
    mStorageEac,
    rule=lambda model, st1, r, y: model.vStorageEac[st1, r, y]
    == sum(
        pStorageEac.get((st1, r, yp)) * model.vStorageNewCap[st1, r, yp]
        for yp in year
        if (
            (st1, r, yp) in mStorageNew
            and ordYear.get((y)) >= ordYear.get((yp))
            and (
                (st1, r) in mStorageOlifeInf
                or ordYear.get((y)) < pStorageOlife.get((st1, r)) + ordYear.get((yp))
            )
            and pStorageInvcost.get((st1, r, yp)) != 0
        )
    ),
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqStorageFixom(stg, region, year)$mStorageFixom(stg, region, year)
if verbose:
    print("eqStorageFixom ", end="")
sys.stdout.flush()
model.eqStorageFixom = Constraint(
    mStorageFixom,
    rule=lambda model, st1, r, y: model.vStorageFixom[st1, r, y]
    == pStorageFixom.get((st1, r, y)) * model.vStorageCap[st1, r, y],
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqStorageVarom(stg, region, year)$mStorageVarom(stg, region, year)
if verbose:
    print("eqStorageVarom ", end="")
sys.stdout.flush()
model.eqStorageVarom = Constraint(
    mStorageVarom,
    rule=lambda model, st1, r, y: model.vStorageVarom[st1, r, y]
    == sum(
        sum(
            pStorageCostInp.get((st1, r, y, s))
            * pSliceWeight.get((y, s))
            * model.vStorageInp[st1, c, r, y, s]
            + pStorageCostOut.get((st1, r, y, s))
            * pSliceWeight.get((y, s))
            * model.vStorageOut[st1, c, r, y, s]
            + pStorageCostStore.get((st1, r, y, s))
            * pSliceWeight.get((y, s))
            * model.vStorageStore[st1, c, r, y, s]
            for s in slice
            if (c, s) in mCommSlice
        )
        for c in comm
        if (st1, c) in mStorageComm
    ),
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqImportTot(comm, dst, year, slice)$mImport(comm, dst, year, slice)
if verbose:
    print("eqImportTot ", end="")
sys.stdout.flush()
model.eqImportTot = Constraint(
    mImport,
    rule=lambda model, c, dst, y, s: model.vImportTot[c, dst, y, s]
    == sum(
        sum(
            (
                (
                    pTradeIrEff.get((t1, src, dst, y, s))
                    * model.vTradeIr[t1, c, src, dst, y, s]
                )
                if (t1, c, src, dst, y, s) in mvTradeIr
                else 0
            )
            for src in region
            if (t1, src, dst) in mTradeRoutes
        )
        for t1 in trade
        if (t1, c) in mTradeComm
    )
    + sum(
        (model.vImportRow[i, c, dst, y, s] if (i, c, dst, y, s) in mImportRow else 0)
        for i in imp
        if (i, c) in mImpComm
    ),
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqExportTot(comm, src, year, slice)$mExport(comm, src, year, slice)
if verbose:
    print("eqExportTot ", end="")
sys.stdout.flush()
model.eqExportTot = Constraint(
    mExport,
    rule=lambda model, c, src, y, s: model.vExportTot[c, src, y, s]
    == sum(
        sum(
            (
                model.vTradeIr[t1, c, src, dst, y, s]
                if (t1, c, src, dst, y, s) in mvTradeIr
                else 0
            )
            for dst in region
            if (t1, src, dst) in mTradeRoutes
        )
        for t1 in trade
        if (t1, c) in mTradeComm
    )
    + sum(
        (model.vExportRow[e, c, src, y, s] if (e, c, src, y, s) in mExportRow else 0)
        for e in expp
        if (e, c) in mExpComm
    ),
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqTradeFlowUp(trade, comm, src, dst, year, slice)$meqTradeFlowUp(trade, comm, src, dst, year, slice)
if verbose:
    print("eqTradeFlowUp ", end="")
sys.stdout.flush()
model.eqTradeFlowUp = Constraint(
    meqTradeFlowUp,
    rule=lambda model, t1, c, src, dst, y, s: model.vTradeIr[t1, c, src, dst, y, s]
    <= pTradeIrUp.get((t1, src, dst, y, s)),
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqTradeFlowLo(trade, comm, src, dst, year, slice)$meqTradeFlowLo(trade, comm, src, dst, year, slice)
if verbose:
    print("eqTradeFlowLo ", end="")
sys.stdout.flush()
model.eqTradeFlowLo = Constraint(
    meqTradeFlowLo,
    rule=lambda model, t1, c, src, dst, y, s: model.vTradeIr[t1, c, src, dst, y, s]
    >= pTradeIrLo.get((t1, src, dst, y, s)),
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqImportIrCost(trade, region, year)$mImportIrCost(trade, region, year)
if verbose:
    print("eqImportIrCost ", end="")
sys.stdout.flush()
model.eqImportIrCost = Constraint(
    mImportIrCost,
    rule=lambda model, t1, r, y: model.vImportIrCost[t1, r, y]
    == sum(
        sum(
            sum(
                (
                    (
                        (
                            pTradeIrCost.get((t1, src, r, y, s))
                            + pTradeIrMarkup.get((t1, src, r, y, s))
                        )
                        * model.vTradeIr[t1, c, src, r, y, s]
                        * pSliceWeight.get((y, s))
                    )
                    if (t1, c, src, r, y, s) in mvTradeIr
                    else 0
                )
                for s in slice
                if (t1, s) in mTradeSlice
            )
            for c in comm
            if (t1, c) in mTradeComm
        )
        for src in region
        if (t1, src, r) in mTradeRoutes
    ),
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqExportIrCost(trade, region, year)$mExportIrCost(trade, region, year)
if verbose:
    print("eqExportIrCost ", end="")
sys.stdout.flush()
model.eqExportIrCost = Constraint(
    mExportIrCost,
    rule=lambda model, t1, r, y: model.vExportIrCost[t1, r, y]
    == -sum(
        sum(
            sum(
                (
                    (
                        (
                            pTradeIrCost.get((t1, r, dst, y, s))
                            + pTradeIrMarkup.get((t1, r, dst, y, s))
                        )
                        * model.vTradeIr[t1, c, r, dst, y, s]
                        * pSliceWeight.get((y, s))
                    )
                    if (t1, c, r, dst, y, s) in mvTradeIr
                    else 0
                )
                for s in slice
                if (t1, s) in mTradeSlice
            )
            for c in comm
            if (t1, c) in mTradeComm
        )
        for dst in region
        if (t1, r, dst) in mTradeRoutes
    ),
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqExportRowUp(expp, comm, region, year, slice)$mExportRowUp(expp, comm, region, year, slice)
if verbose:
    print("eqExportRowUp ", end="")
sys.stdout.flush()
model.eqExportRowUp = Constraint(
    mExportRowUp,
    rule=lambda model, e, c, r, y, s: model.vExportRow[e, c, r, y, s]
    <= pExportRowUp.get((e, r, y, s)),
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqExportRowLo(expp, comm, region, year, slice)$meqExportRowLo(expp, comm, region, year, slice)
if verbose:
    print("eqExportRowLo ", end="")
sys.stdout.flush()
model.eqExportRowLo = Constraint(
    meqExportRowLo,
    rule=lambda model, e, c, r, y, s: model.vExportRow[e, c, r, y, s]
    >= pExportRowLo.get((e, r, y, s)),
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqExportRowCum(expp, comm)$mExpComm(expp, comm)
if verbose:
    print("eqExportRowCum ", end="")
sys.stdout.flush()
model.eqExportRowCum = Constraint(
    mExpComm,
    rule=lambda model, e, c: model.vExportRowCum[e, c]
    == sum(
        pPeriodLen.get((y)) * pSliceWeight.get((y, s)) * model.vExportRow[e, c, r, y, s]
        for r in region
        for y in year
        for s in slice
        if (e, c, r, y, s) in mExportRow
    ),
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqExportRowResUp(expp, comm)$mExportRowCumUp(expp, comm)
if verbose:
    print("eqExportRowResUp ", end="")
sys.stdout.flush()
model.eqExportRowResUp = Constraint(
    mExportRowCumUp,
    rule=lambda model, e, c: model.vExportRowCum[e, c] <= pExportRowRes.get((e)),
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqExportRowCost(expp, region, year)$mExportRowCost(expp, region, year)
if verbose:
    print("eqExportRowCost ", end="")
sys.stdout.flush()
model.eqExportRowCost = Constraint(
    mExportRowCost,
    rule=lambda model, e, r, y: model.vExportRowCost[e, r, y]
    == -sum(
        pExportRowPrice.get((e, r, y, s))
        * pSliceWeight.get((y, s))
        * model.vExportRow[e, c, r, y, s]
        for c in comm
        for s in slice
        if (e, c, r, y, s) in mExportRow
    ),
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqImportRowUp(imp, comm, region, year, slice)$mImportRowUp(imp, comm, region, year, slice)
if verbose:
    print("eqImportRowUp ", end="")
sys.stdout.flush()
model.eqImportRowUp = Constraint(
    mImportRowUp,
    rule=lambda model, i, c, r, y, s: model.vImportRow[i, c, r, y, s]
    <= pImportRowUp.get((i, r, y, s)),
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqImportRowLo(imp, comm, region, year, slice)$meqImportRowLo(imp, comm, region, year, slice)
if verbose:
    print("eqImportRowLo ", end="")
sys.stdout.flush()
model.eqImportRowLo = Constraint(
    meqImportRowLo,
    rule=lambda model, i, c, r, y, s: model.vImportRow[i, c, r, y, s]
    >= pImportRowLo.get((i, r, y, s)),
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqImportRowCum(imp, comm)$mImpComm(imp, comm)
if verbose:
    print("eqImportRowCum ", end="")
sys.stdout.flush()
model.eqImportRowCum = Constraint(
    mImpComm,
    rule=lambda model, i, c: model.vImportRowCum[i, c]
    == sum(
        pPeriodLen.get((y)) * pSliceWeight.get((y, s)) * model.vImportRow[i, c, r, y, s]
        for r in region
        for y in year
        for s in slice
        if (i, c, r, y, s) in mImportRow
    ),
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqImportRowResUp(imp, comm)$mImportRowCumUp(imp, comm)
if verbose:
    print("eqImportRowResUp ", end="")
sys.stdout.flush()
model.eqImportRowResUp = Constraint(
    mImportRowCumUp,
    rule=lambda model, i, c: model.vImportRowCum[i, c] <= pImportRowRes.get((i)),
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqImportRowCost(imp, region, year)$mImportRowCost(imp, region, year)
if verbose:
    print("eqImportRowCost ", end="")
sys.stdout.flush()
model.eqImportRowCost = Constraint(
    mImportRowCost,
    rule=lambda model, i, r, y: model.vImportRowCost[i, r, y]
    == sum(
        pImportRowPrice.get((i, r, y, s))
        * pSliceWeight.get((y, s))
        * model.vImportRow[i, c, r, y, s]
        for c in comm
        for s in slice
        if (i, c, r, y, s) in mImportRow
    ),
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqTradeCapFlow(trade, comm, year, slice)$meqTradeCapFlow(trade, comm, year, slice)
if verbose:
    print("eqTradeCapFlow ", end="")
sys.stdout.flush()
model.eqTradeCapFlow = Constraint(
    meqTradeCapFlow,
    rule=lambda model, t1, c, y, s: pSliceShare.get((s))
    * pTradeCap2Act.get((t1))
    * model.vTradeCap[t1, y]
    >= sum(
        model.vTradeIr[t1, c, src, dst, y, s]
        for src in region
        for dst in region
        if (t1, c, src, dst, y, s) in mvTradeIr
    ),
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqTradeCap(trade, year)$mTradeSpan(trade, year)
if verbose:
    print("eqTradeCap ", end="")
sys.stdout.flush()
model.eqTradeCap = Constraint(
    mTradeSpan,
    rule=lambda model, t1, y: model.vTradeCap[t1, y]
    == pTradeStock.get((t1, y))
    + sum(
        pPeriodLen.get((yp)) * model.vTradeNewCap[t1, yp]
        for yp in year
        if (
            (t1, yp) in mTradeNew
            and ordYear.get((y)) >= ordYear.get((yp))
            and (
                ordYear.get((y)) < pTradeOlife.get((t1)) + ordYear.get((yp))
                or t1 in mTradeOlifeInf
            )
        )
    ),
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqTradeCapLo(trade, year)$mTradeCapLo(trade, year)
if verbose:
    print("eqTradeCapLo ", end="")
sys.stdout.flush()
model.eqTradeCapLo = Constraint(
    mTradeCapLo,
    rule=lambda model, t1, y: model.vTradeCap[t1, y] >= pTradeCapLo.get((t1, y)),
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqTradeCapUp(trade, year)$mTradeCapUp(trade, year)
if verbose:
    print("eqTradeCapUp ", end="")
sys.stdout.flush()
model.eqTradeCapUp = Constraint(
    mTradeCapUp,
    rule=lambda model, t1, y: model.vTradeCap[t1, y] <= pTradeCapUp.get((t1, y)),
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqTradeNewCapLo(trade, year)$mTradeNewCapLo(trade, year)
if verbose:
    print("eqTradeNewCapLo ", end="")
sys.stdout.flush()
model.eqTradeNewCapLo = Constraint(
    mTradeNewCapLo,
    rule=lambda model, t1, y: model.vTradeNewCap[t1, y] * pPeriodLen.get((y))
    >= pTradeNewCapLo.get((t1, y)),
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqTradeNewCapUp(trade, year)$mTradeNewCapUp(trade, year)
if verbose:
    print("eqTradeNewCapUp ", end="")
sys.stdout.flush()
model.eqTradeNewCapUp = Constraint(
    mTradeNewCapUp,
    rule=lambda model, t1, y: model.vTradeNewCap[t1, y] * pPeriodLen.get((y))
    <= pTradeNewCapUp.get((t1, y)),
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqTradeInv(trade, region, year)$mTradeInv(trade, region, year)
if verbose:
    print("eqTradeInv ", end="")
sys.stdout.flush()
model.eqTradeInv = Constraint(
    mTradeInv,
    rule=lambda model, t1, r, y: model.vTradeInv[t1, r, y]
    == pTradeInvcost.get((t1, r, y)) * model.vTradeNewCap[t1, y],
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqTradeEac(trade, region, year)$mTradeEac(trade, region, year)
if verbose:
    print("eqTradeEac ", end="")
sys.stdout.flush()
# [eac-fix] vintaged new-capacity form (pTradeEac applies to NEW capacity only);
# vTradeNewCap has no region index.
model.eqTradeEac = Constraint(
    mTradeEac,
    rule=lambda model, t1, r, y: model.vTradeEac[t1, r, y]
    == sum(
        pTradeEac.get((t1, r, yp)) * model.vTradeNewCap[t1, yp]
        for yp in year
        if (
            (t1, yp) in mTradeNew
            and ordYear.get((y)) >= ordYear.get((yp))
            and (
                ordYear.get((y)) < pTradeOlife.get((t1)) + ordYear.get((yp))
                or t1 in mTradeOlifeInf
            )
        )
    ),
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqTradeFixom(trade, region, year)$mTradeFixom(trade, region, year)
if verbose:
    print("eqTradeFixom ", end="")
sys.stdout.flush()
model.eqTradeFixom = Constraint(
    mTradeFixom,
    rule=lambda model, t1, r, y: model.vTradeFixom[t1, r, y]
    == pTradeFixom.get((t1, r, y)) * model.vTradeCap[t1, y],
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqTradeIrAInp(trade, comm, region, year, slice)$mvTradeIrAInp(trade, comm, region, year, slice)
if verbose:
    print("eqTradeIrAInp ", end="")
sys.stdout.flush()
model.eqTradeIrAInp = Constraint(
    mvTradeIrAInp,
    rule=lambda model, t1, c, r, y, s: model.vTradeIrAInp[t1, c, r, y, s]
    == sum(
        pTradeIrCsrc2Ainp.get((t1, c, r, dst, y, s))
        * sum(
            model.vTradeIr[t1, cp, r, dst, y, s]
            for cp in comm
            if (t1, cp) in mTradeComm
        )
        for dst in region
        if (t1, c, r, dst, y, s) in mTradeIrCsrc2Ainp
    )
    + sum(
        pTradeIrCdst2Ainp.get((t1, c, src, r, y, s))
        * sum(
            model.vTradeIr[t1, cp, src, r, y, s]
            for cp in comm
            if (t1, cp) in mTradeComm
        )
        for src in region
        if (t1, c, src, r, y, s) in mTradeIrCdst2Ainp
    ),
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqTradeIrAOut(trade, comm, region, year, slice)$mvTradeIrAOut(trade, comm, region, year, slice)
if verbose:
    print("eqTradeIrAOut ", end="")
sys.stdout.flush()
model.eqTradeIrAOut = Constraint(
    mvTradeIrAOut,
    rule=lambda model, t1, c, r, y, s: model.vTradeIrAOut[t1, c, r, y, s]
    == sum(
        pTradeIrCsrc2Aout.get((t1, c, r, dst, y, s))
        * sum(
            model.vTradeIr[t1, cp, r, dst, y, s]
            for cp in comm
            if (t1, cp) in mTradeComm
        )
        for dst in region
        if (t1, c, r, dst, y, s) in mTradeIrCsrc2Aout
    )
    + sum(
        pTradeIrCdst2Aout.get((t1, c, src, r, y, s))
        * sum(
            model.vTradeIr[t1, cp, src, r, y, s]
            for cp in comm
            if (t1, cp) in mTradeComm
        )
        for src in region
        if (t1, c, src, r, y, s) in mTradeIrCdst2Aout
    ),
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqTradeIrAInpTot(comm, region, year, slice)$mvTradeIrAInpTot(comm, region, year, slice)
if verbose:
    print("eqTradeIrAInpTot ", end="")
sys.stdout.flush()
model.eqTradeIrAInpTot = Constraint(
    mvTradeIrAInpTot,
    rule=lambda model, c, r, y, s: model.vTradeIrAInpTot[c, r, y, s]
    == sum(
        model.vTradeIrAInp[t1, c, r, y, sp]
        for t1 in trade
        for sp in slice
        if ((c, s, sp) in mCommSliceOrParent and (t1, c, r, y, sp) in mvTradeIrAInp)
    ),
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqTradeIrAOutTot(comm, region, year, slice)$mvTradeIrAOutTot(comm, region, year, slice)
if verbose:
    print("eqTradeIrAOutTot ", end="")
sys.stdout.flush()
model.eqTradeIrAOutTot = Constraint(
    mvTradeIrAOutTot,
    rule=lambda model, c, r, y, s: model.vTradeIrAOutTot[c, r, y, s]
    == sum(
        model.vTradeIrAOut[t1, c, r, y, sp]
        for t1 in trade
        for sp in slice
        if ((c, s, sp) in mCommSliceOrParent and (t1, c, r, y, sp) in mvTradeIrAOut)
    ),
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqBalLo(comm, region, year, slice)$meqBalLo(comm, region, year, slice)
if verbose:
    print("eqBalLo ", end="")
sys.stdout.flush()
model.eqBalLo = Constraint(
    meqBalLo, rule=lambda model, c, r, y, s: model.vBalance[c, r, y, s] >= 0
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqBalUp(comm, region, year, slice)$meqBalUp(comm, region, year, slice)
if verbose:
    print("eqBalUp ", end="")
sys.stdout.flush()
model.eqBalUp = Constraint(
    meqBalUp, rule=lambda model, c, r, y, s: model.vBalance[c, r, y, s] <= 0
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqBalFx(comm, region, year, slice)$meqBalFx(comm, region, year, slice)
if verbose:
    print("eqBalFx ", end="")
sys.stdout.flush()
model.eqBalFx = Constraint(
    meqBalFx, rule=lambda model, c, r, y, s: model.vBalance[c, r, y, s] == 0
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqBal(comm, region, year, slice)$mvBalance(comm, region, year, slice)
if verbose:
    print("eqBal ", end="")
sys.stdout.flush()
model.eqBal = Constraint(
    mvBalance,
    rule=lambda model, c, r, y, s: model.vBalance[c, r, y, s]
    == (model.vOutTot[c, r, y, s] if (c, r, y, s) in mvOutTot else 0)
    - (model.vInpTot[c, r, y, s] if (c, r, y, s) in mvInpTot else 0),
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# [agg-rewrite] eqBalanceRY/vBalanceRY retired (dead reporting)
# eqOutTot(comm, region, year, slice)$mvOutTot(comm, region, year, slice)
if verbose:
    print("eqOutTot ", end="")
sys.stdout.flush()
model.eqOutTot = Constraint(
    mvOutTot,
    rule=lambda model, c, r, y, s: model.vOutTot[c, r, y, s]
    == (model.vDummyImport[c, r, y, s] if (c, r, y, s) in mDummyImport else 0)
    + (model.vSupOutTot[c, r, y, s] if (c, r, y, s) in mSupOutTot else 0)
    + (model.vEmsFuelTot[c, r, y, s] if (c, r, y, s) in mEmsFuelTot else 0)
    + (model.vAggOutTot[c, r, y, s] if (c, r, y, s) in mAggOut else 0)
    + (model.vTechOutTot[c, r, y, s] if (c, r, y, s) in mTechOutTot else 0)
    + (model.vStorageOutTot[c, r, y, s] if (c, r, y, s) in mStorageOutTot else 0)
    + (model.vImportTot[c, r, y, s] if (c, r, y, s) in mImport else 0)
    + (model.vTradeIrAOutTot[c, r, y, s] if (c, r, y, s) in mvTradeIrAOutTot else 0)
    # [agg-rewrite] up-aggregation of immediately-finer children (replaces vOut2Lo)
    + sum(
        pSliceAgg.get((y, s, sp), 0) * model.vOutTot[c, r, y, sp]
        for sp in slice
        if ((s, sp) in mSliceFamily and (c, r, y, sp) in mvOutTot)
    ),
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# [agg-rewrite] eqOutTotRY/vOutTotRY retired (dead reporting)
# [agg-rewrite] eqOut2Lo removed: replaced by up-aggregation in eqOutTot
# (vOut2Lo retired). Mirrors GLPK.
# eqInpTot(comm, region, year, slice)$mvInpTot(comm, region, year, slice)
if verbose:
    print("eqInpTot ", end="")
sys.stdout.flush()
model.eqInpTot = Constraint(
    mvInpTot,
    rule=lambda model, c, r, y, s: model.vInpTot[c, r, y, s]
    == (model.vDemInp[c, r, y, s] if (c, r, y, s) in mvDemInp else 0)
    + (model.vDummyExport[c, r, y, s] if (c, r, y, s) in mDummyExport else 0)
    + (model.vTechInpTot[c, r, y, s] if (c, r, y, s) in mTechInpTot else 0)
    + (model.vStorageInpTot[c, r, y, s] if (c, r, y, s) in mStorageInpTot else 0)
    + (model.vExportTot[c, r, y, s] if (c, r, y, s) in mExport else 0)
    + (model.vTradeIrAInpTot[c, r, y, s] if (c, r, y, s) in mvTradeIrAInpTot else 0)
    # [agg-rewrite] up-aggregation of immediately-finer children (replaces vInp2Lo)
    + sum(
        pSliceAgg.get((y, s, sp), 0) * model.vInpTot[c, r, y, sp]
        for sp in slice
        if ((s, sp) in mSliceFamily and (c, r, y, sp) in mvInpTot)
    ),
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# [agg-rewrite] eqInpTotRY/vInpTotRY retired (dead reporting)
# [agg-rewrite] eqInp2Lo removed: replaced by up-aggregation in eqInpTot
# (vInp2Lo retired). Mirrors GLPK.
# eqSupOutTot(comm, region, year, slice)$mSupOutTot(comm, region, year, slice)
if verbose:
    print("eqSupOutTot ", end="")
sys.stdout.flush()
model.eqSupOutTot = Constraint(
    mSupOutTot,
    rule=lambda model, c, r, y, s: model.vSupOutTot[c, r, y, s]
    == sum(model.vSupOut[s1, c, r, y, s] for s1 in sup if (s1, c) in mSupComm),
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqTechInpTot(comm, region, year, slice)$mTechInpTot(comm, region, year, slice)
if verbose:
    print("eqTechInpTot ", end="")
sys.stdout.flush()
model.eqTechInpTot = Constraint(
    mTechInpTot,
    rule=lambda model, c, r, y, s: model.vTechInpTot[c, r, y, s]
    == sum(
        (model.vTechInp[t, c, r, y, s] if (t, c, r, y, s) in mvTechInp else 0)
        for t in tech
        if (t, c) in mTechInpCommSameSlice
    )
    + sum(
        sum(
            (model.vTechInp[t, c, r, y, sp] if (t, c, r, y, sp) in mvTechInp else 0)
            for sp in slice
            if (t, c, sp, s) in mTechInpCommAggSlice
        )
        for t in tech
        if (t, c) in mTechInpCommAgg
    )
    + sum(
        (model.vTechAInp[t, c, r, y, s] if (t, c, r, y, s) in mvTechAInp else 0)
        for t in tech
        if (t, c) in mTechAInpCommSameSlice
    )
    + sum(
        sum(
            (model.vTechAInp[t, c, r, y, sp] if (t, c, r, y, sp) in mvTechAInp else 0)
            for sp in slice
            if (t, c, sp, s) in mTechAInpCommAggSlice
        )
        for t in tech
        if (t, c) in mTechAInpCommAgg
    ),
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqTechOutTot(comm, region, year, slice)$mTechOutTot(comm, region, year, slice)
if verbose:
    print("eqTechOutTot ", end="")
sys.stdout.flush()
model.eqTechOutTot = Constraint(
    mTechOutTot,
    rule=lambda model, c, r, y, s: model.vTechOutTot[c, r, y, s]
    == sum(
        (model.vTechOut[t, c, r, y, s] if (t, c, r, y, s) in mvTechOut else 0)
        for t in tech
        if (t, c) in mTechOutCommSameSlice
    )
    + sum(
        sum(
            (model.vTechOut[t, c, r, y, sp] if (t, c, r, y, sp) in mvTechOut else 0)
            for sp in slice
            if (t, c, sp, s) in mTechOutCommAggSlice
        )
        for t in tech
        if (t, c) in mTechOutCommAgg
    )
    + sum(
        (model.vTechAOut[t, c, r, y, s] if (t, c, r, y, s) in mvTechAOut else 0)
        for t in tech
        if (t, c) in mTechAOutCommSameSlice
    )
    + sum(
        sum(
            (model.vTechAOut[t, c, r, y, sp] if (t, c, r, y, sp) in mvTechAOut else 0)
            for sp in slice
            if (t, c, sp, s) in mTechAOutCommAggSlice
        )
        for t in tech
        if (t, c) in mTechAOutCommAgg
    ),
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# [agg-rewrite] eqTechOutRY/vTechOutRY retired (dead reporting)
# eqStorageInpTot(comm, region, year, slice)$mStorageInpTot(comm, region, year, slice)
if verbose:
    print("eqStorageInpTot ", end="")
sys.stdout.flush()
model.eqStorageInpTot = Constraint(
    mStorageInpTot,
    rule=lambda model, c, r, y, s: model.vStorageInpTot[c, r, y, s]
    == sum(
        model.vStorageInp[st1, c, r, y, s]
        for st1 in stg
        if (st1, c, r, y, s) in mvStorageStore
    )
    + sum(
        model.vStorageAInp[st1, c, r, y, s]
        for st1 in stg
        if (st1, c, r, y, s) in mvStorageAInp
    ),
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqStorageOutTot(comm, region, year, slice)$mStorageOutTot(comm, region, year, slice)
if verbose:
    print("eqStorageOutTot ", end="")
sys.stdout.flush()
model.eqStorageOutTot = Constraint(
    mStorageOutTot,
    rule=lambda model, c, r, y, s: model.vStorageOutTot[c, r, y, s]
    == sum(
        model.vStorageOut[st1, c, r, y, s]
        for st1 in stg
        if (st1, c, r, y, s) in mvStorageStore
    )
    + sum(
        model.vStorageAOut[st1, c, r, y, s]
        for st1 in stg
        if (st1, c, r, y, s) in mvStorageAOut
    ),
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqDummyImportCost(comm, region, year)$mDummyImportCost(comm, region, year)
if verbose:
    print("eqDummyImportCost ", end="")
sys.stdout.flush()
model.eqDummyImportCost = Constraint(
    mDummyImportCost,
    rule=lambda model, c, r, y: model.vDummyImportCost[c, r, y]
    == sum(
        pSliceWeight.get((y, s))
        * pDummyImportCost.get((c, r, y, s))
        * (model.vDummyImport[c, r, y, s] if (c, r, y, s) in mDummyImport else 0)
        for s in slice
        if (c, r, y, s) in mDummyImport
    ),
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqDummyExportCost(comm, region, year)$mDummyExportCost(comm, region, year)
if verbose:
    print("eqDummyExportCost ", end="")
sys.stdout.flush()
model.eqDummyExportCost = Constraint(
    mDummyExportCost,
    rule=lambda model, c, r, y: model.vDummyExportCost[c, r, y]
    == sum(
        pSliceWeight.get((y, s))
        * pDummyExportCost.get((c, r, y, s))
        * (model.vDummyExport[c, r, y, s] if (c, r, y, s) in mDummyExport else 0)
        for s in slice
        if (c, r, y, s) in mDummyExport
    ),
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqTaxCost(comm, region, year)$mTaxCost(comm, region, year)
if verbose:
    print("eqTaxCost ", end="")
sys.stdout.flush()
model.eqTaxCost = Constraint(
    mTaxCost,
    rule=lambda model, c, r, y: model.vTaxCost[c, r, y]
    == sum(
        pTaxCostOut.get((c, r, y, s))
        * pSliceWeight.get((y, s))
        * model.vOutTot[c, r, y, s]
        for s in slice
        if ((c, r, y, s) in mvOutTot and (c, s) in mCommSlice)
    )
    + sum(
        pTaxCostInp.get((c, r, y, s))
        * pSliceWeight.get((y, s))
        * model.vInpTot[c, r, y, s]
        for s in slice
        if ((c, r, y, s) in mvInpTot and (c, s) in mCommSlice)
    )
    + sum(
        pTaxCostBal.get((c, r, y, s))
        * pSliceWeight.get((y, s))
        * model.vBalance[c, r, y, s]
        for s in slice
        if ((c, r, y, s) in mvBalance and (c, s) in mCommSlice)
    ),
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqSubsCost(comm, region, year)$mSubCost(comm, region, year)
if verbose:
    print("eqSubsCost ", end="")
sys.stdout.flush()
model.eqSubsCost = Constraint(
    mSubCost,
    rule=lambda model, c, r, y: model.vSubsCost[c, r, y]
    == -sum(
        pSubCostOut.get((c, r, y, s))
        * pSliceWeight.get((y, s))
        * model.vOutTot[c, r, y, s]
        for s in slice
        if ((c, r, y, s) in mvOutTot and (c, s) in mCommSlice)
    )
    - sum(
        pSubCostInp.get((c, r, y, s))
        * pSliceWeight.get((y, s))
        * model.vInpTot[c, r, y, s]
        for s in slice
        if ((c, r, y, s) in mvInpTot and (c, s) in mCommSlice)
    )
    - sum(
        pSubCostBal.get((c, r, y, s))
        * pSliceWeight.get((y, s))
        * model.vBalance[c, r, y, s]
        for s in slice
        if ((c, r, y, s) in mvBalance and (c, s) in mCommSlice)
    ),
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqCost(region, year)$mvTotalCost(region, year)
if verbose:
    print("eqCost ", end="")
sys.stdout.flush()
model.eqCost = Constraint(
    mvTotalCost,
    rule=lambda model, r, y: model.vTotalCost[r, y]
    == +sum(
        (model.vSupCost[s1, r, y] if (s1, r, y) in mvSupCost else 0)
        for s1 in sup
        if (s1, r, y) in mvSupCost
    )
    + sum(
        (model.vTechEac[t, r, y] if (t, r, y) in mTechEac else 0)
        for t in tech
        if (t, r, y) in mTechEac
    )
    + sum(
        (model.vTechRetCost[t, r, y] if (t, r, y) in mTechRetCost else 0)
        for t in tech
        if (t, r, y) in mTechRetCost
    )
    + sum(
        (model.vTechFixom[t, r, y] if (t, r, y) in mTechFixom else 0)
        for t in tech
        if (t, r, y) in mTechFixom
    )
    + sum(
        (model.vTechVarom[t, r, y] if (t, r, y) in mTechVarom else 0)
        for t in tech
        if (t, r, y) in mTechVarom
    )
    + sum(
        (model.vStorageEac[st1, r, y] if (st1, r, y) in mStorageEac else 0)
        for st1 in stg
        if (st1, r, y) in mStorageEac
    )
    + sum(
        (model.vStorageFixom[st1, r, y] if (st1, r, y) in mStorageFixom else 0)
        for st1 in stg
        if (st1, r, y) in mStorageFixom
    )
    + sum(
        (model.vStorageVarom[st1, r, y] if (st1, r, y) in mStorageVarom else 0)
        for st1 in stg
        if (st1, r, y) in mStorageVarom
    )
    + sum(
        (model.vImportRowCost[i, r, y] if (i, r, y) in mImportRowCost else 0)
        for i in imp
        if (i, r, y) in mImportRowCost
    )
    + sum(
        (model.vExportRowCost[e, r, y] if (e, r, y) in mExportRowCost else 0)
        for e in expp
        if (e, r, y) in mExportRowCost
    )
    + sum(
        (model.vTradeEac[t1, r, y] if (t1, r, y) in mTradeEac else 0)
        for t1 in trade
        if (t1, r, y) in mTradeEac
    )
    + sum(
        (model.vTradeFixom[t1, r, y] if (t1, r, y) in mTradeFixom else 0)
        for t1 in trade
        if (t1, r, y) in mTradeFixom
    )
    + sum(
        (model.vImportIrCost[t1, r, y] if (t1, r, y) in mImportIrCost else 0)
        for t1 in trade
        if (t1, r, y) in mImportIrCost
    )
    + sum(
        (model.vExportIrCost[t1, r, y] if (t1, r, y) in mExportIrCost else 0)
        for t1 in trade
        if (t1, r, y) in mExportIrCost
    )
    + sum(
        (model.vTaxCost[c, r, y] if (c, r, y) in mTaxCost else 0)
        for c in comm
        if (c, r, y) in mTaxCost
    )
    + sum(
        (model.vSubsCost[c, r, y] if (c, r, y) in mSubCost else 0)
        for c in comm
        if (c, r, y) in mSubCost
    )
    + (model.vTotalUserCosts[r, y] if (r, y) in mvTotalUserCosts else 0)
    + sum(
        (model.vDummyImportCost[c, r, y] if (c, r, y) in mDummyImportCost else 0)
        for c in comm
        if (c, r, y) in mDummyImportCost
    )
    + sum(
        (model.vDummyExportCost[c, r, y] if (c, r, y) in mDummyExportCost else 0)
        for c in comm
        if (c, r, y) in mDummyExportCost
    ),
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqObjective
if verbose:
    print("eqObjective ", end="")
sys.stdout.flush()
model.eqObjective = Constraint(
    rule=lambda model: model.vObjective
    == sum(
        model.vTotalCost[r, y] * pPeriodLen.get((y)) * pDiscountFactor.get((r, y))
        for r in region
        for y in year
        if (r, y) in mvTotalCost
    )
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqLECActivity(tech, region, year)$meqLECActivity(tech, region, year)
if verbose:
    print("eqLECActivity ", end="")
sys.stdout.flush()
model.eqLECActivity = Constraint(
    meqLECActivity,
    rule=lambda model, t, r, y: sum(
        model.vTechAct[t, r, y, s] for s in slice if (t, s) in mTechSlice
    )
    >= pLECLoACT.get((r)),
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
model.obj = Objective(rule=lambda model: model.vObjective, sense=minimize)
exec(open("inc3.py").read())
model.fornontriv = Var(domain=pyo.NonNegativeReals)
model.eqnontriv = Constraint(rule=lambda model: model.fornontriv == 0)
exec(open("inc_constraints.py").read())
exec(open("inc_costs.py").read())
exec(open("inc_solver.py").read())
# opt = SolverFactory('cplex');
exec(open("inc4.py").read())
flog.write('"solver",,"' + str(datetime.datetime.now().strftime("%H:%M:%S")) + '"\n')
print("solving... ")
slv = opt.solve(model, tee=True)
print(
    "done "
    + str(datetime.datetime.now().strftime("%H:%M:%S"))
    + " ("
    + str(round(time.time() - seconds, 2))
    + " s)"
)
flog.write(
    '"solution status",'
    + str((slv.solver.status == SolverStatus.ok) * 1)
    + ',"'
    + str(datetime.datetime.now().strftime("%H:%M:%S"))
    + '"\n'
)
flog.write(
    '"export results",,"' + str(datetime.datetime.now().strftime("%H:%M:%S")) + '"\n'
)
exec(open("inc5.py").read())
exec(open("output.py").read())
flog.write('"done",,"' + str(datetime.datetime.now().strftime("%H:%M:%S")) + '"\n')
flog.close()
