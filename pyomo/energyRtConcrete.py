# energyRt 0.71
# Energy Systems Modeling Toolbox for R
# https://energyRt.org

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

# Data source: "sqlite" (input/data.db), "feather" (input/<name>.arrow) or
# "parquet" (input/<name>.parquet). The R writer (energyRt) rewrites this line
# at write time from the solver's export_format. A table with no rows is not
# written at all -- the model code never reads those -- so a missing table
# reads back as empty rather than failing.
_DATA_FORMAT = "sqlite"
if _DATA_FORMAT == "sqlite":
    import sqlite3

    _con = sqlite3.connect("input/data.db")

    def _read_tbl(name):
        try:
            return pd.read_sql_query(f'SELECT * FROM "{name}"', _con)
        except Exception:
            return pd.DataFrame()

else:
    import os

    try:
        if _DATA_FORMAT == "parquet":
            import pyarrow.parquet as _pq

            _ext, _reader = ".parquet", _pq.read_table
        else:
            import pyarrow.feather as _feather

            _ext, _reader = ".arrow", _feather.read_feather
    except ImportError:
        raise SystemExit(
            "energyRt: the Python package 'pyarrow' is required to read the "
            "model data in the Arrow exchange format.\n"
            "  Install it with en_install_python_deps() in R, or select the "
            "legacy exchange with solver_options$pyomo_cbc_sqlite "
            '(or solver$export_format <- "SQLite").'
        )

    def _read_tbl(name):
        path = "input/" + name + _ext
        if not os.path.exists(path):
            return pd.DataFrame()
        tbl = _reader(path)
        return tbl.to_pandas() if hasattr(tbl, "to_pandas") else tbl


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
model.vTechStockCap = Var(
    mTechSpan, domain=pyo.NonNegativeReals,
    doc="Surviving exogenous (legacy) capacity"
)
model.vTechStockPhaseOut = Var(
    mvTechPhaseOut, domain=pyo.NonNegativeReals,
    doc="Exogenous capacity leaving on the schedule"
)
model.vTechRetiredStock = Var(
    mvTechRetiredStock, domain=pyo.NonNegativeReals, doc="Early retired stock"
)
model.vTechRetiredNewCap = Var(
    mvTechRetiredNewCap, domain=pyo.NonNegativeReals, doc="Early retired new capacity"
)
model.vTechPhaseOut = Var(
    mvTechPhaseOut, domain=pyo.NonNegativeReals, doc="Capacity reaching end of life"
)
model.vStoragePhaseOut = Var(
    mvStoragePhaseOut, domain=pyo.NonNegativeReals, doc="Storage capacity reaching end of life"
)
model.vStorageStockPhaseOut = Var(
    mvStoragePhaseOut, domain=pyo.NonNegativeReals,
    doc="Exogenous storage capacity leaving on the schedule"
)
model.vStorageOutStockCap = Var(
    mStorageSpan, domain=pyo.NonNegativeReals,
    doc="Surviving exogenous (legacy) capacity"
)
model.vStorageOutRetiredStock = Var(
    mvStorageRetiredStock, domain=pyo.NonNegativeReals, doc="Early retired storage discharging stock"
)
model.vStorageOutRetiredNewCap = Var(
    mvStorageRetiredNewCap, domain=pyo.NonNegativeReals, doc="Early retired new storage discharging capacity"
)
model.vStorageInpStockCap = Var(
    mStorageInpCap, domain=pyo.NonNegativeReals,
    doc="Surviving exogenous (legacy) capacity"
)
model.vStorageInpRetiredStock = Var(
    mvStorageRetiredStock, domain=pyo.NonNegativeReals, doc="Early retired storage charging stock"
)
model.vStorageInpRetiredNewCap = Var(
    mvStorageRetiredNewCap, domain=pyo.NonNegativeReals, doc="Early retired new storage charging capacity"
)
model.vStorageStgStockCap = Var(
    mStorageStgCap, domain=pyo.NonNegativeReals,
    doc="Surviving exogenous (legacy) capacity"
)
model.vStorageStgRetiredStock = Var(
    mvStorageRetiredStock, domain=pyo.NonNegativeReals, doc="Early retired storage energy stock"
)
model.vStorageStgRetiredNewCap = Var(
    mvStorageRetiredNewCap, domain=pyo.NonNegativeReals, doc="Early retired new storage energy capacity"
)
model.vStorageRetCost = Var(mStorageRetCost, doc="Storage early retirement costs")
model.vTradeStockCap = Var(
    mTradeSpan, domain=pyo.NonNegativeReals,
    doc="Surviving exogenous (legacy) capacity"
)
model.vTradePhaseOut = Var(
    mvTradePhaseOut, domain=pyo.NonNegativeReals,
    doc="Trade capacity reaching end of life"
)
model.vTradeStockPhaseOut = Var(
    mvTradePhaseOut, domain=pyo.NonNegativeReals,
    doc="Exogenous trade capacity leaving on the schedule"
)
model.vTradeRetiredStock = Var(
    mvTradeRetiredStock, domain=pyo.NonNegativeReals, doc="Early retired trade stock"
)
model.vTradeRetiredNewCap = Var(
    mvTradeRetiredNewCap, domain=pyo.NonNegativeReals, doc="Early retired new trade capacity"
)
model.vTradeRetCost = Var(mTradeRetCost, doc="Trade early retirement costs")
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
    mvStorageInp, domain=pyo.NonNegativeReals, doc="Storage input"
)
model.vStorageOut = Var(
    mvStorageOut, domain=pyo.NonNegativeReals, doc="Storage output"
)
model.vStorageLevel = Var(
    mvStorageLevel, domain=pyo.NonNegativeReals, doc="Storage level"
)
model.vStorageInv = Var(
    mStorageNew, domain=pyo.NonNegativeReals, doc="Storage investments"
)
model.vStorageEac = Var(
    mStorageEac, domain=pyo.NonNegativeReals, doc="Storage EAC investments"
)
model.vStorageOutCap = Var(
    mStorageSpan, domain=pyo.NonNegativeReals, doc="Storage capacity"
)
model.vStorageStgCap = Var(
    mStorageStgCap, domain=pyo.NonNegativeReals,
    doc="Storage energy (storing) capacity"
)
model.vStorageInpCap = Var(
    mStorageInpCap, domain=pyo.NonNegativeReals,
    doc="Storage charging (input) capacity"
)
model.vStorageInpNewCap = Var(
    mStorageInpNew, domain=pyo.NonNegativeReals,
    doc="Storage new charging (input) capacity"
)
model.vStorageStgNewCap = Var(
    mStorageStgNew, domain=pyo.NonNegativeReals,
    doc="Storage new energy (storing) capacity"
)
model.vStorageOutNewCap = Var(
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
# eqTechSng2Sng(tech, region, comm, commp, year, timeslice)$meqTechSng2Sng(tech, region, comm, commp, year, timeslice)
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
# eqTechGrp2Sng(tech, region, group, commp, year, timeslice)$meqTechGrp2Sng(tech, region, group, commp, year, timeslice)
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
# eqTechSng2Grp(tech, region, comm, groupp, year, timeslice)$meqTechSng2Grp(tech, region, comm, groupp, year, timeslice)
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
# eqTechGrp2Grp(tech, region, group, groupp, year, timeslice)$meqTechGrp2Grp(tech, region, group, groupp, year, timeslice)
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
# eqTechShareInpLo(tech, region, group, comm, year, timeslice)$meqTechShareInpLo(tech, region, group, comm, year, timeslice)
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
# eqTechShareInpUp(tech, region, group, comm, year, timeslice)$meqTechShareInpUp(tech, region, group, comm, year, timeslice)
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
# eqTechShareOutLo(tech, region, group, comm, year, timeslice)$meqTechShareOutLo(tech, region, group, comm, year, timeslice)
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
# eqTechShareOutUp(tech, region, group, comm, year, timeslice)$meqTechShareOutUp(tech, region, group, comm, year, timeslice)
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
# eqTechAInp(tech, comm, region, year, timeslice)$mvTechAInp(tech, comm, region, year, timeslice)
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
    + (
        ((model.vTechPhaseOut[t, r, y] if (t, r, y) in mvTechPhaseOut else 0) * pTechPho2AInp.get((t, c, r, y, s)))
        if (t, c, r, y, s) in mTechPho2AInp
        else 0
    )
    + (
        (((model.vTechRetiredStock[t, r, y] if (t, r, y) in mvTechRetiredStock else 0) + sum(model.vTechRetiredNewCap[t, r, yp, y] for yp in year if (t, r, yp, y) in mvTechRetiredNewCap)) * pTechRet2AInp.get((t, c, r, y, s)))
        if (t, c, r, y, s) in mTechRet2AInp
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
# eqTechAOut(tech, comm, region, year, timeslice)$mvTechAOut(tech, comm, region, year, timeslice)
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
    + (
        ((model.vTechPhaseOut[t, r, y] if (t, r, y) in mvTechPhaseOut else 0) * pTechPho2AOut.get((t, c, r, y, s)))
        if (t, c, r, y, s) in mTechPho2AOut
        else 0
    )
    + (
        (((model.vTechRetiredStock[t, r, y] if (t, r, y) in mvTechRetiredStock else 0) + sum(model.vTechRetiredNewCap[t, r, yp, y] for yp in year if (t, r, yp, y) in mvTechRetiredNewCap)) * pTechRet2AOut.get((t, c, r, y, s)))
        if (t, c, r, y, s) in mTechRet2AOut
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
# eqTechAfLo(tech, region, year, timeslice)$meqTechAfLo(tech, region, year, timeslice)
if verbose:
    print("eqTechAfLo ", end="")
sys.stdout.flush()
model.eqTechAfLo = Constraint(
    meqTechAfLo,
    rule=lambda model, t, r, y, s: pTechAfLo.get((t, r, y, s))
    * pTechCap2act.get((t))
    * model.vTechCap[t, r, y]
    * pTimesliceShare.get((s))
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
# eqTechAfUp(tech, region, year, timeslice)$meqTechAfUp(tech, region, year, timeslice)
if verbose:
    print("eqTechAfUp ", end="")
sys.stdout.flush()
model.eqTechAfUp = Constraint(
    meqTechAfUp,
    rule=lambda model, t, r, y, s: model.vTechAct[t, r, y, s]
    <= pTechAfUp.get((t, r, y, s))
    * pTechCap2act.get((t))
    * model.vTechCap[t, r, y]
    * pTimesliceShare.get((s))
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
# eqTechAfsLo(tech, region, year, timeslice)$meqTechAfsLo(tech, region, year, timeslice)
if verbose:
    print("eqTechAfsLo ", end="")
sys.stdout.flush()
model.eqTechAfsLo = Constraint(
    meqTechAfsLo,
    rule=lambda model, t, r, y, s: pTechAfsLo.get((t, r, y, s))
    * pTechCap2act.get((t))
    * model.vTechCap[t, r, y]
    * pTimesliceShare.get((s))
    * prod(
        pTechWeatherAfsLo.get((wth1, t)) * pWeather.get((wth1, r, y, s))
        for wth1 in weather
        if (wth1, t) in mTechWeatherAfsLo
    )
    <= sum(
        (model.vTechAct[t, r, y, sp] if (t, r, y, sp) in mvTechAct else 0)
        for sp in timeslice
        if (s, sp) in mTimesliceParentChildE
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
# eqTechAfsUp(tech, region, year, timeslice)$meqTechAfsUp(tech, region, year, timeslice)
if verbose:
    print("eqTechAfsUp ", end="")
sys.stdout.flush()
model.eqTechAfsUp = Constraint(
    meqTechAfsUp,
    rule=lambda model, t, r, y, s: sum(
        (model.vTechAct[t, r, y, sp] if (t, r, y, sp) in mvTechAct else 0)
        for sp in timeslice
        if (s, sp) in mTimesliceParentChildE
    )
    <= pTechAfsUp.get((t, r, y, s))
    * pTechCap2act.get((t))
    * model.vTechCap[t, r, y]
    * pTimesliceShare.get((s))
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
# eqTechRampUp(tech, region, year, timeslice, timeslicep)$mTechRampUp(tech, region, year, timeslice, timeslicep)
if verbose:
    print("eqTechRampUp ", end="")
sys.stdout.flush()
model.eqTechRampUp = Constraint(
    mTechRampUp,
    rule=lambda model, t, r, y, s, sp: (model.vTechAct[t, r, y, sp])
    / (pTimesliceShare.get((sp)))
    - (model.vTechAct[t, r, y, s]) / (pTimesliceShare.get((s)))
    <= (
        pTimesliceShare.get((s))
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
# eqTechRampDown(tech, region, year, timeslice, timeslicep)$mTechRampDown(tech, region, year, timeslice, timeslicep)
if verbose:
    print("eqTechRampDown ", end="")
sys.stdout.flush()
model.eqTechRampDown = Constraint(
    mTechRampDown,
    rule=lambda model, t, r, y, s, sp: (model.vTechAct[t, r, y, s])
    / (pTimesliceShare.get((s)))
    - (model.vTechAct[t, r, y, sp]) / (pTimesliceShare.get((sp)))
    <= (
        pTimesliceShare.get((s))
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
# eqTechActSng(tech, comm, region, year, timeslice)$meqTechActSng(tech, comm, region, year, timeslice)
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
# eqTechActGrp(tech, group, region, year, timeslice)$meqTechActGrp(tech, group, region, year, timeslice)
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
# eqTechAfcOutLo(tech, region, comm, year, timeslice)$meqTechAfcOutLo(tech, region, comm, year, timeslice)
if verbose:
    print("eqTechAfcOutLo ", end="")
sys.stdout.flush()
model.eqTechAfcOutLo = Constraint(
    meqTechAfcOutLo,
    rule=lambda model, t, r, c, y, s: pTechCact2cout.get((t, c, r, y, s))
    * pTechAfcLo.get((t, c, r, y, s))
    * pTechCap2act.get((t))
    * model.vTechCap[t, r, y]
    * pTimesliceShare.get((s))
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
# eqTechAfcOutUp(tech, region, comm, year, timeslice)$meqTechAfcOutUp(tech, region, comm, year, timeslice)
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
# eqTechAfcInpLo(tech, region, comm, year, timeslice)$meqTechAfcInpLo(tech, region, comm, year, timeslice)
if verbose:
    print("eqTechAfcInpLo ", end="")
sys.stdout.flush()
model.eqTechAfcInpLo = Constraint(
    meqTechAfcInpLo,
    rule=lambda model, t, r, c, y, s: pTechAfcLo.get((t, c, r, y, s))
    * pTechCap2act.get((t))
    * model.vTechCap[t, r, y]
    * pTimesliceShare.get((s))
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
# eqTechAfcInpUp(tech, region, comm, year, timeslice)$meqTechAfcInpUp(tech, region, comm, year, timeslice)
if verbose:
    print("eqTechAfcInpUp ", end="")
sys.stdout.flush()
model.eqTechAfcInpUp = Constraint(
    meqTechAfcInpUp,
    rule=lambda model, t, r, c, y, s: model.vTechInp[t, c, r, y, s]
    <= pTechAfcUp.get((t, c, r, y, s))
    * pTechCap2act.get((t))
    * model.vTechCap[t, r, y]
    * pTimesliceShare.get((s))
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
    == model.vTechStockCap[t, r, y]
    + sum(
        pPeriodLen.get((yp)) * model.vTechNewCap[t, r, yp]
        - sum(
            model.vTechRetiredNewCap[t, r, yp, ye] * pPeriodLen.get((ye))
            for ye in year
            if (
                (t, r, yp, ye) in mvTechRetiredNewCap
                and ordYear.get((y)) >= ordYear.get((ye))
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
# eqTechStockCap -- the legacy fleet as a recursive balance. `pTechStockSurv`
# thins whatever is ACTUALLY standing, so a declining schedule can never demand
# more capacity than exists and can never block early retirement -- the defect
# of the cumulative form this replaces.
if verbose:
    print("eqTechStockCap ", end="")
sys.stdout.flush()
model.eqTechStockCap = Constraint(
    mTechSpan,
    rule=lambda model, t, r, y: model.vTechStockCap[t, r, y]
    == pTechStockSurv.get((t, r, y))
    * sum(
        model.vTechStockCap[t, r, yp]
        for yp in year
        if ((yp, y) in mMilestoneNext and (t, r, yp) in mTechSpan)
    )
    + pTechStockNew.get((t, r, y))
    - (model.vTechRetiredStock[t, r, y] if (t, r, y) in mvTechRetiredStock else 0)
    * pPeriodLen.get((y)),
)
# eqTechStockPhaseOut -- what the schedule actually removed.
if verbose:
    print("eqTechStockPhaseOut ", end="")
sys.stdout.flush()
model.eqTechStockPhaseOut = Constraint(
    mvTechPhaseOut,
    rule=lambda model, t, r, y: model.vTechStockPhaseOut[t, r, y] * pPeriodLen.get((y))
    == (1 - pTechStockSurv.get((t, r, y)))
    * sum(
        model.vTechStockCap[t, r, yp]
        for yp in year
        if ((yp, y) in mMilestoneNext and (t, r, yp) in mTechSpan)
    ),
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
        model.vTechRetiredNewCap[t, r, yp, y]
        for yp in year
        if (t, r, yp, y) in mvTechRetiredNewCap
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
        model.vTechRetiredNewCap[t, r, yp, y]
        for yp in year
        if (t, r, yp, y) in mvTechRetiredNewCap
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
# eqStorageOutRetiredNewCap(stg, region, year)$meqStorageRetiredNewCap
model.eqStorageOutRetiredNewCap = Constraint(
    meqStorageRetiredNewCap,
    rule=lambda model, st1, r, y: (
        sum(
            model.vStorageOutRetiredNewCap[st1, r, y, yp] * pPeriodLen.get((yp))
            for yp in year
            if (st1, r, y, yp) in mvStorageRetiredNewCap
        )
        <= model.vStorageOutNewCap[st1, r, y] * pPeriodLen.get((y))
        if (st1, r, y) in mStorageNew
        else pyo.Constraint.Skip
    ),
)
# eqStorageOutStockCap -- the legacy fleet as a recursive balance.
model.eqStorageOutStockCap = Constraint(
    mStorageSpan,
    rule=lambda model, st1, r, y: model.vStorageOutStockCap[st1, r, y]
    == pStorageOutStockSurv.get((st1, r, y))
    * sum(
        model.vStorageOutStockCap[st1, r, yp]
        for yp in year
        if ((yp, y) in mMilestoneNext and (st1, r, yp) in mStorageSpan)
    )
    + pStorageOutStockNew.get((st1, r, y))
    - (model.vStorageOutRetiredStock[st1, r, y] if (st1, r, y) in mvStorageRetiredStock else 0)
    * pPeriodLen.get((y)),
)
# eqStorageOutRetUp(stg, region, year)$mStorageOutRetUp
model.eqStorageOutRetUp = Constraint(
    mStorageOutRetUp,
    rule=lambda model, st1, r, y: (
        model.vStorageOutRetiredStock[st1, r, y]
        if (st1, r, y) in mvStorageRetiredStock
        else 0
    )
    + sum(
        model.vStorageOutRetiredNewCap[st1, r, yp, y]
        for yp in year
        if (st1, r, yp, y) in mvStorageRetiredNewCap
    )
    <= pStorageOutRetUp.get((st1, r, y)) * pPeriodLen.get((y)),
)
# eqStorageOutRetLo(stg, region, year)$mStorageOutRetLo
model.eqStorageOutRetLo = Constraint(
    mStorageOutRetLo,
    rule=lambda model, st1, r, y: (
        model.vStorageOutRetiredStock[st1, r, y]
        if (st1, r, y) in mvStorageRetiredStock
        else 0
    )
    + sum(
        model.vStorageOutRetiredNewCap[st1, r, yp, y]
        for yp in year
        if (st1, r, yp, y) in mvStorageRetiredNewCap
    )
    >= pStorageOutRetLo.get((st1, r, y)) * pPeriodLen.get((y)),
)
# eqStorageInpRetiredNewCap(stg, region, year)$meqStorageRetiredNewCap
model.eqStorageInpRetiredNewCap = Constraint(
    meqStorageRetiredNewCap,
    rule=lambda model, st1, r, y: (
        sum(
            model.vStorageInpRetiredNewCap[st1, r, y, yp] * pPeriodLen.get((yp))
            for yp in year
            if (st1, r, y, yp) in mvStorageRetiredNewCap
        )
        <= model.vStorageInpNewCap[st1, r, y] * pPeriodLen.get((y))
        if (st1, r, y) in mStorageInpNew
        else pyo.Constraint.Skip
    ),
)
# eqStorageInpStockCap -- the legacy fleet as a recursive balance.
model.eqStorageInpStockCap = Constraint(
    mStorageInpCap,
    rule=lambda model, st1, r, y: model.vStorageInpStockCap[st1, r, y]
    == pStorageInpStockSurv.get((st1, r, y))
    * sum(
        model.vStorageInpStockCap[st1, r, yp]
        for yp in year
        if ((yp, y) in mMilestoneNext and (st1, r, yp) in mStorageInpCap)
    )
    + pStorageInpStockNew.get((st1, r, y))
    - (model.vStorageInpRetiredStock[st1, r, y] if (st1, r, y) in mvStorageRetiredStock else 0)
    * pPeriodLen.get((y)),
)
# eqStorageInpRetUp(stg, region, year)$mStorageInpRetUp
model.eqStorageInpRetUp = Constraint(
    mStorageInpRetUp,
    rule=lambda model, st1, r, y: (
        model.vStorageInpRetiredStock[st1, r, y]
        if (st1, r, y) in mvStorageRetiredStock
        else 0
    )
    + sum(
        model.vStorageInpRetiredNewCap[st1, r, yp, y]
        for yp in year
        if (st1, r, yp, y) in mvStorageRetiredNewCap
    )
    <= pStorageInpRetUp.get((st1, r, y)) * pPeriodLen.get((y)),
)
# eqStorageInpRetLo(stg, region, year)$mStorageInpRetLo
model.eqStorageInpRetLo = Constraint(
    mStorageInpRetLo,
    rule=lambda model, st1, r, y: (
        model.vStorageInpRetiredStock[st1, r, y]
        if (st1, r, y) in mvStorageRetiredStock
        else 0
    )
    + sum(
        model.vStorageInpRetiredNewCap[st1, r, yp, y]
        for yp in year
        if (st1, r, yp, y) in mvStorageRetiredNewCap
    )
    >= pStorageInpRetLo.get((st1, r, y)) * pPeriodLen.get((y)),
)
# eqStorageStgRetiredNewCap(stg, region, year)$meqStorageRetiredNewCap
model.eqStorageStgRetiredNewCap = Constraint(
    meqStorageRetiredNewCap,
    rule=lambda model, st1, r, y: (
        sum(
            model.vStorageStgRetiredNewCap[st1, r, y, yp] * pPeriodLen.get((yp))
            for yp in year
            if (st1, r, y, yp) in mvStorageRetiredNewCap
        )
        <= model.vStorageStgNewCap[st1, r, y] * pPeriodLen.get((y))
        if (st1, r, y) in mStorageStgNew
        else pyo.Constraint.Skip
    ),
)
# eqStorageStgStockCap -- the legacy fleet as a recursive balance.
model.eqStorageStgStockCap = Constraint(
    mStorageStgCap,
    rule=lambda model, st1, r, y: model.vStorageStgStockCap[st1, r, y]
    == pStorageStgStockSurv.get((st1, r, y))
    * sum(
        model.vStorageStgStockCap[st1, r, yp]
        for yp in year
        if ((yp, y) in mMilestoneNext and (st1, r, yp) in mStorageStgCap)
    )
    + pStorageStgStockNew.get((st1, r, y))
    - (model.vStorageStgRetiredStock[st1, r, y] if (st1, r, y) in mvStorageRetiredStock else 0)
    * pPeriodLen.get((y)),
)
# eqStorageStgRetUp(stg, region, year)$mStorageStgRetUp
model.eqStorageStgRetUp = Constraint(
    mStorageStgRetUp,
    rule=lambda model, st1, r, y: (
        model.vStorageStgRetiredStock[st1, r, y]
        if (st1, r, y) in mvStorageRetiredStock
        else 0
    )
    + sum(
        model.vStorageStgRetiredNewCap[st1, r, yp, y]
        for yp in year
        if (st1, r, yp, y) in mvStorageRetiredNewCap
    )
    <= pStorageStgRetUp.get((st1, r, y)) * pPeriodLen.get((y)),
)
# eqStorageStgRetLo(stg, region, year)$mStorageStgRetLo
model.eqStorageStgRetLo = Constraint(
    mStorageStgRetLo,
    rule=lambda model, st1, r, y: (
        model.vStorageStgRetiredStock[st1, r, y]
        if (st1, r, y) in mvStorageRetiredStock
        else 0
    )
    + sum(
        model.vStorageStgRetiredNewCap[st1, r, yp, y]
        for yp in year
        if (st1, r, yp, y) in mvStorageRetiredNewCap
    )
    >= pStorageStgRetLo.get((st1, r, y)) * pPeriodLen.get((y)),
)
# eqStorageRetCost(stg, region, year)$mStorageRetCost
model.eqStorageRetCost = Constraint(
    mStorageRetCost,
    rule=lambda model, st1, r, y: model.vStorageRetCost[st1, r, y]
    == pStorageOutRetCost.get((st1, r, y))
    * (
        (
            model.vStorageOutRetiredStock[st1, r, y]
            if (st1, r, y) in mvStorageRetiredStock
            else 0
        )
        + sum(
            model.vStorageOutRetiredNewCap[st1, r, yp, y]
            for yp in year
            if (st1, r, yp, y) in mvStorageRetiredNewCap
        )
    )
    + pStorageInpRetCost.get((st1, r, y))
    * (
        (
            model.vStorageInpRetiredStock[st1, r, y]
            if (st1, r, y) in mvStorageRetiredStock
            else 0
        )
        + sum(
            model.vStorageInpRetiredNewCap[st1, r, yp, y]
            for yp in year
            if (st1, r, yp, y) in mvStorageRetiredNewCap
        )
    )
    + pStorageStgRetCost.get((st1, r, y))
    * (
        (
            model.vStorageStgRetiredStock[st1, r, y]
            if (st1, r, y) in mvStorageRetiredStock
            else 0
        )
        + sum(
            model.vStorageStgRetiredNewCap[st1, r, yp, y]
            for yp in year
            if (st1, r, yp, y) in mvStorageRetiredNewCap
        )
    ),
)
# eqTradeRetiredNewCap(trade, year)$meqTradeRetiredNewCap
model.eqTradeRetiredNewCap = Constraint(
    meqTradeRetiredNewCap,
    rule=lambda model, t1, y: (
        sum(
            model.vTradeRetiredNewCap[t1, y, yp] * pPeriodLen.get((yp))
            for yp in year
            if (t1, y, yp) in mvTradeRetiredNewCap
        )
        <= model.vTradeNewCap[t1, y] * pPeriodLen.get((y))
        if (t1, y) in mTradeNew
        else pyo.Constraint.Skip
    ),
)
# eqTradeStockCap -- the legacy fleet as a recursive balance.
model.eqTradeStockCap = Constraint(
    mTradeSpan,
    rule=lambda model, t1, y: model.vTradeStockCap[t1, y]
    == pTradeStockSurv.get((t1, y))
    * sum(
        model.vTradeStockCap[t1, yp]
        for yp in year
        if ((yp, y) in mMilestoneNext and (t1, yp) in mTradeSpan)
    )
    + pTradeStockNew.get((t1, y))
    - (model.vTradeRetiredStock[t1, y] if (t1, y) in mvTradeRetiredStock else 0)
    * pPeriodLen.get((y)),
)
# eqTradePhaseOut -- end-of-life departure, read off the capacity balance.
model.eqTradePhaseOut = Constraint(
    mvTradePhaseOut,
    rule=lambda model, t1, y: model.vTradePhaseOut[t1, y] * pPeriodLen.get((y))
    == sum(
        model.vTradeCap[t1, yp]
        for yp in year
        if ((yp, y) in mMilestoneNext and (t1, yp) in mTradeSpan)
    )
    - model.vTradeCap[t1, y]
    + model.vTradeNewCap[t1, y] * pPeriodLen.get((y))
    - (
        (model.vTradeRetiredStock[t1, y] if (t1, y) in mvTradeRetiredStock else 0)
        + sum(
            model.vTradeRetiredNewCap[t1, yp, y]
            for yp in year
            if (t1, yp, y) in mvTradeRetiredNewCap
        )
    )
    * pPeriodLen.get((y))
    + pTradeStockNew.get((t1, y)),
)
# eqTradeStockPhaseOut -- what the schedule actually removed.
model.eqTradeStockPhaseOut = Constraint(
    mvTradePhaseOut,
    rule=lambda model, t1, y: model.vTradeStockPhaseOut[t1, y] * pPeriodLen.get((y))
    == (1 - pTradeStockSurv.get((t1, y)))
    * sum(
        model.vTradeStockCap[t1, yp]
        for yp in year
        if ((yp, y) in mMilestoneNext and (t1, yp) in mTradeSpan)
    ),
)
# eqTradeRetUp(trade, year)$mTradeRetUp
model.eqTradeRetUp = Constraint(
    mTradeRetUp,
    rule=lambda model, t1, y: (
        model.vTradeRetiredStock[t1, y] if (t1, y) in mvTradeRetiredStock else 0
    )
    + sum(
        model.vTradeRetiredNewCap[t1, yp, y]
        for yp in year
        if (t1, yp, y) in mvTradeRetiredNewCap
    )
    <= pTradeRetUp.get((t1, y)) * pPeriodLen.get((y)),
)
# eqTradeRetLo(trade, year)$mTradeRetLo
model.eqTradeRetLo = Constraint(
    mTradeRetLo,
    rule=lambda model, t1, y: (
        model.vTradeRetiredStock[t1, y] if (t1, y) in mvTradeRetiredStock else 0
    )
    + sum(
        model.vTradeRetiredNewCap[t1, yp, y]
        for yp in year
        if (t1, yp, y) in mvTradeRetiredNewCap
    )
    >= pTradeRetLo.get((t1, y)) * pPeriodLen.get((y)),
)
# eqTradeRetCost(trade, region, year)$mTradeRetCost
model.eqTradeRetCost = Constraint(
    mTradeRetCost,
    rule=lambda model, t1, r, y: model.vTradeRetCost[t1, r, y]
    == pTradeRetCost.get((t1, r, y))
    * (
        (model.vTradeRetiredStock[t1, y] if (t1, y) in mvTradeRetiredStock else 0)
        + sum(
            model.vTradeRetiredNewCap[t1, yp, y]
            for yp in year
            if (t1, yp, y) in mvTradeRetiredNewCap
        )
    ),
)
# eqTechPhaseOut -- end-of-life departure, read off the capacity balance.
# `pTechStockNew` covers exogenous stock APPEARING mid-horizon: an inflow, not
# a phase-out. Without it the residual goes negative and the model is infeasible.
model.eqTechPhaseOut = Constraint(
    mvTechPhaseOut,
    rule=lambda model, t, r, y: model.vTechPhaseOut[t, r, y] * pPeriodLen.get((y))
    == sum(
        model.vTechCap[t, r, yp]
        for yp in year
        if ((yp, y) in mMilestoneNext and (t, r, yp) in mTechSpan)
    )
    - model.vTechCap[t, r, y]
    + (model.vTechNewCap[t, r, y] if (t, r, y) in mTechNew else 0)
    * pPeriodLen.get((y))
    - (
        (model.vTechRetiredStock[t, r, y] if (t, r, y) in mvTechRetiredStock else 0)
        + sum(
            model.vTechRetiredNewCap[t, r, yp, y]
            for yp in year
            if (t, r, yp, y) in mvTechRetiredNewCap
        )
    )
    * pPeriodLen.get((y))
    + pTechStockNew.get((t, r, y)),
)
# eqStoragePhaseOut -- end-of-life departure, read off the capacity balance.
# `max(0, dStock)` covers exogenous stock APPEARING mid-horizon: an inflow, not
# a phase-out. Without it the residual goes negative and the model is infeasible.
model.eqStoragePhaseOut = Constraint(
    mvStoragePhaseOut,
    rule=lambda model, st1, r, y: model.vStoragePhaseOut[st1, r, y] * pPeriodLen.get((y))
    == sum(
        model.vStorageOutCap[st1, r, yp]
        for yp in year
        if ((yp, y) in mMilestoneNext and (st1, r, yp) in mStorageSpan)
    )
    - model.vStorageOutCap[st1, r, y]
    + (model.vStorageOutNewCap[st1, r, y] if (st1, r, y) in mStorageNew else 0)
    * pPeriodLen.get((y))
    - (
        (model.vStorageOutRetiredStock[st1, r, y] if (st1, r, y) in mvStorageRetiredStock else 0)
        + sum(
            model.vStorageOutRetiredNewCap[st1, r, yp, y]
            for yp in year
            if (st1, r, yp, y) in mvStorageRetiredNewCap
        )
    )
    * pPeriodLen.get((y))
    + pStorageOutStockNew.get((st1, r, y)),
)
# eqStorageStockPhaseOut -- what the schedule actually removed.
model.eqStorageStockPhaseOut = Constraint(
    mvStoragePhaseOut,
    rule=lambda model, st1, r, y: model.vStorageStockPhaseOut[st1, r, y]
    * pPeriodLen.get((y))
    == (1 - pStorageOutStockSurv.get((st1, r, y)))
    * sum(
        model.vStorageOutStockCap[st1, r, yp]
        for yp in year
        if ((yp, y) in mMilestoneNext and (st1, r, yp) in mStorageSpan)
    ),
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
            # [payback] the annuity is charged over the COST-RECOVERY period:
            # pTechPayback where set (> 0), otherwise the operational life.
            # eqTechCap deliberately keeps pTechOlife -- the technical life still
            # governs when capacity OPERATES. Ported from GLPK 2026-08-19.
            and (
                (
                    pTechPayback.get((t, r, yp)) > 0
                    and ordYear.get((y))
                    < pTechPayback.get((t, r, yp)) + ordYear.get((yp))
                )
                or (
                    pTechPayback.get((t, r, yp)) <= 0
                    and (
                        ordYear.get((y)) < pTechOlife.get((t, r)) + ordYear.get((yp))
                        or (t, r) in mTechOlifeInf
                    )
                )
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
        * pTimesliceWeight.get((y, s))
        * model.vTechAct[t, r, y, s]
        + sum(
            pTechCvarom.get((t, c, r, y, s))
            * pTimesliceWeight.get((y, s))
            * model.vTechInp[t, c, r, y, s]
            for c in comm
            if (t, c) in mTechInpComm
        )
        + sum(
            pTechCvarom.get((t, c, r, y, s))
            * pTimesliceWeight.get((y, s))
            * model.vTechOut[t, c, r, y, s]
            for c in comm
            if (t, c) in mTechOutComm
        )
        + sum(
            pTechAvarom.get((t, c, r, y, s))
            * pTimesliceWeight.get((y, s))
            * model.vTechAOut[t, c, r, y, s]
            for c in comm
            if (t, c, r, y, s) in mvTechAOut
        )
        + sum(
            pTechAvarom.get((t, c, r, y, s))
            * pTimesliceWeight.get((y, s))
            * model.vTechAInp[t, c, r, y, s]
            for c in comm
            if (t, c, r, y, s) in mvTechAInp
        )
        for s in timeslice
        if (t, s) in mTechTimeslice
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
# eqSupAvaUp(sup, comm, region, year, timeslice)$mSupAvaUp(sup, comm, region, year, timeslice)
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
# eqSupAvaLo(sup, comm, region, year, timeslice)$meqSupAvaLo(sup, comm, region, year, timeslice)
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
        pPeriodLen.get((y)) * pTimesliceWeight.get((y, s)) * model.vSupOut[s1, c, r, y, s]
        for y in year
        for s in timeslice
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
        * pTimesliceWeight.get((y, s))
        * model.vSupOut[s1, c, r, y, s]
        for c in comm
        for s in timeslice
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
# eqDemInp(comm, region, year, timeslice)$mvDemInp(comm, region, year, timeslice)
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
# eqAggOutTot(comm, region, year, timeslice)$mAggOut(comm, region, year, timeslice)
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
            for sp in timeslice
            if (
                (c, r, y, sp) in mvOutTot
                and (s, sp) in mTimesliceParentChildE
                and (cp, sp) in mCommTimeslice
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
# eqEmsFuelTot(comm, region, year, timeslice)$mEmsFuelTot(comm, region, year, timeslice)
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
                for sp in timeslice
                if (c, s, sp) in mCommTimesliceOrParent
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
# eqStorageAInp(stg, comm, region, year, timeslice)$mvStorageAInp(stg, comm, region, year, timeslice)
if verbose:
    print("eqStorageAInp ", end="")
sys.stdout.flush()
model.eqStorageAInp = Constraint(
    mvStorageAInp,
    rule=lambda model, st1, c, r, y, s: model.vStorageAInp[st1, c, r, y, s]
    ==
    # [multi-commodity] each referenced flow follows its OWN role's commodity set,
    # and the two capacity terms leave the commodity sum -- inside it they were
    # multiplied by 1 for a single-commodity storage but would be counted once per
    # commodity now that the roles can differ.
    sum(
        (
            (
                pStorageStg2AInp.get((st1, c, r, y, s))
                * model.vStorageLevel[st1, cp, r, y, s]
            )
            if (st1, c, r, y, s) in mStorageStg2AInp
            else 0
        )
        for cp in comm
        if (st1, cp) in mStorageStgComm
    )
    + sum(
        (
            (
                pStorageCinp2AInp.get((st1, c, r, y, s))
                * model.vStorageInp[st1, cp, r, y, s]
            )
            if (st1, c, r, y, s) in mStorageCinp2AInp
            else 0
        )
        for cp in comm
        if (st1, cp) in mStorageInpComm
    )
    + sum(
        (
            (
                pStorageCout2AInp.get((st1, c, r, y, s))
                * model.vStorageOut[st1, cp, r, y, s]
            )
            if (st1, c, r, y, s) in mStorageCout2AInp
            else 0
        )
        for cp in comm
        if (st1, cp) in mStorageOutComm
    )
    + (
            (pStorageOutCap2AInp.get((st1, c, r, y, s)) * model.vStorageOutCap[st1, r, y])
            if (st1, c, r, y, s) in mStorageOutCap2AInp
            else 0
        )
    + (
            (pStorageOutNCap2AInp.get((st1, c, r, y, s)) * model.vStorageOutNewCap[st1, r, y])
            if (st1, c, r, y, s) in mStorageOutNCap2AInp
            else 0
        )
    + (
            (pStorageInpCap2AInp.get((st1, c, r, y, s)) * model.vStorageInpCap[st1, r, y])
            if (st1, c, r, y, s) in mStorageInpCap2AInp
            else 0
        )
    + (
            (pStorageInpNCap2AInp.get((st1, c, r, y, s)) * model.vStorageInpNewCap[st1, r, y])
            if (st1, c, r, y, s) in mStorageInpNCap2AInp
            else 0
        )
    + (
            (pStorageStgCap2AInp.get((st1, c, r, y, s)) * model.vStorageStgCap[st1, r, y])
            if (st1, c, r, y, s) in mStorageStgCap2AInp
            else 0
        )
    + (
            (pStorageStgNCap2AInp.get((st1, c, r, y, s)) * model.vStorageStgNewCap[st1, r, y])
            if (st1, c, r, y, s) in mStorageStgNCap2AInp
            else 0
        )
    + (
            (pStoragePho2AInp.get((st1, c, r, y, s)) * (model.vStoragePhaseOut[st1, r, y] if (st1, r, y) in mvStoragePhaseOut else 0))
            if (st1, c, r, y, s) in mStoragePho2AInp
            else 0
        )
    + (
            (pStorageRet2AInp.get((st1, c, r, y, s)) * ((model.vStorageOutRetiredStock[st1, r, y] if (st1, r, y) in mvStorageRetiredStock else 0) + sum(model.vStorageOutRetiredNewCap[st1, r, yp, y] for yp in year if (st1, r, yp, y) in mvStorageRetiredNewCap)))
            if (st1, c, r, y, s) in mStorageRet2AInp
            else 0
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
# eqStorageAOut(stg, comm, region, year, timeslice)$mvStorageAOut(stg, comm, region, year, timeslice)
if verbose:
    print("eqStorageAOut ", end="")
sys.stdout.flush()
model.eqStorageAOut = Constraint(
    mvStorageAOut,
    rule=lambda model, st1, c, r, y, s: model.vStorageAOut[st1, c, r, y, s]
    ==
    # [multi-commodity] each referenced flow follows its OWN role's commodity set,
    # and the two capacity terms leave the commodity sum -- inside it they were
    # multiplied by 1 for a single-commodity storage but would be counted once per
    # commodity now that the roles can differ.
    sum(
        (
            (
                pStorageStg2AOut.get((st1, c, r, y, s))
                * model.vStorageLevel[st1, cp, r, y, s]
            )
            if (st1, c, r, y, s) in mStorageStg2AOut
            else 0
        )
        for cp in comm
        if (st1, cp) in mStorageStgComm
    )
    + sum(
        (
            (
                pStorageCinp2AOut.get((st1, c, r, y, s))
                * model.vStorageInp[st1, cp, r, y, s]
            )
            if (st1, c, r, y, s) in mStorageCinp2AOut
            else 0
        )
        for cp in comm
        if (st1, cp) in mStorageInpComm
    )
    + sum(
        (
            (
                pStorageCout2AOut.get((st1, c, r, y, s))
                * model.vStorageOut[st1, cp, r, y, s]
            )
            if (st1, c, r, y, s) in mStorageCout2AOut
            else 0
        )
        for cp in comm
        if (st1, cp) in mStorageOutComm
    )
    + (
            (pStorageOutCap2AOut.get((st1, c, r, y, s)) * model.vStorageOutCap[st1, r, y])
            if (st1, c, r, y, s) in mStorageOutCap2AOut
            else 0
        )
    + (
            (pStorageOutNCap2AOut.get((st1, c, r, y, s)) * model.vStorageOutNewCap[st1, r, y])
            if (st1, c, r, y, s) in mStorageOutNCap2AOut
            else 0
        )
    + (
            (pStorageInpCap2AOut.get((st1, c, r, y, s)) * model.vStorageInpCap[st1, r, y])
            if (st1, c, r, y, s) in mStorageInpCap2AOut
            else 0
        )
    + (
            (pStorageInpNCap2AOut.get((st1, c, r, y, s)) * model.vStorageInpNewCap[st1, r, y])
            if (st1, c, r, y, s) in mStorageInpNCap2AOut
            else 0
        )
    + (
            (pStorageStgCap2AOut.get((st1, c, r, y, s)) * model.vStorageStgCap[st1, r, y])
            if (st1, c, r, y, s) in mStorageStgCap2AOut
            else 0
        )
    + (
            (pStorageStgNCap2AOut.get((st1, c, r, y, s)) * model.vStorageStgNewCap[st1, r, y])
            if (st1, c, r, y, s) in mStorageStgNCap2AOut
            else 0
        )
    + (
            (pStoragePho2AOut.get((st1, c, r, y, s)) * (model.vStoragePhaseOut[st1, r, y] if (st1, r, y) in mvStoragePhaseOut else 0))
            if (st1, c, r, y, s) in mStoragePho2AOut
            else 0
        )
    + (
            (pStorageRet2AOut.get((st1, c, r, y, s)) * ((model.vStorageOutRetiredStock[st1, r, y] if (st1, r, y) in mvStorageRetiredStock else 0) + sum(model.vStorageOutRetiredNewCap[st1, r, yp, y] for yp in year if (st1, r, yp, y) in mvStorageRetiredNewCap)))
            if (st1, c, r, y, s) in mStorageRet2AOut
            else 0
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
# eqStorageLevel(stg, comm, region, year, timeslicep, timeslice)$meqStorageLevel(stg, comm, region, year, timeslicep, timeslice)
if verbose:
    print("eqStorageLevel ", end="")
sys.stdout.flush()
model.eqStorageLevel = Constraint(
    meqStorageLevel,
    rule=lambda model, st1, c, r, y, sp, s: model.vStorageLevel[st1, c, r, y, s]
    == pStorageStartLevel.get((st1, c, r, y, s))
    + (
        (pStorageNCap2Stg.get((st1, c, r, y, s)) * model.vStorageOutNewCap[st1, r, y])
        if (st1, r, y) in mStorageNew
        else 0
    )
    # [multi-commodity] the level is kept in `c`, but what fills and empties it need
    # not be the same thing (ELC in, H2 held, ELC out), so each side sums over its
    # OWN role's commodities. With one commodity all three sets coincide, each sum
    # has a single term and this is the previous equation exactly.
    + sum(
        pStorageInpEff.get((st1, ci, r, y, sp)) * model.vStorageInp[st1, ci, r, y, sp]
        for ci in comm
        if (st1, ci, r, y, sp) in mvStorageInp
    )
    + ((pStorageStgEff.get((st1, c, r, y, s))) ** (pTimesliceShare.get((s))))
    * model.vStorageLevel[st1, c, r, y, sp]
    - sum(
        (model.vStorageOut[st1, co, r, y, sp])
        / (pStorageOutEff.get((st1, co, r, y, sp)))
        for co in comm
        if (st1, co, r, y, sp) in mvStorageOut
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
# eqStorageAfLo(stg, comm, region, year, timeslice)$meqStorageAfLo(stg, comm, region, year, timeslice)
if verbose:
    print("eqStorageAfLo ", end="")
sys.stdout.flush()
model.eqStorageAfLo = Constraint(
    meqStorageAfLo,
    rule=lambda model, st1, c, r, y, s: model.vStorageLevel[st1, c, r, y, s]
    >= pStorageAfLo.get((st1, r, y, s))
    # [2c] the storing side is its own variable where it carries data;
    # elsewhere the duration ratio is inlined onto the output capacity.
    * (
        model.vStorageStgCap[st1, r, y]
        if (st1, r, y) in mStorageStgCap
        else pStorageDurationLo.get((st1, r, y)) * model.vStorageOutCap[st1, r, y]
    )
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
# eqStorageAfUp(stg, comm, region, year, timeslice)$meqStorageAfUp(stg, comm, region, year, timeslice)
if verbose:
    print("eqStorageAfUp ", end="")
sys.stdout.flush()
model.eqStorageAfUp = Constraint(
    meqStorageAfUp,
    rule=lambda model, st1, c, r, y, s: model.vStorageLevel[st1, c, r, y, s]
    <= pStorageAfUp.get((st1, r, y, s))
    # [2c] the storing side is its own variable where it carries data;
    # elsewhere the duration ratio is inlined onto the output capacity.
    * (
        model.vStorageStgCap[st1, r, y]
        if (st1, r, y) in mStorageStgCap
        else pStorageDurationUp.get((st1, r, y)) * model.vStorageOutCap[st1, r, y]
    )
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
# eqStorageOutLevel(stg, comm, region, year, timeslice)$mvStorageLevel(stg, comm, region, year, timeslice)
if verbose:
    print("eqStorageOutLevel ", end="")
sys.stdout.flush()
model.eqStorageOutLevel = Constraint(
    mvStorageLevel,
    rule=lambda model, st1, c, r, y, s: sum(
        (model.vStorageOut[st1, co, r, y, s])
        / (pStorageOutEff.get((st1, co, r, y, s)))
        for co in comm
        if (st1, co, r, y, s) in mvStorageOut
    )
    <= model.vStorageLevel[st1, c, r, y, s],
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqStorageInpUp(stg, comm, region, year, timeslice)$meqStorageInpUp(stg, comm, region, year, timeslice)
if verbose:
    print("eqStorageInpUp ", end="")
sys.stdout.flush()
model.eqStorageInpUp = Constraint(
    meqStorageInpUp,
    rule=lambda model, st1, c, r, y, s: model.vStorageInp[st1, c, r, y, s]
    <= (
        # [2c] the charging side is its own variable where it carries data;
        # elsewhere inp2out is inlined onto the output capacity.
        model.vStorageInpCap[st1, r, y]
        if (st1, r, y) in mStorageInpCap
        else pStorageInp2outUp.get((st1, r, y)) * model.vStorageOutCap[st1, r, y]
    )
    # [rate] capacity is per HOUR, not per timeslice
    * pStorageInpCap2act.get((st1))
    * pTimesliceShare.get((s))
    * pStorageInpAfUp.get((st1, c, r, y, s))
    * prod(
        pStorageWeatherInpAfUp.get((wth1, st1)) * pWeather.get((wth1, r, y, s))
        for wth1 in weather
        if (wth1, st1) in mStorageWeatherInpAfUp
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
# eqStorageInpLo(stg, comm, region, year, timeslice)$meqStorageInpLo(stg, comm, region, year, timeslice)
if verbose:
    print("eqStorageInpLo ", end="")
sys.stdout.flush()
model.eqStorageInpLo = Constraint(
    meqStorageInpLo,
    rule=lambda model, st1, c, r, y, s: model.vStorageInp[st1, c, r, y, s]
    >= (
        # [2c] the charging side is its own variable where it carries data;
        # elsewhere inp2out is inlined onto the output capacity.
        model.vStorageInpCap[st1, r, y]
        if (st1, r, y) in mStorageInpCap
        else pStorageInp2outLo.get((st1, r, y)) * model.vStorageOutCap[st1, r, y]
    )
    # [rate] capacity is per HOUR, not per timeslice
    * pStorageInpCap2act.get((st1))
    * pTimesliceShare.get((s))
    * pStorageInpAfLo.get((st1, c, r, y, s))
    * prod(
        pStorageWeatherInpAfLo.get((wth1, st1)) * pWeather.get((wth1, r, y, s))
        for wth1 in weather
        if (wth1, st1) in mStorageWeatherInpAfLo
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
# eqStorageOutUp(stg, comm, region, year, timeslice)$meqStorageOutUp(stg, comm, region, year, timeslice)
if verbose:
    print("eqStorageOutUp ", end="")
sys.stdout.flush()
model.eqStorageOutUp = Constraint(
    meqStorageOutUp,
    rule=lambda model, st1, c, r, y, s: model.vStorageOut[st1, c, r, y, s]
    <= model.vStorageOutCap[st1, r, y]
    * pStorageOutCap2act.get((st1))
    * pTimesliceShare.get((s))
    * pStorageOutAfUp.get((st1, c, r, y, s))
    * prod(
        pStorageWeatherOutAfUp.get((wth1, st1)) * pWeather.get((wth1, r, y, s))
        for wth1 in weather
        if (wth1, st1) in mStorageWeatherOutAfUp
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
# eqStorageOutLo(stg, comm, region, year, timeslice)$meqStorageOutLo(stg, comm, region, year, timeslice)
if verbose:
    print("eqStorageOutLo ", end="")
sys.stdout.flush()
model.eqStorageOutLo = Constraint(
    meqStorageOutLo,
    rule=lambda model, st1, c, r, y, s: model.vStorageOut[st1, c, r, y, s]
    >= model.vStorageOutCap[st1, r, y]
    * pStorageOutCap2act.get((st1))
    * pTimesliceShare.get((s))
    * pStorageOutAfLo.get((st1, c, r, y, s))
    * prod(
        pStorageWeatherOutAfLo.get((wth1, st1)) * pWeather.get((wth1, r, y, s))
        for wth1 in weather
        if (wth1, st1) in mStorageWeatherOutAfLo
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
# eqStorageOutCap(stg, region, year)$mStorageSpan(stg, region, year)
if verbose:
    print("eqStorageOutCap ", end="")
sys.stdout.flush()
model.eqStorageOutCap = Constraint(
    mStorageSpan,
    rule=lambda model, st1, r, y: model.vStorageOutCap[st1, r, y]
    == model.vStorageOutStockCap[st1, r, y]
    + sum(
        pPeriodLen.get((yp)) * model.vStorageOutNewCap[st1, r, yp]
        - sum(
            model.vStorageOutRetiredNewCap[st1, r, yp, ye] * pPeriodLen.get((ye))
            for ye in year
            if (
                (st1, r, yp, ye) in mvStorageRetiredNewCap
                and ordYear.get((y)) >= ordYear.get((ye))
            )
        )
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
# eqStorageOutCapLo(stg, region, year)$mStorageOutCapLo(stg, region, year)
if verbose:
    print("eqStorageOutCapLo ", end="")
sys.stdout.flush()
# eqStorageInpCap -- the CHARGING side, rated independently of the discharger.
model.eqStorageInpCap = Constraint(
    mStorageInpCap,
    rule=lambda model, st1, r, y: model.vStorageInpCap[st1, r, y]
    == model.vStorageInpStockCap[st1, r, y]
    + sum(
        pPeriodLen.get((yp)) * model.vStorageInpNewCap[st1, r, yp]
        - sum(
            model.vStorageInpRetiredNewCap[st1, r, yp, ye] * pPeriodLen.get((ye))
            for ye in year
            if (
                (st1, r, yp, ye) in mvStorageRetiredNewCap
                and ordYear.get((y)) >= ordYear.get((ye))
            )
        )
        for yp in year
        if (st1, r, yp) in mStorageInpNew
        and ordYear.get((y)) >= ordYear.get((yp))
        and (
            (st1, r) in mStorageOlifeInf
            or ordYear.get((y)) < pStorageOlife.get((st1, r)) + ordYear.get((yp))
        )
    ),
)
model.eqStorageInpCapLo = Constraint(
    mStorageInpCapLo,
    rule=lambda model, st1, r, y: model.vStorageInpCap[st1, r, y]
    >= pStorageInpCapLo.get((st1, r, y)),
)
model.eqStorageInpCapUp = Constraint(
    mStorageInpCapUp,
    rule=lambda model, st1, r, y: model.vStorageInpCap[st1, r, y]
    <= pStorageInpCapUp.get((st1, r, y)),
)
model.eqStorageInpNewCapLo = Constraint(
    mStorageInpNewCapLo,
    rule=lambda model, st1, r, y: model.vStorageInpNewCap[st1, r, y]
    >= pStorageInpNewCapLo.get((st1, r, y)) * pPeriodLen.get((y)),
)
model.eqStorageInpNewCapUp = Constraint(
    mStorageInpNewCapUp,
    rule=lambda model, st1, r, y: model.vStorageInpNewCap[st1, r, y]
    <= pStorageInpNewCapUp.get((st1, r, y)) * pPeriodLen.get((y)),
)
# The inp2out LINK: charging capacity per unit of discharging capacity.
model.eqStorageInp2outLo = Constraint(
    mStorageInp2outLo,
    rule=lambda model, st1, r, y: model.vStorageInpCap[st1, r, y]
    >= pStorageInp2outLo.get((st1, r, y)) * model.vStorageOutCap[st1, r, y],
)
model.eqStorageInp2outUp = Constraint(
    mStorageInp2outUp,
    rule=lambda model, st1, r, y: model.vStorageInpCap[st1, r, y]
    <= pStorageInp2outUp.get((st1, r, y)) * model.vStorageOutCap[st1, r, y],
)
# The inp2stg LINK (charging C-rate, 1/h): charging capacity per unit of
# storing (energy) capacity. No binding default -- the maps carry only rows
# with a declared finite bound where both capacity variables exist.
model.eqStorageInp2stgLo = Constraint(
    mStorageInp2stgLo,
    rule=lambda model, st1, r, y: model.vStorageInpCap[st1, r, y]
    >= pStorageInp2stgLo.get((st1, r, y)) * model.vStorageStgCap[st1, r, y],
)
model.eqStorageInp2stgUp = Constraint(
    mStorageInp2stgUp,
    rule=lambda model, st1, r, y: model.vStorageInpCap[st1, r, y]
    <= pStorageInp2stgUp.get((st1, r, y)) * model.vStorageStgCap[st1, r, y],
)
# eqStorageStgCap -- the STORING side's own capacity, in ENERGY. Exists only
# where the storing part carries data; elsewhere the af bounds inline duration.
model.eqStorageStgCap = Constraint(
    mStorageStgCap,
    rule=lambda model, st1, r, y: model.vStorageStgCap[st1, r, y]
    == model.vStorageStgStockCap[st1, r, y]
    + sum(
        pPeriodLen.get((yp)) * model.vStorageStgNewCap[st1, r, yp]
        - sum(
            model.vStorageStgRetiredNewCap[st1, r, yp, ye] * pPeriodLen.get((ye))
            for ye in year
            if (
                (st1, r, yp, ye) in mvStorageRetiredNewCap
                and ordYear.get((y)) >= ordYear.get((ye))
            )
        )
        for yp in year
        if (st1, r, yp) in mStorageStgNew
        and ordYear.get((y)) >= ordYear.get((yp))
        and (
            (st1, r) in mStorageOlifeInf
            or ordYear.get((y)) < pStorageOlife.get((st1, r)) + ordYear.get((yp))
        )
    ),
)
model.eqStorageStgCapLo = Constraint(
    mStorageStgCapLo,
    rule=lambda model, st1, r, y: model.vStorageStgCap[st1, r, y]
    >= pStorageStgCapLo.get((st1, r, y)),
)
model.eqStorageStgCapUp = Constraint(
    mStorageStgCapUp,
    rule=lambda model, st1, r, y: model.vStorageStgCap[st1, r, y]
    <= pStorageStgCapUp.get((st1, r, y)),
)
model.eqStorageStgNewCapLo = Constraint(
    mStorageStgNewCapLo,
    rule=lambda model, st1, r, y: model.vStorageStgNewCap[st1, r, y]
    >= pStorageStgNewCapLo.get((st1, r, y)) * pPeriodLen.get((y)),
)
model.eqStorageStgNewCapUp = Constraint(
    mStorageStgNewCapUp,
    rule=lambda model, st1, r, y: model.vStorageStgNewCap[st1, r, y]
    <= pStorageStgNewCapUp.get((st1, r, y)) * pPeriodLen.get((y)),
)
# The duration LINK, in hours. `.fx` collapses these two onto each other.
model.eqStorageDurationLo = Constraint(
    mStorageDurationLo,
    rule=lambda model, st1, r, y: model.vStorageStgCap[st1, r, y]
    >= pStorageDurationLo.get((st1, r, y)) * model.vStorageOutCap[st1, r, y],
)
model.eqStorageDurationUp = Constraint(
    mStorageDurationUp,
    rule=lambda model, st1, r, y: model.vStorageStgCap[st1, r, y]
    <= pStorageDurationUp.get((st1, r, y)) * model.vStorageOutCap[st1, r, y],
)
model.eqStorageOutCapLo = Constraint(
    mStorageOutCapLo,
    rule=lambda model, st1, r, y: model.vStorageOutCap[st1, r, y]
    >= pStorageOutCapLo.get((st1, r, y)),
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqStorageOutCapUp(stg, region, year)$mStorageOutCapUp(stg, region, year)
if verbose:
    print("eqStorageOutCapUp ", end="")
sys.stdout.flush()
model.eqStorageOutCapUp = Constraint(
    mStorageOutCapUp,
    rule=lambda model, st1, r, y: model.vStorageOutCap[st1, r, y]
    <= pStorageOutCapUp.get((st1, r, y)),
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqStorageOutNewCapLo(stg, region, year)$mStorageOutNewCapLo(stg, region, year)
if verbose:
    print("eqStorageOutNewCapLo ", end="")
sys.stdout.flush()
model.eqStorageOutNewCapLo = Constraint(
    mStorageOutNewCapLo,
    rule=lambda model, st1, r, y: model.vStorageOutNewCap[st1, r, y]
    >= pStorageOutNewCapLo.get((st1, r, y)) * pPeriodLen.get((y)),
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqStorageOutNewCapUp(stg, region, year)$mStorageOutNewCapUp(stg, region, year)
if verbose:
    print("eqStorageOutNewCapUp ", end="")
sys.stdout.flush()
model.eqStorageOutNewCapUp = Constraint(
    mStorageOutNewCapUp,
    rule=lambda model, st1, r, y: model.vStorageOutNewCap[st1, r, y]
    <= pStorageOutNewCapUp.get((st1, r, y)) * pPeriodLen.get((y)),
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
    == pStorageOutInvcost.get((st1, r, y)) * model.vStorageOutNewCap[st1, r, y]
    # [2c] the storing side's capital cost is per unit of ENERGY.
    + (
        pStorageStgInvcost.get((st1, r, y)) * model.vStorageStgNewCap[st1, r, y]
        if (st1, r, y) in mStorageStgNew
        else 0
    )
    + (
        pStorageInpInvcost.get((st1, r, y)) * model.vStorageInpNewCap[st1, r, y]
        if (st1, r, y) in mStorageInpNew
        else 0
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
# eqStorageEac(stg, region, year)$mStorageEac(stg, region, year)
if verbose:
    print("eqStorageEac ", end="")
sys.stdout.flush()
# [eac-fix] vintaged new-capacity form (pStorageOutEac applies to NEW capacity only).
model.eqStorageEac = Constraint(
    mStorageEac,
    rule=lambda model, st1, r, y: model.vStorageEac[st1, r, y]
    == sum(
        (
            pStorageOutEac.get((st1, r, yp)) * model.vStorageOutNewCap[st1, r, yp]
            # [2c] the storing side annuitises separately, same lifetime.
            + (
                pStorageStgEac.get((st1, r, yp))
                * model.vStorageStgNewCap[st1, r, yp]
                if (st1, r, yp) in mStorageStgNew
                else 0
            )
            + (
                pStorageInpEac.get((st1, r, yp))
                * model.vStorageInpNewCap[st1, r, yp]
                if (st1, r, yp) in mStorageInpNew
                else 0
            )
        )
        for yp in year
        if (
            (st1, r, yp) in mStorageNew
            and ordYear.get((y)) >= ordYear.get((yp))
            # [payback] see eqTechEac.
            and (
                (
                    pStorageOutPayback.get((st1, r, yp)) > 0
                    and ordYear.get((y))
                    < pStorageOutPayback.get((st1, r, yp)) + ordYear.get((yp))
                )
                or (
                    pStorageOutPayback.get((st1, r, yp)) <= 0
                    and (
                        (st1, r) in mStorageOlifeInf
                        or ordYear.get((y))
                        < pStorageOlife.get((st1, r)) + ordYear.get((yp))
                    )
                )
            )
            # [eac-fix] the `pStorageOutInvcost <> 0` guard was REMOVED from the summation
            # condition below. It dropped the annuity entirely when a user supplied
            # `@invcost$eac` without `invcost` (pre-annuitised capex),
            # so the storage was built for FREE -- silently, with an OPTIMAL solve.
            # eqTechEac / eqTradeEac never carried it. pStorageOutEac defaults to 0, so a
            # vintage with no capital cost now contributes a zero-coefficient term instead
            # of being dropped from the sum.
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
    == pStorageOutFixom.get((st1, r, y)) * model.vStorageOutCap[st1, r, y]
    + (
        pStorageStgFixom.get((st1, r, y)) * model.vStorageStgCap[st1, r, y]
        if (st1, r, y) in mStorageStgFixom
        else 0
    )
    + (
        pStorageInpFixom.get((st1, r, y)) * model.vStorageInpCap[st1, r, y]
        if (st1, r, y) in mStorageInpFixom
        else 0
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
# eqStorageVarom(stg, region, year)$mStorageVarom(stg, region, year)
if verbose:
    print("eqStorageVarom ", end="")
sys.stdout.flush()
model.eqStorageVarom = Constraint(
    mStorageVarom,
    rule=lambda model, st1, r, y: model.vStorageVarom[st1, r, y]
    # [multi-commodity] one sum per role: charge cost over the input commodities,
    # discharge cost over the outputs, holding cost over the stored one.
    ==
    sum(
        sum(
            pStorageCostInp.get((st1, r, y, s))
            * pTimesliceWeight.get((y, s))
            * model.vStorageInp[st1, c, r, y, s]
            for s in timeslice
            if (c, s) in mCommTimeslice
        )
        for c in comm
        if (st1, c) in mStorageInpComm
    )
    +
    sum(
        sum(
            pStorageCostOut.get((st1, r, y, s))
            * pTimesliceWeight.get((y, s))
            * model.vStorageOut[st1, c, r, y, s]
            for s in timeslice
            if (c, s) in mCommTimeslice
        )
        for c in comm
        if (st1, c) in mStorageOutComm
    )
    +
    sum(
        sum(
            pStorageCostStore.get((st1, r, y, s))
            * pTimesliceWeight.get((y, s))
            * model.vStorageLevel[st1, c, r, y, s]
            for s in timeslice
            if (c, s) in mCommTimeslice
        )
        for c in comm
        if (st1, c) in mStorageStgComm
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
# Route indexes keyed by endpoint. `mTradeRoutes` is sparse (one entry per
# existing corridor), so mapping an endpoint to its routes lets eqImportTot and
# eqExportTot below visit only routes that exist, instead of iterating
# `trade x region` and filtering.
_routesByDst = {}
_routesBySrc = {}
_routesByTrade = {}
for _t1, _s0, _d0 in mTradeRoutes:
    _routesByDst.setdefault(_d0, []).append((_t1, _s0))
    _routesBySrc.setdefault(_s0, []).append((_t1, _d0))
    _routesByTrade.setdefault(_t1, []).append((_s0, _d0))

# eqImportTot(comm, dst, year, timeslice)$mImport(comm, dst, year, timeslice)
if verbose:
    print("eqImportTot ", end="")
sys.stdout.flush()
model.eqImportTot = Constraint(
    mImport,
    rule=lambda model, c, dst, y, s: model.vImportTot[c, dst, y, s]
    == sum(
        (
            (
                pTradeIrEff.get((t1, src, dst, y, s))
                * model.vTradeIr[t1, c, src, dst, y, s]
            )
            if (t1, c, src, dst, y, s) in mvTradeIr
            else 0
        )
        for (t1, src) in _routesByDst.get(dst, ())
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
# eqExportTot(comm, src, year, timeslice)$mExport(comm, src, year, timeslice)
if verbose:
    print("eqExportTot ", end="")
sys.stdout.flush()
model.eqExportTot = Constraint(
    mExport,
    rule=lambda model, c, src, y, s: model.vExportTot[c, src, y, s]
    == sum(
        (
            model.vTradeIr[t1, c, src, dst, y, s]
            if (t1, c, src, dst, y, s) in mvTradeIr
            else 0
        )
        for (t1, dst) in _routesBySrc.get(src, ())
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
# eqTradeFlowUp(trade, comm, src, dst, year, timeslice)$meqTradeFlowUp(trade, comm, src, dst, year, timeslice)
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
# eqTradeFlowLo(trade, comm, src, dst, year, timeslice)$meqTradeFlowLo(trade, comm, src, dst, year, timeslice)
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
# eqTradeIrAfUp(trade, comm, src, dst, year, timeslice)$meqTradeIrAfUp(trade, comm, src, dst, year, timeslice)
# Relative flow bound: a fraction of the object's own capacity rather than an
# absolute quantity, so one trade object carrying several routes can rate each
# of them. Gated, hence empty unless `af` is declared.
if verbose:
    print("eqTradeIrAfUp ", end="")
sys.stdout.flush()
model.eqTradeIrAfUp = Constraint(
    meqTradeIrAfUp,
    rule=lambda model, t1, c, src, dst, y, s: model.vTradeIr[t1, c, src, dst, y, s]
    <= pTradeIrAfUp.get((t1, src, dst, y, s))
    * pTradeCap2Act.get((t1))
    * model.vTradeCap[t1, y]
    * pTimesliceShare.get((s)),
)
if verbose:
    print(
        datetime.datetime.now().strftime("%H:%M:%S"),
        " (",
        round(time.time() - seconds, 2),
        " s)",
        sep="",
    )
# eqTradeIrAfLo(trade, comm, src, dst, year, timeslice)$meqTradeIrAfLo(trade, comm, src, dst, year, timeslice)
# Relative flow bound: a fraction of the object's own capacity rather than an
# absolute quantity, so one trade object carrying several routes can rate each
# of them. Gated, hence empty unless `af` is declared.
if verbose:
    print("eqTradeIrAfLo ", end="")
sys.stdout.flush()
model.eqTradeIrAfLo = Constraint(
    meqTradeIrAfLo,
    rule=lambda model, t1, c, src, dst, y, s: model.vTradeIr[t1, c, src, dst, y, s]
    >= pTradeIrAfLo.get((t1, src, dst, y, s))
    * pTradeCap2Act.get((t1))
    * model.vTradeCap[t1, y]
    * pTimesliceShare.get((s)),
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
                            pTradeIrMarkup.get((t1, src, r, y, s))
                        )
                        * model.vTradeIr[t1, c, src, r, y, s]
                        * pTimesliceWeight.get((y, s))
                    )
                    if (t1, c, src, r, y, s) in mvTradeIr
                    else 0
                )
                for s in timeslice
                if (t1, s) in mTradeTimeslice
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
    == sum(
        sum(
            sum(
                (
                    (
                        (
                            pTradeIrCost.get((t1, r, dst, y, s))
                            - pTradeIrMarkup.get((t1, r, dst, y, s))
                        )
                        * model.vTradeIr[t1, c, r, dst, y, s]
                        * pTimesliceWeight.get((y, s))
                    )
                    if (t1, c, r, dst, y, s) in mvTradeIr
                    else 0
                )
                for s in timeslice
                if (t1, s) in mTradeTimeslice
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
# eqExportRowUp(expp, comm, region, year, timeslice)$mExportRowUp(expp, comm, region, year, timeslice)
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
# eqExportRowLo(expp, comm, region, year, timeslice)$meqExportRowLo(expp, comm, region, year, timeslice)
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
        pPeriodLen.get((y)) * pTimesliceWeight.get((y, s)) * model.vExportRow[e, c, r, y, s]
        for r in region
        for y in year
        for s in timeslice
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
        * pTimesliceWeight.get((y, s))
        * model.vExportRow[e, c, r, y, s]
        for c in comm
        for s in timeslice
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
# eqImportRowUp(imp, comm, region, year, timeslice)$mImportRowUp(imp, comm, region, year, timeslice)
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
# eqImportRowLo(imp, comm, region, year, timeslice)$meqImportRowLo(imp, comm, region, year, timeslice)
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
        pPeriodLen.get((y)) * pTimesliceWeight.get((y, s)) * model.vImportRow[i, c, r, y, s]
        for r in region
        for y in year
        for s in timeslice
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
        * pTimesliceWeight.get((y, s))
        * model.vImportRow[i, c, r, y, s]
        for c in comm
        for s in timeslice
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
# eqTradeCapFlow(trade, comm, year, timeslice)$meqTradeCapFlow(trade, comm, year, timeslice)
if verbose:
    print("eqTradeCapFlow ", end="")
sys.stdout.flush()
model.eqTradeCapFlow = Constraint(
    meqTradeCapFlow,
    rule=lambda model, t1, c, y, s: pTimesliceShare.get((s))
    * pTradeCap2Act.get((t1))
    * model.vTradeCap[t1, y]
    >= sum(
        model.vTradeIr[t1, c, src, dst, y, s]
        for (src, dst) in _routesByTrade.get(t1, ())
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
    == model.vTradeStockCap[t1, y]
    + sum(
        pPeriodLen.get((yp)) * model.vTradeNewCap[t1, yp]
        - sum(
            model.vTradeRetiredNewCap[t1, yp, ye] * pPeriodLen.get((ye))
            for ye in year
            if (
                (t1, yp, ye) in mvTradeRetiredNewCap
                and ordYear.get((y)) >= ordYear.get((ye))
            )
        )
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
            # [payback] see eqTechEac.
            and (
                (
                    pTradePayback.get((t1, r, yp)) > 0
                    and ordYear.get((y))
                    < pTradePayback.get((t1, r, yp)) + ordYear.get((yp))
                )
                or (
                    pTradePayback.get((t1, r, yp)) <= 0
                    and (
                        ordYear.get((y)) < pTradeOlife.get((t1)) + ordYear.get((yp))
                        or t1 in mTradeOlifeInf
                    )
                )
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
# eqTradeIrAInp(trade, comm, region, year, timeslice)$mvTradeIrAInp(trade, comm, region, year, timeslice)
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
            if ((t1, cp) in mTradeComm and (t1, cp, r, dst, y, s) in mvTradeIr)
        )
        for dst in region
        if (t1, c, r, dst, y, s) in mTradeIrCsrc2Ainp
    )
    + sum(
        pTradeIrCdst2Ainp.get((t1, c, src, r, y, s))
        * sum(
            model.vTradeIr[t1, cp, src, r, y, s]
            for cp in comm
            if ((t1, cp) in mTradeComm and (t1, cp, src, r, y, s) in mvTradeIr)
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
# eqTradeIrAOut(trade, comm, region, year, timeslice)$mvTradeIrAOut(trade, comm, region, year, timeslice)
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
            if ((t1, cp) in mTradeComm and (t1, cp, r, dst, y, s) in mvTradeIr)
        )
        for dst in region
        if (t1, c, r, dst, y, s) in mTradeIrCsrc2Aout
    )
    + sum(
        pTradeIrCdst2Aout.get((t1, c, src, r, y, s))
        * sum(
            model.vTradeIr[t1, cp, src, r, y, s]
            for cp in comm
            if ((t1, cp) in mTradeComm and (t1, cp, src, r, y, s) in mvTradeIr)
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
# eqTradeIrAInpTot(comm, region, year, timeslice)$mvTradeIrAInpTot(comm, region, year, timeslice)
if verbose:
    print("eqTradeIrAInpTot ", end="")
sys.stdout.flush()
model.eqTradeIrAInpTot = Constraint(
    mvTradeIrAInpTot,
    rule=lambda model, c, r, y, s: model.vTradeIrAInpTot[c, r, y, s]
    == sum(
        model.vTradeIrAInp[t1, c, r, y, sp]
        for t1 in trade
        for sp in timeslice
        if ((c, s, sp) in mCommTimesliceOrParent and (t1, c, r, y, sp) in mvTradeIrAInp)
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
# eqTradeIrAOutTot(comm, region, year, timeslice)$mvTradeIrAOutTot(comm, region, year, timeslice)
if verbose:
    print("eqTradeIrAOutTot ", end="")
sys.stdout.flush()
model.eqTradeIrAOutTot = Constraint(
    mvTradeIrAOutTot,
    rule=lambda model, c, r, y, s: model.vTradeIrAOutTot[c, r, y, s]
    == sum(
        model.vTradeIrAOut[t1, c, r, y, sp]
        for t1 in trade
        for sp in timeslice
        if ((c, s, sp) in mCommTimesliceOrParent and (t1, c, r, y, sp) in mvTradeIrAOut)
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
# eqBalLo(comm, region, year, timeslice)$meqBalLo(comm, region, year, timeslice)
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
# eqBalUp(comm, region, year, timeslice)$meqBalUp(comm, region, year, timeslice)
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
# eqBalFx(comm, region, year, timeslice)$meqBalFx(comm, region, year, timeslice)
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
# eqBal(comm, region, year, timeslice)$mvBalance(comm, region, year, timeslice)
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
# eqOutTot(comm, region, year, timeslice)$mvOutTot(comm, region, year, timeslice)
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
        pTimesliceAgg.get((y, s, sp), 0) * model.vOutTot[c, r, y, sp]
        for sp in timeslice
        if ((s, sp) in mTimesliceFamily and (c, r, y, sp) in mvOutTot)
    )
    # [nested-regions] up-aggregation of the immediately-finer region level.
    # Plain sum: regional quantities are extensive, unlike the intensive
    # timeslice values above.
    + sum(
        model.vOutTot[c, rp, y, s]
        for rp in region
        if ((r, rp) in mRegionFamily and (c, rp, y, s) in mvOutTot)
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
# eqInpTot(comm, region, year, timeslice)$mvInpTot(comm, region, year, timeslice)
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
        pTimesliceAgg.get((y, s, sp), 0) * model.vInpTot[c, r, y, sp]
        for sp in timeslice
        if ((s, sp) in mTimesliceFamily and (c, r, y, sp) in mvInpTot)
    )
    # [nested-regions] up-aggregation of the immediately-finer region level.
    # Plain sum: regional quantities are extensive, unlike the intensive
    # timeslice values above.
    + sum(
        model.vInpTot[c, rp, y, s]
        for rp in region
        if ((r, rp) in mRegionFamily and (c, rp, y, s) in mvInpTot)
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
# eqSupOutTot(comm, region, year, timeslice)$mSupOutTot(comm, region, year, timeslice)
if verbose:
    print("eqSupOutTot ", end="")
sys.stdout.flush()
# `vSupOut` is declared over `mSupAva`, which carries the REGION. Gating this
# sum on `mSupComm` alone indexes a region-restricted supply outside its own
# domain: harmless in GLPK/GAMS (full-cube declarations, presolved away) and a
# hard KeyError in Julia/Pyomo, which declare variables over their maps.
model.eqSupOutTot = Constraint(
    mSupOutTot,
    rule=lambda model, c, r, y, s: model.vSupOutTot[c, r, y, s]
    == sum(
        model.vSupOut[s1, c, r, y, s]
        for s1 in sup
        if ((s1, c) in mSupComm and (s1, c, r, y, s) in mSupAva)
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
# eqTechInpTot(comm, region, year, timeslice)$mTechInpTot(comm, region, year, timeslice)
if verbose:
    print("eqTechInpTot ", end="")
sys.stdout.flush()
model.eqTechInpTot = Constraint(
    mTechInpTot,
    rule=lambda model, c, r, y, s: model.vTechInpTot[c, r, y, s]
    == sum(
        (model.vTechInp[t, c, r, y, s] if (t, c, r, y, s) in mvTechInp else 0)
        for t in tech
        if (t, c) in mTechInpCommSameTimeslice
    )
    + sum(
        sum(
            (model.vTechInp[t, c, r, y, sp] if (t, c, r, y, sp) in mvTechInp else 0)
            for sp in timeslice
            if (t, c, sp, s) in mTechInpCommAggTimeslice
        )
        for t in tech
        if (t, c) in mTechInpCommAgg
    )
    + sum(
        (model.vTechAInp[t, c, r, y, s] if (t, c, r, y, s) in mvTechAInp else 0)
        for t in tech
        if (t, c) in mTechAInpCommSameTimeslice
    )
    + sum(
        sum(
            (model.vTechAInp[t, c, r, y, sp] if (t, c, r, y, sp) in mvTechAInp else 0)
            for sp in timeslice
            if (t, c, sp, s) in mTechAInpCommAggTimeslice
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
# eqTechOutTot(comm, region, year, timeslice)$mTechOutTot(comm, region, year, timeslice)
if verbose:
    print("eqTechOutTot ", end="")
sys.stdout.flush()
model.eqTechOutTot = Constraint(
    mTechOutTot,
    rule=lambda model, c, r, y, s: model.vTechOutTot[c, r, y, s]
    == sum(
        (model.vTechOut[t, c, r, y, s] if (t, c, r, y, s) in mvTechOut else 0)
        for t in tech
        if (t, c) in mTechOutCommSameTimeslice
    )
    + sum(
        sum(
            (model.vTechOut[t, c, r, y, sp] if (t, c, r, y, sp) in mvTechOut else 0)
            for sp in timeslice
            if (t, c, sp, s) in mTechOutCommAggTimeslice
        )
        for t in tech
        if (t, c) in mTechOutCommAgg
    )
    + sum(
        (model.vTechAOut[t, c, r, y, s] if (t, c, r, y, s) in mvTechAOut else 0)
        for t in tech
        if (t, c) in mTechAOutCommSameTimeslice
    )
    + sum(
        sum(
            (model.vTechAOut[t, c, r, y, sp] if (t, c, r, y, sp) in mvTechAOut else 0)
            for sp in timeslice
            if (t, c, sp, s) in mTechAOutCommAggTimeslice
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
# eqStorageInpTot(comm, region, year, timeslice)$mStorageInpTot(comm, region, year, timeslice)
if verbose:
    print("eqStorageInpTot ", end="")
sys.stdout.flush()
model.eqStorageInpTot = Constraint(
    mStorageInpTot,
    rule=lambda model, c, r, y, s: model.vStorageInpTot[c, r, y, s]
    == sum(
        (model.vStorageInp[st1, c, r, y, s] if (st1, c, r, y, s) in mvStorageInp else 0)
        for st1 in stg
        if (st1, c) in mStorageInpCommSameTimeslice
    )
    + sum(
        sum(
            (model.vStorageInp[st1, c, r, y, sp] if (st1, c, r, y, sp) in mvStorageInp else 0)
            for sp in timeslice
            if (st1, c, sp, s) in mStorageInpCommAggTimeslice
        )
        for st1 in stg
        if (st1, c) in mStorageInpCommAgg
    )
    + sum(
        (model.vStorageAInp[st1, c, r, y, s] if (st1, c, r, y, s) in mvStorageAInp else 0)
        for st1 in stg
        if (st1, c) in mStorageAInpCommSameTimeslice
    )
    + sum(
        sum(
            (model.vStorageAInp[st1, c, r, y, sp] if (st1, c, r, y, sp) in mvStorageAInp else 0)
            for sp in timeslice
            if (st1, c, sp, s) in mStorageAInpCommAggTimeslice
        )
        for st1 in stg
        if (st1, c) in mStorageAInpCommAgg
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
# eqStorageOutTot(comm, region, year, timeslice)$mStorageOutTot(comm, region, year, timeslice)
if verbose:
    print("eqStorageOutTot ", end="")
sys.stdout.flush()
model.eqStorageOutTot = Constraint(
    mStorageOutTot,
    rule=lambda model, c, r, y, s: model.vStorageOutTot[c, r, y, s]
    == sum(
        (model.vStorageOut[st1, c, r, y, s] if (st1, c, r, y, s) in mvStorageOut else 0)
        for st1 in stg
        if (st1, c) in mStorageOutCommSameTimeslice
    )
    + sum(
        sum(
            (model.vStorageOut[st1, c, r, y, sp] if (st1, c, r, y, sp) in mvStorageOut else 0)
            for sp in timeslice
            if (st1, c, sp, s) in mStorageOutCommAggTimeslice
        )
        for st1 in stg
        if (st1, c) in mStorageOutCommAgg
    )
    + sum(
        (model.vStorageAOut[st1, c, r, y, s] if (st1, c, r, y, s) in mvStorageAOut else 0)
        for st1 in stg
        if (st1, c) in mStorageAOutCommSameTimeslice
    )
    + sum(
        sum(
            (model.vStorageAOut[st1, c, r, y, sp] if (st1, c, r, y, sp) in mvStorageAOut else 0)
            for sp in timeslice
            if (st1, c, sp, s) in mStorageAOutCommAggTimeslice
        )
        for st1 in stg
        if (st1, c) in mStorageAOutCommAgg
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
        pTimesliceWeight.get((y, s))
        * pDummyImportCost.get((c, r, y, s))
        * (model.vDummyImport[c, r, y, s] if (c, r, y, s) in mDummyImport else 0)
        for s in timeslice
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
        pTimesliceWeight.get((y, s))
        * pDummyExportCost.get((c, r, y, s))
        * (model.vDummyExport[c, r, y, s] if (c, r, y, s) in mDummyExport else 0)
        for s in timeslice
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
        * pTimesliceWeight.get((y, s))
        * model.vOutTot[c, r, y, s]
        for s in timeslice
        if ((c, r, y, s) in mvOutTot and (c, s) in mCommTimeslice)
    )
    + sum(
        pTaxCostInp.get((c, r, y, s))
        * pTimesliceWeight.get((y, s))
        * model.vInpTot[c, r, y, s]
        for s in timeslice
        if ((c, r, y, s) in mvInpTot and (c, s) in mCommTimeslice)
    )
    + sum(
        pTaxCostBal.get((c, r, y, s))
        * pTimesliceWeight.get((y, s))
        * model.vBalance[c, r, y, s]
        for s in timeslice
        if ((c, r, y, s) in mvBalance and (c, s) in mCommTimeslice)
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
        * pTimesliceWeight.get((y, s))
        * model.vOutTot[c, r, y, s]
        for s in timeslice
        if ((c, r, y, s) in mvOutTot and (c, s) in mCommTimeslice)
    )
    - sum(
        pSubCostInp.get((c, r, y, s))
        * pTimesliceWeight.get((y, s))
        * model.vInpTot[c, r, y, s]
        for s in timeslice
        if ((c, r, y, s) in mvInpTot and (c, s) in mCommTimeslice)
    )
    - sum(
        pSubCostBal.get((c, r, y, s))
        * pTimesliceWeight.get((y, s))
        * model.vBalance[c, r, y, s]
        for s in timeslice
        if ((c, r, y, s) in mvBalance and (c, s) in mCommTimeslice)
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
        (model.vStorageRetCost[st1, r, y] if (st1, r, y) in mStorageRetCost else 0)
        for st1 in stg
        if (st1, r, y) in mStorageRetCost
    )
    + sum(
        (model.vTradeRetCost[t1, r, y] if (t1, r, y) in mTradeRetCost else 0)
        for t1 in trade
        if (t1, r, y) in mTradeRetCost
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
