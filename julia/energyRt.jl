print(
    "Julia v",
    VERSION,
    "
",
)
using Dates
include("inc1.jl")
flog = open("output/log.csv", "w")
println(flog, "parameter,value,time")
println(
    flog,
    "\"model language\",JULIA,\"",
    Dates.format(now(), "yyyy-mm-dd HH:MM:SS"),
    "\"",
)
println("start ", Dates.format(now(), "HH:MM:SS"))
using JuMP
println(flog, "\"load data\",,\"", Dates.format(now(), "yyyy-mm-dd HH:MM:SS"), "\"")
include("data.jl")
include("inc2.jl")
model = Model();
@variable(model, vTechInv[mTechInv]);
@variable(model, vTechEac[mTechEac]);
@variable(model, vTechRetCost[mTechRetCost]);
@variable(model, vTechFixom[mTechFixom]);
@variable(model, vTechVarom[mTechVarom]);
@variable(model, vSupCost[mvSupCost]);
@variable(model, vEmsFuelTot[mEmsFuelTot]);
@variable(model, vBalance[mvBalance]);
@variable(model, vTotalCost[mvTotalCost]);
@variable(model, vObjective);
@variable(model, vTaxCost[mTaxCost]);
@variable(model, vSubsCost[mSubCost]);
@variable(model, vAggOutTot[mAggOut]);
@variable(model, vDummyImportCost[mDummyImportCost]);
@variable(model, vDummyExportCost[mDummyExportCost]);
@variable(model, vStorageFixom[mStorageFixom]);
@variable(model, vStorageVarom[mStorageVarom]);
@variable(model, vTradeEac[mTradeEac]);
@variable(model, vTradeFixom[mTradeFixom]);
@variable(model, vImportIrCost[mImportIrCost]);
@variable(model, vExportIrCost[mExportIrCost]);
@variable(model, vImportRowCost[mImportRowCost]);
@variable(model, vExportRowCost[mExportRowCost]);
@variable(model, vTechNewCap[mTechNew] >= 0);
@variable(model, vTechRetiredStockCum[mvTechRetiredStock] >= 0);
@variable(model, vTechRetiredStock[mvTechRetiredStock] >= 0);
@variable(model, vTechRetiredNewCap[mvTechRetiredNewCap] >= 0);
@variable(model, vTechCap[mTechSpan] >= 0);
@variable(model, vTechAct[mvTechAct] >= 0);
@variable(model, vTechInp[mvTechInp] >= 0);
@variable(model, vTechOut[mvTechOut] >= 0);
@variable(model, vTechAInp[mvTechAInp] >= 0);
@variable(model, vTechAOut[mvTechAOut] >= 0);
@variable(model, vSupOut[mSupAva] >= 0);
@variable(model, vSupReserve[mvSupReserve] >= 0);
@variable(model, vDemInp[mvDemInp] >= 0);
@variable(model, vOutTot[mvOutTot] >= 0);
@variable(model, vInpTot[mvInpTot] >= 0);
# [agg-rewrite] vInp2Lo/vOut2Lo retired (up-aggregation in eqInpTot/eqOutTot)
@variable(model, vSupOutTot[mSupOutTot] >= 0);
@variable(model, vTechInpTot[mTechInpTot] >= 0);
@variable(model, vTechOutTot[mTechOutTot] >= 0);
@variable(model, vStorageInpTot[mStorageInpTot] >= 0);
@variable(model, vStorageOutTot[mStorageOutTot] >= 0);
@variable(model, vStorageAInp[mvStorageAInp] >= 0);
@variable(model, vStorageAOut[mvStorageAOut] >= 0);
@variable(model, vDummyImport[mDummyImport] >= 0);
@variable(model, vDummyExport[mDummyExport] >= 0);
@variable(model, vStorageInp[mvStorageStore] >= 0);
@variable(model, vStorageOut[mvStorageStore] >= 0);
@variable(model, vStorageStore[mvStorageStore] >= 0);
@variable(model, vStorageInv[mStorageNew] >= 0);
@variable(model, vStorageEac[mStorageEac] >= 0);
@variable(model, vStorageCap[mStorageSpan] >= 0);
@variable(model, vStorageNewCap[mStorageNew] >= 0);
@variable(model, vImportTot[mImport] >= 0);
@variable(model, vExportTot[mExport] >= 0);
@variable(model, vTradeIr[mvTradeIr] >= 0);
@variable(model, vTradeIrAInp[mvTradeIrAInp] >= 0);
@variable(model, vTradeIrAInpTot[mvTradeIrAInpTot] >= 0);
@variable(model, vTradeIrAOut[mvTradeIrAOut] >= 0);
@variable(model, vTradeIrAOutTot[mvTradeIrAOutTot] >= 0);
@variable(model, vExportRowCum[mExpComm] >= 0);
@variable(model, vExportRow[mExportRow] >= 0);
@variable(model, vImportRowCum[mImpComm] >= 0);
@variable(model, vImportRow[mImportRow] >= 0);
@variable(model, vTradeCap[mTradeSpan] >= 0);
@variable(model, vTradeInv[mTradeEac] >= 0);
@variable(model, vTradeNewCap[mTradeNew] >= 0);
@variable(model, vTotalUserCosts[mvTotalUserCosts] >= 0);
# eqTechSng2Sng(tech, region, comm, commp, year, slice)$meqTechSng2Sng(tech, region, comm, commp, year, slice)
print("eqTechSng2Sng(tech, region, comm, commp, year, slice)...")
@constraint(
    model,
    [(t, r, c, cp, y, s) in meqTechSng2Sng],
    vTechInp[(t, c, r, y, s)] * (
        if haskey(pTechCinp2use, (t, c, r, y, s))
            pTechCinp2use[(t, c, r, y, s)]
        else
            pTechCinp2useDef
        end
    ) ==
    (vTechOut[(t, cp, r, y, s)]) / (
        (
            if haskey(pTechUse2cact, (t, cp, r, y, s))
                pTechUse2cact[(t, cp, r, y, s)]
            else
                pTechUse2cactDef
            end
        ) * (
            if haskey(pTechCact2cout, (t, cp, r, y, s))
                pTechCact2cout[(t, cp, r, y, s)]
            else
                pTechCact2coutDef
            end
        )
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqTechGrp2Sng(tech, region, group, commp, year, slice)$meqTechGrp2Sng(tech, region, group, commp, year, slice)
print("eqTechGrp2Sng(tech, region, group, commp, year, slice)...")
@constraint(
    model,
    [(t, r, g, cp, y, s) in meqTechGrp2Sng],
    (
        if haskey(pTechGinp2use, (t, g, r, y, s))
            pTechGinp2use[(t, g, r, y, s)]
        else
            pTechGinp2useDef
        end
    ) * sum(
        (
            if (t, c, r, y, s) in mvTechInp
                (
                    vTechInp[(t, c, r, y, s)] * (
                        if haskey(pTechCinp2ginp, (t, c, r, y, s))
                            pTechCinp2ginp[(t, c, r, y, s)]
                        else
                            pTechCinp2ginpDef
                        end
                    )
                )
            else
                0
            end
        ) for c in comm if (t, g, c) in mTechGroupComm
    ) ==
    (vTechOut[(t, cp, r, y, s)]) / (
        (
            if haskey(pTechUse2cact, (t, cp, r, y, s))
                pTechUse2cact[(t, cp, r, y, s)]
            else
                pTechUse2cactDef
            end
        ) * (
            if haskey(pTechCact2cout, (t, cp, r, y, s))
                pTechCact2cout[(t, cp, r, y, s)]
            else
                pTechCact2coutDef
            end
        )
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqTechSng2Grp(tech, region, comm, groupp, year, slice)$meqTechSng2Grp(tech, region, comm, groupp, year, slice)
print("eqTechSng2Grp(tech, region, comm, groupp, year, slice)...")
@constraint(
    model,
    [(t, r, c, gp, y, s) in meqTechSng2Grp],
    vTechInp[(t, c, r, y, s)] * (
        if haskey(pTechCinp2use, (t, c, r, y, s))
            pTechCinp2use[(t, c, r, y, s)]
        else
            pTechCinp2useDef
        end
    ) == sum(
        (
            if (t, cp, r, y, s) in mvTechOut
                (
                    (vTechOut[(t, cp, r, y, s)]) / (
                        (
                            if haskey(pTechUse2cact, (t, cp, r, y, s))
                                pTechUse2cact[(t, cp, r, y, s)]
                            else
                                pTechUse2cactDef
                            end
                        ) * (
                            if haskey(pTechCact2cout, (t, cp, r, y, s))
                                pTechCact2cout[(t, cp, r, y, s)]
                            else
                                pTechCact2coutDef
                            end
                        )
                    )
                )
            else
                0
            end
        ) for cp in comm if (t, gp, cp) in mTechGroupComm
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqTechGrp2Grp(tech, region, group, groupp, year, slice)$meqTechGrp2Grp(tech, region, group, groupp, year, slice)
print("eqTechGrp2Grp(tech, region, group, groupp, year, slice)...")
@constraint(
    model,
    [(t, r, g, gp, y, s) in meqTechGrp2Grp],
    (
        if haskey(pTechGinp2use, (t, g, r, y, s))
            pTechGinp2use[(t, g, r, y, s)]
        else
            pTechGinp2useDef
        end
    ) * sum(
        (
            if (t, c, r, y, s) in mvTechInp
                (
                    vTechInp[(t, c, r, y, s)] * (
                        if haskey(pTechCinp2ginp, (t, c, r, y, s))
                            pTechCinp2ginp[(t, c, r, y, s)]
                        else
                            pTechCinp2ginpDef
                        end
                    )
                )
            else
                0
            end
        ) for c in comm if (t, g, c) in mTechGroupComm
    ) == sum(
        (
            if (t, cp, r, y, s) in mvTechOut
                (
                    (vTechOut[(t, cp, r, y, s)]) / (
                        (
                            if haskey(pTechUse2cact, (t, cp, r, y, s))
                                pTechUse2cact[(t, cp, r, y, s)]
                            else
                                pTechUse2cactDef
                            end
                        ) * (
                            if haskey(pTechCact2cout, (t, cp, r, y, s))
                                pTechCact2cout[(t, cp, r, y, s)]
                            else
                                pTechCact2coutDef
                            end
                        )
                    )
                )
            else
                0
            end
        ) for cp in comm if (t, gp, cp) in mTechGroupComm
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqTechShareInpLo(tech, region, group, comm, year, slice)$meqTechShareInpLo(tech, region, group, comm, year, slice)
print("eqTechShareInpLo(tech, region, group, comm, year, slice)...")
@constraint(
    model,
    [(t, r, g, c, y, s) in meqTechShareInpLo],
    vTechInp[(t, c, r, y, s)] >=
    (
        if haskey(pTechShareLo, (t, c, r, y, s))
            pTechShareLo[(t, c, r, y, s)]
        else
            pTechShareLoDef
        end
    ) * sum(
        (
            if (t, cp, r, y, s) in mvTechInp
                vTechInp[(t, cp, r, y, s)]
            else
                0
            end
        ) for cp in comm if (t, g, cp) in mTechGroupComm
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqTechShareInpUp(tech, region, group, comm, year, slice)$meqTechShareInpUp(tech, region, group, comm, year, slice)
print("eqTechShareInpUp(tech, region, group, comm, year, slice)...")
@constraint(
    model,
    [(t, r, g, c, y, s) in meqTechShareInpUp],
    vTechInp[(t, c, r, y, s)] <=
    (
        if haskey(pTechShareUp, (t, c, r, y, s))
            pTechShareUp[(t, c, r, y, s)]
        else
            pTechShareUpDef
        end
    ) * sum(
        (
            if (t, cp, r, y, s) in mvTechInp
                vTechInp[(t, cp, r, y, s)]
            else
                0
            end
        ) for cp in comm if (t, g, cp) in mTechGroupComm
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqTechShareOutLo(tech, region, group, comm, year, slice)$meqTechShareOutLo(tech, region, group, comm, year, slice)
print("eqTechShareOutLo(tech, region, group, comm, year, slice)...")
@constraint(
    model,
    [(t, r, g, c, y, s) in meqTechShareOutLo],
    vTechOut[(t, c, r, y, s)] >=
    (
        if haskey(pTechShareLo, (t, c, r, y, s))
            pTechShareLo[(t, c, r, y, s)]
        else
            pTechShareLoDef
        end
    ) * sum(
        (
            if (t, cp, r, y, s) in mvTechOut
                vTechOut[(t, cp, r, y, s)]
            else
                0
            end
        ) for cp in comm if (t, g, cp) in mTechGroupComm
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqTechShareOutUp(tech, region, group, comm, year, slice)$meqTechShareOutUp(tech, region, group, comm, year, slice)
print("eqTechShareOutUp(tech, region, group, comm, year, slice)...")
@constraint(
    model,
    [(t, r, g, c, y, s) in meqTechShareOutUp],
    vTechOut[(t, c, r, y, s)] <=
    (
        if haskey(pTechShareUp, (t, c, r, y, s))
            pTechShareUp[(t, c, r, y, s)]
        else
            pTechShareUpDef
        end
    ) * sum(
        (
            if (t, cp, r, y, s) in mvTechOut
                vTechOut[(t, cp, r, y, s)]
            else
                0
            end
        ) for cp in comm if (t, g, cp) in mTechGroupComm
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqTechAInp(tech, comm, region, year, slice)$mvTechAInp(tech, comm, region, year, slice)
print("eqTechAInp(tech, comm, region, year, slice)...")
@constraint(
    model,
    [(t, c, r, y, s) in mvTechAInp],
    vTechAInp[(t, c, r, y, s)] ==
    (
        if (t, c, r, y, s) in mTechAct2AInp
            (vTechAct[(t, r, y, s)] * (
                if haskey(pTechAct2AInp, (t, c, r, y, s))
                    pTechAct2AInp[(t, c, r, y, s)]
                else
                    pTechAct2AInpDef
                end
            ))
        else
            0
        end
    ) +
    (
        if (t, c, r, y, s) in mTechCap2AInp
            (
                (vTechCap[(t, r, y)] * (
                    if haskey(pTechCap2AInp, (t, c, r, y, s))
                        pTechCap2AInp[(t, c, r, y, s)]
                    else
                        pTechCap2AInpDef
                    end
                )) / ((
                    if haskey(pTechCap2act, (t))
                        pTechCap2act[(t)]
                    else
                        pTechCap2actDef
                    end
                ))
            )
        else
            0
        end
    ) +
    (
        if (t, c, r, y, s) in mTechNCap2AInp
            (vTechNewCap[(t, r, y)] * (
                if haskey(pTechNCap2AInp, (t, c, r, y, s))
                    pTechNCap2AInp[(t, c, r, y, s)]
                else
                    pTechNCap2AInpDef
                end
            ))
        else
            0
        end
    ) +
    sum(
        (
            if haskey(pTechCinp2AInp, (t, c, cp, r, y, s))
                pTechCinp2AInp[(t, c, cp, r, y, s)]
            else
                pTechCinp2AInpDef
            end
        ) * vTechInp[(t, cp, r, y, s)] for
        cp in comm if (t, c, cp, r, y, s) in mTechCinp2AInp
    ) +
    sum(
        (
            if haskey(pTechCout2AInp, (t, c, cp, r, y, s))
                pTechCout2AInp[(t, c, cp, r, y, s)]
            else
                pTechCout2AInpDef
            end
        ) * vTechOut[(t, cp, r, y, s)] for
        cp in comm if (t, c, cp, r, y, s) in mTechCout2AInp
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqTechAOut(tech, comm, region, year, slice)$mvTechAOut(tech, comm, region, year, slice)
print("eqTechAOut(tech, comm, region, year, slice)...")
@constraint(
    model,
    [(t, c, r, y, s) in mvTechAOut],
    vTechAOut[(t, c, r, y, s)] ==
    (
        if (t, c, r, y, s) in mTechAct2AOut
            (vTechAct[(t, r, y, s)] * (
                if haskey(pTechAct2AOut, (t, c, r, y, s))
                    pTechAct2AOut[(t, c, r, y, s)]
                else
                    pTechAct2AOutDef
                end
            ))
        else
            0
        end
    ) +
    (
        if (t, c, r, y, s) in mTechCap2AOut
            (
                (vTechCap[(t, r, y)] * (
                    if haskey(pTechCap2AOut, (t, c, r, y, s))
                        pTechCap2AOut[(t, c, r, y, s)]
                    else
                        pTechCap2AOutDef
                    end
                )) / ((
                    if haskey(pTechCap2act, (t))
                        pTechCap2act[(t)]
                    else
                        pTechCap2actDef
                    end
                ))
            )
        else
            0
        end
    ) +
    (
        if (t, c, r, y, s) in mTechNCap2AOut
            (vTechNewCap[(t, r, y)] * (
                if haskey(pTechNCap2AOut, (t, c, r, y, s))
                    pTechNCap2AOut[(t, c, r, y, s)]
                else
                    pTechNCap2AOutDef
                end
            ))
        else
            0
        end
    ) +
    sum(
        (
            if haskey(pTechCinp2AOut, (t, c, cp, r, y, s))
                pTechCinp2AOut[(t, c, cp, r, y, s)]
            else
                pTechCinp2AOutDef
            end
        ) * vTechInp[(t, cp, r, y, s)] for
        cp in comm if (t, c, cp, r, y, s) in mTechCinp2AOut
    ) +
    sum(
        (
            if haskey(pTechCout2AOut, (t, c, cp, r, y, s))
                pTechCout2AOut[(t, c, cp, r, y, s)]
            else
                pTechCout2AOutDef
            end
        ) * vTechOut[(t, cp, r, y, s)] for
        cp in comm if (t, c, cp, r, y, s) in mTechCout2AOut
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqTechAfLo(tech, region, year, slice)$meqTechAfLo(tech, region, year, slice)
print("eqTechAfLo(tech, region, year, slice)...")
@constraint(
    model,
    [(t, r, y, s) in meqTechAfLo],
    (
        if haskey(pTechAfLo, (t, r, y, s))
            pTechAfLo[(t, r, y, s)]
        else
            pTechAfLoDef
        end
    ) *
    (
        if haskey(pTechCap2act, (t))
            pTechCap2act[(t)]
        else
            pTechCap2actDef
        end
    ) *
    vTechCap[(t, r, y)] *
    (
        if haskey(pSliceShare, (s))
            pSliceShare[(s)]
        else
            pSliceShareDef
        end
    ) *
    prod(
        (
            if haskey(pTechWeatherAfLo, (wth1, t))
                pTechWeatherAfLo[(wth1, t)]
            else
                pTechWeatherAfLoDef
            end
        ) * (
            if haskey(pWeather, (wth1, r, y, s))
                pWeather[(wth1, r, y, s)]
            else
                pWeatherDef
            end
        ) for wth1 in weather if (wth1, t) in mTechWeatherAfLo
    ; init = 1) <= vTechAct[(t, r, y, s)]
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqTechAfUp(tech, region, year, slice)$meqTechAfUp(tech, region, year, slice)
print("eqTechAfUp(tech, region, year, slice)...")
@constraint(
    model,
    [(t, r, y, s) in meqTechAfUp],
    vTechAct[(t, r, y, s)] <=
    (
        if haskey(pTechAfUp, (t, r, y, s))
            pTechAfUp[(t, r, y, s)]
        else
            pTechAfUpDef
        end
    ) *
    (
        if haskey(pTechCap2act, (t))
            pTechCap2act[(t)]
        else
            pTechCap2actDef
        end
    ) *
    vTechCap[(t, r, y)] *
    (
        if haskey(pSliceShare, (s))
            pSliceShare[(s)]
        else
            pSliceShareDef
        end
    ) *
    prod(
        (
            if haskey(pTechWeatherAfUp, (wth1, t))
                pTechWeatherAfUp[(wth1, t)]
            else
                pTechWeatherAfUpDef
            end
        ) * (
            if haskey(pWeather, (wth1, r, y, s))
                pWeather[(wth1, r, y, s)]
            else
                pWeatherDef
            end
        ) for wth1 in weather if (wth1, t) in mTechWeatherAfUp
    ; init = 1)
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqTechAfsLo(tech, region, year, slice)$meqTechAfsLo(tech, region, year, slice)
print("eqTechAfsLo(tech, region, year, slice)...")
@constraint(
    model,
    [(t, r, y, s) in meqTechAfsLo],
    (
        if haskey(pTechAfsLo, (t, r, y, s))
            pTechAfsLo[(t, r, y, s)]
        else
            pTechAfsLoDef
        end
    ) *
    (
        if haskey(pTechCap2act, (t))
            pTechCap2act[(t)]
        else
            pTechCap2actDef
        end
    ) *
    vTechCap[(t, r, y)] *
    (
        if haskey(pSliceShare, (s))
            pSliceShare[(s)]
        else
            pSliceShareDef
        end
    ) *
    prod(
        (
            if haskey(pTechWeatherAfsLo, (wth1, t))
                pTechWeatherAfsLo[(wth1, t)]
            else
                pTechWeatherAfsLoDef
            end
        ) * (
            if haskey(pWeather, (wth1, r, y, s))
                pWeather[(wth1, r, y, s)]
            else
                pWeatherDef
            end
        ) for wth1 in weather if (wth1, t) in mTechWeatherAfsLo
    ; init = 1) <= sum(
        (
            if (t, r, y, sp) in mvTechAct
                vTechAct[(t, r, y, sp)]
            else
                0
            end
        ) for sp in slice if (s, sp) in mSliceParentChildE
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqTechAfsUp(tech, region, year, slice)$meqTechAfsUp(tech, region, year, slice)
print("eqTechAfsUp(tech, region, year, slice)...")
@constraint(
    model,
    [(t, r, y, s) in meqTechAfsUp],
    sum(
        (
            if (t, r, y, sp) in mvTechAct
                vTechAct[(t, r, y, sp)]
            else
                0
            end
        ) for sp in slice if (s, sp) in mSliceParentChildE
    ) <=
    (
        if haskey(pTechAfsUp, (t, r, y, s))
            pTechAfsUp[(t, r, y, s)]
        else
            pTechAfsUpDef
        end
    ) *
    (
        if haskey(pTechCap2act, (t))
            pTechCap2act[(t)]
        else
            pTechCap2actDef
        end
    ) *
    vTechCap[(t, r, y)] *
    (
        if haskey(pSliceShare, (s))
            pSliceShare[(s)]
        else
            pSliceShareDef
        end
    ) *
    prod(
        (
            if haskey(pTechWeatherAfsUp, (wth1, t))
                pTechWeatherAfsUp[(wth1, t)]
            else
                pTechWeatherAfsUpDef
            end
        ) * (
            if haskey(pWeather, (wth1, r, y, s))
                pWeather[(wth1, r, y, s)]
            else
                pWeatherDef
            end
        ) for wth1 in weather if (wth1, t) in mTechWeatherAfsUp
    ; init = 1)
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqTechRampUp(tech, region, year, slice, slicep)$mTechRampUp(tech, region, year, slice, slicep)
print("eqTechRampUp(tech, region, year, slice, slicep)...")
@constraint(
    model,
    [(t, r, y, s, sp) in mTechRampUp],
    (vTechAct[(t, r, y, s)]) / ((
        if haskey(pSliceShare, (s))
            pSliceShare[(s)]
        else
            pSliceShareDef
        end
    )) - (vTechAct[(t, r, y, sp)]) / ((
        if haskey(pSliceShare, (sp))
            pSliceShare[(sp)]
        else
            pSliceShareDef
        end
    )) <=
    (
        (
            if haskey(pSliceShare, (s))
                pSliceShare[(s)]
            else
                pSliceShareDef
            end
        ) *
        (
            if haskey(pTechCap2act, (t))
                pTechCap2act[(t)]
            else
                pTechCap2actDef
            end
        ) *
        (
            if haskey(pTechCap2act, (t))
                pTechCap2act[(t)]
            else
                pTechCap2actDef
            end
        ) *
        vTechCap[(t, r, y)]
    ) / ((
        if haskey(pTechRampUp, (t, r, y, s))
            pTechRampUp[(t, r, y, s)]
        else
            pTechRampUpDef
        end
    ))
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqTechRampDown(tech, region, year, slice, slicep)$mTechRampDown(tech, region, year, slice, slicep)
print("eqTechRampDown(tech, region, year, slice, slicep)...")
@constraint(
    model,
    [(t, r, y, s, sp) in mTechRampDown],
    (vTechAct[(t, r, y, sp)]) / ((
        if haskey(pSliceShare, (sp))
            pSliceShare[(sp)]
        else
            pSliceShareDef
        end
    )) - (vTechAct[(t, r, y, s)]) / ((
        if haskey(pSliceShare, (s))
            pSliceShare[(s)]
        else
            pSliceShareDef
        end
    )) <=
    (
        (
            if haskey(pSliceShare, (s))
                pSliceShare[(s)]
            else
                pSliceShareDef
            end
        ) *
        (
            if haskey(pTechCap2act, (t))
                pTechCap2act[(t)]
            else
                pTechCap2actDef
            end
        ) *
        (
            if haskey(pTechCap2act, (t))
                pTechCap2act[(t)]
            else
                pTechCap2actDef
            end
        ) *
        vTechCap[(t, r, y)]
    ) / ((
        if haskey(pTechRampDown, (t, r, y, s))
            pTechRampDown[(t, r, y, s)]
        else
            pTechRampDownDef
        end
    ))
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqTechActSng(tech, comm, region, year, slice)$meqTechActSng(tech, comm, region, year, slice)
print("eqTechActSng(tech, comm, region, year, slice)...")
@constraint(
    model,
    [(t, c, r, y, s) in meqTechActSng],
    vTechAct[(t, r, y, s)] ==
    (vTechOut[(t, c, r, y, s)]) / ((
        if haskey(pTechCact2cout, (t, c, r, y, s))
            pTechCact2cout[(t, c, r, y, s)]
        else
            pTechCact2coutDef
        end
    ))
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqTechActGrp(tech, group, region, year, slice)$meqTechActGrp(tech, group, region, year, slice)
print("eqTechActGrp(tech, group, region, year, slice)...")
@constraint(
    model,
    [(t, g, r, y, s) in meqTechActGrp],
    vTechAct[(t, r, y, s)] == sum(
        (
            if (t, c, r, y, s) in mvTechOut
                (
                    (vTechOut[(t, c, r, y, s)]) / ((
                        if haskey(pTechCact2cout, (t, c, r, y, s))
                            pTechCact2cout[(t, c, r, y, s)]
                        else
                            pTechCact2coutDef
                        end
                    ))
                )
            else
                0
            end
        ) for c in comm if (t, g, c) in mTechGroupComm
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqTechAfcOutLo(tech, region, comm, year, slice)$meqTechAfcOutLo(tech, region, comm, year, slice)
print("eqTechAfcOutLo(tech, region, comm, year, slice)...")
@constraint(
    model,
    [(t, r, c, y, s) in meqTechAfcOutLo],
    (
        if haskey(pTechCact2cout, (t, c, r, y, s))
            pTechCact2cout[(t, c, r, y, s)]
        else
            pTechCact2coutDef
        end
    ) *
    (
        if haskey(pTechAfcLo, (t, c, r, y, s))
            pTechAfcLo[(t, c, r, y, s)]
        else
            pTechAfcLoDef
        end
    ) *
    (
        if haskey(pTechCap2act, (t))
            pTechCap2act[(t)]
        else
            pTechCap2actDef
        end
    ) *
    vTechCap[(t, r, y)] *
    (
        if haskey(pSliceShare, (s))
            pSliceShare[(s)]
        else
            pSliceShareDef
        end
    ) *
    prod(
        (
            if haskey(pTechWeatherAfcLo, (wth1, t, c))
                pTechWeatherAfcLo[(wth1, t, c)]
            else
                pTechWeatherAfcLoDef
            end
        ) * (
            if haskey(pWeather, (wth1, r, y, s))
                pWeather[(wth1, r, y, s)]
            else
                pWeatherDef
            end
        ) for wth1 in weather if (wth1, t, c) in mTechWeatherAfcLo
    ; init = 1) <= vTechOut[(t, c, r, y, s)]
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqTechAfcOutUp(tech, region, comm, year, slice)$meqTechAfcOutUp(tech, region, comm, year, slice)
print("eqTechAfcOutUp(tech, region, comm, year, slice)...")
@constraint(
    model,
    [(t, r, c, y, s) in meqTechAfcOutUp],
    vTechOut[(t, c, r, y, s)] <=
    (
        if haskey(pTechCact2cout, (t, c, r, y, s))
            pTechCact2cout[(t, c, r, y, s)]
        else
            pTechCact2coutDef
        end
    ) *
    (
        if haskey(pTechAfcUp, (t, c, r, y, s))
            pTechAfcUp[(t, c, r, y, s)]
        else
            pTechAfcUpDef
        end
    ) *
    (
        if haskey(pTechCap2act, (t))
            pTechCap2act[(t)]
        else
            pTechCap2actDef
        end
    ) *
    vTechCap[(t, r, y)] *
    prod(
        (
            if haskey(pTechWeatherAfcUp, (wth1, t, c))
                pTechWeatherAfcUp[(wth1, t, c)]
            else
                pTechWeatherAfcUpDef
            end
        ) * (
            if haskey(pWeather, (wth1, r, y, s))
                pWeather[(wth1, r, y, s)]
            else
                pWeatherDef
            end
        ) for wth1 in weather if (wth1, t, c) in mTechWeatherAfcUp
    ; init = 1)
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqTechAfcInpLo(tech, region, comm, year, slice)$meqTechAfcInpLo(tech, region, comm, year, slice)
print("eqTechAfcInpLo(tech, region, comm, year, slice)...")
@constraint(
    model,
    [(t, r, c, y, s) in meqTechAfcInpLo],
    (
        if haskey(pTechAfcLo, (t, c, r, y, s))
            pTechAfcLo[(t, c, r, y, s)]
        else
            pTechAfcLoDef
        end
    ) *
    (
        if haskey(pTechCap2act, (t))
            pTechCap2act[(t)]
        else
            pTechCap2actDef
        end
    ) *
    vTechCap[(t, r, y)] *
    (
        if haskey(pSliceShare, (s))
            pSliceShare[(s)]
        else
            pSliceShareDef
        end
    ) *
    prod(
        (
            if haskey(pTechWeatherAfcLo, (wth1, t, c))
                pTechWeatherAfcLo[(wth1, t, c)]
            else
                pTechWeatherAfcLoDef
            end
        ) * (
            if haskey(pWeather, (wth1, r, y, s))
                pWeather[(wth1, r, y, s)]
            else
                pWeatherDef
            end
        ) for wth1 in weather if (wth1, t, c) in mTechWeatherAfcLo
    ; init = 1) <= vTechInp[(t, c, r, y, s)]
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqTechAfcInpUp(tech, region, comm, year, slice)$meqTechAfcInpUp(tech, region, comm, year, slice)
print("eqTechAfcInpUp(tech, region, comm, year, slice)...")
@constraint(
    model,
    [(t, r, c, y, s) in meqTechAfcInpUp],
    vTechInp[(t, c, r, y, s)] <=
    (
        if haskey(pTechAfcUp, (t, c, r, y, s))
            pTechAfcUp[(t, c, r, y, s)]
        else
            pTechAfcUpDef
        end
    ) *
    (
        if haskey(pTechCap2act, (t))
            pTechCap2act[(t)]
        else
            pTechCap2actDef
        end
    ) *
    vTechCap[(t, r, y)] *
    (
        if haskey(pSliceShare, (s))
            pSliceShare[(s)]
        else
            pSliceShareDef
        end
    ) *
    prod(
        (
            if haskey(pTechWeatherAfcUp, (wth1, t, c))
                pTechWeatherAfcUp[(wth1, t, c)]
            else
                pTechWeatherAfcUpDef
            end
        ) * (
            if haskey(pWeather, (wth1, r, y, s))
                pWeather[(wth1, r, y, s)]
            else
                pWeatherDef
            end
        ) for wth1 in weather if (wth1, t, c) in mTechWeatherAfcUp
    ; init = 1)
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqTechCap(tech, region, year)$mTechSpan(tech, region, year)
print("eqTechCap(tech, region, year)...")
@constraint(
    model,
    [(t, r, y) in mTechSpan],
    vTechCap[(t, r, y)] ==
    (
        if haskey(pTechStock, (t, r, y))
            pTechStock[(t, r, y)]
        else
            pTechStockDef
        end
    ) - (
        if (t, r, y) in mvTechRetiredStock
            vTechRetiredStockCum[(t, r, y)]
        else
            0
        end
    ) + sum(
        (
            if haskey(pPeriodLen, (yp))
                pPeriodLen[(yp)]
            else
                pPeriodLenDef
            end
        ) * (
            vTechNewCap[(t, r, yp)] - sum(
                vTechRetiredNewCap[(t, r, yp, ye)] for ye in year if
                ((t, r, yp, ye) in mvTechRetiredNewCap && ordYear[(y)] >= ordYear[(ye)])
            )
        ) for yp in year if (
            (t, r, yp) in mTechNew &&
            ordYear[(y)] >= ordYear[(yp)] &&
            (
                ordYear[(y)] < (
                    if haskey(pTechOlife, (t, r))
                        pTechOlife[(t, r)]
                    else
                        pTechOlifeDef
                    end
                ) + ordYear[(yp)] || (t, r) in mTechOlifeInf
            )
        )
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqTechCapLo(tech, region, year)$mTechCapLo(tech, region, year)
print("eqTechCapLo(tech, region, year)...")
@constraint(
    model,
    [(t, r, y) in mTechCapLo],
    vTechCap[(t, r, y)] >= (
        if haskey(pTechCapLo, (t, r, y))
            pTechCapLo[(t, r, y)]
        else
            pTechCapLoDef
        end
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqTechCapUp(tech, region, year)$mTechCapUp(tech, region, year)
print("eqTechCapUp(tech, region, year)...")
@constraint(
    model,
    [(t, r, y) in mTechCapUp],
    vTechCap[(t, r, y)] <= (
        if haskey(pTechCapUp, (t, r, y))
            pTechCapUp[(t, r, y)]
        else
            pTechCapUpDef
        end
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqTechNewCapLo(tech, region, year)$mTechNewCapLo(tech, region, year)
print("eqTechNewCapLo(tech, region, year)...")
@constraint(
    model,
    [(t, r, y) in mTechNewCapLo],
    vTechNewCap[(t, r, y)] >=
    (
        if haskey(pTechNewCapLo, (t, r, y))
            pTechNewCapLo[(t, r, y)]
        else
            pTechNewCapLoDef
        end
    ) * (
        if haskey(pPeriodLen, (y))
            pPeriodLen[(y)]
        else
            pPeriodLenDef
        end
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqTechNewCapUp(tech, region, year)$mTechNewCapUp(tech, region, year)
print("eqTechNewCapUp(tech, region, year)...")
@constraint(
    model,
    [(t, r, y) in mTechNewCapUp],
    vTechNewCap[(t, r, y)] <=
    (
        if haskey(pTechNewCapUp, (t, r, y))
            pTechNewCapUp[(t, r, y)]
        else
            pTechNewCapUpDef
        end
    ) * (
        if haskey(pPeriodLen, (y))
            pPeriodLen[(y)]
        else
            pPeriodLenDef
        end
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqTechRetiredNewCap(tech, region, year)$meqTechRetiredNewCap(tech, region, year)
print("eqTechRetiredNewCap(tech, region, year)...")
@constraint(
    model,
    [(t, r, y) in meqTechRetiredNewCap],
    sum(
        vTechRetiredNewCap[(t, r, y, yp)] * (
            if haskey(pPeriodLen, (yp))
                pPeriodLen[(yp)]
            else
                pPeriodLenDef
            end
        ) for yp in year if (t, r, y, yp) in mvTechRetiredNewCap
    ) <= vTechNewCap[(t, r, y)] * (
        if haskey(pPeriodLen, (y))
            pPeriodLen[(y)]
        else
            pPeriodLenDef
        end
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqTechRetiredStockCum(tech, region, year)$mvTechRetiredStock(tech, region, year)
print("eqTechRetiredStockCum(tech, region, year)...")
@constraint(
    model,
    [(t, r, y) in mvTechRetiredStock],
    vTechRetiredStockCum[(t, r, y)] <= (
        if haskey(pTechStock, (t, r, y))
            pTechStock[(t, r, y)]
        else
            pTechStockDef
        end
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqTechRetiredStock(tech, region, year)$mvTechRetiredStock(tech, region, year)
print("eqTechRetiredStock(tech, region, year)...")
@constraint(
    model,
    [(t, r, y) in mvTechRetiredStock],
    vTechRetiredStock[(t, r, y)] * (
        if haskey(pPeriodLen, (y))
            pPeriodLen[(y)]
        else
            pPeriodLenDef
        end
    ) ==
    vTechRetiredStockCum[(t, r, y)] -
    sum(vTechRetiredStockCum[(t, r, yp)] for yp in year if (yp, y) in mMilestoneNext)
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqTechRetUp(tech, region, year)$mTechRetUp(tech, region, year)
print("eqTechRetUp(tech, region, year)...")
@constraint(
    model,
    [(t, r, y) in mTechRetUp],
    (
        if (t, r, y) in mvTechRetiredStock
            vTechRetiredStock[(t, r, y)]
        else
            0
        end
    ) + sum(
        vTechRetiredNewCap[(t, r, y, yp)] for
        yp in year if (t, r, y, yp) in mvTechRetiredNewCap
    ) <= (
        if haskey(pTechRetUp, (t, r, y))
            pTechRetUp[(t, r, y)]
        else
            pTechRetUpDef
        end
    ) * (
        if haskey(pPeriodLen, (y))
            pPeriodLen[(y)]
        else
            pPeriodLenDef
        end
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqTechRetLo(tech, region, year)$mTechRetLo(tech, region, year)
print("eqTechRetLo(tech, region, year)...")
@constraint(
    model,
    [(t, r, y) in mTechRetLo],
    (
        if (t, r, y) in mvTechRetiredStock
            vTechRetiredStock[(t, r, y)]
        else
            0
        end
    ) + sum(
        vTechRetiredNewCap[(t, r, y, yp)] for
        yp in year if (t, r, y, yp) in mvTechRetiredNewCap
    ) >= (
        if haskey(pTechRetLo, (t, r, y))
            pTechRetLo[(t, r, y)]
        else
            pTechRetLoDef
        end
    ) * (
        if haskey(pPeriodLen, (y))
            pPeriodLen[(y)]
        else
            pPeriodLenDef
        end
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqTechRetCost(tech, region, year)$mTechRetCost(tech, region, year)
print("eqTechRetCost(tech, region, year)...")
@constraint(
    model,
    [(t, r, y) in mTechRetCost],
    vTechRetCost[(t, r, y)] ==
    (
        if haskey(pTechRetCost, (t, r, y))
            pTechRetCost[(t, r, y)]
        else
            pTechRetCostDef
        end
    ) * (
        if (t, r, y) in mvTechRetiredStock
            vTechRetiredStock[(t, r, y)]
        else
            0
        end
    ) + sum(
        (
            if haskey(pTechRetCost, (t, r, y))
                pTechRetCost[(t, r, y)]
            else
                pTechRetCostDef
            end
        ) * (
            if (t, r, yp, y) in mvTechRetiredNewCap
                vTechRetiredNewCap[(t, r, yp, y)]
            else
                0
            end
        ) for yp in year if (t, r, yp, y) in mvTechRetiredNewCap
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqTechEac(tech, region, year)$mTechSpan(tech, region, year)
print("eqTechEac(tech, region, year)...")
# [eac-fix] vintaged new-capacity form (pTechEac applies to NEW capacity only)
@constraint(
    model,
    [(t, r, y) in mTechEac],
    vTechEac[(t, r, y)] == sum(
        (
            if haskey(pTechEac, (t, r, yp))
                pTechEac[(t, r, yp)]
            else
                pTechEacDef
            end
        ) * (
            vTechNewCap[(t, r, yp)] - sum(
                vTechRetiredNewCap[(t, r, yp, ye)] for ye in year if
                ((t, r, yp, ye) in mvTechRetiredNewCap && ordYear[(y)] >= ordYear[(ye)]);
                init = 0
            )
        ) for yp in year if (
            (t, r, yp) in mTechNew &&
            ordYear[(y)] >= ordYear[(yp)] &&
            (
                ordYear[(y)] < (
                    if haskey(pTechOlife, (t, r))
                        pTechOlife[(t, r)]
                    else
                        pTechOlifeDef
                    end
                ) + ordYear[(yp)] || (t, r) in mTechOlifeInf
            )
        );
        init = 0
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqTechInv(tech, region, year)$mTechInv(tech, region, year)
print("eqTechInv(tech, region, year)...")
@constraint(
    model,
    [(t, r, y) in mTechInv],
    vTechInv[(t, r, y)] ==
    (
        if haskey(pTechInvcost, (t, r, y))
            pTechInvcost[(t, r, y)]
        else
            pTechInvcostDef
        end
    ) * vTechNewCap[(t, r, y)]
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqTechFixom(tech, region, year)$mTechFixom(tech, region, year)
print("eqTechFixom(tech, region, year)...")
@constraint(
    model,
    [(t, r, y) in mTechFixom],
    vTechFixom[(t, r, y)] == (
        if haskey(pTechFixom, (t, r, y))
            pTechFixom[(t, r, y)]
        else
            pTechFixomDef
        end
    ) * vTechCap[(t, r, y)]
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqTechVarom(tech, region, year)$mTechVarom(tech, region, year)
print("eqTechVarom(tech, region, year)...")
@constraint(
    model,
    [(t, r, y) in mTechVarom],
    vTechVarom[(t, r, y)] == sum(
        (
            if haskey(pTechVarom, (t, r, y, s))
                pTechVarom[(t, r, y, s)]
            else
                pTechVaromDef
            end
        ) *
        (
            if haskey(pSliceWeight, (y, s))
                pSliceWeight[(y, s)]
            else
                pSliceWeightDef
            end
        ) *
        vTechAct[(t, r, y, s)] +
        sum(
            (
                if haskey(pTechCvarom, (t, c, r, y, s))
                    pTechCvarom[(t, c, r, y, s)]
                else
                    pTechCvaromDef
                end
            ) *
            (
                if haskey(pSliceWeight, (y, s))
                    pSliceWeight[(y, s)]
                else
                    pSliceWeightDef
                end
            ) *
            vTechInp[(t, c, r, y, s)] for c in comm if (t, c) in mTechInpComm
        ) +
        sum(
            (
                if haskey(pTechCvarom, (t, c, r, y, s))
                    pTechCvarom[(t, c, r, y, s)]
                else
                    pTechCvaromDef
                end
            ) *
            (
                if haskey(pSliceWeight, (y, s))
                    pSliceWeight[(y, s)]
                else
                    pSliceWeightDef
                end
            ) *
            vTechOut[(t, c, r, y, s)] for c in comm if (t, c) in mTechOutComm
        ) +
        sum(
            (
                if haskey(pTechAvarom, (t, c, r, y, s))
                    pTechAvarom[(t, c, r, y, s)]
                else
                    pTechAvaromDef
                end
            ) *
            (
                if haskey(pSliceWeight, (y, s))
                    pSliceWeight[(y, s)]
                else
                    pSliceWeightDef
                end
            ) *
            vTechAOut[(t, c, r, y, s)] for c in comm if (t, c, r, y, s) in mvTechAOut
        ) +
        sum(
            (
                if haskey(pTechAvarom, (t, c, r, y, s))
                    pTechAvarom[(t, c, r, y, s)]
                else
                    pTechAvaromDef
                end
            ) *
            (
                if haskey(pSliceWeight, (y, s))
                    pSliceWeight[(y, s)]
                else
                    pSliceWeightDef
                end
            ) *
            vTechAInp[(t, c, r, y, s)] for c in comm if (t, c, r, y, s) in mvTechAInp
        ) for s in slice if (t, s) in mTechSlice
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqSupAvaUp(sup, comm, region, year, slice)$mSupAvaUp(sup, comm, region, year, slice)
print("eqSupAvaUp(sup, comm, region, year, slice)...")
@constraint(
    model,
    [(s1, c, r, y, s) in mSupAvaUp],
    vSupOut[(s1, c, r, y, s)] <=
    (
        if haskey(pSupAvaUp, (s1, c, r, y, s))
            pSupAvaUp[(s1, c, r, y, s)]
        else
            pSupAvaUpDef
        end
    ) * prod(
        (
            if haskey(pSupWeatherUp, (wth1, s1))
                pSupWeatherUp[(wth1, s1)]
            else
                pSupWeatherUpDef
            end
        ) * (
            if haskey(pWeather, (wth1, r, y, s))
                pWeather[(wth1, r, y, s)]
            else
                pWeatherDef
            end
        ) for wth1 in weather if (wth1, s1) in mSupWeatherUp
    ; init = 1)
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqSupAvaLo(sup, comm, region, year, slice)$meqSupAvaLo(sup, comm, region, year, slice)
print("eqSupAvaLo(sup, comm, region, year, slice)...")
@constraint(
    model,
    [(s1, c, r, y, s) in meqSupAvaLo],
    vSupOut[(s1, c, r, y, s)] >=
    (
        if haskey(pSupAvaLo, (s1, c, r, y, s))
            pSupAvaLo[(s1, c, r, y, s)]
        else
            pSupAvaLoDef
        end
    ) * prod(
        (
            if haskey(pSupWeatherLo, (wth1, s1))
                pSupWeatherLo[(wth1, s1)]
            else
                pSupWeatherLoDef
            end
        ) * (
            if haskey(pWeather, (wth1, r, y, s))
                pWeather[(wth1, r, y, s)]
            else
                pWeatherDef
            end
        ) for wth1 in weather if (wth1, s1) in mSupWeatherLo
    ; init = 1)
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqSupReserve(sup, comm, region)$mvSupReserve(sup, comm, region)
print("eqSupReserve(sup, comm, region)...")
@constraint(
    model,
    [(s1, c, r) in mvSupReserve],
    vSupReserve[(s1, c, r)] == sum(
        (
            if haskey(pPeriodLen, (y))
                pPeriodLen[(y)]
            else
                pPeriodLenDef
            end
        ) *
        (
            if haskey(pSliceWeight, (y, s))
                pSliceWeight[(y, s)]
            else
                pSliceWeightDef
            end
        ) *
        vSupOut[(s1, c, r, y, s)] for y in year for
        s in slice if (s1, c, r, y, s) in mSupAva
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqSupReserveUp(sup, comm, region)$mSupReserveUp(sup, comm, region)
print("eqSupReserveUp(sup, comm, region)...")
@constraint(
    model,
    [(s1, c, r) in mSupReserveUp],
    vSupReserve[(s1, c, r)] <= (
        if haskey(pSupReserveUp, (s1, c, r))
            pSupReserveUp[(s1, c, r)]
        else
            pSupReserveUpDef
        end
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqSupReserveLo(sup, comm, region)$meqSupReserveLo(sup, comm, region)
print("eqSupReserveLo(sup, comm, region)...")
@constraint(
    model,
    [(s1, c, r) in meqSupReserveLo],
    vSupReserve[(s1, c, r)] >= (
        if haskey(pSupReserveLo, (s1, c, r))
            pSupReserveLo[(s1, c, r)]
        else
            pSupReserveLoDef
        end
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqSupCost(sup, region, year)$mvSupCost(sup, region, year)
print("eqSupCost(sup, region, year)...")
@constraint(
    model,
    [(s1, r, y) in mvSupCost],
    vSupCost[(s1, r, y)] == sum(
        (
            if haskey(pSupCost, (s1, c, r, y, s))
                pSupCost[(s1, c, r, y, s)]
            else
                pSupCostDef
            end
        ) *
        (
            if haskey(pSliceWeight, (y, s))
                pSliceWeight[(y, s)]
            else
                pSliceWeightDef
            end
        ) *
        vSupOut[(s1, c, r, y, s)] for c in comm for
        s in slice if (s1, c, r, y, s) in mSupAva
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqDemInp(comm, region, year, slice)$mvDemInp(comm, region, year, slice)
print("eqDemInp(comm, region, year, slice)...")
@constraint(
    model,
    [(c, r, y, s) in mvDemInp],
    vDemInp[(c, r, y, s)] == sum((
        if haskey(pDemand, (d, c, r, y, s))
            pDemand[(d, c, r, y, s)]
        else
            pDemandDef
        end
    ) for d in dem if (d, c) in mDemComm)
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqAggOutTot(comm, region, year, slice)$mAggOut(comm, region, year, slice)
print("eqAggOutTot(comm, region, year, slice)...")
@constraint(
    model,
    [(c, r, y, s) in mAggOut],
    vAggOutTot[(c, r, y, s)] == sum(
        (
            if haskey(pAggregateFactor, (c, cp))
                pAggregateFactor[(c, cp)]
            else
                pAggregateFactorDef
            end
        ) * sum(
            (
                if (cp, r, y, sp) in mvOutTot
                    vOutTot[(cp, r, y, sp)]
                else
                    0
                end
            ) for sp in slice if (
                (c, r, y, sp) in mvOutTot &&
                (s, sp) in mSliceParentChildE &&
                (cp, sp) in mCommSlice
            )
        ) for cp in comm if (c, cp) in mAggregateFactor
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqEmsFuelTot(comm, region, year, slice)$mEmsFuelTot(comm, region, year, slice)
print("eqEmsFuelTot(comm, region, year, slice)...")
@constraint(
    model,
    [(c, r, y, s) in mEmsFuelTot],
    vEmsFuelTot[(c, r, y, s)] == sum(
        (
            if haskey(pEmissionFactor, (c, cp))
                pEmissionFactor[(c, cp)]
            else
                pEmissionFactorDef
            end
        ) * sum(
            (
                if haskey(pTechEmisComm, (t, cp))
                    pTechEmisComm[(t, cp)]
                else
                    pTechEmisCommDef
                end
            ) * sum(
                (
                    if (t, c, cp, r, y, sp) in mTechEmsFuel
                        vTechInp[(t, cp, r, y, sp)]
                    else
                        0
                    end
                ) for sp in slice if (c, s, sp) in mCommSliceOrParent
            ) for t in tech if (t, cp) in mTechInpComm
        ) for cp in comm if ((
            if haskey(pEmissionFactor, (c, cp))
                pEmissionFactor[(c, cp)]
            else
                pEmissionFactorDef
            end
        ) > 0)
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqStorageAInp(stg, comm, region, year, slice)$mvStorageAInp(stg, comm, region, year, slice)
print("eqStorageAInp(stg, comm, region, year, slice)...")
@constraint(
    model,
    [(st1, c, r, y, s) in mvStorageAInp],
    vStorageAInp[(st1, c, r, y, s)] == sum(
        (
            if (st1, c, r, y, s) in mStorageStg2AInp
                (
                    (
                        if haskey(pStorageStg2AInp, (st1, c, r, y, s))
                            pStorageStg2AInp[(st1, c, r, y, s)]
                        else
                            pStorageStg2AInpDef
                        end
                    ) * vStorageStore[(st1, cp, r, y, s)]
                )
            else
                0
            end
        ) +
        (
            if (st1, c, r, y, s) in mStorageCinp2AInp
                (
                    (
                        if haskey(pStorageCinp2AInp, (st1, c, r, y, s))
                            pStorageCinp2AInp[(st1, c, r, y, s)]
                        else
                            pStorageCinp2AInpDef
                        end
                    ) * vStorageInp[(st1, cp, r, y, s)]
                )
            else
                0
            end
        ) +
        (
            if (st1, c, r, y, s) in mStorageCout2AInp
                (
                    (
                        if haskey(pStorageCout2AInp, (st1, c, r, y, s))
                            pStorageCout2AInp[(st1, c, r, y, s)]
                        else
                            pStorageCout2AInpDef
                        end
                    ) * vStorageOut[(st1, cp, r, y, s)]
                )
            else
                0
            end
        ) +
        (
            if (st1, c, r, y, s) in mStorageCap2AInp
                (
                    (
                        if haskey(pStorageCap2AInp, (st1, c, r, y, s))
                            pStorageCap2AInp[(st1, c, r, y, s)]
                        else
                            pStorageCap2AInpDef
                        end
                    ) * vStorageCap[(st1, r, y)]
                )
            else
                0
            end
        ) +
        (
            if (st1, c, r, y, s) in mStorageNCap2AInp
                (
                    (
                        if haskey(pStorageNCap2AInp, (st1, c, r, y, s))
                            pStorageNCap2AInp[(st1, c, r, y, s)]
                        else
                            pStorageNCap2AInpDef
                        end
                    ) * vStorageNewCap[(st1, r, y)]
                )
            else
                0
            end
        ) for cp in comm if (st1, cp) in mStorageComm
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqStorageAOut(stg, comm, region, year, slice)$mvStorageAOut(stg, comm, region, year, slice)
print("eqStorageAOut(stg, comm, region, year, slice)...")
@constraint(
    model,
    [(st1, c, r, y, s) in mvStorageAOut],
    vStorageAOut[(st1, c, r, y, s)] == sum(
        (
            if (st1, c, r, y, s) in mStorageStg2AOut
                (
                    (
                        if haskey(pStorageStg2AOut, (st1, c, r, y, s))
                            pStorageStg2AOut[(st1, c, r, y, s)]
                        else
                            pStorageStg2AOutDef
                        end
                    ) * vStorageStore[(st1, cp, r, y, s)]
                )
            else
                0
            end
        ) +
        (
            if (st1, c, r, y, s) in mStorageCinp2AOut
                (
                    (
                        if haskey(pStorageCinp2AOut, (st1, c, r, y, s))
                            pStorageCinp2AOut[(st1, c, r, y, s)]
                        else
                            pStorageCinp2AOutDef
                        end
                    ) * vStorageInp[(st1, cp, r, y, s)]
                )
            else
                0
            end
        ) +
        (
            if (st1, c, r, y, s) in mStorageCout2AOut
                (
                    (
                        if haskey(pStorageCout2AOut, (st1, c, r, y, s))
                            pStorageCout2AOut[(st1, c, r, y, s)]
                        else
                            pStorageCout2AOutDef
                        end
                    ) * vStorageOut[(st1, cp, r, y, s)]
                )
            else
                0
            end
        ) +
        (
            if (st1, c, r, y, s) in mStorageCap2AOut
                (
                    (
                        if haskey(pStorageCap2AOut, (st1, c, r, y, s))
                            pStorageCap2AOut[(st1, c, r, y, s)]
                        else
                            pStorageCap2AOutDef
                        end
                    ) * vStorageCap[(st1, r, y)]
                )
            else
                0
            end
        ) +
        (
            if (st1, c, r, y, s) in mStorageNCap2AOut
                (
                    (
                        if haskey(pStorageNCap2AOut, (st1, c, r, y, s))
                            pStorageNCap2AOut[(st1, c, r, y, s)]
                        else
                            pStorageNCap2AOutDef
                        end
                    ) * vStorageNewCap[(st1, r, y)]
                )
            else
                0
            end
        ) for cp in comm if (st1, cp) in mStorageComm
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqStorageStore(stg, comm, region, year, slicep, slice)$meqStorageStore(stg, comm, region, year, slicep, slice)
print("eqStorageStore(stg, comm, region, year, slicep, slice)...")
@constraint(
    model,
    [(st1, c, r, y, sp, s) in meqStorageStore],
    vStorageStore[(st1, c, r, y, s)] ==
    (
        if haskey(pStorageCharge, (st1, c, r, y, s))
            pStorageCharge[(st1, c, r, y, s)]
        else
            pStorageChargeDef
        end
    ) +
    (
        if (st1, r, y) in mStorageNew
            (
                (
                    if haskey(pStorageNCap2Stg, (st1, c, r, y, s))
                        pStorageNCap2Stg[(st1, c, r, y, s)]
                    else
                        pStorageNCap2StgDef
                    end
                ) * vStorageNewCap[(st1, r, y)]
            )
        else
            0
        end
    ) +
    (
        if haskey(pStorageInpEff, (st1, c, r, y, sp))
            pStorageInpEff[(st1, c, r, y, sp)]
        else
            pStorageInpEffDef
        end
    ) * vStorageInp[(st1, c, r, y, sp)] +
    (
        ((
            if haskey(pStorageStgEff, (st1, c, r, y, s))
                pStorageStgEff[(st1, c, r, y, s)]
            else
                pStorageStgEffDef
            end
        ))^((
            if haskey(pSliceShare, (s))
                pSliceShare[(s)]
            else
                pSliceShareDef
            end
        ))
    ) * vStorageStore[(st1, c, r, y, sp)] -
    (vStorageOut[(st1, c, r, y, sp)]) / ((
        if haskey(pStorageOutEff, (st1, c, r, y, sp))
            pStorageOutEff[(st1, c, r, y, sp)]
        else
            pStorageOutEffDef
        end
    ))
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqStorageAfLo(stg, comm, region, year, slice)$meqStorageAfLo(stg, comm, region, year, slice)
print("eqStorageAfLo(stg, comm, region, year, slice)...")
@constraint(
    model,
    [(st1, c, r, y, s) in meqStorageAfLo],
    vStorageStore[(st1, c, r, y, s)] >=
    (
        if haskey(pStorageAfLo, (st1, r, y, s))
            pStorageAfLo[(st1, r, y, s)]
        else
            pStorageAfLoDef
        end
    ) *
    (
        if haskey(pStorageCap2stg, (st1))
            pStorageCap2stg[(st1)]
        else
            pStorageCap2stgDef
        end
    ) *
    vStorageCap[(st1, r, y)] *
    prod(
        (
            if haskey(pStorageWeatherAfLo, (wth1, st1))
                pStorageWeatherAfLo[(wth1, st1)]
            else
                pStorageWeatherAfLoDef
            end
        ) * (
            if haskey(pWeather, (wth1, r, y, s))
                pWeather[(wth1, r, y, s)]
            else
                pWeatherDef
            end
        ) for wth1 in weather if (wth1, st1) in mStorageWeatherAfLo
    ; init = 1)
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqStorageAfUp(stg, comm, region, year, slice)$meqStorageAfUp(stg, comm, region, year, slice)
print("eqStorageAfUp(stg, comm, region, year, slice)...")
@constraint(
    model,
    [(st1, c, r, y, s) in meqStorageAfUp],
    vStorageStore[(st1, c, r, y, s)] <=
    (
        if haskey(pStorageAfUp, (st1, r, y, s))
            pStorageAfUp[(st1, r, y, s)]
        else
            pStorageAfUpDef
        end
    ) *
    (
        if haskey(pStorageCap2stg, (st1))
            pStorageCap2stg[(st1)]
        else
            pStorageCap2stgDef
        end
    ) *
    vStorageCap[(st1, r, y)] *
    prod(
        (
            if haskey(pStorageWeatherAfUp, (wth1, st1))
                pStorageWeatherAfUp[(wth1, st1)]
            else
                pStorageWeatherAfUpDef
            end
        ) * (
            if haskey(pWeather, (wth1, r, y, s))
                pWeather[(wth1, r, y, s)]
            else
                pWeatherDef
            end
        ) for wth1 in weather if (wth1, st1) in mStorageWeatherAfUp
    ; init = 1)
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqStorageClear(stg, comm, region, year, slice)$mvStorageStore(stg, comm, region, year, slice)
print("eqStorageClear(stg, comm, region, year, slice)...")
@constraint(
    model,
    [(st1, c, r, y, s) in mvStorageStore],
    (vStorageOut[(st1, c, r, y, s)]) / ((
        if haskey(pStorageOutEff, (st1, c, r, y, s))
            pStorageOutEff[(st1, c, r, y, s)]
        else
            pStorageOutEffDef
        end
    )) <= vStorageStore[(st1, c, r, y, s)]
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqStorageInpUp(stg, comm, region, year, slice)$meqStorageInpUp(stg, comm, region, year, slice)
print("eqStorageInpUp(stg, comm, region, year, slice)...")
@constraint(
    model,
    [(st1, c, r, y, s) in meqStorageInpUp],
    vStorageInp[(st1, c, r, y, s)] <=
    vStorageCap[(st1, r, y)] *
    (
        if haskey(pStorageCinpUp, (st1, c, r, y, s))
            pStorageCinpUp[(st1, c, r, y, s)]
        else
            pStorageCinpUpDef
        end
    ) *
    prod(
        (
            if haskey(pStorageWeatherCinpUp, (wth1, st1))
                pStorageWeatherCinpUp[(wth1, st1)]
            else
                pStorageWeatherCinpUpDef
            end
        ) * (
            if haskey(pWeather, (wth1, r, y, s))
                pWeather[(wth1, r, y, s)]
            else
                pWeatherDef
            end
        ) for wth1 in weather if (wth1, st1) in mStorageWeatherCinpUp
    ; init = 1)
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqStorageInpLo(stg, comm, region, year, slice)$meqStorageInpLo(stg, comm, region, year, slice)
print("eqStorageInpLo(stg, comm, region, year, slice)...")
@constraint(
    model,
    [(st1, c, r, y, s) in meqStorageInpLo],
    vStorageInp[(st1, c, r, y, s)] >=
    vStorageCap[(st1, r, y)] *
    (
        if haskey(pStorageCinpLo, (st1, c, r, y, s))
            pStorageCinpLo[(st1, c, r, y, s)]
        else
            pStorageCinpLoDef
        end
    ) *
    prod(
        (
            if haskey(pStorageWeatherCinpLo, (wth1, st1))
                pStorageWeatherCinpLo[(wth1, st1)]
            else
                pStorageWeatherCinpLoDef
            end
        ) * (
            if haskey(pWeather, (wth1, r, y, s))
                pWeather[(wth1, r, y, s)]
            else
                pWeatherDef
            end
        ) for wth1 in weather if (wth1, st1) in mStorageWeatherCinpLo
    ; init = 1)
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqStorageOutUp(stg, comm, region, year, slice)$meqStorageOutUp(stg, comm, region, year, slice)
print("eqStorageOutUp(stg, comm, region, year, slice)...")
@constraint(
    model,
    [(st1, c, r, y, s) in meqStorageOutUp],
    vStorageOut[(st1, c, r, y, s)] <=
    vStorageCap[(st1, r, y)] *
    (
        if haskey(pStorageCoutUp, (st1, c, r, y, s))
            pStorageCoutUp[(st1, c, r, y, s)]
        else
            pStorageCoutUpDef
        end
    ) *
    prod(
        (
            if haskey(pStorageWeatherCoutUp, (wth1, st1))
                pStorageWeatherCoutUp[(wth1, st1)]
            else
                pStorageWeatherCoutUpDef
            end
        ) * (
            if haskey(pWeather, (wth1, r, y, s))
                pWeather[(wth1, r, y, s)]
            else
                pWeatherDef
            end
        ) for wth1 in weather if (wth1, st1) in mStorageWeatherCoutUp
    ; init = 1)
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqStorageOutLo(stg, comm, region, year, slice)$meqStorageOutLo(stg, comm, region, year, slice)
print("eqStorageOutLo(stg, comm, region, year, slice)...")
@constraint(
    model,
    [(st1, c, r, y, s) in meqStorageOutLo],
    vStorageOut[(st1, c, r, y, s)] >=
    vStorageCap[(st1, r, y)] *
    (
        if haskey(pStorageCoutLo, (st1, c, r, y, s))
            pStorageCoutLo[(st1, c, r, y, s)]
        else
            pStorageCoutLoDef
        end
    ) *
    prod(
        (
            if haskey(pStorageWeatherCoutLo, (wth1, st1))
                pStorageWeatherCoutLo[(wth1, st1)]
            else
                pStorageWeatherCoutLoDef
            end
        ) * (
            if haskey(pWeather, (wth1, r, y, s))
                pWeather[(wth1, r, y, s)]
            else
                pWeatherDef
            end
        ) for wth1 in weather if (wth1, st1) in mStorageWeatherCoutLo
    ; init = 1)
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqStorageCap(stg, region, year)$mStorageSpan(stg, region, year)
print("eqStorageCap(stg, region, year)...")
@constraint(
    model,
    [(st1, r, y) in mStorageSpan],
    vStorageCap[(st1, r, y)] ==
    (
        if haskey(pStorageStock, (st1, r, y))
            pStorageStock[(st1, r, y)]
        else
            pStorageStockDef
        end
    ) + sum(
        (
            if haskey(pPeriodLen, (yp))
                pPeriodLen[(yp)]
            else
                pPeriodLenDef
            end
        ) * vStorageNewCap[(st1, r, yp)] for yp in year if (
            ordYear[(y)] >= ordYear[(yp)] &&
            (
                (st1, r) in mStorageOlifeInf ||
                ordYear[(y)] < (
                    if haskey(pStorageOlife, (st1, r))
                        pStorageOlife[(st1, r)]
                    else
                        pStorageOlifeDef
                    end
                ) + ordYear[(yp)]
            ) &&
            (st1, r, yp) in mStorageNew
        )
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqStorageCapLo(stg, region, year)$mStorageCapLo(stg, region, year)
print("eqStorageCapLo(stg, region, year)...")
@constraint(
    model,
    [(st1, r, y) in mStorageCapLo],
    vStorageCap[(st1, r, y)] >= (
        if haskey(pStorageCapLo, (st1, r, y))
            pStorageCapLo[(st1, r, y)]
        else
            pStorageCapLoDef
        end
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqStorageCapUp(stg, region, year)$mStorageCapUp(stg, region, year)
print("eqStorageCapUp(stg, region, year)...")
@constraint(
    model,
    [(st1, r, y) in mStorageCapUp],
    vStorageCap[(st1, r, y)] <= (
        if haskey(pStorageCapUp, (st1, r, y))
            pStorageCapUp[(st1, r, y)]
        else
            pStorageCapUpDef
        end
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqStorageNewCapLo(stg, region, year)$mStorageNewCapLo(stg, region, year)
print("eqStorageNewCapLo(stg, region, year)...")
@constraint(
    model,
    [(st1, r, y) in mStorageNewCapLo],
    vStorageNewCap[(st1, r, y)] >=
    (
        if haskey(pStorageNewCapLo, (st1, r, y))
            pStorageNewCapLo[(st1, r, y)]
        else
            pStorageNewCapLoDef
        end
    ) * (
        if haskey(pPeriodLen, (y))
            pPeriodLen[(y)]
        else
            pPeriodLenDef
        end
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqStorageNewCapUp(stg, region, year)$mStorageNewCapUp(stg, region, year)
print("eqStorageNewCapUp(stg, region, year)...")
@constraint(
    model,
    [(st1, r, y) in mStorageNewCapUp],
    vStorageNewCap[(st1, r, y)] <=
    (
        if haskey(pStorageNewCapUp, (st1, r, y))
            pStorageNewCapUp[(st1, r, y)]
        else
            pStorageNewCapUpDef
        end
    ) * (
        if haskey(pPeriodLen, (y))
            pPeriodLen[(y)]
        else
            pPeriodLenDef
        end
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqStorageInv(stg, region, year)$mStorageNew(stg, region, year)
print("eqStorageInv(stg, region, year)...")
@constraint(
    model,
    [(st1, r, y) in mStorageNew],
    vStorageInv[(st1, r, y)] ==
    (
        if haskey(pStorageInvcost, (st1, r, y))
            pStorageInvcost[(st1, r, y)]
        else
            pStorageInvcostDef
        end
    ) * vStorageNewCap[(st1, r, y)]
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqStorageEac(stg, region, year)$mStorageEac(stg, region, year)
print("eqStorageEac(stg, region, year)...")
@constraint(
    model,
    [(st1, r, y) in mStorageEac],
    # [eac-fix] vintaged new-capacity form (pStorageEac applies to NEW capacity only)
    vStorageEac[(st1, r, y)] == sum(
        (
            if haskey(pStorageEac, (st1, r, yp))
                pStorageEac[(st1, r, yp)]
            else
                pStorageEacDef
            end
        ) * vStorageNewCap[(st1, r, yp)] for yp in year if (
            (st1, r, yp) in mStorageNew &&
            ordYear[(y)] >= ordYear[(yp)] &&
            (
                (st1, r) in mStorageOlifeInf ||
                ordYear[(y)] < (
                    if haskey(pStorageOlife, (st1, r))
                        pStorageOlife[(st1, r)]
                    else
                        pStorageOlifeDef
                    end
                ) + ordYear[(yp)]
            ) &&
            (
                if haskey(pStorageInvcost, (st1, r, yp))
                    pStorageInvcost[(st1, r, yp)]
                else
                    0
                end
            ) != 0
        );
        init = 0
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqStorageFixom(stg, region, year)$mStorageFixom(stg, region, year)
print("eqStorageFixom(stg, region, year)...")
@constraint(
    model,
    [(st1, r, y) in mStorageFixom],
    vStorageFixom[(st1, r, y)] ==
    (
        if haskey(pStorageFixom, (st1, r, y))
            pStorageFixom[(st1, r, y)]
        else
            pStorageFixomDef
        end
    ) * vStorageCap[(st1, r, y)]
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqStorageVarom(stg, region, year)$mStorageVarom(stg, region, year)
print("eqStorageVarom(stg, region, year)...")
@constraint(
    model,
    [(st1, r, y) in mStorageVarom],
    vStorageVarom[(st1, r, y)] == sum(
        sum(
            (
                if haskey(pStorageCostInp, (st1, r, y, s))
                    pStorageCostInp[(st1, r, y, s)]
                else
                    pStorageCostInpDef
                end
            ) *
            (
                if haskey(pSliceWeight, (y, s))
                    pSliceWeight[(y, s)]
                else
                    pSliceWeightDef
                end
            ) *
            vStorageInp[(st1, c, r, y, s)] +
            (
                if haskey(pStorageCostOut, (st1, r, y, s))
                    pStorageCostOut[(st1, r, y, s)]
                else
                    pStorageCostOutDef
                end
            ) *
            (
                if haskey(pSliceWeight, (y, s))
                    pSliceWeight[(y, s)]
                else
                    pSliceWeightDef
                end
            ) *
            vStorageOut[(st1, c, r, y, s)] +
            (
                if haskey(pStorageCostStore, (st1, r, y, s))
                    pStorageCostStore[(st1, r, y, s)]
                else
                    pStorageCostStoreDef
                end
            ) *
            (
                if haskey(pSliceWeight, (y, s))
                    pSliceWeight[(y, s)]
                else
                    pSliceWeightDef
                end
            ) *
            vStorageStore[(st1, c, r, y, s)] for s in slice if (c, s) in mCommSlice
        ) for c in comm if (st1, c) in mStorageComm
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqImportTot(comm, dst, year, slice)$mImport(comm, dst, year, slice)
print("eqImportTot(comm, dst, year, slice)...")
@constraint(
    model,
    [(c, dst, y, s) in mImport],
    vImportTot[(c, dst, y, s)] ==
    sum(
        sum(
            (
                if (t1, c, src, dst, y, s) in mvTradeIr
                    (
                        (
                            if haskey(pTradeIrEff, (t1, src, dst, y, s))
                                pTradeIrEff[(t1, src, dst, y, s)]
                            else
                                pTradeIrEffDef
                            end
                        ) * vTradeIr[(t1, c, src, dst, y, s)]
                    )
                else
                    0
                end
            ) for src in region if (t1, src, dst) in mTradeRoutes
        ) for t1 in trade if (t1, c) in mTradeComm
    ) + sum((
        if (i, c, dst, y, s) in mImportRow
            vImportRow[(i, c, dst, y, s)]
        else
            0
        end
    ) for i in imp if (i, c) in mImpComm)
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqExportTot(comm, src, year, slice)$mExport(comm, src, year, slice)
print("eqExportTot(comm, src, year, slice)...")
@constraint(
    model,
    [(c, src, y, s) in mExport],
    vExportTot[(c, src, y, s)] ==
    sum(
        sum(
            (
                if (t1, c, src, dst, y, s) in mvTradeIr
                    vTradeIr[(t1, c, src, dst, y, s)]
                else
                    0
                end
            ) for dst in region if (t1, src, dst) in mTradeRoutes
        ) for t1 in trade if (t1, c) in mTradeComm
    ) + sum((
        if (e, c, src, y, s) in mExportRow
            vExportRow[(e, c, src, y, s)]
        else
            0
        end
    ) for e in expp if (e, c) in mExpComm)
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqTradeFlowUp(trade, comm, src, dst, year, slice)$meqTradeFlowUp(trade, comm, src, dst, year, slice)
print("eqTradeFlowUp(trade, comm, src, dst, year, slice)...")
@constraint(
    model,
    [(t1, c, src, dst, y, s) in meqTradeFlowUp],
    vTradeIr[(t1, c, src, dst, y, s)] <= (
        if haskey(pTradeIrUp, (t1, src, dst, y, s))
            pTradeIrUp[(t1, src, dst, y, s)]
        else
            pTradeIrUpDef
        end
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqTradeFlowLo(trade, comm, src, dst, year, slice)$meqTradeFlowLo(trade, comm, src, dst, year, slice)
print("eqTradeFlowLo(trade, comm, src, dst, year, slice)...")
@constraint(
    model,
    [(t1, c, src, dst, y, s) in meqTradeFlowLo],
    vTradeIr[(t1, c, src, dst, y, s)] >= (
        if haskey(pTradeIrLo, (t1, src, dst, y, s))
            pTradeIrLo[(t1, src, dst, y, s)]
        else
            pTradeIrLoDef
        end
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqImportIrCost(trade, region, year)$mImportIrCost(trade, region, year)
print("eqImportIrCost(trade, region, year)...")
@constraint(
    model,
    [(t1, r, y) in mImportIrCost],
    vImportIrCost[(t1, r, y)] == sum(
        sum(
            sum(
                (
                    if (t1, c, src, r, y, s) in mvTradeIr
                        (
                            (
                                (
                                    if haskey(pTradeIrCost, (t1, src, r, y, s))
                                        pTradeIrCost[(t1, src, r, y, s)]
                                    else
                                        pTradeIrCostDef
                                    end
                                ) + (
                                    if haskey(pTradeIrMarkup, (t1, src, r, y, s))
                                        pTradeIrMarkup[(t1, src, r, y, s)]
                                    else
                                        pTradeIrMarkupDef
                                    end
                                )
                            ) *
                            vTradeIr[(t1, c, src, r, y, s)] *
                            (
                                if haskey(pSliceWeight, (y, s))
                                    pSliceWeight[(y, s)]
                                else
                                    pSliceWeightDef
                                end
                            )
                        )
                    else
                        0
                    end
                ) for s in slice if (t1, s) in mTradeSlice
            ) for c in comm if (t1, c) in mTradeComm
        ) for src in region if (t1, src, r) in mTradeRoutes
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqExportIrCost(trade, region, year)$mExportIrCost(trade, region, year)
print("eqExportIrCost(trade, region, year)...")
@constraint(
    model,
    [(t1, r, y) in mExportIrCost],
    vExportIrCost[(t1, r, y)] ==
    -sum(
        sum(
            sum(
                (
                    if (t1, c, r, dst, y, s) in mvTradeIr
                        (
                            (
                                (
                                    if haskey(pTradeIrCost, (t1, r, dst, y, s))
                                        pTradeIrCost[(t1, r, dst, y, s)]
                                    else
                                        pTradeIrCostDef
                                    end
                                ) + (
                                    if haskey(pTradeIrMarkup, (t1, r, dst, y, s))
                                        pTradeIrMarkup[(t1, r, dst, y, s)]
                                    else
                                        pTradeIrMarkupDef
                                    end
                                )
                            ) *
                            vTradeIr[(t1, c, r, dst, y, s)] *
                            (
                                if haskey(pSliceWeight, (y, s))
                                    pSliceWeight[(y, s)]
                                else
                                    pSliceWeightDef
                                end
                            )
                        )
                    else
                        0
                    end
                ) for s in slice if (t1, s) in mTradeSlice
            ) for c in comm if (t1, c) in mTradeComm
        ) for dst in region if (t1, r, dst) in mTradeRoutes
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqExportRowUp(expp, comm, region, year, slice)$mExportRowUp(expp, comm, region, year, slice)
print("eqExportRowUp(expp, comm, region, year, slice)...")
@constraint(
    model,
    [(e, c, r, y, s) in mExportRowUp],
    vExportRow[(e, c, r, y, s)] <= (
        if haskey(pExportRowUp, (e, r, y, s))
            pExportRowUp[(e, r, y, s)]
        else
            pExportRowUpDef
        end
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqExportRowLo(expp, comm, region, year, slice)$meqExportRowLo(expp, comm, region, year, slice)
print("eqExportRowLo(expp, comm, region, year, slice)...")
@constraint(
    model,
    [(e, c, r, y, s) in meqExportRowLo],
    vExportRow[(e, c, r, y, s)] >= (
        if haskey(pExportRowLo, (e, r, y, s))
            pExportRowLo[(e, r, y, s)]
        else
            pExportRowLoDef
        end
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqExportRowCum(expp, comm)$mExpComm(expp, comm)
print("eqExportRowCum(expp, comm)...")
@constraint(
    model,
    [(e, c) in mExpComm],
    vExportRowCum[(e, c)] == sum(
        (
            if haskey(pPeriodLen, (y))
                pPeriodLen[(y)]
            else
                pPeriodLenDef
            end
        ) *
        (
            if haskey(pSliceWeight, (y, s))
                pSliceWeight[(y, s)]
            else
                pSliceWeightDef
            end
        ) *
        vExportRow[(e, c, r, y, s)] for r in region for y in year for
        s in slice if (e, c, r, y, s) in mExportRow
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqExportRowResUp(expp, comm)$mExportRowCumUp(expp, comm)
print("eqExportRowResUp(expp, comm)...")
@constraint(
    model,
    [(e, c) in mExportRowCumUp],
    vExportRowCum[(e, c)] <= (
        if haskey(pExportRowRes, (e))
            pExportRowRes[(e)]
        else
            pExportRowResDef
        end
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqExportRowCost(expp, region, year)$mExportRowCost(expp, region, year)
print("eqExportRowCost(expp, region, year)...")
@constraint(
    model,
    [(e, r, y) in mExportRowCost],
    vExportRowCost[(e, r, y)] ==
    -sum(
        (
            if haskey(pExportRowPrice, (e, r, y, s))
                pExportRowPrice[(e, r, y, s)]
            else
                pExportRowPriceDef
            end
        ) *
        (
            if haskey(pSliceWeight, (y, s))
                pSliceWeight[(y, s)]
            else
                pSliceWeightDef
            end
        ) *
        vExportRow[(e, c, r, y, s)] for c in comm for
        s in slice if (e, c, r, y, s) in mExportRow
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqImportRowUp(imp, comm, region, year, slice)$mImportRowUp(imp, comm, region, year, slice)
print("eqImportRowUp(imp, comm, region, year, slice)...")
@constraint(
    model,
    [(i, c, r, y, s) in mImportRowUp],
    vImportRow[(i, c, r, y, s)] <= (
        if haskey(pImportRowUp, (i, r, y, s))
            pImportRowUp[(i, r, y, s)]
        else
            pImportRowUpDef
        end
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqImportRowLo(imp, comm, region, year, slice)$meqImportRowLo(imp, comm, region, year, slice)
print("eqImportRowLo(imp, comm, region, year, slice)...")
@constraint(
    model,
    [(i, c, r, y, s) in meqImportRowLo],
    vImportRow[(i, c, r, y, s)] >= (
        if haskey(pImportRowLo, (i, r, y, s))
            pImportRowLo[(i, r, y, s)]
        else
            pImportRowLoDef
        end
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqImportRowCum(imp, comm)$mImpComm(imp, comm)
print("eqImportRowCum(imp, comm)...")
@constraint(
    model,
    [(i, c) in mImpComm],
    vImportRowCum[(i, c)] == sum(
        (
            if haskey(pPeriodLen, (y))
                pPeriodLen[(y)]
            else
                pPeriodLenDef
            end
        ) *
        (
            if haskey(pSliceWeight, (y, s))
                pSliceWeight[(y, s)]
            else
                pSliceWeightDef
            end
        ) *
        vImportRow[(i, c, r, y, s)] for r in region for y in year for
        s in slice if (i, c, r, y, s) in mImportRow
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqImportRowResUp(imp, comm)$mImportRowCumUp(imp, comm)
print("eqImportRowResUp(imp, comm)...")
@constraint(
    model,
    [(i, c) in mImportRowCumUp],
    vImportRowCum[(i, c)] <= (
        if haskey(pImportRowRes, (i))
            pImportRowRes[(i)]
        else
            pImportRowResDef
        end
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqImportRowCost(imp, region, year)$mImportRowCost(imp, region, year)
print("eqImportRowCost(imp, region, year)...")
@constraint(
    model,
    [(i, r, y) in mImportRowCost],
    vImportRowCost[(i, r, y)] == sum(
        (
            if haskey(pImportRowPrice, (i, r, y, s))
                pImportRowPrice[(i, r, y, s)]
            else
                pImportRowPriceDef
            end
        ) *
        (
            if haskey(pSliceWeight, (y, s))
                pSliceWeight[(y, s)]
            else
                pSliceWeightDef
            end
        ) *
        vImportRow[(i, c, r, y, s)] for c in comm for
        s in slice if (i, c, r, y, s) in mImportRow
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqTradeCapFlow(trade, comm, year, slice)$meqTradeCapFlow(trade, comm, year, slice)
print("eqTradeCapFlow(trade, comm, year, slice)...")
@constraint(
    model,
    [(t1, c, y, s) in meqTradeCapFlow],
    (
        if haskey(pSliceShare, (s))
            pSliceShare[(s)]
        else
            pSliceShareDef
        end
    ) *
    (
        if haskey(pTradeCap2Act, (t1))
            pTradeCap2Act[(t1)]
        else
            pTradeCap2ActDef
        end
    ) *
    vTradeCap[(t1, y)] >= sum(
        vTradeIr[(t1, c, src, dst, y, s)] for src in region for
        dst in region if (t1, c, src, dst, y, s) in mvTradeIr
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqTradeCap(trade, year)$mTradeSpan(trade, year)
print("eqTradeCap(trade, year)...")
@constraint(
    model,
    [(t1, y) in mTradeSpan],
    vTradeCap[(t1, y)] ==
    (
        if haskey(pTradeStock, (t1, y))
            pTradeStock[(t1, y)]
        else
            pTradeStockDef
        end
    ) + sum(
        (
            if haskey(pPeriodLen, (yp))
                pPeriodLen[(yp)]
            else
                pPeriodLenDef
            end
        ) * vTradeNewCap[(t1, yp)] for yp in year if (
            (t1, yp) in mTradeNew &&
            ordYear[(y)] >= ordYear[(yp)] &&
            (
                ordYear[(y)] < (
                    if haskey(pTradeOlife, (t1))
                        pTradeOlife[(t1)]
                    else
                        pTradeOlifeDef
                    end
                ) + ordYear[(yp)] || t1 in mTradeOlifeInf
            )
        )
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqTradeCapLo(trade, year)$mTradeCapLo(trade, year)
print("eqTradeCapLo(trade, year)...")
@constraint(
    model,
    [(t1, y) in mTradeCapLo],
    vTradeCap[(t1, y)] >= (
        if haskey(pTradeCapLo, (t1, y))
            pTradeCapLo[(t1, y)]
        else
            pTradeCapLoDef
        end
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqTradeCapUp(trade, year)$mTradeCapUp(trade, year)
print("eqTradeCapUp(trade, year)...")
@constraint(
    model,
    [(t1, y) in mTradeCapUp],
    vTradeCap[(t1, y)] <= (
        if haskey(pTradeCapUp, (t1, y))
            pTradeCapUp[(t1, y)]
        else
            pTradeCapUpDef
        end
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqTradeNewCapLo(trade, year)$mTradeNewCapLo(trade, year)
print("eqTradeNewCapLo(trade, year)...")
@constraint(
    model,
    [(t1, y) in mTradeNewCapLo],
    vTradeNewCap[(t1, y)] * (
        if haskey(pPeriodLen, (y))
            pPeriodLen[(y)]
        else
            pPeriodLenDef
        end
    ) >= (
        if haskey(pTradeNewCapLo, (t1, y))
            pTradeNewCapLo[(t1, y)]
        else
            pTradeNewCapLoDef
        end
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqTradeNewCapUp(trade, year)$mTradeNewCapUp(trade, year)
print("eqTradeNewCapUp(trade, year)...")
@constraint(
    model,
    [(t1, y) in mTradeNewCapUp],
    vTradeNewCap[(t1, y)] * (
        if haskey(pPeriodLen, (y))
            pPeriodLen[(y)]
        else
            pPeriodLenDef
        end
    ) <= (
        if haskey(pTradeNewCapUp, (t1, y))
            pTradeNewCapUp[(t1, y)]
        else
            pTradeNewCapUpDef
        end
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqTradeInv(trade, region, year)$mTradeInv(trade, region, year)
print("eqTradeInv(trade, region, year)...")
@constraint(
    model,
    [(t1, r, y) in mTradeInv],
    vTradeInv[(t1, r, y)] ==
    (
        if haskey(pTradeInvcost, (t1, r, y))
            pTradeInvcost[(t1, r, y)]
        else
            pTradeInvcostDef
        end
    ) * vTradeNewCap[(t1, y)]
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqTradeEac(trade, region, year)$mTradeEac(trade, region, year)
print("eqTradeEac(trade, region, year)...")
@constraint(
    model,
    [(t1, r, y) in mTradeEac],
    # [eac-fix] vintaged new-capacity form (pTradeEac applies to NEW capacity only)
    vTradeEac[(t1, r, y)] == sum(
        (
            if haskey(pTradeEac, (t1, r, yp))
                pTradeEac[(t1, r, yp)]
            else
                pTradeEacDef
            end
        ) * vTradeNewCap[(t1, yp)] for yp in year if (
            (t1, yp) in mTradeNew &&
            ordYear[(y)] >= ordYear[(yp)] &&
            (
                ordYear[(y)] < (
                    if haskey(pTradeOlife, (t1))
                        pTradeOlife[(t1)]
                    else
                        pTradeOlifeDef
                    end
                ) + ordYear[(yp)] || t1 in mTradeOlifeInf
            )
        );
        init = 0
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqTradeFixom(trade, region, year)$mTradeFixom(trade, region, year)
print("eqTradeFixom(trade, region, year)...")
@constraint(
    model,
    [(t1, r, y) in mTradeFixom],
    vTradeFixom[(t1, r, y)] ==
    (
        if haskey(pTradeFixom, (t1, r, y))
            pTradeFixom[(t1, r, y)]
        else
            pTradeFixomDef
        end
    ) * vTradeCap[(t1, y)]
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqTradeIrAInp(trade, comm, region, year, slice)$mvTradeIrAInp(trade, comm, region, year, slice)
print("eqTradeIrAInp(trade, comm, region, year, slice)...")
@constraint(
    model,
    [(t1, c, r, y, s) in mvTradeIrAInp],
    vTradeIrAInp[(t1, c, r, y, s)] ==
    sum(
        (
            if haskey(pTradeIrCsrc2Ainp, (t1, c, r, dst, y, s))
                pTradeIrCsrc2Ainp[(t1, c, r, dst, y, s)]
            else
                pTradeIrCsrc2AinpDef
            end
        ) * sum(vTradeIr[(t1, cp, r, dst, y, s)] for cp in comm if (t1, cp) in mTradeComm)
        for dst in region if (t1, c, r, dst, y, s) in mTradeIrCsrc2Ainp
    ) + sum(
        (
            if haskey(pTradeIrCdst2Ainp, (t1, c, src, r, y, s))
                pTradeIrCdst2Ainp[(t1, c, src, r, y, s)]
            else
                pTradeIrCdst2AinpDef
            end
        ) * sum(vTradeIr[(t1, cp, src, r, y, s)] for cp in comm if (t1, cp) in mTradeComm)
        for src in region if (t1, c, src, r, y, s) in mTradeIrCdst2Ainp
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqTradeIrAOut(trade, comm, region, year, slice)$mvTradeIrAOut(trade, comm, region, year, slice)
print("eqTradeIrAOut(trade, comm, region, year, slice)...")
@constraint(
    model,
    [(t1, c, r, y, s) in mvTradeIrAOut],
    vTradeIrAOut[(t1, c, r, y, s)] ==
    sum(
        (
            if haskey(pTradeIrCsrc2Aout, (t1, c, r, dst, y, s))
                pTradeIrCsrc2Aout[(t1, c, r, dst, y, s)]
            else
                pTradeIrCsrc2AoutDef
            end
        ) * sum(vTradeIr[(t1, cp, r, dst, y, s)] for cp in comm if (t1, cp) in mTradeComm)
        for dst in region if (t1, c, r, dst, y, s) in mTradeIrCsrc2Aout
    ) + sum(
        (
            if haskey(pTradeIrCdst2Aout, (t1, c, src, r, y, s))
                pTradeIrCdst2Aout[(t1, c, src, r, y, s)]
            else
                pTradeIrCdst2AoutDef
            end
        ) * sum(vTradeIr[(t1, cp, src, r, y, s)] for cp in comm if (t1, cp) in mTradeComm)
        for src in region if (t1, c, src, r, y, s) in mTradeIrCdst2Aout
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqTradeIrAInpTot(comm, region, year, slice)$mvTradeIrAInpTot(comm, region, year, slice)
print("eqTradeIrAInpTot(comm, region, year, slice)...")
@constraint(
    model,
    [(c, r, y, s) in mvTradeIrAInpTot],
    vTradeIrAInpTot[(c, r, y, s)] == sum(
        vTradeIrAInp[(t1, c, r, y, sp)] for t1 in trade for sp in slice if
        ((c, s, sp) in mCommSliceOrParent && (t1, c, r, y, sp) in mvTradeIrAInp)
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqTradeIrAOutTot(comm, region, year, slice)$mvTradeIrAOutTot(comm, region, year, slice)
print("eqTradeIrAOutTot(comm, region, year, slice)...")
@constraint(
    model,
    [(c, r, y, s) in mvTradeIrAOutTot],
    vTradeIrAOutTot[(c, r, y, s)] == sum(
        vTradeIrAOut[(t1, c, r, y, sp)] for t1 in trade for sp in slice if
        ((c, s, sp) in mCommSliceOrParent && (t1, c, r, y, sp) in mvTradeIrAOut)
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqBalLo(comm, region, year, slice)$meqBalLo(comm, region, year, slice)
print("eqBalLo(comm, region, year, slice)...")
@constraint(model, [(c, r, y, s) in meqBalLo], vBalance[(c, r, y, s)] >= 0);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqBalUp(comm, region, year, slice)$meqBalUp(comm, region, year, slice)
print("eqBalUp(comm, region, year, slice)...")
@constraint(model, [(c, r, y, s) in meqBalUp], vBalance[(c, r, y, s)] <= 0);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqBalFx(comm, region, year, slice)$meqBalFx(comm, region, year, slice)
print("eqBalFx(comm, region, year, slice)...")
@constraint(model, [(c, r, y, s) in meqBalFx], vBalance[(c, r, y, s)] == 0);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqBal(comm, region, year, slice)$mvBalance(comm, region, year, slice)
print("eqBal(comm, region, year, slice)...")
@constraint(
    model,
    [(c, r, y, s) in mvBalance],
    vBalance[(c, r, y, s)] ==
    (
        if (c, r, y, s) in mvOutTot
            vOutTot[(c, r, y, s)]
        else
            0
        end
    ) - (
        if (c, r, y, s) in mvInpTot
            vInpTot[(c, r, y, s)]
        else
            0
        end
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# [agg-rewrite] eqBalanceRY/vBalanceRY retired (dead reporting: weighted slice-sum, unused)
# eqOutTot(comm, region, year, slice)$mvOutTot(comm, region, year, slice)
print("eqOutTot(comm, region, year, slice)...")
@constraint(
    model,
    [(c, r, y, s) in mvOutTot],
    vOutTot[(c, r, y, s)] ==
    (
        if (c, r, y, s) in mDummyImport
            vDummyImport[(c, r, y, s)]
        else
            0
        end
    ) +
    (
        if (c, r, y, s) in mSupOutTot
            vSupOutTot[(c, r, y, s)]
        else
            0
        end
    ) +
    (
        if (c, r, y, s) in mEmsFuelTot
            vEmsFuelTot[(c, r, y, s)]
        else
            0
        end
    ) +
    (
        if (c, r, y, s) in mAggOut
            vAggOutTot[(c, r, y, s)]
        else
            0
        end
    ) +
    (
        if (c, r, y, s) in mTechOutTot
            vTechOutTot[(c, r, y, s)]
        else
            0
        end
    ) +
    (
        if (c, r, y, s) in mStorageOutTot
            vStorageOutTot[(c, r, y, s)]
        else
            0
        end
    ) +
    (
        if (c, r, y, s) in mImport
            vImportTot[(c, r, y, s)]
        else
            0
        end
    ) +
    (
        if (c, r, y, s) in mvTradeIrAOutTot
            vTradeIrAOutTot[(c, r, y, s)]
        else
            0
        end
    ) +
    # [agg-rewrite] UP-aggregation of the immediately-finer children's totals
    # (pSliceAgg renormalizes intensive values); replaces old mOutSub/vOut2Lo.
    sum(
        (
            if haskey(pSliceAgg, (y, s, sp))
                pSliceAgg[(y, s, sp)]
            else
                pSliceAggDef
            end
        ) * vOutTot[(c, r, y, sp)]
        for sp in slice if ((s, sp) in mSliceFamily && (c, r, y, sp) in mvOutTot);
        init = 0
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# [agg-rewrite] eqOutTotRY/vOutTotRY retired (dead reporting)
# [agg-rewrite] eqOut2Lo removed: down-disaggregation of coarse output is
# replaced by up-aggregation in eqOutTot (vOut2Lo retired). Mirrors GLPK.
# eqInpTot(comm, region, year, slice)$mvInpTot(comm, region, year, slice)
print("eqInpTot(comm, region, year, slice)...")
@constraint(
    model,
    [(c, r, y, s) in mvInpTot],
    vInpTot[(c, r, y, s)] ==
    (
        if (c, r, y, s) in mvDemInp
            vDemInp[(c, r, y, s)]
        else
            0
        end
    ) +
    (
        if (c, r, y, s) in mDummyExport
            vDummyExport[(c, r, y, s)]
        else
            0
        end
    ) +
    (
        if (c, r, y, s) in mTechInpTot
            vTechInpTot[(c, r, y, s)]
        else
            0
        end
    ) +
    (
        if (c, r, y, s) in mStorageInpTot
            vStorageInpTot[(c, r, y, s)]
        else
            0
        end
    ) +
    (
        if (c, r, y, s) in mExport
            vExportTot[(c, r, y, s)]
        else
            0
        end
    ) +
    (
        if (c, r, y, s) in mvTradeIrAInpTot
            vTradeIrAInpTot[(c, r, y, s)]
        else
            0
        end
    ) +
    # [agg-rewrite] UP-aggregation of the immediately-finer children's totals;
    # replaces old mInpSub/vInp2Lo down-disaggregation collector.
    sum(
        (
            if haskey(pSliceAgg, (y, s, sp))
                pSliceAgg[(y, s, sp)]
            else
                pSliceAggDef
            end
        ) * vInpTot[(c, r, y, sp)]
        for sp in slice if ((s, sp) in mSliceFamily && (c, r, y, sp) in mvInpTot);
        init = 0
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# [agg-rewrite] eqInpTotRY/vInpTotRY retired (dead reporting)
# [agg-rewrite] eqInp2Lo removed: down-disaggregation of coarse input is
# replaced by up-aggregation in eqInpTot (vInp2Lo retired). Mirrors GLPK.
# eqSupOutTot(comm, region, year, slice)$mSupOutTot(comm, region, year, slice)
print("eqSupOutTot(comm, region, year, slice)...")
@constraint(
    model,
    [(c, r, y, s) in mSupOutTot],
    vSupOutTot[(c, r, y, s)] ==
    sum(vSupOut[(s1, c, r, y, s)] for s1 in sup if (s1, c) in mSupComm)
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqTechInpTot(comm, region, year, slice)$mTechInpTot(comm, region, year, slice)
print("eqTechInpTot(comm, region, year, slice)...")
@constraint(
    model,
    [(c, r, y, s) in mTechInpTot],
    vTechInpTot[(c, r, y, s)] ==
    sum(
        (
            if (t, c, r, y, s) in mvTechInp
                vTechInp[(t, c, r, y, s)]
            else
                0
            end
        ) for t in tech if (t, c) in mTechInpCommSameSlice
    ) +
    sum(
        sum(
            (
                if (t, c, r, y, sp) in mvTechInp
                    vTechInp[(t, c, r, y, sp)]
                else
                    0
                end
            ) for sp in slice if (t, c, sp, s) in mTechInpCommAggSlice
        ) for t in tech if (t, c) in mTechInpCommAgg
    ) +
    sum(
        (
            if (t, c, r, y, s) in mvTechAInp
                vTechAInp[(t, c, r, y, s)]
            else
                0
            end
        ) for t in tech if (t, c) in mTechAInpCommSameSlice
    ) +
    sum(
        sum(
            (
                if (t, c, r, y, sp) in mvTechAInp
                    vTechAInp[(t, c, r, y, sp)]
                else
                    0
                end
            ) for sp in slice if (t, c, sp, s) in mTechAInpCommAggSlice
        ) for t in tech if (t, c) in mTechAInpCommAgg
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqTechOutTot(comm, region, year, slice)$mTechOutTot(comm, region, year, slice)
print("eqTechOutTot(comm, region, year, slice)...")
@constraint(
    model,
    [(c, r, y, s) in mTechOutTot],
    vTechOutTot[(c, r, y, s)] ==
    sum(
        (
            if (t, c, r, y, s) in mvTechOut
                vTechOut[(t, c, r, y, s)]
            else
                0
            end
        ) for t in tech if (t, c) in mTechOutCommSameSlice
    ) +
    sum(
        sum(
            (
                if (t, c, r, y, sp) in mvTechOut
                    vTechOut[(t, c, r, y, sp)]
                else
                    0
                end
            ) for sp in slice if (t, c, sp, s) in mTechOutCommAggSlice
        ) for t in tech if (t, c) in mTechOutCommAgg
    ) +
    sum(
        (
            if (t, c, r, y, s) in mvTechAOut
                vTechAOut[(t, c, r, y, s)]
            else
                0
            end
        ) for t in tech if (t, c) in mTechAOutCommSameSlice
    ) +
    sum(
        sum(
            (
                if (t, c, r, y, sp) in mvTechAOut
                    vTechAOut[(t, c, r, y, sp)]
                else
                    0
                end
            ) for sp in slice if (t, c, sp, s) in mTechAOutCommAggSlice
        ) for t in tech if (t, c) in mTechAOutCommAgg
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# [agg-rewrite] eqTechOutRY/vTechOutRY retired (dead reporting)
# eqStorageInpTot(comm, region, year, slice)$mStorageInpTot(comm, region, year, slice)
print("eqStorageInpTot(comm, region, year, slice)...")
@constraint(
    model,
    [(c, r, y, s) in mStorageInpTot],
    vStorageInpTot[(c, r, y, s)] ==
    sum(
        vStorageInp[(st1, c, r, y, s)] for st1 in stg if (st1, c, r, y, s) in mvStorageStore
    ) + sum(
        vStorageAInp[(st1, c, r, y, s)] for st1 in stg if (st1, c, r, y, s) in mvStorageAInp
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqStorageOutTot(comm, region, year, slice)$mStorageOutTot(comm, region, year, slice)
print("eqStorageOutTot(comm, region, year, slice)...")
@constraint(
    model,
    [(c, r, y, s) in mStorageOutTot],
    vStorageOutTot[(c, r, y, s)] ==
    sum(
        vStorageOut[(st1, c, r, y, s)] for st1 in stg if (st1, c, r, y, s) in mvStorageStore
    ) + sum(
        vStorageAOut[(st1, c, r, y, s)] for st1 in stg if (st1, c, r, y, s) in mvStorageAOut
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqDummyImportCost(comm, region, year)$mDummyImportCost(comm, region, year)
print("eqDummyImportCost(comm, region, year)...")
@constraint(
    model,
    [(c, r, y) in mDummyImportCost],
    vDummyImportCost[(c, r, y)] == sum(
        (
            if haskey(pSliceWeight, (y, s))
                pSliceWeight[(y, s)]
            else
                pSliceWeightDef
            end
        ) *
        (
            if haskey(pDummyImportCost, (c, r, y, s))
                pDummyImportCost[(c, r, y, s)]
            else
                pDummyImportCostDef
            end
        ) *
        (
            if (c, r, y, s) in mDummyImport
                vDummyImport[(c, r, y, s)]
            else
                0
            end
        ) for s in slice if (c, r, y, s) in mDummyImport
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqDummyExportCost(comm, region, year)$mDummyExportCost(comm, region, year)
print("eqDummyExportCost(comm, region, year)...")
@constraint(
    model,
    [(c, r, y) in mDummyExportCost],
    vDummyExportCost[(c, r, y)] == sum(
        (
            if haskey(pSliceWeight, (y, s))
                pSliceWeight[(y, s)]
            else
                pSliceWeightDef
            end
        ) *
        (
            if haskey(pDummyExportCost, (c, r, y, s))
                pDummyExportCost[(c, r, y, s)]
            else
                pDummyExportCostDef
            end
        ) *
        (
            if (c, r, y, s) in mDummyExport
                vDummyExport[(c, r, y, s)]
            else
                0
            end
        ) for s in slice if (c, r, y, s) in mDummyExport
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqTaxCost(comm, region, year)$mTaxCost(comm, region, year)
print("eqTaxCost(comm, region, year)...")
@constraint(
    model,
    [(c, r, y) in mTaxCost],
    vTaxCost[(c, r, y)] ==
    sum(
        (
            if haskey(pTaxCostOut, (c, r, y, s))
                pTaxCostOut[(c, r, y, s)]
            else
                pTaxCostOutDef
            end
        ) *
        (
            if haskey(pSliceWeight, (y, s))
                pSliceWeight[(y, s)]
            else
                pSliceWeightDef
            end
        ) *
        vOutTot[(c, r, y, s)] for
        s in slice if ((c, r, y, s) in mvOutTot && (c, s) in mCommSlice)
    ) +
    sum(
        (
            if haskey(pTaxCostInp, (c, r, y, s))
                pTaxCostInp[(c, r, y, s)]
            else
                pTaxCostInpDef
            end
        ) *
        (
            if haskey(pSliceWeight, (y, s))
                pSliceWeight[(y, s)]
            else
                pSliceWeightDef
            end
        ) *
        vInpTot[(c, r, y, s)] for
        s in slice if ((c, r, y, s) in mvInpTot && (c, s) in mCommSlice)
    ) +
    sum(
        (
            if haskey(pTaxCostBal, (c, r, y, s))
                pTaxCostBal[(c, r, y, s)]
            else
                pTaxCostBalDef
            end
        ) *
        (
            if haskey(pSliceWeight, (y, s))
                pSliceWeight[(y, s)]
            else
                pSliceWeightDef
            end
        ) *
        vBalance[(c, r, y, s)] for
        s in slice if ((c, r, y, s) in mvBalance && (c, s) in mCommSlice)
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqSubsCost(comm, region, year)$mSubCost(comm, region, year)
print("eqSubsCost(comm, region, year)...")
@constraint(
    model,
    [(c, r, y) in mSubCost],
    vSubsCost[(c, r, y)] ==
    -sum(
        (
            if haskey(pSubCostOut, (c, r, y, s))
                pSubCostOut[(c, r, y, s)]
            else
                pSubCostOutDef
            end
        ) *
        (
            if haskey(pSliceWeight, (y, s))
                pSliceWeight[(y, s)]
            else
                pSliceWeightDef
            end
        ) *
        vOutTot[(c, r, y, s)] for
        s in slice if ((c, r, y, s) in mvOutTot && (c, s) in mCommSlice)
    ) - sum(
        (
            if haskey(pSubCostInp, (c, r, y, s))
                pSubCostInp[(c, r, y, s)]
            else
                pSubCostInpDef
            end
        ) *
        (
            if haskey(pSliceWeight, (y, s))
                pSliceWeight[(y, s)]
            else
                pSliceWeightDef
            end
        ) *
        vInpTot[(c, r, y, s)] for
        s in slice if ((c, r, y, s) in mvInpTot && (c, s) in mCommSlice)
    ) - sum(
        (
            if haskey(pSubCostBal, (c, r, y, s))
                pSubCostBal[(c, r, y, s)]
            else
                pSubCostBalDef
            end
        ) *
        (
            if haskey(pSliceWeight, (y, s))
                pSliceWeight[(y, s)]
            else
                pSliceWeightDef
            end
        ) *
        vBalance[(c, r, y, s)] for
        s in slice if ((c, r, y, s) in mvBalance && (c, s) in mCommSlice)
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqCost(region, year)$mvTotalCost(region, year)
print("eqCost(region, year)...")
@constraint(
    model,
    [(r, y) in mvTotalCost],
    vTotalCost[(r, y)] ==
    + sum((if (s1, r, y) in mvSupCost
        vSupCost[(s1, r, y)]
    else
        0
    end) for s1 in sup if (s1, r, y) in mvSupCost)
    +sum((if (t, r, y) in mTechEac
        vTechEac[(t, r, y)]
    else
        0
    end) for t in tech if (t, r, y) in mTechEac)
    +sum((if (t, r, y) in mTechRetCost
        vTechRetCost[(t, r, y)]
    else
        0
    end) for t in tech if (t, r, y) in mTechRetCost)
    +sum((if (t, r, y) in mTechFixom
        vTechFixom[(t, r, y)]
    else
        0
    end) for t in tech if (t, r, y) in mTechFixom)
    +sum((if (t, r, y) in mTechVarom
        vTechVarom[(t, r, y)]
    else
        0
    end) for t in tech if (t, r, y) in mTechVarom)
    +sum((if (st1, r, y) in mStorageEac
        vStorageEac[(st1, r, y)]
    else
        0
    end) for st1 in stg if (st1, r, y) in mStorageEac)
    +sum(
        (if (st1, r, y) in mStorageFixom
            vStorageFixom[(st1, r, y)]
        else
            0
        end) for st1 in stg if (st1, r, y) in mStorageFixom
    )
    + sum((if (st1,r,y) in mStorageVarom; vStorageVarom[(st1,r,y)]; else 0; end;) for st1 in stg if (st1,r,y) in mStorageVarom)
    + sum((if (i,r,y) in mImportRowCost; vImportRowCost[(i,r,y)]; else 0; end;) for i in imp if (i,r,y) in mImportRowCost)
    + sum((if (e,r,y) in mExportRowCost; vExportRowCost[(e,r,y)]; else 0; end;) for e in expp if (e,r,y) in mExportRowCost)
    + sum((if (t1,r,y) in mTradeEac; vTradeEac[(t1,r,y)]; else 0; end;) for t1 in trade if (t1,r,y) in mTradeEac)
    + sum((if (t1,r,y) in mTradeFixom; vTradeFixom[(t1,r,y)]; else 0; end;) for t1 in trade if (t1,r,y) in mTradeFixom)
    + sum((if (t1,r,y) in mImportIrCost; vImportIrCost[(t1,r,y)]; else 0; end;) for t1 in trade if (t1,r,y) in mImportIrCost)
    + sum((if (t1,r,y) in mExportIrCost; vExportIrCost[(t1,r,y)]; else 0; end;) for t1 in trade if (t1,r,y) in mExportIrCost)
    + sum((if (c,r,y) in mTaxCost; vTaxCost[(c,r,y)]; else 0; end;) for c in comm if (c,r,y) in mTaxCost)
    + sum((if (c,r,y) in mSubCost; vSubsCost[(c,r,y)]; else 0; end;) for c in comm if (c,r,y) in mSubCost)
    + (if (r,y) in mvTotalUserCosts; vTotalUserCosts[(r,y)]; else 0; end;)
    + sum((if (c,r,y) in mDummyImportCost; vDummyImportCost[(c,r,y)]; else 0; end;) for c in comm if (c,r,y) in mDummyImportCost)
    + sum((if (c,r,y) in mDummyExportCost; vDummyExportCost[(c,r,y)]; else 0; end;) for c in comm if (c,r,y) in mDummyExportCost)
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqObjective
print("eqObjective...")
@constraint(
    model,
    vObjective == sum(
        vTotalCost[(r, y)] *
        (
            if haskey(pPeriodLen, (y))
                pPeriodLen[(y)]
            else
                pPeriodLenDef
            end
        ) *
        (
            if haskey(pDiscountFactor, (r, y))
                pDiscountFactor[(r, y)]
            else
                pDiscountFactorDef
            end
        ) for r in region for y in year if (r, y) in mvTotalCost
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqLECActivity(tech, region, year)$meqLECActivity(tech, region, year)
print("eqLECActivity(tech, region, year)...")
@constraint(
    model,
    [(t, r, y) in meqLECActivity],
    sum(vTechAct[(t, r, y, s)] for s in slice if (t, s) in mTechSlice) >= (
        if haskey(pLECLoACT, (r))
            pLECLoACT[(r)]
        else
            pLECLoACTDef
        end
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
println(flog, "\"solver\",,\"", Dates.format(now(), "yyyy-mm-dd HH:MM:SS"), "\"")
@objective(model, Min, vObjective)
include("inc_constraints.jl")
include("inc_costs.jl")
include("inc_solver.jl")
# using Cbc
# set_optimizer(model, Cbc.Optimizer)
include("inc3.jl")
optimize!(model)
hh = "-100"
if termination_status(model) == MOI.OPTIMAL
    hh = "1"
end
println(
    flog,
    "\"solution status\",",
    hh,
    ",\"",
    Dates.format(now(), "yyyy-mm-dd HH:MM:SS"),
    "\"",
)
include("inc4.jl")
println(flog, "\"export results\",,\"", Dates.format(now(), "yyyy-mm-dd HH:MM:SS"), "\"")

# Print solution
include("output.jl")
include("inc5.jl")
println(flog, "\"done\",,\"", Dates.format(now(), "yyyy-mm-dd HH:MM:SS"), "\"")
