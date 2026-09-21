# energyRt 0.71
# Energy Systems Modeling Toolbox for R
# https://energyRt.org

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

# Mappings used as a filter over a whole set, once per constraint row, cost
# O(rows x |set|). Grouping the mapping once makes the iteration sparse, as the
# GAMS `$` restriction these were transliterated from already is.
function _group(mapping, driver, val, key)
    # Order follows `driver`, the set the unindexed filter scanned, so each sum
    # accumulates its terms in the original sequence; floating point addition is
    # not associative.
    order = Dict(x => i for (i, x) in enumerate(driver))
    ix = Dict()
    for item in mapping
        k = length(key) == 1 ? item[key[1]] : Tuple(item[i] for i in key)
        push!(get!(() -> [], ix, k), item[val])
    end
    for (_, v) in ix
        sort!(v, by = x -> order[x])
    end
    return ix
end

mTimesliceFamily_ix = _group(mTimesliceFamily, timeslice, 2, (1,));
mRegionFamily_ix = _group(mRegionFamily, region, 2, (1,));
mTechInpCommSameTimeslice_ix = _group(mTechInpCommSameTimeslice, tech, 1, (2,));
mTechInpCommAgg_ix = _group(mTechInpCommAgg, tech, 1, (2,));
mTechAInpCommSameTimeslice_ix = _group(mTechAInpCommSameTimeslice, tech, 1, (2,));
mTechAInpCommAgg_ix = _group(mTechAInpCommAgg, tech, 1, (2,));
mTechOutCommSameTimeslice_ix = _group(mTechOutCommSameTimeslice, tech, 1, (2,));
mTechOutCommAgg_ix = _group(mTechOutCommAgg, tech, 1, (2,));
mTechAOutCommSameTimeslice_ix = _group(mTechAOutCommSameTimeslice, tech, 1, (2,));
mTechAOutCommAgg_ix = _group(mTechAOutCommAgg, tech, 1, (2,));
mTechInpCommAggTimeslice_ix = _group(mTechInpCommAggTimeslice, timeslice, 3, (1, 2, 4));
mTechAInpCommAggTimeslice_ix = _group(mTechAInpCommAggTimeslice, timeslice, 3, (1, 2, 4));
mTechOutCommAggTimeslice_ix = _group(mTechOutCommAggTimeslice, timeslice, 3, (1, 2, 4));
mTechAOutCommAggTimeslice_ix = _group(mTechAOutCommAggTimeslice, timeslice, 3, (1, 2, 4));

# Indices for the remaining domain-restriction filters; see _group.
mAggregateFactor_ix = _group(mAggregateFactor, comm, 2, (1,));
mCommTimesliceOrParent_ix = _group(mCommTimesliceOrParent, timeslice, 3, (1, 2,));
mCommTimeslice_ix = _group(mCommTimeslice, timeslice, 2, (1,));
mDemComm_ix = _group(mDemComm, dem, 1, (2,));
mDummyExportCost_ix = _group(mDummyExportCost, comm, 1, (2, 3,));
mDummyExport_ix = _group(mDummyExport, timeslice, 4, (1, 2, 3,));
mDummyImportCost_ix = _group(mDummyImportCost, comm, 1, (2, 3,));
mDummyImport_ix = _group(mDummyImport, timeslice, 4, (1, 2, 3,));
mExpComm_ix = _group(mExpComm, expp, 1, (2,));
mExportIrCost_ix = _group(mExportIrCost, trade, 1, (2, 3,));
mExportRowCost_ix = _group(mExportRowCost, expp, 1, (2, 3,));
mImpComm_ix = _group(mImpComm, imp, 1, (2,));
mImportIrCost_ix = _group(mImportIrCost, trade, 1, (2, 3,));
mImportRowCost_ix = _group(mImportRowCost, imp, 1, (2, 3,));
mStorageAInpCommAggTimeslice_ix = _group(mStorageAInpCommAggTimeslice, timeslice, 3, (1, 2, 4,));
mStorageAInpCommAgg_ix = _group(mStorageAInpCommAgg, stg, 1, (2,));
mStorageAInpCommSameTimeslice_ix = _group(mStorageAInpCommSameTimeslice, stg, 1, (2,));
mStorageAOutCommAggTimeslice_ix = _group(mStorageAOutCommAggTimeslice, timeslice, 3, (1, 2, 4,));
mStorageAOutCommAgg_ix = _group(mStorageAOutCommAgg, stg, 1, (2,));
mStorageAOutCommSameTimeslice_ix = _group(mStorageAOutCommSameTimeslice, stg, 1, (2,));
mStorageEac_ix = _group(mStorageEac, stg, 1, (2, 3,));
mStorageFixom_ix = _group(mStorageFixom, stg, 1, (2, 3,));
mStorageInpCommAggTimeslice_ix = _group(mStorageInpCommAggTimeslice, timeslice, 3, (1, 2, 4,));
mStorageInpCommAgg_ix = _group(mStorageInpCommAgg, stg, 1, (2,));
mStorageInpCommSameTimeslice_ix = _group(mStorageInpCommSameTimeslice, stg, 1, (2,));
mStorageInpComm_ix = _group(mStorageInpComm, comm, 2, (1,));
mStorageOutCommAggTimeslice_ix = _group(mStorageOutCommAggTimeslice, timeslice, 3, (1, 2, 4,));
mStorageOutCommAgg_ix = _group(mStorageOutCommAgg, stg, 1, (2,));
mStorageOutCommSameTimeslice_ix = _group(mStorageOutCommSameTimeslice, stg, 1, (2,));
mStorageOutComm_ix = _group(mStorageOutComm, comm, 2, (1,));
mStorageRetCost_ix = _group(mStorageRetCost, stg, 1, (2, 3,));
mStorageStgComm_ix = _group(mStorageStgComm, comm, 2, (1,));
mStorageVarom_ix = _group(mStorageVarom, stg, 1, (2, 3,));
mSubCost_ix = _group(mSubCost, comm, 1, (2, 3,));
mTaxCost_ix = _group(mTaxCost, comm, 1, (2, 3,));
mTechEac_ix = _group(mTechEac, tech, 1, (2, 3,));
mTechFixom_ix = _group(mTechFixom, tech, 1, (2, 3,));
mTechGroupComm_ix = _group(mTechGroupComm, comm, 3, (1, 2,));
mTechInpComm_ix0_1 = _group(mTechInpComm, tech, 1, (2,));
mTechInpComm_ix1_0 = _group(mTechInpComm, comm, 2, (1,));
mTechOutComm_ix = _group(mTechOutComm, comm, 2, (1,));
mTechRetCost_ix = _group(mTechRetCost, tech, 1, (2, 3,));
mTechTimeslice_ix = _group(mTechTimeslice, timeslice, 2, (1,));
mTechVarom_ix = _group(mTechVarom, tech, 1, (2, 3,));
mTimesliceParentChildE_ix = _group(mTimesliceParentChildE, timeslice, 2, (1,));
mTradeComm_ix = _group(mTradeComm, comm, 2, (1,));
mTradeEac_ix = _group(mTradeEac, trade, 1, (2, 3,));
mTradeFixom_ix = _group(mTradeFixom, trade, 1, (2, 3,));
mTradeIrCdst2Ainp_ix = _group(mTradeIrCdst2Ainp, region, 3, (1, 2, 4, 5, 6,));
mTradeIrCdst2Aout_ix = _group(mTradeIrCdst2Aout, region, 3, (1, 2, 4, 5, 6,));
mTradeIrCsrc2Ainp_ix = _group(mTradeIrCsrc2Ainp, region, 4, (1, 2, 3, 5, 6,));
mTradeIrCsrc2Aout_ix = _group(mTradeIrCsrc2Aout, region, 4, (1, 2, 3, 5, 6,));
mTradeRetCost_ix = _group(mTradeRetCost, trade, 1, (2, 3,));
mTradeRoutes_ix1_02 = _group(mTradeRoutes, region, 2, (1, 3,));
mTradeRoutes_ix2_01 = _group(mTradeRoutes, region, 3, (1, 2,));
mTradeTimeslice_ix = _group(mTradeTimeslice, timeslice, 2, (1,));
mvStorageOut_ix = _group(mvStorageOut, comm, 2, (1, 3, 4, 5,));
mvStorageRetiredNewCap_ix = _group(mvStorageRetiredNewCap, year, 3, (1, 2, 4,));
mvSupCost_ix = _group(mvSupCost, sup, 1, (2, 3,));
mvTechAInp_ix = _group(mvTechAInp, comm, 2, (1, 3, 4, 5,));
mvTechAOut_ix = _group(mvTechAOut, comm, 2, (1, 3, 4, 5,));
mvTechRetiredNewCap_ix2_013 = _group(mvTechRetiredNewCap, year, 3, (1, 2, 4,));
mvTechRetiredNewCap_ix3_012 = _group(mvTechRetiredNewCap, year, 4, (1, 2, 3,));
mvTotalCost_ix = _group(mvTotalCost, year, 2, (1,));



# A weather-dependent process carries one weather variate, or a handful; the
# `weather` set reaches thousands on models with per-cluster profiles. The
# equations below transliterate a GAMS `$` domain restriction, which GAMS
# iterates sparsely, so evaluating it as a filter over the whole set makes
# constraint construction O(rows x |weather|). Grouping each mapping once
# restores sparse iteration.
function _weather_index(mapping)
    # Order follows `weather` iteration order, the one the unindexed filter
    # produced, so each product keeps its factors in their original sequence;
    # floating point multiplication is not associative.
    order = Dict(w => i for (i, w) in enumerate(weather))
    ix = Dict()
    for item in mapping
        key = length(item) == 2 ? item[2] : Tuple(item[2:end])
        push!(get!(() -> [], ix, key), item[1])
    end
    for (_, v) in ix
        sort!(v, by = w -> order[w])
    end
    return ix
end

# (weather, region) -> the region whose pWeather series to read. `_group`
# rather than `_weather_index`: that helper keys on everything AFTER the
# weather name and emits the weather, which is the wrong way round here.
mWeatherRegionAt_ix = _group(mWeatherRegionAt, region, 3, (1, 2,));

mTechWeatherAfLo_ix = _weather_index(mTechWeatherAfLo);
mTechWeatherAfUp_ix = _weather_index(mTechWeatherAfUp);
mTechWeatherAfsLo_ix = _weather_index(mTechWeatherAfsLo);
mTechWeatherAfsUp_ix = _weather_index(mTechWeatherAfsUp);
mTechWeatherAfcLo_ix = _weather_index(mTechWeatherAfcLo);
mTechWeatherAfcUp_ix = _weather_index(mTechWeatherAfcUp);
mSupWeatherLo_ix = _weather_index(mSupWeatherLo);
mSupWeatherUp_ix = _weather_index(mSupWeatherUp);
mStorageWeatherAfLo_ix = _weather_index(mStorageWeatherAfLo);
mStorageWeatherAfUp_ix = _weather_index(mStorageWeatherAfUp);
mStorageWeatherInpAfLo_ix = _weather_index(mStorageWeatherInpAfLo);
mStorageWeatherInpAfUp_ix = _weather_index(mStorageWeatherInpAfUp);
mStorageWeatherOutAfLo_ix = _weather_index(mStorageWeatherOutAfLo);
mStorageWeatherOutAfUp_ix = _weather_index(mStorageWeatherOutAfUp);

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
@variable(model, vTechStockCap[mTechSpan] >= 0);
@variable(model, vTechStockPhaseOut[mvTechPhaseOut] >= 0);
@variable(model, vTechRetiredStock[mvTechRetiredStock] >= 0);
@variable(model, vTechRetiredNewCap[mvTechRetiredNewCap] >= 0);
@variable(model, vTechPhaseOut[mvTechPhaseOut] >= 0);
@variable(model, vStoragePhaseOut[mvStoragePhaseOut] >= 0);
@variable(model, vStorageStockPhaseOut[mvStoragePhaseOut] >= 0);
@variable(model, vStorageOutStockCap[mStorageSpan] >= 0);
@variable(model, vStorageOutRetiredStock[mvStorageRetiredStock] >= 0);
@variable(model, vStorageOutRetiredNewCap[mvStorageRetiredNewCap] >= 0);
@variable(model, vStorageInpStockCap[mStorageInpCap] >= 0);
@variable(model, vStorageInpRetiredStock[mvStorageRetiredStock] >= 0);
@variable(model, vStorageInpRetiredNewCap[mvStorageRetiredNewCap] >= 0);
@variable(model, vStorageStgStockCap[mStorageStgCap] >= 0);
@variable(model, vStorageStgRetiredStock[mvStorageRetiredStock] >= 0);
@variable(model, vStorageStgRetiredNewCap[mvStorageRetiredNewCap] >= 0);
@variable(model, vStorageRetCost[mStorageRetCost]);
@variable(model, vTradeStockCap[mTradeSpan] >= 0);
@variable(model, vTradePhaseOut[mvTradePhaseOut] >= 0);
@variable(model, vTradeStockPhaseOut[mvTradePhaseOut] >= 0);
@variable(model, vTradeRetiredStock[mvTradeRetiredStock] >= 0);
@variable(model, vTradeRetiredNewCap[mvTradeRetiredNewCap] >= 0);
@variable(model, vTradeRetCost[mTradeRetCost]);
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
@variable(model, vStorageInp[mvStorageInp] >= 0);
@variable(model, vStorageOut[mvStorageOut] >= 0);
@variable(model, vStorageLevel[mvStorageLevel] >= 0);
@variable(model, vStorageInv[mStorageNew] >= 0);
@variable(model, vStorageEac[mStorageEac] >= 0);
@variable(model, vStorageOutCap[mStorageSpan] >= 0);
@variable(model, vStorageStgCap[mStorageStgCap] >= 0);
@variable(model, vStorageInpCap[mStorageInpCap] >= 0);
@variable(model, vStorageInpNewCap[mStorageInpNew] >= 0);
@variable(model, vStorageStgNewCap[mStorageStgNew] >= 0);
@variable(model, vStorageOutNewCap[mStorageNew] >= 0);
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
# eqTechSng2Sng(tech, region, comm, commp, year, timeslice)$meqTechSng2Sng(tech, region, comm, commp, year, timeslice)
print("eqTechSng2Sng(tech, region, comm, commp, year, timeslice)...")
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
# eqTechGrp2Sng(tech, region, group, commp, year, timeslice)$meqTechGrp2Sng(tech, region, group, commp, year, timeslice)
print("eqTechGrp2Sng(tech, region, group, commp, year, timeslice)...")
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
        ) for c in get(mTechGroupComm_ix, (t, g), ())
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
# eqTechSng2Grp(tech, region, comm, groupp, year, timeslice)$meqTechSng2Grp(tech, region, comm, groupp, year, timeslice)
print("eqTechSng2Grp(tech, region, comm, groupp, year, timeslice)...")
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
        ) for cp in get(mTechGroupComm_ix, (t, gp), ())
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqTechGrp2Grp(tech, region, group, groupp, year, timeslice)$meqTechGrp2Grp(tech, region, group, groupp, year, timeslice)
print("eqTechGrp2Grp(tech, region, group, groupp, year, timeslice)...")
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
        ) for c in get(mTechGroupComm_ix, (t, g), ())
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
        ) for cp in get(mTechGroupComm_ix, (t, gp), ())
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqTechShareInpLo(tech, region, group, comm, year, timeslice)$meqTechShareInpLo(tech, region, group, comm, year, timeslice)
print("eqTechShareInpLo(tech, region, group, comm, year, timeslice)...")
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
        ) for cp in get(mTechGroupComm_ix, (t, g), ())
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqTechShareInpUp(tech, region, group, comm, year, timeslice)$meqTechShareInpUp(tech, region, group, comm, year, timeslice)
print("eqTechShareInpUp(tech, region, group, comm, year, timeslice)...")
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
        ) for cp in get(mTechGroupComm_ix, (t, g), ())
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqTechShareOutLo(tech, region, group, comm, year, timeslice)$meqTechShareOutLo(tech, region, group, comm, year, timeslice)
print("eqTechShareOutLo(tech, region, group, comm, year, timeslice)...")
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
        ) for cp in get(mTechGroupComm_ix, (t, g), ())
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqTechShareOutUp(tech, region, group, comm, year, timeslice)$meqTechShareOutUp(tech, region, group, comm, year, timeslice)
print("eqTechShareOutUp(tech, region, group, comm, year, timeslice)...")
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
        ) for cp in get(mTechGroupComm_ix, (t, g), ())
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqTechAInp(tech, comm, region, year, timeslice)$mvTechAInp(tech, comm, region, year, timeslice)
print("eqTechAInp(tech, comm, region, year, timeslice)...")
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
    (
        if (t, c, r, y, s) in mTechPho2AInp
            ((if (t, r, y) in mvTechPhaseOut; vTechPhaseOut[(t, r, y)]; else; 0; end) * (
                if haskey(pTechPho2AInp, (t, c, r, y, s))
                    pTechPho2AInp[(t, c, r, y, s)]
                else
                    pTechPho2AInpDef
                end
            ))
        else
            0
        end
    ) +
    (
        if (t, c, r, y, s) in mTechRet2AInp
            (((if (t, r, y) in mvTechRetiredStock; vTechRetiredStock[(t, r, y)]; else; 0; end) + sum(vTechRetiredNewCap[(t, r, yp, y)] for yp in get(mvTechRetiredNewCap_ix2_013, (t, r, y), ()))) * (
                if haskey(pTechRet2AInp, (t, c, r, y, s))
                    pTechRet2AInp[(t, c, r, y, s)]
                else
                    pTechRet2AInpDef
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
# eqTechAOut(tech, comm, region, year, timeslice)$mvTechAOut(tech, comm, region, year, timeslice)
print("eqTechAOut(tech, comm, region, year, timeslice)...")
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
    (
        if (t, c, r, y, s) in mTechPho2AOut
            ((if (t, r, y) in mvTechPhaseOut; vTechPhaseOut[(t, r, y)]; else; 0; end) * (
                if haskey(pTechPho2AOut, (t, c, r, y, s))
                    pTechPho2AOut[(t, c, r, y, s)]
                else
                    pTechPho2AOutDef
                end
            ))
        else
            0
        end
    ) +
    (
        if (t, c, r, y, s) in mTechRet2AOut
            (((if (t, r, y) in mvTechRetiredStock; vTechRetiredStock[(t, r, y)]; else; 0; end) + sum(vTechRetiredNewCap[(t, r, yp, y)] for yp in get(mvTechRetiredNewCap_ix2_013, (t, r, y), ()))) * (
                if haskey(pTechRet2AOut, (t, c, r, y, s))
                    pTechRet2AOut[(t, c, r, y, s)]
                else
                    pTechRet2AOutDef
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
# eqTechAfLo(tech, region, year, timeslice)$meqTechAfLo(tech, region, year, timeslice)
print("eqTechAfLo(tech, region, year, timeslice)...")
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
        if haskey(pTimesliceShare, (s))
            pTimesliceShare[(s)]
        else
            pTimesliceShareDef
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
            sum(
                if haskey(pWeather, (wth1, rw, y, s))
                    pWeather[(wth1, rw, y, s)]
                else
                    pWeatherDef
                end for rw in get(mWeatherRegionAt_ix, (wth1, r), ())
            ; init = 0)
        ) for wth1 in get(mTechWeatherAfLo_ix, t, ())
    ; init = 1) <= vTechAct[(t, r, y, s)]
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqTechAfUp(tech, region, year, timeslice)$meqTechAfUp(tech, region, year, timeslice)
print("eqTechAfUp(tech, region, year, timeslice)...")
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
        if haskey(pTimesliceShare, (s))
            pTimesliceShare[(s)]
        else
            pTimesliceShareDef
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
            sum(
                if haskey(pWeather, (wth1, rw, y, s))
                    pWeather[(wth1, rw, y, s)]
                else
                    pWeatherDef
                end for rw in get(mWeatherRegionAt_ix, (wth1, r), ())
            ; init = 0)
        ) for wth1 in get(mTechWeatherAfUp_ix, t, ())
    ; init = 1)
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqTechAfsLo(tech, region, year, timeslice)$meqTechAfsLo(tech, region, year, timeslice)
print("eqTechAfsLo(tech, region, year, timeslice)...")
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
        if haskey(pTimesliceShare, (s))
            pTimesliceShare[(s)]
        else
            pTimesliceShareDef
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
            sum(
                if haskey(pWeather, (wth1, rw, y, s))
                    pWeather[(wth1, rw, y, s)]
                else
                    pWeatherDef
                end for rw in get(mWeatherRegionAt_ix, (wth1, r), ())
            ; init = 0)
        ) for wth1 in get(mTechWeatherAfsLo_ix, t, ())
    ; init = 1) <= sum(
        (
            if haskey(pTimesliceAgg, (y, s, sp))
                pTimesliceAgg[(y, s, sp)]
            else
                pTimesliceAggDef
            end
        ) * (
            if (t, r, y, sp) in mvTechAct
                vTechAct[(t, r, y, sp)]
            else
                0
            end
        ) for sp in get(mTimesliceParentChildE_ix, s, ())
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqTechAfsUp(tech, region, year, timeslice)$meqTechAfsUp(tech, region, year, timeslice)
print("eqTechAfsUp(tech, region, year, timeslice)...")
@constraint(
    model,
    [(t, r, y, s) in meqTechAfsUp],
    sum(
        (
            if haskey(pTimesliceAgg, (y, s, sp))
                pTimesliceAgg[(y, s, sp)]
            else
                pTimesliceAggDef
            end
        ) * (
            if (t, r, y, sp) in mvTechAct
                vTechAct[(t, r, y, sp)]
            else
                0
            end
        ) for sp in get(mTimesliceParentChildE_ix, s, ())
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
        if haskey(pTimesliceShare, (s))
            pTimesliceShare[(s)]
        else
            pTimesliceShareDef
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
            sum(
                if haskey(pWeather, (wth1, rw, y, s))
                    pWeather[(wth1, rw, y, s)]
                else
                    pWeatherDef
                end for rw in get(mWeatherRegionAt_ix, (wth1, r), ())
            ; init = 0)
        ) for wth1 in get(mTechWeatherAfsUp_ix, t, ())
    ; init = 1)
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqTechRampUp(tech, region, year, timeslice, timeslicep)$mTechRampUp(tech, region, year, timeslice, timeslicep)
print("eqTechRampUp(tech, region, year, timeslice, timeslicep)...")
@constraint(
    model,
    [(t, r, y, s, sp) in mTechRampUp],
    (vTechAct[(t, r, y, sp)]) / ((
        if haskey(pTimesliceShare, (sp))
            pTimesliceShare[(sp)]
        else
            pTimesliceShareDef
        end
    )) - (vTechAct[(t, r, y, s)]) / ((
        if haskey(pTimesliceShare, (s))
            pTimesliceShare[(s)]
        else
            pTimesliceShareDef
        end
    )) <=
    (
        (
            if haskey(pTimesliceShare, (s))
                pTimesliceShare[(s)]
            else
                pTimesliceShareDef
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
# eqTechRampDown(tech, region, year, timeslice, timeslicep)$mTechRampDown(tech, region, year, timeslice, timeslicep)
print("eqTechRampDown(tech, region, year, timeslice, timeslicep)...")
@constraint(
    model,
    [(t, r, y, s, sp) in mTechRampDown],
    (vTechAct[(t, r, y, s)]) / ((
        if haskey(pTimesliceShare, (s))
            pTimesliceShare[(s)]
        else
            pTimesliceShareDef
        end
    )) - (vTechAct[(t, r, y, sp)]) / ((
        if haskey(pTimesliceShare, (sp))
            pTimesliceShare[(sp)]
        else
            pTimesliceShareDef
        end
    )) <=
    (
        (
            if haskey(pTimesliceShare, (s))
                pTimesliceShare[(s)]
            else
                pTimesliceShareDef
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
# eqTechActSng(tech, comm, region, year, timeslice)$meqTechActSng(tech, comm, region, year, timeslice)
print("eqTechActSng(tech, comm, region, year, timeslice)...")
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
# eqTechActGrp(tech, group, region, year, timeslice)$meqTechActGrp(tech, group, region, year, timeslice)
print("eqTechActGrp(tech, group, region, year, timeslice)...")
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
        ) for c in get(mTechGroupComm_ix, (t, g), ())
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqTechAfcOutLo(tech, region, comm, year, timeslice)$meqTechAfcOutLo(tech, region, comm, year, timeslice)
print("eqTechAfcOutLo(tech, region, comm, year, timeslice)...")
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
        if haskey(pTimesliceShare, (s))
            pTimesliceShare[(s)]
        else
            pTimesliceShareDef
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
            sum(
                if haskey(pWeather, (wth1, rw, y, s))
                    pWeather[(wth1, rw, y, s)]
                else
                    pWeatherDef
                end for rw in get(mWeatherRegionAt_ix, (wth1, r), ())
            ; init = 0)
        ) for wth1 in get(mTechWeatherAfcLo_ix, (t, c), ())
    ; init = 1) <= vTechOut[(t, c, r, y, s)]
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqTechAfcOutUp(tech, region, comm, year, timeslice)$meqTechAfcOutUp(tech, region, comm, year, timeslice)
print("eqTechAfcOutUp(tech, region, comm, year, timeslice)...")
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
            sum(
                if haskey(pWeather, (wth1, rw, y, s))
                    pWeather[(wth1, rw, y, s)]
                else
                    pWeatherDef
                end for rw in get(mWeatherRegionAt_ix, (wth1, r), ())
            ; init = 0)
        ) for wth1 in get(mTechWeatherAfcUp_ix, (t, c), ())
    ; init = 1)
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqTechAfcInpLo(tech, region, comm, year, timeslice)$meqTechAfcInpLo(tech, region, comm, year, timeslice)
print("eqTechAfcInpLo(tech, region, comm, year, timeslice)...")
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
        if haskey(pTimesliceShare, (s))
            pTimesliceShare[(s)]
        else
            pTimesliceShareDef
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
            sum(
                if haskey(pWeather, (wth1, rw, y, s))
                    pWeather[(wth1, rw, y, s)]
                else
                    pWeatherDef
                end for rw in get(mWeatherRegionAt_ix, (wth1, r), ())
            ; init = 0)
        ) for wth1 in get(mTechWeatherAfcLo_ix, (t, c), ())
    ; init = 1) <= vTechInp[(t, c, r, y, s)]
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqTechAfcInpUp(tech, region, comm, year, timeslice)$meqTechAfcInpUp(tech, region, comm, year, timeslice)
print("eqTechAfcInpUp(tech, region, comm, year, timeslice)...")
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
        if haskey(pTimesliceShare, (s))
            pTimesliceShare[(s)]
        else
            pTimesliceShareDef
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
            sum(
                if haskey(pWeather, (wth1, rw, y, s))
                    pWeather[(wth1, rw, y, s)]
                else
                    pWeatherDef
                end for rw in get(mWeatherRegionAt_ix, (wth1, r), ())
            ; init = 0)
        ) for wth1 in get(mTechWeatherAfcUp_ix, (t, c), ())
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
    vTechStockCap[(t, r, y)] + sum(
        (
            if haskey(pPeriodLen, (yp))
                pPeriodLen[(yp)]
            else
                pPeriodLenDef
            end
        ) * vTechNewCap[(t, r, yp)] - sum(
            vTechRetiredNewCap[(t, r, yp, ye)] * (
                if haskey(pPeriodLen, (ye))
                    pPeriodLen[(ye)]
                else
                    pPeriodLenDef
                end
            ) for ye in year if
            ((t, r, yp, ye) in mvTechRetiredNewCap && ordYear[(y)] >= ordYear[(ye)])
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
        ) for yp in get(mvTechRetiredNewCap_ix3_012, (t, r, y), ())
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
# eqTechStockCap(tech, region, year)$mTechSpan(tech, region, year)
# The legacy fleet as a recursive balance: pTechStockSurv thins whatever is
# ACTUALLY standing, so a declining schedule can never block early retirement.
print("eqTechStockCap(tech, region, year)...")
@constraint(
    model,
    [(t, r, y) in mTechSpan],
    vTechStockCap[(t, r, y)] ==
    (if haskey(pTechStockSurv, (t, r, y)); pTechStockSurv[(t, r, y)]; else; pTechStockSurvDef; end) *
    sum(vTechStockCap[(t, r, yp)] for yp in year
        if ((yp, y) in mMilestoneNext && (t, r, yp) in mTechSpan)) +
    (if haskey(pTechStockNew, (t, r, y)); pTechStockNew[(t, r, y)]; else; pTechStockNewDef; end) -
    (if (t, r, y) in mvTechRetiredStock; vTechRetiredStock[(t, r, y)]; else; 0; end) *
    (if haskey(pPeriodLen, (y)); pPeriodLen[(y)]; else; pPeriodLenDef; end)
);
print(" ", Dates.format(now(), "HH:MM:SS"), "
")
# eqTechStockPhaseOut(tech, region, year)$mvTechPhaseOut(tech, region, year)
print("eqTechStockPhaseOut(tech, region, year)...")
@constraint(
    model,
    [(t, r, y) in mvTechPhaseOut],
    vTechStockPhaseOut[(t, r, y)] * (if haskey(pPeriodLen, (y)); pPeriodLen[(y)]; else; pPeriodLenDef; end) ==
    (1 - (if haskey(pTechStockSurv, (t, r, y)); pTechStockSurv[(t, r, y)]; else; pTechStockSurvDef; end)) *
    sum(vTechStockCap[(t, r, yp)] for yp in year
        if ((yp, y) in mMilestoneNext && (t, r, yp) in mTechSpan))
);
print(" ", Dates.format(now(), "HH:MM:SS"), "
")
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
        vTechRetiredNewCap[(t, r, yp, y)] for
        yp in year if (t, r, yp, y) in mvTechRetiredNewCap
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
        vTechRetiredNewCap[(t, r, yp, y)] for
        yp in year if (t, r, yp, y) in mvTechRetiredNewCap
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
# eqTechPhaseOut(t, region, year)
print("eqTechPhaseOut(...)...")
@constraint(
    model,
    [(t, r, y) in mvTechPhaseOut],
    vTechPhaseOut[(t, r, y)] * (
        if haskey(pPeriodLen, (y))
            pPeriodLen[(y)]
        else
            pPeriodLenDef
        end
    ) == sum(
        vTechCap[(t, r, yp)] for
        yp in year if ((yp, y) in mMilestoneNext && (t, r, yp) in mTechSpan)
    ) - vTechCap[(t, r, y)] + (if (t, r, y) in mTechNew
        vTechNewCap[(t, r, y)]
    else
        0
    end) * (
        if haskey(pPeriodLen, (y))
            pPeriodLen[(y)]
        else
            pPeriodLenDef
        end
    ) - (
        (if (t, r, y) in mvTechRetiredStock; vTechRetiredStock[(t, r, y)]; else; 0; end)
        + sum(
            vTechRetiredNewCap[(t, r, yp, y)] for
            yp in year if (t, r, yp, y) in mvTechRetiredNewCap
        )
    ) * (
        if haskey(pPeriodLen, (y))
            pPeriodLen[(y)]
        else
            pPeriodLenDef
        end
    ) + (if haskey(pTechStockNew, (t, r, y)); pTechStockNew[(t, r, y)]; else; pTechStockNewDef; end)
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqStoragePhaseOut(st1, region, year)
print("eqStoragePhaseOut(...)...")
@constraint(
    model,
    [(st1, r, y) in mvStoragePhaseOut],
    vStoragePhaseOut[(st1, r, y)] * (
        if haskey(pPeriodLen, (y))
            pPeriodLen[(y)]
        else
            pPeriodLenDef
        end
    ) == sum(
        vStorageOutCap[(st1, r, yp)] for
        yp in year if ((yp, y) in mMilestoneNext && (st1, r, yp) in mStorageSpan)
    ) - vStorageOutCap[(st1, r, y)] + (if (st1, r, y) in mStorageNew
        vStorageOutNewCap[(st1, r, y)]
    else
        0
    end) * (
        if haskey(pPeriodLen, (y))
            pPeriodLen[(y)]
        else
            pPeriodLenDef
        end
    ) - (
        (if (st1, r, y) in mvStorageRetiredStock; vStorageOutRetiredStock[(st1, r, y)]; else; 0; end)
        + sum(
            vStorageOutRetiredNewCap[(st1, r, yp, y)] for
            yp in year if (st1, r, yp, y) in mvStorageRetiredNewCap
        )
    ) * (
        if haskey(pPeriodLen, (y))
            pPeriodLen[(y)]
        else
            pPeriodLenDef
        end
    ) + (if haskey(pStorageOutStockNew, (st1, r, y)); pStorageOutStockNew[(st1, r, y)]; else; pStorageOutStockNewDef; end)
);
# eqStorageStockPhaseOut -- what the schedule actually removed.
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
print("eqStorageStockPhaseOut...")
@constraint(
    model,
    [(st1, r, y) in mvStoragePhaseOut],
    vStorageStockPhaseOut[(st1, r, y)] * (if haskey(pPeriodLen, (y)); pPeriodLen[(y)]; else; pPeriodLenDef; end) ==
    (1 - (if haskey(pStorageOutStockSurv, (st1, r, y)); pStorageOutStockSurv[(st1, r, y)]; else; pStorageOutStockSurvDef; end)) *
    sum(vStorageOutStockCap[(st1, r, yp)] for yp in year
        if ((yp, y) in mMilestoneNext && (st1, r, yp) in mStorageSpan))
);
print(" ", Dates.format(now(), "HH:MM:SS"), "
")
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqStorageOutRetiredNewCap(stg, region, year)
print("eqStorageOutRetiredNewCap(stg, region, year)...")
@constraint(
    model,
    [(st1, r, y) in meqStorageRetiredNewCap],
    sum(
        vStorageOutRetiredNewCap[(st1, r, y, yp)] * (
            if haskey(pPeriodLen, (yp))
                pPeriodLen[(yp)]
            else
                pPeriodLenDef
            end
        ) for
        yp in year if (st1, r, y, yp) in mvStorageRetiredNewCap
    ) <= (
        if (st1, r, y) in mStorageNew
            vStorageOutNewCap[(st1, r, y)]
        else
            0
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
# eqStorageOutStockCap -- the legacy fleet as a recursive balance.
print("eqStorageOutStockCap...")
@constraint(
    model,
    [(st1, r, y) in mStorageSpan],
    vStorageOutStockCap[(st1, r, y)] ==
    (if haskey(pStorageOutStockSurv, (st1, r, y)); pStorageOutStockSurv[(st1, r, y)]; else; pStorageOutStockSurvDef; end) *
    sum(vStorageOutStockCap[(st1, r, yp)] for yp in year
        if ((yp, y) in mMilestoneNext && (st1, r, yp) in mStorageSpan)) +
    (if haskey(pStorageOutStockNew, (st1, r, y)); pStorageOutStockNew[(st1, r, y)]; else; pStorageOutStockNewDef; end) -
    (if (st1, r, y) in mvStorageRetiredStock; vStorageOutRetiredStock[(st1, r, y)]; else; 0; end) * (if haskey(pPeriodLen, (y)); pPeriodLen[(y)]; else; pPeriodLenDef; end)
);
print(" ", Dates.format(now(), "HH:MM:SS"), "
")
# eqStorageOutRetUp(stg, region, year)
print("eqStorageOutRetUp(stg, region, year)...")
@constraint(
    model,
    [(st1, r, y) in mStorageOutRetUp],
    (
        if (st1, r, y) in mvStorageRetiredStock
            vStorageOutRetiredStock[(st1, r, y)]
        else
            0
        end
    ) + sum(
        vStorageOutRetiredNewCap[(st1, r, yp, y)] for
        yp in year if (st1, r, yp, y) in mvStorageRetiredNewCap
    ) <= (
        if haskey(pStorageOutRetUp, (st1, r, y))
            pStorageOutRetUp[(st1, r, y)]
        else
            pStorageOutRetUpDef
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
# eqStorageOutRetLo(stg, region, year)
print("eqStorageOutRetLo(stg, region, year)...")
@constraint(
    model,
    [(st1, r, y) in mStorageOutRetLo],
    (
        if (st1, r, y) in mvStorageRetiredStock
            vStorageOutRetiredStock[(st1, r, y)]
        else
            0
        end
    ) + sum(
        vStorageOutRetiredNewCap[(st1, r, yp, y)] for
        yp in year if (st1, r, yp, y) in mvStorageRetiredNewCap
    ) >= (
        if haskey(pStorageOutRetLo, (st1, r, y))
            pStorageOutRetLo[(st1, r, y)]
        else
            pStorageOutRetLoDef
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
# eqStorageInpRetiredNewCap(stg, region, year)
print("eqStorageInpRetiredNewCap(stg, region, year)...")
@constraint(
    model,
    [(st1, r, y) in meqStorageRetiredNewCap],
    sum(
        vStorageInpRetiredNewCap[(st1, r, y, yp)] * (
            if haskey(pPeriodLen, (yp))
                pPeriodLen[(yp)]
            else
                pPeriodLenDef
            end
        ) for
        yp in year if (st1, r, y, yp) in mvStorageRetiredNewCap
    ) <= (
        if (st1, r, y) in mStorageInpNew
            vStorageInpNewCap[(st1, r, y)]
        else
            0
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
# eqStorageInpStockCap -- the legacy fleet as a recursive balance.
print("eqStorageInpStockCap...")
@constraint(
    model,
    [(st1, r, y) in mStorageInpCap],
    vStorageInpStockCap[(st1, r, y)] ==
    (if haskey(pStorageInpStockSurv, (st1, r, y)); pStorageInpStockSurv[(st1, r, y)]; else; pStorageInpStockSurvDef; end) *
    sum(vStorageInpStockCap[(st1, r, yp)] for yp in year
        if ((yp, y) in mMilestoneNext && (st1, r, yp) in mStorageInpCap)) +
    (if haskey(pStorageInpStockNew, (st1, r, y)); pStorageInpStockNew[(st1, r, y)]; else; pStorageInpStockNewDef; end) -
    (if (st1, r, y) in mvStorageRetiredStock; vStorageInpRetiredStock[(st1, r, y)]; else; 0; end) * (if haskey(pPeriodLen, (y)); pPeriodLen[(y)]; else; pPeriodLenDef; end)
);
print(" ", Dates.format(now(), "HH:MM:SS"), "
")
# eqStorageInpRetUp(stg, region, year)
print("eqStorageInpRetUp(stg, region, year)...")
@constraint(
    model,
    [(st1, r, y) in mStorageInpRetUp],
    (
        if (st1, r, y) in mvStorageRetiredStock
            vStorageInpRetiredStock[(st1, r, y)]
        else
            0
        end
    ) + sum(
        vStorageInpRetiredNewCap[(st1, r, yp, y)] for
        yp in year if (st1, r, yp, y) in mvStorageRetiredNewCap
    ) <= (
        if haskey(pStorageInpRetUp, (st1, r, y))
            pStorageInpRetUp[(st1, r, y)]
        else
            pStorageInpRetUpDef
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
# eqStorageInpRetLo(stg, region, year)
print("eqStorageInpRetLo(stg, region, year)...")
@constraint(
    model,
    [(st1, r, y) in mStorageInpRetLo],
    (
        if (st1, r, y) in mvStorageRetiredStock
            vStorageInpRetiredStock[(st1, r, y)]
        else
            0
        end
    ) + sum(
        vStorageInpRetiredNewCap[(st1, r, yp, y)] for
        yp in year if (st1, r, yp, y) in mvStorageRetiredNewCap
    ) >= (
        if haskey(pStorageInpRetLo, (st1, r, y))
            pStorageInpRetLo[(st1, r, y)]
        else
            pStorageInpRetLoDef
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
# eqStorageStgRetiredNewCap(stg, region, year)
print("eqStorageStgRetiredNewCap(stg, region, year)...")
@constraint(
    model,
    [(st1, r, y) in meqStorageRetiredNewCap],
    sum(
        vStorageStgRetiredNewCap[(st1, r, y, yp)] * (
            if haskey(pPeriodLen, (yp))
                pPeriodLen[(yp)]
            else
                pPeriodLenDef
            end
        ) for
        yp in year if (st1, r, y, yp) in mvStorageRetiredNewCap
    ) <= (
        if (st1, r, y) in mStorageStgNew
            vStorageStgNewCap[(st1, r, y)]
        else
            0
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
# eqStorageStgStockCap -- the legacy fleet as a recursive balance.
print("eqStorageStgStockCap...")
@constraint(
    model,
    [(st1, r, y) in mStorageStgCap],
    vStorageStgStockCap[(st1, r, y)] ==
    (if haskey(pStorageStgStockSurv, (st1, r, y)); pStorageStgStockSurv[(st1, r, y)]; else; pStorageStgStockSurvDef; end) *
    sum(vStorageStgStockCap[(st1, r, yp)] for yp in year
        if ((yp, y) in mMilestoneNext && (st1, r, yp) in mStorageStgCap)) +
    (if haskey(pStorageStgStockNew, (st1, r, y)); pStorageStgStockNew[(st1, r, y)]; else; pStorageStgStockNewDef; end) -
    (if (st1, r, y) in mvStorageRetiredStock; vStorageStgRetiredStock[(st1, r, y)]; else; 0; end) * (if haskey(pPeriodLen, (y)); pPeriodLen[(y)]; else; pPeriodLenDef; end)
);
print(" ", Dates.format(now(), "HH:MM:SS"), "
")
# eqStorageStgRetUp(stg, region, year)
print("eqStorageStgRetUp(stg, region, year)...")
@constraint(
    model,
    [(st1, r, y) in mStorageStgRetUp],
    (
        if (st1, r, y) in mvStorageRetiredStock
            vStorageStgRetiredStock[(st1, r, y)]
        else
            0
        end
    ) + sum(
        vStorageStgRetiredNewCap[(st1, r, yp, y)] for
        yp in year if (st1, r, yp, y) in mvStorageRetiredNewCap
    ) <= (
        if haskey(pStorageStgRetUp, (st1, r, y))
            pStorageStgRetUp[(st1, r, y)]
        else
            pStorageStgRetUpDef
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
# eqStorageStgRetLo(stg, region, year)
print("eqStorageStgRetLo(stg, region, year)...")
@constraint(
    model,
    [(st1, r, y) in mStorageStgRetLo],
    (
        if (st1, r, y) in mvStorageRetiredStock
            vStorageStgRetiredStock[(st1, r, y)]
        else
            0
        end
    ) + sum(
        vStorageStgRetiredNewCap[(st1, r, yp, y)] for
        yp in year if (st1, r, yp, y) in mvStorageRetiredNewCap
    ) >= (
        if haskey(pStorageStgRetLo, (st1, r, y))
            pStorageStgRetLo[(st1, r, y)]
        else
            pStorageStgRetLoDef
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
# eqStorageRetCost(stg, region, year)
print("eqStorageRetCost(stg, region, year)...")
@constraint(
    model,
    [(st1, r, y) in mStorageRetCost],
    vStorageRetCost[(st1, r, y)] ==
    (
        if haskey(pStorageOutRetCost, (st1, r, y))
            pStorageOutRetCost[(st1, r, y)]
        else
            pStorageOutRetCostDef
        end
    ) * (
        (
            if (st1, r, y) in mvStorageRetiredStock
                vStorageOutRetiredStock[(st1, r, y)]
            else
                0
            end
        ) + sum(
            vStorageOutRetiredNewCap[(st1, r, yp, y)] for
            yp in year if (st1, r, yp, y) in mvStorageRetiredNewCap
        )
    ) +
    (
        if haskey(pStorageInpRetCost, (st1, r, y))
            pStorageInpRetCost[(st1, r, y)]
        else
            pStorageInpRetCostDef
        end
    ) * (
        (
            if (st1, r, y) in mvStorageRetiredStock
                vStorageInpRetiredStock[(st1, r, y)]
            else
                0
            end
        ) + sum(
            vStorageInpRetiredNewCap[(st1, r, yp, y)] for
            yp in year if (st1, r, yp, y) in mvStorageRetiredNewCap
        )
    ) +
    (
        if haskey(pStorageStgRetCost, (st1, r, y))
            pStorageStgRetCost[(st1, r, y)]
        else
            pStorageStgRetCostDef
        end
    ) * (
        (
            if (st1, r, y) in mvStorageRetiredStock
                vStorageStgRetiredStock[(st1, r, y)]
            else
                0
            end
        ) + sum(
            vStorageStgRetiredNewCap[(st1, r, yp, y)] for
            yp in year if (st1, r, yp, y) in mvStorageRetiredNewCap
        )
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqTradeRetiredNewCap(trade, year)
print("eqTradeRetiredNewCap(trade, year)...")
@constraint(
    model,
    [(t1, y) in meqTradeRetiredNewCap],
    sum(
        vTradeRetiredNewCap[(t1, y, yp)] * (
            if haskey(pPeriodLen, (yp))
                pPeriodLen[(yp)]
            else
                pPeriodLenDef
            end
        ) for
        yp in year if (t1, y, yp) in mvTradeRetiredNewCap
    ) <= (
        if (t1, y) in mTradeNew
            vTradeNewCap[(t1, y)]
        else
            0
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
# eqTradeStockCap -- the legacy fleet as a recursive balance.
print("eqTradeStockCap...")
@constraint(
    model,
    [(t1, y) in mTradeSpan],
    vTradeStockCap[(t1, y)] ==
    (if haskey(pTradeStockSurv, (t1, y)); pTradeStockSurv[(t1, y)]; else; pTradeStockSurvDef; end) *
    sum(vTradeStockCap[(t1, yp)] for yp in year
        if ((yp, y) in mMilestoneNext && (t1, yp) in mTradeSpan)) +
    (if haskey(pTradeStockNew, (t1, y)); pTradeStockNew[(t1, y)]; else; pTradeStockNewDef; end) -
    (if (t1, y) in mvTradeRetiredStock; vTradeRetiredStock[(t1, y)]; else; 0; end) * (if haskey(pPeriodLen, (y)); pPeriodLen[(y)]; else; pPeriodLenDef; end)
);
# eqTradePhaseOut -- end-of-life departure, read off the capacity balance.
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
print("eqTradePhaseOut...")
@constraint(
    model,
    [(t1, y) in mvTradePhaseOut],
    vTradePhaseOut[(t1, y)] * (if haskey(pPeriodLen, (y)); pPeriodLen[(y)]; else; pPeriodLenDef; end) ==
    sum(vTradeCap[(t1, yp)] for yp in year
        if ((yp, y) in mMilestoneNext && (t1, yp) in mTradeSpan)) -
    vTradeCap[(t1, y)] + (if (t1, y) in mTradeNew; vTradeNewCap[(t1, y)]; else; 0; end) * (if haskey(pPeriodLen, (y)); pPeriodLen[(y)]; else; pPeriodLenDef; end) -
    ((if (t1, y) in mvTradeRetiredStock; vTradeRetiredStock[(t1, y)]; else; 0; end) +
     sum(vTradeRetiredNewCap[(t1, yp, y)] for yp in year
         if (t1, yp, y) in mvTradeRetiredNewCap)) * (if haskey(pPeriodLen, (y)); pPeriodLen[(y)]; else; pPeriodLenDef; end) +
    (if haskey(pTradeStockNew, (t1, y)); pTradeStockNew[(t1, y)]; else; pTradeStockNewDef; end)
);
print(" ", Dates.format(now(), "HH:MM:SS"), "
")
# eqTradeStockPhaseOut -- what the schedule actually removed.
print("eqTradeStockPhaseOut...")
@constraint(
    model,
    [(t1, y) in mvTradePhaseOut],
    vTradeStockPhaseOut[(t1, y)] * (if haskey(pPeriodLen, (y)); pPeriodLen[(y)]; else; pPeriodLenDef; end) ==
    (1 - (if haskey(pTradeStockSurv, (t1, y)); pTradeStockSurv[(t1, y)]; else; pTradeStockSurvDef; end)) *
    sum(vTradeStockCap[(t1, yp)] for yp in year
        if ((yp, y) in mMilestoneNext && (t1, yp) in mTradeSpan))
);
print(" ", Dates.format(now(), "HH:MM:SS"), "
")
print(" ", Dates.format(now(), "HH:MM:SS"), "
")
# eqTradeRetUp(trade, year)
print("eqTradeRetUp(trade, year)...")
@constraint(
    model,
    [(t1, y) in mTradeRetUp],
    (
        if (t1, y) in mvTradeRetiredStock
            vTradeRetiredStock[(t1, y)]
        else
            0
        end
    ) + sum(
        vTradeRetiredNewCap[(t1, yp, y)] for
        yp in year if (t1, yp, y) in mvTradeRetiredNewCap
    ) <= (
        if haskey(pTradeRetUp, (t1, y))
            pTradeRetUp[(t1, y)]
        else
            pTradeRetUpDef
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
# eqTradeRetLo(trade, year)
print("eqTradeRetLo(trade, year)...")
@constraint(
    model,
    [(t1, y) in mTradeRetLo],
    (
        if (t1, y) in mvTradeRetiredStock
            vTradeRetiredStock[(t1, y)]
        else
            0
        end
    ) + sum(
        vTradeRetiredNewCap[(t1, yp, y)] for
        yp in year if (t1, yp, y) in mvTradeRetiredNewCap
    ) >= (
        if haskey(pTradeRetLo, (t1, y))
            pTradeRetLo[(t1, y)]
        else
            pTradeRetLoDef
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
# eqTradeRetCost(trade, region, year)
print("eqTradeRetCost(trade, region, year)...")
@constraint(
    model,
    [(t1, r, y) in mTradeRetCost],
    vTradeRetCost[(t1, r, y)] == (
        if haskey(pTradeRetCost, (t1, r, y))
            pTradeRetCost[(t1, r, y)]
        else
            pTradeRetCostDef
        end
    ) * (
        (
            if (t1, y) in mvTradeRetiredStock
                vTradeRetiredStock[(t1, y)]
            else
                0
            end
        ) + sum(
            vTradeRetiredNewCap[(t1, yp, y)] for
            yp in year if (t1, yp, y) in mvTradeRetiredNewCap
        )
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
        ) for yp in get(mvTechRetiredNewCap_ix2_013, (t, r, y), ())
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
# vTechNewCap is an annual build rate; pPeriodLen[yp] converts it to the
# vintage's standing capacity (the same accumulation as eqTechCap).
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
            (
                if haskey(pPeriodLen, (yp))
                    pPeriodLen[(yp)]
                else
                    pPeriodLenDef
                end
            ) * vTechNewCap[(t, r, yp)] - sum(
                vTechRetiredNewCap[(t, r, yp, ye)] * (
                    if haskey(pPeriodLen, (ye))
                        pPeriodLen[(ye)]
                    else
                        pPeriodLenDef
                    end
                ) for ye in year if
                ((t, r, yp, ye) in mvTechRetiredNewCap && ordYear[(y)] >= ordYear[(ye)]);
                init = 0
            )
        ) for yp in year if (
            (t, r, yp) in mTechNew &&
            ordYear[(y)] >= ordYear[(yp)] &&
            # [payback] the annuity is charged over the COST-RECOVERY period:
            # pTechPayback where set (> 0), otherwise the operational life.
            # eqTechCap deliberately keeps pTechOlife -- the technical life still
            # governs when capacity OPERATES. Ported from GLPK 2026-08-19.
            (
                ((
                    if haskey(pTechPayback, (t, r, yp))
                        pTechPayback[(t, r, yp)]
                    else
                        pTechPaybackDef
                    end
                ) > 0 && ordYear[(y)] < (
                    if haskey(pTechPayback, (t, r, yp))
                        pTechPayback[(t, r, yp)]
                    else
                        pTechPaybackDef
                    end
                ) + ordYear[(yp)]) ||
                ((
                    if haskey(pTechPayback, (t, r, yp))
                        pTechPayback[(t, r, yp)]
                    else
                        pTechPaybackDef
                    end
                ) <= 0 && (
                    ordYear[(y)] < (
                        if haskey(pTechOlife, (t, r))
                            pTechOlife[(t, r)]
                        else
                            pTechOlifeDef
                        end
                    ) + ordYear[(yp)] || (t, r) in mTechOlifeInf
                ))
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
            if haskey(pTimesliceWeight, (y, s))
                pTimesliceWeight[(y, s)]
            else
                pTimesliceWeightDef
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
                if haskey(pTimesliceWeight, (y, s))
                    pTimesliceWeight[(y, s)]
                else
                    pTimesliceWeightDef
                end
            ) *
            vTechInp[(t, c, r, y, s)] for c in get(mTechInpComm_ix1_0, t, ())
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
                if haskey(pTimesliceWeight, (y, s))
                    pTimesliceWeight[(y, s)]
                else
                    pTimesliceWeightDef
                end
            ) *
            vTechOut[(t, c, r, y, s)] for c in get(mTechOutComm_ix, t, ())
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
                if haskey(pTimesliceWeight, (y, s))
                    pTimesliceWeight[(y, s)]
                else
                    pTimesliceWeightDef
                end
            ) *
            vTechAOut[(t, c, r, y, s)] for c in get(mvTechAOut_ix, (t, r, y, s), ())
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
                if haskey(pTimesliceWeight, (y, s))
                    pTimesliceWeight[(y, s)]
                else
                    pTimesliceWeightDef
                end
            ) *
            vTechAInp[(t, c, r, y, s)] for c in get(mvTechAInp_ix, (t, r, y, s), ())
        ) for s in get(mTechTimeslice_ix, t, ())
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqSupAvaUp(sup, comm, region, year, timeslice)$mSupAvaUp(sup, comm, region, year, timeslice)
print("eqSupAvaUp(sup, comm, region, year, timeslice)...")
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
            sum(
                if haskey(pWeather, (wth1, rw, y, s))
                    pWeather[(wth1, rw, y, s)]
                else
                    pWeatherDef
                end for rw in get(mWeatherRegionAt_ix, (wth1, r), ())
            ; init = 0)
        ) for wth1 in get(mSupWeatherUp_ix, s1, ())
    ; init = 1)
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqSupAvaLo(sup, comm, region, year, timeslice)$meqSupAvaLo(sup, comm, region, year, timeslice)
print("eqSupAvaLo(sup, comm, region, year, timeslice)...")
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
            sum(
                if haskey(pWeather, (wth1, rw, y, s))
                    pWeather[(wth1, rw, y, s)]
                else
                    pWeatherDef
                end for rw in get(mWeatherRegionAt_ix, (wth1, r), ())
            ; init = 0)
        ) for wth1 in get(mSupWeatherLo_ix, s1, ())
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
            if haskey(pTimesliceWeight, (y, s))
                pTimesliceWeight[(y, s)]
            else
                pTimesliceWeightDef
            end
        ) *
        vSupOut[(s1, c, r, y, s)] for y in year for
        s in timeslice if (s1, c, r, y, s) in mSupAva
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
            if haskey(pTimesliceWeight, (y, s))
                pTimesliceWeight[(y, s)]
            else
                pTimesliceWeightDef
            end
        ) *
        vSupOut[(s1, c, r, y, s)] for c in comm for
        s in timeslice if (s1, c, r, y, s) in mSupAva
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqDemInp(comm, region, year, timeslice)$mvDemInp(comm, region, year, timeslice)
print("eqDemInp(comm, region, year, timeslice)...")
@constraint(
    model,
    [(c, r, y, s) in mvDemInp],
    vDemInp[(c, r, y, s)] == sum((
        if haskey(pDemand, (d, c, r, y, s))
            pDemand[(d, c, r, y, s)]
        else
            pDemandDef
        end
    ) for d in get(mDemComm_ix, c, ()))
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqAggOutTot(comm, region, year, timeslice)$mAggOut(comm, region, year, timeslice)
print("eqAggOutTot(comm, region, year, timeslice)...")
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
                if haskey(pTimesliceAgg, (y, s, sp))
                    pTimesliceAgg[(y, s, sp)]
                else
                    pTimesliceAggDef
                end
            ) * (
                if (cp, r, y, sp) in mvOutTot
                    vOutTot[(cp, r, y, sp)]
                else
                    0
                end
            ) for sp in timeslice if (
                (c, r, y, sp) in mvOutTot &&
                (s, sp) in mTimesliceParentChildE &&
                (cp, sp) in mCommTimeslice
            )
        ) for cp in get(mAggregateFactor_ix, c, ())
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqEmsFuelTot(comm, region, year, timeslice)$mEmsFuelTot(comm, region, year, timeslice)
print("eqEmsFuelTot(comm, region, year, timeslice)...")
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
                    if haskey(pTimesliceAgg, (y, s, sp))
                        pTimesliceAgg[(y, s, sp)]
                    else
                        pTimesliceAggDef
                    end
                ) * (
                    if (t, c, cp, r, y, sp) in mTechEmsFuel
                        vTechInp[(t, cp, r, y, sp)]
                    else
                        0
                    end
                ) for sp in get(mCommTimesliceOrParent_ix, (c, s), ())
            ) for t in get(mTechInpComm_ix0_1, cp, ())
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
# eqStorageAInp(stg, comm, region, year, timeslice)$mvStorageAInp(stg, comm, region, year, timeslice)
print("eqStorageAInp(stg, comm, region, year, timeslice)...")
@constraint(
    model,
    [(st1, c, r, y, s) in mvStorageAInp],
    vStorageAInp[(st1, c, r, y, s)] ==
# [multi-commodity] each referenced flow follows its OWN role's commodity set, and
# the two capacity terms leave the commodity sum -- inside it they were multiplied
# by 1 for a single-commodity storage but would be counted once per commodity now
# that the roles can differ.
    sum(
        (
            if (st1, c, r, y, s) in mStorageStg2AInp
                (
                    (
                        if haskey(pStorageStg2AInp, (st1, c, r, y, s))
                            pStorageStg2AInp[(st1, c, r, y, s)]
                        else
                            pStorageStg2AInpDef
                        end
                    ) * vStorageLevel[(st1, cp, r, y, s)]
                )
            else
                0
            end
        ) for cp in get(mStorageStgComm_ix, st1, ());
        init = 0
    ) +
    sum(
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
        ) for cp in get(mStorageInpComm_ix, st1, ());
        init = 0
    ) +
    sum(
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
        ) for cp in get(mStorageOutComm_ix, st1, ());
        init = 0
    ) +
        (
            if (st1, c, r, y, s) in mStorageOutCap2AInp
                (
                    (
                        if haskey(pStorageOutCap2AInp, (st1, c, r, y, s))
                            pStorageOutCap2AInp[(st1, c, r, y, s)]
                        else
                            pStorageOutCap2AInpDef
                        end
                    ) * vStorageOutCap[(st1, r, y)]
                )
            else
                0
            end
        ) +
        (
            if (st1, c, r, y, s) in mStorageOutNCap2AInp
                (
                    (
                        if haskey(pStorageOutNCap2AInp, (st1, c, r, y, s))
                            pStorageOutNCap2AInp[(st1, c, r, y, s)]
                        else
                            pStorageOutNCap2AInpDef
                        end
                    ) * vStorageOutNewCap[(st1, r, y)]
                )
            else
                0
            end
        ) +
        (
            if (st1, c, r, y, s) in mStorageInpCap2AInp
                (
                    (
                        if haskey(pStorageInpCap2AInp, (st1, c, r, y, s))
                            pStorageInpCap2AInp[(st1, c, r, y, s)]
                        else
                            pStorageInpCap2AInpDef
                        end
                    ) * vStorageInpCap[(st1, r, y)]
                )
            else
                0
            end
        ) +
        (
            if (st1, c, r, y, s) in mStorageInpNCap2AInp
                (
                    (
                        if haskey(pStorageInpNCap2AInp, (st1, c, r, y, s))
                            pStorageInpNCap2AInp[(st1, c, r, y, s)]
                        else
                            pStorageInpNCap2AInpDef
                        end
                    ) * vStorageInpNewCap[(st1, r, y)]
                )
            else
                0
            end
        ) +
        (
            if (st1, c, r, y, s) in mStorageStgCap2AInp
                (
                    (
                        if haskey(pStorageStgCap2AInp, (st1, c, r, y, s))
                            pStorageStgCap2AInp[(st1, c, r, y, s)]
                        else
                            pStorageStgCap2AInpDef
                        end
                    ) * vStorageStgCap[(st1, r, y)]
                )
            else
                0
            end
        ) +
        (
            if (st1, c, r, y, s) in mStorageStgNCap2AInp
                (
                    (
                        if haskey(pStorageStgNCap2AInp, (st1, c, r, y, s))
                            pStorageStgNCap2AInp[(st1, c, r, y, s)]
                        else
                            pStorageStgNCap2AInpDef
                        end
                    ) * vStorageStgNewCap[(st1, r, y)]
                )
            else
                0
            end
        ) +
        (
            if (st1, c, r, y, s) in mStoragePho2AInp
                (
                    (
                        if haskey(pStoragePho2AInp, (st1, c, r, y, s))
                            pStoragePho2AInp[(st1, c, r, y, s)]
                        else
                            pStoragePho2AInpDef
                        end
                    ) * (if (st1, r, y) in mvStoragePhaseOut; vStoragePhaseOut[(st1, r, y)]; else; 0; end)
                )
            else
                0
            end
        ) +
        (
            if (st1, c, r, y, s) in mStorageRet2AInp
                (
                    (
                        if haskey(pStorageRet2AInp, (st1, c, r, y, s))
                            pStorageRet2AInp[(st1, c, r, y, s)]
                        else
                            pStorageRet2AInpDef
                        end
                    ) * ((if (st1, r, y) in mvStorageRetiredStock; vStorageOutRetiredStock[(st1, r, y)]; else; 0; end) + sum(vStorageOutRetiredNewCap[(st1, r, yp, y)] for yp in get(mvStorageRetiredNewCap_ix, (st1, r, y), ())))
                )
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
# eqStorageAOut(stg, comm, region, year, timeslice)$mvStorageAOut(stg, comm, region, year, timeslice)
print("eqStorageAOut(stg, comm, region, year, timeslice)...")
@constraint(
    model,
    [(st1, c, r, y, s) in mvStorageAOut],
    vStorageAOut[(st1, c, r, y, s)] ==
# [multi-commodity] each referenced flow follows its OWN role's commodity set, and
# the two capacity terms leave the commodity sum -- inside it they were multiplied
# by 1 for a single-commodity storage but would be counted once per commodity now
# that the roles can differ.
    sum(
        (
            if (st1, c, r, y, s) in mStorageStg2AOut
                (
                    (
                        if haskey(pStorageStg2AOut, (st1, c, r, y, s))
                            pStorageStg2AOut[(st1, c, r, y, s)]
                        else
                            pStorageStg2AOutDef
                        end
                    ) * vStorageLevel[(st1, cp, r, y, s)]
                )
            else
                0
            end
        ) for cp in get(mStorageStgComm_ix, st1, ());
        init = 0
    ) +
    sum(
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
        ) for cp in get(mStorageInpComm_ix, st1, ());
        init = 0
    ) +
    sum(
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
        ) for cp in get(mStorageOutComm_ix, st1, ());
        init = 0
    ) +
        (
            if (st1, c, r, y, s) in mStorageOutCap2AOut
                (
                    (
                        if haskey(pStorageOutCap2AOut, (st1, c, r, y, s))
                            pStorageOutCap2AOut[(st1, c, r, y, s)]
                        else
                            pStorageOutCap2AOutDef
                        end
                    ) * vStorageOutCap[(st1, r, y)]
                )
            else
                0
            end
        ) +
        (
            if (st1, c, r, y, s) in mStorageOutNCap2AOut
                (
                    (
                        if haskey(pStorageOutNCap2AOut, (st1, c, r, y, s))
                            pStorageOutNCap2AOut[(st1, c, r, y, s)]
                        else
                            pStorageOutNCap2AOutDef
                        end
                    ) * vStorageOutNewCap[(st1, r, y)]
                )
            else
                0
            end
        ) +
        (
            if (st1, c, r, y, s) in mStorageInpCap2AOut
                (
                    (
                        if haskey(pStorageInpCap2AOut, (st1, c, r, y, s))
                            pStorageInpCap2AOut[(st1, c, r, y, s)]
                        else
                            pStorageInpCap2AOutDef
                        end
                    ) * vStorageInpCap[(st1, r, y)]
                )
            else
                0
            end
        ) +
        (
            if (st1, c, r, y, s) in mStorageInpNCap2AOut
                (
                    (
                        if haskey(pStorageInpNCap2AOut, (st1, c, r, y, s))
                            pStorageInpNCap2AOut[(st1, c, r, y, s)]
                        else
                            pStorageInpNCap2AOutDef
                        end
                    ) * vStorageInpNewCap[(st1, r, y)]
                )
            else
                0
            end
        ) +
        (
            if (st1, c, r, y, s) in mStorageStgCap2AOut
                (
                    (
                        if haskey(pStorageStgCap2AOut, (st1, c, r, y, s))
                            pStorageStgCap2AOut[(st1, c, r, y, s)]
                        else
                            pStorageStgCap2AOutDef
                        end
                    ) * vStorageStgCap[(st1, r, y)]
                )
            else
                0
            end
        ) +
        (
            if (st1, c, r, y, s) in mStorageStgNCap2AOut
                (
                    (
                        if haskey(pStorageStgNCap2AOut, (st1, c, r, y, s))
                            pStorageStgNCap2AOut[(st1, c, r, y, s)]
                        else
                            pStorageStgNCap2AOutDef
                        end
                    ) * vStorageStgNewCap[(st1, r, y)]
                )
            else
                0
            end
        ) +
        (
            if (st1, c, r, y, s) in mStoragePho2AOut
                (
                    (
                        if haskey(pStoragePho2AOut, (st1, c, r, y, s))
                            pStoragePho2AOut[(st1, c, r, y, s)]
                        else
                            pStoragePho2AOutDef
                        end
                    ) * (if (st1, r, y) in mvStoragePhaseOut; vStoragePhaseOut[(st1, r, y)]; else; 0; end)
                )
            else
                0
            end
        ) +
        (
            if (st1, c, r, y, s) in mStorageRet2AOut
                (
                    (
                        if haskey(pStorageRet2AOut, (st1, c, r, y, s))
                            pStorageRet2AOut[(st1, c, r, y, s)]
                        else
                            pStorageRet2AOutDef
                        end
                    ) * ((if (st1, r, y) in mvStorageRetiredStock; vStorageOutRetiredStock[(st1, r, y)]; else; 0; end) + sum(vStorageOutRetiredNewCap[(st1, r, yp, y)] for yp in get(mvStorageRetiredNewCap_ix, (st1, r, y), ())))
                )
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
# eqStorageLevel(stg, comm, region, year, timeslicep, timeslice)$meqStorageLevel(stg, comm, region, year, timeslicep, timeslice)
print("eqStorageLevel(stg, comm, region, year, timeslicep, timeslice)...")
@constraint(
    model,
    [(st1, c, r, y, sp, s) in meqStorageLevel],
    vStorageLevel[(st1, c, r, y, s)] ==
    (
        if haskey(pStorageStartLevel, (st1, c, r, y, s))
            pStorageStartLevel[(st1, c, r, y, s)]
        else
            pStorageStartLevelDef
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
                ) * vStorageOutNewCap[(st1, r, y)]
            )
        else
            0
        end
    ) +
    sum(
        (
            if haskey(pStorageInpEff, (st1, ci, r, y, sp))
                pStorageInpEff[(st1, ci, r, y, sp)]
            else
                pStorageInpEffDef
            end
        ) * vStorageInp[(st1, ci, r, y, sp)] for
        ci in comm if (st1, ci, r, y, sp) in mvStorageInp;
        init = 0
    ) +
    (
        ((
            if haskey(pStorageStgEff, (st1, c, r, y, s))
                pStorageStgEff[(st1, c, r, y, s)]
            else
                pStorageStgEffDef
            end
        ))^((
            if haskey(pTimesliceShare, (s))
                pTimesliceShare[(s)]
            else
                pTimesliceShareDef
            end
        ))
    ) * vStorageLevel[(st1, c, r, y, sp)] -
    sum(
        (vStorageOut[(st1, co, r, y, sp)]) / ((
            if haskey(pStorageOutEff, (st1, co, r, y, sp))
                pStorageOutEff[(st1, co, r, y, sp)]
            else
                pStorageOutEffDef
            end
        )) for co in get(mvStorageOut_ix, (st1, r, y, sp), ());
        init = 0
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqStorageAfLo(stg, comm, region, year, timeslice)$meqStorageAfLo(stg, comm, region, year, timeslice)
print("eqStorageAfLo(stg, comm, region, year, timeslice)...")
@constraint(
    model,
    [(st1, c, r, y, s) in meqStorageAfLo],
    vStorageLevel[(st1, c, r, y, s)] >=
    (
        if haskey(pStorageAfLo, (st1, r, y, s))
            pStorageAfLo[(st1, r, y, s)]
        else
            pStorageAfLoDef
        end
    ) *
    (
        # [2c] the storing side is its own variable where it carries data;
        # elsewhere the duration ratio is inlined onto the output capacity.
        if (st1, r, y) in mStorageStgCap
            vStorageStgCap[(st1, r, y)]
        else
            (
                if haskey(pStorageDurationLo, (st1, r, y))
                    pStorageDurationLo[(st1, r, y)]
                else
                    pStorageDurationLoDef
                end
            ) * vStorageOutCap[(st1, r, y)]
        end
    ) *
    prod(
        (
            if haskey(pStorageWeatherAfLo, (wth1, st1))
                pStorageWeatherAfLo[(wth1, st1)]
            else
                pStorageWeatherAfLoDef
            end
        ) * (
            sum(
                if haskey(pWeather, (wth1, rw, y, s))
                    pWeather[(wth1, rw, y, s)]
                else
                    pWeatherDef
                end for rw in get(mWeatherRegionAt_ix, (wth1, r), ())
            ; init = 0)
        ) for wth1 in get(mStorageWeatherAfLo_ix, st1, ())
    ; init = 1)
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqStorageAfUp(stg, comm, region, year, timeslice)$meqStorageAfUp(stg, comm, region, year, timeslice)
print("eqStorageAfUp(stg, comm, region, year, timeslice)...")
@constraint(
    model,
    [(st1, c, r, y, s) in meqStorageAfUp],
    vStorageLevel[(st1, c, r, y, s)] <=
    (
        if haskey(pStorageAfUp, (st1, r, y, s))
            pStorageAfUp[(st1, r, y, s)]
        else
            pStorageAfUpDef
        end
    ) *
    (
        # [2c] the storing side is its own variable where it carries data;
        # elsewhere the duration ratio is inlined onto the output capacity.
        if (st1, r, y) in mStorageStgCap
            vStorageStgCap[(st1, r, y)]
        else
            (
                if haskey(pStorageDurationUp, (st1, r, y))
                    pStorageDurationUp[(st1, r, y)]
                else
                    pStorageDurationUpDef
                end
            ) * vStorageOutCap[(st1, r, y)]
        end
    ) *
    prod(
        (
            if haskey(pStorageWeatherAfUp, (wth1, st1))
                pStorageWeatherAfUp[(wth1, st1)]
            else
                pStorageWeatherAfUpDef
            end
        ) * (
            sum(
                if haskey(pWeather, (wth1, rw, y, s))
                    pWeather[(wth1, rw, y, s)]
                else
                    pWeatherDef
                end for rw in get(mWeatherRegionAt_ix, (wth1, r), ())
            ; init = 0)
        ) for wth1 in get(mStorageWeatherAfUp_ix, st1, ())
    ; init = 1)
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqStorageOutLevel(stg, comm, region, year, timeslice)$mvStorageLevel(stg, comm, region, year, timeslice)
print("eqStorageOutLevel(stg, comm, region, year, timeslice)...")
@constraint(
    model,
    [(st1, c, r, y, s) in mvStorageLevel],
    sum(
        (vStorageOut[(st1, co, r, y, s)]) / ((
            if haskey(pStorageOutEff, (st1, co, r, y, s))
                pStorageOutEff[(st1, co, r, y, s)]
            else
                pStorageOutEffDef
            end
        )) for co in get(mvStorageOut_ix, (st1, r, y, s), ());
        init = 0
    ) <= vStorageLevel[(st1, c, r, y, s)]
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqStorageInpUp(stg, comm, region, year, timeslice)$meqStorageInpUp(stg, comm, region, year, timeslice)
print("eqStorageInpUp(stg, comm, region, year, timeslice)...")
@constraint(
    model,
    [(st1, c, r, y, s) in meqStorageInpUp],
    vStorageInp[(st1, c, r, y, s)] <=
    (
        # [2c] the charging side is its own variable where it carries data;
        # elsewhere inp2out is inlined onto the output capacity.
        if (st1, r, y) in mStorageInpCap
            vStorageInpCap[(st1, r, y)]
        else
            (
        if haskey(pStorageInp2outUp, (st1, r, y))
            pStorageInp2outUp[(st1, r, y)]
        else
            pStorageInp2outUpDef
        end
    ) * vStorageOutCap[(st1, r, y)]
        end
    ) *
    # [rate] capacity is per HOUR, not per timeslice
    (
        if haskey(pStorageInpCap2act, (st1))
            pStorageInpCap2act[(st1)]
        else
            pStorageInpCap2actDef
        end
    ) *
    (
        if haskey(pTimesliceShare, (s))
            pTimesliceShare[(s)]
        else
            pTimesliceShareDef
        end
    ) *
    (
        if haskey(pStorageInpAfUp, (st1, c, r, y, s))
            pStorageInpAfUp[(st1, c, r, y, s)]
        else
            pStorageInpAfUpDef
        end
    ) *
    prod(
        (
            if haskey(pStorageWeatherInpAfUp, (wth1, st1))
                pStorageWeatherInpAfUp[(wth1, st1)]
            else
                pStorageWeatherInpAfUpDef
            end
        ) * (
            sum(
                if haskey(pWeather, (wth1, rw, y, s))
                    pWeather[(wth1, rw, y, s)]
                else
                    pWeatherDef
                end for rw in get(mWeatherRegionAt_ix, (wth1, r), ())
            ; init = 0)
        ) for wth1 in get(mStorageWeatherInpAfUp_ix, st1, ())
    ; init = 1)
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqStorageInpLo(stg, comm, region, year, timeslice)$meqStorageInpLo(stg, comm, region, year, timeslice)
print("eqStorageInpLo(stg, comm, region, year, timeslice)...")
@constraint(
    model,
    [(st1, c, r, y, s) in meqStorageInpLo],
    vStorageInp[(st1, c, r, y, s)] >=
    (
        # [2c] the charging side is its own variable where it carries data;
        # elsewhere inp2out is inlined onto the output capacity.
        if (st1, r, y) in mStorageInpCap
            vStorageInpCap[(st1, r, y)]
        else
            (
        if haskey(pStorageInp2outLo, (st1, r, y))
            pStorageInp2outLo[(st1, r, y)]
        else
            pStorageInp2outLoDef
        end
    ) * vStorageOutCap[(st1, r, y)]
        end
    ) *
    # [rate] capacity is per HOUR, not per timeslice
    (
        if haskey(pStorageInpCap2act, (st1))
            pStorageInpCap2act[(st1)]
        else
            pStorageInpCap2actDef
        end
    ) *
    (
        if haskey(pTimesliceShare, (s))
            pTimesliceShare[(s)]
        else
            pTimesliceShareDef
        end
    ) *
    (
        if haskey(pStorageInpAfLo, (st1, c, r, y, s))
            pStorageInpAfLo[(st1, c, r, y, s)]
        else
            pStorageInpAfLoDef
        end
    ) *
    prod(
        (
            if haskey(pStorageWeatherInpAfLo, (wth1, st1))
                pStorageWeatherInpAfLo[(wth1, st1)]
            else
                pStorageWeatherInpAfLoDef
            end
        ) * (
            sum(
                if haskey(pWeather, (wth1, rw, y, s))
                    pWeather[(wth1, rw, y, s)]
                else
                    pWeatherDef
                end for rw in get(mWeatherRegionAt_ix, (wth1, r), ())
            ; init = 0)
        ) for wth1 in get(mStorageWeatherInpAfLo_ix, st1, ())
    ; init = 1)
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqStorageOutUp(stg, comm, region, year, timeslice)$meqStorageOutUp(stg, comm, region, year, timeslice)
print("eqStorageOutUp(stg, comm, region, year, timeslice)...")
@constraint(
    model,
    [(st1, c, r, y, s) in meqStorageOutUp],
    vStorageOut[(st1, c, r, y, s)] <=
    vStorageOutCap[(st1, r, y)] *
    (
        if haskey(pStorageOutCap2act, (st1))
            pStorageOutCap2act[(st1)]
        else
            pStorageOutCap2actDef
        end
    ) *
    (
        if haskey(pTimesliceShare, (s))
            pTimesliceShare[(s)]
        else
            pTimesliceShareDef
        end
    ) *
    (
        if haskey(pStorageOutAfUp, (st1, c, r, y, s))
            pStorageOutAfUp[(st1, c, r, y, s)]
        else
            pStorageOutAfUpDef
        end
    ) *
    prod(
        (
            if haskey(pStorageWeatherOutAfUp, (wth1, st1))
                pStorageWeatherOutAfUp[(wth1, st1)]
            else
                pStorageWeatherOutAfUpDef
            end
        ) * (
            sum(
                if haskey(pWeather, (wth1, rw, y, s))
                    pWeather[(wth1, rw, y, s)]
                else
                    pWeatherDef
                end for rw in get(mWeatherRegionAt_ix, (wth1, r), ())
            ; init = 0)
        ) for wth1 in get(mStorageWeatherOutAfUp_ix, st1, ())
    ; init = 1)
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqStorageOutLo(stg, comm, region, year, timeslice)$meqStorageOutLo(stg, comm, region, year, timeslice)
print("eqStorageOutLo(stg, comm, region, year, timeslice)...")
@constraint(
    model,
    [(st1, c, r, y, s) in meqStorageOutLo],
    vStorageOut[(st1, c, r, y, s)] >=
    vStorageOutCap[(st1, r, y)] *
    (
        if haskey(pStorageOutCap2act, (st1))
            pStorageOutCap2act[(st1)]
        else
            pStorageOutCap2actDef
        end
    ) *
    (
        if haskey(pTimesliceShare, (s))
            pTimesliceShare[(s)]
        else
            pTimesliceShareDef
        end
    ) *
    (
        if haskey(pStorageOutAfLo, (st1, c, r, y, s))
            pStorageOutAfLo[(st1, c, r, y, s)]
        else
            pStorageOutAfLoDef
        end
    ) *
    prod(
        (
            if haskey(pStorageWeatherOutAfLo, (wth1, st1))
                pStorageWeatherOutAfLo[(wth1, st1)]
            else
                pStorageWeatherOutAfLoDef
            end
        ) * (
            sum(
                if haskey(pWeather, (wth1, rw, y, s))
                    pWeather[(wth1, rw, y, s)]
                else
                    pWeatherDef
                end for rw in get(mWeatherRegionAt_ix, (wth1, r), ())
            ; init = 0)
        ) for wth1 in get(mStorageWeatherOutAfLo_ix, st1, ())
    ; init = 1)
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqStorageOutCap(stg, region, year)$mStorageSpan(stg, region, year)
print("eqStorageOutCap(stg, region, year)...")
@constraint(
    model,
    [(st1, r, y) in mStorageSpan],
    vStorageOutCap[(st1, r, y)] ==
    vStorageOutStockCap[(st1, r, y)] +
    sum(
        (
            if haskey(pPeriodLen, (yp))
                pPeriodLen[(yp)]
            else
                pPeriodLenDef
            end
        ) * vStorageOutNewCap[(st1, r, yp)] - sum(
            vStorageOutRetiredNewCap[(st1, r, yp, ye)] * (
                if haskey(pPeriodLen, (ye))
                    pPeriodLen[(ye)]
                else
                    pPeriodLenDef
                end
            ) for ye in year if
            ((st1, r, yp, ye) in mvStorageRetiredNewCap && ordYear[(y)] >= ordYear[(ye)])
        ) for yp in year if (
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
# eqStorageInpCap(stg, region, year)$mStorageInpCap(stg, region, year)
# [2c] the CHARGING side, rated independently of the discharger.
print("eqStorageInpCap(stg, region, year)...")
@constraint(
    model,
    [(st1, r, y) in mStorageInpCap],
    vStorageInpCap[(st1, r, y)] ==
    vStorageInpStockCap[(st1, r, y)] +
    sum(
        (
        if haskey(pPeriodLen, (yp))
            pPeriodLen[(yp)]
        else
            pPeriodLenDef
        end
    ) * vStorageInpNewCap[(st1, r, yp)] - sum(
            vStorageInpRetiredNewCap[(st1, r, yp, ye)] * (
                if haskey(pPeriodLen, (ye))
                    pPeriodLen[(ye)]
                else
                    pPeriodLenDef
                end
            ) for ye in year if
            ((st1, r, yp, ye) in mvStorageRetiredNewCap && ordYear[(y)] >= ordYear[(ye)])
        ) for yp in year if
        (st1, r, yp) in mStorageInpNew && ordYear[(y)] >= ordYear[(yp)] && (
            (st1, r) in mStorageOlifeInf ||
            ordYear[(y)] < (
        if haskey(pStorageOlife, (st1, r))
            pStorageOlife[(st1, r)]
        else
            pStorageOlifeDef
        end
    ) + ordYear[(yp)]
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
print("eqStorageInpCapLo(stg, region, year)...")
@constraint(model, [(st1, r, y) in mStorageInpCapLo],
    vStorageInpCap[(st1, r, y)] >= (
        if haskey(pStorageInpCapLo, (st1, r, y))
            pStorageInpCapLo[(st1, r, y)]
        else
            pStorageInpCapLoDef
        end
    ));
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
print("eqStorageInpCapUp(stg, region, year)...")
@constraint(model, [(st1, r, y) in mStorageInpCapUp],
    vStorageInpCap[(st1, r, y)] <= (
        if haskey(pStorageInpCapUp, (st1, r, y))
            pStorageInpCapUp[(st1, r, y)]
        else
            pStorageInpCapUpDef
        end
    ));
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
print("eqStorageInpNewCapLo(stg, region, year)...")
@constraint(model, [(st1, r, y) in mStorageInpNewCapLo],
    vStorageInpNewCap[(st1, r, y)] >= (
        if haskey(pStorageInpNewCapLo, (st1, r, y))
            pStorageInpNewCapLo[(st1, r, y)]
        else
            pStorageInpNewCapLoDef
        end
    ) * (
        if haskey(pPeriodLen, (y))
            pPeriodLen[(y)]
        else
            pPeriodLenDef
        end
    ));
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
print("eqStorageInpNewCapUp(stg, region, year)...")
@constraint(model, [(st1, r, y) in mStorageInpNewCapUp],
    vStorageInpNewCap[(st1, r, y)] <= (
        if haskey(pStorageInpNewCapUp, (st1, r, y))
            pStorageInpNewCapUp[(st1, r, y)]
        else
            pStorageInpNewCapUpDef
        end
    ) * (
        if haskey(pPeriodLen, (y))
            pPeriodLen[(y)]
        else
            pPeriodLenDef
        end
    ));
# The inp2out LINK: charging capacity per unit of discharging capacity.
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
print("eqStorageInp2outLo(stg, region, year)...")
@constraint(model, [(st1, r, y) in mStorageInp2outLo],
    vStorageInpCap[(st1, r, y)] >= (
        if haskey(pStorageInp2outLo, (st1, r, y))
            pStorageInp2outLo[(st1, r, y)]
        else
            pStorageInp2outLoDef
        end
    ) * vStorageOutCap[(st1, r, y)]);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
print("eqStorageInp2outUp(stg, region, year)...")
@constraint(model, [(st1, r, y) in mStorageInp2outUp],
    vStorageInpCap[(st1, r, y)] <= (
        if haskey(pStorageInp2outUp, (st1, r, y))
            pStorageInp2outUp[(st1, r, y)]
        else
            pStorageInp2outUpDef
        end
    ) * vStorageOutCap[(st1, r, y)]);
# The inp2stg LINK (charging C-rate, 1/h): charging capacity per unit of
# storing (energy) capacity. No binding default -- the maps carry only rows
# with a declared finite bound where both capacity variables exist.
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
print("eqStorageInp2stgLo(stg, region, year)...")
@constraint(model, [(st1, r, y) in mStorageInp2stgLo],
    vStorageInpCap[(st1, r, y)] >= (
        if haskey(pStorageInp2stgLo, (st1, r, y))
            pStorageInp2stgLo[(st1, r, y)]
        else
            pStorageInp2stgLoDef
        end
    ) * vStorageStgCap[(st1, r, y)]);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
print("eqStorageInp2stgUp(stg, region, year)...")
@constraint(model, [(st1, r, y) in mStorageInp2stgUp],
    vStorageInpCap[(st1, r, y)] <= (
        if haskey(pStorageInp2stgUp, (st1, r, y))
            pStorageInp2stgUp[(st1, r, y)]
        else
            pStorageInp2stgUpDef
        end
    ) * vStorageStgCap[(st1, r, y)]);
# eqStorageStgCap(stg, region, year)$mStorageStgCap(stg, region, year)
# [2c] the STORING side's own capacity, in ENERGY. Exists only where the storing
# part carries data; elsewhere the af bounds inline duration * output capacity.
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
print("eqStorageStgCap(stg, region, year)...")
@constraint(
    model,
    [(st1, r, y) in mStorageStgCap],
    vStorageStgCap[(st1, r, y)] ==
    vStorageStgStockCap[(st1, r, y)] +
    sum(
        (
            if haskey(pPeriodLen, (yp))
                pPeriodLen[(yp)]
            else
                pPeriodLenDef
            end
        ) * vStorageStgNewCap[(st1, r, yp)] - sum(
            vStorageStgRetiredNewCap[(st1, r, yp, ye)] * (
                if haskey(pPeriodLen, (ye))
                    pPeriodLen[(ye)]
                else
                    pPeriodLenDef
                end
            ) for ye in year if
            ((st1, r, yp, ye) in mvStorageRetiredNewCap && ordYear[(y)] >= ordYear[(ye)])
        ) for yp in year if
        (st1, r, yp) in mStorageStgNew && ordYear[(y)] >= ordYear[(yp)] && (
            (st1, r) in mStorageOlifeInf ||
            ordYear[(y)] < (
                if haskey(pStorageOlife, (st1, r))
                    pStorageOlife[(st1, r)]
                else
                    pStorageOlifeDef
                end
            ) + ordYear[(yp)]
        );
        init = 0
    )
);
# eqStorageStgCapLo / Up
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
print("eqStorageStgCapLo(stg, region, year)...")
@constraint(model, [(st1, r, y) in mStorageStgCapLo],
    vStorageStgCap[(st1, r, y)] >= (
        if haskey(pStorageStgCapLo, (st1, r, y))
            pStorageStgCapLo[(st1, r, y)]
        else
            pStorageStgCapLoDef
        end
    ));
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
print("eqStorageStgCapUp(stg, region, year)...")
@constraint(model, [(st1, r, y) in mStorageStgCapUp],
    vStorageStgCap[(st1, r, y)] <= (
        if haskey(pStorageStgCapUp, (st1, r, y))
            pStorageStgCapUp[(st1, r, y)]
        else
            pStorageStgCapUpDef
        end
    ));
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
print("eqStorageStgNewCapLo(stg, region, year)...")
@constraint(model, [(st1, r, y) in mStorageStgNewCapLo],
    vStorageStgNewCap[(st1, r, y)] >= (
        if haskey(pStorageStgNewCapLo, (st1, r, y))
            pStorageStgNewCapLo[(st1, r, y)]
        else
            pStorageStgNewCapLoDef
        end
    ) * (
        if haskey(pPeriodLen, (y))
            pPeriodLen[(y)]
        else
            pPeriodLenDef
        end
    ));
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
print("eqStorageStgNewCapUp(stg, region, year)...")
@constraint(model, [(st1, r, y) in mStorageStgNewCapUp],
    vStorageStgNewCap[(st1, r, y)] <= (
        if haskey(pStorageStgNewCapUp, (st1, r, y))
            pStorageStgNewCapUp[(st1, r, y)]
        else
            pStorageStgNewCapUpDef
        end
    ) * (
        if haskey(pPeriodLen, (y))
            pPeriodLen[(y)]
        else
            pPeriodLenDef
        end
    ));
# The duration LINK, in hours. `.fx` collapses these two onto each other.
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
print("eqStorageDurationLo(stg, region, year)...")
@constraint(model, [(st1, r, y) in mStorageDurationLo],
    vStorageStgCap[(st1, r, y)] >= (
        if haskey(pStorageDurationLo, (st1, r, y))
            pStorageDurationLo[(st1, r, y)]
        else
            pStorageDurationLoDef
        end
    ) * vStorageOutCap[(st1, r, y)]);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
print("eqStorageDurationUp(stg, region, year)...")
@constraint(model, [(st1, r, y) in mStorageDurationUp],
    vStorageStgCap[(st1, r, y)] <= (
        if haskey(pStorageDurationUp, (st1, r, y))
            pStorageDurationUp[(st1, r, y)]
        else
            pStorageDurationUpDef
        end
    ) * vStorageOutCap[(st1, r, y)]);
# eqStorageOutCapLo(stg, region, year)$mStorageOutCapLo(stg, region, year)
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
print("eqStorageOutCapLo(stg, region, year)...")
@constraint(
    model,
    [(st1, r, y) in mStorageOutCapLo],
    vStorageOutCap[(st1, r, y)] >= (
        if haskey(pStorageOutCapLo, (st1, r, y))
            pStorageOutCapLo[(st1, r, y)]
        else
            pStorageOutCapLoDef
        end
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqStorageOutCapUp(stg, region, year)$mStorageOutCapUp(stg, region, year)
print("eqStorageOutCapUp(stg, region, year)...")
@constraint(
    model,
    [(st1, r, y) in mStorageOutCapUp],
    vStorageOutCap[(st1, r, y)] <= (
        if haskey(pStorageOutCapUp, (st1, r, y))
            pStorageOutCapUp[(st1, r, y)]
        else
            pStorageOutCapUpDef
        end
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqStorageOutNewCapLo(stg, region, year)$mStorageOutNewCapLo(stg, region, year)
print("eqStorageOutNewCapLo(stg, region, year)...")
@constraint(
    model,
    [(st1, r, y) in mStorageOutNewCapLo],
    vStorageOutNewCap[(st1, r, y)] >=
    (
        if haskey(pStorageOutNewCapLo, (st1, r, y))
            pStorageOutNewCapLo[(st1, r, y)]
        else
            pStorageOutNewCapLoDef
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
# eqStorageOutNewCapUp(stg, region, year)$mStorageOutNewCapUp(stg, region, year)
print("eqStorageOutNewCapUp(stg, region, year)...")
@constraint(
    model,
    [(st1, r, y) in mStorageOutNewCapUp],
    vStorageOutNewCap[(st1, r, y)] <=
    (
        if haskey(pStorageOutNewCapUp, (st1, r, y))
            pStorageOutNewCapUp[(st1, r, y)]
        else
            pStorageOutNewCapUpDef
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
        if haskey(pStorageOutInvcost, (st1, r, y))
            pStorageOutInvcost[(st1, r, y)]
        else
            pStorageOutInvcostDef
        end
    ) * vStorageOutNewCap[(st1, r, y)] +
    # [2c] the storing side's capital cost is per unit of ENERGY; absent data
    # the (st1, r, y) tuple is not in mStorageStgNew and the term is zero.
    (
        if (st1, r, y) in mStorageStgNew
            (
        if haskey(pStorageStgInvcost, (st1, r, y))
            pStorageStgInvcost[(st1, r, y)]
        else
            pStorageStgInvcostDef
        end
    ) * vStorageStgNewCap[(st1, r, y)]
        else
            0
        end
    ) +
    (
        if (st1, r, y) in mStorageInpNew
            (
        if haskey(pStorageInpInvcost, (st1, r, y))
            pStorageInpInvcost[(st1, r, y)]
        else
            pStorageInpInvcostDef
        end
    ) * vStorageInpNewCap[(st1, r, y)]
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
# eqStorageEac(stg, region, year)$mStorageEac(stg, region, year)
print("eqStorageEac(stg, region, year)...")
@constraint(
    model,
    [(st1, r, y) in mStorageEac],
    # [eac-fix] vintaged new-capacity form (pStorageOutEac applies to NEW capacity only)
    # New-cap variables are annual build rates; pPeriodLen[yp] converts to the
    # vintage's standing capacity (the same accumulation as the cap equations).
    vStorageEac[(st1, r, y)] == sum(
        (
            if haskey(pPeriodLen, (yp))
                pPeriodLen[(yp)]
            else
                pPeriodLenDef
            end
        ) * (
        (
            if haskey(pStorageOutEac, (st1, r, yp))
                pStorageOutEac[(st1, r, yp)]
            else
                pStorageOutEacDef
            end
        ) * vStorageOutNewCap[(st1, r, yp)] +
        # [2c] the storing side annuitises separately, on the same lifetime.
        (
            if (st1, r, yp) in mStorageStgNew
                (
                    if haskey(pStorageStgEac, (st1, r, yp))
                        pStorageStgEac[(st1, r, yp)]
                    else
                        pStorageStgEacDef
                    end
                ) * vStorageStgNewCap[(st1, r, yp)]
            else
                0
            end
        ) +
        (
            if (st1, r, yp) in mStorageInpNew
                (
                    if haskey(pStorageInpEac, (st1, r, yp))
                        pStorageInpEac[(st1, r, yp)]
                    else
                        pStorageInpEacDef
                    end
                ) * vStorageInpNewCap[(st1, r, yp)]
            else
                0
            end
        )) for yp in year if (
            (st1, r, yp) in mStorageNew &&
            ordYear[(y)] >= ordYear[(yp)] &&
            # [payback] see eqTechEac.
            (
                ((
                    if haskey(pStorageOutPayback, (st1, r, yp))
                        pStorageOutPayback[(st1, r, yp)]
                    else
                        pStorageOutPaybackDef
                    end
                ) > 0 && ordYear[(y)] < (
                    if haskey(pStorageOutPayback, (st1, r, yp))
                        pStorageOutPayback[(st1, r, yp)]
                    else
                        pStorageOutPaybackDef
                    end
                ) + ordYear[(yp)]) ||
                ((
                    if haskey(pStorageOutPayback, (st1, r, yp))
                        pStorageOutPayback[(st1, r, yp)]
                    else
                        pStorageOutPaybackDef
                    end
                ) <= 0 && (
                    (st1, r) in mStorageOlifeInf ||
                    ordYear[(y)] < (
                        if haskey(pStorageOlife, (st1, r))
                            pStorageOlife[(st1, r)]
                        else
                            pStorageOlifeDef
                        end
                    ) + ordYear[(yp)]
                ))
            )
            # [eac-fix] the `pStorageOutInvcost != 0` guard was REMOVED from the condition
            # below. It dropped the annuity entirely when a user supplied `@invcost$eac`
            # without `invcost` (pre-annuitised capex), so the storage
            # was built for FREE -- silently, with an OPTIMAL solve. eqTechEac / eqTradeEac
            # never carried it. pStorageOutEac defaults to 0, so a vintage with no capital cost
            # now contributes a zero-coefficient term instead of being dropped from the sum.
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
        if haskey(pStorageOutFixom, (st1, r, y))
            pStorageOutFixom[(st1, r, y)]
        else
            pStorageOutFixomDef
        end
    ) * vStorageOutCap[(st1, r, y)] +
    (
        if (st1, r, y) in mStorageStgFixom
            (
        if haskey(pStorageStgFixom, (st1, r, y))
            pStorageStgFixom[(st1, r, y)]
        else
            pStorageStgFixomDef
        end
    ) * vStorageStgCap[(st1, r, y)]
        else
            0
        end
    ) +
    (
        if (st1, r, y) in mStorageInpFixom
            (
        if haskey(pStorageInpFixom, (st1, r, y))
            pStorageInpFixom[(st1, r, y)]
        else
            pStorageInpFixomDef
        end
    ) * vStorageInpCap[(st1, r, y)]
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
# eqStorageVarom(stg, region, year)$mStorageVarom(stg, region, year)
print("eqStorageVarom(stg, region, year)...")
@constraint(
    model,
    [(st1, r, y) in mStorageVarom],
    vStorageVarom[(st1, r, y)] ==
# [multi-commodity] one sum per role: the charge cost runs over the input
# commodities, the discharge cost over the outputs, the holding cost over the
# stored one. A single-commodity storage has all three sets equal.
    sum(
        sum(
            (
                if haskey(pStorageCostInp, (st1, r, y, s))
                    pStorageCostInp[(st1, r, y, s)]
                else
                    pStorageCostInpDef
                end
            ) *
            (
                if haskey(pTimesliceWeight, (y, s))
                    pTimesliceWeight[(y, s)]
                else
                    pTimesliceWeightDef
                end
            ) *
            vStorageInp[(st1, c, r, y, s)] for s in get(mCommTimeslice_ix, c, ());
            init = 0
        ) for c in get(mStorageInpComm_ix, st1, ());
        init = 0
    ) +
    sum(
        sum(
            (
                if haskey(pStorageCostOut, (st1, r, y, s))
                    pStorageCostOut[(st1, r, y, s)]
                else
                    pStorageCostOutDef
                end
            ) *
            (
                if haskey(pTimesliceWeight, (y, s))
                    pTimesliceWeight[(y, s)]
                else
                    pTimesliceWeightDef
                end
            ) *
            vStorageOut[(st1, c, r, y, s)] for s in get(mCommTimeslice_ix, c, ());
            init = 0
        ) for c in get(mStorageOutComm_ix, st1, ());
        init = 0
    ) +
    sum(
        sum(
            (
                if haskey(pStorageCostStore, (st1, r, y, s))
                    pStorageCostStore[(st1, r, y, s)]
                else
                    pStorageCostStoreDef
                end
            ) *
            (
                if haskey(pTimesliceWeight, (y, s))
                    pTimesliceWeight[(y, s)]
                else
                    pTimesliceWeightDef
                end
            ) *
            vStorageLevel[(st1, c, r, y, s)] for s in get(mCommTimeslice_ix, c, ());
            init = 0
        ) for c in get(mStorageStgComm_ix, st1, ());
        init = 0
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# Route indexes keyed by endpoint. `mTradeRoutes` is sparse (one entry per
# existing corridor), so mapping an endpoint to its routes lets eqImportTot and
# eqExportTot below visit only routes that exist, instead of iterating
# `trade x region` and filtering.
_routesByDst = Dict{Any,Vector{Tuple{Any,Any}}}()
_routesBySrc = Dict{Any,Vector{Tuple{Any,Any}}}()
_routesByTrade = Dict{Any,Vector{Tuple{Any,Any}}}()
for (_t1, _s0, _d0) in mTradeRoutes
    push!(get!(_routesByDst, _d0, Tuple{Any,Any}[]), (_t1, _s0))
    push!(get!(_routesBySrc, _s0, Tuple{Any,Any}[]), (_t1, _d0))
    push!(get!(_routesByTrade, _t1, Tuple{Any,Any}[]), (_s0, _d0))
end
_noRoutes = Tuple{Any,Any}[]

# eqImportTot(comm, dst, year, timeslice)$mImport(comm, dst, year, timeslice)
print("eqImportTot(comm, dst, year, timeslice)...")
@constraint(
    model,
    [(c, dst, y, s) in mImport],
    vImportTot[(c, dst, y, s)] ==
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
            ) for (t1, src) in get(_routesByDst, dst, _noRoutes) if
            (t1, c) in mTradeComm;
        init = 0
    ) + sum((
        if (i, c, dst, y, s) in mImportRow
            vImportRow[(i, c, dst, y, s)]
        else
            0
        end
    ) for i in get(mImpComm_ix, c, ()))
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqExportTot(comm, src, year, timeslice)$mExport(comm, src, year, timeslice)
print("eqExportTot(comm, src, year, timeslice)...")
@constraint(
    model,
    [(c, src, y, s) in mExport],
    vExportTot[(c, src, y, s)] ==
    sum(
        (
                if (t1, c, src, dst, y, s) in mvTradeIr
                    vTradeIr[(t1, c, src, dst, y, s)]
                else
                    0
                end
            ) for (t1, dst) in get(_routesBySrc, src, _noRoutes) if
            (t1, c) in mTradeComm;
        init = 0
    ) + sum((
        if (e, c, src, y, s) in mExportRow
            vExportRow[(e, c, src, y, s)]
        else
            0
        end
    ) for e in get(mExpComm_ix, c, ()))
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqTradeFlowUp(trade, comm, src, dst, year, timeslice)$meqTradeFlowUp(trade, comm, src, dst, year, timeslice)
print("eqTradeFlowUp(trade, comm, src, dst, year, timeslice)...")
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
# eqTradeFlowLo(trade, comm, src, dst, year, timeslice)$meqTradeFlowLo(trade, comm, src, dst, year, timeslice)
print("eqTradeFlowLo(trade, comm, src, dst, year, timeslice)...")
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
# eqTradeIrAfUp(trade, comm, src, dst, year, timeslice)$meqTradeIrAfUp(trade, comm, src, dst, year, timeslice)
# Relative flow bound: a fraction of the object's own capacity rather than an
# absolute quantity, so one trade object carrying several routes can rate each
# of them. Gated, hence empty unless `af` is declared.
print("eqTradeIrAfUp(trade, comm, src, dst, year, timeslice)...")
@constraint(
    model,
    [(t1, c, src, dst, y, s) in meqTradeIrAfUp],
    vTradeIr[(t1, c, src, dst, y, s)] <= (
        if haskey(pTradeIrAfUp, (t1, src, dst, y, s))
            pTradeIrAfUp[(t1, src, dst, y, s)]
        else
            pTradeIrAfUpDef
        end
    ) *
    (
        if haskey(pTradeCap2Act, (t1))
            pTradeCap2Act[(t1)]
        else
            pTradeCap2ActDef
        end
    ) *
    vTradeCap[(t1, y)] *
    (
        if haskey(pTimesliceShare, (s))
            pTimesliceShare[(s)]
        else
            pTimesliceShareDef
        end
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqTradeIrAfLo(trade, comm, src, dst, year, timeslice)$meqTradeIrAfLo(trade, comm, src, dst, year, timeslice)
# Relative flow bound: a fraction of the object's own capacity rather than an
# absolute quantity, so one trade object carrying several routes can rate each
# of them. Gated, hence empty unless `af` is declared.
print("eqTradeIrAfLo(trade, comm, src, dst, year, timeslice)...")
@constraint(
    model,
    [(t1, c, src, dst, y, s) in meqTradeIrAfLo],
    vTradeIr[(t1, c, src, dst, y, s)] >= (
        if haskey(pTradeIrAfLo, (t1, src, dst, y, s))
            pTradeIrAfLo[(t1, src, dst, y, s)]
        else
            pTradeIrAfLoDef
        end
    ) *
    (
        if haskey(pTradeCap2Act, (t1))
            pTradeCap2Act[(t1)]
        else
            pTradeCap2ActDef
        end
    ) *
    vTradeCap[(t1, y)] *
    (
        if haskey(pTimesliceShare, (s))
            pTimesliceShare[(s)]
        else
            pTimesliceShareDef
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
                                    if haskey(pTradeIrMarkup, (t1, src, r, y, s))
                                        pTradeIrMarkup[(t1, src, r, y, s)]
                                    else
                                        pTradeIrMarkupDef
                                    end
                                )
                            ) *
                            vTradeIr[(t1, c, src, r, y, s)] *
                            (
                                if haskey(pTimesliceWeight, (y, s))
                                    pTimesliceWeight[(y, s)]
                                else
                                    pTimesliceWeightDef
                                end
                            )
                        )
                    else
                        0
                    end
                ) for s in get(mTradeTimeslice_ix, t1, ())
            ) for c in get(mTradeComm_ix, t1, ())
        ) for src in get(mTradeRoutes_ix1_02, (t1, r), ())
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
    sum(
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
                                ) - (
                                    if haskey(pTradeIrMarkup, (t1, r, dst, y, s))
                                        pTradeIrMarkup[(t1, r, dst, y, s)]
                                    else
                                        pTradeIrMarkupDef
                                    end
                                )
                            ) *
                            vTradeIr[(t1, c, r, dst, y, s)] *
                            (
                                if haskey(pTimesliceWeight, (y, s))
                                    pTimesliceWeight[(y, s)]
                                else
                                    pTimesliceWeightDef
                                end
                            )
                        )
                    else
                        0
                    end
                ) for s in get(mTradeTimeslice_ix, t1, ())
            ) for c in get(mTradeComm_ix, t1, ())
        ) for dst in get(mTradeRoutes_ix2_01, (t1, r), ())
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqExportRowUp(expp, comm, region, year, timeslice)$mExportRowUp(expp, comm, region, year, timeslice)
print("eqExportRowUp(expp, comm, region, year, timeslice)...")
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
# eqExportRowLo(expp, comm, region, year, timeslice)$meqExportRowLo(expp, comm, region, year, timeslice)
print("eqExportRowLo(expp, comm, region, year, timeslice)...")
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
            if haskey(pTimesliceWeight, (y, s))
                pTimesliceWeight[(y, s)]
            else
                pTimesliceWeightDef
            end
        ) *
        vExportRow[(e, c, r, y, s)] for r in region for y in year for
        s in timeslice if (e, c, r, y, s) in mExportRow
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
            if haskey(pTimesliceWeight, (y, s))
                pTimesliceWeight[(y, s)]
            else
                pTimesliceWeightDef
            end
        ) *
        vExportRow[(e, c, r, y, s)] for c in comm for
        s in timeslice if (e, c, r, y, s) in mExportRow
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqImportRowUp(imp, comm, region, year, timeslice)$mImportRowUp(imp, comm, region, year, timeslice)
print("eqImportRowUp(imp, comm, region, year, timeslice)...")
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
# eqImportRowLo(imp, comm, region, year, timeslice)$meqImportRowLo(imp, comm, region, year, timeslice)
print("eqImportRowLo(imp, comm, region, year, timeslice)...")
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
            if haskey(pTimesliceWeight, (y, s))
                pTimesliceWeight[(y, s)]
            else
                pTimesliceWeightDef
            end
        ) *
        vImportRow[(i, c, r, y, s)] for r in region for y in year for
        s in timeslice if (i, c, r, y, s) in mImportRow
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
            if haskey(pTimesliceWeight, (y, s))
                pTimesliceWeight[(y, s)]
            else
                pTimesliceWeightDef
            end
        ) *
        vImportRow[(i, c, r, y, s)] for c in comm for
        s in timeslice if (i, c, r, y, s) in mImportRow
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqTradeCapFlow(trade, comm, year, timeslice)$meqTradeCapFlow(trade, comm, year, timeslice)
print("eqTradeCapFlow(trade, comm, year, timeslice)...")
@constraint(
    model,
    [(t1, c, y, s) in meqTradeCapFlow],
    (
        if haskey(pTimesliceShare, (s))
            pTimesliceShare[(s)]
        else
            pTimesliceShareDef
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
        vTradeIr[(t1, c, src, dst, y, s)] for
        (src, dst) in get(_routesByTrade, t1, _noRoutes) if
        (t1, c, src, dst, y, s) in mvTradeIr;
        init = 0
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
    vTradeStockCap[(t1, y)] +
    sum(
        (
            if haskey(pPeriodLen, (yp))
                pPeriodLen[(yp)]
            else
                pPeriodLenDef
            end
        ) * vTradeNewCap[(t1, yp)] - sum(
            vTradeRetiredNewCap[(t1, yp, ye)] * (
                if haskey(pPeriodLen, (ye))
                    pPeriodLen[(ye)]
                else
                    pPeriodLenDef
                end
            ) for ye in year if
            ((t1, yp, ye) in mvTradeRetiredNewCap && ordYear[(y)] >= ordYear[(ye)])
        ) for yp in year if (
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
    # vTradeNewCap is an annual build rate; pPeriodLen[yp] converts it to the
    # vintage's standing capacity (the same accumulation as eqTradeCap).
    vTradeEac[(t1, r, y)] == sum(
        (
            if haskey(pTradeEac, (t1, r, yp))
                pTradeEac[(t1, r, yp)]
            else
                pTradeEacDef
            end
        ) * (
            if haskey(pPeriodLen, (yp))
                pPeriodLen[(yp)]
            else
                pPeriodLenDef
            end
        ) * vTradeNewCap[(t1, yp)] for yp in year if (
            (t1, yp) in mTradeNew &&
            ordYear[(y)] >= ordYear[(yp)] &&
            # [payback] see eqTechEac.
            (
                ((
                    if haskey(pTradePayback, (t1, r, yp))
                        pTradePayback[(t1, r, yp)]
                    else
                        pTradePaybackDef
                    end
                ) > 0 && ordYear[(y)] < (
                    if haskey(pTradePayback, (t1, r, yp))
                        pTradePayback[(t1, r, yp)]
                    else
                        pTradePaybackDef
                    end
                ) + ordYear[(yp)]) ||
                ((
                    if haskey(pTradePayback, (t1, r, yp))
                        pTradePayback[(t1, r, yp)]
                    else
                        pTradePaybackDef
                    end
                ) <= 0 && (
                    ordYear[(y)] < (
                        if haskey(pTradeOlife, (t1))
                            pTradeOlife[(t1)]
                        else
                            pTradeOlifeDef
                        end
                    ) + ordYear[(yp)] || t1 in mTradeOlifeInf
                ))
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
# eqTradeIrAInp(trade, comm, region, year, timeslice)$mvTradeIrAInp(trade, comm, region, year, timeslice)
print("eqTradeIrAInp(trade, comm, region, year, timeslice)...")
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
        ) * sum(vTradeIr[(t1, cp, r, dst, y, s)] for cp in comm
              if ((t1, cp) in mTradeComm && (t1, cp, r, dst, y, s) in mvTradeIr))
        for dst in get(mTradeIrCsrc2Ainp_ix, (t1, c, r, y, s), ())
    ) + sum(
        (
            if haskey(pTradeIrCdst2Ainp, (t1, c, src, r, y, s))
                pTradeIrCdst2Ainp[(t1, c, src, r, y, s)]
            else
                pTradeIrCdst2AinpDef
            end
        ) * sum(vTradeIr[(t1, cp, src, r, y, s)] for cp in comm
              if ((t1, cp) in mTradeComm && (t1, cp, src, r, y, s) in mvTradeIr))
        for src in get(mTradeIrCdst2Ainp_ix, (t1, c, r, y, s), ())
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqTradeIrAOut(trade, comm, region, year, timeslice)$mvTradeIrAOut(trade, comm, region, year, timeslice)
print("eqTradeIrAOut(trade, comm, region, year, timeslice)...")
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
        ) * sum(vTradeIr[(t1, cp, r, dst, y, s)] for cp in comm
              if ((t1, cp) in mTradeComm && (t1, cp, r, dst, y, s) in mvTradeIr))
        for dst in get(mTradeIrCsrc2Aout_ix, (t1, c, r, y, s), ())
    ) + sum(
        (
            if haskey(pTradeIrCdst2Aout, (t1, c, src, r, y, s))
                pTradeIrCdst2Aout[(t1, c, src, r, y, s)]
            else
                pTradeIrCdst2AoutDef
            end
        ) * sum(vTradeIr[(t1, cp, src, r, y, s)] for cp in comm
              if ((t1, cp) in mTradeComm && (t1, cp, src, r, y, s) in mvTradeIr))
        for src in get(mTradeIrCdst2Aout_ix, (t1, c, r, y, s), ())
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqTradeIrAInpTot(comm, region, year, timeslice)$mvTradeIrAInpTot(comm, region, year, timeslice)
print("eqTradeIrAInpTot(comm, region, year, timeslice)...")
@constraint(
    model,
    [(c, r, y, s) in mvTradeIrAInpTot],
    vTradeIrAInpTot[(c, r, y, s)] == sum(
        (
            if haskey(pTimesliceAgg, (y, s, sp))
                pTimesliceAgg[(y, s, sp)]
            else
                pTimesliceAggDef
            end
        ) * vTradeIrAInp[(t1, c, r, y, sp)] for t1 in trade for sp in timeslice if
        ((c, s, sp) in mCommTimesliceOrParent && (t1, c, r, y, sp) in mvTradeIrAInp)
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqTradeIrAOutTot(comm, region, year, timeslice)$mvTradeIrAOutTot(comm, region, year, timeslice)
print("eqTradeIrAOutTot(comm, region, year, timeslice)...")
@constraint(
    model,
    [(c, r, y, s) in mvTradeIrAOutTot],
    vTradeIrAOutTot[(c, r, y, s)] == sum(
        (
            if haskey(pTimesliceAgg, (y, s, sp))
                pTimesliceAgg[(y, s, sp)]
            else
                pTimesliceAggDef
            end
        ) * vTradeIrAOut[(t1, c, r, y, sp)] for t1 in trade for sp in timeslice if
        ((c, s, sp) in mCommTimesliceOrParent && (t1, c, r, y, sp) in mvTradeIrAOut)
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqBalLo(comm, region, year, timeslice)$meqBalLo(comm, region, year, timeslice)
print("eqBalLo(comm, region, year, timeslice)...")
@constraint(model, [(c, r, y, s) in meqBalLo], vBalance[(c, r, y, s)] >= 0);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqBalUp(comm, region, year, timeslice)$meqBalUp(comm, region, year, timeslice)
print("eqBalUp(comm, region, year, timeslice)...")
@constraint(model, [(c, r, y, s) in meqBalUp], vBalance[(c, r, y, s)] <= 0);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqBalFx(comm, region, year, timeslice)$meqBalFx(comm, region, year, timeslice)
print("eqBalFx(comm, region, year, timeslice)...")
@constraint(model, [(c, r, y, s) in meqBalFx], vBalance[(c, r, y, s)] == 0);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqBal(comm, region, year, timeslice)$mvBalance(comm, region, year, timeslice)
print("eqBal(comm, region, year, timeslice)...")
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
# [agg-rewrite] eqBalanceRY/vBalanceRY retired (dead reporting: weighted timeslice-sum, unused)
# eqOutTot(comm, region, year, timeslice)$mvOutTot(comm, region, year, timeslice)
print("eqOutTot(comm, region, year, timeslice)...")
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
    # (pTimesliceAgg renormalizes intensive values); replaces old mOutSub/vOut2Lo.
    sum(
        (
            if haskey(pTimesliceAgg, (y, s, sp))
                pTimesliceAgg[(y, s, sp)]
            else
                pTimesliceAggDef
            end
        ) * vOutTot[(c, r, y, sp)]
        for sp in get(mTimesliceFamily_ix, s, ()) if (c, r, y, sp) in mvOutTot;
        init = 0
    ) +
    # [nested-regions] up-aggregation of the immediately-finer region level.
    # Plain sum: regional quantities are extensive, unlike the intensive
    # timeslice values above.
    sum(
        vOutTot[(c, rp, y, s)]
        for rp in get(mRegionFamily_ix, r, ()) if
        ((c, rp, y, s) in mvOutTot && (c, s) in mCommTimeslice);
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
# eqInpTot(comm, region, year, timeslice)$mvInpTot(comm, region, year, timeslice)
print("eqInpTot(comm, region, year, timeslice)...")
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
            if haskey(pTimesliceAgg, (y, s, sp))
                pTimesliceAgg[(y, s, sp)]
            else
                pTimesliceAggDef
            end
        ) * vInpTot[(c, r, y, sp)]
        for sp in get(mTimesliceFamily_ix, s, ()) if (c, r, y, sp) in mvInpTot;
        init = 0
    ) +
    # [nested-regions] up-aggregation of the immediately-finer region level.
    # Plain sum: regional quantities are extensive, unlike the intensive
    # timeslice values above.
    sum(
        vInpTot[(c, rp, y, s)]
        for rp in get(mRegionFamily_ix, r, ()) if
        ((c, rp, y, s) in mvInpTot && (c, s) in mCommTimeslice);
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
# `vSupOut` is declared over `mSupAva`, which carries the REGION. Gating this
# sum on `mSupComm` alone indexes a region-restricted supply outside its own
# domain: harmless in GLPK/GAMS (full-cube declarations, presolved away) and a
# hard KeyError in Julia/Pyomo, which declare variables over their maps.
# eqSupOutTot(comm, region, year, timeslice)$mSupOutTot(comm, region, year, timeslice)
print("eqSupOutTot(comm, region, year, timeslice)...")
@constraint(
    model,
    [(c, r, y, s) in mSupOutTot],
    vSupOutTot[(c, r, y, s)] ==
    sum(vSupOut[(s1, c, r, y, s)] for s1 in sup
        if ((s1, c) in mSupComm && (s1, c, r, y, s) in mSupAva))
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqTechInpTot(comm, region, year, timeslice)$mTechInpTot(comm, region, year, timeslice)
print("eqTechInpTot(comm, region, year, timeslice)...")
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
        ) for t in get(mTechInpCommSameTimeslice_ix, c, ())
    ) +
    sum(
        sum(
            (
                if haskey(pTimesliceAgg, (y, s, sp))
                    pTimesliceAgg[(y, s, sp)]
                else
                    pTimesliceAggDef
                end
            ) * (
                if (t, c, r, y, sp) in mvTechInp
                    vTechInp[(t, c, r, y, sp)]
                else
                    0
                end
            ) for sp in get(mTechInpCommAggTimeslice_ix, (t, c, s), ())
        ) for t in get(mTechInpCommAgg_ix, c, ())
    ) +
    sum(
        (
            if (t, c, r, y, s) in mvTechAInp
                vTechAInp[(t, c, r, y, s)]
            else
                0
            end
        ) for t in get(mTechAInpCommSameTimeslice_ix, c, ())
    ) +
    sum(
        sum(
            (
                if haskey(pTimesliceAgg, (y, s, sp))
                    pTimesliceAgg[(y, s, sp)]
                else
                    pTimesliceAggDef
                end
            ) * (
                if (t, c, r, y, sp) in mvTechAInp
                    vTechAInp[(t, c, r, y, sp)]
                else
                    0
                end
            ) for sp in get(mTechAInpCommAggTimeslice_ix, (t, c, s), ())
        ) for t in get(mTechAInpCommAgg_ix, c, ())
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqTechOutTot(comm, region, year, timeslice)$mTechOutTot(comm, region, year, timeslice)
print("eqTechOutTot(comm, region, year, timeslice)...")
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
        ) for t in get(mTechOutCommSameTimeslice_ix, c, ())
    ) +
    sum(
        sum(
            (
                if haskey(pTimesliceAgg, (y, s, sp))
                    pTimesliceAgg[(y, s, sp)]
                else
                    pTimesliceAggDef
                end
            ) * (
                if (t, c, r, y, sp) in mvTechOut
                    vTechOut[(t, c, r, y, sp)]
                else
                    0
                end
            ) for sp in get(mTechOutCommAggTimeslice_ix, (t, c, s), ())
        ) for t in get(mTechOutCommAgg_ix, c, ())
    ) +
    sum(
        (
            if (t, c, r, y, s) in mvTechAOut
                vTechAOut[(t, c, r, y, s)]
            else
                0
            end
        ) for t in get(mTechAOutCommSameTimeslice_ix, c, ())
    ) +
    sum(
        sum(
            (
                if haskey(pTimesliceAgg, (y, s, sp))
                    pTimesliceAgg[(y, s, sp)]
                else
                    pTimesliceAggDef
                end
            ) * (
                if (t, c, r, y, sp) in mvTechAOut
                    vTechAOut[(t, c, r, y, sp)]
                else
                    0
                end
            ) for sp in get(mTechAOutCommAggTimeslice_ix, (t, c, s), ())
        ) for t in get(mTechAOutCommAgg_ix, c, ())
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# [agg-rewrite] eqTechOutRY/vTechOutRY retired (dead reporting)
# eqStorageInpTot(comm, region, year, timeslice)$mStorageInpTot(comm, region, year, timeslice)
print("eqStorageInpTot(comm, region, year, timeslice)...")
@constraint(
    model,
    [(c, r, y, s) in mStorageInpTot],
    vStorageInpTot[(c, r, y, s)] ==
    sum(
        (
            if (st1, c, r, y, s) in mvStorageInp
                vStorageInp[(st1, c, r, y, s)]
            else
                0
            end
        ) for st1 in get(mStorageInpCommSameTimeslice_ix, c, ())
    ) +
    sum(
        sum(
            (
                if haskey(pTimesliceAgg, (y, s, sp))
                    pTimesliceAgg[(y, s, sp)]
                else
                    pTimesliceAggDef
                end
            ) * (
                if (st1, c, r, y, sp) in mvStorageInp
                    vStorageInp[(st1, c, r, y, sp)]
                else
                    0
                end
            ) for sp in get(mStorageInpCommAggTimeslice_ix, (st1, c, s), ())
        ) for st1 in get(mStorageInpCommAgg_ix, c, ())
    ) +
    sum(
        (
            if (st1, c, r, y, s) in mvStorageAInp
                vStorageAInp[(st1, c, r, y, s)]
            else
                0
            end
        ) for st1 in get(mStorageAInpCommSameTimeslice_ix, c, ())
    ) +
    sum(
        sum(
            (
                if haskey(pTimesliceAgg, (y, s, sp))
                    pTimesliceAgg[(y, s, sp)]
                else
                    pTimesliceAggDef
                end
            ) * (
                if (st1, c, r, y, sp) in mvStorageAInp
                    vStorageAInp[(st1, c, r, y, sp)]
                else
                    0
                end
            ) for sp in get(mStorageAInpCommAggTimeslice_ix, (st1, c, s), ())
        ) for st1 in get(mStorageAInpCommAgg_ix, c, ())
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqStorageOutTot(comm, region, year, timeslice)$mStorageOutTot(comm, region, year, timeslice)
print("eqStorageOutTot(comm, region, year, timeslice)...")
@constraint(
    model,
    [(c, r, y, s) in mStorageOutTot],
    vStorageOutTot[(c, r, y, s)] ==
    sum(
        (
            if (st1, c, r, y, s) in mvStorageOut
                vStorageOut[(st1, c, r, y, s)]
            else
                0
            end
        ) for st1 in get(mStorageOutCommSameTimeslice_ix, c, ())
    ) +
    sum(
        sum(
            (
                if haskey(pTimesliceAgg, (y, s, sp))
                    pTimesliceAgg[(y, s, sp)]
                else
                    pTimesliceAggDef
                end
            ) * (
                if (st1, c, r, y, sp) in mvStorageOut
                    vStorageOut[(st1, c, r, y, sp)]
                else
                    0
                end
            ) for sp in get(mStorageOutCommAggTimeslice_ix, (st1, c, s), ())
        ) for st1 in get(mStorageOutCommAgg_ix, c, ())
    ) +
    sum(
        (
            if (st1, c, r, y, s) in mvStorageAOut
                vStorageAOut[(st1, c, r, y, s)]
            else
                0
            end
        ) for st1 in get(mStorageAOutCommSameTimeslice_ix, c, ())
    ) +
    sum(
        sum(
            (
                if haskey(pTimesliceAgg, (y, s, sp))
                    pTimesliceAgg[(y, s, sp)]
                else
                    pTimesliceAggDef
                end
            ) * (
                if (st1, c, r, y, sp) in mvStorageAOut
                    vStorageAOut[(st1, c, r, y, sp)]
                else
                    0
                end
            ) for sp in get(mStorageAOutCommAggTimeslice_ix, (st1, c, s), ())
        ) for st1 in get(mStorageAOutCommAgg_ix, c, ())
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
            if haskey(pTimesliceWeight, (y, s))
                pTimesliceWeight[(y, s)]
            else
                pTimesliceWeightDef
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
        ) for s in get(mDummyImport_ix, (c, r, y), ())
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
            if haskey(pTimesliceWeight, (y, s))
                pTimesliceWeight[(y, s)]
            else
                pTimesliceWeightDef
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
        ) for s in get(mDummyExport_ix, (c, r, y), ())
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
            if haskey(pTimesliceWeight, (y, s))
                pTimesliceWeight[(y, s)]
            else
                pTimesliceWeightDef
            end
        ) *
        vOutTot[(c, r, y, s)] for
        s in timeslice if ((c, r, y, s) in mvOutTot && (c, s) in mCommTimeslice)
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
            if haskey(pTimesliceWeight, (y, s))
                pTimesliceWeight[(y, s)]
            else
                pTimesliceWeightDef
            end
        ) *
        vInpTot[(c, r, y, s)] for
        s in timeslice if ((c, r, y, s) in mvInpTot && (c, s) in mCommTimeslice)
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
            if haskey(pTimesliceWeight, (y, s))
                pTimesliceWeight[(y, s)]
            else
                pTimesliceWeightDef
            end
        ) *
        vBalance[(c, r, y, s)] for
        s in timeslice if ((c, r, y, s) in mvBalance && (c, s) in mCommTimeslice)
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
            if haskey(pTimesliceWeight, (y, s))
                pTimesliceWeight[(y, s)]
            else
                pTimesliceWeightDef
            end
        ) *
        vOutTot[(c, r, y, s)] for
        s in timeslice if ((c, r, y, s) in mvOutTot && (c, s) in mCommTimeslice)
    ) - sum(
        (
            if haskey(pSubCostInp, (c, r, y, s))
                pSubCostInp[(c, r, y, s)]
            else
                pSubCostInpDef
            end
        ) *
        (
            if haskey(pTimesliceWeight, (y, s))
                pTimesliceWeight[(y, s)]
            else
                pTimesliceWeightDef
            end
        ) *
        vInpTot[(c, r, y, s)] for
        s in timeslice if ((c, r, y, s) in mvInpTot && (c, s) in mCommTimeslice)
    ) - sum(
        (
            if haskey(pSubCostBal, (c, r, y, s))
                pSubCostBal[(c, r, y, s)]
            else
                pSubCostBalDef
            end
        ) *
        (
            if haskey(pTimesliceWeight, (y, s))
                pTimesliceWeight[(y, s)]
            else
                pTimesliceWeightDef
            end
        ) *
        vBalance[(c, r, y, s)] for
        s in timeslice if ((c, r, y, s) in mvBalance && (c, s) in mCommTimeslice)
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
    end) for s1 in get(mvSupCost_ix, (r, y), ()))
    +sum((if (t, r, y) in mTechEac
        vTechEac[(t, r, y)]
    else
        0
    end) for t in get(mTechEac_ix, (r, y), ()))
    +sum((if (t, r, y) in mTechRetCost
        vTechRetCost[(t, r, y)]
    else
        0
    end) for t in get(mTechRetCost_ix, (r, y), ()))
    +sum((if (st1, r, y) in mStorageRetCost
        vStorageRetCost[(st1, r, y)]
    else
        0
    end) for st1 in get(mStorageRetCost_ix, (r, y), ()))
    +sum((if (t1, r, y) in mTradeRetCost
        vTradeRetCost[(t1, r, y)]
    else
        0
    end) for t1 in get(mTradeRetCost_ix, (r, y), ()))
    +sum((if (t, r, y) in mTechFixom
        vTechFixom[(t, r, y)]
    else
        0
    end) for t in get(mTechFixom_ix, (r, y), ()))
    +sum((if (t, r, y) in mTechVarom
        vTechVarom[(t, r, y)]
    else
        0
    end) for t in get(mTechVarom_ix, (r, y), ()))
    +sum((if (st1, r, y) in mStorageEac
        vStorageEac[(st1, r, y)]
    else
        0
    end) for st1 in get(mStorageEac_ix, (r, y), ()))
    +sum(
        (if (st1, r, y) in mStorageFixom
            vStorageFixom[(st1, r, y)]
        else
            0
        end) for st1 in get(mStorageFixom_ix, (r, y), ())
    )
    + sum((if (st1,r,y) in mStorageVarom; vStorageVarom[(st1,r,y)]; else 0; end;) for st1 in get(mStorageVarom_ix, (r, y), ()))
    + sum((if (i,r,y) in mImportRowCost; vImportRowCost[(i,r,y)]; else 0; end;) for i in get(mImportRowCost_ix, (r, y), ()))
    + sum((if (e,r,y) in mExportRowCost; vExportRowCost[(e,r,y)]; else 0; end;) for e in get(mExportRowCost_ix, (r, y), ()))
    + sum((if (t1,r,y) in mTradeEac; vTradeEac[(t1,r,y)]; else 0; end;) for t1 in get(mTradeEac_ix, (r, y), ()))
    + sum((if (t1,r,y) in mTradeFixom; vTradeFixom[(t1,r,y)]; else 0; end;) for t1 in get(mTradeFixom_ix, (r, y), ()))
    + sum((if (t1,r,y) in mImportIrCost; vImportIrCost[(t1,r,y)]; else 0; end;) for t1 in get(mImportIrCost_ix, (r, y), ()))
    + sum((if (t1,r,y) in mExportIrCost; vExportIrCost[(t1,r,y)]; else 0; end;) for t1 in get(mExportIrCost_ix, (r, y), ()))
    + sum((if (c,r,y) in mTaxCost; vTaxCost[(c,r,y)]; else 0; end;) for c in get(mTaxCost_ix, (r, y), ()))
    + sum((if (c,r,y) in mSubCost; vSubsCost[(c,r,y)]; else 0; end;) for c in get(mSubCost_ix, (r, y), ()))
    + (if (r,y) in mvTotalUserCosts; vTotalUserCosts[(r,y)]; else 0; end;)
    + sum((if (c,r,y) in mDummyImportCost; vDummyImportCost[(c,r,y)]; else 0; end;) for c in get(mDummyImportCost_ix, (r, y), ()))
    + sum((if (c,r,y) in mDummyExportCost; vDummyExportCost[(c,r,y)]; else 0; end;) for c in get(mDummyExportCost_ix, (r, y), ()))
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
        ) for r in region for y in get(mvTotalCost_ix, r, ())
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
# The solver's own termination status. A run can stop non-OPTIMAL yet still
# carry a usable solution (time limit, tolerance violation), which "-100" alone
# cannot distinguish from no solution at all.
println(
    flog,
    "\"termination status\",\"",
    termination_status(model),
    "\",\"",
    Dates.format(now(), "yyyy-mm-dd HH:MM:SS"),
    "\"",
)
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

# Print solution. output.jl calls JuMP.value() unguarded; first-order methods
# (pdlp/hipdlp) and time-limited runs can terminate with result_count == 0,
# where value() raises instead of returning a status.
if has_values(model)
    include("output.jl")
else
    println(
        flog,
        "\"no primal solution\",\"",
        termination_status(model),
        "\",\"",
        Dates.format(now(), "yyyy-mm-dd HH:MM:SS"),
        "\"",
    )
end
include("inc5.jl")
println(flog, "\"done\",,\"", Dates.format(now(), "yyyy-mm-dd HH:MM:SS"), "\"")
