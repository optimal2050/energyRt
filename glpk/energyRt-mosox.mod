# energyRt 0.71
# Energy Systems Modeling Toolbox for R
# https://energyRt.org

printf "parameter,value,time\n" > "output/log.csv";
printf  '"model language",glpk,"%s"\n', time2str(gmtime(), "%Y-%m-%d %M:%H:S %TZ") >> "output/log.csv";
printf  '"model definition",,"%s"\n', time2str(gmtime(), "%Y-%m-%d %M:%H:S %TZ") >> "output/log.csv";
set comm;
set region;
set year;
set timeslice;
set sup;
set dem;
set tech;
set stg;
set trade;
set expp;
set imp;
set group;
set weather;
set FORIF;



set mCommReg dimen 2;
set mSameRegion dimen 2;
set mSameTimeslice dimen 2;
set mMilestoneFirst dimen 1;
set mMilestoneLast dimen 1;
set mMilestoneNext dimen 2;
set mMilestoneHasNext dimen 1;
set mStartMilestone dimen 2;
set mEndMilestone dimen 2;
set mMidMilestone dimen 1;
set mCommTimeslice dimen 2;
set mCommTimesliceOrParent dimen 3;
set mTechRetirement dimen 1;
set mTechRetCost dimen 3;
set mTechUpgrade dimen 2;
set mTechInpComm dimen 2;
set mTechOutComm dimen 2;
set mTechInpGroup dimen 2;
set mTechOutGroup dimen 2;
set mTechOneComm dimen 2;
set mTechGroupComm dimen 3;
set mTechAInp dimen 2;
set mTechAOut dimen 2;
set mTechNew dimen 3;
set mTechSpan dimen 3;
set mTechTimeslice dimen 2;
set mSupTimeslice dimen 2;
set mSupComm dimen 2;
set mSupSpan dimen 2;
set mDemComm dimen 2;
set mUpComm dimen 1;
set mLoComm dimen 1;
set mFxComm dimen 1;
set mStorageFullYear dimen 1;
set mStorageComm dimen 2;
# The three storage commodity ROLES: what fills the store, what it releases,
# and what it HOLDS. All default from @commodity, so for a storage written the
# old way the three are identical to mStorageComm above.
set mStorageInpComm dimen 2;
set mStorageOutComm dimen 2;
set mStorageStgComm dimen 2;
set mStorageAInp dimen 2;
set mStorageAOut dimen 2;
set mStorageNew dimen 3;
set mStorageSpan dimen 3;
set mStorageFixom dimen 3;
set mStorageVarom dimen 3;
set mStorageEac dimen 3;
set mTimesliceNext dimen 2;
set mTimesliceFYearNext dimen 2;
set mTradeTimeslice dimen 2;
set mTradeComm dimen 2;
set mTradeRoutes dimen 3;
set mTradeIrAInp dimen 2;
set mTradeIrAOut dimen 2;
set mExpComm dimen 2;
set mImpComm dimen 2;
set mExpTimeslice dimen 2;
set mImpTimeslice dimen 2;
set mDiscountZero dimen 1;
set mTimesliceParentChildE dimen 2;
set mTimesliceParentChild dimen 2;
# [agg-rewrite] immediate parent->child (parent, child); drives the UP-aggregation
# of commodity totals between adjacent timeslice levels (replaces *2Lo disaggregation).
set mTimesliceFamily dimen 2;
# [nested-regions] spatial twin of mTimesliceFamily: immediate parent->child region
# pair (parent, child), one level only. Drives the UP-aggregation of commodity
# totals between adjacent geo-levels. Empty unless a geoscale is attached.
# No pRegionAgg counterpart: regional quantities are extensive, so this is a
# plain sum, whereas timeslice values are intensive rates and need pTimesliceAgg.
set mRegionFamily dimen 2;
# [nested-regions] the region level a commodity is BALANCED at (its @geoframe).
# Restricts mvBalance only -- totals stay available at finer levels so the
# family term above can sum them. Also guards region sums that would otherwise
# double-count a coarse commodity (see eqTaxCost/eqSubsCost).
set mCommRegion dimen 2;
set mTradeSpan dimen 2;
set mTradeNew dimen 2;
set mTradeOlifeInf dimen 1;
set mTradeEac dimen 3;
set mTradeCapacityVariable dimen 1;
set mTradeInv dimen 3;
set mTradeFixom dimen 3;
set mAggregateFactor dimen 2;
set mWeatherTimeslice dimen 2;
set mWeatherRegion dimen 2;
set mSupWeatherLo dimen 2;
set mSupWeatherUp dimen 2;
set mTechWeatherAfLo dimen 2;
set mTechWeatherAfUp dimen 2;
set mTechWeatherAfsLo dimen 2;
set mTechWeatherAfsUp dimen 2;
set mTechWeatherAfcLo dimen 3;
set mTechWeatherAfcUp dimen 3;
set mStorageWeatherAfLo dimen 2;
set mStorageWeatherAfUp dimen 2;
set mStorageWeatherCinpUp dimen 2;
set mStorageWeatherCinpLo dimen 2;
set mStorageWeatherCoutUp dimen 2;
set mStorageWeatherCoutLo dimen 2;
set mvSupCost dimen 3;
set mvTechInp dimen 5;
set mvSupReserve dimen 3;
set mvTechRetiredNewCap dimen 4;
set mvTechRetiredStock dimen 3;
set mvTechPhaseOut dimen 3;
set mvStoragePhaseOut dimen 3;
set mvTradePhaseOut dimen 2;
set mTechPho2AInp dimen 5;
set mTechPho2AOut dimen 5;
set mTechRet2AInp dimen 5;
set mTechRet2AOut dimen 5;
set mStoragePho2AInp dimen 5;
set mStoragePho2AOut dimen 5;
set mStorageRet2AInp dimen 5;
set mStorageRet2AOut dimen 5;
set mvTechAct dimen 4;
set mvTechInpCommSameTimeslice dimen 5;
set mvTechOut dimen 5;
set mvTechAInp dimen 5;
set mvTechAOut dimen 5;
set mvDemInp dimen 4;
set mvBalance dimen 4;
set mvInpTot dimen 4;
set mvOutTot dimen 4;
set mTechCapLo dimen 3;
set mTechCapUp dimen 3;
set mTechNewCapLo dimen 3;
set mTechNewCapUp dimen 3;
set mTechRetLo dimen 3;
set mTechRetUp dimen 3;
set mvStorageAInp dimen 5;
set mvStorageAOut dimen 5;
set mvStorageLevel dimen 5;
# Flow domains follow their OWN commodity, the level follows what is stored.
set mvStorageInp dimen 5;
set mvStorageOut dimen 5;
set meqStorageLevel dimen 6;
set mStorageStg2AOut dimen 5;
set mStorageCinp2AOut dimen 5;
set mStorageCout2AOut dimen 5;
set mStorageCap2AOut dimen 5;
set mStorageNCap2AOut dimen 5;
set mStorageStg2AInp dimen 5;
set mStorageCinp2AInp dimen 5;
set mStorageCout2AInp dimen 5;
set mStorageCap2AInp dimen 5;
set mStorageNCap2AInp dimen 5;
set mStorageInpCap dimen 3;
set mStorageNoInpCap dimen 3;
set mStorageInpNew dimen 3;
set mStorageInpCapLo dimen 3;
set mStorageInpCapUp dimen 3;
set mStorageInpNewCapLo dimen 3;
set mStorageInpNewCapUp dimen 3;
set mStorageInpFixom dimen 3;
set mStorageInpEac dimen 3;
set mStorageInp2outLo dimen 3;
set mStorageInp2outUp dimen 3;
set mStorageStgCap dimen 3;
set mStorageNoStgCap dimen 3;
set mStorageStgNew dimen 3;
set mStorageStgCapLo dimen 3;
set mStorageStgCapUp dimen 3;
set mStorageStgNewCapLo dimen 3;
set mStorageStgNewCapUp dimen 3;
set mStorageStgFixom dimen 3;
set mStorageStgEac dimen 3;
set mStorageDurationLo dimen 3;
set mStorageDurationUp dimen 3;
set mStorageOutCapLo dimen 3;
set mStorageOutCapUp dimen 3;
set mStorageOutNewCapLo dimen 3;
set mStorageOutNewCapUp dimen 3;
set mStorageOutRetLo dimen 3;
set mStorageOutRetUp dimen 3;
set mvStorageRetiredStock dimen 3;
set mvStorageRetiredNewCap dimen 4;
set meqStorageRetiredNewCap dimen 3;
set mStorageRetCost dimen 3;
set mStorageInpRetLo dimen 3;
set mStorageInpRetUp dimen 3;
set mStorageStgRetLo dimen 3;
set mStorageStgRetUp dimen 3;
set mvTradeRetiredStock dimen 2;
set mvTradeRetiredNewCap dimen 3;
set meqTradeRetiredNewCap dimen 2;
set mTradeRetCost dimen 3;
set mvTradeIr dimen 6;
set mTradeIrCsrc2Ainp dimen 6;
set mTradeIrCdst2Ainp dimen 6;
set mTradeIrCsrc2Aout dimen 6;
set mTradeIrCdst2Aout dimen 6;
set mExportRowCost dimen 3;
set mImportRowCost dimen 3;
set mImportIrCost dimen 3;
set mExportIrCost dimen 3;
set mvTotalCost dimen 2;
set mvTotalUserCosts dimen 2;
set mTradeCapLo dimen 2;
set mTradeCapUp dimen 2;
set mTradeNewCapLo dimen 2;
set mTradeNewCapUp dimen 2;
set mTradeRetLo dimen 2;
set mTradeRetUp dimen 2;
set mTechInv dimen 3;
set mTechInpTot dimen 4;
set mTechInpCommSameTimeslice dimen 2;
set mTechInpCommAgg dimen 2;
set mTechInpCommAggTimeslice dimen 4;
set mTechAInpCommSameTimeslice dimen 2;
set mTechAInpCommAgg dimen 2;
set mTechAInpCommAggTimeslice dimen 4;
set mTechOutTot dimen 4;
set mTechOutCommSameTimeslice dimen 2;
set mTechOutCommAgg dimen 2;
set mTechOutCommAggTimeslice dimen 4;
set mTechAOutCommSameTimeslice dimen 2;
set mTechAOutCommAgg dimen 2;
set mTechAOutCommAggTimeslice dimen 4;
set mTechEac dimen 3;
set mTechFixom dimen 3;
set mTechVarom dimen 3;
set mSupOutTot dimen 4;
set mEmsFuelTot dimen 4;
set mTechEmsFuel dimen 6;
set mDummyImport dimen 4;
set mDummyExport dimen 4;
set mDummyImportCost dimen 3;
set mDummyExportCost dimen 3;
set mTradeIr dimen 5;
set mvTradeIrAInp dimen 5;
set mvTradeIrAInpTot dimen 4;
set mvTradeIrAOut dimen 5;
set mvTradeIrAOutTot dimen 4;
set mImportRow dimen 5;
set mImportRowUp dimen 5;
set mImportRowCumUp dimen 2;
set mExportRow dimen 5;
set mExportRowUp dimen 5;
set mExportRowCumUp dimen 2;
set mExport dimen 4;
set mImport dimen 4;
set mStorageInpTot dimen 4;
set mStorageOutTot dimen 4;
set mTaxCost dimen 3;
set mSubCost dimen 3;
set mAggOut dimen 4;
set mTechAfUp dimen 4;
set mTechFullYear dimen 1;
set mTechRampUp dimen 5;
set mTechRampDown dimen 5;
set mTechCommOutTimesliceTimesliceP dimen 4;
set mTechCommAOutTimesliceTimesliceP dimen 4;
set mTechOlifeInf dimen 2;
set mStorageOlifeInf dimen 2;
set mTechAfcUp dimen 5;
set mSupAvaUp dimen 5;
set mSupAva dimen 5;
set mSupReserveUp dimen 3;
set meqTechRetiredNewCap dimen 3;
set meqTechSng2Sng dimen 6;
set meqTechGrp2Sng dimen 6;
set meqTechSng2Grp dimen 6;
set meqTechGrp2Grp dimen 6;
set meqTechShareInpLo dimen 6;
set meqTechShareInpUp dimen 6;
set meqTechShareOutLo dimen 6;
set meqTechShareOutUp dimen 6;
set meqTechAfLo dimen 4;
set meqTechAfUp dimen 4;
set meqTechAfsLo dimen 4;
set meqTechAfsUp dimen 4;
set meqTechActSng dimen 5;
set meqTechActGrp dimen 5;
set meqTechAfcOutLo dimen 5;
set meqTechAfcOutUp dimen 5;
set meqTechAfcInpLo dimen 5;
set meqTechAfcInpUp dimen 5;
set meqSupAvaLo dimen 5;
set meqSupReserveLo dimen 3;
set meqStorageAfLo dimen 5;
set meqStorageAfUp dimen 5;
set meqStorageInpUp dimen 5;
set meqStorageInpLo dimen 5;
set meqStorageOutUp dimen 5;
set meqStorageOutLo dimen 5;
set meqTradeFlowUp dimen 6;
set meqTradeFlowLo dimen 6;
set meqTradeIrAfUp dimen 6;
set meqTradeIrAfLo dimen 6;
set meqExportRowLo dimen 5;
set meqImportRowUp dimen 5;
set meqImportRowLo dimen 5;
set meqTradeCapFlow dimen 4;
set meqBalLo dimen 4;
set meqBalUp dimen 4;
set meqBalFx dimen 4;
set mTechAct2AInp dimen 5;
set mTechCap2AInp dimen 5;
set mTechNCap2AInp dimen 5;
set mTechCinp2AInp dimen 6;
set mTechCout2AInp dimen 6;
set mTechAct2AOut dimen 5;
set mTechCap2AOut dimen 5;
set mTechNCap2AOut dimen 5;
set mTechCinp2AOut dimen 6;
set mTechCout2AOut dimen 6;




param pYearFraction{year};
param pTechOlife{tech, region};
param pTechCinp2ginp{tech, comm, region, year, timeslice};
param pTechGinp2use{tech, group, region, year, timeslice};
param pTechCinp2use{tech, comm, region, year, timeslice};
param pTechUse2cact{tech, comm, region, year, timeslice};
param pTechCact2cout{tech, comm, region, year, timeslice};
param pTechEmisComm{tech, comm};
param pTechAct2AInp{tech, comm, region, year, timeslice};
param pTechCap2AInp{tech, comm, region, year, timeslice};
param pTechNCap2AInp{tech, comm, region, year, timeslice};
param pTechPho2AInp{tech, comm, region, year, timeslice};
param pTechPho2AOut{tech, comm, region, year, timeslice};
param pTechRet2AInp{tech, comm, region, year, timeslice};
param pTechRet2AOut{tech, comm, region, year, timeslice};
param pTechCinp2AInp{tech, comm, comm, region, year, timeslice};
param pTechCout2AInp{tech, comm, comm, region, year, timeslice};
param pTechAct2AOut{tech, comm, region, year, timeslice};
param pTechCap2AOut{tech, comm, region, year, timeslice};
param pTechNCap2AOut{tech, comm, region, year, timeslice};
param pTechCinp2AOut{tech, comm, comm, region, year, timeslice};
param pTechCout2AOut{tech, comm, comm, region, year, timeslice};
param pTechFixom{tech, region, year};
param pTechVarom{tech, region, year, timeslice};
param pTechInvcost{tech, region, year};
param pTechEac{tech, region, year};
param pTechRetCost{tech, region, year};
param pTechShareLo{tech, comm, region, year, timeslice};
param pTechShareUp{tech, comm, region, year, timeslice};
param pTechAfLo{tech, region, year, timeslice};
param pTechAfUp{tech, region, year, timeslice};
param pTechRampUp{tech, region, year, timeslice};
param pTechRampDown{tech, region, year, timeslice};
param pTechAfsLo{tech, region, year, timeslice};
param pTechAfsUp{tech, region, year, timeslice};
param pTechAfcLo{tech, comm, region, year, timeslice};
param pTechAfcUp{tech, comm, region, year, timeslice};
param pTechStock{tech, region, year};
param pTechStockNew{tech, region, year};
param pTechStockSurv{tech, region, year};
param pTechCapUp{tech, region, year};
param pTechCapLo{tech, region, year};
param pTechNewCapUp{tech, region, year};
param pTechNewCapLo{tech, region, year};
param pTechRetUp{tech, region, year};
param pTechRetLo{tech, region, year};
param pTechCap2act{tech};
param pTechCvarom{tech, comm, region, year, timeslice};
param pTechAvarom{tech, comm, region, year, timeslice};
param pWacc{region, year};
param pSdr{region, year};
param pTechWacc{tech, region, year};
param pStorageOutWacc{stg, region, year};
param pTradeWacc{trade, region, year};
param pTechPayback{tech, region, year};
param pStorageOutPayback{stg, region, year};
param pTradePayback{trade, region, year};
param pDiscountFactor{region, year};
param pDiscountFactorMileStone{region, year};
param pSupCost{sup, comm, region, year, timeslice};
param pSupAvaUp{sup, comm, region, year, timeslice};
param pSupAvaLo{sup, comm, region, year, timeslice};
param pSupReserveUp{sup, comm, region};
param pSupReserveLo{sup, comm, region};
param pDemand{dem, comm, region, year, timeslice};
param pEmissionFactor{comm, comm};
param pDummyImportCost{comm, region, year, timeslice};
param pDummyExportCost{comm, region, year, timeslice};
param pTaxCostInp{comm, region, year, timeslice};
param pTaxCostOut{comm, region, year, timeslice};
param pTaxCostBal{comm, region, year, timeslice};
param pSubCostInp{comm, region, year, timeslice};
param pSubCostOut{comm, region, year, timeslice};
param pSubCostBal{comm, region, year, timeslice};
param pAggregateFactor{comm, comm};
param pPeriodLen{year};
param pTimesliceShare{timeslice};
param pTimesliceWeight{year, timeslice};
# [agg-rewrite] intensive aggregation weight (year, coarse parent, immediate child)
# = pTimesliceWeight[year,child] / pTimesliceWeight[year,parent]; renormalizes under timeslice sampling.
param pTimesliceAgg{year, timeslice, timeslice};
param ordYear{year};
param cardYear{year};
param pStorageInpEff{stg, comm, region, year, timeslice};
param pStorageOutEff{stg, comm, region, year, timeslice};
param pStorageStgEff{stg, comm, region, year, timeslice};
param pStorageOutStock{stg, region, year};
param pStorageOutStockNew{stg, region, year};
param pStorageOutStockSurv{stg, region, year};
param pStorageOutCapUp{stg, region, year};
param pStorageOutCapLo{stg, region, year};
param pStorageOutNewCapUp{stg, region, year};
param pStorageOutNewCapLo{stg, region, year};
param pStorageOutRetUp{stg, region, year};
param pStorageOutRetLo{stg, region, year};
param pStorageOlife{stg, region};
param pStorageCostStore{stg, region, year, timeslice};
param pStorageCostInp{stg, region, year, timeslice};
param pStorageCostOut{stg, region, year, timeslice};
param pStorageOutFixom{stg, region, year};
param pStorageOutInvcost{stg, region, year};
param pStorageOutEac{stg, region, year};
param pStorageOutRetCost{stg, region, year};
# Symmetry fill for the charging and storing parts: the discharging part
# has always had these. Declared but NOT used by any equation -- storage
# retirement has no equation in any back-end, for any part. Declaring them
# keeps the .dat readable; giving them meaning is a separate change.
param pStorageInpRetUp{stg, region, year};
param pStorageInpRetLo{stg, region, year};
param pStorageInpWacc{stg, region, year};
param pStorageInpPayback{stg, region, year};
param pStorageInpRetCost{stg, region, year};
param pStorageStgRetUp{stg, region, year};
param pStorageStgRetLo{stg, region, year};
param pStorageStgWacc{stg, region, year};
param pStorageStgPayback{stg, region, year};
param pStorageStgRetCost{stg, region, year};
param pStorageInpCap2act{stg};
param pStorageOutCap2act{stg};
param pStorageInpStock{stg, region, year};
param pStorageInpStockNew{stg, region, year};
param pStorageInpStockSurv{stg, region, year};
param pStorageInpInvcost{stg, region, year};
param pStorageInpFixom{stg, region, year};
param pStorageInpEac{stg, region, year};
param pStorageInpCapLo{stg, region, year};
param pStorageInpCapUp{stg, region, year};
param pStorageInpNewCapLo{stg, region, year};
param pStorageInpNewCapUp{stg, region, year};
param pStorageInp2outLo{stg, region, year};
param pStorageInp2outUp{stg, region, year};
param pStorageStgStock{stg, region, year};
param pStorageStgStockNew{stg, region, year};
param pStorageStgStockSurv{stg, region, year};
param pStorageStgInvcost{stg, region, year};
param pStorageStgFixom{stg, region, year};
param pStorageStgEac{stg, region, year};
param pStorageStgCapLo{stg, region, year};
param pStorageStgCapUp{stg, region, year};
param pStorageStgNewCapLo{stg, region, year};
param pStorageStgNewCapUp{stg, region, year};
param pStorageDurationLo{stg, region, year};
param pStorageDurationUp{stg, region, year};
param pStorageAfLo{stg, region, year, timeslice};
param pStorageAfUp{stg, region, year, timeslice};
param pStorageCinpUp{stg, comm, region, year, timeslice};
param pStorageCinpLo{stg, comm, region, year, timeslice};
param pStorageCoutUp{stg, comm, region, year, timeslice};
param pStorageCoutLo{stg, comm, region, year, timeslice};
param pStorageNCap2Stg{stg, comm, region, year, timeslice};
param pStorageStartLevel{stg, comm, region, year, timeslice};
param pStorageStg2AInp{stg, comm, region, year, timeslice};
param pStorageStg2AOut{stg, comm, region, year, timeslice};
param pStorageCinp2AInp{stg, comm, region, year, timeslice};
param pStorageCinp2AOut{stg, comm, region, year, timeslice};
param pStorageCout2AInp{stg, comm, region, year, timeslice};
param pStorageCout2AOut{stg, comm, region, year, timeslice};
param pStorageCap2AInp{stg, comm, region, year, timeslice};
param pStorageCap2AOut{stg, comm, region, year, timeslice};
param pStorageNCap2AInp{stg, comm, region, year, timeslice};
param pStoragePho2AInp{stg, comm, region, year, timeslice};
param pStoragePho2AOut{stg, comm, region, year, timeslice};
param pStorageRet2AInp{stg, comm, region, year, timeslice};
param pStorageRet2AOut{stg, comm, region, year, timeslice};
param pStorageNCap2AOut{stg, comm, region, year, timeslice};
param pTradeIrEff{trade, region, region, year, timeslice};
param pTradeIrUp{trade, region, region, year, timeslice};
param pTradeIrLo{trade, region, region, year, timeslice};
param pTradeIrAfUp{trade, region, region, year, timeslice};
param pTradeIrAfLo{trade, region, region, year, timeslice};
param pTradeIrCost{trade, region, region, year, timeslice};
param pTradeIrMarkup{trade, region, region, year, timeslice};
param pTradeIrCsrc2Ainp{trade, comm, region, region, year, timeslice};
param pTradeIrCsrc2Aout{trade, comm, region, region, year, timeslice};
param pTradeIrCdst2Ainp{trade, comm, region, region, year, timeslice};
param pTradeIrCdst2Aout{trade, comm, region, region, year, timeslice};
param pExportRowRes{expp};
param pExportRowUp{expp, region, year, timeslice};
param pExportRowLo{expp, region, year, timeslice};
param pExportRowPrice{expp, region, year, timeslice};
param pImportRowRes{imp};
param pImportRowUp{imp, region, year, timeslice};
param pImportRowLo{imp, region, year, timeslice};
param pImportRowPrice{imp, region, year, timeslice};
param pTradeStock{trade, year};
param pTradeStockNew{trade, year};
param pTradeStockSurv{trade, year};
param pTradeCapUp{trade, year};
param pTradeCapLo{trade, year};
param pTradeNewCapUp{trade, year};
param pTradeNewCapLo{trade, year};
param pTradeRetUp{trade, year};
param pTradeRetLo{trade, year};
param pTradeOlife{trade};
param pTradeInvcost{trade, region, year};
param pTradeEac{trade, region, year};
param pTradeRetCost{trade, region, year};
param pTradeFixom{trade, region, year};
# [removed] pTradeVarom -- trade @varom is not implemented: no equation read it,
# it was absent from Julia and Pyomo, and it is unregistered in modInp.yml.
param pTradeCap2Act{trade};
param pWeather{weather, region, year, timeslice};
param pSupWeatherUp{weather, sup};
param pSupWeatherLo{weather, sup};
param pTechWeatherAfLo{weather, tech};
param pTechWeatherAfUp{weather, tech};
param pTechWeatherAfsLo{weather, tech};
param pTechWeatherAfsUp{weather, tech};
param pTechWeatherAfcLo{weather, tech, comm};
param pTechWeatherAfcUp{weather, tech, comm};
param pStorageWeatherAfLo{weather, stg};
param pStorageWeatherAfUp{weather, stg};
param pStorageWeatherCinpUp{weather, stg};
param pStorageWeatherCinpLo{weather, stg};
param pStorageWeatherCoutUp{weather, stg};
param pStorageWeatherCoutLo{weather, stg};
param ORD{year};



var vTechInv{tech, region, year};
var vTechEac{tech, region, year};
var vTechRetCost{tech, region, year};
var vTechFixom{tech, region, year};
var vTechVarom{tech, region, year};
var vSupCost{sup, region, year};
var vEmsFuelTot{comm, region, year, timeslice};
var vBalance{comm, region, year, timeslice};
var vTotalCost{region, year};
var vObjective;
var vTaxCost{comm, region, year};
var vSubsCost{comm, region, year};
var vAggOutTot{comm, region, year, timeslice};
var vDummyImportCost{comm, region, year};
var vDummyExportCost{comm, region, year};
var vStorageFixom{stg, region, year};
var vStorageVarom{stg, region, year};
var vTradeEac{trade, region, year};
var vTradeFixom{trade, region, year};
var vImportIrCost{trade, region, year};
var vExportIrCost{trade, region, year};
var vImportRowCost{imp, region, year};
var vExportRowCost{expp, region, year};




var vTechNewCap{tech, region, year} >= 0;
var vTechRetiredStock{tech, region, year} >= 0;
var vTechRetiredNewCap{tech, region, year, year} >= 0;
var vTechPhaseOut{tech, region, year} >= 0;
var vTechStockCap{tech, region, year} >= 0;
var vTechStockPhaseOut{tech, region, year} >= 0;
var vStoragePhaseOut{stg, region, year} >= 0;
var vStorageStockPhaseOut{stg, region, year} >= 0;
var vStorageOutStockCap{stg, region, year} >= 0;
var vStorageOutRetiredStock{stg, region, year} >= 0;
var vStorageOutRetiredNewCap{stg, region, year, year} >= 0;
var vStorageInpStockCap{stg, region, year} >= 0;
var vStorageInpRetiredStock{stg, region, year} >= 0;
var vStorageInpRetiredNewCap{stg, region, year, year} >= 0;
var vStorageStgStockCap{stg, region, year} >= 0;
var vStorageStgRetiredStock{stg, region, year} >= 0;
var vStorageStgRetiredNewCap{stg, region, year, year} >= 0;
var vStorageRetCost{stg, region, year};
var vTradeStockCap{trade, year} >= 0;
var vTradePhaseOut{trade, year} >= 0;
var vTradeStockPhaseOut{trade, year} >= 0;
var vTradeRetiredStock{trade, year} >= 0;
var vTradeRetiredNewCap{trade, year, year} >= 0;
var vTradeRetCost{trade, region, year};
var vTechCap{tech, region, year} >= 0;
var vTechAct{tech, region, year, timeslice} >= 0;
var vTechInp{tech, comm, region, year, timeslice} >= 0;
var vTechOut{tech, comm, region, year, timeslice} >= 0;
var vTechAInp{tech, comm, region, year, timeslice} >= 0;
var vTechAOut{tech, comm, region, year, timeslice} >= 0;
var vSupOut{sup, comm, region, year, timeslice} >= 0;
var vSupReserve{sup, comm, region} >= 0;
var vDemInp{comm, region, year, timeslice} >= 0;
var vOutTot{comm, region, year, timeslice} >= 0;
var vInpTot{comm, region, year, timeslice} >= 0;
# [agg-rewrite] vInp2Lo/vOut2Lo removed: coarse-level flows are no longer split DOWN
# to native timeslices. They now feed the commodity total at their OWN (coarse) balance
# level, and finer levels aggregate UP (see eqOutTot/eqInpTot, mTimesliceFamily/pTimesliceAgg).
# var vInp2Lo{comm, region, year, timeslice, timeslice} >= 0;
# var vOut2Lo{comm, region, year, timeslice, timeslice} >= 0;
var vSupOutTot{comm, region, year, timeslice} >= 0;
var vTechInpTot{comm, region, year, timeslice} >= 0;
var vTechOutTot{comm, region, year, timeslice} >= 0;
var vStorageInpTot{comm, region, year, timeslice} >= 0;
var vStorageOutTot{comm, region, year, timeslice} >= 0;
var vStorageAInp{stg, comm, region, year, timeslice} >= 0;
var vStorageAOut{stg, comm, region, year, timeslice} >= 0;
var vDummyImport{comm, region, year, timeslice} >= 0;
var vDummyExport{comm, region, year, timeslice} >= 0;
var vStorageInp{stg, comm, region, year, timeslice} >= 0;
var vStorageOut{stg, comm, region, year, timeslice} >= 0;
var vStorageLevel{stg, comm, region, year, timeslice} >= 0;
var vStorageInv{stg, region, year} >= 0;
var vStorageEac{stg, region, year} >= 0;
var vStorageOutCap{stg, region, year} >= 0;
var vStorageStgCap{stg, region, year} >= 0;
var vStorageInpCap{stg, region, year} >= 0;
var vStorageInpNewCap{stg, region, year} >= 0;
var vStorageStgNewCap{stg, region, year} >= 0;
var vStorageOutNewCap{stg, region, year} >= 0;
var vImportTot{comm, region, year, timeslice} >= 0;
var vExportTot{comm, region, year, timeslice} >= 0;
var vTradeIr{trade, comm, region, region, year, timeslice} >= 0;
var vTradeIrAInp{trade, comm, region, year, timeslice} >= 0;
var vTradeIrAInpTot{comm, region, year, timeslice} >= 0;
var vTradeIrAOut{trade, comm, region, year, timeslice} >= 0;
var vTradeIrAOutTot{comm, region, year, timeslice} >= 0;
var vExportRowCum{expp, comm} >= 0;
var vExportRow{expp, comm, region, year, timeslice} >= 0;
var vImportRowCum{imp, comm} >= 0;
var vImportRow{imp, comm, region, year, timeslice} >= 0;
var vTradeCap{trade, year} >= 0;
var vTradeInv{trade, region, year} >= 0;
var vTradeNewCap{trade, year} >= 0;
var vTotalUserCosts{region, year} >= 0;




# Guid for add equation and add mapping & parameter to constrain
# 22b584bd-a17a-4fa0-9cd9-f603ab684e47
s.t.  eqTechSng2Sng{(t, r, c, cp, y, s) in meqTechSng2Sng}: vTechInp[t,c,r,y,s]*pTechCinp2use[t,c,r,y,s]  =  (vTechOut[t,cp,r,y,s]) / (pTechUse2cact[t,cp,r,y,s]*pTechCact2cout[t,cp,r,y,s]);

s.t.  eqTechGrp2Sng{(t, r, g, cp, y, s) in meqTechGrp2Sng}: pTechGinp2use[t,g,r,y,s]*sum{c in comm:((t,g,c) in mTechGroupComm)}(sum{fi in FORIF: (t,c,r,y,s) in mvTechInp} ((vTechInp[t,c,r,y,s]*pTechCinp2ginp[t,c,r,y,s])))  =  (vTechOut[t,cp,r,y,s]) / (pTechUse2cact[t,cp,r,y,s]*pTechCact2cout[t,cp,r,y,s]);

s.t.  eqTechSng2Grp{(t, r, c, gp, y, s) in meqTechSng2Grp}: vTechInp[t,c,r,y,s]*pTechCinp2use[t,c,r,y,s]  =  sum{cp in comm:((t,gp,cp) in mTechGroupComm)}(sum{fi in FORIF: (t,cp,r,y,s) in mvTechOut} (((vTechOut[t,cp,r,y,s]) / (pTechUse2cact[t,cp,r,y,s]*pTechCact2cout[t,cp,r,y,s]))));

s.t.  eqTechGrp2Grp{(t, r, g, gp, y, s) in meqTechGrp2Grp}: pTechGinp2use[t,g,r,y,s]*sum{c in comm:((t,g,c) in mTechGroupComm)}(sum{fi in FORIF: (t,c,r,y,s) in mvTechInp} ((vTechInp[t,c,r,y,s]*pTechCinp2ginp[t,c,r,y,s])))  =  sum{cp in comm:((t,gp,cp) in mTechGroupComm)}(sum{fi in FORIF: (t,cp,r,y,s) in mvTechOut} (((vTechOut[t,cp,r,y,s]) / (pTechUse2cact[t,cp,r,y,s]*pTechCact2cout[t,cp,r,y,s]))));

s.t.  eqTechShareInpLo{(t, r, g, c, y, s) in meqTechShareInpLo}: vTechInp[t,c,r,y,s]  >=  pTechShareLo[t,c,r,y,s]*sum{cp in comm:((t,g,cp) in mTechGroupComm)}(sum{fi in FORIF: (t,cp,r,y,s) in mvTechInp} (vTechInp[t,cp,r,y,s]));

s.t.  eqTechShareInpUp{(t, r, g, c, y, s) in meqTechShareInpUp}: vTechInp[t,c,r,y,s] <=  pTechShareUp[t,c,r,y,s]*sum{cp in comm:((t,g,cp) in mTechGroupComm)}(sum{fi in FORIF: (t,cp,r,y,s) in mvTechInp} (vTechInp[t,cp,r,y,s]));

s.t.  eqTechShareOutLo{(t, r, g, c, y, s) in meqTechShareOutLo}: vTechOut[t,c,r,y,s]  >=  pTechShareLo[t,c,r,y,s]*sum{cp in comm:((t,g,cp) in mTechGroupComm)}(sum{fi in FORIF: (t,cp,r,y,s) in mvTechOut} (vTechOut[t,cp,r,y,s]));

s.t.  eqTechShareOutUp{(t, r, g, c, y, s) in meqTechShareOutUp}: vTechOut[t,c,r,y,s] <=  pTechShareUp[t,c,r,y,s]*sum{cp in comm:((t,g,cp) in mTechGroupComm)}(sum{fi in FORIF: (t,cp,r,y,s) in mvTechOut} (vTechOut[t,cp,r,y,s]));

s.t.  eqTechAInp{(t, c, r, y, s) in mvTechAInp}: vTechAInp[t,c,r,y,s]  =  sum{fi in FORIF: (t,c,r,y,s) in mTechAct2AInp} ((vTechAct[t,r,y,s]*pTechAct2AInp[t,c,r,y,s]))+sum{fi in FORIF: (t,c,r,y,s) in mTechCap2AInp} (((vTechCap[t,r,y]*pTechCap2AInp[t,c,r,y,s]) / (pTechCap2act[t])))+sum{fi in FORIF: (t,c,r,y,s) in mTechNCap2AInp} ((vTechNewCap[t,r,y]*pTechNCap2AInp[t,c,r,y,s]))+sum{fi in FORIF: (t,c,r,y,s) in mTechPho2AInp} ((sum{fi in FORIF: (t,r,y) in mvTechPhaseOut} (vTechPhaseOut[t,r,y])*pTechPho2AInp[t,c,r,y,s]))+sum{fi in FORIF: (t,c,r,y,s) in mTechRet2AInp} (((sum{fi in FORIF: (t,r,y) in mvTechRetiredStock} (vTechRetiredStock[t,r,y])+sum{yp in year:((t,r,yp,y) in mvTechRetiredNewCap)}(vTechRetiredNewCap[t,r,yp,y]))*pTechRet2AInp[t,c,r,y,s]))+sum{cp in comm:((t,c,cp,r,y,s) in mTechCinp2AInp)}(pTechCinp2AInp[t,c,cp,r,y,s]*vTechInp[t,cp,r,y,s])+sum{cp in comm:((t,c,cp,r,y,s) in mTechCout2AInp)}(pTechCout2AInp[t,c,cp,r,y,s]*vTechOut[t,cp,r,y,s]);

s.t.  eqTechAOut{(t, c, r, y, s) in mvTechAOut}: vTechAOut[t,c,r,y,s]  =  sum{fi in FORIF: (t,c,r,y,s) in mTechAct2AOut} ((vTechAct[t,r,y,s]*pTechAct2AOut[t,c,r,y,s]))+sum{fi in FORIF: (t,c,r,y,s) in mTechCap2AOut} (((vTechCap[t,r,y]*pTechCap2AOut[t,c,r,y,s]) / (pTechCap2act[t])))+sum{fi in FORIF: (t,c,r,y,s) in mTechNCap2AOut} ((vTechNewCap[t,r,y]*pTechNCap2AOut[t,c,r,y,s]))+sum{fi in FORIF: (t,c,r,y,s) in mTechPho2AOut} ((sum{fi in FORIF: (t,r,y) in mvTechPhaseOut} (vTechPhaseOut[t,r,y])*pTechPho2AOut[t,c,r,y,s]))+sum{fi in FORIF: (t,c,r,y,s) in mTechRet2AOut} (((sum{fi in FORIF: (t,r,y) in mvTechRetiredStock} (vTechRetiredStock[t,r,y])+sum{yp in year:((t,r,yp,y) in mvTechRetiredNewCap)}(vTechRetiredNewCap[t,r,yp,y]))*pTechRet2AOut[t,c,r,y,s]))+sum{cp in comm:((t,c,cp,r,y,s) in mTechCinp2AOut)}(pTechCinp2AOut[t,c,cp,r,y,s]*vTechInp[t,cp,r,y,s])+sum{cp in comm:((t,c,cp,r,y,s) in mTechCout2AOut)}(pTechCout2AOut[t,c,cp,r,y,s]*vTechOut[t,cp,r,y,s]);

s.t.  eqTechAfLo{(t, r, y, s) in meqTechAfLo}: pTechAfLo[t,r,y,s]*pTechCap2act[t]*vTechCap[t,r,y]*pTimesliceShare[s]*(1 + sum{wth1 in weather:((wth1,t) in mTechWeatherAfLo)}(pTechWeatherAfLo[wth1,t]*pWeather[wth1,r,y,s] - 1)) <=  vTechAct[t,r,y,s];

s.t.  eqTechAfUp{(t, r, y, s) in meqTechAfUp}: vTechAct[t,r,y,s] <=  pTechAfUp[t,r,y,s]*pTechCap2act[t]*vTechCap[t,r,y]*pTimesliceShare[s]*(1 + sum{wth1 in weather:((wth1,t) in mTechWeatherAfUp)}(pTechWeatherAfUp[wth1,t]*pWeather[wth1,r,y,s] - 1));

s.t.  eqTechAfsLo{(t, r, y, s) in meqTechAfsLo}: pTechAfsLo[t,r,y,s]*pTechCap2act[t]*vTechCap[t,r,y]*pTimesliceShare[s]*(1 + sum{wth1 in weather:((wth1,t) in mTechWeatherAfsLo)}(pTechWeatherAfsLo[wth1,t]*pWeather[wth1,r,y,s] - 1)) <=  sum{sp in timeslice:((s,sp) in mTimesliceParentChildE)}(sum{fi in FORIF: (t,r,y,sp) in mvTechAct} (vTechAct[t,r,y,sp]));

s.t.  eqTechAfsUp{(t, r, y, s) in meqTechAfsUp}: sum{sp in timeslice:((s,sp) in mTimesliceParentChildE)}(sum{fi in FORIF: (t,r,y,sp) in mvTechAct} (vTechAct[t,r,y,sp])) <=  pTechAfsUp[t,r,y,s]*pTechCap2act[t]*vTechCap[t,r,y]*pTimesliceShare[s]*(1 + sum{wth1 in weather:((wth1,t) in mTechWeatherAfsUp)}(pTechWeatherAfsUp[wth1,t]*pWeather[wth1,r,y,s] - 1));

s.t.  eqTechRampUp{(t, r, y, s, sp) in mTechRampUp}: (vTechAct[t,r,y,s]) / (pTimesliceShare[s])-(vTechAct[t,r,y,sp]) / (pTimesliceShare[sp]) <=  (pTimesliceShare[s]*pTechCap2act[t]*pTechCap2act[t]*vTechCap[t,r,y]) / (pTechRampUp[t,r,y,s]);

s.t.  eqTechRampDown{(t, r, y, s, sp) in mTechRampDown}: (vTechAct[t,r,y,sp]) / (pTimesliceShare[sp])-(vTechAct[t,r,y,s]) / (pTimesliceShare[s]) <=  (pTimesliceShare[s]*pTechCap2act[t]*pTechCap2act[t]*vTechCap[t,r,y]) / (pTechRampDown[t,r,y,s]);

s.t.  eqTechActSng{(t, c, r, y, s) in meqTechActSng}: vTechAct[t,r,y,s]  =  (vTechOut[t,c,r,y,s]) / (pTechCact2cout[t,c,r,y,s]);

s.t.  eqTechActGrp{(t, g, r, y, s) in meqTechActGrp}: vTechAct[t,r,y,s]  =  sum{c in comm:((t,g,c) in mTechGroupComm)}(sum{fi in FORIF: (t,c,r,y,s) in mvTechOut} (((vTechOut[t,c,r,y,s]) / (pTechCact2cout[t,c,r,y,s]))));

s.t.  eqTechAfcOutLo{(t, r, c, y, s) in meqTechAfcOutLo}: pTechCact2cout[t,c,r,y,s]*pTechAfcLo[t,c,r,y,s]*pTechCap2act[t]*vTechCap[t,r,y]*pTimesliceShare[s]*(1 + sum{wth1 in weather:((wth1,t,c) in mTechWeatherAfcLo)}(pTechWeatherAfcLo[wth1,t,c]*pWeather[wth1,r,y,s] - 1)) <=  vTechOut[t,c,r,y,s];

s.t.  eqTechAfcOutUp{(t, r, c, y, s) in meqTechAfcOutUp}: vTechOut[t,c,r,y,s] <=  pTechCact2cout[t,c,r,y,s]*pTechAfcUp[t,c,r,y,s]*pTechCap2act[t]*vTechCap[t,r,y]*(1 + sum{wth1 in weather:((wth1,t,c) in mTechWeatherAfcUp)}(pTechWeatherAfcUp[wth1,t,c]*pWeather[wth1,r,y,s] - 1));

s.t.  eqTechAfcInpLo{(t, r, c, y, s) in meqTechAfcInpLo}: pTechAfcLo[t,c,r,y,s]*pTechCap2act[t]*vTechCap[t,r,y]*pTimesliceShare[s]*(1 + sum{wth1 in weather:((wth1,t,c) in mTechWeatherAfcLo)}(pTechWeatherAfcLo[wth1,t,c]*pWeather[wth1,r,y,s] - 1)) <=  vTechInp[t,c,r,y,s];

s.t.  eqTechAfcInpUp{(t, r, c, y, s) in meqTechAfcInpUp}: vTechInp[t,c,r,y,s] <=  pTechAfcUp[t,c,r,y,s]*pTechCap2act[t]*vTechCap[t,r,y]*pTimesliceShare[s]*(1 + sum{wth1 in weather:((wth1,t,c) in mTechWeatherAfcUp)}(pTechWeatherAfcUp[wth1,t,c]*pWeather[wth1,r,y,s] - 1));

s.t.  eqTechCap{(t, r, y) in mTechSpan}: vTechCap[t,r,y]  =  vTechStockCap[t,r,y]+sum{yp in year:(((t,r,yp) in mTechNew and ordYear[y] >= ordYear[yp] and (ordYear[y]<pTechOlife[t,r]+ordYear[yp] or (t,r) in mTechOlifeInf)))}(pPeriodLen[yp]*vTechNewCap[t,r,yp]+(-1)*sum{ye in year:(((t,r,yp,ye) in mvTechRetiredNewCap and ordYear[y] >= ordYear[ye]))}(vTechRetiredNewCap[t,r,yp,ye]*pPeriodLen[ye]));

s.t.  eqTechCapLo{(t, r, y) in mTechCapLo}: vTechCap[t,r,y]  >=  pTechCapLo[t,r,y];

s.t.  eqTechCapUp{(t, r, y) in mTechCapUp}: vTechCap[t,r,y] <=  pTechCapUp[t,r,y];

s.t.  eqTechNewCapLo{(t, r, y) in mTechNewCapLo}: vTechNewCap[t,r,y]  >=  pTechNewCapLo[t,r,y]*pPeriodLen[y];

s.t.  eqTechNewCapUp{(t, r, y) in mTechNewCapUp}: vTechNewCap[t,r,y] <=  pTechNewCapUp[t,r,y]*pPeriodLen[y];

s.t.  eqTechRetiredNewCap{(t, r, y) in meqTechRetiredNewCap}: sum{yp in year:((t,r,y,yp) in mvTechRetiredNewCap)}(vTechRetiredNewCap[t,r,y,yp]*pPeriodLen[yp]) <=  vTechNewCap[t,r,y]*pPeriodLen[y];

s.t.  eqTechRetUp{(t, r, y) in mTechRetUp}: sum{fi in FORIF: (t,r,y) in mvTechRetiredStock} (vTechRetiredStock[t,r,y])+sum{yp in year:((t,r,yp,y) in mvTechRetiredNewCap)}(vTechRetiredNewCap[t,r,yp,y]) <=  pTechRetUp[t,r,y]*pPeriodLen[y];

s.t.  eqTechRetLo{(t, r, y) in mTechRetLo}: sum{fi in FORIF: (t,r,y) in mvTechRetiredStock} (vTechRetiredStock[t,r,y])+sum{yp in year:((t,r,yp,y) in mvTechRetiredNewCap)}(vTechRetiredNewCap[t,r,yp,y])  >=  pTechRetLo[t,r,y]*pPeriodLen[y];

s.t.  eqTechRetCost{(t, r, y) in mTechRetCost}: vTechRetCost[t,r,y]  =  pTechRetCost[t,r,y]*sum{fi in FORIF: (t,r,y) in mvTechRetiredStock} (vTechRetiredStock[t,r,y])+sum{yp in year:((t,r,yp,y) in mvTechRetiredNewCap)}(pTechRetCost[t,r,y]*sum{fi in FORIF: (t,r,yp,y) in mvTechRetiredNewCap} (vTechRetiredNewCap[t,r,yp,y]));

s.t.  eqTechPhaseOut{(t, r, y) in mvTechPhaseOut}: vTechPhaseOut[t,r,y]*pPeriodLen[y]  =  sum{yp in year:(((yp,y) in mMilestoneNext and (t,r,yp) in mTechSpan))}(vTechCap[t,r,yp])-vTechCap[t,r,y]+vTechNewCap[t,r,y]*pPeriodLen[y]-(sum{fi in FORIF: (t,r,y) in mvTechRetiredStock} (vTechRetiredStock[t,r,y])+sum{yp in year:((t,r,yp,y) in mvTechRetiredNewCap)}(vTechRetiredNewCap[t,r,yp,y]))*pPeriodLen[y]+pTechStockNew[t,r,y];

s.t.  eqTechStockCap{(t, r, y) in mTechSpan}: vTechStockCap[t,r,y]  =  pTechStockSurv[t,r,y]*sum{yp in year:(((yp,y) in mMilestoneNext and (t,r,yp) in mTechSpan))}(vTechStockCap[t,r,yp])+pTechStockNew[t,r,y]+(-1)*sum{fi in FORIF: (t,r,y) in mvTechRetiredStock} (vTechRetiredStock[t,r,y])*pPeriodLen[y];

s.t.  eqTechStockPhaseOut{(t, r, y) in mvTechPhaseOut}: vTechStockPhaseOut[t,r,y]*pPeriodLen[y]  =  (1-pTechStockSurv[t,r,y])*sum{yp in year:(((yp,y) in mMilestoneNext and (t,r,yp) in mTechSpan))}(vTechStockCap[t,r,yp]);

s.t.  eqStoragePhaseOut{(st1, r, y) in mvStoragePhaseOut}: vStoragePhaseOut[st1,r,y]*pPeriodLen[y]  =  sum{yp in year:(((yp,y) in mMilestoneNext and (st1,r,yp) in mStorageSpan))}(vStorageOutCap[st1,r,yp])-vStorageOutCap[st1,r,y]+vStorageOutNewCap[st1,r,y]*pPeriodLen[y]-(sum{fi in FORIF: (st1,r,y) in mvStorageRetiredStock} (vStorageOutRetiredStock[st1,r,y])+sum{yp in year:((st1,r,yp,y) in mvStorageRetiredNewCap)}(vStorageOutRetiredNewCap[st1,r,yp,y]))*pPeriodLen[y]+pStorageOutStockNew[st1,r,y];

s.t.  eqStorageStockPhaseOut{(st1, r, y) in mvStoragePhaseOut}: vStorageStockPhaseOut[st1,r,y]*pPeriodLen[y]  =  (1-pStorageOutStockSurv[st1,r,y])*sum{yp in year:(((yp,y) in mMilestoneNext and (st1,r,yp) in mStorageSpan))}(vStorageOutStockCap[st1,r,yp]);

s.t.  eqStorageOutRetiredNewCap{(st1, r, y) in meqStorageRetiredNewCap}: sum{yp in year:((st1,r,y,yp) in mvStorageRetiredNewCap)}(vStorageOutRetiredNewCap[st1,r,y,yp]*pPeriodLen[yp]) <=  vStorageOutNewCap[st1,r,y]*pPeriodLen[y];

s.t.  eqStorageOutStockCap{(st1, r, y) in mStorageSpan}: vStorageOutStockCap[st1,r,y]  =  pStorageOutStockSurv[st1,r,y]*sum{yp in year:(((yp,y) in mMilestoneNext and (st1,r,yp) in mStorageSpan))}(vStorageOutStockCap[st1,r,yp])+pStorageOutStockNew[st1,r,y]+(-1)*sum{fi in FORIF: (st1,r,y) in mvStorageRetiredStock} (vStorageOutRetiredStock[st1,r,y])*pPeriodLen[y];

s.t.  eqStorageOutRetUp{(st1, r, y) in mStorageOutRetUp}: sum{fi in FORIF: (st1,r,y) in mvStorageRetiredStock} (vStorageOutRetiredStock[st1,r,y])+sum{yp in year:((st1,r,yp,y) in mvStorageRetiredNewCap)}(vStorageOutRetiredNewCap[st1,r,yp,y]) <=  pStorageOutRetUp[st1,r,y]*pPeriodLen[y];

s.t.  eqStorageOutRetLo{(st1, r, y) in mStorageOutRetLo}: sum{fi in FORIF: (st1,r,y) in mvStorageRetiredStock} (vStorageOutRetiredStock[st1,r,y])+sum{yp in year:((st1,r,yp,y) in mvStorageRetiredNewCap)}(vStorageOutRetiredNewCap[st1,r,yp,y])  >=  pStorageOutRetLo[st1,r,y]*pPeriodLen[y];

s.t.  eqStorageInpRetiredNewCap{(st1, r, y) in meqStorageRetiredNewCap}: sum{yp in year:((st1,r,y,yp) in mvStorageRetiredNewCap)}(vStorageInpRetiredNewCap[st1,r,y,yp]*pPeriodLen[yp]) <=  vStorageInpNewCap[st1,r,y]*pPeriodLen[y];

s.t.  eqStorageInpStockCap{(st1, r, y) in mStorageInpCap}: vStorageInpStockCap[st1,r,y]  =  pStorageInpStockSurv[st1,r,y]*sum{yp in year:(((yp,y) in mMilestoneNext and (st1,r,yp) in mStorageInpCap))}(vStorageInpStockCap[st1,r,yp])+pStorageInpStockNew[st1,r,y]+(-1)*sum{fi in FORIF: (st1,r,y) in mvStorageRetiredStock} (vStorageInpRetiredStock[st1,r,y])*pPeriodLen[y];

s.t.  eqStorageInpRetUp{(st1, r, y) in mStorageInpRetUp}: sum{fi in FORIF: (st1,r,y) in mvStorageRetiredStock} (vStorageInpRetiredStock[st1,r,y])+sum{yp in year:((st1,r,yp,y) in mvStorageRetiredNewCap)}(vStorageInpRetiredNewCap[st1,r,yp,y]) <=  pStorageInpRetUp[st1,r,y]*pPeriodLen[y];

s.t.  eqStorageInpRetLo{(st1, r, y) in mStorageInpRetLo}: sum{fi in FORIF: (st1,r,y) in mvStorageRetiredStock} (vStorageInpRetiredStock[st1,r,y])+sum{yp in year:((st1,r,yp,y) in mvStorageRetiredNewCap)}(vStorageInpRetiredNewCap[st1,r,yp,y])  >=  pStorageInpRetLo[st1,r,y]*pPeriodLen[y];

s.t.  eqStorageStgRetiredNewCap{(st1, r, y) in meqStorageRetiredNewCap}: sum{yp in year:((st1,r,y,yp) in mvStorageRetiredNewCap)}(vStorageStgRetiredNewCap[st1,r,y,yp]*pPeriodLen[yp]) <=  vStorageStgNewCap[st1,r,y]*pPeriodLen[y];

s.t.  eqStorageStgStockCap{(st1, r, y) in mStorageStgCap}: vStorageStgStockCap[st1,r,y]  =  pStorageStgStockSurv[st1,r,y]*sum{yp in year:(((yp,y) in mMilestoneNext and (st1,r,yp) in mStorageStgCap))}(vStorageStgStockCap[st1,r,yp])+pStorageStgStockNew[st1,r,y]+(-1)*sum{fi in FORIF: (st1,r,y) in mvStorageRetiredStock} (vStorageStgRetiredStock[st1,r,y])*pPeriodLen[y];

s.t.  eqStorageStgRetUp{(st1, r, y) in mStorageStgRetUp}: sum{fi in FORIF: (st1,r,y) in mvStorageRetiredStock} (vStorageStgRetiredStock[st1,r,y])+sum{yp in year:((st1,r,yp,y) in mvStorageRetiredNewCap)}(vStorageStgRetiredNewCap[st1,r,yp,y]) <=  pStorageStgRetUp[st1,r,y]*pPeriodLen[y];

s.t.  eqStorageStgRetLo{(st1, r, y) in mStorageStgRetLo}: sum{fi in FORIF: (st1,r,y) in mvStorageRetiredStock} (vStorageStgRetiredStock[st1,r,y])+sum{yp in year:((st1,r,yp,y) in mvStorageRetiredNewCap)}(vStorageStgRetiredNewCap[st1,r,yp,y])  >=  pStorageStgRetLo[st1,r,y]*pPeriodLen[y];

s.t.  eqStorageRetCost{(st1, r, y) in mStorageRetCost}: vStorageRetCost[st1,r,y]  =  pStorageOutRetCost[st1,r,y]*(sum{fi in FORIF: (st1,r,y) in mvStorageRetiredStock} (vStorageOutRetiredStock[st1,r,y])+sum{yp in year:((st1,r,yp,y) in mvStorageRetiredNewCap)}(vStorageOutRetiredNewCap[st1,r,yp,y]))+pStorageInpRetCost[st1,r,y]*(sum{fi in FORIF: (st1,r,y) in mvStorageRetiredStock} (vStorageInpRetiredStock[st1,r,y])+sum{yp in year:((st1,r,yp,y) in mvStorageRetiredNewCap)}(vStorageInpRetiredNewCap[st1,r,yp,y]))+pStorageStgRetCost[st1,r,y]*(sum{fi in FORIF: (st1,r,y) in mvStorageRetiredStock} (vStorageStgRetiredStock[st1,r,y])+sum{yp in year:((st1,r,yp,y) in mvStorageRetiredNewCap)}(vStorageStgRetiredNewCap[st1,r,yp,y]));

s.t.  eqTradeRetiredNewCap{(t1, y) in meqTradeRetiredNewCap}: sum{yp in year:((t1,y,yp) in mvTradeRetiredNewCap)}(vTradeRetiredNewCap[t1,y,yp]*pPeriodLen[yp]) <=  vTradeNewCap[t1,y]*pPeriodLen[y];

s.t.  eqTradeStockCap{(t1, y) in mTradeSpan}: vTradeStockCap[t1,y]  =  pTradeStockSurv[t1,y]*sum{yp in year:(((yp,y) in mMilestoneNext and (t1,yp) in mTradeSpan))}(vTradeStockCap[t1,yp])+pTradeStockNew[t1,y]+(-1)*sum{fi in FORIF: (t1,y) in mvTradeRetiredStock} (vTradeRetiredStock[t1,y])*pPeriodLen[y];

s.t.  eqTradePhaseOut{(t1, y) in mvTradePhaseOut}: vTradePhaseOut[t1,y]*pPeriodLen[y]  =  sum{yp in year:(((yp,y) in mMilestoneNext and (t1,yp) in mTradeSpan))}(vTradeCap[t1,yp])-vTradeCap[t1,y]+vTradeNewCap[t1,y]*pPeriodLen[y]-(sum{fi in FORIF: (t1,y) in mvTradeRetiredStock} (vTradeRetiredStock[t1,y])+sum{yp in year:((t1,yp,y) in mvTradeRetiredNewCap)}(vTradeRetiredNewCap[t1,yp,y]))*pPeriodLen[y]+pTradeStockNew[t1,y];

s.t.  eqTradeStockPhaseOut{(t1, y) in mvTradePhaseOut}: vTradeStockPhaseOut[t1,y]*pPeriodLen[y]  =  (1-pTradeStockSurv[t1,y])*sum{yp in year:(((yp,y) in mMilestoneNext and (t1,yp) in mTradeSpan))}(vTradeStockCap[t1,yp]);

s.t.  eqTradeRetUp{(t1, y) in mTradeRetUp}: sum{fi in FORIF: (t1,y) in mvTradeRetiredStock} (vTradeRetiredStock[t1,y])+sum{yp in year:((t1,yp,y) in mvTradeRetiredNewCap)}(vTradeRetiredNewCap[t1,yp,y]) <=  pTradeRetUp[t1,y]*pPeriodLen[y];

s.t.  eqTradeRetLo{(t1, y) in mTradeRetLo}: sum{fi in FORIF: (t1,y) in mvTradeRetiredStock} (vTradeRetiredStock[t1,y])+sum{yp in year:((t1,yp,y) in mvTradeRetiredNewCap)}(vTradeRetiredNewCap[t1,yp,y])  >=  pTradeRetLo[t1,y]*pPeriodLen[y];

s.t.  eqTradeRetCost{(t1, r, y) in mTradeRetCost}: vTradeRetCost[t1,r,y]  =  pTradeRetCost[t1,r,y]*(sum{fi in FORIF: (t1,y) in mvTradeRetiredStock} (vTradeRetiredStock[t1,y])+sum{yp in year:((t1,yp,y) in mvTradeRetiredNewCap)}(vTradeRetiredNewCap[t1,yp,y]));

# [eac-fix] pTechEac applies to NEW capacity only. Reverted from the simplified
# "pEac*total-capacity" rewrite (which charged annuity on pre-existing stock too)
# back to the legacy vintaged form: sum over still-alive new-capacity vintages.
# OLD: s.t.  eqTechEac{(t, r, y) in mTechSpan}: vTechEac[t,r,y]  =  pTechEac[t,r,y]*vTechCap[t,r,y];
# [payback] The annuity is charged over the COST-RECOVERY period: pTechPayback
# where the user set one, otherwise the operational life as before. Written as a
# boolean disjunction rather than if-then-else, which in MathProg is a numeric
# expression and cannot stand where a logical one is expected. eqTechCap keeps
# pTechOlife -- the technical life still governs when capacity operates.
s.t.  eqTechEac{(t, r, y) in mTechEac}: vTechEac[t,r,y]  =  sum{yp in year:(((t,r,yp) in mTechNew and ordYear[y] >= ordYear[yp] and ((pTechPayback[t,r,yp] > 0 and ordYear[y]<pTechPayback[t,r,yp]+ordYear[yp]) or (pTechPayback[t,r,yp] <= 0 and (ordYear[y]<pTechOlife[t,r]+ordYear[yp] or (t,r) in mTechOlifeInf)))))}(pTechEac[t,r,yp]*(vTechNewCap[t,r,yp]+(-1)*sum{ye in year:(((t,r,yp,ye) in mvTechRetiredNewCap and ordYear[y] >= ordYear[ye]))}(vTechRetiredNewCap[t,r,yp,ye])));

s.t.  eqTechInv{(t, r, y) in mTechInv}: vTechInv[t,r,y]  =  pTechInvcost[t,r,y]*vTechNewCap[t,r,y];

s.t.  eqTechFixom{(t, r, y) in mTechFixom}: vTechFixom[t,r,y]  =  pTechFixom[t,r,y]*vTechCap[t,r,y];

s.t.  eqTechVarom{(t, r, y) in mTechVarom}: vTechVarom[t,r,y]  =  sum{s in timeslice:((t,s) in mTechTimeslice)}(pTechVarom[t,r,y,s]*pTimesliceWeight[y,s]*vTechAct[t,r,y,s]+sum{c in comm:((t,c) in mTechInpComm)}(pTechCvarom[t,c,r,y,s]*pTimesliceWeight[y,s]*vTechInp[t,c,r,y,s])+sum{c in comm:((t,c) in mTechOutComm)}(pTechCvarom[t,c,r,y,s]*pTimesliceWeight[y,s]*vTechOut[t,c,r,y,s])+sum{c in comm:((t,c,r,y,s) in mvTechAOut)}(pTechAvarom[t,c,r,y,s]*pTimesliceWeight[y,s]*vTechAOut[t,c,r,y,s])+sum{c in comm:((t,c,r,y,s) in mvTechAInp)}(pTechAvarom[t,c,r,y,s]*pTimesliceWeight[y,s]*vTechAInp[t,c,r,y,s]));

s.t.  eqSupAvaUp{(s1, c, r, y, s) in mSupAvaUp}: vSupOut[s1,c,r,y,s] <=  pSupAvaUp[s1,c,r,y,s]*(1 + sum{wth1 in weather:((wth1,s1) in mSupWeatherUp)}(pSupWeatherUp[wth1,s1]*pWeather[wth1,r,y,s] - 1));

s.t.  eqSupAvaLo{(s1, c, r, y, s) in meqSupAvaLo}: vSupOut[s1,c,r,y,s]  >=  pSupAvaLo[s1,c,r,y,s]*(1 + sum{wth1 in weather:((wth1,s1) in mSupWeatherLo)}(pSupWeatherLo[wth1,s1]*pWeather[wth1,r,y,s] - 1));

s.t.  eqSupReserve{(s1, c, r) in mvSupReserve}: vSupReserve[s1,c,r]  =  sum{y in year,s in timeslice:((s1,c,r,y,s) in mSupAva)}(pPeriodLen[y]*pTimesliceWeight[y,s]*vSupOut[s1,c,r,y,s]);

s.t.  eqSupReserveUp{(s1, c, r) in mSupReserveUp}: vSupReserve[s1,c,r] <=  pSupReserveUp[s1,c,r];

s.t.  eqSupReserveLo{(s1, c, r) in meqSupReserveLo}: vSupReserve[s1,c,r]  >=  pSupReserveLo[s1,c,r];

s.t.  eqSupCost{(s1, r, y) in mvSupCost}: vSupCost[s1,r,y]  =  sum{c in comm,s in timeslice:((s1,c,r,y,s) in mSupAva)}(pSupCost[s1,c,r,y,s]*pTimesliceWeight[y,s]*vSupOut[s1,c,r,y,s]);

s.t.  eqDemInp{(c, r, y, s) in mvDemInp}: vDemInp[c,r,y,s]  =  sum{d in dem:((d,c) in mDemComm)}(pDemand[d,c,r,y,s]);

s.t.  eqAggOutTot{(c, r, y, s) in mAggOut}: vAggOutTot[c,r,y,s]  =  sum{cp in comm:((c,cp) in mAggregateFactor)}(pAggregateFactor[c,cp]*sum{sp in timeslice:(((c,r,y,sp) in mvOutTot and (s,sp) in mTimesliceParentChildE and (cp,sp) in mCommTimeslice))}(sum{fi in FORIF: (cp,r,y,sp) in mvOutTot} (vOutTot[cp,r,y,sp])));

s.t.  eqEmsFuelTot{(c, r, y, s) in mEmsFuelTot}: vEmsFuelTot[c,r,y,s]  =  sum{cp in comm:((pEmissionFactor[c,cp]>0))}(pEmissionFactor[c,cp]*sum{t in tech:((t,cp) in mTechInpComm)}(pTechEmisComm[t,cp]*sum{sp in timeslice:((c,s,sp) in mCommTimesliceOrParent)}(sum{fi in FORIF: (t,c,cp,r,y,sp) in mTechEmsFuel} (vTechInp[t,cp,r,y,sp]))));

s.t.  eqStorageAInp{(st1, c, r, y, s) in mvStorageAInp}: vStorageAInp[st1,c,r,y,s]  =  sum{cp in comm:((st1,cp) in mStorageStgComm)}(sum{fi in FORIF: (st1,c,r,y,s) in mStorageStg2AInp} ((pStorageStg2AInp[st1,c,r,y,s]*vStorageLevel[st1,cp,r,y,s])))+sum{cp in comm:((st1,cp) in mStorageInpComm)}(sum{fi in FORIF: (st1,c,r,y,s) in mStorageCinp2AInp} ((pStorageCinp2AInp[st1,c,r,y,s]*vStorageInp[st1,cp,r,y,s])))+sum{cp in comm:((st1,cp) in mStorageOutComm)}(sum{fi in FORIF: (st1,c,r,y,s) in mStorageCout2AInp} ((pStorageCout2AInp[st1,c,r,y,s]*vStorageOut[st1,cp,r,y,s])))+sum{fi in FORIF: (st1,c,r,y,s) in mStorageCap2AInp} ((pStorageCap2AInp[st1,c,r,y,s]*vStorageOutCap[st1,r,y]))+sum{fi in FORIF: (st1,c,r,y,s) in mStorageNCap2AInp} ((pStorageNCap2AInp[st1,c,r,y,s]*vStorageOutNewCap[st1,r,y]))+sum{fi in FORIF: (st1,c,r,y,s) in mStoragePho2AInp} ((pStoragePho2AInp[st1,c,r,y,s]*sum{fi in FORIF: (st1,r,y) in mvStoragePhaseOut} (vStoragePhaseOut[st1,r,y])))+sum{fi in FORIF: (st1,c,r,y,s) in mStorageRet2AInp} ((pStorageRet2AInp[st1,c,r,y,s]*(sum{fi in FORIF: (st1,r,y) in mvStorageRetiredStock} (vStorageOutRetiredStock[st1,r,y])+sum{yp in year:((st1,r,yp,y) in mvStorageRetiredNewCap)}(vStorageOutRetiredNewCap[st1,r,yp,y]))));

s.t.  eqStorageAOut{(st1, c, r, y, s) in mvStorageAOut}: vStorageAOut[st1,c,r,y,s]  =  sum{cp in comm:((st1,cp) in mStorageStgComm)}(sum{fi in FORIF: (st1,c,r,y,s) in mStorageStg2AOut} ((pStorageStg2AOut[st1,c,r,y,s]*vStorageLevel[st1,cp,r,y,s])))+sum{cp in comm:((st1,cp) in mStorageInpComm)}(sum{fi in FORIF: (st1,c,r,y,s) in mStorageCinp2AOut} ((pStorageCinp2AOut[st1,c,r,y,s]*vStorageInp[st1,cp,r,y,s])))+sum{cp in comm:((st1,cp) in mStorageOutComm)}(sum{fi in FORIF: (st1,c,r,y,s) in mStorageCout2AOut} ((pStorageCout2AOut[st1,c,r,y,s]*vStorageOut[st1,cp,r,y,s])))+sum{fi in FORIF: (st1,c,r,y,s) in mStorageCap2AOut} ((pStorageCap2AOut[st1,c,r,y,s]*vStorageOutCap[st1,r,y]))+sum{fi in FORIF: (st1,c,r,y,s) in mStorageNCap2AOut} ((pStorageNCap2AOut[st1,c,r,y,s]*vStorageOutNewCap[st1,r,y]))+sum{fi in FORIF: (st1,c,r,y,s) in mStoragePho2AOut} ((pStoragePho2AOut[st1,c,r,y,s]*sum{fi in FORIF: (st1,r,y) in mvStoragePhaseOut} (vStoragePhaseOut[st1,r,y])))+sum{fi in FORIF: (st1,c,r,y,s) in mStorageRet2AOut} ((pStorageRet2AOut[st1,c,r,y,s]*(sum{fi in FORIF: (st1,r,y) in mvStorageRetiredStock} (vStorageOutRetiredStock[st1,r,y])+sum{yp in year:((st1,r,yp,y) in mvStorageRetiredNewCap)}(vStorageOutRetiredNewCap[st1,r,yp,y]))));

s.t.  eqStorageLevel{(st1, c, r, y, sp, s) in meqStorageLevel}: vStorageLevel[st1,c,r,y,s]  =  pStorageStartLevel[st1,c,r,y,s]+sum{fi in FORIF: (st1,r,y) in mStorageNew} ((pStorageNCap2Stg[st1,c,r,y,s]*vStorageOutNewCap[st1,r,y]))+sum{ci in comm:((st1,ci,r,y,sp) in mvStorageInp)}(pStorageInpEff[st1,ci,r,y,sp]*vStorageInp[st1,ci,r,y,sp])+((pStorageStgEff[st1,c,r,y,s])^(pTimesliceShare[s]))*vStorageLevel[st1,c,r,y,sp]-sum{co in comm:((st1,co,r,y,sp) in mvStorageOut)}((vStorageOut[st1,co,r,y,sp]) / (pStorageOutEff[st1,co,r,y,sp]));

# [duration] now a BOUND on (stg, region, year): AfLo takes the LOWER ratio,
# AfUp the UPPER. defVal [1,1] keeps a storage that says nothing tied at 1 h.
s.t.  eqStorageAfLo{(st1, c, r, y, s) in meqStorageAfLo}: vStorageLevel[st1,c,r,y,s]  >=  pStorageAfLo[st1,r,y,s]*(sum{fi in FORIF: (st1,r,y) in mStorageStgCap} (vStorageStgCap[st1,r,y])+sum{fi in FORIF: (st1,r,y) in mStorageNoStgCap} (pStorageDurationLo[st1,r,y]*vStorageOutCap[st1,r,y]))*(1 + sum{wth1 in weather:((wth1,st1) in mStorageWeatherAfLo)}(pStorageWeatherAfLo[wth1,st1]*pWeather[wth1,r,y,s] - 1));

s.t.  eqStorageAfUp{(st1, c, r, y, s) in meqStorageAfUp}: vStorageLevel[st1,c,r,y,s] <=  pStorageAfUp[st1,r,y,s]*(sum{fi in FORIF: (st1,r,y) in mStorageStgCap} (vStorageStgCap[st1,r,y])+sum{fi in FORIF: (st1,r,y) in mStorageNoStgCap} (pStorageDurationUp[st1,r,y]*vStorageOutCap[st1,r,y]))*(1 + sum{wth1 in weather:((wth1,st1) in mStorageWeatherAfUp)}(pStorageWeatherAfUp[wth1,st1]*pWeather[wth1,r,y,s] - 1));

# [rename] eqStorageClear -> eqStorageOutLevel. Draw only from what was ALREADY
# stored: stricter than the level's non-negativity, which would also permit
# discharging energy charged in the SAME timeslice. No Lo/Up suffix -- that marks
# a paired bound, and this one has no partner (cf. eqTradeCapFlow).
s.t.  eqStorageOutLevel{(st1, c, r, y, s) in mvStorageLevel}: sum{co in comm:((st1,co,r,y,s) in mvStorageOut)}((vStorageOut[st1,co,r,y,s]) / (pStorageOutEff[st1,co,r,y,s])) <=  vStorageLevel[st1,c,r,y,s];

s.t.  eqStorageInpUp{(st1, c, r, y, s) in meqStorageInpUp}: vStorageInp[st1,c,r,y,s] <=  (sum{fi in FORIF: (st1,r,y) in mStorageInpCap} (vStorageInpCap[st1,r,y])+sum{fi in FORIF: (st1,r,y) in mStorageNoInpCap} (pStorageInp2outUp[st1,r,y]*vStorageOutCap[st1,r,y]))*pStorageInpCap2act[st1]*pTimesliceShare[s]*pStorageCinpUp[st1,c,r,y,s]*(1 + sum{wth1 in weather:((wth1,st1) in mStorageWeatherCinpUp)}(pStorageWeatherCinpUp[wth1,st1]*pWeather[wth1,r,y,s] - 1));

s.t.  eqStorageInpLo{(st1, c, r, y, s) in meqStorageInpLo}: vStorageInp[st1,c,r,y,s]  >=  (sum{fi in FORIF: (st1,r,y) in mStorageInpCap} (vStorageInpCap[st1,r,y])+sum{fi in FORIF: (st1,r,y) in mStorageNoInpCap} (pStorageInp2outLo[st1,r,y]*vStorageOutCap[st1,r,y]))*pStorageInpCap2act[st1]*pTimesliceShare[s]*pStorageCinpLo[st1,c,r,y,s]*(1 + sum{wth1 in weather:((wth1,st1) in mStorageWeatherCinpLo)}(pStorageWeatherCinpLo[wth1,st1]*pWeather[wth1,r,y,s] - 1));

s.t.  eqStorageOutUp{(st1, c, r, y, s) in meqStorageOutUp}: vStorageOut[st1,c,r,y,s] <=  vStorageOutCap[st1,r,y]*pStorageOutCap2act[st1]*pTimesliceShare[s]*pStorageCoutUp[st1,c,r,y,s]*(1 + sum{wth1 in weather:((wth1,st1) in mStorageWeatherCoutUp)}(pStorageWeatherCoutUp[wth1,st1]*pWeather[wth1,r,y,s] - 1));

s.t.  eqStorageOutLo{(st1, c, r, y, s) in meqStorageOutLo}: vStorageOut[st1,c,r,y,s]  >=  vStorageOutCap[st1,r,y]*pStorageOutCap2act[st1]*pTimesliceShare[s]*pStorageCoutLo[st1,c,r,y,s]*(1 + sum{wth1 in weather:((wth1,st1) in mStorageWeatherCoutLo)}(pStorageWeatherCoutLo[wth1,st1]*pWeather[wth1,r,y,s] - 1));

s.t.  eqStorageOutCap{(st1, r, y) in mStorageSpan}: vStorageOutCap[st1,r,y]  =  vStorageOutStockCap[st1,r,y]+sum{yp in year:((ordYear[y] >= ordYear[yp] and ((st1,r) in mStorageOlifeInf or ordYear[y]<pStorageOlife[st1,r]+ordYear[yp]) and (st1,r,yp) in mStorageNew))}(pPeriodLen[yp]*vStorageOutNewCap[st1,r,yp]+(-1)*sum{ye in year:(((st1,r,yp,ye) in mvStorageRetiredNewCap and ordYear[y] >= ordYear[ye]))}(vStorageOutRetiredNewCap[st1,r,yp,ye]*pPeriodLen[ye]));

# [2c] The STORING side's own capacity, in ENERGY. It exists only where the
# storing part carries data (mStorageStgCap); everywhere else the af bounds above
# use `duration * vStorageOutCap` and this block emits nothing -- which is what
# keeps a storage written the old way at its previous row and column count.
# [2c] The CHARGING side's own capacity, in power, rated independently of the
# discharger. Exists only where the charging part carries data; elsewhere the
# input bounds inline `inp2out * vStorageOutCap`, which at the default of 1 is
# the previous model.
s.t.  eqStorageInpCap{(st1, r, y) in mStorageInpCap}: vStorageInpCap[st1,r,y]  =  vStorageInpStockCap[st1,r,y]+sum{yp in year:((ordYear[y] >= ordYear[yp] and ((st1,r) in mStorageOlifeInf or ordYear[y]<pStorageOlife[st1,r]+ordYear[yp]) and (st1,r,yp) in mStorageInpNew))}(pPeriodLen[yp]*vStorageInpNewCap[st1,r,yp]+(-1)*sum{ye in year:(((st1,r,yp,ye) in mvStorageRetiredNewCap and ordYear[y] >= ordYear[ye]))}(vStorageInpRetiredNewCap[st1,r,yp,ye]*pPeriodLen[ye]));

s.t.  eqStorageInpCapLo{(st1, r, y) in mStorageInpCapLo}: vStorageInpCap[st1,r,y]  >=  pStorageInpCapLo[st1,r,y];

s.t.  eqStorageInpCapUp{(st1, r, y) in mStorageInpCapUp}: vStorageInpCap[st1,r,y] <=  pStorageInpCapUp[st1,r,y];

s.t.  eqStorageInpNewCapLo{(st1, r, y) in mStorageInpNewCapLo}: vStorageInpNewCap[st1,r,y]  >=  pStorageInpNewCapLo[st1,r,y]*pPeriodLen[y];

s.t.  eqStorageInpNewCapUp{(st1, r, y) in mStorageInpNewCapUp}: vStorageInpNewCap[st1,r,y] <=  pStorageInpNewCapUp[st1,r,y]*pPeriodLen[y];

# The inp2out LINK: charging capacity per unit of discharging capacity.
s.t.  eqStorageInp2outLo{(st1, r, y) in mStorageInp2outLo}: vStorageInpCap[st1,r,y]  >=  pStorageInp2outLo[st1,r,y]*vStorageOutCap[st1,r,y];

s.t.  eqStorageInp2outUp{(st1, r, y) in mStorageInp2outUp}: vStorageInpCap[st1,r,y] <=  pStorageInp2outUp[st1,r,y]*vStorageOutCap[st1,r,y];

s.t.  eqStorageStgCap{(st1, r, y) in mStorageStgCap}: vStorageStgCap[st1,r,y]  =  vStorageStgStockCap[st1,r,y]+sum{yp in year:((ordYear[y] >= ordYear[yp] and ((st1,r) in mStorageOlifeInf or ordYear[y]<pStorageOlife[st1,r]+ordYear[yp]) and (st1,r,yp) in mStorageStgNew))}(pPeriodLen[yp]*vStorageStgNewCap[st1,r,yp]+(-1)*sum{ye in year:(((st1,r,yp,ye) in mvStorageRetiredNewCap and ordYear[y] >= ordYear[ye]))}(vStorageStgRetiredNewCap[st1,r,yp,ye]*pPeriodLen[ye]));

s.t.  eqStorageStgCapLo{(st1, r, y) in mStorageStgCapLo}: vStorageStgCap[st1,r,y]  >=  pStorageStgCapLo[st1,r,y];

s.t.  eqStorageStgCapUp{(st1, r, y) in mStorageStgCapUp}: vStorageStgCap[st1,r,y] <=  pStorageStgCapUp[st1,r,y];

s.t.  eqStorageStgNewCapLo{(st1, r, y) in mStorageStgNewCapLo}: vStorageStgNewCap[st1,r,y]  >=  pStorageStgNewCapLo[st1,r,y]*pPeriodLen[y];

s.t.  eqStorageStgNewCapUp{(st1, r, y) in mStorageStgNewCapUp}: vStorageStgNewCap[st1,r,y] <=  pStorageStgNewCapUp[st1,r,y]*pPeriodLen[y];

# The duration LINK, in hours: energy capacity per unit of output (power)
# capacity. `.fx` collapses these two rows onto each other and pins the ratio;
# `.lo`/`.up` let the model choose it. Emitted only where vStorageStgCap exists.
s.t.  eqStorageDurationLo{(st1, r, y) in mStorageDurationLo}: vStorageStgCap[st1,r,y]  >=  pStorageDurationLo[st1,r,y]*vStorageOutCap[st1,r,y];

s.t.  eqStorageDurationUp{(st1, r, y) in mStorageDurationUp}: vStorageStgCap[st1,r,y] <=  pStorageDurationUp[st1,r,y]*vStorageOutCap[st1,r,y];

s.t.  eqStorageOutCapLo{(st1, r, y) in mStorageOutCapLo}: vStorageOutCap[st1,r,y]  >=  pStorageOutCapLo[st1,r,y];

s.t.  eqStorageOutCapUp{(st1, r, y) in mStorageOutCapUp}: vStorageOutCap[st1,r,y] <=  pStorageOutCapUp[st1,r,y];

s.t.  eqStorageOutNewCapLo{(st1, r, y) in mStorageOutNewCapLo}: vStorageOutNewCap[st1,r,y]  >=  pStorageOutNewCapLo[st1,r,y]*pPeriodLen[y];

s.t.  eqStorageOutNewCapUp{(st1, r, y) in mStorageOutNewCapUp}: vStorageOutNewCap[st1,r,y] <=  pStorageOutNewCapUp[st1,r,y]*pPeriodLen[y];

s.t.  eqStorageInv{(st1, r, y) in mStorageNew}: vStorageInv[st1,r,y]  =  pStorageOutInvcost[st1,r,y]*vStorageOutNewCap[st1,r,y]+sum{fi in FORIF: (st1,r,y) in mStorageStgNew} (pStorageStgInvcost[st1,r,y]*vStorageStgNewCap[st1,r,y])+sum{fi in FORIF: (st1,r,y) in mStorageInpNew} (pStorageInpInvcost[st1,r,y]*vStorageInpNewCap[st1,r,y]);

# [eac-fix] reverted to legacy vintaged new-capacity form (see eqTechEac).
# OLD: s.t.  eqStorageEac{(st1, r, y) in mStorageEac}: vStorageEac[st1,r,y]  =  pStorageOutEac[st1,r,y]*vStorageOutCap[st1,r,y];
# [payback] see eqTechEac.
# [eac-fix] the `pStorageOutInvcost <> 0` guard was REMOVED from the summation
# condition below. It dropped the annuity entirely when a user supplied
# `@invcost$eac` without `invcost` (pre-annuitised capex, e.g. a PyPSA import),
# so the storage was built for FREE -- silently, with an OPTIMAL solve.
# eqTechEac / eqTradeEac never carried it. pStorageOutEac defaults to 0, so a
# vintage with no capital cost now contributes a zero-coefficient term instead
# of being dropped from the sum.
s.t.  eqStorageEac{(st1, r, y) in mStorageEac}: vStorageEac[st1,r,y]  =  sum{yp in year:(((st1,r,yp) in mStorageNew and ordYear[y] >= ordYear[yp] and ((pStorageOutPayback[st1,r,yp] > 0 and ordYear[y]<pStorageOutPayback[st1,r,yp]+ordYear[yp]) or (pStorageOutPayback[st1,r,yp] <= 0 and ((st1,r) in mStorageOlifeInf or ordYear[y]<pStorageOlife[st1,r]+ordYear[yp])))))}(pStorageOutEac[st1,r,yp]*vStorageOutNewCap[st1,r,yp]+sum{fi in FORIF: (st1,r,yp) in mStorageStgNew} (pStorageStgEac[st1,r,yp]*vStorageStgNewCap[st1,r,yp])+sum{fi in FORIF: (st1,r,yp) in mStorageInpNew} (pStorageInpEac[st1,r,yp]*vStorageInpNewCap[st1,r,yp]));

s.t.  eqStorageFixom{(st1, r, y) in mStorageFixom}: vStorageFixom[st1,r,y]  =  pStorageOutFixom[st1,r,y]*vStorageOutCap[st1,r,y]+sum{fi in FORIF: (st1,r,y) in mStorageStgFixom} (pStorageStgFixom[st1,r,y]*vStorageStgCap[st1,r,y])+sum{fi in FORIF: (st1,r,y) in mStorageInpFixom} (pStorageInpFixom[st1,r,y]*vStorageInpCap[st1,r,y]);

s.t.  eqStorageVarom{(st1, r, y) in mStorageVarom}: vStorageVarom[st1,r,y]  =  sum{c in comm:((st1,c) in mStorageInpComm)}(sum{s in timeslice:((c,s) in mCommTimeslice)}(pStorageCostInp[st1,r,y,s]*pTimesliceWeight[y,s]*vStorageInp[st1,c,r,y,s]))+sum{c in comm:((st1,c) in mStorageOutComm)}(sum{s in timeslice:((c,s) in mCommTimeslice)}(pStorageCostOut[st1,r,y,s]*pTimesliceWeight[y,s]*vStorageOut[st1,c,r,y,s]))+sum{c in comm:((st1,c) in mStorageStgComm)}(sum{s in timeslice:((c,s) in mCommTimeslice)}(pStorageCostStore[st1,r,y,s]*pTimesliceWeight[y,s]*vStorageLevel[st1,c,r,y,s]));

s.t.  eqImportTot{(c, dst, y, s) in mImport}: vImportTot[c,dst,y,s]  =  sum{t1 in trade:((t1,c) in mTradeComm)}(sum{src in region:((t1,src,dst) in mTradeRoutes)}(sum{fi in FORIF: (t1,c,src,dst,y,s) in mvTradeIr} ((pTradeIrEff[t1,src,dst,y,s]*vTradeIr[t1,c,src,dst,y,s]))))+sum{i in imp:((i,c) in mImpComm)}(sum{fi in FORIF: (i,c,dst,y,s) in mImportRow} (vImportRow[i,c,dst,y,s]));

s.t.  eqExportTot{(c, src, y, s) in mExport}: vExportTot[c,src,y,s]  =  sum{t1 in trade:((t1,c) in mTradeComm)}(sum{dst in region:((t1,src,dst) in mTradeRoutes)}(sum{fi in FORIF: (t1,c,src,dst,y,s) in mvTradeIr} (vTradeIr[t1,c,src,dst,y,s])))+sum{e in expp:((e,c) in mExpComm)}(sum{fi in FORIF: (e,c,src,y,s) in mExportRow} (vExportRow[e,c,src,y,s]));

s.t.  eqTradeFlowUp{(t1, c, src, dst, y, s) in meqTradeFlowUp}: vTradeIr[t1,c,src,dst,y,s] <=  pTradeIrUp[t1,src,dst,y,s];

s.t.  eqTradeFlowLo{(t1, c, src, dst, y, s) in meqTradeFlowLo}: vTradeIr[t1,c,src,dst,y,s]  >=  pTradeIrLo[t1,src,dst,y,s];

# Relative flow bounds: the same limit as above, but as a fraction of the
# object's own capacity rather than an absolute quantity, so one trade object
# carrying several routes can rate each of them. Gated, hence empty unless
# `af` is declared.
s.t.  eqTradeIrAfUp{(t1, c, src, dst, y, s) in meqTradeIrAfUp}: vTradeIr[t1,c,src,dst,y,s] <=  pTradeIrAfUp[t1,src,dst,y,s]*pTradeCap2Act[t1]*vTradeCap[t1,y]*pTimesliceShare[s];

s.t.  eqTradeIrAfLo{(t1, c, src, dst, y, s) in meqTradeIrAfLo}: vTradeIr[t1,c,src,dst,y,s]  >=  pTradeIrAfLo[t1,src,dst,y,s]*pTradeCap2Act[t1]*vTradeCap[t1,y]*pTimesliceShare[s];

s.t.  eqImportIrCost{(t1, r, y) in mImportIrCost}: vImportIrCost[t1,r,y]  =  sum{src in region:((t1,src,r) in mTradeRoutes)}(sum{c in comm:((t1,c) in mTradeComm)}(sum{s in timeslice:((t1,s) in mTradeTimeslice)}(sum{fi in FORIF: (t1,c,src,r,y,s) in mvTradeIr} (((pTradeIrMarkup[t1,src,r,y,s])*vTradeIr[t1,c,src,r,y,s]*pTimesliceWeight[y,s])))));

s.t.  eqExportIrCost{(t1, r, y) in mExportIrCost}: vExportIrCost[t1,r,y]  =  sum{dst in region:((t1,r,dst) in mTradeRoutes)}(sum{c in comm:((t1,c) in mTradeComm)}(sum{s in timeslice:((t1,s) in mTradeTimeslice)}(sum{fi in FORIF: (t1,c,r,dst,y,s) in mvTradeIr} (((pTradeIrCost[t1,r,dst,y,s]-pTradeIrMarkup[t1,r,dst,y,s])*vTradeIr[t1,c,r,dst,y,s]*pTimesliceWeight[y,s])))));

s.t.  eqExportRowUp{(e, c, r, y, s) in mExportRowUp}: vExportRow[e,c,r,y,s] <=  pExportRowUp[e,r,y,s];

s.t.  eqExportRowLo{(e, c, r, y, s) in meqExportRowLo}: vExportRow[e,c,r,y,s]  >=  pExportRowLo[e,r,y,s];

s.t.  eqExportRowCum{(e, c) in mExpComm}: vExportRowCum[e,c]  =  sum{r in region,y in year,s in timeslice:((e,c,r,y,s) in mExportRow)}(pPeriodLen[y]*pTimesliceWeight[y,s]*vExportRow[e,c,r,y,s]);

s.t.  eqExportRowResUp{(e, c) in mExportRowCumUp}: vExportRowCum[e,c] <=  pExportRowRes[e];

s.t.  eqExportRowCost{(e, r, y) in mExportRowCost}: vExportRowCost[e,r,y]  =  -sum{c in comm,s in timeslice:((e,c,r,y,s) in mExportRow)}(pExportRowPrice[e,r,y,s]*pTimesliceWeight[y,s]*vExportRow[e,c,r,y,s]);

s.t.  eqImportRowUp{(i, c, r, y, s) in mImportRowUp}: vImportRow[i,c,r,y,s] <=  pImportRowUp[i,r,y,s];

s.t.  eqImportRowLo{(i, c, r, y, s) in meqImportRowLo}: vImportRow[i,c,r,y,s]  >=  pImportRowLo[i,r,y,s];

s.t.  eqImportRowCum{(i, c) in mImpComm}: vImportRowCum[i,c]  =  sum{r in region,y in year,s in timeslice:((i,c,r,y,s) in mImportRow)}(pPeriodLen[y]*pTimesliceWeight[y,s]*vImportRow[i,c,r,y,s]);

s.t.  eqImportRowResUp{(i, c) in mImportRowCumUp}: vImportRowCum[i,c] <=  pImportRowRes[i];

s.t.  eqImportRowCost{(i, r, y) in mImportRowCost}: vImportRowCost[i,r,y]  =  sum{c in comm,s in timeslice:((i,c,r,y,s) in mImportRow)}(pImportRowPrice[i,r,y,s]*pTimesliceWeight[y,s]*vImportRow[i,c,r,y,s]);

s.t.  eqTradeCapFlow{(t1, c, y, s) in meqTradeCapFlow}: pTimesliceShare[s]*pTradeCap2Act[t1]*vTradeCap[t1,y]  >=  sum{src in region,dst in region:((t1,c,src,dst,y,s) in mvTradeIr)}(vTradeIr[t1,c,src,dst,y,s]);

s.t.  eqTradeCap{(t1, y) in mTradeSpan}: vTradeCap[t1,y]  =  vTradeStockCap[t1,y]+sum{yp in year:(((t1,yp) in mTradeNew and ordYear[y] >= ordYear[yp] and (ordYear[y]<pTradeOlife[t1]+ordYear[yp] or (sum{t1x in mTradeOlifeInf : t1x = t1} 1) > 0)))}(pPeriodLen[yp]*vTradeNewCap[t1,yp]+(-1)*sum{ye in year:(((t1,yp,ye) in mvTradeRetiredNewCap and ordYear[y] >= ordYear[ye]))}(vTradeRetiredNewCap[t1,yp,ye]*pPeriodLen[ye]));

s.t.  eqTradeCapLo{(t1, y) in mTradeCapLo}: vTradeCap[t1,y]  >=  pTradeCapLo[t1,y];

s.t.  eqTradeCapUp{(t1, y) in mTradeCapUp}: vTradeCap[t1,y] <=  pTradeCapUp[t1,y];

s.t.  eqTradeNewCapLo{(t1, y) in mTradeNewCapLo}: vTradeNewCap[t1,y]*pPeriodLen[y]  >=  pTradeNewCapLo[t1,y];

s.t.  eqTradeNewCapUp{(t1, y) in mTradeNewCapUp}: vTradeNewCap[t1,y]*pPeriodLen[y] <=  pTradeNewCapUp[t1,y];

s.t.  eqTradeInv{(t1, r, y) in mTradeInv}: vTradeInv[t1,r,y]  =  pTradeInvcost[t1,r,y]*vTradeNewCap[t1,y];

# [eac-fix] reverted to legacy vintaged new-capacity form (see eqTechEac).
# OLD: s.t.  eqTradeEac{(t1, r, y) in mTradeEac}: vTradeEac[t1,r,y]  =  pTradeEac[t1,r,y]*vTradeCap[t1,y];
# [payback] see eqTechEac. pTradePayback is region-indexed while vTradeNewCap is
# not, so each region amortises its own share of one corridor on its own schedule.
s.t.  eqTradeEac{(t1, r, y) in mTradeEac}: vTradeEac[t1,r,y]  =  sum{yp in year:(((t1,yp) in mTradeNew and ordYear[y] >= ordYear[yp] and ((pTradePayback[t1,r,yp] > 0 and ordYear[y]<pTradePayback[t1,r,yp]+ordYear[yp]) or (pTradePayback[t1,r,yp] <= 0 and (ordYear[y]<pTradeOlife[t1]+ordYear[yp] or (sum{t1x in mTradeOlifeInf : t1x = t1} 1) > 0)))))}(pTradeEac[t1,r,yp]*vTradeNewCap[t1,yp]);

s.t.  eqTradeFixom{(t1, r, y) in mTradeFixom}: vTradeFixom[t1,r,y]  =  pTradeFixom[t1,r,y]*vTradeCap[t1,y];

s.t.  eqTradeIrAInp{(t1, c, r, y, s) in mvTradeIrAInp}: vTradeIrAInp[t1,c,r,y,s]  =  sum{dst in region:((t1,c,r,dst,y,s) in mTradeIrCsrc2Ainp)}(pTradeIrCsrc2Ainp[t1,c,r,dst,y,s]*sum{cp in comm:(((t1,cp) in mTradeComm and (t1,cp,r,dst,y,s) in mvTradeIr))}(vTradeIr[t1,cp,r,dst,y,s]))+sum{src in region:((t1,c,src,r,y,s) in mTradeIrCdst2Ainp)}(pTradeIrCdst2Ainp[t1,c,src,r,y,s]*sum{cp in comm:(((t1,cp) in mTradeComm and (t1,cp,src,r,y,s) in mvTradeIr))}(vTradeIr[t1,cp,src,r,y,s]));

s.t.  eqTradeIrAOut{(t1, c, r, y, s) in mvTradeIrAOut}: vTradeIrAOut[t1,c,r,y,s]  =  sum{dst in region:((t1,c,r,dst,y,s) in mTradeIrCsrc2Aout)}(pTradeIrCsrc2Aout[t1,c,r,dst,y,s]*sum{cp in comm:(((t1,cp) in mTradeComm and (t1,cp,r,dst,y,s) in mvTradeIr))}(vTradeIr[t1,cp,r,dst,y,s]))+sum{src in region:((t1,c,src,r,y,s) in mTradeIrCdst2Aout)}(pTradeIrCdst2Aout[t1,c,src,r,y,s]*sum{cp in comm:(((t1,cp) in mTradeComm and (t1,cp,src,r,y,s) in mvTradeIr))}(vTradeIr[t1,cp,src,r,y,s]));

s.t.  eqTradeIrAInpTot{(c, r, y, s) in mvTradeIrAInpTot}: vTradeIrAInpTot[c,r,y,s]  =  sum{t1 in trade,sp in timeslice:(((c,s,sp) in mCommTimesliceOrParent and (t1,c,r,y,sp) in mvTradeIrAInp))}(vTradeIrAInp[t1,c,r,y,sp]);

s.t.  eqTradeIrAOutTot{(c, r, y, s) in mvTradeIrAOutTot}: vTradeIrAOutTot[c,r,y,s]  =  sum{t1 in trade,sp in timeslice:(((c,s,sp) in mCommTimesliceOrParent and (t1,c,r,y,sp) in mvTradeIrAOut))}(vTradeIrAOut[t1,c,r,y,sp]);

s.t.  eqBalLo{(c, r, y, s) in meqBalLo}: vBalance[c,r,y,s]  >=  0;

s.t.  eqBalUp{(c, r, y, s) in meqBalUp}: vBalance[c,r,y,s] <=  0;

s.t.  eqBalFx{(c, r, y, s) in meqBalFx}: vBalance[c,r,y,s]  =  0;

s.t.  eqBal{(c, r, y, s) in mvBalance}: vBalance[c,r,y,s]  =  sum{fi in FORIF: (c,r,y,s) in mvOutTot} (vOutTot[c,r,y,s])-sum{fi in FORIF: (c,r,y,s) in mvInpTot} (vInpTot[c,r,y,s]);

# [agg-rewrite] eqBalanceRY/vBalanceRY retired (dead reporting: weighted timeslice-sum, unused)

# [agg-rewrite] eqOutTot: process totals native to timeslice s + UP-aggregation of the
# immediately-finer children's totals (pTimesliceAgg renormalizes the intensive values).
# The final term replaces the old mOutSub/vOut2Lo down-disaggregation collector.
# OLD:
# s.t.  eqOutTot{(c, r, y, s) in mvOutTot}: vOutTot[c,r,y,s]  =  sum{fi in FORIF: (c,r,y,s) in mDummyImport} (vDummyImport[c,r,y,s])+sum{fi in FORIF: (c,r,y,s) in mSupOutTot} (vSupOutTot[c,r,y,s])+sum{fi in FORIF: (c,r,y,s) in mEmsFuelTot} (vEmsFuelTot[c,r,y,s])+sum{fi in FORIF: (c,r,y,s) in mAggOut} (vAggOutTot[c,r,y,s])+sum{fi in FORIF: (c,r,y,s) in mTechOutTot} (vTechOutTot[c,r,y,s])+sum{fi in FORIF: (c,r,y,s) in mStorageOutTot} (vStorageOutTot[c,r,y,s])+sum{fi in FORIF: (c,r,y,s) in mImport} (vImportTot[c,r,y,s])+sum{fi in FORIF: (c,r,y,s) in mvTradeIrAOutTot} (vTradeIrAOutTot[c,r,y,s])+sum{fi in FORIF: (c,r,y,s) in mOutSub} (sum{sp in timeslice:(((sp,s) in mTimesliceParentChild and (c,r,y,sp,s) in mvOut2Lo))}(vOut2Lo[c,r,y,sp,s]));
s.t.  eqOutTot{(c, r, y, s) in mvOutTot}: vOutTot[c,r,y,s]  =  sum{fi in FORIF: (c,r,y,s) in mDummyImport} (vDummyImport[c,r,y,s])+sum{fi in FORIF: (c,r,y,s) in mSupOutTot} (vSupOutTot[c,r,y,s])+sum{fi in FORIF: (c,r,y,s) in mEmsFuelTot} (vEmsFuelTot[c,r,y,s])+sum{fi in FORIF: (c,r,y,s) in mAggOut} (vAggOutTot[c,r,y,s])+sum{fi in FORIF: (c,r,y,s) in mTechOutTot} (vTechOutTot[c,r,y,s])+sum{fi in FORIF: (c,r,y,s) in mStorageOutTot} (vStorageOutTot[c,r,y,s])+sum{fi in FORIF: (c,r,y,s) in mImport} (vImportTot[c,r,y,s])+sum{fi in FORIF: (c,r,y,s) in mvTradeIrAOutTot} (vTradeIrAOutTot[c,r,y,s])+sum{sp in timeslice:((s,sp) in mTimesliceFamily and (c,r,y,sp) in mvOutTot)}(pTimesliceAgg[y,s,sp]*vOutTot[c,r,y,sp])+sum{rp in region:((r,rp) in mRegionFamily and (c,rp,y,s) in mvOutTot)}(vOutTot[c,rp,y,s]);

# [agg-rewrite] eqOutTotRY/vOutTotRY retired (dead reporting)

# [agg-rewrite] eqOut2Lo removed: down-disaggregation of coarse output is replaced by
# up-aggregation in eqOutTot. Coarse output now lands in vOutTot at its own balance level.
# s.t.  eqOut2Lo{(c, r, y, s) in mOut2Lo}: sum{sp in timeslice:((c,r,y,s,sp) in mvOut2Lo)}(vOut2Lo[c,r,y,s,sp])  =  sum{fi in FORIF: (c,r,y,s) in mSupOutTot} (vSupOutTot[c,r,y,s])+sum{fi in FORIF: (c,r,y,s) in mEmsFuelTot} (vEmsFuelTot[c,r,y,s])+sum{fi in FORIF: (c,r,y,s) in mAggOut} (vAggOutTot[c,r,y,s])+sum{fi in FORIF: (c,r,y,s) in mTechOutTot} (vTechOutTot[c,r,y,s])+sum{fi in FORIF: (c,r,y,s) in mStorageOutTot} (vStorageOutTot[c,r,y,s])+sum{fi in FORIF: (c,r,y,s) in mImport} (vImportTot[c,r,y,s])+sum{fi in FORIF: (c,r,y,s) in mvTradeIrAOutTot} (vTradeIrAOutTot[c,r,y,s]);

# [agg-rewrite] eqInpTot: process totals native to timeslice s + UP-aggregation of the
# immediately-finer children's totals. Final term replaces old mInpSub/vInp2Lo collector.
# OLD:
# s.t.  eqInpTot{(c, r, y, s) in mvInpTot}: vInpTot[c,r,y,s]  =  sum{fi in FORIF: (c,r,y,s) in mvDemInp} (vDemInp[c,r,y,s])+sum{fi in FORIF: (c,r,y,s) in mDummyExport} (vDummyExport[c,r,y,s])+sum{fi in FORIF: (c,r,y,s) in mTechInpTot} (vTechInpTot[c,r,y,s])+sum{fi in FORIF: (c,r,y,s) in mStorageInpTot} (vStorageInpTot[c,r,y,s])+sum{fi in FORIF: (c,r,y,s) in mExport} (vExportTot[c,r,y,s])+sum{fi in FORIF: (c,r,y,s) in mvTradeIrAInpTot} (vTradeIrAInpTot[c,r,y,s])+sum{fi in FORIF: (c,r,y,s) in mInpSub} (sum{sp in timeslice:(((sp,s) in mTimesliceParentChild and (c,r,y,sp,s) in mvInp2Lo))}(vInp2Lo[c,r,y,sp,s]));
s.t.  eqInpTot{(c, r, y, s) in mvInpTot}: vInpTot[c,r,y,s]  =  sum{fi in FORIF: (c,r,y,s) in mvDemInp} (vDemInp[c,r,y,s])+sum{fi in FORIF: (c,r,y,s) in mDummyExport} (vDummyExport[c,r,y,s])+sum{fi in FORIF: (c,r,y,s) in mTechInpTot} (vTechInpTot[c,r,y,s])+sum{fi in FORIF: (c,r,y,s) in mStorageInpTot} (vStorageInpTot[c,r,y,s])+sum{fi in FORIF: (c,r,y,s) in mExport} (vExportTot[c,r,y,s])+sum{fi in FORIF: (c,r,y,s) in mvTradeIrAInpTot} (vTradeIrAInpTot[c,r,y,s])+sum{sp in timeslice:((s,sp) in mTimesliceFamily and (c,r,y,sp) in mvInpTot)}(pTimesliceAgg[y,s,sp]*vInpTot[c,r,y,sp])+sum{rp in region:((r,rp) in mRegionFamily and (c,rp,y,s) in mvInpTot)}(vInpTot[c,rp,y,s]);

# [agg-rewrite] eqInpTotRY/vInpTotRY retired (dead reporting)

# [agg-rewrite] eqInp2Lo removed: down-disaggregation of coarse input is replaced by
# up-aggregation in eqInpTot. Coarse input now lands in vInpTot at its own balance level.
# s.t.  eqInp2Lo{(c, r, y, s) in mInp2Lo}: sum{sp in timeslice:((c,r,y,s,sp) in mvInp2Lo)}(vInp2Lo[c,r,y,s,sp])  =  sum{fi in FORIF: (c,r,y,s) in mTechInpTot} (vTechInpTot[c,r,y,s])+sum{fi in FORIF: (c,r,y,s) in mStorageInpTot} (vStorageInpTot[c,r,y,s])+sum{fi in FORIF: (c,r,y,s) in mExport} (vExportTot[c,r,y,s])+sum{fi in FORIF: (c,r,y,s) in mvTradeIrAInpTot} (vTradeIrAInpTot[c,r,y,s]);

s.t.  eqSupOutTot{(c, r, y, s) in mSupOutTot}: vSupOutTot[c,r,y,s]  =  sum{s1 in sup:(((s1,c) in mSupComm and (s1,c,r,y,s) in mSupAva))}(vSupOut[s1,c,r,y,s]);

s.t.  eqTechInpTot{(c, r, y, s) in mTechInpTot}: vTechInpTot[c,r,y,s]  =  sum{t in tech:((t,c) in mTechInpCommSameTimeslice)}(sum{fi in FORIF: (t,c,r,y,s) in mvTechInp} (vTechInp[t,c,r,y,s]))+sum{t in tech:((t,c) in mTechInpCommAgg)}(sum{sp in timeslice:((t,c,sp,s) in mTechInpCommAggTimeslice)}(sum{fi in FORIF: (t,c,r,y,sp) in mvTechInp} (vTechInp[t,c,r,y,sp])))+sum{t in tech:((t,c) in mTechAInpCommSameTimeslice)}(sum{fi in FORIF: (t,c,r,y,s) in mvTechAInp} (vTechAInp[t,c,r,y,s]))+sum{t in tech:((t,c) in mTechAInpCommAgg)}(sum{sp in timeslice:((t,c,sp,s) in mTechAInpCommAggTimeslice)}(sum{fi in FORIF: (t,c,r,y,sp) in mvTechAInp} (vTechAInp[t,c,r,y,sp])));

s.t.  eqTechOutTot{(c, r, y, s) in mTechOutTot}: vTechOutTot[c,r,y,s]  =  sum{t in tech:((t,c) in mTechOutCommSameTimeslice)}(sum{fi in FORIF: (t,c,r,y,s) in mvTechOut} (vTechOut[t,c,r,y,s]))+sum{t in tech:((t,c) in mTechOutCommAgg)}(sum{sp in timeslice:((t,c,sp,s) in mTechOutCommAggTimeslice)}(sum{fi in FORIF: (t,c,r,y,sp) in mvTechOut} (vTechOut[t,c,r,y,sp])))+sum{t in tech:((t,c) in mTechAOutCommSameTimeslice)}(sum{fi in FORIF: (t,c,r,y,s) in mvTechAOut} (vTechAOut[t,c,r,y,s]))+sum{t in tech:((t,c) in mTechAOutCommAgg)}(sum{sp in timeslice:((t,c,sp,s) in mTechAOutCommAggTimeslice)}(sum{fi in FORIF: (t,c,r,y,sp) in mvTechAOut} (vTechAOut[t,c,r,y,sp])));

# [agg-rewrite] eqTechOutRY/vTechOutRY retired (dead reporting)

s.t.  eqStorageInpTot{(c, r, y, s) in mStorageInpTot}: vStorageInpTot[c,r,y,s]  =  sum{st1 in stg:((st1,c,r,y,s) in mvStorageInp)}(vStorageInp[st1,c,r,y,s])+sum{st1 in stg:((st1,c,r,y,s) in mvStorageAInp)}(vStorageAInp[st1,c,r,y,s]);

s.t.  eqStorageOutTot{(c, r, y, s) in mStorageOutTot}: vStorageOutTot[c,r,y,s]  =  sum{st1 in stg:((st1,c,r,y,s) in mvStorageOut)}(vStorageOut[st1,c,r,y,s])+sum{st1 in stg:((st1,c,r,y,s) in mvStorageAOut)}(vStorageAOut[st1,c,r,y,s]);

s.t.  eqDummyImportCost{(c, r, y) in mDummyImportCost}: vDummyImportCost[c,r,y]  =  sum{s in timeslice:((c,r,y,s) in mDummyImport)}(pTimesliceWeight[y,s]*pDummyImportCost[c,r,y,s]*sum{fi in FORIF: (c,r,y,s) in mDummyImport} (vDummyImport[c,r,y,s]));

s.t.  eqDummyExportCost{(c, r, y) in mDummyExportCost}: vDummyExportCost[c,r,y]  =  sum{s in timeslice:((c,r,y,s) in mDummyExport)}(pTimesliceWeight[y,s]*pDummyExportCost[c,r,y,s]*sum{fi in FORIF: (c,r,y,s) in mDummyExport} (vDummyExport[c,r,y,s]));

s.t.  eqTaxCost{(c, r, y) in mTaxCost}: vTaxCost[c,r,y]  =  sum{s in timeslice:(((c,r,y,s) in mvOutTot and (c,s) in mCommTimeslice))}(pTaxCostOut[c,r,y,s]*pTimesliceWeight[y,s]*vOutTot[c,r,y,s])+sum{s in timeslice:(((c,r,y,s) in mvInpTot and (c,s) in mCommTimeslice))}(pTaxCostInp[c,r,y,s]*pTimesliceWeight[y,s]*vInpTot[c,r,y,s])+sum{s in timeslice:(((c,r,y,s) in mvBalance and (c,s) in mCommTimeslice))}(pTaxCostBal[c,r,y,s]*pTimesliceWeight[y,s]*vBalance[c,r,y,s]);

s.t.  eqSubsCost{(c, r, y) in mSubCost}: vSubsCost[c,r,y]  =  -sum{s in timeslice:(((c,r,y,s) in mvOutTot and (c,s) in mCommTimeslice))}(pSubCostOut[c,r,y,s]*pTimesliceWeight[y,s]*vOutTot[c,r,y,s])-sum{s in timeslice:(((c,r,y,s) in mvInpTot and (c,s) in mCommTimeslice))}(pSubCostInp[c,r,y,s]*pTimesliceWeight[y,s]*vInpTot[c,r,y,s])-sum{s in timeslice:(((c,r,y,s) in mvBalance and (c,s) in mCommTimeslice))}(pSubCostBal[c,r,y,s]*pTimesliceWeight[y,s]*vBalance[c,r,y,s]);

s.t.  eqCost{(r, y) in mvTotalCost}: vTotalCost[r,y]  =  0+sum{s1 in sup:((s1,r,y) in mvSupCost)}(sum{fi in FORIF: (s1,r,y) in mvSupCost} (vSupCost[s1,r,y]))+sum{t in tech:((t,r,y) in mTechEac)}(sum{fi in FORIF: (t,r,y) in mTechEac} (vTechEac[t,r,y]))+sum{t in tech:((t,r,y) in mTechRetCost)}(sum{fi in FORIF: (t,r,y) in mTechRetCost} (vTechRetCost[t,r,y]))+sum{st1 in stg:((st1,r,y) in mStorageRetCost)}(sum{fi in FORIF: (st1,r,y) in mStorageRetCost} (vStorageRetCost[st1,r,y]))+sum{t1 in trade:((t1,r,y) in mTradeRetCost)}(sum{fi in FORIF: (t1,r,y) in mTradeRetCost} (vTradeRetCost[t1,r,y]))+sum{t in tech:((t,r,y) in mTechFixom)}(sum{fi in FORIF: (t,r,y) in mTechFixom} (vTechFixom[t,r,y]))+sum{t in tech:((t,r,y) in mTechVarom)}(sum{fi in FORIF: (t,r,y) in mTechVarom} (vTechVarom[t,r,y]))+sum{st1 in stg:((st1,r,y) in mStorageEac)}(sum{fi in FORIF: (st1,r,y) in mStorageEac} (vStorageEac[st1,r,y]))+sum{st1 in stg:((st1,r,y) in mStorageFixom)}(sum{fi in FORIF: (st1,r,y) in mStorageFixom} (vStorageFixom[st1,r,y]))+sum{st1 in stg:((st1,r,y) in mStorageVarom)}(sum{fi in FORIF: (st1,r,y) in mStorageVarom} (vStorageVarom[st1,r,y]))+sum{i in imp:((i,r,y) in mImportRowCost)}(sum{fi in FORIF: (i,r,y) in mImportRowCost} (vImportRowCost[i,r,y]))+sum{e in expp:((e,r,y) in mExportRowCost)}(sum{fi in FORIF: (e,r,y) in mExportRowCost} (vExportRowCost[e,r,y]))+sum{t1 in trade:((t1,r,y) in mTradeEac)}(sum{fi in FORIF: (t1,r,y) in mTradeEac} (vTradeEac[t1,r,y]))+sum{t1 in trade:((t1,r,y) in mTradeFixom)}(sum{fi in FORIF: (t1,r,y) in mTradeFixom} (vTradeFixom[t1,r,y]))+sum{t1 in trade:((t1,r,y) in mImportIrCost)}(sum{fi in FORIF: (t1,r,y) in mImportIrCost} (vImportIrCost[t1,r,y]))+sum{t1 in trade:((t1,r,y) in mExportIrCost)}(sum{fi in FORIF: (t1,r,y) in mExportIrCost} (vExportIrCost[t1,r,y]))+sum{c in comm:((c,r,y) in mTaxCost)}(sum{fi in FORIF: (c,r,y) in mTaxCost} (vTaxCost[c,r,y]))+sum{c in comm:((c,r,y) in mSubCost)}(sum{fi in FORIF: (c,r,y) in mSubCost} (vSubsCost[c,r,y]))+sum{fi in FORIF: (r,y) in mvTotalUserCosts} (vTotalUserCosts[r,y])+sum{c in comm:((c,r,y) in mDummyImportCost)}(sum{fi in FORIF: (c,r,y) in mDummyImportCost} (vDummyImportCost[c,r,y]))+sum{c in comm:((c,r,y) in mDummyExportCost)}(sum{fi in FORIF: (c,r,y) in mDummyExportCost} (vDummyExportCost[c,r,y]));

s.t.  eqObjective: vObjective  =  sum{r in region,y in year:((r,y) in mvTotalCost)}(vTotalCost[r,y]*pPeriodLen[y]*pDiscountFactor[r,y]);


printf  '"solver",,"%s"\n', time2str(gmtime(), "%Y-%m-%d %M:%H:S %TZ") >> "output/log.csv";
minimize vObjective2 : vObjective;

solve;
end;


