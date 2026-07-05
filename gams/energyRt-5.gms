$include inc1.gms

OPTION RESLIM=50000, PROFILE=0, SOLVEOPT=REPLACE;
OPTION ITERLIM=999999, LIMROW=0, LIMCOL=0, SOLPRINT=OFF;
*OPTION RESLIM=50000, PROFILE=1, SOLVEOPT=REPLACE;
*OPTION ITERLIM=999999, LIMROW=10000, LIMCOL=10000, SOLPRINT=ON;

* start log file
file log_stat / 'output/log.csv'/;
log_stat.lp = 1;
put log_stat;
put "parameter,value,time"/;
put '"model language",gams,"' GYear(JNow):0:0 "-" GMonth(JNow):0:0 "-" GDay(JNow):0:0 " " GHour(JNow):0:0 ":" GMinute(JNow):0:0 ":" GSecond(JNow):0:0'"'/;
put '"model definition",,"' GYear(JNow):0:0 "-" GMonth(JNow):0:0 "-" GDay(JNow):0:0 " " GHour(JNow):0:0 ":" GMinute(JNow):0:0 ":" GSecond(JNow):0:0'"'/;

* Main sets
sets
comm   commodity
region region
year   year
slice  time slices
sup    supply
dem    demand
tech   technology
stg    storage
trade  trade between regions
expp   export to the rest of the world (ROW)
imp    import from the ROW
group  group of input or output commodities in technology
weather weather
;

alias (tech, techp), (region, regionp), (year, yearp), (year, yeare), (year, yearn);
alias (slice, slicep), (slice, slicepp), (group, groupp), (comm, commp), (comm, acomm), (comm, comme), (sup, supp);
alias (region, src), (region, dst), (region, region2), (year, year2), (slice, slice2);

* Mapping sets
sets
mCommReg(comm, region)        Commodity to region mapping (to filter out unused cases)
mSameRegion(region, region)   The same region (used in GLPK)
mSameSlice(slice, slice)      The same slice (used in GLPK)
*! technology:input
mMilestoneFirst(year)          First period milestone
mMilestoneLast(year)           Last period milestone
mMilestoneNext(year, year)     Next period milestone
mMilestoneHasNext(year)        Is there next period milestone
mStartMilestone(year, year)    Start of the period
mEndMilestone(year, year)      End of the period
mMidMilestone(year)            Milestone year
mCommSlice(comm, slice)        Commodity to slice
mCommSliceOrParent(comm, slice, slice)
mTechRetirement(tech)          Early retirement option
mTechRetCost(tech, region, year)  Costs of early retirement
mTechUpgrade(tech, tech)       Upgrade technology (not implemented yet)
mTechInpComm(tech, comm)       Input commodity
mTechOutComm(tech, comm)       Output commodity
mTechInpGroup(tech, group)     Group input
mTechOutGroup(tech, group)     Group output
mTechOneComm(tech, comm)       Commodity without group
*
mTechGroupComm(tech, group, comm)  Mapping between commodity-groups and commodities
* Aux input comm map
mTechAInp(tech, comm)            Auxiliary input
* Aux output comm map
mTechAOut(tech, comm)            Auxiliary output
*
mTechNew(tech, region, year)     Technologies available for investment
mTechSpan(tech, region, year)    Availability of each technology by regions and milestone years
mTechSlice(tech, slice)          Technology to slice-level
* Supply
mSupSlice(sup, slice)             Supply to slices-level
mSupComm(sup, comm)               Supplied commodities
mSupSpan(sup, region)             Supply in regions
* Demand
mDemComm(dem, comm)               Demand commodities
* Ballance
mUpComm(comm)  Commodity balance type TOTAL SUPPLY <= TOTAL DEMAND
mLoComm(comm)  Commodity balance type TOTAL SUPPLY >= TOTAL DEMAND
mFxComm(comm)  Commodity balance type TOTAL SUPPLY == TOTAL DEMAND
* Storage
mStorageFullYear(stg)              Mapping of storage with joint slice
mStorageComm(stg, comm)            Mapping of storage technology and respective commodity
mStorageAInp(stg, comm)            Aux-commodity input to storage
mStorageAOut(stg, comm)            Aux-commodity output from storage
mStorageNew(stg, region, year)     Storage available for investment
mStorageSpan(stg, region, year)    Storage set showing if the storage may exist in the year and region
*mStorageOMCost(stg, region, year)  drop
mStorageFixom(stg, region, year)   add
mStorageVarom(stg, region, year)   add
mStorageEac(stg, region, year)
mSliceNext(slice, slice)           Next slice
mSliceFYearNext(slice, slice)      Next slice joint
* Trade and the ROW
mTradeSlice(trade, slice)          Trade to slice
mTradeComm(trade, comm)            Trade commodities
mTradeRoutes(trade, region, region)
mTradeIrAInp(trade, comm)          Auxiliary  input commodity in source region
mTradeIrAOut(trade, comm)          Auxiliary output commodity in source region
mExpComm(expp, comm)               Export commodities
mImpComm(imp, comm)                Import commodities
mExpSlice(expp, slice)             Export to slice
mImpSlice(imp, slice)              Import to slice
* Mapping for Salvage (temporary)
mDiscountZero(region)
mSliceParentChildE(slice, slice)   Child slice or the same
mSliceParentChild(slice, slice)    Child slice not the same
*
mTradeRoutes(trade, region, region)
mTradeSpan(trade, year)
mTradeNew(trade, year)
mTradeOlifeInf(trade)
mTradeEac(trade, region, year)
mTradeCapacityVariable(trade)
mTradeInv(trade, region, year)
mTradeFixom(trade, region, year)
mAggregateFactor(comm, comm)
;

* Parameter
parameters
* Fraction of a year of selected time slices
pYearFraction(year)   fraction of sum of sampled slices in year -- experimental
* Technology parameters
pTechOlife(tech, region)                           Operational life of technologies
pTechCinp2ginp(tech, comm, region, year, slice)    Commodity input to group input
pTechGinp2use(tech, group, region, year, slice)    Group input into use
pTechCinp2use(tech, comm, region, year, slice)     Commodity input to use
pTechUse2cact(tech, comm, region, year, slice)     Use to commodity activity
pTechCact2cout(tech, comm, region, year, slice)    Commodity activity to commodity output
pTechEmisComm(tech, comm)                          Combustion factor for input commodity (from 0 to 1)
* Auxiliary input commodities
pTechAct2AInp(tech, comm, region, year, slice)     Activity to aux-commodity input
pTechCap2AInp(tech, comm, region, year, slice)     Capacity to aux-commodity input
pTechNCap2AInp(tech, comm, region, year, slice)     New capacity to aux-commodity input
pTechCinp2AInp(tech, comm, comm, region, year, slice)    Commodity input to aux-commodity input
pTechCout2AInp(tech, comm, comm, region, year, slice)    Commodity output to aux-commodity input
* Aux output comm map
pTechAct2AOut(tech, comm, region, year, slice)     Activity to aux-commodity output
pTechCap2AOut(tech, comm, region, year, slice)     Capacity to aux-commodity output
pTechNCap2AOut(tech, comm, region, year, slice)     New capacity to aux-commodity output
pTechCinp2AOut(tech, comm, comm, region, year, slice)     Commodity to aux-commodity output
pTechCout2AOut(tech, comm, comm, region, year, slice)     Commodity-output to aux-commodity input
*
pTechFixom(tech, region, year)                      Fixed Operating and maintenance (O&M) costs (per unit of capacity)
pTechVarom(tech, region, year, slice)               Variable O&M costs (per unit of acticity)
pTechInvcost(tech, region, year)                    Overnight Investment costs (per unit of capacity)
pTechEac(tech, region, year)                        Equivalent annual (investment) cost
pTechRetCost(tech, region, year)                    Early retirement costs
pTechShareLo(tech, comm, region, year, slice)       Lower bound of the share of the commodity in total group input or output
pTechShareUp(tech, comm, region, year, slice)       Upper bound of the share of the commodity in total group input or output
pTechAfLo(tech, region, year, slice)                Lower bound on availability factor by slices
pTechAfUp(tech, region, year, slice)                Upper bound on availability factor by slices
pTechRampUp(tech, region, year, slice)              Ramp Up on availability factor
pTechRampDown(tech, region, year, slice)            Ramp Down on availability
pTechAfsLo(tech, region, year, slice)               Lower bound on availability factor by groups of slices
pTechAfsUp(tech, region, year, slice)               Upper bound on availability factor by groups of slices
pTechAfcLo(tech, comm, region, year, slice)         Lower bound for commodity output
pTechAfcUp(tech, comm, region, year, slice)         Upper bound for commodity output
pTechStock(tech, region, year)                      Technology capacity stock
pTechCapUp(tech, region, year)                      Upper bound on technology capacity
pTechCapLo(tech, region, year)                      Lower bound on technology capacity
pTechNewCapUp(tech, region, year)                   Upper bound on new technology capacity
pTechNewCapLo(tech, region, year)                   Lower bound on new technology capacity
pTechRetUp(tech, region, year)                      Upper bound on early retirement
pTechRetLo(tech, region, year)                      Lower bound on early retirement
pTechCap2act(tech)                                  Technology capacity units to activity units conversion factor
pTechCvarom(tech, comm, region, year, slice)        Commodity-specific variable costs (per unit of commodity input or output)
pTechAvarom(tech, comm, region, year, slice)        Auxilary Commodity-specific variable costs (per unit of commodity input or output)
* Discount
pDiscount(region, year)                             Discount rate (can be region and year specific)
pDiscountFactor(region, year)                       Discount factor (cumulative)
pDiscountFactorMileStone(region, year)              Discount factor (cumulative) sum for MileStone
* Supply
pSupCost(sup, comm, region, year, slice)            Costs of supply (price per unit)
pSupAvaUp(sup, comm, region, year, slice)           Upper bound for supply
pSupAvaLo(sup, comm, region, year, slice)           Lower bound for supply
pSupReserveUp(sup, comm, region)                    Upper constraint on cumulative supply
pSupReserveLo(sup, comm, region)                    Lower constraint on cumulative supply
* Demand
pDemand(dem, comm, region, year, slice)             Exogenous demand
* Emissions
pEmissionFactor(comm, comm)                         Emission factor
* Dummy import
pDummyImportCost(comm, region, year, slice)         Dummy costs parameters (for debugging)
pDummyExportCost(comm, region, year, slice)         Dummy costs parameters (for debuging)
* Taxes and subsidies
pTaxCostInp(comm, region, year, slice)              Commodity taxes for input
pTaxCostOut(comm, region, year, slice)              Commodity taxes for output
pTaxCostBal(comm, region, year, slice)              Commodity taxes for balance
pSubCostInp(comm, region, year, slice)              Commodity subsidies for input
pSubCostOut(comm, region, year, slice)              Commodity subsidies for output
pSubCostBal(comm, region, year, slice)              Commodity subsidies for balance
* Aggregation
pAggregateFactor(comm, comm)                        Aggregation factor of commodities
* System parameters
pPeriodLen(year)        Length of milestone-year-period
pSliceShare(slice)      Slice share in year
pSliceWeight(year, slice)     Slice weight
ordYear(year)           ord year (used in GLPK-MathProg)
cardYear(year)          card year (used in GLPK-MathProg)
;

* Storage technology parameters
parameters
pStorageInpEff(stg, comm, region, year, slice)      Storage input efficiency
pStorageOutEff(stg, comm, region, year, slice)      Storage output efficiency
pStorageStgEff(stg, comm, region, year, slice)      Storage time-efficiency (annual)
pStorageStock(stg, region, year)                    Storage capacity stock
pStorageCapUp(stg, region, year)                    Upper bound on storage capacity
pStorageCapLo(stg, region, year)                    Lower bound on storage capacity
pStorageNewCapUp(stg, region, year)                 Upper bound on new storage capacity
pStorageNewCapLo(stg, region, year)                 Lower bound on new storage capacity
pStorageRetUp(stg, region, year)                    Upper bound on early retirement
pStorageRetLo(stg, region, year)                    Lower bound on early retirement
pStorageOlife(stg, region)                          Storage operational life
pStorageCostStore(stg, region, year, slice)         Storing costs per stored amount (annual)
pStorageCostInp(stg, region, year, slice)           Storage input costs
pStorageCostOut(stg, region, year, slice)           Storage output costs
pStorageFixom(stg, region, year)                    Storage fixed O&M costs
pStorageInvcost(stg, region, year)                  Storage investment costs
pStorageEac(stg, region, year)                      Storage equivalent annual costs
pStorageRetCost(stg, region, year)                  Storage early retirement costs
pStorageCap2stg(stg)                                Storage (dis)charging capacity to accumulating capacity (to be renamed to duration)
pStorageAfLo(stg, region, year, slice)              Storage availability factor lower bound (minimum charging level)
pStorageAfUp(stg, region, year, slice)              Storage availability factor upper bound (maximum charging level)
*pStorageAfsLo(tech, region, year, slice)           add parameter and eq?
*pStorageAfsUp(tech, region, year, slice)           add parameter and eq?
pStorageCinpUp(stg, comm, region, year, slice)      Storage input upper bound
pStorageCinpLo(stg, comm, region, year, slice)      Storage input lower bound
pStorageCoutUp(stg, comm, region, year, slice)      Storage output upper bound
pStorageCoutLo(stg, comm, region, year, slice)      Storage output lower bound
pStorageNCap2Stg(stg, comm, region, year, slice)    Initial storage charge level for new investment
pStorageCharge(stg, comm, region, year, slice)      Initial storage charge level for stock
pStorageStg2AInp(stg, comm, region, year, slice)    Storage level to auxilary input
pStorageStg2AOut(stg, comm, region, year, slice)    Storage level output
pStorageCinp2AInp(stg, comm, region, year, slice)   Storage input to auxilary input
pStorageCinp2AOut(stg, comm, region, year, slice)   Storage input to auxilary output
pStorageCout2AInp(stg, comm, region, year, slice)   Storage output to auxilary input
pStorageCout2AOut(stg, comm, region, year, slice)   Storage output to auxilary output
pStorageCap2AInp(stg, comm, region, year, slice)    Storage capacity to auxilary input
pStorageCap2AOut(stg, comm, region, year, slice)    Storage capacity to auxilary output
pStorageNCap2AInp(stg, comm, region, year, slice)   Storage new capacity to auxilary input
pStorageNCap2AOut(stg, comm, region, year, slice)   Storage new capacity to auxilary output
;
* Trade parameters
parameters
pTradeIrEff(trade, region, region, year, slice)     Inter-regional trade efficiency
pTradeIrUp(trade, region, region, year, slice)      Upper bound on trade flow
pTradeIrLo(trade, region, region, year, slice)      Lower bound on trade flow
pTradeIrCost(trade, region, region, year, slice)    Costs of trade flow
pTradeIrMarkup(trade, region, region, year, slice)  Markup of trade flow
* Aux input and output
pTradeIrCsrc2Ainp(trade, comm, region, region, year, slice)   Auxiliary input commodity in source region
pTradeIrCsrc2Aout(trade, comm, region, region, year, slice)   Auxiliary output commodity in source region
pTradeIrCdst2Ainp(trade, comm, region, region, year, slice)   Auxiliary input commodity in destination region
pTradeIrCdst2Aout(trade, comm, region, region, year, slice)   Auxiliary output commodity in destination region
pExportRowRes(expp)                                  Upper bound on cumulative export to the ROW
pExportRowUp(expp, region, year, slice)              Upper bound on export to the ROW
pExportRowLo(expp, region, year, slice)              Lower bound on export to the ROW
pExportRowPrice(expp, region, year, slice)           Export prices to the ROW
pImportRowRes(imp)                                   Upper bound on cumulative import to the ROW
pImportRowUp(imp, region, year, slice)               Upper bount on import from the ROW
pImportRowLo(imp, region, year, slice)               Lower bound on import from the ROW
pImportRowPrice(imp, region, year, slice)            Import prices from the ROW
pTradeStock(trade, year)                             Existing capacity
pTradeCapUp(trade, year)                             Upper bound on trade capacity
pTradeCapLo(trade, year)                             Lower bound on trade capacity
pTradeNewCapUp(trade, year)                          Upper bound on new trade capacity
pTradeNewCapLo(trade, year)                          Lower bound on new trade capacity
pTradeRetUp(trade, year)                             Upper bound on early retirement
pTradeRetLo(trade, year)                             Lower bound on early retirement
pTradeOlife(trade)                                   Operational life
pTradeInvcost(trade, region, year)                   Overnight investment costs
pTradeEac(trade, region, year)                       Equivalent annual costs
pTradeRetCost(trade, region, year)                   Early retirement costs
pTradeFixom(trade, region, year)                             Fixed O&M costs
pTradeVarom(trade, region, region, year, slice)      Variable O&M costs
pTradeCap2Act(trade)                                 Capacity to activity factor
;

* Weather mapping
sets
mWeatherSlice(weather, slice)
mWeatherRegion(weather, region)
mSupWeatherLo(weather, sup)
mSupWeatherUp(weather, sup)
mTechWeatherAfLo(weather, tech)
mTechWeatherAfUp(weather, tech)
mTechWeatherAfsLo(weather, tech)
mTechWeatherAfsUp(weather, tech)
mTechWeatherAfcLo(weather, tech, comm)
mTechWeatherAfcUp(weather, tech, comm)
mStorageWeatherAfLo(weather, stg)
mStorageWeatherAfUp(weather, stg)
mStorageWeatherCinpUp(weather, stg)
mStorageWeatherCinpLo(weather, stg)
mStorageWeatherCoutUp(weather, stg)
mStorageWeatherCoutLo(weather, stg)
;
* Weather parameter
parameters
pWeather(weather, region, year, slice)          weather factors
pSupWeatherUp(weather, sup)                     weather factor for supply upper value (ava.up)
pSupWeatherLo(weather, sup)                     weather factor for supply lower value (ava.lo)
pTechWeatherAfLo(weather, tech)                 weather factor for technology availability lower value (af.lo)
pTechWeatherAfUp(weather, tech)                 weather factor for technology availability upper value (af.up)
pTechWeatherAfsLo(weather, tech)                weather factor for technology availability lower value (af.lo)
pTechWeatherAfsUp(weather, tech)                weather factor for technology availability upper value (afs.lo)
pTechWeatherAfcLo(weather, tech, comm)          weather factor for technology availability lower value (afs.lo)
pTechWeatherAfcUp(weather, tech, comm)          weather factor for commodity availability upper value (afc.lo)
pStorageWeatherAfLo(weather, stg)               weather factor for storage availability lower value (af.lo)
pStorageWeatherAfUp(weather, stg)               weather factor for storage availability upper value (af.up)
pStorageWeatherCinpUp(weather, stg)             weather factor for storage commodity input upper value (cinp.up)
pStorageWeatherCinpLo(weather, stg)             weather factor for storage commodity input lower value (cinp.lo)
pStorageWeatherCoutUp(weather, stg)             weather factor for storage commodity output upper value (cout.up)
pStorageWeatherCoutLo(weather, stg)             weather factor for storage commodity output lower value (cout.lo)
;

sets
mvSupCost(sup, region, year)
mvTechInp(tech, comm, region, year, slice)
mvSupReserve(sup, comm, region)
mvTechRetiredNewCap(tech, region, year, year)

mvTechRetiredStock(tech, region, year)
*meqTechRetUp(tech, region, year)
*meqTechRetLo(tech, region, year)
mvTechAct(tech, region, year, slice)
mvTechInp(tech, comm, region, year, slice)
mvTechInpCommSameSlice(tech, comm, region, year, slice)
mvTechOut(tech, comm, region, year, slice)
mTechOutRY(tech, comm, region, year)
mvTechAInp(tech, comm, region, year, slice)
mvTechAOut(tech, comm, region, year, slice)
mvDemInp(comm, region, year, slice)
mvBalance(comm, region, year, slice)
mBalanceRY(comm, region, year)
mvInpTot(comm, region, year, slice)
mInpTotRY(comm, region, year)
mvOutTot(comm, region, year, slice)
mOutTotRY(comm, region, year)
mvInp2Lo(comm, region, year, slice, slice)
mvOut2Lo(comm, region, year, slice, slice)
mInpSub(comm, region, year, slice)
mOutSub(comm, region, year, slice)

mTechCapLo(tech, region, year)
mTechCapUp(tech, region, year)
mTechNewCapLo(tech, region, year)
mTechNewCapUp(tech, region, year)
mTechRetLo(tech, region, year)
mTechRetUp(tech, region, year)

mvStorageAInp(stg, comm, region, year, slice)
mvStorageAOut(stg, comm, region, year, slice)
mvStorageStore(stg, comm, region, year, slice)
meqStorageStore(stg, comm, region, year, slicep, slice)
mStorageStg2AOut(stg, comm, region, year, slice)
mStorageCinp2AOut(stg, comm, region, year, slice)
mStorageCout2AOut(stg, comm, region, year, slice)
mStorageCap2AOut(stg, comm, region, year, slice)
mStorageNCap2AOut(stg, comm, region, year, slice)
mStorageStg2AInp(stg, comm, region, year, slice)
mStorageCinp2AInp(stg, comm, region, year, slice)
mStorageCout2AInp(stg, comm, region, year, slice)
mStorageCap2AInp(stg, comm, region, year, slice)
mStorageNCap2AInp(stg, comm, region, year, slice)

mStorageCapLo(stg, region, year)
mStorageCapUp(stg, region, year)
mStorageNewCapLo(stg, region, year)
mStorageNewCapUp(stg, region, year)
mStorageRetLo(stg, region, year)
mStorageRetUp(stg, region, year)

mvTradeIr(trade, comm, region, region, year, slice)
mTradeIrCsrc2Ainp(trade, comm, region, region, year, slice)
mTradeIrCdst2Ainp(trade, comm, region, region, year, slice)
mTradeIrCsrc2Aout(trade, comm, region, region, year, slice)
mTradeIrCdst2Aout(trade, comm, region, region, year, slice)
mvTradeCost(region, year)
mvTradeRowCost(region, year)
mExportRowCost(expp, region, year)
mImportRowCost(imp, region, year)
*mvTradeIrCost(region, year)
mImportIrCost(trade, region, year)
mExportIrCost(trade, region, year)
mvTotalCost(region, year)
mvTotalUserCosts(region, year)

mTradeCapLo(trade, year)
mTradeCapUp(trade, year)
mTradeNewCapLo(trade, year)
mTradeNewCapUp(trade, year)
mTradeRetLo(trade, year)
mTradeRetUp(trade, year)
;

* Endogenous variables
positive variables
*@ mTechNew(tech, region, year)
vTechNewCap(tech, region, year)                      New capacity
*@ mvTechRetiredStock(tech, region, year)
vTechRetiredStockCum(tech, region, year)                Early retired stock
*@ mvTechRetiredStock(tech, region, year)
vTechRetiredStock(tech, region, year)            Early retired stock
*@ mvTechRetiredNewCap(tech, region, year, year)
vTechRetiredNewCap(tech, region, year, year)         Early retired new capacity
* Activity and intput-output
*@ mTechSpan(tech, region, year)
vTechCap(tech, region, year)                         Total capacity of the technology
*@ mvTechAct(tech, region, year, slice)
vTechAct(tech, region, year, slice)                  Activity level of technology
*@ mvTechInp(tech, comm, region, year, slice)
vTechInp(tech, comm, region, year, slice)            Input level
*@ mvTechOut(tech, comm, region, year, slice)
vTechOut(tech, comm, region, year, slice)            Commodity output from technology - tech timeframe
*@ mTechOutRY(tech, comm, region, year)
vTechOutRY(tech, comm, region, year)            Commodity output from technology - tech timeframe
* Auxiliary input & output
*@ mvTechAInp(tech, comm, region, year, slice)
vTechAInp(tech, comm, region, year, slice)           Auxiliary commodity input
*@ mvTechAOut(tech, comm, region, year, slice)
vTechAOut(tech, comm, region, year, slice)           Auxiliary commodity output
;
variables
*@ mTechInv(tech, region, year)
vTechInv(tech, region, year)                         Overnight investment costs
*@ mTechEac(tech, region, year)
vTechEac(tech, region, year)                         Annualized investment costs
*@ mTechRetCost(tech, region, year)
vTechRetCost(tech, region, year)                     Early retirement costs
* mTechOMCost(tech, region, year)
* vTechOMCost(tech, region, year)                      Sum of all operational costs is equal vTechFixom + vTechVarom (AVarom + CVarom + ActVarom)
*@ mTechFixom(tech, region, year)
vTechFixom(tech, region, year)                       Fixed O&M costs
*@ mTechVarom(tech, region, year)
vTechVarom(tech, region, year)                 Variable O&M costs (AVarom + CVarom + ActVarom)
;
positive variables
* Supply
*@ mSupAva(sup, comm, region, year, slice)
vSupOut(sup, comm, region, year, slice)              Output of supply
*@ mvSupReserve(sup, comm, region)
vSupReserve(sup, comm, region)                       Cumulative supply (weighted)
;
variables
*@ mvSupCost(sup, region, year)
vSupCost(sup, region, year)                          Supply costs (weighted)
;
positive variables
* Demand
*@ mvDemInp(comm, region, year, slice)
vDemInp(comm, region, year, slice)                   Input to demand
;
variables
* Emission
*@ mEmsFuelTot(comm, region, year, slice)
vEmsFuelTot(comm, region, year, slice)               Total emissions from fuels combustion (technologies)
;
variables
* Ballance
*@ mvBalance(comm, region, year, slice)
vBalance(comm, region, year, slice)                  Net commodity balance (all sources)
*@ mBalanceRY(comm, region, year)
vBalanceRY(comm, region, year)                     Net commodity balance by region and year (weighted)
;
positive variables
*@ mvOutTot(comm, region, year, slice)
vOutTot(comm, region, year, slice)                   Total commodity output (all processes) (weighted)
*@ mOutTotRY(comm, region, year)
vOutTotRY(comm, region, year)                   Total commodity output (all processes) (weighted)
*@ mvInpTot(comm, region, year, slice)
vInpTot(comm, region, year, slice)                   Total commodity input (all processes) (weighted)
*@ mInpTotRY(comm, region, year)
vInpTotRY(comm, region, year)                   Total commodity input (all processes) (weighted)
*@ mvInp2Lo(comm, region, year, slice, slice)
vInp2Lo(comm, region, year, slice, slice)            Desagregation of slices for input parent to (grand)child
*@ mvOut2Lo(comm, region, year, slice, slice)
vOut2Lo(comm, region, year, slice, slice)            Desagregation of slices for output parent to (grand)child
*@ mSupOutTot(comm, region, year, slice)
vSupOutTot(comm, region, year, slice)                Total commodity supply (weighted)
*@ mTechInpTot(comm, region, year, slice)
vTechInpTot(comm, region, year, slice)               Total commodity (main & aux) input to technologies (weighted)
*@ mTechOutTot(comm, region, year, slice)
vTechOutTot(comm, region, year, slice)               Total commodity (main & aux) output from technologies (weighted)
*@ mStorageInpTot(comm, region, year, slice)
vStorageInpTot(comm, region, year, slice)            Total commodity (main & aux) input to storage (weighted)
*@ mStorageOutTot(comm, region, year, slice)
vStorageOutTot(comm, region, year, slice)            Total commodity (main & aux) output from storage (weighted)
*@ mvStorageAInp(stg, comm, region, year, slice)
vStorageAInp(stg, comm, region, year, slice)         Aux-commodity input to storage
*@ mvStorageAOut(stg, comm, region, year, slice)
vStorageAOut(stg, comm, region, year, slice)         Aux-commodity input from storage
;
variable
* Costs variable
*@ mvTotalCost(region, year)
vTotalCost(region, year)                             Regional annual total costs (weighted)
vObjective                                           Objective costs
;
positive variables
* Dummy import
*@ mDummyImport(comm, region, year, slice)
vDummyImport(comm, region, year, slice)               Dummy import (for debugging)
*@ mDummyExport(comm, region, year, slice)
vDummyExport(comm, region, year, slice)               Dummy export (for debugging)
;
variable
* Taxes
*@ mTaxCost(comm, region, year)
vTaxCost(comm, region, year)                         Total tax levies (tax costs)
* Subsidies
*@ mSubCost(comm, region, year)
vSubsCost(comm, region, year)                        Total subsidies (substracted from costs)
*@ mAggOut(comm, region, year, slice)
vAggOutTot(comm, region, year, slice)                Aggregated commodity output (weighted)
*@ mDummyImportCost(comm, region, year)
vDummyImportCost(comm, region, year)   Dummy import costs (weighted)
*@ mDummyExportCost(comm, region, year)
vDummyExportCost(comm, region, year)   Dummy export costs (weighted)
;
* Reserves
positive variable
*@ mvStorageStore(stg, comm, region, year, slice)
vStorageInp(stg, comm, region, year, slice)          Storage input
*@ mvStorageStore(stg, comm, region, year, slice)
vStorageOut(stg, comm, region, year, slice)          Storage output
*@ mvStorageStore(stg, comm, region, year, slice)
vStorageStore(stg, comm, region, year, slice)        Storage level
*@ mStorageNew(stg, region, year)
vStorageInv(stg, region, year)                       Storage investments
*@ mStorageEac(stg, region, year)
vStorageEac(stg, region, year)                       Storage EAC investments
*@ mStorageSpan(stg, region, year)
vStorageCap(stg, region, year)                       Storage capacity
*@ mStorageNew(stg, region, year)
vStorageNewCap(stg, region, year)                    Storage new capacity
;
variable
* mStorageOMCost(stg, region, year)
*vStorageOMCost(stg, region, year)                    Storage O&M costs (weighted)
*@ mStorageFixom(stg, region, year)
vStorageFixom(stg, region, year)                     Storage fixed O&M costs
*@ mStorageVarom(stg, region, year)
vStorageVarom(stg, region, year)                     Storage variable O&M costs
;

* Trade and Row variables
positive variables
*@ mImport(comm, region, year, slice)
vImportTot(comm, region, year, slice)                Total regional import (Ir + ROW) (weighted)
*@ mExport(comm, region, year, slice)
vExportTot(comm, region, year, slice)                Total regional export (Ir + ROW) (weighted)
*@ mvTradeIr(trade, comm, region, region, year, slice)
vTradeIr(trade, comm, region, region, year, slice)   Total physical trade flows between regions
*@ mvTradeIrAInp(trade, comm, region, year, slice)
vTradeIrAInp(trade, comm, region, year, slice)       Trade auxilari input
*@ mvTradeIrAInpTot(comm, region, year, slice)
vTradeIrAInpTot(comm, region, year, slice)           Trade total auxilari input (weighted)
*@ mvTradeIrAOut(trade, comm, region, year, slice)
vTradeIrAOut(trade, comm, region, year, slice)       Trade auxilari output
*@ mvTradeIrAOutTot(comm, region, year, slice)
vTradeIrAOutTot(comm, region, year, slice)           Trade auxilari output total (weighted)
*@ mExpComm(expp, comm)
vExportRowCum(expp, comm)                            Cumulative export to the ROW
*@ mExportRow(expp, comm, region, year, slice)
vExportRow(expp, comm, region, year, slice)          Export to the ROW
*@ mImpComm(imp, comm)
vImportRowCum(imp, comm)                             Cumulative import from the ROW
*@ mImportRow(imp, comm, region, year, slice)
vImportRow(imp, comm, region, year, slice)           Import from the ROW
;
variable
* mvTradeCost(region, year)
*vTradeCost(region, year)                             Total trade costs (weighted)
*@ mTradeEac(trade, region, year)
vTradeEac(trade, region, year)                       Annualized investments in Interregional trade capacity
*@ mTradeFixom(trade, region, year)
vTradeFixom(trade, region, year)                     Interregional trade fixed O&M costs
*mvTradeIrCost(region, year)
*vTradeIrCost(region, year)                           Interregional trade variable costs and markups
*@ mImportIrCost(trade, region, year)
vImportIrCost(trade, region, year)                     Import costs from other regions
*@ mExportIrCost(trade, region, year)
vExportIrCost(trade, region, year)                    Credits (revenue) for export to other regions
*@ mImportRowCost(imp, region, year)
vImportRowCost(imp, region, year)                    Import costs from the ROW
*@ mExportRowCost(expp, region, year)
vExportRowCost(expp, region, year)                   Credits for export to the ROW
;
positive variables
*@ mTradeSpan(trade, year)
vTradeCap(trade, year)                               Trade capacity
*@ mTradeEac(trade, region, year)
vTradeInv(trade, region, year)                       Investment in trade capacity (overnight)
*@ mTradeNew(trade, year)
vTradeNewCap(trade, year)                            New trade capacity
*@ mvTotalUserCosts(region, year)
vTotalUserCosts(region, year)                        Total additional costs (set by user)
;

********************************************************************************
* Mapping to drop unised variables and parameters, speed-up the model generation
* (especially in GLPK)
********************************************************************************
sets
mTechInv(tech, region, year)
mTechInpTot(comm, region, year, slice)               Total technology input  mapp
mTechInpCommSameSlice(tech, comm)
mTechInpCommAgg(tech, comm)
mTechInpCommAggSlice(tech, comm, slice, slicep)
mTechAInpCommSameSlice(tech, comm)
mTechAInpCommAgg(tech, comm)
mTechAInpCommAggSlice(tech, comm, slice, slicep)
mTechOutTot(comm, region, year, slice)               Total technology output mapp
mTechOutCommSameSlice(tech, comm)
mTechOutCommAgg(tech, comm)
mTechOutCommAggSlice(tech, comm, slice, slicep)
mTechAOutCommSameSlice(tech, comm)
mTechAOutCommAgg(tech, comm)
mTechAOutCommAggSlice(tech, comm, slice, slicep)
mTechEac(tech, region, year)
*mTechOMCost(tech, region, year)  depreciate
mTechFixom(tech, region, year)   added
mTechVarom(tech, region, year)   added
mSupOutTot(comm, region, year, slice)
mEmsFuelTot(comm, region, year, slice)
mTechEmsFuel(tech, comm, comm, region, year, slice)
mDummyImport(comm, region, year, slice)
mDummyExport(comm, region, year, slice)
mDummyImportCost(comm, region, year)
mDummyExportCost(comm, region, year)
mTradeIr(trade, region, region, year, slice)
mvTradeIrAInp(trade, comm, region, year, slice)
mvTradeIrAInpTot(comm, region, year, slice)
mvTradeIrAOut(trade, comm, region, year, slice)
mvTradeIrAOutTot(comm, region, year, slice)
mImportRow(imp, comm, region, year, slice)
mImportRowUp(imp, comm, region, year, slice)
mImportRowCumUp(imp, comm)
mExportRow(expp, comm, region, year, slice)
mExportRowUp(expp, comm, region, year, slice)
mExportRowCumUp(expp, comm)
mExport(comm, region, year, slice)
mImport(comm, region, year, slice)
mStorageInpTot(comm, region, year, slice)
mStorageOutTot(comm, region, year, slice)
mTaxCost(comm, region, year)
mSubCost(comm, region, year)
mAggOut(comm, region, year, slice)
mTechAfUp(tech, region, year, slice)
mTechFullYear(tech)
mTechRampUp(tech, region, year, slice, slicep)
mTechRampDown(tech, region, year, slice, slicep)
*mTechCommSliceSliceP(tech, comm, slice, slicep)
mTechCommOutSliceSliceP(tech, comm, slice, slicep)
mTechCommAOutSliceSliceP(tech, comm, slice, slicep)
mTechOlifeInf(tech, region)
mStorageOlifeInf(stg, region)
mTechAfcUp(tech, comm, region, year, slice)
mSupAvaUp(sup, comm, region, year, slice)
mSupAva(sup, comm, region, year, slice)
mSupReserveUp(sup, comm, region)
mOut2Lo(comm, region, year, slice)
mInp2Lo(comm, region, year, slice)
;

sets
meqTechRetiredNewCap(tech, region, year)
meqTechSng2Sng(tech, region, comm, comm, year, slice)
meqTechGrp2Sng(tech, region, group, comm, year, slice)
meqTechSng2Grp(tech, region, comm, group, year, slice)
meqTechGrp2Grp(tech, region, group, group, year, slice)
meqTechShareInpLo(tech, region, group, comm, year, slice)
meqTechShareInpUp(tech, region, group, comm, year, slice)
meqTechShareOutLo(tech, region, group, comm, year, slice)
meqTechShareOutUp(tech, region, group, comm, year, slice)
meqTechAfLo(tech, region, year, slice)
meqTechAfUp(tech, region, year, slice)
meqTechAfsLo(tech, region, year, slice)
meqTechAfsUp(tech, region, year, slice)
meqTechActSng(tech, comm, region, year, slice)
meqTechActGrp(tech, group, region, year, slice)
meqTechAfcOutLo(tech, region, comm, year, slice)
meqTechAfcOutUp(tech, region, comm, year, slice)
meqTechAfcInpLo(tech, region, comm, year, slice)
meqTechAfcInpUp(tech, region, comm, year, slice)
meqSupAvaLo(sup, comm, region, year, slice)
meqSupReserveLo(sup, comm, region)
meqStorageAfLo(stg, comm, region, year, slice)
meqStorageAfUp(stg, comm, region, year, slice)
meqStorageInpUp(stg, comm, region, year, slice)
meqStorageInpLo(stg, comm, region, year, slice)
meqStorageOutUp(stg, comm, region, year, slice)
meqStorageOutLo(stg, comm, region, year, slice)
meqTradeFlowUp(trade, comm, region, region, year, slice)
meqTradeFlowLo(trade, comm, region, region, year, slice)
meqExportRowLo(expp, comm, region, year, slice)
meqImportRowUp(imp, comm, region, year, slice)
meqImportRowLo(imp, comm, region, year, slice)
meqTradeCapFlow(trade, comm, year, slice)
meqBalLo(comm, region, year, slice)
meqBalUp(comm, region, year, slice)
meqBalFx(comm, region, year, slice)
meqLECActivity(tech, region, year)
mTechAct2AInp(tech, comm, region, year, slice)
mTechCap2AInp(tech, comm, region, year, slice)
mTechNCap2AInp(tech, comm, region, year, slice)
mTechCinp2AInp(tech, comm, comm, region, year, slice)
mTechCout2AInp(tech, comm, comm, region, year, slice)
mTechAct2AOut(tech, comm, region, year, slice)
mTechCap2AOut(tech, comm, region, year, slice)
mTechNCap2AOut(tech, comm, region, year, slice)
mTechCinp2AOut(tech, comm, comm, region, year, slice)
mTechCout2AOut(tech, comm, comm, region, year, slice)
;

$include inc2.gms

********************************************************************************
* Equations
********************************************************************************
********************************************************************************
** Technology
********************************************************************************

********************************************************************************
*** Activity Input & Output
********************************************************************************
Equations
* Input & Output of ungrouped (single) commodities
eqTechSng2Sng(tech, region, comm, commp, year, slice)      Technology input to output
eqTechGrp2Sng(tech, region, group, commp, year, slice)     Technology group input to output
eqTechSng2Grp(tech, region, comm, groupp, year, slice)     Technology input to group output
eqTechGrp2Grp(tech, region, group, groupp, year, slice)    Technology group input to group output
;

eqTechSng2Sng(tech, region, comm, commp, year, slice)$meqTechSng2Sng(tech, region, comm, commp, year, slice)..
   vTechInp(tech, comm, region, year, slice) *
   pTechCinp2use(tech, comm, region, year, slice)
   =e=
   vTechOut(tech, commp, region, year, slice) /
           pTechUse2cact(tech, commp, region, year, slice) /
           pTechCact2cout(tech, commp, region, year, slice);

eqTechGrp2Sng(tech, region, group, commp, year, slice)$meqTechGrp2Sng(tech, region, group, commp, year, slice)..
   pTechGinp2use(tech, group, region, year, slice) *
   sum(comm$mTechGroupComm(tech, group, comm),
           (vTechInp(tech, comm, region, year, slice) *
           pTechCinp2ginp(tech, comm, region, year, slice))$mvTechInp(tech, comm, region, year, slice)
   )
   =e=
   vTechOut(tech, commp, region, year, slice) /
           pTechUse2cact(tech, commp, region, year, slice) /
           pTechCact2cout(tech, commp, region, year, slice);


eqTechSng2Grp(tech, region, comm, groupp, year, slice)$meqTechSng2Grp(tech, region, comm, groupp, year, slice)..
   vTechInp(tech, comm, region, year, slice) *
   pTechCinp2use(tech, comm, region, year, slice)
   =e=
    sum(commp$mTechGroupComm(tech, groupp, commp),
           (vTechOut(tech, commp, region, year, slice) /
           pTechUse2cact(tech, commp, region, year, slice) /
           pTechCact2cout(tech, commp, region, year, slice))$mvTechOut(tech, commp, region, year, slice)
   );

eqTechGrp2Grp(tech, region, group, groupp, year, slice)$meqTechGrp2Grp(tech, region, group, groupp, year, slice)..
   pTechGinp2use(tech, group, region, year, slice) *
   sum(comm$mTechGroupComm(tech, group, comm),
           (vTechInp(tech, comm, region, year, slice) *
           pTechCinp2ginp(tech, comm, region, year, slice))$mvTechInp(tech, comm, region, year, slice)
   )
   =e=
   sum(commp$mTechGroupComm(tech, groupp, commp),
           (vTechOut(tech, commp, region, year, slice) /
           pTechUse2cact(tech, commp, region, year, slice) /
           pTechCact2cout(tech, commp, region, year, slice))$mvTechOut(tech, commp, region, year, slice)
   );

********************************************************************************
*** Shares for grouped commodities
********************************************************************************
Equations
* Input Share LO
eqTechShareInpLo(tech, region, group, comm, year, slice)    Technology lower bound on input share
* Input Share UP
eqTechShareInpUp(tech, region, group, comm, year, slice)    Technology upper bound on input share
* Output Share LO
eqTechShareOutLo(tech, region, group, comm, year, slice)    Technology lower bound on output share
* Output Share UP
eqTechShareOutUp(tech, region, group, comm, year, slice)    Technology upper bound on output share
;

* Lower bound on Input Share
eqTechShareInpLo(tech, region, group, comm, year, slice)$meqTechShareInpLo(tech, region, group, comm, year, slice)..
                  vTechInp(tech, comm, region, year, slice)
                  =g=
                  pTechShareLo(tech, comm, region, year, slice) *
                  sum(commp$mTechGroupComm(tech, group, commp),
                          vTechInp(tech, commp, region, year, slice)$mvTechInp(tech, commp, region, year, slice)
                  );

* Upper bound on Input Share
eqTechShareInpUp(tech, region, group, comm, year, slice)$meqTechShareInpUp(tech, region, group, comm, year, slice)..
                  vTechInp(tech, comm, region, year, slice)
                  =l=
                  pTechShareUp(tech, comm, region, year, slice) *
                  sum(commp$mTechGroupComm(tech, group, commp),
                          vTechInp(tech, commp, region, year, slice)$mvTechInp(tech, commp, region, year, slice)
                  );

* Lower bound on Output Share
eqTechShareOutLo(tech, region, group, comm, year, slice)$meqTechShareOutLo(tech, region, group, comm, year, slice)..
                  vTechOut(tech, comm, region, year, slice)
                  =g=
                  pTechShareLo(tech, comm, region, year, slice) *
                  sum(commp$mTechGroupComm(tech, group, commp),
                          vTechOut(tech, commp, region, year, slice)$mvTechOut(tech, commp, region, year, slice)
                  );

* Upper bound on Output Share
eqTechShareOutUp(tech, region, group, comm, year, slice)$meqTechShareOutUp(tech, region, group, comm, year, slice)..
                  vTechOut(tech, comm, region, year, slice)
                  =l=
                  pTechShareUp(tech, comm, region, year, slice) *
                  sum(commp$mTechGroupComm(tech, group, commp),
                          vTechOut(tech, commp, region, year, slice)$mvTechOut(tech, commp, region, year, slice)
                  );

********************************************************************************
*** Auxiliary input & output
********************************************************************************
equation
eqTechAInp(tech, comm, region, year, slice)   Technology auxiliary commodity input
eqTechAOut(tech, comm, region, year, slice)   Technology auxiliary commodity output
;

eqTechAInp(tech, comm, region, year, slice)$mvTechAInp(tech, comm, region, year, slice)..
  vTechAInp(tech, comm, region, year, slice) =e=
  (vTechAct(tech, region, year, slice) *
    pTechAct2AInp(tech, comm, region, year, slice))$mTechAct2AInp(tech, comm, region, year, slice) +
  (vTechCap(tech, region, year) *
    pTechCap2AInp(tech, comm, region, year, slice) / pTechCap2act(tech))$mTechCap2AInp(tech, comm, region, year, slice) +
  (vTechNewCap(tech, region, year) *
    pTechNCap2AInp(tech, comm, region, year, slice))$mTechNCap2AInp(tech, comm, region, year, slice) +
  sum(commp$mTechCinp2AInp(tech, comm, commp, region, year, slice),
      pTechCinp2AInp(tech, comm, commp, region, year, slice) *
         vTechInp(tech, commp, region, year, slice)) +
  sum(commp$mTechCout2AInp(tech, comm, commp, region, year, slice),
      pTechCout2AInp(tech, comm, commp, region, year, slice) *
         vTechOut(tech, commp, region, year, slice));

eqTechAOut(tech, comm, region, year, slice)$mvTechAOut(tech, comm, region, year, slice)..
  vTechAOut(tech, comm, region, year, slice) =e=
  (vTechAct(tech, region, year, slice) *
    pTechAct2AOut(tech, comm, region, year, slice))$mTechAct2AOut(tech, comm, region, year, slice) +
  (vTechCap(tech, region, year) *
    pTechCap2AOut(tech, comm, region, year, slice) / pTechCap2act(tech))$mTechCap2AOut(tech, comm, region, year, slice) +
  (vTechNewCap(tech, region, year) *
    pTechNCap2AOut(tech, comm, region, year, slice))$mTechNCap2AOut(tech, comm, region, year, slice) +
  sum(commp$mTechCinp2AOut(tech, comm, commp, region, year, slice),
      pTechCinp2AOut(tech, comm, commp, region, year, slice) *
         vTechInp(tech, commp, region, year, slice)) +
  sum(commp$mTechCout2AOut(tech, comm, commp, region, year, slice),
      pTechCout2AOut(tech, comm, commp, region, year, slice) *
         vTechOut(tech, commp, region, year, slice));

********************************************************************************
*** Availability
********************************************************************************
Equation
* Availability factor LO
eqTechAfLo(tech, region, year, slice) Technology availability factor lower bound
* Availability factor UP
eqTechAfUp(tech, region, year, slice) Technology availability factor upper bound
* Availability sum factor LO
eqTechAfsLo(tech, region, year, slice) Technology availability factor for sum of slices lower bound
* Availability sum factor UP
eqTechAfsUp(tech, region, year, slice) Technology availability factor for sum of slices upper bound
* Ramp Up factor
eqTechRampUp(tech, region, year, slice, slicep) Technology ramp up
* Ramp Down factor
eqTechRampDown(tech, region, year, slice, slicep) Technology ramp down
;

* Availability factor LO
eqTechAfLo(tech, region, year, slice)$meqTechAfLo(tech, region, year, slice)..
         pTechAfLo(tech, region, year, slice) *
*         pYearFraction(year) *
         pTechCap2act(tech) *
         vTechCap(tech, region, year) *
         pSliceShare(slice)  *
         prod(weather$mTechWeatherAfLo(weather, tech),
              pTechWeatherAfLo(weather, tech) *
              pWeather(weather, region, year, slice)
              )
         =l=
         vTechAct(tech, region, year, slice);

* Availability factor UP
eqTechAfUp(tech, region, year, slice)$meqTechAfUp(tech, region, year, slice)..
         vTechAct(tech, region, year, slice)
         =l=
         pTechAfUp(tech, region, year, slice) *
*         pYearFraction(year) *
         pTechCap2act(tech) *
         vTechCap(tech, region, year) *
         pSliceShare(slice) *
         prod(weather$mTechWeatherAfUp(weather, tech),
              pTechWeatherAfUp(weather, tech) *
              pWeather(weather, region, year, slice)
              );

* Availability factor for sum LO
eqTechAfsLo(tech, region, year, slice)$meqTechAfsLo(tech, region, year, slice)..
         pTechAfsLo(tech, region, year, slice) *
*         pYearFraction(year) *
         pTechCap2act(tech) *
         vTechCap(tech, region, year) *
         pSliceShare(slice)  *
         prod(weather$mTechWeatherAfsLo(weather, tech),
              pTechWeatherAfsLo(weather, tech) *
              pWeather(weather, region, year, slice)
              )
         =l=
         sum(slicep$mSliceParentChildE(slice, slicep),
             vTechAct(tech, region, year, slicep)$mvTechAct(tech, region, year, slicep));

* Availability factor for sum UP
eqTechAfsUp(tech, region, year, slice)$meqTechAfsUp(tech, region, year, slice)..
         sum(slicep$mSliceParentChildE(slice, slicep),
         vTechAct(tech, region, year, slicep)$mvTechAct(tech, region, year, slicep))
         =l=
         pTechAfsUp(tech, region, year, slice) *
*         pYearFraction(year) *
         pTechCap2act(tech) *
         vTechCap(tech, region, year) *
         pSliceShare(slice) *  prod(weather$mTechWeatherAfsUp(weather, tech),
            pTechWeatherAfsUp(weather, tech) * pWeather(weather, region, year, slice));

* Ramp Up factor - new mapping
eqTechRampUp(tech, region, year, slice, slicep)$mTechRampUp(tech, region, year, slice, slicep)..
         vTechAct(tech, region, year, slice) / pSliceShare(slice)
         - vTechAct(tech, region, year, slicep) / pSliceShare(slicep)
         =l=
         pSliceShare(slice) * pTechCap2act(tech)
*         * pYearFraction(year)
         / pTechRampUp(tech, region, year, slice)
*         * pYearFraction(year)
         * pTechCap2act(tech) * vTechCap(tech, region, year);

* Ramp Down factor - new mapping
eqTechRampDown(tech, region, year, slice, slicep)$mTechRampDown(tech, region, year, slice, slicep)..
         vTechAct(tech, region, year, slicep) / pSliceShare(slicep)
         - vTechAct(tech, region, year, slice) / pSliceShare(slice)
         =l=
         pSliceShare(slice) * pTechCap2act(tech)
*         * pYearFraction(year)
         / pTechRampDown(tech, region, year, slice)
*         * pYearFraction(year)
         * pTechCap2act(tech) * vTechCap(tech, region, year);

********************************************************************************
*** Connect activity with output
********************************************************************************
Equation
* Connect activity with output
eqTechActSng(tech, comm, region, year, slice)  Technology activity to commodity output
eqTechActGrp(tech, group, region, year, slice) Technology activity to group output
;

* Connect activity with output
eqTechActSng(tech, comm, region, year, slice)$meqTechActSng(tech, comm, region, year, slice)..
  vTechAct(tech, region, year, slice) =e=
                 vTechOut(tech, comm, region, year, slice) /
                 pTechCact2cout(tech, comm, region, year, slice);

eqTechActGrp(tech, group, region, year, slice)$meqTechActGrp(tech, group, region, year, slice)..
    vTechAct(tech, region, year, slice) =e=
         sum(comm$mTechGroupComm(tech, group, comm),
                 (vTechOut(tech, comm, region, year, slice) /
                 pTechCact2cout(tech, comm, region, year, slice))$mvTechOut(tech, comm, region, year, slice)
         );

********************************************************************************
*** Availability commodity factor
********************************************************************************
Equation
* Availability commodity factor LO output equations
eqTechAfcOutLo(tech, region, comm, year, slice) Technology commodity availability factor lower bound
* Availability commodity factor UP output equations
eqTechAfcOutUp(tech, region, comm, year, slice) Technology commodity availability factor upper bound
* Availability commodity factor LO input equations
eqTechAfcInpLo(tech, region, comm, year, slice) Technology commodity availability factor lower bound
* Availability commodity factor UP input equations
eqTechAfcInpUp(tech, region, comm, year, slice) Technology commodity availability factor upper bound
;

* Availability commodity factor LO output equations
eqTechAfcOutLo(tech, region, comm, year, slice)$meqTechAfcOutLo(tech, region, comm, year, slice)..
         pTechCact2cout(tech, comm, region, year, slice) *
         pTechAfcLo(tech, comm, region, year, slice) *
*         pYearFraction(year) *
         pTechCap2act(tech) *
         vTechCap(tech, region, year) *
         pSliceShare(slice) * prod(weather$mTechWeatherAfcLo(weather, tech, comm),
            pTechWeatherAfcLo(weather, tech, comm) * pWeather(weather, region, year, slice))
         =l=
         vTechOut(tech, comm, region, year, slice);

* Availability commodity factor UP output equations
eqTechAfcOutUp(tech, region, comm, year, slice)$meqTechAfcOutUp(tech, region, comm, year, slice)..
         vTechOut(tech, comm, region, year, slice)
         =l=
         pTechCact2cout(tech, comm, region, year, slice) *
         pTechAfcUp(tech, comm, region, year, slice) *
*         pYearFraction(year) *
         pTechCap2act(tech) *
         vTechCap(tech, region, year) *  prod(weather$mTechWeatherAfcUp(weather, tech, comm),
            pTechWeatherAfcUp(weather, tech, comm) * pWeather(weather, region, year, slice));

* Availability commodity factor LO input equations
eqTechAfcInpLo(tech, region, comm, year, slice)$meqTechAfcInpLo(tech, region, comm, year, slice)..
         pTechAfcLo(tech, comm, region, year, slice) *
*         pYearFraction(year) *
         pTechCap2act(tech) *
         vTechCap(tech, region, year) *
         pSliceShare(slice)  *  prod(weather$mTechWeatherAfcLo(weather, tech, comm),
            pTechWeatherAfcLo(weather, tech, comm) * pWeather(weather, region, year, slice))
         =l=
         vTechInp(tech, comm, region, year, slice);

* Availability commodity factor UP input equations
eqTechAfcInpUp(tech, region, comm, year, slice)$meqTechAfcInpUp(tech, region, comm, year, slice)..
         vTechInp(tech, comm, region, year, slice)
         =l=
         pTechAfcUp(tech, comm, region, year, slice) *
*         pYearFraction(year) *
         pTechCap2act(tech) *
         vTechCap(tech, region, year) *
         pSliceShare(slice)  *  prod(weather$mTechWeatherAfcUp(weather, tech, comm),
            pTechWeatherAfcUp(weather, tech, comm) * pWeather(weather, region, year, slice));

********************************************************************************
*** Capacity and costs equations
********************************************************************************
Equation
* Capacity equations
eqTechCap(tech, region, year)        Technology capacity
eqTechCapLo(tech, region, year)      Technology capacity lower bound
eqTechCapUp(tech, region, year)      Technology capacity upper bound
eqTechNewCapLo(tech, region, year)   Lower bound on new capacity
eqTechNewCapUp(tech, region, year)   Upper bound on new capacity
eqTechRetiredStock(tech, region, year)    Retirement of existing capacity
eqTechRetiredStockCum(tech, region, year) Cumulative retirement of existing capacity
eqTechRetiredNewCap(tech, region, year)   Retirement of new capacity
eqTechRetLo(tech, region, year)    Lower bound on economic retirement of capacity
eqTechRetUp(tech, region, year)    Upper bound on economic retirement of capacity
eqTechEac(tech, region, year)             Technology Equivalent Annual Cost (EAC)
* Investment equation
eqTechInv(tech, region, year)             Technology overnight investment costs
* Aggregated annual costs
eqTechFixom(tech, region, year)        Technology fixed costs
eqTechVarom(tech, region, year)   Technology variable costs
eqTechRetCost(tech, region, year)  Costs of retired capacity of technology
;

* Capacity equations
eqTechCap(tech, region, year)$mTechSpan(tech, region, year)..
         vTechCap(tech, region, year)
         =e=
         pTechStock(tech, region, year) -
         vTechRetiredStockCum(tech, region, year)$mvTechRetiredStock(tech, region, year) +
         sum(yearp$(mTechNew(tech, region, yearp)
                    and
                    ordYear(year) >= ordYear(yearp)
                    and
                    (ordYear(year) < pTechOlife(tech, region) + ordYear(yearp)
                     or
                     mTechOlifeInf(tech, region)
                     )
                    ),
                 pPeriodLen(yearp) *
                 (vTechNewCap(tech, region, yearp) -
                   sum(yeare$(mvTechRetiredNewCap(tech, region, yearp, yeare)
                              and
                              ordYear(year) >= ordYear(yeare)),
                       vTechRetiredNewCap(tech, region, yearp, yeare)
                       )
                 )
         );

eqTechCapLo(tech, region, year)$mTechCapLo(tech, region, year)..
        vTechCap(tech, region, year) =g= pTechCapLo(tech, region, year);

eqTechCapUp(tech, region, year)$mTechCapUp(tech, region, year)..
        vTechCap(tech, region, year) =l= pTechCapUp(tech, region, year);

eqTechNewCapLo(tech, region, year)$mTechNewCapLo(tech, region, year)..
        vTechNewCap(tech, region, year) =g= pTechNewCapLo(tech, region, year) * pPeriodLen(year);

eqTechNewCapUp(tech, region, year)$mTechNewCapUp(tech, region, year)..
        vTechNewCap(tech, region, year) =l= pTechNewCapUp(tech, region, year) * pPeriodLen(year);


eqTechRetiredNewCap(tech, region, year)$meqTechRetiredNewCap(tech, region, year)..
    sum(yearp$mvTechRetiredNewCap(tech, region, year, yearp),
         vTechRetiredNewCap(tech, region, year, yearp) * pPeriodLen(yearp))
         =l=
         vTechNewCap(tech, region, year) * pPeriodLen(year);

* Cumulative retirement of existing capacity
eqTechRetiredStockCum(tech, region, year)$mvTechRetiredStock(tech, region, year)..
         vTechRetiredStockCum(tech, region, year) =l= pTechStock(tech, region, year);

eqTechRetiredStock(tech, region, year)$mvTechRetiredStock(tech, region, year)..
         vTechRetiredStock(tech, region, year) * pPeriodLen(year)  =e=
         vTechRetiredStockCum(tech, region, year) -
         sum(yearp$mMilestoneNext(yearp, year),
             vTechRetiredStockCum(tech, region, yearp)
          );

eqTechRetUp(tech, region, year)$mTechRetUp(tech, region, year)..
         vTechRetiredStock(tech, region, year)$mvTechRetiredStock(tech, region, year)
         + sum(yearp$mvTechRetiredNewCap(tech, region, year, yearp),
               vTechRetiredNewCap(tech, region, year, yearp))
         =l= pTechRetUp(tech, region, year) * pPeriodLen(year);

eqTechRetLo(tech, region, year)$mTechRetLo(tech, region, year)..
         vTechRetiredStock(tech, region, year)$mvTechRetiredStock(tech, region, year)
         + sum(yearp$mvTechRetiredNewCap(tech, region, year, yearp),
               vTechRetiredNewCap(tech, region, year, yearp))
         =g= pTechRetLo(tech, region, year) * pPeriodLen(year);

* Costs of early retirement
eqTechRetCost(tech, region, year)$mTechRetCost(tech, region, year)..
         vTechRetCost(tech, region, year) =e=
* retired stock
        pTechRetCost(tech, region, year) * vTechRetiredStock(tech, region, year)$mvTechRetiredStock(tech, region, year)
* technology endogenous retirement of new capacity, built in previous periods
* !!! check year & yearp placement/sum in the vTechRetiredNewCap
        + sum((yearp)$mvTechRetiredNewCap(tech, region, yearp, year),
              pTechRetCost(tech, region, year)
              * vTechRetiredNewCap(tech, region, yearp, year)$mvTechRetiredNewCap(tech, region, yearp, year)
              );

* Technology Equivalent Annual Cost (EAC)
eqTechEac(tech, region, year)$mTechEac(tech, region, year)..
         vTechEac(tech, region, year)
         =e=
         sum(yearp$(mTechNew(tech, region, yearp)
                    and
                    ordYear(year) >= ordYear(yearp)
                    and
                    (ordYear(year) < pTechOlife(tech, region) + ordYear(yearp)
                     or
                     mTechOlifeInf(tech, region)
                     )
                    ),
              pTechEac(tech, region, yearp)
              * (
                vTechNewCap(tech, region, yearp)
                - sum(yeare$(mvTechRetiredNewCap(tech, region, yearp, yeare)
                             and
                             ordYear(year) >= ordYear(yeare)
                             ),
                       vTechRetiredNewCap(tech, region, yearp, yeare)
                       )
                )
              );

* Investment equation
eqTechInv(tech, region, year)$mTechInv(tech, region, year)..
        vTechInv(tech, region, year)
        =e=
*        pYearFraction(year) *
        pTechInvcost(tech, region, year) * vTechNewCap(tech, region, year);


$ontext
* Annual O&M costs
eqTechOMCost(tech, region, year)$mTechOMCost(tech, region, year)..
        vTechOMCost(tech, region, year)
        =e=
*        pYearFraction(year) *
        pTechFixom(tech, region, year) * vTechCap(tech, region, year) +
        sum(slice$mTechSlice(tech, slice),
            pTechVarom(tech, region, year, slice)
            * pSliceWeight(year, slice)
            * vTechAct(tech, region, year, slice) +
            sum(comm$mTechInpComm(tech, comm),
                pTechCvarom(tech, comm, region, year, slice)
                * pSliceWeight(year, slice)
                * vTechInp(tech, comm, region, year, slice)
            )
            +
            sum(comm$mTechOutComm(tech, comm),
                pTechCvarom(tech, comm, region, year, slice)
                * pSliceWeight(year, slice)
                * vTechOut(tech, comm, region, year, slice)
            )
            +
            sum(comm$mvTechAOut(tech, comm, region, year, slice),
                pTechAvarom(tech, comm, region, year, slice)
                * pSliceWeight(year, slice)
                * vTechAOut(tech, comm, region, year, slice)
            )
            +
            sum(comm$mvTechAInp(tech, comm, region, year, slice),
                pTechAvarom(tech, comm, region, year, slice)
                * pSliceWeight(year, slice)
                * vTechAInp(tech, comm, region, year, slice)
            )
         );
*         / pYearFraction(year) ;
$offtext
* Fixed O&M costs
*mTechFixom(tech, region, year) = mTechSpan(tech, region, year);
eqTechFixom(tech, region, year)$mTechFixom(tech, region, year)..
        vTechFixom(tech, region, year)
        =e=
        pTechFixom(tech, region, year) * vTechCap(tech, region, year);

* Variable O&M costs
*mTechVarom(tech, region, year) = mTechSpan(tech, region, year);
eqTechVarom(tech, region, year)$mTechVarom(tech, region, year)..
        vTechVarom(tech, region, year)
        =e=
        sum(slice$mTechSlice(tech, slice),
            pTechVarom(tech, region, year, slice)
            * pSliceWeight(year, slice)
            * vTechAct(tech, region, year, slice)
            +
            sum(comm$mTechInpComm(tech, comm),
                pTechCvarom(tech, comm, region, year, slice)
                * pSliceWeight(year, slice)
                * vTechInp(tech, comm, region, year, slice))
            +
            sum(comm$mTechOutComm(tech, comm),
                pTechCvarom(tech, comm, region, year, slice)
                * pSliceWeight(year, slice)
                * vTechOut(tech, comm, region, year, slice))
            +
            sum(comm$mvTechAOut(tech, comm, region, year, slice),
                pTechAvarom(tech, comm, region, year, slice)
                * pSliceWeight(year, slice)
                * vTechAOut(tech, comm, region, year, slice))
            +
            sum(comm$mvTechAInp(tech, comm, region, year, slice),
                pTechAvarom(tech, comm, region, year, slice)
                * pSliceWeight(year, slice)
                * vTechAInp(tech, comm, region, year, slice))
        );


**************************************
** Supply
**************************************
Equation
eqSupAvaUp(sup, comm, region, year, slice)  Supply availability upper bound
eqSupAvaLo(sup, comm, region, year, slice)  Supply availability lower bound
eqSupReserve(sup, comm, region)             Cumulative supply (use of reserve)
eqSupReserveUp(sup, comm, region)           Cumulative supply upper constraint
eqSupReserveLo(sup, comm, region)           Cumulative supply lower constraint
eqSupCost(sup, region, year)                Supply costs
;

eqSupAvaUp(sup, comm, region, year, slice)$mSupAvaUp(sup, comm, region, year, slice)..
         vSupOut(sup, comm, region, year, slice)
         =l=
         pSupAvaUp(sup, comm, region, year, slice)
         * prod(weather$mSupWeatherUp(weather, sup),
                pSupWeatherUp(weather, sup)
                * pWeather(weather, region, year, slice));

eqSupAvaLo(sup, comm, region, year, slice)$meqSupAvaLo(sup, comm, region, year, slice)..
         vSupOut(sup, comm, region, year, slice)
         =g=
         pSupAvaLo(sup, comm, region, year, slice)
         * prod(weather$mSupWeatherLo(weather, sup),
                pSupWeatherLo(weather, sup)
                * pWeather(weather, region, year, slice));

eqSupReserve(sup, comm, region)$mvSupReserve(sup, comm, region)..
         vSupReserve(sup, comm, region)
         =e=
         sum((year, slice)$mSupAva(sup, comm, region, year, slice),
             pPeriodLen(year) * pSliceWeight(year, slice)
             * vSupOut(sup, comm, region, year, slice)
*              / pYearFraction(year)
         );

eqSupReserveUp(sup, comm, region)$mSupReserveUp(sup, comm, region)..
         vSupReserve(sup, comm, region) =l= pSupReserveUp(sup, comm, region);

eqSupReserveLo(sup, comm, region)$meqSupReserveLo(sup, comm, region)..
         vSupReserve(sup, comm, region) =g= pSupReserveLo(sup, comm, region);

eqSupCost(sup, region, year)$mvSupCost(sup, region, year)..
         vSupCost(sup, region, year)
         =e=
         sum((comm, slice)$mSupAva(sup, comm, region, year, slice),
             pSupCost(sup, comm, region, year, slice)
             * pSliceWeight(year, slice)
             * vSupOut(sup, comm, region, year, slice));

**************************************
** Demand
**************************************
Equation
eqDemInp(comm, region, year, slice)  Demand equation
;

eqDemInp(comm, region, year, slice)$mvDemInp(comm, region, year, slice)..
         vDemInp(comm, region, year, slice)
         =e=
         sum(dem$mDemComm(dem, comm), pDemand(dem, comm, region, year, slice));

********************************************************************************
** Emission & Aggregating commodity equation
********************************************************************************
Equation
eqAggOutTot(comm, region, year, slice)         Aggregating-commodity output (weighted)
eqEmsFuelTot(comm, region, year, slice)        Total emissions from commodity consumption (weighted)
;

eqAggOutTot(comm, region, year, slice)$mAggOut(comm, region, year, slice)..
         vAggOutTot(comm, region, year, slice)
         =e=
         sum(commp$mAggregateFactor(comm, commp),
             pAggregateFactor(comm, commp)
             * sum(slicep$(mvOutTot(comm, region, year, slicep)
                           and
                           mSliceParentChildE(slice, slicep)
                           and mCommSlice(commp, slicep)
                           ),
                    vOutTot(commp, region, year, slicep)$mvOutTot(commp, region, year, slicep)
                    )
            );

eqEmsFuelTot(comm, region, year, slice)$mEmsFuelTot(comm, region, year, slice)..
     vEmsFuelTot(comm, region, year, slice)
         =e=
         sum(commp$(pEmissionFactor(comm, commp) > 0),
             pEmissionFactor(comm, commp)
             * sum(tech$mTechInpComm(tech, commp),
                   pTechEmisComm(tech, commp)
*                   * pSliceWeight(year, slice)
                   * sum(slicep$mCommSliceOrParent(comm, slice, slicep),
*                         pSliceWeight(year, slicep) *
                         vTechInp(tech, commp, region, year, slicep)$mTechEmsFuel(
                                  tech, comm, commp, region, year, slicep)
                         )
                  )
           );
*          ) * pSliceWeight(year, slice);

********************************************************************************
** Storage
********************************************************************************
********************************************************************************
*** Input & Output
********************************************************************************
Equations
*eqStorageStore - rename to eqStorageLevel
eqStorageStore(stg, comm, region, year, slicep, slice)  Storage level
*eqStorageStorePS(stg, comm, region, year, slice)  Storage level of parent-slice storage cycle
*eqStorageStoreFY(stg, comm, region, year, slice)  Storage level of full-year storage cycle
eqStorageAfLo(stg, comm, region, year, slice)   Storage availability factor lower
eqStorageAfUp(stg, comm, region, year, slice)   Storage availability factor upper
*eqStorageClean(stg, comm, region, year, slice)  Storage output vs level
eqStorageClear(stg, comm, region, year, slice)  Storage output vs level
eqStorageAInp(stg, comm, region, year, slice)   Storage aux-commodity input
eqStorageAOut(stg, comm, region, year, slice)   Storage aux-commodity output
eqStorageInpUp(stg, comm, region, year, slice)  Storage input upper constraint
eqStorageInpLo(stg, comm, region, year, slice)  Storage input lower constraint
eqStorageOutUp(stg, comm, region, year, slice)  Storage output upper constraint
eqStorageOutLo(stg, comm, region, year, slice)  Storage output lower constraint
;

eqStorageAInp(stg, comm, region, year, slice)$mvStorageAInp(stg, comm, region, year, slice)..
  vStorageAInp(stg, comm, region, year, slice) =e=
    sum(commp$mStorageComm(stg, commp),
        (pStorageStg2AInp(stg, comm, region, year, slice)
        * vStorageStore(stg, commp, region, year, slice)
        )$mStorageStg2AInp(stg, comm, region, year, slice)
      + (pStorageCinp2AInp(stg, comm, region, year, slice)
          * vStorageInp(stg, commp, region, year, slice)
        )$mStorageCinp2AInp(stg, comm, region, year, slice)
      + (pStorageCout2AInp(stg, comm, region, year, slice)
          * vStorageOut(stg, commp, region, year, slice)
        )$mStorageCout2AInp(stg, comm, region, year, slice)
      + (pStorageCap2AInp(stg, comm, region, year, slice)
           * vStorageCap(stg, region, year)
        )$mStorageCap2AInp(stg, comm, region, year, slice)
      + (pStorageNCap2AInp(stg, comm, region, year, slice)
           * vStorageNewCap(stg, region, year)
        )$mStorageNCap2AInp(stg, comm, region, year, slice)
    );

eqStorageAOut(stg, comm, region, year, slice)$mvStorageAOut(stg, comm, region, year, slice)..
  vStorageAOut(stg, comm, region, year, slice)
  =e=
  sum(commp$mStorageComm(stg, commp),
      (pStorageStg2AOut(stg, comm, region, year, slice)
       * vStorageStore(stg, commp, region, year, slice)
      )$mStorageStg2AOut(stg, comm, region, year, slice)
      +
      (pStorageCinp2AOut(stg, comm, region, year, slice)
       * vStorageInp(stg, commp, region, year, slice)
      )$mStorageCinp2AOut(stg, comm, region, year, slice)
      +
      (pStorageCout2AOut(stg, comm, region, year, slice)
       * vStorageOut(stg, commp, region, year, slice)
      )$mStorageCout2AOut(stg, comm, region, year, slice)
      +
      (pStorageCap2AOut(stg, comm, region, year, slice)
       * vStorageCap(stg, region, year)
      )$mStorageCap2AOut(stg, comm, region, year, slice)
      +
      (pStorageNCap2AOut(stg, comm, region, year, slice)
       * vStorageNewCap(stg, region, year)
      )$mStorageNCap2AOut(stg, comm, region, year, slice)
);

* slicep == slice[-1]
eqStorageStore(stg, comm, region, year, slicep, slice)$meqStorageStore(stg, comm, region, year, slicep, slice)..
  vStorageStore(stg, comm, region, year, slice)
  =e=
  pStorageCharge(stg, comm, region, year, slice)
  + (pStorageNCap2Stg(stg, comm, region, year, slice)
     * vStorageNewCap(stg, region, year)
    )$mStorageNew(stg, region, year)
  +
*  sum(slicep$(mCommSlice(comm, slicep) and mSliceNext(slicep, slice)),
  pStorageInpEff(stg, comm, region, year, slicep)
    * vStorageInp(stg, comm, region, year, slicep)
  + (pStorageStgEff(stg, comm, region, year, slice) ** pSliceShare(slice))
    * vStorageStore(stg, comm, region, year, slicep)
  - vStorageOut(stg, comm, region, year, slicep)
    / pStorageOutEff(stg, comm, region, year, slicep)
*        )
;

eqStorageAfLo(stg, comm, region, year, slice)$meqStorageAfLo(stg, comm, region, year, slice)..
    vStorageStore(stg, comm, region, year, slice)
    =g=
    pStorageAfLo(stg, region, year, slice) *
      pStorageCap2stg(stg) * vStorageCap(stg, region, year)
      * prod(weather$mStorageWeatherAfLo(weather, stg),
             pStorageWeatherAfLo(weather, stg)
             * pWeather(weather, region, year, slice));

eqStorageAfUp(stg, comm, region, year, slice)$meqStorageAfUp(stg, comm, region, year, slice)..
    vStorageStore(stg, comm, region, year, slice)
    =l=
    pStorageAfUp(stg, region, year, slice) *
      pStorageCap2stg(stg) * vStorageCap(stg, region, year)
      * prod(weather$mStorageWeatherAfUp(weather, stg),
             pStorageWeatherAfUp(weather, stg)
             * pWeather(weather, region, year, slice));

eqStorageClear(stg, comm, region, year, slice)$mvStorageStore(stg, comm, region, year, slice)..
  vStorageOut(stg, comm, region, year, slice) / pStorageOutEff(stg, comm, region, year, slice)
*   * pStorageCap2stg(stg)
   =l=
   vStorageStore(stg, comm, region, year, slice);

* Input constraints
eqStorageInpUp(stg, comm, region, year, slice)$meqStorageInpUp(stg, comm, region, year, slice)..
  vStorageInp(stg, comm, region, year, slice) =l=
*    pStorageCap2stg(stg) *
    vStorageCap(stg, region, year) * pStorageCinpUp(stg, comm, region, year, slice)
*         * pSliceShare(slice) *
    * prod(weather$mStorageWeatherCinpUp(weather, stg),
           pStorageWeatherCinpUp(weather, stg)
           * pWeather(weather, region, year, slice));

eqStorageInpLo(stg, comm, region, year, slice)$meqStorageInpLo(stg, comm, region, year, slice)..
  vStorageInp(stg, comm, region, year, slice) =g=
*    pStorageCap2stg(stg) *
    vStorageCap(stg, region, year) * pStorageCinpLo(stg, comm, region, year, slice)
*    * pSliceShare(slice)
    * prod(weather$mStorageWeatherCinpLo(weather, stg),
           pStorageWeatherCinpLo(weather, stg)
           * pWeather(weather, region, year, slice));

* Output constraints
eqStorageOutUp(stg, comm, region, year, slice)$meqStorageOutUp(stg, comm, region, year, slice)..
  vStorageOut(stg, comm, region, year, slice) =l=
*    pStorageCap2stg(stg) *
    vStorageCap(stg, region, year) * pStorageCoutUp(stg, comm, region, year, slice)
*    * pSliceShare(slice)
    * prod(weather$mStorageWeatherCoutUp(weather, stg),
           pStorageWeatherCoutUp(weather, stg)
           * pWeather(weather, region, year, slice));

eqStorageOutLo(stg, comm, region, year, slice)$meqStorageOutLo(stg, comm, region, year, slice)..
  vStorageOut(stg, comm, region, year, slice)  =g=
*    pStorageCap2stg(stg) *
    vStorageCap(stg, region, year) * pStorageCoutLo(stg, comm, region, year, slice)
*    * pSliceShare(slice)
    * prod(weather$mStorageWeatherCoutLo(weather, stg),
           pStorageWeatherCoutLo(weather, stg)
           * pWeather(weather, region, year, slice));

********************************************************************************
*** Capacity and costs for storage
********************************************************************************
Equation
* Capacity equations
eqStorageCap(stg, region, year)       Storage capacity
eqStorageCapLo(stg, region, year)     Storage capacity lower bound
eqStorageCapUp(stg, region, year)     Storage capacity upper bound
eqStorageNewCapLo(stg, region, year)  Storage new capacity lower bound
eqStorageNewCapUp(stg, region, year)  Storage new capacity upper bound
* Investment equation
eqStorageInv(stg, region, year)       Storage overnight investment costs
eqStorageEac(stg, region, year)       Storage equivalent annual cost
* Aggregated annual costs
* eqStorageCost(stg, region, year)      Storage total costs
eqStorageFixom(stg, region, year)     Storage fixed costs
eqStorageVarom(stg, region, year)    Storage variable costs
* Constrain capacity
;

* Capacity equation
eqStorageCap(stg, region, year)$mStorageSpan(stg, region, year)..
         vStorageCap(stg, region, year)
         =e=
         pStorageStock(stg, region, year) +
         sum(yearp$(ordYear(year) >= ordYear(yearp)
                    and
                    (mStorageOlifeInf(stg, region)
                      or
                     ordYear(year) < pStorageOlife(stg, region) + ordYear(yearp)
                     )
                    and
                     mStorageNew(stg, region, yearp)
                 ),
                 pPeriodLen(yearp) *
                 vStorageNewCap(stg, region, yearp)
         );

eqStorageCapLo(stg, region, year)$mStorageCapLo(stg, region, year)..
         vStorageCap(stg, region, year) =g= pStorageCapLo(stg, region, year);

eqStorageCapUp(stg, region, year)$mStorageCapUp(stg, region, year)..
          vStorageCap(stg, region, year) =l= pStorageCapUp(stg, region, year);

eqStorageNewCapLo(stg, region, year)$mStorageNewCapLo(stg, region, year)..
          vStorageNewCap(stg, region, year) =g= pStorageNewCapLo(stg, region, year) * pPeriodLen(year);

eqStorageNewCapUp(stg, region, year)$mStorageNewCapUp(stg, region, year)..
          vStorageNewCap(stg, region, year) =l= pStorageNewCapUp(stg, region, year) * pPeriodLen(year);

* Investment equation
eqStorageInv(stg, region, year)$mStorageNew(stg, region, year)..
         vStorageInv(stg, region, year)
         =e=
         pStorageInvcost(stg, region, year) *
         vStorageNewCap(stg, region, year);

* EAC equation
eqStorageEac(stg, region, year)$mStorageEac(stg, region, year)..
         vStorageEac(stg, region, year)
         =e=
         sum(yearp$(mStorageNew(stg, region, yearp)
                    and ordYear(year) >= ordYear(yearp)
                    and (mStorageOlifeInf(stg, region)
                         or ordYear(year) < pStorageOlife(stg, region) + ordYear(yearp)
                         )
                    and pStorageInvcost(stg, region, yearp) <> 0
                    ),
*                  pYearFraction(year) *
            pStorageEac(stg, region, yearp)
*            * pPeriodLen(yearp)
            * vStorageNewCap(stg, region, yearp)
         );

$ontext
* Fixed and Variable O&M costs
eqStorageCost(stg, region, year)$mStorageOMCost(stg, region, year)..
         vStorageOMCost(stg, region, year)
         =e=
*         pYearFraction(year) *
         pStorageFixom(stg, region, year) * vStorageCap(stg, region, year)
         +
         sum(comm$mStorageComm(stg, comm),
             sum(slice$mCommSlice(comm, slice),
                 pStorageCostInp(stg, region, year, slice)
                    * pSliceWeight(year, slice)
                    * vStorageInp(stg, comm, region, year, slice)
                 + pStorageCostOut(stg, region, year, slice)
                    * pSliceWeight(year, slice)
                    * vStorageOut(stg, comm, region, year, slice)
                 + pStorageCostStore(stg, region, year, slice)
                    * pSliceWeight(year, slice)
                    * vStorageStore(stg, comm, region, year, slice)
             )
         );
$offtext
* Storage fixed costs
*!!!eqStorageFixom(stg, region, year)$mStorageFixom(stg, region, year)..
eqStorageFixom(stg, region, year)$mStorageFixom(stg, region, year)..
         vStorageFixom(stg, region, year)
         =e=
         pStorageFixom(stg, region, year) * vStorageCap(stg, region, year);

* Storage variable costs
eqStorageVarom(stg, region, year)$mStorageVarom(stg, region, year)..
          vStorageVarom(stg, region, year)
          =e=
          sum(comm$mStorageComm(stg, comm),
             sum(slice$mCommSlice(comm, slice),
                 pStorageCostInp(stg, region, year, slice)
                    * pSliceWeight(year, slice)
                    * vStorageInp(stg, comm, region, year, slice)
                 + pStorageCostOut(stg, region, year, slice)
                    * pSliceWeight(year, slice)
                    * vStorageOut(stg, comm, region, year, slice)
                 + pStorageCostStore(stg, region, year, slice)
                    * pSliceWeight(year, slice)
                    * vStorageStore(stg, comm, region, year, slice)
             )
         );

********************************************************************************
** Interregional and ROW Trade equations
********************************************************************************
********************************************************************************
*** Trade Flows
********************************************************************************
equation
eqImportTot(comm, region, year, slice)             Import equation (Ir & ROW)
eqExportTot(comm, region, year, slice)             Export equation (Ir & ROW)
eqTradeFlowUp(trade, comm, region, region, year, slice)   Trade upper bound
eqTradeFlowLo(trade, comm, region, region, year, slice)   Trade lower bound
*eqCostTrade(region, year)                         Total trade costs
*eqCostRowTrade(region, year)                      Costs of trade with the Rest of the World (ROW)
* eqCostIrTrade(region, year)                       Costs of import
eqExportRowUp(expp, comm, region, year, slice)    Export to the ROW upper constraint
eqExportRowLo(expp, comm, region, year, slice)    Export to the ROW lower constraint
eqExportRowCum(expp, comm)                 Cumulative export to the ROW
eqExportRowResUp(expp, comm)                      Cumulative export to the ROW upper constraint
eqExportRowCost(expp, region, year)               Export to the ROW costs equation
eqImportRowUp(imp, comm, region, year, slice)     Import from the ROW upper constraint
eqImportRowLo(imp, comm, region, year, slice)     Import of the ROW lower constraint
eqImportRowCum(imp, comm)                 Cumulative import from the ROW
eqImportRowResUp(imp, comm)                       Cumulative import from the ROW upper constraint
eqImportRowCost(imp, region, year)                Import from the ROW costs equation
eqTradeCap(trade, year)                           Trade capacity
eqTradeCapLo(trade, year)                         Trade capacity lower bound
eqTradeCapUp(trade, year)                         Trade capacity upper bound
eqTradeNewCapLo(trade, year)                      Trade new capacity lower bound
eqTradeNewCapUp(trade, year)                      Trade new capacity upper bound
eqTradeCapFlow(trade, comm, year, slice)          Trade capacity to activity
eqTradeInv(trade, region, year)                   Trade overnight investment costs
eqTradeEac(trade, region, year)                   Trade equivalent annual costs
eqTradeFixom(trade, region, year)
eqImportIrCost(trade, region, year)               Interregional trade import costs
eqExportIrCost(trade, region, year)               Interregional trade export costs
;

$ontext
* rewritten (dropped sum(slicep$mCommSliceOrParent...)
* because timeframe of all one-commodity processes (sup, dem, exp, imp, ir-trd)
* is fixed to the timeframe of commodity
eqImportTot(comm, dst, year, slice)$mImport(comm, dst, year, slice)..
  vImportTot(comm, dst, year, slice) =e=
     sum(slicep$mCommSliceOrParent(comm, slice, slicep),
         sum(trade$mTradeComm(trade, comm),
             sum(src$mTradeRoutes(trade, src, dst),
                 (pTradeIrEff(trade, src, dst, year, slicep)
                  * vTradeIr(trade, comm, src, dst, year, slicep)
                  )$mvTradeIr(trade, comm, src, dst, year, slicep)
                 )
            )
         )
     +
     sum(slicep$mCommSliceOrParent(comm, slice, slicep),
         sum(imp$mImpComm(imp, comm),
             vImportRow(imp, comm, dst, year, slicep)$mImportRow(imp, comm, dst, year, slicep)
         )
     );
$offtext

eqImportTot(comm, dst, year, slice)$mImport(comm, dst, year, slice)..
  vImportTot(comm, dst, year, slice) =e=
    sum(trade$mTradeComm(trade, comm),
      sum(src$mTradeRoutes(trade, src, dst),
          (pTradeIrEff(trade, src, dst, year, slice)
           * vTradeIr(trade, comm, src, dst, year, slice)
          )$mvTradeIr(trade, comm, src, dst, year, slice)
      )
    )
*    ) * pSliceWeight(year, slice)
    +
    sum(imp$mImpComm(imp, comm),
          vImportRow(imp, comm, dst, year, slice)$mImportRow(imp, comm, dst, year, slice)
    );
*    ) * pSliceWeight(year, slice);

$ontext
* rewritten (dropped sum(slicep$mCommSliceOrParent...)
* because timeframe of all one-commodity processes (sup, dem, exp, imp, ir-trd)
* is fixed to the timeframe of commodity

eqExportTot(comm, src, year, slice)$mExport(comm, src, year, slice)..
  vExportTot(comm, src, year, slice)
  =e=
  sum(slicep$mCommSliceOrParent(comm, slice, slicep),
      sum(trade$mTradeComm(trade, comm),
          sum(dst$mTradeRoutes(trade, src, dst),
              vTradeIr(trade, comm, src, dst, year, slicep)$mvTradeIr(trade, comm, src, dst, year, slicep)
          )
      )
  ) +
  sum(slicep$mCommSliceOrParent(comm, slice, slicep),
      sum(expp$mExpComm(expp, comm),
          vExportRow(expp, comm, src, year, slicep)$mExportRow(expp, comm, src, year, slicep)
      )
  );
$offtext

eqExportTot(comm, src, year, slice)$mExport(comm, src, year, slice)..

  vExportTot(comm, src, year, slice)
  =e=
  sum(trade$mTradeComm(trade, comm),
      sum(dst$mTradeRoutes(trade, src, dst),
          vTradeIr(trade, comm, src, dst, year, slice)$mvTradeIr(trade, comm, src, dst, year, slice)
      )
  )
*  ) * pSliceWeight(year, slice)
  +
  sum(expp$mExpComm(expp, comm),
      vExportRow(expp, comm, src, year, slice)$mExportRow(expp, comm, src, year, slice)
  );
*  ) * pSliceWeight(year, slice);

eqTradeFlowUp(trade, comm, src, dst, year, slice)$meqTradeFlowUp(trade, comm, src, dst, year, slice)..
      vTradeIr(trade, comm, src, dst, year, slice) =l= pTradeIrUp(trade, src, dst, year, slice);

eqTradeFlowLo(trade, comm, src, dst, year, slice)$meqTradeFlowLo(trade, comm, src, dst, year, slice)..
      vTradeIr(trade, comm, src, dst, year, slice) =g= pTradeIrLo(trade, src, dst, year, slice);

$ontext
eqCostTrade(region, year)$mvTradeCost(region, year)..
  vTradeCost(region, year)
  =e=
  vTradeRowCost(region, year)$mvTradeRowCost(region, year)
  + vTradeIrCost(region, year)$mvTradeIrCost(region, year);
*$offtext

eqCostRowTrade(region, year)$mvTradeRowCost(region, year)..
  vTradeRowCost(region, year)
  =e=
* Row
  sum((imp, comm, slice)$mImportRow(imp, comm, region, year, slice),
      pImportRowPrice(imp, region, year, slice) * pSliceWeight(year, slice)
      * vImportRow(imp, comm, region, year, slice)
  ) -
  sum((expp, comm, slice)$mExportRow(expp, comm, region, year, slice),
      pExportRowPrice(expp, region, year, slice) * pSliceWeight(year, slice)
      * vExportRow(expp, comm, region, year, slice)
  );
*$offtext

eqCostIrTrade(region, year)$mvTradeIrCost(region, year)..
  vTradeIrCost(region, year)
  =e=
* * Fixed O&M of stock (!!! check mTradeSpan when not mTradeCapacityVariable)
*   sum(trade$mTradeSpan(trade, year),
*       pTradeFixom(trade, year) * pTradeStock(trade, year) * pPeriodLen(year)
*   )
*   +
* * Fixed O&M of new installations
*   sum(trade$(mTradeSpan(trade, year) and mTradeCapacityVariable(trade)),
*       pTradeFixom(trade, year) * (vTradeCap(trade, year) - pTradeStock(trade, year)) * pPeriodLen(year)
*   )
* Fixed O&M of trade
  sum(trade$mTradeSpan(trade, year),
      pTradeFixom(trade, region, year) * vTradeCap(trade, year) * pPeriodLen(year)
  )
  +
* Eac
  sum(trade$mTradeEac(trade, region, year),
      vTradeEac(trade, region, year) * pPeriodLen(year)
  )
* Import (IR)
  + sum((trade, src)$mTradeRoutes(trade, src, region),
        sum(comm$mTradeComm(trade, comm),
            sum(slice$mTradeSlice(trade, slice),
                ((pTradeIrCost(trade, src, region, year, slice)
                   + pTradeIrMarkup(trade, src, region, year, slice)
                  ) * vTradeIr(trade, comm, src, region, year, slice) * pSliceWeight(year, slice) * pPeriodLen(year)
                )$mvTradeIr(trade, comm, src, region, year, slice)
            )
        )
    )
* Export (IR)
  - sum((trade, dst)$mTradeRoutes(trade, region, dst),
        sum(comm$mTradeComm(trade, comm),
            sum(slice$mTradeSlice(trade, slice),
                ((pTradeIrCost(trade, region, dst, year, slice)
                  + pTradeIrMarkup(trade, region, dst, year, slice)
                  )* vTradeIr(trade, comm, region, dst, year, slice) * pSliceWeight(year, slice) * pPeriodLen(year)
                 )$mvTradeIr(trade, comm, region, dst, year, slice)
            )
        )
    );
*$offtext

* Rewritten IR-trade costs equation activity (non-capital) costs
eqCostIrTrade(region, year)$mvTradeIrCost(region, year)..
  vTradeIrCost(region, year)
  =e=
* Import (IR)
  + sum((trade, src)$mTradeRoutes(trade, src, region),
        sum(comm$mTradeComm(trade, comm),
            sum(slice$mTradeSlice(trade, slice),
                ((pTradeIrCost(trade, src, region, year, slice)
                   + pTradeIrMarkup(trade, src, region, year, slice)
                  ) * vTradeIr(trade, comm, src, region, year, slice) * pSliceWeight(year, slice)
                )$mvTradeIr(trade, comm, src, region, year, slice)
            )
        )
    )
* Export (IR)
  - sum((trade, dst)$mTradeRoutes(trade, region, dst),
        sum(comm$mTradeComm(trade, comm),
            sum(slice$mTradeSlice(trade, slice),
                ((pTradeIrCost(trade, region, dst, year, slice)
                  + pTradeIrMarkup(trade, region, dst, year, slice)
                  ) * vTradeIr(trade, comm, region, dst, year, slice) * pSliceWeight(year, slice)
                 )$mvTradeIr(trade, comm, region, dst, year, slice)
            )
        )
    );
$offtext

* Import (IR)
eqImportIrCost(trade, region, year)$mImportIrCost(trade, region, year)..
  vImportIrCost(trade, region, year)
  =e=
    sum((src)$mTradeRoutes(trade, src, region),
        sum(comm$mTradeComm(trade, comm),
            sum(slice$mTradeSlice(trade, slice),
                ((pTradeIrCost(trade, src, region, year, slice)
                   + pTradeIrMarkup(trade, src, region, year, slice)
                  ) * vTradeIr(trade, comm, src, region, year, slice) * pSliceWeight(year, slice)
                )$mvTradeIr(trade, comm, src, region, year, slice)
            )
        )
    );

* Export (IR)
eqExportIrCost(trade, region, year)$mExportIrCost(trade, region, year)..
  vExportIrCost(trade, region, year)
  =e=
    - sum((dst)$mTradeRoutes(trade, region, dst),
        sum(comm$mTradeComm(trade, comm),
            sum(slice$mTradeSlice(trade, slice),
                ((pTradeIrCost(trade, region, dst, year, slice)
                  + pTradeIrMarkup(trade, region, dst, year, slice)
                  ) * vTradeIr(trade, comm, region, dst, year, slice) * pSliceWeight(year, slice)
                 )$mvTradeIr(trade, comm, region, dst, year, slice)
            )
        )
    );

eqExportRowUp(expp, comm, region, year, slice)$mExportRowUp(expp, comm, region, year, slice)..
  vExportRow(expp, comm, region, year, slice)
  =l=
  pExportRowUp(expp, region, year, slice);

eqExportRowLo(expp, comm, region, year, slice)$meqExportRowLo(expp, comm, region, year, slice)..
  vExportRow(expp, comm, region, year, slice)
  =g=
  pExportRowLo(expp, region, year, slice);

eqExportRowCum(expp, comm)$mExpComm(expp, comm)..
  vExportRowCum(expp, comm)
  =e=
  sum((region, year, slice)$mExportRow(expp, comm, region, year, slice),
      pPeriodLen(year) * pSliceWeight(year, slice)
      * vExportRow(expp, comm, region, year, slice)
  );

eqExportRowResUp(expp, comm)$mExportRowCumUp(expp, comm)..
  vExportRowCum(expp, comm) =l= pExportRowRes(expp);

*
eqExportRowCost(expp, region, year)$mExportRowCost(expp, region, year)..
  vExportRowCost(expp, region, year)
  =e=
  - sum((comm, slice)$mExportRow(expp, comm, region, year, slice),
      pExportRowPrice(expp, region, year, slice) * pSliceWeight(year, slice)
      * vExportRow(expp, comm, region, year, slice)
  );

eqImportRowUp(imp, comm, region, year, slice)$mImportRowUp(imp, comm, region, year, slice)..
  vImportRow(imp, comm, region, year, slice)  =l= pImportRowUp(imp, region, year, slice);

eqImportRowLo(imp, comm, region, year, slice)$meqImportRowLo(imp, comm, region, year, slice)..
  vImportRow(imp, comm, region, year, slice)  =g= pImportRowLo(imp, region, year, slice);

eqImportRowCum(imp, comm)$mImpComm(imp, comm)..
  vImportRowCum(imp, comm)
  =e=
  sum((region, year, slice)$mImportRow(imp, comm, region, year, slice),
      pPeriodLen(year) * pSliceWeight(year, slice)
      * vImportRow(imp, comm, region, year, slice)
  );

eqImportRowResUp(imp, comm)$mImportRowCumUp(imp, comm)..
  vImportRowCum(imp, comm) =l= pImportRowRes(imp);

* mapping? $mImpComm(imp, region)
eqImportRowCost(imp, region, year)$mImportRowCost(imp, region, year)..
  vImportRowCost(imp, region, year)
  =e=
  sum((comm, slice)$mImportRow(imp, comm, region, year, slice),
      pImportRowPrice(imp, region, year, slice) * pSliceWeight(year, slice)
      * vImportRow(imp, comm, region, year, slice)
  );

********************************************************************************
*** Trade IR capacity equations
********************************************************************************

* Capacity to activity constraint
eqTradeCapFlow(trade, comm, year, slice)$meqTradeCapFlow(trade, comm, year, slice)..
         pSliceShare(slice) * pTradeCap2Act(trade) * vTradeCap(trade, year) =g=
                 sum((src, dst)$mvTradeIr(trade, comm, src, dst, year, slice),
                     vTradeIr(trade, comm, src, dst, year, slice)
                     );

* Capacity equation
eqTradeCap(trade, year)$mTradeSpan(trade, year)..
         vTradeCap(trade, year)
         =e=
         pTradeStock(trade, year) +
         sum(yearp$(mTradeNew(trade, yearp) and  ordYear(year) >= ordYear(yearp) and
                    (ordYear(year) < pTradeOlife(trade) + ordYear(yearp) or mTradeOlifeInf(trade))),
             pPeriodLen(yearp) *
             vTradeNewCap(trade, yearp)
             );

eqTradeCapLo(trade, year)$mTradeCapLo(trade, year)..
        vTradeCap(trade, year) =g= pTradeCapLo(trade, year);

eqTradeCapUp(trade, year)$mTradeCapUp(trade, year)..
        vTradeCap(trade, year) =l= pTradeCapUp(trade, year);

eqTradeNewCapLo(trade, year)$mTradeNewCapLo(trade, year)..
        vTradeNewCap(trade, year) * pPeriodLen(year) =g= pTradeNewCapLo(trade, year);

eqTradeNewCapUp(trade, year)$mTradeNewCapUp(trade, year)..
        vTradeNewCap(trade, year) * pPeriodLen(year) =l= pTradeNewCapUp(trade, year);

* Investment equation
eqTradeInv(trade, region, year)$mTradeInv(trade, region, year)..
        vTradeInv(trade, region, year) =e=
                  pTradeInvcost(trade, region, year) * vTradeNewCap(trade, year);

* EAC equation
eqTradeEac(trade, region, year)$mTradeEac(trade, region, year)..
         vTradeEac(trade, region, year)
         =e=
         sum(yearp$(mTradeNew(trade, yearp) and  ordYear(year) >= ordYear(yearp) and
            (ordYear(year) < pTradeOlife(trade) + ordYear(yearp) or mTradeOlifeInf(trade))),
                pTradeEac(trade, region, yearp) * vTradeNewCap(trade, yearp));

* Fixed O&M costs
eqTradeFixom(trade, region, year)$mTradeFixom(trade, region, year)..
         vTradeFixom(trade, region, year)
         =e=
         pTradeFixom(trade, region, year) * vTradeCap(trade, year);

********************************************************************************
*** Auxiliary input & output equations
********************************************************************************
equation
eqTradeIrAInp(trade, comm, region, year, slice) Trade auxiliary commodity input
eqTradeIrAOut(trade, comm, region, year, slice) Trade auxiliary commodity output
eqTradeIrAInpTot(comm, region, year, slice) Trade auxiliary commodity input
eqTradeIrAOutTot(comm, region, year, slice) Trade auxiliary commodity output
;

eqTradeIrAInp(trade, comm, region, year, slice)$mvTradeIrAInp(trade, comm, region, year, slice)..
  vTradeIrAInp(trade, comm, region, year, slice)
  =e=
  sum(dst$mTradeIrCsrc2Ainp(trade, comm, region, dst, year, slice),
      pTradeIrCsrc2Ainp(trade, comm, region, dst, year, slice)
      * sum(commp$mTradeComm(trade, commp),
            vTradeIr(trade, commp, region, dst, year, slice)
        )
  ) +
  sum(src$mTradeIrCdst2Ainp(trade, comm, src, region, year, slice),
      pTradeIrCdst2Ainp(trade, comm, src, region, year, slice)
      * sum(commp$mTradeComm(trade, commp),
            vTradeIr(trade, commp, src, region, year, slice)
        )
  );

eqTradeIrAOut(trade, comm, region, year, slice)$mvTradeIrAOut(trade, comm, region, year, slice)..
  vTradeIrAOut(trade, comm, region, year, slice)
  =e=
  sum(dst$mTradeIrCsrc2Aout(trade, comm, region, dst, year, slice),
      pTradeIrCsrc2Aout(trade, comm, region, dst, year, slice)
      * sum(commp$mTradeComm(trade, commp),
            vTradeIr(trade, commp, region, dst, year, slice)
        )
  ) +
  sum(src$mTradeIrCdst2Aout(trade, comm, src, region, year, slice),
      pTradeIrCdst2Aout(trade, comm, src, region, year, slice)
      * sum(commp$mTradeComm(trade, commp),
            vTradeIr(trade, commp, src, region, year, slice)
        )
  );

eqTradeIrAInpTot(comm, region, year, slice)$mvTradeIrAInpTot(comm, region, year, slice)..
  vTradeIrAInpTot(comm, region, year, slice)
  =e=
*  pSliceWeight(year, slice) *
  sum((trade, slicep)$(mCommSliceOrParent(comm, slice, slicep)
                       and mvTradeIrAInp(trade, comm, region, year, slicep)
                       ),
      vTradeIrAInp(trade, comm, region, year, slicep)
  );

eqTradeIrAOutTot(comm, region, year, slice)$mvTradeIrAOutTot(comm, region, year, slice)..
  vTradeIrAOutTot(comm, region, year, slice)
  =e=
*  pSliceWeight(year, slice) *
  sum((trade, slicep)$(mCommSliceOrParent(comm, slice, slicep)
                       and mvTradeIrAOut(trade, comm, region, year, slicep)
                       ),
      vTradeIrAOut(trade, comm, region, year, slicep)
  );

********************************************************************************
*** Balance equations & dummy import & export
********************************************************************************
Equation
eqBalUp(comm, region, year, slice)   Commodity balance <= 0 (e.g. upper limit - deficit is allowed)
eqBalLo(comm, region, year, slice)   Commodity balance >= 0 (e.g. lower limit - excess is allower)
eqBalFx(comm, region, year, slice)   Commodity balance >= 0 (no excess nor deficit is allowed)
eqBal(comm, region, year, slice)     Commodity balance
eqBalanceRY(comm, region, year)
eqOutTot(comm, region, year, slice)     Total commodity output
eqOutTotRY(comm, region, year)     Total commodity output
eqInpTot(comm, region, year, slice)     Total commodity input
eqInpTotRY(comm, region, year)     Total commodity input
eqInp2Lo(comm, region, year, slice)         From commodity slice to lo level
eqOut2Lo(comm, region, year, slice)         From commodity slice to lo level
eqSupOutTot(comm, region, year, slice)      Supply total output
eqTechInpTot(comm, region, year, slice)     Technology total input
eqTechOutTot(comm, region, year, slice)     Technology total output
eqTechOutRY(tech, comm, region, year)     Technology total output (year)
eqStorageInpTot(comm, region, year, slice)  Storage total input
eqStorageOutTot(comm, region, year, slice)  Storage total output
;

eqBalLo(comm, region, year, slice)$meqBalLo(comm, region, year, slice)..
  vBalance(comm, region, year, slice) =g= 0;

eqBalUp(comm, region, year, slice)$meqBalUp(comm, region, year, slice)..
  vBalance(comm, region, year, slice) =l= 0;

eqBalFx(comm, region, year, slice)$meqBalFx(comm, region, year, slice)..
  vBalance(comm, region, year, slice) =e= 0;

eqBal(comm, region, year, slice)$mvBalance(comm, region, year, slice)..
  vBalance(comm, region, year, slice)
* pSliceWeight(year, slice)
  =e=
  vOutTot(comm, region, year, slice)$mvOutTot(comm, region, year, slice)
  -
  vInpTot(comm, region, year, slice)$mvInpTot(comm, region, year, slice);

eqBalanceRY(comm, region, year)$mBalanceRY(comm, region, year)..
  vBalanceRY(comm, region, year)
  =e=
  sum(slice$mvBalance(comm, region, year, slice),
        pSliceWeight(year, slice)
        * vBalance(comm, region, year, slice)$mvBalance(comm, region, year, slice)
    );

eqOutTot(comm, region, year, slice)$mvOutTot(comm, region, year, slice)..
         vOutTot(comm, region, year, slice)
         =e=
           vDummyImport(comm, region, year, slice)$mDummyImport(comm, region, year, slice)
         + vSupOutTot(comm, region, year, slice)$mSupOutTot(comm, region, year, slice)
         + vEmsFuelTot(comm, region, year, slice)$mEmsFuelTot(comm, region, year, slice)
         + vAggOutTot(comm, region, year, slice)$mAggOut(comm, region, year, slice)
         + vTechOutTot(comm, region, year, slice)$mTechOutTot(comm, region, year, slice)
         + vStorageOutTot(comm, region, year, slice)$mStorageOutTot(comm, region, year, slice)
         + vImportTot(comm, region, year, slice)$mImport(comm, region, year, slice)
         + vTradeIrAOutTot(comm, region, year, slice)$mvTradeIrAOutTot(comm, region, year, slice)
         + sum(slicep$(mSliceParentChild(slicep, slice)
               and
               mvOut2Lo(comm, region, year, slicep, slice)
              ),
              vOut2Lo(comm, region, year, slicep, slice)
           )$mOutSub(comm, region, year, slice);

eqOutTotRY(comm, region, year)$mOutTotRY(comm, region, year)..
  vOutTotRY(comm, region, year)
  =e=
  sum(slice$mvOutTot(comm, region, year, slice),
      pSliceWeight(year, slice) *
      vOutTot(comm, region, year, slice)$mvOutTot(comm, region, year, slice)
      );

eqOut2Lo(comm, region, year, slice)$mOut2Lo(comm, region, year, slice)..
         sum(slicep$mvOut2Lo(comm, region, year, slice, slicep),
             vOut2Lo(comm, region, year, slice, slicep))
         =e=
         vSupOutTot(comm, region, year, slice)$mSupOutTot(comm, region, year, slice)
         + vEmsFuelTot(comm, region, year, slice)$mEmsFuelTot(comm, region, year, slice)
         + vAggOutTot(comm, region, year, slice)$mAggOut(comm, region, year, slice)
         + vTechOutTot(comm, region, year, slice)$mTechOutTot(comm, region, year, slice)
         + vStorageOutTot(comm, region, year, slice)$mStorageOutTot(comm, region, year, slice)
         + vImportTot(comm, region, year, slice)$mImport(comm, region, year, slice)
         + vTradeIrAOutTot(comm, region, year, slice)$mvTradeIrAOutTot(comm, region, year, slice);

eqInpTot(comm, region, year, slice)$mvInpTot(comm, region, year, slice)..
        vInpTot(comm, region, year, slice)
        =e=
         vDemInp(comm, region, year, slice)$mvDemInp(comm, region, year, slice)
        + vDummyExport(comm, region, year, slice)$mDummyExport(comm, region, year, slice)
        + vTechInpTot(comm, region, year, slice)$mTechInpTot(comm, region, year, slice)
        + vStorageInpTot(comm, region, year, slice)$mStorageInpTot(comm, region, year, slice)
        + vExportTot(comm, region, year, slice)$mExport(comm, region, year, slice)
        + vTradeIrAInpTot(comm, region, year, slice)$mvTradeIrAInpTot(comm, region, year, slice)
        + sum(slicep$(mSliceParentChild(slicep, slice)
                    and
                    mvInp2Lo(comm, region, year, slicep, slice)
            ),
            vInp2Lo(comm, region, year, slicep, slice)
        )$mInpSub(comm, region, year, slice);

eqInpTotRY(comm, region, year)$mInpTotRY(comm, region, year)..
        vInpTotRY(comm, region, year)
        =e=
        sum(slice$mvInpTot(comm, region, year, slice),
            pSliceWeight(year, slice) *
            vInpTot(comm, region, year, slice)$mvInpTot(comm, region, year, slice)
        );

eqInp2Lo(comm, region, year, slice)$mInp2Lo(comm, region, year, slice)..
        sum(slicep$mvInp2Lo(comm, region, year, slice, slicep),
            vInp2Lo(comm, region, year, slice, slicep)
        )
        =e=
        vTechInpTot(comm, region, year, slice)$mTechInpTot(comm, region, year, slice)
        + vStorageInpTot(comm, region, year, slice)$mStorageInpTot(comm, region, year, slice)
        + vExportTot(comm, region, year, slice)$mExport(comm, region, year, slice)
        + vTradeIrAInpTot(comm, region, year, slice)$mvTradeIrAInpTot(comm, region, year, slice);

eqSupOutTot(comm, region, year, slice)$mSupOutTot(comm, region, year, slice)..
          vSupOutTot(comm, region, year, slice)
          =e=
          sum(sup$mSupComm(sup, comm),
              vSupOut(sup, comm, region, year, slice)
          );

* the sum removed since supply-timeframe is fixed to the commodity's timeframe
*              sum(slicep$(mCommSliceOrParent(comm, slice, slicep)
*                          and mSupAva(sup, comm, region, year, slicep)
*                         ),
*                  vSupOut(sup, comm, region, year, slicep)
*              )
*          );

*$ontext
eqTechInpTot(comm, region, year, slice)$mTechInpTot(comm, region, year, slice)..
        vTechInpTot(comm, region, year, slice)
        =e=
*        pSliceWeight(year, slice) *
        sum(tech$mTechInpCommSameSlice(tech, comm),
            vTechInp(tech, comm, region, year, slice)$mvTechInp(tech, comm, region, year, slice)
        )
        +
*        pSliceWeight(year, slice) *
        sum(tech$mTechInpCommAgg(tech, comm),
            sum(slicep$mTechInpCommAggSlice(tech, comm, slicep, slice),
                vTechInp(tech, comm, region, year, slicep)$mvTechInp(tech, comm, region, year, slicep)
            )
        )
        +
*        pSliceWeight(year, slice) *
        sum(tech$mTechAInpCommSameSlice(tech, comm),
            vTechAInp(tech, comm, region, year, slice)$mvTechAInp(tech, comm, region, year, slice)
        )
        +
*        pSliceWeight(year, slice) *
        sum(tech$mTechAInpCommAgg(tech, comm),
            sum(slicep$mTechAInpCommAggSlice(tech, comm, slicep, slice),
            vTechAInp(tech, comm, region, year, slicep)$mvTechAInp(tech, comm, region, year, slicep)
            )
        );

eqTechOutTot(comm, region, year, slice)$mTechOutTot(comm, region, year, slice)..
        vTechOutTot(comm, region, year, slice)
        =e=
*        pSliceWeight(year, slice) *
        sum(tech$mTechOutCommSameSlice(tech, comm),
            vTechOut(tech, comm, region, year, slice)$mvTechOut(tech, comm, region, year, slice)
        )
        +
*        pSliceWeight(year, slice) *
        sum(tech$mTechOutCommAgg(tech, comm),
            sum(slicep$mTechOutCommAggSlice(tech, comm, slicep, slice),
                vTechOut(tech, comm, region, year, slicep)$mvTechOut(tech, comm, region, year, slicep)
            )
        )
        +
*        pSliceWeight(year, slice) *
        sum(tech$mTechAOutCommSameSlice(tech, comm),
            vTechAOut(tech, comm, region, year, slice)$mvTechAOut(tech, comm, region, year, slice)
        )
        +
*        pSliceWeight(year, slice) *
        sum(tech$mTechAOutCommAgg(tech, comm),
            sum(slicep$mTechAOutCommAggSlice(tech, comm, slicep, slice),
                vTechAOut(tech, comm, region, year, slicep)$mvTechAOut(tech, comm, region, year, slicep)
            )
        );

eqTechOutRY(tech, comm, region, year)$mTechOutRY(tech, comm, region, year)..
        vTechOutRY(tech, comm, region, year)
        =e=
        sum(slice$mTechOutTot(comm, region, year, slice),
            pSliceWeight(year, slice) *
            vTechOut(tech, comm, region, year, slice)$mvTechOut(tech, comm, region, year, slice)
        );

eqStorageInpTot(comm, region, year, slice)$mStorageInpTot(comm, region, year, slice)..
        vStorageInpTot(comm, region, year, slice)
        =e=
*        pSliceWeight(year, slice) *
        sum(stg$mvStorageStore(stg, comm, region, year, slice),
            vStorageInp(stg, comm, region, year, slice)
        )
*        + pSliceWeight(year, slice) *
        + sum(stg$mvStorageAInp(stg, comm, region, year, slice),
            vStorageAInp(stg, comm, region, year, slice)
        );

eqStorageOutTot(comm, region, year, slice)$mStorageOutTot(comm, region, year, slice)..
        vStorageOutTot(comm, region, year, slice)
        =e=
*        pSliceWeight(year, slice) *
        sum(stg$mvStorageStore(stg, comm, region, year, slice),
            vStorageOut(stg, comm, region, year, slice)
        )
*        + pSliceWeight(year, slice) *
        + sum(stg$mvStorageAOut(stg, comm, region, year, slice),
            vStorageAOut(stg, comm, region, year, slice)
        );

**********************************************
** Objective and aggregated costs equations
**********************************************
Equation
eqDummyImportCost(comm, region, year)   Dummy import costs (weighted)
eqDummyExportCost(comm, region, year)   Dummy export costs (weighted)
eqTaxCost(comm, region, year)       Commodity taxes (weighted)
eqSubsCost(comm, region, year)      Commodity subsidy (weighted)
eqCost(region, year)                Total costs (weighted)
eqObjective                         Objective equation NPV of total costs
;



* dummy import (to debug infesibility)
eqDummyImportCost(comm, region, year)$mDummyImportCost(comm, region, year)..
        vDummyImportCost(comm, region, year)
        =e=
        sum(slice$mDummyImport(comm, region, year, slice),
            pSliceWeight(year, slice)
            * pDummyImportCost(comm, region, year, slice)
            * vDummyImport(comm, region, year, slice)$mDummyImport(comm, region, year, slice)
            );

* dummy export (to debug infesibility)
eqDummyExportCost(comm, region, year)$mDummyExportCost(comm, region, year)..
        vDummyExportCost(comm, region, year)
        =e=
        sum(slice$mDummyExport(comm, region, year, slice),
            pSliceWeight(year, slice)
            * pDummyExportCost(comm, region, year, slice)
            * vDummyExport(comm, region, year, slice)$mDummyExport(comm, region, year, slice)
            );

* tax levies
eqTaxCost(comm, region, year)$mTaxCost(comm, region, year)..
         vTaxCost(comm, region, year)
         =e=
         sum(slice$(mvOutTot(comm, region, year, slice) and mCommSlice(comm, slice)),
             pTaxCostOut(comm, region, year, slice)
             * pSliceWeight(year, slice)
             * vOutTot(comm, region, year, slice)
             )
       + sum(slice$(mvInpTot(comm, region, year, slice) and mCommSlice(comm, slice)),
             pTaxCostInp(comm, region, year, slice)
             * pSliceWeight(year, slice)
             * vInpTot(comm, region, year, slice)
             )
       + sum(slice$(mvBalance(comm, region, year, slice) and mCommSlice(comm, slice)),
             pTaxCostBal(comm, region, year, slice)
             * pSliceWeight(year, slice)
             * vBalance(comm, region, year, slice)
             );

* subsidies
eqSubsCost(comm, region, year)$mSubCost(comm, region, year)..
        vSubsCost(comm, region, year)
        =e=
      - sum(slice$(mvOutTot(comm, region, year, slice) and mCommSlice(comm, slice)),
            pSubCostOut(comm, region, year, slice)
            * pSliceWeight(year, slice)
            * vOutTot(comm, region, year, slice)
        )
      - sum(slice$(mvInpTot(comm, region, year, slice) and mCommSlice(comm, slice)),
            pSubCostInp(comm, region, year, slice)
            * pSliceWeight(year, slice)
            * vInpTot(comm, region, year, slice)
        )
      - sum(slice$(mvBalance(comm, region, year, slice) and mCommSlice(comm, slice)),
            pSubCostBal(comm, region, year, slice)
            * pSliceWeight(year, slice)
            * vBalance(comm, region, year, slice));
*==============================================================================*
eqCost(region, year)$mvTotalCost(region, year)..
        vTotalCost(region, year)
        =e=
* supply costs
        + sum(sup$mvSupCost(sup, region, year), vSupCost(sup, region, year)$mvSupCost(sup, region, year))
* technology capital costs
        + sum(tech$mTechEac(tech, region, year), vTechEac(tech, region, year)$mTechEac(tech, region, year))
* technology endogenous retirement
        + sum(tech$mTechRetCost(tech, region, year), vTechRetCost(tech, region, year)$mTechRetCost(tech, region, year))
* technology fixed O&M costs
        + sum(tech$mTechFixom(tech, region, year), vTechFixom(tech, region, year)$mTechFixom(tech, region, year))
* technology variable O&M costs
        + sum(tech$mTechVarom(tech, region, year), vTechVarom(tech, region, year)$mTechVarom(tech, region, year))
* storage capital costs
        + sum(stg$mStorageEac(stg, region, year), vStorageEac(stg, region, year)$mStorageEac(stg, region, year))
* storage fixed O&M costs
        + sum(stg$mStorageFixom(stg, region, year), vStorageFixom(stg, region, year)$mStorageFixom(stg, region, year))
* storage variable O&M costs
        + sum(stg$mStorageVarom(stg, region, year), vStorageVarom(stg, region, year)$mStorageVarom(stg, region, year))
* RoW import costs/credits
        + sum(imp$mImportRowCost(imp, region, year), vImportRowCost(imp, region, year)$mImportRowCost(imp, region, year))
* RoW export credit/costs
        + sum(expp$mExportRowCost(expp, region, year), vExportRowCost(expp, region, year)$mExportRowCost(expp, region, year))
* IR trade capital costs
        + sum(trade$mTradeEac(trade, region, year), vTradeEac(trade, region, year)$mTradeEac(trade, region, year))
* IR trade fixed O&M costs
        + sum(trade$mTradeFixom(trade, region, year), vTradeFixom(trade, region, year)$mTradeFixom(trade, region, year))
* IR trade import costs
        + sum(trade$mImportIrCost(trade, region, year), vImportIrCost(trade, region, year)$mImportIrCost(trade, region, year))
* IR trade export costs
        + sum(trade$mExportIrCost(trade, region, year), vExportIrCost(trade, region, year)$mExportIrCost(trade, region, year))
* commodity taxes
        + sum(comm$mTaxCost(comm, region, year), vTaxCost(comm, region, year)$mTaxCost(comm, region, year))
* commodity subsidies
        + sum(comm$mSubCost(comm, region, year), vSubsCost(comm, region, year)$mSubCost(comm, region, year))
* custom user costs
        + vTotalUserCosts(region, year)$mvTotalUserCosts(region, year)
* "dummy" import costs for debugging
        + sum((comm)$mDummyImportCost(comm, region, year), vDummyImportCost(comm, region, year)$mDummyImportCost(comm, region, year))
* "dummy" export costs for debugging
        + sum((comm)$mDummyExportCost(comm, region, year), vDummyExportCost(comm, region, year)$mDummyExportCost(comm, region, year))
;

eqObjective..
   vObjective =e=
         sum((region, year)$mvTotalCost(region, year),
*           pDiscountFactorMileStone(region, year) *
            vTotalCost(region, year) * pPeriodLen(year) * pDiscountFactor(region, year));

set
mLECRegion(region)
;

parameter
pLECLoACT(region)  levelized costs interim parameter
;

**************************************
** LEC equation
**************************************
Equation
eqLECActivity(tech, region, year)  levelized costs (auxiliary equation)
;

eqLECActivity(tech, region, year)$meqLECActivity(tech, region, year)..
         sum(slice$mTechSlice(tech, slice), vTechAct(tech, region, year, slice)) =g= pLECLoACT(region);

$include inc_constraints.gms

$include inc_costs.gms

model energyRt / all / ;

put log_stat;
put '"load data",,"' GYear(JNow):0:0 "-" GMonth(JNow):0:0 "-" GDay(JNow):0:0 " " GHour(JNow):0:0 ":" GMinute(JNow):0:0 ":" GSecond(JNow):0:0'"'/;


$include data.gms

$include inc3.gms

put log_stat;
put '"solver",,"' GYear(JNow):0:0 "-" GMonth(JNow):0:0 "-" GDay(JNow):0:0 " " GHour(JNow):0:0 ":" GMinute(JNow):0:0 ":" GSecond(JNow):0:0'"'/;

$include inc_solver.gms

Solve energyRt minimizing vObjective using LP;
*$ontext

put log_stat;
put '"solution status",' energyRt.Modelstat:0:0 ',"' GYear(JNow):0:0 "-" GMonth(JNow):0:0 "-" GDay(JNow):0:0 " " GHour(JNow):0:0 ":" GMinute(JNow):0:0 ":" GSecond(JNow):0:0'"'/;
put '"export results",,"' GYear(JNow):0:0 "-" GMonth(JNow):0:0 "-" GDay(JNow):0:0 " " GHour(JNow):0:0 ":" GMinute(JNow):0:0 ":" GSecond(JNow):0:0'"'/;

$include inc4.gms

$include output.gms

$include inc5.gms


put log_stat;
put 'done,,"' GYear(JNow):0:0 "-" GMonth(JNow):0:0 "-" GDay(JNow):0:0 " " GHour(JNow):0:0 ":" GMinute(JNow):0:0 ":" GSecond(JNow):0:0 '"'/;
putclose;
*$offtext
