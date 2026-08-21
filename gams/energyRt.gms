* energyRt 0.71
* Energy Systems Modeling Toolbox for R
* https://energyRt.org

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
timeslice  time timeslices
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
alias (timeslice, timeslicep), (timeslice, timeslicepp), (group, groupp), (comm, commp), (comm, acomm), (comm, comme), (sup, supp);
alias (region, src), (region, dst), (region, region2), (year, year2), (timeslice, timeslice2);

* Mapping sets
sets
mCommReg(comm, region)        Commodity to region mapping (to filter out unused cases)
mSameRegion(region, region)   The same region (used in GLPK)
mSameTimeslice(timeslice, timeslice)      The same timeslice (used in GLPK)
*! technology:input
mMilestoneFirst(year)          First period milestone
mMilestoneLast(year)           Last period milestone
mMilestoneNext(year, year)     Next period milestone
mMilestoneHasNext(year)        Is there next period milestone
mStartMilestone(year, year)    Start of the period
mEndMilestone(year, year)      End of the period
mMidMilestone(year)            Milestone year
mCommTimeslice(comm, timeslice)        Commodity to timeslice
mCommTimesliceOrParent(comm, timeslice, timeslice)
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
mTechTimeslice(tech, timeslice)          Technology to timeslice-level
* Supply
mSupTimeslice(sup, timeslice)             Supply to timeslices-level
mSupComm(sup, comm)               Supplied commodities
mSupSpan(sup, region)             Supply in regions
* Demand
mDemComm(dem, comm)               Demand commodities
* Ballance
mUpComm(comm)  Commodity balance type TOTAL SUPPLY <= TOTAL DEMAND
mLoComm(comm)  Commodity balance type TOTAL SUPPLY >= TOTAL DEMAND
mFxComm(comm)  Commodity balance type TOTAL SUPPLY == TOTAL DEMAND
* Storage
mStorageFullYear(stg)              Mapping of storage with joint timeslice
mStorageComm(stg, comm)            Mapping of storage technology and respective commodity
* The three storage commodity ROLES -- what fills the store, what it releases,
* and what it HOLDS. All default from @commodity, so for a storage written the
* old way the three are identical to mStorageComm above.
mStorageInpComm(stg, comm)         Storage input commodity (the charging side)
mStorageOutComm(stg, comm)         Storage output commodity (the discharging side)
mStorageStgComm(stg, comm)         Storage stored commodity (the level)
mStorageAInp(stg, comm)            Aux-commodity input to storage
mStorageAOut(stg, comm)            Aux-commodity output from storage
mStorageNew(stg, region, year)     Storage available for investment
mStorageSpan(stg, region, year)    Storage set showing if the storage may exist in the year and region
*mStorageOMCost(stg, region, year)  drop
mStorageFixom(stg, region, year)   add
mStorageVarom(stg, region, year)   add
mStorageEac(stg, region, year)
mTimesliceNext(timeslice, timeslice)           Next timeslice
mTimesliceFYearNext(timeslice, timeslice)      Next timeslice joint
* Trade and the ROW
mTradeTimeslice(trade, timeslice)          Trade to timeslice
mTradeComm(trade, comm)            Trade commodities
mTradeRoutes(trade, region, region)
mTradeIrAInp(trade, comm)          Auxiliary  input commodity in source region
mTradeIrAOut(trade, comm)          Auxiliary output commodity in source region
mExpComm(expp, comm)               Export commodities
mImpComm(imp, comm)                Import commodities
mExpTimeslice(expp, timeslice)             Export to timeslice
mImpTimeslice(imp, timeslice)              Import to timeslice
* Mapping for Salvage (temporary)
mDiscountZero(region)
mTimesliceParentChildE(timeslice, timeslice)   Child timeslice or the same
mTimesliceParentChild(timeslice, timeslice)    Child timeslice not the same
mTimesliceFamily(timeslice, timeslice)         Immediate timeslice parent-child (one level) [agg-rewrite]
* [nested-regions] spatial twin of mTimesliceFamily. No pRegionAgg counterpart:
* regional quantities are extensive and simply add up. Empty without a geoscale.
mRegionFamily(region, region)      Immediate region parent-child (one level) [nested-regions]
mCommRegion(comm, region)          Region level a commodity is balanced at [nested-regions]
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
* Fraction of a year of selected time timeslices
pYearFraction(year)   fraction of sum of sampled timeslices in year -- experimental
* Technology parameters
pTechOlife(tech, region)                           Operational life of technologies
pTechCinp2ginp(tech, comm, region, year, timeslice)    Commodity input to group input
pTechGinp2use(tech, group, region, year, timeslice)    Group input into use
pTechCinp2use(tech, comm, region, year, timeslice)     Commodity input to use
pTechUse2cact(tech, comm, region, year, timeslice)     Use to commodity activity
pTechCact2cout(tech, comm, region, year, timeslice)    Commodity activity to commodity output
pTechEmisComm(tech, comm)                          Combustion factor for input commodity (from 0 to 1)
* Auxiliary input commodities
pTechAct2AInp(tech, comm, region, year, timeslice)     Activity to aux-commodity input
pTechCap2AInp(tech, comm, region, year, timeslice)     Capacity to aux-commodity input
pTechNCap2AInp(tech, comm, region, year, timeslice)     New capacity to aux-commodity input
pTechCinp2AInp(tech, comm, comm, region, year, timeslice)    Commodity input to aux-commodity input
pTechCout2AInp(tech, comm, comm, region, year, timeslice)    Commodity output to aux-commodity input
* Aux output comm map
pTechAct2AOut(tech, comm, region, year, timeslice)     Activity to aux-commodity output
pTechCap2AOut(tech, comm, region, year, timeslice)     Capacity to aux-commodity output
pTechNCap2AOut(tech, comm, region, year, timeslice)     New capacity to aux-commodity output
pTechCinp2AOut(tech, comm, comm, region, year, timeslice)     Commodity to aux-commodity output
pTechCout2AOut(tech, comm, comm, region, year, timeslice)     Commodity-output to aux-commodity input
*
pTechFixom(tech, region, year)                      Fixed Operating and maintenance (O&M) costs (per unit of capacity)
pTechVarom(tech, region, year, timeslice)               Variable O&M costs (per unit of acticity)
pTechInvcost(tech, region, year)                    Overnight Investment costs (per unit of capacity)
pTechEac(tech, region, year)                        Equivalent annual (investment) cost
pTechRetCost(tech, region, year)                    Early retirement costs
pTechShareLo(tech, comm, region, year, timeslice)       Lower bound of the share of the commodity in total group input or output
pTechShareUp(tech, comm, region, year, timeslice)       Upper bound of the share of the commodity in total group input or output
pTechAfLo(tech, region, year, timeslice)                Lower bound on availability factor by timeslices
pTechAfUp(tech, region, year, timeslice)                Upper bound on availability factor by timeslices
pTechRampUp(tech, region, year, timeslice)              Ramp Up on availability factor
pTechRampDown(tech, region, year, timeslice)            Ramp Down on availability
pTechAfsLo(tech, region, year, timeslice)               Lower bound on availability factor by groups of timeslices
pTechAfsUp(tech, region, year, timeslice)               Upper bound on availability factor by groups of timeslices
pTechAfcLo(tech, comm, region, year, timeslice)         Lower bound for commodity output
pTechAfcUp(tech, comm, region, year, timeslice)         Upper bound for commodity output
pTechStock(tech, region, year)                      Technology capacity stock
pTechCapUp(tech, region, year)                      Upper bound on technology capacity
pTechCapLo(tech, region, year)                      Lower bound on technology capacity
pTechNewCapUp(tech, region, year)                   Upper bound on new technology capacity
pTechNewCapLo(tech, region, year)                   Lower bound on new technology capacity
pTechRetUp(tech, region, year)                      Upper bound on early retirement
pTechRetLo(tech, region, year)                      Lower bound on early retirement
pTechCap2act(tech)                                  Technology capacity units to activity units conversion factor
pTechCvarom(tech, comm, region, year, timeslice)        Commodity-specific variable costs (per unit of commodity input or output)
pTechAvarom(tech, comm, region, year, timeslice)        Auxilary Commodity-specific variable costs (per unit of commodity input or output)
* Rates
* pXPayback narrows the annuity to a cost-recovery period shorter than the
* operational life; pXOlife still governs when capacity operates. Used in
* eqTechEac / eqStorageEac / eqTradeEac below (ported from GLPK 2026-08-19).
pWacc(region, year)                                 Weighted average cost of capital (can be region and year specific)
pSdr(region, year)                                  Social discount rate (can be region and year specific)
pTechWacc(tech, region, year)                       Technology-specific cost of capital
pStorageWacc(stg, region, year)                     Storage-specific cost of capital
pTradeWacc(trade, region, year)                     Trade-specific cost of capital
pTechPayback(tech, region, year)                    Cost-recovery period of a technology
pStoragePayback(stg, region, year)                  Cost-recovery period of a storage
pTradePayback(trade, region, year)                  Cost-recovery period of a trade
pDiscountFactor(region, year)                       Discount factor (cumulative)
pDiscountFactorMileStone(region, year)              Discount factor (cumulative) sum for MileStone
* Supply
pSupCost(sup, comm, region, year, timeslice)            Costs of supply (price per unit)
pSupAvaUp(sup, comm, region, year, timeslice)           Upper bound for supply
pSupAvaLo(sup, comm, region, year, timeslice)           Lower bound for supply
pSupReserveUp(sup, comm, region)                    Upper constraint on cumulative supply
pSupReserveLo(sup, comm, region)                    Lower constraint on cumulative supply
* Demand
pDemand(dem, comm, region, year, timeslice)             Exogenous demand
* Emissions
pEmissionFactor(comm, comm)                         Emission factor
* Dummy import
pDummyImportCost(comm, region, year, timeslice)         Dummy costs parameters (for debugging)
pDummyExportCost(comm, region, year, timeslice)         Dummy costs parameters (for debuging)
* Taxes and subsidies
pTaxCostInp(comm, region, year, timeslice)              Commodity taxes for input
pTaxCostOut(comm, region, year, timeslice)              Commodity taxes for output
pTaxCostBal(comm, region, year, timeslice)              Commodity taxes for balance
pSubCostInp(comm, region, year, timeslice)              Commodity subsidies for input
pSubCostOut(comm, region, year, timeslice)              Commodity subsidies for output
pSubCostBal(comm, region, year, timeslice)              Commodity subsidies for balance
* Aggregation
pAggregateFactor(comm, comm)                        Aggregation factor of commodities
* System parameters
pPeriodLen(year)        Length of milestone-year-period
pTimesliceShare(timeslice)      Timeslice share in year
pTimesliceWeight(year, timeslice)     Timeslice weight
pTimesliceAgg(year, timeslice, timeslice)   Intensive timeslice aggregation weight (parent from child) [agg-rewrite]
ordYear(year)           ord year (used in GLPK-MathProg)
cardYear(year)          card year (used in GLPK-MathProg)
;

* Storage technology parameters
parameters
pStorageInpEff(stg, comm, region, year, timeslice)      Storage input efficiency
pStorageOutEff(stg, comm, region, year, timeslice)      Storage output efficiency
pStorageStgEff(stg, comm, region, year, timeslice)      Storage time-efficiency (annual)
pStorageStock(stg, region, year)                    Storage capacity stock
pStorageCapUp(stg, region, year)                    Upper bound on storage capacity
pStorageInpCap2act(stg)                             Storage charging capacity to annual flow
pStorageOutCap2act(stg)                             Storage discharging capacity to annual flow
pStorageInpStock(stg, region, year)                 Storage exogenous charging capacity
pStorageInpInvcost(stg, region, year)               Storage investment cost per unit of charging capacity
pStorageInpFixom(stg, region, year)                 Storage fixed OM cost per unit of charging capacity
pStorageInpEac(stg, region, year)                   Storage annualised charging-capacity investment cost
pStorageInpCapLo(stg, region, year)                 Lower bound on storage charging capacity
pStorageInpCapUp(stg, region, year)                 Upper bound on storage charging capacity
pStorageInpNewCapLo(stg, region, year)              Lower bound on new storage charging capacity
pStorageInpNewCapUp(stg, region, year)              Upper bound on new storage charging capacity
pStorageInp2outLo(stg, region, year)                Lower bound on the charge-to-discharge ratio
pStorageInp2outUp(stg, region, year)                Upper bound on the charge-to-discharge ratio
pStorageStgStock(stg, region, year)                 Storage exogenous energy capacity
pStorageStgInvcost(stg, region, year)               Storage investment cost per unit of energy capacity
pStorageStgFixom(stg, region, year)                 Storage fixed OM cost per unit of energy capacity
pStorageStgEac(stg, region, year)                   Storage annualised energy-capacity investment cost
pStorageStgCapLo(stg, region, year)                 Lower bound on storage energy capacity
pStorageStgCapUp(stg, region, year)                 Upper bound on storage energy capacity
pStorageStgNewCapLo(stg, region, year)              Lower bound on new storage energy capacity
pStorageStgNewCapUp(stg, region, year)              Upper bound on new storage energy capacity
pStorageCapLo(stg, region, year)                    Lower bound on storage capacity
pStorageNewCapUp(stg, region, year)                 Upper bound on new storage capacity
pStorageNewCapLo(stg, region, year)                 Lower bound on new storage capacity
pStorageRetUp(stg, region, year)                    Upper bound on early retirement
pStorageRetLo(stg, region, year)                    Lower bound on early retirement
pStorageOlife(stg, region)                          Storage operational life
pStorageCostStore(stg, region, year, timeslice)         Storing costs per stored amount (annual)
pStorageCostInp(stg, region, year, timeslice)           Storage input costs
pStorageCostOut(stg, region, year, timeslice)           Storage output costs
pStorageFixom(stg, region, year)                    Storage fixed O&M costs
pStorageInvcost(stg, region, year)                  Storage investment costs
pStorageEac(stg, region, year)                      Storage equivalent annual costs
pStorageRetCost(stg, region, year)                  Storage early retirement costs
pStorageDurationLo(stg, region, year)               Storage energy-to-power ratio, hours, lower bound
pStorageDurationUp(stg, region, year)               Storage energy-to-power ratio, hours, upper bound
pStorageAfLo(stg, region, year, timeslice)              Storage availability factor lower bound (minimum charging level)
pStorageAfUp(stg, region, year, timeslice)              Storage availability factor upper bound (maximum charging level)
*pStorageAfsLo(tech, region, year, timeslice)           add parameter and eq?
*pStorageAfsUp(tech, region, year, timeslice)           add parameter and eq?
pStorageCinpUp(stg, comm, region, year, timeslice)      Storage input upper bound
pStorageCinpLo(stg, comm, region, year, timeslice)      Storage input lower bound
pStorageCoutUp(stg, comm, region, year, timeslice)      Storage output upper bound
pStorageCoutLo(stg, comm, region, year, timeslice)      Storage output lower bound
pStorageNCap2Stg(stg, comm, region, year, timeslice)    Initial storage charge level for new investment
pStorageStartLevel(stg, comm, region, year, timeslice)      Initial storage charge level for stock
pStorageStg2AInp(stg, comm, region, year, timeslice)    Storage level to auxilary input
pStorageStg2AOut(stg, comm, region, year, timeslice)    Storage level output
pStorageCinp2AInp(stg, comm, region, year, timeslice)   Storage input to auxilary input
pStorageCinp2AOut(stg, comm, region, year, timeslice)   Storage input to auxilary output
pStorageCout2AInp(stg, comm, region, year, timeslice)   Storage output to auxilary input
pStorageCout2AOut(stg, comm, region, year, timeslice)   Storage output to auxilary output
pStorageCap2AInp(stg, comm, region, year, timeslice)    Storage capacity to auxilary input
pStorageCap2AOut(stg, comm, region, year, timeslice)    Storage capacity to auxilary output
pStorageNCap2AInp(stg, comm, region, year, timeslice)   Storage new capacity to auxilary input
pStorageNCap2AOut(stg, comm, region, year, timeslice)   Storage new capacity to auxilary output
;
* Trade parameters
parameters
pTradeIrEff(trade, region, region, year, timeslice)     Inter-regional trade efficiency
pTradeIrUp(trade, region, region, year, timeslice)      Upper bound on trade flow
pTradeIrLo(trade, region, region, year, timeslice)      Lower bound on trade flow
pTradeIrCost(trade, region, region, year, timeslice)    Costs of trade flow
pTradeIrMarkup(trade, region, region, year, timeslice)  Markup of trade flow
* Aux input and output
pTradeIrCsrc2Ainp(trade, comm, region, region, year, timeslice)   Auxiliary input commodity in source region
pTradeIrCsrc2Aout(trade, comm, region, region, year, timeslice)   Auxiliary output commodity in source region
pTradeIrCdst2Ainp(trade, comm, region, region, year, timeslice)   Auxiliary input commodity in destination region
pTradeIrCdst2Aout(trade, comm, region, region, year, timeslice)   Auxiliary output commodity in destination region
pExportRowRes(expp)                                  Upper bound on cumulative export to the ROW
pExportRowUp(expp, region, year, timeslice)              Upper bound on export to the ROW
pExportRowLo(expp, region, year, timeslice)              Lower bound on export to the ROW
pExportRowPrice(expp, region, year, timeslice)           Export prices to the ROW
pImportRowRes(imp)                                   Upper bound on cumulative import to the ROW
pImportRowUp(imp, region, year, timeslice)               Upper bount on import from the ROW
pImportRowLo(imp, region, year, timeslice)               Lower bound on import from the ROW
pImportRowPrice(imp, region, year, timeslice)            Import prices from the ROW
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
pTradeVarom(trade, region, region, year, timeslice)      Variable O&M costs
pTradeCap2Act(trade)                                 Capacity to activity factor
;

* Weather mapping
sets
mWeatherTimeslice(weather, timeslice)
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
pWeather(weather, region, year, timeslice)          weather factors
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
mvTechInp(tech, comm, region, year, timeslice)
mvSupReserve(sup, comm, region)
mvTechRetiredNewCap(tech, region, year, year)

mvTechRetiredStock(tech, region, year)
*meqTechRetUp(tech, region, year)
*meqTechRetLo(tech, region, year)
mvTechAct(tech, region, year, timeslice)
mvTechInp(tech, comm, region, year, timeslice)
mvTechInpCommSameTimeslice(tech, comm, region, year, timeslice)
mvTechOut(tech, comm, region, year, timeslice)
mvTechAInp(tech, comm, region, year, timeslice)
mvTechAOut(tech, comm, region, year, timeslice)
mvDemInp(comm, region, year, timeslice)
mvBalance(comm, region, year, timeslice)
mvInpTot(comm, region, year, timeslice)
mvOutTot(comm, region, year, timeslice)

mTechCapLo(tech, region, year)
mTechCapUp(tech, region, year)
mTechNewCapLo(tech, region, year)
mTechNewCapUp(tech, region, year)
mTechRetLo(tech, region, year)
mTechRetUp(tech, region, year)

mvStorageAInp(stg, comm, region, year, timeslice)
mvStorageAOut(stg, comm, region, year, timeslice)
mvStorageLevel(stg, comm, region, year, timeslice)
* Flow domains follow their OWN commodity, the level follows what is stored.
mvStorageInp(stg, comm, region, year, timeslice)   Storage input flow domain
mvStorageOut(stg, comm, region, year, timeslice)   Storage output flow domain
meqStorageLevel(stg, comm, region, year, timeslicep, timeslice)
mStorageStg2AOut(stg, comm, region, year, timeslice)
mStorageCinp2AOut(stg, comm, region, year, timeslice)
mStorageCout2AOut(stg, comm, region, year, timeslice)
mStorageCap2AOut(stg, comm, region, year, timeslice)
mStorageNCap2AOut(stg, comm, region, year, timeslice)
mStorageStg2AInp(stg, comm, region, year, timeslice)
mStorageCinp2AInp(stg, comm, region, year, timeslice)
mStorageCout2AInp(stg, comm, region, year, timeslice)
mStorageCap2AInp(stg, comm, region, year, timeslice)
mStorageNCap2AInp(stg, comm, region, year, timeslice)

mStorageInpCap(stg, region, year)
mStorageNoInpCap(stg, region, year)
mStorageInpNew(stg, region, year)
mStorageInpCapLo(stg, region, year)
mStorageInpCapUp(stg, region, year)
mStorageInpNewCapLo(stg, region, year)
mStorageInpNewCapUp(stg, region, year)
mStorageInpFixom(stg, region, year)
mStorageInpEac(stg, region, year)
mStorageInp2outLo(stg, region, year)
mStorageInp2outUp(stg, region, year)
mStorageStgCap(stg, region, year)
mStorageNoStgCap(stg, region, year)
mStorageStgNew(stg, region, year)
mStorageStgCapLo(stg, region, year)
mStorageStgCapUp(stg, region, year)
mStorageStgNewCapLo(stg, region, year)
mStorageStgNewCapUp(stg, region, year)
mStorageStgFixom(stg, region, year)
mStorageStgEac(stg, region, year)
mStorageDurationLo(stg, region, year)
mStorageDurationUp(stg, region, year)
mStorageCapLo(stg, region, year)
mStorageCapUp(stg, region, year)
mStorageNewCapLo(stg, region, year)
mStorageNewCapUp(stg, region, year)
mStorageRetLo(stg, region, year)
mStorageRetUp(stg, region, year)

mvTradeIr(trade, comm, region, region, year, timeslice)
mTradeIrCsrc2Ainp(trade, comm, region, region, year, timeslice)
mTradeIrCdst2Ainp(trade, comm, region, region, year, timeslice)
mTradeIrCsrc2Aout(trade, comm, region, region, year, timeslice)
mTradeIrCdst2Aout(trade, comm, region, region, year, timeslice)
* retired: trade costs are vTradeEac + vTradeFixom + eqImport/ExportIrCost
*mvTradeCost(region, year)
*mvTradeRowCost(region, year)
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
*@ mvTechAct(tech, region, year, timeslice)
vTechAct(tech, region, year, timeslice)                  Activity level of technology
*@ mvTechInp(tech, comm, region, year, timeslice)
vTechInp(tech, comm, region, year, timeslice)            Input level
*@ mvTechOut(tech, comm, region, year, timeslice)
vTechOut(tech, comm, region, year, timeslice)            Commodity output from technology - tech timeframe
* Auxiliary input & output
*@ mvTechAInp(tech, comm, region, year, timeslice)
vTechAInp(tech, comm, region, year, timeslice)           Auxiliary commodity input
*@ mvTechAOut(tech, comm, region, year, timeslice)
vTechAOut(tech, comm, region, year, timeslice)           Auxiliary commodity output
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
*@ mSupAva(sup, comm, region, year, timeslice)
vSupOut(sup, comm, region, year, timeslice)              Output of supply
*@ mvSupReserve(sup, comm, region)
vSupReserve(sup, comm, region)                       Cumulative supply (weighted)
;
variables
*@ mvSupCost(sup, region, year)
vSupCost(sup, region, year)                          Supply costs (weighted)
;
positive variables
* Demand
*@ mvDemInp(comm, region, year, timeslice)
vDemInp(comm, region, year, timeslice)                   Input to demand
;
variables
* Emission
*@ mEmsFuelTot(comm, region, year, timeslice)
vEmsFuelTot(comm, region, year, timeslice)               Total emissions from fuels combustion (technologies)
;
variables
* Ballance
*@ mvBalance(comm, region, year, timeslice)
vBalance(comm, region, year, timeslice)                  Net commodity balance (all sources)
;
positive variables
*@ mvOutTot(comm, region, year, timeslice)
vOutTot(comm, region, year, timeslice)                   Total commodity output (all processes) (weighted)
*@ mvInpTot(comm, region, year, timeslice)
vInpTot(comm, region, year, timeslice)                   Total commodity input (all processes) (weighted)
* [agg-rewrite] vInp2Lo/vOut2Lo retired (up-aggregation in eqInpTot/eqOutTot)
*@ mvInp2Lo(comm, region, year, timeslice, timeslice)
* vInp2Lo(comm, region, year, timeslice, timeslice)            Desagregation of timeslices for input parent to (grand)child
*@ mvOut2Lo(comm, region, year, timeslice, timeslice)
* vOut2Lo(comm, region, year, timeslice, timeslice)            Desagregation of timeslices for output parent to (grand)child
*@ mSupOutTot(comm, region, year, timeslice)
vSupOutTot(comm, region, year, timeslice)                Total commodity supply (weighted)
*@ mTechInpTot(comm, region, year, timeslice)
vTechInpTot(comm, region, year, timeslice)               Total commodity (main & aux) input to technologies (weighted)
*@ mTechOutTot(comm, region, year, timeslice)
vTechOutTot(comm, region, year, timeslice)               Total commodity (main & aux) output from technologies (weighted)
*@ mStorageInpTot(comm, region, year, timeslice)
vStorageInpTot(comm, region, year, timeslice)            Total commodity (main & aux) input to storage (weighted)
*@ mStorageOutTot(comm, region, year, timeslice)
vStorageOutTot(comm, region, year, timeslice)            Total commodity (main & aux) output from storage (weighted)
*@ mvStorageAInp(stg, comm, region, year, timeslice)
vStorageAInp(stg, comm, region, year, timeslice)         Aux-commodity input to storage
*@ mvStorageAOut(stg, comm, region, year, timeslice)
vStorageAOut(stg, comm, region, year, timeslice)         Aux-commodity input from storage
;
variable
* Costs variable
*@ mvTotalCost(region, year)
vTotalCost(region, year)                             Regional annual total costs (weighted)
vObjective                                           Objective costs
;
positive variables
* Dummy import
*@ mDummyImport(comm, region, year, timeslice)
vDummyImport(comm, region, year, timeslice)               Dummy import (for debugging)
*@ mDummyExport(comm, region, year, timeslice)
vDummyExport(comm, region, year, timeslice)               Dummy export (for debugging)
;
variable
* Taxes
*@ mTaxCost(comm, region, year)
vTaxCost(comm, region, year)                         Total tax levies (tax costs)
* Subsidies
*@ mSubCost(comm, region, year)
vSubsCost(comm, region, year)                        Total subsidies (substracted from costs)
*@ mAggOut(comm, region, year, timeslice)
vAggOutTot(comm, region, year, timeslice)                Aggregated commodity output (weighted)
*@ mDummyImportCost(comm, region, year)
vDummyImportCost(comm, region, year)   Dummy import costs (weighted)
*@ mDummyExportCost(comm, region, year)
vDummyExportCost(comm, region, year)   Dummy export costs (weighted)
;
* Reserves
positive variable
*@ mvStorageLevel(stg, comm, region, year, timeslice)
vStorageInp(stg, comm, region, year, timeslice)          Storage input
*@ mvStorageLevel(stg, comm, region, year, timeslice)
vStorageOut(stg, comm, region, year, timeslice)          Storage output
*@ mvStorageLevel(stg, comm, region, year, timeslice)
vStorageLevel(stg, comm, region, year, timeslice)        Storage level
*@ mStorageNew(stg, region, year)
vStorageInv(stg, region, year)                       Storage investments
*@ mStorageEac(stg, region, year)
vStorageEac(stg, region, year)                       Storage EAC investments
*@ mStorageSpan(stg, region, year)
vStorageOutCap(stg, region, year)                       Storage capacity
vStorageStgCap(stg, region, year)                       Storage energy (storing) capacity
vStorageInpCap(stg, region, year)                       Storage charging (input) capacity
vStorageInpNewCap(stg, region, year)                    Storage new charging (input) capacity
vStorageStgNewCap(stg, region, year)                    Storage new energy (storing) capacity
*@ mStorageNew(stg, region, year)
vStorageOutNewCap(stg, region, year)                    Storage new capacity
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
*@ mImport(comm, region, year, timeslice)
vImportTot(comm, region, year, timeslice)                Total regional import (Ir + ROW) (weighted)
*@ mExport(comm, region, year, timeslice)
vExportTot(comm, region, year, timeslice)                Total regional export (Ir + ROW) (weighted)
*@ mvTradeIr(trade, comm, region, region, year, timeslice)
vTradeIr(trade, comm, region, region, year, timeslice)   Total physical trade flows between regions
*@ mvTradeIrAInp(trade, comm, region, year, timeslice)
vTradeIrAInp(trade, comm, region, year, timeslice)       Trade auxilari input
*@ mvTradeIrAInpTot(comm, region, year, timeslice)
vTradeIrAInpTot(comm, region, year, timeslice)           Trade total auxilari input (weighted)
*@ mvTradeIrAOut(trade, comm, region, year, timeslice)
vTradeIrAOut(trade, comm, region, year, timeslice)       Trade auxilari output
*@ mvTradeIrAOutTot(comm, region, year, timeslice)
vTradeIrAOutTot(comm, region, year, timeslice)           Trade auxilari output total (weighted)
*@ mExpComm(expp, comm)
vExportRowCum(expp, comm)                            Cumulative export to the ROW
*@ mExportRow(expp, comm, region, year, timeslice)
vExportRow(expp, comm, region, year, timeslice)          Export to the ROW
*@ mImpComm(imp, comm)
vImportRowCum(imp, comm)                             Cumulative import from the ROW
*@ mImportRow(imp, comm, region, year, timeslice)
vImportRow(imp, comm, region, year, timeslice)           Import from the ROW
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
mTechInpTot(comm, region, year, timeslice)               Total technology input  mapp
mTechInpCommSameTimeslice(tech, comm)
mTechInpCommAgg(tech, comm)
mTechInpCommAggTimeslice(tech, comm, timeslice, timeslicep)
mTechAInpCommSameTimeslice(tech, comm)
mTechAInpCommAgg(tech, comm)
mTechAInpCommAggTimeslice(tech, comm, timeslice, timeslicep)
mTechOutTot(comm, region, year, timeslice)               Total technology output mapp
mTechOutCommSameTimeslice(tech, comm)
mTechOutCommAgg(tech, comm)
mTechOutCommAggTimeslice(tech, comm, timeslice, timeslicep)
mTechAOutCommSameTimeslice(tech, comm)
mTechAOutCommAgg(tech, comm)
mTechAOutCommAggTimeslice(tech, comm, timeslice, timeslicep)
mTechEac(tech, region, year)
*mTechOMCost(tech, region, year)  depreciate
mTechFixom(tech, region, year)   added
mTechVarom(tech, region, year)   added
mSupOutTot(comm, region, year, timeslice)
mEmsFuelTot(comm, region, year, timeslice)
mTechEmsFuel(tech, comm, comm, region, year, timeslice)
mDummyImport(comm, region, year, timeslice)
mDummyExport(comm, region, year, timeslice)
mDummyImportCost(comm, region, year)
mDummyExportCost(comm, region, year)
mTradeIr(trade, region, region, year, timeslice)
mvTradeIrAInp(trade, comm, region, year, timeslice)
mvTradeIrAInpTot(comm, region, year, timeslice)
mvTradeIrAOut(trade, comm, region, year, timeslice)
mvTradeIrAOutTot(comm, region, year, timeslice)
mImportRow(imp, comm, region, year, timeslice)
mImportRowUp(imp, comm, region, year, timeslice)
mImportRowCumUp(imp, comm)
mExportRow(expp, comm, region, year, timeslice)
mExportRowUp(expp, comm, region, year, timeslice)
mExportRowCumUp(expp, comm)
mExport(comm, region, year, timeslice)
mImport(comm, region, year, timeslice)
mStorageInpTot(comm, region, year, timeslice)
mStorageOutTot(comm, region, year, timeslice)
mTaxCost(comm, region, year)
mSubCost(comm, region, year)
mAggOut(comm, region, year, timeslice)
mTechAfUp(tech, region, year, timeslice)
mTechFullYear(tech)
mTechRampUp(tech, region, year, timeslice, timeslicep)
mTechRampDown(tech, region, year, timeslice, timeslicep)
*mTechCommTimesliceTimesliceP(tech, comm, timeslice, timeslicep)
mTechCommOutTimesliceTimesliceP(tech, comm, timeslice, timeslicep)
mTechCommAOutTimesliceTimesliceP(tech, comm, timeslice, timeslicep)
mTechOlifeInf(tech, region)
mStorageOlifeInf(stg, region)
mTechAfcUp(tech, comm, region, year, timeslice)
mSupAvaUp(sup, comm, region, year, timeslice)
mSupAva(sup, comm, region, year, timeslice)
mSupReserveUp(sup, comm, region)
;

sets
meqTechRetiredNewCap(tech, region, year)
meqTechSng2Sng(tech, region, comm, comm, year, timeslice)
meqTechGrp2Sng(tech, region, group, comm, year, timeslice)
meqTechSng2Grp(tech, region, comm, group, year, timeslice)
meqTechGrp2Grp(tech, region, group, group, year, timeslice)
meqTechShareInpLo(tech, region, group, comm, year, timeslice)
meqTechShareInpUp(tech, region, group, comm, year, timeslice)
meqTechShareOutLo(tech, region, group, comm, year, timeslice)
meqTechShareOutUp(tech, region, group, comm, year, timeslice)
meqTechAfLo(tech, region, year, timeslice)
meqTechAfUp(tech, region, year, timeslice)
meqTechAfsLo(tech, region, year, timeslice)
meqTechAfsUp(tech, region, year, timeslice)
meqTechActSng(tech, comm, region, year, timeslice)
meqTechActGrp(tech, group, region, year, timeslice)
meqTechAfcOutLo(tech, region, comm, year, timeslice)
meqTechAfcOutUp(tech, region, comm, year, timeslice)
meqTechAfcInpLo(tech, region, comm, year, timeslice)
meqTechAfcInpUp(tech, region, comm, year, timeslice)
meqSupAvaLo(sup, comm, region, year, timeslice)
meqSupReserveLo(sup, comm, region)
meqStorageAfLo(stg, comm, region, year, timeslice)
meqStorageAfUp(stg, comm, region, year, timeslice)
meqStorageInpUp(stg, comm, region, year, timeslice)
meqStorageInpLo(stg, comm, region, year, timeslice)
meqStorageOutUp(stg, comm, region, year, timeslice)
meqStorageOutLo(stg, comm, region, year, timeslice)
meqTradeFlowUp(trade, comm, region, region, year, timeslice)
meqTradeFlowLo(trade, comm, region, region, year, timeslice)
meqExportRowLo(expp, comm, region, year, timeslice)
meqImportRowUp(imp, comm, region, year, timeslice)
meqImportRowLo(imp, comm, region, year, timeslice)
meqTradeCapFlow(trade, comm, year, timeslice)
meqBalLo(comm, region, year, timeslice)
meqBalUp(comm, region, year, timeslice)
meqBalFx(comm, region, year, timeslice)
mTechAct2AInp(tech, comm, region, year, timeslice)
mTechCap2AInp(tech, comm, region, year, timeslice)
mTechNCap2AInp(tech, comm, region, year, timeslice)
mTechCinp2AInp(tech, comm, comm, region, year, timeslice)
mTechCout2AInp(tech, comm, comm, region, year, timeslice)
mTechAct2AOut(tech, comm, region, year, timeslice)
mTechCap2AOut(tech, comm, region, year, timeslice)
mTechNCap2AOut(tech, comm, region, year, timeslice)
mTechCinp2AOut(tech, comm, comm, region, year, timeslice)
mTechCout2AOut(tech, comm, comm, region, year, timeslice)
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
eqTechSng2Sng(tech, region, comm, commp, year, timeslice)      Technology input to output
eqTechGrp2Sng(tech, region, group, commp, year, timeslice)     Technology group input to output
eqTechSng2Grp(tech, region, comm, groupp, year, timeslice)     Technology input to group output
eqTechGrp2Grp(tech, region, group, groupp, year, timeslice)    Technology group input to group output
;

eqTechSng2Sng(tech, region, comm, commp, year, timeslice)$meqTechSng2Sng(tech, region, comm, commp, year, timeslice)..
  vTechOut(tech, commp, region, year, timeslice)
   =e=
   pTechCinp2use(tech, comm, region, year, timeslice)
   * pTechUse2cact(tech, commp, region, year, timeslice)
   * pTechCact2cout(tech, commp, region, year, timeslice)
   * vTechInp(tech, comm, region, year, timeslice)
;

eqTechGrp2Sng(tech, region, group, commp, year, timeslice)$meqTechGrp2Sng(tech, region, group, commp, year, timeslice)..
   pTechGinp2use(tech, group, region, year, timeslice) *
   sum(comm$mTechGroupComm(tech, group, comm),
           (vTechInp(tech, comm, region, year, timeslice) *
           pTechCinp2ginp(tech, comm, region, year, timeslice))$mvTechInp(tech, comm, region, year, timeslice)
   )
   =e=
   vTechOut(tech, commp, region, year, timeslice) /
           pTechUse2cact(tech, commp, region, year, timeslice) /
           pTechCact2cout(tech, commp, region, year, timeslice);


eqTechSng2Grp(tech, region, comm, groupp, year, timeslice)$meqTechSng2Grp(tech, region, comm, groupp, year, timeslice)..
   vTechInp(tech, comm, region, year, timeslice) *
   pTechCinp2use(tech, comm, region, year, timeslice)
   =e=
    sum(commp$mTechGroupComm(tech, groupp, commp),
           (vTechOut(tech, commp, region, year, timeslice) /
           pTechUse2cact(tech, commp, region, year, timeslice) /
           pTechCact2cout(tech, commp, region, year, timeslice))$mvTechOut(tech, commp, region, year, timeslice)
   );

eqTechGrp2Grp(tech, region, group, groupp, year, timeslice)$meqTechGrp2Grp(tech, region, group, groupp, year, timeslice)..
   pTechGinp2use(tech, group, region, year, timeslice) *
   sum(comm$mTechGroupComm(tech, group, comm),
           (vTechInp(tech, comm, region, year, timeslice) *
           pTechCinp2ginp(tech, comm, region, year, timeslice))$mvTechInp(tech, comm, region, year, timeslice)
   )
   =e=
   sum(commp$mTechGroupComm(tech, groupp, commp),
           (vTechOut(tech, commp, region, year, timeslice) /
           pTechUse2cact(tech, commp, region, year, timeslice) /
           pTechCact2cout(tech, commp, region, year, timeslice))$mvTechOut(tech, commp, region, year, timeslice)
   );

********************************************************************************
*** Shares for grouped commodities
********************************************************************************
Equations
* Input Share LO
eqTechShareInpLo(tech, region, group, comm, year, timeslice)    Technology lower bound on input share
* Input Share UP
eqTechShareInpUp(tech, region, group, comm, year, timeslice)    Technology upper bound on input share
* Output Share LO
eqTechShareOutLo(tech, region, group, comm, year, timeslice)    Technology lower bound on output share
* Output Share UP
eqTechShareOutUp(tech, region, group, comm, year, timeslice)    Technology upper bound on output share
;

* Lower bound on Input Share
eqTechShareInpLo(tech, region, group, comm, year, timeslice)$meqTechShareInpLo(tech, region, group, comm, year, timeslice)..
                  vTechInp(tech, comm, region, year, timeslice)
                  =g=
                  pTechShareLo(tech, comm, region, year, timeslice) *
                  sum(commp$mTechGroupComm(tech, group, commp),
                          vTechInp(tech, commp, region, year, timeslice)$mvTechInp(tech, commp, region, year, timeslice)
                  );

* Upper bound on Input Share
eqTechShareInpUp(tech, region, group, comm, year, timeslice)$meqTechShareInpUp(tech, region, group, comm, year, timeslice)..
                  vTechInp(tech, comm, region, year, timeslice)
                  =l=
                  pTechShareUp(tech, comm, region, year, timeslice) *
                  sum(commp$mTechGroupComm(tech, group, commp),
                          vTechInp(tech, commp, region, year, timeslice)$mvTechInp(tech, commp, region, year, timeslice)
                  );

* Lower bound on Output Share
eqTechShareOutLo(tech, region, group, comm, year, timeslice)$meqTechShareOutLo(tech, region, group, comm, year, timeslice)..
                  vTechOut(tech, comm, region, year, timeslice)
                  =g=
                  pTechShareLo(tech, comm, region, year, timeslice) *
                  sum(commp$mTechGroupComm(tech, group, commp),
                          vTechOut(tech, commp, region, year, timeslice)$mvTechOut(tech, commp, region, year, timeslice)
                  );

* Upper bound on Output Share
eqTechShareOutUp(tech, region, group, comm, year, timeslice)$meqTechShareOutUp(tech, region, group, comm, year, timeslice)..
                  vTechOut(tech, comm, region, year, timeslice)
                  =l=
                  pTechShareUp(tech, comm, region, year, timeslice) *
                  sum(commp$mTechGroupComm(tech, group, commp),
                          vTechOut(tech, commp, region, year, timeslice)$mvTechOut(tech, commp, region, year, timeslice)
                  );

********************************************************************************
*** Auxiliary input & output
********************************************************************************
equation
eqTechAInp(tech, comm, region, year, timeslice)   Technology auxiliary commodity input
eqTechAOut(tech, comm, region, year, timeslice)   Technology auxiliary commodity output
;

eqTechAInp(tech, comm, region, year, timeslice)$mvTechAInp(tech, comm, region, year, timeslice)..
  vTechAInp(tech, comm, region, year, timeslice) =e=
  (vTechAct(tech, region, year, timeslice) *
    pTechAct2AInp(tech, comm, region, year, timeslice))$mTechAct2AInp(tech, comm, region, year, timeslice) +
  (vTechCap(tech, region, year) *
    pTechCap2AInp(tech, comm, region, year, timeslice) / pTechCap2act(tech))$mTechCap2AInp(tech, comm, region, year, timeslice) +
  (vTechNewCap(tech, region, year) *
    pTechNCap2AInp(tech, comm, region, year, timeslice))$mTechNCap2AInp(tech, comm, region, year, timeslice) +
  sum(commp$mTechCinp2AInp(tech, comm, commp, region, year, timeslice),
      pTechCinp2AInp(tech, comm, commp, region, year, timeslice) *
         vTechInp(tech, commp, region, year, timeslice)) +
  sum(commp$mTechCout2AInp(tech, comm, commp, region, year, timeslice),
      pTechCout2AInp(tech, comm, commp, region, year, timeslice) *
         vTechOut(tech, commp, region, year, timeslice));

eqTechAOut(tech, comm, region, year, timeslice)$mvTechAOut(tech, comm, region, year, timeslice)..
  vTechAOut(tech, comm, region, year, timeslice) =e=
  (vTechAct(tech, region, year, timeslice) *
    pTechAct2AOut(tech, comm, region, year, timeslice))$mTechAct2AOut(tech, comm, region, year, timeslice) +
  (vTechCap(tech, region, year) *
    pTechCap2AOut(tech, comm, region, year, timeslice) / pTechCap2act(tech))$mTechCap2AOut(tech, comm, region, year, timeslice) +
  (vTechNewCap(tech, region, year) *
    pTechNCap2AOut(tech, comm, region, year, timeslice))$mTechNCap2AOut(tech, comm, region, year, timeslice) +
  sum(commp$mTechCinp2AOut(tech, comm, commp, region, year, timeslice),
      pTechCinp2AOut(tech, comm, commp, region, year, timeslice) *
         vTechInp(tech, commp, region, year, timeslice)) +
  sum(commp$mTechCout2AOut(tech, comm, commp, region, year, timeslice),
      pTechCout2AOut(tech, comm, commp, region, year, timeslice) *
         vTechOut(tech, commp, region, year, timeslice));

********************************************************************************
*** Availability
********************************************************************************
Equation
* Availability factor LO
eqTechAfLo(tech, region, year, timeslice) Technology availability factor lower bound
* Availability factor UP
eqTechAfUp(tech, region, year, timeslice) Technology availability factor upper bound
* Availability sum factor LO
eqTechAfsLo(tech, region, year, timeslice) Technology availability factor for sum of timeslices lower bound
* Availability sum factor UP
eqTechAfsUp(tech, region, year, timeslice) Technology availability factor for sum of timeslices upper bound
* Ramp Up factor
eqTechRampUp(tech, region, year, timeslice, timeslicep) Technology ramp up
* Ramp Down factor
eqTechRampDown(tech, region, year, timeslice, timeslicep) Technology ramp down
;

* Availability factor LO
eqTechAfLo(tech, region, year, timeslice)$meqTechAfLo(tech, region, year, timeslice)..
         pTechAfLo(tech, region, year, timeslice) *
*         pYearFraction(year) *
         pTechCap2act(tech) *
         vTechCap(tech, region, year) *
         pTimesliceShare(timeslice)  *
         prod(weather$mTechWeatherAfLo(weather, tech),
              pTechWeatherAfLo(weather, tech) *
              pWeather(weather, region, year, timeslice)
              )
         =l=
         vTechAct(tech, region, year, timeslice);

* Availability factor UP
eqTechAfUp(tech, region, year, timeslice)$meqTechAfUp(tech, region, year, timeslice)..
         vTechAct(tech, region, year, timeslice)
         =l=
         pTechAfUp(tech, region, year, timeslice) *
*         pYearFraction(year) *
         pTechCap2act(tech) *
         vTechCap(tech, region, year) *
         pTimesliceShare(timeslice) *
         prod(weather$mTechWeatherAfUp(weather, tech),
              pTechWeatherAfUp(weather, tech) *
              pWeather(weather, region, year, timeslice)
              );

* Availability factor for sum LO
eqTechAfsLo(tech, region, year, timeslice)$meqTechAfsLo(tech, region, year, timeslice)..
         pTechAfsLo(tech, region, year, timeslice) *
*         pYearFraction(year) *
         pTechCap2act(tech) *
         vTechCap(tech, region, year) *
         pTimesliceShare(timeslice)  *
         prod(weather$mTechWeatherAfsLo(weather, tech),
              pTechWeatherAfsLo(weather, tech) *
              pWeather(weather, region, year, timeslice)
              )
         =l=
         sum(timeslicep$mTimesliceParentChildE(timeslice, timeslicep),
             vTechAct(tech, region, year, timeslicep)$mvTechAct(tech, region, year, timeslicep));

* Availability factor for sum UP
eqTechAfsUp(tech, region, year, timeslice)$meqTechAfsUp(tech, region, year, timeslice)..
         sum(timeslicep$mTimesliceParentChildE(timeslice, timeslicep),
         vTechAct(tech, region, year, timeslicep)$mvTechAct(tech, region, year, timeslicep))
         =l=
         pTechAfsUp(tech, region, year, timeslice) *
*         pYearFraction(year) *
         pTechCap2act(tech) *
         vTechCap(tech, region, year) *
         pTimesliceShare(timeslice) *  prod(weather$mTechWeatherAfsUp(weather, tech),
            pTechWeatherAfsUp(weather, tech) * pWeather(weather, region, year, timeslice));

* Ramp Up factor - new mapping
eqTechRampUp(tech, region, year, timeslice, timeslicep)$mTechRampUp(tech, region, year, timeslice, timeslicep)..
         vTechAct(tech, region, year, timeslice) / pTimesliceShare(timeslice)
         - vTechAct(tech, region, year, timeslicep) / pTimesliceShare(timeslicep)
         =l=
         pTimesliceShare(timeslice) * pTechCap2act(tech)
*         * pYearFraction(year)
         / pTechRampUp(tech, region, year, timeslice)
*         * pYearFraction(year)
         * pTechCap2act(tech) * vTechCap(tech, region, year);

* Ramp Down factor - new mapping
eqTechRampDown(tech, region, year, timeslice, timeslicep)$mTechRampDown(tech, region, year, timeslice, timeslicep)..
         vTechAct(tech, region, year, timeslicep) / pTimesliceShare(timeslicep)
         - vTechAct(tech, region, year, timeslice) / pTimesliceShare(timeslice)
         =l=
         pTimesliceShare(timeslice) * pTechCap2act(tech)
*         * pYearFraction(year)
         / pTechRampDown(tech, region, year, timeslice)
*         * pYearFraction(year)
         * pTechCap2act(tech) * vTechCap(tech, region, year);

********************************************************************************
*** Connect activity with output
********************************************************************************
Equation
* Connect activity with output
eqTechActSng(tech, comm, region, year, timeslice)  Technology activity to commodity output
eqTechActGrp(tech, group, region, year, timeslice) Technology activity to group output
;

* Connect activity with output
eqTechActSng(tech, comm, region, year, timeslice)$meqTechActSng(tech, comm, region, year, timeslice)..
  vTechAct(tech, region, year, timeslice) =e=
                 vTechOut(tech, comm, region, year, timeslice) /
                 pTechCact2cout(tech, comm, region, year, timeslice);

eqTechActGrp(tech, group, region, year, timeslice)$meqTechActGrp(tech, group, region, year, timeslice)..
    vTechAct(tech, region, year, timeslice) =e=
         sum(comm$mTechGroupComm(tech, group, comm),
                 (vTechOut(tech, comm, region, year, timeslice) /
                 pTechCact2cout(tech, comm, region, year, timeslice))$mvTechOut(tech, comm, region, year, timeslice)
         );

********************************************************************************
*** Availability commodity factor
********************************************************************************
Equation
* Availability commodity factor LO output equations
eqTechAfcOutLo(tech, region, comm, year, timeslice) Technology commodity availability factor lower bound
* Availability commodity factor UP output equations
eqTechAfcOutUp(tech, region, comm, year, timeslice) Technology commodity availability factor upper bound
* Availability commodity factor LO input equations
eqTechAfcInpLo(tech, region, comm, year, timeslice) Technology commodity availability factor lower bound
* Availability commodity factor UP input equations
eqTechAfcInpUp(tech, region, comm, year, timeslice) Technology commodity availability factor upper bound
;

* Availability commodity factor LO output equations
eqTechAfcOutLo(tech, region, comm, year, timeslice)$meqTechAfcOutLo(tech, region, comm, year, timeslice)..
         pTechCact2cout(tech, comm, region, year, timeslice) *
         pTechAfcLo(tech, comm, region, year, timeslice) *
*         pYearFraction(year) *
         pTechCap2act(tech) *
         vTechCap(tech, region, year) *
         pTimesliceShare(timeslice) * prod(weather$mTechWeatherAfcLo(weather, tech, comm),
            pTechWeatherAfcLo(weather, tech, comm) * pWeather(weather, region, year, timeslice))
         =l=
         vTechOut(tech, comm, region, year, timeslice);

* Availability commodity factor UP output equations
eqTechAfcOutUp(tech, region, comm, year, timeslice)$meqTechAfcOutUp(tech, region, comm, year, timeslice)..
         vTechOut(tech, comm, region, year, timeslice)
         =l=
         pTechCact2cout(tech, comm, region, year, timeslice) *
         pTechAfcUp(tech, comm, region, year, timeslice) *
*         pYearFraction(year) *
         pTechCap2act(tech) *
         vTechCap(tech, region, year) *  prod(weather$mTechWeatherAfcUp(weather, tech, comm),
            pTechWeatherAfcUp(weather, tech, comm) * pWeather(weather, region, year, timeslice));

* Availability commodity factor LO input equations
eqTechAfcInpLo(tech, region, comm, year, timeslice)$meqTechAfcInpLo(tech, region, comm, year, timeslice)..
         pTechAfcLo(tech, comm, region, year, timeslice) *
*         pYearFraction(year) *
         pTechCap2act(tech) *
         vTechCap(tech, region, year) *
         pTimesliceShare(timeslice)  *  prod(weather$mTechWeatherAfcLo(weather, tech, comm),
            pTechWeatherAfcLo(weather, tech, comm) * pWeather(weather, region, year, timeslice))
         =l=
         vTechInp(tech, comm, region, year, timeslice);

* Availability commodity factor UP input equations
eqTechAfcInpUp(tech, region, comm, year, timeslice)$meqTechAfcInpUp(tech, region, comm, year, timeslice)..
         vTechInp(tech, comm, region, year, timeslice)
         =l=
         pTechAfcUp(tech, comm, region, year, timeslice) *
*         pYearFraction(year) *
         pTechCap2act(tech) *
         vTechCap(tech, region, year) *
         pTimesliceShare(timeslice)  *  prod(weather$mTechWeatherAfcUp(weather, tech, comm),
            pTechWeatherAfcUp(weather, tech, comm) * pWeather(weather, region, year, timeslice));

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
* [eac-fix] vintaged new-capacity form active (pTechEac applies to NEW capacity only)
eqTechEac(tech, region, year)$mTechEac(tech, region, year)..
         vTechEac(tech, region, year)
* rewritten to include EAC for existing capacity
         =e=
         sum(yearp$(mTechNew(tech, region, yearp)
                    and
                    ordYear(year) >= ordYear(yearp)
                    and
*                  [payback] the annuity is charged over the COST-RECOVERY period:
*                  pTechPayback where the user set one, otherwise the operational
*                  life as before. Ported from the GLPK model 2026-08-19; the
*                  parameters were already declared here "for parity" (see above).
                    (
                     (pTechPayback(tech, region, yearp) > 0
                      and
                      ordYear(year) < pTechPayback(tech, region, yearp) + ordYear(yearp)
                      )
                     or
                     (pTechPayback(tech, region, yearp) <= 0
                      and
                      (ordYear(year) < pTechOlife(tech, region) + ordYear(yearp)
                       or
                       mTechOlifeInf(tech, region)
                       )
                      )
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
$ontext
* [eac-fix] simplified "EAC for existing capacity" form disabled:
eqTechEac(tech, region, year)$mTechSpan(tech, region, year)..
         vTechEac(tech, region, year)
* rewritten to include EAC for existing capacity
         =e=
         pTechEac(tech, region, year) * vTechCap(tech, region, year)
;
$offtext

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
        sum(timeslice$mTechTimeslice(tech, timeslice),
            pTechVarom(tech, region, year, timeslice)
            * pTimesliceWeight(year, timeslice)
            * vTechAct(tech, region, year, timeslice) +
            sum(comm$mTechInpComm(tech, comm),
                pTechCvarom(tech, comm, region, year, timeslice)
                * pTimesliceWeight(year, timeslice)
                * vTechInp(tech, comm, region, year, timeslice)
            )
            +
            sum(comm$mTechOutComm(tech, comm),
                pTechCvarom(tech, comm, region, year, timeslice)
                * pTimesliceWeight(year, timeslice)
                * vTechOut(tech, comm, region, year, timeslice)
            )
            +
            sum(comm$mvTechAOut(tech, comm, region, year, timeslice),
                pTechAvarom(tech, comm, region, year, timeslice)
                * pTimesliceWeight(year, timeslice)
                * vTechAOut(tech, comm, region, year, timeslice)
            )
            +
            sum(comm$mvTechAInp(tech, comm, region, year, timeslice),
                pTechAvarom(tech, comm, region, year, timeslice)
                * pTimesliceWeight(year, timeslice)
                * vTechAInp(tech, comm, region, year, timeslice)
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
        sum(timeslice$mTechTimeslice(tech, timeslice),
            pTechVarom(tech, region, year, timeslice)
            * pTimesliceWeight(year, timeslice)
            * vTechAct(tech, region, year, timeslice)
            +
            sum(comm$mTechInpComm(tech, comm),
                pTechCvarom(tech, comm, region, year, timeslice)
                * pTimesliceWeight(year, timeslice)
                * vTechInp(tech, comm, region, year, timeslice))
            +
            sum(comm$mTechOutComm(tech, comm),
                pTechCvarom(tech, comm, region, year, timeslice)
                * pTimesliceWeight(year, timeslice)
                * vTechOut(tech, comm, region, year, timeslice))
            +
            sum(comm$mvTechAOut(tech, comm, region, year, timeslice),
                pTechAvarom(tech, comm, region, year, timeslice)
                * pTimesliceWeight(year, timeslice)
                * vTechAOut(tech, comm, region, year, timeslice))
            +
            sum(comm$mvTechAInp(tech, comm, region, year, timeslice),
                pTechAvarom(tech, comm, region, year, timeslice)
                * pTimesliceWeight(year, timeslice)
                * vTechAInp(tech, comm, region, year, timeslice))
        );


**************************************
** Supply
**************************************
Equation
eqSupAvaUp(sup, comm, region, year, timeslice)  Supply availability upper bound
eqSupAvaLo(sup, comm, region, year, timeslice)  Supply availability lower bound
eqSupReserve(sup, comm, region)             Cumulative supply (use of reserve)
eqSupReserveUp(sup, comm, region)           Cumulative supply upper constraint
eqSupReserveLo(sup, comm, region)           Cumulative supply lower constraint
eqSupCost(sup, region, year)                Supply costs
;

eqSupAvaUp(sup, comm, region, year, timeslice)$mSupAvaUp(sup, comm, region, year, timeslice)..
         vSupOut(sup, comm, region, year, timeslice)
         =l=
         pSupAvaUp(sup, comm, region, year, timeslice)
         * prod(weather$mSupWeatherUp(weather, sup),
                pSupWeatherUp(weather, sup)
                * pWeather(weather, region, year, timeslice));

eqSupAvaLo(sup, comm, region, year, timeslice)$meqSupAvaLo(sup, comm, region, year, timeslice)..
         vSupOut(sup, comm, region, year, timeslice)
         =g=
         pSupAvaLo(sup, comm, region, year, timeslice)
         * prod(weather$mSupWeatherLo(weather, sup),
                pSupWeatherLo(weather, sup)
                * pWeather(weather, region, year, timeslice));

eqSupReserve(sup, comm, region)$mvSupReserve(sup, comm, region)..
         vSupReserve(sup, comm, region)
         =e=
         sum((year, timeslice)$mSupAva(sup, comm, region, year, timeslice),
             pPeriodLen(year) * pTimesliceWeight(year, timeslice)
             * vSupOut(sup, comm, region, year, timeslice)
*              / pYearFraction(year)
         );

eqSupReserveUp(sup, comm, region)$mSupReserveUp(sup, comm, region)..
         vSupReserve(sup, comm, region) =l= pSupReserveUp(sup, comm, region);

eqSupReserveLo(sup, comm, region)$meqSupReserveLo(sup, comm, region)..
         vSupReserve(sup, comm, region) =g= pSupReserveLo(sup, comm, region);

eqSupCost(sup, region, year)$mvSupCost(sup, region, year)..
         vSupCost(sup, region, year)
         =e=
         sum((comm, timeslice)$mSupAva(sup, comm, region, year, timeslice),
             pSupCost(sup, comm, region, year, timeslice)
             * pTimesliceWeight(year, timeslice)
             * vSupOut(sup, comm, region, year, timeslice));

**************************************
** Demand
**************************************
Equation
eqDemInp(comm, region, year, timeslice)  Demand equation
;

eqDemInp(comm, region, year, timeslice)$mvDemInp(comm, region, year, timeslice)..
         vDemInp(comm, region, year, timeslice)
         =e=
         sum(dem$mDemComm(dem, comm), pDemand(dem, comm, region, year, timeslice));

********************************************************************************
** Emission & Aggregating commodity equation
********************************************************************************
Equation
eqAggOutTot(comm, region, year, timeslice)         Aggregating-commodity output (weighted)
eqEmsFuelTot(comm, region, year, timeslice)        Total emissions from commodity consumption (weighted)
;

eqAggOutTot(comm, region, year, timeslice)$mAggOut(comm, region, year, timeslice)..
         vAggOutTot(comm, region, year, timeslice)
         =e=
         sum(commp$mAggregateFactor(comm, commp),
             pAggregateFactor(comm, commp)
             * sum(timeslicep$(mvOutTot(comm, region, year, timeslicep)
                           and
                           mTimesliceParentChildE(timeslice, timeslicep)
                           and mCommTimeslice(commp, timeslicep)
                           ),
                    vOutTot(commp, region, year, timeslicep)$mvOutTot(commp, region, year, timeslicep)
                    )
            );

eqEmsFuelTot(comm, region, year, timeslice)$mEmsFuelTot(comm, region, year, timeslice)..
     vEmsFuelTot(comm, region, year, timeslice)
         =e=
         sum(commp$(pEmissionFactor(comm, commp) > 0),
             pEmissionFactor(comm, commp)
             * sum(tech$mTechInpComm(tech, commp),
                   pTechEmisComm(tech, commp)
*                   * pTimesliceWeight(year, timeslice)
                   * sum(timeslicep$mCommTimesliceOrParent(comm, timeslice, timeslicep),
*                         pTimesliceWeight(year, timeslicep) *
                         vTechInp(tech, commp, region, year, timeslicep)$mTechEmsFuel(
                                  tech, comm, commp, region, year, timeslicep)
                         )
                  )
           );
*          ) * pTimesliceWeight(year, timeslice);

********************************************************************************
** Storage
********************************************************************************
********************************************************************************
*** Input & Output
********************************************************************************
Equations
* [rename] eqStorageStore -> eqStorageLevel, vStorageStore -> vStorageLevel
* (with mvStorageStore/meqStorageStore following) 2026-08-19. The level is a
* STATE, not a flow -- summing it over timeslices is meaningless -- and the old
* name read as a verb. The rename was already flagged here before it was made.
* eqStorageLevelPS / eqStorageLevelFY were placeholders for splitting the
* parent-timeslice and full-year storage cycles into separate equations. That
* split never happened: the choice lives entirely in meqStorageLevel, which
* picks mTimesliceNext or mTimesliceFYearNext per storage (see @fullYear).
eqStorageLevel(stg, comm, region, year, timeslicep, timeslice)  Storage level
eqStorageAfLo(stg, comm, region, year, timeslice)   Storage availability factor lower
eqStorageAfUp(stg, comm, region, year, timeslice)   Storage availability factor upper
* [rename] eqStorageClear -> eqStorageOutLevel 2026-08-19. "Clear" said nothing;
* the equation bounds vStorageOut by the level available. It takes no Lo/Up
* suffix because that marks a PAIRED bound (a .lo and .up from one parameter) --
* cf. eqTradeCapFlow, eqTechRetiredStockCum. There is no "discharge at least the
* level". The dead eqStorageClean placeholder alongside it is removed.
eqStorageOutLevel(stg, comm, region, year, timeslice)  Storage output limited by the level
eqStorageAInp(stg, comm, region, year, timeslice)   Storage aux-commodity input
eqStorageAOut(stg, comm, region, year, timeslice)   Storage aux-commodity output
eqStorageInpUp(stg, comm, region, year, timeslice)  Storage input upper constraint
eqStorageInpLo(stg, comm, region, year, timeslice)  Storage input lower constraint
eqStorageOutUp(stg, comm, region, year, timeslice)  Storage output upper constraint
eqStorageOutLo(stg, comm, region, year, timeslice)  Storage output lower constraint
;

eqStorageAInp(stg, comm, region, year, timeslice)$mvStorageAInp(stg, comm, region, year, timeslice)..
  vStorageAInp(stg, comm, region, year, timeslice) =e=
* [multi-commodity] each referenced flow follows its OWN role's commodity set,
* and the two capacity terms sit OUTSIDE the commodity sum -- inside it they were
* multiplied by 1 for a single-commodity storage but would be counted once per
* commodity now that the roles can differ.
    sum(commp$mStorageStgComm(stg, commp),
        (pStorageStg2AInp(stg, comm, region, year, timeslice)
        * vStorageLevel(stg, commp, region, year, timeslice)
        )$mStorageStg2AInp(stg, comm, region, year, timeslice))
      + sum(commp$mStorageInpComm(stg, commp),
        (pStorageCinp2AInp(stg, comm, region, year, timeslice)
          * vStorageInp(stg, commp, region, year, timeslice)
        )$mStorageCinp2AInp(stg, comm, region, year, timeslice))
      + sum(commp$mStorageOutComm(stg, commp),
        (pStorageCout2AInp(stg, comm, region, year, timeslice)
          * vStorageOut(stg, commp, region, year, timeslice)
        )$mStorageCout2AInp(stg, comm, region, year, timeslice))
      + (pStorageCap2AInp(stg, comm, region, year, timeslice)
           * vStorageOutCap(stg, region, year)
        )$mStorageCap2AInp(stg, comm, region, year, timeslice)
      + (pStorageNCap2AInp(stg, comm, region, year, timeslice)
           * vStorageOutNewCap(stg, region, year)
        )$mStorageNCap2AInp(stg, comm, region, year, timeslice)
    ;

eqStorageAOut(stg, comm, region, year, timeslice)$mvStorageAOut(stg, comm, region, year, timeslice)..
  vStorageAOut(stg, comm, region, year, timeslice)
  =e=
* [multi-commodity] each referenced flow follows its OWN role's commodity set,
* and the two capacity terms sit OUTSIDE the commodity sum -- inside it they were
* multiplied by 1 for a single-commodity storage but would be counted once per
* commodity now that the roles can differ.
  sum(commp$mStorageStgComm(stg, commp),
      (pStorageStg2AOut(stg, comm, region, year, timeslice)
       * vStorageLevel(stg, commp, region, year, timeslice)
      )$mStorageStg2AOut(stg, comm, region, year, timeslice))
      +
      sum(commp$mStorageInpComm(stg, commp),
      (pStorageCinp2AOut(stg, comm, region, year, timeslice)
       * vStorageInp(stg, commp, region, year, timeslice)
      )$mStorageCinp2AOut(stg, comm, region, year, timeslice))
      +
      sum(commp$mStorageOutComm(stg, commp),
      (pStorageCout2AOut(stg, comm, region, year, timeslice)
       * vStorageOut(stg, commp, region, year, timeslice)
      )$mStorageCout2AOut(stg, comm, region, year, timeslice))
      +
      (pStorageCap2AOut(stg, comm, region, year, timeslice)
       * vStorageOutCap(stg, region, year)
      )$mStorageCap2AOut(stg, comm, region, year, timeslice)
      +
      (pStorageNCap2AOut(stg, comm, region, year, timeslice)
       * vStorageOutNewCap(stg, region, year)
      )$mStorageNCap2AOut(stg, comm, region, year, timeslice)
;

* timeslicep == timeslice[-1]
eqStorageLevel(stg, comm, region, year, timeslicep, timeslice)$meqStorageLevel(stg, comm, region, year, timeslicep, timeslice)..
  vStorageLevel(stg, comm, region, year, timeslice)
  =e=
  pStorageStartLevel(stg, comm, region, year, timeslice)
  + (pStorageNCap2Stg(stg, comm, region, year, timeslice)
     * vStorageOutNewCap(stg, region, year)
    )$mStorageNew(stg, region, year)
  +
*  sum(timeslicep$(mCommTimeslice(comm, timeslicep) and mTimesliceNext(timeslicep, timeslice)),
* [multi-commodity] the level is kept in `comm`, but what fills and empties it
* need not be the same thing (ELC in, H2 held, ELC out). Each side sums over its
* OWN role's commodities, and inpeff/outeff become cross-commodity conversion
* factors. With one commodity all three sets coincide, each sum has a single term
* and this is the previous equation exactly.
  sum(commp$mvStorageInp(stg, commp, region, year, timeslicep),
      pStorageInpEff(stg, commp, region, year, timeslicep)
      * vStorageInp(stg, commp, region, year, timeslicep))
  + (pStorageStgEff(stg, comm, region, year, timeslice) ** pTimesliceShare(timeslice))
    * vStorageLevel(stg, comm, region, year, timeslicep)
  - sum(commp$mvStorageOut(stg, commp, region, year, timeslicep),
        vStorageOut(stg, commp, region, year, timeslicep)
        / pStorageOutEff(stg, commp, region, year, timeslicep))
*        )
;

eqStorageAfLo(stg, comm, region, year, timeslice)$meqStorageAfLo(stg, comm, region, year, timeslice)..
    vStorageLevel(stg, comm, region, year, timeslice)
    =g=
    pStorageAfLo(stg, region, year, timeslice) *
      (vStorageStgCap(stg, region, year)$mStorageStgCap(stg, region, year)
       + (pStorageDurationLo(stg, region, year)
          * vStorageOutCap(stg, region, year)
         )$mStorageNoStgCap(stg, region, year))
      * prod(weather$mStorageWeatherAfLo(weather, stg),
             pStorageWeatherAfLo(weather, stg)
             * pWeather(weather, region, year, timeslice));

eqStorageAfUp(stg, comm, region, year, timeslice)$meqStorageAfUp(stg, comm, region, year, timeslice)..
    vStorageLevel(stg, comm, region, year, timeslice)
    =l=
    pStorageAfUp(stg, region, year, timeslice) *
      (vStorageStgCap(stg, region, year)$mStorageStgCap(stg, region, year)
       + (pStorageDurationUp(stg, region, year)
          * vStorageOutCap(stg, region, year)
         )$mStorageNoStgCap(stg, region, year))
      * prod(weather$mStorageWeatherAfUp(weather, stg),
             pStorageWeatherAfUp(weather, stg)
             * pWeather(weather, region, year, timeslice));

* Draw only from what was ALREADY stored. This is stricter than the level's own
* non-negativity, which would also permit discharging energy charged in the SAME
* timeslice (pass-through) and would carry the decay factor; this does neither.
eqStorageOutLevel(stg, comm, region, year, timeslice)$mvStorageLevel(stg, comm, region, year, timeslice)..
  sum(commp$mvStorageOut(stg, commp, region, year, timeslice),
      vStorageOut(stg, commp, region, year, timeslice)
      / pStorageOutEff(stg, commp, region, year, timeslice))
*   * pStorageDuration(stg)
   =l=
   vStorageLevel(stg, comm, region, year, timeslice);

* Input constraints
eqStorageInpUp(stg, comm, region, year, timeslice)$meqStorageInpUp(stg, comm, region, year, timeslice)..
  vStorageInp(stg, comm, region, year, timeslice) =l=
*    pStorageDuration(stg) *
    (vStorageInpCap(stg, region, year)$mStorageInpCap(stg, region, year)
     + (pStorageInp2outUp(stg, region, year)
        * vStorageOutCap(stg, region, year)
       )$mStorageNoInpCap(stg, region, year))
*   [rate] capacity is per HOUR, not per timeslice -- see pStorageInpCap2act
    * pStorageInpCap2act(stg) * pTimesliceShare(timeslice)
    * pStorageCinpUp(stg, comm, region, year, timeslice)
*         * pTimesliceShare(timeslice) *
    * prod(weather$mStorageWeatherCinpUp(weather, stg),
           pStorageWeatherCinpUp(weather, stg)
           * pWeather(weather, region, year, timeslice));

eqStorageInpLo(stg, comm, region, year, timeslice)$meqStorageInpLo(stg, comm, region, year, timeslice)..
  vStorageInp(stg, comm, region, year, timeslice) =g=
*    pStorageDuration(stg) *
    (vStorageInpCap(stg, region, year)$mStorageInpCap(stg, region, year)
     + (pStorageInp2outLo(stg, region, year)
        * vStorageOutCap(stg, region, year)
       )$mStorageNoInpCap(stg, region, year))
*   [rate] capacity is per HOUR, not per timeslice -- see pStorageInpCap2act
    * pStorageInpCap2act(stg) * pTimesliceShare(timeslice)
    * pStorageCinpLo(stg, comm, region, year, timeslice)
*    * pTimesliceShare(timeslice)
    * prod(weather$mStorageWeatherCinpLo(weather, stg),
           pStorageWeatherCinpLo(weather, stg)
           * pWeather(weather, region, year, timeslice));

* Output constraints
eqStorageOutUp(stg, comm, region, year, timeslice)$meqStorageOutUp(stg, comm, region, year, timeslice)..
  vStorageOut(stg, comm, region, year, timeslice) =l=
*    pStorageDuration(stg) *
    vStorageOutCap(stg, region, year)
    * pStorageOutCap2act(stg) * pTimesliceShare(timeslice)
    * pStorageCoutUp(stg, comm, region, year, timeslice)
    * prod(weather$mStorageWeatherCoutUp(weather, stg),
           pStorageWeatherCoutUp(weather, stg)
           * pWeather(weather, region, year, timeslice));

eqStorageOutLo(stg, comm, region, year, timeslice)$meqStorageOutLo(stg, comm, region, year, timeslice)..
  vStorageOut(stg, comm, region, year, timeslice)  =g=
*    pStorageDuration(stg) *
    vStorageOutCap(stg, region, year)
    * pStorageOutCap2act(stg) * pTimesliceShare(timeslice)
    * pStorageCoutLo(stg, comm, region, year, timeslice)
    * prod(weather$mStorageWeatherCoutLo(weather, stg),
           pStorageWeatherCoutLo(weather, stg)
           * pWeather(weather, region, year, timeslice));

********************************************************************************
*** Capacity and costs for storage
********************************************************************************
Equation
* Capacity equations
eqStorageCap(stg, region, year)       Storage capacity
eqStorageInpCap(stg, region, year)      Storage charging capacity accounting
eqStorageInpCapLo(stg, region, year)    Storage charging capacity lower bound
eqStorageInpCapUp(stg, region, year)    Storage charging capacity upper bound
eqStorageInpNewCapLo(stg, region, year) Storage new charging capacity lower bound
eqStorageInpNewCapUp(stg, region, year) Storage new charging capacity upper bound
eqStorageInp2outLo(stg, region, year)   Storage charge-to-discharge ratio lower bound
eqStorageInp2outUp(stg, region, year)   Storage charge-to-discharge ratio upper bound
eqStorageStgCap(stg, region, year)      Storage energy capacity accounting
eqStorageStgCapLo(stg, region, year)    Storage energy capacity lower bound
eqStorageStgCapUp(stg, region, year)    Storage energy capacity upper bound
eqStorageStgNewCapLo(stg, region, year) Storage new energy capacity lower bound
eqStorageStgNewCapUp(stg, region, year) Storage new energy capacity upper bound
eqStorageDurationLo(stg, region, year)  Storage energy-to-power ratio lower bound
eqStorageDurationUp(stg, region, year)  Storage energy-to-power ratio upper bound
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
         vStorageOutCap(stg, region, year)
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
                 vStorageOutNewCap(stg, region, yearp)
         );

* [2c] The CHARGING side has its own capacity, in power, rated independently
* of the discharger. Where the charging part carries no data this block emits
* nothing and the input bounds inline inp2out * output capacity instead.
eqStorageInpCap(stg, region, year)$mStorageInpCap(stg, region, year)..
  vStorageInpCap(stg, region, year)
  =e=
  pStorageInpStock(stg, region, year)
  + sum(yearp$(ord(year) >= ord(yearp)
        and (mStorageOlifeInf(stg, region)
             or ord(year) < pStorageOlife(stg, region) + ord(yearp))
        and mStorageInpNew(stg, region, yearp)),
      pPeriodLen(yearp) * vStorageInpNewCap(stg, region, yearp));

eqStorageInpCapLo(stg, region, year)$mStorageInpCapLo(stg, region, year)..
  vStorageInpCap(stg, region, year) =g= pStorageInpCapLo(stg, region, year);

eqStorageInpCapUp(stg, region, year)$mStorageInpCapUp(stg, region, year)..
  vStorageInpCap(stg, region, year) =l= pStorageInpCapUp(stg, region, year);

eqStorageInpNewCapLo(stg, region, year)$mStorageInpNewCapLo(stg, region, year)..
  vStorageInpNewCap(stg, region, year) =g= pStorageInpNewCapLo(stg, region, year) * pPeriodLen(year);

eqStorageInpNewCapUp(stg, region, year)$mStorageInpNewCapUp(stg, region, year)..
  vStorageInpNewCap(stg, region, year) =l= pStorageInpNewCapUp(stg, region, year) * pPeriodLen(year);

* The inp2out LINK: charging capacity per unit of discharging capacity.
eqStorageInp2outLo(stg, region, year)$mStorageInp2outLo(stg, region, year)..
  vStorageInpCap(stg, region, year) =g= pStorageInp2outLo(stg, region, year) * vStorageOutCap(stg, region, year);

eqStorageInp2outUp(stg, region, year)$mStorageInp2outUp(stg, region, year)..
  vStorageInpCap(stg, region, year) =l= pStorageInp2outUp(stg, region, year) * vStorageOutCap(stg, region, year);

* [2c] The STORING side's own capacity, in ENERGY. Emitted only where the
* storing part carries data; elsewhere the af bounds inline duration * power.
eqStorageStgCap(stg, region, year)$mStorageStgCap(stg, region, year)..
  vStorageStgCap(stg, region, year)
  =e=
  pStorageStgStock(stg, region, year)
  + sum(yearp$(ord(year) >= ord(yearp)
        and (mStorageOlifeInf(stg, region)
             or ord(year) < pStorageOlife(stg, region) + ord(yearp))
        and mStorageStgNew(stg, region, yearp)),
      pPeriodLen(yearp) * vStorageStgNewCap(stg, region, yearp));

eqStorageStgCapLo(stg, region, year)$mStorageStgCapLo(stg, region, year)..
  vStorageStgCap(stg, region, year) =g= pStorageStgCapLo(stg, region, year);

eqStorageStgCapUp(stg, region, year)$mStorageStgCapUp(stg, region, year)..
  vStorageStgCap(stg, region, year) =l= pStorageStgCapUp(stg, region, year);

eqStorageStgNewCapLo(stg, region, year)$mStorageStgNewCapLo(stg, region, year)..
  vStorageStgNewCap(stg, region, year) =g= pStorageStgNewCapLo(stg, region, year) * pPeriodLen(year);

eqStorageStgNewCapUp(stg, region, year)$mStorageStgNewCapUp(stg, region, year)..
  vStorageStgNewCap(stg, region, year) =l= pStorageStgNewCapUp(stg, region, year) * pPeriodLen(year);

* The duration LINK, in hours. `.fx` collapses these two onto each other.
eqStorageDurationLo(stg, region, year)$mStorageDurationLo(stg, region, year)..
  vStorageStgCap(stg, region, year) =g= pStorageDurationLo(stg, region, year) * vStorageOutCap(stg, region, year);

eqStorageDurationUp(stg, region, year)$mStorageDurationUp(stg, region, year)..
  vStorageStgCap(stg, region, year) =l= pStorageDurationUp(stg, region, year) * vStorageOutCap(stg, region, year);

eqStorageCapLo(stg, region, year)$mStorageCapLo(stg, region, year)..
         vStorageOutCap(stg, region, year) =g= pStorageCapLo(stg, region, year);

eqStorageCapUp(stg, region, year)$mStorageCapUp(stg, region, year)..
          vStorageOutCap(stg, region, year) =l= pStorageCapUp(stg, region, year);

eqStorageNewCapLo(stg, region, year)$mStorageNewCapLo(stg, region, year)..
          vStorageOutNewCap(stg, region, year) =g= pStorageNewCapLo(stg, region, year) * pPeriodLen(year);

eqStorageNewCapUp(stg, region, year)$mStorageNewCapUp(stg, region, year)..
          vStorageOutNewCap(stg, region, year) =l= pStorageNewCapUp(stg, region, year) * pPeriodLen(year);

* Investment equation
eqStorageInv(stg, region, year)$mStorageNew(stg, region, year)..
         vStorageInv(stg, region, year)
         =e=
         pStorageInvcost(stg, region, year) *
         vStorageOutNewCap(stg, region, year)
*        [2c] the storing side's capital cost is per unit of ENERGY and rides on
*        the same investment variable; absent data, the term does not exist.
         + (pStorageStgInvcost(stg, region, year)
            * vStorageStgNewCap(stg, region, year)
           )$mStorageStgNew(stg, region, year)
         + (pStorageInpInvcost(stg, region, year)
            * vStorageInpNewCap(stg, region, year)
           )$mStorageInpNew(stg, region, year);

* EAC equation
* [eac-fix] vintaged new-capacity form active (pStorageEac applies to NEW capacity only)
eqStorageEac(stg, region, year)$mStorageEac(stg, region, year)..
         vStorageEac(stg, region, year)
         =e=
         sum(yearp$(mStorageNew(stg, region, yearp)
                    and ordYear(year) >= ordYear(yearp)
*                  [payback] see eqTechEac.
                    and (
                         (pStoragePayback(stg, region, yearp) > 0
                          and ordYear(year) < pStoragePayback(stg, region, yearp) + ordYear(yearp))
                         or
                         (pStoragePayback(stg, region, yearp) <= 0
                          and (mStorageOlifeInf(stg, region)
                               or ordYear(year) < pStorageOlife(stg, region) + ordYear(yearp)
                               )
                          )
                         )
*                  [eac-fix] the `pStorageInvcost <> 0` guard was REMOVED from the summation
*                  condition below. It dropped the annuity entirely when a user supplied
*                  `@invcost$eac` without `invcost` (pre-annuitised capex, e.g. a PyPSA import),
*                  so the storage was built for FREE -- silently, with an OPTIMAL solve.
*                  eqTechEac / eqTradeEac never carried it. pStorageEac defaults to 0, so a
*                  vintage with no capital cost now contributes a zero-coefficient term instead
*                  of being dropped from the sum.
                    ),
*                  pYearFraction(year) *
            (pStorageEac(stg, region, yearp)
*            * pPeriodLen(yearp)
             * vStorageOutNewCap(stg, region, yearp)
             + (pStorageStgEac(stg, region, yearp)
                * vStorageStgNewCap(stg, region, yearp)
               )$mStorageStgNew(stg, region, yearp)
             + (pStorageInpEac(stg, region, yearp)
                * vStorageInpNewCap(stg, region, yearp)
               )$mStorageInpNew(stg, region, yearp))
         );
$ontext
* [eac-fix] simplified "EAC for existing capacity" form disabled:
eqStorageEac(stg, region, year)$mStorageEac(stg, region, year)..
         vStorageEac(stg, region, year)
         =e=
         pStorageEac(stg, region, year) * vStorageOutCap(stg, region, year);
$offtext


$ontext
* Fixed and Variable O&M costs
eqStorageCost(stg, region, year)$mStorageOMCost(stg, region, year)..
         vStorageOMCost(stg, region, year)
         =e=
*         pYearFraction(year) *
         pStorageFixom(stg, region, year) * vStorageOutCap(stg, region, year)
         +
         sum(comm$mStorageInpComm(stg, comm),
             sum(timeslice$mCommTimeslice(comm, timeslice),
                 pStorageCostInp(stg, region, year, timeslice)
                    * pTimesliceWeight(year, timeslice)
                    * vStorageInp(stg, comm, region, year, timeslice)))
          + sum(comm$mStorageOutComm(stg, comm),
             sum(timeslice$mCommTimeslice(comm, timeslice),
                 pStorageCostOut(stg, region, year, timeslice)
                    * pTimesliceWeight(year, timeslice)
                    * vStorageOut(stg, comm, region, year, timeslice)))
          + sum(comm$mStorageStgComm(stg, comm),
             sum(timeslice$mCommTimeslice(comm, timeslice),
                 pStorageCostStore(stg, region, year, timeslice)
                    * pTimesliceWeight(year, timeslice)
                    * vStorageLevel(stg, comm, region, year, timeslice)
             )
         );
$offtext
* Storage fixed costs
*!!!eqStorageFixom(stg, region, year)$mStorageFixom(stg, region, year)..
eqStorageFixom(stg, region, year)$mStorageFixom(stg, region, year)..
         vStorageFixom(stg, region, year)
         =e=
         pStorageFixom(stg, region, year) * vStorageOutCap(stg, region, year)
         + (pStorageStgFixom(stg, region, year)
            * vStorageStgCap(stg, region, year)
           )$mStorageStgFixom(stg, region, year)
         + (pStorageInpFixom(stg, region, year)
            * vStorageInpCap(stg, region, year)
           )$mStorageInpFixom(stg, region, year);

* Storage variable costs
eqStorageVarom(stg, region, year)$mStorageVarom(stg, region, year)..
          vStorageVarom(stg, region, year)
          =e=
          sum(comm$mStorageInpComm(stg, comm),
             sum(timeslice$mCommTimeslice(comm, timeslice),
                 pStorageCostInp(stg, region, year, timeslice)
                    * pTimesliceWeight(year, timeslice)
                    * vStorageInp(stg, comm, region, year, timeslice)))
          + sum(comm$mStorageOutComm(stg, comm),
             sum(timeslice$mCommTimeslice(comm, timeslice),
                 pStorageCostOut(stg, region, year, timeslice)
                    * pTimesliceWeight(year, timeslice)
                    * vStorageOut(stg, comm, region, year, timeslice)))
          + sum(comm$mStorageStgComm(stg, comm),
             sum(timeslice$mCommTimeslice(comm, timeslice),
                 pStorageCostStore(stg, region, year, timeslice)
                    * pTimesliceWeight(year, timeslice)
                    * vStorageLevel(stg, comm, region, year, timeslice)
             )
         );

********************************************************************************
** Interregional and ROW Trade equations
********************************************************************************
********************************************************************************
*** Trade Flows
********************************************************************************
equation
eqImportTot(comm, region, year, timeslice)             Import equation (Ir & ROW)
eqExportTot(comm, region, year, timeslice)             Export equation (Ir & ROW)
eqTradeFlowUp(trade, comm, region, region, year, timeslice)   Trade upper bound
eqTradeFlowLo(trade, comm, region, region, year, timeslice)   Trade lower bound
*eqCostTrade(region, year)                         Total trade costs
*eqCostRowTrade(region, year)                      Costs of trade with the Rest of the World (ROW)
* eqCostIrTrade(region, year)                       Costs of import
eqExportRowUp(expp, comm, region, year, timeslice)    Export to the ROW upper constraint
eqExportRowLo(expp, comm, region, year, timeslice)    Export to the ROW lower constraint
eqExportRowCum(expp, comm)                 Cumulative export to the ROW
eqExportRowResUp(expp, comm)                      Cumulative export to the ROW upper constraint
eqExportRowCost(expp, region, year)               Export to the ROW costs equation
eqImportRowUp(imp, comm, region, year, timeslice)     Import from the ROW upper constraint
eqImportRowLo(imp, comm, region, year, timeslice)     Import of the ROW lower constraint
eqImportRowCum(imp, comm)                 Cumulative import from the ROW
eqImportRowResUp(imp, comm)                       Cumulative import from the ROW upper constraint
eqImportRowCost(imp, region, year)                Import from the ROW costs equation
eqTradeCap(trade, year)                           Trade capacity
eqTradeCapLo(trade, year)                         Trade capacity lower bound
eqTradeCapUp(trade, year)                         Trade capacity upper bound
eqTradeNewCapLo(trade, year)                      Trade new capacity lower bound
eqTradeNewCapUp(trade, year)                      Trade new capacity upper bound
eqTradeCapFlow(trade, comm, year, timeslice)          Trade capacity to activity
eqTradeInv(trade, region, year)                   Trade overnight investment costs
eqTradeEac(trade, region, year)                   Trade equivalent annual costs
eqTradeFixom(trade, region, year)
eqImportIrCost(trade, region, year)               Interregional trade import costs
eqExportIrCost(trade, region, year)               Interregional trade export costs
;

$ontext
* rewritten (dropped sum(timeslicep$mCommTimesliceOrParent...)
* because timeframe of all one-commodity processes (sup, dem, exp, imp, ir-trd)
* is fixed to the timeframe of commodity
eqImportTot(comm, dst, year, timeslice)$mImport(comm, dst, year, timeslice)..
  vImportTot(comm, dst, year, timeslice) =e=
     sum(timeslicep$mCommTimesliceOrParent(comm, timeslice, timeslicep),
         sum(trade$mTradeComm(trade, comm),
             sum(src$mTradeRoutes(trade, src, dst),
                 (pTradeIrEff(trade, src, dst, year, timeslicep)
                  * vTradeIr(trade, comm, src, dst, year, timeslicep)
                  )$mvTradeIr(trade, comm, src, dst, year, timeslicep)
                 )
            )
         )
     +
     sum(timeslicep$mCommTimesliceOrParent(comm, timeslice, timeslicep),
         sum(imp$mImpComm(imp, comm),
             vImportRow(imp, comm, dst, year, timeslicep)$mImportRow(imp, comm, dst, year, timeslicep)
         )
     );
$offtext

eqImportTot(comm, dst, year, timeslice)$mImport(comm, dst, year, timeslice)..
  vImportTot(comm, dst, year, timeslice) =e=
    sum(trade$mTradeComm(trade, comm),
      sum(src$mTradeRoutes(trade, src, dst),
          (pTradeIrEff(trade, src, dst, year, timeslice)
           * vTradeIr(trade, comm, src, dst, year, timeslice)
          )$mvTradeIr(trade, comm, src, dst, year, timeslice)
      )
    )
*    ) * pTimesliceWeight(year, timeslice)
    +
    sum(imp$mImpComm(imp, comm),
          vImportRow(imp, comm, dst, year, timeslice)$mImportRow(imp, comm, dst, year, timeslice)
    );
*    ) * pTimesliceWeight(year, timeslice);

$ontext
* rewritten (dropped sum(timeslicep$mCommTimesliceOrParent...)
* because timeframe of all one-commodity processes (sup, dem, exp, imp, ir-trd)
* is fixed to the timeframe of commodity

eqExportTot(comm, src, year, timeslice)$mExport(comm, src, year, timeslice)..
  vExportTot(comm, src, year, timeslice)
  =e=
  sum(timeslicep$mCommTimesliceOrParent(comm, timeslice, timeslicep),
      sum(trade$mTradeComm(trade, comm),
          sum(dst$mTradeRoutes(trade, src, dst),
              vTradeIr(trade, comm, src, dst, year, timeslicep)$mvTradeIr(trade, comm, src, dst, year, timeslicep)
          )
      )
  ) +
  sum(timeslicep$mCommTimesliceOrParent(comm, timeslice, timeslicep),
      sum(expp$mExpComm(expp, comm),
          vExportRow(expp, comm, src, year, timeslicep)$mExportRow(expp, comm, src, year, timeslicep)
      )
  );
$offtext

eqExportTot(comm, src, year, timeslice)$mExport(comm, src, year, timeslice)..

  vExportTot(comm, src, year, timeslice)
  =e=
  sum(trade$mTradeComm(trade, comm),
      sum(dst$mTradeRoutes(trade, src, dst),
          vTradeIr(trade, comm, src, dst, year, timeslice)$mvTradeIr(trade, comm, src, dst, year, timeslice)
      )
  )
*  ) * pTimesliceWeight(year, timeslice)
  +
  sum(expp$mExpComm(expp, comm),
      vExportRow(expp, comm, src, year, timeslice)$mExportRow(expp, comm, src, year, timeslice)
  );
*  ) * pTimesliceWeight(year, timeslice);

eqTradeFlowUp(trade, comm, src, dst, year, timeslice)$meqTradeFlowUp(trade, comm, src, dst, year, timeslice)..
      vTradeIr(trade, comm, src, dst, year, timeslice) =l= pTradeIrUp(trade, src, dst, year, timeslice);

eqTradeFlowLo(trade, comm, src, dst, year, timeslice)$meqTradeFlowLo(trade, comm, src, dst, year, timeslice)..
      vTradeIr(trade, comm, src, dst, year, timeslice) =g= pTradeIrLo(trade, src, dst, year, timeslice);

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
  sum((imp, comm, timeslice)$mImportRow(imp, comm, region, year, timeslice),
      pImportRowPrice(imp, region, year, timeslice) * pTimesliceWeight(year, timeslice)
      * vImportRow(imp, comm, region, year, timeslice)
  ) -
  sum((expp, comm, timeslice)$mExportRow(expp, comm, region, year, timeslice),
      pExportRowPrice(expp, region, year, timeslice) * pTimesliceWeight(year, timeslice)
      * vExportRow(expp, comm, region, year, timeslice)
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
            sum(timeslice$mTradeTimeslice(trade, timeslice),
                ((pTradeIrCost(trade, src, region, year, timeslice)
                   + pTradeIrMarkup(trade, src, region, year, timeslice)
                  ) * vTradeIr(trade, comm, src, region, year, timeslice) * pTimesliceWeight(year, timeslice) * pPeriodLen(year)
                )$mvTradeIr(trade, comm, src, region, year, timeslice)
            )
        )
    )
* Export (IR)
  - sum((trade, dst)$mTradeRoutes(trade, region, dst),
        sum(comm$mTradeComm(trade, comm),
            sum(timeslice$mTradeTimeslice(trade, timeslice),
                ((pTradeIrCost(trade, region, dst, year, timeslice)
                  + pTradeIrMarkup(trade, region, dst, year, timeslice)
                  )* vTradeIr(trade, comm, region, dst, year, timeslice) * pTimesliceWeight(year, timeslice) * pPeriodLen(year)
                 )$mvTradeIr(trade, comm, region, dst, year, timeslice)
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
            sum(timeslice$mTradeTimeslice(trade, timeslice),
                ((pTradeIrCost(trade, src, region, year, timeslice)
                   + pTradeIrMarkup(trade, src, region, year, timeslice)
                  ) * vTradeIr(trade, comm, src, region, year, timeslice) * pTimesliceWeight(year, timeslice)
                )$mvTradeIr(trade, comm, src, region, year, timeslice)
            )
        )
    )
* Export (IR)
  - sum((trade, dst)$mTradeRoutes(trade, region, dst),
        sum(comm$mTradeComm(trade, comm),
            sum(timeslice$mTradeTimeslice(trade, timeslice),
                ((pTradeIrCost(trade, region, dst, year, timeslice)
                  + pTradeIrMarkup(trade, region, dst, year, timeslice)
                  ) * vTradeIr(trade, comm, region, dst, year, timeslice) * pTimesliceWeight(year, timeslice)
                 )$mvTradeIr(trade, comm, region, dst, year, timeslice)
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
            sum(timeslice$mTradeTimeslice(trade, timeslice),
                ((pTradeIrCost(trade, src, region, year, timeslice)
                   + pTradeIrMarkup(trade, src, region, year, timeslice)
                  ) * vTradeIr(trade, comm, src, region, year, timeslice) * pTimesliceWeight(year, timeslice)
                )$mvTradeIr(trade, comm, src, region, year, timeslice)
            )
        )
    );

* Export (IR)
eqExportIrCost(trade, region, year)$mExportIrCost(trade, region, year)..
  vExportIrCost(trade, region, year)
  =e=
    - sum((dst)$mTradeRoutes(trade, region, dst),
        sum(comm$mTradeComm(trade, comm),
            sum(timeslice$mTradeTimeslice(trade, timeslice),
                ((pTradeIrCost(trade, region, dst, year, timeslice)
                  + pTradeIrMarkup(trade, region, dst, year, timeslice)
                  ) * vTradeIr(trade, comm, region, dst, year, timeslice) * pTimesliceWeight(year, timeslice)
                 )$mvTradeIr(trade, comm, region, dst, year, timeslice)
            )
        )
    );

eqExportRowUp(expp, comm, region, year, timeslice)$mExportRowUp(expp, comm, region, year, timeslice)..
  vExportRow(expp, comm, region, year, timeslice)
  =l=
  pExportRowUp(expp, region, year, timeslice);

eqExportRowLo(expp, comm, region, year, timeslice)$meqExportRowLo(expp, comm, region, year, timeslice)..
  vExportRow(expp, comm, region, year, timeslice)
  =g=
  pExportRowLo(expp, region, year, timeslice);

eqExportRowCum(expp, comm)$mExpComm(expp, comm)..
  vExportRowCum(expp, comm)
  =e=
  sum((region, year, timeslice)$mExportRow(expp, comm, region, year, timeslice),
      pPeriodLen(year) * pTimesliceWeight(year, timeslice)
      * vExportRow(expp, comm, region, year, timeslice)
  );

eqExportRowResUp(expp, comm)$mExportRowCumUp(expp, comm)..
  vExportRowCum(expp, comm) =l= pExportRowRes(expp);

*
eqExportRowCost(expp, region, year)$mExportRowCost(expp, region, year)..
  vExportRowCost(expp, region, year)
  =e=
  - sum((comm, timeslice)$mExportRow(expp, comm, region, year, timeslice),
      pExportRowPrice(expp, region, year, timeslice) * pTimesliceWeight(year, timeslice)
      * vExportRow(expp, comm, region, year, timeslice)
  );

eqImportRowUp(imp, comm, region, year, timeslice)$mImportRowUp(imp, comm, region, year, timeslice)..
  vImportRow(imp, comm, region, year, timeslice)  =l= pImportRowUp(imp, region, year, timeslice);

eqImportRowLo(imp, comm, region, year, timeslice)$meqImportRowLo(imp, comm, region, year, timeslice)..
  vImportRow(imp, comm, region, year, timeslice)  =g= pImportRowLo(imp, region, year, timeslice);

eqImportRowCum(imp, comm)$mImpComm(imp, comm)..
  vImportRowCum(imp, comm)
  =e=
  sum((region, year, timeslice)$mImportRow(imp, comm, region, year, timeslice),
      pPeriodLen(year) * pTimesliceWeight(year, timeslice)
      * vImportRow(imp, comm, region, year, timeslice)
  );

eqImportRowResUp(imp, comm)$mImportRowCumUp(imp, comm)..
  vImportRowCum(imp, comm) =l= pImportRowRes(imp);

* mapping? $mImpComm(imp, region)
eqImportRowCost(imp, region, year)$mImportRowCost(imp, region, year)..
  vImportRowCost(imp, region, year)
  =e=
  sum((comm, timeslice)$mImportRow(imp, comm, region, year, timeslice),
      pImportRowPrice(imp, region, year, timeslice) * pTimesliceWeight(year, timeslice)
      * vImportRow(imp, comm, region, year, timeslice)
  );

********************************************************************************
*** Trade IR capacity equations
********************************************************************************

* Capacity to activity constraint
eqTradeCapFlow(trade, comm, year, timeslice)$meqTradeCapFlow(trade, comm, year, timeslice)..
         pTimesliceShare(timeslice) * pTradeCap2Act(trade) * vTradeCap(trade, year) =g=
                 sum((src, dst)$mvTradeIr(trade, comm, src, dst, year, timeslice),
                     vTradeIr(trade, comm, src, dst, year, timeslice)
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
* [eac-fix] vintaged new-capacity form active (pTradeEac applies to NEW capacity only)
eqTradeEac(trade, region, year)$mTradeEac(trade, region, year)..
         vTradeEac(trade, region, year)
         =e=
         sum(yearp$(mTradeNew(trade, yearp) and  ordYear(year) >= ordYear(yearp) and
*           [payback] see eqTechEac.
            ((pTradePayback(trade, region, yearp) > 0
              and ordYear(year) < pTradePayback(trade, region, yearp) + ordYear(yearp))
             or
             (pTradePayback(trade, region, yearp) <= 0
              and (ordYear(year) < pTradeOlife(trade) + ordYear(yearp)
                   or mTradeOlifeInf(trade))))),
                pTradeEac(trade, region, yearp) * vTradeNewCap(trade, yearp));
$ontext
* [eac-fix] simplified "EAC for existing capacity" form disabled:
eqTradeEac(trade, region, year)$mTradeEac(trade, region, year)..
         vTradeEac(trade, region, year)
         =e=
         pTradeEac(trade, region, year) * vTradeCap(trade, year);
$offtext


* Fixed O&M costs
eqTradeFixom(trade, region, year)$mTradeFixom(trade, region, year)..
         vTradeFixom(trade, region, year)
         =e=
         pTradeFixom(trade, region, year) * vTradeCap(trade, year);

********************************************************************************
*** Auxiliary input & output equations
********************************************************************************
equation
eqTradeIrAInp(trade, comm, region, year, timeslice) Trade auxiliary commodity input
eqTradeIrAOut(trade, comm, region, year, timeslice) Trade auxiliary commodity output
eqTradeIrAInpTot(comm, region, year, timeslice) Trade auxiliary commodity input
eqTradeIrAOutTot(comm, region, year, timeslice) Trade auxiliary commodity output
;

eqTradeIrAInp(trade, comm, region, year, timeslice)$mvTradeIrAInp(trade, comm, region, year, timeslice)..
  vTradeIrAInp(trade, comm, region, year, timeslice)
  =e=
  sum(dst$mTradeIrCsrc2Ainp(trade, comm, region, dst, year, timeslice),
      pTradeIrCsrc2Ainp(trade, comm, region, dst, year, timeslice)
      * sum(commp$mTradeComm(trade, commp),
            vTradeIr(trade, commp, region, dst, year, timeslice)
        )
  ) +
  sum(src$mTradeIrCdst2Ainp(trade, comm, src, region, year, timeslice),
      pTradeIrCdst2Ainp(trade, comm, src, region, year, timeslice)
      * sum(commp$mTradeComm(trade, commp),
            vTradeIr(trade, commp, src, region, year, timeslice)
        )
  );

eqTradeIrAOut(trade, comm, region, year, timeslice)$mvTradeIrAOut(trade, comm, region, year, timeslice)..
  vTradeIrAOut(trade, comm, region, year, timeslice)
  =e=
  sum(dst$mTradeIrCsrc2Aout(trade, comm, region, dst, year, timeslice),
      pTradeIrCsrc2Aout(trade, comm, region, dst, year, timeslice)
      * sum(commp$mTradeComm(trade, commp),
            vTradeIr(trade, commp, region, dst, year, timeslice)
        )
  ) +
  sum(src$mTradeIrCdst2Aout(trade, comm, src, region, year, timeslice),
      pTradeIrCdst2Aout(trade, comm, src, region, year, timeslice)
      * sum(commp$mTradeComm(trade, commp),
            vTradeIr(trade, commp, src, region, year, timeslice)
        )
  );

eqTradeIrAInpTot(comm, region, year, timeslice)$mvTradeIrAInpTot(comm, region, year, timeslice)..
  vTradeIrAInpTot(comm, region, year, timeslice)
  =e=
*  pTimesliceWeight(year, timeslice) *
  sum((trade, timeslicep)$(mCommTimesliceOrParent(comm, timeslice, timeslicep)
                       and mvTradeIrAInp(trade, comm, region, year, timeslicep)
                       ),
      vTradeIrAInp(trade, comm, region, year, timeslicep)
  );

eqTradeIrAOutTot(comm, region, year, timeslice)$mvTradeIrAOutTot(comm, region, year, timeslice)..
  vTradeIrAOutTot(comm, region, year, timeslice)
  =e=
*  pTimesliceWeight(year, timeslice) *
  sum((trade, timeslicep)$(mCommTimesliceOrParent(comm, timeslice, timeslicep)
                       and mvTradeIrAOut(trade, comm, region, year, timeslicep)
                       ),
      vTradeIrAOut(trade, comm, region, year, timeslicep)
  );

********************************************************************************
*** Balance equations & dummy import & export
********************************************************************************
Equation
eqBalUp(comm, region, year, timeslice)   Commodity balance <= 0 (e.g. upper limit - deficit is allowed)
eqBalLo(comm, region, year, timeslice)   Commodity balance >= 0 (e.g. lower limit - excess is allower)
eqBalFx(comm, region, year, timeslice)   Commodity balance >= 0 (no excess nor deficit is allowed)
eqBal(comm, region, year, timeslice)     Commodity balance
eqOutTot(comm, region, year, timeslice)     Total commodity output
eqInpTot(comm, region, year, timeslice)     Total commodity input
* [agg-rewrite] eqInp2Lo/eqOut2Lo retired (up-aggregation in eqInpTot/eqOutTot)
* eqInp2Lo(comm, region, year, timeslice)         From commodity timeslice to lo level
* eqOut2Lo(comm, region, year, timeslice)         From commodity timeslice to lo level
eqSupOutTot(comm, region, year, timeslice)      Supply total output
eqTechInpTot(comm, region, year, timeslice)     Technology total input
eqTechOutTot(comm, region, year, timeslice)     Technology total output
eqStorageInpTot(comm, region, year, timeslice)  Storage total input
eqStorageOutTot(comm, region, year, timeslice)  Storage total output
;

eqBalLo(comm, region, year, timeslice)$meqBalLo(comm, region, year, timeslice)..
  vBalance(comm, region, year, timeslice) =g= 0;

eqBalUp(comm, region, year, timeslice)$meqBalUp(comm, region, year, timeslice)..
  vBalance(comm, region, year, timeslice) =l= 0;

eqBalFx(comm, region, year, timeslice)$meqBalFx(comm, region, year, timeslice)..
  vBalance(comm, region, year, timeslice) =e= 0;

eqBal(comm, region, year, timeslice)$mvBalance(comm, region, year, timeslice)..
  vBalance(comm, region, year, timeslice)
* pTimesliceWeight(year, timeslice)
  =e=
  vOutTot(comm, region, year, timeslice)$mvOutTot(comm, region, year, timeslice)
  -
  vInpTot(comm, region, year, timeslice)$mvInpTot(comm, region, year, timeslice);

* [agg-rewrite] eqBalanceRY/vBalanceRY retired (dead reporting)

eqOutTot(comm, region, year, timeslice)$mvOutTot(comm, region, year, timeslice)..
         vOutTot(comm, region, year, timeslice)
         =e=
           vDummyImport(comm, region, year, timeslice)$mDummyImport(comm, region, year, timeslice)
         + vSupOutTot(comm, region, year, timeslice)$mSupOutTot(comm, region, year, timeslice)
         + vEmsFuelTot(comm, region, year, timeslice)$mEmsFuelTot(comm, region, year, timeslice)
         + vAggOutTot(comm, region, year, timeslice)$mAggOut(comm, region, year, timeslice)
         + vTechOutTot(comm, region, year, timeslice)$mTechOutTot(comm, region, year, timeslice)
         + vStorageOutTot(comm, region, year, timeslice)$mStorageOutTot(comm, region, year, timeslice)
         + vImportTot(comm, region, year, timeslice)$mImport(comm, region, year, timeslice)
         + vTradeIrAOutTot(comm, region, year, timeslice)$mvTradeIrAOutTot(comm, region, year, timeslice)
* [agg-rewrite] UP-aggregation of immediately-finer children (replaces mOutSub/vOut2Lo)
         + sum(timeslicep$(mTimesliceFamily(timeslice, timeslicep)
               and
               mvOutTot(comm, region, year, timeslicep)
              ),
              pTimesliceAgg(year, timeslice, timeslicep) * vOutTot(comm, region, year, timeslicep)
           )
* [nested-regions] UP-aggregation of the immediately-finer region level. Plain
* sum: regional quantities are extensive, unlike the intensive timeslice values above.
         + sum(regionp$(mRegionFamily(region, regionp)
               and
               mvOutTot(comm, regionp, year, timeslice)
              ),
              vOutTot(comm, regionp, year, timeslice)
           );

* [agg-rewrite] eqOutTotRY/vOutTotRY retired (dead reporting)

* [agg-rewrite] eqOut2Lo removed (up-aggregation in eqOutTot; vOut2Lo retired)
$ontext
eqOut2Lo(comm, region, year, timeslice)$mOut2Lo(comm, region, year, timeslice)..
         sum(timeslicep$mvOut2Lo(comm, region, year, timeslice, timeslicep),
             vOut2Lo(comm, region, year, timeslice, timeslicep))
         =e=
         vSupOutTot(comm, region, year, timeslice)$mSupOutTot(comm, region, year, timeslice)
         + vEmsFuelTot(comm, region, year, timeslice)$mEmsFuelTot(comm, region, year, timeslice)
         + vAggOutTot(comm, region, year, timeslice)$mAggOut(comm, region, year, timeslice)
         + vTechOutTot(comm, region, year, timeslice)$mTechOutTot(comm, region, year, timeslice)
         + vStorageOutTot(comm, region, year, timeslice)$mStorageOutTot(comm, region, year, timeslice)
         + vImportTot(comm, region, year, timeslice)$mImport(comm, region, year, timeslice)
         + vTradeIrAOutTot(comm, region, year, timeslice)$mvTradeIrAOutTot(comm, region, year, timeslice);
$offtext

eqInpTot(comm, region, year, timeslice)$mvInpTot(comm, region, year, timeslice)..
        vInpTot(comm, region, year, timeslice)
        =e=
         vDemInp(comm, region, year, timeslice)$mvDemInp(comm, region, year, timeslice)
        + vDummyExport(comm, region, year, timeslice)$mDummyExport(comm, region, year, timeslice)
        + vTechInpTot(comm, region, year, timeslice)$mTechInpTot(comm, region, year, timeslice)
        + vStorageInpTot(comm, region, year, timeslice)$mStorageInpTot(comm, region, year, timeslice)
        + vExportTot(comm, region, year, timeslice)$mExport(comm, region, year, timeslice)
        + vTradeIrAInpTot(comm, region, year, timeslice)$mvTradeIrAInpTot(comm, region, year, timeslice)
* [agg-rewrite] UP-aggregation of immediately-finer children (replaces mInpSub/vInp2Lo)
        + sum(timeslicep$(mTimesliceFamily(timeslice, timeslicep)
                    and
                    mvInpTot(comm, region, year, timeslicep)
            ),
            pTimesliceAgg(year, timeslice, timeslicep) * vInpTot(comm, region, year, timeslicep)
        )
* [nested-regions] UP-aggregation of the immediately-finer region level (plain
* sum -- extensive quantities).
        + sum(regionp$(mRegionFamily(region, regionp)
                    and
                    mvInpTot(comm, regionp, year, timeslice)
            ),
            vInpTot(comm, regionp, year, timeslice)
        );

* [agg-rewrite] eqInpTotRY/vInpTotRY retired (dead reporting)

* [agg-rewrite] eqInp2Lo removed (up-aggregation in eqInpTot; vInp2Lo retired)
$ontext
eqInp2Lo(comm, region, year, timeslice)$mInp2Lo(comm, region, year, timeslice)..
        sum(timeslicep$mvInp2Lo(comm, region, year, timeslice, timeslicep),
            vInp2Lo(comm, region, year, timeslice, timeslicep)
        )
        =e=
        vTechInpTot(comm, region, year, timeslice)$mTechInpTot(comm, region, year, timeslice)
        + vStorageInpTot(comm, region, year, timeslice)$mStorageInpTot(comm, region, year, timeslice)
        + vExportTot(comm, region, year, timeslice)$mExport(comm, region, year, timeslice)
        + vTradeIrAInpTot(comm, region, year, timeslice)$mvTradeIrAInpTot(comm, region, year, timeslice);
$offtext

eqSupOutTot(comm, region, year, timeslice)$mSupOutTot(comm, region, year, timeslice)..
          vSupOutTot(comm, region, year, timeslice)
          =e=
          sum(sup$mSupComm(sup, comm),
              vSupOut(sup, comm, region, year, timeslice)
          );

* the sum removed since supply-timeframe is fixed to the commodity's timeframe
*              sum(timeslicep$(mCommTimesliceOrParent(comm, timeslice, timeslicep)
*                          and mSupAva(sup, comm, region, year, timeslicep)
*                         ),
*                  vSupOut(sup, comm, region, year, timeslicep)
*              )
*          );

*$ontext
eqTechInpTot(comm, region, year, timeslice)$mTechInpTot(comm, region, year, timeslice)..
        vTechInpTot(comm, region, year, timeslice)
        =e=
*        pTimesliceWeight(year, timeslice) *
        sum(tech$mTechInpCommSameTimeslice(tech, comm),
            vTechInp(tech, comm, region, year, timeslice)$mvTechInp(tech, comm, region, year, timeslice)
        )
        +
*        pTimesliceWeight(year, timeslice) *
        sum(tech$mTechInpCommAgg(tech, comm),
            sum(timeslicep$mTechInpCommAggTimeslice(tech, comm, timeslicep, timeslice),
                vTechInp(tech, comm, region, year, timeslicep)$mvTechInp(tech, comm, region, year, timeslicep)
            )
        )
        +
*        pTimesliceWeight(year, timeslice) *
        sum(tech$mTechAInpCommSameTimeslice(tech, comm),
            vTechAInp(tech, comm, region, year, timeslice)$mvTechAInp(tech, comm, region, year, timeslice)
        )
        +
*        pTimesliceWeight(year, timeslice) *
        sum(tech$mTechAInpCommAgg(tech, comm),
            sum(timeslicep$mTechAInpCommAggTimeslice(tech, comm, timeslicep, timeslice),
            vTechAInp(tech, comm, region, year, timeslicep)$mvTechAInp(tech, comm, region, year, timeslicep)
            )
        );

eqTechOutTot(comm, region, year, timeslice)$mTechOutTot(comm, region, year, timeslice)..
        vTechOutTot(comm, region, year, timeslice)
        =e=
*        pTimesliceWeight(year, timeslice) *
        sum(tech$mTechOutCommSameTimeslice(tech, comm),
            vTechOut(tech, comm, region, year, timeslice)$mvTechOut(tech, comm, region, year, timeslice)
        )
        +
*        pTimesliceWeight(year, timeslice) *
        sum(tech$mTechOutCommAgg(tech, comm),
            sum(timeslicep$mTechOutCommAggTimeslice(tech, comm, timeslicep, timeslice),
                vTechOut(tech, comm, region, year, timeslicep)$mvTechOut(tech, comm, region, year, timeslicep)
            )
        )
        +
*        pTimesliceWeight(year, timeslice) *
        sum(tech$mTechAOutCommSameTimeslice(tech, comm),
            vTechAOut(tech, comm, region, year, timeslice)$mvTechAOut(tech, comm, region, year, timeslice)
        )
        +
*        pTimesliceWeight(year, timeslice) *
        sum(tech$mTechAOutCommAgg(tech, comm),
            sum(timeslicep$mTechAOutCommAggTimeslice(tech, comm, timeslicep, timeslice),
                vTechAOut(tech, comm, region, year, timeslicep)$mvTechAOut(tech, comm, region, year, timeslicep)
            )
        );

* [agg-rewrite] eqTechOutRY/vTechOutRY retired (dead reporting)

eqStorageInpTot(comm, region, year, timeslice)$mStorageInpTot(comm, region, year, timeslice)..
        vStorageInpTot(comm, region, year, timeslice)
        =e=
*        pTimesliceWeight(year, timeslice) *
        sum(stg$mvStorageInp(stg, comm, region, year, timeslice),
            vStorageInp(stg, comm, region, year, timeslice)
        )
*        + pTimesliceWeight(year, timeslice) *
        + sum(stg$mvStorageAInp(stg, comm, region, year, timeslice),
            vStorageAInp(stg, comm, region, year, timeslice)
        );

eqStorageOutTot(comm, region, year, timeslice)$mStorageOutTot(comm, region, year, timeslice)..
        vStorageOutTot(comm, region, year, timeslice)
        =e=
*        pTimesliceWeight(year, timeslice) *
        sum(stg$mvStorageOut(stg, comm, region, year, timeslice),
            vStorageOut(stg, comm, region, year, timeslice)
        )
*        + pTimesliceWeight(year, timeslice) *
        + sum(stg$mvStorageAOut(stg, comm, region, year, timeslice),
            vStorageAOut(stg, comm, region, year, timeslice)
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
        sum(timeslice$mDummyImport(comm, region, year, timeslice),
            pTimesliceWeight(year, timeslice)
            * pDummyImportCost(comm, region, year, timeslice)
            * vDummyImport(comm, region, year, timeslice)$mDummyImport(comm, region, year, timeslice)
            );

* dummy export (to debug infesibility)
eqDummyExportCost(comm, region, year)$mDummyExportCost(comm, region, year)..
        vDummyExportCost(comm, region, year)
        =e=
        sum(timeslice$mDummyExport(comm, region, year, timeslice),
            pTimesliceWeight(year, timeslice)
            * pDummyExportCost(comm, region, year, timeslice)
            * vDummyExport(comm, region, year, timeslice)$mDummyExport(comm, region, year, timeslice)
            );

* tax levies
eqTaxCost(comm, region, year)$mTaxCost(comm, region, year)..
         vTaxCost(comm, region, year)
         =e=
         sum(timeslice$(mvOutTot(comm, region, year, timeslice) and mCommTimeslice(comm, timeslice)),
             pTaxCostOut(comm, region, year, timeslice)
             * pTimesliceWeight(year, timeslice)
             * vOutTot(comm, region, year, timeslice)
             )
       + sum(timeslice$(mvInpTot(comm, region, year, timeslice) and mCommTimeslice(comm, timeslice)),
             pTaxCostInp(comm, region, year, timeslice)
             * pTimesliceWeight(year, timeslice)
             * vInpTot(comm, region, year, timeslice)
             )
       + sum(timeslice$(mvBalance(comm, region, year, timeslice) and mCommTimeslice(comm, timeslice)),
             pTaxCostBal(comm, region, year, timeslice)
             * pTimesliceWeight(year, timeslice)
             * vBalance(comm, region, year, timeslice)
             );

* subsidies
eqSubsCost(comm, region, year)$mSubCost(comm, region, year)..
        vSubsCost(comm, region, year)
        =e=
      - sum(timeslice$(mvOutTot(comm, region, year, timeslice) and mCommTimeslice(comm, timeslice)),
            pSubCostOut(comm, region, year, timeslice)
            * pTimesliceWeight(year, timeslice)
            * vOutTot(comm, region, year, timeslice)
        )
      - sum(timeslice$(mvInpTot(comm, region, year, timeslice) and mCommTimeslice(comm, timeslice)),
            pSubCostInp(comm, region, year, timeslice)
            * pTimesliceWeight(year, timeslice)
            * vInpTot(comm, region, year, timeslice)
        )
      - sum(timeslice$(mvBalance(comm, region, year, timeslice) and mCommTimeslice(comm, timeslice)),
            pSubCostBal(comm, region, year, timeslice)
            * pTimesliceWeight(year, timeslice)
            * vBalance(comm, region, year, timeslice));
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
