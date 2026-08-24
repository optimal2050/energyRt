# Coverage matrix summary

Generated: 2026-08-23 20:44 | energyRt 0.86.0.9000

## Rows by kind x depth

```
Key: <kind>
     kind  none     C     I     S     X
   <char> <int> <int> <int> <int> <int>
   bounds    21     3     1     6     6
 equation   137     0     1     2     8
      map   232     0    39     3    11
   numpar    68     7     7    10    41
      set     1     1     0     2     9
 variable    60     0     2    10    23
```

## Numeric parameters: family x depth

```
Key: <family>
                      family  none     C     I     S     X
                      <char> <int> <int> <int> <int> <int>
         calendar-timeslices     1     0     0     3     0
                   commodity     2     0     0     0     0
                      demand     1     0     0     0     0
                 discounting     0     0     0     0     3
                 dummy-debug     2     0     0     0     0
           horizon-periodlen     1     0     0     0     2
           import-export-row     6     0     0     0     0
           storage-aux-flows    14     1     0     0     0
           storage-costs-eac    17     0     2     1     8
            storage-duration     0     0     0     0     1
          storage-efficiency     0     0     0     3     0
             storage-inp2out     1     0     0     0     0
    storage-roles-capacities     8     0     0     1     5
 storage-startlevel-fullyear     0     0     1     0     0
               storage-varom     3     0     0     0     0
             storage-weather     2     1     0     0     0
                      supply     1     1     1     0     1
                 tax-subsidy     0     2     0     0     4
              tech-aux-flows    12     1     1     0     0
           tech-availability     3     0     0     1     0
        tech-capacity-bounds     1     0     0     1     1
                  tech-costs     0     2     0     0     3
       tech-eac-wacc-payback     0     0     0     1     2
             tech-efficiency     1     1     1     2     1
                   tech-emis     1     0     0     0     0
           tech-share-bounds     0     0     0     1     0
       tech-stock-retirement     0     0     1     0     5
                tech-weather     2     1     0     0     0
      trade-capacity-vintage     4     0     1     2     7
                  trade-core     2     0     0     0     1
                 trade-costs     4     0     0     0     2
               weather-class     0     0     0     0     1
                      family  none     C     I     S     X
                      <char> <int> <int> <int> <int> <int>
```

## Zero-coverage numeric parameters (89)

```
                 name                   family      class         slot
               <char>                   <char>     <char>       <char>
        pYearFraction      calendar-timeslices   settings yearFraction
     pAggregateFactor                commodity  commodity          agg
      pEmissionFactor                commodity  commodity         emis
              pDemand                   demand     demand          dem
     pDummyExportCost              dummy-debug   settings        debug
     pDummyImportCost              dummy-debug   settings        debug
             cardYear        horizon-periodlen                        
           pExportRow        import-export-row     export availability
      pExportRowPrice        import-export-row     export availability
        pExportRowRes        import-export-row     export      reserve
           pImportRow        import-export-row     import availability
      pImportRowPrice        import-export-row     import availability
        pImportRowRes        import-export-row     import      reserve
     pStorageCap2AInp        storage-aux-flows    storage         aeff
     pStorageCap2AOut        storage-aux-flows    storage         aeff
    pStorageCinp2AOut        storage-aux-flows    storage         aeff
    pStorageCout2AInp        storage-aux-flows    storage         aeff
    pStorageCout2AOut        storage-aux-flows    storage         aeff
    pStorageNCap2AInp        storage-aux-flows    storage         aeff
    pStorageNCap2AOut        storage-aux-flows    storage         aeff
     pStorageNCap2Stg        storage-aux-flows    storage         aeff
     pStoragePho2AInp        storage-aux-flows    storage         aeff
     pStoragePho2AOut        storage-aux-flows    storage         aeff
     pStorageRet2AInp        storage-aux-flows    storage         aeff
     pStorageRet2AOut        storage-aux-flows    storage         aeff
     pStorageStg2AInp        storage-aux-flows    storage         aeff
     pStorageStg2AOut        storage-aux-flows    storage         aeff
       pStorageInpEac        storage-costs-eac    storage      invcost
     pStorageInpFixom        storage-costs-eac    storage        fixom
   pStorageInpPayback        storage-costs-eac    storage      invcost
   pStorageInpRetCost        storage-costs-eac    storage      invcost
  pStorageInpStockNew        storage-costs-eac                        
 pStorageInpStockSurv        storage-costs-eac                        
      pStorageInpWacc        storage-costs-eac    storage      invcost
   pStorageOutRetCost        storage-costs-eac    storage      invcost
  pStorageOutStockNew        storage-costs-eac                        
 pStorageOutStockSurv        storage-costs-eac                        
      pStorageOutWacc        storage-costs-eac    storage      invcost
     pStorageStgFixom        storage-costs-eac    storage        fixom
   pStorageStgPayback        storage-costs-eac    storage      invcost
   pStorageStgRetCost        storage-costs-eac    storage      invcost
  pStorageStgStockNew        storage-costs-eac                        
 pStorageStgStockSurv        storage-costs-eac                        
      pStorageStgWacc        storage-costs-eac    storage      invcost
      pStorageInp2out          storage-inp2out    storage      inp2out
           pStorageAf storage-roles-capacities    storage           af
         pStorageCinp storage-roles-capacities    storage           af
         pStorageCout storage-roles-capacities    storage           af
       pStorageInpCap storage-roles-capacities    storage     capacity
    pStorageInpNewCap storage-roles-capacities    storage     capacity
    pStorageOutNewCap storage-roles-capacities    storage     capacity
       pStorageStgCap storage-roles-capacities    storage     capacity
    pStorageStgNewCap storage-roles-capacities    storage     capacity
      pStorageCostInp            storage-varom    storage        varom
      pStorageCostOut            storage-varom    storage        varom
    pStorageCostStore            storage-varom    storage        varom
  pStorageWeatherCinp          storage-weather    storage      weather
  pStorageWeatherCout          storage-weather    storage      weather
          pSupReserve                   supply     supply      reserve
        pTechAct2AOut           tech-aux-flows technology         aeff
        pTechCap2AInp           tech-aux-flows technology         aeff
        pTechCap2AOut           tech-aux-flows technology         aeff
       pTechCinp2AOut           tech-aux-flows technology         aeff
       pTechCout2AInp           tech-aux-flows technology         aeff
       pTechCout2AOut           tech-aux-flows technology         aeff
       pTechNCap2AInp           tech-aux-flows technology         aeff
       pTechNCap2AOut           tech-aux-flows technology         aeff
        pTechPho2AInp           tech-aux-flows technology         aeff
        pTechPho2AOut           tech-aux-flows technology         aeff
        pTechRet2AInp           tech-aux-flows technology         aeff
        pTechRet2AOut           tech-aux-flows technology         aeff
              pTechAf        tech-availability technology           af
        pTechRampDown        tech-availability technology     rampdown
          pTechRampUp        tech-availability technology       rampup
          pTechNewCap     tech-capacity-bounds technology     capacity
       pTechCinp2ginp          tech-efficiency technology         ceff
        pTechEmisComm                tech-emis technology        input
      pTechWeatherAfc             tech-weather technology      weather
      pTechWeatherAfs             tech-weather technology      weather
        pTradeCap2Act   trade-capacity-vintage      trade      cap2act
         pTradeNewCap   trade-capacity-vintage      trade     capacity
       pTradeStockNew   trade-capacity-vintage                        
      pTradeStockSurv   trade-capacity-vintage                        
             pTradeIr               trade-core      trade        trade
           pTradeIrAf               trade-core      trade        trade
    pTradeIrCdst2Ainp              trade-costs      trade         aeff
    pTradeIrCdst2Aout              trade-costs      trade         aeff
    pTradeIrCsrc2Ainp              trade-costs      trade         aeff
    pTradeIrCsrc2Aout              trade-costs      trade         aeff
                 name                   family      class         slot
               <char>                   <char>     <char>       <char>
     colName
      <char>
            
         agg
        emis
         dem
 dummyExport
 dummyImport
            
         exp
       price
            
         imp
       price
            
    cap2ainp
    cap2aout
   cinp2aout
   cout2ainp
   cout2aout
   ncap2ainp
   ncap2aout
    ncap2stg
    pho2ainp
    pho2aout
    ret2ainp
    ret2aout
    stg2ainp
    stg2aout
     inp.eac
   inp.fixom
 inp.payback
 inp.retcost
            
            
    inp.wacc
 out.retcost
            
            
    out.wacc
   stg.fixom
 stg.payback
 stg.retcost
            
            
    stg.wacc
     inp2out
          af
        cinp
        cout
     inp.cap
    inp.ncap
    out.ncap
     stg.cap
    stg.ncap
     inpcost
     outcost
     stgcost
       wcinp
       wcout
         res
    act2aout
    cap2ainp
    cap2aout
   cinp2aout
   cout2ainp
   cout2aout
   ncap2ainp
   ncap2aout
    pho2ainp
    pho2aout
    ret2ainp
    ret2aout
          af
    rampdown
      rampup
        ncap
   cinp2ginp
  combustion
        wafc
        wafs
            
        ncap
            
            
         ava
          af
   cdst2ainp
   cdst2aout
   csrc2ainp
   csrc2aout
     colName
      <char>
```

Tagged rows: 3 | inferred: 189 | uncovered: 519 of 711
