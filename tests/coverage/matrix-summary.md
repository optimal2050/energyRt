# Coverage matrix summary

Generated: 2026-08-24 23:00 | energyRt 0.87.1.9000

## Rows by kind x depth

```
Key: <kind>
     kind  none     C     I     S     X
   <char> <int> <int> <int> <int> <int>
   bounds    15     2     3     5    12
 equation   120     0     1    19     8
      map   228     0    39     5    13
   numpar    36     3    18    37    39
      set     0     0     0     1    12
 variable    32     0     4    32    27
```

## Numeric parameters: family x depth

```
Key: <family>
                      family  none     C     I     S     X
                      <char> <int> <int> <int> <int> <int>
         calendar-timeslices     0     0     0     4     0
                   commodity     2     0     0     0     0
                      demand     0     0     0     1     0
                 discounting     0     0     0     0     3
                 dummy-debug     0     0     0     2     0
           horizon-periodlen     1     0     0     0     2
           import-export-row     0     0     1     1     4
           storage-aux-flows     5     0     5     5     0
           storage-costs-eac    15     0     1     4     8
            storage-duration     0     0     0     0     1
          storage-efficiency     0     0     0     3     0
             storage-inp2out     0     0     0     1     0
    storage-roles-capacities     8     0     0     1     5
 storage-startlevel-fullyear     0     0     1     0     0
               storage-varom     0     0     0     3     0
             storage-weather     2     1     0     0     0
                      supply     1     1     1     0     1
                 tax-subsidy     0     0     2     4     0
              tech-aux-flows     4     0     4     6     0
           tech-availability     0     0     1     1     2
        tech-capacity-bounds     1     0     0     0     2
                  tech-costs     0     2     0     0     3
       tech-eac-wacc-payback     0     0     0     1     2
             tech-efficiency     1     1     1     2     1
                   tech-emis     1     0     0     0     0
           tech-share-bounds     0     0     0     0     1
       tech-stock-retirement     0     0     1     0     5
                tech-weather     0     0     2     1     0
      trade-capacity-vintage     4     0     1     1     8
                  trade-core     2     0     0     0     1
                 trade-costs     4     0     0     0     2
               weather-class     0     0     0     1     0
                      family  none     C     I     S     X
                      <char> <int> <int> <int> <int> <int>
```

## Zero-coverage numeric parameters (51)

```
                 name                   family      class     slot     colName
               <char>                   <char>     <char>   <char>      <char>
     pAggregateFactor                commodity  commodity      agg         agg
      pEmissionFactor                commodity  commodity     emis        emis
             cardYear        horizon-periodlen                                
     pStorageNCap2Stg        storage-aux-flows    storage     aeff    ncap2stg
     pStoragePho2AInp        storage-aux-flows    storage     aeff    pho2ainp
     pStoragePho2AOut        storage-aux-flows    storage     aeff    pho2aout
     pStorageRet2AInp        storage-aux-flows    storage     aeff    ret2ainp
     pStorageRet2AOut        storage-aux-flows    storage     aeff    ret2aout
       pStorageInpEac        storage-costs-eac    storage  invcost     inp.eac
   pStorageInpPayback        storage-costs-eac    storage  invcost inp.payback
   pStorageInpRetCost        storage-costs-eac    storage  invcost inp.retcost
  pStorageInpStockNew        storage-costs-eac                                
 pStorageInpStockSurv        storage-costs-eac                                
      pStorageInpWacc        storage-costs-eac    storage  invcost    inp.wacc
   pStorageOutRetCost        storage-costs-eac    storage  invcost out.retcost
  pStorageOutStockNew        storage-costs-eac                                
 pStorageOutStockSurv        storage-costs-eac                                
      pStorageOutWacc        storage-costs-eac    storage  invcost    out.wacc
   pStorageStgPayback        storage-costs-eac    storage  invcost stg.payback
   pStorageStgRetCost        storage-costs-eac    storage  invcost stg.retcost
  pStorageStgStockNew        storage-costs-eac                                
 pStorageStgStockSurv        storage-costs-eac                                
      pStorageStgWacc        storage-costs-eac    storage  invcost    stg.wacc
           pStorageAf storage-roles-capacities    storage       af          af
        pStorageInpAf storage-roles-capacities    storage       af      inp.af
       pStorageInpCap storage-roles-capacities    storage capacity     inp.cap
    pStorageInpNewCap storage-roles-capacities    storage capacity    inp.ncap
        pStorageOutAf storage-roles-capacities    storage       af      out.af
    pStorageOutNewCap storage-roles-capacities    storage capacity    out.ncap
       pStorageStgCap storage-roles-capacities    storage capacity     stg.cap
    pStorageStgNewCap storage-roles-capacities    storage capacity    stg.ncap
 pStorageWeatherInpAf          storage-weather    storage  weather     inp.waf
 pStorageWeatherOutAf          storage-weather    storage  weather     out.waf
          pSupReserve                   supply     supply  reserve         res
        pTechPho2AInp           tech-aux-flows technology     aeff    pho2ainp
        pTechPho2AOut           tech-aux-flows technology     aeff    pho2aout
        pTechRet2AInp           tech-aux-flows technology     aeff    ret2ainp
        pTechRet2AOut           tech-aux-flows technology     aeff    ret2aout
          pTechNewCap     tech-capacity-bounds technology capacity        ncap
       pTechCinp2ginp          tech-efficiency technology     ceff   cinp2ginp
        pTechEmisComm                tech-emis technology    input  combustion
        pTradeCap2Act   trade-capacity-vintage      trade  cap2act            
         pTradeNewCap   trade-capacity-vintage      trade capacity        ncap
       pTradeStockNew   trade-capacity-vintage                                
      pTradeStockSurv   trade-capacity-vintage                                
             pTradeIr               trade-core      trade    trade         ava
           pTradeIrAf               trade-core      trade    trade          af
    pTradeIrCdst2Ainp              trade-costs      trade     aeff   cdst2ainp
    pTradeIrCdst2Aout              trade-costs      trade     aeff   cdst2aout
    pTradeIrCsrc2Ainp              trade-costs      trade     aeff   csrc2ainp
    pTradeIrCsrc2Aout              trade-costs      trade     aeff   csrc2aout
                 name                   family      class     slot     colName
               <char>                   <char>     <char>   <char>      <char>
```

Tagged rows: 100 | inferred: 180 | uncovered: 431 of 711
