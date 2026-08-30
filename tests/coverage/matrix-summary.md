# Coverage matrix summary

Generated: 2026-08-26 08:39 | energyRt 0.87.5.9000

## Rows by kind x depth

```
Key: <kind>
     kind  none     C     I     S     X
   <char> <int> <int> <int> <int> <int>
   bounds     5     1     3    16    12
 equation   116     0     1    22     9
      map   228     0    42    14    13
   numpar    21     2    23    52    35
      set     0     0     0     1    12
 variable    26     0     3    39    27
```

## Numeric parameters: family x depth

```
Key: <family>
                      family  none     C     I     S     X
                      <char> <int> <int> <int> <int> <int>
         calendar-timeslices     0     0     0     4     0
                   commodity     0     0     0     2     0
                      demand     0     0     0     0     1
                 discounting     0     0     0     0     3
                 dummy-debug     0     0     0     2     0
           horizon-periodlen     1     0     0     0     2
           import-export-row     0     0     1     1     4
           storage-aux-flows     5     0     5     5     0
           storage-costs-eac     9     0     3    11     5
            storage-duration     0     0     0     0     1
          storage-efficiency     0     0     0     3     0
             storage-inp2out     0     0     0     1     0
    storage-roles-capacities     3     0     0     6     5
 storage-startlevel-fullyear     0     0     1     0     0
               storage-varom     0     0     0     3     0
             storage-weather     1     1     0     1     0
                      supply     0     0     1     3     0
                 tax-subsidy     0     0     2     4     0
              tech-aux-flows     4     0     4     6     0
           tech-availability     0     0     1     1     2
        tech-capacity-bounds     0     0     0     1     2
                  tech-costs     0     2     0     1     2
       tech-eac-wacc-payback     0     0     0     1     2
             tech-efficiency     0     0     2     3     1
                   tech-emis     0     0     0     1     0
           tech-share-bounds     0     0     0     0     1
       tech-stock-retirement     0     0     1     0     5
                tech-weather     0     0     2     1     0
      trade-capacity-vintage     3     0     1     2     8
                  trade-core     0     0     0     2     1
                 trade-costs     0     0     2     2     2
               weather-class     0     0     0     1     0
                      family  none     C     I     S     X
                      <char> <int> <int> <int> <int> <int>
```

## Zero-coverage numeric parameters (26)

```
                 name                   family      class     slot     colName
               <char>                   <char>     <char>   <char>      <char>
             cardYear        horizon-periodlen                                
     pStorageNCap2Stg        storage-aux-flows    storage     aeff    ncap2stg
     pStoragePho2AInp        storage-aux-flows    storage     aeff    pho2ainp
     pStoragePho2AOut        storage-aux-flows    storage     aeff    pho2aout
     pStorageRet2AInp        storage-aux-flows    storage     aeff    ret2ainp
     pStorageRet2AOut        storage-aux-flows    storage     aeff    ret2aout
   pStorageInpRetCost        storage-costs-eac    storage  invcost inp.retcost
  pStorageInpStockNew        storage-costs-eac                                
 pStorageInpStockSurv        storage-costs-eac                                
   pStorageOutRetCost        storage-costs-eac    storage  invcost out.retcost
  pStorageOutStockNew        storage-costs-eac                                
 pStorageOutStockSurv        storage-costs-eac                                
   pStorageStgRetCost        storage-costs-eac    storage  invcost stg.retcost
  pStorageStgStockNew        storage-costs-eac                                
 pStorageStgStockSurv        storage-costs-eac                                
        pStorageInpAf storage-roles-capacities    storage       af      inp.af
    pStorageOutNewCap storage-roles-capacities    storage capacity    out.ncap
    pStorageStgNewCap storage-roles-capacities    storage capacity    stg.ncap
 pStorageWeatherOutAf          storage-weather    storage  weather     out.waf
        pTechPho2AInp           tech-aux-flows technology     aeff    pho2ainp
        pTechPho2AOut           tech-aux-flows technology     aeff    pho2aout
        pTechRet2AInp           tech-aux-flows technology     aeff    ret2ainp
        pTechRet2AOut           tech-aux-flows technology     aeff    ret2aout
         pTradeNewCap   trade-capacity-vintage      trade capacity        ncap
       pTradeStockNew   trade-capacity-vintage                                
      pTradeStockSurv   trade-capacity-vintage                                
                 name                   family      class     slot     colName
               <char>                   <char>     <char>   <char>      <char>
```

Tagged rows: 161 | inferred: 166 | uncovered: 396 of 723
