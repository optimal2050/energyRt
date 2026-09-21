# Coverage matrix summary

Generated: 2026-09-21 13:13 | energyRt 0.90.0.9001

## Rows by kind x depth

```
Key: <kind>
     kind  none     C     I     S     X
   <char> <int> <int> <int> <int> <int>
   bounds     3     2     3    19    11
 equation   113     0     1    26    10
      map   223     1    46    27    12
   numpar    29     2    23    53    34
      set     0     0     0     2    11
 variable    26     0     3    39    27
```

## Numeric parameters: family x depth

```
Key: <family>
                      family  none     C     I     S     X
                      <char> <int> <int> <int> <int> <int>
         calendar-timeslices     0     0     0     4     0
                   commodity     0     0     0     2     0
                      demand     0     0     0     1     0
                 discounting     0     0     0     0     3
                 dummy-debug     0     0     0     2     0
           horizon-periodlen     1     0     0     0     2
           import-export-row     0     0     1     1     4
           storage-aux-flows    13     0     5     5     0
           storage-costs-eac     9     0     3    11     5
            storage-duration     0     0     0     0     1
          storage-efficiency     0     0     0     3     0
             storage-inp2out     0     0     0     1     0
             storage-inp2stg     0     0     0     1     0
    storage-roles-capacities     2     0     0     7     5
 storage-startlevel-fullyear     0     0     1     0     0
               storage-varom     0     0     0     3     0
             storage-weather     1     1     0     1     0
                      supply     0     0     1     3     0
                 tax-subsidy     0     0     2     4     0
              tech-aux-flows     4     0     4     6     0
           tech-availability     0     0     1     1     2
        tech-capacity-bounds     0     1     0     1     1
                  tech-costs     0     2     0     1     2
       tech-eac-wacc-payback     0     0     0     1     2
             tech-efficiency     0     0     2     3     1
                   tech-emis     0     0     0     1     0
           tech-share-bounds     0     0     0     0     1
       tech-stock-retirement     0     0     1     0     5
                tech-weather     0     0     2     1     0
      trade-capacity-vintage     2     0     1     3     8
                  trade-core     0     0     0     2     1
                 trade-costs     0     0     2     2     2
               weather-class     0     0     0     1     0
                      family  none     C     I     S     X
                      <char> <int> <int> <int> <int> <int>
```

## Zero-coverage numeric parameters (32)

```
                 name                   family      class     slot
               <char>                   <char>     <char>   <char>
             cardYear        horizon-periodlen                    
  pStorageInpCap2AInp        storage-aux-flows    storage     aeff
  pStorageInpCap2AOut        storage-aux-flows    storage     aeff
 pStorageInpNCap2AInp        storage-aux-flows    storage     aeff
 pStorageInpNCap2AOut        storage-aux-flows    storage     aeff
     pStorageNCap2Stg        storage-aux-flows    storage     aeff
  pStorageOutCap2AOut        storage-aux-flows    storage     aeff
 pStorageOutNCap2AOut        storage-aux-flows    storage     aeff
     pStoragePho2AInp        storage-aux-flows    storage     aeff
     pStoragePho2AOut        storage-aux-flows    storage     aeff
     pStorageRet2AInp        storage-aux-flows    storage     aeff
     pStorageRet2AOut        storage-aux-flows    storage     aeff
  pStorageStgCap2AOut        storage-aux-flows    storage     aeff
 pStorageStgNCap2AOut        storage-aux-flows    storage     aeff
   pStorageInpRetCost        storage-costs-eac    storage  invcost
  pStorageInpStockNew        storage-costs-eac                    
 pStorageInpStockSurv        storage-costs-eac                    
   pStorageOutRetCost        storage-costs-eac    storage  invcost
  pStorageOutStockNew        storage-costs-eac                    
 pStorageOutStockSurv        storage-costs-eac                    
   pStorageStgRetCost        storage-costs-eac    storage  invcost
  pStorageStgStockNew        storage-costs-eac                    
 pStorageStgStockSurv        storage-costs-eac                    
    pStorageOutNewCap storage-roles-capacities    storage capacity
    pStorageStgNewCap storage-roles-capacities    storage capacity
 pStorageWeatherOutAf          storage-weather    storage  weather
        pTechPho2AInp           tech-aux-flows technology     aeff
        pTechPho2AOut           tech-aux-flows technology     aeff
        pTechRet2AInp           tech-aux-flows technology     aeff
        pTechRet2AOut           tech-aux-flows technology     aeff
       pTradeStockNew   trade-capacity-vintage                    
      pTradeStockSurv   trade-capacity-vintage                    
                 name                   family      class     slot
               <char>                   <char>     <char>   <char>
       colName
        <char>
              
  inp.cap2ainp
  inp.cap2aout
 inp.ncap2ainp
 inp.ncap2aout
      ncap2stg
  out.cap2aout
 out.ncap2aout
      pho2ainp
      pho2aout
      ret2ainp
      ret2aout
  stg.cap2aout
 stg.ncap2aout
   inp.retcost
              
              
   out.retcost
              
              
   stg.retcost
              
              
      out.ncap
      stg.ncap
       out.waf
      pho2ainp
      pho2aout
      ret2ainp
      ret2aout
              
              
       colName
        <char>
```

Tagged rows: 183 | inferred: 169 | uncovered: 394 of 746
