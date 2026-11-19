# Coverage matrix summary

Generated: 2026-11-18 20:01 | energyRt 0.91.5.9006

## Rows by kind x depth

```
Key: <kind>
     kind  none     C     I     S     X
   <char> <int> <int> <int> <int> <int>
   bounds     2     1     5    22    11
 equation   126     2     2    36    18
      map   205     2    58    69    26
   numpar    20     0    21    81    40
      set     0     0     0     2    14
 variable    21     1     0    48    27
```

## Numeric parameters: family x depth

```
Key: <family>
                      family  none     C     I     S     X
                      <char> <int> <int> <int> <int> <int>
         calendar-timeslices     0     0     0     3     1
                   commodity     0     0     0     2     0
                      demand     0     0     0     1     0
                 discounting     0     0     0     0     3
                 dummy-debug     0     0     0     2     0
           horizon-periodlen     1     0     0     0     2
           import-export-row     0     0     1     1     4
                       other     0     0     0     0     1
           storage-aux-flows     0     0     1    22     0
           storage-costs-eac     9     0     4    10     5
            storage-duration     0     0     0     0     1
          storage-efficiency     0     0     0     3     0
             storage-inp2out     0     0     0     1     0
             storage-inp2stg     0     0     0     1     0
    storage-roles-capacities     0     0     1     8     5
 storage-startlevel-fullyear     0     0     1     0     0
              storage-target     0     0     2     0     0
               storage-varom     0     0     0     3     0
             storage-weather     2     0     0     1     0
                      supply     0     0     1     3     0
                 tax-subsidy     8     0     4     8     4
              tech-aux-flows     0     0     0    13     1
           tech-availability     0     0     1     1     2
        tech-capacity-bounds     0     1     0     1     1
                  tech-costs     0     0     2     2     1
       tech-eac-wacc-payback     0     0     0     0     3
             tech-efficiency     0     0     2     4     0
                   tech-emis     0     0     0     1     0
           tech-share-bounds     0     0     0     0     1
       tech-stock-retirement     0     0     1     0     5
                tech-weather     0     0     2     1     0
      trade-capacity-vintage     2     0     1     5     8
                  trade-core     0     0     0     2     1
                 trade-costs     0     0     2     3     2
               weather-class     0     0     0     1     0
                      family  none     C     I     S     X
                      <char> <int> <int> <int> <int> <int>
```

## Zero-coverage numeric parameters (22)

```
                 name                 family   class    slot     colName
               <char>                 <char>  <char>  <char>      <char>
             cardYear      horizon-periodlen                            
   pStorageInpRetCost      storage-costs-eac storage invcost inp.retcost
  pStorageInpStockNew      storage-costs-eac                            
 pStorageInpStockSurv      storage-costs-eac                            
   pStorageOutRetCost      storage-costs-eac storage invcost out.retcost
  pStorageOutStockNew      storage-costs-eac                            
 pStorageOutStockSurv      storage-costs-eac                            
   pStorageStgRetCost      storage-costs-eac storage invcost stg.retcost
  pStorageStgStockNew      storage-costs-eac                            
 pStorageStgStockSurv      storage-costs-eac                            
    pStorageWeatherAf        storage-weather storage weather     stg.waf
 pStorageWeatherOutAf        storage-weather storage weather     out.waf
          pSubProcAct            tax-subsidy                            
          pSubProcCap            tax-subsidy                            
          pSubProcInp            tax-subsidy                            
       pSubProcInpCap            tax-subsidy                            
      pSubProcInpNcap            tax-subsidy                            
          pSubProcOut            tax-subsidy                            
       pSubProcOutCap            tax-subsidy                            
      pTaxProcOutNcap            tax-subsidy                            
       pTradeStockNew trade-capacity-vintage                            
      pTradeStockSurv trade-capacity-vintage                            
                 name                 family   class    slot     colName
               <char>                 <char>  <char>  <char>      <char>
```

Tagged rows: 272 | inferred: 214 | uncovered: 374 of 860
