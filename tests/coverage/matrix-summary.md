# Coverage matrix summary

Generated: 2026-10-18 20:55 | energyRt 0.90.0.9028

## Rows by kind x depth

```
Key: <kind>
     kind  none     C     I     S     X
   <char> <int> <int> <int> <int> <int>
   bounds     3     2     4    18    11
 equation   129     2     2    38    11
      map   216     2    52    57    25
   numpar    17     2    13    80    39
      set     0     0     0     2    13
 variable    24     0     0    45    26
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
           storage-aux-flows     1     0     0    22     0
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
                 tax-subsidy     4     0     2     6     4
              tech-aux-flows     0     0     0    13     1
           tech-availability     0     0     1     1     2
        tech-capacity-bounds     0     1     0     1     1
                  tech-costs     0     2     0     1     2
       tech-eac-wacc-payback     0     0     0     1     2
             tech-efficiency     0     0     2     3     1
                   tech-emis     0     0     0     1     0
           tech-share-bounds     0     0     0     0     1
       tech-stock-retirement     0     0     1     0     5
                tech-weather     0     0     2     1     0
      trade-capacity-vintage     2     0     1     4     7
                  trade-core     0     0     0     2     1
                 trade-costs     0     0     2     2     2
               weather-class     0     0     0     1     0
                      family  none     C     I     S     X
                      <char> <int> <int> <int> <int> <int>
```

## Zero-coverage numeric parameters (20)

```
                 name                   family   class     slot     colName
               <char>                   <char>  <char>   <char>      <char>
             cardYear        horizon-periodlen                             
     pStorageNCap2Stg        storage-aux-flows storage     aeff    ncap2stg
   pStorageInpRetCost        storage-costs-eac storage  invcost inp.retcost
  pStorageInpStockNew        storage-costs-eac                             
 pStorageInpStockSurv        storage-costs-eac                             
   pStorageOutRetCost        storage-costs-eac storage  invcost out.retcost
  pStorageOutStockNew        storage-costs-eac                             
 pStorageOutStockSurv        storage-costs-eac                             
   pStorageStgRetCost        storage-costs-eac storage  invcost stg.retcost
  pStorageStgStockNew        storage-costs-eac                             
 pStorageStgStockSurv        storage-costs-eac                             
    pStorageOutNewCap storage-roles-capacities storage capacity    out.ncap
    pStorageStgNewCap storage-roles-capacities storage capacity    stg.ncap
 pStorageWeatherOutAf          storage-weather storage  weather     out.waf
          pSubProcAct              tax-subsidy                             
          pSubProcCap              tax-subsidy                             
          pSubProcInp              tax-subsidy                             
          pSubProcOut              tax-subsidy                             
       pTradeStockNew   trade-capacity-vintage                             
      pTradeStockSurv   trade-capacity-vintage                             
                 name                   family   class     slot     colName
               <char>                   <char>  <char>   <char>      <char>
```

Tagged rows: 247 | inferred: 197 | uncovered: 389 of 833
