# Compare two scenarios' model inputs

Mechanically diffs the interpolated model input (`modInp`) of two
scenarios: parameter presence, per-parameter metadata (`dimSets` –
distinguishing order-only changes – `type`, `defVal`, `interpolation`,
`desc`), parameter data (key-based, row-order insensitive, with numeric
tolerance), model sets, the compiled user constraints / user cost terms,
and `@misc`. The classic use is explaining WHY two solved scenarios
differ, or guarding a refactor of the interpolation pipeline.

## Usage

``` r
compare_inputs(a, b, tol = 1e-08, unfold = FALSE, names = c("A", "B"))

# S3 method for class 'inputs_cmp'
print(x, max_diffs = 5L, ...)
```

## Arguments

- a, b:

  `scenario` or `modInp` objects.

- tol:

  numeric, absolute tolerance for value comparisons.

- unfold:

  logical, unfold folded parameters before comparing (scenario inputs
  only).

- names:

  character(2), display names of the two sides.

- x:

  an `inputs_cmp` object.

- max_diffs:

  integer, number of differing parameters to render with a detailed
  waldo diff (when waldo is installed).

- ...:

  ignored.

## Value

An `inputs_cmp` object (a list) with components `names`, `summary` (one
row per common parameter:
`parameter, status, n_A, n_B, n_diff, max_abs_diff, meta`; status is one
of `identical`, `meta-diff`, `value-diff`, `rows-mismatch`,
`rows-only-A`, `rows-only-B`, `structural`, `diff`), `params` (`only_A`
/ `only_B` / `common` with per-parameter detail), `sets`, `equations`,
`misc`, and `tol`. [`print()`](https://energyRt.org/reference/print.md)
gives a compact report; with waldo installed it renders textual diffs
for the first differing parameters.

## Details

Data is compared as stored by default; `unfold = TRUE` collapses
fold-vs-dense representation drift first (requires scenario inputs).

## See also

[`compare_scenarios()`](https://energyRt.org/reference/compare_scenarios.md)
for solved-results comparison,
[`compare_models()`](https://energyRt.org/reference/compare_models.md)
for declaration-level model diffs.

Other comparison:
[`compare_models()`](https://energyRt.org/reference/compare_models.md),
[`compare_scenarios()`](https://energyRt.org/reference/compare_scenarios.md)

## Examples

``` r
# \donttest{
ek  <- topia$modules$electricity$R3
mod <- newModel("TOPIA", data = ek$repo,
                calendar = topia$modules$calendars$s4_h24,
                region = ek$regions,
                horizon = topia$modules$horizons$base,
                discount = 0.05)
mod <- setGeoscale(mod, ek$geoscale)

# the same model with a three-step gas supply curve instead of flat supply
mod2 <- add(mod, ek$GAS_CURVE, overwrite = TRUE)
a <- interpolate_model(mod,  name = "FLAT")
#> Cost declared at a coarse geoscale level:
#>   TBD_COA_WEST_CENTRAL@invcost: CENTRAL, WEST
#>   TBD_GAS_WEST_CENTRAL@invcost: CENTRAL, WEST
#> The cost is charged ONCE at that cell rather than at each child region, and is discounted at the children's rate. That is usually what a national cost datum means -- said here because it is a different number from naming the children individually.
b <- interpolate_model(mod2, name = "CURVE")
#> Cost declared at a coarse geoscale level:
#>   TBD_COA_WEST_CENTRAL@invcost: CENTRAL, WEST
#>   TBD_GAS_WEST_CENTRAL@invcost: CENTRAL, WEST
#> The cost is charged ONCE at that cell rather than at each child region, and is discounted at the children's rate. That is usually what a national cost datum means -- said here because it is a different number from naming the children individually.

cmp <- compare_inputs(a, b, names = c("flat", "curve"))
cmp
#> ===========================================================================
#> compare_inputs:  A = flat   vs   B = curve   (tol=1e-08, unfold=FALSE)
#> ===========================================================================
#> Parameters: 574 common  | identical: 564  differing: 10  | only-flat: 0  only-curve: 0
#> 
#> --- Differing parameters ---
#>      parameter        status  n_A  n_B n_diff max_abs_diff meta
#>        mSupAva rows-mismatch 2716 2724      0            0     
#>      mSupAvaUp rows-mismatch   24   32      0            0     
#>       mSupComm rows-mismatch    7    9      0            0     
#>       mSupSpan rows-mismatch   14   16      0            0     
#>  mSupTimeslice rows-mismatch  292  294      0            0     
#>      mvSupCost rows-mismatch   28   36      0            0     
#>        pSupAva rows-mismatch   24   32      0            0     
#>       pSupCost rows-mismatch   28   36      0            0     
#>        process rows-mismatch   23   25      0            0     
#>            sup rows-mismatch    7    9      0            0     
#> 
#> --- Sets ---
#>   only in curve: variant
#>   sup             +flat:[SUP_GAS]  +curve:[SUP_GAS_CLS1,SUP_GAS_CLS2,SUP_GAS_CLS3]
#>   process         +flat:[SUP_GAS]  +curve:[SUP_GAS_CLS1,SUP_GAS_CLS2,SUP_GAS_CLS3]
#> User constraints / costs: identical.
#> misc: identical.
#> 
#> --- Data diffs (waldo, first 5 of 10) ---
#> ------------------------------------------------------------
#> process  [rows-mismatch]
#>  `attr(flat, 'row.names')[21:23]`: 21 22 23      
#> `attr(curve, 'row.names')[21:25]`: 21 22 23 24 25
#> 
#> flat vs curve
#>                            process
#>   flat[15, ]  STG_ELC             
#>   flat[16, ]  SUP_BIO             
#>   flat[17, ]  SUP_COA             
#> - flat[18, ]  SUP_GAS             
#> + curve[18, ] SUP_GAS_CLS1        
#> + curve[19, ] SUP_GAS_CLS2        
#> + curve[20, ] SUP_GAS_CLS3        
#>   flat[19, ]  SUP_NUC             
#>   flat[20, ]  TBD_COA_WEST_CENTRAL
#>   flat[21, ]  TBD_ELC_W1_W2       
#> 
#>      flat$process           | curve$process              
#> [15] "STG_ELC"              | "STG_ELC"              [15]
#> [16] "SUP_BIO"              | "SUP_BIO"              [16]
#> [17] "SUP_COA"              | "SUP_COA"              [17]
#> [18] "SUP_GAS"              - "SUP_GAS_CLS1"         [18]
#>                             - "SUP_GAS_CLS2"         [19]
#>                             - "SUP_GAS_CLS3"         [20]
#> [19] "SUP_NUC"              | "SUP_NUC"              [21]
#> [20] "TBD_COA_WEST_CENTRAL" | "TBD_COA_WEST_CENTRAL" [22]
#> [21] "TBD_ELC_W1_W2"        | "TBD_ELC_W1_W2"        [23]
#> ------------------------------------------------------------
#> sup  [rows-mismatch]
#>  `attr(flat, 'row.names')[5:7]`: 5 6 7    
#> `attr(curve, 'row.names')[5:9]`: 5 6 7 8 9
#> 
#> flat vs curve
#>                       sup
#>   flat[3, ]  RES_WIN     
#>   flat[4, ]  SUP_BIO     
#>   flat[5, ]  SUP_COA     
#> - flat[6, ]  SUP_GAS     
#> + curve[6, ] SUP_GAS_CLS1
#> + curve[7, ] SUP_GAS_CLS2
#> + curve[8, ] SUP_GAS_CLS3
#>   flat[7, ]  SUP_NUC     
#> 
#>     flat$sup  | curve$sup         
#> [3] "RES_WIN" | "RES_WIN"      [3]
#> [4] "SUP_BIO" | "SUP_BIO"      [4]
#> [5] "SUP_COA" | "SUP_COA"      [5]
#> [6] "SUP_GAS" - "SUP_GAS_CLS1" [6]
#>               - "SUP_GAS_CLS2" [7]
#>               - "SUP_GAS_CLS3" [8]
#> [7] "SUP_NUC" | "SUP_NUC"      [9]
#> ------------------------------------------------------------
#> pSupCost  [rows-mismatch]
#>  `attr(flat, 'row.names')[26:28]`: 26 27 28                      and 1 more...
#> `attr(curve, 'row.names')[26:36]`: 26 27 28 29 30 31 32 33 34 35           ...
#> 
#> flat vs curve
#>               comm  region          sup timeslice value year
#>   flat[18, ]   COA WEST    SUP_COA         ANNUAL   1.8 2030
#>   flat[19, ]   COA WEST    SUP_COA         ANNUAL   1.8 2040
#>   flat[20, ]   COA WEST    SUP_COA         ANNUAL   1.8 2050
#> - flat[21, ]   GAS WEST    SUP_GAS         ANNUAL   6.0 2025
#> - flat[22, ]   GAS WEST    SUP_GAS         ANNUAL   6.0 2030
#> - flat[23, ]   GAS WEST    SUP_GAS         ANNUAL   6.0 2040
#> - flat[24, ]   GAS WEST    SUP_GAS         ANNUAL   6.0 2050
#> + curve[21, ]  GAS WEST    SUP_GAS_CLS1    ANNUAL   4.0 2025
#> + curve[22, ]  GAS WEST    SUP_GAS_CLS1    ANNUAL   4.0 2030
#> + curve[23, ]  GAS WEST    SUP_GAS_CLS1    ANNUAL   4.0 2040
#> + curve[24, ]  GAS WEST    SUP_GAS_CLS1    ANNUAL   4.0 2050
#> + curve[25, ]  GAS WEST    SUP_GAS_CLS2    ANNUAL   8.0 2025
#> + curve[26, ]  GAS WEST    SUP_GAS_CLS2    ANNUAL   8.0 2030
#> + curve[27, ]  GAS WEST    SUP_GAS_CLS2    ANNUAL   8.0 2040
#> and 8 more ...
#> 
#>      flat$comm | curve$comm               
#> [22] "GAS"     | "GAS"      [22]          
#> [23] "GAS"     | "GAS"      [23]          
#> [24] "GAS"     | "GAS"      [24]          
#>                - "GAS"      [25]          
#>                - "GAS"      [26]          
#>                - "GAS"      [27]          
#>                - "GAS"      [28]          
#>                - "GAS"      [29]          
#>                - "GAS"      [30]          
#>                - "GAS"      [31]          
#>  ... ...         ...        and 4 more ...
#> 
#>      flat$region | curve$region               
#> [22] "WEST"      | "WEST"       [22]          
#> [23] "WEST"      | "WEST"       [23]          
#> [24] "WEST"      | "WEST"       [24]          
#>                  - "WEST"       [25]          
#>                  - "WEST"       [26]          
#>                  - "WEST"       [27]          
#>                  - "WEST"       [28]          
#>                  - "WEST"       [29]          
#>                  - "WEST"       [30]          
#>                  - "WEST"       [31]          
#>  ... ...           ...          and 4 more ...
#> 
#>      flat$sup  | curve$sup                    
#> [18] "SUP_COA" | "SUP_COA"      [18]          
#> [19] "SUP_COA" | "SUP_COA"      [19]          
#> [20] "SUP_COA" | "SUP_COA"      [20]          
#> [21] "SUP_GAS" - "SUP_GAS_CLS1" [21]          
#> [22] "SUP_GAS" - "SUP_GAS_CLS1" [22]          
#> [23] "SUP_GAS" - "SUP_GAS_CLS1" [23]          
#> [24] "SUP_GAS" - "SUP_GAS_CLS1" [24]          
#>                - "SUP_GAS_CLS2" [25]          
#>                - "SUP_GAS_CLS2" [26]          
#>                - "SUP_GAS_CLS2" [27]          
#>  ... ...         ...            and 8 more ...
#> 
#>      flat$timeslice | curve$timeslice               
#> [26] "ANNUAL"       | "ANNUAL"        [26]          
#> [27] "ANNUAL"       | "ANNUAL"        [27]          
#> [28] "ANNUAL"       | "ANNUAL"        [28]          
#>                     - "ANNUAL"        [29]          
#>                     - "ANNUAL"        [30]          
#>                     - "ANNUAL"        [31]          
#>                     - "ANNUAL"        [32]          
#>                     - "ANNUAL"        [33]          
#>                     - "ANNUAL"        [34]          
#>                     - "ANNUAL"        [35]          
#>  ... ...              ...             and 1 more ...
#> 
#>  `flat$value[18:27]`: 1.8 1.8 1.8 6 6 6 6       and 8 more...
#> `curve$value[18:35]`: 1.8 1.8 1.8 4 4 4 4 8 8 8           ...
#> 
#>      flat$year | curve$year               
#> [26] 2030      | 2030       [26]          
#> [27] 2040      | 2040       [27]          
#> [28] 2050      | 2050       [28]          
#>                - 2025       [29]          
#>                - 2030       [30]          
#>                - 2040       [31]          
#>                - 2050       [32]          
#>                - 2025       [33]          
#>                - 2030       [34]          
#>                - 2040       [35]          
#>  ... ...         ...        and 1 more ...
#> ------------------------------------------------------------
#> pSupAva  [rows-mismatch]
#>  `attr(flat, 'row.names')[22:24]`: 22 23 24                      and 1 more...
#> `attr(curve, 'row.names')[22:32]`: 22 23 24 25 26 27 28 29 30 31           ...
#> 
#> flat vs curve
#>               comm  region          sup timeslice type     value year
#>   flat[18, ]   COA WEST    SUP_COA         ANNUAL   up 300.00000 2030
#>   flat[19, ]   COA WEST    SUP_COA         ANNUAL   up 300.00000 2040
#>   flat[20, ]   COA WEST    SUP_COA         ANNUAL   up 300.00000 2050
#> - flat[21, ]   GAS WEST    SUP_GAS         ANNUAL   up  50.00000 2025
#> - flat[22, ]   GAS WEST    SUP_GAS         ANNUAL   up  50.00000 2030
#> - flat[23, ]   GAS WEST    SUP_GAS         ANNUAL   up  50.00000 2040
#> - flat[24, ]   GAS WEST    SUP_GAS         ANNUAL   up  50.00000 2050
#> + curve[21, ]  GAS WEST    SUP_GAS_CLS1    ANNUAL   up  16.66667 2025
#> + curve[22, ]  GAS WEST    SUP_GAS_CLS1    ANNUAL   up  16.66667 2030
#> + curve[23, ]  GAS WEST    SUP_GAS_CLS1    ANNUAL   up  16.66667 2040
#> + curve[24, ]  GAS WEST    SUP_GAS_CLS1    ANNUAL   up  16.66667 2050
#> + curve[25, ]  GAS WEST    SUP_GAS_CLS2    ANNUAL   up  16.66667 2025
#> + curve[26, ]  GAS WEST    SUP_GAS_CLS2    ANNUAL   up  16.66667 2030
#> + curve[27, ]  GAS WEST    SUP_GAS_CLS2    ANNUAL   up  16.66667 2040
#> and 5 more ...
#> 
#>      flat$comm | curve$comm               
#> [22] "GAS"     | "GAS"      [22]          
#> [23] "GAS"     | "GAS"      [23]          
#> [24] "GAS"     | "GAS"      [24]          
#>                - "GAS"      [25]          
#>                - "GAS"      [26]          
#>                - "GAS"      [27]          
#>                - "GAS"      [28]          
#>                - "GAS"      [29]          
#>                - "GAS"      [30]          
#>                - "GAS"      [31]          
#>  ... ...         ...        and 1 more ...
#> 
#>      flat$region | curve$region               
#> [22] "WEST"      | "WEST"       [22]          
#> [23] "WEST"      | "WEST"       [23]          
#> [24] "WEST"      | "WEST"       [24]          
#>                  - "WEST"       [25]          
#>                  - "WEST"       [26]          
#>                  - "WEST"       [27]          
#>                  - "WEST"       [28]          
#>                  - "WEST"       [29]          
#>                  - "WEST"       [30]          
#>                  - "WEST"       [31]          
#>  ... ...           ...          and 1 more ...
#> 
#>      flat$sup  | curve$sup                    
#> [18] "SUP_COA" | "SUP_COA"      [18]          
#> [19] "SUP_COA" | "SUP_COA"      [19]          
#> [20] "SUP_COA" | "SUP_COA"      [20]          
#> [21] "SUP_GAS" - "SUP_GAS_CLS1" [21]          
#> [22] "SUP_GAS" - "SUP_GAS_CLS1" [22]          
#> [23] "SUP_GAS" - "SUP_GAS_CLS1" [23]          
#> [24] "SUP_GAS" - "SUP_GAS_CLS1" [24]          
#>                - "SUP_GAS_CLS2" [25]          
#>                - "SUP_GAS_CLS2" [26]          
#>                - "SUP_GAS_CLS2" [27]          
#>  ... ...         ...            and 5 more ...
#> 
#>      flat$timeslice | curve$timeslice               
#> [22] "ANNUAL"       | "ANNUAL"        [22]          
#> [23] "ANNUAL"       | "ANNUAL"        [23]          
#> [24] "ANNUAL"       | "ANNUAL"        [24]          
#>                     - "ANNUAL"        [25]          
#>                     - "ANNUAL"        [26]          
#>                     - "ANNUAL"        [27]          
#>                     - "ANNUAL"        [28]          
#>                     - "ANNUAL"        [29]          
#>                     - "ANNUAL"        [30]          
#>                     - "ANNUAL"        [31]          
#>  ... ...              ...             and 1 more ...
#> 
#>      flat$type | curve$type               
#> [22] "up"      | "up"       [22]          
#> [23] "up"      | "up"       [23]          
#> [24] "up"      | "up"       [24]          
#>                - "up"       [25]          
#>                - "up"       [26]          
#>                - "up"       [27]          
#>                - "up"       [28]          
#>                - "up"       [29]          
#>                - "up"       [30]          
#>                - "up"       [31]          
#>  ... ...         ...        and 1 more ...
#> 
#>      flat$value | curve$value                    
#> [18] 300        | 300              [18]          
#> [19] 300        | 300              [19]          
#> [20] 300        | 300              [20]          
#> [21] 50         - 16.6666666666667 [21]          
#> [22] 50         - 16.6666666666667 [22]          
#> [23] 50         - 16.6666666666667 [23]          
#> [24] 50         - 16.6666666666667 [24]          
#>                 - 16.6666666666667 [25]          
#>                 - 16.6666666666667 [26]          
#>                 - 16.6666666666667 [27]          
#>  ... ...          ...              and 5 more ...
#> 
#>      flat$year | curve$year               
#> [22] 2030      | 2030       [22]          
#> [23] 2040      | 2040       [23]          
#> [24] 2050      | 2050       [24]          
#>                - 2025       [25]          
#>                - 2030       [26]          
#>                - 2040       [27]          
#>                - 2050       [28]          
#>                - 2025       [29]          
#>                - 2030       [30]          
#>                - 2040       [31]          
#>  ... ...         ...        and 1 more ...
#> ------------------------------------------------------------
#> mSupTimeslice  [rows-mismatch]
#>  `attr(flat, 'row.names')[290:292]`: 290 291 292        
#> `attr(curve, 'row.names')[290:294]`: 290 291 292 293 294
#> 
#> flat vs curve
#>                         sup timeslice
#>   flat[288, ]  RES_WIN        WIN_h23
#>   flat[289, ]  SUP_BIO        ANNUAL 
#>   flat[290, ]  SUP_COA        ANNUAL 
#> - flat[291, ]  SUP_GAS        ANNUAL 
#> + curve[291, ] SUP_GAS_CLS1   ANNUAL 
#> + curve[292, ] SUP_GAS_CLS2   ANNUAL 
#> + curve[293, ] SUP_GAS_CLS3   ANNUAL 
#>   flat[292, ]  SUP_NUC        ANNUAL 
#> 
#>       flat$sup  | curve$sup           
#> [288] "RES_WIN" | "RES_WIN"      [288]
#> [289] "SUP_BIO" | "SUP_BIO"      [289]
#> [290] "SUP_COA" | "SUP_COA"      [290]
#> [291] "SUP_GAS" - "SUP_GAS_CLS1" [291]
#>                 - "SUP_GAS_CLS2" [292]
#>                 - "SUP_GAS_CLS3" [293]
#> [292] "SUP_NUC" | "SUP_NUC"      [294]
#> 
#> `flat$timeslice[290:292]`:  "ANNUAL" "ANNUAL" "ANNUAL"                  
#> `curve$timeslice[290:294]`: "ANNUAL" "ANNUAL" "ANNUAL" "ANNUAL" "ANNUAL"
#> 
#> ===========================================================================
print(cmp, max_diffs = 3)
#> ===========================================================================
#> compare_inputs:  A = flat   vs   B = curve   (tol=1e-08, unfold=FALSE)
#> ===========================================================================
#> Parameters: 574 common  | identical: 564  differing: 10  | only-flat: 0  only-curve: 0
#> 
#> --- Differing parameters ---
#>      parameter        status  n_A  n_B n_diff max_abs_diff meta
#>        mSupAva rows-mismatch 2716 2724      0            0     
#>      mSupAvaUp rows-mismatch   24   32      0            0     
#>       mSupComm rows-mismatch    7    9      0            0     
#>       mSupSpan rows-mismatch   14   16      0            0     
#>  mSupTimeslice rows-mismatch  292  294      0            0     
#>      mvSupCost rows-mismatch   28   36      0            0     
#>        pSupAva rows-mismatch   24   32      0            0     
#>       pSupCost rows-mismatch   28   36      0            0     
#>        process rows-mismatch   23   25      0            0     
#>            sup rows-mismatch    7    9      0            0     
#> 
#> --- Sets ---
#>   only in curve: variant
#>   sup             +flat:[SUP_GAS]  +curve:[SUP_GAS_CLS1,SUP_GAS_CLS2,SUP_GAS_CLS3]
#>   process         +flat:[SUP_GAS]  +curve:[SUP_GAS_CLS1,SUP_GAS_CLS2,SUP_GAS_CLS3]
#> User constraints / costs: identical.
#> misc: identical.
#> 
#> --- Data diffs (waldo, first 3 of 10) ---
#> ------------------------------------------------------------
#> process  [rows-mismatch]
#>  `attr(flat, 'row.names')[21:23]`: 21 22 23      
#> `attr(curve, 'row.names')[21:25]`: 21 22 23 24 25
#> 
#> flat vs curve
#>                            process
#>   flat[15, ]  STG_ELC             
#>   flat[16, ]  SUP_BIO             
#>   flat[17, ]  SUP_COA             
#> - flat[18, ]  SUP_GAS             
#> + curve[18, ] SUP_GAS_CLS1        
#> + curve[19, ] SUP_GAS_CLS2        
#> + curve[20, ] SUP_GAS_CLS3        
#>   flat[19, ]  SUP_NUC             
#>   flat[20, ]  TBD_COA_WEST_CENTRAL
#>   flat[21, ]  TBD_ELC_W1_W2       
#> 
#>      flat$process           | curve$process              
#> [15] "STG_ELC"              | "STG_ELC"              [15]
#> [16] "SUP_BIO"              | "SUP_BIO"              [16]
#> [17] "SUP_COA"              | "SUP_COA"              [17]
#> [18] "SUP_GAS"              - "SUP_GAS_CLS1"         [18]
#>                             - "SUP_GAS_CLS2"         [19]
#>                             - "SUP_GAS_CLS3"         [20]
#> [19] "SUP_NUC"              | "SUP_NUC"              [21]
#> [20] "TBD_COA_WEST_CENTRAL" | "TBD_COA_WEST_CENTRAL" [22]
#> [21] "TBD_ELC_W1_W2"        | "TBD_ELC_W1_W2"        [23]
#> ------------------------------------------------------------
#> sup  [rows-mismatch]
#>  `attr(flat, 'row.names')[5:7]`: 5 6 7    
#> `attr(curve, 'row.names')[5:9]`: 5 6 7 8 9
#> 
#> flat vs curve
#>                       sup
#>   flat[3, ]  RES_WIN     
#>   flat[4, ]  SUP_BIO     
#>   flat[5, ]  SUP_COA     
#> - flat[6, ]  SUP_GAS     
#> + curve[6, ] SUP_GAS_CLS1
#> + curve[7, ] SUP_GAS_CLS2
#> + curve[8, ] SUP_GAS_CLS3
#>   flat[7, ]  SUP_NUC     
#> 
#>     flat$sup  | curve$sup         
#> [3] "RES_WIN" | "RES_WIN"      [3]
#> [4] "SUP_BIO" | "SUP_BIO"      [4]
#> [5] "SUP_COA" | "SUP_COA"      [5]
#> [6] "SUP_GAS" - "SUP_GAS_CLS1" [6]
#>               - "SUP_GAS_CLS2" [7]
#>               - "SUP_GAS_CLS3" [8]
#> [7] "SUP_NUC" | "SUP_NUC"      [9]
#> ------------------------------------------------------------
#> pSupCost  [rows-mismatch]
#>  `attr(flat, 'row.names')[26:28]`: 26 27 28                      and 1 more...
#> `attr(curve, 'row.names')[26:36]`: 26 27 28 29 30 31 32 33 34 35           ...
#> 
#> flat vs curve
#>               comm  region          sup timeslice value year
#>   flat[18, ]   COA WEST    SUP_COA         ANNUAL   1.8 2030
#>   flat[19, ]   COA WEST    SUP_COA         ANNUAL   1.8 2040
#>   flat[20, ]   COA WEST    SUP_COA         ANNUAL   1.8 2050
#> - flat[21, ]   GAS WEST    SUP_GAS         ANNUAL   6.0 2025
#> - flat[22, ]   GAS WEST    SUP_GAS         ANNUAL   6.0 2030
#> - flat[23, ]   GAS WEST    SUP_GAS         ANNUAL   6.0 2040
#> - flat[24, ]   GAS WEST    SUP_GAS         ANNUAL   6.0 2050
#> + curve[21, ]  GAS WEST    SUP_GAS_CLS1    ANNUAL   4.0 2025
#> + curve[22, ]  GAS WEST    SUP_GAS_CLS1    ANNUAL   4.0 2030
#> + curve[23, ]  GAS WEST    SUP_GAS_CLS1    ANNUAL   4.0 2040
#> + curve[24, ]  GAS WEST    SUP_GAS_CLS1    ANNUAL   4.0 2050
#> + curve[25, ]  GAS WEST    SUP_GAS_CLS2    ANNUAL   8.0 2025
#> + curve[26, ]  GAS WEST    SUP_GAS_CLS2    ANNUAL   8.0 2030
#> + curve[27, ]  GAS WEST    SUP_GAS_CLS2    ANNUAL   8.0 2040
#> and 8 more ...
#> 
#>      flat$comm | curve$comm               
#> [22] "GAS"     | "GAS"      [22]          
#> [23] "GAS"     | "GAS"      [23]          
#> [24] "GAS"     | "GAS"      [24]          
#>                - "GAS"      [25]          
#>                - "GAS"      [26]          
#>                - "GAS"      [27]          
#>                - "GAS"      [28]          
#>                - "GAS"      [29]          
#>                - "GAS"      [30]          
#>                - "GAS"      [31]          
#>  ... ...         ...        and 4 more ...
#> 
#>      flat$region | curve$region               
#> [22] "WEST"      | "WEST"       [22]          
#> [23] "WEST"      | "WEST"       [23]          
#> [24] "WEST"      | "WEST"       [24]          
#>                  - "WEST"       [25]          
#>                  - "WEST"       [26]          
#>                  - "WEST"       [27]          
#>                  - "WEST"       [28]          
#>                  - "WEST"       [29]          
#>                  - "WEST"       [30]          
#>                  - "WEST"       [31]          
#>  ... ...           ...          and 4 more ...
#> 
#>      flat$sup  | curve$sup                    
#> [18] "SUP_COA" | "SUP_COA"      [18]          
#> [19] "SUP_COA" | "SUP_COA"      [19]          
#> [20] "SUP_COA" | "SUP_COA"      [20]          
#> [21] "SUP_GAS" - "SUP_GAS_CLS1" [21]          
#> [22] "SUP_GAS" - "SUP_GAS_CLS1" [22]          
#> [23] "SUP_GAS" - "SUP_GAS_CLS1" [23]          
#> [24] "SUP_GAS" - "SUP_GAS_CLS1" [24]          
#>                - "SUP_GAS_CLS2" [25]          
#>                - "SUP_GAS_CLS2" [26]          
#>                - "SUP_GAS_CLS2" [27]          
#>  ... ...         ...            and 8 more ...
#> 
#>      flat$timeslice | curve$timeslice               
#> [26] "ANNUAL"       | "ANNUAL"        [26]          
#> [27] "ANNUAL"       | "ANNUAL"        [27]          
#> [28] "ANNUAL"       | "ANNUAL"        [28]          
#>                     - "ANNUAL"        [29]          
#>                     - "ANNUAL"        [30]          
#>                     - "ANNUAL"        [31]          
#>                     - "ANNUAL"        [32]          
#>                     - "ANNUAL"        [33]          
#>                     - "ANNUAL"        [34]          
#>                     - "ANNUAL"        [35]          
#>  ... ...              ...             and 1 more ...
#> 
#>  `flat$value[18:27]`: 1.8 1.8 1.8 6 6 6 6       and 8 more...
#> `curve$value[18:35]`: 1.8 1.8 1.8 4 4 4 4 8 8 8           ...
#> 
#>      flat$year | curve$year               
#> [26] 2030      | 2030       [26]          
#> [27] 2040      | 2040       [27]          
#> [28] 2050      | 2050       [28]          
#>                - 2025       [29]          
#>                - 2030       [30]          
#>                - 2040       [31]          
#>                - 2050       [32]          
#>                - 2025       [33]          
#>                - 2030       [34]          
#>                - 2040       [35]          
#>  ... ...         ...        and 1 more ...
#> 
#> ===========================================================================
# }
```
