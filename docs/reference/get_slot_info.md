# Retrieve slot details in rd-format

Retrieve slot details in rd-format

## Usage

``` r
get_slot_info(class_name = "technology", slot_name = "ceff", col_names = TRUE)
```

## Arguments

- class_name:

  character, name of class.

- slot_name:

  character, name of slot to retrieve.

- col_names:

  logical, if columns information should be returned for data.frame
  slots.

## Value

character, roxygen2 formatted string with slot details.

## Examples

``` r
slotNames("technology")
#>  [1] "name"               "desc"               "input"             
#>  [4] "output"             "aux"                "units"             
#>  [7] "group"              "cap2act"            "geff"              
#> [10] "ceff"               "aeff"               "af"                
#> [13] "afs"                "weather"            "fixom"             
#> [16] "varom"              "invcost"            "start"             
#> [19] "end"                "olife"              "capacity"          
#> [22] "optimizeRetirement" "fullYear"           "timeframe"         
#> [25] "region"             "misc"              
get_slot_info("technology", "input") |> cat()
#> data.frame. Main commodities input. Main commodities are linked to the process capacity and activity. Their parameters are defined in the `ceff` slot.
#>   \describe{
#>     \item{comm}{character. Name of the input commodity.}
#>     \item{unit}{character. Unit of the input commodity.}
#>     \item{group}{character. Name of input-commodities-group.}
#>     \item{combustion}{numeric. combustion factor from 0 to 1 (default 1) to calculate emissions from fuels combustion (commodities intermediate consumption, more broadly)
#> }
#>  }
get_slot_info("technology", "capacity") |> cat()
#> data.frame. Capacity of the installed technology (in units of capacity).
#>   \describe{
#>     \item{region}{character. Region name to apply the parameter, NA for every region.}
#>     \item{year}{integer. Year to apply the parameter, required, values between specified years will be interpolated.}
#>     \item{stock}{numeric. Predefined capacity of the technology in units of capacity, default is 0. This parameter also defines the exogenous capacity retirement (age-based), or exogenous capacity additions, not optimized by the model, and not included in investment costs.
#> }
#>     \item{cap.lo}{numeric. Lower bound on the total capacity (preexisting stock and new installations), ignored if NA.}
#>     \item{cap.up}{numeric. Upper bound on the total capacity (preexisting stock and new installations), ignored if NA.}
#>     \item{cap.fx}{numeric. Fixed total capacity (preexisting stock and new installations), ignored if NA. This parameter overrides `cap.lo` and `cap.up`.}
#>     \item{ncap.lo}{numeric. Lower bound on the new capacity (new installations), ignored if NA.}
#>     \item{ncap.up}{numeric. Upper bound on the new capacity (new installations), ignored if NA.}
#>     \item{ncap.fx}{numeric. Fixed new capacity (new installations), ignored if NA. This parameter overrides `ncap.lo` and `ncap.up`.}
#>     \item{ret.lo}{numeric. Lower bound on the capacity retirement (age-based), ignored if NA.}
#>     \item{ret.up}{numeric. Upper bound on the capacity retirement (age-based), ignored if NA.}
#>     \item{ret.fx}{numeric. Fixed capacity retirement (age-based), ignored if NA. This parameter overrides `ret.lo` and `ret.up`.}
#>  }
get_slot_info("demand", "dem") |> cat()
#> data.frame. Specification of the demand.
#>   \describe{
#>     \item{region}{character. Name of region for the demand value. NA for every region.}
#>     \item{year}{integer. Year of the demand. NA for every year.}
#>     \item{slice}{character. Name of the slice for the demand value. NA for every slice.}
#>     \item{dem}{numeric. Value of the demand.}
#>  }
get_slot_info("commodity", "agg") |> cat()
#> data.frame. Used to define an aggregation of several commodities into the `name` commodity.
#> 
#>   \describe{
#>     \item{comm}{character. Name of a commodity being aggregated.}
#>     \item{unit}{character. Unit of the commodity being aggregated.}
#>     \item{agg}{numeric. weight of the commodity in the aggregation, must be set for all aggregated commodities.
#> }
#>  }
```
