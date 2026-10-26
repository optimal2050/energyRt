# List the available report templates

Scans the shipped templates directory (`inst/templates/` of the
installed package) and returns one row per template. The `class` column
says which object class a template is designed for; it is read from the
optional `report-class:` field of the template's YAML front matter,
falling back to inference from the file name. Use the `name` as the
`template =` argument of
[`report()`](https://energyRt.org/reference/report.md); a template
designed for another class can still be forced by passing its file path.

## Usage

``` r
report_templates(class = NULL)
```

## Arguments

- class:

  Optional filter: return only templates whose `class` matches (e.g.
  `"model"`, `"scenario"`, `"process"`).

## Value

A data.frame with columns `name`, `class`, `title`, `path`.

## Examples

``` r
report_templates()
#>          name      class               title
#> 1    calendar   calendar            Calendar
#> 2   commodity  commodity           Commodity
#> 3      config     config       Configuration
#> 4  constraint constraint          Constraint
#> 5       costs      costs          User costs
#> 6      demand     demand              Demand
#> 7      export     export              Export
#> 8     generic    process   Technology Report
#> 9     horizon    horizon             Horizon
#> 10     import     import              Import
#> 11      model      model        Model report
#> 12       full      model Model report (full)
#> 13    summary      model       Model summary
#> 14   scenario   scenario     Scenario report
#> 15       full   scenario     Scenario report
#> 16    summary   scenario    Scenario summary
#> 17  scenarios  scenarios Scenario comparison
#> 18    subsidy    subsidy             Subsidy
#> 19    summary    process  Technology Summary
#> 20     supply     supply              Supply
#> 21        tax        tax                 Tax
#> 22      trade      trade               Trade
#> 23    vehicle    process                <NA>
#> 24    weather    weather             Weather
#>                                                                                                                    path
#> 1          C:/Users/admin/AppData/Local/Temp/RtmpushJ02/temp_libpathca342b697fdb/energyRt/templates/report_calendar.Rmd
#> 2         C:/Users/admin/AppData/Local/Temp/RtmpushJ02/temp_libpathca342b697fdb/energyRt/templates/report_commodity.Rmd
#> 3            C:/Users/admin/AppData/Local/Temp/RtmpushJ02/temp_libpathca342b697fdb/energyRt/templates/report_config.Rmd
#> 4        C:/Users/admin/AppData/Local/Temp/RtmpushJ02/temp_libpathca342b697fdb/energyRt/templates/report_constraint.Rmd
#> 5             C:/Users/admin/AppData/Local/Temp/RtmpushJ02/temp_libpathca342b697fdb/energyRt/templates/report_costs.Rmd
#> 6            C:/Users/admin/AppData/Local/Temp/RtmpushJ02/temp_libpathca342b697fdb/energyRt/templates/report_demand.Rmd
#> 7            C:/Users/admin/AppData/Local/Temp/RtmpushJ02/temp_libpathca342b697fdb/energyRt/templates/report_export.Rmd
#> 8           C:/Users/admin/AppData/Local/Temp/RtmpushJ02/temp_libpathca342b697fdb/energyRt/templates/report_generic.Rmd
#> 9           C:/Users/admin/AppData/Local/Temp/RtmpushJ02/temp_libpathca342b697fdb/energyRt/templates/report_horizon.Rmd
#> 10           C:/Users/admin/AppData/Local/Temp/RtmpushJ02/temp_libpathca342b697fdb/energyRt/templates/report_import.Rmd
#> 11            C:/Users/admin/AppData/Local/Temp/RtmpushJ02/temp_libpathca342b697fdb/energyRt/templates/report_model.Rmd
#> 12       C:/Users/admin/AppData/Local/Temp/RtmpushJ02/temp_libpathca342b697fdb/energyRt/templates/report_model_full.Rmd
#> 13    C:/Users/admin/AppData/Local/Temp/RtmpushJ02/temp_libpathca342b697fdb/energyRt/templates/report_model_summary.Rmd
#> 14         C:/Users/admin/AppData/Local/Temp/RtmpushJ02/temp_libpathca342b697fdb/energyRt/templates/report_scenario.Rmd
#> 15    C:/Users/admin/AppData/Local/Temp/RtmpushJ02/temp_libpathca342b697fdb/energyRt/templates/report_scenario_full.Rmd
#> 16 C:/Users/admin/AppData/Local/Temp/RtmpushJ02/temp_libpathca342b697fdb/energyRt/templates/report_scenario_summary.Rmd
#> 17        C:/Users/admin/AppData/Local/Temp/RtmpushJ02/temp_libpathca342b697fdb/energyRt/templates/report_scenarios.Rmd
#> 18          C:/Users/admin/AppData/Local/Temp/RtmpushJ02/temp_libpathca342b697fdb/energyRt/templates/report_subsidy.Rmd
#> 19          C:/Users/admin/AppData/Local/Temp/RtmpushJ02/temp_libpathca342b697fdb/energyRt/templates/report_summary.Rmd
#> 20           C:/Users/admin/AppData/Local/Temp/RtmpushJ02/temp_libpathca342b697fdb/energyRt/templates/report_supply.Rmd
#> 21              C:/Users/admin/AppData/Local/Temp/RtmpushJ02/temp_libpathca342b697fdb/energyRt/templates/report_tax.Rmd
#> 22            C:/Users/admin/AppData/Local/Temp/RtmpushJ02/temp_libpathca342b697fdb/energyRt/templates/report_trade.Rmd
#> 23          C:/Users/admin/AppData/Local/Temp/RtmpushJ02/temp_libpathca342b697fdb/energyRt/templates/report_vehicle.Rmd
#> 24          C:/Users/admin/AppData/Local/Temp/RtmpushJ02/temp_libpathca342b697fdb/energyRt/templates/report_weather.Rmd
```
