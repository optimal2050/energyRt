# Package index

## Classes

Data structures of the key model components.

- [`class-constraint`](https://energyRt.org/reference/class-constraint.md)
  [`summand-class`](https://energyRt.org/reference/class-constraint.md)
  **\[experimental\]** : An S4 class to represent a custom constraint.
- [`storage-class`](https://energyRt.org/reference/class-storage.md) :
  An S4 class to represent storage type of technological process.
- [`subsidy-class`](https://energyRt.org/reference/class-subsidy.md) :
  An S4 class to represent a commodity subsidy
- [`tax-class`](https://energyRt.org/reference/class-tax.md) : An S4
  class to represent a commodity tax
- [`get_process_class()`](https://energyRt.org/reference/get_process_class.md)
  : Get class of processes
- [`supply-class`](https://energyRt.org/reference/supply-class.md) : An
  S4 class to represent a supply of a commodity
- [`class-calendar`](https://energyRt.org/reference/class-calendar.md) :
  An S4 class to represent sub-annual time resolution structure.
- [`class-commodity`](https://energyRt.org/reference/class-commodity.md)
  : An S4 class to represent a commodity
- [`class-config`](https://energyRt.org/reference/class-config.md) : An
  S4 class to represent default model configuration.
- [`class-costs`](https://energyRt.org/reference/class-costs.md)
  **\[experimental\]** : An S4 class to add costs to objective function
- [`class-demand`](https://energyRt.org/reference/class-demand.md) : An
  S4 class to declare a demand in the model
- [`class-export`](https://energyRt.org/reference/class-export.md) : An
  S4 class to represent commodity export to the rest of the world.
- [`class-horizon`](https://energyRt.org/reference/class-horizon.md) :
  An S4 class to represent model/scenario planning horizon with
  intervals (year-steps)
- [`class-import`](https://energyRt.org/reference/class-import.md) : An
  S4 class to represent commodity import from the rest of the world.
- [`class-model`](https://energyRt.org/reference/class-model.md)
  [`model`](https://energyRt.org/reference/class-model.md) : An S4 class
  to represent model
- [`class-modInp`](https://energyRt.org/reference/class-modInp.md) : An
  S4 class to represent model input
- [`class-modOut`](https://energyRt.org/reference/class-modOut.md) : An
  S4 class to store results of a solved scenario
- [`class-parameter`](https://energyRt.org/reference/class-parameter.md)
  : An S4 class to specify the model set or parameter
- [`class-repository`](https://energyRt.org/reference/class-repository.md)
  : An S4 class to store the model objects.
- [`class-scenario`](https://energyRt.org/reference/class-scenario.md)
  [`scenario`](https://energyRt.org/reference/class-scenario.md) : An S4
  class to represent scenario, an interpolated and/or solved model.
- [`class-settings`](https://energyRt.org/reference/class-settings.md) :
  An S4 class to represent scenario settings
- [`.upgrade_summand()`](https://energyRt.org/reference/class-summand.md)
  : An S4 class to represent a summand in a constraint.
- [`class-technology`](https://energyRt.org/reference/class-technology.md)
  : An S4 class to represent technology
- [`class-trade`](https://energyRt.org/reference/class-trade.md) : An S4
  class to represent inter-regional trade
- [`class-variable`](https://energyRt.org/reference/class-variable.md) :
  An S4 class to specify a model variable
- [`class-weather`](https://energyRt.org/reference/class-weather.md) :
  S4 class to represent weather factors

## Building a model

Functions to create model components.

- [`newACLine()`](https://energyRt.org/reference/newACLine.md)
  **\[experimental\]** : Create an AC transmission line
- [`newCalendar()`](https://energyRt.org/reference/newCalendar.md) :
  Generate a new calendar object from
- [`newCommodity()`](https://energyRt.org/reference/newCommodity.md) :
  Create new commodity object
- [`newConstraint()`](https://energyRt.org/reference/newConstraint.md)
  [`newConstraintS()`](https://energyRt.org/reference/newConstraint.md)
  : Create constraint object to add custom constraints to the model.
- [`newCosts()`](https://energyRt.org/reference/newCosts.md) : Create
  new costs object
- [`newDCLink()`](https://energyRt.org/reference/newDCLink.md)
  **\[experimental\]** : Create a DC transmission link
- [`newDemand()`](https://energyRt.org/reference/newDemand.md)
  [`update(`*`<demand>`*`)`](https://energyRt.org/reference/newDemand.md)
  : Create new demand object
- [`newExport()`](https://energyRt.org/reference/newExport.md) : Create
  new export object
- [`newHorizon()`](https://energyRt.org/reference/newHorizon.md)
  [`update(`*`<horizon>`*`)`](https://energyRt.org/reference/newHorizon.md)
  [`setHorizon(`*`<config>`*`)`](https://energyRt.org/reference/newHorizon.md)
  [`update(`*`<config>`*`)`](https://energyRt.org/reference/newHorizon.md)
  : Create a new object of class 'horizon'
- [`newImport()`](https://energyRt.org/reference/newImport.md) : Create
  new export object
- [`newModel()`](https://energyRt.org/reference/newModel.md)
  [`setHorizon(`*`<model>`*`)`](https://energyRt.org/reference/newModel.md)
  [`getHorizon(`*`<model>`*`)`](https://energyRt.org/reference/newModel.md)
  : Create new model object
- [`newRepository()`](https://energyRt.org/reference/newRepository.md) :
  A constructor for the repository class
- [`newScenario()`](https://energyRt.org/reference/newScenario.md) :
  Generate a new scenario object
- [`newSub()`](https://energyRt.org/reference/newSubsidy.md) : Create a
  new subsidy object
- [`newSupply()`](https://energyRt.org/reference/newSupply.md) :
  Constructor for supply object.
- [`newTax()`](https://energyRt.org/reference/newTax.md) : Create a new
  tax object
- [`newTrade()`](https://energyRt.org/reference/newTrade.md) : Create
  new trade object
- [`newWeather()`](https://energyRt.org/reference/newWeather.md) :
  Create new weather object
- [`newRegistry()`](https://energyRt.org/reference/registry.md)
  [`load_registry()`](https://energyRt.org/reference/registry.md)
  [`save_registry()`](https://energyRt.org/reference/registry.md)
  [`add_to_registry()`](https://energyRt.org/reference/registry.md)
  [`add(`*`<en_registry>`*`)`](https://energyRt.org/reference/registry.md)
  [`find_in_registry()`](https://energyRt.org/reference/registry.md)
  [`refresh_registry()`](https://energyRt.org/reference/registry.md) :
  Project registry of models, scenarios, and runs
- [`newStorage()`](https://energyRt.org/reference/storage.md) : Create
  new storage object
- [`newTechnology()`](https://energyRt.org/reference/technology.md)
  [`update(`*`<technology>`*`)`](https://energyRt.org/reference/technology.md)
  : Create a new "technology" object.
- [`make_timetable()`](https://energyRt.org/reference/calendar.md) :
  Create timetable of time-timeslices from given structure as a list
- [`update(`*`<export>`*`)`](https://energyRt.org/reference/newTechnology.md)
  [`update(`*`<weather>`*`)`](https://energyRt.org/reference/newTechnology.md)
  : Update export object
- [`update(`*`<supply>`*`)`](https://energyRt.org/reference/sypply.md) :
  Update supply object
- [`update(`*`<storage>`*`)`](https://energyRt.org/reference/update.md)
  [`update(`*`<trade>`*`)`](https://energyRt.org/reference/update.md)
  [`update(`*`<import>`*`)`](https://energyRt.org/reference/update.md) :
  Update trade object
- [`update_parameter()`](https://energyRt.org/reference/update_parameter.md)
  : Update parameter in the scenario by adding data to it
- [`add(`*`<repository>`*`)`](https://energyRt.org/reference/add.md)
  [`add(`*`<model>`*`)`](https://energyRt.org/reference/add.md) : Add an
  object to the model's repository
- [`convert(`*`<character>`*`)`](https://energyRt.org/reference/convert.md)
  [`convert(`*`<numeric>`*`)`](https://energyRt.org/reference/convert.md)
  [`add_to_convert(`*`<character>`*`,`*`<character>`*`,`*`<numeric>`*`)`](https://energyRt.org/reference/convert.md)
  : Convert units
- [`asSupplyCurve()`](https://energyRt.org/reference/supply-curve.md)
  [`asImportCurve()`](https://energyRt.org/reference/supply-curve.md)
  [`asExportCurve()`](https://energyRt.org/reference/supply-curve.md)
  **\[experimental\]** : Stepped supply, export and import curves
- [`lossTranches()`](https://energyRt.org/reference/lossTranches.md)
  **\[experimental\]** : Loss tranches for a transmission line

## Process specs

Portable process specifications (YAML/JSON containers) and vintage
tools.

- [`process_from_spec()`](https://energyRt.org/reference/process_from_spec.md)
  : Build a process object from a spec
- [`process_to_spec()`](https://energyRt.org/reference/process_to_spec.md)
  : Export a process object as a spec list
- [`process_spec_code()`](https://energyRt.org/reference/process_spec_code.md)
  : The constructor call equivalent to a process spec, as text
- [`process_spec_issues()`](https://energyRt.org/reference/process_spec_issues.md)
  : Validate a process spec and report issues
- [`read_process_spec()`](https://energyRt.org/reference/read_process_spec.md)
  : Read and validate a process spec
- [`process_designer()`](https://energyRt.org/reference/process_designer.md)
  : Launch the process designer
- [`combine_vintages()`](https://energyRt.org/reference/combine_vintages.md)
  : Combine per-vintage technology objects into one vintaged technology

## Configuration and Settings

Model configuration and scenario settings

- [`class-config`](https://energyRt.org/reference/class-config.md) : An
  S4 class to represent default model configuration.
- [`class-settings`](https://energyRt.org/reference/class-settings.md) :
  An S4 class to represent scenario settings
- [`compare_interp_settings()`](https://energyRt.org/reference/compare_interp_settings.md)
  : Compare interpolation settings (size & build time) for a model
- [`compare_solve_settings()`](https://energyRt.org/reference/compare_solve_settings.md)
  : Compare interpolation settings AND solvers for a model
- [`class-calendar`](https://energyRt.org/reference/class-calendar.md) :
  An S4 class to represent sub-annual time resolution structure.
- [`newCalendar()`](https://energyRt.org/reference/newCalendar.md) :
  Generate a new calendar object from
- [`autoplot(`*`<calendar>`*`)`](https://energyRt.org/reference/plot_calendar_method.md)
  : Visualize a Calendar object
- [`setGeoscale(`*`<config>`*`)`](https://energyRt.org/reference/setGeoscale.md)
  [`getGeoscale(`*`<config>`*`)`](https://energyRt.org/reference/setGeoscale.md)
  [`setGeoscale(`*`<model>`*`)`](https://energyRt.org/reference/setGeoscale.md)
  [`getGeoscale(`*`<model>`*`)`](https://energyRt.org/reference/setGeoscale.md)
  [`setGeoscale(`*`<scenario>`*`)`](https://energyRt.org/reference/setGeoscale.md)
  [`getGeoscale(`*`<scenario>`*`)`](https://energyRt.org/reference/setGeoscale.md)
  [`getCalendar(`*`<config>`*`)`](https://energyRt.org/reference/setGeoscale.md)
  [`getCalendar(`*`<model>`*`)`](https://energyRt.org/reference/setGeoscale.md)
  [`getCalendar(`*`<scenario>`*`)`](https://energyRt.org/reference/setGeoscale.md)
  : Attach or read a model's geoscale
- [`class-horizon`](https://energyRt.org/reference/class-horizon.md) :
  An S4 class to represent model/scenario planning horizon with
  intervals (year-steps)
- [`newHorizon()`](https://energyRt.org/reference/newHorizon.md)
  [`update(`*`<horizon>`*`)`](https://energyRt.org/reference/newHorizon.md)
  [`setHorizon(`*`<config>`*`)`](https://energyRt.org/reference/newHorizon.md)
  [`update(`*`<config>`*`)`](https://energyRt.org/reference/newHorizon.md)
  : Create a new object of class 'horizon'
- [`autoplot(`*`<horizon>`*`)`](https://energyRt.org/reference/plot_horizon_method.md)
  : Visualize a Horizon object
- [`calendars`](https://energyRt.org/reference/calendars.md) : Example
  calendars
- [`horizons`](https://energyRt.org/reference/horizons.md) : Example
  planning horizons

## Solving

- [`interpolate_model()`](https://energyRt.org/reference/interpolate_model.md)
  : Interpolate a model into a solver-ready scenario

- [`write_script()`](https://energyRt.org/reference/write.md)
  [`write_sc()`](https://energyRt.org/reference/write.md) : Write
  scenario object as a Python, Julia, GAMS, or MathProg script with data
  files to a directory

- [`write_technology_xlsx()`](https://energyRt.org/reference/write_technology_xlsx.md)
  :

  Write `technology` object(s) to an Excel workbook

- [`set_log_file()`](https://energyRt.org/reference/log.md)
  [`get_log_file()`](https://energyRt.org/reference/log.md)
  [`read_log()`](https://energyRt.org/reference/log.md) : Read the
  operation log

- [`read_solution()`](https://energyRt.org/reference/read.md)
  [`read(`*`<scenario>`*`)`](https://energyRt.org/reference/read.md) :
  Read solution

- [`read_process_spec()`](https://energyRt.org/reference/read_process_spec.md)
  : Read and validate a process spec

- [`read_technology_xlsx()`](https://energyRt.org/reference/read_technology_xlsx.md)
  :

  Read `technology` object(s) from an Excel workbook

- [`solve_model()`](https://energyRt.org/reference/solve_model.md)
  [`solve_scenario()`](https://energyRt.org/reference/solve_model.md) :
  Solve a model or an interpolated scenario

- [`solve_myopic()`](https://energyRt.org/reference/solve_myopic.md) :
  Solve a model sequentially (myopic foresight)

- [`subset_model_regions()`](https://energyRt.org/reference/subset_model_regions.md)
  : Restrict a model to a region subset

- [`with_solver_log()`](https://energyRt.org/reference/with_solver_log.md)
  : Send the solver's progress log to a file

- [`horizon_windows()`](https://energyRt.org/reference/horizon_windows.md)
  : Split a horizon into sequential solve windows

- [`solution_ledger()`](https://energyRt.org/reference/solution_ledger.md)
  : Extract the carry-forward ledger from a solved scenario

- [`apply_ledger()`](https://energyRt.org/reference/apply_ledger.md) :
  Apply a carry ledger to a model

- [`myopic_objective()`](https://energyRt.org/reference/myopic_objective.md)
  : Globally discounted total cost of a myopic sequence

- [`trim_parameters_by_maps()`](https://energyRt.org/reference/trim_parameters_by_maps.md)
  : Trim numeric/bounds parameters to the domain of the maps that index
  them

- [`validate_scenario_parameters()`](https://energyRt.org/reference/validate_scenario_parameters.md)
  : Validate interpolated scenario parameters

## Remote solver (NEOS)

- [`neos_ping()`](https://energyRt.org/reference/neos.md)
  [`neos_list_solvers()`](https://energyRt.org/reference/neos.md) : NEOS
  Server client (query the remote solver service)

## Reporting

- [`report()`](https://energyRt.org/reference/report.md) : Generate a
  summary report for an energyRt object
- [`report_docx()`](https://energyRt.org/reference/report_docx.md) :
  Generate a Word report for an energyRt object
- [`report_output()`](https://energyRt.org/reference/report_helpers.md)
  [`report_setup()`](https://energyRt.org/reference/report_helpers.md)
  [`report_layout()`](https://energyRt.org/reference/report_helpers.md)
  [`report_esc()`](https://energyRt.org/reference/report_helpers.md)
  [`report_fmt_val()`](https://energyRt.org/reference/report_helpers.md)
  [`report_img()`](https://energyRt.org/reference/report_helpers.md)
  [`report_img_row()`](https://energyRt.org/reference/report_helpers.md)
  [`report_pagebreak()`](https://energyRt.org/reference/report_helpers.md)
  [`report_plot_png()`](https://energyRt.org/reference/report_helpers.md)
  [`report_sec()`](https://energyRt.org/reference/report_helpers.md)
  [`report_tbl()`](https://energyRt.org/reference/report_helpers.md)
  [`report_css()`](https://energyRt.org/reference/report_helpers.md)
  [`report_header()`](https://energyRt.org/reference/report_helpers.md)
  : Report template helpers
- [`report_html()`](https://energyRt.org/reference/report_html.md) :
  Generate an HTML report for an energyRt object
- [`report_pdf()`](https://energyRt.org/reference/report_pdf.md) :
  Generate a PDF report for an energyRt object
- [`report_templates()`](https://energyRt.org/reference/report_templates.md)
  : List the available report templates
- [`report_tex()`](https://energyRt.org/reference/report_tex.md) :
  Generate a LaTeX report for an energyRt object

## Comparison

Compare scenarios’ solved results (or runs of one scenario), models’
declarations, and interpolated model inputs.

- [`compare_inputs()`](https://energyRt.org/reference/compare_inputs.md)
  [`print(`*`<inputs_cmp>`*`)`](https://energyRt.org/reference/compare_inputs.md)
  : Compare two scenarios' model inputs
- [`compare_interp_settings()`](https://energyRt.org/reference/compare_interp_settings.md)
  : Compare interpolation settings (size & build time) for a model
- [`compare_models()`](https://energyRt.org/reference/compare_models.md)
  [`print(`*`<models_cmp>`*`)`](https://energyRt.org/reference/compare_models.md)
  : Compare two models' contents
- [`compare_scenarios()`](https://energyRt.org/reference/compare_scenarios.md)
  [`print(`*`<scenarios_cmp>`*`)`](https://energyRt.org/reference/compare_scenarios.md)
  [`autoplot(`*`<scenarios_cmp>`*`)`](https://energyRt.org/reference/compare_scenarios.md)
  [`plot(`*`<scenarios_cmp>`*`)`](https://energyRt.org/reference/compare_scenarios.md)
  : Compare solved scenarios (or runs of one scenario)
- [`compare_solve_settings()`](https://energyRt.org/reference/compare_solve_settings.md)
  : Compare interpolation settings AND solvers for a model

## Example datasets

Data bundled with the package for the examples and articles. `calendars`
and `horizons` are listed under Configuration above.

- [`vre_cf`](https://energyRt.org/reference/vre_cf.md) : Hourly wind and
  solar capacity factors, one site, one year
- [`vre_storage_duration`](https://energyRt.org/reference/vre_storage_duration.md)
  : Duration bands of a full-year battery, precomputed

## Data manipulation

- [`convert(`*`<character>`*`)`](https://energyRt.org/reference/convert.md)
  [`convert(`*`<numeric>`*`)`](https://energyRt.org/reference/convert.md)
  [`add_to_convert(`*`<character>`*`,`*`<character>`*`,`*`<numeric>`*`)`](https://energyRt.org/reference/convert.md)
  : Convert units

- [`convert_data`](https://energyRt.org/reference/convert_data.md) :

  Basic units conversion database for `convert` methods

- [`getUnits(`*`<commodity>`*`)`](https://energyRt.org/reference/getUnits.md)
  [`getUnits(`*`<technology>`*`)`](https://energyRt.org/reference/getUnits.md)
  : Get parameter units for energyRt class objects

- [`commodity_properties()`](https://energyRt.org/reference/commodity_properties.md)
  : Recognised commodity properties

- [`commodity_property()`](https://energyRt.org/reference/commodity_property.md)
  : Physical property of a commodity

- [`dataset_hash()`](https://energyRt.org/reference/dataset_store.md)
  [`save_dataset()`](https://energyRt.org/reference/dataset_store.md)
  [`load_dataset()`](https://energyRt.org/reference/dataset_store.md) :
  Content hash of a dataset payload

- [`getData()`](https://energyRt.org/reference/getData.md) : Extract
  data from energyRt objects

- [`set_registry_file()`](https://energyRt.org/reference/registry_file.md)
  [`get_registry_file()`](https://energyRt.org/reference/registry_file.md)
  [`set_models_path()`](https://energyRt.org/reference/registry_file.md)
  [`get_models_path()`](https://energyRt.org/reference/registry_file.md)
  [`set_repositories_path()`](https://energyRt.org/reference/registry_file.md)
  [`get_repositories_path()`](https://energyRt.org/reference/registry_file.md)
  [`set_datasets_path()`](https://energyRt.org/reference/registry_file.md)
  [`get_datasets_path()`](https://energyRt.org/reference/registry_file.md)
  : Registry file and model store locations

- [`seal_model()`](https://energyRt.org/reference/seal.md)
  [`unseal_model()`](https://energyRt.org/reference/seal.md)
  [`seal_repository()`](https://energyRt.org/reference/seal.md)
  [`unseal_repository()`](https://energyRt.org/reference/seal.md)
  [`seal_dataset()`](https://energyRt.org/reference/seal.md)
  [`unseal_dataset()`](https://energyRt.org/reference/seal.md)
  [`seal_scenario()`](https://energyRt.org/reference/seal.md)
  [`unseal_scenario()`](https://energyRt.org/reference/seal.md)
  [`mark_delete()`](https://energyRt.org/reference/seal.md)
  [`unmark_delete()`](https://energyRt.org/reference/seal.md)
  [`delete_marked()`](https://energyRt.org/reference/seal.md) : Seal and
  unseal store entries; mark them for deletion

- [`getObject()`](https://energyRt.org/reference/getObject.md) :
  Retrieve model objects from a repository, model or scenario

- [`getScenario()`](https://energyRt.org/reference/accessors.md)
  [`getModel()`](https://energyRt.org/reference/accessors.md)
  [`getRepository()`](https://energyRt.org/reference/accessors.md)
  [`getDataset()`](https://energyRt.org/reference/accessors.md)
  [`open_project()`](https://energyRt.org/reference/accessors.md) :
  Access registered scenarios, models, repositories, and datasets by
  name

- [`getMix()`](https://energyRt.org/reference/getMix.md) : Extract a
  tidy mix (generation, capacity, fuel) from a solved scenario

- [`getVariants()`](https://energyRt.org/reference/getVariants.md) :
  Process variants of a solved (or interpolated) scenario

- [`storage_duration()`](https://energyRt.org/reference/storage_duration.md)
  : Split a storage level into duration bands

- [`variantSummary()`](https://energyRt.org/reference/variantSummary.md)
  : Summarise a result variable across technology variants

- [`findData()`](https://energyRt.org/reference/findData.md) :

  Performs search for available data in *scenario* object.

- [`findDuplicates()`](https://energyRt.org/reference/findDuplicates.md)
  : Function to find duplicated values in interpolated scenario.

- [`find_in_model()`](https://energyRt.org/reference/find_in_model.md) :
  Find where value(s) are stored across a model's objects

- [`newRegistry()`](https://energyRt.org/reference/registry.md)
  [`load_registry()`](https://energyRt.org/reference/registry.md)
  [`save_registry()`](https://energyRt.org/reference/registry.md)
  [`add_to_registry()`](https://energyRt.org/reference/registry.md)
  [`add(`*`<en_registry>`*`)`](https://energyRt.org/reference/registry.md)
  [`find_in_registry()`](https://energyRt.org/reference/registry.md)
  [`refresh_registry()`](https://energyRt.org/reference/registry.md) :
  Project registry of models, scenarios, and runs

- [`renameSets()`](https://energyRt.org/reference/renameSets.md) :
  Rename data.frame columns of list of data.frames.

- [`revalueSets()`](https://energyRt.org/reference/revalueSets.md) :
  Replace specified values with new values in factor or character
  columns of a data.frame.

- [`model_size()`](https://energyRt.org/reference/model_size.md) : Size
  estimate of an interpolated scenario and the result of folding

- [`size()`](https://energyRt.org/reference/size.md) : Size of an object

- [`hour2HOUR()`](https://energyRt.org/reference/hour2HOUR.md) : Convert
  hours (integer) values to HOUR set 'hNN'

- [`yday2YDAY()`](https://energyRt.org/reference/yday2YDAY.md) : Convert
  year-days to YDAY set 'dNNN'

- [`tsl_formats`](https://energyRt.org/reference/timeslices.md)
  [`tsl_sets`](https://energyRt.org/reference/timeslices.md)
  [`dtm2tsl()`](https://energyRt.org/reference/timeslices.md)
  [`tsl2dtm()`](https://energyRt.org/reference/timeslices.md) : Common
  formats of time-timeslices.

- [`tsl2year()`](https://energyRt.org/reference/tsl2dtm.md)
  [`tsl2yday()`](https://energyRt.org/reference/tsl2dtm.md)
  [`tsl2hour()`](https://energyRt.org/reference/tsl2dtm.md)
  [`tsl2month()`](https://energyRt.org/reference/tsl2dtm.md) : Mapping
  function between time-timeslices and day of the year

- [`tsl_guess_format()`](https://energyRt.org/reference/tsl_guess_format.md)
  : Guess format of time-timeslices

- [`obj2mem()`](https://energyRt.org/reference/obj2mem.md) : Loads
  objects from disk to memory

- [`isInMemory()`](https://energyRt.org/reference/isInMemory.md) : Is
  object stored in memory?

- [`isOnDisk()`](https://energyRt.org/reference/isOnDisk.md) : Is object
  stored on disk?

- [`save_model()`](https://energyRt.org/reference/model_store.md)
  [`load_model()`](https://energyRt.org/reference/model_store.md) : Save
  a model to / load a model from the model store

- [`save_repository()`](https://energyRt.org/reference/repo_store.md)
  [`load_repository()`](https://energyRt.org/reference/repo_store.md) :
  Save a repository to / load a repository from the repository store

- [`save_scenario()`](https://energyRt.org/reference/save_scenario.md) :

  Save scenario object on disk in parquet format using `arrow` package.

- [`load_scenario()`](https://energyRt.org/reference/load_scenario.md)
  [`load_scenarios()`](https://energyRt.org/reference/load_scenario.md)
  : Load scenario (in progress)

- [`model_hash()`](https://energyRt.org/reference/model_hash.md)
  [`object_hash()`](https://energyRt.org/reference/model_hash.md) :
  Content hash of a model

- [`repository_hash()`](https://energyRt.org/reference/repository_hash.md)
  : Content hash of a repository

- [`scenario_runs()`](https://energyRt.org/reference/scenario_runs.md)
  [`scenario_run_info()`](https://energyRt.org/reference/scenario_runs.md)
  [`drop_scenario_run()`](https://energyRt.org/reference/scenario_runs.md)
  : List, inspect, and drop a scenario's runs

- [`upgrade_scenario_layout()`](https://energyRt.org/reference/upgrade_scenario_layout.md)
  : Upgrade a scenario folder to the current on-disk layout

- [`clear_levcost_cache()`](https://energyRt.org/reference/clear_levcost_cache.md)
  : Clear cached levcost results

- [`utopia`](https://energyRt.org/reference/utopia.md) : The UTOPIA
  reference dataset

- [`utopia_geoscale()`](https://energyRt.org/reference/utopia_geoscale.md)
  : A geoscale for the UTOPIA reference model

- [`utopia_profile()`](https://energyRt.org/reference/utopia_profile.md)
  : Synthetic input shapes on a calendar (deterministic)

- [`utopia_profiles()`](https://energyRt.org/reference/utopia_profiles.md)
  : UTOPIA input profiles (deterministic)

## Visualisation

- [`draw()`](https://energyRt.org/reference/draw.md)
  [`draw.technology()`](https://energyRt.org/reference/draw.md) : Draw a
  schematic representation of a process
- [`autoplot(`*`<commodity>`*`)`](https://energyRt.org/reference/autoplot.commodity.md)
  : Visualize a Commodity object
- [`autoplot(`*`<tax>`*`)`](https://energyRt.org/reference/autoplot.lever.md)
  [`autoplot(`*`<subsidy>`*`)`](https://energyRt.org/reference/autoplot.lever.md)
  [`autoplot(`*`<constraint>`*`)`](https://energyRt.org/reference/autoplot.lever.md)
  : Plot a tax, subsidy or user constraint over years
- [`autoplot(`*`<supply>`*`)`](https://energyRt.org/reference/autoplot.process.md)
  [`autoplot(`*`<import>`*`)`](https://energyRt.org/reference/autoplot.process.md)
  [`autoplot(`*`<export>`*`)`](https://energyRt.org/reference/autoplot.process.md)
  [`autoplot(`*`<technology>`*`)`](https://energyRt.org/reference/autoplot.process.md)
  [`autoplot(`*`<storage>`*`)`](https://energyRt.org/reference/autoplot.process.md)
  : Visualize a process object over years
- [`autoplot(`*`<scenario>`*`)`](https://energyRt.org/reference/autoplot.scenario.md)
  : Plot mixes from a solved scenario
- [`compare_scenarios()`](https://energyRt.org/reference/compare_scenarios.md)
  [`print(`*`<scenarios_cmp>`*`)`](https://energyRt.org/reference/compare_scenarios.md)
  [`autoplot(`*`<scenarios_cmp>`*`)`](https://energyRt.org/reference/compare_scenarios.md)
  [`plot(`*`<scenarios_cmp>`*`)`](https://energyRt.org/reference/compare_scenarios.md)
  : Compare solved scenarios (or runs of one scenario)
- [`autoplot(`*`<calendar>`*`)`](https://energyRt.org/reference/plot_calendar_method.md)
  : Visualize a Calendar object
- [`plot_geoscale()`](https://energyRt.org/reference/plot_geoscale.md) :
  Draw a model's geoscale
- [`plot_heatmap()`](https://energyRt.org/reference/plot_heatmap.md) :
  Heatmap of timeslice-indexed values over a calendar
- [`autoplot(`*`<horizon>`*`)`](https://energyRt.org/reference/plot_horizon_method.md)
  : Visualize a Horizon object
- [`plot_map()`](https://energyRt.org/reference/plot_map.md) : Map model
  results by region
- [`plot_share_frontier()`](https://energyRt.org/reference/plot_share_frontier.md)
  : Plot feasible share-mix diagram for technology inputs/outputs
- [`plot_trade_map()`](https://energyRt.org/reference/plot_trade_map.md)
  [`autoplot(`*`<trade>`*`)`](https://energyRt.org/reference/plot_trade_map.md)
  : Map inter-regional trade routes
- [`report_output()`](https://energyRt.org/reference/report_helpers.md)
  [`report_setup()`](https://energyRt.org/reference/report_helpers.md)
  [`report_layout()`](https://energyRt.org/reference/report_helpers.md)
  [`report_esc()`](https://energyRt.org/reference/report_helpers.md)
  [`report_fmt_val()`](https://energyRt.org/reference/report_helpers.md)
  [`report_img()`](https://energyRt.org/reference/report_helpers.md)
  [`report_img_row()`](https://energyRt.org/reference/report_helpers.md)
  [`report_pagebreak()`](https://energyRt.org/reference/report_helpers.md)
  [`report_plot_png()`](https://energyRt.org/reference/report_helpers.md)
  [`report_sec()`](https://energyRt.org/reference/report_helpers.md)
  [`report_tbl()`](https://energyRt.org/reference/report_helpers.md)
  [`report_css()`](https://energyRt.org/reference/report_helpers.md)
  [`report_header()`](https://energyRt.org/reference/report_helpers.md)
  : Report template helpers
- [`en_config_show()`](https://energyRt.org/reference/en_config_show.md)
  : Show the effective energyRt configuration
- [`set_progress_bar()`](https://energyRt.org/reference/progress.md)
  [`show_progress_bar()`](https://energyRt.org/reference/progress.md) :
  Switch on/off and select/customize progress bar
- [`design()`](https://energyRt.org/reference/design.md) : Generate an R
  script that recreates an energyRt object
- [`process_designer()`](https://energyRt.org/reference/process_designer.md)
  : Launch the process designer
- [`object_image()`](https://energyRt.org/reference/object_image.md) :
  Image or icon attached to a model object
- [`ghost_options()`](https://energyRt.org/reference/ghost_options.md) :
  Geometry of the faded "ghost" vintages behind a drawn technology
- [`theme_energyRt()`](https://energyRt.org/reference/theme_energyRt.md)
  : energyRt's default ggplot theme

## Validation

- [`check_geoscale_regions()`](https://energyRt.org/reference/check_geoscale_regions.md)
  : Check a geoscale against the model's declared regions
- [`verify_solution()`](https://energyRt.org/reference/verify_solution.md)
  : Verify accounting identities of a solved scenario
- [`clear_levcost_cache()`](https://energyRt.org/reference/clear_levcost_cache.md)
  : Clear cached levcost results
- [`levcost()`](https://energyRt.org/reference/levcost.md) : Levelized
  cost of commodity production
- [`set_reports_path()`](https://energyRt.org/reference/reports_path.md)
  [`get_reports_path()`](https://energyRt.org/reference/reports_path.md)
  [`set_levcost_cache_path()`](https://energyRt.org/reference/reports_path.md)
  [`get_levcost_cache_path()`](https://energyRt.org/reference/reports_path.md)
  : Report and levcost output locations
- [`tech_share_frontier()`](https://energyRt.org/reference/tech_share_frontier.md)
  : Extract feasible share ranges for grouped inputs/outputs

## Exporting

- [`compare_inputs()`](https://energyRt.org/reference/compare_inputs.md)
  [`print(`*`<inputs_cmp>`*`)`](https://energyRt.org/reference/compare_inputs.md)
  : Compare two scenarios' model inputs
- [`compare_models()`](https://energyRt.org/reference/compare_models.md)
  [`print(`*`<models_cmp>`*`)`](https://energyRt.org/reference/compare_models.md)
  : Compare two models' contents
- [`compare_scenarios()`](https://energyRt.org/reference/compare_scenarios.md)
  [`print(`*`<scenarios_cmp>`*`)`](https://energyRt.org/reference/compare_scenarios.md)
  [`autoplot(`*`<scenarios_cmp>`*`)`](https://energyRt.org/reference/compare_scenarios.md)
  [`plot(`*`<scenarios_cmp>`*`)`](https://energyRt.org/reference/compare_scenarios.md)
  : Compare solved scenarios (or runs of one scenario)
- [`print()`](https://energyRt.org/reference/print.md) : Print methods
  for the energyRt classes
- [`summary(`*`<model>`*`)`](https://energyRt.org/reference/summary.md)
  [`summary(`*`<scenario>`*`)`](https://energyRt.org/reference/summary.md)
  : Summarize a model or scenario

## energyRt options and settings

- [`get_default_solver()`](https://energyRt.org/reference/default_solver.md)
  [`set_default_solver()`](https://energyRt.org/reference/default_solver.md)
  : Default solver
- [`set_option()`](https://energyRt.org/reference/en_option.md)
  [`get_option()`](https://energyRt.org/reference/en_option.md) : Get or
  set an energyRt option
- [`get_exchange_format()`](https://energyRt.org/reference/exchange_format.md)
  [`set_exchange_format()`](https://energyRt.org/reference/exchange_format.md)
  [`get_exchange_compression()`](https://energyRt.org/reference/exchange_format.md)
  [`set_exchange_compression()`](https://energyRt.org/reference/exchange_format.md)
  [`get_exchange_compression_level()`](https://energyRt.org/reference/exchange_format.md)
  [`set_exchange_compression_level()`](https://energyRt.org/reference/exchange_format.md)
  : Solver data-exchange options
- [`set_log_file()`](https://energyRt.org/reference/log.md)
  [`get_log_file()`](https://energyRt.org/reference/log.md)
  [`read_log()`](https://energyRt.org/reference/log.md) : Read the
  operation log
- [`get_neos_email()`](https://energyRt.org/reference/neos_email.md)
  [`set_neos_email()`](https://energyRt.org/reference/neos_email.md) :
  NEOS submission email
- [`newHorizon()`](https://energyRt.org/reference/newHorizon.md)
  [`update(`*`<horizon>`*`)`](https://energyRt.org/reference/newHorizon.md)
  [`setHorizon(`*`<config>`*`)`](https://energyRt.org/reference/newHorizon.md)
  [`update(`*`<config>`*`)`](https://energyRt.org/reference/newHorizon.md)
  : Create a new object of class 'horizon'
- [`newModel()`](https://energyRt.org/reference/newModel.md)
  [`setHorizon(`*`<model>`*`)`](https://energyRt.org/reference/newModel.md)
  [`getHorizon(`*`<model>`*`)`](https://energyRt.org/reference/newModel.md)
  : Create new model object
- [`set_path_builder()`](https://energyRt.org/reference/path_builders.md)
  [`get_path_builder()`](https://energyRt.org/reference/path_builders.md)
  : Override how folder names are derived
- [`set_progress_bar()`](https://energyRt.org/reference/progress.md)
  [`show_progress_bar()`](https://energyRt.org/reference/progress.md) :
  Switch on/off and select/customize progress bar
- [`set_registry_file()`](https://energyRt.org/reference/registry_file.md)
  [`get_registry_file()`](https://energyRt.org/reference/registry_file.md)
  [`set_models_path()`](https://energyRt.org/reference/registry_file.md)
  [`get_models_path()`](https://energyRt.org/reference/registry_file.md)
  [`set_repositories_path()`](https://energyRt.org/reference/registry_file.md)
  [`get_repositories_path()`](https://energyRt.org/reference/registry_file.md)
  [`set_datasets_path()`](https://energyRt.org/reference/registry_file.md)
  [`get_datasets_path()`](https://energyRt.org/reference/registry_file.md)
  : Registry file and model store locations
- [`set_reports_path()`](https://energyRt.org/reference/reports_path.md)
  [`get_reports_path()`](https://energyRt.org/reference/reports_path.md)
  [`set_levcost_cache_path()`](https://energyRt.org/reference/reports_path.md)
  [`get_levcost_cache_path()`](https://energyRt.org/reference/reports_path.md)
  : Report and levcost output locations
- [`set_scenarios_path()`](https://energyRt.org/reference/scenarios_path.md)
  [`get_scenarios_path()`](https://energyRt.org/reference/scenarios_path.md)
  : Set or get the directory with scenarios
- [`setGeoscale(`*`<config>`*`)`](https://energyRt.org/reference/setGeoscale.md)
  [`getGeoscale(`*`<config>`*`)`](https://energyRt.org/reference/setGeoscale.md)
  [`setGeoscale(`*`<model>`*`)`](https://energyRt.org/reference/setGeoscale.md)
  [`getGeoscale(`*`<model>`*`)`](https://energyRt.org/reference/setGeoscale.md)
  [`setGeoscale(`*`<scenario>`*`)`](https://energyRt.org/reference/setGeoscale.md)
  [`getGeoscale(`*`<scenario>`*`)`](https://energyRt.org/reference/setGeoscale.md)
  [`getCalendar(`*`<config>`*`)`](https://energyRt.org/reference/setGeoscale.md)
  [`getCalendar(`*`<model>`*`)`](https://energyRt.org/reference/setGeoscale.md)
  [`getCalendar(`*`<scenario>`*`)`](https://energyRt.org/reference/setGeoscale.md)
  : Attach or read a model's geoscale
- [`set_gams_path()`](https://energyRt.org/reference/solver.md)
  [`get_gams_path()`](https://energyRt.org/reference/solver.md)
  [`set_gdxlib_path()`](https://energyRt.org/reference/solver.md)
  [`get_gdxlib_path()`](https://energyRt.org/reference/solver.md)
  [`set_glpk_path()`](https://energyRt.org/reference/solver.md)
  [`get_glpk_path()`](https://energyRt.org/reference/solver.md)
  [`set_julia_path()`](https://energyRt.org/reference/solver.md)
  [`get_julia_path()`](https://energyRt.org/reference/solver.md)
  [`set_python_path()`](https://energyRt.org/reference/solver.md)
  [`get_python_path()`](https://energyRt.org/reference/solver.md) : Set
  GAMS and GDX library directory
- [`set_solver_path()`](https://energyRt.org/reference/solver_path.md)
  [`get_solver_path()`](https://energyRt.org/reference/solver_path.md) :
  Set or get the path to a solver toolchain
- [`get_storage_format()`](https://energyRt.org/reference/storage_format.md)
  [`set_storage_format()`](https://energyRt.org/reference/storage_format.md)
  [`get_storage_compression()`](https://energyRt.org/reference/storage_format.md)
  [`set_storage_compression()`](https://energyRt.org/reference/storage_format.md)
  [`get_storage_compression_level()`](https://energyRt.org/reference/storage_format.md)
  [`set_storage_compression_level()`](https://energyRt.org/reference/storage_format.md)
  : On-disk table storage options
- [`energyRt-options`](https://energyRt.org/reference/energyRt-options.md)
  : energyRt Options
- [`en_config_path()`](https://energyRt.org/reference/en_config.md)
  [`en_config_read()`](https://energyRt.org/reference/en_config.md)
  [`en_config_write()`](https://energyRt.org/reference/en_config.md) :
  The energyRt configuration file
- [`en_config_show()`](https://energyRt.org/reference/en_config_show.md)
  : Show the effective energyRt configuration
- [`isVerbose()`](https://energyRt.org/reference/isVerbose.md)
  [`isDebug()`](https://energyRt.org/reference/isVerbose.md) : Verbosity
  and debug level

## utils

- [`en_install_deps()`](https://energyRt.org/reference/en_install_deps.md)
  : Install the energyRt dependency library layer
- [`en_install_julia_pkgs()`](https://energyRt.org/reference/en_install_julia_pkgs.md)
  : Install Julia packages
- [`en_install_python_deps()`](https://energyRt.org/reference/en_install_python_deps.md)
  : Install Python/Pyomo dependencies
- [`en_setup()`](https://energyRt.org/reference/en_setup.md) : Set up
  energyRt after installation
- [`en_check_glpk()`](https://energyRt.org/reference/en_check.md)
  [`en_check_julia()`](https://energyRt.org/reference/en_check.md)
  [`en_check_python()`](https://energyRt.org/reference/en_check.md)
  [`en_check_pyomo()`](https://energyRt.org/reference/en_check.md)
  [`en_check_gams()`](https://energyRt.org/reference/en_check.md)
  [`en_check_gdx()`](https://energyRt.org/reference/en_check.md)
  [`en_check_julia_pkgs()`](https://energyRt.org/reference/en_check.md)
  [`en_check_dependencies()`](https://energyRt.org/reference/en_check.md)
  : Detect external solver software and dependencies
- [`en_check_packages()`](https://energyRt.org/reference/en_check_packages.md)
  : Check R package and external-tool dependencies
- [`get_default_solver()`](https://energyRt.org/reference/default_solver.md)
  [`set_default_solver()`](https://energyRt.org/reference/default_solver.md)
  : Default solver
- [`set_option()`](https://energyRt.org/reference/en_option.md)
  [`get_option()`](https://energyRt.org/reference/en_option.md) : Get or
  set an energyRt option
- [`get_exchange_format()`](https://energyRt.org/reference/exchange_format.md)
  [`set_exchange_format()`](https://energyRt.org/reference/exchange_format.md)
  [`get_exchange_compression()`](https://energyRt.org/reference/exchange_format.md)
  [`set_exchange_compression()`](https://energyRt.org/reference/exchange_format.md)
  [`get_exchange_compression_level()`](https://energyRt.org/reference/exchange_format.md)
  [`set_exchange_compression_level()`](https://energyRt.org/reference/exchange_format.md)
  : Solver data-exchange options
- [`getData()`](https://energyRt.org/reference/getData.md) : Extract
  data from energyRt objects
- [`get_default_value()`](https://energyRt.org/reference/get_default_value.md)
  : Return default value for one, several, or all parameters
- [`get_process_aux()`](https://energyRt.org/reference/get_process_aux.md)
  : Get auxiliary commodities for each process
- [`get_process_class()`](https://energyRt.org/reference/get_process_class.md)
  : Get class of processes
- [`get_process_comm()`](https://energyRt.org/reference/get_process_comm.md)
  : Commodities associated with a processes
- [`get_process_inputs()`](https://energyRt.org/reference/get_process_inputs.md)
  : Get inputs of processes
- [`get_process_invest_window()`](https://energyRt.org/reference/get_process_invest_window.md)
  : Start and end years for process by region
- [`get_process_outputs()`](https://energyRt.org/reference/get_process_outputs.md)
  : Get output commodities for each process
- [`get_process_region()`](https://energyRt.org/reference/get_process_region.md)
  : Get regions for each process
- [`get_process_stored()`](https://energyRt.org/reference/get_process_stored.md)
  : Stored commodities of each process
- [`get_process_timeframe()`](https://energyRt.org/reference/get_process_timeframe.md)
  : Get operational timeframe of processes
- [`get_region()`](https://energyRt.org/reference/get_region.md) :
  Collect the regions an object operates in
- [`get_variable()`](https://energyRt.org/reference/get_variable.md) :
  Get one solved variable from a scenario
- [`get_weather()`](https://energyRt.org/reference/get_weather.md) :
  Collect the weather objects an object refers to
- [`set_log_file()`](https://energyRt.org/reference/log.md)
  [`get_log_file()`](https://energyRt.org/reference/log.md)
  [`read_log()`](https://energyRt.org/reference/log.md) : Read the
  operation log
- [`get_neos_email()`](https://energyRt.org/reference/neos_email.md)
  [`set_neos_email()`](https://energyRt.org/reference/neos_email.md) :
  NEOS submission email
- [`set_path_builder()`](https://energyRt.org/reference/path_builders.md)
  [`get_path_builder()`](https://energyRt.org/reference/path_builders.md)
  : Override how folder names are derived
- [`set_registry_file()`](https://energyRt.org/reference/registry_file.md)
  [`get_registry_file()`](https://energyRt.org/reference/registry_file.md)
  [`set_models_path()`](https://energyRt.org/reference/registry_file.md)
  [`get_models_path()`](https://energyRt.org/reference/registry_file.md)
  [`set_repositories_path()`](https://energyRt.org/reference/registry_file.md)
  [`get_repositories_path()`](https://energyRt.org/reference/registry_file.md)
  [`set_datasets_path()`](https://energyRt.org/reference/registry_file.md)
  [`get_datasets_path()`](https://energyRt.org/reference/registry_file.md)
  : Registry file and model store locations
- [`set_reports_path()`](https://energyRt.org/reference/reports_path.md)
  [`get_reports_path()`](https://energyRt.org/reference/reports_path.md)
  [`set_levcost_cache_path()`](https://energyRt.org/reference/reports_path.md)
  [`get_levcost_cache_path()`](https://energyRt.org/reference/reports_path.md)
  : Report and levcost output locations
- [`set_scenarios_path()`](https://energyRt.org/reference/scenarios_path.md)
  [`get_scenarios_path()`](https://energyRt.org/reference/scenarios_path.md)
  : Set or get the directory with scenarios
- [`set_gams_path()`](https://energyRt.org/reference/solver.md)
  [`get_gams_path()`](https://energyRt.org/reference/solver.md)
  [`set_gdxlib_path()`](https://energyRt.org/reference/solver.md)
  [`get_gdxlib_path()`](https://energyRt.org/reference/solver.md)
  [`set_glpk_path()`](https://energyRt.org/reference/solver.md)
  [`get_glpk_path()`](https://energyRt.org/reference/solver.md)
  [`set_julia_path()`](https://energyRt.org/reference/solver.md)
  [`get_julia_path()`](https://energyRt.org/reference/solver.md)
  [`set_python_path()`](https://energyRt.org/reference/solver.md)
  [`get_python_path()`](https://energyRt.org/reference/solver.md) : Set
  GAMS and GDX library directory
- [`set_solver_path()`](https://energyRt.org/reference/solver_path.md)
  [`get_solver_path()`](https://energyRt.org/reference/solver_path.md) :
  Set or get the path to a solver toolchain
- [`get_storage_format()`](https://energyRt.org/reference/storage_format.md)
  [`set_storage_format()`](https://energyRt.org/reference/storage_format.md)
  [`get_storage_compression()`](https://energyRt.org/reference/storage_format.md)
  [`set_storage_compression()`](https://energyRt.org/reference/storage_format.md)
  [`get_storage_compression_level()`](https://energyRt.org/reference/storage_format.md)
  [`set_storage_compression_level()`](https://energyRt.org/reference/storage_format.md)
  : On-disk table storage options
