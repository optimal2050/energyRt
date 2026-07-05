# Changelog

## energyRt 0.50.9-dev

**New features & critical changes in the model code:**

- Weighting of time-slices in a subset has been revised and rewritten.
  Since this version, all variables with ‘slice’ dimension are not
  weighed for consistency of slice-level operation across sampled and
  non-sampled model runs.

- Slice-weights can vary across model years.

- All variables with ‘year’ dimension are not weighted to the interval
  lengths.   
  Exception:

  - cumulative variables (`vSupReserveCum`, etc.) which have to account
    the interval length for every milestone year. 

  - capacity variables (`vTechCap`, etc.) represent the state of the
    variable by the end of the period, including any accumulation or
    retirement of capacity over the interval of the milestone year.

- New capacity variables (`vTechNewCap`, etc.) are given for a year.
  Period length (`pPeriodLen`) must be applied to the annual capacity
  additions (such as `vTechNewCap`) to get total new capacity of a
  process by the end of each period.

- System costs have been regrouped by type (capital, fixed O&M, variable
  O&M, supply, taxes, subsidies) and by process type (technology, trade,
  storage, etc.) to facilitate the analysis of the cost structure. Total
  Costs equation has been rewritten to reflect the new cost structure.  
    
  **Bug fixes**

- Early retirement option (`optimizeRetirment = TRUE`) is corrected to
  exclude retirement of “new” technologies at the same time as their
  installation.

- [`draw()`](https://energyRt.org/reference/draw.md) method for ‘trade’
  is fixed to exclude repeated arrows in the plot.

- [`newCosts()`](https://energyRt.org/reference/newCosts.md) is
  debugged, an example is added to the Utopia tutorial.

- [`tsl2hour()`](https://energyRt.org/reference/tsl2dtm.md) fixed to be
  able identify n-digits hours (previously worked for 2 only).  
    
  **Miscellaneous**

- A new version (3) of the logo design idea (by DALL-E).

- The code clean-up, testing, and documentation are in progress to
  comply with CRAN requirements.

- Functions/method in progress:
  [`levcost()`](https://energyRt.org/reference/levcost.md) and
  [`report()`](https://energyRt.org/reference/report.md),

- `add_weights` and `add_intervals` arguments will be added to
  [`getData()`](https://energyRt.org/reference/getData.md) function to
  add time-slice weights and interval lengths to the requested data if
  applicable.

## energyRt 0.50.7-dev

- Fixed a few stability issues in the
  [`draw()`](https://energyRt.org/reference/draw.md) method.
- Added “Hello World” example to the tutorial.
- A new version of the logo design idea (by DALL-E).
- The code clean-up and documentation are in progress to comply with
  CRAN requirements.
- The very first draft of the package CRAN-like
  [manual](https://github.com/optimal2050/energyRt/blob/master/man/figures/energyRt-manual.pdf)
  is added.
- The version might be unstable due to ongoing changes.

## energyRt 0.50.6-dev

- draw() is drafted for all processes: ‘technology’, ‘export’, ‘import’,
  ‘supply’, ‘demand’, ‘trade’, ’storage
- docs completed for main classes with examples.
- code clean-up and documentation in progress.

## energyRt 0.50.5-dev

- draw() is rewritten based on ‘grid’ package, and is now a generic
  method.
- added draw() methods for ‘technology’, ‘export’, and ‘import’ classes.
- fixed several interface-level bugs introduced in 0.50.4-dev during
  clean-up and documentation.

## energyRt 0.50.4-dev

- Documentation of classes is in progress (~70% docs completed).
- Logo-search has started! “logo” page added.
- Website is reshaped, added new, not populated yet “articles”.
- !!! Not Tested!!! Due to the ongoing changes in both documentation and
  functions/methods clean-up, the version may have “surprises” - tests
  are in progress.

## energyRt 0.50.3-dev

- Development version in the preparation for CRAN submission.
- Added a `NEWS.md` file to track changes to the package.
- Added functions to document classes from yaml file ‘classes.yaml’.
- `technology-class` and `newTechnology` function documented.
