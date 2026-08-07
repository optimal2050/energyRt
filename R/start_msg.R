.onAttach <- function(...) {
  packageStartupMessage(
    glue::glue('energyRt {utils::packageVersion("energyRt")}-dev ({utils::packageDate("energyRt")})'),
    "\nDevelopment version, please report bugs/issues:",
    "\nhttps://github.com/optimal2050/energyRt/issues"
    )

  # Deprecated: settings used to be an R script sourced from the home
  # directory. `~/.energyRt/config.yml` replaces it -- it is data rather than
  # code, so it can be inspected (`en_config_show()`) and written
  # (`en_config_write()`) instead of only executed. Honoured for one release.
  if (file.exists("~/.energyRt.R")) {
    packageStartupMessage(
      'Sourcing "~/.energyRt.R" (deprecated).\n',
      "Migrate with: energyRt::en_config_write()"
    )
    try(source("~/.energyRt.R"))
  }

  # environments
  .initiate_env <- function(e) {
    if (!exists(e, envir = .GlobalEnv)) {
      assign(e, new.env(parent = .GlobalEnv), envir = .GlobalEnv)
    }
  }
  .initiate_env(".scen")
  .initiate_env(".tmp")
}
