# Internal function to add (append) elements to sets in `modInp@set`
#
# @param obj modInp object
# @param app object which name `obj@name` will be appended to sets.
# @param approxim list with interpolation rules.
#
#' @include class-scenario.R
#' @include generics.R class-costs.R
#
# @return
#
# setGeneric(".add2set", function(obj, app, approxim) standardGeneric(".add2set"))
# ToDo: !!!
#   - rename arguments

#==============================================================================#
# Add commodity ####
#==============================================================================#
setMethod(
  ".add2set",
  signature(obj = "modInp", app = "commodity", approxim = "list"),
  function(obj, app, approxim) {
    # cmd <- .upper_case(app)
    cmd <- app
    if (!check_name(cmd@name)) {
      stop(paste('Incorrect commodity name "', cmd@name, '"', sep = ""))
    }
    if (.find_commodity(obj, cmd@name)) {
      warning(paste('The commodity "', cmd@name, '" already exists, ',
        "replacing the existing data",
        sep = ""
      ))
      obj <- .drop_commodity(obj, cmd@name)
    }
    obj@parameters[["comm"]] <- .dat2par(obj@parameters[["comm"]], cmd@name)
    obj
  }
)

#==============================================================================#
# Add demand ####
#==============================================================================#
setMethod(
  ".add2set",
  signature(obj = "modInp", app = "demand", approxim = "list"),
  function(obj, app, approxim) {
    # dem <- .upper_case(app)
    dem <- app
    if (!check_name(dem@name)) {
      stop(paste('Incorrect demand name "', dem@name, '"', sep = ""))
    }
    if (.find_demand(obj, dem@name)) {
      warning(paste('The demand "', dem@name, '" already exists, ',
        "replacing the existing data",
        sep = ""
      ))
      obj <- .drop_demand(obj, dem@name)
    }
    obj@parameters[["dem"]] <- .dat2par(obj@parameters[["dem"]], dem@name)
    obj
  }
)

#==============================================================================#
# Add weather ####
#==============================================================================#
setMethod(
  ".add2set",
  signature(obj = "modInp", app = "weather", approxim = "list"),
  function(obj, app, approxim) {
    # wth <- .upper_case(app)
    wth <- app
    if (!check_name(wth@name)) {
      stop(paste('Incorrect weather name "', wth@name, '"', sep = ""))
    }
    if (.find_weather(obj, wth@name)) {
      warning(paste('The weather "', wth@name, '" already exists, ',
        "replacing the existing data",
        sep = ""
      ))
      obj <- .drop_weather(obj, wth@name)
    }
    obj@parameters[["weather"]] <- .dat2par(obj@parameters[["weather"]], wth@name)
    obj
  }
)

#==============================================================================#
# Add constraint ####
#==============================================================================#
setMethod(
  ".add2set",
  signature(obj = "modInp", app = "constraint", approxim = "list"),
  function(obj, app, approxim) {
    obj
  }
)

#==============================================================================#
# Add costs ####
#==============================================================================#
setMethod(
  ".add2set",
  signature(obj = "modInp", app = "costs", approxim = "list"),
  function(obj, app, approxim) {
    obj
  }
)

#==============================================================================#
# Add supply ####
#==============================================================================#
setMethod(
  ".add2set",
  signature(obj = "modInp", app = "supply", approxim = "list"),
  function(obj, app, approxim) {
    # sup <- .upper_case(app)
    sup <- app
    if (!check_name(sup@name)) {
      stop(paste('Incorrect supply name "', sup@name, '"', sep = ""))
    }
    if (.find_supply(obj, sup@name)) {
      warning(paste('The supply "', sup@name, '" already exists, ',
        "replacing the existing data",
        sep = ""
      ))
      obj <- .drop_supply(obj, sup@name)
    }
    obj@parameters[["sup"]] <- .dat2par(obj@parameters[["sup"]], sup@name)
    obj
  }
)

#==============================================================================#
# Add storage ####
#==============================================================================#
setMethod(
  ".add2set",
  signature(obj = "modInp", app = "storage", approxim = "list"),
  function(obj, app, approxim) {
    # stg <- .upper_case(app)
    stg <- app
    if (!check_name(stg@name)) {
      stop(paste('Incorrect storage name "', stg@name, '"', sep = ""))
    }
    if (.find_supply(obj, stg@name)) {
      warning(paste('The storage "', stg@name, '" already exists, ',
        "replacing the existing data",
        sep = ""
      ))
      obj <- .drop_storage(obj, stg@name)
    }
    obj@parameters[["stg"]] <- .dat2par(obj@parameters[["stg"]], stg@name)
    obj
  }
)

#==============================================================================#
# Add export ####
#==============================================================================#
setMethod(
  ".add2set",
  signature(obj = "modInp", app = "export", approxim = "list"),
  function(obj, app, approxim) {
    # exp <- .upper_case(app)
    exp <- app
    if (!check_name(exp@name)) {
      stop(paste('Incorrect export name "', exp@name, '"', sep = ""))
    }
    if (.find_export(obj, exp@name)) {
      warning(paste('The export "', exp@name, '" already exists, ',
        "replacing the existing data",
        sep = ""
      ))
      obj <- .drop_export(obj, exp@name)
    }
    obj@parameters[["expp"]] <- .dat2par(obj@parameters[["expp"]], exp@name)
    obj
  }
)

#==============================================================================#
# Add import ####
#==============================================================================#
setMethod(
  ".add2set",
  signature(obj = "modInp", app = "import", approxim = "list"),
  function(obj, app, approxim) {
    # imp <- .upper_case(app)
    imp <- app
    if (!check_name(imp@name)) {
      stop(paste('Incorrect import name "', imp@name, '"', sep = ""))
    }
    if (.find_import(obj, imp@name)) {
      warning(paste('The import "', imp@name, '" already exists, ',
        "replacing the existing data",
        sep = ""
      ))
      obj <- .drop_import(obj, imp@name)
    }
    obj@parameters[["imp"]] <- .dat2par(obj@parameters[["imp"]], imp@name)
    obj
  }
)

#==============================================================================#
# Add technology ####
#==============================================================================#
setMethod(
  ".add2set",
  signature(obj = "modInp", app = "technology", approxim = "list"),
  function(obj, app, approxim) {
    # tech <- .upper_case(app)
    tech <- app
    # Temporary solution for infinite-olife technology
    if (nrow(tech@olife) == 0) {
      tech@olife[1, ] <- NA
      tech@olife[1, "olife"] <- 1e3
    }
    if (!check_name(tech@name)) {
      stop(paste('Incorrect technology name "', tech@name, '"', sep = ""))
    }
    if (.find_technology(obj, tech@name)) {
      warning(paste('The technology "', tech@name, '" already exists, ',
        "replacing the existing data",
        sep = ""
      ))
      obj <- .drop_technology(obj, tech@name)
    }
    obj@parameters[["tech"]] <- .dat2par(obj@parameters[["tech"]], tech@name)
    obj
  }
)

#==============================================================================#
# Add trade ####
#==============================================================================#
setMethod(
  ".add2set",
  signature(obj = "modInp", app = "trade", approxim = "list"),
  function(obj, app, approxim) {
    # trd <- .upper_case(app)
    trd <- app
    if (!check_name(trd@name)) {
      stop(paste('Incorrect trade name "', trd@name, '"', sep = ""))
    }
    if (.find_trade(obj, trd@name)) {
      warning(paste('The trade "', trd@name, '" already exists in sets, ',
        "replacing the existing data",
        sep = ""
      ))
      obj <- .drop_trade(obj, trd@name)
    }
    obj@parameters[["trade"]] <- .dat2par(obj@parameters[["trade"]], trd@name)
    obj
  }
)

#==============================================================================#
# Add tax ####
#==============================================================================#
setMethod(
  ".add2set",
  signature(obj = "modInp", app = "tax", approxim = "list"),
  function(obj, app, approxim) {
    obj
  }
)

#==============================================================================#
# Add subsidy ####
#==============================================================================#
setMethod(
  ".add2set",
  signature(obj = "modInp", app = "sub", approxim = "list"),
  function(obj, app, approxim) {
    obj
  }
)

#==============================================================================#
# Internal functions ####
#==============================================================================#

# !!! use methods instead?
.find_commodity <- function(modInp, name) {
  fl <- FALSE
  for (i in c("comm", "mUpComm", "mLoComm", "mFxComm")) {
    fl <- fl || any(modInp@parameters[[i]]@data$comm == name, na.rm = TRUE)
  }
  for (i in c("pEmissionFactor")) { # 'ems_from',
    fl <- fl || any(modInp@parameters[[i]]@data$commp == name, na.rm = TRUE)
  }
  fl
}

.drop_commodity <- function(modInp, name) {
  for (i in c("comm", "mUpComm", "mLoComm", "mFxComm")) {
    modInp@parameters[[i]] <- .drop_set_value(modInp@parameters[[i]], "comm", name)
  }
  for (i in c("pEmissionFactor")) { # 'ems_from',
    modInp@parameters[[i]] <- .drop_set_value(modInp@parameters[[i]], "commp", name)
  }
  modInp
}

.find_demand <- function(obj, name) {
  fl <- FALSE
  for (i in c("pDemand")) {
    fl <- fl || any(obj@parameters[[i]]@data$comm == name, na.rm = TRUE)
  }
  fl
}

.drop_demand <- function(modInp, name) {
  for (i in c("pDemand")) {
    modInp@parameters[[i]] <- .drop_set_value(modInp@parameters[[i]], "comm", name)
  }
  modInp
}

.find_weather <- function(modInp, name) {
  fl <- FALSE
  for (i in c("pWeather")) {
    fl <- fl || any(modInp@parameters[[i]]@data$comm == name, na.rm = TRUE)
  }
  fl
}

.drop_weather <- function(modInp, name) {
  stop(
    'The method is not available for class "weather", ',
    "re-interpolation is required ", name
  )
  modInp
}

.find_supply <- function(modInp, name) {
  fl <- FALSE
  for (i in c("sup", "mSupComm", "pSupCost", "pSupAva", "pSupReserve")) {
    fl <- fl || any(modInp@parameters[[i]]@data$sup == name, na.rm = TRUE)
  }
  fl
}

.drop_supply <- function(modInp, name) {
  for (i in c("sup", "mSupComm", "pSupCost", "pSupAva", "pSupReserve")) {
    modInp@parameters[[i]] <- .drop_set_value(modInp@parameters[[i]], "sup", name)
  }
  modInp
}

.find_export <- function(modInp, name) {
  fl <- FALSE
  for (i in c("expp", "mExpComm", "pExportRowPrice", "pExportRowRes", "pExportRow")) {
    fl <- fl || any(modInp@parameters[[i]]@data$expp == name, na.rm = TRUE)
  }
  fl
}

.drop_export <- function(modInp, name) {
  for (i in c("expp", "mExpComm", "pRowExportPrice", "pRowExportRes", "pRowExport")) {
    modInp@parameters[[i]] <- .drop_set_value(modInp@parameters[[i]], "expp", name)
  }
  modInp
}

.find_import <- function(modInp, name) {
  fl <- FALSE
  for (i in c(
    "imp", "mImpComm", "pImportRowPrice", "pImportRowRes",
    "pImportRow"
  )) {
    fl <- fl || any(modInp@parameters[[i]]@data$imp == name, na.rm = TRUE)
  }
  fl
}

.drop_import <- function(modInp, name) {
  for (i in c(
    "imp", "mImpComm", "pImportRowPrice", "pImportRowRes",
    "pImportRow"
  )) {
    modInp@parameters[[i]] <- .drop_set_value(modInp@parameters[[i]], "imp", name)
  }
  modInp
}

.find_trade <- function(modInp, name) {
  any(modInp@parameters$trade@data$trade == name, na.rm = TRUE)
}

.drop_trade <- function(modInp, name) {
  stop(
    'The method is not available for class "trade",',
    " re-interpolation is required ", name
  )
  modInp
}

.find_technology <- function(modInp, name) {
  any(modInp@parameters$tech@data$tech == name, na.rm = TRUE)
}

.drop_technology <- function(modInp, name) {
  stop(
    'The method is not available for class "technology", ',
    "re-interpolation is required ", name
  )
  modInp
}

.find_constraint <- function(modInp, name) {
  any(names(modInp@parameters$constraint) == name)
}

.drop_config_param <- function(modInp) {
  for (i in c("pDiscount", "pDummyImportCost", "pDummyExportCost")) {
    modInp@parameters[[i]] <- .resetParameter(modInp@parameters[[i]])
  }
  modInp
}

.get_stg_prm_lst <- function() {
  # vector with parameter-names, relevant to "storage"
  # ? add mStorageOMCost?
  c(
    "mStorageSlice", "ndefpStorageOlife", "mStorageComm",
    "mStorageNew", "mStorageSpan", "ndefpStorageCapUp",
    "ndefpStorageAvaUp", "pStorageInpLoss", "pStorageOutLoss",
    "pStorageStoreLoss", "pStorageStock", "pStorageOlife",
    "pStorageCapUp", "pStorageCapLo", "pStorageCostStore",
    "pStorageCostInp", "pStorageCostOut", "pStorageFixom",
    "pStorageInvcost", "pStorageAvaLo", "pStorageAvaUp"
  )
}

.find_storage <- function(modInp, name) {
  fl <- FALSE
  for (i in .get_stg_prm_lst()) {
    fl <- fl || any(modInp@parameters[[i]]@data$stg == name, na.rm = TRUE)
  }
  fl
}

.drop_storage <- function(obj, name) {
  for (i in .get_stg_prm_lst()) {
    obj@parameters[[i]] <- .drop_set_value(obj@parameters[[i]], "stg", name)
  }
  obj
}

#### end ===================================================================####
