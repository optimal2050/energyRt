# class ######################################################################
#' An S4 class to represent technology
#'
#' @name class-technology
#'
#' @description
#' Technology of a technological process in the model is used to convert input
#' commodities into output commodities with consumption or production of auxiliary
#' commodities linked to other parameters or variables of the technology.
#' A broad set of parameters provides flexibility to model various technological
#' processes, including efficiency, availability, costs, and exogenous
#' shocks (weather factors).
#' @md
#'
#' @slot name `r get_slot_doc("technology", "name")`
#' @slot desc `r get_slot_doc("technology", "desc")`
#' @slot input `r get_slot_doc("technology", "input")`
#' @slot output `r get_slot_doc("technology", "output")`
#' @slot aux `r get_slot_doc("technology", "aux")`
#' @slot units `r get_slot_doc("technology", "units")`
#' @slot group `r get_slot_doc("technology", "group")`
#' @slot cap2act `r get_slot_doc("technology", "cap2act")`
#' @slot geff `r get_slot_doc("technology", "geff")`
#' @slot ceff `r get_slot_doc("technology", "ceff")`
#' @slot aeff `r get_slot_doc("technology", "aeff")`
#' @slot af `r get_slot_doc("technology", "af")`
#' @slot afs `r get_slot_doc("technology", "afs")`
#' @slot weather `r get_slot_doc("technology", "weather")`
#' @slot fixom `r get_slot_doc("technology", "fixom")`
#' @slot varom `r get_slot_doc("technology", "varom")`
#' @slot invcost `r get_slot_doc("technology", "invcost")`
#' @slot start `r get_slot_doc("technology", "start")`
#' @slot end `r get_slot_doc("technology", "end")`
#' @slot olife `r get_slot_doc("technology", "olife")`
#' @slot capacity `r get_slot_doc("technology", "capacity")`
#' @slot optimizeRetirement `r get_slot_doc("technology", "optimizeRetirement")`
#' @slot fullYear `r get_slot_doc("technology", "fullYear")`
#' @slot timeframe `r get_slot_doc("technology", "timeframe")`
#' @slot region `r get_slot_doc("technology", "region")`
#' @slot misc `r get_slot_doc("technology", "misc")`
#'
#' @rdname class-technology
#' @include class-supply.R
#' @family technology process class
#' @export
setClass("technology",
  representation(
    # General information
    name = "character",
    desc = "character",
    input = "data.frame",
    output = "data.frame",
    aux = "data.frame",
    units = "data.frame",
    group = "data.frame",
    cap2act = "numeric",
    # Performance parameters
    geff = "data.frame",
    ceff = "data.frame",
    aeff = "data.frame",
    af = "data.frame",
    afs = "data.frame",
    weather = "data.frame",
    # Costs
    fixom = "data.frame",
    varom = "data.frame",
    invcost = "data.frame",
    # Market
    start = "data.frame",
    end = "data.frame",
    olife = "data.frame",
    capacity = "data.frame",
    optimizeRetirement = "logical",
    # upgrade.technology = "character",
    fullYear = "logical",
    timeframe = "character",
    region = "character",
    misc = "list"
  ),
  prototype(
    name = "",
    desc = "",
    input = data.frame(
      comm = character(),
      unit = character(),
      group = character(),
      combustion = numeric(),
      stringsAsFactors = FALSE
    ),
    output = data.frame(
      comm = character(),
      unit = character(),
      group = character(),
      stringsAsFactors = FALSE
    ),
    aux = data.frame(
      acomm = character(),
      unit = character(),
      stringsAsFactors = FALSE
    ),
    units = data.frame(
      capacity = character(),
      use = character(),
      activity = character(),
      costs = character(),
      stringsAsFactors = FALSE
    ),
    group = data.frame(
      group = character(),
      desc = character(),
      unit = character(),
      stringsAsFactors = FALSE
    ),
    cap2act = 1,
    # group efficiency
    geff = data.frame(
      region = character(),
      year = integer(),
      slice = character(),
      group = character(),
      ginp2use = numeric(),
      stringsAsFactors = FALSE
    ),
    # commodity efficiency
    ceff = data.frame(
      region = character(),
      year = integer(),
      slice = character(),
      comm = character(),
      cinp2use = numeric(),
      use2cact = numeric(),
      cact2cout = numeric(),
      cinp2ginp = numeric(),
      share.lo = numeric(),
      share.up = numeric(),
      share.fx = numeric(),
      afc.lo = numeric(), # !!! check and potentially rename avc.*
      afc.up = numeric(),
      afc.fx = numeric(),
      stringsAsFactors = FALSE
    ),
    # Auxilary parameter
    aeff = data.frame(
      acomm = character(),
      comm = character(),
      region = character(),
      year = integer(),
      slice = character(),
      cinp2ainp = numeric(),
      cinp2aout = numeric(),
      cout2ainp = numeric(),
      cout2aout = numeric(),
      act2ainp = numeric(),
      act2aout = numeric(),
      cap2ainp = numeric(),
      cap2aout = numeric(),
      ncap2ainp = numeric(),
      ncap2aout = numeric(),
      # storage part
      stg2ainp = numeric(),
      sinp2ainp = numeric(),
      sout2ainp = numeric(),
      stg2aout = numeric(),
      sinp2aout = numeric(),
      sout2aout = numeric(),
      stringsAsFactors = FALSE
    ),
    af = data.frame(
      region = character(),
      year = integer(),
      slice = character(),
      af.lo = numeric(),
      af.up = numeric(),
      af.fx = numeric(),
      rampup = numeric(),
      rampdown = numeric(),
      stringsAsFactors = FALSE
    ),
    afs = data.frame(
      region = character(),
      year = integer(),
      slice = character(),
      afs.lo = numeric(),
      afs.up = numeric(),
      afs.fx = numeric(),
      stringsAsFactors = FALSE
    ),
    weather = data.frame(
      weather = character(),
      comm = character(),
      wafc.lo = numeric(),
      wafc.up = numeric(),
      wafc.fx = numeric(),
      waf.lo = numeric(),
      waf.up = numeric(),
      waf.fx = numeric(),
      wafs.lo = numeric(),
      wafs.up = numeric(),
      wafs.fx = numeric(),
      stringsAsFactors = FALSE
    ),
    fixom = data.frame(
      region = character(),
      year = integer(),
      fixom = numeric(),
      stringsAsFactors = FALSE
    ),
    varom = data.frame(
      region = character(),
      year = integer(),
      slice = character(),
      comm = character(),
      acomm = character(),
      varom = numeric(),
      cvarom = numeric(),
      avarom = numeric(),
      stringsAsFactors = FALSE
    ),
    invcost = data.frame(
      region = character(),
      year = integer(),
      invcost = numeric(),
      wacc = numeric(),
      eac = numeric(),
      retcost = numeric(),
      stringsAsFactors = FALSE
    ),
    start = data.frame(
      region = character(),
      start = integer(),
      stringsAsFactors = FALSE
    ),
    end = data.frame(
      region = character(),
      end = integer(),
      stringsAsFactors = FALSE
    ),
    olife = data.frame(
      region = character(),
      olife = integer(),
      stringsAsFactors = FALSE
    ),
    capacity = data.frame(
      region = character(),
      year = integer(),
      stock = numeric(),
      cap.lo = numeric(),
      cap.up = numeric(),
      cap.fx = numeric(),
      ncap.lo = numeric(),
      ncap.up = numeric(),
      ncap.fx = numeric(),
      ret.lo = numeric(),
      ret.up = numeric(),
      ret.fx = numeric(),
      stringsAsFactors = FALSE
    ),
    optimizeRetirement = TRUE,
    # upgrade.technology = character(),
    region = character(),
    timeframe = character(),
    misc = list()
  ),
  # validity = .check_technology_data_frame,
  S3methods = FALSE
)

setMethod("initialize", "technology", function(.Object, ...) {
  .Object
})

# constructor ###############################################################
#' @title Create a new "technology" object.
#' @name newTechnology
#' @description
#' This function initializes and returns an S4 object of class `technology`,
#' representing a specific technology with given attributes.
#' The function has the same arguments as slot-names in the `technology` class.
#' Every argument has a specific format as described below and in the class
#' documentation.
#'
#' @param name `r get_slot_doc("technology", "name")`
#' @param desc `r get_slot_doc("technology", "desc")`
#' @param input `r get_slot_doc("technology", "input")`
#' @param output `r get_slot_doc("technology", "output")`
#' @param group `r get_slot_doc("technology", "group")`
#' @param aux `r get_slot_doc("technology", "aux")`
#' @param units `r get_slot_doc("technology", "units")`
#' @param cap2act `r get_slot_doc("technology", "cap2act")`
#' @param geff `r get_slot_doc("technology", "geff")`
#' @param ceff `r get_slot_doc("technology", "ceff")`
#' @param aeff `r get_slot_doc("technology", "aeff")`
#' @param af `r get_slot_doc("technology", "af")`
#' @param afs `r get_slot_doc("technology", "afs")`
#' @param weather `r get_slot_doc("technology", "weather")`
#' @param capacity `r get_slot_doc("technology", "capacity")`
#' @param invcost `r get_slot_doc("technology", "invcost")`
#' @param fixom `r get_slot_doc("technology", "fixom")`
#' @param varom `r get_slot_doc("technology", "varom")`
#' @param olife `r get_slot_doc("technology", "olife")`
#' @param region `r get_slot_doc("technology", "region")`
#' @param start `r get_slot_doc("technology", "start")`
#' @param end `r get_slot_doc("technology", "end")`
#' @param timeframe `r get_slot_doc("technology", "timeframe")`
#' @param fullYear `r get_slot_doc("technology", "fullYear")`
#' @param optimizeRetirement `r get_slot_doc("technology", "optimizeRetirement")`
#' @param misc `r get_slot_doc("technology", "misc")`
#'
#' @family technology process
#' @rdname technology
#'
#' @return An object of class technology.
#' @export
#' @inherit draw examples
#' @examples
#' ECOAL <- newTechnology(
#'   name = "ECOAL", # name, used in sets, no white spaces or special characters
#'   desc = "Generic coal power plant", # any description of the technology
#'   input = data.frame(
#'     comm = "COAL", # name of input commodity
#'     unit = "MMBtu", # unit of the input commodity
#'     # combustion factor from 0 to 1 (default 1) to calculate emissions
#'     # from fuels combustion (commodities intermediate consumption, more broadly)
#'     combustion = 1
#'   ),
#'   output = data.frame(
#'     comm = "ELC", # name of output commodity
#'     unit = "MWh" # unit of the output commodity
#'   ),
#'   aux = data.frame(
#'     acomm = c("NOx", "SO2", "Hg"), # names of auxilary commodities
#'     unit = c("kg", "kg", "g") # units
#'   ),
#'   # Capacity to activity ration: 8760 MWh output a year per MW of capacity
#'   cap2act = 8760,
#'   ceff = data.frame( # efficiency parameters for the main commodities
#'     comm = "COAL",
#'     # efficiency, 1/10 MWh per MMBtu, inverse heat rate
#'     # check: 1 / convert(10, "MMBtu", "MWh") ~= 34% efficiency
#'     cinp2use = 1 / 10
#'   ),
#'   aeff = data.frame( # paramaters for the auxilary commodities
#'     acomm = c("NOx", "SO2", "Hg"),
#'     act2aout = c(0.1, 0.2, 0.3) # emission factors, linked to activity
#'   ),
#'   af = data.frame( # availability (capacity) factor by time slices
#'     af.up = 0.95 # maximum 95% per hour
#'   ),
#'   afs = data.frame( # availability factor by timeframes
#'     slice = "ANNUAL", # annual availability factor
#'     afs.lo = 0.40, # at least 40% per year
#'     afs.up = 0.85 # maximum 85% per year
#'   ),
#'   fixom = data.frame( # fixed operational and maintenance cost
#'     region = c("R1", "R2", NA), # regions, NA - all other regions
#'     fixom = c(100, 200, 150) # MW a year
#'   ),
#'   varom = data.frame( # variable operational and maintenance cost
#'     region = c("R1", "R2"), # regions
#'     varom = c(1, 2) # $1 and $2 per MWh
#'   ),
#'   invcost = data.frame( # investment cost
#'     year = c(2020, 2030, 2040), # to differentiate by years
#'     invcost = c(1000, 900, 800) # $1000, $900, $800 per MW
#'   ),
#'   start = data.frame( # start year
#'     start = 2020 # can be installed from 2020
#'   ),
#'   end = data.frame( # end year
#'     end = 2040 # can be installed until 2040
#'   ),
#'   olife = data.frame( # operational life
#'     olife = 30 # years
#'   ),
#'   capacity = data.frame( # existing capacity
#'     year = c(2020, 2030, 2040), # to differentiate by years
#'     region = c("R1"), # exists only in R1
#'     stock = c(300, 200, 100) # age-based exogenous retirement
#'   ),
#'   # regions where the technology can be installed
#'   region = c("R1", "R2", "R5", "R7"),
#' )
#' draw(ECOAL)
#'
newTechnology <- function(
    name = "",
    desc = "",
    input = data.frame(),
    output = data.frame(),
    group = data.frame(),
    aux = data.frame(),
    units = data.frame(),
    cap2act = as.numeric(1),
    geff = data.frame(),
    ceff = data.frame(),
    aeff = data.frame(),
    af = data.frame(),
    afs = data.frame(),
    weather = data.frame(),
    # stock = data.frame(),
    capacity = data.frame(),
    invcost = data.frame(),
    fixom = data.frame(),
    varom = data.frame(),
    olife = data.frame(),
    region = character(),
    start = data.frame(),
    end = data.frame(),
    timeframe = character(),
    fullYear = TRUE,
    optimizeRetirement = FALSE,
    # upgrade.technology = character(),
    misc = list(),
    ...) {
  .data2slots("technology", name,
    desc = desc,
    input = input,
    output = output,
    group = group,
    aux = aux,
    units = units,
    cap2act = cap2act,
    geff = geff,
    ceff = ceff,
    aeff = aeff,
    af = af,
    afs = afs,
    weather = weather,
    capacity = capacity,
    invcost = invcost,
    fixom = fixom,
    varom = varom,
    olife = olife,
    region = region,
    start = start,
    end = end,
    timeframe = timeframe,
    fullYear = fullYear,
    optimizeRetirement = optimizeRetirement,
    # upgrade.technology = upgrade.technology,
    misc = misc,
    ...
  )
}

# methods ###################################################################
#' @title Update a "technology" object.
#' @param object object of class technology
#'
#' @param ... slot-names with data to update (see `newTechnology`)
#'
#' @rdname technology
#' @family update technology process
#' @method update technology
#' @export
setMethod("update", "technology", function(object, ...) {
  .data2slots("technology", object, ...)
})

# internal functions ########################################################
# get names of data.frame slots
.technology_data_frame <- function() {
  # get technology slot data.frame names
  g <- getClass("technology")
  names(g@slots)[sapply(names(g@slots), function(z) g@slots[[z]] == "data.frame")]
}

# table commodity_type and checks
checkInpOut <- function(tech) {
  ctype <- data.frame(
    type = factor(NULL, c("input", "output", "aux")),
    group = character(),
    comb = numeric(),
    unit = character(),
    stringsAsFactors = FALSE
  )
  # Define type commodity
  icomm <- tech@input$comm
  ocomm <- tech@output$comm
  acomm <- tech@aux$acomm
  comm <- c(icomm, ocomm, acomm)
  ctype[seq(along = comm), ] <- NA
  rownames(ctype) <- comm
  ctype[icomm, "type"] <- "input"
  ctype[ocomm, "type"] <- "output"
  ctype[acomm, "type"] <- "aux"
  ctype[icomm, c("group", "unit")] <- tech@input[, c("group", "unit")]
  ctype[ocomm, c("group", "unit")] <- tech@output[, c("group", "unit")]
  ctype[, "comb"] <- 0
  tech@input$combustion[is.na(tech@input$combustion)] <- 1
  ctype[tech@input$comm, "comb"] <- tech@input$combustion
  aux <- data.frame(
    input = logical(),
    output = logical(),
    stringsAsFactors = FALSE
  )
  if (length(acomm)) {
    aux[seq(along = acomm), ] <- FALSE
    rownames(aux) <- acomm
  }
  #  Check type
  # ! have to realised
  if (length(icomm) == 0 && length(ocomm) == 0) {
    warnings("There is no input & output commodity")
  }

  # Define technology type by parameter
  for (i in comm) {
    # Group ?
    if (any(!is.na(tech@ceff[tech@ceff$comm == i, c(
      "cinp2ginp", "share.lo",
      "share.up", "share.fx"
    )]))) {
      if (is.na(ctype[i, "group"])) {
        stop('Wrong commodity "', tech@name, '": "', i, '"')
      }
    }
    # Not group ?
    if (any(!is.na(tech@ceff[tech@ceff$comm == i, "cinp2use"]))) {
      if (!is.na(ctype[i, "group"])) {
        stop('Wrong commodity "', tech@name, '": "', i, '"')
      }
    }
    # Input ?
    if (any(!is.na(tech@ceff[
      tech@ceff$comm == i,

      c("cinp2use", "cinp2ginp")
    ]))) {
      if (ctype[i, "type"] != "input") {
        stop('Wrong commodity "', tech@name, '": "', i, '"')
      }
    }
    # Output ?
    if (any(!is.na(tech@ceff[tech@ceff$comm == i, c(
      "use2cact", "cact2cout" # , "afc.lo", "afc.up", "afc.fx"
    )]))) {
      if (ctype[i, "type"] != "output") {
        stop('Wrong commodity "', tech@name, '": "', i, '"')
      }
    }
    # Aux ?
    if (any(!is.na(tech@aeff[tech@aeff$acomm == i, c(
      "act2ainp", "act2aout",
      "cap2ainp", "cap2aout", "ncap2ainp", "ncap2aout"
    )])) ||
      any(!is.na(tech@aeff[tech@aeff$acomm == i, c(
        "cinp2ainp",
        "cinp2aout", "cout2ainp", "cout2aout"
      )]))) {
      if (ctype[i, "type"] != "aux") {
        stop('Wrong commodity "', tech@name, '": "', i, '"')
      }
    }
  }
  for (i in acomm) {
    aux[i, "input"] <- (any(!is.na(
      tech@aeff[tech@aeff$acomm == i, c("act2ainp", "cap2ainp", "ncap2ainp")]
    )) ||
      any(!is.na(tech@aeff[tech@aeff$acomm == i, c("cinp2ainp", "cout2ainp")])))
    aux[i, "output"] <- (any(!is.na(
      tech@aeff[tech@aeff$acomm == i, c("act2aout", "cap2aout", "ncap2aout")]
    )) ||
      any(!is.na(tech@aeff[tech@aeff$acomm == i, c("cinp2aout", "cout2aout")])))
  }
  gtype <- data.frame(
    type = factor(NULL, c("input", "output")),
    stringsAsFactors = FALSE
  )

  # Define type group
  group <- unique(c(
    tech@geff$group, tech@geff$geff,
    tech@group$group, tech@group$geff,
    tech@input$group, tech@output$group
  ))
  group <- group[!is.na(group)]
  if (length(group) != 0) {
    if (any(is.na(c(tech@group$group, tech@group$geff)))) {
      stop('There is NA group in technology "', tech@name, '"')
    }
    gtype[seq(along = group), ] <- NA
    rownames(gtype) <- group
    for (i in unique(tech@geff$group)) {
      if (any(!is.na(tech@geff[tech@geff$group == i, "ginp2use"]))) {
        if (!is.na(gtype[i, "type"]) && gtype[i, "type"] == "output") {
          stop('Wrong group in technology "', tech@name, '": "', i, '"')
        }
        gtype[i, "type"] <- "input"
      }
    }
    for (i in group) {
      jj <- rownames(ctype)[!is.na(ctype$group) & ctype$group == i]
      if (length(jj) != 0) {
        if (any(ctype[jj, "type"] == "input")) {
          if (!is.na(gtype[i, "type"]) && gtype[i, "type"] == "output") {
            stop('Wrong group in technology "', tech@name, '": "', i, '"')
          }
          gtype[i, "type"] <- "input"
        }
        if (any(ctype[jj, "type"] == "output")) {
          if (!is.na(gtype[i, "type"]) && gtype[i, "type"] == "input") {
            stop('Wrong group in technology "', tech@name, '": "', i, '"')
          }
          gtype[i, "type"] <- "output"
        }
      }
    }
    if (any(is.na(gtype[, "type"]))) {
      stop(
        'Wrong group in technology "', tech@name, '": "',
        paste(rownames(gtype)[is.na(gtype[, "type"])], collapse = '", "'), '"'
      )
    }
  }
  fcmd <- c(tech@ceff$comm, tech@aeff$comm[!is.na(tech@aeff$comm)], tech@aeff$acomm)
  fcmd <- fcmd[!(fcmd %in% c(tech@input$comm, tech@output$comm, tech@aux$acomm))]
  if (length(fcmd) != 0) {
    stop(
      'Unknow commodity in technology (there is not definition in input, output or aux) "',
      tech@name, '": ', paste(fcmd, collapse = '", "'), '"'
    )
  }
  list(comm = ctype, group = gtype, aux = aux)
}
