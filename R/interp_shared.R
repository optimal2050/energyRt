# =============================================================================#
# interp_shared.R  -- shared utilities relocated out of the (retired) legacy
# interpolation files (interpolate.R / interpolate2.R / obj2modInp.R / add2set.R,
# now in depreciated/R/). These functions are used by the NEW interp_mod()
# pipeline and/or general package code, so they survive the legacy retirement.
# Extracted verbatim (srcref) by tmp/_extract.R; see PIPELINE.md.
# =============================================================================#

# ---- from add2set.R ----
.drop_config_param <- .drop_config_param <- function(modInp) {
  for (i in c("pDiscount", "pDummyImportCost", "pDummyExportCost")) {
    modInp@parameters[[i]] <- .resetParameter(modInp@parameters[[i]])
  }
  modInp
}

# ---- from interpolate.R ----
.apply_to_code_ret_list <- .apply_to_code_ret_list <- function(scen, func, ..., clss = NULL,
                                    need.name = TRUE) {
  rs <- list()
  for (i in seq(along = scen@model@data)) {
    for (j in seq(along = scen@model@data[[i]]@data)) {
      if (is.null(clss) || any(class(scen@model@data[[i]]@data[[j]]) == clss)) {
        if (need.name) {
          rr <- func(scen@model@data[[i]]@data[[j]], ...)
          rs[[rr$name]] <- rr$val
        } else {
          rs[[length(rs) + 1]] <- func(scen@model@data[[i]]@data[[j]], ...)
        }
      }
    }
  }
  rs
}

.check_scen_par <- .check_scen_par <- function(scen) {
  # Check for non negative parameters, all except 'pAggregateFactor', 'pTechCvarom', 'pTechAvarom', 'pTechVarom', 'pTechInvcost'
  non_negative <- unique(c(
    "pSliceShare", "pSliceWeight", "pTechOlife", "pTechCinp2ginp",
    "pTechGinp2use", "pTechCinp2use", "pTechUse2cact", "pTechCact2cout",
    "pTechEmisComm", "pTechAct2AInp", "pTechCap2AInp", "pTechNCap2AInp",
    "pTechCinp2AInp", "pTechCout2AInp",
    "pTechAct2AOut", "pTechCap2AOut", "pTechNCap2AOut", "pTechCinp2AOut",
    "pTechCout2AOut", "pTechFixom", "pTechShare",
    "pTechShare", "pTechAf", "pTechAf", "pTechAfs", "pTechAfs", "pTechAfc",
    "pTechAfc", "pTechStock", "pTechCap2act", "pDiscount",
    "pDiscountFactor", "pSupCost", "pSupAva", "pSupAva", "pSupReserve",
    "pSupReserve", "pDemand", "pEmissionFactor", "pDummyImportCost",
    "pDummyExportCost", "pTaxCostInp", "pSubCostInp", "pTaxCostOut",
    "pSubCostOut", "pTaxCostBal", "pSubCostBal",
    "pWeather", "pSupWeather", "pSupWeather", "pTechWeatherAf",
    "pTechWeatherAf", "pTechWeatherAfs", "pTechWeatherAfs",
    "pTechWeatherAfc", "pTechWeatherAfc", "pStorageWeatherAf",
    "pStorageWeatherAf", "pStorageWeatherCinp", "pStorageWeatherCinp",
    "pStorageWeatherCout", "pStorageWeatherCout", "pStorageInpEff",
    "pStorageOutEff", "pStorageStgEff", "pStorageStock", "pStorageOlife",
    "pStorageCostStore", "pStorageCostInp",
    "pStorageCostOut", "pStorageFixom", "pStorageInvcost", "pStorageCap2stg",
    "pStorageAf", "pStorageAf", "pStorageCinp", "pStorageCinp", "pStorageCout",
    "pStorageCout", "pStorageStg2AInp", "pStorageStg2AOut", "pStorageCinp2AInp",
    "pStorageCinp2AOut", "pStorageCout2AInp", "pStorageCout2AOut",
    "pStorageCap2AInp", "pStorageCap2AOut", "pStorageNCap2AInp",
    "pStorageNCap2AOut", "pTradeIrEff", "pTradeIr", "pTradeIr",
    "pTradeIrCost", "pTradeIrMarkup", "pTradeIrCsrc2Ainp",
    "pTradeIrCsrc2Aout", "pTradeIrCdst2Ainp", "pTradeIrCdst2Aout",
    "pExportRowRes", "pExportRow",
    # "pExportRowPrice",
    "pImportRowRes", "pImportRow",
    "pImportRow",
    "pTechRet", "pTechCap", "pTechNewCap",
    "pStorageRet", "pStorageCap", "pStorageNewCap",
    "pTradeRet", "pTradeCap", "pTradeNewCap"
    # "pImportRowPrice"
  ))
  # browser()
  msg_small_err <- NULL
  for (i in non_negative) {
    if (any(scen@modInp@parameters[[i]]@data$value < 0)) {
      if (any(scen@modInp@parameters[[i]]@data$value < -1e-7)) {
        msg <- paste0('An attempt to assignin negative numbers
                      to non-negative parameter: "', i)
        tmp <- scen@modInp@parameters[[i]]@data[
          scen@modInp@parameters[[i]]@data$value < 0, ,
          drop = FALSE]
        msg <- c(msg, capture.output(print(tmp[1:min(c(10, nrow(tmp))), ,
                                               drop = FALSE])))

        if (nrow(tmp) > 10) {
          msg <- c(msg,
                   paste0("Showing only the first 10 errors in data, from ",
                          nrow(tmp), "\n")
                   )
        }
        stop(paste0(msg, collapse = "\n"))
      } else {
        msg_small_err <- c(msg_small_err, i)
        scen@modInp@parameters[[i]]@data[
          scen@modInp@parameters[[i]]@data$value > -1e-7 &
          scen@modInp@parameters[[i]]@data$value < 0, "value"] <- 0
      }
    }
  }
  if (length(msg_small_err) > 0) {
    warning(paste0(
      "There small negative value (abs(err) < 1e-7) in parameter",
      "s"[length(msg_small_err) > 1], ': "',
      paste0(msg_small_err, collapse = '", "'), '". Assigned as zerro.'
    ))
  }
  # Check share
  if (nrow(scen@modInp@parameters$pTechShare@data) > 0) {
    # Share check is not working, probably unfinished migration to dplyr & data.table
    # !!! ToDo: rewrite this function.
    # browser()
    # mTechGroupComm <- .get_data_slot(scen@modInp@parameters$mTechGroupComm)
    # # scen@modInp@parameters$pTechShare@data <- merge(scen@modInp@parameters$pTechShare@data, mTechGroupComm)
    # # if (scen@modInp@parameters$pTechShare@misc$nValues != - 1)
    # # 		scen@modInp@parameters$pTechShare@misc$nValues <- nrow(scen@modInp@parameters$pTechShare@data)
    # tmp <- .add_dropped_zeros(scen@modInp, "pTechShare")
    # mTechSpan <- .get_data_slot(scen@modInp@parameters$mTechSpan)
    # browser()
    # tmp <- merge(tmp, mTechSpan)
    # tmp_lo <- merge0(tmp[tmp$type == "lo", , drop = FALSE], mTechGroupComm)
    # tmp_up <- merge0(tmp[tmp$type == "up", , drop = FALSE], mTechGroupComm)
    # tmp_lo <- aggregate(
    #   tmp_lo[, "value", drop = FALSE],
    #   select(tmp_lo, -any_of(c("type", "comm", "value"))),
    #   # tmp_lo[, !(colnames(tmp_lo) %in% c("type", "comm", "value")),
    #   #        drop = FALSE],
    #   sum)
    #   tmp_up <- aggregate(
    #   tmp_up[, "value", drop = FALSE],
    #   tmp_up[, !(colnames(tmp_up) %in% c("type", "comm", "value")),
    #          drop = FALSE],
    #   sum)
    # if (any(tmp_lo$value > 1) || any(tmp_up$value < 1)) {
    #   tech_wrong_lo <- tmp_lo[tmp_lo$value > 1, , drop = FALSE]
    #   tech_wrong_up <- tmp_up[tmp_up$value < 1, , drop = FALSE]
    #   tech_wrong <- unique(c(tech_wrong_up$tech, tech_wrong_lo$tech))
    #   assign("tech_wrong_lo", tech_wrong_lo, globalenv())
    #   assign("tech_wrong_up", tech_wrong_up, globalenv())
    #   stop(paste0(
    #     "Error in share (sum of up < 1 or sum of lo > 1)",
    #     "(see `tech_wrong_lo` and `tech_wrong_up`)",
    #     ' for technology "', paste0(tech_wrong, collapse = '", "'), '"'
    #   ))
    # }
    # fl <- colnames(tmp)[!(colnames(tmp) %in% c("type"))]
    # tmp_cmd <- merge(tmp[tmp$type == "lo", fl, drop = FALSE],
    #                  tmp[tmp$type == "up", fl, drop = FALSE],
    #                  by = fl[fl != "value"])
    # if (any(tmp_cmd$value.x > tmp_cmd$value.y)) {
    #   tech_wrong <- tmp_cmd[tmp_cmd$value.x > tmp_cmd$value.y, , drop = FALSE]
    #   assign("tech_wrong", tech_wrong, globalenv())
    #   stop(paste0(
    #     'Error in share data (tuple (tech, comm, region, year, slice) lo",
    #     " share > up), see `tech_wrong`"',
    #     paste0(unique(tech_wrong$tech), collapse = '", "'), '"'
    #   ))
    # }
  }
  scen
}

.get_map_commodity_slice_map <- .get_map_commodity_slice_map <- function(scen) {
  .apply_to_code_ret_list(
    scen = scen,
    clss = "commodity",
    func = function(x) {
      list(name = x@name, val = x@timeframe)
    }
  )
}

.get_map_commodity_slice_map_obj <- .get_map_commodity_slice_map_obj <- function(obj) {
  xx <- list()
  for (i in seq(along = obj@data)) {
    for (j in seq(along = obj@data[[i]]@data)) { #
      prec <- .add2set(
        prec,
        obj@data[[i]]@data[[j]],
        approxim = approxim)
      if (is(obj@data[[i]]@data[[j]], "commodity")) {
        if (length(obj@data[[i]]@data[[j]]@timeframe) == 0) {
          obj@data[[i]]@data[[j]]@timeframe <-
            approxim$calendar@default_timeframe
        }
        commodity_slice_map[[obj@data[[i]]@data[[j]]@name]] <-
          obj@data[[i]]@data[[j]]@timeframe
      }
    }
  }
}

.interpolation_message <- .interpolation_message <- function(name, num, interpolation_count,
                                   interpolation_start_time, len_name) {
  jj <- paste0(
    num, " (", interpolation_count, "),",
    paste0(rep(" ", max(c(1, 15 - (nchar(name) %% 15)))), collapse = ""),
    name, ", time: ", round(proc.time()[3] - interpolation_start_time, 2), "s"
  )
  # bug "invalid langth.out element - workaround
  length_out <- len_name - nchar(jj)
  if (length_out < 0) {
    len_name <- len_name + abs(length_out)
    length_out <- 0
  }
  jj <- paste0(jj, paste0(rep(" ", length_out), collapse = ""))
  # cat(rep_len("\b", len_name), jj, sep = "") # , rep(' ', 100), rep('\b', 100)
}

interpolate_slot <- interpolate_slot <- function(
    x,
    keys = c("region", "slice", "comm", "acomm", "tech", "process",
             "weather", "stg", "sub", "dst", "src"),
    year_seq = NULL,
    val = "value"
) {
  # browser()
  if (!is.null(x$year)) {
    # year_seq = full_seq(c(x$year, year_seq), 1)
    if (is.null(year_seq)) year_seq = full_seq(x$year, 1)
    x <- x |>
      group_by(
        across(any_of(keys))
      ) |>
      complete(year = year_seq) |>
      ungroup()
    if (!is.null(val) && !is.na(val)) {
      x <- x |>
        mutate(
          {{val}} := zoo::na.approx(.data[[val]], x = year)
        )
    }
  }
  # if (is.null(year_seq)) year_seq = full_seq(x$year, 1)
    #
    # mutate(
    #   {{val}} := zoo::na.approx(.data[[val]], x = year)
    # ) |>
    # as.data.table() |>
    # ungroup()
  x
}

# ---- from obj2modInp.R ----
.add_ramp0 <- .add_ramp0 <- function(obj, name, tech, mact, approxim) {
  if (any(!is.na(tech@af[[name]]))) {
    # browser()
    pname <- paste0(
      "p", c("technology" = "Tech", "storage" = "Storage")[class(tech)],
      c("rampup" = "RampUp", "rampdown" = "RampDown", name)[name]
    )
    set_name <- c("technology" = "tech", "storage" = "stg")[class(tech)]
    mname <- sub("^p", "m", pname)
    rampup <- tech@af[!is.na(tech@af[[name]]), ]
    approxim2 <- approxim
    if (all(!is.na(rampup$slice))) {
      approxim2$slice <- approxim2$slice[approxim2$slice %in% unique(rampup$slice)]
    }
    pTechRampUp <- .interp_numpar(
      rampup, name,
      obj@parameters[[pname]], approxim2, set_name, tech@name
    )
    # mTechRampUp <- pTechRampUp[, colnames(pTechRampUp) != "value", drop = FALSE]
    mTechRampUp <- select(pTechRampUp, -value)
    if (ncol(mTechRampUp) != ncol(obj@parameters[[mname]]@data)) {
      mTechRampUp <- merge0(mTechRampUp, mact)
    }
    # browser()
    # adding slicep (next) to the mapping
    # ramp_data <- c(tech@af$rampdown, tech@af$rampup)
    # if (!is_empty(ramp_data) && any(!is.na(ramp_data))) {

    if (tech@fullYear) {
      SliceNext <- obj@parameters[["mSliceFYearNext"]]@data
    } else {
      SliceNext <- obj@parameters[["mSliceNext"]]@data
    }
    mTechRampUp <- left_join(mTechRampUp, SliceNext, by = "slice") |>
      select(all_of(obj@parameters[[mname]]@dimSets))

    # tech_name <- tech@name
      # mTechRampSliceNext <- mTechRampSliceNext |>
      #   mutate(tech = tech_name, .before = 1) |>
      #   merge0(mvTechAct) |>
        # select(all_of(obj@parameters[["mTechRampSliceNext"]]@dimSets))
      # obj@parameters[["mTechRampSliceNext"]] <-
      #   .dat2par(obj@parameters[["mTechRampSliceNext"]], mTechRampSliceNext)
    # }
    # !!! Temporary fix: drop values beyond technology lifespan
    # synchronizing with activity slices
    # if (!is.null(pTechRampUp$region)) {
    #   pTechRampUp <- dplyr::filter(pTechRampUp, region %in% unique(mact$region))
    #   mTechRampUp <- dplyr::filter(mTechRampUp, region %in% unique(mact$region))
    # }
    # if (!is.null(pTechRampUp$year)) {
    #   pTechRampUp <- dplyr::filter(pTechRampUp, year %in% unique(mact$year))
    #   mTechRampUp <- dplyr::filter(mTechRampUp, year %in% unique(mact$year))
    # }
    # if (!is.null(pTechRampUp$slice)) {
    #   pTechRampUp <- dplyr::filter(pTechRampUp, slice %in% unique(mact$slice))
    #   mTechRampUp <- dplyr::filter(mTechRampUp, slice %in% unique(mact$slice))
    # }
    # !!! end
    #
    obj@parameters[[pname]] <- .dat2par(obj@parameters[[pname]], pTechRampUp)
    obj@parameters[[mname]] <- .dat2par(obj@parameters[[mname]], mTechRampUp)
    #
    # browser()
    # adding mapping for `slicep`
    # "mTechRampSliceNext" # tech, region, year, slice, slicep
    # if (tech@fullYear) {
    #   x <- obj@parameters[["mSliceFYearNext"]]@data
    # } else {
    #   x <- obj@parameters[["mSliceNext"]]@data
    # }
    # tech_name <- tech@name
    # x <- mutate(x, tech = tech_name, .before = 1) |>
    #   merge0(mact)
    # obj@parameters[["mTechFullYear"]]@data
    # obj@parameters[["mSliceFYearNext"]]@data
    # obj@parameters[["mSliceNext"]]
    # obj@parameters[["mvTechAct"]]@data
  }
  obj
}

.filter_data_in_slots <- .filter_data_in_slots <- function(obj, lst, coln) {
  # browser()
  # filter out
  ss <- getSlots(class(obj))
  if (any(names(ss) == coln) && ss[coln] == "character") {
    # !!! adding (potentially) missing filter for character slots like region
    if (!all(is.na(slot(obj, coln))) && length(slot(obj, coln)) > 0) {
      slot(obj, coln) <- slot(obj, coln)[slot(obj, coln) %in% lst]
    }
  }
  ss <- names(ss)[ss %in% "data.frame"]
  ss <- ss[sapply(ss, function(x) {
    any(colnames(slot(obj, x)) == coln) && nrow(slot(obj, x)) != 0
    })]
  for (sl in ss) {
    slot(obj, sl) <- slot(obj, sl)[
      is.na(slot(obj, sl)[, coln]) |
        slot(obj, sl)[, coln] %in% lst, ,
      drop = FALSE]
  }
  obj
}

.fix_approximation_list <- .fix_approximation_list <- function(approxim, lev = NULL, comm = NULL) {
  # better name?
  # browser()
  if (length(lev) == 0) {
    if (length(comm) == 0) {
      stop("Internal error: 66a37cde-24e2-4ac5-ab24-b79e0f603bf7")
    }
    lev <- approxim$commodity_slice_map[[comm]]
  }
  # ??? better name for approxim$parent_child ???
  approxim$parent_child <- approxim$calendar@slice_ancestry
  approxim$slice <- approxim$calendar@timeframes[[lev]]
  # approxim$parent_child <-
  #   approxim$parent_child[approxim$parent_child$child %in% approxim$slice, ,
  #                         drop = FALSE]
  approxim$parent_child <- approxim$parent_child |>
    filter(child %in% approxim$slice)
  approxim
}

.force_value_class_df <- .force_value_class_df <- function(dtf) {
  if (!is.data.frame(dtf) & !is.list(dtf)) invisible()  # browser() disabled
  # return(dtf)
  # temporary solution to avoid merging conflicts
  if (!is.null(dtf[["value"]]) && !inherits(dtf[["value"]], "numeric")) {
    print(as_tibble(dtf))
    stop("Non-numeric 'value' column")
    dtf[["value"]] <- as.numeric(dtf[["value"]])
  }
  as.data.table(dtf)
}

.force_year_class_df <- .force_year_class_df <- function(dtf) {
  # if (!is.data.frame(dtf) & !is.list(dtf)) browser()
  # return(dtf)
  # temporary solution to avoid merging conflicts
  year_vars <- c("year", "yearp", "start", "end", "olife")
  force_class <- "integer"
  # force_class <- "numeric"
  for (y in year_vars) {
    if (!is.null(dtf[[y]]) && !inherits(dtf, force_class)) {
      dtf[[y]] <- as(dtf[[y]], force_class)
    }
  }
  as.data.table(dtf)
}

.null_to_empty_param <- .null_to_empty_param <- function(pname, pp) {
  # pp - podInp@parameters
  # browser()
  # pp <- get(pp, envir = parent.frame())
  p <- get(pname, envir = parent.frame())
  if (is.null(p)) p <- pp[[pname]]@data[0, ]
  assign(pname, p, envir = parent.frame())
  # p
}

.process_lifespan <- .process_lifespan <- function(approxim, obj, als, stock_exist) {
  #!!! ToDo: check if @invcost$eac is considered
  # browser()
  if (is.null(stock_exist)) stock_exist <- data.table()
  stock_exist <- stock_exist[stock_exist$value != 0, ]
  # Start / End year
  dd <- data.table(
    enable = rep(TRUE, length(approxim$region) * length(approxim$year)),
    obj = rep(obj@name, length(approxim$region) * length(approxim$year)),
    region = rep(approxim$region, length(approxim$year)),
    year = c(t(matrix(rep(approxim$year, length(approxim$region)),
                      length(approxim$year)))),
    stringsAsFactors = FALSE
  )
  colnames(dd)[2] <- als
  dstart <- data.table(
    # row.names = approxim$region,
    region = approxim$region,
    year = as.integer(rep(NA, length(approxim$region))),
    stringsAsFactors = FALSE
  )
  fl <- is.na(obj@start$region)
  if (any(fl)) {
    if (sum(fl) != 1) {
      # stop('Wrong start year for "', class(obj), '" ', obj@name)
      stop('Two or more "NA" values in "@start" slot, column "region", class "',
           class(obj), '" ', obj@name)
    }
    dstart[, "year"] <- obj@start[fl, "start"]
  }
  if (any(!fl)) {
    # if (obj@name == "BASN_battery_moderate_0") browser()
    # dstart[obj@start[!fl, "region"], "year"] <- obj@start[!fl, "start"]
    ob_x <- obj@start[!fl, ] |> rename(year = start)
    dstart <- rows_update(dstart, ob_x, by = "region")
  }
  # dstart <- dstart[!is.na(dstart$year), , drop = FALSE]
  dstart <- filter(dstart, !is.na(year))
  for (rr in dstart$region) {
    # browser()
    # if (!is.na(dstart[rr, "year"]) && any(dd$year < dstart[rr, "year"]))
    #   dd[dd$region == rr & dd$year < dstart[rr, "year"], "enable"] <- FALSE
    ii <- dstart$region %in% rr
    if (!is.na(dstart$year[ii]) && any(dd$year < dstart$year[ii])) {
      dd$enable[dd$region == rr & dd$year < dstart$year[ii]] <- FALSE
    }
  }
  dd_able <- dd
  ## end
  dend <- data.table(
    row.names = approxim$region,
    region = approxim$region,
    year = as.integer(rep(NA, length(approxim$region))),
    stringsAsFactors = FALSE
  )
  fl <- is.na(obj@end$region)
  if (any(fl)) {
    if (sum(fl) != 1) {
      stop('Two or more "NA" values in "@end" slot, column "region", class "',
           class(obj), '" ', obj@name)
    }
    dend[, "year"] <- obj@end[fl, "end"]
  }
  if (any(!fl)) {
    # if (obj@name == "ECCG") browser()
    # suppressMessages({
    # dend <- dend |> filter(!fl) |> select(-year) |>
    #     left_join(obj@end[!fl, ], by = "region") |> rename(year = end) |>
    #     rbind(filter(dend, fl))
    # })
    # dend[obj@end[!fl, "region"], "year"] <- obj@end[!fl, "end"]
    ob_x <- obj@end[!fl, ] |> rename(year = end)
    dend <- rows_update(dend, ob_x, by = "region")
    rm(ob_x)
  }
  # dend <- dend[!is.na(dend$year), , drop = FALSE]
  dend <- filter(dend, !is.na(year))
  for (rr in dend$region) {
    ii <- dend$region %in% rr
    if (any(dd$year > dend$year[ii]))
      dd[dd$region == rr & dd$year > dend$year[ii], "enable"] <- FALSE
  }
  dd <- dd[dd$enable, -1, drop = FALSE]
  ## life
  dlife <- data.table(
    # row.names = approxim$region,
    region = approxim$region,
    year = as.integer(rep(NA, length(approxim$region))),
    stringsAsFactors = FALSE
  )
  fl <- is.na(obj@olife$region)
  if (any(fl)) {
    if (sum(fl) != 1) {
      # stop('Wrong start year for "', class(obj), '" ', obj@name)
      stop('Two or more "NA" values in "@olife" slot, column "region", class "',
           class(obj), '" ', obj@name)
    }
    dlife[, "year"] <- obj@olife[fl, "olife"] # !!! ???
  }
  if (any(!fl)) {
    # dlife[obj@olife[!fl, "region"], "year"] <- obj@olife[!fl, "olife"]
    ob_x <- obj@olife[!fl, ] |> rename(year = olife)
    dlife <- rows_update(dlife, ob_x, by = "region")
    rm(ob_x)
  }
  # dlife <- dlife[!is.na(dlife$year), , drop = FALSE]
  dlife <- filter(dlife, !is.na(year))
  # if (obj@name == "stg_BASN_conventional_hydroelectric_1") browser()
  for (rr in dlife$region[dlife$region %in% dend$region]) {
    # if (obj@name == "stg_BASN_conventional_hydroelectric_1") browser()
    nn <- dend$region %in% rr
    ii <- dlife$region %in% rr
    if (any(dd_able$year >= dend$year[nn] + dlife$year[ii])) {
      ee <- dd_able$region == rr &
        dd_able$year >= dend$year[nn] + dlife$year[rr]
      dd_able$enable[ee] <- FALSE
    }
  }
  dd_eac <- dd_able
  if (nrow(stock_exist) != 0 && any(!dd_able$enable)) {
    for (rr in unique(stock_exist$region)) {
      ii <- stock_exist$region == rr
      ee <- dd_able$region == rr & dd_able$year %in% stock_exist$year[ii]
      dd_able$enable[ee] <- TRUE
    }
  }
  #
  dd_able <- dd_able[dd_able$enable, -1, drop = FALSE]
  dd_eac <- dd_eac[dd_eac$enable, -1, drop = FALSE]
  dd <- dd[dd$year %in% approxim$mileStoneYears, ]
  dd_eac <- dd_eac[dd_eac$year %in% approxim$mileStoneYears, ]
  dd_able <- dd_able[dd_able$year %in% approxim$mileStoneYears, ]
  # browser()
  # list(new = dd, span = dd_able, eac = dd_eac)
  # redefining EAC for all years of investment
  list(new = dd, span = dd_able, eac = dd_able)

}

.toWeatherImply <- .toWeatherImply <- function(dtf, val, add_set, add_val, sets = NULL) {
  dtf <- as.data.table(dtf)
  # browser() ### !!! ToDo: dplyr
  # f1 <- dtf[!is.na(dtf[, paste0(val, ".up")]),
  #           c(paste0(val, ".up"), "weather", sets),
  #           drop = FALSE]
  # colnames(f1)[1] <- "value"
  c_nm <- paste0(val, ".up")
  ii <- select(dtf, all_of(c_nm))[[1]] |> is.na()
  f1 <- dtf |>
    filter(!ii) |>
    select(all_of(c(c_nm, "weather", sets))) |>
    rename(value = all_of(c_nm))
  # f2 <- dtf[!is.na(dtf[, paste0(val, ".fx")]),
  #           c(paste0(val, ".fx"), "weather", sets),
  #           drop = FALSE]
  # colnames(f2)[1] <- "value"
  c_nm <- paste0(val, ".fx")
  ii <- select(dtf, all_of(c_nm))[[1]] |> is.na()
  f2 <- dtf |>
    filter(!ii) |>
    select(all_of(c(c_nm, "weather", sets))) |>
    rename(value = all_of(c_nm))
  # f3 <- dtf[!is.na(dtf[, paste0(val, ".lo")]),
  #           c(paste0(val, ".lo"), "weather", sets),
  #           drop = FALSE]
  # colnames(f3)[1] <- "value"
  c_nm <- paste0(val, ".lo")
  ii <- select(dtf, all_of(c_nm))[[1]] |> is.na()
  f3 <- dtf |>
    filter(!ii) |>
    select(all_of(c(c_nm, "weather", sets))) |>
    rename(value = all_of(c_nm))
  rs <- list(par = NULL)
  if (nrow(f1) + nrow(f2) != 0) {
    tmp <- rbind(f1, f2)
    # tmp[, add_set] <- add_val
    tmp[[add_set]] <- add_val
    # rs$mapup <- tmp[, -1, drop = FALSE]
    rs$mapup <- select(tmp, -value)
    tmp$type <- "up"
    rs$par <- tmp
  }
  if (nrow(f3) + nrow(f2) != 0) {
    tmp <- rbind(f3, f2)
    # tmp[, add_set] <- add_val
    tmp[[add_set]] <- add_val
    # rs$maplo <- tmp[, -1, drop = FALSE]
    rs$maplo <- select(tmp, -value)
    tmp$type <- "lo"
    rs$par <- rbind(rs$par, tmp)
  }
  rs
}

merge0 <- merge0 <- function(x, y,
                   by = intersect(
                     colnames(as.data.table(x)),
                     colnames(as.data.table(y))
                   ),
                   ...) {
  # assign('x', x, globalenv()) assign('y', y, globalenv())
  if (length(by) != 0) {
    # browser()
    y <- as.data.table(y) |> .force_year_class_df()
    x <- as.data.table(x) |> .force_year_class_df()
    xy <- merge(x, y, by = by, ..., allow.cartesian = TRUE)
    # return(as.data.table(xy)) # debug pDiscountFactorMileStone
    return(xy)
  }
  # browser()
  # y <- as.data.table(y) |> .force_year_class_df()
  # x <- as.data.table(x) |> .force_year_class_df()
  y <- .force_year_class_df(y)
  x <- .force_year_class_df(x)
  # xy <- merge(x, y)
  suppressMessages({
    xy <- dplyr::cross_join(x, y) # !!! rewrite
  })
  # colnames(xy) <- c(colnames(x), colnames(y)) # ???
  # return(as.data.table(xy))
  return(as.data.table(xy))
}

